import Browser
import Html exposing (Html, text, div, input, button, span, b, a)
import Html.Lazy exposing (lazy)
import Html.Attributes as A
import Html.Events exposing (onClick, onInput, onDoubleClick)
import Html.Events.Extra exposing (onEnter)
import Http
import XmlParser exposing (Node(..))
import String.Mark as Mark
import File.Download as Download

import Debug

main 
  = Browser.element
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = view
  }

type alias Cue =
  { start : Float
  , duration : Float
  , content : String
  }

type alias Model = 
  { input_field : String
  , current_id : ID
  , current_position : Float
  , search_term : String
  , cues : List Cue
  , stats : List(List(String, Int))
  , preferred_stats : (Stat_View, Order)
  , state : State
  }

type State = Fresh | XML_Invalid | ID_Invalid | Load_Failed | Loading_YT | Loading_Cues | Received Editing | Stats Category

type Editing = No | Yes Int
type Category = Word_Length | Word_Count 
type Field = F_LinkID | F_Search
type Stat_View = Group_Size Order | Merged 
type Order = ASCENDING | DESCENDING
type Download = Transcript | Word_Counts
type alias ID = String

type Error
  = BadInput
  | NotAYtURL
  | EmptyString

type Msg
  = ID_Changed String
  | Cue_Changed Int String
  | Validate_And_Fetch
  | GotCues (Result Http.Error String)
  | Player_Loaded
  | Player_Time_At Float
  | JumpTo Float
  | Summon_Editor Int
  | Search_Changed String
  | View_Stats
  | View_Cues
  | Clear_Field Field
  | Download_Data Download

port send_to_yt_API       : String -> Cmd msg
port receive_msg_from_API : (Float -> msg) -> Sub msg

init : () -> (Model, Cmd Msg)
init _ = (
  { input_field = "I7jf_U89ddk"
  , current_id = ""
  , current_position = 0.0
  , search_term = ""
  , cues = []
  , stats = []
  , preferred_stats = (Group_Size ASCENDING, ASCENDING)
  , state = (Fresh)
  },
  Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Player_Loaded ->
      case model.state of
        Loading_YT         -> ({model | state = Loading_Cues}            ,(fetch_transcript model.current_id ))
        _ -> doNothing model
    GotCues result ->
      case model.state of
        Loading_Cues ->
          case result of
            Ok fullText -> 
              case parse_xml fullText of 
                Nothing    -> ({model | state = XML_Invalid }             ,Cmd.none)
                Just cues  -> ({model | state = Received No, cues = cues} ,Cmd.none)
            Err _          -> ({model | state = Load_Failed }             ,Cmd.none)
        _ -> doNothing model
    Summon_Editor who ->
      case model.state of
        Received _         -> ({model | state = (Received (Yes who)) }    ,Cmd.none)
        _ -> doNothing model
    JumpTo position ->
      case model.state of
        Received (Yes who) -> ({model | state = (Received No), current_position = position } ,Cmd.none) 
        Received No        -> ({model | current_position = position }     ,send_to_yt_API <| String.concat ["Seek:", String.fromFloat <| position])
        _ -> doNothing model
    Player_Time_At second ->
      case model.state of
        Received _         -> ({model | current_position = second }       ,Cmd.none)
        _ -> doNothing model
    Search_Changed new ->
      case model.state of 
        Received _         -> ({model | search_term = new }               ,Cmd.none)
        _ -> doNothing model
    View_Stats ->
      case model.state of
        Received _         -> ({model | state = Stats Word_Length, stats = count_words_once model}    ,Cmd.none)
        _ -> doNothing model
    View_Cues ->
      case model.state of
        Stats _            -> ({model | state = Received No }             ,Cmd.none)
        _ -> doNothing model
    Clear_Field which ->
      case which of 
        F_Search           -> ({model | search_term = "" }                ,Cmd.none)
        F_LinkID           -> ({model | input_field = "" }                ,Cmd.none)
    Download_Data dl       -> ( model                                     ,package_and_download dl model)
    Validate_And_Fetch ->
      case model.state of
        Loading_Cues -> doNothing model
        Loading_YT   -> doNothing model
        _ -> 
          case validate_id model.input_field of
            Ok video_id    -> ({model | state = Loading_YT, current_id = video_id } ,send_to_yt_API <| ("ID:"++video_id))
            Err _          -> ({model | state = ID_Invalid }              ,Cmd.none) 
    Cue_Changed who new ->
      case model.state of
        Received (Yes _)   -> ({model | cues = (List.indexedMap (update_if new who) model.cues), stats = haeufigkeitsanalyse model.cues } ,Cmd.none)
        _ -> doNothing model
    ID_Changed new ->
      case model.state of
        Received (Yes who) -> ({model | state = (Received No) }           ,Cmd.none)
        _                  -> ({model | input_field = new }               ,Cmd.none)

view : Model -> Html Msg
view model =
  case model.state of
    Fresh         -> (lazy (unloaded_elements model.input_field) "")
    XML_Invalid   -> (lazy (unloaded_elements model.input_field) "Sorry, couldn't parse transcript!")
    ID_Invalid    -> (lazy (unloaded_elements model.input_field) "The link/ID is unrecognisable.")
    Load_Failed   -> (lazy (unloaded_elements model.input_field) "Sorry, couldn't find transcript!")
    Loading_YT    -> (lazy (unloaded_elements model.input_field) "Loading...")
    Loading_Cues  -> (lazy (unloaded_elements model.input_field) "Loading...")
    Received edit -> 
      div [ A.class "TLE-everything" ] 
        [ lazy (div [A.class "TLE-flex-row"]) [search_field model.search_term, delete_content F_Search "search" "Clears the search term", download_data Transcript "download-transcript" "Download the transcript as-is", stats_button]
        , lazy (div ([ A.id "cues-container", A.class "TLE-text", A.class "TLE-container"] ) ) (List.indexedMap (generate_html_from_cue model.current_position edit model.search_term) model.cues)
        , lazy (unloaded_elements model.input_field) <| "Loaded "++model.current_id
        ]
    Stats which   -> 
      div [ A.class "TLE-everything" ] 
        [ lazy (div [A.class "TLE-flex-row"]) [search_field model.search_term, delete_content F_Search "search" "Clears the search term", download_data Word_Counts "download-statistics" "Download the counted word data", cue_button]
        , lazy (div ([ A.id "stats-container", A.class "TLE-text", A.class "TLE-container"] )) (display_stats model.preferred_stats <| List.map (List.sortBy Tuple.second) (model.stats))
        , lazy (unloaded_elements model.input_field) <| "Loaded "++model.current_id
        ]
subscriptions : Model -> Sub Msg
subscriptions _ = receive_msg_from_API loaded_or_position

-- Helper --

fetch_transcript : String -> Cmd Msg
fetch_transcript video_id = 
  Http.get  { url = "https://video.google.com/timedtext?v=" ++ video_id ++ "&lang=en", expect = Http.expectString GotCues }

unloaded_elements : String -> String -> Html Msg
unloaded_elements input_field msg = 
  div [A.class "TLE-flex-row"]
  ([ tooltip_button
  , url_field <| input_field -- ID input
  , delete_content F_LinkID "Link-ID" "Clears the URL/link"
  , fetch_button             -- Fetch Subtitles
  , status_message <| msg    -- Whether it's loaded or an Error has occurred
  ])

tooltip_button : Html Msg
tooltip_button = a 
  [ A.id "TLE-tooltip"
  , A.tabindex -1
  , A.title "Enter a YouTube link or ID and press the \"Fetch\" button. Then click on any word in the transcript box to get transported to the point in the video where that word is said. Watching the video will do the same in reverse. You can edit the text by double-clicking any word as well."
  , A.class "TLE-top-elements"
  ] [text "i"]
  
url_field : String -> Html Msg
url_field id = input 
  [ A.placeholder "Please provide a YouTube link/ID."
  , A.value id
  , onInput ID_Changed
  , onEnter Validate_And_Fetch
  , A.id "input-video-url-or-id"
  , A.title "Please provide a YouTube URL or just the ID"
  , A.class "TLE-top-elements"
  ] []

fetch_button : Html Msg
fetch_button = button 
  [ onClick Validate_And_Fetch
  , A.id "button-fetches-transcript-by-id"
  , A.title "Fetch the video and its transcript"
  , A.class "TLE-top-elements"
  ] [ text "Fetch" ]
stats_button : Html Msg
stats_button = button 
  [ onClick View_Stats
  , A.id "button-switches-to-stats-view"
  , A.title "View statistics about this transcript"
  , A.class "TLE-top-elements"
  ] [ text "Display Stats" ]
cue_button : Html Msg
cue_button = button 
  [ onClick View_Cues
  , A.id "button-switches-to-cues-view"
  , A.title "Go back to the transcript"
  , A.class "TLE-top-elements"
  ] [ text "Display Transcript" ]
  
delete_content : Field -> String -> String -> Html Msg
delete_content field name title = button 
  [ onClick (Clear_Field field)
  , A.id ("button-"++name)
  , A.title title
  , A.class "TLE-top-elements"
  , A.class "TLE-X-delete"
  ] [text "X"]
download_data : Download -> String -> String -> Html Msg
download_data download name title = button 
  [ onClick (Download_Data download)
  , A.id ("button-"++name)
  , A.title title
  , A.class "TLE-top-elements"
  , A.class "TLE-download-button"
  ] [text "Download"]

status_message : String -> Html Msg
status_message message = span [ A.class "TLE-top-elements"] [text message]
  
search_field : String -> Html Msg
search_field search_term = input 
  [ A.placeholder "Search"
  , A.value search_term
  , onInput Search_Changed
  , A.id "input-search-within-text"
  , A.class "TLE-top-elements"
  ] []

package_and_download : Download -> Model -> Cmd Msg
package_and_download dl model =
  case dl of
    Transcript  -> Download.string (model.current_id++"_transcript.csv") "text/csv" ("start,duration,content\r\n"++ String.concat (List.map (\{start, duration, content} -> String.concat [String.fromFloat start, ",", String.fromFloat duration, ",", (String.replace "\n" "" content), "\r\n"]) (List.drop 1 model.cues)))
    Word_Counts -> Download.string (model.current_id++"_stats.csv") "text/csv" ("word,count\r\n"++ String.concat (List.map (\(w, c) -> String.concat [w, ",", String.fromInt c, "\r\n"]) (List.foldl (++) [] model.stats)))

generate_html_from_cue : Float -> Editing -> String -> Int -> Cue -> Html Msg 
generate_html_from_cue time_sec whether_editing search_for index cue = 
  case whether_editing of
    No         ->                         (create_cue_span index cue (cue.start <= time_sec && time_sec < cue.start+cue.duration) search_for)
    Yes number -> if index == number then (create_editable_cue index cue)
                                     else (create_cue_span index cue False search_for)

create_cue_span : Int -> Cue -> Bool -> String -> Html Msg
create_cue_span index cue highlight search_for = 
  span 
    [ onClick (JumpTo cue.start)
    , onDoubleClick (Summon_Editor index)
    , A.id ("cue-"++ (String.fromInt index))
    , A.class "cue"
    , (A.style "background-color" ((\h -> if h then "#8CF" else "#FFF") highlight))
    ] <| Mark.mark search_for cue.content
    
create_editable_cue : Int -> Cue -> Html Msg
create_editable_cue index cue =
  input (
    [ onInput (Cue_Changed index)
    , onDoubleClick (JumpTo cue.start)
    , onEnter (JumpTo cue.start)
    , A.id ("cue-input-"++ (String.fromInt index))
    , A.class "cue-input"
    , A.value cue.content
    , A.style "min-width" (String.fromInt (round((toFloat(String.length cue.content))*0.8))++"ch")
    , A.class "TLE-text"
    ]) []



------------------------------------------------------------------------------------------
---------------------------------------- HELPERS  ----------------------------------------
------------------------------------------------------------------------------------------

loaded_or_position : Float -> Msg
loaded_or_position msg =
  if msg < 0 then Player_Loaded
  else            Player_Time_At msg
  
doNothing : Model -> (Model, Cmd Msg)
doNothing model = (model, Cmd.none)
  
validate_id : String -> Result Error String
validate_id input = 
  if String.isEmpty input then                                                Err EmptyString
  else if ((String.length input) < 15) && ((String.length input) > 10) then   Ok input -- 11 chars in an ID gives 1.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000.000 permutations :D
  else if String.contains "youtu.be" input then                               butcher_URL input ".be/"      "?" BadInput
  else if String.contains "youtube" input then                                
    if String.contains "/v/" input then                                       butcher_URL input "/v/"       "?" BadInput
    else if String.contains "/watch?v=" input then                            butcher_URL input "/watch?v=" "&" BadInput
    else                                                                      Err NotAYtURL
  else                                                                        Err NotAYtURL
  
butcher_URL : String -> String -> String -> Error -> Result Error String
butcher_URL url cut1 cut2 error =
  case (String.split cut1 url) of
      head::tail::[] -> 
        case (String.split cut2 tail) of
          id::xs -> Ok id
          _ -> Err error
      _ -> Err error

update_if : String -> Int -> Int -> Cue -> Cue -- to be used with indexedMap
update_if new_content only_this_one_gets_changed current_index current_cue = 
  if current_index == only_this_one_gets_changed then { current_cue | content = new_content }
  else                                                  current_cue  

parse_xml : String -> Maybe (List Cue)
parse_xml xml = 
  case XmlParser.parse xml of
    Err error ->
      Nothing
    Ok result ->
      case result.root of 
        (Text text) ->
          Nothing
        (Element text attributes cue_nodes) ->
          Just (empty_cue :: (corral_cues cue_nodes))

corral_cues : List Node -> List Cue
corral_cues nodes = 
  case nodes of
    [] -> []
    [cue] -> 
      [extract_information cue]
    c1::c2::c3::c4::c5::c6::c7::c8::xs -> -- this is to prevent recursion limit problems for very long videos.
      List.append 
        (  funnel_cue c1
        ++ funnel_cue c2
        ++ funnel_cue c3
        ++ funnel_cue c4
        ++ funnel_cue c5
        ++ funnel_cue c6
        ++ funnel_cue c7
        ++ funnel_cue c8
        ) 
        (corral_cues xs) 
    cue::xs ->
      funnel_cue cue ++ corral_cues xs

funnel_cue : Node -> List Cue
funnel_cue node = 
  case node of
    Text text ->
      []
    Element text attr content ->
      [extract_information node]

extract_information : Node -> Cue
extract_information node =
  case node of
    Element _ ( start_attr :: duration_attr :: []) (( Text cue_text) :: []) ->
      { start    = (toFloat_with_default start_attr.value 0.0)
      , duration = (toFloat_with_default duration_attr.value 0.0)
      , content  = ((String.replace "&#39;" "'" <| String.replace "&quot;" "\"" <| cue_text) ++ " ")
      }
    _ -> empty_cue

empty_cue : Cue
empty_cue = {start=0.0, duration=0.0, content=">> "}

toFloat_with_default : String -> Float -> Float
toFloat_with_default string def =
  case String.toFloat string of
    Nothing    -> def
    Just value -> value
    
-----------------------------------------------------------------------------------
-------------------------------------- STATS --------------------------------------
-----------------------------------------------------------------------------------

haeufigkeitsanalyse : List(Cue) -> List (List (String, Int))
haeufigkeitsanalyse cues =
  let the_text = cues |> List.map (extract_content) |> String.concat
      the_words= the_text |> String.words
  in List.map (make_list_of_groups_and_count_and_cull the_words) [1,2,3]

make_list_of_groups_and_count_and_cull : List String -> Int -> List (String, Int)
make_list_of_groups_and_count_and_cull the_words group_size =
  group_list the_words group_size |> List.sort |> tally_in_sorted_list |> cull_list

group_list : List String -> Int -> List String
group_list a_list group_size =
  let chunk_size = 4
  in   if List.length a_list > group_size + chunk_size then -- again, bulk appending to give myself n times the recursion limit
    List.map                 (\n -> List.drop n a_list |> List.take group_size |> String.join " ") (List.range 0 chunk_size)
    ++ (group_list (List.drop chunk_size a_list) group_size)
  else if List.length a_list > group_size then (a_list |> List.take group_size |> String.join " ") :: group_list (List.drop 1 a_list) group_size
  else                                         [a_list                         |> String.join " "]

tally_in_sorted_list : List String -> List (String, Int)
tally_in_sorted_list a_list = 
  case List.head a_list of
    Nothing -> []
    Just x  -> 
      let n = count_in_sorted_list x a_list
      in (x, n) :: tally_in_sorted_list (List.drop n a_list)

count_in_sorted_list : a -> List a -> Int
count_in_sorted_list x xs = List.length (takeWhile ((==)x) xs)

takeWhile : (a -> Bool) -> List a -> List a 
takeWhile cond a_list = 
  case a_list of
    []   -> []
    x::xs -> 
      if cond x then x :: takeWhile cond xs
      else [] 

cull_list : List (String, Int) -> List (String, Int)
cull_list the_list = List.filter (filter_occurrences) the_list

filter_occurrences : (String, Int) -> Bool
filter_occurrences (s, i) = (i > 1)

extract_content : Cue -> String
extract_content cue = cue.content

count_words_once : Model -> List (List (String, Int))
count_words_once model = 
  case List.head model.stats of
    Nothing -> haeufigkeitsanalyse model.cues
    Just _  -> model.stats

display_stats : (Stat_View, Order) -> List(List(String, Int)) -> List(Html Msg)
display_stats (how, ord) data = 
  let 
    structured_data =
      case how of
        Group_Size g_ord ->
          case g_ord of
            ASCENDING  -> data
            DESCENDING -> List.reverse data
        Merged -> [List.foldl (merge_keep_order) [] data]
    ordered_data = 
      case ord of
        ASCENDING  -> structured_data
        DESCENDING -> List.map List.reverse structured_data 
    
  in
    create_stats_entries ordered_data

create_stats_entries : List(List(String, Int)) -> List(Html Msg)
create_stats_entries data =
  case data of
    []      -> []
    xs::xss -> (create_stats_entry xs) ++ (create_stats_entries xss)
    
create_stats_entry : List(String, Int) -> List(Html Msg)
create_stats_entry data =
  case data of
    []    -> []
    (w,c)::xs -> [div [A.class "TLE-stats-entry"] [span [A.class "TLE-stats-count"] [text <| (String.fromInt c)++"x"], span [A.class "TLE-stats-word"] [text w]]]++create_stats_entry xs

merge_keep_order : List (String, Int) -> List (String, Int) -> List (String, Int)
merge_keep_order list_a list_b =
  case (list_a, list_b) of
    ([], ys) -> ys
    (xs, []) -> xs
    (x::xs, y::ys) -> 
      if Tuple.second x <= Tuple.second y then
        x :: merge_keep_order (xs) (y::ys)
      else
        y :: merge_keep_order (x::xs) (ys)

