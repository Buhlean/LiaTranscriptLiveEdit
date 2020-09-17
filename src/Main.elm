import Browser
import Html exposing (Html, text, div, input, button, span, b, a)
import Html.Lazy exposing (lazy)
import Html.Attributes as A
import Html.Events exposing (onClick, onInput, onDoubleClick)
import Http
import XmlParser exposing (Node(..))

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
  , current_position : Float
  , state : State
  }

type State = Fresh | XML_Invalid | ID_Invalid | Load_Failed | Loading_YT ID | Loading_Cues ID | Received (List Cue) ID Editing 

type Editing = No | Yes Int

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

port send_to_yt_API       : String -> Cmd msg
port receive_msg_from_API : (Float -> msg) -> Sub msg

init : () -> (Model, Cmd Msg)
init _ = (
  { input_field = "I7jf_U89ddk"
  , current_position = 0.0
  , state = (Fresh)
  },
  Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Player_Loaded ->
      case model.state of
        Loading_YT video_id         -> ({model | state = Loading_Cues video_id}         ,(fetch_transcript video_id ))
        _ -> doNothing model
    GotCues result ->
      case model.state of
        Loading_Cues id ->
          case result of
            Ok fullText -> 
              case parse_xml fullText of 
                Nothing             -> ({model | state = XML_Invalid }                  ,Cmd.none)
                Just cues           -> ({model | state = Received cues id No}           ,Cmd.none)
            Err _                   -> ({model | state = Load_Failed }                  ,Cmd.none)
        _ -> doNothing model
    Summon_Editor who ->
      case model.state of
        Received cues id _          -> ({model | state = (Received cues id (Yes who)) } ,Cmd.none)
        _ -> doNothing model
    JumpTo position ->
      case model.state of
        Received cues id (Yes who)  -> ({model | state = (Received cues id No), current_position = position } ,Cmd.none) 
        Received cues id No         -> ({model | current_position = position }          ,send_to_yt_API <| String.concat ["Seek:", String.fromFloat <| position])
        _ -> doNothing model
    Player_Time_At second ->
      case model.state of
        Received _ _ _              -> ({model | current_position = second }            ,Cmd.none)
        _ -> doNothing model
    Validate_And_Fetch ->
      case model.state of
        Loading_Cues id -> doNothing model
        Loading_YT   id -> doNothing model
        _                           -> 
          case validate_id model.input_field of
            Ok video_id             -> ({model | state = (Loading_YT video_id) }        ,send_to_yt_API <| ("ID:"++video_id))
            Err _                   -> ({model | state = ID_Invalid }                   ,Cmd.none) 
    Cue_Changed who new ->
      case model.state of
        Received cues id (Yes _)    -> ({model | state = (Received (List.indexedMap (update_if new who) cues) id (Yes who))}  ,Cmd.none)
        _ -> doNothing model
    ID_Changed new ->
      case model.state of
        Received cues id (Yes who)  -> ({model | state = (Received cues id No) }        ,Cmd.none)
        _                           -> ({model | input_field = new }                    ,Cmd.none)

view : Model -> Html Msg
view model =
  case model.state of
    Fresh                 -> (lazy (unloaded_elements model.input_field) "")
    XML_Invalid           -> (lazy (unloaded_elements model.input_field) "Sorry, couldn't parse transcript!")
    ID_Invalid            -> (lazy (unloaded_elements model.input_field) "The link/ID is unrecognisable.")
    Load_Failed           -> (lazy (unloaded_elements model.input_field) "Sorry, couldn't find transcript!")
    Loading_YT   video_id -> (lazy (unloaded_elements model.input_field) "Loading...")
    Loading_Cues video_id -> (lazy (unloaded_elements model.input_field) "Loading...")
    Received cues video_id edit -> 
      div 
        [ A.style "width" "100%" ] 
        [ lazy (unloaded_elements model.input_field) ("Loaded: "++ video_id)
        , lazy (div 
          ([ A.id "cues-container" ] ++ common_style_text ++ common_style_container ) )
          (List.indexedMap (generate_html_from_cue model.current_position edit) cues)
        ]
subscriptions : Model -> Sub Msg
subscriptions _ = receive_msg_from_API loaded_or_position

-- Helper --
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

fetch_transcript : String -> Cmd Msg
fetch_transcript video_id = 
  Http.get  { url = "https://video.google.com/timedtext?v=" ++ video_id ++ "&lang=en", expect = Http.expectString GotCues }

unloaded_elements : String -> String -> Html Msg
unloaded_elements input_field msg = 
  div 
  [ A.style "width" "100%"
  , A.style "font-family" "Helvetica"
  , A.style "display" "flex"
  , A.style "flex-wrap" "wrap"
  , A.style "align-items" "center"
  , A.style "align-content" "flex-start"
  ] 
  [ tooltip_button
  , (url_field input_field) -- ID input
  , fetch_button            -- Fetch Subtitles
  , (status_message msg)    -- Whether it's loaded or an Error has occurred
  ]
common_style_top : (List (Html.Attribute Msg))
common_style_top = 
  [ A.style "margin" "2px"
  , A.style "font-size" "1.06em"
  , A.style "padding" "2px 2px 2px 2px"
  ]
common_style_text : (List (Html.Attribute Msg))
common_style_text =
  [ A.style "font-size" "1.06em"
  , A.style "color" "#000"
  , A.style "line-height" "1.4"
  ]
common_style_container : (List (Html.Attribute Msg))
common_style_container =
  [ A.style "overflow-y" "auto"
  , A.style "height" "40vh"
  , A.style "margin" "2px"
  , A.style "border" "1px solid black"
  , A.style "padding" "4px 4px 4px 12px"
  ]
tooltip_button : Html Msg
tooltip_button = a (common_style_top++
  [ A.style "font-family" "Lucida Bright, Georgia, serif"
  , A.style "font-size" "1.2em"
  , A.style "font-weight" "700"
  , A.style "text-align" "center"
  , A.style "display" "inline-block"
  , A.style "width" "28px"
  , A.style "height" "28px"
  , A.style "border" "2px solid black"
  , A.alt "Enter a YouTube link or ID and press the \"Fetch\" button. Then click on any word in the transcript box to get transported to the point in the video where that word is said. Watching the video will do the same in reverse. You can edit the text by double-clicking any word as well."
  ]) [text " i "]
url_field : String -> Html Msg
url_field id = input (
  [ A.placeholder "Please provide a YouTube link/ID."
  , A.value id
  , onInput ID_Changed
  , A.id "input-video-url-or-id"
  , A.style "min-width" "27ch"
  ]++common_style_top) []

fetch_button : Html Msg
fetch_button = button (
  [ onClick Validate_And_Fetch
  , A.id "button-fetches-transcript-by-id"

  ]++common_style_top) [ text "Fetch transcript" ]

status_message : String -> Html Msg
status_message message = span 
  ([]++common_style_top ) [text message]

generate_html_from_cue : Float -> Editing -> Int -> Cue -> Html Msg 
generate_html_from_cue time_sec whether_editing index cue = 
  case whether_editing of
    No         ->                         (create_cue_span index cue (cue.start <= time_sec && time_sec < cue.start+cue.duration))
    Yes number -> if index == number then (create_editable_cue index cue)
                                     else (create_cue_span index cue False)

create_cue_span : Int -> Cue -> Bool -> Html Msg
create_cue_span index cue highlight = 
  span 
    [ onClick (JumpTo cue.start)
    , onDoubleClick (Summon_Editor index)
    , A.id ("cue-"++ (String.fromInt index))
    , A.class "cue"
    , (A.style "background-color" ((\h -> if h then "#8CF" else "#FFF") highlight))
    ] [ text cue.content ]
    
create_editable_cue : Int -> Cue -> Html Msg
create_editable_cue index cue =
  input (
    [ onInput (Cue_Changed index)
    , onDoubleClick (JumpTo cue.start)
    , A.id ("cue-input-"++ (String.fromInt index))
    , A.class "cue-input"
    , A.value cue.content
    , A.style "min-width" (String.fromInt (round((toFloat(String.length cue.content))*0.8))++"ch")
    ]++common_style_text) []

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
    cue::xs ->
      case cue of
        Text text ->
          corral_cues xs
        Element text attr content ->
          [extract_information cue] ++ corral_cues xs

empty_cue : Cue
empty_cue = {start=0.0, duration=0.0, content=">> "}

toFloat_with_default : String -> Float -> Float
toFloat_with_default string def =
  case String.toFloat string of
    Nothing    -> def
    Just value -> value
  
extract_information : Node -> Cue
extract_information node =
  case node of
    Element _ ( start_attr :: duration_attr :: []) (( Text cue_text) :: []) ->
      { start    = (toFloat_with_default start_attr.value 0.0)
      , duration = (toFloat_with_default duration_attr.value 0.0)
      , content  = ((String.replace "&#39;" "'" cue_text) ++ " ")
      }
    _ -> empty_cue
