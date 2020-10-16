
var loadedAPI = false;
var loadedPlayer = false;
var video_id = "";
var app;

function startElm(){
  console.log("DEBUG: ElmStart");
  let storedState = localStorage.getItem('lia-transcript-live-editor');
  console.log("DEBUG: type: " + typeof(storedState));
  console.log("DEBUG: stored: " + storedState);
  if (storedState == null) {
    app = Elm.Main.init({ flags: "",          node: document.getElementById('ElmHook') });
  } else {
    app = Elm.Main.init({ flags: storedState, node: document.getElementById('ElmHook') });
  }
  app.ports.setStorage.subscribe(function(state) {
      localStorage.setItem('lia-transcript-live-editor', state);
  });
  console.log("DEBUG: AfterElmStart");
  app.ports.send_to_yt_API.subscribe(function(message){
    console.log("DEBUG: GotMessage! "+message);
    if (message.indexOf('ID:') !== -1) {
     create_and_change_player(message.slice(3)); }
    else if (message.indexOf('Seek:') !== -1) {
      console.log(message);
      console.log(player);
      try{
        if (player.getPlayerState() !== -1) {seekTo(parseInt(message.slice(5))); }
      } catch(e) {
        console.log("DEBUG: SeekTo Exception"+e);
      } }
    else if (message.indexOf('Play:') !== -1) {
      playVideo(); }
    else if (message.indexOf('Pause:') !== -1) {
      pauseVideo(); }
  });
  console.log("DEBUG: Subscribed to port!");
}
function playVideo() {
  console.log("DEBUG: play");
  player.playVideo();
}
function pauseVideo() {
  console.log("DEBUG: pause");
  player.pauseVideo();
}
function stopVideo() {
  console.log("DEBUG: stop");
  player.stopVideo();
}
function seekTo(seconds) {
  console.log("DEBUG: seek" + seconds);
  player.seekTo(seconds, true);
}
function create_and_change_player(id) {
  console.log("DEBUG: create");
  loadedPlayer = false;
  if (typeof(id) === 'string') {
    console.log("DEBUG: type correct");
    video_id = id;
    if (!(loadedAPI)) {
      console.log("DEBUG: branch firsttime");
      var tag = document.createElement('script');
      tag.src = "https://www.youtube.com/iframe_api";
      var firstScriptTag = document.getElementsByTagName('script')[0];
      firstScriptTag.parentNode.insertBefore(tag, firstScriptTag);
    }
    else {
      console.log("DEBUG: branch change");
      create_player();
    }
  }
  else {
    console.log("DEBUG: type wrong");
  }
}
var player;
function onYouTubeIframeAPIReady() {
  console.log("DEBUG: apiready");
  loadedAPI = true;
  create_player();
}
function create_player() {
  player = new YT.Player('player',
    { height: '270'
    , width: '480'
    , videoId: video_id
    , playerVars: { 'origin': 'http://localhost:8000', 'enablejsapi':1 }
    , events:
      { 'onReady': onPlayerReady
      , 'onStateChange': stateChanged
      }
  });
  console.log("DEBUG: created");
  app.ports.receive_msg_from_API.send(-1);
  console.log("DEBUG: ELM notified");
}
function onPlayerReady(event) {
  console.log("DEBUG: playerready");
  event.target.setVolume(10);
  loadedPlayer = true;
}
var timerId;
function stateChanged(event){
  if(event.data == 1){
    timerId = setInterval(regularly_report_time, 800);
  }
  else{
    clearInterval(timerId);
  }
}
function regularly_report_time(){
    let second = player.getCurrentTime();
    console.log("DEBUG: time: "+second);
    app.ports.receive_msg_from_API.send(second);
}
