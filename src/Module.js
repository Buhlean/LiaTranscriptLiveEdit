
var loadedAPI = false;
var loadedPlayer = false;
var startedPlaying = false;
var video_id = "";
var app;


function playVideo() {
  console.log("play");
  player.playVideo();
}
function pauseVideo() {
  console.log("pause");
  player.pauseVideo();
}
function stopVideo() {
  console.log("stop");
  player.stopVideo();
}
function seekTo(seconds) {
  console.log("seek" + seconds);
  player.seekTo(seconds, true);
}
function create_and_change_player(id) {
  console.log("create");
  loadedPlayer = false;
  startedPlaying = false;
  if (typeof(id) === 'string') {
    console.log("type correct");
    video_id = id;
    if (!(loadedAPI)) {
      console.log("branch firsttime");
      var tag = document.createElement('script');
      tag.src = "https://www.youtube.com/iframe_api";
      var firstScriptTag = document.getElementsByTagName('script')[0];
      firstScriptTag.parentNode.insertBefore(tag, firstScriptTag);
    }
    else {
      console.log("branch change");
      create_player();
    }
  }
  else {
    console.log("type wrong");
  }
}
var player;
function onYouTubeIframeAPIReady() {
  console.log("apiready");
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
  console.log("created");
  app.ports.receive_msg_from_API.send(-1);
  console.log("ELM notified");
}
function onPlayerReady(event) {
  console.log("playerready");
  event.target.setVolume(10);
  loadedPlayer = true;
}
var timerId;
function stateChanged(event){
  if(event.data == 1){
    timerId = setInterval(regularly_report_time, 2000);
  }
  else{
    clearInterval(timerId);
  }
}
function regularly_report_time(){
    let second = Math.floor(player.getCurrentTime());
    console.log("time: "+second);
    app.ports.receive_msg_from_API.send(second);
}
