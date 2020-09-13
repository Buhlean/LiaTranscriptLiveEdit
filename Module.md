<!--

author: Alexander Buhl
version: 2.0.0

comment: Both an embedded youtube player and a transcript download and edit tool designed to assist presentation teaching in university.

script: https://cdn.jsdelivr.net/gh/Buhlean/LiaTranscriptLiveEdit/src/Module.js
script: https://cdn.jsdelivr.net/gh/Buhlean/LiaTranscriptLiveEdit/src/ElmModule.js

@RunTranscriptLE
<div id="player"></div>
<div id="ElmHook"></div>
<script>
  function startElm(){
    console.log("ElmStart");
    app = Elm.Main.init({
      node: document.getElementById('ElmHook')
    });
    console.log("AfterElmStart");
    app.ports.send_to_yt_API.subscribe(function(message){
      console.log("GotMessage! "+message);
      if (message.indexOf('ID:') !== -1) {
        if (message.indexOf('default') !== -1) {
          create_and_change_player('6Af6b_wyiwI'); }
        else { create_and_change_player(message.slice(3)); } }
      else if (message.indexOf('Seek:') !== -1) {
        console.log(message);
        console.log(player);
        try{
          if (player.getPlayerState() !== -1) {seekTo(parseInt(message.slice(5))); }
        } catch(e) {
          console.log("SeekTo Exception"+e);
        } }
      else if (message.indexOf('Play:') !== -1) {
        playVideo(); }
      else if (message.indexOf('Pause:') !== -1) {
        pauseVideo(); }
    });
    console.log("Subscribed to port!");
  }
  startELm();
</script>
@end

-->

### Module TranscriptLiveEdit

**Rendered version here:**<br/>
https://liascript.github.io/course/?https://raw.githubusercontent.com/Buhlean/LiaTranscriptLiveEdit/master/Module.md

**Example IDs:**
* 5p-SsuTm130
* 6Af6b_wyiwI
* I7jf_U89ddk

## Try it!

@RunTranscriptLE
