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
  function startupElm(){
    try{
      startElm();
    }catch(e){
      console.log("Loading")
      setTimeout(startupELm, 500);
    }
  }
  startupElm()
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
