<!--

author: Alexander Buhl
version: 2.0.0

comment: Both an embedded youtube player and a transcript download and edit tool designed to assist presentation teaching in university.

script: https://cdn.jsdelivr.net/gh/Buhlean/LiaTranscriptLiveEdit/src/Module.js
script: https://cdn.jsdelivr.net/gh/Buhlean/LiaTranscriptLiveEdit/src/ElmModule.js
import: https://cdn.jsdelivr.net/gh/Buhlean/LiaTranscriptLiveEdit/src/style.css

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

[![LiaScript](https://raw.githubusercontent.com/LiaScript/LiaScript/master/badges/course.svg)](https://liascript.github.io/course/?https://raw.githubusercontent.com/Buhlean/LiaTranscriptLiveEdit/master/Module.md)

### Module TranscriptLiveEdit

**Example IDs:**
* 5p-SsuTm130
* 6Af6b_wyiwI
* I7jf_U89ddk

## Try it!

@RunTranscriptLE
