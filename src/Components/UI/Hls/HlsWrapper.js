import Hls from "hls.js";

export function initHls(videoSrc) {
  return function () {
    return initHlsJS(videoSrc)    
  }
}

function initHlsJS(apiVideoPath) {
  const video = document.getElementById('video');
  const videoSrc = `http://${location.host}/${apiVideoPath}`
  const accessToken = localStorage.getItem('access')
  if (accessToken) {
    const hlsConfig = {
      xhrSetup: function(xhr, url) {
        xhr.setRequestHeader('Authorization', `Bearer ${accessToken}`)
      }
    }
    if (Hls.isSupported()) {
      let hls = new Hls(hlsConfig);
      hls.loadSource(videoSrc);
      hls.attachMedia(video);
      video.play();
    }
    else if (video.canPlayType('application/vnd.apple.mpegurl')) {
      video.src = videoSrc;
    }
  }
}

