import Hls from "hls.js";

export function initHls(videoSrc) {
  return function () {
    return initHlsJS(videoSrc)    
  }
}

function initHlsJS(apiVideoPath) {
  const video = document.getElementById('video');
  const videoSrc = `http://${location.host}/${apiVideoPath}`
  if (Hls.isSupported()) {
    let hls = new Hls();
    hls.loadSource(videoSrc);
    hls.attachMedia(video);
    video.play();
  }
  else if (video.canPlayType('application/vnd.apple.mpegurl')) {
    video.src = videoSrc;
  }
}

