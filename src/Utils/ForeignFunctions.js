export function getPathName() {
  return getPathNameJS();
}

function getPathNameJS() {
  return window.location.pathname;
}

export function goToPathName(pathName) {
  return function () {
    return goToPathNameJS(pathName);
  }
}

function goToPathNameJS(pathName) {
  window.history.pushState({}, '', pathName);

}
