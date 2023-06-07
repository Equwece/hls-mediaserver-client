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

export function getAuthFormData() {
  let usernameInput = document.querySelector("#usernameInput").value;
  let passwordInput = document.querySelector("#passwordInput").value;
  return {usernameInput, passwordInput};
}

export function saveJwtPair(jwtPair) {
  return function () {
    return saveJwtPairJS(jwtPair);
  }
}

export function saveJwtPairJS(jwtPair) {
  localStorage.setItem('access', jwtPair.access);
  localStorage.setItem('refresh', jwtPair.refresh);
}
