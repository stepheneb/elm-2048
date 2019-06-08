import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

//  localStorage
const storageKey = "elm2048";

const getItem = () => JSON.parse(localStorage.getItem(storageKey));

const setItem = (data) => localStorage.setItem(storageKey, JSON.stringify(data));

const gameState = getItem();

var app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: gameState
});

app.ports.cacheData.subscribe(function(data) {
  setItem(data);
});

registerServiceWorker();

window.addEventListener("keydown", event => {
  if(event.key.includes("Arrow")){
    event.preventDefault()
  }
})
