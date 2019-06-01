import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

Elm.Main.init({
  node: document.getElementById('root')
});

registerServiceWorker();

window.addEventListener("keydown", event => {
  if(event.key.includes("Arrow")){
    event.preventDefault()
  }
})
