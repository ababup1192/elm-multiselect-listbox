import { Elm } from "./Main.elm";
import usStates from "./data/USStates";

const app = Elm.Main.init({
  node: document.getElementById("main"),
  flags: usStates
});
