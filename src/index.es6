import 'custom-scrollbar'
import { Elm } from '../dist/elm.js'
import { todoList } from './initialData.es6'

Elm.Main.init({
  node: document.getElementById('elm'),
  flags: JSON.stringify(todoList)
});
