# transparent

another templating engine

In progress.

    showDirective = "tr-show"
    hideDirective = "tr-hide"
    repeatDirective = "tr-repeat"
    insertDirective = "tr-insert"
    replaceDirective = "tr-replace"
    classDirective = "tr-class"
    exampleDirective = "tr-example"

input.html

input.json


### t-replace


    # json

    { "replace_word": "WORLD" }

    # template

    <p>Hello <span t-replace="replace_word">world</span></p>

    # output 

    <p>Hello WORLD</p>


### t-example

This directive strips example HTML from the rendered output:

E.g., this HTML element 

    <li tr-example>
      <p>Name: <strong> Spiderman</strong></p>
      <p>Species: <em>Fish</em></p>
      <p>Injuries: Lots of hurt</p>
    </li>

would be removed.

`t-example` lets you add HTML elements for mockup purposes and have
them transparently stripped during rendering.


### dynamic ids

    <li tr-repeat="person in party" id="id-{{person.name}}">
      <p>Name: <strong tr-insert="person.name">Some body</strong></p>
    </li>

generates 

    <li class="dwarf" id="id-Thorin">
      <p>Name: <strong>Thorin</strong></p>
    </li>
    <li class="dwarf" id="id-Fili">
      <p>Name: <strong>Fili</strong></p>
    </li>
    <li class="hobbit" id="id-Bilbo">
      <p>Name: <strong>Bilbo</strong></p>
    </li>
  
