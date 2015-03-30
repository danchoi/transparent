# transparent

another templating engine

In progress.



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
