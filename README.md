# transparent

another templating engine

In progress.

    showDirective = "t-show"
    hideDirective = "t-hide"
    repeatDirective = "t-repeat"
    insertDirective = "t-insert"
    replaceDirective = "t-replace"
    classDirective = "t-class"
    exampleDirective = "t-example"

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

    <li t-example>
      <p>Name: <strong> Spiderman</strong></p>
      <p>Species: <em>Fish</em></p>
      <p>Injuries: Lots of hurt</p>
    </li>

would be removed.

`t-example` lets you add HTML elements for mockup purposes and have
them transparently stripped during rendering.


### dynamic ids

    <li t-repeat="person in party" id="person-{{person.name}}">
      <p>Name: <strong t-insert="person.name">Some body</strong></p>
    </li>

generates 

    <li class="dwarf" id="person-Thorin">
      <p>Name: <strong>Thorin</strong></p>
    </li>
    <li class="dwarf" id="person-Fili">
      <p>Name: <strong>Fili</strong></p>
    </li>
    <li class="hobbit" id="person-Bilbo">
      <p>Name: <strong>Bilbo</strong></p>
    </li>

### Merging templates


layout.html
```html
<html>
  <head>
    <title>Layout example</title>
  </head>
  <body>
    <h1>Layout</h1>
    <yield/>
  </body>
</html>
```

content.html
```html
<html>
  <head>
    <title>Content example</title>
  </head>
  <body>
    <h1>Content layout</h1>
    <div id="content">
      <p>This is content</p>
    </div>
  </body>
</html>
```

$ dist merge-templates layout.html content.html  

```html
<html>
  <head>
    <title>Layout example</title>
  </head>
  <body>
    <h1>Layout</h1>
    <div id="content">
      <p>This is content</p>
    </div>
  </body>
</html>
```
