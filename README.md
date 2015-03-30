# transparent

another templating engine

In progress.



### t-replace


    # json

    { "replace_word": "WORLD" }

    # template

    <p>Hello <span t-replace="replace_word">world</span></p>

    # output 

    <p>Hello <span t-replace="replace_word">world</span></p>

