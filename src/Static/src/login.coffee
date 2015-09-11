s = require "./jsm/base/mss"

mss = ->
    body:
        width: '640px'
        margin: '0 auto'

    h1: textAlign: 'center'

    '#loginForm_#registerForm':
        textAlign: 'left'
        input_label_textArea:
            display: 'block'
            width: '100%'
            border: 'none'

        input_textArea:
            borderBottom: '1px solid #ddd'

        textArea:
            height: '240px'

        SubmitBtn:
            border: '1px solid #444'
            margin: '12px 0'



s.mount mss
