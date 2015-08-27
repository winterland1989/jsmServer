s = require "./jsm/base/mss"
theme = require "./theme"

mss = ->
    html_body:
        fontSize: '14px'
    body:
        width: '480px'
        margin: '0 auto'

    p:
        fontSize: '1.4em'
        textAlign: 'center'
        padding: '20px 0'

    '#loginForm_#registerForm':
        textAlign: 'left'
        input_label_textArea:
            display: 'block'
            width: '100%'
            margin: '8px'
            border: 'none'

        input_textArea:
            borderBottom: '1px solid #ddd'

        textArea:
            height: '240px'

        SubmitBtn:
            color: '#fff'
            background: theme.mainColor
            border: 'none'
            padding: '4px 0'



s.mount mss
