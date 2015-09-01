s = require "./jsm/base/mss"

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
            background: '#000'
            border: 'none'
            padding: '4px 0'



s.mount mss
