require './topBar'

s = require './jsm/base/mss'



s.tag
    body_html:
        fontSize: '14px'
    '#userDesc_#userList':
        width: '800px'
        margin: '0 auto'
    '#userList':
        a_span: s.LineSize('2em', '1em')
            margin: '0 4px'
            textDecoration: 'none'

        a: s.TextEllip$
            display: 'inline-block'
            color: 'red'
            width: '100px'
