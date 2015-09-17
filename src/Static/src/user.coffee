require './topBar'

s = require './jsm/base/mss'

s.tag
    '#userDesc_#userList':
        maxWidth: '960px'
        margin: '0 auto'

    '#userList':
        CodeInfo:
            a_span: s.LineSize('2em', '1em')
                margin: '0 4px'
                textDecoration: 'none'

            Deprecated: color: '#b48'

            a: s.TextEllip$
                color: '#b48'
                width: '100px'
