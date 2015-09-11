m = require './jsm/base/mithril'
s = require './jsm/base/mss'

fireSearch = (e) ->
    if (e.keyCode == 13)
        window.location = '/search?sort=mtime&page=0&keywords=' + e.target.value

search = view: ->
    m '.Search',
        m 'span.SearchLabel', 'search:'
        m '.SimpleAutoComplete',
            m 'input',
                onkeyup: fireSearch
            m '.Suggetion'

s.tag
    '#topBar': s.PosRel() s.LineSize('48px', '18px') s.ClearFix$
        width: '100%'
        background: '#000'
        color: '#fff'
        minWidth: '960px'

        Search:
            float: 'left'
            SearchLabel:
                margin: '0 4px'

            SimpleAutoComplete:
                display: 'inline-block'
                span_input:
                    margin: '0 4px'
                input: s.LineSize('48px', '18px')
                    width: '480px'
                    border: 'none'
                    background: '#555'
                    color: '#fff'

    '#indexLink':
        float: 'left'
        color: '#f48'
        padding:'0 16px'
        textDecoration: 'none'

    '#userinfo':
        float: 'right'
        paddingRight: '4px'
        a_span:
            margin: '0 4px'
            color: '#fff'
            textDecoration: 'none'


m.mount document.getElementById('search'), search
