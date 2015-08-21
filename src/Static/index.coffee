m = require "./jsm/base/mithril"
s = require "./jsm/base/mss"

topBarColor = '#6cc'

SimpleAutoComplete = require './SimpleAutoComplete'

autoComplete = new SimpleAutoComplete
    lineHeight: '1.5em'
    fontSize: '1em'
    width: '480px'
    fontColor: '#fff'
    bgColor: topBarColor

topBar = view: -> [
    m '.AutoCompleteWrapper',
        m 'span.SearchLabel', 'search:'
        autoComplete.view()
]

mss = ->
    html_body:
        fontSize: '14px'
    '#topBar': s.PosRel() s.ClearFix$
        width: '100%'
        background: topBarColor
        padding: '8px 0'
        '': s.merge(
            autoComplete.mss()
        )
        color: '#fff'
        SearchLabel:
            lineHeight: '1.5em'
            fontSize: '1em'
            margin: '0 10px'
    '#introduction':
        margin: '20px'
        h1:
            fontSize: '2em'
        p:
            fontSize: '1em'
    '#latestList':
        margin: '20px'
        ul:
            margin: '20px 0'
        span:
            margin: '0 4px'

    AutoCompleteWrapper:
        float: 'left'

    "#userinfo":
        float: 'right'
        paddingRight: '4px'
        a: s.LineSize('1.5em', '1em')
            color: '#fff'
            textDecoration: 'none'


s.mount mss
m.mount (document.getElementById 'searchDiv'), topBar




