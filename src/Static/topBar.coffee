m = require "./jsm/base/mithril"
s = require "./jsm/base/mss"

theme = require "./theme"

SimpleAutoComplete = require './SimpleAutoComplete'
autoComplete = new SimpleAutoComplete()

search = view: ->
    m '.AutoCompleteWrapper',
        m 'span.SearchLabel', 'search:'
        autoComplete.view()

s.tag do (lineHeight = '40px', fontSize = '1em', fontColor = '#fff', bgColor = theme.mainColor) ->
    '#topBar': s.PosRel() s.ClearFix$
        width: '100%'
        background: bgColor

        color: fontColor
        SearchLabel: s.LineSize(lineHeight, fontSize)
            margin: '0 4px'

        '': SimpleAutoComplete.mss(lineHeight, fontSize, '480px', fontColor, bgColor)

    "#userinfo":
        float: 'right'
        paddingRight: '4px'
        a: s.LineSize(lineHeight, fontSize)
            color: fontColor
            margin: '0 4px'
            textDecoration: 'none'

    AutoCompleteWrapper:
        float: 'left'

m.mount document.getElementById('search'), search
