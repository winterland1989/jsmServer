m = require './jsm/base/mithril'
s = require './jsm/base/mss'

class SimpleAutoComplete
    constructor: (
    ) ->

    view: ->
        m '.SimpleAutoComplete',
            m 'input'
            m '.Suggetion'

SimpleAutoComplete.mss = (lineHeight, fontSize, width, fontColor, bgColor)->
    SimpleAutoComplete:  s.LineSize(lineHeight, fontSize)
        display: 'inline-block'
        span_input:
            margin: '0 4px'
        input:
            width: width
            border: 'none'
            borderBottom: '1px solid #fff'
            background: bgColor
            color: fontColor

autoComplete = new SimpleAutoComplete()

search = view: ->
    m '.AutoCompleteWrapper',
        m 'span.SearchLabel', 'search:'
        autoComplete.view()

s.tag do (lineHeight = '48px', fontSize = '1em', fontColor = '#fff', bgColor = '#000') ->
    '#topBar': s.PosRel() s.ClearFix$
        width: '100%'
        background: bgColor

        color: fontColor
        SearchLabel: s.LineSize(lineHeight, fontSize)
            margin: '0 4px'

        '': SimpleAutoComplete.mss(lineHeight, fontSize, '480px', fontColor, bgColor)

    '#indexLink': s.LineSize('48px', '18px')
        float: 'left'
        color: 'red'
        padding:'0 16px'
        textDecoration: 'none'

    '#userinfo':
        float: 'right'
        paddingRight: '4px'
        a: s.LineSize(lineHeight, fontSize)
            color: fontColor
            margin: '0 4px'
            textDecoration: 'none'

    AutoCompleteWrapper:
        float: 'left'

m.mount document.getElementById('search'), search
