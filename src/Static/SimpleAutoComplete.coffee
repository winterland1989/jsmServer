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

module.exports = SimpleAutoComplete
