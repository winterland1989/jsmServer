require './topBar'
theme = require "./theme"

m = require "./jsm/base/mithril"
s = require './jsm/base/mss'


s.tag
    html_body:
        fontSize: '14px'

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
