require './topBar'
require './snippetPreview.coffee'

m = require "./jsm/base/mithril"

currentHref = window.location.href
searchParams = m.route.parseQueryString(
        decodeURIComponent(currentHref.substr(currentHref.indexOf('?') + 1))
    )

searchDesc = view: -> [
        m 'span', 'Search for: ' + searchParams.keywords
    ]

m.mount (document.getElementById 'searchDesc'), searchDesc
