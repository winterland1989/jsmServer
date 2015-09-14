m = require './jsm/base/mithril'
s = require './jsm/base/mss'


currentHref = window.location.href
searchParams = m.route.parseQueryString(
        decodeURIComponent(currentHref.substr(currentHref.indexOf('?') + 1))
    )

selectedIndex = 0
inputContent = if searchParams.keywords? then searchParams.keywords else ''
debounceTimerId = undefined
debounceTime = 200
onInputKeyDown = (e) ->
    switch e.keyCode || e.which
        when 9 # tab key
            if completeList.length
                if e.shiftKey == true
                    selectedIndex-- if selectedIndex > 0
                else
                    selectedIndex++ if selectedIndex < completeList.length
                    if selectedIndex == completeList.length
                        selectedIndex = 0

                inputContent = completeList[selectedIndex]
            false

        when 37, 38 # up, left arrow
            if completeList.length
                selectedIndex-- if selectedIndex >= 0
                if selectedIndex == -1
                    selectedIndex = completeList.length - 1
                inputContent = completeList[selectedIndex]
            false

        when 39, 40 # down, right arrow
            if completeList.length
                selectedIndex++ if selectedIndex < completeList.length
                if selectedIndex == completeList.length
                    selectedIndex = 0
                inputContent = completeList[selectedIndex]
            false

        when 13 # enter key
            if inputContent != ''
                window.location = '/search?sort=mtime&page=0&keywords=' + e.target.value
            false

        else
            true


onInputInput = (e) ->

    selectedIndex = 0
    inputContent = e.target.value

    if debounceTimerId
        clearTimeout debounceTimerId

    debounceTimerId = setTimeout(
            ->
                complete e.target
        ,   debounceTime
        )


completeList = []
complete = (inputDom) ->
    inputWords = inputContent.split ' '
    lastWord = inputWords.pop()
    inputPre = inputWords.join ' '
    if lastWord != ''
        m.request
            url: '/keywords?predict=' + lastWord
            method: 'GET'
            background: true
        .then (keywords) ->
            completeList = for word in keywords
                if inputPre == '' then word
                else inputPre + ' ' + word
            completeList.unshift inputContent
            m.redraw()
    else
        completeList = []
        m.redraw()


search = view: ->
    m '.Search',
        m 'span.SearchLabel', 'search:'
        m '.SimpleAutoComplete',
            m 'input',
                oninput: onInputInput
                onkeydown: onInputKeyDown
                value: inputContent
            m 'ul.Suggestion',
                for keyword, index in completeList
                    if index > 0
                        m 'li',
                            className: if index == selectedIndex then 'Selected' else ''
                        , keyword


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

            SimpleAutoComplete: s.PosRel(0, 0)
                zIndex: 999
                display: 'inline-block'
                span_input:
                    margin: '0 4px'
                input: s.LineSize('48px', '18px')
                    width: '480px'
                    margin: 0
                    padding: '0 8px'
                    border: 'none'
                    background: '#555'
                    color: '#fff'
                Suggestion: s.PosAbs('48px', 0)
                    li: s.LineSize('48px', '18px')
                        width: '480px'
                        padding: '0 8px'
                        background: '#555'
                    Selected:
                        background: '#000'


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
