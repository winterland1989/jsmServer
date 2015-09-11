require './topBar'
m = require './jsm/base/mithril'
s = require './jsm/base/mss'
aceMode = require './aceMode'

language = (document.getElementById 'editor').dataset.language
editor = ace.edit 'editor'
editor.setTheme 'ace/theme/tomorrow'

editor.getSession().setMode aceMode[language]

controlPanelDom = document.createElement 'div'
(document.getElementById 'snippetInfo').appendChild controlPanelDom

compiled = false
sourceCode = editor.getValue()
toggleCompile = (e) ->
    unless compiled
        switch language
            when 'coffeescript'
                editor.setValue(CoffeeScript.compile(sourceCode, bare: true), -1)
                editor.getSession().setMode aceMode['javascript']
            when 'livescript'
                editor.setValue(LiveScript.compile(sourceCode, bare: true), -1)
                editor.getSession().setMode aceMode['javascript']
        compiled = true
    else
        editor.setValue(sourceCode)
        editor.getSession().setMode aceMode[language]
        compiled = false

controlPanel = view: ->
    if language != "javascript"
        m 'input.CompileToggle',
            type: 'submit'
            onclick: toggleCompile
            value: if compiled then 'Don\'t compile' else 'Compile to js'

m.mount controlPanelDom, controlPanel

s.tag
    html_body: s.Size('100%', '100%') s.PosRel(0,0) {}

    '#editor': s.PosAbs('48px', '400px', 0, 0) {}

    '#sideBar': s.PosAbs('40px', 0, 0)
        width: '400px'
        padding: 0
        border: 'none'
        overflowY: 'scroll'

    '#snippetInfo_#snippetComment': s.PosAbs()
        padding: '16px'
        borderLeft: '1px dashed #000'
        p: margin: '0'

    '#snippetInfo':
        top: 0
        width: '100%'
        height: '300px'
        CompileToggle: s.Size('100%', '32px')
            margin: '12px 0'
            padding: 0
            borderWidth: '1px'

    '#snippetComment':
        top: '300px'
        bottom: 0
        width: '100%'
        borderTop: '1px dashed #000'
        overflow: 'scroll'

        textArea: s.Size('100%', '96px') {}
        input: s.Size('100%', '32px')
            padding: 0
            borderWidth: '1px'

        ul:
            marginTop: '12px'
            padding: 0
        li:
            borderTop: '1px dashed #000'
            padding: '12px'

        CommentInfo: s.TextEllip$
            width: '100%'
