require './topBar'
m = require './jsm/base/mithril'
s = require './jsm/base/mss'
aceMode = require './aceMode'

language = (document.getElementById 'editor').dataset.language
editor = ace.edit 'editor'
editor.setTheme 'ace/theme/tomorrow'

editor.getSession().setMode aceMode[language]
editor.setOptions({ readOnly: true })
editor.setKeyboardHandler('ace/keyboard/vim')

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
        editor.setValue(sourceCode, -1)
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

    '#editor': s.PosAbs('48px', '360px', 0, 0)
        borderRight: '1px dashed #000'

    '#sideBar': s.PosAbs('40px', 0, 0)
        width: '360px'
        padding: 0
        border: 'none'
        overflowY: 'scroll'

    '#snippetInfo_#snippetComment':
        padding: '16px'
        p: margin: '0'

    '#snippetInfo':
        top: 0
        width: '100%'
        SnippetDeprecated:
            margin: '0 4px'
            color: '#f48'
        CompileToggle: s.Size('100%', '32px')
            margin: '12px 0'
            padding: 0
            borderWidth: '1px'

    '#snippetComment':
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
            listStyle: 'none'

        CommentInfo: s.TextEllip$
            width: '100%'
