require './topBar'

m = require "./jsm/base/mithril"
s = require './jsm/base/mss'
aceMode = require './aceMode'

previews = document.getElementsByClassName 'CodePreview'

for preview in previews
    language = preview.dataset.language
    editor = ace.edit preview
    editor.setTheme 'ace/theme/tomorrow'
    editor.setShowFoldWidgets false
    editor.renderer.setShowGutter false
    editor.getSession().setMode aceMode[language]

s.tag
    h1:
        textAlign: 'center'
    '#latestList_#introduction':
        maxWidth: '960px'
        margin: '0 auto'
    '#latestList':
        ul:
            padding: 0
            listStyleType: 'none'

        li:
            CodeInfo: s.LineSize('32px', '1em')
                textAlign: 'center'
                background: '#000'
                color: '#fff'
                a:
                    textDecoration: 'none'
                    margin: '0 4px'
                    color: '#f48'

            CodePreview:
                height: '150px'


        span:
            margin: '0 4px'
