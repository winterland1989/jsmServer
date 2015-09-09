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
    html_body:
        fontSize: '14px'
    h1:
        textAlign: 'center'
    '#latestList_#introduction':
        width: '800px'
        margin: '0 auto'
    '#introduction':
        marginTop: '20px'
        h1:
            fontSize: '2em'
        p:
            fontSize: '1em'
    '#latestList':
        marginTop: '20px'
        ul:
            margin: '20px 0'
            padding: 0
            listStyleType: 'none'

        li:
            CodeInfo: s.LineSize('2em', '1em')
                textAlign: 'center'
                background: '#000'
                color: '#fff'
                a:
                    textDecoration: 'none'
                    padding: '4px'
                    color: 'red'

            CodePreview:
                height: '150px'


        span:
            margin: '0 4px'
