require './topBar'

m = require "./jsm/base/mithril"
aceMode = require './aceMode'

previews = document.getElementsByClassName 'CodePreview'

for preview in previews
    language = preview.dataset.language
    editor = ace.edit preview
    editor.setTheme 'ace/theme/tomorrow'
    editor.setShowFoldWidgets false
    editor.renderer.setShowGutter false
    editor.getSession().setMode aceMode[language]
