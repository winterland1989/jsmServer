require './topBar'
m = require './jsm/base/mithril'

editor = ace.edit 'editor'
editor.setTheme 'ace/theme/tomorrow'
editor.getSession().setMode 'ace/mode/javascript'


commentInputDom = document.getElementById 'commentInput'
sid = commentInputDom.dataset.sid

commentLoading = true
commentArray = []

if sid?
    m.request
        url: '/comment'
        method: 'GET'
        background: true
        data:
            sid: sid
            page: 0
            perPage: 10

comment = view: ->
    if commentLoading or !sid?
        m 'p', 'loading comment...'
    else
        m 'ul.CommentList',
            for cmt in commentArray
                m 'li',
                    m '.User', cmt.name
                    m 'content', cmt

m.mount (document.getElementById 'comment'), comment

commentText = ''
onTextAreaChange = (e) -> commentText = e.target.value
submitComment = (e) ->
    data = new FormData()
    data.append('sid', sid)
    data.append('content', commentText)
    m.request
        url: '/comment'
        method: 'POST'
        background: true
        serialize: (data) -> data
        data: data

commentForm = view: ->
    [
        m 'p', 'Discuss'
        m 'textarea.CommentInput',
            onchange: onTextAreaChange
        m 'input.CommentBtn',
            type: 'submit'
            onclick: submitComment
    ]

commentFormDom = document.getElementById 'commentForm'
if commentFormDom
    m.mount commentFormDom, commentForm

s = require './jsm/base/mss'
s.tag
    html_body: s.Size('100%', '100%') s.PosRel(0,0)
        fontSize: '14px'

    '#editor': s.PosAbs('48px', '400px', 0, 0) {}

    '#sideBar': s.PosAbs('40px', 0, 0)
        width: '400px'
        padding: 0
        border: 'none'
        overflowY: 'scroll'

    '#snippetInfo_#commentInput_#comment':
        padding: '16px'
        borderLeft: '1px dashed #000'

    '#commentInput_#comment':
        borderTop: '1px dashed #000'

    '#snippetInfo':
        p:
            margin: '4px'
            fontSize: '1em'
            color: '#999'

    '#commentInput':
        p_a:
            margin: '4px'
            fontSize: '1em'
            color: '#333'

        a:
            display: 'inline-block'
            textDecoration: 'none'

        CommentInput: s.Size('100%', '40px')
            margin: '4px 0'
            fontSize: '1em'
            border: 'none'
            borderBottom: '1px solid #000'
            padding: '4px 0'

        CommentBtn: s.Size('100%', '24px')
            margin: '4px 0'
            background: '#000'
            color: '#fff'
            border: 'none'

    '#comment':
        ul: {}
