Jsm is a tool for simple code management, unlike npm or bower, it's designed to be very lightweight. see [doc](/doc/en), [文档](/doc/cn) and [API](/doc/api). 

The source code is on github: [jsm-client](https://github.com/winterland1989/jsm-client.git), [server](https://github.com/winterland1989/jsmServer.git), got a minute?:

```bash
# install jsm using npm
npm install -g https://github.com/winterland1989/jsm-client.git

# edit some script, require snippet you wanna use
# for example: _ = require('./jsm/base/lodash3')
vim index.js

# use jsm download snippet for index.js file, and start a testing server.
jsm install index.js
jsm server index.js

```
