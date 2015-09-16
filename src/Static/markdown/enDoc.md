Javascript Snippet Manager
==========================

Jsm is a very lightweight frontend code manager, it's particularily suitable to install and publish short code snippet. Beside Javascript, jsm online viewer support coffeescirpt and livescript with compile function.

[Jsm-client](https://github.com/winterland1989/jsm-client) is ready to use but some functions are still W.I.P, any issues are welcomed!

Email: [handong05@meituan.com](mailto:handong05@meituan.com).

jsm i | install
---------------

Jsm-client run on node.js，use npm to install to global：

    npm install -g https://github.com/winterland1989/jsm-client.git

Now create your entry file，require the snippet you need，for example if you need `lodash`：

    require('./jsm/base/lodash3')

Assuming your entry called `index.js`：

    jsm install index.js

You will find `lodash3` is installed to the right place and ready to be packed：

    ├── index.js
    ├── jsm
    │   ├── base
    │   │   ├── lodash3.js

+ Version and namespace management

Version and namespace are directly written into your code, never add file extensions:

    require('./jsm/base/lodash3')
    require('./jsm/base/zepto1')
    require('./jsm/winter/mss1')
    
Jsm will looking for `lodash3` and `zepto1` from user `base`，`mss1` from user `winter`, and place them accordingly.

+ What if there're more than one entry?

Just

    jsm install A.js B.js C.coffee ...

+ What about the files not directly require by entry?

All files that under `jsm` will be install, assuming `index.coffee` require `./widget/Dialog.coffee`, and `./widget/Dialog.coffee` require `./jsm/base/lodash3`, then after `jsm install index.js`, the tree should be like this:   

    ├── index.coffee
    ├── jsm
    ├── widget
    │   ├── Dialog.coffee
    │   │   ├── jsm
    │   │   │   ├── lodash3.js
    
Most of the time we don't need multiple `jsm` folders all over the place, so change the require line in `./widget/Dialog.coffee` into:

    require('../jsm/base/lodash3')
    
Note that we use `../jsm` to as relative path.

+ Can we change the name `jsm`

No, it's a keyword hard coded to recognize files under `jsm` management, all snippets will be install to a `jsm` folder.

jsm p | publish
---------------

Run following command to publish a snippet to jsm:

    jsm publish ./.../.../userName/snippetNameVersion.js
    
`jsm` will try to parse `userName` and `Version` for you, just follow the prompt.

Pleas make sure your snippet meet following requirements:

+ only require code already on jsm

+ require snippet path correctly

For example, myModule1.js is the snippet you want to share:

    ├── index.coffee
    ├── jsm
    │   ├── base
    │   │   ├── jquery2.js
    │   ├── myName
    │   │   ├── myModule1.js
    │   │   ├── myModule2.js
    
And you need to require `jquery2.js` and `myModule2`

    var $ = require('../base/jquery2')
    var myModule2 = require('../base/myModule2')
    
+ Add version number (use `0` to mark beta, see below), if any API lead a breaking change, increase version like `foo2 -> foo3`

+ Provide some document with snippet.

jsm u | update
--------------

Same as install, provide single or multiple entry files, `jsm` will update all snippet they required to latest revision, and warning about DEPRECATED snippets.

jsm d | deprecate
-----------------

Provide a snippet file path, `jsm` will try to parse author , title and version, and mark the snippet DEPRECATED.

jsm s | server
--------------

Provide an entry file and a port number(default = 8080), `jsm` will start a test server, serving an empty HTML page with bundled entry file. The server also use port 35729 to do live-reload.

jsm w | webpack
---------------

Provide your entry files, `jsm` will generate a minimium [webpack](http://webpack.github.io/) config file `webpack.config.js` with javascrit, coffeescirpt and livescript support.

    jsm i pageA.js pageB.js
    jsm w pageA.js pageB.js
    webpack

Will bundle `pageA.bundle.js`, `pageB.bundle.js`.

Conventions
===========

+ snippet's name need to be /^[a-zA-Z]+\d+$/

```
    camelCase1 //singleton, function...
    CamelCase2 // Class，Factory...
```

+ The default version is `0` even you don't add a version number to file , version `0` marked a testing snippet with possible API changes, so think before you require a version `0` snippet, ready to modify your code while the author are working on it.

+ `jsm` will parse filename to get keyword for searching:
    
    imageLoader.js

Will get `image`, `Loader` and `imageLoader`(searching is case-insensitive), if you want to add more, use:

```js
//-jsm-keywords: async process ...
```

or in coffee/live:

```coffee
#-jsm-keywords: async process ...
```

`jsm publish` will display all keywords it found.

+ You can add all snippets to you git(or other VCS), or ignore them by add `.gitignore`, if you want an automatic install command, write a shell script, for example:

```shell
!#/bin/bash

find ./main -maxdepth 1 -name '*.coffee' | xargs jsm u
```
