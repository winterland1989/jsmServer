Javascript Snippet Manager使用手册
=================================

Jsm是一个简单粗暴的前端代码管理工具，它可以帮助你快速安装，发布长度在几百行左右的CommonJs代码片段，除了支持Javascript, jsm还支持coffeescirpt, livescript和jsx, 在线阅读器可以直接编译方便阅读(babel赞不支持)，欢迎使用和反馈:)

Email:[handong05@meituan.com](mailto:handong05@meituan.com)

jsm i | install
---------------

jsm-client需要机器上有node环境，使用npm安装：

    npm install -g https://github.com/winterland1989/jsm-client.git

新建你的入口文件，然后去`require`你需要的代码片段，例如添加`lodash`的依赖，在你代码开头添加：

    require('./jsm/base/lodash3')

假如你的入口文件是index.js，运行如下命令安装依赖：

    jsm install index.js

安装完成之后的目录结构应该如下：

    ├── index.js
    ├── jsm
    │   ├── base
    │   │   ├── lodash3.js

就是这么简单，不要`bower.json`之类的配置，OK，现在问题来了

+ 如何管理依赖版本？如何管理namespace？

require的时候写死作者名和大版本，但从来不要写文件后缀，例如：

    require('./jsm/base/lodash3')
    require('./jsm/base/zepto1')
    require('./jsm/winter/mss1')
    
会分别从base账户下找到lodash3和zepto1，winter账户下找到mss1，然后安装到对应文件夹。

+ 如果有多个入口文件怎么办？

只需

    jsm install A.js B.js C.coffee ...

+ 不是入口文件直接依赖的snippet会被安装么？

当然！假如`index.coffee`require了`./widget/Dialog.coffee`，然后`./widget/Dialog.coffee`里require了`./jsm/base/lodash3`，那么运行`jsm install index.js`会产生如下目录结构：   

    ├── index.coffee
    ├── jsm
    ├── widget
    │   ├── Dialog.coffee
    │   │   ├── jsm
    │   │   │   ├── lodash3.js
    
一般来说我们不希望有多个层级的`jsm`目录被创建，改变`./widget/Dialog.coffee`中require的路径即可：

    require('../jsm/base/lodash3')
    
注意`Dialog.coffee`相对于`jsm`文件夹的位置`../jsm`。

+ 路径中的jsm可以改么？

`jsm`是识别使用`jsm`管理的代码的关键词，不能改，为了和之后的打包流程一致，snippet都会被安装到`jsm`文件夹下。

jsm p | publish
---------------

运行如下命令把代码发布到jsm上：

    jsm publish ./.../.../userName/snippetNameVersion.js
    
`jsm`会试着根据路径解析`userName`和`Version`，然后跟着提示走即可。

请检查你的snippet符合一下的要求，以保证别人安装了之后可以顺利使用：

+ 只包含jsm上已经存在的snippet

+ 正确require别的snippet

一般说来你的snippet可能会在这样的一个目录下：

    ├── index.coffee
    ├── jsm
    │   ├── base
    │   │   ├── jquery2.js
    │   ├── myName
    │   │   ├── myModule1.js
    │   │   ├── myModule2.js
    
假设`myModule1.js`是你想发布的代码，你希望在代码中使用`jquery2.js`和`myModule2`，那么这样引用：

    var $ = require('../base/jquery2')
    var myModule2$ = require('../myName/myModule2')
    
+ 请添加合适的版本号（不确定请使用`0`，见下文），如果API改变导致snippet和之前版本不兼容，请务必增加版本号，例如`foo2 -> foo3`

+ 请在代码中提供必要的文档。

jsm u | update
--------------

提供一个或多个入口文件，`jsm`会自动更新所有依赖至最新的revision，并提示作者停止维护的版本。

jsm d | deprecate
-----------------

提供一个snippet文件路径，`jsm`会自动解析作者、标题和版本，然后把对应的snippet标记为停止维护(DEPRECATED)。

jsm s | server
--------------

提供一个入口文件和一个端口号（默认8080），`jsm`会自动启动一个测试服务器，提供一个包含打包好的入口文件的空HTML页面。同时使用35729端口实现自动刷新。

jsm w | webpack
---------------

提供一个或多个入口文件，`jsm`会自动产生一个简单的[webpack](http://webpack.github.io/)配置文件`webpack.config.js`：

    jsm i pageA.js pageB.js
    jsm w pageA.js pageB.js
    webpack

默认编译生成`pageA.bundle.js`、`pageB.bundle.js`。

规范和约定
==========

+ snippet命名请遵循如下规范(/^[a-zA-Z]+\d+$/):

```
    camelCase1 //单实例，函数...
    CamelCase2 // Class，工厂函数...
```

+ jsm中默认的版本号是`0`（即使发布时文件名称不包含版本），`0`版本可以用作快速迭代，允许API任意改动，所以当你依赖了一个版本号是`0`的snippet时，做好大幅修改的准备，因为作者需要时间完善它！

+ `jsm`默认解析snippet的名称用来作为搜索的关键词，例如
    
    imageLoader.js

会产生三个关键词`image`,`loader`和`imageloader`（搜索时关键词不区分大小写）,如果希望添加新的关键词，可以在代码的顶层区域中添加：

```js
//-jsm-keywords: async process ...
```

或者coffee/live中

```coffee
#-jsm-keywords: async process ...
```

`jsm publish`时会提示解析出的关键词。

+ 根据需求安装的snippet可以跟随项目的版本管理，也可以通过添加`.gitignore`忽略`jsm`文件夹，如果希望提供安装脚本，使用普通的shell脚本即可，例如：

```shell
!#/bin/bash

find ./main -maxdepth 1 -name '*.coffee' | xargs jsm u
```
