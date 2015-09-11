Javascript Snippet Manager
==========================

Jsm是一个简单粗暴的前端代码管理工具，它可以帮助你快速安装，发布长度在几百行左右的CommonJs代码片段，Jsm支持的语言包括js, coffeescirpt和livescript, 在线的代码阅读器可以直接编译以方便阅读，Jsm的服务端和客户端还在持续改进，欢迎使用和反馈:(handong05@meituan.com)[mailto:handong05@meituan.com]。

使用jsm安装代码
---------------

jsm-client需要机器上有node环境，使用npm安装：

    npm install -g https://github.com/winterland1989/jsm-client.git

新建你的入口文件，然后去require你需要require的代码片段吧，例如添加lodash的依赖，在你代码开头添加：

    require('./jsm/base/lodash3')

假如你的入口文件是index.js，运行如下命令安装依赖：

    jsm install index.js

安装完成之后的目录结构应该如下：

    ├── index.js
    ├── jsm
    │   ├── base
    │   │   ├── lodash3.js

就是这么简单，不要bower.json之类的配置，OK，现在问题来了

+ 如何管理依赖版本？如何管理namespace？

require的时候写死作者名和大版本，例如：

    require('./jsm/base/lodash3')
    require('./jsm/base/zepto1')
    require('./jsm/winter/mss2')
    
会分别从base账户下找到lodash3和zepto1，winter账户下找到mss2，然后安装到对应文件夹。

+ 如果有多个入口文件怎么办？

```bash
jsm install A.js B.js C.coffee ...
```

+ 不是入口文件直接依赖的snippet会被安装么？

当然！假如`index.coffee`require了`./widget/Dialog.coffee`，然后`./widget/Dialog.coffee`里require了`./jsm/base/lodash3`，那么运行`jsm install index.js`会产生如下目录结构：   

    ├── index.coffee
    ├── jsm
    ├── widget
    │   ├── Dialog.coffee
    │   │   ├── jsm
    │   │   │   ├── lodash3.js
    
一般来说我们不希望有多个层级的`jsm`目录被创建，自需要改变`./widget/Dialog.coffee`中require的路径即可：

    require('../jsm/base/lodash3')
    
注意`Dialog.coffee`相对于`jsm`文件夹的位置`../jsm`。

+ 路径中的jsm是什么东西，可以改么？

是个识别用的关键词，不能改，正如上面提到的`jsm install`本身并不假定文件夹位置，而只是识别`jsm`关键字，为了和之后的打包流程一致，snippet都会被安装到`jsm`文件夹下。

使用jsm发布代码
---------------

现在假如你有一段代码希望和别人分享，awesome！运行如下命令把它发布到jsm上面吧：

    jsm publish ./.../.../userName/snippetNameVersion.js
    
`jsm`会试着根据路径解析`userName`和`Version`，不过有时你可能想换个userName发布（比如改了别人的代码之后想存到自己的名下），`jsm`会提示你输入新的用户名，然后输入你的用户密码，done！

虽然分享代码是一件很好的事情，但同时也意味着你要开始维护你的代码了，因此请先检查你的snippet符合一下的要求，以保证别人安装了之后可以顺利使用：

+ 只包含jsm上已经存在的snippet

如果你的snippet包含了还未发布的别的snippet，那么别人安装的时候是无法找到依赖的，好在发布的时候`jsm`会帮助你检查你的依赖都已经存在在`jsm`的服务器上了。

+ 正确require别的snippet

一般说来你的snippet可能会在这样的一个目录下：

    ├── index.coffee
    ├── jsm
    │   ├── base
    │   │   ├── jquery2.js
    │   ├── myName
    │   │   ├── myModule1.js
    
假设`myModule1.js`是你想发布的代码，你希望在代码中使用`jquery2.js`，那么这样引用：

    $ = require('../base/jquery2.js')
    
+ 请添加合适的版本号（不确定请使用`0`，见下文），如果API改变导致snippet和之前版本不兼容，请务必增加版本号，例如`foo2 -> foo3`

+ 请务必在代码中提供必要的文档！

规范和约定
==========

+ snippet命名请遵循如下规范，不合理的snippet名称会被服务器拒绝！

```
    camelCase1 //单实例，函数...
    
    CamelCase2 // Class，工厂函数...
```

+ jsm中默认的版本号是`0`（即使发布时文件名称不包含版本），`0`版本可以用作快速迭代，允许API任意改动，所以当你依赖了一个版本号是`0`的snippet时，做好大幅修改的准备，因为作者需要时间完善它！

+ `jsm`默认解析snippet的名称用来作为搜索的关键词，例如
    
    imageLoader.js

会产生三个关键词`image`,`Loader`和`imageLoader`（搜索时关键词不区分大小写）,如果希望添加新的关键词，可以在代码的顶层区域中添加：

```js
//-jsm-keywords: async process ...
```
或者coffee/live中
```coffee
#-jsm-keywords: async process ...
```
在`jsm publish`的时候会提示解析出的关键词。

待续
====

还有`update`和`deprecate`两个命令没实现，WIP...
