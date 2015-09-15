module.exports = {
    context: __dirname + "/src",
    entry: {
        index: "./index.coffee",
        search: "./search.coffee",
        snippet: "./snippet.coffee",
        login: "./login.coffee",
        user: "./user.coffee",
        doc: "./doc.coffee"
    },
    output: {
        path: __dirname + '/dist',
        filename: "[name].js"
    },
    module: {
        loaders: [
            { test: /\.coffee$/, loader: "coffee-loader" },
            { test: /\.(coffee\.md|litcoffee)$/, loader: "coffee-loader?literate" }
        ]
    },
    resolve: {
        extensions: ["", ".coffee", ".js"]
    }
};
