module.exports = {
    entry: {
        index: "./index.coffee",
        search: "./search.coffee",
        snippet: "./snippet.coffee",
        login: "./login.coffee",
        user: "./user.coffee"
    },
    output: {
        path: __dirname,
        filename: "[name].js"
    },
    module: {
        loaders: [
            { test: /\.css$/, loader: "style!css" },
            { test: /\.coffee$/, loader: "coffee-loader" },
            { test: /\.(coffee\.md|litcoffee)$/, loader: "coffee-loader?literate" }
        ]
    },
    resolve: {
        extensions: ["", ".coffee", ".js"]
    }
};
