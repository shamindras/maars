hook_output = knitr::knit_hooks$get('output')
knitr::knit_hooks$set(output = function(x, options) {
    # this hook is used only when the linewidth option is not NULL
    if (!is.null(n <- options$linewidth)) {
        x = knitr:::split_lines(x)
        # any lines wider than n should be wrapped
        if (any(nchar(x) > n)) x = strwrap(x, width = n)
        x = paste(x, collapse = '\n')
    }
    hook_output(x, options)
})
