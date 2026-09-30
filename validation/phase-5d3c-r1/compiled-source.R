(function(target) {
    root<-normalizePath(if(file.exists('R/utils.R')) '.' else '../..')
    previous<-setwd(root);on.exit(setwd(previous))
    sys.source('R/compiled_runtime.R',envir=target)
    stopifnot(!is.null(target$cgv_compiled_expressions('R/utils.R')))
    target$cgv_source_runtime('R/utils.R',target)
})(environment())
