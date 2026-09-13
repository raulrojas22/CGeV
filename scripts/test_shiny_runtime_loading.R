#!/usr/bin/env Rscript
# Run in a fresh process from the app root, with APP_COMPILED_RUNTIME=0 and 1.
# Unlike testServer(), this preserves Shiny's actual server closure and bytecode.
app <- shiny::shinyAppDir(normalizePath('.'))
app$onStart()
server <- app$serverFuncSource()
server_env <- environment(server)
stopifnot(exists('lib_env', envir = .GlobalEnv, inherits = FALSE))
binding_names <- ls(lib_env, all.names = TRUE)
mismatches <- binding_names[!vapply(binding_names, function(name) {
    identical(get(name, envir = server_env), get(name, envir = lib_env))
}, logical(1))]
if (length(mismatches)) stop('Server shadows explicit runtime bindings: ', paste(mismatches, collapse = ', '))

compiled <- !tolower(Sys.getenv('APP_COMPILED_RUNTIME', '1')) %in% c('0', 'false', 'off') &&
    !is.null(cgv_compiled_expressions('server.R'))
if ('--require-compiled' %in% commandArgs(TRUE) && !compiled) {
    stop('Expected valid compiled runtime artifacts')
}
functions <- c('create_gene_plot', 'prepare_gene_plot_model', 'plotServerHomologous',
               'plotServerOrtologous', 'init_plot_lifecycle_domain')
if (compiled) {
    # Diagnostic only: R has no exported bytecode-body predicate.
    stopifnot(identical(typeof(.Internal(bodyCode(server))), 'bytecode'))
    for (name in functions) {
        stopifnot(identical(typeof(.Internal(bodyCode(get(name, server_env)))), 'bytecode'))
    }
}
stopifnot(is.function(app$httpHandler))
cat('PASS: actual Shiny server resolves all', length(binding_names),
    'explicit library bindings; runtime =', if (compiled) 'compiled' else 'source', '\n')
if (is.function(app$onStop)) app$onStop()
