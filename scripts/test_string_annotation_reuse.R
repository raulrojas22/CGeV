#!/usr/bin/env Rscript
# Offline full-payload parity against the frozen pre-PR3 function, then actual
# Shiny observer scheduling/invalidation. Run from the repository root.
source('R/utils.R')
source('R/string_cache.R')
source('R/string_worker.R')
source('R/string_annotation.R')
Sys.setenv(APP_PERF_TIMING = '0', APP_DEBUG_LOGS = '0')
server_lines <- readLines('server.R')
first <- grep('    stringAnnotationState <- new_string_annotation_state()', server_lines, fixed = TRUE)
last <- grep('    build_string_network_widget_async <- function', server_lines, fixed = TRUE)
production <- parse(text = server_lines[first:(last - 1L)])
reference <- parse('tests/fixtures/string_query_payload_pre_pr3.R')

parser <- parse_gff_attributes
decoder <- safe_url_decode
counts <- c(parse = 0L, decode = 0L)
reset_counts <- function() counts[] <<- 0L
parse_gff_attributes <- function(attr) {
    counts[['parse']] <<- counts[['parse']] + 1L
    parser(attr)
}
safe_url_decode <- function(x) {
    counts[['decode']] <<- counts[['decode']] + 1L
    decoder(x)
}
frame <- function(attrs, types = c('gene', rep('exon', length(attrs) - 1L))) {
    data.frame(V1 = rep('chr1', length(attrs)), V3 = types, V4 = seq_along(attrs),
               V5 = seq_along(attrs) + 100L, V9 = attrs)
}
gff <- frame(c(
    'ID=gene:G;Parent=rna:P;gene=G%253B1;Name=G%3B1;gene_name=gene%3A%20G%253B1;gene_id=gid;Alias=ignored',
    'ID=rna:T;Parent=gene:G;protein_id=NP_1;Dbxref=GeneID:12,Ref:part:tail;gene_synonym=A,B,A',
    'ID=cds:C;Name=N;Name=second;db_xref=X:Y;gene_synonyms=B,C;synonym=D,,E;locus_tag= LOC ',
    'ID=cds:C;Name=N;Name=second;db_xref=X:Y;gene_synonyms=B,C;synonym=D,,E;locus_tag= LOC ',
    'gene_id "gtf"; gene_name "GTF name"; transcript_id "tx"; Name=100%bad%2Z',
    NA_character_, '', '.', 'broken;=empty-key;ID=;Name=a=b;DBXREF=,X:,X::Y'))

make_env <- function() {
    e <- new.env(parent = globalenv())
    e$preparation <- NULL
    e$hfiles <- list('1' = gff, '2' = frame('ID=other;Name=Other'))
    e$ofiles <- list('1' = frame('ID=mouse;Name=Mouse'))
    e$horg <- list('1' = list(name = 'Human', taxid = 9606), '2' = list(name = 'Human', taxid = 9606))
    e$oorg <- list('1' = list(name = 'Mouse', taxid = 10090))
    e$htitles <- list('1' = 'fallback', '2' = 'Other')
    e$otitles <- list('1' = 'Mouse')
    e$hpaths <- list('1' = 'human.gff', '2' = 'human.gff')
    e$opaths <- list('1' = 'mouse.gff')
    eval(quote({
        fileDataHomologous <- function() hfiles
        fileDataOrthologous <- function() ofiles
        organismInfoHomologous <- function() horg
        organismInfoOrthologous <- function() oorg
        annotationPathsHomologous <- function() hpaths
        annotationPathsOrthologous <- function() opaths
        titlesHomologous <- function() htitles
        titlesOrthologous <- function() otitles
        activePlotIdsHomologous <- function() names(fileDataHomologous())
        activePlotIdsOrthologous <- function() names(fileDataOrthologous())
        get_chart_plot_data <- function(pid, ctx) {
            if (identical(ctx, 'homo')) list(file_data = fileDataHomologous()[[pid]], org_info = organismInfoHomologous()[[pid]])
            else list(file_data = fileDataOrthologous()[[pid]], org_info = organismInfoOrthologous()[[pid]])
        }
        extract_title_field <- function(title, field) title
        resolve_taxid_for_go_lookup <- function(path, name) if (identical(path, 'missing')) NA_real_ else 9606
        build_string_error_widget <- function(...) list(...)
        input <- list(app_theme = 'dark')
        observe <- function(expr) preparation <<- substitute(expr)
    }), e)
    eval(reference, e)
    e$original <- e$build_string_query_payload
    eval(production, e)
    e
}
# This dependency must not silently fall through the builder's tryCatch. V1's
# harness omitted string_worker.R, so screen_variants coverage was incomplete.
dependency_env <- make_env()
before <- dependency_env$original('1', 'homo')
stopifnot(length(before$screen_variants) > 0L)
dependency_env$hfiles[['2']]$V9 <- 'ID=other;Name=Other;protein_id=UNIQUE_Y_ALIAS'
after <- dependency_env$original('1', 'homo')
stopifnot(identical(before$id_candidates, after$id_candidates),
          !identical(before$screen_variants, after$screen_variants),
          'UNIQUE_Y_ALIAS' %in% after$screen_variants,
          identical(after, dependency_env$build_string_query_payload('1', 'homo')))

e <- make_env()
parity <- function() {
    for (ctx in c('homo', 'ortho')) {
        ids <- if (ctx == 'homo') names(e$hfiles) else names(e$ofiles)
        for (pid in ids) stopifnot(identical(e$original(pid, ctx), e$build_string_query_payload(pid, ctx)))
    }
}
parity()
# Lifecycle pruning after the parity requests must leave valid projections reusable.
eval(e$preparation, e)
reset_counts()
warm <- e$build_string_query_payload('1', 'homo')
for (i in 1:5) stopifnot(identical(warm, e$build_string_query_payload('1', 'homo')))
stopifnot(identical(unname(counts), c(0L, 0L)))
cat('Repeated payloads after preparation: 0 parser calls, 0 decoder calls\n')

# Each relevant source change must refresh; title/theme are assembled live.
changes <- list(
    function() e$hfiles[['1']]$V9[2] <- 'ID=new;Parent=newparent;protein_id=NEW;Name=New',
    function() e$hfiles[['1']] <- e$hfiles[['1']][rev(seq_len(nrow(e$hfiles[['1']]))), ],
    function() e$hfiles[['1']]$V4 <- e$hfiles[['1']]$V4 + 1000L,
    function() e$hfiles[['1']]$V3[1] <- 'gene',
    function() e$hpaths[['1']] <- 'replacement.gff',
    function() e$horg[['1']]$taxid <- 10090,
    function() e$horg[['1']]$name <- 'Replacement species'
)
for (change in changes) {
    change()
    reset_counts()
    e$build_string_query_payload('1', 'homo')
    stopifnot(counts[['parse']] > 0L)
    parity()
}
fixtures <- list(
    frame(c('Parent=gene:P;ID=gene:F;gene_id=preferred', 'ID=cds:F;Dbxref=A:B')), # title fallback, Parent
    frame('gene_id "GT"; gene_name "GTF";'),
    frame(c('ID=transcript:only;Name=Only', 'Parent=transcript:only'), c('mRNA', 'exon')), # no gene row
    frame(c(NA_character_, '', '; ;', 'malformed', '=x', 'ID=%;Name=%FF', 'Dbxref=::,,')),
    frame(paste0('ID=id', 1:90, ';Name=N', 1:90)), # 64-candidate cap
    data.frame(V3 = 'gene'), # missing V9
    frame(character(0), character(0))
)
for (f in fixtures) {
    e$hfiles[['1']] <- f
    parity()
}
# Deterministic mixed-format permutations: duplicate keys/values, encoded
# delimiters, invalid escapes, mixed feature types and missing rows.
set.seed(303)
attrs_pool <- c(gff$V9, 'ID=gene:A;Name=A+B;Name=ignored;gene_name=G%253B2',
                'GENE=Upper;ID=A%3BB;Parent=P%2CQ;Dbxref=X%3AY%2CZ;gene_synonym=S%2CT',
                'gene_id "quoted"; gene_name "gene: quoted";', 'Name=Gene', 'ID==')
for (i in seq_len(40L)) {
    attrs <- sample(attrs_pool, 8L, replace = TRUE)
    e$hfiles[['1']] <- frame(attrs, sample(c('gene', 'mRNA', 'CDS', 'exon'), 8L, replace = TRUE))
    parity()
}
e$hfiles[['1']] <- frame('ID=X')
e$htitles[['1']] <- 'Gene'
parity() # no gene name
e$htitles[['1']] <- 'fallback changed'
e$horg[['1']]$taxid <- NA_real_
e$hpaths[['1']] <- 'missing'
parity() # missing TaxID
e$hpaths[['1']] <- 'resolve-from-path'
parity() # taxid resolver fallback
e$input$app_theme <- 'light'
parity()

# One row parse per preparation shared by both consumers, plus the unchanged
# name/ID helpers' two parses of the first gene row. No exact-string memoizer.
reset_counts()
projection <- string_prepare_gff_identifiers(gff)
stopifnot(counts[['parse']] == nrow(gff) + 2L)
stopifnot(anyDuplicated(projection$id_candidates) > 0L)
# Projection retains no parsed rows; duplicate handling remains at original sites.
stopifnot(identical(names(projection), c('gene_name', 'gene_id', 'screen_ids', 'id_candidates')))
state <- new_string_annotation_state()
source_state <- list(file_data = gff, org_info = list(taxid = 9606), annotation_path = 'human.gff')
a <- invisible(state$get('homo:1', source_state))
reset_counts()
stopifnot(identical(a, state$get('homo:1', source_state)), counts[['parse']] == 0L)
state$prune(character())
invisible(state$get('homo:1', source_state))
stopifnot(counts[['parse']] == nrow(gff) + 2L)
reset_counts()
invisible(new_string_annotation_state()$get('homo:1', source_state))
stopifnot(counts[['parse']] == nrow(gff) + 2L) # session isolation

# A 368-plot establishment and restore must perform no STRING projection work.
shiny::testServer(function(input, output, session) {
    env <- make_env()
    ids <- as.character(seq_len(368L))
    env$hfiles <- setNames(rep(list(gff), length(ids)), ids)
    env$horg <- setNames(rep(list(list(name = 'Human', taxid = 9606)), length(ids)), ids)
    env$hpaths <- setNames(rep(list('human.gff'), length(ids)), ids)
    env$ofiles <- list()
    env$observe <- shiny::observe
    env$fileDataHomologous <- shiny::reactiveVal(env$hfiles)
    eval(production, env)
}, {
    reset_counts()
    session$flushReact()
    stopifnot(counts[['parse']] == 0L, counts[['decode']] == 0L)
    # Direct getter verifies lazy projection storage and pruning separately from
    # the full payload's cross-plot screen_variants dependency.
    d <- env$get_chart_plot_data('1', 'homo')
    x <- env$get_string_annotation_ids('1', 'homo', d)
    stopifnot(counts[['parse']] == nrow(gff) + 2L)
    reset_counts()
    fd <- env$fileDataHomologous()
    env$fileDataHomologous(list())
    session$flushReact()
    stopifnot(counts[['parse']] == 0L, counts[['decode']] == 0L)
    env$fileDataHomologous(fd)
    session$flushReact()
    stopifnot(counts[['parse']] == 0L, counts[['decode']] == 0L)
    stopifnot(identical(x, env$get_string_annotation_ids('1', 'homo', d)))
    stopifnot(counts[['parse']] == nrow(gff) + 2L)
})

# Exercise the production observer with genuine Shiny reactive state, including
# deletion, restore, an unrelated plot update, and opening before observer flush.
shiny::testServer(function(input, output, session) {
    env <- make_env()
    env$observe <- shiny::observe
    env$fileDataHomologous <- shiny::reactiveVal(env$hfiles)
    env$fileDataOrthologous <- shiny::reactiveVal(env$ofiles)
    env$organismInfoHomologous <- shiny::reactiveVal(env$horg)
    env$organismInfoOrthologous <- shiny::reactiveVal(env$oorg)
    env$annotationPathsHomologous <- shiny::reactiveVal(env$hpaths)
    env$annotationPathsOrthologous <- shiny::reactiveVal(env$opaths)
    eval(production, env)
}, {
    reset_counts()
    session$flushReact()
    stopifnot(counts[['parse']] == 0L, counts[['decode']] == 0L)
    p <- env$build_string_query_payload('1', 'homo')
    reset_counts()
    stopifnot(identical(p, env$build_string_query_payload('1', 'homo')))
    stopifnot(counts[['parse']] == 0L, counts[['decode']] == 0L)
    fd <- env$fileDataHomologous()
    fd[['2']] <- frame('ID=changed;Name=Changed')
    env$fileDataHomologous(fd)
    session$flushReact()
    stopifnot(counts[['parse']] == 0L, counts[['decode']] == 0L)
    env$build_string_query_payload('1', 'homo')
    stopifnot(counts[['parse']] == 3L) # screen dependency: changed one-row plot
    fd[['1']]$V9[1] <- 'ID=changed-first;Name=ChangedFirst'
    env$fileDataHomologous(fd)
    # No flush yet: synchronous guard must detect the changed snapshot.
    stopifnot(identical(env$original('1', 'homo'), env$build_string_query_payload('1', 'homo')))
    session$flushReact()
    reset_counts()
    env$build_string_query_payload('1', 'homo')
    stopifnot(counts[['parse']] == 0L)
    env$fileDataHomologous(list())
    session$flushReact()
    reset_counts()
    env$fileDataHomologous(fd)
    session$flushReact()
    stopifnot(counts[['parse']] == 0L, counts[['decode']] == 0L)
    env$build_string_query_payload('1', 'homo')
    stopifnot(counts[['parse']] == nrow(fd[['1']]) + nrow(fd[['2']]) + 4L)
})
# Timing disabled must not emit logs; enabled exposes materialization and reuse.
stopifnot(length(capture.output(invisible(state$get('homo:1', source_state)), type = 'message')) == 0L)
Sys.setenv(APP_PERF_TIMING = '1')
messages <- capture.output({
    state$prune(character())
    invisible(state$get('homo:1', source_state))
    invisible(state$get('homo:1', source_state))
}, type = 'message')
stopifnot(any(grepl('annotation_prepare_ms', messages)),
          any(grepl('annotation_materialized', messages)), any(grepl('annotation_reuse', messages)))
Sys.setenv(APP_PERF_TIMING = '0')
cat('string-annotation-reuse-ok\n')
