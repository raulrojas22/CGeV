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
    stopifnot(counts[['parse']] == 0L, counts[['decode']] == 1L) # lightweight Y screen only
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
    stopifnot(counts[['parse']] == nrow(fd[['1']]) + 2L) # full query only for X
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

# Extract the original nested screen function directly from the frozen builder.
find_assignment <- function(expr, name) {
    if (!is.call(expr)) return(NULL)
    if (identical(expr[[1]], as.name('<-')) && identical(expr[[2]], as.name(name))) return(expr)
    for (child in as.list(expr)[-1L]) {
        result <- find_assignment(child, name)
        if (!is.null(result)) return(result)
    }
    NULL
}
ref_env <- new.env(parent = globalenv())
eval(find_assignment(reference[[1]], 'extract_gff_ids_for_plot'), ref_env)
ref_screen <- function(attrs) ref_env$extract_gff_ids_for_plot('1', 'homo', list(file_data = frame(attrs)))
# Mixed GFF/GTF, encoded keys, duplicate keys, unknown fields, invalid UTF-8
# escapes, embedded NUL escapes, malformed percent signs and trailing equals.
edges <- c(gff$V9, paste0('Name=A%', sprintf('%02X', 0:255), 'B;ID=tail'),
           'Name=A=;ID=B', 'Name=A==;ID=B', '%4Eame=Encoded;ID=x',
           'Name = spaced;ID=x', 'Name="quoted";ID=x', 'Name=A%2525B',
           'Parent=ignored;gene_name=ignored;Alias=ignored;gene_synonym=ignored',
           'gene_id "GTF"; gene_name "Ignored"; Name "Used";',
           'Name=A;unknown=%00;ID=after', 'Name=A;unknown=%FF;ID=after',
           'Name=A\tB;ID=C', 'NAME=first;name=second;Dbxref=A:B;DBXREF=C:D',
           'Dbxref=:,::,A:,A::B,,;db_xref=X:Y', rawToChar(as.raw(255)))
screen_state <- new_string_annotation_state()
for (i in seq_along(edges)) {
    attrs <- c('ID=before;Name=Before', edges[i], 'ID=after;Name=After')
    src <- list(file_data = frame(attrs), annotation_path = 'fixture', org_info = list(taxid = 9606))
    actual <- suppressWarnings(screen_state$screen(list('homo:1' = src))[[1]])
    expected <- suppressWarnings(ref_screen(attrs))
    stopifnot(identical(actual, expected))
}
# Deduplicating shared raw rows must not change row order or early-abort behavior.
set.seed(304)
for (i in 1:100) {
    attrs <- sample(edges[-length(edges)], 12L, replace = TRUE)
    src <- list(file_data = frame(attrs), annotation_path = 'fixture', org_info = list(taxid = 9606))
    stopifnot(identical(suppressWarnings(screen_state$screen(list('homo:1' = src))[[1]]),
                        suppressWarnings(ref_screen(attrs))))
}

# Count complete projections and unique rows processed by the lightweight path.
full_prepare <- string_prepare_gff_identifiers
full_count <- 0L
string_prepare_gff_identifiers <- function(file_data) {
    full_count <<- full_count + 1L
    full_prepare(file_data)
}
screen_prepare <- string_prepare_screen_rows
screen_rows_seen <- 0L
string_prepare_screen_rows <- function(attrs) {
    screen_rows_seen <<- screen_rows_seen + length(attrs)
    screen_prepare(attrs)
}
reset_work <- function() {
    reset_counts()
    full_count <<- 0L
    screen_rows_seen <<- 0L
}
large <- make_env()
ids <- as.character(seq_len(368L))
shared <- 'ID=gene:shared;Name=Shared;Dbxref=GeneID:123;gene_name=Shared;Parent=unused'
large$hfiles <- setNames(lapply(ids, function(id) frame(c(shared,
    paste0('ID=transcript:T', id, ';Name=T', id, ';protein_id=NP_', id),
    'ID=exon:common;Parent=gene:shared'))), ids)
large$horg <- setNames(rep(list(list(name = 'Human', taxid = 9606)), length(ids)), ids)
large$hpaths <- setNames(rep(list('human.gff'), length(ids)), ids)
large$htitles <- setNames(paste0('Title', ids), ids)
large$ofiles <- list()
reset_work()
eval(large$preparation, large)
stopifnot(full_count == 0L, screen_rows_seen == 0L, all(counts == 0L))
reset_work()
original_x <- large$original('1', 'homo')
original_counts <- counts
reset_work()
x <- large$build_string_query_payload('1', 'homo')
first_counts <- counts
stopifnot(identical(x, original_x), full_count == 1L, screen_rows_seen == 369L,
          counts[['parse']] == 5L, counts[['parse']] < original_counts[['parse']] / 10,
          counts[['decode']] < original_counts[['decode']] / 10,
          'NP_368' %in% x$screen_variants)
reset_work()
for (i in 1:5) stopifnot(identical(x, large$build_string_query_payload('1', 'homo')))
stopifnot(full_count == 0L, screen_rows_seen == 0L, all(counts == 0L))
reset_work()
y <- large$build_string_query_payload('2', 'homo')
y_counts <- counts
stopifnot(full_count == 1L, screen_rows_seen == 0L, counts[['parse']] == 5L)
stopifnot(identical(y, large$original('2', 'homo')))
# Alter only Y: X's query remains cached, screen candidates update lazily.
large$hfiles[['2']]$V9[2] <- 'ID=transcript:T2;protein_id=Y_CHANGED_ONLY'
reset_work()
x_changed <- large$build_string_query_payload('1', 'homo')
stopifnot(full_count == 0L, screen_rows_seen == 3L, counts[['parse']] == 0L,
          counts[['decode']] == 1L, identical(x$id_candidates, x_changed$id_candidates),
          'Y_CHANGED_ONLY' %in% x_changed$screen_variants,
          !'NP_2' %in% x_changed$screen_variants)
stopifnot(identical(x_changed, large$original('1', 'homo')))
reset_work()
invisible(large$build_string_query_payload('1', 'homo'))
stopifnot(full_count == 0L, screen_rows_seen == 0L, all(counts == 0L))
cat(sprintf('368-plot synthetic fixture: original X parse/decode=%d/%d; first X=%d/%d; first Y=%d/%d; repeated X=0/0\n',
    original_counts[['parse']], original_counts[['decode']], first_counts[['parse']], first_counts[['decode']],
    y_counts[['parse']], y_counts[['decode']]))
cat('string-screen-projection-ok\n')

# Functional reason for cross-plot aliases: a related node is visually classified
# as plotted when Y is on screen, and neighbor when Y is removed or another taxon.
local({
    tmp <- tempfile('string-screen-roles-')
    dir.create(tmp)
    on.exit(unlink(tmp, recursive = TRUE))
    roles_env <- make_env()
    roles_env$hfiles[['2']] <- frame('ID=Y;protein_id=Y_EXCLUSIVE_ALIAS')
    roles_env$htitles[['2']] <- 'Unrelated display title'
    string_resolution_cache_set(9606L, 'Y_EXCLUSIVE_ALIAS',
        list(found = TRUE, string_id = '9606.TP53', preferred_name = 'TP53'), tmp)
    graph <- list(taxid = 9606L, resolved_id = '9606.BRCA1', nodes = data.frame(
        id = c('9606.BRCA1', '9606.TP53', '9606.NEIGHBOR'), label = c('BRCA1', 'TP53', 'Neighbor')))
    compare_roles <- function(expected) {
        original <- roles_env$original('1', 'homo')
        candidate <- roles_env$build_string_query_payload('1', 'homo')
        stopifnot(identical(original, candidate))
        a <- string_apply_display_roles(graph, original, tmp, resolve_missing = FALSE)
        b <- string_apply_display_roles(graph, candidate, tmp, resolve_missing = FALSE)
        a$role_applied_at <- b$role_applied_at <- NULL
        stopifnot(identical(a, b), identical(b$nodes$role, c('target', expected, 'neighbor')))
    }
    compare_roles('plotted')
    roles_env$horg[['2']]$taxid <- 10090
    compare_roles('neighbor')
    roles_env$horg[['2']]$taxid <- 9606
    compare_roles('plotted')
    saved <- roles_env$hfiles[['2']]
    roles_env$hfiles[['2']] <- NULL
    reset_work()
    eval(roles_env$preparation, roles_env)
    stopifnot(full_count == 0L, screen_rows_seen == 0L, all(counts == 0L))
    compare_roles('neighbor')
    roles_env$hfiles[['2']] <- saved
    reset_work()
    eval(roles_env$preparation, roles_env)
    stopifnot(full_count == 0L, screen_rows_seen == 0L, all(counts == 0L))
    invisible(roles_env$build_string_query_payload('1', 'homo'))
    stopifnot(full_count == 0L, screen_rows_seen == 1L, counts[['parse']] == 0L)
    compare_roles('plotted')
})
cat('string-cross-plot-visual-roles-ok\n')

# Separate screen timing identifies fallback-heavy inputs without row logging.
local({
    state <- new_string_annotation_state()
    src <- list(file_data = frame(c('ID=G;Name=G', 'gene_id "gtf";')),
                org_info = list(taxid = 9606), annotation_path = 'timing')
    quiet <- capture.output(invisible(state$screen(list('homo:1' = src))), type = 'message')
    stopifnot(length(quiet) == 0L)
    state$prune(character())
    Sys.setenv(APP_PERF_TIMING = '1')
    on.exit(Sys.setenv(APP_PERF_TIMING = '0'))
    messages <- capture.output({
        invisible(state$screen(list('homo:1' = src)))
        invisible(state$screen(list('homo:1' = src)))
    }, type = 'message')
    stopifnot(any(grepl('screen_prepare_ms', messages)),
              any(grepl('screen_materialized=1 screen_reused=0 screen_unique_rows=2 screen_fallback_rows=1', messages)),
              any(grepl('screen_materialized=0 screen_reused=1 screen_unique_rows=0 screen_fallback_rows=0', messages)))
})
cat('string-screen-timing-ok\n')
