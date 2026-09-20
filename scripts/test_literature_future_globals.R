#!/usr/bin/env Rscript
find_assignment <- function(x, target) {
    if (!is.call(x) && !is.expression(x)) return(NULL)
    if (is.call(x) && identical(x[[1L]], as.name('<-')) && identical(x[[2L]], target)) return(x)
    for (i in seq_along(x)) {
        if (identical(x[[i]], quote(expr = ))) next
        hit <- find_assignment(x[[i]], target)
        if (!is.null(hit)) return(hit)
    }
    NULL
}
server <- parse('server.R')
e <- new.env(parent = baseenv())
e$`%||%` <- function(a, b) if (!is.null(a)) a else b
e$unused_session <- raw(2 * 1024^2)
definition <- find_assignment(server, quote(search_papers_epmc))
rebind <- find_assignment(server, quote(environment(search_papers_epmc)))
stopifnot(!is.null(definition), !is.null(rebind))
eval(definition, e)
before <- e$search_papers_epmc
eval(rebind, e)
after <- e$search_papers_epmc
stopifnot(identical(body(before), body(after)),
          identical(ls(environment(after), all.names = TRUE), '%||%'),
          identical(parent.env(environment(after)), baseenv()),
          identical(environment(environment(after)$`%||%`), baseenv()),
          length(serialize(after, NULL)) < 20000L)
response <- function(status, json = '{}') httr2::response(
    status_code = status, body = charToRaw(json),
    headers = list(`content-type` = 'application/json'))
fixture <- list(response(200, '{"hitCount":2,"nextCursorMark":"page2","resultList":{"result":[]}}'),
    response(200, '{"hitCount":2,"nextCursorMark":"page2","resultList":{"result":[{"title":"A &amp; B","abstractText":"<p>Study &lt;gene&gt;</p>","authorString":"Example","pubYear":"2025","journalTitle":"Journal","doi":"10.1/example","citedByCount":3,"source":"MED"}]}}'))
args <- list(gene_names = c('ABC;1', 'alias'), organism_scientific = 'Homo sapiens',
             organism_aliases = 'human', page = 2L, page_size = 1L, sort_by = 'DATE desc')
a <- httr2::with_mocked_responses(fixture, do.call(before, args))
b <- httr2::with_mocked_responses(fixture, do.call(after, args))
stopifnot(identical(a, b), identical(b$papers[[1]]$title, 'A & B'),
          identical(b$papers[[1]]$abstract, 'Study'), b$hits == 2L,
          identical(b$query, '("ABC;1" OR "alias") AND ("Homo sapiens" OR "human")'))
for (status in c(400L, 503L)) {
    a <- httr2::with_mocked_responses(list(response(status)), do.call(before, args))
    b <- httr2::with_mocked_responses(list(response(status)), do.call(after, args))
    stopifnot(identical(a, b)) # preserve existing error semantics, including NULL error
}
stopifnot(identical(before(character()), after(character())),
          identical(before('gene'), after('gene')))
local({
    future::plan(future::multisession, workers = I(1L))
    on.exit(future::plan(future::sequential))
    f <- future::future(httr2::with_mocked_responses(fixture, do.call(search_papers_epmc, args)),
        globals = list(search_papers_epmc = after, fixture = fixture, args = args), packages = 'httr2')
    remote <- future::value(f)
    expected <- httr2::with_mocked_responses(fixture, do.call(before, args))
    stopifnot(identical(remote, expected))
})
cat('literature-future-globals-ok\n')
transport <- list(search_papers_epmc = after, gene_names = args$gene_names,
    organism_str = args$organism_scientific, org_aliases = args$organism_aliases,
    page = args$page, page_size = args$page_size, sort_by = args$sort_by)
cat('Literature explicit globals:', length(transport),
    'serialized bytes (fixture):', length(serialize(transport, NULL)), '\n')
