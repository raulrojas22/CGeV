# Isolated browser fixture: actual C1a transport handlers and Shiny modal teardown.
# The confirmation case bridges real browser transport to actual server observer
# bodies in an isolated test session and real GFF lookup on synthetic annotations.
# No application data, external requests, plotting modules or worker plan is loaded.
# Run from repository root: Rscript tests/manual/search_ownership_transport_app.R
library(shiny)
source('tests/testthat/helper-search-ownership.R')
for (node in parse('tests/testthat/test-search-review-regressions.R')) {
    if (is.call(node) && identical(node[[1L]], quote(`<-`))) eval(node)
}
Sys.setenv(APP_ANNOTATION_DISK_CACHE_DIR = tempfile('transport-annotation-cache-'))
ui_text <- readLines('ui.R', warn = FALSE)
extract_handler <- function(start_pattern) {
    start <- which(grepl(start_pattern, ui_text, fixed = TRUE))[[1L]]
    end <- start - 1L + which(trimws(ui_text[start:length(ui_text)]) == '});')[[1L]]
    paste(ui_text[start:end], collapse = '\n')
}
transport <- paste(
    extract_handler("$(document).on('hidden.bs.modal'"),
    extract_handler("Shiny.addCustomMessageHandler('cgv_trigger_homo_search'"),
    extract_handler("Shiny.addCustomMessageHandler('cgv_trigger_ortho_search'"),
    paste(readLines('www/js/gene_catalog.js'), collapse = '\n'), sep = '\n')
ui <- fluidPage(
    tags$script(HTML(paste0('$(function(){', transport, '});'))),
    h3('Isolated C1a browser transport validation'),
    actionButton('homo', 'Send homologous'), actionButton('ortho', 'Send orthologous'),
    actionButton('catalog_homo', 'Send homologous catalog'), actionButton('catalog_ortho', 'Send orthologous catalog'),
    actionButton('confirmation', 'Show pending confirmation'),
    verbatimTextOutput('received'),
    actionButton('reference_confirmation', 'Test selected reference confirmation'),
    verbatimTextOutput('reference_result')
)
server <- function(input, output, session) {
    engine <- ownership_harness()
    fixture <- review_gff_fixture(engine)
    session$onSessionEnded(engine$close)
    refresh <- reactiveVal(0L)
    sent <- engine$session$sendCustomMessage
    engine$session$sendCustomMessage <- function(type, message) {
        sent(type, message)
        session$sendCustomMessage(type, message)
    }
    engine$e$build_app_confirmation_modal <- function(title, body, confirm_input_id, confirm_label, ...) {
        modalDialog(title = title, body,
            footer = tagList(modalButton('Dismiss'), actionButton(confirm_input_id, confirm_label)))
    }
    engine$e$showModal <- function(dialog) shiny::showModal(dialog, session = session)
    engine$e$removeModal <- function() shiny::removeModal(session = session)
    observeEvent(input$reference_confirmation, {
        engine$e$fileDataOrthologous(list(old = list())); engine$e$lastOrthoSearchedGene('OLD')
        engine$e$execute_orthologous_search('BROAD', 'orthology_reference', reference_anchor = list(
            local_gene_id = 'LOC_SELECTED', organism_id = 'a', organism_name = 'A', query = 'BROAD'))
        refresh(isolate(refresh()) + 1L)
    })
    observeEvent(input$cgv_ortho_search_dispatch, {
        engine$session$setInputs(cgv_ortho_search_dispatch = input$cgv_ortho_search_dispatch)
        refresh(isolate(refresh()) + 1L)
    })
    observeEvent(input$confirm_ortho_gene_change_btn, {
        engine$session$setInputs(confirm_ortho_gene_change_btn = input$confirm_ortho_gene_change_btn)
        refresh(isolate(refresh()) + 1L)
    })
    observeEvent(input$cgv_search_confirmation_closed, {
        engine$session$setInputs(cgv_search_confirmation_closed = input$cgv_search_confirmation_closed)
        refresh(isolate(refresh()) + 1L)
    })
    output$reference_result <- renderText({
        refresh()
        jsonlite::toJSON(list(
            requests = lapply(Filter(function(x) x$type == 'cgv_trigger_ortho_search', engine$e$messages), `[[`, 'payload'),
            scientific_results = lapply(seq_along(fixture$results()), function(i) {
                r <- fixture$results()[[i]]
                list(query = fixture$jobs()[[i]]$gene_name, gene_id = r$lookup$matched_gene_id,
                    coordinates = range(as.numeric(r$data$V4), as.numeric(r$data$V5)), strand = unique(r$data$V7))
            })
        ), auto_unbox = TRUE, pretty = TRUE)
    })
    observeEvent(input$homo, session$sendCustomMessage('cgv_trigger_homo_search',
        list(request_id = 'search_homo', dispatch_id = 1, origin = 'local')))
    observeEvent(input$ortho, session$sendCustomMessage('cgv_trigger_ortho_search',
        list(request_id = 'search_ortho', dispatch_id = 2, origin = 'local')))
    observeEvent(input$catalog_homo, session$sendCustomMessage('cgv:catalog-open-multigene',
        list(request_id = 'search_catalog_homo', species_id = 'a', gene = 'G')))
    observeEvent(input$catalog_ortho, session$sendCustomMessage('cgv:catalog-open-cross',
        list(request_id = 'search_catalog_ortho', species_ids = c('a', 'b'), gene = 'G')))
    observeEvent(input$confirmation, showModal(modalDialog(
        title = 'Pending request',
        tags$span(style = 'display:none', `data-cgv-search-mode` = 'homologous',
            `data-cgv-search-request` = 'search_confirmation'),
        'Close this fixture dialog.', footer = modalButton('Close confirmation'))))
    output$received <- renderText(jsonlite::toJSON(list(
        homologous = input$cgv_homo_search_dispatch,
        orthologous = input$cgv_ortho_search_dispatch,
        catalog_homologous = input$catalog_multigene_ready,
        catalog_orthologous = input$catalog_cross_ready,
        closed_confirmation = input$cgv_search_confirmation_closed
    ), auto_unbox = TRUE, pretty = TRUE))
}
shiny::runApp(shinyApp(ui, server), host = '127.0.0.1',
    port = as.integer(Sys.getenv('CGV_TRANSPORT_TEST_PORT', '47831')), launch.browser = FALSE)
