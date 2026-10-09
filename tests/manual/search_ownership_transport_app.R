# Isolated browser fixture: actual C1a transport handlers and Shiny modal teardown.
# No CGeV server, scientific producers, external requests or worker plan is loaded.
# Run from repository root: Rscript tests/manual/search_ownership_transport_app.R
library(shiny)
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
    verbatimTextOutput('received')
)
server <- function(input, output, session) {
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
