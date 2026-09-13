# ui.R

analytics_chart_order_choices <- c(
  "Load order" = "load",
  "Gene length (longest first)" = "gene_len_desc",
  "Gene length (shortest first)" = "gene_len_asc",
  "Transcript length (longest first)" = "tx_len_desc",
  "Transcript length (shortest first)" = "tx_len_asc",
  "Exon count (high to low)" = "exon_desc",
  "Exon count (low to high)" = "exon_asc",
  "Intron count (high to low)" = "intron_desc",
  "Intron count (low to high)" = "intron_asc",
  "GC% (high to low)" = "gc_desc",
  "GC% (low to high)" = "gc_asc",
  "Label (A-Z)" = "label_asc",
  "Label (Z-A)" = "label_desc"
)

cgv_gene_catalog_enabled <-
  exists("gene_catalog_enabled", mode = "function") &&
  isTRUE(gene_catalog_enabled())

guide_media_path <- function(path) {
  path <- as.character(path)
  if (length(path) == 0 || is.na(path)) {
    path <- ""
  }
  if (!nzchar(path)) {
    return("")
  }
  full_path <- file.path("www", path)
  if (file.exists(full_path)) {
    info <- file.info(full_path)
    mtime <- suppressWarnings(as.numeric(info$mtime))
    size <- suppressWarnings(as.numeric(info$size))
    video_version <- sprintf(
      "%s-%s",
      if (is.finite(mtime)) as.character(round(mtime)) else "0",
      if (is.finite(size)) as.character(round(size)) else "0"
    )
    return(sprintf("%s?gv=%s", static_asset_path(path), video_version))
  }
  ""
}

cgv_manual_path <- guide_media_path(file.path("docs", "CGeV_User_Manual.pdf"))
if (!nzchar(cgv_manual_path)) {
  cgv_manual_path <- "docs/CGeV_User_Manual.pdf"
}

guide_media_files <- c(
  "guide-intro.mp4",
  "guide-multigene-01a-preloaded-organism.mp4",
  "guide-multigene-01b-ncbi-search.mp4",
  "guide-multigene-01c-upload-files.mp4",
  "guide-multigene-02a-add-one-gene.mp4",
  "guide-multigene-02b-add-batch-genes.mp4",
  "guide-multigene-03-generate-visualization.mp4",
  "guide-multigene-04a-compact-visualization.mp4",
  "guide-multigene-04b-detailed-visualization.mp4",
  "guide-multigene-05-alignment-optional.mp4",
  "guide-multigene-06a-export-figures.mp4",
  "guide-multigene-06b-export-tables-results.mp4",
  "guide-cross-01a-preloaded-organisms.mp4",
  "guide-cross-01b-ncbi-search.mp4",
  "guide-cross-01c-upload-files.mp4",
  "guide-cross-01d-mixed-sources.mp4",
  "guide-cross-02-search-gene.mp4",
  "guide-cross-03-generate-visualization.mp4",
  "guide-cross-03a-compact-visualization.mp4",
  "guide-cross-03b-detailed-visualization.mp4",
  "guide-cross-05a-comparative-synteny-align.mp4",
  "guide-cross-05b-lastz-blocks.mp4",
  "guide-cross-05c-multipip.mp4",
  "guide-cross-06a-export-alignment-visual-figures.mp4",
  "guide-common-01-review-analytics-charts.mp4",
  "guide-common-02-review-tables-results.mp4",
  "guide-common-03-visualize-transcript-variants.mp4",
  "guide-common-04-inspect-gene-information.mp4",
  "guide-common-05-download-promoter-sequences.mp4",
  "guide-common-06-review-literature.mp4",
  "guide-common-07-review-organism-assembly-info.mp4",
  "guide-common-08-configure-external-alias-lookup.mp4",
  "guide-common-09a-save-work-session.mp4",
  "guide-common-09b-load-work-session.mp4",
  "guide-common-10-clear-visualizations.mp4",
  "guide-common-share-analysis-web.mp4",
  "guide-common-export-report-desktop.mp4",
  "guide-figure-studio-01-open-workspace.mp4",
  "guide-figure-studio-02-add-panels.mp4",
  "guide-figure-studio-03-arrange-and-style.mp4",
  "guide-figure-studio-04-preview-and-export.mp4",
  "guide-desktop-downloads-01-open-settings.mp4",
  "guide-desktop-downloads-02-open-organism-catalog.mp4",
  "guide-desktop-downloads-03-search-and-filter.mp4",
  "guide-desktop-downloads-04-download-organism.mp4",
  "guide-desktop-downloads-05-confirm-availability.mp4",
  "guide-desktop-downloads-06-remove-installed-organisms.mp4"
)
guide_media_map <- stats::setNames(
  vapply(guide_media_files, function(file) guide_media_path(file.path("screencasts", file)), character(1)),
  guide_media_files
)

initial_summary_context_header <- function(search_mode_label = "Multi-Gene", gene_hint = "Genes: pending", align_detail = "Compare transcripts") {
  orientation_input_id <- if (grepl("Cross", as.character(search_mode_label), ignore.case = TRUE)) {
    "ortho_orientation_pick"
  } else {
    "homo_orientation_pick"
  }
  div(
    class = "summary-context-card summary-context-card-inline summary-context-card-initial",
    div(
      class = "summary-context-title",
      span(class = "summary-context-kicker", "Search type"),
      span(class = "summary-context-mode", search_mode_label)
    ),
    div(
      class = "summary-context-organisms",
      span(class = "summary-context-empty", "Waiting for organisms")
    ),
    div(
      class = "summary-context-actions",
      span(class = "summary-context-gene-hint", gene_hint),
      tags$button(
        type = "button",
        class = "app-notification-center-btn app-notification-center-toggle",
        `aria-label` = "Open notification history",
        `aria-expanded` = "false",
        title = "Notification history",
        icon("bell"),
        span(class = "app-notification-center-badge", "0")
      )
    )
  )
}

result_workspace_subheader <- function(scope = c("homo", "ortho")) {
  scope <- match.arg(scope)
  is_ortho <- identical(scope, "ortho")
  sort_choices <- if (is_ortho) {
    c(
      "Load order" = "load",
      "Total exon difference (high to low)" = "exondiff_desc",
      "Total exon difference (low to high)" = "exondiff_asc",
      "Transcript length (longest first)" = "tx_len_desc",
      "Transcript length (shortest first)" = "tx_len_asc",
      "Exon count (high to low)" = "exon_desc",
      "Exon count (low to high)" = "exon_asc",
      "Organism name (A-Z)" = "organism_asc",
      "Organism name (Z-A)" = "organism_desc",
      "Transcript name (A-Z)" = "tx_asc",
      "Transcript name (Z-A)" = "tx_desc"
    )
  } else {
    c(
      "Load order" = "load",
      "Total exon difference (high to low)" = "exondiff_desc",
      "Total exon difference (low to high)" = "exondiff_asc",
      "Transcript length (longest first)" = "tx_len_desc",
      "Transcript length (shortest first)" = "tx_len_asc",
      "Exon count (high to low)" = "exon_desc",
      "Exon count (low to high)" = "exon_asc",
      "Transcript name (A-Z)" = "tx_asc",
      "Transcript name (Z-A)" = "tx_desc",
      "Chromosome (A-Z)" = "chr_asc",
      "Chromosome (Z-A)" = "chr_desc"
    )
  }
  workflow_label <- if (is_ortho) "Cross-Species" else "Multi-Gene"

  workspace_nav_button <- function(view, label, icon_name, active = FALSE, disabled = FALSE) {
    htmltools::tagAppendAttributes(
      actionButton(
        inputId = paste0(scope, "_workspace_", view),
        label = span(label),
        icon = icon(icon_name),
        class = paste("result-workspace-nav-button", if (isTRUE(active)) "is-active" else "")
      ),
      `data-workspace-view` = view,
      `aria-pressed` = if (isTRUE(active)) "true" else "false",
      disabled = if (isTRUE(disabled)) NA else NULL
    )
  }

  tool_trigger <- function(name, label, icon_name) {
    tags$button(
      id = paste0(scope, "_workspace_", name, "_trigger"),
      type = "button",
      class = "result-workspace-tool-trigger",
      `data-workspace-menu-trigger` = name,
      `aria-expanded` = "false",
      `aria-controls` = paste0(scope, "_workspace_", name, "_menu"),
      disabled = NA,
      icon(icon_name),
      span(label),
      icon("chevron-down", class = "result-workspace-tool-chevron")
    )
  }

  div(
    id = paste0(scope, "_result_workspace_subheader"),
    class = "result-workspace-subheader",
    `data-workspace-scope` = scope,
    div(
      class = "result-workspace-navigation",
      div(
        class = "result-workspace-tabs",
        role = "tablist",
        `aria-label` = paste(workflow_label, "result views"),
        workspace_nav_button("visualization", "Visualization", "eye", active = TRUE),
        workspace_nav_button("alignment", "Alignment", "project-diagram", disabled = TRUE),
        workspace_nav_button("analytics", "Analytics", "chart-bar", disabled = TRUE),
        workspace_nav_button("table", "Table", "table", disabled = TRUE)
      )
    ),
    if (is_ortho) {
      div(
        class = "result-workspace-alignment-methods",
        `data-workspace-alignment-methods` = "ortho",
        `aria-label` = "Cross-Species alignment method",
        span(class = "result-workspace-alignment-methods-label", icon("project-diagram"), "Method"),
        div(
          class = "result-workspace-alignment-method-options",
          role = "group",
          `aria-label` = "Choose an alignment method",
          tags$button(
            type = "button",
            class = "result-workspace-alignment-method is-active",
            `data-workspace-alignment-method` = "aligned",
            `aria-pressed` = "true",
            title = "Compare ordered gene and transcript structures across organisms",
            "Synteny"
          ),
          tags$button(
            type = "button",
            class = "result-workspace-alignment-method",
            `data-workspace-alignment-method` = "pip_blocks",
            `aria-pressed` = "false",
            title = "Inspect local LASTZ alignment blocks across organism loci",
            "LASTZ Blocks"
          ),
          tags$button(
            type = "button",
            class = "result-workspace-alignment-method",
            `data-workspace-alignment-method` = "pip_multipip",
            `aria-pressed` = "false",
            title = "Inspect reference-centered conservation segments across organisms",
            "MultiPIP"
          )
        )
      )
    } else {
      NULL
    },
    div(
      class = "result-workspace-tools",
      div(
        class = "result-workspace-zoom",
        id = paste0(scope, "-zoom-control"),
        style = "display:none;",
        tags$button(
          id = paste0(scope, "-zoom-out"), class = "plot-zoom-btn",
          `data-zoom-action` = "out", `data-zoom-mode` = scope,
          title = "Zoom out", disabled = NA, "\u2212"
        ),
        span(id = paste0(scope, "-zoom-label"), class = "plot-zoom-label", "1\u00d7"),
        tags$button(
          id = paste0(scope, "-zoom-in"), class = "plot-zoom-btn",
          `data-zoom-action` = "in", `data-zoom-mode` = scope,
          title = "Zoom in", "+"
        )
      ),
      div(
        class = "result-workspace-tool",
        `data-workspace-tool` = "view",
        tool_trigger("view", "View", "sliders-h"),
        div(
          id = paste0(scope, "_workspace_view_menu"),
          class = "result-workspace-menu result-workspace-menu--view",
          `data-workspace-menu` = "view",
          role = "dialog",
          `aria-label` = "Visualization settings",
          div(
            class = "result-workspace-menu-heading",
            div(icon("sliders-h"), span(strong("View settings"), tags$small("Choose detail, context and direction"))),
            tags$button(type = "button", class = "result-workspace-menu-close", `aria-label` = "Close view settings", icon("times"))
          ),
          uiOutput(paste0(scope, "_header_mode_switch"))
        )
      ),
      div(
        class = "result-workspace-tool",
        `data-workspace-tool` = "sort",
        tool_trigger("sort", "Sort", "sort-amount-desc"),
        div(
          id = paste0(scope, "_workspace_sort_menu"),
          class = "result-workspace-menu result-workspace-menu--sort",
          `data-workspace-menu` = "sort",
          role = "dialog",
          `aria-label` = "Sort plots",
          div(
            class = "result-workspace-menu-heading",
            div(icon("sort-amount-desc"), span(strong("Sort plots"), tags$small("Reorder the current visualization"))),
            tags$button(type = "button", class = "result-workspace-menu-close", `aria-label` = "Close sort menu", icon("times"))
          ),
          selectInput(
            inputId = paste0(scope, "_sort_mode"),
            label = NULL,
            choices = sort_choices,
            selected = "load",
            width = "100%"
          )
        )
      ),
      div(
        class = "result-workspace-tool",
        `data-workspace-tool` = "download",
        tool_trigger("download", "Download", "download"),
        div(
          id = paste0(scope, "_workspace_download_menu"),
          class = "result-workspace-menu result-workspace-menu--download",
          `data-workspace-menu` = "download",
          role = "dialog",
          `aria-label` = "Download current results",
          div(
            class = "result-workspace-menu-heading",
            div(icon("download"), span(strong("Download"), tags$small("Exports adapt to the active view"))),
            tags$button(type = "button", class = "result-workspace-menu-close", `aria-label` = "Close download menu", icon("times"))
          ),
          div(
            class = "result-workspace-download-list",
            div(
              class = "result-workspace-download-option workspace-download-visualization",
              actionButton(
                inputId = paste0("btn_download_all_", scope, "_svg"),
                label = tagList(icon("file-zipper"), span(strong("Result SVGs"), tags$small("Download all visible gene plots as a ZIP"))),
                class = "btn btn-sm btn-download-zip result-workspace-download-button",
                style = "display:none;",
                onclick = sprintf("exportAllSVGs('%s')", scope)
              )
            ),
            div(
              class = "result-workspace-download-option workspace-download-analytics",
              actionButton(
                inputId = paste0("btn_download_all_", scope, "_analytics_svg"),
                label = tagList(icon("file-zipper"), span(strong("Analytics SVGs"), tags$small("Download available analytics charts as a ZIP"))),
                class = "btn btn-sm btn-download-zip result-workspace-download-button",
                style = "display:none;",
                onclick = sprintf("exportAnalyticsSVGs('%s')", scope)
              )
            ),
            div(
              class = "result-workspace-download-option workspace-download-table",
              downloadButton(
                paste0("download_", scope, "_summary_csv"),
                tagList(icon("file-csv"), span(strong("Summary CSV"), tags$small("Export the complete result table"))),
                class = "btn-sm btn-download result-workspace-download-button"
              )
            )
          )
        )
      )
    ),
    actionButton(
      inputId = paste0("toggle_", scope, "_analytics"),
      label = "Toggle analytics",
      class = "result-workspace-legacy-toggle",
      style = "display:none;"
    ),
    actionButton(
      inputId = paste0("toggle_", scope, "_summary"),
      label = "Toggle summary table",
      class = "result-workspace-legacy-toggle",
      style = "display:none;"
    )
  )
}

analytics_arch_order_choices <- c(
  "Load order" = "load",
  "Gene length (longest first)" = "gene_len_desc",
  "Gene length (shortest first)" = "gene_len_asc",
  "CDS/Transcript % (high to low)" = "cds_tx_desc",
  "CDS/Transcript % (low to high)" = "cds_tx_asc",
  "Intronic bp (high to low)" = "intron_bp_desc",
  "Intronic bp (low to high)" = "intron_bp_asc",
  "Label (A-Z)" = "label_asc",
  "Label (Z-A)" = "label_desc"
)

analytics_exon_order_choices <- c(
  "Load order" = "load",
  "Exon count (high to low)" = "exon_desc",
  "Exon count (low to high)" = "exon_asc",
  "Intron count (high to low)" = "intron_desc",
  "Intron count (low to high)" = "intron_asc",
  "Exonic bp (high to low)" = "exonic_bp_desc",
  "Exonic bp (low to high)" = "exonic_bp_asc",
  "Intronic bp (high to low)" = "intron_bp_desc",
  "Intronic bp (low to high)" = "intron_bp_asc",
  "Label (A-Z)" = "label_asc",
  "Label (Z-A)" = "label_desc"
)

analytics_seq_order_choices <- c(
  "Load order" = "load",
  "GC% (high to low)" = "gc_desc",
  "GC% (low to high)" = "gc_asc",
  "A% (high to low)" = "a_desc",
  "A% (low to high)" = "a_asc",
  "T% (high to low)" = "t_desc",
  "T% (low to high)" = "t_asc",
  "C% (high to low)" = "c_desc",
  "C% (low to high)" = "c_asc",
  "G% (high to low)" = "g_desc",
  "G% (low to high)" = "g_asc",
  "Label (A-Z)" = "label_asc",
  "Label (Z-A)" = "label_desc"
)

analytics_context_order_choices <- c(
  "Load order" = "load",
  "Upstream distance |bp| (high to low)" = "up_abs_desc",
  "Upstream distance |bp| (low to high)" = "up_abs_asc",
  "Downstream distance |bp| (high to low)" = "down_abs_desc",
  "Downstream distance |bp| (low to high)" = "down_abs_asc",
  "Any overlap first" = "overlap_first",
  "Any overlap last" = "overlap_last",
  "Label (A-Z)" = "label_asc",
  "Label (Z-A)" = "label_desc"
)

analytics_exon_dist_order_choices <- c(
  "Load order" = "load",
  "Exon count (high to low)" = "exon_desc",
  "Exon count (low to high)" = "exon_asc",
  "Transcript length (longest first)" = "tx_len_desc",
  "Transcript length (shortest first)" = "tx_len_asc",
  "Gene length (longest first)" = "gene_len_desc",
  "Gene length (shortest first)" = "gene_len_asc",
  "Label (A-Z)" = "label_asc",
  "Label (Z-A)" = "label_desc"
)

analytics_intron_dist_order_choices <- c(
  "Load order" = "load",
  "Intron count (high to low)" = "intron_desc",
  "Intron count (low to high)" = "intron_asc",
  "Intronic bp (high to low)" = "intron_bp_desc",
  "Intronic bp (low to high)" = "intron_bp_asc",
  "Gene length (longest first)" = "gene_len_desc",
  "Gene length (shortest first)" = "gene_len_asc",
  "Label (A-Z)" = "label_asc",
  "Label (Z-A)" = "label_desc"
)

analytics_scatter_order_choices <- c(
  "Load order" = "load",
  "Draw largest genes on top" = "gene_len_desc",
  "Draw smallest genes on top" = "gene_len_asc",
  "Draw high GC% on top" = "gc_desc",
  "Draw low GC% on top" = "gc_asc",
  "Draw high exon-count on top" = "exon_desc",
  "Draw low exon-count on top" = "exon_asc",
  "Label (A-Z)" = "label_asc",
  "Label (Z-A)" = "label_desc"
)

analytics_heatmap_order_choices <- c(
  "Load order" = "load",
  "Gene length (longest first)" = "gene_len_desc",
  "Gene length (shortest first)" = "gene_len_asc",
  "Transcript length (longest first)" = "tx_len_desc",
  "Transcript length (shortest first)" = "tx_len_asc",
  "GC% (high to low)" = "gc_desc",
  "GC% (low to high)" = "gc_asc",
  "Exon count (high to low)" = "exon_desc",
  "Exon count (low to high)" = "exon_asc",
  "Label (A-Z)" = "label_asc",
  "Label (Z-A)" = "label_desc"
)

analytics_radar_order_choices <- c(
  "Load order" = "load",
  "Gene length (longest first)" = "gene_len_desc",
  "Gene length (shortest first)" = "gene_len_asc",
  "Transcript length (longest first)" = "tx_len_desc",
  "Transcript length (shortest first)" = "tx_len_asc",
  "GC% (high to low)" = "gc_desc",
  "GC% (low to high)" = "gc_asc",
  "CDS/Transcript % (high to low)" = "cds_tx_desc",
  "CDS/Transcript % (low to high)" = "cds_tx_asc",
  "Label (A-Z)" = "label_asc",
  "Label (Z-A)" = "label_desc"
)

analytics_corr_order_choices <- c(
  "Default metric order" = "metric_default",
  "Metric name (A-Z)" = "metric_alpha_asc",
  "Metric name (Z-A)" = "metric_alpha_desc",
  "By mean |r| (high to low)" = "mean_abs_corr_desc",
  "By mean |r| (low to high)" = "mean_abs_corr_asc",
  "Clustered (similar together)" = "cluster"
)

analytics_chart_toolbar <- function(container_id, filename, order_input_id, label = "SVG", order_width = "248px", choices = analytics_chart_order_choices) {
  selected_value <- if (length(choices) > 0) unname(as.character(choices[[1]])) else "load"
  if (!nzchar(selected_value)) selected_value <- "load"
  tags$div(
    class = "chart-export-tip chart-export-tip--with-order",
    tags$button(
      type = "button",
      class = "btn btn-sm btn-export-svg chart-export-btn",
      onclick = sprintf("exportSVG('%s', '%s');", container_id, filename),
      icon("download"),
      label
    ),
    div(
      class = "chart-order-inline",
      selectInput(
        inputId = order_input_id,
        label = NULL,
        choices = choices,
        selected = selected_value,
        width = order_width
      )
    )
  )
}

figure_studio_page <- function() {
  div(
    class = "content-wrapper app-main-pane figure-studio-page",
    `data-figure-studio-version` = cgv_release_version,
    div(
      class = "figure-studio-header",
      div(
        class = "figure-studio-header-main",
        div(
          class = "figure-studio-hero-icon",
          icon("object-group")
        ),
        div(
          class = "figure-studio-heading",
          div(
            class = "figure-studio-kicker",
            span("Publication workspace")
          ),
          h2("Figure Studio"),
          p("Build a publication-ready figure one independent chart panel at a time.")
        )
      ),
      div(
        class = "figure-studio-header-actions",
        tags$button(
          id = "figure-studio-guide-toggle",
          type = "button",
          class = "figure-studio-btn figure-studio-btn-guide",
          `data-figure-guide-open` = "true",
          `data-figure-tooltip` = "Open the five-step Figure Studio guide",
          `aria-controls` = "figure-studio-guide",
          `aria-expanded` = "false",
          icon("question-circle"),
          span("How it works")
        ),
        tags$button(
          id = "figure-studio-back",
          type = "button",
          class = "figure-studio-btn figure-studio-btn-secondary",
          `data-figure-tooltip` = "Return to the CGeV results without deleting this temporary draft",
          icon("arrow-left"),
          span("Back to results")
        ),
        tags$button(
          id = "figure-studio-new",
          type = "button",
          class = "figure-studio-btn figure-studio-btn-secondary",
          `data-figure-tooltip` = "Start a clean composition while preserving CGeV results",
          icon("file"),
          span("New figure")
        ),
        span(
          class = "figure-studio-tooltip-anchor",
          `data-figure-tooltip` = "Undo the latest Figure Studio edit",
          tags$button(
            id = "figure-studio-undo",
            type = "button",
            class = "figure-studio-icon-btn",
            title = "Undo",
            `aria-label` = "Undo",
            disabled = "disabled",
            icon("undo")
          )
        ),
        span(
          class = "figure-studio-tooltip-anchor",
          `data-figure-tooltip` = "Redo the latest undone edit",
          tags$button(
            id = "figure-studio-redo",
            type = "button",
            class = "figure-studio-icon-btn",
            title = "Redo",
            `aria-label` = "Redo",
            disabled = "disabled",
            icon("redo")
          )
        )
      )
    ),
    div(
      id = "figure-studio-guide",
      class = "figure-studio-guide",
      role = "region",
      `aria-label` = "Figure Studio step-by-step guide",
      hidden = "hidden",
      div(
        class = "figure-studio-guide-header",
        div(
          span(class = "figure-studio-pane-kicker", "Quick guide"),
          h3("Build a figure in five steps")
        ),
        tags$button(
          type = "button",
          class = "figure-studio-icon-btn",
          `data-figure-guide-close` = "true",
          `aria-label` = "Close Figure Studio guide",
          icon("times")
        )
      ),
      tags$ol(
        class = "figure-studio-guide-steps",
        tags$li(
          span(class = "figure-studio-guide-number", "1"),
          div(strong("Choose the context"), span("Select Multi-Gene or Cross-Species results in the Panel library."))
        ),
        tags$li(
          span(class = "figure-studio-guide-number", "2"),
          div(strong("Add one chart per panel"), span("Open a category and use + Add for every chart you want in the figure."))
        ),
        tags$li(
          span(class = "figure-studio-guide-number", "3"),
          div(strong("Arrange and refine"), span("Select a panel to change its title, width, height, order, or visibility."))
        ),
        tags$li(
          span(class = "figure-studio-guide-number", "4"),
          div(strong("Check the final output"), span("Use Preview to inspect the exact publication composition before exporting."))
        ),
        tags$li(
          span(class = "figure-studio-guide-number", "5"),
          div(strong("Export or keep working later"), span("Download SVG/PNG. Save the CGeV session if you want this draft restored later."))
        )
      )
    ),
    div(
      class = "figure-studio-toolbar",
      div(
        class = "figure-studio-title-column",
        div(
          class = "figure-studio-toolbar-intro",
          span(class = "figure-studio-toolbar-icon", icon("sliders-h")),
          div(
            span(class = "figure-studio-pane-kicker", "Figure setup"),
            strong("Shape the publication frame")
          )
        ),
        div(
          class = "figure-studio-title-fields",
          tags$label(
            class = "figure-studio-field figure-studio-field-title",
            span("Figure title"),
            tags$input(
              id = "figure-studio-title",
              type = "text",
              value = "CGeV comparative analysis",
              maxlength = "140",
              placeholder = "Optional — leave blank for no figure title",
              title = "Optional figure heading. Clear this field to export without a title.",
              autocomplete = "off"
            )
          ),
          tags$label(
            class = "figure-studio-field figure-studio-field-subtitle",
            span("Subtitle (optional)"),
            tags$input(
              id = "figure-studio-subtitle",
              type = "text",
              value = "",
              maxlength = "200",
              placeholder = "Study, cohort, method, or comparison",
              title = "Optional line shown beneath the figure title.",
              autocomplete = "off"
            )
          )
        )
      ),
      div(
        class = "figure-studio-layout-fields",
        tags$label(
          class = "figure-studio-field",
          title = "Set the publication grid to one, two, or three columns.",
          span("Columns"),
          tags$select(
            id = "figure-studio-columns",
            tags$option(value = "1", "1"),
            tags$option(value = "2", selected = "selected", "2"),
            tags$option(value = "3", "3")
          )
        ),
        tags$label(
          class = "figure-studio-field",
          title = "Apply the selected color profile to the preview and final export.",
          span("Figure style"),
          tags$select(
            id = "figure-studio-profile",
            tags$option(value = "color", "Full Color"),
            tags$option(value = "paper-color", "Paper Color"),
            tags$option(value = "colorblind", "Colorblind"),
            tags$option(value = "gray", "Paper Gray"),
            tags$option(value = "mono", "Paper Mono")
          )
        ),
        tags$label(
          class = "figure-studio-field",
          title = "Controls PNG raster resolution only. SVG remains vector.",
          span("PNG size"),
          tags$select(
            id = "figure-studio-resolution",
            tags$option(value = "1", "Screen (1×)"),
            tags$option(value = "2", selected = "selected", "High (2×)"),
            tags$option(value = "3", "Ultra (3×)")
          )
        )
      ),
      div(
        class = "figure-studio-export-actions",
        tags$button(
          id = "figure-studio-preview",
          type = "button",
          class = "figure-studio-btn figure-studio-btn-secondary",
          `data-figure-tooltip` = "Inspect the exact final composition before downloading",
          icon("eye"),
          span("Preview")
        ),
        tags$button(
          id = "figure-studio-export-svg",
          type = "button",
          class = "figure-studio-btn figure-studio-btn-primary",
          `data-figure-tooltip` = "Download an editable, publication-ready vector figure",
          icon("project-diagram"),
          span("Export SVG")
        ),
        tags$button(
          id = "figure-studio-export-png",
          type = "button",
          class = "figure-studio-btn figure-studio-btn-primary",
          `data-figure-tooltip` = "Download the final figure at the selected PNG size",
          icon("image"),
          span("Export PNG")
        )
      )
    ),
    div(
      class = "figure-studio-workspace",
      tags$aside(
        class = "figure-studio-library",
        div(
          class = "figure-studio-pane-heading",
          div(class = "figure-studio-pane-icon", icon("th-large")),
          div(
            class = "figure-studio-pane-title",
            span(class = "figure-studio-pane-kicker", "Panel library"),
            h3("Add one chart")
          ),
          tags$button(
            type = "button",
            class = "figure-studio-help-dot",
            `data-figure-guide-open` = "true",
            `data-figure-tooltip` = "Open the step-by-step guide",
            `aria-controls` = "figure-studio-guide",
            `aria-expanded` = "false",
            `aria-label` = "Open Figure Studio guide",
            "?"
          )
        ),
        p(class = "figure-studio-pane-copy", "Choose a data context, then add any available visualization as a new panel."),
        tags$label(
          class = "figure-studio-field figure-studio-field-block",
          title = "Choose which CGeV workflow supplies charts to the library.",
          span("Data context"),
          tags$select(
            id = "figure-studio-context",
            tags$option(value = "homo", "Multi-Gene Search"),
            tags$option(value = "ortho", "Cross-Species Search")
          )
        ),
        tags$label(
          class = "figure-studio-search",
          icon("search"),
          tags$input(
            id = "figure-studio-catalog-search",
            type = "search",
            placeholder = "Find a chart...",
            autocomplete = "off"
          )
        ),
        div(id = "figure-studio-catalog", class = "figure-studio-catalog"),
        div(
          class = "figure-studio-library-note",
          icon("info-circle"),
          span("Unavailable charts remain visible and explain which CGeV result must be generated first.")
        )
      ),
      div(
        class = "figure-studio-canvas-shell",
        div(
          class = "figure-studio-canvas-topline",
          div(
            class = "figure-studio-canvas-meta",
            span(class = "figure-studio-canvas-kicker", "Composition"),
            strong(id = "figure-studio-panel-summary", "0 panels"),
            span(
              id = "figure-studio-save-status",
              title = "This temporary draft is kept only when you save the CGeV session.",
              "Temporary draft"
            )
          ),
          div(
            class = "figure-studio-canvas-actions",
            tags$button(
              id = "figure-studio-add-panel",
              type = "button",
              class = "figure-studio-btn figure-studio-btn-secondary",
              `data-figure-tooltip` = "Jump to the Panel library and choose another chart",
              icon("plus"),
              span("Add panel")
            ),
            tags$button(
              id = "figure-studio-clear",
              type = "button",
              class = "figure-studio-btn figure-studio-btn-danger",
              `data-figure-tooltip` = "Remove every panel from this composition",
              icon("trash"),
              span("Clear")
            )
          )
        ),
        div(
          id = "figure-studio-empty",
          class = "figure-studio-empty",
          div(class = "figure-studio-empty-icon", icon("object-group")),
          span(class = "figure-studio-empty-kicker", "Publication canvas"),
          h3("Start with an empty publication canvas"),
          p("Add as many panels as the figure needs. Each panel contains one chart and can be reordered, resized, duplicated, or removed."),
          tags$button(
            type = "button",
            class = "figure-studio-btn figure-studio-btn-primary",
            `data-figure-empty-add` = "true",
            icon("plus"),
            span("Add first panel")
          )
        ),
        div(
          id = "figure-studio-canvas",
          class = "figure-studio-canvas",
          `aria-label` = "Publication figure canvas"
        ),
        div(
          id = "figure-studio-size-warning",
          class = "figure-studio-size-warning",
          hidden = "hidden",
          icon("exclamation-triangle"),
          span()
        )
      ),
      tags$aside(
        class = "figure-studio-inspector",
        div(
          class = "figure-studio-pane-heading",
          div(class = "figure-studio-pane-icon", icon("sliders-h")),
          div(
            class = "figure-studio-pane-title",
            span(class = "figure-studio-pane-kicker", "Selected panel"),
            h3(id = "figure-studio-inspector-title", "No panel selected")
          )
        ),
        div(
          id = "figure-studio-inspector-empty",
          class = "figure-studio-inspector-empty",
          icon("mouse-pointer"),
          p("Select a panel to edit its title, size, position, and source.")
        ),
        div(
          id = "figure-studio-inspector-form",
          class = "figure-studio-inspector-form",
          hidden = "hidden",
          tags$label(
            class = "figure-studio-field figure-studio-field-block",
            title = "This heading appears beside the automatic A, B, C… panel label. Scientific names such as Homo sapiens are italicized automatically in preview and export.",
            span("Panel title"),
            tags$input(
              id = "figure-studio-panel-title",
              type = "text",
              maxlength = "120",
              placeholder = "Example: TP53 in Homo sapiens",
              autocomplete = "off"
            )
          ),
          div(
            class = "figure-studio-source-readout",
            span("Chart source"),
            strong(id = "figure-studio-panel-source", "—")
          ),
          tags$label(
            class = "figure-studio-field figure-studio-field-block",
            title = "Choose how many grid columns this panel occupies.",
            span("Panel width"),
            tags$select(
              id = "figure-studio-panel-span",
              tags$option(value = "1", "1 column"),
              tags$option(value = "2", "2 columns"),
              tags$option(value = "3", "3 columns")
            )
          ),
          tags$label(
            class = "figure-studio-field figure-studio-field-block",
            title = "Auto preserves the chart proportions; manual sizes intentionally compress or expand it.",
            span("Panel height"),
            tags$select(
              id = "figure-studio-panel-height",
              tags$option(value = "auto", selected = "selected", "Auto · fit chart"),
              tags$option(value = "compact", "Compact"),
              tags$option(value = "standard", "Standard"),
              tags$option(value = "tall", "Tall")
            )
          ),
          tags$label(
            class = "figure-studio-check-field",
            tags$input(id = "figure-studio-panel-show-title", type = "checkbox", checked = "checked"),
            span("Show panel label and title")
          ),
          p(class = "figure-studio-inspector-note", "Auto uses the chart's real proportions and adapts when its number of genes or transcripts changes. Manual heights remain available for intentional compression."),
          div(
            class = "figure-studio-panel-order-actions",
            tags$button(
              id = "figure-studio-move-back",
              type = "button",
              class = "figure-studio-btn figure-studio-btn-secondary",
              icon("arrow-left"),
              span("Earlier")
            ),
            tags$button(
              id = "figure-studio-move-forward",
              type = "button",
              class = "figure-studio-btn figure-studio-btn-secondary",
              span("Later"),
              icon("arrow-right")
            )
          ),
          tags$button(
            id = "figure-studio-duplicate",
            type = "button",
            class = "figure-studio-btn figure-studio-btn-secondary figure-studio-btn-block",
            icon("clone"),
            span("Duplicate panel")
          ),
          tags$button(
            id = "figure-studio-remove",
            type = "button",
            class = "figure-studio-btn figure-studio-btn-danger figure-studio-btn-block",
            icon("trash"),
            span("Remove panel")
          )
        )
      )
    ),
    tags$input(id = "figure_studio_state", type = "hidden", value = ""),
    div(
      id = "figure-studio-preview-modal",
      class = "figure-studio-preview-modal",
      role = "dialog",
      `aria-modal` = "true",
      `aria-hidden` = "true",
      `aria-labelledby` = "figure-studio-preview-title",
      hidden = "hidden",
      div(
        class = "figure-studio-preview-backdrop",
        `data-figure-preview-close` = "true"
      ),
      div(
        class = "figure-studio-preview-dialog",
        div(
          class = "figure-studio-preview-header",
          div(
            span(class = "figure-studio-pane-kicker", "Final output"),
            h3(id = "figure-studio-preview-title", "Export preview"),
            p("This uses the same composition and SVG source as the exported figure.")
          ),
          tags$button(
            id = "figure-studio-preview-close",
            type = "button",
            class = "figure-studio-icon-btn",
            title = "Close preview",
            `aria-label` = "Close preview",
            icon("times")
          )
        ),
        div(
          class = "figure-studio-preview-info",
          span(id = "figure-studio-preview-meta", "Preparing preview…"),
          span("Zoom with the browser or scroll to inspect details.")
        ),
        div(
          class = "figure-studio-preview-stage",
          div(
            id = "figure-studio-preview-paper",
            class = "figure-studio-preview-paper"
          )
        ),
        div(
          class = "figure-studio-preview-footer",
          span("If this looks correct, export it directly from here."),
          div(
            class = "figure-studio-preview-actions",
            tags$button(
            id = "figure-studio-preview-export-svg",
            type = "button",
            class = "figure-studio-btn figure-studio-btn-secondary",
            `data-figure-tooltip` = "Download this exact preview as SVG",
            icon("project-diagram"),
              span("Export SVG")
            ),
            tags$button(
            id = "figure-studio-preview-export-png",
            type = "button",
            class = "figure-studio-btn figure-studio-btn-primary",
            `data-figure-tooltip` = "Download this exact preview as PNG",
            icon("image"),
              span("Export PNG")
            )
          )
        )
      )
    ),
    div(
      id = "figure-studio-toast",
      class = "figure-studio-toast",
      role = "status",
      `aria-live` = "polite",
      hidden = "hidden"
    )
  )
}

analytics_export_bank <- function(prefix) {
  ids <- paste0(
    prefix,
    c(
      "_arch_chart_export", "_exon_chart_export", "_seq_chart_export",
      "_context_chart_export", "_exon_dist_chart_export", "_intron_dist_chart_export", "_scatter_chart_export",
      "_heatmap_chart_export", "_radar_chart_export", "_corr_chart_export"
    )
  )
  tags$div(
    id = paste0(prefix, "_analytics_export_bank"),
    class = "analytics-export-bank",
    `aria-hidden` = "true",
    style = paste(
      "position:absolute;left:-10000px;top:0;width:1200px;",
      "max-width:1200px;overflow:hidden;opacity:0.01;",
      "pointer-events:none;z-index:-1;"
    ),
    lapply(ids, function(output_id) {
      tags$div(
        id = paste0(output_id, "_wrap"),
        style = "width:1200px;min-height:480px;",
        ggiraph::girafeOutput(output_id, height = "auto", width = "100%")
      )
    })
  )
}

analytics_scatter_tip_html <- paste0(
  "<strong>GC Content vs Gene Length</strong><br/>",
  "Bubble scatter built from the analytics summary table. Each point is one loaded entry.",
  "<div class='ci-axes'><b>X-axis:</b> GC content (%) [0&ndash;100]<br/>",
  "<b>Y-axis:</b> Gene length (bp, >0)<br/>",
  "<b>Bubble size:</b> Exon count (larger = more exons)</div>",
  "<div class='ci-formula'><b>GC formula:</b> GC% = ((G + C) / (A + T + G + C)) &times; 100<br/>",
  "<b>Where:</b> G,C,A,T = counts of each nucleotide in the sequence; denominator = total nucleotide count.</div>",
  "<div class='ci-ranges'><b>How to read:</b> upper-right = long + GC-rich genes; lower-left = compact + low-GC genes. ",
  "<b>Draw order</b> only changes which bubbles are visible on top when points overlap.</div>"
)

analytics_heatmap_tip_html <- paste0(
  "<strong>Gene Metrics Heatmap</strong><br/>",
  "Compares 8 metrics across loaded entries: Gene length, representative transcript length, exons, introns, GC%, CDS/Tx%, exonic bp, and CDS bp.",
  "<div class='ci-axes'><b>X-axis:</b> Loaded entries<br/>",
  "<b>Y-axis:</b> Metrics<br/><b>Color:</b> Blue = lower than group average, White = near average, Red = higher</div>",
  "<div class='ci-formula'><b>Z-score:</b> Z = (x &minus; &mu;) / &sigma;<br/>",
  "<b>Where:</b> x = this gene value; &mu; = mean across loaded genes; &sigma; = standard deviation across loaded genes.</div>",
  "<div class='ci-ranges'><b>Range guide:</b> Z = 0 mean; +1/+2 above mean; &minus;1/&minus;2 below mean.</div>",
  "<div class='ci-tip'>bp metrics (gene length, representative transcript length, exonic bp, CDS bp) are log&#8321;&#8320;-transformed before Z-scoring. Requires &ge;2 genes.</div>"
)

analytics_radar_tip_html <- paste0(
  "<strong>Gene Profile Radar</strong><br/>",
  "Spider plot across 6 metrics: Gene length, representative transcript length, exons, introns, GC%, and CDS/Transcript%.",
  "<div class='ci-axes'><b>Scale:</b> normalized 0&ndash;1 on each axis independently<br/>",
  "<b>Rings:</b> 25%, 50%, 75%, 100%</div>",
  "<div class='ci-formula'><b>Min&ndash;max normalization:</b> value = (x &minus; min) / (max &minus; min)<br/>",
  "<b>Where:</b> x = this gene value in one metric; min/max = smallest/largest value for that same metric across loaded genes.</div>",
  "<div class='ci-ranges'><b>Range guide:</b> 0 = lowest value in the loaded set, 1 = highest, 0.5 = midpoint.</div>",
  "<div class='ci-tip'>Larger and rounder polygons indicate broadly higher values across metrics. Requires &ge;2 genes with complete metrics.</div>"
)

analytics_corr_tip_html <- paste0(
  "<strong>Pairwise Correlation Matrix</strong><br/>",
  "Pearson correlation between the same 8 structural metrics across loaded entries.",
  "<div class='ci-axes'><b>Axes:</b> Metric names on X and Y<br/>",
  "<b>Color:</b> Blue = negative, White = near 0, Red = positive<br/>",
  "<b>Cell value:</b> r from &minus;1 to +1</div>",
  "<div class='ci-formula'><b>Pearson formula:</b> r = cov(X,Y) / (&sigma;<sub>X</sub>&sigma;<sub>Y</sub>)<br/>",
  "<b>Where:</b> X,Y = two metrics being compared; cov(X,Y) = covariance; &sigma;<sub>X</sub>, &sigma;<sub>Y</sub> = standard deviations of X and Y.</div>",
  "<div class='ci-ranges'><b>r bands:</b> 0.9&ndash;1.0 very strong (+), 0.7&ndash;0.9 strong, 0.5&ndash;0.7 moderate, 0.0&ndash;0.3 negligible; ",
  "&minus;0.3&ndash;0.0 negligible (&minus;), &minus;0.7&ndash;&minus;0.3 weak/moderate (&minus;), &minus;1.0&ndash;&minus;0.7 strong/very strong (&minus;).</div>",
  "<div class='ci-tip'>Diagonal cells are always 1.00. bp metrics are log&#8321;&#8320;-transformed before correlation. Requires &ge;3 genes.</div>"
)

get_initial_ready_preloaded_registry <- function() {
  reg <- tryCatch(
    get_preloaded_species_registry(registry_path = file.path("annotations", "registry.tsv"), base_dir = "."),
    error = function(e) data.frame()
  )
  if (is.null(reg) || !is.data.frame(reg) || nrow(reg) == 0) {
    return(data.frame())
  }
  if (!"ready" %in% colnames(reg)) {
    reg$ready <- TRUE
  }
  reg[as.logical(reg$ready), , drop = FALSE]
}

build_initial_species_grouped_grid <- function(df_in, input_id, mode) {
  if (is.null(df_in) || !is.data.frame(df_in) || nrow(df_in) == 0) {
    return(list(div(
      class = "species-grid-empty",
      "No installed organisms yet. Install organisms in Settings > Organisms, use NCBI Search, or upload your own files."
    )))
  }
  if (!"kingdom" %in% colnames(df_in)) df_in$kingdom <- ""
  kingdom_order <- c("Animalia", "Plantae", "Fungi")
  df_in$kingdom[!nzchar(as.character(df_in$kingdom))] <- "Other"
  kingdoms_present <- intersect(kingdom_order, unique(as.character(df_in$kingdom)))
  leftover <- setdiff(unique(as.character(df_in$kingdom)), kingdom_order)
  kingdoms_present <- c(kingdoms_present, leftover)

  sections <- lapply(kingdoms_present, function(kg) {
    sub_df <- df_in[as.character(df_in$kingdom) == kg, , drop = FALSE]
    if (nrow(sub_df) == 0) return(NULL)

    cards <- lapply(seq_len(nrow(sub_df)), function(i) {
      sid <- as.character(sub_df$species_id[i] %||% "")
      org <- as.character(sub_df$organism[i] %||% sub_df$label[i] %||% "Unknown")
      icn <- as.character(sub_df$icon_url[i] %||% "/icons/DNA.ico")
      div(
        class = "species-grid-card",
        `data-species-id` = sid,
        role = "button",
        tabindex = "0",
        style = "cursor:pointer;",
        div(
          class = "species-grid-icon-wrap",
          tags$img(
            class = "species-grid-icon-img",
            src = icn,
            alt = org,
            loading = "lazy"
          )
        ),
        div(
          class = "species-grid-text",
          span(class = "species-grid-name", org)
        )
      )
    })

    div(
      class = "species-kingdom-group",
      div(class = "species-kingdom-divider"),
      div(class = "species-kingdom-header", kg),
      div(
        class = "species-grid",
        `data-input-id` = input_id,
        `data-mode` = mode,
        cards
      )
    )
  })
  Filter(Negate(is.null), sections)
}

initial_ready_preloaded_registry <- get_initial_ready_preloaded_registry()
initial_homo_species_grid <- tagList(
  div(
    class = "species-grid-shell species-grid-shell-initial",
    build_initial_species_grouped_grid(initial_ready_preloaded_registry, "homo_preloaded_species", "single")
  )
)
initial_ortho_species_grid <- tagList(
  div(
    class = "species-grid-shell species-grid-shell-initial",
    build_initial_species_grouped_grid(initial_ready_preloaded_registry, "ortho_preloaded_species", "multi")
  )
)

source("R/ui_desktop_downloads.R", local = TRUE)
figure_studio_loader_source <- paste(
  readLines(file.path("www", "js", "figure_studio_loader.js"), warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

fluidPage(
  theme = theme_custom,
  tagList(
    # Configuración general
    tags$head(
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1, viewport-fit=cover"),
      tags$meta(name = "cgv-app-version", content = app_asset_version),
      tags$meta(`http-equiv` = "Cache-Control", content = "no-cache, must-revalidate"),
      tags$meta(`http-equiv` = "Pragma", content = "no-cache"),
      tags$meta(`http-equiv` = "Expires", content = "0"),
      tags$style(HTML("
        .isoform-toggle-label {
          margin:0;
          display:inline-flex;
          align-items:center;
          gap:4px;
        }
        .isoform-toggle-checkbox {
          position:relative !important;
          width:14px !important;
          height:14px !important;
          margin:0 !important;
          opacity:1 !important;
          pointer-events:auto !important;
          cursor:pointer !important;
          accent-color:#4B6072;
        }
        .isoform-toggle-up { display:none; }
        .isoform-toggle-checkbox:checked ~ .isoform-toggle-down { display:none; }
        .isoform-toggle-checkbox:checked ~ .isoform-toggle-up { display:inline; }
        .isoform-toggle-checkbox:focus-visible ~ .isoform-toggle-text {
          outline:2px solid #2f80ed;
          outline-offset:2px;
          border-radius:3px;
        }
      ")),
      # These Analytics rules historically lived in figure_studio.css. Keep
      # their exact eager behavior while the Studio-only stylesheet is loaded
      # on demand.
      tags$style(HTML("
        #homo_analytics_tabs > .nav,
        #ortho_analytics_tabs > .nav {
          padding-right: 112px;
        }
        .btn-analytics-download-all[aria-busy=\"true\"] {
          min-width: 142px;
          border-color: #78b9ad !important;
          background: #edf8f5 !important;
          color: #176f60 !important;
          cursor: progress !important;
          opacity: 1 !important;
        }
        .btn-analytics-download-all[aria-busy=\"true\"] .fa {
          color: #18a98c;
        }
        @media (max-width: 1080px) {
          .btn-analytics-download-all {
            position: static !important;
            margin: 0 5px 6px 0 !important;
          }
          #homo_analytics_tabs > .nav,
          #ortho_analytics_tabs > .nav {
            padding-right: 0;
          }
        }
      ")),
      tags$script(HTML("
        (function() {
          function formatBytes(value) {
            var n = Number(value || 0);
            if (!isFinite(n) || n <= 0) return '';
            var units = ['B', 'KB', 'MB', 'GB', 'TB'];
            var unit = 0;
            while (n >= 1024 && unit < units.length - 1) {
              n = n / 1024;
              unit += 1;
            }
            return n.toFixed(n >= 10 || unit === 0 ? 0 : 1) + ' ' + units[unit];
          }

          function setText(id, text) {
            var el = document.getElementById(id);
            if (el) el.textContent = text || '';
          }

          var desktopOrganismStatusTimer = null;

          function setDesktopOrganismStatus(text, clearAfterMs) {
            if (desktopOrganismStatusTimer) {
              window.clearTimeout(desktopOrganismStatusTimer);
              desktopOrganismStatusTimer = null;
            }
            setText('desktop-organism-status', text || '');
            if (text && clearAfterMs) {
              desktopOrganismStatusTimer = window.setTimeout(function() {
                setText('desktop-organism-status', '');
                desktopOrganismStatusTimer = null;
              }, clearAfterMs);
            }
          }

          function formatEta(value) {
            var seconds = Number(value || 0);
            if (!isFinite(seconds) || seconds <= 0) return '';
            if (seconds < 60) return Math.ceil(seconds) + 's';
            var minutes = Math.ceil(seconds / 60);
            return minutes + 'm';
          }

          function statusLabel(status) {
            var map = {
              installed: 'Installed',
              not_installed: 'Not installed',
              partial: 'Partial',
              update_available: 'Update available',
              bundled: 'Bundled'
            };
            return map[status] || status || 'Unknown';
          }

          function phaseLabel(phase) {
            var map = {
              preparing: 'Preparing',
              connecting: 'Connecting',
              downloading: 'Downloading',
              verifying: 'Verifying',
              extracting: 'Extracting',
              installing_cache: 'Installing cache',
              cancelling: 'Cancelling',
              cancelled: 'Cancelled',
              complete: 'Complete',
              error: 'Error'
            };
            return map[phase] || phase || '';
          }

          var desktopDatasetProgress = {};
          var desktopDatasetManifest = null;
          var desktopDatasetRemovalSelection = new Set();
          var desktopDatasetRefreshPromise = null;

          function isInstalledDesktopDataset(item) {
            var status = item && item.local && item.local.status || '';
            return status === 'installed' || status === 'update_available' || status === 'bundled';
          }

          function isRemovableDesktopDataset(item) {
            var status = item && item.local && item.local.status || '';
            return item && item.downloadable !== false && (status === 'installed' || status === 'update_available');
          }

          function hasActiveDesktopDatasetProgress() {
            return Object.keys(desktopDatasetProgress).some(function(datasetId) {
              var phase = desktopDatasetProgress[datasetId] && desktopDatasetProgress[datasetId].phase || '';
              return phase && phase !== 'complete' && phase !== 'error' && phase !== 'cancelled';
            });
          }

          function clearDesktopOrganismStatusIfIdle() {
            if (!hasActiveDesktopDatasetProgress()) setDesktopOrganismStatus('');
          }

          function iconForDataset(item) {
            var label = String(item.label || '').trim();
            var speciesId = String(item.speciesId || item.id || '').toLowerCase();
            var normalizedLabel = label.toLowerCase();
            var iconAliases = [
              { test: function() { return normalizedLabel.indexOf('botrytis cinerea') === 0 || speciesId.indexOf('botrytis_cinerea') === 0; }, icon: 'icons/Botrytis cinerea.ico' },
              { test: function() { return normalizedLabel.indexOf('fragaria vesca') === 0 || speciesId.indexOf('fragaria_vesca') === 0; }, icon: 'icons/Fragaria vesca.ico' },
              { test: function() { return normalizedLabel.indexOf('oryza sativa indica') === 0 || speciesId.indexOf('oryza_sativa_indica') === 0; }, icon: 'icons/Oryza sativa indica.ico' },
              { test: function() { return normalizedLabel.indexOf('oryza sativa ssp. japonica') === 0 || speciesId.indexOf('oryza_sativa_ssp_japonica') === 0; }, icon: 'icons/Oryza sativa japonica.ico' }
            ];
            if (item.icon) return String(item.icon).replace(/^\\/+/, '');
            for (var i = 0; i < iconAliases.length; i += 1) {
              if (iconAliases[i].test()) return iconAliases[i].icon;
            }
            if (label) return 'icons/' + label + '.ico';
            return 'icons/DNA.ico';
          }

          function updateDesktopDatasetProgress(datasetId, payload) {
            if (!datasetId) return;
            desktopDatasetProgress[datasetId] = Object.assign({}, desktopDatasetProgress[datasetId] || {}, payload || {});
            var escapedId = window.CSS && CSS.escape ? CSS.escape(datasetId) : String(datasetId).replace(/\"/g, '\\\\\"');
            var card = document.querySelector('[data-desktop-dataset-id=\"' + escapedId + '\"]');
            if (!card) return;
            applyDesktopDatasetProgress(card, desktopDatasetProgress[datasetId]);
          }

          function applyDesktopDatasetProgress(card, progress) {
            progress = progress || {};
            var phase = progress.phase || (progress.skipped ? 'complete' : '');
            var percent = progress.percent == null ? null : Math.max(0, Math.min(1, Number(progress.percent)));
            var fill = card.querySelector('.desktop-organism-progress-fill');
            var text = card.querySelector('.desktop-organism-progress-text');
            var button = card.querySelector('.desktop-organism-download');
            var status = card.querySelector('.desktop-organism-status-pill');
            var active = phase && phase !== 'complete' && phase !== 'error' && phase !== 'cancelled';
            var cancellable = ['preparing', 'connecting', 'downloading', 'verifying'].indexOf(phase) !== -1;
            card.classList.toggle('is-downloading', active);
            card.classList.toggle('has-error', phase === 'error');
            if (fill) {
              fill.style.width = percent == null ? '28%' : Math.round(percent * 100) + '%';
              fill.classList.toggle('is-indeterminate', percent == null && active);
            }
            if (text) {
              var bits = [];
              if (phase) bits.push(phaseLabel(phase));
              if (progress.message) bits.push(progress.message);
              if (percent != null && phase !== 'complete') bits.push(Math.round(percent * 100) + '%');
              if (progress.speed && phase === 'downloading') bits.push(formatBytes(progress.speed) + '/s');
              if (progress.eta && phase === 'downloading') bits.push(formatEta(progress.eta) + ' left');
              text.textContent = bits.join(' · ');
            }
            if (status && phase) status.textContent = phase === 'complete' ? 'Installed' : phaseLabel(phase);
            if (button) {
              button.dataset.action = cancellable ? 'cancel' : (active ? 'wait' : 'download');
              button.textContent = cancellable ? 'Cancel' : (phase === 'cancelling' ? 'Cancelling...' : (active ? 'Working...' : button.dataset.defaultLabel));
              button.disabled = (active && !cancellable) || (!active && button.dataset.downloadable !== 'true');
            }
          }

          function renderDesktopDatasets(manifest) {
            var root = document.getElementById('desktop-organism-list');
            if (!root) return;
            var datasets = (manifest && manifest.datasets) || [];
            desktopDatasetManifest = manifest || { datasets: [] };
            root.textContent = '';
            var installedCount = datasets.filter(isInstalledDesktopDataset).length;
            var pendingCount = Math.max(0, datasets.length - installedCount);
            var removableCount = datasets.filter(isRemovableDesktopDataset).length;
            setText('desktop-organism-count', datasets.length ? datasets.length + ' available' : 'No catalog configured');
            setText('desktop-organism-installed-count', installedCount ? installedCount + ' installed' : 'No organisms installed');
            setText('desktop-organism-pending-count', pendingCount ? pendingCount + ' not installed' : 'Catalog complete');
            var resetBtn = document.getElementById('desktop-organism-reset');
            if (resetBtn) {
              resetBtn.disabled = removableCount === 0;
              resetBtn.title = removableCount
                ? 'Choose one or more installed organisms to remove'
                : 'No downloaded organisms are installed';
            }
            var dataPath = document.getElementById('desktop-organism-data-path');
            var cachePath = document.getElementById('desktop-organism-cache-path');
            if (dataPath && manifest && manifest.dataRoot) dataPath.textContent = manifest.dataRoot;
            if (cachePath && manifest && manifest.cacheRoot) cachePath.textContent = manifest.cacheRoot;

            if (!window.cgvDesktop) {
              root.innerHTML = '<div class=\"desktop-organism-empty\">Desktop organism downloads are available only in CGeV Desktop.</div>';
              return;
            }
            if (!datasets.length) {
              root.innerHTML = '<div class=\"desktop-organism-empty\">No downloadable organism catalog is configured yet. Add a catalog URL to desktop/data-manifest.json or CGV_DESKTOP_CATALOG_URL.</div>';
              return;
            }

            renderDesktopOrganismHighlights(datasets);
            renderDesktopOrganismModalList();
            renderDesktopOrganismRemovalList();
          }

          function renderDesktopOrganismHighlights(datasets) {
            var root = document.getElementById('desktop-organism-list');
            if (!root) return;
            root.textContent = '';
            var installed = datasets.filter(isInstalledDesktopDataset).slice(0, 6);
            if (!installed.length) {
              root.innerHTML = '<div class=\"desktop-organism-empty\">No organisms installed yet. Open the catalog to download the references you need.</div>';
              return;
            }
            installed.forEach(function(item) {
              var chip = document.createElement('span');
              chip.className = 'desktop-organism-chip';
              var img = document.createElement('img');
              img.alt = '';
              img.src = iconForDataset(item);
              img.onerror = function() { this.onerror = null; this.src = 'icons/DNA.ico'; };
              var text = document.createElement('span');
              text.className = 'desktop-organism-scientific-name';
              text.textContent = item.label || item.id;
              chip.append(img, text);
              root.append(chip);
            });
            if (datasets.filter(isInstalledDesktopDataset).length > installed.length) {
              var more = document.createElement('span');
              more.className = 'desktop-organism-chip desktop-organism-chip-muted';
              more.textContent = '+' + (datasets.filter(isInstalledDesktopDataset).length - installed.length) + ' more';
              root.append(more);
            }
          }

          function activeDesktopDatasetFilter() {
            var search = document.getElementById('desktop-organism-search');
            var status = document.getElementById('desktop-organism-filter');
            return {
              query: String(search && search.value || '').trim().toLowerCase(),
              status: String(status && status.value || 'all')
            };
          }

          function matchesDesktopDatasetFilter(item, filter) {
            var status = item.local && item.local.status || 'not_installed';
            if (filter.status !== 'all') {
              if (filter.status === 'installed' && !(status === 'installed' || status === 'bundled')) return false;
              if (filter.status === 'available' && (status === 'installed' || status === 'bundled')) return false;
              if (filter.status === 'not_installed' && status !== 'not_installed') return false;
              if (filter.status === 'updates' && status !== 'update_available') return false;
            }
            if (!filter.query) return true;
            var haystack = [item.label, item.id, item.speciesId, item.description, item.version].filter(Boolean).join(' ').toLowerCase();
            return haystack.indexOf(filter.query) !== -1;
          }

          function renderDesktopOrganismModalList() {
            var modalList = document.getElementById('desktop-organism-modal-list');
            if (!modalList) return;
            var datasets = desktopDatasetManifest && desktopDatasetManifest.datasets || [];
            var filter = activeDesktopDatasetFilter();
            var visible = datasets.filter(function(item) { return matchesDesktopDatasetFilter(item, filter); });
            modalList.textContent = '';
            setText('desktop-organism-modal-count', visible.length + ' shown');
            if (!visible.length) {
              modalList.innerHTML = '<div class=\"desktop-organism-empty\">No organisms match this search.</div>';
              return;
            }

            visible.forEach(function(item) {
              var status = item.local && item.local.status || 'not_installed';
              var card = document.createElement('article');
              card.className = 'desktop-organism-card desktop-organism-card-' + status;
              card.dataset.desktopDatasetId = item.id || '';

              var iconWrap = document.createElement('div');
              iconWrap.className = 'desktop-organism-icon-wrap';
              var iconImg = document.createElement('img');
              iconImg.className = 'desktop-organism-icon';
              iconImg.alt = '';
              iconImg.src = iconForDataset(item);
              iconImg.onerror = function() { this.onerror = null; this.src = 'icons/DNA.ico'; };
              iconWrap.append(iconImg);

              var body = document.createElement('div');
              body.className = 'desktop-organism-card-body';

              var title = document.createElement('strong');
              title.className = 'desktop-organism-title desktop-organism-scientific-name';
              title.textContent = item.label || item.id;

              var details = document.createElement('span');
              details.className = 'desktop-organism-details';
              var size = formatBytes(item.sizeBytes || item.package && item.package.sizeBytes);
              var version = item.version ? 'v' + item.version : '';
              details.textContent = [size, version, item.speciesId || item.id].filter(Boolean).join(' · ');

              var meta = document.createElement('code');
              meta.className = 'desktop-organism-meta';
              meta.textContent = item.description || '';

              var progress = document.createElement('div');
              progress.className = 'desktop-organism-progress';
              var progressTrack = document.createElement('span');
              progressTrack.className = 'desktop-organism-progress-track';
              var progressFill = document.createElement('span');
              progressFill.className = 'desktop-organism-progress-fill';
              progressTrack.append(progressFill);
              var progressText = document.createElement('span');
              progressText.className = 'desktop-organism-progress-text';
              progress.append(progressTrack, progressText);
              body.append(title, details, meta, progress);

              var actions = document.createElement('div');
              actions.className = 'desktop-organism-actions';
              var pill = document.createElement('span');
              pill.className = 'desktop-organism-status-pill desktop-organism-status-pill-' + status;
              pill.textContent = statusLabel(status);

              var button = document.createElement('button');
              button.type = 'button';
              button.className = 'btn btn-sm btn-download desktop-organism-download';
              var downloadable = item.downloadable !== false;
              button.dataset.downloadable = downloadable ? 'true' : 'false';
              button.textContent = downloadable
                ? (status === 'installed' ? 'Verify' : 'Download')
                : 'Bundled';
              button.dataset.defaultLabel = button.textContent;
              button.dataset.action = 'download';
              button.disabled = !downloadable;

              button.addEventListener('click', function() {
                if (!downloadable || !window.cgvDesktop) return;
                if (button.dataset.action === 'cancel' && window.cgvDesktop.cancelDatasetDownload) {
                  updateDesktopDatasetProgress(item.id, { phase: 'cancelling', percent: null });
                  window.cgvDesktop.cancelDatasetDownload(item.id);
                  return;
                }
                updateDesktopDatasetProgress(item.id, { phase: 'preparing', percent: null });
                setDesktopOrganismStatus('Preparing ' + (item.label || item.id) + '...');
                window.cgvDesktop.downloadDataset(item.id).then(function(result) {
                  if (result && result.canceled) {
                    updateDesktopDatasetProgress(item.id, { phase: 'cancelled', message: 'Download canceled.' });
                    setDesktopOrganismStatus('Download canceled.', 3000);
                    return null;
                  }
                  if (window.Shiny) {
                    Shiny.setInputValue('desktop_dataset_installed', { id: item.id, at: Date.now() }, { priority: 'event' });
                  }
                  return window.cgvDesktop.listDatasets();
                }).then(function(manifest) {
                  if (manifest) {
                    renderDesktopDatasets(manifest);
                    delete desktopDatasetProgress[item.id];
                    clearDesktopOrganismStatusIfIdle();
                  }
                }).catch(function(error) {
                  updateDesktopDatasetProgress(item.id, { phase: 'error', percent: null });
                  setDesktopOrganismStatus(error && error.message ? error.message : 'Download failed.');
                });
              });

              actions.append(pill, button);
              card.append(iconWrap, body, actions);
              modalList.append(card);
              if (desktopDatasetProgress[item.id]) applyDesktopDatasetProgress(card, desktopDatasetProgress[item.id]);
            });
          }

          function removableDesktopDatasets() {
            var datasets = desktopDatasetManifest && desktopDatasetManifest.datasets || [];
            return datasets.filter(isRemovableDesktopDataset);
          }

          function updateDesktopOrganismRemovalControls(removable) {
            removable = removable || removableDesktopDatasets();
            var removableIds = new Set(removable.map(function(item) { return item.id; }));
            desktopDatasetRemovalSelection.forEach(function(datasetId) {
              if (!removableIds.has(datasetId)) desktopDatasetRemovalSelection.delete(datasetId);
            });
            var selectedCount = desktopDatasetRemovalSelection.size;
            var selectAll = document.getElementById('desktop-organism-remove-all');
            var removeBtn = document.getElementById('desktop-organism-remove-selected');
            if (selectAll) {
              selectAll.disabled = removable.length === 0;
              selectAll.checked = removable.length > 0 && selectedCount === removable.length;
              selectAll.indeterminate = selectedCount > 0 && selectedCount < removable.length;
            }
            if (removeBtn) {
              removeBtn.disabled = selectedCount === 0;
              var label = removeBtn.querySelector('span');
              if (label) label.textContent = selectedCount ? 'Remove selected (' + selectedCount + ')' : 'Remove selected';
            }
            setText(
              'desktop-organism-remove-count',
              selectedCount + ' selected · ' + removable.length + ' removable'
            );
          }

          function renderDesktopOrganismRemovalList() {
            var root = document.getElementById('desktop-organism-remove-list');
            if (!root) return;
            var removable = removableDesktopDatasets();
            root.textContent = '';
            updateDesktopOrganismRemovalControls(removable);
            if (!removable.length) {
              root.innerHTML = '<div class=\"desktop-organism-empty\">No downloaded organisms are installed.</div>';
              return;
            }
            removable.forEach(function(item) {
              var option = document.createElement('label');
              option.className = 'desktop-organism-removal-option';
              option.dataset.desktopDatasetId = item.id || '';

              var checkbox = document.createElement('input');
              checkbox.type = 'checkbox';
              checkbox.className = 'desktop-organism-removal-checkbox';
              checkbox.checked = desktopDatasetRemovalSelection.has(item.id);
              checkbox.addEventListener('change', function() {
                if (checkbox.checked) desktopDatasetRemovalSelection.add(item.id);
                else desktopDatasetRemovalSelection.delete(item.id);
                option.classList.toggle('is-selected', checkbox.checked);
                updateDesktopOrganismRemovalControls(removable);
              });

              var img = document.createElement('img');
              img.alt = '';
              img.src = iconForDataset(item);
              img.onerror = function() { this.onerror = null; this.src = 'icons/DNA.ico'; };

              var body = document.createElement('span');
              body.className = 'desktop-organism-removal-body';
              var title = document.createElement('strong');
              title.className = 'desktop-organism-scientific-name';
              title.textContent = item.label || item.id;
              var details = document.createElement('small');
              details.textContent = [item.version ? 'v' + item.version : '', item.speciesId || item.id].filter(Boolean).join(' · ');
              body.append(title, details);

              option.classList.toggle('is-selected', checkbox.checked);
              option.append(checkbox, img, body);
              root.append(option);
            });
          }

          function refreshDesktopDatasets(options) {
            options = options || {};
            if (!window.cgvDesktop) {
              renderDesktopDatasets({ datasets: [] });
              return Promise.resolve({ datasets: [] });
            }
            var listPromise = window.cgvDesktop.listDatasets().then(function(manifest) {
              renderDesktopDatasets(manifest);
              if (options.clearStatus) clearDesktopOrganismStatusIfIdle();
              return manifest;
            }).catch(function(error) {
              setDesktopOrganismStatus(error && error.message ? error.message : 'Unable to load organism catalog.');
              return null;
            });
            if (window.cgvDesktop.getRuntime) {
              window.cgvDesktop.getRuntime().then(function(runtime) {
                setText('desktop-organism-data-path', runtime.dataRoot || '');
                setText('desktop-organism-cache-path', runtime.cacheRoot || '');
              }).catch(function() {});
            }
            return listPromise;
          }

          function requestDesktopDatasets(options) {
            options = options || {};
            // Share only the currently active request. Once it settles, a later
            // Settings activation or explicit Refresh must revalidate again.
            if (!desktopDatasetRefreshPromise) {
              desktopDatasetRefreshPromise = Promise.resolve()
                .then(function() { return refreshDesktopDatasets(); })
                .then(function(manifest) {
                  desktopDatasetRefreshPromise = null;
                  return manifest;
                }, function(error) {
                  desktopDatasetRefreshPromise = null;
                  throw error;
                });
            }
            if (!options.clearStatus) return desktopDatasetRefreshPromise;
            return desktopDatasetRefreshPromise.then(function(manifest) {
              if (manifest) clearDesktopOrganismStatusIfIdle();
              return manifest;
            });
          }

          function desktopSettingsIsActive() {
            var settingsRoot = document.querySelector('.app-settings-pane');
            var pane = settingsRoot && settingsRoot.closest ? settingsRoot.closest('.tab-pane') : null;
            if (pane && (pane.classList.contains('active') || pane.classList.contains('show'))) return true;
            var activeTab = document.querySelector('#navtabs li.active > a[data-value=\"settings\"], #navtabs a.active[data-value=\"settings\"]');
            if (activeTab) return true;
            return !!document.querySelector('.app-nav-btn.is-active[data-target=\"settings\"]');
          }

          function eventActivatesDesktopSettings(event) {
            if (event && event.name === 'navtabs' && event.value === 'settings') return true;
            var target = event && event.target;
            var settingsTarget = target && target.closest
              ? target.closest('.app-nav-btn[data-target=\"settings\"], #navtabs a[data-value=\"settings\"]')
              : null;
            if (settingsTarget) return true;
            return !!(event && String(event.type || '').indexOf('shown') === 0 && desktopSettingsIsActive());
          }

          function activateDesktopDatasetsFromEvent(event) {
            if (eventActivatesDesktopSettings(event)) requestDesktopDatasets();
          }

          document.addEventListener('DOMContentLoaded', function() {
            var openCatalogBtn = document.getElementById('desktop-organism-open-catalog');
            var closeCatalogBtn = document.getElementById('desktop-organism-modal-close');
            var catalogModal = document.getElementById('desktop-organism-modal');
            var catalogBackdrop = catalogModal && catalogModal.querySelector('.desktop-organism-modal-backdrop');
            var catalogSearch = document.getElementById('desktop-organism-search');
            var catalogFilter = document.getElementById('desktop-organism-filter');
            var removalModal = document.getElementById('desktop-organism-remove-modal');
            var removalCloseBtn = document.getElementById('desktop-organism-remove-close');
            var removalCancelBtn = document.getElementById('desktop-organism-remove-cancel');
            var removalBackdrop = removalModal && removalModal.querySelector('.desktop-organism-modal-backdrop');
            var removalSelectAll = document.getElementById('desktop-organism-remove-all');
            var removalSubmitBtn = document.getElementById('desktop-organism-remove-selected');
            function openCatalogModal() {
              if (!catalogModal) return;
              requestDesktopDatasets();
              catalogModal.classList.add('is-open');
              catalogModal.setAttribute('aria-hidden', 'false');
              renderDesktopOrganismModalList();
              if (catalogSearch) catalogSearch.focus();
            }
            function closeCatalogModal() {
              if (!catalogModal) return;
              catalogModal.classList.remove('is-open');
              catalogModal.setAttribute('aria-hidden', 'true');
            }
            function openRemovalModal() {
              if (!removalModal || !removableDesktopDatasets().length) return;
              desktopDatasetRemovalSelection.clear();
              renderDesktopOrganismRemovalList();
              removalModal.classList.add('is-open');
              removalModal.setAttribute('aria-hidden', 'false');
              if (removalSelectAll) removalSelectAll.focus();
            }
            function closeRemovalModal() {
              if (!removalModal) return;
              removalModal.classList.remove('is-open');
              removalModal.setAttribute('aria-hidden', 'true');
              desktopDatasetRemovalSelection.clear();
              renderDesktopOrganismRemovalList();
            }
            if (openCatalogBtn) openCatalogBtn.addEventListener('click', openCatalogModal);
            if (closeCatalogBtn) closeCatalogBtn.addEventListener('click', closeCatalogModal);
            if (catalogBackdrop) catalogBackdrop.addEventListener('click', closeCatalogModal);
            if (removalCloseBtn) removalCloseBtn.addEventListener('click', closeRemovalModal);
            if (removalCancelBtn) removalCancelBtn.addEventListener('click', closeRemovalModal);
            if (removalBackdrop) removalBackdrop.addEventListener('click', closeRemovalModal);
            if (catalogSearch) catalogSearch.addEventListener('input', renderDesktopOrganismModalList);
            if (catalogFilter) catalogFilter.addEventListener('change', renderDesktopOrganismModalList);
            document.addEventListener('keydown', function(event) {
              if (event.key === 'Escape' && catalogModal && catalogModal.classList.contains('is-open')) closeCatalogModal();
              if (event.key === 'Escape' && removalModal && removalModal.classList.contains('is-open')) closeRemovalModal();
            });
            var refreshBtn = document.getElementById('desktop-organism-refresh');
            if (refreshBtn) refreshBtn.addEventListener('click', function() {
              requestDesktopDatasets({ clearStatus: true });
            });
            var resetBtn = document.getElementById('desktop-organism-reset');
            if (resetBtn) resetBtn.addEventListener('click', openRemovalModal);
            if (removalSelectAll) removalSelectAll.addEventListener('change', function() {
              var removable = removableDesktopDatasets();
              desktopDatasetRemovalSelection.clear();
              if (removalSelectAll.checked) {
                removable.forEach(function(item) { desktopDatasetRemovalSelection.add(item.id); });
              }
              renderDesktopOrganismRemovalList();
            });
            if (removalSubmitBtn) removalSubmitBtn.addEventListener('click', function() {
              if (!window.cgvDesktop || !window.cgvDesktop.removeInstalledOrganisms) return;
              var selectedIds = Array.from(desktopDatasetRemovalSelection);
              if (!selectedIds.length) return;
              var noun = selectedIds.length === 1 ? 'organism' : 'organisms';
              var possessive = selectedIds.length === 1 ? 'its' : 'their';
              var ok = window.confirm(
                'Remove ' + selectedIds.length + ' selected ' + noun + ' and ' + possessive + ' local files and caches from this desktop profile?'
              );
              if (!ok) return;
              removalSubmitBtn.disabled = true;
              if (resetBtn) resetBtn.disabled = true;
              setDesktopOrganismStatus('Removing ' + selectedIds.length + ' selected ' + noun + '...');
              window.cgvDesktop.removeInstalledOrganisms(selectedIds).then(function(result) {
                var removedIds = result && result.removedDatasetIds || selectedIds;
                removedIds.forEach(function(datasetId) {
                  delete desktopDatasetProgress[datasetId];
                  desktopDatasetRemovalSelection.delete(datasetId);
                });
                if (window.Shiny) {
                  Shiny.setInputValue('desktop_dataset_installed', { id: 'reset', at: Date.now() }, { priority: 'event' });
                }
                return window.cgvDesktop.listDatasets();
              }).then(function(manifest) {
                renderDesktopDatasets(manifest);
                closeRemovalModal();
                clearDesktopOrganismStatusIfIdle();
              }).catch(function(error) {
                setDesktopOrganismStatus(error && error.message ? error.message : 'Unable to remove installed organisms.');
              }).finally(function() {
                updateDesktopOrganismRemovalControls();
                if (resetBtn) resetBtn.disabled = removableDesktopDatasets().length === 0;
              });
            });
            if (window.cgvDesktop && window.cgvDesktop.onDownloadProgress) {
              window.cgvDesktop.onDownloadProgress(function(payload) {
                if (!payload) return;
                if (payload.datasetId) updateDesktopDatasetProgress(payload.datasetId, payload);
                var label = phaseLabel(payload.phase);
                if (payload.phase === 'error') {
                  setDesktopOrganismStatus(payload.message || 'Download failed.');
                  return;
                }
                if (payload.phase === 'cancelled') {
                  setDesktopOrganismStatus(payload.message || 'Download canceled.', 3000);
                  return;
                }
                if (payload.phase === 'complete') {
                  clearDesktopOrganismStatusIfIdle();
                  return;
                }
                if (payload.phase === 'connecting' || payload.phase === 'preparing') {
                  setDesktopOrganismStatus(label + ' organism package...');
                  return;
                }
                if (payload.phase === 'extracting' || payload.phase === 'installing_cache' || payload.phase === 'verifying') {
                  setDesktopOrganismStatus(label + ' organism package...');
                  return;
                }
                if (payload.skipped) {
                  clearDesktopOrganismStatusIfIdle();
                  return;
                }
                if (payload.percent != null) {
                  var bits = ['Downloading... ' + Math.round(payload.percent * 100) + '%'];
                  if (payload.speed) bits.push(formatBytes(payload.speed) + '/s');
                  if (payload.eta) bits.push(formatEta(payload.eta) + ' left');
                  setDesktopOrganismStatus(bits.join(' · '));
                }
              });
            }
            document.addEventListener('click', activateDesktopDatasetsFromEvent);
            document.addEventListener('shown.bs.tab', activateDesktopDatasetsFromEvent);
            if (window.jQuery) {
              jQuery(document).on('shown.bs.tab.cgvDesktopDatasets', activateDesktopDatasetsFromEvent);
              jQuery(document).on('shiny:inputchanged.cgvDesktopDatasets', function(event) {
                if (event.name === 'navtabs' && event.value === 'settings') requestDesktopDatasets();
              });
            }
            if (!window.cgvDesktop) renderDesktopDatasets({ datasets: [] });
            else if (desktopSettingsIsActive()) requestDesktopDatasets();
          });
        })();
      ")),
      tags$title("CGeV | Comparative Gene Viewer"),
      tags$link(rel = "stylesheet", href = versioned_asset_path("css/cross_species_header_status.css")),
      tags$link(rel = "stylesheet", href = versioned_asset_path("css/cgv_desktop_downloads.css")),
      tags$script(HTML(sprintf(
        "window.__cgvTransportTiming = %s; window.__cgvTransportFlushMs = %s; window.__cgvPlotPaintTiming = %s;",
        jsonlite::toJSON(app_env_flag("APP_TRANSPORT_TIMING", FALSE), auto_unbox = TRUE),
        jsonlite::toJSON(app_env_int("APP_TRANSPORT_FLUSH_MS", 5000L, min_value = 50L), auto_unbox = TRUE),
        jsonlite::toJSON(app_perf_enabled(), auto_unbox = TRUE)
      ))),
      tags$script(HTML(sprintf("
        window.__cgvAppVersion = %s;
        window.__cgvVersionProbeUrl = %s;
        window.__cgvDefaultLightIcon = %s;
        window.__cgvDefaultDarkIcon = %s;
      ",
      jsonlite::toJSON(app_asset_version, auto_unbox = TRUE),
      jsonlite::toJSON("cgv-meta/version.json", auto_unbox = TRUE),
      jsonlite::toJSON(versioned_asset_path("favicon2.ico?v=2"), auto_unbox = TRUE),
      jsonlite::toJSON(versioned_asset_path("favicon.ico?v=2"), auto_unbox = TRUE)))),
      tags$script(src = versioned_asset_path("js/transport_metrics.js")),
      tags$script(src = versioned_asset_path("js/plot_paint_timing.js")),
      tags$script(src = versioned_asset_path("js/lazy_jszip.js")),
      tags$script(src = versioned_asset_path("js/version_probe.js")),
      tags$script(src = versioned_asset_path("js/rounded_rects.js")),
      tags$script(src = versioned_asset_path("js/autocomplete_core.js")),
      tags$script(src = versioned_asset_path("js/footer_scroll_controls.js")),
      tags$script(HTML("
        Shiny.addCustomMessageHandler('lastz_debug', function(msg) {
          var level = msg.level || 'log';
          var prefix = '%c[LASTZ-DEBUG]';
          var style = 'color:#e74c3c;font-weight:bold';
          if (level === 'error') {
            console.error(prefix, style, msg.step, msg.detail || '');
          } else if (level === 'warn') {
            console.warn(prefix, style, msg.step, msg.detail || '');
          } else {
            console.log(prefix, style, msg.step, msg.detail || '');
          }
          if (msg.data) { console.table(msg.data); }
        });
      ")),
      tags$link(rel = "shortcut icon", type = "image/x-icon", href = versioned_asset_path("favicon.ico?v=2")),
      tags$script(HTML("
      Shiny.addCustomMessageHandler('force_refresh_picker', function(message) {
        var ids = (message && message.ids) ? message.ids : [];
        ids.forEach(function(id) {
          var $el = $('#' + id);
          if ($el.length && $el.hasClass('selectpicker')) {
            $el.prop('disabled', false);
            $el.selectpicker('refresh');
            $el.selectpicker('render');
            $el.parent('.bootstrap-select').removeClass('disabled');
          }
        });
      });

      document.addEventListener('DOMContentLoaded', function() {

        // These panes are inside a collapsible sidebar. In that context,
        // Shiny conditionalPanel can fail to re-evaluate its initial state.
        // Keep data-source visibility deterministic on every radio change.
        var dataSourcePanelModes = {
          'homo-preloaded': ['preloaded'],
          'homo-ncbi': ['ncbi'],
          'homo-upload': ['upload'],
          'ortho-preloaded': ['preloaded', 'mixed'],
          'ortho-ncbi': ['ncbi', 'mixed'],
          'ortho-upload': ['upload', 'mixed']
        };
        var syncDataSourcePanels = function() {
          document.querySelectorAll('[data-source-panel]').forEach(function(panel) {
            var panelName = String(panel.getAttribute('data-source-panel') || '');
            var workflow = panelName.split('-')[0];
            var selected = document.querySelector('input[name=\"' + workflow + '_data_mode\"]:checked');
            var mode = selected ? String(selected.value || '') : 'preloaded';
            var shouldShow = (dataSourcePanelModes[panelName] || []).indexOf(mode) !== -1;
            panel.toggleAttribute('hidden', !shouldShow);
            panel.setAttribute('aria-hidden', shouldShow ? 'false' : 'true');
            panel.querySelectorAll('[data-source-mixed-divider]').forEach(function(divider) {
              divider.toggleAttribute('hidden', mode !== 'mixed');
            });
          });
        };
        window.syncDataSourcePanels = syncDataSourcePanels;

        var syncVizModeToggles = function() {
          var groups = document.querySelectorAll('.viz-mode-toggle .shiny-options-group');
          groups.forEach(function(group) {
            var labels = group.querySelectorAll('.radio-inline');
            var activeIndex = 0;
            var hasActive = false;

            labels.forEach(function(lbl, idx) {
              var radio = lbl.querySelector('input[type=\"radio\"]');
              var isActive = !!(radio && radio.checked);
              lbl.classList.toggle('is-active', isActive);
              if (isActive) {
                activeIndex = idx;
                hasActive = true;
              }
            });

            group.style.setProperty('--app-switch-count', String(Math.max(labels.length, 1)));
            group.style.setProperty('--app-switch-index', String(activeIndex));
            group.classList.toggle('has-selection', hasActive);
          });
        };
        window.syncVizModeToggles = syncVizModeToggles;
        var syncVizModeToggleForTarget = function(target) {
          if (!target || !target.closest) return;
          var label = target.closest('.viz-mode-toggle .radio-inline');
          if (!label) return;
          var radio = label.querySelector('input[type=\"radio\"]');
          if (!radio || radio.disabled) return;
          var name = String(radio.name || '');
          var value = String(radio.value || '');
          var group = label.closest('.shiny-options-group');
          if (!group) return;
          var labels = Array.prototype.slice.call(group.querySelectorAll('.radio-inline'));
          var activeIndex = Math.max(0, labels.indexOf(label));
          labels.forEach(function(lbl, idx) {
            lbl.classList.toggle('is-active', idx === activeIndex);
            var lblRadio = lbl.querySelector('input[type=\"radio\"]');
            if (lblRadio && lblRadio.name === name) {
              lblRadio.checked = idx === activeIndex;
            }
          });
          group.style.setProperty('--app-switch-count', String(Math.max(labels.length, 1)));
          group.style.setProperty('--app-switch-index', String(activeIndex));
          group.classList.add('has-selection');
          if (name === 'homo_visual_mode' && window.updateHomoSpecialCardVisibility) {
            window.updateHomoSpecialCardVisibility();
          } else if (name === 'ortho_visual_mode' && window.updateOrthoSpecialCardVisibility) {
            window.updateOrthoSpecialCardVisibility();
          }
          var toggleRoot = label.closest('.viz-mode-toggle');
          if (toggleRoot) {
            toggleRoot.classList.add('is-mode-pending');
            window.setTimeout(function() {
              toggleRoot.classList.remove('is-mode-pending');
            }, 2400);
          }
          if (window.Shiny && Shiny.setInputValue && name && value) {
            Shiny.setInputValue(name, value, { priority: 'event' });
          }
          syncDataSourcePanels();
        };
        document.addEventListener('pointerdown', function(evt) {
          syncVizModeToggleForTarget(evt && evt.target ? evt.target : null);
        }, true);
        document.addEventListener('mousedown', function(evt) {
          syncVizModeToggleForTarget(evt && evt.target ? evt.target : null);
        }, true);
        document.addEventListener('touchstart', function(evt) {
          syncVizModeToggleForTarget(evt && evt.target ? evt.target : null);
        }, true);
        document.addEventListener('click', function(evt) {
          syncVizModeToggleForTarget(evt && evt.target ? evt.target : null);
        }, true);
        document.addEventListener('change', function(evt) {
          var t = evt && evt.target ? evt.target : null;
          if (t && t.matches('.viz-mode-toggle input[type=\"radio\"]')) {
            syncVizModeToggles();
            syncDataSourcePanels();
          }
        });
        $(document).on('shiny:value', syncVizModeToggles);
        $(document).on('shiny:connected shiny:value', syncDataSourcePanels);
        $(document).on('shiny:value', function() {
          document.querySelectorAll('.viz-mode-toggle.is-mode-pending').forEach(function(node) {
            node.classList.remove('is-mode-pending');
          });
        });
        syncVizModeToggles();
        syncDataSourcePanels();

        var input1 = document.getElementById('filter1');
        if (input1) {
          input1.setAttribute('list', 'filter1_suggestions');
          input1.setAttribute('autocomplete', 'off');
          input1.setAttribute('autocorrect', 'off');
          input1.setAttribute('autocapitalize', 'off');
          input1.setAttribute('spellcheck', 'false');
          input1.style.color = '#2C3E50';
          input1.style.backgroundColor = '#F8FCFB';
          input1.style.caretColor = '#2C3E50';
          input1.style.colorScheme = 'light';
        }
        var input2 = document.getElementById('gene_name');
        if (input2) {
          input2.setAttribute('list', 'gene_name_suggestions');
          input2.setAttribute('autocomplete', 'off');
          input2.setAttribute('autocorrect', 'off');
          input2.setAttribute('autocapitalize', 'off');
          input2.setAttribute('spellcheck', 'false');
          input2.style.color = '#2C3E50';
          input2.style.backgroundColor = '#F8FCFB';
          input2.style.caretColor = '#2C3E50';
          input2.style.colorScheme = 'light';
        }

        function triggerWorkflowEnterSearch(mode, rawValue) {
          if (!window.Shiny || typeof Shiny.setInputValue !== 'function') return false;
          var normalizedMode = String(mode || '').trim();
          if (normalizedMode !== 'homologous' && normalizedMode !== 'orthologous') return false;
          var gene = String(rawValue == null ? '' : rawValue);
          if (window.cgvBeginSearchFeedback) {
            if (!window.cgvBeginSearchFeedback(normalizedMode, gene)) return false;
          }
          Shiny.setInputValue('workflow_enter_search', {
            mode: normalizedMode,
            gene: gene,
            nonce: Date.now()
          }, { priority: 'event' });
          return true;
        }

        if (input1 && input1.getAttribute('data-enter-search-bound') !== '1') {
          input1.setAttribute('data-enter-search-bound', '1');
          input1.addEventListener('keydown', function(evt) {
            if (!evt || evt.key !== 'Enter' || evt.ctrlKey || evt.metaKey || evt.altKey || evt.shiftKey) return;
            evt.preventDefault();
            evt.stopPropagation();
            triggerWorkflowEnterSearch('homologous', input1.value || '');
          });
        }

        if (input2 && input2.getAttribute('data-enter-search-bound') !== '1') {
          input2.setAttribute('data-enter-search-bound', '1');
          input2.addEventListener('keydown', function(evt) {
            if (!evt || evt.key !== 'Enter' || evt.ctrlKey || evt.metaKey || evt.altKey || evt.shiftKey) return;
            evt.preventDefault();
            evt.stopPropagation();
            triggerWorkflowEnterSearch('orthologous', input2.value || '');
          });
        }
      });

      function getOrganismSummaryTarget(inputId) {
        if (inputId === 'homo_preloaded_species') return $('#homo-organism-summary-text');
        if (inputId === 'ortho_preloaded_species') return $('#ortho-organism-summary-text');
        return $();
      }

      function setSpeciesInputValue(inputId, value) {
        if (!inputId) return;
        window.__currentSpeciesSelections = window.__currentSpeciesSelections || {};
        window.__currentSpeciesSelections[inputId] = value;
        if (window.Shiny && typeof Shiny.setInputValue === 'function') {
          Shiny.setInputValue(inputId, value, {priority: 'event'});
          return;
        }
        window.__pendingSpeciesSelections = window.__pendingSpeciesSelections || {};
        window.__pendingSpeciesSelections[inputId] = value;
      }

      function flushPendingSpeciesSelections() {
        var pending = window.__pendingSpeciesSelections || {};
        Object.keys(pending).forEach(function(inputId) {
          setSpeciesInputValue(inputId, pending[inputId]);
        });
        window.__pendingSpeciesSelections = {};
      }

      function restoreSpeciesGridSelection(inputId) {
        inputId = String(inputId || '');
        if (!inputId) return;
        var current = window.__currentSpeciesSelections && window.__currentSpeciesSelections[inputId];
        if (current === undefined || current === null || current === '') return;
        var selected = Array.isArray(current) ? current.map(String) : [String(current)];
        var $grids = $('.species-grid[data-input-id=\"' + inputId + '\"]');
        if (!$grids.length) return;
        $grids.find('.species-grid-card').each(function() {
          var id = String($(this).attr('data-species-id') || '');
          $(this).toggleClass('selected', selected.indexOf(id) >= 0);
        });
        updateOrganismSummaryFromGrid($grids.first());
      }

      function updateOrganismSummaryFromGrid($grid) {
        if (!$grid || !$grid.length) return;

        var inputId = String($grid.attr('data-input-id') || '');
        if (!inputId) return;
        var $allGrids = $('.species-grid[data-input-id=\"' + inputId + '\"]');
        if (!$allGrids.length) return;
        var mode = String($allGrids.first().attr('data-mode') || 'single');
        var $target = getOrganismSummaryTarget(inputId);
        if (!$target.length) return;

        var defaultLabel = 'Organism selection';
        var selectedManyTemplate = '{n} organisms selected';

        var $selected = $allGrids.find('.species-grid-card.selected');
        if ($selected.length === 0) {
          $target.text(defaultLabel);
          return;
        }

        var $first = $selected.first();
        var name = String($first.find('.species-grid-name').text() || '').trim();
        var iconSrc = String($first.find('.species-grid-icon-img').attr('src') || '').trim();

        if (mode === 'single' || $selected.length === 1) {
          if (iconSrc) {
            $target.html(
              '<span class=\"app-organism-selected\">' +
                '<span class=\"app-organism-selected-icon\"><img class=\"app-organism-selected-img\" src=\"' + $('<span>').text(iconSrc).html() + '\" alt=\"\"/></span>' +
                '<span class=\"app-organism-selected-name\"><em>' + $('<span>').text(name || defaultLabel).html() + '</em></span>' +
              '</span>'
            );
          } else {
            $target.html('<em>' + $('<span>').text(name || defaultLabel).html() + '</em>');
          }
          return;
        }

        var countText = selectedManyTemplate.replace('{n}', String($selected.length));
        $target.text(countText);
      }

      /* ── Species grid: single-select (homologous) ── */
      $(document).on('click', '.species-grid[data-mode=\"single\"] .species-grid-card', function(e) {
        e.stopPropagation();
        var $card = $(this);
        var $grid = $card.closest('.species-grid');
        var inputId = String($grid.attr('data-input-id') || '');
        var id    = $card.attr('data-species-id') || '';
        if (!inputId) return;
        $('.species-grid[data-input-id=\"' + inputId + '\"] .species-grid-card').removeClass('selected');
        $card.addClass('selected');
        setSpeciesInputValue(inputId, id);
        updateOrganismSummaryFromGrid($grid);
        var $details = $grid.closest('details');
        if ($details.length) {
          $details.removeAttr('open').removeProp('open');
        }
      });

      /* ── Species grid: multi-select (orthologous) ── */
      $(document).on('click', '.species-grid[data-mode=\"multi\"] .species-grid-card', function(e) {
        e.stopPropagation();
        var $card = $(this);
        $card.toggleClass('selected');
        var $grid = $card.closest('.species-grid');
        var inputId = String($grid.attr('data-input-id') || '');
        if (!inputId) return;
        var $allGrids = $('.species-grid[data-input-id=\"' + inputId + '\"]');
        var sel   = [];
        $allGrids.find('.species-grid-card.selected').each(function() {
          sel.push($(this).attr('data-species-id'));
        });
        setSpeciesInputValue(inputId, sel.length ? sel : null);
        updateOrganismSummaryFromGrid($grid);
      });

      $(document).on('shiny:connected', flushPendingSpeciesSelections);

      // Keep organism summary labels consistent after Shiny UI re-renders.
      $(document).on('shiny:value', function() {
        setTimeout(function() {
          ['homo_species_grid', 'ortho_species_grid'].forEach(function(outputId) {
            var live = document.getElementById(outputId);
            var fallback = document.getElementById(outputId + '_initial');
            if (live && fallback && $.trim($(live).html() || '').length) {
              fallback.parentNode.removeChild(fallback);
            }
          });
          restoreSpeciesGridSelection('homo_preloaded_species');
          restoreSpeciesGridSelection('ortho_preloaded_species');
        }, 0);
        $('.species-grid[data-input-id]').each(function() {
          updateOrganismSummaryFromGrid($(this));
        });
      });

      function selectSpeciesGridCard(inputId, speciesId) {
        inputId = String(inputId || '');
        speciesId = String(speciesId || '');
        if (!inputId || !speciesId) return false;
        var $grids = $('.species-grid[data-input-id=\"' + inputId + '\"]');
        if (!$grids.length) return false;
        var $card = $grids.find('.species-grid-card[data-species-id=\"' + speciesId + '\"]').first();
        if (!$card.length) return false;
        var mode = String($grids.first().attr('data-mode') || 'single');
        if (mode === 'single') {
          $grids.find('.species-grid-card').removeClass('selected');
          $card.addClass('selected');
          setSpeciesInputValue(inputId, speciesId);
        } else {
          $card.addClass('selected');
          var selected = [];
          $grids.find('.species-grid-card.selected').each(function() {
            var id = $(this).attr('data-species-id') || '';
            if (id && selected.indexOf(id) < 0) selected.push(id);
          });
          setSpeciesInputValue(inputId, selected.length ? selected : null);
        }
        updateOrganismSummaryFromGrid($card.closest('.species-grid'));
        return true;
      }

      if (window.Shiny && typeof Shiny.addCustomMessageHandler === 'function') {
        Shiny.addCustomMessageHandler('cgv:species-grid-refresh', function(msg) {
          var speciesId = msg && msg.species_id ? String(msg.species_id) : '';
          var maxAttempts = speciesId ? 10 : 1;
          var attemptDelayMs = 100;

          function applyGridRefresh(attempt) {
            $('.species-grid[data-input-id]').each(function() {
              updateOrganismSummaryFromGrid($(this));
            });
            var selectedHomo = true;
            var selectedOrtho = true;
            if (speciesId) {
              selectedHomo = selectSpeciesGridCard('homo_preloaded_species', speciesId);
              selectedOrtho = selectSpeciesGridCard('ortho_preloaded_species', speciesId);
            }
            flushPendingSpeciesSelections();

            if (speciesId && (!selectedHomo || !selectedOrtho) && attempt < maxAttempts) {
              setTimeout(function() {
                applyGridRefresh(attempt + 1);
              }, attemptDelayMs);
            }
          }

          setTimeout(function() {
            applyGridRefresh(1);
          }, 0);
        });
      }

      // Initial sync (for cases where grids are already present).
      setTimeout(function() {
        $('.species-grid[data-input-id]').each(function() {
          updateOrganismSummaryFromGrid($(this));
        });
      }, 0);
    ")),
      tags$script(src = versioned_asset_path("js/keepalive.js")),
      tags$script(src = versioned_asset_path("js/dna_loader_unifier.js")),
      tags$script(src = versioned_asset_path("js/activity_feedback.js")),
      tags$script(src = versioned_asset_path("js/gene_geometry_update.js")),
      tags$script(src = versioned_asset_path("js/plot_zoom.js")),
      tags$script(src = versioned_asset_path("js/genomic_ruler_toggle.js")),
      tags$script(src = versioned_asset_path("js/export_svg.js")),
      tags$script(
        id = "cgv-figure-studio-loader",
        `data-module-src` = versioned_asset_path("js/figure_studio.js"),
        `data-style-href` = versioned_asset_path("css/figure_studio.css"),
        HTML(figure_studio_loader_source)
      ),
      tags$script(src = versioned_asset_path("js/reproducible_report.js")),
      tags$script(src = versioned_asset_path("js/string_theme.js")),
      tags$script(src = versioned_asset_path("js/status_popup.js")),
      tags$script(src = versioned_asset_path("js/search_submit_feedback.js")),
      tags$script(src = versioned_asset_path("js/promoter_popup.js")),
      tags$script(src = versioned_asset_path("js/genomic_neighbor_popup.js")),
      tags$script(src = versioned_asset_path("js/go_terms_popup.js")),
      tags$script(src = versioned_asset_path("js/papers_popup.js")),
      tags$script(src = versioned_asset_path("js/preloaded_assembly_popup.js")),
      tags$script(src = versioned_asset_path("js/metrics_popup.js")),
      tags$script(src = versioned_asset_path("js/ortho_lazy_loader.js")),
      tags$script(src = versioned_asset_path("js/summary_context_layout.js")),
      tags$script(src = versioned_asset_path("js/result_workspace.js")),
      tags$script(src = versioned_asset_path("js/gene_match_evidence_help.js")),
      tags$script(src = versioned_asset_path("js/ncbi_search.js")),
      if (isTRUE(cgv_gene_catalog_enabled)) {
        tags$script(src = versioned_asset_path("js/gene_catalog.js"))
      } else NULL,
      tags$script(src = versioned_asset_path("js/organism_images.js")),
      tags$script(src = versioned_asset_path("js/mobile_enhancements.js"), defer = NA),
      tags$script(src = versioned_asset_path("js/cgv_desktop_downloads.js"), defer = NA),
      tags$script(HTML("
      (function () {
        var validTargets = ['home', 'catalog', 'homologous', 'orthologous', 'figure-studio', 'guide', 'desktop-app', 'settings', 'help', 'feedback'];
        var suggestionState = { items: [], activeIndex: -1 };
        var collapsedSuggestionState = { items: [], activeIndex: -1 };
        var suggestionObserver = null;
        var globalSuggestionPool = Array.isArray(window.__globalGeneSuggestionPool)
          ? window.__globalGeneSuggestionPool.slice()
          : [];
        var globalSearchChips = [];
        var lastWorkflowTarget = 'homologous';
        var quickFabPanel = null;
        var quickFabVisibilityFrame = null;
        var quickFabResizeObserver = null;
        var suppressWorkflowPanelOnNextNav = false;

        function isValidTarget(target) {
          return validTargets.indexOf(target) !== -1;
        }

        function updateBatchUiVisibility(target) {
          // Batch UI is exclusive to Homologous Search
          var isHomologous = target === 'homologous';
          var batchGroup       = document.getElementById('sidebar-batch-ui-group');
          var batchCount       = document.getElementById('global-search-batch-count');
          var collapsedPreview = document.getElementById('collapsed-batch-preview');
          var searchHint       = document.getElementById('sidebar-search-hint');
          var searchInput      = document.getElementById('global_search_query');
          var collapsedInput   = document.getElementById('global_search_query_collapsed');
          if (batchGroup)       batchGroup.style.display       = isHomologous ? '' : 'none';
          if (batchCount)       batchCount.style.display       = isHomologous ? '' : 'none';
          if (collapsedPreview) collapsedPreview.style.display = isHomologous ? '' : 'none';
          if (searchHint)       searchHint.style.display       = isHomologous ? '' : 'none';
          // Placeholder reflects mode: batch hint only in homologous
          var ph = isHomologous ? 'Search gene (add to batch)' : 'Search gene';
          if (searchInput)    searchInput.placeholder    = ph;
          if (collapsedInput) collapsedInput.placeholder = ph;
        }

        function syncSummaryContextVisibility(target) {
          var homoSection = document.getElementById('homo_context_section');
          var orthoSection = document.getElementById('ortho_context_section');
          if (homoSection) homoSection.style.display = target === 'homologous' ? 'flex' : 'none';
          if (orthoSection) orthoSection.style.display = target === 'orthologous' ? 'flex' : 'none';
          if (target === 'homologous' || target === 'orthologous') {
            keepSummaryContextOpaque();
            if (window.requestAnimationFrame) {
              window.requestAnimationFrame(function () {
                document.dispatchEvent(new Event('shiny:value'));
              });
            }
          }
        }

        function setActiveNav(target) {
          if (target === 'homologous' || target === 'orthologous') {
            lastWorkflowTarget = target;
          }
          updateBatchUiVisibility(target);
          syncSummaryContextVisibility(target);
          var buttons = document.querySelectorAll('.app-nav-btn');
          buttons.forEach(function (btn) {
            btn.classList.toggle('is-active', btn.getAttribute('data-target') === target);
          });
          syncQuickFabModeControls();
          updateCompactModeLayoutState();
          updateQuickFabScrollTopVisibility();
        }

        function getActiveNavTarget() {
          var activeTabLink = document.querySelector('#navtabs li.active > a[data-value]');
          if (activeTabLink) {
            var tabValue = String(activeTabLink.getAttribute('data-value') || '').trim();
            if (isValidTarget(tabValue)) return tabValue;
          }

          var activePane = getActiveTabPane();
          if (activePane && activePane.id) {
            var paneSelector = '.app-main .nav a[data-value][href=\"#' + activePane.id + '\"]';
            var paneLink = document.querySelector(paneSelector);
            if (paneLink) {
              var paneValue = String(paneLink.getAttribute('data-value') || '').trim();
              if (isValidTarget(paneValue)) return paneValue;
            }
          }

          var activeBtn = document.querySelector('.app-nav-btn.is-active[data-target]');
          return activeBtn ? activeBtn.getAttribute('data-target') : null;
        }

        function activateNavTabImmediately(target) {
          if (!isValidTarget(target)) return false;
          var link = document.querySelector('#navtabs a[data-value=\"' + target + '\"]');
          if (!link) return false;
          try {
            if (window.jQuery && jQuery.fn && jQuery.fn.tab) {
              jQuery(link).tab('show');
            } else {
              link.click();
            }
            return true;
          } catch (err) {
            try {
              link.click();
              return true;
            } catch (err2) {
              return false;
            }
          }
        }

        function updateCompactModeLayoutState() {
          var shell = document.querySelector('.app-shell');
          if (!shell) return;
          var target = getActiveNavTarget();
          var mode = '';
          if (target === 'homologous') {
            var homoMode = document.querySelector('input[name=\"homo_visual_mode\"]:checked');
            mode = homoMode ? String(homoMode.value || '').trim().toLowerCase() : '';
          } else if (target === 'orthologous') {
            var orthoMode = document.querySelector('input[name=\"ortho_visual_mode\"]:checked');
            mode = orthoMode ? String(orthoMode.value || '').trim().toLowerCase() : '';
          }
          shell.classList.toggle('compact-visual-mode', mode === 'compact');
        }

        function updateOrthoSpecialCardVisibility() {
          var alignedShell = document.getElementById('ortho-aligned-card-shell');
          var pipShell = document.getElementById('ortho-pip-card-shell');
          var multipipShell = document.getElementById('ortho-multipip-card-shell');
          var regularCards = document.getElementById('ortho-plot-cards-container');
          var orthoMode = document.querySelector('input[name=\"ortho_visual_mode\"]:checked');
          var mode = orthoMode ? String(orthoMode.value || '').trim().toLowerCase() : 'compact';
          if (mode === 'pip') mode = 'pip_blocks';

          if (alignedShell) alignedShell.style.display = mode === 'aligned' ? '' : 'none';
          if (pipShell) pipShell.style.display = mode === 'pip_blocks' ? '' : 'none';
          if (multipipShell) multipipShell.style.display = mode === 'pip_multipip' ? '' : 'none';
          if (regularCards) {
            regularCards.style.display = (mode === 'aligned' || mode === 'pip_blocks' || mode === 'pip_multipip') ? 'none' : '';
          }
        }
        window.updateOrthoSpecialCardVisibility = updateOrthoSpecialCardVisibility;

        function updateHomoSpecialCardVisibility() {
          var alignedShell = document.getElementById('homo-aligned-card-shell');
          var pipShell = document.getElementById('homo-pip-card-shell');
          var multipipShell = document.getElementById('homo-multipip-card-shell');
          var regularCards = document.getElementById('homo-plot-cards-container');
          var homoMode = document.querySelector('input[name=\"homo_visual_mode\"]:checked');
          var mode = homoMode ? String(homoMode.value || '').trim().toLowerCase() : 'compact';
          if (mode === 'pip') mode = 'pip_blocks';

          if (alignedShell) alignedShell.style.display = mode === 'aligned' ? '' : 'none';
          if (pipShell) pipShell.style.display = mode === 'pip_blocks' ? '' : 'none';
          if (multipipShell) multipipShell.style.display = mode === 'pip_multipip' ? '' : 'none';
          if (regularCards) {
            regularCards.style.display = (mode === 'aligned' || mode === 'pip_blocks' || mode === 'pip_multipip') ? 'none' : '';
          }
        }
        window.updateHomoSpecialCardVisibility = updateHomoSpecialCardVisibility;

        function closeOrganismSubmenus(scope) {
          var root = scope || document;
          var detailsList = root.querySelectorAll('details.app-submenu-organism-inline');
          detailsList.forEach(function (detailsEl) {
            detailsEl.open = false;
            detailsEl.removeAttribute('open');
          });
        }

        function fitWorkflowPanelToViewport(panel) {
          if (!panel || !panel.classList || !panel.classList.contains('is-open')) return;
          if (window.matchMedia && window.matchMedia('(max-width: 960px)').matches) {
            panel.style.maxHeight = '';
            return;
          }

          var rect = panel.getBoundingClientRect();
          var viewportHeight = window.innerHeight || document.documentElement.clientHeight || 0;
          var bottomPad = 14;
          var available = Math.max(220, viewportHeight - rect.top - bottomPad);
          panel.style.maxHeight = available + 'px';
        }

        function fitOpenWorkflowPanelToViewport() {
          window.requestAnimationFrame(function () {
            var panel = document.querySelector('.app-nav-inline-panel[data-workflow-panel].is-open');
            fitWorkflowPanelToViewport(panel);
          });
        }

        function setWorkflowPanel(target) {
          var panels = document.querySelectorAll('.app-nav-inline-panel[data-workflow-panel]');
          panels.forEach(function (panel) {
            var panelTarget = panel.getAttribute('data-workflow-panel');
            var isOpen = panelTarget === target;
            panel.classList.toggle('is-open', isOpen);

            if (isOpen) {
              closeOrganismSubmenus(panel);
              fitWorkflowPanelToViewport(panel);
            } else {
              panel.style.maxHeight = '';
            }

            var relatedBtn = document.querySelector('.app-nav-btn[data-target=\"' + panelTarget + '\"]');
            if (relatedBtn) {
              relatedBtn.classList.toggle('is-expanded', isOpen);
              relatedBtn.setAttribute('aria-expanded', isOpen ? 'true' : 'false');
            }
          });
        }

        document.addEventListener('toggle', function (evt) {
          if (!evt.target || !evt.target.closest || !evt.target.closest('.app-nav-inline-panel[data-workflow-panel]')) return;
          fitOpenWorkflowPanelToViewport();
        }, true);

        window.addEventListener('resize', fitOpenWorkflowPanelToViewport);

        function suppressWorkflowPanelAutoOpenOnce() {
          suppressWorkflowPanelOnNextNav = true;
        }

        function isDesktopRuntime() {
          return !!(window.cgvDesktop && typeof window.cgvDesktop.getRuntime === 'function');
        }

        function getStoredTheme() {
          if (!isDesktopRuntime()) {
            // Web sessions always start in light mode. Clear a preference saved by
            // previous releases so it cannot affect a later browser visit.
            try {
              localStorage.removeItem('cgv-theme');
            } catch (err) {}
            return 'light';
          }

          try {
            return localStorage.getItem('cgv-theme') || 'light';
          } catch (err) {
            return 'light';
          }
        }

        function persistTheme(themeName) {
          if (!isDesktopRuntime()) return;
          try {
            localStorage.setItem('cgv-theme', themeName);
          } catch (err) {}
        }

        function syncThemeWithShiny(themeName, forceSend) {
          if (window.Shiny && Shiny.setInputValue) {
            if (!forceSend && window.__lastShinyThemeSent === themeName) {
              return;
            }
            window.__lastShinyThemeSent = themeName;
            Shiny.setInputValue('app_theme', themeName, { priority: 'event' });
          }
        }

        function applyTheme(theme) {
          var nextTheme = theme === 'dark' ? 'dark' : 'light';
          document.documentElement.setAttribute('data-app-theme', nextTheme);

          persistTheme(nextTheme);

          var themeLogos = document.querySelectorAll('img[data-light-src][data-dark-src]');
          if (themeLogos && themeLogos.length) {
            themeLogos.forEach(function(logo) {
              var lightSrc = logo.getAttribute('data-light-src') || window.__cgvDefaultLightIcon || 'favicon2.ico?v=2';
              var darkSrc = logo.getAttribute('data-dark-src') || window.__cgvDefaultDarkIcon || 'favicon.ico?v=2';
              var nextSrc = nextTheme === 'dark' ? darkSrc : lightSrc;
              if (logo.getAttribute('src') !== nextSrc) {
                logo.setAttribute('src', nextSrc);
              }
            });
          }

          var toggleInput = document.getElementById('theme-toggle-input');
          if (toggleInput) {
            toggleInput.checked = (nextTheme === 'dark');
          }

          syncThemeWithShiny(nextTheme, false);

          var homeIframe = document.getElementById('cgv-home-iframe');
          if (homeIframe && homeIframe.contentWindow) {
            try {
              homeIframe.contentWindow.postMessage({ type: 'cgv-theme-change', theme: nextTheme }, '*');
            } catch (err) {}
          }
        }

        function toggleTheme() {
          var current = document.documentElement.getAttribute('data-app-theme') || 'light';
          applyTheme(current === 'dark' ? 'light' : 'dark');
        }

        function getStoredSidebarCollapsed() {
          try {
            return localStorage.getItem('cgv-sidebar-collapsed') === '1';
          } catch (err) {
            return false;
          }
        }

        function isSidebarCollapsed() {
          var shell = document.querySelector('.app-shell');
          return !!(shell && shell.classList.contains('sidebar-collapsed'));
        }

        function getCollapsedSearchPanel() {
          return document.getElementById('app-collapsed-search-panel');
        }

        function getCollapsedSearchToggle() {
          return document.getElementById('global_search_toggle_collapsed');
        }

        function getCollapsedSearchInput() {
          return document.getElementById('global_search_query_collapsed');
        }

        function getBestSearchQuery() {
          var fromGlobal = document.getElementById('global_search_query');
          var fromHomo = document.getElementById('filter1');
          var fromOrtho = document.getElementById('gene_name');
          var q = '';
          if (fromGlobal) q = String(fromGlobal.value || '').trim();
          if (!q && fromHomo) q = String(fromHomo.value || '').trim();
          if (!q && fromOrtho) q = String(fromOrtho.value || '').trim();
          if (!q && globalSearchChips.length) q = globalSearchChips.join(' || ');
          return q;
        }

        function getQuickFabRoot() {
          return document.getElementById('app-quick-fab');
        }

        function getQuickFabMainToggle() {
          return document.getElementById('app-fab-main-toggle');
        }

        function getQuickFabScrollTopToggle() {
          return document.getElementById('app-fab-scroll-top');
        }

        function getFloatingTopButton() {
          return document.getElementById('app-floating-top');
        }

        function getQuickFabSearchToggle() {
          return document.getElementById('app-fab-search-toggle');
        }

        function getQuickFabModeToggle() {
          return document.getElementById('app-fab-mode-toggle');
        }

        function getQuickFabZoomToggle() {
          return document.getElementById('app-fab-zoom-toggle');
        }

        function getQuickFabSearchPanel() {
          return document.getElementById('app-fab-search-panel');
        }

        function getQuickFabModePanel() {
          return document.getElementById('app-fab-mode-panel');
        }

        function getQuickFabZoomPanel() {
          return document.getElementById('app-fab-zoom-panel');
        }

        function getQuickFabSearchInput() {
          return document.getElementById('app-fab-search-query');
        }

        function getQuickFabEnabledToggle() {
          return document.getElementById('quick_fab_enabled');
        }

        function getStoredQuickFabEnabled() {
          try {
            return localStorage.getItem('cgv-quick-fab-enabled') === '1';
          } catch (err) {
            return false;
          }
        }

        function isQuickFabEnabled() {
          var toggle = getQuickFabEnabledToggle();
          if (toggle) return !!toggle.checked;
          return getStoredQuickFabEnabled();
        }

        function applyQuickFabEnabled(enabled) {
          var nextEnabled = !!enabled;
          var root = getQuickFabRoot();
          var mainToggle = getQuickFabMainToggle();
          var settingsToggle = getQuickFabEnabledToggle();

          try {
            localStorage.setItem('cgv-quick-fab-enabled', nextEnabled ? '1' : '0');
          } catch (err) {}

          if (settingsToggle && settingsToggle.checked !== nextEnabled) {
            settingsToggle.checked = nextEnabled;
          }

          if (root) {
            root.classList.toggle('is-disabled', !nextEnabled);
            if (!nextEnabled) {
              setQuickFabPanel(null);
              root.classList.remove('is-open', 'has-open-panel', 'show-scroll-top');
              if (mainToggle) mainToggle.setAttribute('aria-expanded', 'false');
            }
          }

          updateQuickFabScrollTopVisibility();
        }

        function isQuickFabOpen() {
          var root = getQuickFabRoot();
          return !!(root && root.classList.contains('is-open'));
        }

        function getActiveTabPane() {
          return document.querySelector('.app-main > .tab-content > .tab-pane.active');
        }

        function isElementDisplayed(el) {
          if (!el) return false;
          var style = null;
          try {
            style = window.getComputedStyle(el);
          } catch (err) {}
          if (!style) return true;
          if (style.display === 'none' || style.visibility === 'hidden') return false;
          return true;
        }

        function getWindowScrollY() {
          return window.pageYOffset || document.documentElement.scrollTop || document.body.scrollTop || 0;
        }

        function canElementScrollVertically(el) {
          if (!el) return false;
          var style = null;
          try {
            style = window.getComputedStyle(el);
          } catch (err) {}
          var overflowY = style ? String(style.overflowY || '') : '';
          var allowsScroll = overflowY === 'auto' || overflowY === 'scroll' || overflowY === 'overlay';
          return allowsScroll && (el.scrollHeight - el.clientHeight > 4);
        }

        function getWorkflowScrollNodes() {
          var activeTab = getActiveTabPane();

          var seen = [];
          function pushNode(node) {
            if (!node) return;
            if (seen.indexOf(node) !== -1) return;
            seen.push(node);
          }

          var scope = activeTab || document;
          pushNode(activeTab);
          pushNode(scope.querySelector('.app-main-pane-search-results'));
          pushNode(scope.querySelector('.app-main-pane'));
          pushNode(scope.querySelector('.content-wrapper'));
          pushNode(scope.querySelector('.plots-zoom-wrap'));
          pushNode(document.getElementById('plot-container'));
          pushNode(document.getElementById('ortho-plot-container'));

          var dynamicNodes = scope.querySelectorAll('.content-wrapper, .app-main-pane, .app-main-pane-search-results, .plots-zoom-wrap, [data-scroll-container], .tab-pane');
          dynamicNodes.forEach(function (node) {
            pushNode(node);
          });

          return seen.filter(function (node) {
            return !!node && ((node.scrollTop || 0) > 0 || canElementScrollVertically(node));
          });
        }

        function getActiveScrollTopValue() {
          var nodes = getWorkflowScrollNodes();
          var maxTop = 0;
          nodes.forEach(function (node) {
            var topVal = node && node.scrollTop ? node.scrollTop : 0;
            if (topVal > maxTop) maxTop = topVal;
          });
          if (maxTop > 0) return maxTop;
          return getWindowScrollY();
        }

        function isWorkflowTabActive() {
          var target = getActiveNavTarget();
          return target === 'homologous' || target === 'orthologous';
        }

        function updateFloatingTopVisibility(scrollYValue) {
          var floatingBtn = getFloatingTopButton();
          if (!floatingBtn) return;
          var scrollY = isFinite(scrollYValue) ? scrollYValue : getActiveScrollTopValue();
          var shouldShow = scrollY > 120;
          floatingBtn.classList.toggle('is-visible', shouldShow);
          floatingBtn.setAttribute('aria-hidden', shouldShow ? 'false' : 'true');
        }

        function updateQuickFabScrollTopVisibility() {
          var root = getQuickFabRoot();
          var mainToggle = getQuickFabMainToggle();
          var scrollTopBtn = getQuickFabScrollTopToggle();
          var scrollY = getActiveScrollTopValue();
          var isWorkflowActive = isWorkflowTabActive();
          var isFabEnabled = isQuickFabEnabled();
          var hasWorkflowScroll = getWorkflowScrollNodes().length > 0;
          var shouldShow = isFabEnabled && isWorkflowActive && !isQuickFabOpen() && (scrollY > 120 || hasWorkflowScroll);

          if (root) {
            root.classList.toggle('is-disabled', !isFabEnabled);
            root.classList.toggle('is-hidden', !isFabEnabled || !isWorkflowActive);
            root.setAttribute('aria-hidden', (isFabEnabled && isWorkflowActive) ? 'false' : 'true');
            if (isFabEnabled && isWorkflowActive) {
              root.classList.toggle('show-scroll-top', shouldShow);
            } else {
              root.classList.remove('show-scroll-top');
              if (root.classList.contains('is-open')) {
                setQuickFabPanel(null);
                root.classList.remove('is-open', 'has-open-panel');
              }
              if (mainToggle) {
                mainToggle.setAttribute('aria-expanded', 'false');
              }
            }
          }

          if (scrollTopBtn) {
            scrollTopBtn.setAttribute('aria-hidden', shouldShow ? 'false' : 'true');
          }
          updateFloatingTopVisibility(scrollY);
        }

        function scheduleQuickFabVisibilityUpdate() {
          if (quickFabVisibilityFrame !== null) return;
          var scheduleFrame = window.requestAnimationFrame || function(callback) {
            return window.setTimeout(callback, 16);
          };
          quickFabVisibilityFrame = scheduleFrame(function() {
            quickFabVisibilityFrame = null;
            updateQuickFabScrollTopVisibility();
          });
        }

        function observeQuickFabScrollSize(node) {
          if (!node || typeof window.ResizeObserver !== 'function') return;
          if (!quickFabResizeObserver) {
            quickFabResizeObserver = new window.ResizeObserver(scheduleQuickFabVisibilityUpdate);
          }
          if (node.getAttribute('data-fab-resize-bound') === '1') return;
          node.setAttribute('data-fab-resize-bound', '1');
          quickFabResizeObserver.observe(node);
        }

        function scrollAppToTop() {
          var prefersReducedMotion = false;
          try {
            prefersReducedMotion = !!(window.matchMedia && window.matchMedia('(prefers-reduced-motion: reduce)').matches);
          } catch (err) {}

          var nodes = getWorkflowScrollNodes();
          nodes.forEach(function (node) {
            if (!node || typeof node.scrollTo !== 'function') return;
            node.scrollTo({
              top: 0,
              behavior: prefersReducedMotion ? 'auto' : 'smooth'
            });
          });

          try {
            window.scrollTo({
              top: 0,
              behavior: prefersReducedMotion ? 'auto' : 'smooth'
            });
          } catch (err) {
            window.scrollTo(0, 0);
          }
        }

        function bindQuickFabScrollListeners(scope) {
          var rootScope = scope || document;
          if (!rootScope || !rootScope.querySelectorAll) return;

          var scrollTargets = rootScope.querySelectorAll(
            '.app-main, .app-main > .tab-content, .app-main > .tab-content > .tab-pane, .app-main-pane, .app-main-pane-search-results'
          );

          scrollTargets.forEach(function (node) {
            if (!node) return;
            if (node.getAttribute('data-fab-scroll-bound') !== '1') {
              node.setAttribute('data-fab-scroll-bound', '1');
              node.addEventListener('scroll', scheduleQuickFabVisibilityUpdate, { passive: true });
            }
            observeQuickFabScrollSize(node);
          });
        }

        function setQuickFabPanel(panelName) {
          var root = getQuickFabRoot();
          var searchPanel = getQuickFabSearchPanel();
          var modePanel = getQuickFabModePanel();
          var zoomPanel = getQuickFabZoomPanel();
          var searchToggle = getQuickFabSearchToggle();
          var modeToggle = getQuickFabModeToggle();
          var zoomToggle = getQuickFabZoomToggle();
          quickFabPanel = panelName || null;

          if (searchPanel) {
            searchPanel.classList.toggle('is-open', quickFabPanel === 'search');
          }
          if (modePanel) {
            modePanel.classList.toggle('is-open', quickFabPanel === 'mode');
          }
          if (zoomPanel) {
            zoomPanel.classList.toggle('is-open', quickFabPanel === 'zoom');
          }
          if (searchToggle) {
            searchToggle.classList.toggle('is-active', quickFabPanel === 'search');
            searchToggle.setAttribute('aria-expanded', quickFabPanel === 'search' ? 'true' : 'false');
          }
          if (modeToggle) {
            modeToggle.classList.toggle('is-active', quickFabPanel === 'mode');
            modeToggle.setAttribute('aria-expanded', quickFabPanel === 'mode' ? 'true' : 'false');
          }
          if (zoomToggle) {
            zoomToggle.classList.toggle('is-active', quickFabPanel === 'zoom');
            zoomToggle.setAttribute('aria-expanded', quickFabPanel === 'zoom' ? 'true' : 'false');
          }
          if (root) {
            root.classList.toggle('has-open-panel', !!quickFabPanel);
          }

          if (quickFabPanel === 'mode') {
            syncQuickFabModeControls();
          }

          if (quickFabPanel === 'zoom') {
            if (window.syncQuickFabZoomControls) window.syncQuickFabZoomControls();
          }

          if (quickFabPanel === 'search') {
            var searchInput = getQuickFabSearchInput();
            if (searchInput) {
              if (!String(searchInput.value || '').trim()) {
                searchInput.value = getBestSearchQuery();
              }
              setTimeout(function () {
                searchInput.focus();
                searchInput.select();
              }, 0);
            }
          }
        }

        function setQuickFabOpen(open) {
          var root = getQuickFabRoot();
          var mainToggle = getQuickFabMainToggle();
          if (!root) return;
          var nextOpen = !!open;
          root.classList.toggle('is-open', nextOpen);
          if (mainToggle) {
            mainToggle.setAttribute('aria-expanded', nextOpen ? 'true' : 'false');
          }
          if (!nextOpen) {
            setQuickFabPanel(null);
          } else {
            syncQuickFabModeControls();
            if (window.syncQuickFabZoomControls) window.syncQuickFabZoomControls();
          }
          updateQuickFabScrollTopVisibility();
        }

        function toggleQuickFab() {
          setQuickFabOpen(!isQuickFabOpen());
        }

        function getQuickFabWorkflowTarget() {
          var current = getActiveNavTarget();
          if (current === 'homologous' || current === 'orthologous') {
            return current;
          }
          return lastWorkflowTarget === 'orthologous' ? 'orthologous' : 'homologous';
        }

        function getQuickFabModeInputId(target) {
          if (target === 'orthologous') return 'ortho_visual_mode';
          return 'homo_visual_mode';
        }

        function getQuickFabModeChoices(target) {
          if (target === 'orthologous') return ['compact', 'detailed', 'aligned', 'pip_blocks', 'pip_multipip'];
          return ['compact', 'detailed'];
        }

        function syncQuickFabModeControls() {
          var root = getQuickFabRoot();
          if (!root) return;

          var target = getQuickFabWorkflowTarget();
          var inputId = getQuickFabModeInputId(target);
          var modeChoices = getQuickFabModeChoices(target);
          var checkedRadio = document.querySelector('input[name=\"' + inputId + '\"]:checked');
          var activeMode = checkedRadio ? String(checkedRadio.value || '').trim() : modeChoices[0];
          if (modeChoices.indexOf(activeMode) === -1) {
            activeMode = modeChoices[0];
          }

          var context = document.getElementById('app-fab-mode-context');
          if (context) {
            context.textContent = target === 'orthologous'
              ? 'Cross-Species mode'
              : 'Multi-Gene mode';
          }

          var modeButtons = root.querySelectorAll('.app-fab-mode-option[data-mode]');
          modeButtons.forEach(function (btn) {
            var mode = String(btn.getAttribute('data-mode') || '');
            var isAllowed = modeChoices.indexOf(mode) !== -1;
            var isActive = isAllowed && mode === activeMode;
            btn.classList.toggle('is-hidden', !isAllowed);
            btn.classList.toggle('is-active', isActive);
            btn.setAttribute('aria-hidden', isAllowed ? 'false' : 'true');
            btn.setAttribute('aria-pressed', isActive ? 'true' : 'false');
            if (isAllowed) {
              btn.removeAttribute('tabindex');
            } else {
              btn.setAttribute('tabindex', '-1');
            }
          });

          var alignmentGroup = document.getElementById('app-fab-alignment-mode-group');
          if (alignmentGroup) {
            var showAlignment = target === 'orthologous';
            alignmentGroup.classList.toggle('is-hidden', !showAlignment);
            alignmentGroup.setAttribute('aria-hidden', showAlignment ? 'false' : 'true');
          }
        }

        function setQuickFabMode(mode) {
          var nextMode = String(mode || '').trim();
          if (!nextMode) return;
          var target = getQuickFabWorkflowTarget();
          var inputId = getQuickFabModeInputId(target);
          var modeChoices = getQuickFabModeChoices(target);
          if (modeChoices.indexOf(nextMode) === -1) return;

          var radio = document.querySelector('input[name=\"' + inputId + '\"][value=\"' + nextMode + '\"]');
          if (!radio) return;

          if (!radio.checked) {
            radio.checked = true;
            try {
              radio.dispatchEvent(new Event('change', { bubbles: true }));
            } catch (err) {
              var ev = document.createEvent('Event');
              ev.initEvent('change', true, true);
              radio.dispatchEvent(ev);
            }
          }

          syncQuickFabModeControls();
          setQuickFabPanel(null);
          setQuickFabOpen(false);
        }

        function runQuickFabSearch() {
          var searchInput = getQuickFabSearchInput();
          if (!searchInput) return;

          var query = String(searchInput.value || '').trim();
          if (!query) {
            query = getBestSearchQuery();
          }
          if (!query && globalSearchChips.length) {
            query = globalSearchChips.join(' || ');
          }
          if (!query) {
            searchInput.focus();
            return;
          }

          searchInput.value = query;

          var globalInput = getGlobalSearchInput();
          var collapsedInput = getCollapsedSearchInput();
          if (globalInput) globalInput.value = query;
          if (collapsedInput) collapsedInput.value = query;

          if (window.Shiny && Shiny.setInputValue) {
            Shiny.setInputValue('global_search_query', query, { priority: 'event' });
            Shiny.setInputValue('global_search_query_collapsed', query, { priority: 'event' });
          }

          hideGlobalSuggestions();
          hideCollapsedSuggestions();

          var runBtn = document.getElementById('global_search_go');
          if (runBtn) runBtn.click();

          setQuickFabPanel(null);
          setQuickFabOpen(false);
        }

        function initializeQuickFab() {
          var root = getQuickFabRoot();
          if (!root || root.getAttribute('data-ready') === '1') return;
          root.setAttribute('data-ready', '1');

          var mainToggle = getQuickFabMainToggle();
          var scrollTopToggle = getQuickFabScrollTopToggle();
          var floatingTopToggle = getFloatingTopButton();
          var searchToggle = getQuickFabSearchToggle();
          var modeToggle = getQuickFabModeToggle();
          var zoomToggle = getQuickFabZoomToggle();
          var searchRunBtn = document.getElementById('app-fab-search-run');
          var searchInput = getQuickFabSearchInput();
          var settingsToggle = getQuickFabEnabledToggle();

          applyQuickFabEnabled(getStoredQuickFabEnabled());

          if (mainToggle) {
            mainToggle.addEventListener('click', function (evt) {
              evt.preventDefault();
              evt.stopPropagation();
              toggleQuickFab();
            });
          }

          if (scrollTopToggle) {
            scrollTopToggle.addEventListener('click', function (evt) {
              evt.preventDefault();
              evt.stopPropagation();
              scrollAppToTop();
            });
          }

          if (floatingTopToggle) {
            floatingTopToggle.addEventListener('click', function (evt) {
              evt.preventDefault();
              evt.stopPropagation();
              scrollAppToTop();
            });
          }

          if (searchToggle) {
            searchToggle.addEventListener('click', function (evt) {
              evt.preventDefault();
              evt.stopPropagation();
              if (!isQuickFabOpen()) setQuickFabOpen(true);
              setQuickFabPanel(quickFabPanel === 'search' ? null : 'search');
            });
          }

          if (modeToggle) {
            modeToggle.addEventListener('click', function (evt) {
              evt.preventDefault();
              evt.stopPropagation();
              if (!isQuickFabOpen()) setQuickFabOpen(true);
              setQuickFabPanel(quickFabPanel === 'mode' ? null : 'mode');
            });
          }

          if (zoomToggle) {
            zoomToggle.addEventListener('click', function (evt) {
              evt.preventDefault();
              evt.stopPropagation();
              if (!isQuickFabOpen()) setQuickFabOpen(true);
              setQuickFabPanel(quickFabPanel === 'zoom' ? null : 'zoom');
              if (window.syncQuickFabZoomControls) {
                setTimeout(window.syncQuickFabZoomControls, 0);
              }
            });
          }

          if (searchRunBtn) {
            searchRunBtn.addEventListener('click', function (evt) {
              evt.preventDefault();
              evt.stopPropagation();
              runQuickFabSearch();
            });
          }

          if (searchInput) {
            searchInput.addEventListener('keydown', function (evt) {
              if (isSpaceBatchShortcut(evt)) {
                var quickSpaceChip = String(searchInput.value || '').trim();
                if (quickSpaceChip) {
                  evt.preventDefault();
                  addGlobalSearchChip(quickSpaceChip);
                  searchInput.value = '';
                }
                return;
              }
              if (evt.key === 'Enter' && (evt.ctrlKey || evt.metaKey)) {
                evt.preventDefault();
                var quickChip = String(searchInput.value || '').trim();
                if (quickChip) {
                  addGlobalSearchChip(quickChip);
                  searchInput.value = '';
                }
                return;
              }
              if (evt.key === 'Enter') {
                evt.preventDefault();
                runQuickFabSearch();
                return;
              }
              if (evt.key === 'Escape') {
                evt.preventDefault();
                setQuickFabPanel(null);
              }
            });
          }

          var modeButtons = root.querySelectorAll('.app-fab-mode-option[data-mode]');
          modeButtons.forEach(function (btn) {
            btn.addEventListener('click', function (evt) {
              evt.preventDefault();
              evt.stopPropagation();
              setQuickFabMode(btn.getAttribute('data-mode') || '');
            });
          });

          document.addEventListener('keydown', function (evt) {
            if (evt.key === 'Escape' && isQuickFabOpen()) {
              setQuickFabPanel(null);
              setQuickFabOpen(false);
            }
          });

          document.addEventListener('change', function (evt) {
            var target = evt && evt.target ? evt.target : null;
            if (!target) return;
            var name = String(target.name || '');
            if (name === 'homo_visual_mode' || name === 'ortho_visual_mode') {
              syncQuickFabModeControls();
              updateCompactModeLayoutState();
              updateOrthoSpecialCardVisibility();
              updateHomoSpecialCardVisibility();
            }
          });

          document.addEventListener('change', function (evt) {
            var target = evt && evt.target ? evt.target : null;
            if (!target || target.id !== 'quick_fab_enabled') return;
            applyQuickFabEnabled(!!target.checked);
          });

          if (settingsToggle) {
            settingsToggle.checked = getStoredQuickFabEnabled();
          }

          window.addEventListener('scroll', scheduleQuickFabVisibilityUpdate, { passive: true });
          window.addEventListener('resize', scheduleQuickFabVisibilityUpdate, { passive: true });
          document.addEventListener('shown.bs.tab', scheduleQuickFabVisibilityUpdate);
          document.addEventListener('shiny:value', scheduleQuickFabVisibilityUpdate);
          bindQuickFabScrollListeners(document);

          // Attach listeners to any pane/container added later by Shiny re-renders.
          var paneObserver = new MutationObserver(function() {
            bindQuickFabScrollListeners(document);
            scheduleQuickFabVisibilityUpdate();
          });
          if (document.body && document.body.nodeType === 1) {
            paneObserver.observe(document.body, { childList: true, subtree: true });
          } else {
            document.addEventListener('DOMContentLoaded', function () {
              if (document.body && document.body.nodeType === 1) {
                paneObserver.observe(document.body, { childList: true, subtree: true });
              }
            }, { once: true });
          }

          syncQuickFabModeControls();
          updateCompactModeLayoutState();
          updateOrthoSpecialCardVisibility();
          updateHomoSpecialCardVisibility();
          updateQuickFabScrollTopVisibility();
        }

        function syncCollapsedSearchFromMain() {
          var input = getCollapsedSearchInput();
          if (!input) return;
          input.value = getBestSearchQuery();
        }

        function syncMainSearchFromCollapsed() {
          var collapsedInput = getCollapsedSearchInput();
          if (!collapsedInput) return;
          var query = String(collapsedInput.value || '').trim();
          var globalInput = document.getElementById('global_search_query');
          if (globalInput) globalInput.value = query;
          if (window.Shiny && Shiny.setInputValue) {
            Shiny.setInputValue('global_search_query', query, { priority: 'event' });
            Shiny.setInputValue('global_search_query_collapsed', query, { priority: 'event' });
          }
        }

        function setCollapsedSearchPanelOpen(open) {
          var canOpen = !!open && isSidebarCollapsed();
          var panel = getCollapsedSearchPanel();
          var toggleBtn = getCollapsedSearchToggle();
          if (panel) {
            panel.classList.toggle('is-open', canOpen);
          }
          if (toggleBtn) {
            toggleBtn.classList.toggle('is-expanded', canOpen);
            toggleBtn.setAttribute('aria-expanded', canOpen ? 'true' : 'false');
          }
          if (!canOpen) {
            hideCollapsedSuggestions();
          }
          if (canOpen) {
            syncCollapsedSuggestionDataList();
            syncCollapsedSearchFromMain();
            var input = getCollapsedSearchInput();
            if (input) {
              setTimeout(function () {
                input.focus();
                input.select();
                renderCollapsedSuggestions(input.value || '');
              }, 0);
            }
          }
        }

        function applySidebarCollapsed(collapsed) {
          var shell = document.querySelector('.app-shell');
          var isMobile = window.matchMedia('(max-width: 960px)').matches;
          var desiredCollapsed = !!collapsed;
          var nextCollapsed = isMobile ? false : desiredCollapsed;
          var stateChanged = !!shell && shell.classList.contains('sidebar-collapsed') !== nextCollapsed;
          if (stateChanged) prepareHomeSidebarTransition(nextCollapsed);
          if (shell) {
            shell.classList.toggle('sidebar-collapsed', nextCollapsed);
          }

          var btn = document.getElementById('sidebar-collapse-btn');
          if (btn) {
            btn.setAttribute('aria-expanded', nextCollapsed ? 'false' : 'true');
            var sidebarLabel = nextCollapsed ? 'Expand sidebar' : 'Collapse sidebar';
            btn.setAttribute('title', sidebarLabel);
            btn.setAttribute('aria-label', sidebarLabel);
            var glyph = btn.querySelector('.sidebar-collapse-glyph');
            if (glyph) glyph.textContent = nextCollapsed ? '▶' : '◀';
          }

          setCollapsedSearchPanelOpen(false);

          try {
            localStorage.setItem('cgv-sidebar-collapsed', desiredCollapsed ? '1' : '0');
          } catch (err) {}
        }

        function prepareHomeSidebarTransition(nextCollapsed) {
          if (getActiveNavTarget() !== 'home') return false;
          var iframe = document.getElementById('cgv-home-iframe');
          if (!iframe) return false;
          var shell = document.querySelector('.app-shell');
          var sidebar = shell ? shell.querySelector('.app-sidebar') : null;
          var w = iframe.getBoundingClientRect().width;
          var sidebarWidth = sidebar ? sidebar.getBoundingClientRect().width : (nextCollapsed ? 300 : 88);
          var targetSidebarWidth = nextCollapsed ? 88 : 300;
          var destinationWidth = Math.max(
            320,
            Math.round(w + sidebarWidth - targetSidebarWidth)
          );
          if (w && w > 100) {
            iframe.style.width = destinationWidth + 'px';
            iframe.style.maxWidth = 'none';
          }
          iframe.classList.add('cgv-home-iframe-resizing');
          if (shell) shell.classList.add('home-sidebar-transitioning');
          if (iframe.__cgvHomeSidebarTimer) {
            clearTimeout(iframe.__cgvHomeSidebarTimer);
          }
          if (iframe.__cgvHomeSidebarEndHandler && sidebar) {
            sidebar.removeEventListener('transitionend', iframe.__cgvHomeSidebarEndHandler);
          }
          if (iframe.contentWindow) {
            try { iframe.contentWindow.postMessage({ type: 'cgv-parent-resizing' }, '*'); } catch (err) {}
          }

          var finishTransition = function () {
            if (iframe.__cgvHomeSidebarTimer) {
              clearTimeout(iframe.__cgvHomeSidebarTimer);
              iframe.__cgvHomeSidebarTimer = null;
            }
            if (sidebar && iframe.__cgvHomeSidebarEndHandler) {
              sidebar.removeEventListener('transitionend', iframe.__cgvHomeSidebarEndHandler);
            }
            iframe.__cgvHomeSidebarEndHandler = null;
            iframe.style.width = '100%';
            iframe.style.maxWidth = '';
            iframe.classList.remove('cgv-home-iframe-resizing');
            if (shell) shell.classList.remove('home-sidebar-transitioning');
            if (iframe.contentWindow) {
              try { iframe.contentWindow.postMessage({ type: 'cgv-parent-resize-done' }, '*'); } catch (err) {}
            }
          };

          iframe.__cgvHomeSidebarEndHandler = function (event) {
            if (event.target === sidebar && (event.propertyName === 'width' || event.propertyName === 'min-width')) {
              finishTransition();
            }
          };
          if (sidebar) sidebar.addEventListener('transitionend', iframe.__cgvHomeSidebarEndHandler);
          iframe.__cgvHomeSidebarTimer = setTimeout(finishTransition, 260);
          return true;
        }

        function notifyPlotLayoutResize() {
          if (!window || typeof window.dispatchEvent !== 'function') return;
          if (getActiveNavTarget() === 'home') return;
          window.dispatchEvent(new Event('resize'));
          setTimeout(function () {
            window.dispatchEvent(new Event('resize'));
          }, 320);
        }

        function toggleSidebarCollapsed() {
          var isMobile = window.matchMedia('(max-width: 960px)').matches;
          if (isMobile) {
            toggleMobileNav();
            return;
          }
          applySidebarCollapsed(!getStoredSidebarCollapsed());
          notifyPlotLayoutResize();
        }

        function toggleMobileNav() {
          var shell = document.querySelector('.app-shell');
          if (!shell) return;
          var isOpen = shell.classList.contains('mobile-nav-open');
          shell.classList.toggle('mobile-nav-open', !isOpen);
          var btn = document.getElementById('sidebar-collapse-btn');
          if (btn) {
            var glyph = btn.querySelector('.sidebar-collapse-glyph');
            if (glyph) glyph.textContent = isOpen ? String.fromCharCode(0x2630) : String.fromCharCode(0x2715);
            btn.setAttribute('aria-expanded', isOpen ? 'false' : 'true');
            btn.setAttribute('title', isOpen ? 'Open menu' : 'Close menu');
          }
        }
        window.toggleMobileNav = toggleMobileNav;

        function getGlobalSearchInput() {
          return document.getElementById('global_search_query');
        }

        function getGlobalSuggestionPanel() {
          return document.getElementById('global-search-suggestions');
        }

        function getCollapsedSearchSuggestionPanel() {
          return document.getElementById('global-search-suggestions-collapsed');
        }

        function getGlobalSuggestionDataList() {
          return document.getElementById('global_search_query_suggestions');
        }

        function getGlobalChipPayloadInput() {
          return document.getElementById('global_search_chip_payload');
        }

        function getGlobalChipListNodes() {
          var nodes = document.querySelectorAll('.app-search-chip-list-sync');
          if (!nodes || !nodes.length) return [];
          return Array.prototype.slice.call(nodes);
        }

        function getChipEmptyLabel(node) {
          if (!node) return 'No genes added yet.';
          var custom = String(node.getAttribute('data-empty-label') || '').trim();
          return custom || 'No genes added yet.';
        }

        function getGlobalChipAddBtn() {
          return document.getElementById('global_search_add_chip');
        }

        function getGlobalChipClearBtn() {
          return document.getElementById('global_search_clear_chips');
        }

        function getGlobalChipCountNode() {
          return document.getElementById('global-search-batch-count');
        }

        function formatGlobalBatchCount(countValue) {
          var nn = parseInt(countValue, 10);
          if (!isFinite(nn) || nn < 0) nn = 0;
          return 'Batch: ' + nn + ' gene' + (nn === 1 ? '' : 's');
        }

        function isSpaceBatchShortcut(evt) {
          if (!evt) return false;
          // Space-to-batch is only valid in Homologous Search mode
          if (lastWorkflowTarget !== 'homologous') return false;
          if (evt.ctrlKey || evt.metaKey || evt.altKey) return false;
          var key = String(evt.key || '');
          var code = String(evt.code || '');
          return key === ' ' || key === 'Spacebar' || code === 'Space';
        }

        function normalizeChipGene(value) {
          var vv = (value === null || value === undefined) ? '' : String(value);
          vv = vv.replace(/\\s+/g, ' ').trim();
          return vv;
        }

        function hasGlobalChipGene(gene) {
          var key = normalizeChipGene(gene).toLowerCase();
          if (!key) return false;
          return globalSearchChips.some(function (item) {
            return normalizeChipGene(item).toLowerCase() === key;
          });
        }

        function setGlobalChipActionState() {
          var clearBtn = getGlobalChipClearBtn();
          if (clearBtn) clearBtn.disabled = globalSearchChips.length === 0;
          var root = document.getElementById('global-search-chipbar');
          if (root) root.classList.toggle('has-chips', globalSearchChips.length > 0);
          var countNode = getGlobalChipCountNode();
          if (countNode) {
            countNode.textContent = formatGlobalBatchCount(globalSearchChips.length);
            countNode.classList.toggle('is-empty', globalSearchChips.length === 0);
          }
        }

        function syncGlobalChipPayload() {
          var payloadInput = getGlobalChipPayloadInput();
          if (payloadInput) {
            payloadInput.value = globalSearchChips.join(' || ');
          }
          if (window.Shiny && Shiny.setInputValue) {
            Shiny.setInputValue('global_search_chip_payload', globalSearchChips.slice(), { priority: 'event' });
          }
          setGlobalChipActionState();
        }

        function renderGlobalSearchChips() {
          var nodes = getGlobalChipListNodes();
          if (!nodes.length) {
            syncGlobalChipPayload();
            return;
          }

          nodes.forEach(function (node) {
            node.innerHTML = '';
            if (!globalSearchChips.length) {
              var empty = document.createElement('span');
              empty.className = 'app-search-chip-empty';
              empty.textContent = getChipEmptyLabel(node);
              node.appendChild(empty);
              return;
            }

            globalSearchChips.forEach(function (gene, idx) {
              var chip = document.createElement('span');
              chip.className = 'app-search-chip';

              var label = document.createElement('span');
              label.className = 'app-search-chip-label';
              label.textContent = gene;
              chip.appendChild(label);

              var removeBtn = document.createElement('button');
              removeBtn.type = 'button';
              removeBtn.className = 'app-search-chip-remove';
              removeBtn.setAttribute('data-chip-index', String(idx));
              removeBtn.setAttribute('title', 'Remove from batch');
              removeBtn.setAttribute('aria-label', 'Remove from batch');
              removeBtn.textContent = 'x';
              chip.appendChild(removeBtn);

              node.appendChild(chip);
            });
          });

          syncGlobalChipPayload();
        }

        function addGlobalSearchChip(rawGene, options) {
          var opts = options || {};
          var gene = normalizeChipGene(rawGene);
          if (!gene) return false;
          if (hasGlobalChipGene(gene)) return false;

          globalSearchChips.push(gene);
          renderGlobalSearchChips();

          if (!opts.keepInput) {
            var input = getGlobalSearchInput();
            if (input) input.value = '';
            if (window.Shiny && Shiny.setInputValue) {
              Shiny.setInputValue('global_search_query', '', { priority: 'event' });
            }
          }

          if (!opts.keepCollapsedInput) {
            var collapsedInput = getCollapsedSearchInput();
            if (collapsedInput) collapsedInput.value = '';
            if (window.Shiny && Shiny.setInputValue) {
              Shiny.setInputValue('global_search_query_collapsed', '', { priority: 'event' });
            }
          }

          hideGlobalSuggestions();
          hideCollapsedSuggestions();
          return true;
        }

        function removeGlobalSearchChipAt(index) {
          var idx = parseInt(index, 10);
          if (!isFinite(idx) || idx < 0 || idx >= globalSearchChips.length) return false;
          globalSearchChips.splice(idx, 1);
          renderGlobalSearchChips();
          return true;
        }

        function clearGlobalSearchChips() {
          if (!globalSearchChips.length) return false;
          globalSearchChips = [];
          renderGlobalSearchChips();
          return true;
        }

        function clearAllSearchState() {
          clearGlobalSearchChips();

          var input = getGlobalSearchInput();
          if (input) input.value = '';

          var collapsedInput = getCollapsedSearchInput();
          if (collapsedInput) collapsedInput.value = '';

          var homoInput = document.getElementById('filter1');
          if (homoInput) homoInput.value = '';

          var orthoInput = document.getElementById('gene_name');
          if (orthoInput) orthoInput.value = '';

          var fabInput = getQuickFabSearchInput();
          if (fabInput) fabInput.value = '';

          hideGlobalSuggestions();
          hideCollapsedSuggestions();

          if (window.Shiny && Shiny.setInputValue) {
            Shiny.setInputValue('global_search_query', '', { priority: 'event' });
            Shiny.setInputValue('global_search_query_collapsed', '', { priority: 'event' });
            Shiny.setInputValue('filter1', '', { priority: 'event' });
            Shiny.setInputValue('gene_name', '', { priority: 'event' });
            Shiny.setInputValue('global_search_chip_payload', [], { priority: 'event' });
          }
        }

        if (window.Shiny && typeof Shiny.addCustomMessageHandler === 'function') {
          Shiny.addCustomMessageHandler('clear_global_search_state', function(message) {
            clearAllSearchState();
          });
          // Retrigger homologous search after organism-change confirmation
          Shiny.addCustomMessageHandler('cgv_trigger_homo_search', function(message) {
            if (message && message.origin === 'genomic_neighbor') {
              setWorkflowPanel(null);
            }
            var btn = document.getElementById('generate1');
            if (btn) btn.click();
          });
          // Retrigger orthologous search after gene-change confirmation
          Shiny.addCustomMessageHandler('cgv_trigger_ortho_search', function(message) {
            if (message && message.origin === 'genomic_neighbor') {
              setWorkflowPanel(null);
            }
            var btn = document.getElementById('search_gene');
            if (btn) btn.click();
          });
        }

        function parsePartialGeneSuggestionPayload(node) {
          var raw = node && node.getAttribute ? String(node.getAttribute('data-resolution') || '') : '';
          var out = {};
          if (raw) {
            try {
              out = JSON.parse(raw) || {};
            } catch (err) {
              out = {};
            }
          }
          var fallbackGene = String((node && (node.getAttribute('data-gene') || node.value)) || '').trim();
          if (!out.gene && fallbackGene) out.gene = fallbackGene;
          if (!out.plot_gene) out.plot_gene = String((node && node.value) || out.local_gene_id || out.gene || '').trim();
          out.gene = String(out.gene || '').trim();
          out.plot_gene = String(out.plot_gene || '').trim();
          out.local_gene_id = String(out.local_gene_id || '').trim();
          out.local_symbol = String(out.local_symbol || '').trim();
          out.term_type = String(out.term_type || '').trim();
          out.source_db = String(out.source_db || '').trim();
          out.confidence = String(out.confidence || '').trim();
          out.match_role = String(out.match_role || '').trim();
          out.requires_confirmation = !!out.requires_confirmation;
          return out;
        }

        function normalizeGeneMatchFilter(value) {
          return String(value || '').toLocaleLowerCase().replace(/\\s+/g, ' ').trim();
        }

        function updateGeneMatchBrowser(browser, requestedPage) {
          if (!browser) return;
          var allItems = Array.prototype.slice.call(browser.querySelectorAll('[data-gene-match-item=true]'));
          var input = browser.querySelector('.gene-match-filter-input');
          var filterText = normalizeGeneMatchFilter(input ? input.value : '');
          var pageSize = parseInt(browser.getAttribute('data-page-size') || '24', 10);
          if (!isFinite(pageSize) || pageSize < 1) pageSize = 24;
          var matchingItems = allItems.filter(function(item) {
            var searchable = normalizeGeneMatchFilter(item.getAttribute('data-search-text') || item.textContent || '');
            return !filterText || searchable.indexOf(filterText) !== -1;
          });
          var pageCount = Math.max(1, Math.ceil(matchingItems.length / pageSize));
          var currentPage = parseInt(requestedPage || browser.getAttribute('data-current-page') || '1', 10);
          if (!isFinite(currentPage)) currentPage = 1;
          currentPage = Math.max(1, Math.min(pageCount, currentPage));
          browser.setAttribute('data-current-page', String(currentPage));

          var visibleStart = (currentPage - 1) * pageSize;
          var visibleEnd = Math.min(visibleStart + pageSize, matchingItems.length);
          var visibleSet = new Set(matchingItems.slice(visibleStart, visibleEnd));
          allItems.forEach(function(item) {
            item.hidden = !visibleSet.has(item);
          });

          var exactCount = allItems.filter(function(item) {
            return item.getAttribute('data-exact-match') === 'true';
          }).length;
          var resultCount = browser.querySelector('.gene-match-result-count');
          if (resultCount) {
            var resultLabel = String(browser.getAttribute('data-result-label') || 'suggestion').trim() || 'suggestion';
            var totalLabel = allItems.length + ' total ' + resultLabel + (allItems.length === 1 ? '' : 's');
            var exactLabel = exactCount ? ' • ' + exactCount + ' exact match' + (exactCount === 1 ? '' : 'es') : '';
            resultCount.textContent = filterText
              ? matchingItems.length + ' matching • ' + totalLabel + exactLabel
              : totalLabel + exactLabel;
          }

          var pageStatus = browser.querySelector('.gene-match-page-status');
          if (pageStatus) {
            pageStatus.textContent = matchingItems.length
              ? 'Showing ' + (visibleStart + 1) + '–' + visibleEnd + ' of ' + matchingItems.length +
                ' • Page ' + currentPage + ' of ' + pageCount
              : 'No suggestions match this filter';
          }
          var pagination = browser.querySelector('.gene-match-pagination');
          if (pagination) pagination.hidden = matchingItems.length === 0 || pageCount <= 1;
          var prev = browser.querySelector('.gene-match-page-prev');
          var next = browser.querySelector('.gene-match-page-next');
          if (prev) prev.disabled = currentPage <= 1;
          if (next) next.disabled = currentPage >= pageCount;

          var empty = browser.querySelector('.gene-match-filter-empty');
          if (!empty) {
            empty = document.createElement('p');
            empty.className = 'gene-match-filter-empty';
            empty.textContent = 'No suggestions match this filter.';
            var list = browser.querySelector('.partial-gene-suggestion-list');
            if (list && list.parentNode) list.parentNode.insertBefore(empty, list.nextSibling);
          }
          empty.hidden = matchingItems.length !== 0;
          browser.setAttribute('data-gene-match-ready', 'true');
        }

        function initGeneMatchBrowsers(root) {
          var scope = root && root.querySelectorAll ? root : document;
          var browsers = [];
          if (scope.matches && scope.matches('.gene-match-browser')) browsers.push(scope);
          browsers = browsers.concat(Array.prototype.slice.call(scope.querySelectorAll('.gene-match-browser')));
          browsers.forEach(function(browser) {
            if (browser.getAttribute('data-gene-match-ready') !== 'true') updateGeneMatchBrowser(browser, 1);
          });
        }

        document.addEventListener('input', function(evt) {
          var input = evt.target;
          if (!input || !input.matches || !input.matches('.gene-match-filter-input')) return;
          updateGeneMatchBrowser(input.closest('.gene-match-browser'), 1);
        }, true);

        document.addEventListener('click', function(evt) {
          var pageButton = evt.target && evt.target.closest ? evt.target.closest('.gene-match-page-btn') : null;
          if (!pageButton) return;
          evt.preventDefault();
          var browser = pageButton.closest('.gene-match-browser');
          if (!browser) return;
          var currentPage = parseInt(browser.getAttribute('data-current-page') || '1', 10) || 1;
          updateGeneMatchBrowser(browser, currentPage + (pageButton.matches('.gene-match-page-prev') ? -1 : 1));
        }, true);

        var geneMatchObserver = new MutationObserver(function(mutations) {
          mutations.forEach(function(mutation) {
            Array.prototype.slice.call(mutation.addedNodes || []).forEach(function(node) {
              if (node && node.nodeType === 1) initGeneMatchBrowsers(node);
            });
          });
        });
        if (document.body) geneMatchObserver.observe(document.body, { childList: true, subtree: true });
        initGeneMatchBrowsers(document);

        document.addEventListener('click', function(evt) {
          var btn = evt.target && evt.target.closest ? evt.target.closest('.partial-gene-suggestion-btn') : null;
          if (!btn || !window.Shiny || !Shiny.setInputValue) return;
          evt.preventDefault();
          var mode = String(btn.getAttribute('data-mode') || 'homologous');
          var item = parsePartialGeneSuggestionPayload(btn);
          var gene = String(item.plot_gene || item.local_gene_id || item.gene || '').trim();
          if (!gene) return;
          Shiny.setInputValue('partial_gene_suggestion_pick', {
            mode: mode,
            gene: gene,
            item: item,
            nonce: Date.now()
          }, { priority: 'event' });
        }, true);

        document.addEventListener('click', function(evt) {
          var selectAllBtn = evt.target && evt.target.closest ? evt.target.closest('.gene-match-select-all') : null;
          if (!selectAllBtn) return;
          evt.preventDefault();
          var modal = selectAllBtn.closest('.modal-content') || document;
          var selector = String(selectAllBtn.getAttribute('data-check-selector') || '').trim();
          if (!selector) return;
          var checks = Array.prototype.slice.call(modal.querySelectorAll(selector));
          var shouldSelect = checks.some(function(node) { return !node.checked; });
          checks.forEach(function(node) {
            node.checked = shouldSelect;
            node.dispatchEvent(new Event('change', { bubbles: true }));
          });
        }, true);

        function updateGeneMatchSelection(modal, selector) {
          if (!modal || !selector) return;
          var checks = Array.prototype.slice.call(modal.querySelectorAll(selector));
          var selectedCount = checks.filter(function(node) { return node.checked; }).length;
          modal.querySelectorAll('.gene-match-selection-count').forEach(function(status) {
            if (String(status.getAttribute('data-check-selector') || '') === selector) {
              status.textContent = selectedCount + ' selected';
            }
          });
          modal.querySelectorAll('.gene-match-select-all').forEach(function(button) {
            if (String(button.getAttribute('data-check-selector') || '') !== selector) return;
            var allSelected = checks.length > 0 && selectedCount === checks.length;
            button.setAttribute('aria-pressed', allSelected ? 'true' : 'false');
            var label = button.querySelector('.gene-match-select-all-label');
            if (label) label.textContent = allSelected ? 'Deselect all' : 'Select all';
            var runSelector = String(button.getAttribute('data-run-selector') || '').trim();
            var runButton = runSelector ? modal.querySelector(runSelector) : null;
            if (runButton) runButton.disabled = selectedCount === 0;
          });
        }

        document.addEventListener('change', function(evt) {
          var check = evt.target;
          if (!check || !check.matches ||
              !check.matches('.partial-gene-suggestion-check, .alias-index-match-check')) return;
          var modal = check.closest('.modal-content') || document;
          var selector = check.matches('.alias-index-match-check')
            ? '.alias-index-match-check'
            : '.partial-gene-suggestion-check';
          updateGeneMatchSelection(modal, selector);
        }, true);

        document.addEventListener('click', function(evt) {
          var runBtn = evt.target && evt.target.closest ? evt.target.closest('#partial-gene-suggestion-search-selected') : null;
          if (!runBtn || !window.Shiny || !Shiny.setInputValue) return;
          evt.preventDefault();
          var checks = document.querySelectorAll('.partial-gene-suggestion-check:checked');
          var items = Array.prototype.slice.call(checks).map(parsePartialGeneSuggestionPayload).filter(function(item) {
            return item && String(item.plot_gene || item.local_gene_id || item.gene || '').trim();
          });
          var seen = {};
          items = items.filter(function(item) {
            var key = String(item.plot_gene || item.local_gene_id || item.gene || '').trim();
            if (!key || seen[key]) return false;
            seen[key] = true;
            return true;
          });
          var genes = items.map(function(item) {
            return String(item.plot_gene || item.local_gene_id || item.gene || '').trim();
          });
          if (!genes.length) return;
          Shiny.setInputValue('partial_gene_suggestion_pick', {
            mode: 'homologous',
            genes: genes,
            items: items,
            nonce: Date.now()
          }, { priority: 'event' });
        }, true);

        document.addEventListener('click', function(evt) {
          var runBtn = evt.target && evt.target.closest ? evt.target.closest('#alias-index-match-plot-selected') : null;
          if (!runBtn || !window.Shiny || !Shiny.setInputValue) return;
          evt.preventDefault();
          var modal = runBtn.closest('.modal-content') || document;
          var checks = modal.querySelectorAll('.alias-index-match-check:checked');
          var genes = Array.prototype.slice.call(checks).map(function(node) {
            return String(node.value || '').trim();
          }).filter(function(value, index, arr) {
            return value && arr.indexOf(value) === index;
          });
          if (!genes.length) return;
          Shiny.setInputValue('alias_index_match_pick', {
            mode: 'homologous',
            genes: genes,
            nonce: Date.now()
          }, { priority: 'event' });
        }, true);

        document.addEventListener('click', function(evt) {
          var extBtn = evt.target && evt.target.closest ? evt.target.closest('#partial-gene-suggestion-search-external') : null;
          if (!extBtn || !window.Shiny || !Shiny.setInputValue) return;
          evt.preventDefault();
          var mode = String(extBtn.getAttribute('data-mode') || 'homologous');
          var query = String(extBtn.getAttribute('data-query') || '').trim();
          if (!query) return;
          Shiny.setInputValue('partial_gene_external_alias_search', {
            mode: mode,
            gene: query,
            nonce: Date.now()
          }, { priority: 'event' });
        }, true);

        function keepSummaryContextOpaque() {
          var sections = document.querySelectorAll('#homo_context_section, #ortho_context_section');
          sections.forEach(function(section) {
            if (section.style.opacity && section.style.opacity !== '1') section.style.opacity = '1';
            if (section.style.filter && section.style.filter !== 'none') section.style.filter = 'none';
            section.classList.remove('recalculating');
            section.querySelectorAll('.shiny-bound-output, .recalculating').forEach(function(el) {
              if (el.style.opacity && el.style.opacity !== '1') el.style.opacity = '1';
              if (el.style.filter && el.style.filter !== 'none') el.style.filter = 'none';
              if (el.classList.contains('recalculating')) el.classList.remove('recalculating');
            });
          });
        }

        function hideInitialSummaryContextFallback(outputId) {
          if (outputId !== 'homo_context_header' && outputId !== 'ortho_context_header') return;
          var outputEl = document.getElementById(outputId);
          var section = outputEl ? outputEl.closest('.summary-context-section') : null;
          var fallback = section ? section.querySelector('.summary-context-card-initial') : null;
          if (fallback && outputEl && outputEl.children && outputEl.children.length > 0) {
            fallback.style.display = 'none';
          }
        }

        document.addEventListener('shiny:value', function(evt) {
          var target = evt && evt.target;
          if (target && target.id) hideInitialSummaryContextFallback(target.id);
        });

        function watchInitialSummaryContextFallbacks() {
          if (typeof MutationObserver !== 'function') return;
          ['homo_context_header', 'ortho_context_header'].forEach(function(outputId) {
            var outputEl = document.getElementById(outputId);
            if (!outputEl || outputEl.__cgvInitialFallbackObserver) return;
            outputEl.__cgvInitialFallbackObserver = true;
            var observer = new MutationObserver(function() {
              hideInitialSummaryContextFallback(outputId);
            });
            observer.observe(outputEl, { childList: true });
            hideInitialSummaryContextFallback(outputId);
          });
        }

        if (document.readyState === 'loading') {
          document.addEventListener('DOMContentLoaded', watchInitialSummaryContextFallbacks, { once: true });
        } else {
          watchInitialSummaryContextFallbacks();
        }

        function initSummaryContextTooltips(root) {
          if (!window.jQuery || !window.jQuery.fn || !window.jQuery.fn.tooltip) return;
          var scope = root || document;
          window.jQuery(scope).find('.summary-organism-pill').each(function() {
            try { window.jQuery(this).tooltip('destroy'); } catch (e) {}
            this.removeAttribute('data-toggle');
            this.removeAttribute('data-html');
            this.removeAttribute('data-container');
            this.removeAttribute('data-placement');
          });
          window.jQuery('.tooltip').remove();
        }

        function markOrthoHeaderLookupStarted() {
          var section = document.getElementById('ortho_context_section');
          if (!section) return;
          section.style.display = '';
          keepSummaryContextOpaque();
          section.querySelectorAll('.summary-organism-pill').forEach(function(pill) {
            pill.classList.remove(
              'summary-organism-pill-found',
              'summary-organism-pill-found_local',
              'summary-organism-pill-found_alias',
              'summary-organism-pill-found_external',
              'summary-organism-pill-external_search',
              'summary-organism-pill-not_found'
            );
            pill.classList.add('summary-organism-pill-local_search');
            pill.setAttribute('title', 'Searching local annotation');
            pill.removeAttribute('data-status-tooltip');
          });
          initSummaryContextTooltips(section);
        }

        document.addEventListener('click', function(e) {
          var trigger = e.target && e.target.closest ? e.target.closest('#search_gene') : null;
          if (trigger) {
            markOrthoHeaderLookupStarted();
            setTimeout(keepSummaryContextOpaque, 0);
            setTimeout(keepSummaryContextOpaque, 120);
            setTimeout(keepSummaryContextOpaque, 500);
          }
        }, true);

        if (window.MutationObserver) {
          var summaryOpacityScheduled = false;
          var summaryOpacityObserver = new MutationObserver(function() {
            if (summaryOpacityScheduled) return;
            summaryOpacityScheduled = true;
            window.requestAnimationFrame(function() {
              summaryOpacityScheduled = false;
              keepSummaryContextOpaque();
            });
          });
          document.addEventListener('DOMContentLoaded', function() {
            var root = document.body || document.documentElement;
            if (root) {
              summaryOpacityObserver.observe(root, {
                subtree: true,
                attributes: true,
                attributeFilter: ['class', 'style']
              });
            }
          });
        }

        document.addEventListener('DOMContentLoaded', function() {
          initSummaryContextTooltips(document);
        });
        if (window.jQuery) {
          window.jQuery(document).on('shiny:value shiny:bound shiny:idle', function() {
            initSummaryContextTooltips(document);
          });
        }

        function addChipFromMainSearchInput() {
          var input = getGlobalSearchInput();
          var candidate = input ? String(input.value || '') : '';
          if (!normalizeChipGene(candidate) && suggestionState.activeIndex >= 0 && suggestionState.activeIndex < suggestionState.items.length) {
            candidate = suggestionState.items[suggestionState.activeIndex];
          }
          if (!normalizeChipGene(candidate)) {
            var collapsedInput = getCollapsedSearchInput();
            candidate = collapsedInput ? String(collapsedInput.value || '') : '';
          }
          return addGlobalSearchChip(candidate);
        }

        function initializeGlobalSearchChipControls() {
          var root = document.getElementById('global-search-chipbar');
          if (!root) return;
          if (root.getAttribute('data-ready') === '1') {
            renderGlobalSearchChips();
            return;
          }
          root.setAttribute('data-ready', '1');

          var addBtn = getGlobalChipAddBtn();
          var clearBtn = getGlobalChipClearBtn();
          if (addBtn) {
            addBtn.addEventListener('click', function (evt) {
              evt.preventDefault();
              addChipFromMainSearchInput();
            });
          }
          if (clearBtn) {
            clearBtn.addEventListener('click', function (evt) {
              evt.preventDefault();
              clearGlobalSearchChips();
            });
          }

          renderGlobalSearchChips();
        }

        function syncCollapsedSuggestionDataList() {
          return;
        }

        function escapeHtml(value) {
          return String(value || '')
            .replace(/&/g, '&amp;')
            .replace(/</g, '&lt;')
            .replace(/>/g, '&gt;')
            .replace(/\"/g, '&quot;')
            .replace(/'/g, '&#39;');
        }

        function readGlobalSuggestionPool() {
          if (Array.isArray(window.__globalGeneSuggestionPool)) {
            globalSuggestionPool = window.__globalGeneSuggestionPool.slice();
          }
          if (globalSuggestionPool.length > 0) {
            return globalSuggestionPool.slice();
          }
          var dl = getGlobalSuggestionDataList();
          if (!dl) return [];
          var options = dl.querySelectorAll('option');
          var values = [];
          options.forEach(function (opt) {
            var value = (opt.value || opt.getAttribute('value') || '').trim();
            if (value) values.push(value);
          });
          return values;
        }

        function hideGlobalSuggestions() {
          var panel = getGlobalSuggestionPanel();
          suggestionState.items = [];
          suggestionState.activeIndex = -1;
          if (!panel) return;
          panel.classList.remove('open');
          panel.innerHTML = '';
        }

        function hideCollapsedSuggestions() {
          var panel = getCollapsedSearchSuggestionPanel();
          collapsedSuggestionState.items = [];
          collapsedSuggestionState.activeIndex = -1;
          if (!panel) return;
          panel.classList.remove('open');
          panel.innerHTML = '';
        }

        function setActiveSuggestion(index) {
          var panel = getGlobalSuggestionPanel();
          if (!panel) return;
          var nodes = panel.querySelectorAll('.app-search-suggestion');
          nodes.forEach(function (node, idx) {
            node.classList.toggle('is-active', idx === index);
          });
          suggestionState.activeIndex = index;
        }

        function setActiveCollapsedSuggestion(index) {
          var panel = getCollapsedSearchSuggestionPanel();
          if (!panel) return;
          var nodes = panel.querySelectorAll('.app-search-suggestion-collapsed');
          nodes.forEach(function (node, idx) {
            node.classList.toggle('is-active', idx === index);
          });
          collapsedSuggestionState.activeIndex = index;
        }

        function commitSuggestion(index) {
          if (index < 0 || index >= suggestionState.items.length) return false;
          var input = getGlobalSearchInput();
          if (!input) return false;
          var picked = suggestionState.items[index];
          input.value = picked;
          if (window.Shiny && Shiny.setInputValue) {
            Shiny.setInputValue('global_search_query', picked, { priority: 'event' });
          }
          hideGlobalSuggestions();
          return true;
        }

        function commitCollapsedSuggestion(index) {
          if (index < 0 || index >= collapsedSuggestionState.items.length) return false;
          var input = getCollapsedSearchInput();
          if (!input) return false;
          var picked = collapsedSuggestionState.items[index];
          input.value = picked;
          if (window.Shiny && Shiny.setInputValue) {
            Shiny.setInputValue('global_search_query_collapsed', picked, { priority: 'event' });
            Shiny.setInputValue('global_search_query', picked, { priority: 'event' });
          }
          hideCollapsedSuggestions();
          return true;
        }

        function renderGlobalSuggestions(rawQuery) {
          var input = getGlobalSearchInput();
          var panel = getGlobalSuggestionPanel();
          if (!input || !panel) return;

          var query = String(rawQuery || '').trim().toLowerCase();
          if (!query) {
            hideGlobalSuggestions();
            return;
          }

          var pool = readGlobalSuggestionPool();
          var filtered = pool.filter(function (value) {
            return value.toLowerCase().indexOf(query) !== -1;
          });

          filtered.sort(function (a, b) {
            var aStarts = a.toLowerCase().indexOf(query) === 0 ? 0 : 1;
            var bStarts = b.toLowerCase().indexOf(query) === 0 ? 0 : 1;
            if (aStarts !== bStarts) return aStarts - bStarts;
            return a.localeCompare(b);
          });

          filtered = filtered.slice(0, 8);
          suggestionState.items = filtered;
          suggestionState.activeIndex = -1;

          if (!filtered.length) {
            hideGlobalSuggestions();
            return;
          }

          panel.innerHTML = filtered.map(function (item, idx) {
            return '<button type=\"button\" class=\"app-search-suggestion\" data-index=\"' + idx + '\">' + escapeHtml(item) + '</button>';
          }).join('');
          panel.classList.add('open');
        }

        function renderCollapsedSuggestions(rawQuery) {
          var input = getCollapsedSearchInput();
          var panel = getCollapsedSearchSuggestionPanel();
          if (!input || !panel) return;

          var query = String(rawQuery || '').trim().toLowerCase();
          if (!query) {
            hideCollapsedSuggestions();
            return;
          }

          var pool = readGlobalSuggestionPool();
          var filtered = pool.filter(function (value) {
            return value.toLowerCase().indexOf(query) !== -1;
          });

          filtered.sort(function (a, b) {
            var aStarts = a.toLowerCase().indexOf(query) === 0 ? 0 : 1;
            var bStarts = b.toLowerCase().indexOf(query) === 0 ? 0 : 1;
            if (aStarts !== bStarts) return aStarts - bStarts;
            return a.localeCompare(b);
          });

          filtered = filtered.slice(0, 8);
          collapsedSuggestionState.items = filtered;
          collapsedSuggestionState.activeIndex = -1;

          if (!filtered.length) {
            hideCollapsedSuggestions();
            return;
          }

          panel.innerHTML = filtered.map(function (item, idx) {
            return '<button type=\"button\" class=\"app-search-suggestion app-search-suggestion-collapsed\" data-index=\"' + idx + '\">' + escapeHtml(item) + '</button>';
          }).join('');
          panel.classList.add('open');
        }

        function bindGlobalSuggestionObserver() {
          if (suggestionObserver) {
            suggestionObserver.disconnect();
            suggestionObserver = null;
          }
        }

        function initializeGlobalSearchInput() {
          var input = getGlobalSearchInput();
          if (!input) return;
          input.removeAttribute('list');
          input.setAttribute('autocomplete', 'new-password');
          input.setAttribute('data-form-type', 'other');
          input.setAttribute('data-lpignore', 'true');
          input.setAttribute('data-1p-ignore', 'true');
          input.setAttribute('autocorrect', 'off');
          input.setAttribute('autocapitalize', 'off');
          input.setAttribute('spellcheck', 'false');

          input.addEventListener('input', function () {
            renderGlobalSuggestions(input.value || '');
          });

          input.addEventListener('focus', function () {
            setWorkflowPanel(null);
            renderGlobalSuggestions(input.value || '');
          });

          input.addEventListener('keydown', function (evt) {
            var panel = getGlobalSuggestionPanel();
            var isOpen = panel && panel.classList.contains('open');
            if (isSpaceBatchShortcut(evt)) {
              var spaceCandidate = '';
              if (isOpen && suggestionState.activeIndex >= 0 && suggestionState.activeIndex < suggestionState.items.length) {
                spaceCandidate = suggestionState.items[suggestionState.activeIndex];
              }
              if (!spaceCandidate) {
                spaceCandidate = input.value || '';
              }
              if (normalizeChipGene(spaceCandidate)) {
                evt.preventDefault();
                addGlobalSearchChip(spaceCandidate);
              }
              return;
            }
            if (evt.key === 'Enter' && (evt.ctrlKey || evt.metaKey)) {
              evt.preventDefault();
              var candidate = '';
              if (isOpen && suggestionState.activeIndex >= 0 && suggestionState.activeIndex < suggestionState.items.length) {
                candidate = suggestionState.items[suggestionState.activeIndex];
              }
              if (!candidate) {
                candidate = input.value || '';
              }
              addGlobalSearchChip(candidate);
              return;
            }
            if (evt.key === 'ArrowDown') {
              if (!isOpen && (input.value || '').trim()) {
                renderGlobalSuggestions(input.value || '');
                panel = getGlobalSuggestionPanel();
                isOpen = panel && panel.classList.contains('open');
              }
              if (!isOpen || !suggestionState.items.length) return;
              evt.preventDefault();
              var downIdx = suggestionState.activeIndex + 1;
              if (downIdx >= suggestionState.items.length) downIdx = 0;
              setActiveSuggestion(downIdx);
              return;
            }
            if (evt.key === 'ArrowUp') {
              if (!isOpen || !suggestionState.items.length) return;
              evt.preventDefault();
              var upIdx = suggestionState.activeIndex - 1;
              if (upIdx < 0) upIdx = suggestionState.items.length - 1;
              setActiveSuggestion(upIdx);
              return;
            }
            if (evt.key === 'Escape') {
              hideGlobalSuggestions();
              return;
            }
            if (evt.key === 'Enter') {
              if (isOpen && suggestionState.activeIndex >= 0) {
                evt.preventDefault();
                if (commitSuggestion(suggestionState.activeIndex)) return;
              }
              evt.preventDefault();
              hideGlobalSuggestions();
              if (window.Shiny && Shiny.setInputValue) {
                Shiny.setInputValue('global_search_query', input.value, { priority: 'event' });
              }
              var btn = document.getElementById('global_search_go');
              if (btn) btn.click();
            }
          });
        }

        document.documentElement.setAttribute('data-app-theme', getStoredTheme());

        document.addEventListener('DOMContentLoaded', function () {
          applyTheme(getStoredTheme());
          applySidebarCollapsed(getStoredSidebarCollapsed());
          /* On mobile: set hamburger glyph and ensure nav is closed */
          (function() {
            var isMobile = window.matchMedia('(max-width: 960px)').matches;
            if (isMobile) {
              var btn = document.getElementById('sidebar-collapse-btn');
              if (btn) {
                var glyph = btn.querySelector('.sidebar-collapse-glyph');
                if (glyph) glyph.textContent = String.fromCharCode(0x2630);
                btn.setAttribute('title', 'Open menu');
              }
            }
          })();
          setActiveNav('home');
          setWorkflowPanel(null);
          initializeGlobalSearchInput();
          initializeGlobalSearchChipControls();
          bindGlobalSuggestionObserver();
          syncCollapsedSuggestionDataList();
          initializeQuickFab();
          /* Close mobile nav when user triggers a search */
          var searchGoBtn = document.getElementById('global_search_go');
          if (searchGoBtn) {
            searchGoBtn.addEventListener('click', function() {
              if (window.matchMedia('(max-width: 960px)').matches) {
                var s = document.querySelector('.app-shell');
                if (s && s.classList.contains('mobile-nav-open')) {
                  toggleMobileNav();
                }
              }
            });
          }
          document.addEventListener('cgv-global-suggestions-updated', function () {
            globalSuggestionPool = Array.isArray(window.__globalGeneSuggestionPool)
              ? window.__globalGeneSuggestionPool.slice()
              : [];
            var activeInput = getGlobalSearchInput();
            if (activeInput && document.activeElement === activeInput) {
              renderGlobalSuggestions(activeInput.value || '');
            }
            var activeCollapsedInput = getCollapsedSearchInput();
            if (activeCollapsedInput && document.activeElement === activeCollapsedInput) {
              renderCollapsedSuggestions(activeCollapsedInput.value || '');
            }
          });

          var collapsedInput = getCollapsedSearchInput();
          if (collapsedInput) {
            collapsedInput.addEventListener('input', function () {
              renderCollapsedSuggestions(collapsedInput.value || '');
            });

            collapsedInput.addEventListener('focus', function () {
              renderCollapsedSuggestions(collapsedInput.value || '');
            });

            collapsedInput.addEventListener('keydown', function (evt) {
              var panel = getCollapsedSearchSuggestionPanel();
              var isOpen = panel && panel.classList.contains('open');
              if (isSpaceBatchShortcut(evt)) {
                var collapsedSpaceChip = '';
                if (isOpen && collapsedSuggestionState.activeIndex >= 0 && collapsedSuggestionState.activeIndex < collapsedSuggestionState.items.length) {
                  collapsedSpaceChip = collapsedSuggestionState.items[collapsedSuggestionState.activeIndex];
                }
                if (!collapsedSpaceChip) {
                  collapsedSpaceChip = collapsedInput.value || '';
                }
                if (normalizeChipGene(collapsedSpaceChip)) {
                  evt.preventDefault();
                  addGlobalSearchChip(collapsedSpaceChip, { keepCollapsedInput: false });
                }
                return;
              }
              if (evt.key === 'Enter' && (evt.ctrlKey || evt.metaKey)) {
                evt.preventDefault();
                var chipCandidate = '';
                if (isOpen && collapsedSuggestionState.activeIndex >= 0 && collapsedSuggestionState.activeIndex < collapsedSuggestionState.items.length) {
                  chipCandidate = collapsedSuggestionState.items[collapsedSuggestionState.activeIndex];
                }
                if (!chipCandidate) {
                  chipCandidate = collapsedInput.value || '';
                }
                addGlobalSearchChip(chipCandidate, { keepCollapsedInput: false });
                return;
              }
              if (evt.key === 'ArrowDown') {
                if (!isOpen && (collapsedInput.value || '').trim()) {
                  renderCollapsedSuggestions(collapsedInput.value || '');
                  panel = getCollapsedSearchSuggestionPanel();
                  isOpen = panel && panel.classList.contains('open');
                }
                if (!isOpen || !collapsedSuggestionState.items.length) return;
                evt.preventDefault();
                var downIdx = collapsedSuggestionState.activeIndex + 1;
                if (downIdx >= collapsedSuggestionState.items.length) downIdx = 0;
                setActiveCollapsedSuggestion(downIdx);
                return;
              }
              if (evt.key === 'ArrowUp') {
                if (!isOpen || !collapsedSuggestionState.items.length) return;
                evt.preventDefault();
                var upIdx = collapsedSuggestionState.activeIndex - 1;
                if (upIdx < 0) upIdx = collapsedSuggestionState.items.length - 1;
                setActiveCollapsedSuggestion(upIdx);
                return;
              }
              if (evt.key === 'Escape') {
                hideCollapsedSuggestions();
                return;
              }
              if (evt.key !== 'Enter') return;
              if (isOpen && collapsedSuggestionState.activeIndex >= 0) {
                evt.preventDefault();
                if (commitCollapsedSuggestion(collapsedSuggestionState.activeIndex)) return;
              }
              evt.preventDefault();
              hideCollapsedSuggestions();
              syncMainSearchFromCollapsed();
              var goBtn = document.getElementById('global_search_go_collapsed');
              if (goBtn) goBtn.click();
            });
          }

          var runBtnMain = document.getElementById('global_search_go');
          if (runBtnMain) {
            runBtnMain.addEventListener('click', function () {
              suppressWorkflowPanelAutoOpenOnce();
            });
          }

          var runBtnCollapsed = document.getElementById('global_search_go_collapsed');
          if (runBtnCollapsed) {
            runBtnCollapsed.addEventListener('click', function () {
              suppressWorkflowPanelAutoOpenOnce();
            });
          }
        });

        window.addEventListener('resize', function () {
          applySidebarCollapsed(getStoredSidebarCollapsed());
          syncQuickFabModeControls();
          updateQuickFabScrollTopVisibility();
        });

        document.addEventListener('click', function (evt) {
          var chipRemoveBtn = evt.target.closest('.app-search-chip-remove');
          if (chipRemoveBtn) {
            evt.preventDefault();
            var chipIndex = parseInt(chipRemoveBtn.getAttribute('data-chip-index') || '-1', 10);
            removeGlobalSearchChipAt(chipIndex);
            return;
          }

          var collapsedSuggestionBtn = evt.target.closest('.app-search-suggestion-collapsed');
          if (collapsedSuggestionBtn) {
            evt.preventDefault();
            var collapsedIndex = parseInt(collapsedSuggestionBtn.getAttribute('data-index') || '-1', 10);
            commitCollapsedSuggestion(collapsedIndex);
            return;
          }

          var suggestionBtn = evt.target.closest('.app-search-suggestion');
          if (suggestionBtn) {
            evt.preventDefault();
            var index = parseInt(suggestionBtn.getAttribute('data-index') || '-1', 10);
            commitSuggestion(index);
            return;
          }

          if (!evt.target.closest('.app-primary-search-input-wrap')) {
            hideGlobalSuggestions();
          }

          if (!evt.target.closest('.app-collapsed-search-input-wrap')) {
            hideCollapsedSuggestions();
          }

          if (!evt.target.closest('#app-quick-fab') && isQuickFabOpen()) {
            setQuickFabPanel(null);
            setQuickFabOpen(false);
          }

          var collapseBtn = evt.target.closest('#sidebar-collapse-btn');
          if (collapseBtn) {
            evt.preventDefault();
            toggleSidebarCollapsed();
            return;
          }

          var collapsedSearchToggle = evt.target.closest('#global_search_toggle_collapsed');
          if (collapsedSearchToggle) {
            evt.preventDefault();
            if (!isSidebarCollapsed()) return;
            var panel = getCollapsedSearchPanel();
            var isOpen = !!(panel && panel.classList.contains('is-open'));
            if (!isOpen) {
              setWorkflowPanel(null);
            }
            setCollapsedSearchPanelOpen(!isOpen);
            return;
          }

          var collapsedSearchSubmit = evt.target.closest('#global_search_go_collapsed');
          if (collapsedSearchSubmit) {
            syncMainSearchFromCollapsed();
            setWorkflowPanel(null);
            setCollapsedSearchPanelOpen(false);
            return;
          }

          var collapsedPanel = getCollapsedSearchPanel();
          if (collapsedPanel && collapsedPanel.classList.contains('is-open')) {
            if (!evt.target.closest('#app-collapsed-search-panel') && !evt.target.closest('#global_search_toggle_collapsed')) {
              setCollapsedSearchPanelOpen(false);
            }
          }

          var openWorkflowPanel = document.querySelector('.app-nav-inline-panel[data-workflow-panel].is-open');
          if (openWorkflowPanel) {
            var clickedInsideWorkflowPanel = !!evt.target.closest('.app-nav-inline-panel[data-workflow-panel]');
            var clickedWorkflowTrigger = !!evt.target.closest('.app-nav-btn-workflow');
            var clickedInsideModal = !!evt.target.closest('#shiny-modal, .modal, .modal-backdrop');
            if (!clickedInsideWorkflowPanel && !clickedWorkflowTrigger && !clickedInsideModal) {
              setWorkflowPanel(null);
            }
          }

          var navBtn = evt.target.closest('.app-nav-btn');
          if (navBtn) {
            evt.preventDefault();
            setCollapsedSearchPanelOpen(false);
            var target = navBtn.getAttribute('data-target');
            if (!isValidTarget(target)) return;
            var isWorkflow = target === 'homologous' || target === 'orthologous';
            var activeNavTarget = getActiveNavTarget();
            var isAlreadyActive = activeNavTarget === target;
            if (isWorkflow) {
              var panel = document.querySelector('.app-nav-inline-panel[data-workflow-panel=\"' + target + '\"]');
              var isOpen = !!(panel && panel.classList.contains('is-open'));
              if (isAlreadyActive) {
                // Toggle compact/expanded state without leaving the active workflow tab.
                setWorkflowPanel(isOpen ? null : target);
                setActiveNav(target);
              } else {
                setWorkflowPanel(target);
                setActiveNav(target);
                if (!activateNavTabImmediately(target) && window.Shiny && Shiny.setInputValue) {
                  Shiny.setInputValue('app_nav_click', target, { priority: 'event' });
                }
              }
            } else {
              setWorkflowPanel(null);
              setActiveNav(target);
              if (!isAlreadyActive && !activateNavTabImmediately(target) && window.Shiny && Shiny.setInputValue) {
                Shiny.setInputValue('app_nav_click', target, { priority: 'event' });
              }
              /* Auto-close mobile nav when navigating to a non-workflow tab */
              if (window.matchMedia('(max-width: 960px)').matches) {
                var shell2 = document.querySelector('.app-shell');
                if (shell2 && shell2.classList.contains('mobile-nav-open')) {
                  toggleMobileNav();
                }
              }
            }
            setTimeout(function () {
              var current = document.documentElement.getAttribute('data-app-theme') || 'light';
              applyTheme(current);
              bindGlobalSuggestionObserver();
            }, 0);
            return;
          }

          var themeBtn = evt.target.closest('#theme-toggle-btn');
          if (themeBtn) {
            evt.preventDefault();
            toggleTheme();
          }
        });

        $(document).on('shiny:connected', function () {
          if (window.Shiny && Shiny.setInputValue) {
            Shiny.setInputValue('app_nav_click', 'home', { priority: 'event' });
          }
          syncThemeWithShiny(document.documentElement.getAttribute('data-app-theme') || getStoredTheme(), true);

          if (window.Shiny) {
            Shiny.addCustomMessageHandler('set_colorblind_mode', function(message) {
              if (message === true) {
                document.documentElement.setAttribute('data-colorblind-mode', 'true');
              } else {
                document.documentElement.setAttribute('data-colorblind-mode', 'false');
              }
            });

            Shiny.addCustomMessageHandler('reset_color_palette_ui', function(message) {
              var defaultColors = {
                'color_exon': '#F45D75',
                'color_cds': '#E8A44F',
                'color_utr': '#5BC0EB',
                'color_gene': '#FFB7BF',
                'color_identity_high': '#CC2929',
                'color_identity_mid': '#E07858',
                'color_identity_low': '#5CB85C',
                'color_header_gene': '#2C3E50',
                'color_header_transcript': '#24445B'
              };
              for (var id in defaultColors) {
                var el = document.getElementById(id);
                if (el) el.value = defaultColors[id];
              }
            });
          }
        });

        $(document).on('shiny:inputchanged', function (event) {
          if (event.name === 'navtabs' && isValidTarget(event.value)) {
            setActiveNav(event.value);
            if (event.value === 'homologous' || event.value === 'orthologous') {
              if (suppressWorkflowPanelOnNextNav) {
                setWorkflowPanel(null);
                suppressWorkflowPanelOnNextNav = false;
              } else {
                setWorkflowPanel(event.value);
              }
            } else {
              setWorkflowPanel(null);
              suppressWorkflowPanelOnNextNav = false;
            }
            setCollapsedSearchPanelOpen(false);
            setQuickFabPanel(null);
            setQuickFabOpen(false);
            setTimeout(bindGlobalSuggestionObserver, 0);
            setTimeout(syncQuickFabModeControls, 0);
          }
        });
      })();
      "))
    ),
    div(
      id = "app-status-popup",
      class = "app-status-popup",
      div(
        class = "app-status-popup-header",
        div(
          class = "app-status-popup-title",
          icon("bell"),
          span("Notifications")
        ),
        div(
          class = "app-status-popup-actions",
          tags$button(id = "app-status-popup-clear", type = "button", class = "app-status-popup-btn", span("Clear")),
          tags$button(id = "app-status-popup-close", type = "button", class = "app-status-popup-btn", HTML("&times;"))
        )
      ),
      div(
        id = "app-status-popup-loader",
        class = "app-status-popup-loader",
        span(class = "app-status-popup-current-label", "Current activity"),
        div(id = "app-status-popup-loader-text", class = "app-status-popup-loader-text", "Working...")
      ),
      div(id = "app-status-popup-log", class = "app-status-popup-log")
    ),
    div(
      id = "app-work-indicator",
      class = "app-work-indicator",
      role = "status",
      `aria-live` = "polite",
      `aria-atomic` = "true",
      `aria-hidden` = "true",
      hidden = TRUE,
      div(
        class = "app-dna-loader app-dna-loader--spinner-sm",
        `aria-hidden` = "true",
        div(
          class = "app-dna-wave app-dna-wave-top",
          lapply(seq_len(16), function(i) span(class = "app-dna-node", style = sprintf("--i:%d;", i - 1)))
        ),
        div(
          class = "app-dna-wave app-dna-wave-bottom",
          lapply(seq_len(16), function(i) span(class = "app-dna-node", style = sprintf("--i:%d;", i - 1)))
        )
      ),
      div(
        class = "app-work-indicator-copy",
        tags$strong(id = "app-work-indicator-headline", "CGeV is working"),
        span(id = "app-work-indicator-detail", "Processing data…")
      )
    ),
    shinyjs::useShinyjs()
  ),
  div(
    id = "app-top-sentinel",
    class = "app-top-sentinel",
    `aria-hidden` = "true"
  ),
  div(
    class = "app-shell",
    tags$aside(
      class = "app-sidebar",
      div(
        class = "app-sidebar-top",
        div(
          class = "app-brand",
          tags$img(
            src = versioned_asset_path("favicon2.ico?v=2"),
            class = "app-brand-logo",
            `data-light-src` = versioned_asset_path("favicon2.ico?v=2"),
            `data-dark-src` = versioned_asset_path("favicon.ico?v=2"),
            alt = "CGeV logo"
          ),
          div(
            class = "app-brand-text",
            div(class = "app-brand-title", "CGeV"),
            div(class = "app-brand-divider"),
            div(
              class = "app-brand-descriptor",
              div(class = "app-brand-kicker", "Comparative"),
              div(class = "app-brand-subtitle", "Gene Viewer"),
              div(class = "app-brand-version", paste0("v", cgv_release_version))
            )
          ),
          tags$button(
            id = "sidebar-collapse-btn",
            type = "button",
            class = "sidebar-collapse-btn",
            `aria-label` = "Collapse sidebar",
            `aria-expanded` = "true",
            title = "Collapse sidebar",
            span(class = "sidebar-collapse-glyph", "\u25C0")
          )
        )
      ),
      div(
        class = "app-nav",
        tags$button(type = "button", class = "app-nav-btn is-active", `data-target` = "home", title = "Home", icon("home"), span("Home")),
        div(
          class = "app-nav-group",
          tags$button(
            type = "button",
            class = "app-nav-btn app-nav-btn-workflow",
            `data-target` = "homologous",
            `aria-expanded` = "false",
            `aria-controls` = "app-workflow-panel-homologous",
            title = "Multi-Gene Search",
            icon("dna"),
            span("Multi-Gene Search"),
            span(class = "app-nav-collapsed-label", "Multi")
          ),
          div(
            class = "app-nav-inline-panel",
            id = "app-workflow-panel-homologous",
            `data-workflow-panel` = "homologous",
            div(
              class = "panel-content app-control-panel app-workflow-panel",
              div(
                class = "app-submenu app-submenu-controls",
                div(
                  class = "app-submenu-body app-submenu-controls-body",
                  div(
                    class = "app-workflow-intro",
                    p("Query multiple annotated genes within one organism.")
                  ),
                  div(
                    class = "app-compact-section app-compact-section-tight",
                    div(class = "app-compact-label", icon("database"), span("Data source")),
                    div(
                      class = "viz-mode-wrap",
                      htmltools::tagAppendAttributes(
                        radioButtons(
                          inputId = "homo_data_mode",
                          label = NULL,
                          choices = c("Preloaded organism" = "preloaded", "NCBI Search" = "ncbi", "Upload files" = "upload"),
                          selected = "preloaded",
                          inline = TRUE
                        ),
                        class = "viz-mode-toggle"
                      )
                    ),
                    div(
                      class = "app-source-panel app-upload-inputs",
                      `data-source-panel` = "homo-upload",
                      hidden = "hidden",
                      fileInput(
                        inputId = "file1",
                        label = "Annotation file",
                        accept = c(".gff", ".gff3", ".gtf", ".txt")
                      ),
                      fileInput(
                        inputId = "genome_fasta1",
                        label = "Genome FASTA/2bit file",
                        accept = c(".fa", ".fasta", ".fna", ".fa.gz", ".fasta.gz", ".fna.gz", ".2bit")
                      ),
                      tags$details(
                        class = "app-optional-uploads-panel",
                        tags$summary(
                          class = "app-optional-uploads-summary",
                          icon("plus-circle"),
                          span("Optional files"),
                          span(class = "app-optional-uploads-hint", "Enhance your analysis")
                        ),
                        div(
                          class = "app-optional-uploads-body",
                          fileInput(
                            inputId = "homo_assembly_report",
                            label = tags$span(icon("file-alt"), "Assembly report (.txt)"),
                            accept = c(".txt")
                          ),
                          fileInput(
                            inputId = "homo_assembly_stats",
                            label = tags$span(icon("chart-bar"), "Assembly stats (.txt)"),
                            accept = c(".txt")
                          ),
                          fileInput(
                            inputId = "homo_go_annotations",
                            label = tags$span(icon("project-diagram"), "GO annotations (.gaf, .gaf.gz)"),
                            accept = c(".gaf", ".gaf.gz", ".gz")
                          )
                        )
                      )
                    ),
                    div(
                      class = "app-source-panel app-ncbi-search-panel",
                      `data-source-panel` = "homo-ncbi",
                      hidden = "hidden",
                      div(
                        class = "app-ncbi-search-input-wrap",
                        tags$label(class = "app-ncbi-search-label", icon("globe"), "Search NCBI for an organism"),
                        div(
                          class = "app-ncbi-search-row",
                          textInput(
                            inputId = "homo_ncbi_query",
                            label = NULL,
                            placeholder = "e.g. Drosophila virilis, Coffea arabica..."
                          ),
                          actionButton("homo_ncbi_search_btn", icon("search"),
                                       class = "btn-sm app-ncbi-search-go")
                        )
                      ),
                      uiOutput("homo_ncbi_results"),
                      uiOutput("homo_ncbi_preview"),
                      uiOutput("homo_ncbi_download_status")
                    )
                  ),
                  div(
                    class = "app-hidden-query",
                    tags$datalist(id = "filter1_suggestions"),
                    div(
                      htmltools::tagAppendAttributes(
                        textInput(
                          inputId = "filter1",
                          label = "Search gene",
                          placeholder = "Enter gene name"
                        ),
                        list = "filter1_suggestions"
                      )
                    ),
                    uiOutput("homo_visual_mode_ui")
                  ),
                  div(
                    class = "app-source-panel",
                    `data-source-panel` = "homo-preloaded",
                    div(
                      class = "app-compact-section app-compact-section-tight app-organism-inline-section",
                      tags$details(
                        class = "app-submenu app-submenu-organism app-submenu-organism-inline",
                        tags$summary(
                          span(
                            class = "app-organism-summary-main",
                            icon("dna"),
                            span(id = "homo-organism-summary-text", class = "app-organism-summary-text", "Organism selection")
                          ),
                          span(class = "app-organism-summary-info", uiOutput("homo_preloaded_info_icon", container = span))
                        ),
                        div(
                          class = "app-submenu-body",
                          div(
                            id = "homo_species_grid_initial",
                            class = "species-grid-fallback",
                            initial_homo_species_grid
                          ),
                          uiOutput("homo_species_grid")
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        ),
        div(
          class = "app-nav-group",
          tags$button(
            type = "button",
            class = "app-nav-btn app-nav-btn-workflow",
            `data-target` = "orthologous",
            `aria-expanded` = "false",
            `aria-controls` = "app-workflow-panel-orthologous",
            title = "Cross-Species Gene Search",
            icon("sitemap"),
            span("Cross-Species Gene Search"),
            span(class = "app-nav-collapsed-label", "Cross")
          ),
          div(
            class = "app-nav-inline-panel",
            id = "app-workflow-panel-orthologous",
            `data-workflow-panel` = "orthologous",
            div(
              class = "panel-content app-control-panel app-workflow-panel",
              div(
                class = "app-submenu app-submenu-controls",
                div(
                  class = "app-submenu-body app-submenu-controls-body",
                  div(
                    class = "app-workflow-intro",
                    p("Query one selected gene across multiple organisms.")
                  ),
                  div(
                    class = "app-compact-section app-compact-section-tight",
                    div(class = "app-compact-label", icon("database"), span("Data source")),
                    div(
                      class = "viz-mode-wrap",
                      htmltools::tagAppendAttributes(
                        radioButtons(
                          inputId = "ortho_data_mode",
                          label = NULL,
                          choices = c("Preloaded organisms" = "preloaded", "NCBI Search" = "ncbi", "Upload files" = "upload", "Mixed sources" = "mixed"),
                          selected = "preloaded",
                          inline = TRUE
                        ),
                        class = "viz-mode-toggle"
                      )
                    ),
                    div(
                      class = "app-source-panel",
                      `data-source-panel` = "ortho-preloaded",
                      div(
                        class = "app-compact-section app-compact-section-tight app-organism-inline-section",
                        div(class = "app-source-divider", `data-source-mixed-divider` = "true", hidden = "hidden", span("Preloaded organisms")),
                        tags$details(
                          class = "app-submenu app-submenu-organism app-submenu-organism-inline",
                          tags$summary(
                            span(
                              class = "app-organism-summary-main",
                              icon("dna"),
                              span(id = "ortho-organism-summary-text", class = "app-organism-summary-text", "Organism selection")
                            ),
                            span(class = "app-organism-summary-info", uiOutput("ortho_preloaded_info_icon", container = span))
                          ),
                          div(
                            class = "app-submenu-body",
                            div(
                              id = "ortho_species_grid_initial",
                              class = "species-grid-fallback",
                              initial_ortho_species_grid
                            ),
                            uiOutput("ortho_species_grid")
                          )
                        )
                      )
                    ),
                    div(
                      class = "app-source-panel app-upload-inputs",
                      `data-source-panel` = "ortho-upload",
                      hidden = "hidden",
                      div(class = "app-source-divider", `data-source-mixed-divider` = "true", hidden = "hidden", span("Upload files")),
                      fileInput(
                        inputId = "files",
                        label = "Annotation files",
                        multiple = TRUE,
                        accept = c(".gff", ".gff3", ".gtf", ".txt")
                      ),
                      fileInput(
                        inputId = "genome_fasta_ortho",
                        label = "Genome FASTA/2bit files",
                        multiple = TRUE,
                        accept = c(".fa", ".fasta", ".fna", ".fa.gz", ".fasta.gz", ".fna.gz", ".2bit")
                      ),
                      tags$details(
                        class = "app-optional-uploads-panel",
                        tags$summary(
                          class = "app-optional-uploads-summary",
                          icon("plus-circle"),
                          span("Optional files"),
                          span(class = "app-optional-uploads-hint", "Enhance your analysis")
                        ),
                        div(
                          class = "app-optional-uploads-body",
                          fileInput(
                            inputId = "ortho_assembly_reports",
                            label = tags$span(icon("file-alt"), "Assembly reports (.txt)"),
                            multiple = TRUE,
                            accept = c(".txt")
                          ),
                          fileInput(
                            inputId = "ortho_assembly_stats",
                            label = tags$span(icon("chart-bar"), "Assembly stats (.txt)"),
                            multiple = TRUE,
                            accept = c(".txt")
                          ),
                          fileInput(
                            inputId = "ortho_go_annotations",
                            label = tags$span(icon("project-diagram"), "GO annotations (.gaf, .gaf.gz)"),
                            multiple = TRUE,
                            accept = c(".gaf", ".gaf.gz", ".gz")
                          )
                        )
                      )
                    ),
                    div(
                      class = "app-source-panel app-ncbi-search-panel",
                      `data-source-panel` = "ortho-ncbi",
                      hidden = "hidden",
                      div(class = "app-source-divider", `data-source-mixed-divider` = "true", hidden = "hidden", span("NCBI Search")),
                      div(
                        class = "app-ncbi-search-input-wrap",
                        tags$label(class = "app-ncbi-search-label", icon("globe"), "Search NCBI for organisms"),
                        div(
                          class = "app-ncbi-search-row",
                          textInput(
                            inputId = "ortho_ncbi_query",
                            label = NULL,
                            placeholder = "e.g. Drosophila virilis, Coffea arabica..."
                          ),
                          actionButton("ortho_ncbi_search_btn", icon("search"),
                                       class = "btn-sm app-ncbi-search-go")
                        )
                      ),
                      uiOutput("ortho_ncbi_results"),
                      uiOutput("ortho_ncbi_queue"),
                      uiOutput("ortho_ncbi_download_status")
                    )
                  ),
                  div(
                    class = "app-hidden-query",
                    tags$datalist(id = "gene_name_suggestions"),
                    div(
                      htmltools::tagAppendAttributes(
                        textInput(
                          inputId = "gene_name",
                          label = "Search gene",
                          placeholder = "Enter gene name"
                        ),
                        list = "gene_name_suggestions"
                      )
                    ),
                    div(
                      class = "viz-mode-wrap",
                      htmltools::tagAppendAttributes(
                        radioButtons(
                          inputId = "ortho_visual_mode",
                          label = NULL,
                          choices = c(
                            "Compact" = "compact",
                            "Detailed" = "detailed",
                            "Aligned synteny" = "aligned",
                            "LASTZ Blocks" = "pip_blocks",
                            "MultiPIP-style" = "pip_multipip"
                          ),
                          selected = "compact",
                          inline = TRUE
                        ),
                        class = "viz-mode-toggle"
                      )
                    )
                  )
                )
              )
            )
          )
        ),
        if (isTRUE(cgv_gene_catalog_enabled)) {
          tags$button(
            type = "button",
            class = "app-nav-btn app-nav-btn-catalog",
            `data-target` = "catalog",
            title = "Gene Catalog — search aliases across installed organisms",
            icon("search"),
            span("Gene Catalog"),
            span(class = "app-nav-collapsed-label", "Catalog")
          )
        } else NULL,
        tags$button(
          type = "button",
          class = "app-nav-btn app-nav-btn-figure-studio",
          `data-target` = "figure-studio",
          title = "Build a publication-ready multi-panel figure",
          icon("object-group"),
          span("Figure Studio")
        ),
        tags$button(type = "button", class = "app-nav-btn", `data-target` = "guide", title = "CGeV Guide", icon("route"), span("CGeV Guide")),
        tags$button(type = "button", class = "app-nav-btn", `data-target` = "settings", title = "Settings", icon("cog"), span("Settings")),

        tags$button(type = "button", class = "app-nav-btn", `data-target` = "feedback", title = "Feedback", icon("comment-dots"), span("Feedback")),
        tags$button(
          type = "button",
          class = "app-nav-btn app-nav-btn-desktop",
          `data-target` = "desktop-app",
          `data-tooltip` = "CGeV Desktop downloads",
          `aria-label` = "CGeV Desktop downloads",
          title = "CGeV Desktop downloads",
          icon("laptop"),
          span("CGeV Desktop")
        ),
        div(class = "app-nav-separator"),
        div(
          class = "app-nav-actions",
          div(
            class = "app-primary-search",
            tags$i(class = "fa fa-search app-primary-search-icon"),
            div(
              class = "app-primary-search-input-wrap",
              htmltools::tagAppendAttributes(
                textInput(
                  inputId = "global_search_query",
                  label = NULL,
                  placeholder = "Search gene"
                ),
                autocomplete = "new-password",
                autocorrect = "off",
                autocapitalize = "off",
                spellcheck = "false",
                `data-form-type` = "other",
                `data-lpignore` = "true",
                `data-1p-ignore` = "true"
              ),
              tags$datalist(id = "global_search_query_suggestions"),
              div(id = "global-search-suggestions", class = "app-search-suggestions")
            )
          ),
          # sidebar-batch-ui-group: hidden in orthologous mode (batch not allowed for ortho)
          div(
            id = "sidebar-batch-ui-group",
            div(
              id = "global-search-chipbar",
              class = "app-search-chipbar",
              div(
                id = "global-search-chip-list",
                class = "app-search-chip-list app-search-chip-list-sync",
                span(class = "app-search-chip-empty", "No genes added yet.")
              ),
              div(
                class = "app-search-chip-actions",
                tags$button(
                  id = "global_search_add_chip",
                  type = "button",
                  class = "app-search-chip-btn",
                  icon("plus"),
                  span("Add to batch")
                ),
                tags$button(
                  id = "global_search_clear_chips",
                  type = "button",
                  class = "app-search-chip-btn app-search-chip-btn-clear",
                  icon("times"),
                  span("Clear batch"),
                  disabled = "disabled"
                )
              ),
              tags$input(id = "global_search_chip_payload", type = "hidden", value = "")
            )
          ), # end sidebar-batch-ui-group
          actionButton(
            inputId = "global_search_go",
            label = span("Generate visualization"),
            icon = icon("search"),
            class = "btn-primary app-action-btn app-sidebar-primary-btn"
          ),
          div(
            id = "global-search-batch-count",
            class = "app-search-batch-count is-empty",
            "Batch: 0 genes"
          ),
          tags$button(
            id = "global_search_toggle_collapsed",
            type = "button",
            class = "btn btn-primary app-action-btn app-sidebar-primary-btn app-collapsed-search-toggle",
            `aria-expanded` = "false",
            title = "Search gene",
            icon("search"),
            span("Search gene")
          ),
          div(
            id = "app-collapsed-search-panel",
            class = "app-nav-inline-panel app-collapsed-search-panel",
            div(
              class = "panel-content app-control-panel",
              div(
                class = "app-collapsed-search-control",
                div(
                  class = "app-inline-control-title",
                  icon("search"),
                  span("Gene search")
                ),
                div(
                  class = "app-collapsed-search-input-wrap",
                  htmltools::tagAppendAttributes(
                    textInput(
                      inputId = "global_search_query_collapsed",
                      label = NULL,
                      placeholder = "Enter gene name (add to batch)"
                    ),
                    list = "global_search_query_suggestions_collapsed",
                    autocomplete = "new-password",
                    autocorrect = "off",
                    autocapitalize = "off",
                    spellcheck = "false",
                    `data-form-type` = "other",
                    `data-lpignore` = "true",
                    `data-1p-ignore` = "true"
                  ),
                  div(id = "global-search-suggestions-collapsed", class = "app-search-suggestions app-search-suggestions-collapsed"),
                  tags$datalist(id = "global_search_query_suggestions_collapsed")
                ),
                div(
                  id = "collapsed-batch-preview",
                  class = "app-collapsed-search-chip-preview",
                  div(class = "app-collapsed-search-chip-title", "Batch preview"),
                  div(
                    id = "global-search-chip-list-collapsed",
                    class = "app-search-chip-list app-search-chip-list-sync app-search-chip-list-compact",
                    `data-empty-label` = "No batch genes.",
                    span(class = "app-search-chip-empty", "No batch genes.")
                  )
                ),
                actionButton(
                  inputId = "global_search_go_collapsed",
                  label = span("Generate visualization"),
                  icon = icon("search"),
                  class = "btn-primary app-action-btn app-collapsed-generate-btn"
                )
              )
            )
          ),
          actionButton(
            inputId = "clear_all_visualizations",
            label = span("Clear visualizations"),
            icon = icon("trash"),
            class = "btn-clear-plots app-clear-btn app-sidebar-clear-btn",
            title = "Clear visualizations"
          ),
          p(id = "sidebar-search-hint", class = "app-nav-actions-hint", "Tip: press Space to add a gene to batch (also Ctrl+Enter), then Generate visualization.")
        )
      ),
      div(
        id = "app-hidden-workflow-triggers",
        style = "display:none;",
        actionButton("generate1", span("Generate")),
        actionButton("search_gene", span("Search"))
      )
    ),
    tags$main(
      class = "app-main",
      # ── Mobile context bar: sticky breadcrumb for mobile navigation ──
      div(
        class = "mobile-context-bar",
        span(class = "mobile-ctx-label",
          span(class = "mobile-ctx-gene", ""),
          span(class = "mobile-ctx-workflow", "")
        )
      ),
      tabsetPanel(
        id = "navtabs",
        type = "hidden",
        selected = "home",
        tabPanel(
          title = "CGeV Guide",
          value = "guide",
          div(
            class = "content-wrapper app-main-pane app-guide-pane",
            tags$link(rel = "stylesheet", href = versioned_asset_path("css/cgv_guide.css")),
            div(
              class = "guide-shell",
              div(
                class = "guide-route-panel",
                div(
                  class = "guide-route-intro",
                  div(
                    class = "guide-intro-copy",
                    span(class = "guide-kicker", icon("route"), "CGeV Guide"),
                    h1(class = "guide-title", "Choose a workflow, then follow each step"),
                    p(class = "guide-subtitle", "Use the video routes for a guided walkthrough or open the complete Web and Desktop reference.")
                  ),
                  div(
                    class = "guide-manual-card",
                    div(class = "guide-manual-icon", icon("book-open")),
                    div(
                      class = "guide-manual-copy",
                      span(class = "guide-manual-eyebrow", "Complete documentation"),
                      strong("CGeV User Manual"),
                      tags$small("English · Web and Desktop · Always opens the latest published edition")
                    ),
                    div(
                      class = "guide-manual-actions",
                      tags$a(
                        href = cgv_manual_path,
                        target = "_blank",
                        rel = "noopener noreferrer",
                        class = "guide-manual-open",
                        `aria-label` = "Open the latest CGeV User Manual",
                        icon("arrow-up-right-from-square"),
                        span("Open manual")
                      ),
                      tags$a(
                        href = cgv_manual_path,
                        download = "CGeV_User_Manual.pdf",
                        class = "guide-manual-download",
                        `aria-label` = "Download the latest CGeV User Manual PDF",
                        icon("download"),
                        span("PDF")
                      )
                    )
                  )
                ),
                tags$button(
                  type = "button",
                  class = "guide-route-card guide-desktop-only",
                  `data-guide-route` = "desktop-downloads",
                  span(class = "guide-route-number", "00"),
                  span(class = "guide-route-icon", icon("download")),
                  span(class = "guide-route-copy",
                    tags$strong("Desktop Downloads"),
                    tags$small("Install organisms from the desktop catalog before starting an analysis.")
                  )
                ),
                tags$button(
                  type = "button",
                  class = "guide-route-card is-active",
                  `data-guide-route` = "multigene",
                  span(class = "guide-route-number", "01"),
                  span(class = "guide-route-icon", icon("search")),
                  span(class = "guide-route-copy",
                    tags$strong("Multi-Gene Search"),
                    tags$small("Compare several genes inside one selected organism.")
                  )
                ),
                tags$button(
                  type = "button",
                  class = "guide-route-card",
                  `data-guide-route` = "cross",
                  span(class = "guide-route-number", "02"),
                  span(class = "guide-route-icon", icon("sitemap")),
                  span(class = "guide-route-copy",
                    tags$strong("Cross-Species Search"),
                    tags$small("Compare one gene across multiple organisms.")
                  )
                ),
                tags$button(
                  type = "button",
                  class = "guide-route-card",
                  `data-guide-route` = "common",
                  span(class = "guide-route-number", "03"),
                  span(class = "guide-route-icon", icon("chart-line")),
                  span(class = "guide-route-copy",
                    tags$strong("Common Analysis"),
                    tags$small("Review models, analytics, reports, and shared tools.")
                  )
                ),
                tags$button(
                  type = "button",
                  class = "guide-route-card",
                  `data-guide-route` = "figure-studio",
                  span(class = "guide-route-number", "04"),
                  span(class = "guide-route-icon", icon("object-group")),
                  span(class = "guide-route-copy",
                    tags$strong("Figure Studio"),
                    tags$small("Compose CGeV results as a publication-ready multi-panel figure.")
                  )
                )
              ),
              div(
                class = "guide-workspace",
              div(
                class = "guide-step-panel",
                div(
                  class = "guide-panel-head",
                  span(class = "guide-kicker", icon("list-ol"), "Step by step"),
                  h2(id = "guide-route-title", "Multi-Gene Search"),
                  p(id = "guide-route-summary", "Use this path when the analysis is centered on one organism and several genes.")
                ),
                div(
                  class = "guide-step-list",
                  tags$button(
                    type = "button",
                    class = "guide-step-item guide-step-parent",
                    `data-step-index` = "0",
                    `data-step-title` = "Choose organism",
                    `data-step-copy` = "Select the organism or assembly that will define the annotation set for the Multi-Gene workflow.",
                    `data-step-media` = guide_media_map[["guide-multigene-01a-preloaded-organism.mp4"]],
                    span(class = "guide-step-number", "01"),
                    span(class = "guide-step-text",
                      tags$strong("Choose organism"),
                      tags$small("Pick the reference organism.")
                    ),
                    span(class = "guide-step-chevron", icon("chevron-down"))
                  ),
                  tags$button(
                    type = "button",
                    class = "guide-step-item guide-step-parent",
                    `data-step-index` = "1",
                    `data-step-title` = "Search and add genes",
                    `data-step-copy` = "Type gene names, use autocomplete suggestions, and build a batch for comparison.",
                    `data-step-media` = "",
                    span(class = "guide-step-number", "02"),
                    span(class = "guide-step-text",
                      tags$strong("Search and add genes"),
                      tags$small("Build the gene batch.")
                    ),
                    span(class = "guide-step-chevron", icon("chevron-down"))
                  ),
                  tags$button(
                    type = "button",
                    class = "guide-step-item",
                    `data-step-index` = "2",
                    `data-step-title` = "Generate visualization",
                    `data-step-copy` = "Run the search with Generate visualization, or press Enter where the active search control supports it.",
                    `data-step-media` = "",
                    span(class = "guide-step-number", "03"),
                    span(class = "guide-step-text",
                      tags$strong("Generate visualization"),
                      tags$small("Create the comparison view.")
                    )
                  ),
                  tags$button(
                    type = "button",
                    class = "guide-step-item guide-step-parent",
                    `data-step-index` = "3",
                    `data-step-title` = "Inspect chart",
                    `data-step-copy` = "Review the rendered gene model, inspect compact or detailed visualizations, and use hover tooltips for feature context.",
                    `data-step-media` = "",
                    span(class = "guide-step-number", "04"),
                    span(class = "guide-step-text",
                      tags$strong("Inspect chart"),
                      tags$small("Compact or detailed views.")
                    ),
                    span(class = "guide-step-chevron", icon("chevron-down"))
                  )
                ),
                div(
                  class = "guide-actions",
                  tags$button(
                    type = "button",
                    class = "guide-primary-btn",
                    `data-guide-open` = "multigene",
                    onclick = "document.querySelector('.app-nav-btn[data-target=\"homologous\"]').click();",
                    icon("search"),
                    "Open Multi-Gene"
                  ),
                  tags$button(
                    type = "button",
                    class = "guide-secondary-btn",
                    `data-guide-open` = "cross",
                    onclick = "document.querySelector('.app-nav-btn[data-target=\"orthologous\"]').click();",
                    icon("sitemap"),
                    "Open Cross-Species"
                  ),
                  tags$button(
                    type = "button",
                    class = "guide-secondary-btn",
                    `data-guide-open` = "figure-studio",
                    onclick = "document.querySelector('.app-nav-btn[data-target=\"figure-studio\"]').click();",
                    icon("object-group"),
                    "Open Figure Studio"
                  )
                )
              ),
              div(
                class = "guide-media-panel",
                div(
                  class = "guide-video-shell",
                  div(
                    class = "guide-media-caption",
                    h3(id = "guide-media-title", "Welcome to CGeV Guide"),
                    p(id = "guide-media-copy", "Choose a workflow, then select the step you want to learn. The intro video appears here until you pick a specific step.")
                  ),
                  div(
                    class = "guide-video-frame",
                    div(
                      class = "guide-video-placeholder",
                      tags$video(
                        id = "guide-media-video",
                        class = "guide-media-video",
                        autoplay = NA,
                        muted = NA,
                        loop = NA,
                        playsinline = NA,
                        preload = "none",
                        tabindex = "-1"
                      ),
                      span(class = "guide-video-icon", icon("film")),
                      h3("Video / GIF placeholder")
                    )
                  )
                ),
                div(
                  class = "guide-media-notes",
                  div(
                    class = "guide-note",
                    span(class = "guide-note-icon", icon("video")),
                    span("Drop a short MP4 or GIF into this placeholder when the screen capture is ready.")
                  ),
                  div(
                    class = "guide-note",
                    span(class = "guide-note-icon", icon("mouse-pointer")),
                    span("Users can move route by route and step by step without leaving this page.")
                  )
                )
              )
              )
            ),
            tags$script(HTML(sprintf(
              "window.CGV_GUIDE_MEDIA = %s;",
              jsonlite::toJSON(as.list(guide_media_map), auto_unbox = TRUE)
            ))),
            tags$script(HTML("
              (function() {
                var root = document.querySelector('.app-guide-pane');
                if (!root || root.dataset.guideReady === '1') return;
                root.dataset.guideReady = '1';
                var guideMedia = window.CGV_GUIDE_MEDIA || {};

                function guideVideo(fileName) {
                  return guideMedia[fileName] || '';
                }

                var routeData = {
                  multigene: {
                    title: 'Multi-Gene Search',
                    summary: 'Use this path when the analysis is centered on one organism and several genes.',
                    open: 'multigene',
                    steps: [
                      {
                        title: 'Choose organism',
                        copy: 'Select the organism or assembly that will define the annotation set for the Multi-Gene workflow.',
                        short: 'Preloaded, NCBI, or upload.',
                        substeps: [
                          { title: 'Preloaded organism', short: 'Use an available organism.', copy: 'Choose an organism whose reference annotation and genome are available in this CGeV session. Hosted deployments may provide references directly, while CGeV Desktop installs them on demand.', media: guideVideo('guide-multigene-01a-preloaded-organism.mp4') },
                          { title: 'NCBI search', short: 'Find a new assembly.', copy: 'Search NCBI when the organism is not preloaded, then let CGeV prepare the selected assembly for the workflow.', media: guideVideo('guide-multigene-01b-ncbi-search.mp4') },
                          { title: 'Upload files', short: 'Bring your own data.', copy: 'Upload compatible annotation and genome files when you want to work with your own local dataset.', media: guideVideo('guide-multigene-01c-upload-files.mp4') }
                        ]
                      },
                      {
                        title: 'Search and add genes',
                        copy: 'Type gene names, use autocomplete suggestions, and build the gene set that will be visualized together.',
                        short: 'One gene or batch genes.',
                        substeps: [
                          { title: 'Add one gene', short: 'Search a single gene.', copy: 'Enter one gene name, select the best match when suggestions appear, and add it to the current analysis.', media: guideVideo('guide-multigene-02a-add-one-gene.mp4') },
                          { title: 'Add batch genes', short: 'Build a comparison set.', copy: 'Add several genes to the batch list before running the visualization, so CGeV can render them as one comparison.', media: guideVideo('guide-multigene-02b-add-batch-genes.mp4') }
                        ]
                      },
                      {
                        title: 'Generate visualization',
                        copy: 'Click Generate visualization to render the selected genes and keep the resulting charts in the workspace.',
                        short: 'Render the gene models.',
                        media: guideVideo('guide-multigene-03-generate-visualization.mp4')
                      },
                      {
                        title: 'Inspect chart',
                        copy: 'Review the rendered gene model, inspect compact or detailed visualizations, and use hover tooltips for feature context.',
                        short: 'Compact or detailed views.',
                        substeps: [
                          { title: 'Compact visualization', short: 'Hover for tooltips.', copy: 'Use compact view for a condensed structural overview, then hover over interactive regions to inspect coordinates and feature details.', media: guideVideo('guide-multigene-04a-compact-visualization.mp4') },
                          { title: 'Detailed visualization', short: 'Feature-level view.', copy: 'Switch to detailed view when you need exon, CDS, UTR, intron, transcript, and coordinate detail at a more granular level.', media: guideVideo('guide-multigene-04b-detailed-visualization.mp4') }
                        ]
                      },
                      {
                        title: 'Alignment, optional',
                        copy: 'Use Synteny to compare transcript structures within the same gene when multiple transcript isoforms are available.',
                        short: 'Compare isoforms with Synteny.',
                        media: guideVideo('guide-multigene-05-alignment-optional.mp4')
                      },
                      {
                        title: 'Export',
                        copy: 'Download publication-ready figures and supporting result tables when the analysis is ready to share.',
                        short: 'Save figures and tables.',
                        substeps: [
                          { title: 'Export figures', short: 'Save visual outputs.', copy: 'Export the generated visualizations for reports, teaching material, or publication figures.', media: guideVideo('guide-multigene-06a-export-figures.mp4') },
                          { title: 'Export tables/results', short: 'Save supporting data.', copy: 'Download available tables or result summaries when you need the structured data behind the visual analysis.', media: guideVideo('guide-multigene-06b-export-tables-results.mp4') }
                        ]
                      }
                    ]
                  },
                  cross: {
                    title: 'Cross-Species Search',
                    summary: 'Use this path to compare one selected gene across organisms. This comparison is not a complete inventory of every gene-family member within each organism.',
                    open: 'cross',
                    steps: [
                      {
                        title: 'Choose organisms',
                        copy: 'Build the organism set that CGeV will use for the cross-species comparison.',
                        short: 'Preloaded, NCBI, upload, or mixed.',
                        substeps: [
                          { title: 'Preloaded organisms', short: 'Use available species.', copy: 'Select organisms from the available registry when their annotations and genomes are ready in this session. In CGeV Desktop, install organisms from the catalog before using them here.', media: guideVideo('guide-cross-01a-preloaded-organisms.mp4') },
                          { title: 'NCBI search', short: 'Add external assemblies.', copy: 'Search NCBI to add organisms or assemblies that are not part of the preloaded set.', media: guideVideo('guide-cross-01b-ncbi-search.mp4') },
                          { title: 'Upload files', short: 'Use your own data.', copy: 'Upload compatible annotation and genome files for organisms that should be included in the comparison.', media: guideVideo('guide-cross-01c-upload-files.mp4') },
                          { title: 'Mixed sources', short: 'Combine inputs.', copy: 'Combine preloaded organisms, NCBI-downloaded assemblies, and uploaded files in the same cross-species analysis.', media: guideVideo('guide-cross-01d-mixed-sources.mp4') }
                        ]
                      },
                      { title: 'Search gene', copy: 'Enter one target gene and let CGeV resolve compatible identifiers across the selected organisms. Additional family members found only within individual organisms are not listed here; use Multi-Gene Search to explore a family in one organism.', short: 'Focus on one shared gene.', media: guideVideo('guide-cross-02-search-gene.mp4') },
                      {
                        title: 'Generate visualization',
                        copy: 'Click Generate visualization to render the selected cross-species comparison and keep the returned charts in the workspace.',
                        short: 'Render the comparison.',
                        media: guideVideo('guide-cross-03-generate-visualization.mp4')
                      },
                      {
                        title: 'Inspect chart',
                        copy: 'Review the rendered cross-species gene model, inspect compact or detailed visualizations, and use hover tooltips for feature context.',
                        short: 'Compact or detailed views.',
                        substeps: [
                          { title: 'Compact visualization', short: 'Condensed comparison.', copy: 'Use compact visualization to compare representative gene structures across organisms in a concise layout.', media: guideVideo('guide-cross-03a-compact-visualization.mp4') },
                          { title: 'Detailed visualization', short: 'Inspect features.', copy: 'Use detailed visualization to inspect feature-level structure and transcript details across organisms.', media: guideVideo('guide-cross-03b-detailed-visualization.mp4') }
                        ]
                      },
                      {
                        title: 'Align mode',
                        copy: 'Use alignment-oriented views when you need structural or sequence-level comparison across organisms.',
                        short: 'Synteny, LASTZ, or MultiPIP.',
                        substeps: [
                          { title: 'Comparative synteny align', short: 'Compare ordered structures.', copy: 'Use comparative synteny alignment to inspect how gene structures line up across selected organisms.', media: guideVideo('guide-cross-05a-comparative-synteny-align.mp4') },
                          { title: 'LASTZ blocks', short: 'Inspect local blocks.', copy: 'Use LASTZ blocks to review local alignment fragments and identity patterns between loci.', media: guideVideo('guide-cross-05b-lastz-blocks.mp4') },
                          { title: 'MultiPIP', short: 'Multi-organism alignment view.', copy: 'Use MultiPIP to inspect aligned conservation-style blocks across multiple organisms when the required data is available.', media: guideVideo('guide-cross-05c-multipip.mp4') }
                        ]
                      },
                      {
                        title: 'Export',
                        copy: 'Export cross-species visualizations or alignment views once the comparison is ready to share.',
                        short: 'Save comparison figures.',
                        media: guideVideo('guide-cross-06a-export-alignment-visual-figures.mp4')
                      }
                    ]
                  },
                  common: {
                    title: 'Common Analysis',
                    summary: 'Shared tools for interpreting models, charts, reports, sessions, settings, and cleanup in CGeV.',
                    open: '',
                    steps: [
                      { title: 'Review analytics charts', copy: 'Analyze the statistics generated from the current visualizations, including structural and sequence-derived summaries.', short: 'Statistics from rendered data.', media: guideVideo('guide-common-01-review-analytics-charts.mp4') },
                      { title: 'Review tables/results', copy: 'Inspect the tables and result summaries that support the currently rendered gene or cross-species views.', short: 'Structured result views.', media: guideVideo('guide-common-02-review-tables-results.mp4') },
                      { title: 'Visualize transcript variants', copy: 'Compare transcript isoforms from the same gene to inspect differences in exon structure, CDS organization, UTRs, and transcript length.', short: 'Isoforms from one gene.', media: guideVideo('guide-common-03-visualize-transcript-variants.mp4') },
                      { title: 'Inspect gene information', copy: 'Open gene context layers from available external databases and CGeV popups to add biological meaning to the visualization.', short: 'External database context.', media: guideVideo('guide-common-04-inspect-gene-information.mp4') },
                      { title: 'Download promoter sequences', copy: 'Use promoter tools when you need upstream sequence context associated with the selected gene model.', short: 'Promoter sequence context.', media: guideVideo('guide-common-05-download-promoter-sequences.mp4') },
                      { title: 'Review literature', copy: 'Inspect literature associated with the gene when CGeV can connect the gene context to papers or external references.', short: 'Gene-associated papers.', media: guideVideo('guide-common-06-review-literature.mp4') },
                      { title: 'Review organism and assembly info', copy: 'Open organism photos, assembly reports, assembly statistics, and related organism metadata when available.', short: 'Organisms, assemblies, photos.', media: guideVideo('guide-common-07-review-organism-assembly-info.mp4') },
                      { title: 'Configure external alias lookup', copy: 'Turn external alias lookup sources on or off to control which databases CGeV uses when local gene names do not match directly.', short: 'Control alias databases.', media: guideVideo('guide-common-08-configure-external-alias-lookup.mp4') },
                      {
                        title: 'Sessions',
                        copy: 'Save or restore work sessions so you can continue an analysis without rebuilding it from scratch.',
                        short: 'Save or load work.',
                        substeps: [
                          { title: 'Save work session', short: 'Export current state.', copy: 'Save the current plots and settings as a session file when you want to return to the same analysis later.', media: guideVideo('guide-common-09a-save-work-session.mp4') },
                          { title: 'Load work session', short: 'Restore previous state.', copy: 'Load a previously saved session file to restore plots and settings into the current CGeV session.', media: guideVideo('guide-common-09b-load-work-session.mp4') }
                        ]
                      },
                      {
                        title: window.cgvDesktop ? 'Export interactive report' : 'Share analysis',
                        copy: window.cgvDesktop
                          ? 'Use Share to save a self-contained read-only HTML report and reproducibility ZIP locally. CGeV Desktop does not upload the analysis or create a public URL.'
                          : 'Use Share to create an expiring secret read-only URL. Choose Multi-Gene, Cross-Species, or both, review privacy and download options, and copy the completed link for the intended readers.',
                        short: window.cgvDesktop ? 'Save HTML and ZIP locally.' : 'Create a secret read-only link.',
                        media: window.cgvDesktop
                          ? guideVideo('guide-common-export-report-desktop.mp4')
                          : guideVideo('guide-common-share-analysis-web.mp4')
                      },
                      { title: 'Clear visualizations', copy: 'Remove current visualizations when you want to reset the workspace before starting a new analysis.', short: 'Reset the workspace.', media: guideVideo('guide-common-10-clear-visualizations.mp4') }
                    ]
                  },
                  'figure-studio': {
                    title: 'Figure Studio',
                    summary: 'Turn structural views, alignments, analytics, and labels already generated in CGeV into one reusable multi-panel figure.',
                    open: 'figure-studio',
                    steps: [
                      {
                        title: 'Open the workspace',
                        copy: 'Open Figure Studio after generating the CGeV results you want to compose. The source library only includes figures available in the current analysis.',
                        short: 'Start from current results.',
                        media: guideVideo('guide-figure-studio-01-open-workspace.mp4')
                      },
                      {
                        title: 'Add source panels',
                        copy: 'Choose gene structures, synteny or alignment views, analytics charts, and other available SVG sources, then add the required panels to the canvas.',
                        short: 'Choose CGeV visual sources.',
                        media: guideVideo('guide-figure-studio-02-add-panels.mp4')
                      },
                      {
                        title: 'Arrange and style',
                        copy: 'Reorder and resize panels, edit labels and captions, adjust the page layout, and keep related visual elements aligned.',
                        short: 'Build the multi-panel layout.',
                        media: guideVideo('guide-figure-studio-03-arrange-and-style.mp4')
                      },
                      {
                        title: 'Preview and export',
                        copy: 'Review the final composition and export SVG for editable vector output or PNG for convenient sharing. The Figure Studio composition is also included in an interactive report when it contains panels.',
                        short: 'Export SVG or PNG.',
                        media: guideVideo('guide-figure-studio-04-preview-and-export.mp4')
                      }
                    ]
                  },
                  'desktop-downloads': {
                    title: 'Desktop Downloads',
                    summary: 'Install organisms from the CGeV Desktop catalog. The base installer ships without organisms; up to 25 installable organisms are available depending on the catalog and what you choose to download.',
                    open: 'settings',
                    desktopOnly: true,
                    steps: [
                      { title: 'Open Settings', copy: 'Go to Settings and find the Organisms section. This section is available only in CGeV Desktop.', short: 'Go to Settings.', media: guideVideo('guide-desktop-downloads-01-open-settings.mp4') },
                      { title: 'Open organism catalog', copy: 'Click Open catalog to view the downloadable organism library for this desktop profile.', short: 'View the library.', media: guideVideo('guide-desktop-downloads-02-open-organism-catalog.mp4') },
                      { title: 'Search or filter', copy: 'Use search and status filters to find an organism. The catalog can provide up to 25 installable organisms, but only downloaded organisms appear in Preloaded selectors.', short: 'Find an organism.', media: guideVideo('guide-desktop-downloads-03-search-and-filter.mp4') },
                      { title: 'Download organism', copy: 'Click Download for the organism you need. CGeV Desktop downloads the package, verifies it, extracts it, and installs local caches.', short: 'Install references.', media: guideVideo('guide-desktop-downloads-04-download-organism.mp4') },
                      { title: 'Confirm availability', copy: 'After installation, return to Multi-Gene or Cross-Species Search and choose the organism from the Preloaded organism selectors.', short: 'Use the organism.', media: guideVideo('guide-desktop-downloads-05-confirm-availability.mp4') },
                      { title: 'Remove organisms', copy: 'Choose one, several, or all installed catalog organisms and remove only the selected local datasets and caches.', short: 'Selective cleanup.', media: guideVideo('guide-desktop-downloads-06-remove-installed-organisms.mp4') }
                    ]
                  }
                };

                root.classList.toggle('guide-runtime-desktop', !!window.cgvDesktop);
                if (!window.cgvDesktop) {
                  delete routeData['desktop-downloads'];
                  root.querySelectorAll('.guide-desktop-only').forEach(function(node) {
                    node.remove();
                  });
                }

                var routeTitle = root.querySelector('#guide-route-title');
                var routeSummary = root.querySelector('#guide-route-summary');
                var mediaTitle = root.querySelector('#guide-media-title');
                var mediaCopy = root.querySelector('#guide-media-copy');
                var mediaCaption = root.querySelector('.guide-media-caption');
                var mediaVideo = root.querySelector('#guide-media-video');
                var stepList = root.querySelector('.guide-step-list');
                var openButtons = root.querySelectorAll('[data-guide-open]');
                var videoBlobCache = {};
                var nextVideoPreload = null;
                var guideActivated = false;
                var currentMediaContent = null;
                var introMedia = guideVideo('guide-intro.mp4');
                var introContent = {
                  title: 'Welcome to CGeV Guide',
                  copy: 'Choose a workflow, then select the step you want to learn. The intro video appears here until you pick a specific step.',
                  media: introMedia
                };

                function markVideoReady(isReady) {
                  if (!mediaVideo) return;
                  mediaVideo.classList.toggle('is-ready', !!isReady);
                }

                function markVideoHasMedia(hasMedia) {
                  if (!mediaVideo) return;
                  var frame = mediaVideo.closest('.guide-video-placeholder');
                  if (frame) frame.classList.toggle('has-media', !!hasMedia);
                }

                if (mediaVideo) {
                  mediaVideo.muted = true;
                  mediaVideo.defaultMuted = true;
                  mediaVideo.playsInline = true;
                  mediaVideo.setAttribute('muted', '');
                  mediaVideo.setAttribute('playsinline', '');
                  mediaVideo.setAttribute('webkit-playsinline', '');
                  ['loadedmetadata', 'loadeddata', 'canplay', 'canplaythrough', 'playing'].forEach(function(eventName) {
                    mediaVideo.addEventListener(eventName, function() { markVideoReady(true); });
                  });
                  mediaVideo.addEventListener('error', function() {
                    markVideoReady(false);
                    if (mediaVideo.dataset.guideBlobSource !== '1') {
                      loadVideoAsBlob(mediaVideo.dataset.guideOriginalSource || mediaVideo.dataset.guideSource || '');
                    }
                  });
                }

                function playCurrentVideo() {
                  if (!mediaVideo) return;
                  window.requestAnimationFrame(function() {
                    mediaVideo.muted = true;
                    mediaVideo.defaultMuted = true;
                    mediaVideo.playsInline = true;
                    var playAttempt = mediaVideo.play();
                    if (playAttempt && typeof playAttempt.then === 'function') {
                      playAttempt.then(function() { markVideoReady(true); }).catch(function() {});
                    }
                  });
                }

                function setVideoSource(src, isBlobSource, originalSrc) {
                  if (!mediaVideo || !src) return;
                  markVideoHasMedia(true);
                  mediaVideo.dataset.guideSource = src;
                  mediaVideo.dataset.guideBlobSource = isBlobSource ? '1' : '0';
                  mediaVideo.dataset.guideOriginalSource = isBlobSource ? (originalSrc || '') : src;
                  mediaVideo.setAttribute('src', src);
                  var source = mediaVideo.querySelector('source');
                  if (source) source.setAttribute('src', src);
                  mediaVideo.load();
                  playCurrentVideo();
                }

                function loadVideoAsBlob(originalSrc) {
                  if (!mediaVideo || !originalSrc || !window.fetch || !window.URL || !window.URL.createObjectURL) return;
                  if (mediaVideo.dataset.guideBlobSource === '1') return;
                  if (videoBlobCache[originalSrc]) {
                    setVideoSource(videoBlobCache[originalSrc], true, originalSrc);
                    return;
                  }
                  fetch(originalSrc, { cache: 'force-cache' })
                    .then(function(response) {
                      if (!response.ok) throw new Error('Video request failed');
                      return response.blob();
                    })
                    .then(function(blob) {
                      if (!blob || !blob.size) return;
                      var blobUrl = URL.createObjectURL(blob);
                      videoBlobCache[originalSrc] = blobUrl;
                      if (mediaVideo.dataset.guideSource === originalSrc && !mediaVideo.classList.contains('is-ready')) {
                        setVideoSource(blobUrl, true, originalSrc);
                      }
                    })
                    .catch(function() {});
                }

                function routeVideoSequence(route) {
                  var data = routeData[route];
                  var sequence = introMedia ? [introMedia] : [];
                  if (!data || !data.steps) return sequence;
                  data.steps.forEach(function(step) {
                    if (step.substeps && step.substeps.length) {
                      step.substeps.forEach(function(substep) {
                        var nestedMedia = substep.media || step.media || '';
                        if (nestedMedia) sequence.push(nestedMedia);
                      });
                    } else if (step.media) {
                      sequence.push(step.media);
                    }
                  });
                  return sequence;
                }

                function getNextVideoSource(content) {
                  var source = content && content.media ? content.media : '';
                  if (!source) return '';
                  var sequence = routeVideoSequence(root.dataset.activeGuideRoute || 'multigene');
                  var currentIndex = sequence.indexOf(source);
                  return currentIndex >= 0 ? (sequence[currentIndex + 1] || '') : '';
                }

                function preloadNextVideo(src) {
                  if (nextVideoPreload && nextVideoPreload.getAttribute('href') === src) return;
                  if (nextVideoPreload && nextVideoPreload.parentNode) {
                    nextVideoPreload.parentNode.removeChild(nextVideoPreload);
                  }
                  nextVideoPreload = null;
                  if (!guideActivated || !src) return;
                  nextVideoPreload = document.createElement('link');
                  nextVideoPreload.rel = 'preload';
                  nextVideoPreload.as = 'video';
                  nextVideoPreload.href = src;
                  document.head.appendChild(nextVideoPreload);
                }

                function getStep(route, index, subIndex) {
                  var data = routeData[route];
                  var step = (data && data.steps[index]) || (data && data.steps[0]);
                  if (!step) return null;
                  if (subIndex !== null && subIndex !== undefined && step.substeps && step.substeps[subIndex]) {
                    var substep = step.substeps[subIndex];
                    return {
                      title: substep.title,
                      copy: substep.copy || step.copy,
                      short: substep.short || step.short || '',
                      media: substep.media || step.media || '',
                      parentTitle: step.title
                    };
                  }
                  return step;
                }

                function updateMedia(content) {
                  if (!content) return;
                  currentMediaContent = content;
                  if (mediaCaption) mediaCaption.classList.remove('is-refreshing');
                  if (mediaTitle) mediaTitle.textContent = content.title;
                  if (mediaCopy) mediaCopy.textContent = content.copy;
                  if (mediaCaption) {
                    void mediaCaption.offsetWidth;
                    mediaCaption.classList.add('is-refreshing');
                  }
                  if (!guideActivated) {
                    markVideoHasMedia(false);
                    markVideoReady(false);
                    return;
                  }
                  if (mediaVideo && content.media) {
                    var current = mediaVideo.dataset.guideOriginalSource || mediaVideo.dataset.guideSource || mediaVideo.getAttribute('src') || mediaVideo.currentSrc || '';
                    if (current !== content.media) {
                      setVideoSource(content.media, false, content.media);
                    }
                    preloadNextVideo(getNextVideoSource(content));
                  } else {
                    if (mediaVideo) {
                      mediaVideo.pause();
                      mediaVideo.removeAttribute('src');
                      mediaVideo.dataset.guideSource = '';
                      mediaVideo.dataset.guideOriginalSource = '';
                      mediaVideo.dataset.guideBlobSource = '0';
                      mediaVideo.load();
                    }
                    markVideoHasMedia(false);
                    markVideoReady(false);
                    preloadNextVideo('');
                  }
                }

                function clearStepSelection() {
                  root.querySelectorAll('.guide-step-item').forEach(function(item) {
                    item.classList.remove('is-active', 'is-expanded');
                    item.setAttribute('aria-expanded', 'false');
                  });
                  root.querySelectorAll('.guide-step-group').forEach(function(group) {
                    group.classList.remove('is-expanded');
                  });
                  root.querySelectorAll('.guide-substep-item').forEach(function(item) {
                    item.classList.remove('is-active');
                  });
                }

                function renderIntro() {
                  clearStepSelection();
                  updateMedia(introContent);
                }

                function renderStep(route, index, subIndex) {
                  var data = routeData[route];
                  var parentStep = data && data.steps[index];
                  if ((subIndex === null || subIndex === undefined) && parentStep && parentStep.substeps && parentStep.substeps.length) {
                    subIndex = 0;
                  }
                  var step = getStep(route, index, subIndex);
                  if (!data || !step) return;
                  root.querySelectorAll('.guide-step-item').forEach(function(item) {
                    item.classList.toggle('is-active', item.dataset.stepIndex === String(index));
                    item.classList.toggle('is-expanded', item.dataset.stepIndex === String(index));
                    item.setAttribute('aria-expanded', item.dataset.stepIndex === String(index) ? 'true' : 'false');
                  });
                  root.querySelectorAll('.guide-step-group').forEach(function(group) {
                    group.classList.toggle('is-expanded', group.dataset.stepIndex === String(index));
                  });
                  root.querySelectorAll('.guide-substep-item').forEach(function(item) {
                    item.classList.toggle(
                      'is-active',
                      item.dataset.stepIndex === String(index) && item.dataset.substepIndex === String(subIndex)
                    );
                  });
                  updateMedia(step);
                }

                function renderRoute(route) {
                  var data = routeData[route] || routeData.multigene;
                  root.dataset.activeGuideRoute = route;
                  root.querySelectorAll('[data-guide-route]').forEach(function(card) {
                    card.classList.toggle('is-active', card.dataset.guideRoute === route);
                  });
                  if (routeTitle) routeTitle.textContent = data.title;
                  if (routeSummary) routeSummary.textContent = data.summary;
                  if (stepList) {
                    stepList.innerHTML = '';
                    data.steps.forEach(function(step, index) {
                      var group = document.createElement('div');
                      group.className = 'guide-step-group';
                      group.dataset.stepIndex = String(index);
                      var button = document.createElement('button');
                      button.type = 'button';
                      button.className = 'guide-step-item guide-step-parent';
                      button.dataset.stepIndex = String(index);
                      button.dataset.stepTitle = step.title;
                      button.dataset.stepCopy = step.copy;
                      button.dataset.stepMedia = step.media || '';
                      button.setAttribute('aria-expanded', 'false');
                      button.innerHTML =
                        '<span class=\"guide-step-number\">' + String(index + 1).padStart(2, '0') + '</span>' +
                        '<span class=\"guide-step-text\"><strong></strong><small></small></span>' +
                        (step.substeps && step.substeps.length ? '<span class=\"guide-step-chevron\"><i class=\"fa fa-chevron-down\"></i></span>' : '');
                      button.querySelector('strong').textContent = step.title;
                      button.querySelector('small').textContent = step.short || '';
                      button.addEventListener('click', function() { renderStep(route, index, null); });
                      group.appendChild(button);
                      if (step.substeps && step.substeps.length) {
                        var subList = document.createElement('div');
                        subList.className = 'guide-substep-list';
                        step.substeps.forEach(function(substep, subIndex) {
                          var subButton = document.createElement('button');
                          subButton.type = 'button';
                          subButton.className = 'guide-substep-item';
                          subButton.dataset.stepIndex = String(index);
                          subButton.dataset.substepIndex = String(subIndex);
                          subButton.innerHTML =
                            '<span class=\"guide-substep-marker\"></span>' +
                            '<span class=\"guide-substep-text\"><strong></strong><small></small></span>';
                          subButton.querySelector('strong').textContent = substep.title;
                          subButton.querySelector('small').textContent = substep.short || '';
                          subButton.addEventListener('click', function() { renderStep(route, index, subIndex); });
                          subList.appendChild(subButton);
                        });
                        group.appendChild(subList);
                      }
                      stepList.appendChild(group);
                    });
                  }
                  openButtons.forEach(function(button) {
                    var target = button.dataset.guideOpen;
                    button.hidden = route === 'common' || route === 'desktop-downloads' || (target !== data.open && target !== 'common');
                  });
                  renderIntro();
                }

                function isGuideTabActive() {
                  var pane = root.closest ? root.closest('.tab-pane') : null;
                  if (pane && (pane.classList.contains('active') || pane.classList.contains('show'))) return true;
                  var activeButton = document.querySelector('.app-nav-btn.is-active[data-target=guide]');
                  return !!activeButton;
                }

                function activateGuideMedia() {
                  if (guideActivated) return;
                  guideActivated = true;
                  root.dataset.guideMediaActive = '1';
                  if (mediaVideo) mediaVideo.preload = 'auto';
                  updateMedia(currentMediaContent || introContent);
                }

                function activateGuideFromEvent(event) {
                  var target = event && event.target ? event.target : null;
                  var guideTarget = target && target.closest
                    ? target.closest('.app-nav-btn[data-target=guide], #navtabs a[data-value=guide]')
                    : null;
                  if (guideTarget || isGuideTabActive()) activateGuideMedia();
                }

                root.querySelectorAll('[data-guide-route]').forEach(function(card) {
                  card.addEventListener('click', function() {
                    var route = card.dataset.guideRoute || 'multigene';
                    if (!routeData[route]) route = 'multigene';
                    renderRoute(route);
                  });
                });
                document.addEventListener('click', activateGuideFromEvent);
                document.addEventListener('shown.bs.tab', activateGuideFromEvent);
                if (window.jQuery) {
                  jQuery(document).on('shown.bs.tab.cgvGuideMedia', activateGuideFromEvent);
                  jQuery(document).on('shiny:inputchanged.cgvGuideMedia', function(event) {
                    if (event.name === 'navtabs' && event.value === 'guide') activateGuideMedia();
                  });
                }
                renderRoute('multigene');
                if (isGuideTabActive()) activateGuideMedia();
              })();
            "))
          )
        ),
        tabPanel(
          title = "Home",
          value = "home",
          div(
            class = "content-wrapper app-main-pane app-home-pane app-home-pane-isolated",
            tags$iframe(
              id = "cgv-home-iframe",
              class = "cgv-home-iframe",
              src = versioned_asset_path("home_preview_cgv.html"),
              title = "Comparative Gene Viewer Home",
              loading = "eager",
              scrolling = "yes"
            )
          ),
          tags$style(HTML("
            .app-home-pane-isolated {
              padding: 0 !important;
              overflow: visible !important;
              background: transparent !important;
            }
            .cgv-home-iframe {
              display: block;
              width: 100%;
              height: calc(100dvh - 12px);
              min-height: 640px;
              max-height: calc(100dvh - 12px);
              border: 0;
              background: transparent;
              overflow: auto;
              contain: strict;
              transform: translateZ(0);
              backface-visibility: hidden;
              transition: none !important;
            }
            .cgv-home-iframe.cgv-home-iframe-resizing {
              pointer-events: none;
              transform: translateZ(0);
            }
            .app-shell.home-sidebar-transitioning .app-home-pane-isolated {
              contain: strict;
              overflow: hidden !important;
            }
            .app-home-pane-isolated {
              contain: layout paint style;
            }
            /* Feedback uses the same animation utility classes as Home.
               Because the new Home is now inside an iframe, we force Feedback
               to remain visible even if the Home animation script does not run there. */
            .feedback-shell .home-reveal,
            .feedback-shell .home-stagger-child,
            .feedback-shell .home-stagger-parent,
            .feedback-shell .feedback-hero,
            .feedback-shell .feedback-info-card,
            .feedback-shell .feedback-form-section {
              opacity: 1 !important;
              visibility: visible !important;
              transform: none !important;
              filter: none !important;
            }
          ")),
          tags$script(HTML("
            (function () {
              if (window.__cgvHomeIframeBridgeBound) return;
              window.__cgvHomeIframeBridgeBound = true;

              var allowedTargets = ['home', 'catalog', 'homologous', 'orthologous', 'figure-studio', 'guide', 'desktop-app', 'settings', 'help', 'feedback'];

              function resetHomeIframeScroll() {
                var iframe = document.getElementById('cgv-home-iframe');
                if (!iframe || !iframe.contentWindow) return;
                try { iframe.contentWindow.scrollTo(0, 0); } catch (err) {}
              }

              function activateTarget(target) {
                if (allowedTargets.indexOf(target) === -1) return;

                var sidebarButton = document.querySelector('.app-nav-btn[data-target=\"' + target + '\"]');
                if (sidebarButton) {
                  sidebarButton.click();
                  return;
                }

                var tabLink = document.querySelector('#navtabs a[data-value=\"' + target + '\"]');
                if (tabLink) {
                  if (window.jQuery && jQuery.fn && jQuery.fn.tab) jQuery(tabLink).tab('show');
                  else tabLink.click();
                }
              }

              window.addEventListener('message', function (event) {
                var data = event.data || {};
                if (!data || typeof data !== 'object') return;

                if (data.type === 'cgv-home-height') {
                  // Intentionally ignored.
                  // The Home uses internal scroll/sticky storytelling; auto-expanding
                  // the iframe to full document height breaks that interaction.
                }

                if (data.type === 'cgv-home-nav') {
                  activateTarget(String(data.target || ''));
                }
              });

              document.addEventListener('click', function (event) {
                var btn = event.target && event.target.closest ? event.target.closest('.app-nav-btn[data-target=\"home\"]') : null;
                if (btn) setTimeout(resetHomeIframeScroll, 80);
              });

              // Sidebar resizing is coordinated by prepareHomeSidebarTransition().
              // Keeping the guard in one place avoids duplicate timers and layouts.
            })();
          "))
        ),
        if (isTRUE(cgv_gene_catalog_enabled)) tabPanel(
          title = "Gene Catalog",
          value = "catalog",
          div(
            class = "content-wrapper app-main-pane gene-catalog-pane",
            div(
              class = "gene-catalog-shell",
              div(
                class = "gene-catalog-hero",
                div(
                  class = "gene-catalog-heading",
                  span(class = "gene-catalog-kicker", icon("database"), "Installed organism indexes"),
                  h2("Gene Catalog"),
                  p("Discover exact genes and gene-family candidates by symbol, stable ID, alias, or synonym — without choosing an organism first.")
                ),
                div(
                  class = "gene-catalog-search-row",
                  textInput(
                    inputId = "catalog_gene_query",
                    label = NULL,
                    placeholder = "Try HKT, TP53, BRCA1, actin…"
                  ),
                  actionButton(
                    inputId = "catalog_search_go",
                    label = span(icon("search"), "Search catalog"),
                    class = "btn-primary gene-catalog-search-btn"
                  )
                ),
                div(class = "gene-catalog-meta", icon("circle-info"), textOutput("catalog_search_status", inline = TRUE))
              ),
              div(
                class = "gene-catalog-results-card",
                div(
                  class = "gene-catalog-results-toolbar",
                  div(
                    tags$strong("Matches"),
                    span("Exact results rank first; family and partial candidates follow.")
                  ),
                  downloadButton("download_catalog_csv", span(icon("download"), "Download CSV"), class = "btn-sm btn-download")
                ),
                uiOutput("catalog_search_summary"),
                div(
                  class = "gene-catalog-filterbar",
                  selectInput("catalog_kingdom_filter", "Kingdom", choices = c("All kingdoms" = "all", "Animalia", "Plantae", "Fungi", "Other"), selected = "all"),
                  selectInput("catalog_match_filter", "Match", choices = c("All matches" = "all", "Exact / normalized" = "exact", "Prefix / family" = "prefix", "Contains" = "contains"), selected = "all"),
                  selectInput("catalog_confidence_filter", "Confidence", choices = c("All confidence" = "all", "High" = "HIGH", "Medium" = "MEDIUM", "Low" = "LOW"), selected = "all"),
                  checkboxInput("catalog_best_per_organism", "Best candidate per organism", value = FALSE)
                ),
                uiOutput("catalog_selection_actions"),
                DT::dataTableOutput("catalog_results_dt")
              )
            )
          )
        ) else NULL,
        tabPanel(
          title = "Figure Studio",
          value = "figure-studio",
          figure_studio_page()
        ),
        tabPanel(
          title = "CGeV Desktop",
          value = "desktop-app",
          cgv_desktop_downloads_page()
        ),
        tabPanel(
          title = "Multi-Gene Search",
          value = "homologous",
          div(
            class = "content-wrapper app-main-pane app-main-pane-search-results",
            div(
              id = "homo_context_section",
              class = "summary-context-section",
              style = "display:none;",
              initial_summary_context_header("Multi-Gene", "Genes: pending"),
              uiOutput("homo_context_header"),
              result_workspace_subheader("homo")
            ),
            div(
              id = "homo_summary_section",
              class = "summary-section homo-summary-section",
              style = "display:none;",
              div(
                class = "result-workspace-view-intro",
                div(
                  class = "result-workspace-view-intro-copy",
                  span(class = "result-workspace-view-kicker", icon("table"), " Structured results"),
                  h2("Summary table"),
                  p("Search, sort and review the records behind the current gene visualizations.")
                ),
                tags$button(
                  type = "button",
                  class = "result-workspace-view-back",
                  `data-workspace-back` = "true",
                  `data-workspace-scope` = "homo",
                  `aria-label` = "Back to Multi-Gene visualization",
                  icon("arrow-left"),
                  span("Back to visualization")
                )
              ),
              div(
                id = "homo_summary_body",
                class = "result-workspace-table-body",
                style = "display:none;",
                DT::dataTableOutput("homo_summary_dt")
              )
            ),
            # ── Sección Analytics Homologous ──────────────────────
            div(
              id = "homo_analytics_section",
              class = "analytics-section",
              style = "display:none;",
              div(
                id = "homo_analytics_body",
                class = "analytics-body",
                style = "display:none; margin-top:2px;",
                div(
                  class = "result-workspace-view-intro",
                  div(
                    class = "result-workspace-view-intro-copy",
                    span(class = "result-workspace-view-kicker", icon("chart-bar"), " Comparative metrics"),
                    h2("Analytics"),
                    p("Explore statistical views generated from the active set of gene results.")
                  ),
                  tags$button(
                    type = "button",
                    class = "result-workspace-view-back",
                    `data-workspace-back` = "true",
                    `data-workspace-scope` = "homo",
                    `aria-label` = "Back to Multi-Gene visualization",
                    icon("arrow-left"),
                    span("Back to visualization")
                  )
                ),
                div(
                class = "analytics-tabs-shell",
                  tabsetPanel(
                    id = "homo_analytics_tabs",
                    type = "pills",
                    tabPanel(
                      title = tagList(icon("dna"), " Architecture"),
                      value = "arch",
                      div(
                        class = "analytics-chart-wrap",
                        analytics_chart_toolbar("homo_arch_chart", "homo_architecture.svg", "homo_arch_order", choices = analytics_arch_order_choices),
                        chart_info_tip("<strong>Gene Architecture</strong><br/>Stacked bar showing the base-pair composition of each gene: CDS (coding), UTR, and introns.<div class='ci-axes'><b>Y-axis:</b> Gene entry<br/><b>X-axis:</b> Base pairs (bp)</div><div class='ci-tip'>Longer bars = larger genes. Compare CDS proportion across genes.</div>"),
                        shinycssloaders::withSpinner(
                          ggiraph::girafeOutput("homo_arch_chart", height = "auto", width = "100%"),
                          color = "#18BC9C", type = 6
                        )
                      )
                    ),
                    tabPanel(
                      title = tagList(icon("layer-group"), " Exons / Introns"),
                      value = "exon",
                      div(
                        class = "analytics-chart-wrap",
                        analytics_chart_toolbar("homo_exon_chart", "homo_exons_introns.svg", "homo_exon_order", choices = analytics_exon_order_choices),
                        chart_info_tip("<strong>Exon &amp; Intron Comparison</strong><br/>Left panel: exon vs intron count. Right panel: exonic vs intronic base pairs.<div class='ci-axes'><b>Left Y-axis:</b> Count<br/><b>Right Y-axis:</b> Base pairs (bp)</div><div class='ci-tip'>Compare structural complexity &mdash; more introns often indicate regulatory complexity.</div>"),
                        shinycssloaders::withSpinner(
                          ggiraph::girafeOutput("homo_exon_chart", height = "auto", width = "100%"),
                          color = "#18BC9C", type = 6
                        )
                      )
                    ),
                    tabPanel(
                      title = tagList(icon("microscope"), " Sequence"),
                      value = "seq",
                      div(
                        class = "analytics-chart-wrap",
                        analytics_chart_toolbar("homo_seq_chart", "homo_sequence_composition.svg", "homo_seq_order", choices = analytics_seq_order_choices),
                        chart_info_tip("<strong>Nucleotide Composition</strong><br/>Percentage of each nucleotide (A, T, C, G) per gene. GC% annotated on the right.<div class='ci-axes'><b>Y-axis:</b> Gene entry<br/><b>X-axis:</b> Percentage (0&ndash;50%)</div><div class='ci-tip'>Higher GC% may indicate thermal stability or coding density.</div>"),
                        shinycssloaders::withSpinner(
                          ggiraph::girafeOutput("homo_seq_chart", height = "auto", width = "100%"),
                          color = "#18BC9C", type = 6
                        )
                      )
                    ),
                    tabPanel(
                      title = tagList(icon("map-marker-alt"), " Genomic Context"),
                      value = "context",
                      div(
                        class = "analytics-chart-wrap",
                        analytics_chart_toolbar("homo_context_chart", "homo_genomic_context.svg", "homo_context_order", choices = analytics_context_order_choices),
                        chart_info_tip("<strong>Genomic Context</strong><br/>Distance to the nearest neighboring gene in separate panels (Upstream and Downstream).<div class='ci-axes'><b>Y-axis:</b> Gene entry<br/><b>X-axis:</b> Signed edge-to-edge distance (bp)</div><div class='ci-tip'>Negative = overlap; zero/positive = gap. The panel defines the biological side (not the sign).</div>"),
                        shinycssloaders::withSpinner(
                          ggiraph::girafeOutput("homo_context_chart", height = "auto", width = "100%"),
                          color = "#18BC9C", type = 6
                        )
                      )
                    ),
                    tabPanel(
                      title = tagList(icon("ruler-horizontal"), " Exon Lengths"),
                      value = "exon_dist",
                      div(
                        class = "analytics-chart-wrap",
                        analytics_chart_toolbar("homo_exon_dist_chart", "homo_exon_lengths.svg", "homo_exon_dist_order", choices = analytics_exon_dist_order_choices),
                        chart_info_tip("<strong>Exon Length Distribution</strong><br/>Violin + box + jitter showing individual exon lengths per gene on a log&#8321;&#8320; scale.<div class='ci-axes'><b>Y-axis:</b> Gene entry<br/><b>X-axis:</b> Exon length in bp (log&#8321;&#8320;)</div><div class='ci-tip'>Box = IQR (25th&ndash;75th percentile). Line = median. Dots = individual exons.</div>"),
                        shinycssloaders::withSpinner(
                          ggiraph::girafeOutput("homo_exon_dist_chart",
                            height = "auto", width = "100%"
                          ),
                          type = 4, color = "#2C3E50", size = 0.6
                        )
                      )
                    ),
                    tabPanel(
                      title = tagList(icon("arrows-left-right"), " Intron Lengths"),
                      value = "intron_dist",
                      div(
                        class = "analytics-chart-wrap",
                        analytics_chart_toolbar("homo_intron_dist_chart", "homo_intron_lengths.svg", "homo_intron_dist_order", choices = analytics_intron_dist_order_choices),
                        chart_info_tip("<strong>Intron Length Distribution</strong><br/>Violin + box + jitter showing individual intron lengths inferred from gaps between consecutive annotated exons.<div class='ci-axes'><b>Y-axis:</b> Gene entry<br/><b>X-axis:</b> Intron length in bp (log&#8321;&#8320;)</div><div class='ci-tip'>Genes without introns are omitted from the distribution; if none have introns, a clear no-intron message is shown.</div>"),
                        shinycssloaders::withSpinner(
                          ggiraph::girafeOutput("homo_intron_dist_chart",
                            height = "auto", width = "100%"
                          ),
                          type = 4, color = "#2C3E50", size = 0.6
                        )
                      )
                    ),
                    tabPanel(
                      title = tagList(icon("circle-dot"), " Scatter"),
                      value = "scatter",
                      div(
                        class = "analytics-chart-wrap",
                        analytics_chart_toolbar("homo_scatter_chart", "homo_scatter.svg", "homo_scatter_order", choices = analytics_scatter_order_choices),
                        chart_info_tip(analytics_scatter_tip_html),
                        shinycssloaders::withSpinner(
                          ggiraph::girafeOutput("homo_scatter_chart",
                            height = "auto", width = "100%"
                          ),
                          type = 4, color = "#2C3E50", size = 0.6
                        )
                      )
                    ),
                    tabPanel(
                      title = tagList(icon("table-cells"), " Heatmap"),
                      value = "heatmap",
                      div(
                        class = "analytics-chart-wrap",
                        analytics_chart_toolbar("homo_heatmap_chart", "homo_heatmap.svg", "homo_heatmap_order", choices = analytics_heatmap_order_choices),
                        chart_info_tip(analytics_heatmap_tip_html),
                        shinycssloaders::withSpinner(
                          ggiraph::girafeOutput("homo_heatmap_chart",
                            height = "auto", width = "100%"
                          ),
                          type = 4, color = "#2C3E50", size = 0.6
                        )
                      )
                    ),
                    tabPanel(
                      title = tagList(icon("chart-pie"), " Radar"),
                      value = "radar",
                      div(
                        class = "analytics-chart-wrap",
                        analytics_chart_toolbar("homo_radar_chart", "homo_radar.svg", "homo_radar_order", choices = analytics_radar_order_choices),
                        chart_info_tip(analytics_radar_tip_html),
                        shinycssloaders::withSpinner(
                          ggiraph::girafeOutput("homo_radar_chart",
                            height = "auto", width = "100%"
                          ),
                          type = 4, color = "#2C3E50", size = 0.6
                        )
                      )
                    ),
                    tabPanel(
                      title = tagList(icon("grip-lines"), " Correlations"),
                      value = "corr",
                      div(
                        class = "analytics-chart-wrap",
                        analytics_chart_toolbar(
                          "homo_corr_chart", "homo_correlations.svg", "homo_corr_order",
                          choices = analytics_corr_order_choices
                        ),
                        chart_info_tip(analytics_corr_tip_html),
                        shinycssloaders::withSpinner(
                          ggiraph::girafeOutput("homo_corr_chart",
                            height = "auto", width = "100%"
                          ),
                          type = 4, color = "#2C3E50", size = 0.6
                        )
                      )
                    )
                  )
                )
              )
            ),
            analytics_export_bank("homo"),
            # ──────────────────────────────────────────────────────
            div(
              class = "plots-zoom-wrap",
              div(
                id = "plot-container",
                style = "position: relative;",
                uiOutput("homo_special_cards_ui"),
                div(id = "homo-plot-cards-container"),
                uiOutput("homo_load_more_banner")
              )
            )
          )
        ),
        tabPanel(
          title = "Cross-Species Gene Search",
          value = "orthologous",
          div(
            class = "content-wrapper app-main-pane app-main-pane-search-results",
            div(
              id = "ortho_context_section",
              class = "summary-context-section",
              style = "display:none;",
              initial_summary_context_header("Cross-Species", "Gene: pending", "Compare across organisms"),
              uiOutput("ortho_context_header"),
              result_workspace_subheader("ortho")
            ),
            div(
              id = "ortho_summary_section",
              class = "summary-section ortho-summary-section",
              style = "display:none;",
              div(
                class = "result-workspace-view-intro",
                div(
                  class = "result-workspace-view-intro-copy",
                  span(class = "result-workspace-view-kicker", icon("table"), " Structured results"),
                  h2("Summary table"),
                  p("Search, sort and review the records behind the cross-species comparison.")
                ),
                tags$button(
                  type = "button",
                  class = "result-workspace-view-back",
                  `data-workspace-back` = "true",
                  `data-workspace-scope` = "ortho",
                  `aria-label` = "Back to Cross-Species visualization",
                  icon("arrow-left"),
                  span("Back to visualization")
                )
              ),
              div(
                id = "ortho_summary_body",
                class = "result-workspace-table-body",
                style = "display:none;",
                DT::dataTableOutput("ortho_summary_dt")
              )
            ),
            # ── Sección Analytics Orthologous ─────────────────────
            div(
              id = "ortho_analytics_section",
              class = "analytics-section",
              style = "display:none;",
              div(
                id = "ortho_analytics_body",
                class = "analytics-body",
                style = "display:none; margin-top:2px;",
                div(
                  class = "result-workspace-view-intro",
                  div(
                    class = "result-workspace-view-intro-copy",
                    span(class = "result-workspace-view-kicker", icon("chart-bar"), " Comparative metrics"),
                    h2("Analytics"),
                    p("Explore statistical views generated from the active cross-species result set.")
                  ),
                  tags$button(
                    type = "button",
                    class = "result-workspace-view-back",
                    `data-workspace-back` = "true",
                    `data-workspace-scope` = "ortho",
                    `aria-label` = "Back to Cross-Species visualization",
                    icon("arrow-left"),
                    span("Back to visualization")
                  )
                ),
                div(
                class = "analytics-tabs-shell",
                  tabsetPanel(
                    id = "ortho_analytics_tabs",
                    type = "pills",
                    tabPanel(
                      title = tagList(icon("dna"), " Architecture"),
                      value = "arch",
                      div(
                        class = "analytics-chart-wrap",
                        analytics_chart_toolbar("ortho_arch_chart", "ortho_architecture.svg", "ortho_arch_order", choices = analytics_arch_order_choices),
                        chart_info_tip("<strong>Gene Architecture</strong><br/>Stacked bar showing the base-pair composition of each gene: CDS (coding), UTR, and introns.<div class='ci-axes'><b>Y-axis:</b> Organism<br/><b>X-axis:</b> Base pairs (bp)</div><div class='ci-tip'>Longer bars = larger genes. Compare CDS proportion across the compared gene models.</div>"),
                        shinycssloaders::withSpinner(
                          ggiraph::girafeOutput("ortho_arch_chart", height = "auto", width = "100%"),
                          color = "#18BC9C", type = 6
                        )
                      )
                    ),
                    tabPanel(
                      title = tagList(icon("layer-group"), " Exons / Introns"),
                      value = "exon",
                      div(
                        class = "analytics-chart-wrap",
                        analytics_chart_toolbar("ortho_exon_chart", "ortho_exons_introns.svg", "ortho_exon_order", choices = analytics_exon_order_choices),
                        chart_info_tip("<strong>Exon &amp; Intron Comparison</strong><br/>Left panel: exon vs intron count. Right panel: exonic vs intronic base pairs.<div class='ci-axes'><b>Left Y-axis:</b> Count<br/><b>Right Y-axis:</b> Base pairs (bp)</div><div class='ci-tip'>Compare structural complexity &mdash; more introns often indicate regulatory complexity.</div>"),
                        shinycssloaders::withSpinner(
                          ggiraph::girafeOutput("ortho_exon_chart", height = "auto", width = "100%"),
                          color = "#18BC9C", type = 6
                        )
                      )
                    ),
                    tabPanel(
                      title = tagList(icon("microscope"), " Sequence"),
                      value = "seq",
                      div(
                        class = "analytics-chart-wrap",
                        analytics_chart_toolbar("ortho_seq_chart", "ortho_sequence_composition.svg", "ortho_seq_order", choices = analytics_seq_order_choices),
                        chart_info_tip("<strong>Nucleotide Composition</strong><br/>Percentage of each nucleotide (A, T, C, G) per gene. GC% annotated on the right.<div class='ci-axes'><b>Y-axis:</b> Organism<br/><b>X-axis:</b> Percentage (0&ndash;50%)</div><div class='ci-tip'>Higher GC% may indicate thermal stability or coding density.</div>"),
                        shinycssloaders::withSpinner(
                          ggiraph::girafeOutput("ortho_seq_chart", height = "auto", width = "100%"),
                          color = "#18BC9C", type = 6
                        )
                      )
                    ),
                    tabPanel(
                      title = tagList(icon("map-marker-alt"), " Genomic Context"),
                      value = "context",
                      div(
                        class = "analytics-chart-wrap",
                        analytics_chart_toolbar("ortho_context_chart", "ortho_genomic_context.svg", "ortho_context_order", choices = analytics_context_order_choices),
                        chart_info_tip("<strong>Genomic Context</strong><br/>Distance to the nearest neighboring gene in separate panels (Upstream and Downstream).<div class='ci-axes'><b>Y-axis:</b> Organism<br/><b>X-axis:</b> Signed edge-to-edge distance (bp)</div><div class='ci-tip'>Negative = overlap; zero/positive = gap. The panel defines the biological side (not the sign).</div>"),
                        shinycssloaders::withSpinner(
                          ggiraph::girafeOutput("ortho_context_chart", height = "auto", width = "100%"),
                          color = "#18BC9C", type = 6
                        )
                      )
                    ),
                    tabPanel(
                      title = tagList(icon("ruler-horizontal"), " Exon Lengths"),
                      value = "exon_dist",
                      div(
                        class = "analytics-chart-wrap",
                        analytics_chart_toolbar("ortho_exon_dist_chart", "ortho_exon_lengths.svg", "ortho_exon_dist_order", choices = analytics_exon_dist_order_choices),
                        chart_info_tip("<strong>Exon Length Distribution</strong><br/>Violin + box + jitter showing individual exon lengths per gene on a log&#8321;&#8320; scale.<div class='ci-axes'><b>Y-axis:</b> Organism<br/><b>X-axis:</b> Exon length in bp (log&#8321;&#8320;)</div><div class='ci-tip'>Box = IQR (25th&ndash;75th percentile). Line = median. Dots = individual exons.</div>"),
                        shinycssloaders::withSpinner(
                          ggiraph::girafeOutput("ortho_exon_dist_chart",
                            height = "auto", width = "100%"
                          ),
                          type = 4, color = "#2C3E50", size = 0.6
                        )
                      )
                    ),
                    tabPanel(
                      title = tagList(icon("arrows-left-right"), " Intron Lengths"),
                      value = "intron_dist",
                      div(
                        class = "analytics-chart-wrap",
                        analytics_chart_toolbar("ortho_intron_dist_chart", "ortho_intron_lengths.svg", "ortho_intron_dist_order", choices = analytics_intron_dist_order_choices),
                        chart_info_tip("<strong>Intron Length Distribution</strong><br/>Violin + box + jitter showing individual intron lengths inferred from gaps between consecutive annotated exons.<div class='ci-axes'><b>Y-axis:</b> Organism<br/><b>X-axis:</b> Intron length in bp (log&#8321;&#8320;)</div><div class='ci-tip'>Genes without introns are omitted from the distribution; if none have introns, a clear no-intron message is shown.</div>"),
                        shinycssloaders::withSpinner(
                          ggiraph::girafeOutput("ortho_intron_dist_chart",
                            height = "auto", width = "100%"
                          ),
                          type = 4, color = "#2C3E50", size = 0.6
                        )
                      )
                    ),
                    tabPanel(
                      title = tagList(icon("circle-dot"), " Scatter"),
                      value = "scatter",
                      div(
                        class = "analytics-chart-wrap",
                        analytics_chart_toolbar("ortho_scatter_chart", "ortho_scatter.svg", "ortho_scatter_order", choices = analytics_scatter_order_choices),
                        chart_info_tip(analytics_scatter_tip_html),
                        shinycssloaders::withSpinner(
                          ggiraph::girafeOutput("ortho_scatter_chart",
                            height = "auto", width = "100%"
                          ),
                          type = 4, color = "#2C3E50", size = 0.6
                        )
                      )
                    ),
                    tabPanel(
                      title = tagList(icon("table-cells"), " Heatmap"),
                      value = "heatmap",
                      div(
                        class = "analytics-chart-wrap",
                        analytics_chart_toolbar("ortho_heatmap_chart", "ortho_heatmap.svg", "ortho_heatmap_order", choices = analytics_heatmap_order_choices),
                        chart_info_tip(analytics_heatmap_tip_html),
                        shinycssloaders::withSpinner(
                          ggiraph::girafeOutput("ortho_heatmap_chart",
                            height = "auto", width = "100%"
                          ),
                          type = 4, color = "#2C3E50", size = 0.6
                        )
                      )
                    ),
                    tabPanel(
                      title = tagList(icon("chart-pie"), " Radar"),
                      value = "radar",
                      div(
                        class = "analytics-chart-wrap",
                        analytics_chart_toolbar("ortho_radar_chart", "ortho_radar.svg", "ortho_radar_order", choices = analytics_radar_order_choices),
                        chart_info_tip(analytics_radar_tip_html),
                        shinycssloaders::withSpinner(
                          ggiraph::girafeOutput("ortho_radar_chart",
                            height = "auto", width = "100%"
                          ),
                          type = 4, color = "#2C3E50", size = 0.6
                        )
                      )
                    ),
                    tabPanel(
                      title = tagList(icon("grip-lines"), " Correlations"),
                      value = "corr",
                      div(
                        class = "analytics-chart-wrap",
                        analytics_chart_toolbar(
                          "ortho_corr_chart", "ortho_correlations.svg", "ortho_corr_order",
                          choices = analytics_corr_order_choices
                        ),
                        chart_info_tip(analytics_corr_tip_html),
                        shinycssloaders::withSpinner(
                          ggiraph::girafeOutput("ortho_corr_chart",
                            height = "auto", width = "100%"
                          ),
                          type = 4, color = "#2C3E50", size = 0.6
                        )
                      )
                    )
                  )
                )
              )
            ),
            analytics_export_bank("ortho"),
            # ──────────────────────────────────────────────────────
            div(
              class = "plots-zoom-wrap",
              div(
                id = "ortho-plot-container",
                style = "position: relative;",
                uiOutput("ortho_special_cards_ui"),
                div(
                  id = "ortho-multipip-card-shell",
                  style = "display:none;",
                  div(
                    class = "card mb-3",
                    style = "width: 100%; display:flex; flex-direction:column; box-shadow: 0 4px 12px rgba(0,0,0,0.12); border-radius: 10px; overflow:hidden;",
                    div(
                      class = "card-header",
                      style = "background: linear-gradient(135deg, #1E3A52, #2A5469); color: white; padding: 12px 18px; font-weight: bold; display:flex; align-items:center; justify-content:space-between;",
                      div(
                        style = "display:flex; align-items:center; gap:10px;",
                        tags$i(class = "fa fa-stream", style = "font-size:16px;"),
                        span("MultiPIP-style View")
                      ),
                      div(
                        style = "display:flex; align-items:center; gap:8px;",
                        tags$button(
                          class = "btn btn-sm btn-export-svg",
                          title = "Export plot as SVG",
                          onclick = "exportAlignmentSVG('ortho_multipip_plot_out', 'multipip_style_view')",
                          style = "background:rgba(255,255,255,0.15); border:1px solid rgba(255,255,255,0.3); color:white; border-radius:6px; padding:4px 10px; font-size:12px;",
                          HTML("&#x2913; SVG")
                        ),
                        downloadButton("download_multipip_seq", "Download sequences",
                          class = "btn-download",
                          style = "background:rgba(255,255,255,0.15); border:1px solid rgba(255,255,255,0.3); color:white; border-radius:6px; padding:4px 10px; font-size:12px;"
                        )
                      )
                    ),
                    div(
                      class = "ortho-aligned-toolbar ortho-pip-toolbar ortho-multipip-toolbar",
                      style = "padding: 10px 16px 12px 16px; border-bottom: 1px solid #DDEAF0;",
                      div(
                        class = "ortho-aligned-toolbar-row ortho-pip-toolbar-row--primary",
                        uiOutput("ortho_multipip_reference_ui"),
                        div(
                          class = "ortho-aligned-control ortho-aligned-control--select",
                          tags$label("Alignment window:", `for` = "ortho_multipip_span", class = "ortho-aligned-control-label"),
                          shiny::selectInput(
                            inputId = "ortho_multipip_span",
                            label = NULL,
                            choices = c(
                              "Gene body only" = "gene",
                              "Gene ± 5 kb flanks" = "5kb",
                              "Gene ± 10 kb flanks" = "10kb",
                              "Gene ± 25 kb flanks" = "25kb",
                              "Gene ± 50 kb flanks" = "50kb"
                            ),
                            selected = "gene",
                            width = "100%"
                          )
                        ),
                        div(
                          class = "ortho-aligned-control ortho-aligned-control--select",
                          tags$label("Track order:", `for` = "ortho_multipip_track_order", class = "ortho-aligned-control-label"),
                          shiny::selectInput(
                            inputId = "ortho_multipip_track_order",
                            label = NULL,
                            choices = c(
                              "Loaded order" = "load",
                              "Organism A-Z" = "organism",
                              "Similarity to reference (planned)" = "similarity"
                            ),
                            selected = "load",
                            width = "100%"
                          )
                        ),
                        div(
                          class = "ortho-aligned-control ortho-aligned-control--select",
                          tags$label("Min. segment length:", `for` = "ortho_multipip_min_segment_bp", class = "ortho-aligned-control-label"),
                          shiny::selectInput(
                            inputId = "ortho_multipip_min_segment_bp",
                            label = NULL,
                            choices = c(
                              "10 bp" = "10",
                              "25 bp (recommended)" = "25",
                              "50 bp" = "50",
                              "100 bp" = "100"
                            ),
                            selected = "25",
                            width = "100%"
                          )
                        )
                      ),
                      div(
                        class = "ortho-pip-toolbar-info-row",
                        div(
                          class = "ortho-pip-toolbar-note-row ortho-pip-toolbar-note-row--panel",
                          uiOutput("ortho_multipip_reference_note_ui")
                        ),
                        div(
                          class = "ortho-pip-toolbar-help-card",
                          div(class = "ortho-aligned-mode-hint", uiOutput("ortho_multipip_help"))
                        )
                      ),
                      div(
                        class = "ortho-pip-toolbar-control-row",
                        div(
                          class = "ortho-aligned-control ortho-aligned-control--slider",
                          tags$label("Min. local identity:", `for` = "ortho_multipip_min_identity", class = "ortho-aligned-control-label"),
                          shiny::sliderInput(
                            inputId = "ortho_multipip_min_identity",
                            label = NULL,
                            min = 50, max = 100, value = 70, step = 5,
                            post = "%", width = "100%", ticks = FALSE
                          )
                        ),
                        div(
                          class = "ortho-pip-toolbar-actions-row ortho-pip-toolbar-actions-row--stacked",
                          div(
                            class = "ortho-pip-toolbar-actions-col",
                            div(class = "ortho-pip-toolbar-actions-label", "Local backend:"),
                            div(
                              class = "ortho-pip-toolbar-actions-buttons",
                              actionButton(
                                inputId = "ortho_multipip_run_alignments",
                                label = "Run local alignments",
                                icon = icon("play"),
                                class = "btn btn-sm btn-success",
                                style = "width:100%; height:38px; border-radius:10px; font-weight:700;"
                              )
                            )
                          )
                        )
                      )
                    ),
                    div(
                      style = "padding: 0 16px 8px 16px; border-bottom: 1px solid #DDEAF0;",
                      uiOutput("ortho_multipip_interpretation_help")
                    ),
                    div(
                      class = "card-body",
                      style = "background-color: #FAFBFC; overflow-x: auto; padding: 10px 15px;",
                      ggiraph::girafeOutput("ortho_multipip_plot_out", width = "100%", height = "auto")
                    ),
                    div(
                      class = "card-footer",
                      style = "background-color: #F2F4F6; padding: 10px 18px; border-top: 1px solid #DDE2E8; font-size: 12px; color: #4B6072;",
                      uiOutput("ortho_multipip_legend"),
                      uiOutput("ortho_multipip_footer")
                    )
                  )
                ),
                div(id = "ortho-plot-cards-container"),
                uiOutput("orthologous_plots_ui")
              )
            )
          )
        ),
        tabPanel(
          title = "Settings",
          value = "settings",
          div(
            class = "content-wrapper app-main-pane app-settings-pane",
            div(
              class = "help-content settings-content",
              h2(icon("cog"), span("Settings")),
              div(
                class = "help-section",
                h3(icon("paint-brush"), span("Appearance")),
                # ── Fila: Modo Día / Noche ──────────────────────────────
                div(
                  class = "settings-row",
                  div(
                    class = "settings-row-info",
                    tags$span(
                      class = "settings-row-title",
                      icon("sun"), "Day / Night Mode"
                    ),
                    tags$span(
                      class = "settings-row-desc",
                      "Switch between the light and dark theme."
                    )
                  ),
                  HTML('
                  <label class="switch" id="theme-toggle-btn" style="margin-bottom: 0; flex-shrink: 0;">
                    <span class="sun"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24"><g fill="#ffd43b"><circle r="5" cy="12" cx="12"></circle><path d="m21 13h-1a1 1 0 0 1 0-2h1a1 1 0 0 1 0 2zm-17 0h-1a1 1 0 0 1 0-2h1a1 1 0 0 1 0 2zm13.66-5.66a1 1 0 0 1 -.66-.29 1 1 0 0 1 0-1.41l.71-.71a1 1 0 1 1 1.41 1.41l-.71.71a1 1 0 0 1 -.75.29zm-12.02 12.02a1 1 0 0 1 -.71-.29 1 1 0 0 1 0-1.41l.71-.66a1 1 0 0 1 1.41 1.41l-.71.71a1 1 0 0 1 -.7.24zm6.36-14.36a1 1 0 0 1 -1-1v-1a1 1 0 0 1 2 0v1a1 1 0 0 1 -1 1zm0 17a1 1 0 0 1 -1-1v-1a1 1 0 0 1 2 0v1a1 1 0 0 1 -1 1zm-5.66-14.66a1 1 0 0 1 -.7-.29l-.71-.71a1 1 0 0 1 1.41-1.41l.71.71a1 1 0 0 1 0 1.41 1 1 0 0 1 -.71.29zm12.02 12.02a1 1 0 0 1 -.7-.29l-.66-.71a1 1 0 0 1 1.36-1.36l.71.71a1 1 0 0 1 0 1.41 1 1 0 0 1 -.71.24z"></path></g></svg></span>
                    <span class="moon"><svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 384 512"><path d="m223.5 32c-123.5 0-223.5 100.3-223.5 224s100 224 223.5 224c60.6 0 115.5-24.2 155.8-63.4 5-4.9 6.3-12.5 3.1-18.7s-10.1-9.7-17-8.5c-9.8 1.7-19.8 2.6-30.1 2.6-96.9 0-175.5-78.8-175.5-176 0-65.8 36-123.1 89.3-153.3 6.1-3.5 9.2-10.5 7.7-17.3s-7.3-11.9-14.3-12.5c-6.3-.5-12.6-.8-19-.8z"></path></svg></span>
                    <input type="checkbox" class="input" id="theme-toggle-input">
                    <span class="slider"></span>
                  </label>
                  ')
                ),
                # ── Fila: Colorblind Palette ────────────────────────────
                div(
                  class = "settings-row",
                  div(
                    class = "settings-row-info",
                    tags$span(
                      class = "settings-row-title",
                      icon("eye"), "Colorblind Palette"
                    ),
                    tags$span(
                      class = "settings-row-desc",
                      "Enables an accessible color scheme for color vision deficiency."
                    )
                  ),
                  shinyWidgets::materialSwitch(
                    inputId = "colorblind_mode",
                    label = NULL,
                    value = FALSE,
                    status = "primary"
                  )
                ),
                # ── Color Palette Customization ────────────────────────────
                div(
                  class = "color-palette-section",
                  div(
                    class = "palette-group",
                    style = "display: none;",
                    h4(icon("palette"), span("Transcript Features")),
                    p("Customize colors for gene model elements."),
                    div(
                      class = "palette-row",
                      div(class = "palette-item",
                          tags$label(`for` = "color_exon", "Exon"),
                          tags$input(type = "color", id = "color_exon", class = "color-input", value = "#F45D75")),
                      div(class = "palette-item",
                          tags$label(`for` = "color_cds", "CDS"),
                          tags$input(type = "color", id = "color_cds", class = "color-input", value = "#E8A44F")),
                      div(class = "palette-item",
                          tags$label(`for` = "color_utr", "UTR"),
                          tags$input(type = "color", id = "color_utr", class = "color-input", value = "#5BC0EB")),
                      div(class = "palette-item",
                          tags$label(`for` = "color_gene", "Gene"),
                          tags$input(type = "color", id = "color_gene", class = "color-input", value = "#FFB7BF"))
                    )
                  ),
                  div(
                    class = "palette-group",
                    style = "display: none;",
                    h4(icon("layer-group"), span("LASTZ Identity")),
                    p("Colors for alignment fragments by identity percentage."),
                    div(
                      class = "palette-row",
                      div(class = "palette-item",
                          tags$label(`for` = "color_identity_high", "High (≥80%)"),
                          tags$input(type = "color", id = "color_identity_high", class = "color-input", value = "#CC2929")),
                      div(class = "palette-item",
                          tags$label(`for` = "color_identity_mid", "Medium (50-79%)"),
                          tags$input(type = "color", id = "color_identity_mid", class = "color-input", value = "#E07858")),
                      div(class = "palette-item",
                          tags$label(`for` = "color_identity_low", "Low (<50%)"),
                          tags$input(type = "color", id = "color_identity_low", class = "color-input", value = "#5CB85C"))
                    )
                  ),
                  div(
                    class = "palette-group",
                    style = "display: none;",
                    h4(icon("square"), span("Card Headers")),
                    p("Background gradient colors for gene and transcript headers."),
                    div(
                      class = "palette-row",
                      div(class = "palette-item",
                          tags$label(`for` = "color_header_gene", "Gene header"),
                          tags$input(type = "color", id = "color_header_gene", class = "color-input", value = "#2C3E50")),
                      div(class = "palette-item",
                          tags$label(`for` = "color_header_transcript", "Transcript header"),
                          tags$input(type = "color", id = "color_header_transcript", class = "color-input", value = "#24445B"))
                    )
                  ),
                  div(
                    class = "palette-reset-row",
                    style = "display: none;",
                    actionButton("reset_color_palette", label = span("Reset to defaults"), icon = icon("rotate-left"), class = "btn btn-sm btn-reset-palette")
                  )
                )
              ),
              div(
                class = "help-section",
                h3(icon("sliders"), span("Interface")),
                # ── Fila: Confirm before delete ─────────────────────────
                div(
                  class = "settings-row",
                  div(
                    class = "settings-row-info",
                    tags$span(
                      class = "settings-row-title",
                      icon("trash-can"), "Confirm before deleting"
                    ),
                    tags$span(
                      class = "settings-row-desc",
                      "Show a confirmation dialog before removing plots or sessions."
                    )
                  ),
                  shinyWidgets::materialSwitch(
                    inputId = "confirm_delete_actions",
                    label = NULL,
                    value = TRUE,
                    status = "primary"
                  )
                ),
                div(
                  class = "settings-row",
                  div(
                    class = "settings-row-info",
                    tags$span(
                      class = "settings-row-title",
                      icon("location-arrow"), "Quick navigation button"
                    ),
                    tags$span(
                      class = "settings-row-desc",
                      "Show the floating quick-actions button for search, display mode, and plot zoom."
                    )
                  ),
                  shinyWidgets::materialSwitch(
                    inputId = "quick_fab_enabled",
                    label = NULL,
                    value = FALSE,
                    status = "primary"
                  )
                )
              ),
              div(
                class = "help-section",
                h3(icon("database"), span("External alias lookup")),
                p("Choose which external databases are used when a gene is not found in the local annotation."),
                div(
                  class = "db-source-grid",
                  div(
                    class = "db-source-item db-source-item-mygene",
                    tags$input(
                      id = "ext_alias_source_mygene",
                      type = "checkbox",
                      class = "db-source-input",
                      checked = "checked"
                    ),
                    tags$label(
                      `for` = "ext_alias_source_mygene",
                      class = "db-source-card",
                      div(
                        class = "db-source-card-icon-wrap",
                        tags$img(
                          src = "icons/databases/mygene.ico",
                          alt = "MyGene",
                          class = "db-source-card-icon",
                          loading = "lazy",
                          onerror = "this.onerror=null;this.src='icons/DNA.ico';"
                        )
                      ),
                      div(
                        class = "db-source-card-text",
                        span(class = "db-source-card-title", "MyGene")
                      ),
                      span(class = "db-source-card-indicator"),
                      span(class = "db-source-card-bar")
                    )
                  ),
                  div(
                    class = "db-source-item db-source-item-ncbi",
                    tags$input(
                      id = "ext_alias_source_ncbi",
                      type = "checkbox",
                      class = "db-source-input",
                      checked = "checked"
                    ),
                    tags$label(
                      `for` = "ext_alias_source_ncbi",
                      class = "db-source-card",
                      div(
                        class = "db-source-card-icon-wrap",
                        tags$img(
                          src = "icons/databases/ncbi.ico",
                          alt = "NCBI Gene",
                          class = "db-source-card-icon",
                          loading = "lazy",
                          onerror = "this.onerror=null;this.src='icons/DNA.ico';"
                        )
                      ),
                      div(
                        class = "db-source-card-text",
                        span(class = "db-source-card-title", "NCBI Gene")
                      ),
                      span(class = "db-source-card-indicator"),
                      span(class = "db-source-card-bar")
                    )
                  ),
                  div(
                    class = "db-source-item db-source-item-uniprot",
                    tags$input(
                      id = "ext_alias_source_uniprot",
                      type = "checkbox",
                      class = "db-source-input",
                      checked = "checked"
                    ),
                    tags$label(
                      `for` = "ext_alias_source_uniprot",
                      class = "db-source-card",
                      div(
                        class = "db-source-card-icon-wrap",
                        tags$img(
                          src = "icons/databases/uniprot.ico",
                          alt = "UniProt",
                          class = "db-source-card-icon",
                          loading = "lazy",
                          onerror = "this.onerror=null;this.src='icons/DNA.ico';"
                        )
                      ),
                      div(
                        class = "db-source-card-text",
                        span(class = "db-source-card-title", "UniProt")
                      ),
                      span(class = "db-source-card-indicator"),
                      span(class = "db-source-card-bar")
                    )
                  ),
                  div(
                    class = "db-source-item db-source-item-ensembl",
                    tags$input(
                      id = "ext_alias_source_ensembl",
                      type = "checkbox",
                      class = "db-source-input",
                      checked = "checked"
                    ),
                    tags$label(
                      `for` = "ext_alias_source_ensembl",
                      class = "db-source-card",
                      div(
                        class = "db-source-card-icon-wrap",
                        tags$img(
                          src = "icons/databases/ensembl.ico",
                          alt = "Ensembl",
                          class = "db-source-card-icon",
                          loading = "lazy",
                          onerror = "this.onerror=null;this.src='icons/DNA.ico';"
                        )
                      ),
                      div(
                        class = "db-source-card-text",
                        span(class = "db-source-card-title", "Ensembl")
                      ),
                      span(class = "db-source-card-indicator"),
                      span(class = "db-source-card-bar")
                    )
                  )
                )
              ),
              div(
                class = "help-section desktop-organisms-section",
                h3(icon("download"), span("Organisms")),
                p("Install reference organisms into this desktop profile. The base installer ships without organisms. The catalog can provide up to 25 installable organisms, depending on availability and what you choose to download. Downloaded datasets appear in the Preloaded organism selectors after installation."),
                div(
                  class = "desktop-organism-toolbar",
                  div(
                    class = "desktop-organism-summary-stats",
                    span(id = "desktop-organism-count", class = "desktop-organism-count", "Checking catalog..."),
                    span(id = "desktop-organism-installed-count", class = "desktop-organism-count", ""),
                    span(id = "desktop-organism-pending-count", class = "desktop-organism-count", "")
                  ),
                  div(
                    class = "desktop-organism-toolbar-actions",
                    tags$button(
                      id = "desktop-organism-open-catalog",
                      type = "button",
                      class = "btn btn-sm btn-download",
                      icon("table-cells-large"),
                      span("Open catalog")
                    ),
                    tags$button(
                      id = "desktop-organism-reset",
                      type = "button",
                      class = "btn btn-sm desktop-organism-reset",
                      icon("trash"),
                      span("Remove organisms")
                    ),
                    tags$button(
                      id = "desktop-organism-refresh",
                      type = "button",
                      class = "btn btn-sm btn-download",
                      icon("rotate-right"),
                      span("Refresh")
                    )
                  )
                ),
                div(
                  class = "desktop-organism-paths",
                  div(tags$span("Data"), tags$code(id = "desktop-organism-data-path", "")),
                  div(tags$span("Cache"), tags$code(id = "desktop-organism-cache-path", ""))
                ),
                div(id = "desktop-organism-status", class = "desktop-organism-status", ""),
                div(id = "desktop-organism-list", class = "desktop-organism-list desktop-organism-summary-list"),
                div(
                  id = "desktop-organism-modal",
                  class = "desktop-organism-modal",
                  `aria-hidden` = "true",
                  div(class = "desktop-organism-modal-backdrop"),
                  div(
                    class = "desktop-organism-modal-panel",
                    role = "dialog",
                    `aria-modal` = "true",
                    `aria-labelledby` = "desktop-organism-modal-title",
                    div(
                      class = "desktop-organism-modal-header",
                      div(
                        h4(id = "desktop-organism-modal-title", icon("download"), span("Organism catalog")),
                        span(id = "desktop-organism-modal-count", class = "desktop-organism-count", "")
                      ),
                      tags$button(
                        id = "desktop-organism-modal-close",
                        type = "button",
                        class = "btn btn-sm desktop-organism-icon-button",
                        `aria-label` = "Close catalog",
                        icon("xmark")
                      )
                    ),
                    div(
                      class = "desktop-organism-modal-controls",
                      tags$input(
                        id = "desktop-organism-search",
                        type = "search",
                        class = "desktop-organism-search",
                        placeholder = "Search organisms"
                      ),
                      tags$select(
                        id = "desktop-organism-filter",
                        class = "desktop-organism-filter",
                        tags$option(value = "all", "All"),
                        tags$option(value = "available", "Available"),
                        tags$option(value = "not_installed", "Not installed"),
                        tags$option(value = "installed", "Installed"),
                        tags$option(value = "updates", "Updates")
                      )
                    ),
                    div(id = "desktop-organism-modal-list", class = "desktop-organism-list desktop-organism-modal-list")
                  )
                ),
                div(
                  id = "desktop-organism-remove-modal",
                  class = "desktop-organism-modal desktop-organism-remove-modal",
                  `aria-hidden` = "true",
                  div(class = "desktop-organism-modal-backdrop"),
                  div(
                    class = "desktop-organism-modal-panel desktop-organism-remove-panel",
                    role = "dialog",
                    `aria-modal` = "true",
                    `aria-labelledby` = "desktop-organism-remove-title",
                    div(
                      class = "desktop-organism-modal-header",
                      div(
                        h4(id = "desktop-organism-remove-title", icon("trash"), span("Remove installed organisms")),
                        span(
                          id = "desktop-organism-remove-count",
                          class = "desktop-organism-count",
                          "0 selected"
                        )
                      ),
                      tags$button(
                        id = "desktop-organism-remove-close",
                        type = "button",
                        class = "btn btn-sm desktop-organism-icon-button",
                        `aria-label` = "Close removal dialog",
                        icon("xmark")
                      )
                    ),
                    div(
                      class = "desktop-organism-removal-intro",
                      p("Choose one, several, or all downloaded organisms. Only the selected organisms and their local dataset files and caches will be removed."),
                      tags$label(
                        class = "desktop-organism-select-all",
                        tags$input(id = "desktop-organism-remove-all", type = "checkbox"),
                        span("Select all installed organisms")
                      )
                    ),
                    div(
                      id = "desktop-organism-remove-list",
                      class = "desktop-organism-removal-list"
                    ),
                    div(
                      class = "desktop-organism-removal-footer",
                      tags$button(
                        id = "desktop-organism-remove-cancel",
                        type = "button",
                        class = "btn btn-sm desktop-organism-icon-button desktop-organism-cancel-button",
                        span("Cancel")
                      ),
                      tags$button(
                        id = "desktop-organism-remove-selected",
                        type = "button",
                        class = "btn btn-sm desktop-organism-remove-selected",
                        disabled = "disabled",
                        icon("trash"),
                        span("Remove selected")
                      )
                    )
                  )
                )
              ),
              div(
                class = "help-section",
                h3(icon("save"), span("Work sessions")),
                p("Download your current work session (plots and settings) as an .rds file, or upload a previously saved session file to restore it."),
                div(
                  class = "settings-session-controls settings-session-import",
                  downloadButton("download_work_session", span("Export current session (.rds)"), class = "btn-sm btn-download"),
                  hr(style = "margin: 14px 0 10px 0;border-top:1px dashed #ccc;"),
                  fileInput(
                    inputId = "upload_work_session",
                    label = "Restore from session file (.rds)",
                    accept = c(".rds")
                  ),
                  actionButton(
                    inputId = "load_work_session_btn",
                    label = span("Restore session"),
                    icon = icon("folder-open"),
                    class = "btn btn-sm btn-download settings-session-btn"
                  )
                ),
                p(class = "app-submenu-hint", "Loading a session replaces current visualizations.")
              ),
              div(
                class = "help-section",
                h3(icon("share-nodes"), span("Shared analysis reports")),
                p("Secret read-only reports created in this browser appear below. They can be copied or revoked without a CGeV account."),
                div(id = "cgv-shared-report-list", class = "cgv-shared-report-list"),
                p(
                  class = "app-submenu-hint",
                  "This list is stored only in this browser. Reports still expire automatically if the browser list is cleared."
                )
              )
            )
          )
        ),
        tabPanel(
          title = "Feedback",
          value = "feedback",
          div(
            class = "content-wrapper app-main-pane app-settings-pane",
            div(
              class = "help-content settings-content feedback-shell",

              # ── Inline CSS for new animated elements ──────────────────────

              # ── Hero ──────────────────────────────────────────────────────
              div(
                class = "feedback-hero home-reveal",
                div(
                  class = "feedback-hero-copy",
                  div(class = "feedback-kicker", icon("comment-dots"), span("Community Feedback")),
                  h2("Feedback & Support"),
                  p("Help us improve CGeV. Report bugs, suggest features, or flag rough edges in the workflow."),
                  div(
                    class = "feedback-pill-row home-stagger-parent",
                    span(class = "feedback-pill home-stagger-child", icon("flask"), span("Scientific workflows")),
                    span(class = "feedback-pill home-stagger-child", icon("shield-alt"), span("Anti-spam protected")),
                    span(class = "feedback-pill home-stagger-child", icon("envelope"), span("Replies to your email"))
                  )
                )
              ),

              div(
                class = "feedback-manual-prompt home-reveal",
                div(class = "feedback-manual-prompt-icon", icon("book-open")),
                div(
                  class = "feedback-manual-prompt-copy",
                  span("Documentation"),
                  h3("Check the CGeV User Manual"),
                  p("Search the complete Web and Desktop reference for workflows, interpretation guidance, exports, and troubleshooting.")
                ),
                tags$a(
                  href = cgv_manual_path,
                  target = "_blank",
                  rel = "noopener noreferrer",
                  class = "feedback-manual-prompt-action",
                  `aria-label` = "Open the latest CGeV User Manual",
                  icon("arrow-up-right-from-square"),
                  span("Open manual")
                )
              ),

              # ── Info strip ─────────────────────────────────────────────────
              div(
                class = "feedback-info-strip home-stagger-parent",

                # Card 1 — What we capture
                div(
                  class = "feedback-info-card home-stagger-child",
                  div(class = "feedback-info-card-icon", icon("circle-info")),
                  h4("What we capture automatically"),
                  tags$ul(
                    tags$li("The CGeV section active when you submitted."),
                    tags$li("Submission timestamp and page context."),
                    tags$li("Your contact details for follow-up.")
                  )
                ),

                # Card 2 — What happens next
                div(
                  class = "feedback-info-card home-stagger-child",
                  div(class = "feedback-info-card-icon", icon("route")),
                  h4("What happens after you send it?"),
                  tags$ul(
                    class = "feedback-step-list",
                    tags$li(span(class = "feedback-step-num", "1"), "Your report is delivered to the CGeV inbox."),
                    tags$li(span(class = "feedback-step-num", "2"), "Bug reports blocking analysis get reviewed first."),
                    tags$li(span(class = "feedback-step-num", "3"), "Suggestions shape community-facing improvements.")
                  )
                ),

                # Card 3 — Tips
                div(
                  class = "feedback-info-card home-stagger-child",
                  div(class = "feedback-info-card-icon", icon("wand-magic-sparkles")),
                  h4("Tips for a useful report"),
                  tags$ul(
                    tags$li("Describe what you were trying to do, not only what broke."),
                    tags$li("For bugs: organism, gene, view mode, and browser help a lot."),
                    tags$li("For suggestions: explain how the change fits your workflow.")
                  )
                )
              ),

              # ── Form (bottom, most important) ──────────────────────────────
              div(
                class = "feedback-form-section home-reveal",

                # Header
                div(
                  class = "feedback-form-section-header",
                  div(class = "feedback-form-section-header-icon", icon("paper-plane")),
                  div(
                    h3("Send Feedback"),
                    p("Fields marked * are required. They help us reproduce issues faster.")
                  )
                ),

                # Body
                div(
                  class = "feedback-form-body",

                  # Type selector
                  div(
                    class = "help-section feedback-type-section",
                    h3(icon("clipboard-list"), span("What would you like to do?")),
                    div(
                      class = "viz-mode-wrap feedback-type-switch-wrap",
                      htmltools::tagAppendAttributes(
                        radioButtons(
                          "feedback_type",
                          NULL,
                          choices = c("Report a Bug" = "bug", "Suggest a Feature" = "suggestion"),
                          selected = "bug",
                          inline = TRUE
                        ),
                        class = "viz-mode-toggle feedback-type-switch"
                      )
                    )
                  ),

                  # Contact info (compact 2-col grid)
                  div(
                    class = "help-section feedback-contact-card",
                    h3(icon("id-card"), span("Your information")),
                    p(class = "feedback-field-note", "Use the email where you'd like to receive our reply."),
                    div(
                      class = "feedback-contact-grid",
                      textInput("feedback_name", HTML('Full Name <span class="field-required">*</span>'), placeholder = "Your full name", width = "100%"),
                      textInput("feedback_email", HTML('Email Address <span class="field-required">*</span>'), placeholder = "name@example.org", width = "100%")
                    ),
                    div(
                      class = "feedback-honeypot",
                      textInput("feedback_website", "Leave this field empty", width = "100%")
                    )
                  ),

                  # Bug form (conditional — ID and condition UNCHANGED)
                  conditionalPanel(
                    condition = "input.feedback_type == 'bug'",
                    div(
                      class = "help-section feedback-detail-card",
                      h3(icon("bug"), span("Bug report")),
                      textInput("bug_title", HTML('Short Title <span class="field-required">*</span>'), placeholder = "Example: Comparative plot gets cut off after export", width = "100%"),
                      textAreaInput("bug_steps", HTML('What happened? Include steps to reproduce <span class="field-required">*</span>'), placeholder = "1. Opened Cross-Species Gene Search...\n2. Generated the aligned view...\n3. Exported SVG...\n4. The right side of the plot was missing.", width = "100%", rows = 7),
                      textAreaInput("bug_expected", "What did you expect to happen? (Optional)", placeholder = "Example: The full plot should export without clipping.", width = "100%", rows = 3)
                    )
                  ),

                  # Suggestion form (conditional — ID and condition UNCHANGED)
                  conditionalPanel(
                    condition = "input.feedback_type == 'suggestion'",
                    div(
                      class = "help-section feedback-detail-card",
                      h3(icon("lightbulb"), span("Feature suggestion")),
                      textInput("sugg_title", HTML('Short Title <span class="field-required">*</span>'), placeholder = "Example: Add PDF export for publication-ready figures", width = "100%"),
                      textAreaInput("sugg_problem", HTML('What is missing or difficult today? <span class="field-required">*</span>'), placeholder = "Describe the gap in the current workflow.", width = "100%", rows = 4),
                      textAreaInput("sugg_idea", "What would you like CGeV to do? (Optional)", placeholder = "Explain the feature or improvement you would like to see.", width = "100%", rows = 4)
                    )
                  ),

                  # Submit row (UNCHANGED)
                  div(
                    class = "feedback-submit-row",
                    div(
                      class = "feedback-submit-meta",
                      p(
                        class = "feedback-submit-note",
                        "Your email address is used only for this report and any follow-up."
                      )
                    ),
                    actionButton("submit_feedback_btn", "Send Feedback", icon = icon("paper-plane"), class = "btn-primary feedback-submit-btn")
                  )
                )
              )
            )
          )
        )
      ),
      div(
        id = "app-quick-fab",
        class = "app-quick-fab",
        tags$div(
          class = "app-quick-fab-actions",
          tags$button(
            id = "app-fab-search-toggle",
            type = "button",
            class = "app-fab-action-btn",
            `data-label` = "Search gene",
            `aria-label` = "Search gene",
            `aria-expanded` = "false",
            icon("search")
          ),
          tags$button(
            id = "app-fab-mode-toggle",
            type = "button",
            class = "app-fab-action-btn",
            `data-label` = "Display mode",
            `aria-label` = "Display mode",
            `aria-expanded` = "false",
            icon("sliders")
          ),
          tags$button(
            id = "app-fab-zoom-toggle",
            type = "button",
            class = "app-fab-action-btn",
            `data-label` = "Plot zoom",
            `aria-label` = "Plot zoom",
            `aria-expanded` = "false",
            icon("search-plus")
          )
        ),
        div(
          id = "app-fab-search-panel",
          class = "app-fab-panel app-fab-search-panel",
          div(class = "app-fab-panel-title", icon("search"), span("Search gene")),
          tags$input(
            id = "app-fab-search-query",
            type = "text",
            class = "app-fab-search-input",
            placeholder = "Enter gene name (add to batch)",
            autocomplete = "off",
            autocorrect = "off",
            autocapitalize = "off",
            spellcheck = "false"
          ),
          div(
            class = "app-fab-chip-preview",
            div(
              id = "global-search-chip-list-fab",
              class = "app-search-chip-list app-search-chip-list-sync app-search-chip-list-compact",
              `data-empty-label` = "No batch genes.",
              span(class = "app-search-chip-empty", "No batch genes.")
            )
          ),
          tags$button(
            id = "app-fab-search-run",
            type = "button",
            class = "app-fab-panel-btn",
            icon("search"),
            span("Generate visualization")
          )
        ),
        div(
          id = "app-fab-mode-panel",
          class = "app-fab-panel app-fab-mode-panel",
          div(class = "app-fab-panel-title", icon("sliders"), span("Display mode")),
          div(
            class = "app-fab-mode-group app-fab-visualization-mode-group",
            div(class = "app-fab-mode-group-label", "Visualization mode"),
            div(
              class = "app-fab-mode-options",
              tags$button(type = "button", class = "app-fab-mode-option", `data-mode` = "compact", `aria-pressed` = "false", "Compact"),
              tags$button(type = "button", class = "app-fab-mode-option", `data-mode` = "detailed", `aria-pressed` = "false", "Detailed")
            )
          ),
          div(
            id = "app-fab-alignment-mode-group",
            class = "app-fab-mode-group app-fab-alignment-mode-group",
            div(class = "app-fab-mode-group-label", "Alignment mode"),
            div(
              class = "app-fab-mode-options",
              tags$button(type = "button", class = "app-fab-mode-option", `data-mode` = "aligned", `aria-pressed` = "false", "Aligned"),
              tags$button(type = "button", class = "app-fab-mode-option", `data-mode` = "pip_blocks", `aria-pressed` = "false", "LASTZ Blocks"),
              tags$button(type = "button", class = "app-fab-mode-option", `data-mode` = "pip_multipip", `aria-pressed` = "false", "MultiPIP")
            )
          ),
          div(id = "app-fab-mode-context", class = "app-fab-mode-context", "Multi-Gene mode")
        ),
        div(
          id = "app-fab-zoom-panel",
          class = "app-fab-panel app-fab-zoom-panel",
          div(class = "app-fab-panel-title", icon("search-plus"), span("Plot zoom")),
          div(
            class = "app-fab-zoom-control",
            tags$button(
              id = "fab-zoom-in",
              type = "button",
              class = "app-fab-zoom-btn",
              `data-zoom-action` = "in",
              `data-zoom-mode` = "homo",
              title = "Zoom in",
              "\u002b"
            ),
            span(
              id = "fab-zoom-label",
              class = "app-fab-zoom-label",
              "1\u00d7"
            ),
            tags$button(
              id = "fab-zoom-out",
              type = "button",
              class = "app-fab-zoom-btn",
              `data-zoom-action` = "out",
              `data-zoom-mode` = "homo",
              title = "Zoom out",
              disabled = NA,
              "−"
            )
          )
        ),
        tags$button(
          id = "app-fab-main-toggle",
          type = "button",
          class = "app-fab-main-btn",
          `aria-label` = "Quick actions",
          `aria-expanded` = "false",
          icon("plus", class = "app-fab-main-icon")
        )
      ),
      tags$button(
        id = "app-floating-top",
        type = "button",
        class = "app-floating-top-btn",
        `aria-label` = "Scroll to top",
        `aria-hidden` = "true",
        icon("chevron-up")
      )
    )
  )
)
