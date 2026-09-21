#!/usr/bin/env Rscript

script_file <- tryCatch(sys.frame(1)$ofile, error = function(e) "")
if (!nzchar(as.character(script_file))) {
    file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
    script_file <- if (length(file_arg) > 0L) sub("^--file=", "", file_arg[[1]]) else file.path("scripts", "test_string_network_roles.R")
}
root <- normalizePath(file.path(dirname(script_file), ".."), winslash = "/", mustWork = FALSE)
if (!file.exists(file.path(root, "R", "string_worker.R"))) {
    root <- normalizePath(".", winslash = "/", mustWork = FALSE)
}
setwd(root)

source(file.path("R", "utils.R"))
source(file.path("R", "string_cache.R"))
source(file.path("R", "string_worker.R"))

string_api_request <- function(path, query = list(), has_header = TRUE, seconds = 20) {
    if (identical(path, "get_string_ids")) {
        identifiers <- unlist(strsplit(as.character(query$identifiers %||% ""), "\r", fixed = TRUE))
        rows <- list()
        for (i in seq_along(identifiers)) {
            key <- tolower(trimws(identifiers[i]))
            hit <- switch(key,
                target = c("9606.TARGET", "TARGET"),
                plta = c("9606.PLTA", "PLTA"),
                other_alias = c("9606.OTHER", "OTHER"),
                NULL
            )
            if (is.null(hit)) next
            rows[[length(rows) + 1L]] <- data.frame(
                queryIndex = i,
                stringId = hit[[1]],
                ncbiTaxonId = as.character(query$species %||% "9606"),
                taxonName = "Homo sapiens",
                preferredName = hit[[2]],
                annotation = "",
                stringsAsFactors = FALSE
            )
        }
        if (length(rows) == 0L) {
            return(data.frame(stringsAsFactors = FALSE))
        }
        return(do.call(rbind, rows))
    }

    if (identical(path, "network")) {
        return(data.frame(
            stringId_A = c("9606.TARGET", "9606.TARGET"),
            stringId_B = c("9606.PLTA", "9606.OTHER"),
            preferredName_A = c("TARGET", "TARGET"),
            preferredName_B = c("PLTA", "OTHER"),
            score = c(910, 820),
            stringsAsFactors = FALSE
        ))
    }

    stop("unexpected STRING path: ", path)
}

tmp <- tempfile("string-role-cache-")
dir.create(tmp, recursive = TRUE)
on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

snapshot_a <- list(
    taxid = 9606L,
    id_candidates = c("target"),
    screen_variants = c("plta"),
    required_score = 600L,
    add_nodes = 8L
)
res_a <- string_resolve_and_fetch(snapshot_a, base_dir = tmp)
stopifnot(isTRUE(res_a$ok))
roles_a <- setNames(res_a$payload$nodes$role, res_a$payload$nodes$id)
stopifnot(identical(roles_a[["9606.TARGET"]], "target"))
stopifnot(identical(roles_a[["9606.PLTA"]], "plotted"))
stopifnot(identical(roles_a[["9606.OTHER"]], "neighbor"))

# Compare the complete payload (nodes, labels, roles, preferred names and IDs),
# ignoring only the timestamp written on each role application.
without_role_time <- function(payload) {
    payload$role_applied_at <- NULL
    payload
}
stopifnot(identical(
    without_role_time(string_apply_display_roles(res_a$payload, snapshot_a, tmp, FALSE)),
    without_role_time(string_apply_display_roles(res_a$payload, snapshot_a, tmp, TRUE))
))

# The pending-promise key excludes screen_variants. A second callback can join
# the first worker with an alias that worker never resolved. This is the PR1
# stop-condition counterexample: keep resolution enabled in that callback.
joined_snapshot <- snapshot_a
joined_snapshot$screen_variants <- "other_alias"
joined_without_io <- string_apply_display_roles(res_a$payload, joined_snapshot, tmp, FALSE)
joined_with_io <- string_apply_display_roles(res_a$payload, joined_snapshot, tmp, TRUE)
stopifnot(identical(joined_without_io$nodes$id, joined_with_io$nodes$id),
          identical(joined_without_io$nodes$label, joined_with_io$nodes$label),
          identical(joined_without_io$preferred_name, joined_with_io$preferred_name))
role_for <- function(payload, id) payload$nodes$role[match(id, payload$nodes$id)]
stopifnot(identical(role_for(joined_without_io, "9606.OTHER"), "neighbor"),
          identical(role_for(joined_with_io, "9606.OTHER"), "plotted"))
cat("resolve_missing=FALSE counterexample: joined callback changes OTHER role plotted -> neighbor\n")

snapshot_b <- snapshot_a
snapshot_b$screen_variants <- c("other_alias")
res_b <- string_resolve_and_fetch(snapshot_b, base_dir = tmp)
stopifnot(isTRUE(res_b$ok))
stopifnot(isTRUE(res_b$cache_hit))
roles_b <- setNames(res_b$payload$nodes$role, res_b$payload$nodes$id)
stopifnot(identical(roles_b[["9606.TARGET"]], "target"))
stopifnot(identical(roles_b[["9606.PLTA"]], "neighbor"))
stopifnot(identical(roles_b[["9606.OTHER"]], "plotted"))

snapshot_c <- snapshot_a
snapshot_c$screen_variants <- c("target", "plta")
res_c <- string_resolve_and_fetch(snapshot_c, base_dir = tmp)
roles_c <- setNames(res_c$payload$nodes$role, res_c$payload$nodes$id)
stopifnot(identical(roles_c[["9606.TARGET"]], "target"))

same_taxid <- string_collect_same_taxid_candidates(
    list(
        list(taxid = 9606L, candidates = c("A")),
        list(taxid = 10090L, candidates = c("B"))
    ),
    9606L
)
stopifnot(identical(same_taxid, "A"))

cat("string-network-roles-ok\n")
