#!/usr/bin/env Rscript
# Real httr2 request/error handling with in-process responses; never use STRING.
source("R/utils.R")
source("R/string_cache.R")
source("R/string_worker.R")
`%>%` <- magrittr::`%>%`

local({
    tmp <- tempfile("string-http-")
    dir.create(tmp)
    on.exit(unlink(tmp, recursive = TRUE))
    response <- function(status, body = "") httr2::response(
        status_code = status, body = charToRaw(body),
        headers = list(`content-type` = "text/tab-separated-values")
    )
    expect_error_class <- function(expr, expected) {
        caught <- tryCatch(force(expr), error = identity)
        stopifnot(inherits(caught, expected))
    }
    httr2::with_mocked_responses(list(response(200,
        "0\t9606.TARGET\t9606\tHomo sapiens\tTARGET\tannotation\n")), {
        mapped <- string_resolve_candidates(9606L, "target", tmp)
        stopifnot(isTRUE(mapped$found), identical(mapped$string_id, "9606.TARGET"),
                  identical(mapped$preferred_name, "TARGET"))
    })
    httr2::with_mocked_responses(list(response(404)), {
        result <- string_resolve_and_fetch(list(taxid = 9606L, id_candidates = "missing"), tmp)
        stopifnot(identical(result, list(ok = FALSE, reason = "not_found")))
        stopifnot(identical(string_resolution_cache_get(9606L, "missing", tmp)$found, FALSE))
    })
    for (status in c(400L, 401L, 500L, 503L)) {
        # Two responses permit the existing retry policy for transient statuses.
        httr2::with_mocked_responses(list(response(status), response(status)), {
            expect_error_class(string_resolve_candidates(9606L, paste0("error", status), tmp),
                               paste0("httr2_http_", status))
        })
    }
    httr2::with_mocked_responses(list(response(404)), {
        expect_error_class(string_api_request("network"), "httr2_http_404")
    })
    for (kind in c("timeout", "transport", "invalid_response")) {
        # Existing test seam: replace only the request boundary, preserving the condition.
        request_error <- structure(list(message = kind, call = NULL),
                                   class = c(paste0("test_", kind), "error", "condition"))
        original_request <- string_api_request
        string_api_request <- function(...) stop(request_error)
        environment(string_resolve_candidates) <- environment()
        caught <- tryCatch(string_resolve_candidates(9606L, kind, tmp), error = identity)
        stopifnot(identical(caught, request_error))
        string_api_request <- original_request
    }
})
cat("string-http-errors-ok\n")
