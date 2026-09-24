library(testthat)

utils_root <- if (file.exists("R/utils.R")) "." else "../.."
compact_env <- new.env(parent = globalenv())
compact_env$`%||%` <- function(a, b) if (!is.null(a)) a else b

# Evaluate only the compactor from R/utils.R so the test stays independent of
# unrelated runtime helpers.
utils_exprs <- parse(file.path(utils_root, "R/utils.R"))
keep <- vapply(utils_exprs, function(expr) {
    is.call(expr) &&
        identical(as.character(expr[[1]])[1], "<-") &&
        as.character(expr[[2]])[1] == "compact_girafe_svg_html"
}, logical(1))
stopifnot(sum(keep) == 1L)
eval(utils_exprs[[which(keep)]], compact_env)
compact_svg <- compact_env$compact_girafe_svg_html

# Reference implementation of the pre-optimization transformation. Kept here to
# pin byte-identical behavior; production code must not depend on it.
reference_compact <- function(html, decimals = 2L) {
    if (!is.character(html) || length(html) == 0L || !nzchar(html[1])) return(html)
    out <- gsub(">\\s+<", "><", html, perl = TRUE)
    decimals_i <- suppressWarnings(as.integer(decimals %||% 2L))
    if (!is.finite(decimals_i) || is.na(decimals_i) || decimals_i < 0L) return(out)
    tag_matches <- gregexpr("<[^>]+>", out, perl = TRUE)
    tags <- regmatches(out, tag_matches)
    if (!length(tags) || !length(tags[[1]])) return(out)
    compact_tag <- function(tag) {
        matches <- gregexpr("(?<![A-Za-z0-9_])-?\\d+\\.\\d{4,}(?![A-Za-z0-9_])", tag, perl = TRUE)
        vals <- regmatches(tag, matches)
        if (!length(vals) || !length(vals[[1]])) return(tag)
        regmatches(tag, matches) <- lapply(vals, function(x) {
            nums <- suppressWarnings(as.numeric(x))
            repl <- ifelse(is.finite(nums), format(round(nums, decimals_i), scientific = FALSE, trim = TRUE), x)
            repl <- sub("\\.?0+$", "", repl, perl = TRUE)
            repl[repl == "-0"] <- "0"
            repl
        })
        tag
    }
    regmatches(out, tag_matches) <- list(vapply(tags[[1]], compact_tag, character(1)))
    out
}

test_that("whitespace between tags is removed", {
    html <- "<svg>\n  <g>\n\t<rect x='1.5'/>\n  </g>\n</svg>"
    expect_identical(compact_svg(html), "<svg><g><rect x='1.5'/></g></svg>")
})

test_that("high-precision tag attributes are rounded and trimmed", {
    html <- "<svg><rect x='1.234567' y='-0.000001' width='10.500000' height='0.100000'/></svg>"
    out <- compact_svg(html)
    expect_identical(out, "<svg><rect x='1.23' y='0' width='10.5' height='0.1'/></svg>")
})

test_that("text-node precision is never touched", {
    html <- "<svg><text x='1.234567'>6.5345</text><rect x='2.345678'/></svg>"
    out <- compact_svg(html)
    expect_true(grepl("6.5345", out, fixed = TRUE))
    expect_true(grepl("x='2.35'", out, fixed = TRUE))
})

test_that("non-numeric and multibyte content is preserved", {
    html <- "<svg><text x='1.234567'>caf\u00e9 \u00b1 \u0661.23456</text><rect x='4.56789'/></svg>"
    out <- compact_svg(html)
    expect_true(grepl("caf\u00e9 \u00b1 \u0661.23456", out, fixed = TRUE))
    expect_true(grepl("x='4.57'", out, fixed = TRUE))
})

test_that("fast path is exactly whitespace removal when no candidate exists", {
    html <- paste0(
        "<svg viewBox='0 0 1202.4 82.8'>\n",
        "  <g class='ggiraph-svg-rootg'>\n",
        "    <rect x='25.5' y='64.94' width='1.5'/>\n",
        "    <text x='10.5'>TP53</text>\n",
        "  </g>\n",
        "</svg>"
    )
    expect_identical(compact_svg(html), gsub(">\\s+<", "><", html, perl = TRUE))
})

test_that("invalid decimals short-circuit after whitespace removal", {
    html <- "<svg>\n  <rect x='1.234567'/>\n</svg>"
    expect_identical(compact_svg(html, decimals = -1L), "<svg><rect x='1.234567'/></svg>")
    expect_identical(compact_svg("", decimals = 2L), "")
    expect_identical(compact_svg(NULL), NULL)
})

test_that("candidate matches reference byte-for-byte on mixed payloads", {
    payloads <- list(
        no_candidates = paste0("<svg>", paste(rep("<rect x='1.5' y='2.25'/>", 200), collapse = "\n"), "</svg>"),
        with_candidates = paste0("<svg>", paste(rep("<rect x='1.234567' y='-0.000001'/>", 200), collapse = " "), "</svg>"),
        text_only = "<svg><text x='1.5'>3.14159265</text><text y='2.5'>2.7182818</text></svg>",
        mixed = "<svg><rect x='1.234567'/><text>9.999999</text><rect x='-0.000001'/><rect x='5.5'/></svg>",
        unicode = "<svg><text x='1.234567'>\u03b1\u03b2\u03b3 \u00b1 3.14159</text><rect x='2.345678'/></svg>"
    )
    for (name in names(payloads)) {
        expect_identical(
            compact_svg(payloads[[name]]),
            reference_compact(payloads[[name]]),
            info = name
        )
    }
})
