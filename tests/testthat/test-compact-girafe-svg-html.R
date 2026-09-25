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

test_that("candidates that only appear in text nodes do not trigger tag work", {
    html <- paste0(
        "<svg>",
        paste(rep("<text>6.5345 Mb</text><rect x='1.5'/>", 100), collapse = " "),
        "</svg>"
    )
    expect_identical(compact_svg(html), gsub(">\\s+<", "><", html, perl = TRUE))
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

test_that("NA input preserves baseline behavior", {
    expect_identical(compact_svg(NA_character_), reference_compact(NA_character_))
    expect_identical(compact_svg(NA_character_), NA_character_)
})

test_that("multi-element vectors preserve baseline behavior", {
    with_candidate <- c("<svg><rect x='1.5'/></svg>", "<svg><rect x='1.2345'/></svg>")
    no_candidate <- c("<svg><rect x='1.5'/></svg>", "<svg><rect x='2.5'/></svg>")
    three_elements <- c(
        "<svg><rect x='1.5'/></svg>",
        "<svg><rect x='2.5'/></svg>",
        "<svg><rect x='1.2345'/></svg>"
    )
    expect_identical(compact_svg(with_candidate), reference_compact(with_candidate))
    expect_identical(compact_svg(no_candidate), reference_compact(no_candidate))
    expect_identical(compact_svg(three_elements), reference_compact(three_elements))
})

test_that("tag-boundary and delimiter edge cases match baseline", {
    raw_gt <- "<svg><rect title='a > b' x='1.2345'/></svg>"
    expect_identical(compact_svg(raw_gt), reference_compact(raw_gt))
    expect_identical(compact_svg(raw_gt), raw_gt)
    escaped_gt <- "<svg><rect title='a &gt; 1.2345' x='2.3456'/></svg>"
    expect_identical(compact_svg(escaped_gt), reference_compact(escaped_gt))
    expect_identical(compact_svg(escaped_gt), "<svg><rect title='a &gt; 1.23' x='2.35'/></svg>")
    comment <- "<svg><!-- 1.2345 --></svg>"
    expect_identical(compact_svg(comment), reference_compact(comment))
    expect_identical(compact_svg(comment), "<svg><!-- 1.23 --></svg>")
    cdata <- "<svg><![CDATA[ 1.2345 ]]></svg>"
    expect_identical(compact_svg(cdata), reference_compact(cdata))
    expect_identical(compact_svg(cdata), "<svg><![CDATA[ 1.23 ]]></svg>")
})

test_that("fractional-digit boundary and quoting are exact", {
    expect_identical(compact_svg("<svg><rect x='1.234'/></svg>"), "<svg><rect x='1.234'/></svg>")
    expect_identical(compact_svg("<svg><rect x='1.2345'/></svg>"), "<svg><rect x='1.23'/></svg>")
    expect_identical(compact_svg("<svg><rect x=\"1.2345\"/></svg>"), "<svg><rect x=\"1.23\"/></svg>")
})

test_that("multibyte prefixes and overflow values match baseline", {
    multibyte <- paste0("caf\u00e9\u03b1", "<svg><rect x='1.2345'/></svg>")
    expect_identical(compact_svg(multibyte), reference_compact(multibyte))
    expect_identical(compact_svg(multibyte), paste0("caf\u00e9\u03b1", "<svg><rect x='1.23'/></svg>"))
    overflow <- paste0("<svg><rect x='", strrep("9", 320), ".99999'/></svg>")
    expect_identical(compact_svg(overflow), reference_compact(overflow))
    expect_identical(compact_svg(overflow), overflow)
})

test_that("attributes and class follow baseline semantics", {
    classed <- structure("<svg><rect x='1.5'/></svg>", class = "html")
    expect_identical(compact_svg(classed), reference_compact(classed))
    expect_null(attributes(compact_svg(classed)))
    custom <- structure("<svg><rect x='1.5'/></svg>", foo = "bar")
    expect_identical(compact_svg(custom), reference_compact(custom))
    expect_null(attributes(compact_svg(custom)))
    named <- structure("<svg><rect x='1.5'/></svg>", names = "nm")
    expect_identical(compact_svg(named), reference_compact(named))
    expect_identical(names(compact_svg(named)), "nm")
    dimmed <- structure("<svg><rect x='1.5'/></svg>", dim = 1L)
    expect_identical(compact_svg(dimmed), reference_compact(dimmed))
    expect_null(attributes(compact_svg(dimmed)))
})

test_that("tag-free attributed input is returned unchanged", {
    plain_classed <- structure("plain text", class = "html")
    expect_identical(compact_svg(plain_classed), reference_compact(plain_classed))
    expect_identical(compact_svg(plain_classed), plain_classed)
    plain_named <- structure("plain text", names = "nm")
    expect_identical(compact_svg(plain_named), reference_compact(plain_named))
    expect_identical(compact_svg(plain_named), plain_named)
})

test_that("Latin-1 and bytes-marked inputs match baseline raw bytes", {
    latin1 <- iconv("<svg><text>caf\u00e9</text><rect x='1.5'/></svg>", to = "latin1")
    skip_if(is.na(latin1), "latin1 conversion unavailable on this platform")
    out <- compact_svg(latin1)
    ref <- reference_compact(latin1)
    expect_identical(out, ref)
    expect_identical(Encoding(out), "UTF-8")
    expect_identical(charToRaw(out), charToRaw(ref))
    expect_true(any(as.integer(charToRaw(out)) == 0xc3L))
    bytes_marked <- local({
        x <- rawToChar(as.raw(c(0x3c, 0x73, 0x76, 0x67, 0x3e, 0xc3, 0xa9, 0x3c, 0x78, 0x3e)))
        Encoding(x) <- "bytes"
        x
    })
    expect_identical(compact_svg(bytes_marked), reference_compact(bytes_marked))
    expect_identical(charToRaw(compact_svg(bytes_marked)), charToRaw(bytes_marked))
    expect_identical(Encoding(compact_svg(bytes_marked)), "bytes")
})

test_that("the fast branch skips tag extraction for candidate-free payloads", {
    probe_env <- new.env(parent = compact_env)
    probe_log <- new.env(parent = emptyenv())
    probe_log$calls <- character(0)
    probe_env$gregexpr <- function(pattern, text, ...) {
        probe_log$calls <- c(probe_log$calls, paste0("gregexpr:", pattern, ":", isTRUE(list(...)$useBytes)))
        base::gregexpr(pattern, text, ...)
    }
    probe_env$grepl <- function(pattern, text, ...) {
        probe_log$calls <- c(probe_log$calls, paste0("grepl:", pattern, ":", isTRUE(list(...)$useBytes)))
        base::grepl(pattern, text, ...)
    }
    probe <- compact_svg
    environment(probe) <- probe_env
    payload <- paste0("<svg>", paste(rep("<rect x='1.5'/>", 2000), collapse = ""), "</svg>")
    out <- probe(payload)
    expect_identical(out, gsub(">\\s+<", "><", payload, perl = TRUE))
    expect_false(any(grepl("gregexpr:<[^>]+>", probe_log$calls, fixed = TRUE)))
    expect_true(any(grepl("grepl:<[^>]+>:", probe_log$calls, fixed = TRUE)))
})

test_that("the slow branch still runs the original tag pipeline", {
    probe_env <- new.env(parent = compact_env)
    probe_log <- new.env(parent = emptyenv())
    probe_log$calls <- character(0)
    probe_env$gregexpr <- function(pattern, text, ...) {
        probe_log$calls <- c(probe_log$calls, paste0("gregexpr:", pattern, ":", isTRUE(list(...)$useBytes)))
        base::gregexpr(pattern, text, ...)
    }
    probe <- compact_svg
    environment(probe) <- probe_env
    payload <- "<svg><rect x='1.2345'/></svg>"
    expect_identical(probe(payload), reference_compact(payload))
    expect_true(any(grepl("gregexpr:<[^>]+>:FALSE", probe_log$calls, fixed = TRUE)))
})
