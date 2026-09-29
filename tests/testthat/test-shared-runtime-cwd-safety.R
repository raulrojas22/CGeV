cwd_repo_file <- function(relative) {
    path <- file.path(relative)
    if (!file.exists(path)) {
        path <- file.path("..", "..", relative)
    }
    path
}

new_cwd_record_env <- function() {
    counters <- new.env(parent = emptyenv())
    counters$setwd_calls <- character(0)
    counters$getwd_calls <- 0L
    env <- new.env(parent = globalenv())
    env$setwd <- function(dir) {
        counters$setwd_calls <- c(counters$setwd_calls, as.character(dir))
        base::setwd(dir)
    }
    env$getwd <- function() {
        counters$getwd_calls <- counters$getwd_calls + 1L
        base::getwd()
    }
    list(env = env, counters = counters)
}

new_shared_domain_env <- function() {
    record <- new_cwd_record_env()
    sys.source(cwd_repo_file(file.path("R", "utils.R")), envir = record$env)
    sys.source(cwd_repo_file(file.path("R", "server_shared_analysis_domain.R")), envir = record$env)
    record
}

new_ncbi_domain_env <- function() {
    record <- new_cwd_record_env()
    sys.source(cwd_repo_file(file.path("R", "utils.R")), envir = record$env)
    sys.source(cwd_repo_file(file.path("R", "server_ncbi_download_domain.R")), envir = record$env)
    record
}

cwd_test_artifacts <- function() {
    analysis <- list(
        schema_version = 1L,
        analysis_id = "cwd-safety-fixed-id",
        created_at = "2026-01-02T03:04:05+0000",
        expires_at = "2027-01-02T03:04:05+0000",
        generator = list(version = "1.1.0-test"),
        query = list(genes = c("TEST1", "TEST2")),
        organisms = list(),
        privacy = list(private_data_included = TRUE),
        figures = list(
            list(id = "fig_one", svg = paste0(
                "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"10\" height=\"10\">",
                "<rect width=\"10\" height=\"10\"/></svg>"
            )),
            list(id = "fig_two", svg = paste0(
                "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"20\" height=\"20\">",
                "<circle cx=\"5\" cy=\"5\" r=\"4\"/></svg>"
            ))
        ),
        results = list()
    )
    structure(
        list(
            analysis = analysis,
            json = '{"analysis_id":"cwd-safety-fixed-id","query":{"genes":["TEST1","TEST2"]}}',
            html = "<html></html>"
        ),
        class = "cgv_report_artifacts"
    )
}

cwd_test_snapshot <- function() {
    list(
        schema_version = 2L,
        homologous = list(plots = list(list(
            id = "1",
            sequence_blob = ">TX1\nACGTACGT",
            annotation_path = "",
            genome_path = ""
        ))),
        orthologous = list(plots = list())
    )
}

cwd_write_package <- function(env, base_dir) {
    env$cgv_write_reproducibility_package(
        analysis = cwd_test_artifacts()$analysis,
        session_snapshot = cwd_test_snapshot(),
        homo_summary = data.frame(Gene = "TEST1", Transcript = "TX1"),
        include_private = TRUE,
        base_dir = base_dir,
        artifacts = cwd_test_artifacts()
    )
}

test_that("reproducibility packages keep relative member names without mutating cwd", {
    record <- new_shared_domain_env()
    work <- tempfile("cgv-cwd-zip-")
    dir.create(work, recursive = TRUE)
    on.exit(unlink(work, recursive = TRUE, force = TRUE), add = TRUE)
    pkg_root <- file.path(work, "packages")
    dir.create(pkg_root, recursive = TRUE)

    old_wd <- setwd(work)
    cwd_before <- getwd()
    on.exit(setwd(old_wd), add = TRUE)

    zip_path <- cwd_write_package(record$env, pkg_root)

    expect_true(file.exists(zip_path))
    expect_identical(getwd(), cwd_before)
    expect_length(record$counters$setwd_calls, 0L)
    expect_identical(record$counters$getwd_calls, 0L)

    members <- sort(utils::unzip(zip_path, list = TRUE)$Name)
    expect_identical(members, sort(c(
        "analysis.json",
        "CHECKSUMS.sha256",
        "figures/fig_one.svg",
        "figures/fig_two.svg",
        "README.md",
        "sequences/homologous_1.fasta",
        "session/cgv_session.rds",
        "tables/multi_gene_summary.csv"
    )))
    expect_false(any(grepl("^(/|[A-Za-z]:)", members)))
    expect_false(any(grepl(work, members, fixed = TRUE)))

    unzip_dir <- file.path(work, "unzipped")
    utils::unzip(zip_path, exdir = unzip_dir)
    expect_identical(
        paste(readLines(file.path(unzip_dir, "analysis.json"), warn = FALSE), collapse = "\n"),
        cwd_test_artifacts()$json
    )
    expect_true(file.exists(file.path(unzip_dir, "figures", "fig_one.svg")))
    expect_true(file.exists(file.path(unzip_dir, "CHECKSUMS.sha256")))
    expect_gte(length(readLines(file.path(unzip_dir, "CHECKSUMS.sha256"), warn = FALSE)), 5L)
})

test_that("reproducibility ZIP semantics do not depend on the caller cwd", {
    record <- new_shared_domain_env()
    work <- tempfile("cgv-cwd-parity-")
    dir.create(work, recursive = TRUE)
    on.exit(unlink(work, recursive = TRUE, force = TRUE), add = TRUE)

    build <- function(tag) {
        caller <- file.path(work, paste0("caller-", tag))
        pkg_root <- file.path(work, paste0("packages-", tag))
        out_dir <- file.path(work, paste0("unzipped-", tag))
        dir.create(caller, recursive = TRUE)
        dir.create(pkg_root, recursive = TRUE)
        old_wd <- setwd(caller)
        on.exit(setwd(old_wd), add = TRUE)
        zip_path <- cwd_write_package(record$env, pkg_root)
        utils::unzip(zip_path, exdir = out_dir)
        members <- sort(utils::unzip(zip_path, list = TRUE)$Name)
        hashes <- vapply(members, function(member) {
            unname(tools::md5sum(file.path(out_dir, member)))
        }, character(1))
        list(members = members, hashes = hashes)
    }

    first <- build("first")
    second <- build("second")

    expect_identical(first$members, second$members)
    expect_identical(first$hashes, second$hashes)
})

test_that("reproducibility packaging errors leave the process cwd untouched", {
    record <- new_shared_domain_env()
    work <- tempfile("cgv-cwd-errors-")
    dir.create(work, recursive = TRUE)
    on.exit(unlink(work, recursive = TRUE, force = TRUE), add = TRUE)

    old_wd <- setwd(work)
    cwd_before <- getwd()
    on.exit(setwd(old_wd), add = TRUE)

    broken_root <- file.path(work, "broken-root")
    dir.create(file.path(broken_root, "cache"), recursive = TRUE)
    writeLines("not a directory", file.path(broken_root, "cache", "reproducibility_packages"))
    expect_error(
        cwd_write_package(record$env, broken_root),
        "Could not create the reproducibility package archive"
    )
    expect_identical(getwd(), cwd_before)
    expect_length(record$counters$setwd_calls, 0L)

    old_zipcmd <- Sys.getenv("R_ZIPCMD", unset = NA_character_)
    on.exit({
        if (is.na(old_zipcmd)) Sys.unsetenv("R_ZIPCMD") else Sys.setenv(R_ZIPCMD = old_zipcmd)
    }, add = TRUE)
    Sys.setenv(R_ZIPCMD = file.path(work, "missing-zip-binary"))
    pkg_root <- file.path(work, "packages")
    dir.create(pkg_root, recursive = TRUE)
    expect_error(
        cwd_write_package(record$env, pkg_root),
        "Could not create the reproducibility package archive"
    )
    expect_identical(getwd(), cwd_before)
    expect_length(record$counters$setwd_calls, 0L)
})

test_that("cgv_zip_directory archives relative members from an explicit working directory", {
    record <- new_shared_domain_env()
    work <- tempfile("cgv-zip-helper-")
    dir.create(work, recursive = TRUE)
    on.exit(unlink(work, recursive = TRUE, force = TRUE), add = TRUE)

    source_dir <- file.path(work, "source")
    dir.create(file.path(source_dir, "nested"), recursive = TRUE)
    writeLines("top", file.path(source_dir, "top.txt"))
    writeLines("deep", file.path(source_dir, "nested", "deep.txt"))

    old_wd <- setwd(work)
    cwd_before <- getwd()
    on.exit(setwd(old_wd), add = TRUE)

    zip_path <- file.path(work, "explicit.zip")
    record$env$cgv_zip_directory(zip_path, source_dir)

    expect_identical(getwd(), cwd_before)
    expect_identical(sort(utils::unzip(zip_path, list = TRUE)$Name),
                     sort(c("top.txt", "nested/deep.txt")))
    expect_length(record$counters$setwd_calls, 0L)

    selected_zip <- file.path(work, "selected.zip")
    record$env$cgv_zip_directory(selected_zip, source_dir, files = "nested/deep.txt")
    expect_identical(utils::unzip(selected_zip, list = TRUE)$Name, "nested/deep.txt")
    expect_identical(getwd(), cwd_before)
})

test_that("ncbi_resolve_cached_path anchors relative paths to the canonical data root", {
    record <- new_ncbi_domain_env()
    work <- tempfile("cgv-cwd-ncbi-")
    dir.create(work, recursive = TRUE)
    on.exit(unlink(work, recursive = TRUE, force = TRUE), add = TRUE)

    data_root <- file.path(work, "data-root")
    acc_dir <- file.path(data_root, "ncbi_downloads", "GCF_000001.1")
    dir.create(acc_dir, recursive = TRUE)
    annotation_abs <- file.path(acc_dir, "annotation.gff.gz")
    genome_abs <- file.path(acc_dir, "genome.2bit")
    writeLines("##gff-version 3", annotation_abs)
    writeLines("2bit", genome_abs)
    annotation_rel <- "ncbi_downloads/GCF_000001.1/annotation.gff.gz"
    genome_rel <- "ncbi_downloads/GCF_000001.1/genome.2bit"

    old_root <- Sys.getenv("CGV_DATA_ROOT", unset = NA_character_)
    on.exit({
        if (is.na(old_root)) Sys.unsetenv("CGV_DATA_ROOT") else Sys.setenv(CGV_DATA_ROOT = old_root)
    }, add = TRUE)
    Sys.setenv(CGV_DATA_ROOT = data_root)

    expected_annotation <- normalizePath(annotation_abs, winslash = "/", mustWork = TRUE)
    probe <- function(caller) {
        old <- setwd(caller)
        on.exit(setwd(old), add = TRUE)
        before <- getwd()
        list(
            relative = record$env$ncbi_resolve_cached_path(annotation_rel),
            missing = record$env$ncbi_resolve_cached_path("ncbi_downloads/missing/file.gz"),
            absolute = record$env$ncbi_resolve_cached_path(annotation_abs),
            empty = record$env$ncbi_resolve_cached_path("   "),
            validation = record$env$ncbi_validate_cache_entry(
                list(annotation_tabix = annotation_rel, genome_2bit = genome_rel)
            ),
            cwd_before = before,
            cwd_after = getwd()
        )
    }

    caller_a <- file.path(work, "caller-a")
    caller_b <- file.path(work, "caller-b")
    dir.create(caller_a)
    dir.create(caller_b)
    first <- probe(caller_a)
    second <- probe(caller_b)

    expect_identical(normalizePath(first$relative, winslash = "/", mustWork = TRUE), expected_annotation)
    expect_identical(first$relative, second$relative)
    expect_identical(first$missing, second$missing)
    expect_identical(
        normalizePath(first$missing, winslash = "/", mustWork = FALSE),
        normalizePath(file.path(record$env$get_cgv_data_root("."), "ncbi_downloads", "missing", "file.gz"),
                      winslash = "/", mustWork = FALSE)
    )
    expect_identical(first$absolute, annotation_abs)
    expect_identical(first$empty, "")
    expect_true(isTRUE(first$validation$ok))
    expect_true(isTRUE(second$validation$ok))
    expect_identical(first$cwd_before, first$cwd_after)
    expect_identical(second$cwd_before, second$cwd_after)
    expect_identical(record$counters$getwd_calls, 0L)
    expect_length(record$counters$setwd_calls, 0L)
})

test_that("ncbi_resolve_cached_path falls back to the canonical root when CGV_DATA_ROOT is unset", {
    record <- new_ncbi_domain_env()
    work <- tempfile("cgv-cwd-ncbi-legacy-")
    dir.create(work, recursive = TRUE)
    on.exit(unlink(work, recursive = TRUE, force = TRUE), add = TRUE)

    old_root <- Sys.getenv("CGV_DATA_ROOT", unset = NA_character_)
    on.exit({
        if (is.na(old_root)) Sys.unsetenv("CGV_DATA_ROOT") else Sys.setenv(CGV_DATA_ROOT = old_root)
    }, add = TRUE)
    Sys.unsetenv("CGV_DATA_ROOT")

    old_wd <- setwd(work)
    on.exit(setwd(old_wd), add = TRUE)

    resolved <- record$env$ncbi_resolve_cached_path("ncbi_downloads/missing/file.gz")
    canonical_root <- record$env$get_cgv_data_root(".")
    expect_identical(
        normalizePath(resolved, winslash = "/", mustWork = FALSE),
        normalizePath(file.path(canonical_root, "ncbi_downloads/missing/file.gz"),
                      winslash = "/", mustWork = FALSE)
    )
    expect_identical(record$counters$getwd_calls, 0L)
})

test_that("request-time shared runtime does not call setwd or getwd", {
    shared <- new_shared_domain_env()
    ncbi <- new_ncbi_domain_env()

    package_body <- paste(deparse(shared$env$cgv_write_reproducibility_package), collapse = "\n")
    expect_false(grepl("setwd(", package_body, fixed = TRUE))
    expect_false(grepl("getwd(", package_body, fixed = TRUE))

    zip_body <- paste(deparse(shared$env$cgv_zip_directory), collapse = "\n")
    expect_false(grepl("setwd(", zip_body, fixed = TRUE))
    expect_false(grepl("getwd(", zip_body, fixed = TRUE))
    expect_true(grepl("wd = directory", zip_body, fixed = TRUE))

    ncbi_body <- paste(deparse(ncbi$env$ncbi_resolve_cached_path), collapse = "\n")
    expect_false(grepl("getwd(", ncbi_body, fixed = TRUE))
    expect_true(grepl("get_cgv_data_root", ncbi_body, fixed = TRUE))
})

test_that("one session's packaging leaves another session's path resolution untouched", {
    session_a <- new_shared_domain_env()
    session_b <- new_ncbi_domain_env()
    work <- tempfile("cgv-cwd-sessions-")
    dir.create(work, recursive = TRUE)
    on.exit(unlink(work, recursive = TRUE, force = TRUE), add = TRUE)

    data_root <- file.path(work, "data-root")
    acc_dir <- file.path(data_root, "ncbi_downloads", "GCF_000003.3")
    dir.create(acc_dir, recursive = TRUE)
    annotation_abs <- file.path(acc_dir, "annotation.gff.gz")
    genome_abs <- file.path(acc_dir, "genome.2bit")
    writeLines("##gff-version 3", annotation_abs)
    writeLines("2bit", genome_abs)
    annotation_rel <- "ncbi_downloads/GCF_000003.3/annotation.gff.gz"
    genome_rel <- "ncbi_downloads/GCF_000003.3/genome.2bit"

    old_root <- Sys.getenv("CGV_DATA_ROOT", unset = NA_character_)
    on.exit({
        if (is.na(old_root)) Sys.unsetenv("CGV_DATA_ROOT") else Sys.setenv(CGV_DATA_ROOT = old_root)
    }, add = TRUE)
    Sys.setenv(CGV_DATA_ROOT = data_root)

    old_wd <- setwd(work)
    cwd_before <- getwd()
    on.exit(setwd(old_wd), add = TRUE)

    resolve_b <- function() {
        session_b$env$ncbi_resolve_cached_path(annotation_rel)
    }

    before <- resolve_b()
    pkg_root <- file.path(work, "packages")
    dir.create(pkg_root, recursive = TRUE)
    zip_path <- cwd_write_package(session_a$env, pkg_root)
    after <- resolve_b()

    expect_true(file.exists(zip_path))
    expect_identical(before, after)
    expect_identical(getwd(), cwd_before)
    expect_length(session_a$counters$setwd_calls, 0L)
    expect_length(session_b$counters$setwd_calls, 0L)
    expect_identical(session_b$counters$getwd_calls, 0L)
    expect_true(isTRUE(session_b$env$ncbi_validate_cache_entry(
        list(annotation_tabix = annotation_rel, genome_2bit = genome_rel)
    )$ok))
})