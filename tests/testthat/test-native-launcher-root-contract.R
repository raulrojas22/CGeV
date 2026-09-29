native_contract_repo_root <- function() {
    candidates <- c(".", file.path("..", ".."))
    for (candidate in candidates) {
        root <- normalizePath(candidate, winslash = "/", mustWork = FALSE)
        if (file.exists(file.path(root, "scripts", "run-native.sh"))) {
            return(root)
        }
    }
    NULL
}

native_contract_load_env <- function(root) {
    env <- new.env(parent = globalenv())
    sys.source(file.path(root, "R", "utils.R"), envir = env)
    sys.source(file.path(root, "R", "server_ncbi_download_domain.R"), envir = env)
    env
}

native_contract_write_entry <- function(env, acc, ann_rel, gen_rel) {
    env$ncbi_update_downloads_registry(
        list(
            accession = acc,
            annotation_tabix = ann_rel,
            annotation_index = paste0(ann_rel, ".tbi"),
            genome_2bit = gen_rel
        ),
        organism_name = paste("Native contract", acc),
        taxid = "999999"
    )
}

test_that("run-native.sh exports an absolute APP_DIR to the R child process", {
    root <- native_contract_repo_root()
    skip_if(is.null(root), "repository root not found")

    work <- tempfile("cgv-native-launcher-")
    dir.create(work, recursive = TRUE)
    on.exit(unlink(work, recursive = TRUE, force = TRUE), add = TRUE)

    app <- file.path(work, "app")
    dir.create(file.path(app, "scripts"), recursive = TRUE)
    file.copy(
        file.path(root, "scripts", "run-native.sh"),
        file.path(app, "scripts", "run-native.sh"),
        overwrite = TRUE
    )
    file.copy(file.path(root, ".env.example"), file.path(app, ".env"), overwrite = TRUE)

    fakebin <- file.path(work, "fakebin")
    dir.create(fakebin, recursive = TRUE)
    stub <- file.path(fakebin, "Rscript")
    writeLines(c(
        "#!/usr/bin/env bash",
        "out=\"${FAKE_RSCRIPT_ENV_OUT}\"",
        "{",
        "  echo \"CWD=$(pwd)\"",
        "  env | LC_ALL=C sort",
        "} > \"$out\"",
        "exit 0"
    ), stub)
    Sys.chmod(stub, "0755")

    runner <- file.path(work, "run-contract.sh")
    writeLines(c(
        "#!/usr/bin/env bash",
        "set -euo pipefail",
        "unset APP_DIR CGV_DATA_ROOT CGV_NCBI_DOWNLOADS_DIR",
        paste0("exec bash ", shQuote(file.path(app, "scripts", "run-native.sh")))
    ), runner)
    Sys.chmod(runner, "0755")

    env_out <- file.path(work, "child-env.txt")
    output <- system2(
        "bash", shQuote(runner),
        env = c(
            paste0("PATH=", fakebin, ":", Sys.getenv("PATH")),
            paste0("FAKE_RSCRIPT_ENV_OUT=", env_out)
        ),
        stdout = TRUE, stderr = TRUE
    )
    expect_true(file.exists(env_out), info = paste(output, collapse = "\n"))
    lines <- readLines(env_out, warn = FALSE)

    app_lines <- grep("^APP_DIR=", lines, value = TRUE)
    expect_length(app_lines, 1L)
    expect_identical(
        normalizePath(sub("^APP_DIR=", "", app_lines), winslash = "/", mustWork = FALSE),
        normalizePath(app, winslash = "/", mustWork = FALSE)
    )
    expect_false(any(grepl("^CGV_DATA_ROOT=", lines)))
    expect_true(any(grepl("^CGV_NCBI_DOWNLOADS_DIR=", lines)))
    expect_true(any(grepl("^CWD=", lines)))
})

test_that("native relative registry entries resolve against exported APP_DIR with cache hits", {
    root <- native_contract_repo_root()
    skip_if(is.null(root), "repository root not found")
    env <- native_contract_load_env(root)

    work <- tempfile("cgv-native-cache-")
    dir.create(work, recursive = TRUE)
    on.exit(unlink(work, recursive = TRUE, force = TRUE), add = TRUE)

    old_root <- Sys.getenv("CGV_DATA_ROOT", unset = NA_character_)
    old_app <- Sys.getenv("APP_DIR", unset = NA_character_)
    old_ncbi <- Sys.getenv("CGV_NCBI_DOWNLOADS_DIR", unset = NA_character_)
    old_wd <- getwd()
    on.exit({
        if (is.na(old_root)) Sys.unsetenv("CGV_DATA_ROOT") else Sys.setenv(CGV_DATA_ROOT = old_root)
        if (is.na(old_app)) Sys.unsetenv("APP_DIR") else Sys.setenv(APP_DIR = old_app)
        if (is.na(old_ncbi)) Sys.unsetenv("CGV_NCBI_DOWNLOADS_DIR") else Sys.setenv(CGV_NCBI_DOWNLOADS_DIR = old_ncbi)
        setwd(old_wd)
    }, add = TRUE)

    app_root <- file.path(work, "app-root")
    acc_dot <- "GCF_000001.1"
    acc_plain <- "GCF_000002.2"
    dot_ann <- file.path("ncbi_downloads", acc_dot, paste0(acc_dot, "_genomic.gff.gz"))
    dot_gen <- file.path("ncbi_downloads", acc_dot, paste0(acc_dot, "_genomic.2bit"))
    plain_ann <- file.path("ncbi_downloads", acc_plain, paste0(acc_plain, "_genomic.gff.gz"))
    plain_gen <- file.path("ncbi_downloads", acc_plain, paste0(acc_plain, "_genomic.2bit"))

    dir.create(file.path(app_root, "ncbi_downloads", acc_dot), recursive = TRUE)
    dir.create(file.path(app_root, "ncbi_downloads", acc_plain), recursive = TRUE)
    writeLines("canonical-dot-gff", file.path(app_root, dot_ann))
    writeLines("canonical-dot-2bit", file.path(app_root, dot_gen))
    writeLines("canonical-plain-gff", file.path(app_root, plain_ann))
    writeLines("canonical-plain-2bit", file.path(app_root, plain_gen))

    Sys.unsetenv("CGV_DATA_ROOT")
    Sys.setenv(APP_DIR = app_root, CGV_NCBI_DOWNLOADS_DIR = "./ncbi_downloads")
    setwd(app_root)

    native_contract_write_entry(env, acc_dot, paste0("./", dot_ann), paste0("./", dot_gen))
    native_contract_write_entry(env, acc_plain, plain_ann, plain_gen)

    dot_hit <- env$ncbi_check_already_downloaded(acc_dot)
    plain_hit <- env$ncbi_check_already_downloaded(acc_plain)
    expect_false(is.null(dot_hit))
    expect_false(is.null(plain_hit))

    registry <- env$read_ncbi_downloads_registry()
    dot_entry <- as.list(registry[registry$accession == acc_dot, , drop = FALSE][1, , drop = FALSE])
    dot_validation <- env$ncbi_validate_cache_entry(dot_entry)
    expect_true(isTRUE(dot_validation$ok))
    expect_identical(readLines(dot_validation$annotation_path), "canonical-dot-gff")
    expect_identical(
        normalizePath(dot_validation$annotation_path, winslash = "/", mustWork = TRUE),
        normalizePath(file.path(app_root, dot_ann), winslash = "/", mustWork = TRUE)
    )

    decoy <- file.path(work, "decoy")
    dir.create(file.path(decoy, "ncbi_downloads", acc_dot), recursive = TRUE)
    dir.create(file.path(decoy, "ncbi_downloads", acc_plain), recursive = TRUE)
    writeLines("decoy-dot-gff", file.path(decoy, dot_ann))
    writeLines("decoy-dot-2bit", file.path(decoy, dot_gen))
    writeLines("decoy-plain-gff", file.path(decoy, plain_ann))
    writeLines("decoy-plain-2bit", file.path(decoy, plain_gen))

    setwd(decoy)
    decoy_resolution <- env$ncbi_resolve_cached_path(paste0("./", dot_ann))
    expect_identical(
        normalizePath(decoy_resolution, winslash = "/", mustWork = TRUE),
        normalizePath(file.path(app_root, dot_ann), winslash = "/", mustWork = TRUE)
    )
    decoy_validation <- env$ncbi_validate_cache_entry(dot_entry)
    expect_true(isTRUE(decoy_validation$ok))
    expect_identical(readLines(decoy_validation$annotation_path), "canonical-dot-gff")
    expect_false(startsWith(
        normalizePath(decoy_validation$annotation_path, winslash = "/", mustWork = TRUE),
        normalizePath(decoy, winslash = "/", mustWork = TRUE)
    ))
})


