library(testthat)
model_root <- if (file.exists('R/modules.R')) '.' else '../..'
model_env <- new.env(parent=globalenv())
model_env$`%||%` <- function(a,b) if (!is.null(a)) a else b
sys.source(file.path(model_root,'R/modules.R'),model_env)

model_fixture <- function() list(
    df=data.frame(xstart=c(10,18,35),xend=c(20,25,45),seqid='chr1',
                  strand='+',attributes_raw=c('ID=a','ID=b','ID=c')),
    gene=data.frame(V4=10,V5=45,V1='chr1',V9='ID=gene'),
    tx=data.frame(V4=10,V5=45,V9='ID=tx1'))
model_identity <- function(x) model_env$make_gene_plot_model_data_key(x$df,x$gene,x$tx)

test_that('model identity covers the complete input, even with the same locus span', {
    x <- model_fixture(); key <- model_identity(x)
    expect_identical(key,model_identity(unserialize(serialize(x,NULL))))
    for (field in c('xstart','xend','seqid','strand','attributes_raw')) {
        changed <- x
        changed$df[[field]][2] <- if(is.numeric(changed$df[[field]])) 19 else 'changed'
        expect_false(identical(key,model_identity(changed)))
    }
    changed <- x; changed$gene$V9 <- 'ID=another-gene'
    expect_false(identical(key,model_identity(changed)))
    changed <- x; changed$tx$V9 <- 'ID=another-transcript'
    expect_false(identical(key,model_identity(changed)))
    changed <- x; changed$df <- changed$df[3:1,]
    expect_false(identical(key,model_identity(changed)))
})

test_that('model keys include only the inputs used by model preparation', {
    key <- model_identity(model_fixture())
    compact <- model_env$make_gene_plot_model_cache_key(key,'compact',TRUE)
    expect_false(identical(compact,model_env$make_gene_plot_model_cache_key(key,'detailed',TRUE)))
    expect_false(identical(compact,model_env$make_gene_plot_model_cache_key(key,'compact',FALSE)))
    expect_false(identical(compact,model_env$make_gene_plot_model_cache_key(key,'compact',TRUE,3)))
    expect_identical(model_env$make_gene_plot_model_cache_key(key,'detailed',TRUE),
                     model_env$make_gene_plot_model_cache_key(key,'detailed',FALSE))
    expect_identical(model_env$make_gene_plot_model_cache_key(NULL),'')
    # The independent SVG identity still varies with every visual option.
    base <- model_env$make_girafe_plot_cache_key('homo',plot_signature=key)
    for (option in list(list(max_gene_length_key=100),list(theme_mode='dark'),
                        list(orientation_mode='transcription'),list(is_colorblind_mode=TRUE))) {
        expect_false(identical(base,do.call(model_env$make_girafe_plot_cache_key,
                     c(list(plot_context='homo',plot_signature=key),option))))
    }
})

test_that('reused models are immutable and keep their bounded LRU behavior', {
    old <- Sys.getenv('APP_GENE_PLOT_MODEL_CACHE_MAX_ENTRIES',unset=NA_character_)
    on.exit(if(is.na(old)) Sys.unsetenv('APP_GENE_PLOT_MODEL_CACHE_MAX_ENTRIES') else
        Sys.setenv(APP_GENE_PLOT_MODEL_CACHE_MAX_ENTRIES=old),add=TRUE)
    Sys.setenv(APP_GENE_PLOT_MODEL_CACHE_MAX_ENTRIES='2')
    x <- model_fixture()
    prepared <- model_env$prepare_gene_plot_model(x$df,x$gene,x$tx)
    model_env$set_gene_plot_model_cache('a',prepared)
    copy <- model_env$get_gene_plot_model_cache('a'); copy$df$xstart[1] <- 999
    expect_identical(model_env$get_gene_plot_model_cache('a'),prepared)
    model_env$set_gene_plot_model_cache('b',prepared)
    model_env$get_gene_plot_model_cache('a')
    model_env$set_gene_plot_model_cache('c',prepared)
    expect_null(model_env$get_gene_plot_model_cache('b'))
    expect_length(ls(model_env$.cgv_gene_plot_model_cache),2)
})
