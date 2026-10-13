args <- commandArgs(TRUE)
setwd(args[[1L]])
spec <- readRDS(args[[2L]])
writeLines(as.character(Sys.getpid()), paste0(args[[3L]], '.started'))
deadline <- Sys.time() + 45
while (!file.exists(args[[3L]]) && Sys.time() < deadline) Sys.sleep(.01)
job <- spec$job
value <- tryCatch({
    value <- eval(spec$expr, envir = globalenv())
    stopifnot(isTRUE(value$found),identical(range(as.numeric(value$data$V4),as.numeric(value$data$V5)),c(210,240)))
    if (isTRUE(spec$fail)) stop('controlled late worker failure')
    value
}, error = function(e) e)
saveRDS(list(pid = Sys.getpid(), value = value), args[[4L]])
