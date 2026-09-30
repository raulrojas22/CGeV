# External mutation: restore only the old combined companion-validation gate.
# The working runtime file is never modified.
p<-tempfile(fileext='.R');mutant<-tempfile(fileext='.R')
stopifnot(system2('git',c('show','5c7e094dea4cd7e8fc16573f1e7da8ffa67a0e23:R/utils.R'),stdout=p)==0L)
old<-paste(readLines(p),collapse='\n');current<-paste(readLines('R/utils.R'),collapse='\n')
a<-'sequence_cache_validate <- function(path)';b<-'sequence_cache_finish <- function(identity)'
lo<-regexpr(a,old,fixed=TRUE)[1];hi<-regexpr(b,old,fixed=TRUE)[1]
x<-regexpr(a,current,fixed=TRUE)[1];y<-regexpr(b,current,fixed=TRUE)[1]
writeLines(paste0(substr(current,1,x-1),substr(old,lo,hi-1),substr(current,y,nchar(current))),mutant)
Sys.setenv(CGV_COMPANION_SOURCE=mutant)
r<-as.data.frame(testthat::test_file('tests/testthat/test-fasta-companion-invalidation.R',reporter='summary'))
stopifnot(sum(r$failed)>0L)
unlink(c(p,mutant))
