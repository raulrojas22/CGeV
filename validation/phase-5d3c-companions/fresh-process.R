# Load fixture helpers only, without executing test cases.
for(expr in parse('tests/testthat/test-fasta-companion-invalidation.R')) {
    if(is.call(expr) && identical(expr[[1]],as.name('for')))break
    eval(expr)
}
d<-tempfile();dir.create(d);x<-companion_data()
a<-companion_fixture(d,'a',x$a,60L);b<-companion_fixture(d,'b',x$b,73L)
p<-file.path(d,'live.fa.gz');companion_publish(b,p)
file.copy(paste0(a,'.fai'),paste0(p,'.fai'),overwrite=TRUE)
Sys.setFileTime(paste0(p,'.fai'),Sys.time()+1)
input<-file.path(d,'input.rds');saveRDS(list(path=p,b=x$b,runtime=normalizePath('R/utils.R')),input)
script<-file.path(d,'child.R')
writeLines(c(
 'x<-readRDS(commandArgs(TRUE)[1]);e<-new.env(parent=baseenv());sys.source(x$runtime,e)',
 'ex<-data.frame(start=c(70001L,100001L),end=c(70010L,100010L))',
 'want<-paste0(substr(x$b,70001,70010),substr(x$b,100001,100010))',
 'for(i in 1:2){stopifnot(identical(e$extract_sequence_from_fasta(x$path,"chr1",70001,70028),substr(x$b,70001,70028)))',
 'stopifnot(identical(e$extract_spliced_exon_sequence(x$path,"chr1",ex),want))',
 'counts<-setNames(vapply(c("A","T","C","G"),function(z)sum(strsplit(want,"")[[1]]==z),integer(1)),c("A","T","C","G"))',
 'stopifnot(identical(e$get_transcript_composition_cached(x$path,"chr1",ex)$counts,counts))}',
 'stopifnot(!e$sequence_fasta_index_usable(normalizePath(x$path)))',
 'cat("Fresh process Case K PASS; child PID",Sys.getpid(),"\\n")'),script)
stopifnot(system2(file.path(R.home('bin'),'Rscript'),c(shQuote(script),shQuote(input)))==0L)
unlink(d,recursive=TRUE)
