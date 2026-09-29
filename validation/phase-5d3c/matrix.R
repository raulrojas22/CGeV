root <- normalizePath('.')
new_runtime <- function(fallback=FALSE) {
 e <- new.env(parent=globalenv()); sys.source(file.path(root,'R/utils.R'),e)
 if(fallback) e$requireNamespace <- function(package,...) if(package=='Rsamtools') FALSE else base::requireNamespace(package,...)
 e
}
a <- 'AAAATTTTAAAATTTT'; b <- 'CCCCGGGGCCCCGGGG'; ex <- data.frame(start=c(1,9),end=c(4,12))
for(fallback in c(FALSE,TRUE)) for(mode in c('atomic','rewrite','same-size','preserved-mtime','delete-recreate')) {
 e <- new_runtime(fallback); p <- tempfile(fileext='.fa'); writeLines(c('>chr1',a),p)
 e$extract_sequence_from_fasta(p,'chr1',1,16); e$extract_spliced_exon_sequence(p,'chr1',ex); e$get_transcript_composition_cached(p,'chr1',ex)
 before <- file.info(p); Sys.sleep(1.1)
 if(mode=='rewrite') writeLines(c('>chr1',b),p) else {
  if(mode=='delete-recreate') { unlink(p); cat('deletion',fallback, 'interval=',e$extract_sequence_from_fasta(p,'chr1',1,16),'splice=',e$extract_spliced_exon_sequence(p,'chr1',ex),'\n') }
  q<-tempfile(); writeLines(c('>chr1',b),q); if(mode=='preserved-mtime') Sys.setFileTime(q,before$mtime); stopifnot(file.rename(q,p))
 }
 cat(mode,if(fallback)'fallback' else 'indexed','exact=',e$extract_sequence_from_fasta(p,'chr1',1,16),'unseen=',e$extract_sequence_from_fasta(p,'chr1',5,8),'splice=',e$extract_spliced_exon_sequence(p,'chr1',ex),'counts=',paste(e$get_transcript_composition_cached(p,'chr1',ex)$counts,collapse=','),'\n')
 unlink(c(p,paste0(p,'.fai')))
}
future::plan(future::multisession,workers=I(1))
p<-tempfile(fileext='.fa'); writeLines(c('>chr1',a),p)
# State persists in a worker option, not exported afresh with each future.
run <- function() future::value(future::future({
 e<-getOption('cgev.sequence.probe'); if(is.null(e)){e<-new.env(parent=baseenv());sys.source(file.path(root,'R/utils.R'),e);options(cgev.sequence.probe=e)}
 list(pid=Sys.getpid(),seq=e$extract_spliced_exon_sequence(p,'chr1',ex),counts=e$get_transcript_composition_cached(p,'chr1',ex)$counts)
},globals=list(root=root,p=p,ex=ex)))
print(run()); Sys.sleep(1.1); q<-tempfile();writeLines(c('>chr1',b),q);stopifnot(file.rename(q,p));print(run())
future::plan(future::sequential);unlink(c(p,paste0(p,'.fai')))
