# Supply synthetic data for standalone scripts whose production fixtures/tools
# are absent. The UCSC executable is explicitly replaced by rtracklayer here.
dir.create('genomes',showWarnings=FALSE)
p<-'genomes/GCF_000001735.4_TAIR10.1_genomic.2bit'
stopifnot(!file.exists(p),!file.exists('faToTwoBit'))
rtracklayer::export(Biostrings::DNAStringSet(c(chr1=strrep('ACGT',1000))),p)
writeLines(c('#!/usr/local/bin/Rscript','a<-commandArgs(TRUE)','rtracklayer::export(Biostrings::readDNAStringSet(a[1]),a[2],format="2bit")'),'faToTwoBit')
Sys.chmod('faToTwoBit','0755')
tryCatch({
 for(s in c('test_transcript_composition_cache.R','test_twobit_single_span_splice.R')) {
  status<-system2(file.path(R.home('bin'),'Rscript'),file.path('scripts',s))
  stopifnot(status==0L)
 }
},finally={unlink(p);unlink('faToTwoBit')})
