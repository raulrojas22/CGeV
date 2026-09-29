src<-Sys.getenv('CGV_SEQUENCE_TEST_SOURCE','R/utils.R');source(src)
bench<-function(label,fn,n=500){for(i in 1:10)fn();elapsed<-system.time(for(i in seq_len(n))fn())[['elapsed']];cat(label,elapsed/n*1000,'ms/request\n')}
for(spec in list(list(name='TP53-like',span=20000L,n=11L),list(name='BRCA1-like',span=80000L,n=23L))){
 p<-tempfile(fileext='.fa');writeLines(c('>chr1',strrep('ACGT',spec$span/4)),p)
 starts<-as.integer(seq(1,spec$span-100,length.out=spec$n));ex<-data.frame(start=starts,end=starts+99L)
 request<-function(){extract_sequence_from_fasta(p,'chr1',1,spec$span);extract_spliced_exon_sequence(p,'chr1',ex);get_transcript_composition_cached(p,'chr1',ex);for(i in seq_len(nrow(ex)))extract_sequence_from_fasta(p,'chr1',ex$start[i],ex$end[i]);invisible(NULL)}
 bench(spec$name,request);unlink(c(p,paste0(p,'.fai')))
}
