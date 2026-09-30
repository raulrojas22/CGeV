# Serial before/after comparison using the existing R1 scoped request workload.
parent<-tempfile(fileext='.R')
stopifnot(system2('git',c('show','5c7e094dea4cd7e8fc16573f1e7da8ffa67a0e23:R/utils.R'),stdout=parent)==0L)
rows<-list();counts<-list()
for(state in c('parent','candidate')) {
 e<-new.env(parent=baseenv());sys.source(if(state=='parent')parent else 'R/utils.R',e)
 for(spec in list(list(name='TP53',span=20000L,n=11L),list(name='BRCA1',span=80000L,n=23L))) {
  p<-tempfile(fileext='.fa');writeLines(c('>chr1',strrep('ACGT',spec$span/4)),p)
  starts<-as.integer(seq(1,spec$span-100,length.out=spec$n));ex<-data.frame(start=starts,end=starts+99L)
  request<-function()e$with_sequence_file_identity(p,function(ctx){
   e$extract_sequence_from_fasta(p,'chr1',1,spec$span,.sequence_context=ctx)
   e$extract_spliced_exon_sequence(p,'chr1',ex,.sequence_context=ctx)
   e$get_transcript_composition_cached(p,'chr1',ex,.sequence_context=ctx)
   for(i in seq_len(nrow(ex)))e$extract_sequence_from_fasta(p,'chr1',ex$start[i],ex$end[i],.sequence_context=ctx)
  })
  for(i in 1:10)request()
  times<-vapply(1:5,function(i){gc();system.time(for(j in 1:500)request())[['elapsed']]/500*1e6},numeric(1))
  rows[[length(rows)+1L]]<-data.frame(state=state,request=spec$name,median_us=median(times),samples=paste(times,collapse=','))
  tracked<-c('sequence_file_identity','file.info','sequence_cache_validate','sequence_cache_finish','sequence_fasta_companions_compatible')
  cnt<-setNames(integer(length(tracked)),tracked);originals<-list()
  for(n in tracked)if(exists(n,e,inherits=TRUE)){
   originals[[n]]<-get(n,e,inherits=TRUE)
   e[[n]]<-local({nm<-n;f<-originals[[n]];function(...){cnt[[nm]]<<-cnt[[nm]]+1L;f(...)}})
  }
  request();counts[[length(counts)+1L]]<-data.frame(state=state,request=spec$name,t(cnt),check.names=FALSE)
  for(n in names(originals))e[[n]]<-originals[[n]]
  unlink(c(p,paste0(p,'.fai')))
 }
}
print(do.call(rbind,rows));print(do.call(rbind,counts))
write.table(do.call(rbind,rows),'validation/phase-5d3c-companions/performance.tsv',sep='\t',row.names=FALSE)
write.table(do.call(rbind,counts),'validation/phase-5d3c-companions/counts.tsv',sep='\t',row.names=FALSE)
# Isolate compatibility cost: one real bgzip scan per call on an unchanged
# fixture, deliberately bypassing memoized state only for this measurement.
e<-new.env(parent=baseenv());sys.source('R/utils.R',e)
set.seed(541L)
for(n in c(150000L,5000000L)) {
 p<-tempfile(fileext='.fa');bases<-paste(sample(c('A','C','G','T'),n,TRUE),collapse='')
 starts<-seq.int(1L,n,by=73L);writeLines(c('>chr1',substring(bases,starts,pmin(starts+72L,n))),p)
 gz<-paste0(p,'.gz');Rsamtools::bgzip(p,dest=gz);Rsamtools::indexFa(gz)
 e$sequence_fasta_companions_compatible(gz,TRUE,TRUE)
 times<-vapply(1:5,function(i){gc();system.time(stopifnot(e$sequence_fasta_companions_compatible(gz,TRUE,TRUE)))[['elapsed']]*1000},numeric(1))
 cat('Compatibility scan',n,'bases median_ms',median(times),'samples',paste(times,collapse=','),'\n')
 unlink(c(p,gz,paste0(gz,c('.fai','.gzi'))))
}
unlink(parent)
