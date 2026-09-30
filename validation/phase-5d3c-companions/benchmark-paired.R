# Serial alternating batches to reduce host/time drift in the sequential run.
parent<-tempfile(fileext='.R');stopifnot(system2('git',c('show','5c7e094dea4cd7e8fc16573f1e7da8ffa67a0e23:R/utils.R'),stdout=parent)==0L)
make_request<-function(source,span,n){
 e<-new.env(parent=baseenv());sys.source(source,e)
 p<-tempfile(fileext='.fa');writeLines(c('>chr1',strrep('ACGT',span/4)),p)
 starts<-as.integer(seq(1,span-100,length.out=n));ex<-data.frame(start=starts,end=starts+99L)
 fn<-function()e$with_sequence_file_identity(p,function(ctx){
  e$extract_sequence_from_fasta(p,'chr1',1,span,.sequence_context=ctx)
  e$extract_spliced_exon_sequence(p,'chr1',ex,.sequence_context=ctx)
  e$get_transcript_composition_cached(p,'chr1',ex,.sequence_context=ctx)
  for(i in seq_len(nrow(ex)))e$extract_sequence_from_fasta(p,'chr1',ex$start[i],ex$end[i],.sequence_context=ctx)
 })
 for(i in 1:20)fn()
 list(fn=fn,path=p)
}
rows<-list()
for(spec in list(list(name='TP53',span=20000L,n=11L),list(name='BRCA1',span=80000L,n=23L))){
 runs<-list(parent=make_request(parent,spec$span,spec$n),candidate=make_request('R/utils.R',spec$span,spec$n))
 for(batch in 1:5)for(state in if(batch%%2L)names(runs) else rev(names(runs))){
  gc();fn<-runs[[state]]$fn
  us<-system.time(for(i in 1:500)fn())[['elapsed']]/500*1e6
  rows[[length(rows)+1L]]<-data.frame(request=spec$name,batch=batch,state=state,us=us)
 }
 for(run in runs)unlink(c(run$path,paste0(run$path,'.fai')))
}
r<-do.call(rbind,rows);print(r);print(aggregate(us~request+state,r,median))
write.table(r,'validation/phase-5d3c-companions/performance-paired.tsv',sep='\t',row.names=FALSE)
unlink(parent)
