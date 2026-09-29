source('R/utils.R')
p<-tempfile(fileext='.fa');writeLines(c('>chr1',paste(rep('ACGT',250),collapse='')),p)
ex<-data.frame(start=c(1,101,301),end=c(50,150,350))
extract_spliced_exon_sequence(p,'chr1',ex); get_transcript_composition_cached(p,'chr1',ex)
k<-get_seq_extract_cache_key(p,'chr1',1,350)
bench<-function(label,fn,n=2000L){for(i in 1:20)fn(); t<-system.time(for(i in seq_len(n))fn())[['elapsed']];cat(label,t/n*1e6,'us/call\n')}
bench('normalizePath',function()normalizePath(p,winslash='/',mustWork=FALSE))
bench('file.info',function()file.info(p))
bench('lookup',function()cache_env_get(.seq_extract_cache,k))
bench('interval-hit',function()extract_sequence_from_fasta(p,'chr1',1,350))
bench('splice-hit',function()extract_spliced_exon_sequence(p,'chr1',ex))
bench('composition-hit',function()get_transcript_composition_cached(p,'chr1',ex))
if(exists('sequence_file_identity')) {
 id<-sequence_file_identity(p)
 bench('identity',function()sequence_file_identity(p))
 bench('identity-comparison',function()identical(id,id))
 bench('identity-guard',function()sequence_cache_validate(p))
}
unlink(c(p,paste0(p,'.fai')))
# Attribute list construction independently of normalizePath and stat.
if(exists('sequence_file_identity')) {
 p<-tempfile();writeLines('fixture',p);paths<-c(p,paste0(p,'.fai'),paste0(p,'.gzi'));fi<-file.info(paths)
 construct<-function(){number<-function(x){x<-as.numeric(x);x[is.na(x)]<-NA_real_;x};list(path=p,size=number(fi$size),mtime=number(fi$mtime),ctime=number(fi$ctime),valid=!is.na(fi$size[1L])&&!isTRUE(fi$isdir[1L]))}
 id<-construct();other<-construct()
 bench('file.info-three-paths',function()file.info(paths))
 bench('identity-construction-only',construct)
 bench('identity-independent-comparison',function()identical(id,other))
 unlink(p)
}
