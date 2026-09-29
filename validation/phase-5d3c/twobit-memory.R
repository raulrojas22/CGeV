source('R/utils.R')
root<-tempfile();dir.create(root);Sys.setenv(CGV_CACHE_DIR=file.path(root,'cache'));p<-file.path(root,'g.2bit')
ex<-data.frame(start=c(1,9),end=c(4,12))
caches<-c('.seq_extract_cache','.spliced_seq_cache','.twobit_seqinfo_cache','.twobit_native_index_cache','.twobit_handle_cache','.transcript_composition_cache')
for(i in 1:12){
 bases<-if(i%%2L)'AAAATTTTAAAATTTT' else 'CCCCGGGGCCCCGGGG';nm<-if(i%%2L)'old' else 'new'
 q<-file.path(root,'next.2bit');rtracklayer::export(Biostrings::DNAStringSet(setNames(bases,nm)),q);stopifnot(file.rename(q,p))
 stopifnot(identical(get_twobit_seqnames(p),nm),identical(extract_sequence_from_fasta(p,nm,1,16),bases),identical(extract_spliced_exon_sequence(p,nm,ex),if(i%%2L)'AAAAAAAA' else 'CCCCCCCC'))
 stopifnot(identical(get_transcript_composition_cached(p,nm,ex)$counts,if(i%%2L)c(A=8L,T=0L,C=0L,G=0L) else c(A=0L,T=0L,C=8L,G=0L)))
 counts<-vapply(caches,function(n)length(cache_env_entry_keys(get(n))),integer(1))
 bytes<-vapply(caches,function(n)sum(vapply(as.list(get(n),all.names=TRUE),function(x)as.numeric(object.size(x)),numeric(1))),numeric(1))
 cat(i,paste(counts,collapse=','),paste(bytes,collapse=','),'total',sum(bytes),'\n')
}
cat('order:',paste(caches,collapse=','),'\n');unlink(root,recursive=TRUE)
