# Extra equal-compressed-size replacement with changed bases in tested exons.
for(expr in parse('tests/testthat/test-fasta-companion-invalidation.R')) {
 if(is.call(expr) && identical(expr[[1]],as.name('for')))break
 eval(expr)
}
d<-tempfile();dir.create(d);x<-companion_data();a<-companion_fixture(d,'a',x$a,60L)
found<-FALSE
for(pos in 70001:70015){
 for(base in setdiff(c('A','C','G','T'),substr(x$a,pos,pos))){
  changed<-x$a;substr(changed,pos,pos)<-base
  unlink(file.path(d,c('b.fa.gz.fai','b.fa.gz.gzi')))
  b<-companion_fixture(d,'b',changed,60L)
  if(file.info(a)$size==file.info(b)$size){found<-TRUE;break}
 }
 if(found)break
}
stopifnot(found,!identical(changed,x$a))
p<-file.path(d,'live.fa.gz');companion_publish(a,p);e<-companion_env();companion_oracle(e,p,x$a)
companion_publish(b,p);companion_oracle(e,p,changed)
stopifnot(e$sequence_fasta_index_usable(normalizePath(p)))
cat('Equal compressed bytes:',file.info(a)$size,'; changed base position:',pos,'; repeated bases/splice/composition assertions: 12; indexed: TRUE\n')
unlink(d,recursive=TRUE)
