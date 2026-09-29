scripts <- c('test_transcript_composition_cache.R','test_twobit_single_span_splice.R',
 'test_twobit_seqnames_sidecar.R','test_sequence_prefetch_future_globals.R',
 'test_selected_sequence_download.R','test_sequence_download_ui_static.R',
 'test_inline_fast_sequence_prefetch.R','test_coordinated_memory_cache_budget.R',
 'test_runtime_cache_env.R')
scripts <- scripts[file.exists(file.path('scripts',scripts))]
for(s in scripts) {
 log<-file.path('validation/phase-5d3c',paste0(s,'.log'))
 status<-system2(file.path(R.home('bin'),'Rscript'),file.path('scripts',s),stdout=log,stderr=log)
 cat(s,': exit',status,'\n')
}
