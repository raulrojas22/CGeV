#!/usr/bin/env Rscript
# Generate real ggiraph SVG pairs for test_gene_geometry_browser.js.
Sys.setenv(APP_FUTURE_MODE='sequential', APP_GENE_PLOT_RENDERER_PREWARM='0')
source('global.R')
out <- commandArgs(trailingOnly = TRUE)
stopifnot(length(out) == 1L)
df <- data.frame(y=1, xstart=c(110,150), xend=c(130,170),
    group=factor(c('exon','cds')), text=c('ID=exon1','ID=cds1'),
    feature_type=c('exon','cds'), seqid='chr1', source='test',
    feature_raw=c('exon','CDS'), score='.', strand='+', phase=c('.','0'),
    attributes_raw=c('ID=exon1;Parent=tx1','ID=cds1;Parent=tx1'), largo=21)
gene <- data.frame(V1='chr1',V2='test',V3='gene',V4=100,V5=200,
    V6='.',V7='+',V8='.',V9='ID=gene1;Name=GENE1',largo=101)
tx <- transform(gene, V3='mRNA',V9='ID=tx1;Parent=gene1')
fixtures <- list()
for (mode in c('compact','detailed')) for (strand in c('+','-')) {
    for (orientation in c('genomic','transcription')) {
        df$strand <- strand; gene$V7 <- strand; tx$V7 <- strand
        build <- function(delta) create_gene_plot(df,gene,tx,101,delta,NULL,
            'Gene Length: 101 pb','Transcript Length: 101 pb', visual_mode=mode,
            orientation_mode=orientation,plot_id='fixture',plot_context='test')
        first <- build(0); next_plot <- build(1)
        large_plot <- build(499)
        fixtures[[length(fixtures)+1L]] <- list(
            name=paste(mode,strand,orientation),old=first$x$html,"next"=next_plot$x$html, large=large_plot$x$html)
    }
}
jsonlite::write_json(fixtures,out[[1]],auto_unbox=TRUE)
cat('geometry-fixtures-ok:',length(fixtures),'pairs\n')
