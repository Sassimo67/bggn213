library(DESeq2)
colData <- read.csv("GSE37704_metadata.csv", row.names=1)
countData <- read.csv("GSE37704_featurecounts.csv", row.names=1)
head(colData)
head(countData)
countData <- as.matrix(countData[,-1])
head(countData)
countData = countData[rowSums(countData)>0, ]
head(countData)

dds = DESeqDataSetFromMatrix(countData=countData,
                             colData=colData,
                             design=~condition)

dds = DESeq(dds)
dds
resultsNames(dds)
res = results(dds)
summary(dds)

plot( res$log2FoldChange, -log(res$padj) )
mycols <- rep("gray", nrow(res))
mycols[ abs(res$log2FoldChange) > 2 ]  <- "red" 

inds <- (res$padj < 0.01) & (abs(res$log2FoldChange) > 2 )
mycols[ inds ] <- "blue"


plot( res$log2FoldChange,  -log(res$padj), 
      col=mycols, ylab="-Log(P-value)", xlab="Log2(FoldChange)" )

library("AnnotationDbi")
library("org.Hs.eg.db")
columns(org.Hs.eg.db) 

res$symbol <- mapIds(org.Hs.eg.db,
                     keys=row.names(res), 
                     keytype="ENSEMBL",     
                     column="SYMBOL",         
                     multiVals="first")
res$entrez <- mapIds(org.Hs.eg.db,
                     keys=row.names(res),
                     column="ENTREZID",
                     keytype="ENSEMBL",
                     multiVals="first")

res$name <- mapIds(org.Hs.eg.db,
                   keys=row.names(res),
                   column="GENENAME",
                   keytype="ENSEMBL",
                   multiVals="first")

head(res, 10)

res = res[order(res$pvalue),]
write.csv(res, file = "deseq_results.csv")

library(gage)
library(gageData)

data(kegg.sets.hs)
data(sigmet.idx.hs)

kegg.sets.hs = kegg.sets.hs[sigmet.idx.hs]
head(kegg.sets.hs, 3)

foldchanges = res$log2FoldChange
names(foldchanges) = res$entrez
head(foldchanges)
keggres = gage(foldchanges, gsets=kegg.sets.hs)
attributes(keggres)
head(keggres$less)

keggrespathways <- rownames(keggres$greater)[1:5]
keggresids = substr(keggrespathways, start=1, stop=8)
keggresids

keggrespathways <- rownames(keggres$less)[1:5]
keggresids = substr(keggrespathways, start=1, stop=8)
keggresids

data(go.sets.hs)
data(go.subs.hs)

gobpsets = go.sets.hs[go.subs.hs$BP]

gobpres = gage(foldchanges, gsets=gobpsets, same.dir=TRUE)

lapply(gobpres, head)

sig_genes <- res[res$padj <= 0.05 & !is.na(res$padj), "symbol"]
print(paste("Total number of significant genes:", length(sig_genes)))
write.table(sig_genes, file="significant_genes.txt", row.names=FALSE, col.names=FALSE, quote=FALSE)

sessionInfo()