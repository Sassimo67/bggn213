counts <- read.csv("airway_scaledcounts.csv", row.names=1)
metadata <-  read.csv("airway_metadata.csv")
head(counts)
head(metadata)

control <- metadata[metadata[,"dex"]=="control",]
control.counts <- counts[ ,control$id]
control.mean <- rowSums( control.counts )/4 
head(control.mean)

treated <- metadata[metadata[,"dex"]=="treated",]
treated.mean <- rowSums( counts[,treated$id] )/4 
names(treated.mean) <- counts$ensgene
head(treated.mean)

meancounts <- data.frame(control.mean, treated.mean)
colSums(meancounts)
plot(meancounts[,1],meancounts[,2], log = "xy", xlab="Control", ylab="Treated")
meancounts$log2fc <- log2(meancounts[,"treated.mean"]/meancounts[,"control.mean"])
head(meancounts)
zero.vals <- which(meancounts[,1:2]==0, arr.ind=TRUE)

to.rm <- unique(zero.vals[,1])
mycounts <- meancounts[-to.rm,]
head(mycounts)
up.ind <- mycounts$log2fc > 2
down.ind <- mycounts$log2fc < (-2)

library(DESeq2)
citation("DESeq2")
dds <- DESeqDataSetFromMatrix(countData=counts, 
                              colData=metadata, 
                              design=~dex)
dds
dds <- DESeq(dds)
res <- results(dds)
res
as.data.frame(res)
View(res)
summary(res)
res05 <- results(dds, alpha=0.05)
summary

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

res$uniprot <- mapIds(org.Hs.eg.db,
                      keys=row.names(res),
                      column="UNIPROT",
                      keytype="ENSEMBL",
                      multiVals="first")

res$genename <- mapIds(org.Hs.eg.db,
                       keys=row.names(res),
                       column="GENENAME",
                       keytype="ENSEMBL",
                       multiVals="first")

head(res)
ord <- order( res$padj )
head(res[ord,])

write.csv(res[ord,], "deseq_results.csv")

plot( res$log2FoldChange,  -log(res$padj), 
      xlab="Log2(FoldChange)",
      ylab="-Log(P-value)")
plot( res$log2FoldChange,  -log(res$padj), 
      ylab="-Log(P-value)", xlab="Log2(FoldChange)")

abline(v=c(-2,2), col="darkgray", lty=2)
abline(h=-log(0.05), col="darkgray", lty=2)

mycols <- rep("gray", nrow(res))
mycols[ abs(res$log2FoldChange) > 2 ]  <- "red" 

inds <- (res$padj < 0.01) & (abs(res$log2FoldChange) > 2 )
mycols[ inds ] <- "blue"

 
plot( res$log2FoldChange,  -log(res$padj), 
      col=mycols, ylab="-Log(P-value)", xlab="Log2(FoldChange)" )


abline(v=c(-2,2), col="gray", lty=2)
abline(h=-log(0.1), col="gray", lty=2)
library(EnhancedVolcano)
x <- as.data.frame(res)

EnhancedVolcano(x,
                lab = x$symbol,
                x = 'log2FoldChange',
                y = 'pvalue')
library(pathview)
library(gage)
library(gageData)
data(kegg.sets.hs)
head(kegg.sets.hs, 2)
foldchanges = res$log2FoldChange
names(foldchanges) = res$entrez
head(foldchanges)
keggres = gage(foldchanges, gsets=kegg.sets.hs)
attributes(keggres)
head(keggres$less, 3)


sessionInfo()