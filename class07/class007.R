
url <- "https://tinyurl.com/UK-foods"
x <- read.csv(url)
dim(x)
rownames(x) <- x[,1]
x <- x[,-1]
head(x)
dim(x)

barplot(as.matrix(x), beside=T, col=rainbow(nrow(x)))
pairs(x, col=rainbow(10), pch=16)

pca <- prcomp( t(x) )
summary(pca)
plot(pca$x[,1], pca$x[,2], xlab="PC1", ylab="PC2", xlim=c(-270,500))
text(pca$x[,1], pca$x[,2], colnames(x))

v <- round( pca$sdev^2/sum(pca$sdev^2) * 100 )
v

z <- summary(pca)
z$importance

barplot(v, xlab="Principal Component", ylab="Percent Variation")
par(mar=c(10, 3, 0.35, 0))
barplot( pca$rotation[,1], las=2 )
par(mar=c(10, 3, 0.35, 0))
barplot( pca$rotation[,2], las=2 )

biplot(pca)
url2 <- "https://tinyurl.com/expression-CSV"
rna.data <- read.csv(url2, row.names=1)
head(rna.data)
dim(rna.data)
pca <- prcomp(t(rna.data), scale=TRUE)
plot(pca$x[,1], pca$x[,2], xlab="PC1", ylab="PC2")
summary(pca)
plot(pca, main="Quick scree plot")
pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
pca.var.per
barplot(pca.var.per, main="Scree Plot", 
        names.arg = paste0("PC", 1:10),
        xlab="Principal Component", ylab="Percent Variation")
colvec <- colnames(rna.data)
colvec[grep("wt", colvec)] <- "red"
colvec[grep("ko", colvec)] <- "blue"

plot(pca$x[,1], pca$x[,2], col=colvec, pch=16,
     xlab=paste0("PC1 (", pca.var.per[1], "%)"),
     ylab=paste0("PC2 (", pca.var.per[2], "%)"))

text(pca$x[,1], pca$x[,2], labels = colnames(rna.data), pos=c(rep(4,5), rep(2,5)))  

library(ggplot2)

df <- as.data.frame(pca$x)

ggplot(df) + 
  aes(PC1, PC2) + 
  geom_point()
df$samples <- colnames(rna.data) 
df$condition <- substr(colnames(rna.data),1,2)

p <- ggplot(df) + 
  aes(PC1, PC2, label=samples, col=condition) + 
  geom_label(show.legend = FALSE)
p  
p + labs(title="PCA of RNASeq Data",
         subtitle = "PC1 clealy seperates wild-type from knock-out samples",
         x=paste0("PC1 (", pca.var.per[1], "%)"),
         y=paste0("PC2 (", pca.var.per[2], "%)"),
         caption="BIMM143 example data") +
  theme_bw()