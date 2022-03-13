wisc.df <- read.csv("WisconsinCancer.csv", row.names=1)
wisc.data <- wisc.df[,-1]
diagnosis <- factor(wisc.data[,1])
colMeans(wisc.data[,-1])
apply(wisc.data,2,sd)
wisc.pr <- prcomp(wisc.data[,-1])
summary(wisc.pr)

biplot(wisc.pr)

plot(wisc.pr$x, col = diagnosis,
     xlab = "PC1", ylab = "PC2")

plot(wisc.pr$x[, 2 ], col = diagnosis, 
     xlab = "PC1", ylab = "PC3")

df <- as.data.frame(wisc.pr$x)
df$diagnosis <- diagnosis

library(ggplot2)
ggplot(df) + 
  aes(PC1, PC2, col= diagnosis) + 
  geom_point()

pr.var <- wisc.pr$sdev^2
head(pr.var)

pve <- pr.var / 30
plot(pve, xlab = "Principal Component", 
     ylab = "Proportion of Variance Explained", 
     ylim = c(0, 1), type = "o")

barplot(pve, ylab = "Precent of Variance Explained",
        names.arg=paste0("PC",1:length(pve)), las=2, axes = FALSE)
axis(2, at=pve, labels=round(pve,2)*100 )

data.scaled <- scale(wisc.data[,-1])
data.dist <- dist(data.scaled)
wisc.hclust <- hclust(data.dist, method = "complete")
plot(wisc.hclust)
abline(h =19 , col="red", lty=2)

wisc.hclust.clusters <- cutree(wisc.hclust, k = 4)
table(wisc.hclust.clusters, diagnosis)

grps <- cutree(wisc.hclust, k=2)
table(grps)

table(grps, diagnosis)
plot(wisc.pr$x[,1:2], col=grps)

url <- "https://tinyurl.com/new-samples-CSV"
new <- read.csv(url)
npc <- predict(wisc.pr, newdata=new)
npc

plot(wisc.pr$x[,1:2])
points(npc[,1], npc[,2], col="blue", pch=16, cex=3)
text(npc[,1], npc[,2], c(1,2), col="white")

sessionInfo()
