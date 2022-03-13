Structural Bioinformatics
================

library(bio3d) pdb \<- read.pdb(“1hsg”) pdb attributes(pdb)

library(bio3d) aa \<- get.seq(“1ake_A”) aa b \<- blast.pdb(aa) hits \<-
plot(b)
head(hits*p**d**b*.*i**d*)*h**i**t**s* \<  − *N**U**L**L**h**i**t**s*pdb.id
\<-
c(‘1AKE_A’,‘6S36_A’,‘6RZE_A’,‘3HPR_A’,‘1E4V_A’,‘5EJE_A’,‘1E4Y_A’,‘3X2S_A’,‘6HAP_A’,‘6HAM_A’,‘4K46_A’,‘3GMT_A’,‘4PZL_A’)
files \<-
get.pdb(hits$pdb.id, path="pdbs", split=TRUE, gzip=TRUE) pdbs \<- pdbaln(files, fit = TRUE)#, exefile="msa") ids \<- basename.pdb(pdbs$id)
plot(pdbs, labels=ids)
