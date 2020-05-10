#Gene expression analysis

#Adapted from scripts written by Michael I. Love, Simon Anders, and Wolfgang Huber - Analyzing RNA-seq data with DESeq2 (http://bioconductor.org/packages/release/bioc/vignettes/DESeq2/inst/doc/DESeq2.html)

#Load Libraries

library("DESeq2")
library("pheatmap")
library("ggplot2")

#Load and prepare data

countData <- read.csv("gene_demo.csv",header=TRUE,row.names=1, check.names = FALSE)
colData <- read.csv("Info_Demo.csv",header=TRUE,row.names=1, check.names = FALSE)
dds <- DESeqDataSetFromMatrix(countData = countData,colData = colData,design = ~ Mat_type + Day_Night)
dds <- DESeq(dds)

keep <- rowSums(counts(dds)) >= 10
dds <- dds[keep,]

vsd <- varianceStabilizingTransformation(dds, blind=FALSE)

#Plot Heatmap

select <- order(rowMeans(counts(dds,normalized=TRUE)),
decreasing=TRUE)[1:50]
df <- as.data.frame(colData(dds)[,c("Mat_type", "Day_Night")])
pheatmap(assay(vsd)[select,], cluster_rows=FALSE, show_rownames=TRUE,
cluster_cols=FALSE, annotation_col=df, cellwidth = 15, cellheight = 12, fontsize = 8)

#Plot PCA

pcaData <- plotPCA(vsd, intgroup=c("Mat_type","Day_Night"), returnData=TRUE)
percentVar <- round(100 * attr(pcaData, "percentVar"))
ggplot(pcaData, aes(PC1, PC2, color=Mat_type, shape=Day_Night)) +
  geom_point(size=3) +
  xlab(paste0("PC1: ",percentVar[1],"% variance")) +
  ylab(paste0("PC2: ",percentVar[2],"% variance")) +
  coord_fixed() + theme_classic()
