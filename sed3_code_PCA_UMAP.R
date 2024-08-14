
#step1: load library
library(Seurat)
library(dplyr)
library(cowplot)
library(reshape2)

#step2:input data
object.data <-read.table(file="total_stat.xls",check.names=F)
#object.data <- t(object.data)
object_name <- CreateSeuratObject(counts = object.data, min.cells = 1, min.features = 1)
object_name <- NormalizeData(object = object_name, normalization.method = "LogNormalize",scale.factor = 1000)
object_name <- FindVariableFeatures(object = object_name, selection.method = "vst", nfeatures = 1000)

# Scaling the data
all.genes <- rownames(x = object_name)
object_name <- ScaleData(object = object_name, features = all.genes)
object_name <- RunPCA(object = object_name, features = VariableFeatures(object = object_name),npcs = 100)

#Cluster the cells
object_name <- FindNeighbors(object = object_name, dims = 1:100)
object_name <- FindClusters(object = object_name, resolution = 2)

#PCA
pca <- RunPCA(object_name, features = VariableFeatures(object = object_name))
pdf("PCA_out.pdf")
DimPlot(pca, reduction = "pca") #,split.by = 'ident')
dev.off()

#Run non-linear dimensional reduction (UMAP/tSNE)
object_u <- RunUMAP(object = object_name, dims = 1:100)
pdf("umap_out.pdf")
DimPlot(object = object_u, reduction = "umap",label=TRUE)
dev.off()

#tSNE
object_t <- RunTSNE(object = object_name, check_duplicates = FALSE, dims = 1:100)
pdf("tSNE_out.pdf")
DimPlot(object = object_t, reduction = "tsne")
dev.off()

saveRDS(object_u, file = "umap_out.rds")
data <- readRDS("umap_out.rds")
cluster_ID=as.data.frame(Idents(object = data))
cluster_cor= as.data.frame(Embeddings(object = data,reduction = "umap"))
res=cbind(cluster_ID,cluster_cor)
write.table(res,"umap_cellcoor.txt",sep="\t",quote = FALSE)
nordata <- as.data.frame(as.matrix(GetAssayData(object= data)))
write.table(nordata,"umap_normalized.txt",sep="\t",quote = FALSE)

saveRDS(object_t, file = "tSNE_out.rds")
data <- readRDS("tSNE_out.rds")
cluster_ID=as.data.frame(Idents(object = data))
cluster_cor= as.data.frame(Embeddings(object = data,reduction = "tsne"))
res=cbind(cluster_ID,cluster_cor)
write.table(res,"tSNE_cellcoor.txt",sep="\t",quote = FALSE)
nordata <- as.data.frame(as.matrix(GetAssayData(object= data)))
write.table(nordata,"tSNE_normalized.txt",sep="\t",quote = FALSE)

