library(ggdendro)
library(cluster)
library(factoextra)
View(Deep_sea_data1)
Deep_sea_data1 <- read_excel("Neha patel/Deep_sea_data1.xlsx", 
                             +     sheet = "Natural")
DSD_synth<-data.frame(Deep_sea_data1, row.names = "Zones")
View(DSD_synth)
DSD.pca <- prcomp(DSD_synth[,c(8:10),11], center = TRUE,scale. = TRUE)
distance_mat <- dist(DSD_synth, method = 'euclidean')
Hierar_cl <- hclust(distance_mat, method = "average")
ggdendrogram(Hierar_cl, rotate = FALSE, size = 12, lwd = 10)
distance <- get_dist(DSD_synth)
library(cluster)
library(factoextra)
#create plot of number of clusters vs total within sum of squares
fviz_nbclust(DSD_synth, kmeans, method = "wss",k.max = 4)+geom_vline(xintercept = 3, 
                                                                     linetype = 2)


fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
k2 <- kmeans(DSD_synth, centers = 2, nstart = 25)
fviz_cluster(k2, data = DSD_synth, pointsize = 5, labelsize = 24,
             main = "Cluster plot for HM data", 
             ggtheme = theme_classic())
fviz_cluster(k2, data = DSD_synth, pointsize = 5, labelsize = 24,
             main = "Cluster plot for Natural data", 
             ggtheme = theme_classic(),cex.axis=5.0)
fviz_cluster(k2, data = DSD_synth, pointsize = 5, labelsize = 24,
             main = "Cluster plot for Semisynthetic data",
             ggtheme = theme_classic())
k2
