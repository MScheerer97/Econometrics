################ Seminar Paper - Econometrics - Winter Term 2021/2022

######## Name: Martin Scheerer
######## Student ID: 5631373

# Libraries  --------------------------------------------------------------
rm(list = ls())

###### k-means Clustering on npi
## cluster - cases/deaths
## cluster - worldmap

if (!require("tidyverse")) install.packages("tidyverse")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tibble")) install.packages("tibble")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("cluster")) install.packages("cluster")
if (!require("ggrepel")) install.packages("ggrepel")
if (!require("factoextra")) install.packages("factoextra")
if (!require("devEMF")) install.packages("devEMF")


library(tidyverse)
library(dplyr)
library(tibble)
library(ggplot2)
library(cluster)
library(ggrepel)
library(factoextra)
library(devEMF)

# # prepare data for clustering: mean 0 and std 1 to normalize data
# 
# measures <- cross_sec[, c(1:18, 40:41)]
# rownames(measures) <- cross_sec$Country
# 
# colnames(measures[, c(-1:-3, -19:-20)])
# distance <- as.data.frame(lapply(measures[, c(-1:-3, -19:-20)], scale))
#   
# # k-means clustering)
# 
# fviz_nbclust(distance, kmeans, method = "wss")
# 
# set.seed(123)
# kmeans <- kmeans(distance, 4, nstart = 25)
# clusters <- data.frame(cluster = kmeans$cluster, row.names = NULL)
# 
# measures$cluster <- as.factor(as.character(clusters$cluster))

#### Case-Death Plot with cluster coloring

# cases_deaths_plot <- ggplot(measures, aes(`Cases Per 10 Million`, `Deaths Per 100 Thousand`)) +
#   geom_jitter(height = 10, size = 6, aes(colour = cluster)) + 
#   scale_color_viridis_d(alpha = 0.45) +
#   geom_smooth(method = "lm", se = FALSE) + 
#   geom_text_repel(aes(label = Country), angle = 20, nudge_y  = ifelse(measures$Country == "Australia", 10, 0), 
#             nudge_x = ifelse(measures$Country == "Korea, Rep.", 250000, 0)) +
#   scale_x_continuous(breaks = seq(0, 3000000, 500000)) + 
#   scale_y_continuous(breaks = seq(0, 400, 50)) + 
#   labs(x = "Total cases per 10 million population", y = "Total deaths per 100 thousand population") +
#   theme_classic() +
#   theme(legend.position = c(0.9, 0.5)) +
#   guides(colour = guide_legend(title = "Cluster\nmembership\n", title.position = "top"), 
#          label.hjust = 0.5) 
# 
# ggsave("figures/deaths_cases_cluster.png", width = 14) 

#### Hierarchical Clustering

cross_sec <- readRDS("Output/cross_section_data.rds") 
  

measures <- scale(cross_sec[, 4:16])
rownames(measures) <- cross_sec$Country

distance <- dist(measures, method = "euclidean")

# ## Clustering: Agglomerative - Single Linkage
# 
# hc1 <- hclust(distance, method = "single")
# hc1 <- as.dendrogram(hc1)
# 
# nodePar <- list(lab.cex = 0.6, pch = c(19, 19), cex = 0.8, col = "blue")
# emf(file = "figures/deaths_cases_cluster_single_linkage.emf")
# 
# par(mar = c(5, 1, 1, 5))
# plot(hc1,  xlab = "Height",nodePar = nodePar, horiz = TRUE)
# 
# dev.off()


## Clustering: Agglomerative - Average Linkage

hc <- hclust(distance, method = "average")
hc2 <- as.dendrogram(hc)

nodePar <- list(lab.cex = 0.6, pch = c(19, 19), cex = 0.8, col = "blue")
emf(file = "figures/deaths_cases_cluster_average_linkage.emf")

par(mar = c(5, 1, 1, 5))
plot(hc2, xlab = "Height", nodePar = nodePar, horiz = TRUE, type = "rectangle")

dev.off()

# Create Screeplot

heights <- as.data.frame(cbind(sort(hc$height, decreasing=T), 1:length(hc$height)))
colnames(heights) <- c("Height", "Clusters")

scree <- ggplot(heights[1:10, ], aes(x = Clusters, y = Height)) +
  geom_point() +
  geom_line(color = "blue") +
  labs(x = "Number of clusters", y = "Height") +
  scale_x_continuous(breaks = seq(1, 10, 2)) +
  theme_classic() +
  theme(axis.title.x = element_text(margin = margin(t = 10)), 
        axis.title.y = element_text(margin = margin(r = 10)))

scree

## Add cluster to data

clusters <- cutree(hc, k = 5)
names(clusters) <- NULL

clust <- cross_sec %>%
  select(Country, Cumulative_cases, Cumulative_deaths, pop_per_10_million, pop_per_one_hundred_k) %>%
  mutate("Cases Per 10 Million" = round(Cumulative_cases/pop_per_10_million, 2), 
         "Deaths Per 100 Thousand" = round(Cumulative_deaths/pop_per_one_hundred_k, 2)) %>%
  select(-Cumulative_cases, -Cumulative_deaths, -pop_per_one_hundred_k, -pop_per_10_million) %>%
  arrange(Country) 
clust$Cluster <- as.factor(clusters)

## Case-Death Plot with cluster coloring

cases_deaths_cluster <- ggplot(clust, aes(`Cases Per 10 Million`, `Deaths Per 100 Thousand`)) +
   geom_jitter(height = 10, size = 6, aes(colour = Cluster)) + 
   scale_color_viridis_d(alpha = 0.65, option = "D") +
   geom_smooth(method = "lm", se = FALSE) + 
   geom_text_repel(aes(label = Country), angle = 20, nudge_y  = ifelse(clust$Country == "Australia", 12, 0), 
             nudge_x = ifelse(clust$Country == "Korea, Rep.", 235000, 0)) +
   scale_x_continuous(breaks = seq(0, 3000000, 500000)) + 
   scale_y_continuous(breaks = seq(0, 400, 50)) + 
   labs(x = "Total cases per 10 million population", y = "Total deaths per 100 thousand population") +
   theme_classic() +
   theme(legend.position = c(0.9, 0.5)) +
   guides(colour = guide_legend(title = "Cluster\nmembership\n", title.position = "top"), 
          label.hjust = 0.5) 
 

ggsave("figures/deaths_cases_cluster.png", width = 14) 






