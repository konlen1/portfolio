#install necessary packages
install.packages("readxl")
install.packages("tidyverse")
install.packages("gt")
install.packages("cluster")
install.packages("ggpubr")
install.packages("factoextra")
#load packages
library(readxl)
library(tidyverse)
library(gt)
library(cluster)
library(ggpubr)
library(factoextra)
#importing data from Excel
imported <- read_excel(file.choose())
#explore data set
names(imported)
summary(imported)
#save data set without id variable
df <- imported[-c(1)]
names(df)
##Segmentation step
#standardize values
dfz <- scale(df)
## Ward Hierarchical Clustering
# calculate distance matrix with euclidian distance
dis <- dist(dfz, method = "euclidean")
#clustering algorithm
fit <- hclust(dis, method="ward.D2")
# display dendrogram
plot(fit)
# cut dendrogram into 4 clusters
cluster <- cutree(fit, k=4)
#explore clusters
cluster
table(cluster)

# draw dendogram with red borders around the 4 clusters
rect.hclust(fit, k=4, border="red")
#add cluster to original data set
df_final <- cbind(df, cluster)
names(df_final)
View(df_final)

##Description step
#calculate segment size in percentages
proportions <- table(df_final$cluster)/length(df_final$cluster)
percentages <- proportions*100
percentages
#Explore mean values of variables in clusters
segments <- df_final %>%
  group_by(cluster) %>%
  summarise_at(vars(time, time_2, time_total, buy_past, hist_sales, sales_freq, rel_length, referrals, image, diag_flex, softw_integr, techn_assis, price, trust_tv, trust_radio, trust_online, trust_magazine, trust_peer),
               list(M = mean))
segments
#Create simple table with mean values
segments %>%
  gt() %>%
  tab_header(
    title = md("Mean Values for Clusters"))
fit_2 <- kmeans(df_final, 4)
clusplot(df_final, fit_2)
