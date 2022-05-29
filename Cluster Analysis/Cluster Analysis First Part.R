## GETTING DATA
unique(smartwatch)
summary(smartwatch)
length(smartwatch)
smartwatch[,1]
### preparing data for cluster analysis
data_part1 <- smartwatch[, 1:7]
data_part2 <- smartwatch[, 11:12]
data_numeric <- cbind(data_part1, data_part2)
View(data_numeric)
### parsing binary and caterogical parameters
install.packages("fastDummies")
library(fastDummies)
binary_row <- smartwatch[, 8:10]
View(binary_row)
?dummy_cols
names(binary_row)
binary_dums <- dummy_cols(binary_row, select_columns = c("AmznP", "Female", "Degree"))
View(binary_dums)
binary_dums_final <- binary_dums[-c(1:3)]
View(binary_dums_final)
### CREATING FINAL DATAFRAME
df <- cbind(data_numeric, binary_dums_final)
View(df)
### Scaling parameters 
df_z <- scale(df)
View(df_z)
summary(df_z)
?hclust
### install mutating cluster columns to original dataset. 
install.packages("dplyr")
library(dplyr)
### install ggplot2 library for visualizing Clusters 
install.packages("ggplot2")
library(ggplot2)
### install dendextend for dying dendrograms according to its clusters
library(dendextend)
### load gt library for possible table creations 
library(gt)
### cluster library already loaded. 
### pam function for plotting silhouette plot inside of cluster library.
### library purr for Elbow plot (it is also inside of tidyverse)
### Tidyverse >>> ggplot2, for data visualisation.
# 1) dplyr, for data manipulation.
# 2) tidyr, for data tidying.
# 3) readr, for data import.
# 4) purrr, for functional programming.
# 5) tibble, for tibbles, a modern re-imagining of data frames.
# 6)stringr, for strings.
# 7)forcats, for factors.
library(tidyverse)

### Finding possible number of clusters 

### 1) dendrogram 2) elbow 3) silhouette 4) ggplot cluster visualization

### First iteration
watch_dist1 <- dist(df_z, method = "euclidean")
watch_clust1 <- hclust(watch_dist1, method = "complete") 
plot(watch_clust1)
### ward d2 version of dendrogram
watch_clust1_ward.d2 <- hclust(watch_dist1, method = "ward.D2")
plot(watch_clust1_ward.d2) ### ward.d2 has more clear clusters than complete method.


### Elbow Plot for ward.d2 by using library purr
### did not work. Error >> Result 1 must be a single double, not NULL of length 0.
### so i am trying load tidyr library.(did not work)
### error>> 'list' object cannot be coerced to type 'double'
### try to convert dataframe to numeric unlist version. 
### againg same error ! Result 1 must be a single double, not NULL of length 0

# <- as.numeric(unlist(df_z))  
library(tidyr)
###Use map_dbl to run many models with varying value of k (centers)
# total_withins_one <- map_dbl(1:10, function(k){
 # model_one <- kmeans(x = df_z_num, centers = k)
  #model_one$total_withins_one
# })
# elbow_one_df <- data.frame(
# k = 1:10,
# total_withins_one = total_withins_one
#)
### i give up trying to solve these issue using another elbow technique
### loaded factoextra library
library(factoextra)
?fviz_nbclust()
fviz_nbclust(df_z, kmeans, method = "wss", k.max = 15) + labs(subtitle = "Elbow Method")
### it suggests k=2. It failed.
fviz_nbclust(df_z, kmeans, method = "wss", k.max = 12) + labs(subtitle = "Elbow Method")
### same.
fviz_nbclust(df_z, method = "wss", hcut, k.max = 12) + labs(subtitle = "Elbow Method")
### when i changed kmeans argument to h cut it showed 2 and 6. 
### from dendrogram 6 was suitable for k=6. So, for first iteration elbow suggest k=6

### i tried hcut for 
#total_withins_one <- map_dbl(1:10, function(k){
#model_one <- hcut(x = df_z, centers = k)
#model_one$total_withins_one
#})
### but it also failed.


### using silhoutte in addition to elbow. 
pam_k6 <- pam(df_z, k = 6)
pam_k6$silinfo$widths
pam_k6_silplot <- silhouette(pam_k6)
plot(pam_k6_silplot) ## average silhoutte width : 0.14 (not satisfiying)
### Trying to find best k from graph. 
sil_witdh <- map_dbl(2:13, function(k){
  model_k6 <- pam(x = df_z, k = k)
  model_k6$silinfo$avg.width
})
sil_df <- data.frame(
  k = 2:13,
  sil_witdh = sil_witdh
)
print(sil_df)
ggplot(sil_df, aes(x=k, y=sil_witdh)) + 
  geom_line() +
  scale_x_continuous(breaks = 2:13)
### plot suggests k=6 > 0.13 , k=10 > 0.145, k=13 > 0.155
first_cluster <- cutree(watch_clust1_ward.d2, k = 6)
table(first_cluster)
segment_customer_1 <- mutate(df, cluster = first_cluster)
segment_customer_1 %>%
  group_by(cluster) %>%
  summarise_all(list(mean))
########################################################
second_cluster <- cutree(watch_clust1_ward.d2, k = 10)
table(second_cluster)
segment_customer_2 %>%
  group_by(cluster) %>%
  summarise_all(list(mean))

table(first_cluster)
mean(df$Age)
max(df$Age)
min(df$Age)
view(df)
view(df_z)
fviz_cluster(list(data=df_z, cluster= first_cluster))
fviz_cluster(list(data=df_z, cluster= second_cluster))
