library(cluster)
dist_std <- dist(df_z, method = "euclidean")
hc_z <- hclust(dist_std, method = "ward.D2")
plot(hc_z)
hc_tree <- cutree(hc_z, k=3)
rect.hclust(hc_z, k = 3, border = 2:5)
fviz_cluster(list(data = df_z, cluster = hc_tree))
View(df_z)
##################################
hc.cutt <- hcut(df_z, k = 2, hc_method = "ward.D2")
fviz_cluster(hc.cutt, geom = "point", ellipse.type = "norm"  )
#################################
hc_tree
table(hc_tree)
cluster_ward <- cbind(smartwatch, hc_tree )
cluster_ward
cluster_ward %>%
  group_by(hc_tree) %>%
  summarise_all(list(mean))
unique(df$data_part2)
table(cluster_ward$Age)
#########################################################
View(df)
df_age_binary<- cbind(df[-12], age_binary[1:4])
View(df_age_binary)
df_age_binary<- df_age_binary[c(-14)]
df_age_binary_scaled<- scale(df_age_binary)
### k - means 
fviz_nbclust(df_z, kmeans, "wss")+
  labs(subtitle = "Elbow method")
km_amk <- kmeans(df_z, centers = 2, nstart = 100)
print(km_amk)
km_cluster <- km_amk$cluster
fviz_cluster(list(data = df_z, cluster = km_cluster))
summarise(km_amk)
km_main_table <- cbind(smartwatch, km_cluster)
ab <- km_main_table %>%
  group_by(km_cluster) %>%
  summarise_all(list(mean))
ab
count(df_z, cluster)
km_amk
View(km_main_table)
###########
fviz_nbclust(df_age_binary_scaled, kmeans, "wss")+
labs(subtitle = "Elbow method")
km_amk <- kmeans(df_z, centers = 2, nstart = 100)
print(km_amk)
km_cluster <- km_amk$cluster
fviz_cluster(list(data = df_age_binary_scaled, cluster = km_cluster))
summarise(km_amk)
km_main_table <- cbind(smartwatch, km_cluster)
ab <- km_main_table %>%
  group_by(km_cluster) %>%
  summarise_all(list(mean))
ab
count(df_z, cluster)
km_amk
View(km_main_table)##############################################

####################################################
install.packages("xlsx")
library(xlsx)
library(cluster)
pam_k2 <- pam(df_z, k = 2)
pam_k2$silinfo$widths
k2_plot_sil <- silhouette(pam_k2)
plot(k2_plot_sil)
pam_k2$silinfo$avg.width
sil_witdh <- map_dbl(2:10, function(k){
  model<-pam(x=df_z, k=k)
  model$silinfo$avg.width
})
sil_df <- data.frame(
  k=2:10,
  sil_witdh = sil_witdh
)
print(sil_witdh)
library(cluster)
library(factoextra)
ddd <- dentmax[c(-1)]
view(ddd)
dddzz<- scale(ddd)
dist_std_dentmax <- dist(dddzz, method = "euclidean")
hc_z_dentmax <- hclust(dist_std_dentmax, method = "ward.D2")
plot(hc_z)
hc_tree_dentmax <- cutree(hc_z_dentmax, k=4)
rect.hclust(hc_z, k = 3, border = 2:5)
fviz_cluster(list(data = dddzz, cluster = hc_tree_dentmax))


pam_dentmax <- pam(dddzz, k = 4)
pam_dentmax <- pam(dddzz, k = 4)
pam_dentmax$silinfo$widths
k2_plot_sil <- silhouette(pam_dentmax)
plot(k2_plot_sil)
pam_k2$silinfo$avg.width
sil_witdh <- map_dbl(2:10, function(k){
  model<-pam(x=dddzz, k=k)
  model$silinfo$avg.width
})
sil_df <- data.frame(
  k=2:10,
  sil_witdh = sil_witdh
)
print(sil_witdh)
view(smartwatch)

d_scaled <- smartwatch[c(-1)]
sss <- d_scaled[c(-5)]
sdsd <- sss[c(-6:-10)]
view(sdsd)
qrqr <- scale(sdsd)

view(d_scaled)
dist_scaled_shit <- dist(df_income_scaled, method = "euclidean")
scaled_shit_tree <- hclust(dist_scaled_shit, method = "ward.D2")
plot(scaled_shit_tree)
hc_shit_cluster <- cutree(scaled_shit_tree, k=3)
rect.hclust(scaled_shit_tree, k = 3, border = 2:4)
fviz_cluster(list(data = df_income_scaled, cluster = hc_shit_cluster))

fviz_nbclust(d_scaled, hcut, "euclidean")+
  labs(subtitle = "Elbow method")
km_amk <- kmeans(d_scaled, centers = 10, nstart = 100)
print(km_amk)
km_cluster <- km_amk$cluster
fviz_cluster(list(data = d_scaled, cluster = km_cluster))
summarise(km_amk)
km_main_table <- cbind(smartwatch, km_cluster)
ab <- km_main_table %>%
  group_by(km_cluster) %>%
  summarise_all(list(mean))
ab
############################################################################
View(smartwatch)
data_part2_v1 <- smartwatch[,12]
data_part2_v1
table(smartwatch$Age)
grp_age <- cut(c(data_part2_v1$Age), labels = c("24,29", "30,35", "36,40", "41,47"), breaks = 4 )
data_part2_v1
fin_grpage <- cbind(data_part2_v1, grp_age)
View(fin_grpage)
factor(x=fin_grpage$grp_age, levels = c("24,29", "30,35", "36,40", "41,47"), ordered = TRUE)
library(fastDummies)
binary_dums_age <- dummy_cols(fin_grpage, select_columns = c("grp_age"))
age_binary <- binary_dums_age[c(3:6)]
View(age_binary)
View(binary_dums_age)
View(df)
asdqwe <- df[c(-9)]
View(df)
ass <- df[8]
binary_income <- dummy_cols(ass, select_columns = "Income")
binary_income
View(binary_income)
?factor
exclude_age <- df[-c(9)]
view(exclude_age)
exclude_age_std <- scale(exclude_age)


dist_std_excage <- dist(exclude_age_std, method = "euclidean")
hc_z_ex <- hclust(dist_std_excage, method = "ward.D2")
plot(hc_z)
hc_tree_ex <- cutree(hc_z_ex, k=7)
rect.hclust(hc_z, k = 3, border = 2:5)
fviz_cluster(list(data = exclude_age_std, cluster = hc_tree_ex))
View(df_z)
hc_tree

View(df_z)
dist_std <- dist(df_z, method = "euclidean")
hc_z <- hclust(dist_std, method = "ward.D2")
plot(hc_z)
hc_tree <- cutree(hc_z, k=2)
rect.hclust(hc_z, k = 2, border = 2:4)
ad<- cbind(smartwatch, hc_tree)
fviz_cluster(list(data = ad, cluster = hc_tree))
plot(smartwatch, col = hc_tree)
View(df_z)
View(d_scaled)
View(aaq)
aaq<- smartwatch[c(-8:-10)]
aaq_scaled<- scale(aaq)
d_scaled_really<- scale(d_scaled)
######################################################################
library(fastDummies)
############################# directly use smartwatch. 
View(smartwatch)
dist_gower <- daisy(smartwatch, metric = "gower")
hc_gower <- hclust(dist_gower, method = "ward.D2")
plot(hc_gower)
tree_gower <- cutree(hc_gower, k=3)
fviz_cluster(list(data = smartwatch, cluster = tree_gower))
################################ normalization except binary attributes. 
non_binary <- smartwatch[c(-8:-10)]
non_binary_scaled<- scale(non_binary)
nonbinaryscaled_normalbinaries<- cbind(non_binary_scaled, smartwatch[c(8:10)])
view(nonbinaryscaled_normalbinaries)
dist_gower_ver2<- daisy(nonbinaryscaled_normalbinaries, metric = "gower")
hc_gower2<- hclust(dist_gower_ver2, method = "ward.D2")
plot(hc_gower2)
gower2_tree <- cutree(hc_gower2, k=4)
fviz_cluster(list(data=nonbinaryscaled_normalbinaries, cluster = gower2_tree))
###############################################################################use dummyis for just binaries.
binary_dums<- dummy_cols(smartwatch, select_columns = c("AmznP", "Female", "Degree"))
View(binary_dums)
binary_dums_final <- binary_dums[c(-8:-10)]
View(binary_dums_final)
binary_dums_final_scaled <- scale(binary_dums_final)
dist_bdfs<- dist(binary_dums_final_scaled, method = "euclidean")
hc_bdfs<- hclust(dist_bdfs, method = "average")
plot(hc_bdfs)
################################################################################use just 1-7 questions.
df_1_7 <- smartwatch[c(1:7)]
df_1_7_scaled<- scale(df_1_7)
dist_1_7<- dist(df_1_7_scaled, method = "euclidean")
hc_1_7<- hclust(dist_1_7, method = "ward.D2")
plot(hc_1_7)
tree_1_7<- cutree(hc_1_7, k=3)
fviz_cluster(list(data=df_1_7_scaled, cluster = tree_1_7))
################################################################################add income to 1-7 questions. 
df_income <- cbind(df_1_7, smartwatch[11])
View(df_income)
df_income_scaled <- scale(df_income)
dist_income<- dist(df_income_scaled, method = "euclidean")
hc_income <- hclust(dist_income, method = "ward.D2")
plot(hc_income)
tree_income<- cutree(hc_income, k=3)
fviz_cluster(list(data=df_income_scaled, cluster = tree_income))
#########################################################################2-7 and income and age. 
df_minus1_income<- cbind(df_income[c(-1)], smartwatch[12])
names(df_minus1_income)
df_minus1_scaled<- scale(df_minus1_income)
dist_minus1 <- dist(df_minus1_scaled, method = "euclidean")
hc_minus1<- hclust(dist_minus1, method = "ward.D2")
plot(hc_minus1)
tree_minus1<- cutree(hc_minus1, k=2)
fviz_cluster(list(data=df_minus1_scaled, cluster = tree_minus1))
########################################################################1-3-4-5-6-7 questions. +income+age
view(smartwatch)
df_2nd<- cbind(smartwatch[1], smartwatch[c(3:7)], smartwatch[c(11:12)])
df_2nd_scale<- scale(df_2nd)
dist_2nd<- dist(df_2nd_scale, method = "euclidean")
hc_2nd<- hclust(dist_2nd, method = "ward.D2")
plot(hc_2nd)
tree_2nd<- cutree(hc_2nd, k=2)
fviz_cluster(list(data=df_2nd_scale, cluster = tree_2nd))
######################################################################without 4 +income+age
df_4th <- cbind(smartwatch[c(1:3)], smartwatch[c(5:7)], smartwatch[c(11:12)])
df_4th_scaled<- scale(df_4th)
dist_4h<- dist(df_4th, method = "euclidean")
hc_4th<- hclust(dist_4h, method = "ward.D2")
plot(hc_4th)
tree_fourth<- cutree(hc_4th, k=2)
fviz_cluster(list(data=df_4th_scaled, cluster = tree_fourth))
#####################################################################without 4 and 6 +income+age
df_without_4_6<- cbind(smartwatch[c(1:3)], smartwatch[5],smartwatch[7],smartwatch[c(11:12)])
df_4_6_scaled<- scale(df_without_4_6)
dist_4_6<- dist(df_4_6_scaled, method = "euclidean")
hc_4_6<- hclust(dist_4_6, method = "ward.D2")
plot(hc_4_6)
tree_4_6<- cutree(hc_4_6, k=2)
fviz_cluster(list(data=df_4_6_scaled, cluster = tree_4_6))
####################################################################without 1=2=4 +income+age
df_1_2_4<- cbind(smartwatch[3], smartwatch[c(5:7)], smartwatch[c(11:12)])
names(df_1_2_4)
df_1_2_4_scaled<- scale(df_1_2_4)
dist_1_2_4<- dist(df_1_2_4_scaled, method = "euclidean")
hc_1_2_4<- hclust(dist_1_2_4, method = "ward.D2")
plot(hc_1_2_4)
tree_1_2_4<- cutree(hc_1_2_4, k=2)
fviz_cluster(list(data= df_1_2_4_scaled, cluster = tree_1_2_4))
#########################################################################without 2-4+income
df_az <- cbind(smartwatch[1], smartwatch[3], smartwatch[c(5:7)], smartwatch[11])
names(df_az)
df_az_scaled<- scale(df_az)
dist_az<- dist(df_az_scaled, method = "euclidean")
hc_az<- hclust(dist_az, method = "ward.D2")
plot(hc_az)
tree_az<- cutree(hc_az, k=4)
fviz_cluster(list(data=df_az_scaled, cluster = tree_az))
######################################################################## without 1-3-7+income
df_aq<- cbind(smartwatch[2], smartwatch[c(7)], smartwatch[11])

df_aq_scaled<- scale(df_aq)
dist_aq<- dist(df_aq_scaled, method = "euclidean")
hc_aq<- hclust(dist_aq, method = "ward.D2")

plot(hc_aq)
tree_aq<- cutree(hc_aq, k=3)
fviz_cluster(list(data=df_aq_scaled, cluster = tree_aq))
#########################################################################without 3-4+income
df_aqq<- cbind(smartwatch[c(1:2)], smartwatch[c(11:12)], smartwatch[7])
df_scale_aqq<- scale(df_aqq)
dist_aaq<- dist(df_scale_aqq, method = "euclidean")
hc_aaq<- hclust(dist_aaq, method = "ward.D2")
plot(hc_aaq)
tree_aaq<- cutree(hc_aaq, k=3)
fviz_cluster(list(data=df_scale_aqq, cluster=tree_aaq))
#########################################################################without 4-5
df_aw<- cbind(smartwatch[c(2:3)], smartwatch[c(7)], smartwatch[c(11)])
df_aw_scaled<- scale(df_aw)
dist_aw<- dist(df_aw_scaled, method = "euclidean")
hc_aw<- hclust(dist_aw, method = "ward.D2")
plot(hc_aw)
tree_aw<- cutree(hc_aw, k=2)
fviz_cluster(list(data=dddzz, cluster = hc_tree_dentmax))
####################################################################################
summary(smartwatch)
class(smartwatch)
summarise_all(smartwatch)
####################################################################################
df_aw<- cbind(smartwatch[c(11:12)], smartwatch[c(1)], smartwatch[c(7)])
df_aw_scaled<- scale(df_aw)
hc.cut <- hcut(dum_tgthr_scale, k = 13, hc_method = "ward.D2")
fviz_cluster(hc.cut, geom = "point", ellipse.type = "norm"  )
#########################################################################################
df_ab<- cbind(smartwatch[c(1:4)])
hc.cut <- hcut(dum_together, k = 13, hc_method = "ward.D2")
fviz_cluster(hc.cut, geom = "point", ellipse.type = "norm"  )
##########################################################################################
df_aa<- cbind(smartwatch[c(1:10)], smartwatch[c(12)])
df_aa_scaled<- scale(df_aw)
hc.cutt <- hcut(factor_scale, k = 2, hc_method = "ward.D2")
fviz_cluster(hc.cutt, geom = "point", ellipse.type = "norm"  )
#############################################################################################
sil_witdh <- map_dbl(2:13, function(k){
  model_k6 <- pam(x = df_z, k = k)
  model_k6$silinfo$avg.width
})
sil_df <- data.frame(
  k = 2:13,
  sil_witdh = sil_witdh
)
print(sil_df)
#####################################################################################untitled 3 
ver_one <- scale(smartwatch)
head(ver_one)
ver_one_dist <- dist(ver_one, method = "euclidean")
ver_one_hc<- hclust(ver_one_dist, method = "ward.D2")
plot(ver_one_hc)
rect.hclust(ver_one_cluster, k=4, border = "red")
ver_one_tree<- cutree(ver_one_hc, k=4)
fviz_cluster(list(data=ver_one, cluster = ver_one_tree))
'silhuette'
sil_witdh <- map_dbl(2:13, function(k){
  model_k6 <- pam(x = ver_one, k = k)
  model_k6$silinfo$avg.width
})
sil_df <- data.frame(
  k = 2:13,
  sil_witdh = sil_witdh
)
print(sil_df)###############silhoutte around 0.15
ggplot(sil_df, aes(x=k, y=sil_witdh)) + 
  geom_line() +
  scale_x_continuous(breaks = 2:13)
######################################################################
sil_witdh <- map_dbl(2:13, function(k){
  model_k6 <- pam(x = df_age_binary_scaled, k = k)
  model_k6$silinfo$avg.width
})
sil_df <- data.frame(
  k = 2:13,
  sil_witdh = sil_witdh
)
print(sil_df)####### silhoutte >>>> around 0.13
ggplot(sil_df, aes(x=k, y=sil_witdh)) + 
  geom_line() +
  scale_x_continuous(breaks = 2:13)
##################################################################################### silhoutte around 0.17
sil_witdh <- map_dbl(2:13, function(k){
  model_k6 <- pam(x = exclude_age_std, k = k)
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
#################################################################silhoutte around 0.2
sil_witdh <- map_dbl(2:13, function(k){
  model_k6 <- pam(x = qrqr, k = k)
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
###############################################################silhoutte around 0,4 for k=2 without first parameter.
View(d_scaled)
sil_witdh <- map_dbl(2:13, function(k){
  model_k6 <- pam(x = d_scaled_really, k = k)
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
#######################################################################0.2 silhoutte without binary variables
sil_witdh <- map_dbl(2:13, function(k){
  model_k6 <- pam(x = aaq_scaled, k = k)
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
###################################################DENTMAX SILHOUETTE >>> 0.67 !
sil_witdh <- map_dbl(2:13, function(k){
  model_k6 <- pam(x = dddzz, k = k)
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
###################################################0.144 just binaries dummy. 
sil_witdh <- map_dbl(2:13, function(k){
  model_k6 <- pam(x = nonbinaryscaled_normalbinaries, k = k)
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
#######################################################0.215 1 to 7 questions.
sil_witdh <- map_dbl(2:13, function(k){
  model_k6 <- pam(x = df_1_7_scaled, k = k)
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
#########################################################1 to 7 question+ income 0.212 k=3 harita okay gibi
sil_witdh <- map_dbl(2:13, function(k){
  model_k6 <- pam(x = df_income_scaled, k = k)
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
##################################################################2-7 +income + age>>> k=2 0.26
sil_witdh <- map_dbl(2:13, function(k){
  model_k6 <- pam(x = df_minus1_scaled, k = k)
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
################################################################1-3-4-5-6-7 questions. +income+age>>>k=2 0.26
sil_witdh <- map_dbl(2:13, function(k){
  model_k6 <- pam(x = df_2nd_scale, k = k)
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
##################################################################without 4 +income+age> k=2 0.26
sil_witdh <- map_dbl(2:13, function(k){
  model_k6 <- pam(x = df_2nd_scale, k = k)
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
############################################################################without 4 +income+age k=2 >> 0.25
sil_witdh <- map_dbl(2:13, function(k){
  model_k6 <- pam(x = df_4_6_scaled, k = k)
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
############################################################################without 1=2=4 +income+age k=2 0.32
sil_witdh <- map_dbl(2:13, function(k){
  model_k6 <- pam(x = df_1_2_4_scaled, k = k)
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
###############################################################################without 2-4+income
sil_witdh <- map_dbl(2:13, function(k){
  model_k6 <- pam(x = df_az_scaled, k = k)
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
################################################################################without 1-3-7+income
sil_witdh <- map_dbl(2:13, function(k){
  model_k6 <- pam(x = df_aq_scaled, k = k)
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
###############################################################################without 3-4+income k=2 0.25
sil_witdh <- map_dbl(2:13, function(k){
  model_k6 <- pam(x = df_aw_scaled, k = k)
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
###############################################################################

sil_witdh <- map_dbl(2:13, function(k){
  model_k6 <- pam(x = df_1_2_4_scaled, k = k)
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
#################################################################################
sil_witdh <- map_dbl(2:13, function(k){
  model_k6 <- pam(x = df_1_2_4_scaled, k = k)
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
##################################################################################
sil_witdh <- map_dbl(2:13, function(k){
  model_k6 <- pam(x = df_1_2_4_scaled, k = k)
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
#####################################################################################
is.factor(smartwatch$TimelyInf)
names(smartwatch)
f_constcom <- factor(smartwatch$ConstCom)

f_timelyinf<- factor(smartwatch$TimelyInf)
f_taskmgm<- factor(smartwatch$TaskMgm)
f_devicest<- factor(smartwatch$DeviceSt)
f_wellnes<- factor(smartwatch$Wellness)
exp<- cbind(f_constcom, f_timelyinf)
view(dummy_income)
view(dum_together)
view(binary_dums_final)
df<- cbind(smartwatch[c(1:7)], dum_amzn, dum_gender, dum_degree,smartwatch[11:12])
class(main_variable_sensitive)
scaled_main_variable <- scale(main_variable_sensitive)
factor_1<- cbind(factor_data[1], factor_data[3], factor_data[7])
factor_11<- scale(factor_1)
#############################################
view(factor_data)
scaled_factor<- scale(factor_data)
##################

class(f_taskmgm)
dummy_income<- dummy_cols(main_variable_sensitive[8])
###################################################
dum_income<- dummy_income[c(-1)]
dum_amzn<- binary_dums_final[c(10:11)]
dum_gender<- binary_dums_final[c(12:13)]
dum_degree<- binary_dums_final[c(14:15)]
dum_together <- cbind(dum_income, dum_amzn, dum_gender, dum_degree)
dum_binary<- cbind(dum_amzn, dum_gender, dum_degree)
dum_tgthr_scale<- scale(dum_together)
a<-scale(f_athlete)
f_athlete <- factor(smartwatch$Athlete)
f_style <- factor(smartwatch$Style)
f_income <- factor(smartwatch$Income)
##################factor variables
factor_data<- cbind(f_constcom, f_timelyinf, f_taskmgm, f_devicest, f_wellnes, f_style, f_income)
summary(factor_data)
fac_scaled<- scale(factor_data)
view(fac_scaled)
#####################################################################
clasic_df<- cbind(smartwatch[1:7], smartwatch[c(11)], dum_amzn)
clasic_scale<- scale(clasic_df)
############################################################################
factor_df<- cbind(f_style, f_constcom, f_wellnes)
factor_scale<- scale(factor_df)
exp_df <- cbind(smartwatch[7], smartwatch[1], smartwatch[11], smartwatch[6])
exp_scale<- scale(exp_df)
#######################################################################
sil_witdh <- map_dbl(2:13, function(k){
  model_k6 <- pam(x = df_sample1, k = k)
  model_k6$silinfo$avg.width
})
sil_df <- data.frame(
  k = 2:13,
  sil_witdh = sil_witdh
)
print(sil_df)
####### silhoutte >>>> around 0.13
ggplot(sil_df, aes(x=k, y=sil_witdh)) + 
  geom_line() +
  scale_x_continuous(breaks = 2:13)
##############################################################################


