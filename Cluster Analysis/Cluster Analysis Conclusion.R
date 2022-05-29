library(cluster)
library(factoextra)
library(tidyverse)
set.seed(123)
view(df_income_scaled)
## fviz_nbclust(great, FUN = hcut, method = "silhouette")
fviz_nbclust(df_z, FUN = hcut, method = "silhouette")
fviz_nbclust(df_z, FUN = hcut, method = "gap_stat")
fviz_nbclust(df_z, FUN = hcut, "wss")+
  labs(subtitle = "Elbow method")
###
df_1_7<- cbind(df_z[c(1:7)])
fviz_nbclust(df_z, FUN = hcut, method = "silhouette")
hc.cutt <- hcut(df_z, k = 3, hc_method = "ward.D2")
fviz_cluster(hc.cutt, geom = "point", ellipse.type = "norm"  )

view(df_age_binary_scaled)
df_ar<- cbind(df_income_scaled
####
view(df)
dist_df_z<- dist(df_z, method = "euclidean")
hc_df_z<- hclust(dist_df_z, method = "ward.D2")
plot(hc_df_z)
tree_ver_two<- cutree(hc_df_z, h=65)
fviz_cluster(list(data= df_z, cluster = tree_ver_two))
table(tree_ver_two)
df_final_2<- mutate(df, cluster = tree_ver_two)
view(df_final_2)
names(df_final_2)
proportions_2<- table(df_final_2$cluster)/length(df_final_2$cluster)
percenteges_2<- proportions_2*100
percenteges_2
####
names(df_final_2)
segments_2 <- df_final_2 %>%
  group_by(cluster)%>%
  summarise_at(vars(ConstCom, TimelyInf, TaskMgm, DeviceSt, Wellness, Athlete, Style, AmznP_0, AmznP_1, Female_0, Female_1, Degree_1, Degree_2, Income, Age),
               list(M = mean))
segments_2
################################################################################################################
dist_df_z<- dist(df_z, method = "euclidean")
hc_df_z<- hclust(dist_df_z, method = "ward.D2")
plot(hc_df_z)
tree_ver_three<- cutree(hc_df_z, h=50)
fviz_cluster(list(data= df_z, cluster = tree_ver_three))
table(tree_ver_three)
df_final_3<- mutate(df, cluster = tree_ver_three)
view(df_final_3)
names(df_final_3)
proportions_3<- table(df_final_3$cluster)/length(df_final_3$cluster)
percenteges_3<- proportions_2*100
percenteges_3
####
names(df_final_3)
segments_3 <- df_final_3 %>%
  group_by(cluster)%>%
  summarise_at(vars(ConstCom, TimelyInf, TaskMgm, DeviceSt, Wellness, Athlete, Style, AmznP_0, AmznP_1, Female_0, Female_1, Degree_1, Degree_2, Income, Age),
               list(M = mean))
segments_3
#############################################################
data_part1 <- smartwatch[, 1:7]
data_part2 <- smartwatch[, 11:12]
data_numeric <- cbind(data_part1, data_part2)
View(data_numeric)
### parsing binary and caterogical parameters
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
view(df)
df_dummy<- dummy_cols(df, select_columns = c("Income"), remove_selected_columns = TRUE)
view(df_dummy)
df_dummy_scaled<- scale(df_dummy)
view(smartwatch)
##############################################################################
library(cluster)

fviz_nbclust(df_dummy_scaled, FUN = hcut, method = "silhouette")
fviz_nbclust(df_dummy_scaled, FUN = hcut, "wss")+
  labs(subtitle = "Elbow method")
hc.cutt <- hcut(df_dummy_scaled, k = 5, hc_method = "ward.D2")
fviz_cluster(hc.cutt, geom = "point", ellipse.type = "norm"  )
################################################################################k=2
dist_final<- dist(df_dummy_scaled, method = "euclidean")
hc_final<- hclust(dist_final, method = "ward.D2")
plot(hc_final)
tree_dummy_2<- cutree(hc_final, k=2)
fviz_cluster(list(data= df_dummy_scaled, cluster = tree_dummy_2))
table(tree_dummy_2)
df_dummy_final2<- mutate(df_dummy, cluster = tree_dummy_2)
View(df_dummy_final2)
names(df_dummy_final2)
proportions_dummy_2<- table(df_dummy_final2$cluster)/length(df_dummy_final2$cluster)
percenteges_dummy_2<- proportions_dummy_2*100
percenteges_dummy_2
segments_dummy_2 <- df_dummy_final2 %>%
  group_by(cluster)%>%
  summarise_at(vars(ConstCom, TimelyInf, TaskMgm, DeviceSt, Wellness, Athlete, Style, AmznP_0, AmznP_1, Female_0, Female_1, Degree_1, Degree_2, Age, Income_1, Income_2, Income_3, Income_4, Income_5),
               list(M = mean))

segments_dummy_2
#########################################################################################kkkkkkkkkkkk=3
dist_final_3<- dist(df_dummy_scaled, method = "euclidean")
hc_final_3<- hclust(dist_final_3, method = "ward.D2")
plot(hc_final_3)
tree_dummy_3<- cutree(hc_final_3, k=3)
fviz_cluster(list(data= df_dummy_scaled, cluster = tree_dummy_3))
table(tree_dummy_3)
df_dummy_final3<- mutate(df_dummy, cluster = tree_dummy_3)
view(df_dummy_final3)
names(df_dummy_final3)
proportions_dummy_3<- table(df_dummy_final3$cluster)/length(df_dummy_final3$cluster)
percenteges_dummy_3<- proportions_dummy_3*100
percenteges_dummy_3
segments_dummy_3 <- df_dummy_final3 %>%
  group_by(cluster)%>%
  summarise_at(vars(ConstCom, TimelyInf, TaskMgm, DeviceSt, Wellness, Athlete, Style, AmznP_0, AmznP_1, Female_0, Female_1, Degree_1, Degree_2, Age, Income_1, Income_2, Income_3, Income_4, Income_5),
               list(M = mean))
segments_dummy_3
###########################################################################################################
########################################################################################k=4
dist_final_4<- dist(df_dummy_scaled, method = "euclidean")
hc_final_4<- hclust(dist_final_4, method = "ward.D2")
plot(hc_final_4)
tree_dummy_4<- cutree(hc_final_4, k=4)
fviz_cluster(list(data= df_dummy_scaled, cluster = tree_dummy_4))
table(tree_dummy_4)
df_dummy_final4<- mutate(df_dummy, cluster = tree_dummy_4)
view(df_dummy_final4)
names(df_dummy_final4)
proportions_dummy_4<- table(df_dummy_final4$cluster)/length(df_dummy_final4$cluster)
percenteges_dummy_4<- proportions_dummy_4*100
percenteges_dummy_4
segments_dummy_4 <- df_dummy_final4 %>%
  group_by(cluster)%>%
  summarise_at(vars(ConstCom, TimelyInf, TaskMgm, DeviceSt, Wellness, Athlete, Style, AmznP_0, AmznP_1, Female_0, Female_1, Degree_1, Degree_2, Age, Income_1, Income_2, Income_3, Income_4, Income_5),
               list(M = mean))
segments_dummy_4
################################################################################ k=5
dist_final_5<- dist(df_dummy_scaled, method = "euclidean")
hc_final_5<- hclust(dist_final_5, method = "ward.D2")
plot(hc_final_5)
tree_dummy_5<- cutree(hc_final_5, k=5)
fviz_cluster(list(data= df_dummy_scaled, cluster = tree_dummy_5))
table(tree_dummy_5)
df_dummy_final5<- mutate(df_dummy, cluster = tree_dummy_5)
view(df_dummy_final5)
names(df_dummy_final5)
rect.hclust(hc_final_5, k = 6, border = 2:8)
proportions_dummy_5<- table(df_dummy_final5$cluster)/length(df_dummy_final5$cluster)
percenteges_dummy_5<- proportions_dummy_5*100
percenteges_dummy_5
segments_dummy_5 <- df_dummy_final5 %>%
  group_by(cluster)%>%
  summarise_at(vars(ConstCom, TimelyInf, TaskMgm, DeviceSt, Wellness, Athlete, Style, AmznP_0, AmznP_1, Female_0, Female_1, Degree_1, Degree_2, Age, Income_1, Income_2, Income_3, Income_4, Income_5),
               list(M = mean))
segments_dummy_5

##########################################################################################kkkkkkkkkk=6
dist_final_6<- dist(df_dummy_scaled, method = "euclidean")
hc_final_6<- hclust(dist_final_6, method = "ward.D2")
plot(hc_final_6)
tree_dummy_6<- cutree(hc_final_6, k=6)
rect.hclust(hc_final_6, k = 6, border = 2:8)
fviz_cluster(list(data= df_dummy_scaled, cluster = tree_dummy_6))
table(tree_dummy_6)
df_dummy_final6<- mutate(df_dummy, cluster = tree_dummy_6)
view(df_dummy_final6)
names(df_dummy_final6)
proportions_dummy_6<- table(df_dummy_final6$cluster)/length(df_dummy_final6$cluster)
percenteges_dummy_6<- proportions_dummy_6*100
percenteges_dummy_6
segments_dummy_6 <- df_dummy_final6 %>%
  group_by(cluster)%>%
  summarise_at(vars(ConstCom, TimelyInf, TaskMgm, DeviceSt, Wellness, Athlete, Style, AmznP_0, AmznP_1, Female_0, Female_1, Degree_1, Degree_2, Age, Income_1, Income_2, Income_3, Income_4, Income_5),
               list(M = mean))
segments_dummy_6

install.packages("writexl")
library(writexl)
ex_t2<- data.frame(segments_dummy_2)
p_2<- data.frame(percenteges_dummy_2)
p_3<- data.frame(percenteges_dummy_3)
p_4<- data.frame(percenteges_dummy_4)
p_5<- data.frame(percenteges_dummy_5)
p_6<- data.frame(percenteges_dummy_6)
write_xlsx(segments_dummy_2, "C:\\Users\\kutay\\Desktop\\lectures\\3.xlsx")
write_xlsx(list_2, "C:\\Users\\kutay\\Desktop\\lectures\\7.xlsx")
list_2<- list(p_2, segments_dummy_2, p_3, segments_dummy_3, p_4, segments_dummy_4, p_5, segments_dummy_5, p_6, segments_dummy_6)
############################ 

