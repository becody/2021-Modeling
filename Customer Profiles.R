################ Customer Profiles #######################
# Date: 11/2/2020
# Goal: build customer profiles using customer health data 
# Author: Brittany Cody
# Contact: brittany.cody@learnplatform.com

# Install and load packages
remove.packages("klaR")
install.packages("ggdendro")
install.packages("klaR", dependencies = TRUE)
install.packages("cba")
install.packages("haven")
install.packages("FeatureImpCluster")
install.packages("attempt")
library(mlbench)
library(varhandle)
library(hablar)
library(corrplot)
library(caret)
library(dplyr)
library(forcats)
library(mlbench)
library(tidyverse)
library(Hmisc)
library(tidyr)
library(stats)
library(ggplot2)
library(ggdendro)
library(cluster)
library(cba)
library(klaR)
library(FeatureImpCluster)
library(attempt)
library(plyr)

################ Load all datasets #################
path <- "C:\\Users\\17046\\OneDrive\\Documents\\LearnPlatform\\Analysis\\Data\\Customer Profiles ROCK\\"
files <- list.files(path=path, pattern="*.csv")
for(file in files)
{
  perpos <- which(strsplit(file, "")[[1]]==".")
  assign(
    gsub(" ","",substr(file, 1, perpos-1)), 
    read.csv(paste(path,file,sep="")))
}

############### Clean datasets and merge ###########

# Merge organizations and contract data
organizations$organization_id <- organizations$ï..org_id

price_org <- organizations %>%
  merge(contract073120, by = "organization_id") %>%
  mutate(org_name = coalesce(org_name, organization)) %>%
  mutate(parent_orgid = coalesce(parent_orgid_string, parent_orgid))%>%
  mutate(state = coalesce(state.x, state.y)) %>%
  dplyr::select(-c(ï..org_id, org_name_id, parent_orgid_string,additional_context,
             organization_comment, description, state.x, state.y))
names(price_org)

# Group by organization id to have one row per unique id
f <- function(x) {
  x <- na.omit(x)
  if (length(x) > 0) first(x) else NA
}

df <- price_org %>%
  group_by(organization_id) %>%
  summarise_all(funs(f))

# Merge price_org and customer_health which should have additional information on renewal outcome
  # Again merge on org_id
customer_health$organization_id <- customer_health$org_id

orig_cust <- price_org %>%
  merge(customer_health, by = "organization_id", all.x = TRUE, allow.cartesian=TRUE) %>%
  distinct(id, .keep_all = TRUE) %>%
  filter(!is.na(renewal_outcome))

orig_cust <- rbind.fill(price_org, customer_health)

# orig_cust <- price_org
# Divide data into those who renewed and those who did not
cust_renew <- orig_cust %>%
  filter(renewal_outcome == 1)

cust_nonrenew <-orig_cust %>%
  filter(renewal_outcome == 0)

########### Using hclust method ##########
# On the customers who renewed and non renewed 
install.packages("gmfd")
library(gmfd)
hc <- hclust(dist(cust_renew))
hc_n <- hclust(dist(cust_nonrenew, method = "mahalanobis"))

# Plot that clustering 
PR <- ggdendrogram(hc, rotate = TRUE)
PR

PNR <-

###### K Modes - for Cust-NonRenew ############
# Get rid of NAs in dataset 
str(cust_nonrenew)

# Check for na and infinity values
indexes <- apply(cust_nonrenew, 2, function(x) any(is.na(x) | is.infinite(x)))
colnames(cust_nonrenew)[indexes]

# Remove columns that do have na and infinity values - also remove
# renewal_outcome because we want it to cluster by that

cust_trends <- cust_nonrenew %>%
  dplyr::select(-c("organization_id",   
            "organization",
            "parent_orgid",
            "subscription",
            "start_date",
            "updated_date",
            "applicants",
            "badges",                                
            "organization_memberlist_administrators",
            "organization_memberlist_all",
            "organization_memberlist_educators",
            "organization_memberlist_informationtechnology",
            "organization_memberlist_other",
            "organization_memberlist_productdeveloper",
            "organization_memberlist_students",
            "organization_rostercount_estimate",
            "products_graded",
            "statuses",
            "target",
            "users_graded",
            "tool_id",
            "org_tool_id",
            "tool_name",
            "unit",
            "period",
            "item_type",
            "id",
            "quantity_sum",
            "cost_sum",
            "status",
            "custom_status_id",
            "privacy_status",
            "custom_privacy_status_id",
            "updated_at",
            "contract_expires_at",
            "created_at",
            "started_at",
            "ended_at",
            "deleted_at",
            "cost_per_student_sum",
            "cost_per_unit_sum",
            "students_impacted_sum",
            "user_count_sum",
            "buy_now_feature_enabled",
            "district_count_sum",
            "rating",
            "school_count_sum",
            "state",
            "NCES_ID__c",
            "org_id",
            "product_count",
            "survey_count",
            "tool_request",
            "tool_request_completed",
            "provider_request",
            "vendor_response",
            "months_to_renewal",
            "Type.Opportunity.")) %>%
  distinct(org_name, .keep_all = TRUE)

str(cust_trends)
# Plot first 15 vars of the new dataset to see variance
for (i in 1:15) {
  plot(cust_trends[ ,i], main=colnames(cust_trends)[i],
       ylab = "Count", col="steelblue", las = 0)
}

################ Using Gower's Method and dissimilarity matrix #################
# Remove college name before clustering
gower_dist <- daisy(cust_trends[, -1],
                    metric = "gower",
                    type = list(logratio = 3))
# Check attributes to ensure the correct methods are being used
# (I = interval, N = nominal)
# Note that despite logratio being called, 
# the type remains coded as "I"
summary(gower_dist)

# Check the top two most similar and least similar 
gower_mat <- as.matrix(gower_dist)
# Output most similar pair 
cust_trends[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]
# Sequoia Union High School District 2020 Q3 , CA
# Pickerington Local School District 2020 Q3, OH

# Output most dissimilar pair
cust_trends[
  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]
# 2017 Q2 - Natick Public Schools, MA
# 2017 Q2 - Lipman Middle School, CA

# Calculate silhouette width for many k using PAM
sil_width <- c(NA)
for(i in 2:10){
  
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}
# Plot sihouette width (higher is better)
plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)

# We will use silhouette width, an internal validation metric which is an aggregated measure of how similar an observation is to its own cluster 
# compared its closest neighboring cluster. The metric can range from -1 to 1, where higher values are better. 
# Partitioning around medoids is an iterative clustering procedure with the following steps:
#   
# Choose k random entities to become the medoids
# Assign every entity to its closest medoid (using our custom distance matrix in this case)
# For each cluster, identify the observation that would yield the lowest average distance if it were to be re-assigned as the medoid. If so, make this observation the new medoid.
# If at least one medoid has changed, return to step 2. Otherwise, end the algorithm.
# If you know the k-means algorithm, this might look very familiar. In fact, both approaches are identical, except k-means has cluster centers defined by Euclidean distance (i.e., centroids), while cluster centers for PAM are restricted to be the observations themselves (i.e., medoids).
# 
# pros: Easy to understand, more robust to noise and outliers when compared to k-means, and has the added benefit of having an observation serve as the exemplar for each cluster
# cons: Both run time and memory are quadratic (i.e., $O(n^2)$)

# As a result of the silhouette width, we should do 2 clusters which has the best number (3 is the next highest)

# Perform PAM algorithm - we can interpret by running summary on each cluster 
pam_fit <- pam(gower_dist, diss = TRUE, k = 2)
pam_results <- cust_trends %>%
  dplyr::select(-org_name) %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam_results$the_summary

# Check the medoid for each of these clusters
cust_trends[pam_fit$medoids, ]

# Visualize many variables in a lower-dimensional space t-SNE (stochastic neighborhood embedding)
# See how the clusters are divided
library(Rtsne)
length(gower_dist)
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE, perplexity = 8)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         name = cust_trends$org_name)
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))


###### K Modes - for Cust-Renew ############
# Get rid of NAs in dataset 
str(cust_renew)

# Check for na and infinity values
indexes <- apply(cust_renew, 2, function(x) any(is.na(x) | is.infinite(x)))
colnames(cust_renew)[indexes]

# Remove columns that do have na and infinity values - also remove
# renewal_outcome because we want it to cluster by that

cust_trends <- cust_renew %>%
  dplyr::select(-c("organization_id",   
                   "organization",
                   "parent_orgid",
                   "subscription",
                   "start_date",
                   "updated_date",
                   "applicants",
                   "badges",                                
                   "organization_memberlist_administrators",
                   "organization_memberlist_all",
                   "organization_memberlist_educators",
                   "organization_memberlist_informationtechnology",
                   "organization_memberlist_other",
                   "organization_memberlist_productdeveloper",
                   "organization_memberlist_students",
                   "organization_rostercount_estimate",
                   "products_graded",
                   "statuses",
                   "target",
                   "users_graded",
                   "tool_id",
                   "org_tool_id",
                   "tool_name",
                   "unit",
                   "period",
                   "item_type",
                   "id",
                   "quantity_sum",
                   "cost_sum",
                   "status",
                   "custom_status_id",
                   "privacy_status",
                   "custom_privacy_status_id",
                   "updated_at",
                   "contract_expires_at",
                   "created_at",
                   "started_at",
                   "ended_at",
                   "deleted_at",
                   "cost_per_student_sum",
                   "cost_per_unit_sum",
                   "students_impacted_sum",
                   "user_count_sum",
                   "buy_now_feature_enabled",
                   "district_count_sum",
                   "rating",
                   "school_count_sum",
                   "state",
                   "NCES_ID__c",
                   "org_id",
                   "product_count",
                   "survey_count",
                   "tool_request",
                   "tool_request_completed",
                   "provider_request",
                   "vendor_response",
                   "months_to_renewal",
                   "Type.Opportunity.")) %>%
  distinct(org_name, .keep_all = TRUE)

str(cust_trends)
# Plot first 15 vars of the new dataset to see variance
for (i in 1:15) {
  plot(cust_trends[ ,i], main=colnames(cust_trends)[i],
       ylab = "Count", col="steelblue", las = 0)
}

################ Using Gower's Method and dissimilarity matrix #################
# Remove college name before clustering
gower_dist <- daisy(cust_trends[, -1],
                    metric = "gower",
                    type = list(logratio = 3))
# Check attributes to ensure the correct methods are being used
# (I = interval, N = nominal)
# Note that despite logratio being called, 
# the type remains coded as "I"
summary(gower_dist)

# Check the top two most similar and least similar 
gower_mat <- as.matrix(gower_dist)
# Output most similar pair 
cust_trends[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]
  # 2020 Q2 Community High School District, IL
  # 2020 Q2 - Cedarburg School District, WI

# Output most dissimilar pair
cust_trends[
  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]
  # 2020 Q2 Nashoba Regional School District, MA 
  # 2018 Q3 Washington County Public Schools, MD

# Calculate silhouette width for many k using PAM
sil_width <- c(NA)
for(i in 2:10){
  
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}
# Plot sihouette width (higher is better)
plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)

# We will use silhouette width, an internal validation metric which is an aggregated measure of how similar an observation is to its own cluster 
# compared its closest neighboring cluster. The metric can range from -1 to 1, where higher values are better. 
# Partitioning around medoids is an iterative clustering procedure with the following steps:
#   
# Choose k random entities to become the medoids
# Assign every entity to its closest medoid (using our custom distance matrix in this case)
# For each cluster, identify the observation that would yield the lowest average distance if it were to be re-assigned as the medoid. If so, make this observation the new medoid.
# If at least one medoid has changed, return to step 2. Otherwise, end the algorithm.
# If you know the k-means algorithm, this might look very familiar. In fact, both approaches are identical, except k-means has cluster centers defined by Euclidean distance (i.e., centroids), while cluster centers for PAM are restricted to be the observations themselves (i.e., medoids).
# 
# pros: Easy to understand, more robust to noise and outliers when compared to k-means, and has the added benefit of having an observation serve as the exemplar for each cluster
# cons: Both run time and memory are quadratic (i.e., $O(n^2)$)

# As a result of the silhouette width, we should do 2 clusters which has the best number (3 is the next highest)

# Perform PAM algorithm - we can interpret by running summary on each cluster 
pam_fit <- pam(gower_dist, diss = TRUE, k = 2)
pam_results <- cust_trends %>%
  dplyr::select(-org_name) %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam_results$the_summary

# Check the medoid for each of these clusters
cust_trends[pam_fit$medoids, ]

# Visualize many variables in a lower-dimensional space t-SNE (stochastic neighborhood embedding)
# See how the clusters are divided
library(Rtsne)
length(gower_dist)
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE, perplexity = 8)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         name = cust_trends$org_name)
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))

###### K Modes - for Combined Renew and non-Renew ############
# Get rid of NAs in dataset 
str(orig_cust)

# Check for na and infinity values
indexes <- apply(orig_cust, 2, function(x) any(is.na(x) | is.infinite(x)))
colnames(orig_cust)[indexes]

# Remove columns that do have na and infinity values - also remove
# renewal_outcome because we want it to cluster by that

cust_trends <- orig_cust %>%
  dplyr::select(-c("organization_id","organization","parent_orgid",                                 
                   "subscription","start_date","updated_date",                                 
                    "admin_signins","applicants","badges",                                       
                    "educator_signins","feedback_requests_with_responses","feedback_total_respondents",                   
                    "logins","organization_memberlist_administrators","organization_memberlist_all",                  
                    "organization_memberlist_educators","organization_memberlist_informationtechnology","organization_memberlist_other",                
                    "organization_memberlist_productdeveloper","organization_memberlist_students","organization_rostercount_estimate",            
                    "pct_with_assets","pct_with_cost","pct_with_grade",                               
                    "pct_with_privacystatus","products_graded","products_with_assets",                         
                    "products_with_grades","products_with_status","products_with_privacystatus",                  
                    "statuses","target","tool_requests",                                
                    "users_graded","tool_id","org_tool_id",                                  
                    "tool_name","unit","period",                                       
                    "item_type","id","quantity_sum",                                 
                    "cost_sum","status","organization_type",                            
                    "custom_status_id","privacy_status","custom_privacy_status_id",                     
                    "updated_at","contract_expires_at","created_at",                                   
                    "started_at","ended_at","deleted_at",                                   
                    "cost_per_student_sum","cost_per_unit_sum","students_impacted_sum",                        
                    "user_count_sum","buy_now_feature_enabled","district_count_sum",                           
                    "rating","school_count_sum","state",                                        
                    "LP_Org_ID__c","Name","Name.Opportunity.",                            
                    "Fiscal","FiscalYear","FiscalQuarter",                                
                    "NCES_ID__c","Org_Size__c","Locale__c",                                    
                    "LastModifiedDate.Opportunity.","Type.Opportunity.", "StageName",                                    
                    "Number_of_Students__c..AVG.","Number_of_Schools__c..AVG.","renewal_date",                                 
                    "org_id","time",                                         
                    "product_count","survey_count","tool_request",                                 
                    "tool_request_completed","provider_request","vendor_response",                              
                    "time_to_renewal","months_to_renewal","organization_roster_total")) %>%
  distinct(org_name, .keep_all = TRUE) %>%
  filter(!is.na(renewal_outcome))

str(cust_trends)
# Plot first 15 vars of the new dataset to see variance
for (i in 1:15) {
  plot(cust_trends[ ,i], main=colnames(cust_trends)[i],
       ylab = "Count", col="steelblue", las = 0)
}

# Try running clustering on OHE dataset
# dummy <- dummyVars(" ~ .", data=cust_trends)
# cust_trends <- data.frame(predict(dummy, newdata = cust_trends))

################ Using Gower's Method and dissimilarity matrix #################
# Remove college name before clustering
gower_dist <- daisy(cust_trends[, -1],
                    metric = "gower",
                    type = list(logratio = 3))
# Check attributes to ensure the correct methods are being used
# (I = interval, N = nominal)
# Note that despite logratio being called, 
# the type remains coded as "I"
summary(gower_dist)

# Check the top two most similar and least similar 
gower_mat <- as.matrix(gower_dist)
plot(gower_mat, cex = 0.5)
# Output most similar pair 
cust_trends[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]
# Conroe Independent School District
# San Francisco Unified School District

# Output most dissimilar pair
cust_trends[
  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]
# Stonington School District
# Technology for Education Consortium

# Calculate silhouette width for many k using PAM
sil_width <- c(NA)
for(i in 2:10){
  
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}
# Plot sihouette width (higher is better)
plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)

# We will use silhouette width, an internal validation metric which is an aggregated measure of how similar an observation is to its own cluster 
# compared its closest neighboring cluster. The metric can range from -1 to 1, where higher values are better. 
# Partitioning around medoids is an iterative clustering procedure with the following steps:
#   
# Choose k random entities to become the medoids
# Assign every entity to its closest medoid (using our custom distance matrix in this case)
# For each cluster, identify the observation that would yield the lowest average distance if it were to be re-assigned as the medoid. If so, make this observation the new medoid.
# If at least one medoid has changed, return to step 2. Otherwise, end the algorithm.
# If you know the k-means algorithm, this might look very familiar. In fact, both approaches are identical, except k-means has cluster centers defined by Euclidean distance (i.e., centroids), while cluster centers for PAM are restricted to be the observations themselves (i.e., medoids).
# 
# pros: Easy to understand, more robust to noise and outliers when compared to k-means, and has the added benefit of having an observation serve as the exemplar for each cluster
# cons: Both run time and memory are quadratic (i.e., $O(n^2)$)

# As a result of the silhouette width, we should do 2 clusters which has the best number (3 is the next highest)

# Perform PAM algorithm - we can interpret by running summary on each cluster 
pam_fit <- pam(gower_dist, diss = TRUE, k = 2)
pam_results <- cust_trends %>%
  dplyr::select(-org_name) %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam_results$the_summary
cluster1 <- cust_trends[pam_fit$clustering == 1,]
cluster2 <- cust_trends[pam_fit$clustering == 2,]

table(cluster1$renewal_outcome)
table(cluster2$renewal_outcome)
# Check to see how it is dividing clusters

# Check the medoid for each of these clusters
cust_trends[pam_fit$medoids, ]

# Visualize many variables in a lower-dimensional space t-SNE (stochastic neighborhood embedding)
# See how the clusters are divided
library(Rtsne)
length(gower_dist)
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE, perplexity = 7)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         name = cust_trends$org_name)
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))

library(flexclust)
res <- kcca(cust_trends,k=2, family= kccaFamily("kmedians"))

stripes(gower_mat)

# Filter out schools in smaller cluster
tsne_data %>%
  filter(X > -20 & X < -10) %>%
  left_join(orig_cust, by = "org_name") %>%
  collect %>%
  .[["org_name"]]

############## Post-diagnostics ############

# Visualize each cluster based on variables
str(cust_trends)
ToCluster.c<-cbind(cust_trends, pam_results$cluster)
colnames(ToCluster.c)[11]<-c("Group")

df.m <- melt(ToCluster.c, id.var = "Group")
df.m$Group <- as.character(df.m$Group)

p <- ggplot(data = df.m, aes(x=variable, y=value)) +
  geom_boxplot(aes(fill = Group),outlier.size = 1) +
  facet_wrap( ~ variable, scales="free", ncol = 2) +
  xlab(label = NULL) + ylab(label = NULL) + ggtitle("Boxplots for 3 Groups of Clients") +
  guides(fill=guide_legend(title="Groups"))
p

# featureImp_res <- FeatureImpCluster(pam_fit, as.data.table(cust_trends), mode = "gower")



install.packages("flexclust")
install.packages("factoextra")
library(flexclust)
library(cluster)
library(factoextra)

ToCluster.c<-cbind(cust_trends, pam_results$cluster)
colnames(ToCluster.c)[27]<-c("Group")

df.m <- melt(ToCluster.c, id.var = "Group")
df.m$Group <- as.character(df.m$Group)

p <- ggplot(data = df.m, aes(x="Group", y=value)) +
  geom_boxplot(aes(fill = Group),outlier.size = 1) +
  facet_wrap( ~ variable, scales="free", ncol = 2) +
  xlab(label = NULL) + ylab(label = NULL) + ggtitle("Boxplots for 2 Groups of Customers") +
  guides(fill=guide_legend(title="Groups")) 
p
str(ToCluster.c)
col_names <- colnames(ToCluster.c)
col_names <- col_names[-27]
for (i in col_names){
  plot <- ggplot(data=ToCluster.c, aes_string(x = ToCluster.c$Group, y = i)) + geom_bar(stat = "identity") + 
    facet_wrap(ToCluster.c$Group ~.)
  print(plot)
}

for (i in col_names){
  plot <- ggplot(data=ToCluster.c, aes_string(x = i, fill = as.factor(ToCluster.c$Group))) + geom_bar(stat = "count") +
    facet_wrap(ToCluster.c$Group ~.)
  print(plot)
}

ToCluster.c %>%
  keep(is.numeric) %>% 
  filter(Group == 1)%>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_bar()


ToCluster.c %>%
  keep(is.numeric) %>% 
  filter(Group == 2)%>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

clust1 <- ToCluster.c %>%
  filter(Group == 1)
ggplot(clust1) + geom_bar(aes(x = type))

clust2 <- ToCluster.c %>%
  filter(Group == 2)
ggplot(clust2) + geom_bar(aes(x = type))

plot(ToCluster.c$products_with_expired_date)
sum(is.null(df.m))

write.csv(ToCluster.c, "C:\\Users\\17046\\OneDrive\\Documents\\LearnPlatform\\Analysis\\Data\\Customer Profiles ROCK\\clustering.csv")


install.packages("ggforce")
library(ggforce)

fviz_cluster(clusters, data = cust_trends, ellipse.type = "convex",
             xlab = "x-axis label", ylab = "y-axis label") +
  theme_minimal()

barplot(clusters, oneplot=FALSE)

set.seed(7)
clusters <- kmeans(cust_trends, 2)

myColors <- c("darkblue", "red", "green", "brown", "pink", "purple", "yellow")
barplot(t(clusters$centers), beside = TRUE, xlab="cluster", 
        ylab="value", col = myColors)
legend("top", ncol=2, fill = myColors)

names(cust_trends)






