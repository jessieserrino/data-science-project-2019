suppressWarnings(source("library.R"))
# Package options
suppressWarnings(ggthemr('fresh'))  # ggplot theme
opts_knit$set(progress=FALSE, verbose=FALSE)
opts_chunk$set(echo=FALSE, fig.align="center", fig.width=10, fig.height=6.35, results="asis")
options(knitr.kable.NA = '')

# New versions of the networkD3 package may not work properly, so install the following version
#packageurl <- "https://cran.r-project.org/src/contrib/Archive/networkD3/networkD3_0.2.13.tar.gz"
#install.packages(packageurl, repos=NULL, type="source")
if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")}
pacman::p_load("caret", "dplyr", "e1071", "glmnet","lift","MASS", "ROCR", "partykit", "pracma","xgboost")
library(stringr)
library(dplyr)
source("team_helper_functions.r")


# The Data
# Please enter the minimum number below which you would like not to print - this makes the readability of the tables easier. Default values are either 10e6 (to print everything) or 0.5. Try both to see the difference.
MIN_VALUE = 0.5
# Please enter the maximum number of observations to show in the report and slides. 
max_data_report = 10

ProjectData <- read.csv("input/data.csv", na.strings=c(""," ","NA"), header=TRUE) # Loading data
ProjectData <- clean(ProjectData)
ProjectData <- data.matrix(ProjectData) 
ProjectData_INITIAL <- ProjectData

# Part 1: Key Customer Characteristics
# Please ENTER the original raw attributes to use. 
# Please use numbers, not column names, e.g. c(1:5, 7, 8) uses columns 1,2,3,4,5,7,8
factor_attributes_used = c(2:ncol(ProjectData)) # for BOATS Data

# Choices: "eigenvalue", "variance", "manual"
factor_selectionciterion = "eigenvalue"

# (Only used in case "variance" is the factor selection criterion used). 
minimum_variance_explained = 65  # between 1 and 100

# Please ENTER the number of factors to use 
# (Only used in case "manual" is the factor selection criterion used).
manual_numb_factors_used = 6

# Please ENTER the rotation eventually used (e.g. "none", "varimax", "quatimax", "promax", "oblimin", "simplimax", and "cluster" - see help(principal)). Default is "varimax"
rotation_used = "varimax"

factor_attributes_used <- intersect(factor_attributes_used, 1:ncol(ProjectData))
ProjectDataFactor <- ProjectData[,factor_attributes_used]
ProjectDataFactor <- data.matrix(ProjectDataFactor)

## Steps 1-2: Check the Data 
rownames(ProjectDataFactor) <- paste0("Obs.", sprintf("%02i", 1:nrow(ProjectDataFactor)))
iprint.df(t(head(round(ProjectDataFactor, 2), max_data_report)))
iprint.df(round(my_summary(ProjectDataFactor), 2))

## Step 3: Check Correlations
correlations = round(cor(ProjectDataFactor),2)
correlations_thres <- correlations
correlations_thres[abs(correlations_thres) < MIN_VALUE]<-NA
colnames(correlations_thres)<- colnames(correlations)
rownames(correlations_thres)<- rownames(correlations)

iprint.df(correlations_thres, scale=TRUE)

## Step 4: Choose number of factors

# Here is how the `principal` function is used 
UnRotated_Results<-principal(ProjectDataFactor, nfactors=ncol(ProjectDataFactor), rotate="none",score=TRUE)
UnRotated_Factors<-round(UnRotated_Results$loadings,2)
UnRotated_Factors<-as.data.frame(unclass(UnRotated_Factors))
colnames(UnRotated_Factors)<-paste("Comp",1:ncol(UnRotated_Factors),sep="")

# Here is how we use the `PCA` function 
Variance_Explained_Table_results<-PCA(ProjectDataFactor, graph=FALSE)
Variance_Explained_Table<-Variance_Explained_Table_results$eig
Variance_Explained_Table_copy<-Variance_Explained_Table

rownames(Variance_Explained_Table) <- paste("Component", 1:nrow(Variance_Explained_Table), sep=" ")
colnames(Variance_Explained_Table) <- c("Eigenvalue", "Pct of explained variance", "Cumulative pct of explained variance")


iprint.df(round(Variance_Explained_Table, 2))

eigenvalues  <- Variance_Explained_Table[, "Eigenvalue"]
df           <- cbind(as.data.frame(eigenvalues), c(1:length(eigenvalues)), rep(1, length(eigenvalues)))
colnames(df) <- c("eigenvalues", "components", "abline")
iplot.df(melt(df, id="components"))

## Step 5: Interpret the factors
if (factor_selectionciterion == "eigenvalue")
  factors_selected = sum(Variance_Explained_Table_copy[,1] >= 1)
if (factor_selectionciterion == "variance")
  factors_selected = 1:head(which(Variance_Explained_Table_copy[,"cumulative percentage of variance"]>= minimum_variance_explained),1)
if (factor_selectionciterion == "manual")
  factors_selected = manual_numb_factors_used

Rotated_Results<-principal(ProjectDataFactor, nfactors=max(factors_selected), rotate=rotation_used,score=TRUE)
Rotated_Factors<-round(Rotated_Results$loadings,2)
Rotated_Factors<-as.data.frame(unclass(Rotated_Factors))
colnames(Rotated_Factors)<-paste("Comp.",1:ncol(Rotated_Factors),sep="")

sorted_rows <- sort(Rotated_Factors[,1], decreasing = TRUE, index.return = TRUE)$ix
Rotated_Factors <- Rotated_Factors[sorted_rows,]

iprint.df(Rotated_Factors, scale=TRUE)

Rotated_Factors_thres <- Rotated_Factors
Rotated_Factors_thres[abs(Rotated_Factors_thres) < MIN_VALUE]<-NA
colnames(Rotated_Factors_thres)<- colnames(Rotated_Factors)
rownames(Rotated_Factors_thres)<- rownames(Rotated_Factors)

iprint.df(Rotated_Factors_thres, scale=TRUE)

## Step 6:  Save factor scores 
NEW_ProjectData <- round(Rotated_Results$scores[,1:factors_selected,drop=F],2)
colnames(NEW_ProjectData)<-paste("Component(Factor)",1:ncol(NEW_ProjectData),sep=" ")

iprint.df(t(head(NEW_ProjectData, 10)), scale=TRUE)

# Part 2: Customer Segmentation 
# Please ENTER then original raw attributes to use for the segmentation (the "segmentation attributes")
# Please use numbers, not column names, e.g. c(1:5, 7, 8) uses columns 1,2,3,4,5,7,8
segmentation_attributes_used = c(29,17,24,14,8,9,22,7,20,3,12,27,18) 
#segmentation_attributes_used = c(2:ncol(ProjectData)) 

profile_attributes_used = c(29,17,24,14,8,9,22,7,20,3,12,27,18) 
numb_clusters_used = 6 

profile_with = "hclust" #  "hclust" or "kmeans"

# Please ENTER the distance metric eventually used for the clustering in case of hierarchical clustering 
# (e.g. "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski" - see help(dist)). 
# DEFAULT is "euclidean"
distance_used = "euclidean"

# Please ENTER the hierarchical clustering method to use (options are:
# "ward", "single", "complete", "average", "mcquitty", "median" or "centroid").
# DEFAULT is "ward"
#hclust_method = "ward.D"
hclust_method = "ward.D"

# Please ENTER the kmeans clustering method to use (options are:
# "Hartigan-Wong", "Lloyd", "Forgy", "MacQueen").
# DEFAULT is "Lloyd"
kmeans_method = "Lloyd"

# Same as the initial data
ProjectData <- ProjectData_INITIAL

segmentation_attributes_used <- intersect(segmentation_attributes_used, 1:ncol(ProjectData))
profile_attributes_used <- intersect(profile_attributes_used, 1:ncol(ProjectData))

ProjectData_segment <- ProjectData[,segmentation_attributes_used]
ProjectData_profile <- ProjectData[,profile_attributes_used]

ProjectData_scaled <- apply(ProjectData, 2, function(r) if (sd(r)!=0) (r-mean(r))/sd(r) else 0*r)

## Step 3. Select Segmentation Variables

euclidean_pairwise <- as.matrix(dist(head(ProjectData_segment, max_data_report), method="euclidean"))
euclidean_pairwise <- euclidean_pairwise*lower.tri(euclidean_pairwise) + euclidean_pairwise*diag(euclidean_pairwise) + 10e10*upper.tri(euclidean_pairwise)
euclidean_pairwise[euclidean_pairwise==10e10] <- NA
rownames(euclidean_pairwise) <- colnames(euclidean_pairwise) <- sprintf("Obs.%02d", 1:max_data_report)

iprint.df(round(euclidean_pairwise))

## Step 5: Visualize Pair-wise Distances
variables_to_plot = 1:2
do.call(iplot.grid, lapply(variables_to_plot, function(n){
  iplot.hist(ProjectData_segment[, n], breaks=10, xlab = paste("Variable", n))
}))

Pairwise_Distances <- dist(ProjectData_segment, method = distance_used) 
#iplot.hist(Pairwise_Distances, breaks=10)

gc()
Hierarchical_Cluster_distances <- dist(ProjectData_segment, method=distance_used)
Hierarchical_Cluster <- hclust(Hierarchical_Cluster_distances, method=hclust_method)
# Display dendogram
#iplot.dendrogram(Hierarchical_Cluster)

#
hcd <- as.dendrogram(Hierarchical_Cluster)
#plot(hcd)
#rect.hclust(Hierarchical_Cluster, k=numb_clusters_used, border="red") 

# TODO: Draw dendogram with red borders around the 3 clusters

num <- nrow(ProjectData) - 1
df1 <- cbind(as.data.frame(Hierarchical_Cluster$height[length(Hierarchical_Cluster$height):1]), c(1:num))
colnames(df1) <- c("distances","index")
iplot.df(melt(head(df1, 30), id="index"), xlab="Number of Components", ylab = "Distances")

cluster_memberships_hclust <- as.vector(cutree(Hierarchical_Cluster, k=numb_clusters_used)) # cut tree into as many clusters as numb_clusters_used
cluster_ids_hclust=unique(cluster_memberships_hclust)

ProjectData_with_hclust_membership <- cbind(1:length(cluster_memberships_hclust),cluster_memberships_hclust)
colnames(ProjectData_with_hclust_membership)<-c("Observation Number","Cluster_Membership")

iprint.df(round(head(ProjectData_with_hclust_membership, max_data_report), 2))

kmeans_clusters <- kmeans(ProjectData_segment,centers= numb_clusters_used, iter.max=2000, algorithm=kmeans_method)

ProjectData_with_kmeans_membership <- cbind(1:length(kmeans_clusters$cluster),kmeans_clusters$cluster)
colnames(ProjectData_with_kmeans_membership)<-c("Observation Number","Cluster_Membership")

iprint.df(round(head(ProjectData_with_kmeans_membership, max_data_report), 2))

cluster_memberships_kmeans <- kmeans_clusters$cluster 
cluster_ids_kmeans <- unique(cluster_memberships_kmeans)

if (profile_with == "hclust"){
  cluster_memberships <- cluster_memberships_hclust
  cluster_ids <-  cluster_ids_hclust  
}
if (profile_with == "kmeans"){
  cluster_memberships <- cluster_memberships_kmeans
  cluster_ids <-  cluster_ids_kmeans
}

# WE WILL USE THESE IN THE CLASSIFICATION PART LATER
NewData = matrix(cluster_memberships,ncol=1)

population_average = matrix(apply(ProjectData_profile, 2, mean), ncol=1)
colnames(population_average) <- "Population"
Cluster_Profile_mean <- sapply(sort(cluster_ids), function(i) apply(ProjectData_profile[(cluster_memberships==i), ], 2, mean))
if (ncol(ProjectData_profile) <2)
  Cluster_Profile_mean=t(Cluster_Profile_mean)
colnames(Cluster_Profile_mean) <- paste("Seg.", 1:length(cluster_ids), sep="")
cluster.profile <- cbind (population_average,Cluster_Profile_mean)

iprint.df(round(cluster.profile, 2))

ProjectData_scaled_profile = ProjectData_scaled[, profile_attributes_used,drop=F]

Cluster_Profile_standar_mean <- sapply(sort(cluster_ids), function(i) apply(ProjectData_scaled_profile[(cluster_memberships==i), ,drop = F], 2, mean))
if (ncol(ProjectData_scaled_profile) < 2)
  Cluster_Profile_standar_mean = t(Cluster_Profile_standar_mean)
colnames(Cluster_Profile_standar_mean) <- paste("Seg ", 1:length(cluster_ids), sep="")

iplot.df(melt(cbind.data.frame(idx=as.numeric(1:nrow(Cluster_Profile_standar_mean)), Cluster_Profile_standar_mean), id="idx"), xlab="Profiling variables (standardized)",  ylab="Mean of cluster")

population_average_matrix <- population_average[,"Population",drop=F] %*% matrix(rep(1,ncol(Cluster_Profile_mean)),nrow=1)
cluster_profile_ratios <- (ifelse(population_average_matrix==0, 0,Cluster_Profile_mean/population_average_matrix))
colnames(cluster_profile_ratios) <- paste("Seg.", 1:ncol(cluster_profile_ratios), sep="")
rownames(cluster_profile_ratios) <- colnames(ProjectData)[profile_attributes_used]
## printing the result in a clean-slate table
iprint.df(round(cluster_profile_ratios-1, 2))

Rotated_Factors_thresx <- cluster_profile_ratios-1
Rotated_Factors_thresx[abs(Rotated_Factors_thresx) < 0.1]<-NA
colnames(Rotated_Factors_thresx)<- colnames(cluster_profile_ratios)
rownames(Rotated_Factors_thresx)<- rownames(cluster_profile_ratios)

iprint.df(round(Rotated_Factors_thresx, 2))

#ProjectData_with_hclust_membership
#as.data.frame(table(ProjectData_with_hclust_membership))

a <- table(ProjectData_with_hclust_membership)

cluster1 <- a[names(a)==1]
cluster2 <- a[names(a)==2]
cluster3 <- a[names(a)==3]
clustertotal <- cluster1 + cluster2 + cluster3
cluster1p <- cluster1*100/(clustertotal)
cluster2p <- cluster2*100/(clustertotal)
cluster3p <- cluster3*100/(clustertotal)
print(cluster1p)
print(cluster2p)
print(cluster3p)