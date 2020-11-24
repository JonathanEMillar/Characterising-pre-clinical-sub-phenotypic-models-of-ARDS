#==============================================================================
# CELTIC model analysis
# Clustering
#==============================================================================
# Author: JE Millar
# Date: 2020-11-04
# Version: 2.0 
# [R version 4.0.3]
#==============================================================================
# libraries
shhh <- suppressPackageStartupMessages
shhh(library(groundhog))
groundhog_day <- "2020-11-04"
shhh(groundhog.library(tidyverse, groundhog_day))
shhh(groundhog.library(cluster, groundhog_day))
shhh(groundhog.library(NbClust, groundhog_day))
shhh(groundhog.library(fpc, groundhog_day))
#------------------------------------------------------------------------------
set.seed(1984)
cm <- readRDS("CELTICmodel_imputed_sixhrs.rds")
#------------------------------------------------------------------------------
# subset data
cm_cluster <- cm %>%
	dplyr::select(-c(Sheep_ID, Group, Weight, Fluid_input))
#------------------------------------------------------------------------------
# scale datset
# centered by subtracting variable mean and scaled by dividing by variable 
# standard deviation
cm_cluster_scale <- cm_cluster %>%
	scale(center = TRUE, scale = TRUE) 
#------------------------------------------------------------------------------
# optimal cluster number
# using NbClust function  in NbClust package
# https://www.rdocumentation.org/packages/NbClust/versions/3.0/topics/NbClust
# using majority vote of 26 measures of optimal cluster number
NbClust::NbClust(t(cm_cluster),
	distance = "euclidean",
	min.nc = 2,
	max.nc = 18,
	method = "kmeans",
	index = "all")
#------------------------------------------------------------------------------
# Partitioning around medoids (PAM)
# https://www.rdocumentation.org/packages/cluster/versions/2.1.0/topics/pam
cm_pam <- cluster::pam(cm_cluster_scale,
	2,
	diss = FALSE,
	metric = "euclidean",
	stand = FALSE)
#------------------------------------------------------------------------------
# cluster sizes
cm_pam_tib <- as_tibble(cm_pam$clustering)
table(cm_pam_tib)
#------------------------------------------------------------------------------
# summary stats
cm_clustered <- cm %>%
	add_column(Cluster = cm_pam$clustering) %>%
	mutate_at(vars(Sheep_ID, Group, Cluster), list(factor))
cm_clustered %>% 
	split(cm_clustered$Cluster) %>% 
	map(summary)
#------------------------------------------------------------------------------
# diagnostics
# using clusterboot function in FPC package
# https://www.rdocumentation.org/packages/fpc/versions/2.1-9/topics/clusterboot
# 1000 resamplings 
cm_boot <- fpc::clusterboot(cm_cluster_scale,
	B = 1000,
	distances = FALSE,
	bootmethod = "boot",
	clustermethod = pamkCBI,
	krange = 2,
	seed = 1984)
#==============================================================================
