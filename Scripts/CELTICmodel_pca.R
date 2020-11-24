#==============================================================================
# CELTIC model analysis
# Principal component analysis
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
shhh(groundhog.library(FactoMineR, groundhog_day))
shhh(groundhog.library(factoextra, groundhog_day))
shhh(groundhog.library(corrplot, groundhog_day))
shhh(groundhog.library(RColorBrewer, groundhog_day))
shhh(groundhog.library(cowplot, groundhog_day))
#------------------------------------------------------------------------------
set.seed(1984)
cm <- readRDS("CELTICmodel_imputed_sixhrs.rds")
cm_clustered <- readRDS("CELTICmodel_clustered.rds")
#------------------------------------------------------------------------------
# subset data
cm_pca <- cm %>%
	dplyr::select(-c(Sheep_ID, Group, Weight, Fluid_input))
#------------------------------------------------------------------------------
# examine outliers (using Hampel filter)
upper.boundary <- function(x) {
	upper_bound <- median(x) + 3 * mad(x)
	return(upper_bound)
}
lower.boundary <- function(x) {
	lower_bound <- median(x) - 3 * mad(x)
	return(lower_bound)
}
apply(cm_pca, 2, upper.boundary)
apply(cm_pca, 2, lower.boundary)
outliers <- function(x) {
	upper_bound <- upper.boundary(x)
	lower_bound <- lower.boundary(x)
	outlier_ind <- which(x < lower_bound | x > upper_bound)
	return(outlier_ind)
}
apply(cm_pca, 2, outliers)
#------------------------------------------------------------------------------
# scale datset
# centered by subtracting variable mean and scaled by dividing by variable 
# standard deviation
cm_pca <- as.matrix(cm_pca)
cm_pca_scale <- cm_pca %>%
	scale(center = TRUE, scale = TRUE) 
#------------------------------------------------------------------------------
# PCA
# using FactoMineR package
# https://cran.r-project.org/web/packages/FactoMineR/index.html
cm_pca_res <- FactoMineR::PCA(cm_pca_scale, 
	ncp = 8,
	scale.unit = FALSE,
	graph = FALSE)
# summary
summary(cm_pca_res,
	ncp = 8)
#------------------------------------------------------------------------------
# scree plot
factoextra::fviz_eig(cm_pca_res,
	ncp = 8,
	addlabels = FALSE,
	choice = "variance",
	barfill = "white",
	barcolor = "darkblue",
	linecolor = "red") + 
scale_y_continuous(limits = c(0,50), breaks = seq(0,50,5), expand = c(0,0)) +
theme_classic()
#------------------------------------------------------------------------------
# pairs plot
source("CELTICmodel_pca_pairsutility.R")
cowplot::plot_grid(a, b, c, d, e, f, g, NULL, h, i, j, k, l, m, 
			NULL, NULL, n, o, p, q, r, NULL, NULL, NULL, s, t, u, v,
			NULL, NULL, NULL, NULL, w, x, y, NULL, NULL, NULL, NULL, NULL, z, 
			aa, NULL, NULL, NULL, NULL, NULL, NULL, bb, ncol = 7)
#------------------------------------------------------------------------------
# quality of representation of the variables
var <- factoextra::get_pca_var(cm_pca_res)
col <- RColorBrewer::brewer.pal(n = 9, name = "RdBu")
corrplot::corrplot(var$cos2,
	is.corr = FALSE,
	method = "color",
	col = col)
#------------------------------------------------------------------------------
# contribution of variables to pc
corrplot::corrplot(var$contrib,
	is.corr = FALSE,
	method = "color",
	col = col)
#------------------------------------------------------------------------------
# biplot
cm_clustered$Cluster <- as.numeric(cm_clustered$Cluster)
factoextra::fviz_pca_biplot(cm_pca_res,
	axes = c(1,2),
	geom.ind = "point",
	fill.ind = cm_clustered$Group,
	col.ind = "black",
	pointshape = 21,
	pointsize = 5,
	palette = palette,
	addEllispses = FALSE,
	repel = TRUE,
	col.var = "contrib",
	alpha.var = "contrib",
	select.var = list(contrib = 5),
	gradient.cols = "RdYlBu")
#==============================================================================
