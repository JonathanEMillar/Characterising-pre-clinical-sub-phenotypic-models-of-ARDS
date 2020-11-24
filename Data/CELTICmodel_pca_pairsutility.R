#==============================================================================
# CELTIC model analysis
# PCA pairs plot utility
#==============================================================================
# packages
shhh <- suppressPackageStartupMessages
shhh(library(groundhog))
groundhog_day <- "2020-11-04"
shhh(groundhog.library(tidyverse, groundhog_day))
shhh(groundhog.library(FactoMineR, groundhog_day))
shhh(groundhog.library(factoextra, groundhog_day))
#------------------------------------------------------------------------------
set.seed(1984)
# load PCA based on agnes clustering
cm_clustered <- readRDS("CELTICmodel_clustered.rds")
cm_pca <- readRDS("CELTICmodel_pca.rds")
#------------------------------------------------------------------------------
# pairs plot
palette <- c("#00946C", "#D9500C", "#6963A8")
pc1 <- c(2:8)
indiv.pc1.plots <- vector("list", length(pc1))
for (val in pc1) {
	indiv.pc1.plots[[val]] <- factoextra::fviz_pca_ind(cm_pca,
		geom.ind = "point",
		pointshape = 21,
		col.ind = cm_clustered$Group,
		fill.ind = cm_clustered$Group,
		palette = palette,
		addEllipses = FALSE,
		axes = c(1, val),
		title = "") +
	theme(legend.position = "none")
}
pc2 <- c(3:8)
indiv.pc2.plots <- vector("list", length(pc2))
for (val in pc2) {
	indiv.pc2.plots[[val]] <- factoextra::fviz_pca_ind(cm_pca,
		geom.ind = "point",
		pointshape = 21,
		col.ind = cm_clustered$Group,
		fill.ind = cm_clustered$Group,
		palette = palette,
		addEllipses = FALSE,
		axes = c(2, val),
		title = "") +
	theme(legend.position = "none")
 }
pc3 <- c(4:8)
indiv.pc3.plots <- vector("list", length(pc3))
for (val in pc3) {
	indiv.pc3.plots[[val]] <- factoextra::fviz_pca_ind(cm_pca,
		geom.ind = "point",
		pointshape = 21,
		col.ind = cm_clustered$Group,
		fill.ind = cm_clustered$Group,
		palette = palette,
		addEllipses = FALSE,
		axes = c(3, val),
		title = "") +
	theme(legend.position = "none")
 }
pc4 <- c(5:8)
indiv.pc4.plots <- vector("list", length(pc4))
for (val in pc4) {
	indiv.pc4.plots[[val]] <- factoextra::fviz_pca_ind(cm_pca,
		geom.ind = "point",
		pointshape = 21,
		col.ind = cm_clustered$Group,
		fill.ind = cm_clustered$Group,
		palette = palette,
		addEllipses = FALSE,
		axes = c(4, val),
		title = "") +
	theme(legend.position = "none")
 }
pc5 <- c(5:8)
indiv.pc5.plots <- vector("list", length(pc5))
for (val in pc5) {
	indiv.pc5.plots[[val]] <- factoextra::fviz_pca_ind(cm_pca,
		geom.ind = "point",
		pointshape = 21,
		col.ind = cm_clustered$Group,
		fill.ind = cm_clustered$Group,
		palette = palette,
		addEllipses = FALSE,
		axes = c(5, val),
		title = "") +
	theme(legend.position = "none")
 }
pc6 <- c(6:8)
indiv.pc6.plots <- vector("list", length(pc6))
for (val in pc6) {
	indiv.pc6.plots[[val]] <- factoextra::fviz_pca_ind(cm_pca,
		geom.ind = "point",
		pointshape = 21,
		col.ind = cm_clustered$Group,
		fill.ind = cm_clustered$Group,
		palette = palette,
		addEllipses = FALSE,
		axes = c(6, val),
		title = "") +
	theme(legend.position = "none")
 }
pc7 <- c(7:8)
indiv.pc7.plots <- vector("list", length(pc7))
for (val in pc7) {
	indiv.pc7.plots[[val]] <- factoextra::fviz_pca_ind(cm_pca,
		geom.ind = "point",
		pointshape = 21,
		col.ind = cm_clustered$Group,
		fill.ind = cm_clustered$Group,
		palette = palette,
		addEllipses = FALSE,
		axes = c(7, val),
		title = "") +
	theme(legend.position = "none")
 }
indiv.pc1.plots[[2]] -> a
indiv.pc1.plots[[3]] -> b
indiv.pc1.plots[[4]] -> c
indiv.pc1.plots[[5]] -> d
indiv.pc1.plots[[6]] -> e
indiv.pc1.plots[[7]] -> f
indiv.pc1.plots[[8]] -> g
indiv.pc2.plots[[3]] -> h
indiv.pc2.plots[[4]] -> i
indiv.pc2.plots[[5]] -> j
indiv.pc2.plots[[6]] -> k
indiv.pc2.plots[[7]] -> l
indiv.pc2.plots[[8]] -> m 
indiv.pc3.plots[[4]] -> n
indiv.pc3.plots[[5]] -> o
indiv.pc3.plots[[6]] -> p
indiv.pc3.plots[[7]] -> q
indiv.pc3.plots[[8]] -> r
indiv.pc4.plots[[5]] -> s
indiv.pc4.plots[[6]] -> t
indiv.pc4.plots[[7]] -> u
indiv.pc4.plots[[8]] -> v
indiv.pc5.plots[[6]] -> w
indiv.pc5.plots[[7]] -> x
indiv.pc5.plots[[8]] -> y
indiv.pc6.plots[[7]] -> z
indiv.pc6.plots[[8]] -> aa 
indiv.pc7.plots[[8]] -> bb
#==============================================================================