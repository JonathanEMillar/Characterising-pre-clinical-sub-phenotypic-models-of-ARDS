#==============================================================================
# CELTIC model analysis
# Correlation
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
shhh(groundhog.library(corrplot, groundhog_day))
shhh(groundhog.library(RColorBrewer, groundhog_day))
#------------------------------------------------------------------------------
set.seed(1984)
cm <- readRDS("CELTICmodel_imputed_sixhrs.rds")
#------------------------------------------------------------------------------
# correlation plot
#
# Plot wrangling
cm <- cm %>%
	dplyr::select(-c(Sheep_ID, Group)) %>%
	as.data.frame()
M <- cor(cm,
		use="na.or.complete",
		method="spearman")
Mp <- corrplot::cor.mtest(cm, conf.level=.95)
### Plot ###
cor.plot <- corrplot::corrplot(M,
	p.mat=Mp$p,
	type="full",
	diag=FALSE,
	method="color",
	col = brewer.pal(n = 10, name = "RdYlBu"),
	sig.level=0.05,
	insig="blank",
	order="original")
#==============================================================================
