#==============================================================================
# CELTIC model analysis
# Imputation
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
shhh(groundhog.library(missRanger, groundhog_day))
shhh(groundhog.library(psych, groundhog_day))
#------------------------------------------------------------------------------
set.seed(1984)
cm <- readRDS("CELTICmodel_sixhrs.rds")
#------------------------------------------------------------------------------
# imputation
# using missRanger pkg - chained random forests and predictive mean matching.
# pmm.k = number of candidate non-missing values to to sample in the pmm step.
# using all variables except Group to impute all missing variables.
# https://doi.org/10.1093/bioinformatics/btr597
cm.imp <- missRanger::missRanger(cm,
	formula = . ~ . -Group,
	pmm.k = 20,
	maxiter = 500,
	seed = 1984,
	verbose = 2,
	returnOOB = TRUE)
#------------------------------------------------------------------------------
# comparing imputed and non-imputed datasets
# label and bind
cm.i <- cm.imp %>%
	dplyr::mutate(imp = ifelse(is.na(Sheep_ID), NA, "Yes"))
cm.noni <- cm %>%
	dplyr::mutate(imp = ifelse(is.na(Sheep_ID), NA, "No"))
cm.c <- rbind(cm.noni, cm.i)
add.stats <- cm.c %>%
	dplyr::select(-c(1:3)) 
psych::describeBy(add.stats, group = cm.c$imp, quant = c(0.25, 0.75))
#==============================================================================
