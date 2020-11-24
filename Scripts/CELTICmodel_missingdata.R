#==============================================================================
# CELTIC model analysis
# Missing data
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
shhh(groundhog.library(naniar, groundhog_day))
#------------------------------------------------------------------------------
options(tibble.print_max = Inf)
set.seed(1984)
cm <- readRDS("CELTICmodel_wide.rds")
#------------------------------------------------------------------------------
# missing data
# tabular summaries
missing.summary <- naniar::miss_summary(cm)
	missing.summary$miss_case_table # number of variables missing per case
	missing.summary$miss_var_table # number of cases missing per variable
	missing.summary$miss_var_summary # variables ranked by number/% missing
# upset plot (sets = number of cases with missing value for variable)
miss.upset <- naniar::gg_miss_upset(cm,
	nsets = 50,
	nintersects = NA,
	order.by = c("freq", "degree"), 
	decreasing = c(TRUE,FALSE))
#==============================================================================
