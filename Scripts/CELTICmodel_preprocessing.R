#==============================================================================
# CELTIC model analysis
# Data pre-processing
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
#------------------------------------------------------------------------------
options(max.print = 10000)
set.seed(1984)
cm <- read_csv("celticmaster.csv")
#------------------------------------------------------------------------------
# Sheep_ID to character and Group to factor
cm <- cm %>%
	dplyr::mutate(Sheep_ID = as.character(Sheep_ID),
				  Group = as.factor(Group))
#------------------------------------------------------------------------------
# cretae long format datset
cm_long <- cm %>%
	dplyr::select(-c(Weight, Vt, Min_vol_6hrs, Fluid_input, Ur_output,
				   Cumulative_balance)) %>%
	tidyr::pivot_longer(
		cols = c(FiO2_base:CK_6hrs),
		names_to = c("Variable", "Timepoint"),
		names_sep = "_",
		names_ptype = list(Timepoint = factor()),
		values_to = "Value")
#------------------------------------------------------------------------------
# create 6 hr dataset
cm_6hrs <- cm %>%
	dplyr::select(-ends_with(c("_base", "_0hrs", "_1hrs", "_2hrs", "_4hrs")))
#==============================================================================
