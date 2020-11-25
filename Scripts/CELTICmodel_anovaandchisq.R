#==============================================================================
# CELTIC model analysis
# One-way ANOVA and chi-squared tests
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
# one-way ANOVA for fluid balance
# load data
cm <- readRDS("celticmodel_wide.rds")
# filter data
fb <- cm %>%
	dplyr::select(c(Group, Ur_output, Cumulative_balance))
# one-way ANOVA for urine output
uo_aov <- aov(Ur_output ~ Group, data = fb)
summary(uo_aov)
# one-way anova for cumulative balance
cb_aov <- aov(Cumulative_balance ~ Group, data = fb)
summary(cb_aov)
# cumulative balance post-hoc comparison
TukeyHSD(cb_aov)
#------------------------------------------------------------------------------
# chi-squared for cluster proportions
# load data
cp <- read_csv("clustercounts.csv")
# group column to rowname
cp <- cp %>%
	column_to_rownames(var = "Group")
cp_chisq <- chisq.test(cp)
# p value
cp_chisq$p.value
# residuals
round(cp_chisq$residuals, 2)
# expected
round(cp_chisq$expected, 0)
#==============================================================================