#==============================================================================
# CELTIC model analysis
# Summary statistics 
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
shhh(groundhog.library(gtsummary, groundhog_day)) 
shhh(groundhog.library(flextable, groundhog_day)) 
shhh(groundhog.library(officer, groundhog_day)) 
shhh(groundhog.library(psych, groundhog_day)) 
shhh(groundhog.library(RColorBrewer, groundhog_day)) 
shhh(groundhog.library(corrplot, groundhog_day)) 
#------------------------------------------------------------------------------
set.seed(1984)
cm_wide <- readRDS("CELTICmodel_wide.rds")
#------------------------------------------------------------------------------
# summary statistics tables
# set JAMA theme
## gtsummary::theme_gtsummary_journal(journal = "jama", set_theme = TRUE)
## gtsummary::theme_gtsummary_compact()
## function for construct table with median (IQR) and frequency (%)
## summary.table <- function(x) {
##		by = Group, 
##		type = everything() ~ "continuous",
##		missing_text = "Missing") %>%
##	bold_labels() %>%
##	modify_header(label ~ "**Variable**") %>%
##	modify_footnote(starts_with("stat_") ~ "Median (IQR) or 
##	##										Frequency (%)") %>%
##	as_flex_table()
##}
# subset for timepoints
##cm_timepoints <- list()
##timepoint_suffix <- c("_base", "_0hrs", "_1hrs", "_2hrs", "_4hrs", "_6hrs")
##for (i in timepoint_suffix) {
##	cm_timepoints[[i]] <- cm %>%
##		dplyr::select(ends_with(c(i, "Group")))
##}
# baseline summary table in word format
##summary_table_base <- summary.table(cm_timepoints$`_base`)
##doc <- officer::read_docx()
##doc <- flextable::body_add_flextable(doc, value = summary_table_base)
##print(doc, target = "CELTICmodel_baseline_summarystats.docx")
# baseline summary table in word format
##summary_table_injury <- summary.table(cm_timepoints$`_0hrs`)
##doc <- officer::read_docx()
##doc <- flextable::body_add_flextable(doc, value = summary_table_injury)
##print(doc, target = "CELTICmodel_injury_summarystats.docx")
##summary_table_end <- summary.table(cm_timepoints$`_6hrs`)
##doc <- officer::read_docx()
##doc <- flextable::body_add_flextable(doc, value = summary_table_end)
##print(doc, target = "CELTICmodel_end_summarystats.docx")
#------------------------------------------------------------------------------
# print descriptive statistics
# overall
overall.stats <- cm_wide %>%
	dplyr::select(-c(Sheep_ID, Group))  %>%
	psych::describe(quant = c(0.25, 0.75))
# by group
group.stats <- cm_wide %>%
	dplyr::select(c(Ur_output, Cumulative_balance))  %>%
	psych::describeBy(group = cm_wide$Group, quant = c(0.25, 0.75))
#==============================================================================
