#==============================================================================
# CELTIC model analysis
# Reapeated measures mixed models
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
shhh(groundhog.library(lme4, groundhog_day))
shhh(groundhog.library(rstatix, groundhog_day))
#------------------------------------------------------------------------------
set.seed(1984)
cm <- readRDS("CELTICmodel_long.rds")
#------------------------------------------------------------------------------
# order timepoints and convert Variable/Sheep_ID to factors
cm$Timepoint <- factor(cm$Timepoint, levels = c("base", "0hrs", "1hrs", "2hrs", 
	"4hrs", "6hrs"))
cm <- cm %>%
	dplyr::mutate(Variable = as.factor(Variable), 
				  Sheep_ID = as.numeric(Sheep_ID))
#------------------------------------------------------------------------------
# create variable subsets
variable_sets <- list()
variables <- unique(cm$Variable)
for (i in variables) {
	variable_sets[[i]] <- cm %>%
							dplyr::filter(Variable == i) %>%
							dplyr::select(-Variable) %>%
							as.data.frame()
}
names(variable_sets) <- variables
list2env(variable_sets, envir = .GlobalEnv)
#------------------------------------------------------------------------------
# fit model
# mixed model with intercept allowed to vary by animal
data_fit <- lme4::lmer(Value ~ Group*Timepoint + (1|Sheep_ID), data = data)
#------------------------------------------------------------------------------
# obtain model summaries and post-hoc comaprisons
## summary summary(data_fit)
## tidy(Anova(data_fit, type = "III"))
## data %>% 
##	group_by(Timepoint) %>% 
##	rstatix::tukey_hsd(Value~Group, p.adjust.method = "fdr")
#==============================================================================
# plot observations
# summarise variable datsets
summarised_sets <- list()
for (i in 1:length(variable_sets)) {
	summarised_sets[[i]] <- Rmisc::summarySE(data = variable_sets[[i]], 
								measurevar = "Value", 
								groupvar = c("Group", "Timepoint"), 
								na.rm = TRUE)
}
names(summarised_sets) <- unique(as.character(cm$Variable))
# plot code
# choose variable ...
## plot.df <- summarised_sets$...
## plot.df$Timepoint <- as.factor(plot.df$Timepoint)
## plot.df$Timepoint <- factor(plot.df$Timepoint, 
##	levels = c("base", "0hrs", "1hrs", "2hrs", "4hrs", "6hrs"))
##plot <- ggplot(plot.df, aes(Timepoint, Value, group = Group, color = Group)) +
##	geom_line() +
##	geom_point() +
##	geom_errorbar(aes(ymax = Value+ci, ymin = Value-ci, width = 0.1)) +
##	scale_y_continuous(limits=c(a,b), breaks=seq(d,e,f), expand = c(0,0)) +
##	theme_classic() +
##	scale_color_brewer(palette = "Set2")
#==============================================================================
