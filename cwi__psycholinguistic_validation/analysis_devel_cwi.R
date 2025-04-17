# Script to reproduce the analysis and results reported in: Patil, Calvillo,
# Lago, & Schuhmann (2025). Quantifying word complexity for Leichte Sprache: 
# A computational metric and its psycholinguistic validation. MT Summit 2025.
# Updated by Sol Lago, April 2025.

# ..............................................................................
# Load packages ----
# ..............................................................................

# R version 4.5.0.
# RStudio version 2024.12.1+563. 

# Load packages.
library(tidyverse)            # data manipulation and plotting
library(scales)               # scales for plots
library(patchwork)            # concatenate plots
library(corrplot)             # calculate correlations
library(lmerTest)             # frequentist stats
library(ggeffects)            # model predictions
library(emmeans)              # nested comparisons
library(performance)          # model diagnostics
library(texreg)               # export tables
library(xtable)               # export tables

# ..............................................................................
# Load data ----
# ..............................................................................

# Load file including DeveL dataset and CWI complexity.
develdata <- read.table("data_devel_cwi.csv", 
                        stringsAsFactors = TRUE,
                        na.strings = NA,
                        sep = ",", 
                        fileEncoding = "UTF-8",
                        header = TRUE)

# ..............................................................................
# Average child performance across grades ----
# ..............................................................................

# Group child data across grades 1-4 and 6.
develdata <- develdata |> 
  
  # Perform operations rowwise.
  rowwise() |> 
  
  # Compute mean response times across grades.
  mutate(rt_child_m = round(mean(na.rm = TRUE, c_across(
    c("rt_g1_m", "rt_g2_m", "rt_g3_m", "rt_g4_m","rt_g6_m"))))) |> 
  
  # Compute number of children across grades.
  mutate(rt_child_n = sum(na.rm = TRUE, c_across(
    c("rt_g1_n", "rt_g2_n", "rt_g3_n", "rt_g4_n","rt_g6_n")))) |> 

  # Ungroup to return data frame to its original grouping structure.
  ungroup()
  
# Exclude children grade-related columns.
develdata <- develdata |> dplyr::select(-("rt_g1_n":"rt_g6_se"))

# ..............................................................................
# Convert data to long format for plotting ----
# ..............................................................................

# Create data frame in long format for plotting.
plotdata <- develdata |>
  
  # Keep only the columns with the mean response times per group.
  dplyr::select(c("word", "letter_cnt", "complexity_cwi", 
                  "dwds_lemma_freq", "dwds_old20",
                  "rt_child_m", "rt_ya_m","rt_oa_m")) |>
  
  # Convert data to long format.
  pivot_longer(cols = c("rt_child_m", "rt_ya_m","rt_oa_m"), 
               names_to = "group", 
               values_to = "response_time") |>
  
  # Re-order and re-name the group column.
  mutate(group = fct_relevel(group, "rt_child_m", "rt_ya_m", "rt_oa_m")) |>
  mutate(group = fct_recode(group, 
                            "child" = "rt_child_m",
                            "young" = "rt_ya_m",
                            "old"   = "rt_oa_m")) 

# ..............................................................................
# Figure 1: effect of CWI complexity in adults ----
# ..............................................................................

# Plotting parameters.
col_group <- c("#ffa600", "#003f5c")
lbl_group <- c("child" = "Children",
               "young" = "Younger adults",
               "old"   = "Older adults")

# Plot.
plot_complexity <- plotdata |>
  
  # Keep only adult data.
  filter(group %in% c("young", "old")) |> 
  droplevels() |> 
  
  # Plot.
  ggplot(aes(x = complexity_cwi, y = log(response_time), 
             group = group, color = group)) +
  
  # Draw points for the response time of each word.
  geom_point(shape = 16, size = .4, alpha = .4, show.legend = FALSE) + 
  
  # Add linear effect of complexity.
  geom_smooth(aes(fill = group), method = "lm", linewidth = .3, alpha = .5) +
 
  # Customize axes and scales.
  theme_light() +
  labs(x = "CWI word complexity", y = "Recognition time (log)") +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = .25)) +
  scale_color_manual(values = col_group, labels =lbl_group) +
  scale_fill_manual(values = col_group, labels =lbl_group) +
  
  # Customize theme.
  theme(legend.title = element_blank(), legend.position = "top") +
  theme(legend.text = element_text(size = 10)) + 
  theme(text = element_text(size = 12, colour = "gray28")) + 
  theme(strip.text = element_text(size = 12)) +
  theme(axis.title.x = element_text(size = 10)) +
  theme(axis.title.y = element_text(size = 10)) +
  theme(panel.grid = element_blank())

# Save plot.
ggsave(plot = plot_complexity, 
       filename = "Figure1_complexity.png", 
       units = "cm", 
       width = 10, 
       height = 10, 
       dpi = 600)

# ..............................................................................
# Stats: set-up and variable coding ----
# ..............................................................................

# Make word a factor.
develdata$word <- factor(develdata$word, ordered = FALSE)

# Center length variable.
# Length is the (integer) number of letters in a word.
develdata$letter_cnt_c <- as.numeric(
  scale(develdata$letter_cnt, scale = FALSE)) 

# Log and center type frequency variable (for adults and children).
# Normalized type frequency refers to the number of occurrences of a type, 
# (i.e. a distinct word form) in a corpus per million tokens. It is based 
# on the DWS corpus for adults and on the childLex corpus for children.
develdata$dwds_type_freq_c <- as.numeric(
  scale(log(develdata$dwds_type_freq), scale = FALSE)) 

develdata$child_type_freq_c <- as.numeric(
  scale(log(develdata$child_type_freq), scale = FALSE)) 

# Log and center lemma frequency variable (for adults and children).
# Lemma frequency is the total number of occurrences of a distinct word stem 
# (lemma) per million words. It is basedon the DWS corpus for adults 
# and on the childLex corpus for children.
develdata$dwds_lemma_freq_c <- as.numeric(
  scale(log(develdata$dwds_lemma_freq), scale = FALSE)) 

develdata$child_lemma_freq_c <- as.numeric(
  scale(log(develdata$child_lemma_freq), scale = FALSE)) 

# Center neighborhood size (for adults and children).
# OLD20 is the mean Levenshtein Distance from a word to its 20 closest 
# orthographic neighbors.
develdata$dwds_old20_c <- as.numeric(
  scale(develdata$dwds_old20, scale = FALSE)) 

develdata$child_old20_c <- as.numeric(
  scale(develdata$child_old20, scale = FALSE)) 

# Center trigram frequency (based on childLex type frequencies).
# This is the sum the sum of the frequencies of a sequence of three letters 
# within a word (again treating begin and end of a word as separate letters,
# Schröter & Schroeder, 2017).
develdata$trigram_c <- as.numeric(
  scale(develdata$trigram, scale = FALSE)) 

# Center CWI complexity variable.
develdata$complexity_cwi_c <- as.numeric(
  scale(develdata$complexity_cwi, scale = FALSE)) 

# ..............................................................................
# Stats: estimate correlations between predictors ----
# ..............................................................................

# The complexity measure is highly correlated with letter count and 
# orthographic neighborhood (positively), and with lemma and type frequency
# (negatively).

# Select only variables to be computed for correlation.
corrdata <- develdata |> 
  select(c("letter_cnt_c", "trigram_c",
           "dwds_lemma_freq_c", "child_lemma_freq_c",
           "dwds_type_freq_c", "child_type_freq_c",
           "dwds_old20_c", "child_old20_c",
           "complexity_cwi_c"))

# Calculate correlation table.
# Note the high correlation between the type and lemma frequency measures.
# To avoid multicollinearity in the statistical analysis, only type frequency
# was used as a predictor.
corrtable <- cor(corrdata, method = "pearson")
(corrtable)

# Visualize correlation table.
corrplot(corrtable, method = "circle", type = "lower")

# ..............................................................................
# Stats: create data for statistical analysis ----
# ..............................................................................

# "statdata" contains data only for adults, with the relevant predictors.
statdata <- develdata |>
  
  # Keep only the relevant columns for stats.
  dplyr::select(c("word", "letter_cnt_c", "trigram_c",
                  "dwds_lemma_freq_c", "dwds_type_freq_c",
                  "dwds_old20_c", "complexity_cwi_c",
                  "rt_ya_m","rt_oa_m")) |>
  
  # Convert data to long format to make "group" a factor.
  pivot_longer(cols = c("rt_ya_m","rt_oa_m"), 
               names_to = "group", 
               values_to = "response_time")

# Re-name and re-order levels for 'group'.
statdata$group <- fct_recode(statdata$group, 
                             "young" = "rt_ya_m",
                             "old"   = "rt_oa_m")

statdata$group <- fct_relevel(statdata$group , "young", "old")

# Set treatment contrasts for group (young adults are the reference level).
contrasts(statdata$group) <- contr.treatment(2, base = 1)

# ..............................................................................
# Stats: analysis adults ----
# ..............................................................................

# Run model.
# Note: since the predictors type and lemma frequency showed a correlation
# above .93 and caused multicollinearity issues, only type frequency was used 
# in the final model.
m_adult <- lmer(
  log(response_time) 
  ~ group 
  / (letter_cnt_c 
     + trigram_c 
     + dwds_type_freq_c  
     + dwds_old20_c 
     + complexity_cwi_c) 
  + (1 | word), 
  data = statdata)

# Inspect model.
summary(m_adult) 

# Diagnose model fit.
performance::check_model(m_adult, detrend = FALSE)

# Visualize model.
predict_response(m_adult, 
                 terms = c("complexity_cwi_c [0:1 by=0.1]", "group"), 
                 margin = "marginalmeans") |> plot()

# Check for collinearities between predictors.
# Note that the interaction between group and length has a higher correlation,
# but since it is only slightly above 10 (James et al., 2013), it is kept 
# in the model for exhaustivity.
performance::check_collinearity(m_adult) 

# ..............................................................................
# Stats: Table 1 ----
# ..............................................................................

# Extract statistics from adult model.
table1 <- data.frame(coef(summary(m_adult))) |> 
  
  # Remove degrees of freedom.
  dplyr::select(-df) |> 
  
  # Rename columns.
  rename(
    "Std. Error" = "Std..Error",
    "t-value" = "t.value",
    "p-value" = "Pr...t..") 

# Convert to Latex code (added manually to the manuscript).
print(xtable(table1, digits = 3), 
      type = "latex", 
      file = "Table1.tex")

# Print model statistics to Latex code (added manually to the manuscript).
texreg(m_adult)

# ..............................................................................
# Appendix: child group statistical analysis ----
# ..............................................................................

# The advantage of running a separate model for children is that we
# can use the frequency and neighborhood size predictors 
# derived specifically from child corpora.

# Model with baseline and complexity variables.
m_child <- lm(
  log(rt_child_m) 
  ~ letter_cnt_c 
  + trigram_c 
  + child_type_freq_c 
  + child_old20_c 
  + complexity_cwi_c,
  data = develdata)

# Inspect model.
summary(m_child)

# Diagnose model fit.
performance::check_model(m_child, detrend = FALSE)

# ..............................................................................
# Appendix: Figure A2.1 ----
# ..............................................................................

# Plot response times in children as a function of CWI complexity.

# Plotting parameters.
col_group <- c("#bc5090")
lbl_group <- c("child" = "Children (school grades 1–6)",
               "young" = "Younger adults",
               "old"   = "Older adults")

# Plot.
plot_children <- plotdata |>
  
  # Keep only adult data.
  filter(group %in% c("child")) |> 
  droplevels() |> 
  
  # Plot.
  ggplot(aes(x = complexity_cwi, y = log(response_time), 
             group = group, color = group)) +
  
  # Draw points for the response time of each word.
  geom_point(shape = 16, size = .4, alpha = .4, show.legend = FALSE) + 
  
  # Add linear effect of complexity.
  geom_smooth(aes(fill = group), method = "lm", linewidth = .3, alpha = .5) +
  
  # Customize axes and scales.
  theme_light() +
  labs(x = "CWI word complexity", y = "Recognition time (log)") +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = .25)) +
  scale_color_manual(values = col_group, labels =lbl_group) +
  scale_fill_manual(values = col_group, labels =lbl_group) +
  
  # Customize theme.
  theme(legend.title = element_blank(), legend.position = "top") +
  theme(legend.text = element_text(size = 10)) + 
  theme(text = element_text(size = 12, colour = "gray28")) + 
  theme(strip.text = element_text(size = 12)) +
  theme(axis.title.x = element_text(size = 10)) +
  theme(axis.title.y = element_text(size = 10)) +
  theme(panel.grid = element_blank())

# Save plot.
ggsave(plot = plot_children,
       filename = "FigureA2_children.png", 
       units = "cm", 
       width = 10, 
       height = 10, 
       dpi = 600)

# ..............................................................................
# Appendix: Table A2.1 ----
# ..............................................................................

# Extract statistics from model on child data.
tableA2 <- data.frame(coef(summary(m_child))) |> 
  
  # Rename columns.
  rename(
    "Std. Error" = "Std..Error",
    "t-value" = "t.value",
    "p-value" = "Pr...t..") 

# Convert to Latex code (added manually to the manuscript).
print(xtable(tableA2, digits = 3), 
      type = "latex", 
      file = "TableA2.tex")

# Print model statistics to Latex code (added manually to the manuscript).
texreg(m_child)
