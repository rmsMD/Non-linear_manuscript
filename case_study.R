
# Load required packages
library(rms)
library(rmsMD)
library(ggplot2)

# ---- Data loading and set-up ----

# load in the simulated data
# data has been simulated such that age has a linear relationship and BMI has a 
# U-shaped relationship with outcome
source("simulate_data.R")

# Set up data distribution information for rms. These two lines are standard when using the rms package
dd <- datadist(data)
options(datadist = "dd")

# quickly examine the dataframe. We will assume that data has been appropriately examined, plotted and cleaned
head(data, 10)

# ---- Data summary for demographics table ----

library(table1)

table1(~age + bmi + sex + smoking + as.factor(majorcomplication), data = data)

# ---- Model building and examining ----

# Fit logistic regression model using restricted cubic splines

fit <- lrm(majorcomplication ~ rcs(age, 3) + rcs(bmi, 3) + sex + smoking,
               data = data,
               x = TRUE, y = TRUE) 

# note, x = TRUE, y = TRUE is recommended for lrm and cph models to allow subsequent
# LR tests to be performed

# Check the model fit and diagnostics including number of observations and events
# Note the spline term coefficients (age, age', bmi, bmi') are difficult to interpret
fit

# Use modelsummary_rms function from rmsMD package to get an output for medical journals
# for spline terms this gives an overall p-value for the association of that variable with outcome
modelsummary_rms(fit)

# Use ggrmsMD from rmsMD package to plot the splines
# This determines which variables were analysed as RCS, and plots them appropriately
# As this is a logistic regression it plots OR and 95% confidence interval
# combined = TRUE means that a single combined plot with all spline terms in the model is outputted
ggrmsMD(fit, data, combined = TRUE)

# for logistic regression models. the lrm_prob argument can be used to plot predicted probabilities
# rather than odds ratios
ggrmsMD(fit, data, combined = TRUE, lrm_prob = TRUE)

# To assess whether rcs variables are significantly non-linear use anova
# for each RCS term, a p-value for "Nonlinear" is given
anova(fit, test = "LR")
# As expected, BMI, but not age, is significalty non-linear


# ---- Publication ready tables ----

# This section creates the results shown in Table 2 of the accompanying  manuscript.

# to output table use flextable and officer packages
library(flextable)
library(officer)

# modelsummary_rms will output a dataframe. first make this into a flextable
results <- modelsummary_rms(fit)
results_flextable <- flextable(results)

# output to a word document
doc <- read_docx()
doc <- body_add_flextable(doc, results_flextable)
print(doc, target = "Results_of_main_model.docx")

# creating a model which does not use RCS terms to use for comparison
# note this model assumes linear relationships, and incorrectly finds no association
# between bmi and outcome
fit_linear <- lrm(majorcomplication ~ age + bmi + sex + smoking,
           data = data,
           x = TRUE, y = TRUE)
fit_linear # check diagnostics etc
results_linear <- modelsummary_rms(fit_linear) # get results dataframe
results_linear_flextable <- flextable(results_linear)
doc <- read_docx() # set up word doc
doc <- body_add_flextable(doc, results_linear_flextable) # add results
print(doc, target = "Results_of_model_with_linear_assumption.docx")

# ---- Publication ready plots ----

# This creates Figure 2 of the accompanying manuscript.

# plot adjusted odds ratio. note the y axis is plotted here on a log-scale rather than
# a linear scale. The shade_inferior argument is used to shade either side of the no
# effect line (OR = 1) to give a visual cue for which side represents inferior outcome.
plots_OR <- ggrmsMD(fit, data, combined = FALSE,
                    shade_inferior = "higher",
                    ylab = "Occurence of surgical complications \n(adjusted OR)",
                    xlabs = list(age = "Age (years)", bmi = "BMI (kg/m²)"),
                    log_y = TRUE,
                    ylim = c(0.25, 4)
)

# plot the predicted probability
plots_prob <- ggrmsMD(fit, data, combined = FALSE,
                ylab = "Predicted probability of \nsurgical complications",
                xlabs = list(age = "Age (years)", bmi = "BMI (kg/m²)"),
                lrm_prob = TRUE,
                ylim = c(0,0.3)
)

# combine the plot lists and making a single multipanel figure
library(cowplot)
plotlist <- c(plots_OR, plots_prob)
plots <- plot_grid(plotlist = plotlist, labels = "AUTO", align = "vh")

# to save as pdf
ggsave(file = "Case_study_RCS_plot.pdf", plot = plots, width = 8, height = 8)

# to save as jpg
ggsave(file = "Case_study_RCS_plot.jpg", plot = plots, width = 8, height = 8)



