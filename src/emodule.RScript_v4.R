

# This is a R-script to show the possibilities for statistics in research.

# A data file is generated to show the possibilities of R and to show how data can be analysed. 
# But remember more ways are possible to get the same/similar result.

# A dataset is simulated for species PHOENIX.
# The Phoenix is a sign of death and rebirth in the Greek mythology. 
# Feeling drawn to the Phoenix or seeing it in your dreams can be a prophetic symbol 
# that difficulties are coming your way but with the strength of the phoenix behind you, 
# you will prevail.

##### no need to run next lines when you received the resulting files (simulated_data.csv en simulated_data.xlsx) 
set.seed(1234321)
sim.data <- data.frame(id = c(1:200), 
                       bw = round(c(rnorm(100, 4.9, 1.2), rnorm(100, 3.9, 1.2)), 1),
                       sex = c(rep("M", 100), rep("F", 100)),
                       diseased = c(rbinom(100, 1, 0.3), rbinom(100, 1, 0.4)),
                       immuno.status = I(rbinom(200, size = 2, prob = c(0.1,0.2,0.8)) + 1)
                       )
sim.data$age <- round(sim.data$bw + abs(rnorm(200, 0, 2))) 
sim.data$age[sim.data$age < 2] <- 2

write.table(sim.data, file = "simulated_data.csv", quote = FALSE, sep = "\t", dec = ".", row.names = FALSE,
            col.names = TRUE)
rm(sim.data) # remove/delete unnecessary objects, clean desk policy
# start excel , open simulated_data.csv, change column via menu Data "Text to column" and save as simulated_data.xlsx

### Start from here when you have the data files


################
## Part 1 Research question
################
# Research questions
# = Is the body weight associated with sex, immunological status, disease status and/or age?
#
# This can be split in several questions:
# = 1. Is sex associated with body weight?
# = 2. Is disease status related to body weight?
# = 3. Is immunological status related to body weight?
# = 4. Is age related to body weight?
# = 5. But at the end of the day we want to study all explanatory variable together with body weight
#
# = Another research question could be:
# = Is disease status associated with sex, immunological status and/or age?
# Again: this can be split in several questions:
# = 6. Is sex associated with disease status?
# = 7. Is immunological status related to disease status?
# = 8. Is age related to disease status?
# = 9. But at the end we want to study all explanatory variable together with disease status
#
# Each of the questions should be addressed separately.
# Best to start is with the simple univariable summaries and bivariable associations (1-6).


################################
## Part 2 Reading the data into R and study the content
################################

# R, the basics: https://rpubs.com/alliechoate/504079

## Part 2a Cookbook/Legend of the dataset: short description explaining the content of the dataset

# id = identification number of the subject
# bw = body weight (kg)
# sex = the sex of the subject (M = male, F = female)
# diseased = disease status of the subject (0 = not diseased, 1 = diseased)
# immuno.status = immunological status of the subject (1= low, 2 = medium, 3 = high)
# age = age of the subject (year)

# Reading/loading the data depends on the file format of the data
# The format could be as e.g. unformatted txt of csv file (Part 2a) or an excel file (Part 2b)

## Part 2b Reading .txt or .csv file and study the imported data
sim.data <- read.table(file = "simulated_data.csv", sep = "\t", dec = ".", header = TRUE)

## checking the imported file
# Show the first 6 records
head(sim.data, 6)
# Show the last 6 records
tail(sim.data, 6)
# record number 25 - 30
sim.data[25:30,]
# simple summary per variable (column)
summary(sim.data)

## Part 2a Reading .xlsx file
#
# installing packages/libraries with commands which are not available in the standard installation
install.packages("readxl") # run only once, after installation add a hashtag in front of this line and save the script

# library for reading xlsx file
library(readxl)
# which sheets are available in the excel file?
excel_sheets("simulated_data.xlsx") # which sheets are present in the xlsx file? Which one contains the data for analysis?
sim.data <- read_xlsx("simulated_data.xlsx", sheet = "data")
## checking the imported file
# Show the first 6 records
head(sim.data, 6)
# Show the last 6 records
tail(sim.data, 6)
# record number 25 - 30
sim.data[25:30,]
# simple summary per variable (column)
summary(sim.data)

################################
## Part 3 Summary statistics: 
## to summarize the data 
## - getting to know the data better for (unexpected) outliers and distribution of the individual variables
## - to calculate statistics (i.e. mean, proportion), construct graphs and tables to answer the research question intuitively
################################

# let's install some extra packages which contain useful commands
# library for handling the data
# Description of the library psych: https://cran.r-project.org/web/packages/psych/vignettes/intro.pdf
install.packages("psych") # only one time needed to install, when done then put hashtag in front of this line!
library(psych)

# library for nice plotting
# Description of the library ggplot2 : https://www.tutorialspoint.com/ggplot2/ggplot2_tutorial.pdf
install.packages("ggplot2") # only one time needed to install, when done then put hashtag in front of this line!
library(ggplot2)

# library for handling the data
# Description of the library  dplyr: https://cran.r-project.org/web/packages/dplyr/vignettes/dplyr.html
install.packages("dplyr") # only one time needed to install, when done then put hashtag in front of this line!
library(dplyr)

# names of the variables
names(sim.data)
# simple summary per variable of the data
summary(sim.data)

# frequency tables of original variables
attach(sim.data)
table(sex)
table(diseased)
table(immuno.status)
detach(sim.data)

# how to label variables to be more informative
# cheat sheet dplyr: https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf

# recode variables https://dplyr.tidyverse.org/reference/recode.html
sim.data <- sim.data %>% mutate(sex = recode_factor(sex, `F` = "Female", `M` = "Male"),
                                diseased = recode_factor(diseased, `0` = "Not diseased", `1` = "Diseased"),
                                immuno.status = recode_factor(immuno.status, `1` = "low", `2` = "medium", `3` = "high"))

# frequency tables of adjusted variables; check if the adjustments worked well !
attach(sim.data)
table(sex)
table(diseased)
table(immuno.status)
detach(sim.data)

######################
## Part 3a Statistics: i.e. mean, sd, proportion
######################

attach(sim.data) # open/activate the data object into memory
# some summary statistics for a continuous variable i.e. body weight
# one continuous variable
?describe # to receive help info about this command
describe(bw)
# one continuous variable split by ONE categorical variable
?describeBy # to receive help info about this command and arguments i.e. mat and digits
describeBy(bw, sex, mat = T, digits = 1) # run: ?describeBy
describeBy(bw, immuno.status, mat = T, digits = 1)
# one continuous variable split by TWO categorical variables
describeBy(bw, list(immuno.status, sex), mat = T, digits = 1)

# >>> TODO yourself: Make above statistics for age

# frequency table for a categorical variable i.e. sex
table(sex) # frequencies
prop.table(table(sex)) # proportions
prop.table(table(sex))*100 # percentages

# >>> TODO yourself: Make above frequency tables for diseased and immuno.status

# contingency/cross table for sex x immuno.status
table(sex, immuno.status) # freqeuncies
?prop.table # ask for help inforation about this command
prop.table(table(sex, immuno.status), margin = 1) # total sum up within sex to 1, you can write just 1 (removing margin =)
prop.table(table(sex, immuno.status), margin = 2) # total sum up within immuno.status to 1
# Which of 2 previous commands is most important? Think about the biological direction.

# >>> TODO yourself: Make above tables for sex x immuno.status

# contingency/cross table for sex x immuno.status x diseased
table(immuno.status, diseased, sex) # cross table immuno.status x diseased per sex
prop.table(table(immuno.status, diseased, sex)) # total added up to 1
prop.table(table(immuno.status, diseased, sex), c(1,3))  # total sum up to 1 within immuno.status per sex
prop.table(table(immuno.status, diseased, sex), c(2,3))  # total sum up to 1 within diseased per sex
prop.table(table(immuno.status, diseased, sex), c(1))  # total sum up to 1 within immuno.status
prop.table(table(immuno.status, diseased, sex), c(2))  # total sum up to 1 within diseased 
prop.table(table(immuno.status, diseased, sex), c(3))  # total sum up to 1 within sex
detach(sim.data) # close the data object

# Above tables can be constructed in LONG form using commands of the dplyr library
# Split the data by sex and count the number of sex and the average body weight by sex
df <- sim.data %>% group_by(sex) %>% summarise(count = n(), avg.bw = mean(bw))
df # de tabel
rm(df) # clean desk policy

# Split the data by sex and diseased and count the number by sex x diseased and the average body weight by sex x diseased
df <- sim.data %>% group_by(sex, diseased) %>% summarise(count = n(), avg.bw = mean(bw))
df # de tabel
rm(df) # clean desk policy

# Split the data by sex and diseased and count the number by sex x diseased x immuno.status 
# and the average body weight by sex x diseased x immuno.status
df <- sim.data %>% group_by(sex, diseased, immuno.status) %>% summarise(count = n(), avg.bw = mean(bw))
df # de tabel
rm(df) # clean desk policy

#########################
## Part 3b Graphs
## Summarizing the data in graphs is a good way to explore the data and show the (possible) association in the data
#########################
# cheat sheet ggplot2: https://www.maths.usyd.edu.au/u/UG/SM/STAT3022/r/current/Misc/data-visualization-2.1.pdf
# Fundamentals of Data Visualization (with ggplot2), Claus O. Wilke , https://clauswilke.com/dataviz/

# scatterplot bw versus age
# https://r-charts.com/correlation/scatter-plot-ggplot2/
# next line the basic of the graph is assigned to p, in next lines an extra feature is added to p (the graph)
# p becomes a completer graph
p <- ggplot(sim.data, aes(x = age, y = bw)) 
p <- p + geom_point(aes(colour = diseased)) # add dot for each observation
p <- p + labs(title = "Scatterplot of body weight by age", x = "Age (year)", y = "Body weight (kg)")
p <- p + theme(plot.title = element_text(size = 20), axis.title = element_text(size = 12), axis.text = element_text(size = 10))  # x-axis text size
p
rm(p) # why add and run this line?

# boxplot bw
# https://www.r-bloggers.com/2016/11/make-a-box-plot-with-single-column-data-using-ggplot2-tutorial/
p <- ggplot(sim.data, aes(x = "", y = bw))
p <- p + geom_boxplot() + geom_point()
p <- p + labs(title = "boxplot of body weight", y = "Body weight(kg)")
p
rm(p) # clean desk policy

# Similar to previous graph with some adjustments
p <- ggplot(sim.data, aes(x = "", y = bw))
p <- p + geom_boxplot() + geom_point()
p <- p + labs(title = "Boxplot of body weight", x= "", y = "Body weight (kg)") # no x-axis label
p <- p + coord_flip() # graph is flipped
p
rm(p) # clean desk policy

# boxplot bw by sex
# https://r-graph-gallery.com/89-box-and-scatter-plot-with-ggplot2.html
p <- ggplot(sim.data, aes(x = sex, y = bw))
p <- p + geom_boxplot() + geom_point()
p <- p + labs(title = "boxplot of body weight per sex", x = "Sex of the subject", y = "Body weight of the Phoenix (kg)")
p
rm(p) # clean desk policy

# boxplot bw per sex with jitter
p <- ggplot(sim.data, aes(x = sex, y = bw))
p <- p + geom_boxplot() + geom_jitter(width = 0.1) # spread the points in horizontal direction 
p <- p + labs(title = "boxplot of body weight per sex", x = "Sex of the Phoenix", y = "Body weight of the Phoenix (kg)")
p
rm(p) # clean desk policy

## How to construct a barplot 
# first construct a table in long form for sex, disease status and average bw
# https://dplyr.tidyverse.org/reference/group_by.html
df <- sim.data %>% group_by(sex, diseased) %>% summarise(count = n(), avg.bw = mean(bw))
df # show the constructed table in object df

# https://www.statology.org/ggplot-change-x-axis-labels/
# https://statisticsglobe.com/change-font-size-of-ggplot2-plot-in-r-axis-text-main-title-legend

# barplot sex versus diseased
p <- ggplot(df, aes(x = sex, y = count, fill = diseased)) # use the constructed object df
p <- p + geom_bar(stat="identity", position = "dodge") # a bar of each disease status by sex; dodge means along each other
p <- p + labs(title = "Barplot of disease status per sex", x = "Sex of the subject", y = "Frequency")
p <- p + labs(fill = "Disease status") # changing the legend name
p <- p + scale_x_discrete(labels=c("Female", "Male"))
p
rm(p)

# similar barplot sex versus diseased with adjustment
p <- ggplot(df, aes(x = sex, y = count, fill = diseased))
p <- p + geom_bar(stat="identity", position = "stack")  # one bar of disease status stacked (on top of each other) by sex
p <- p + theme(text = element_text(size = 20)) # Change ALL font sizes to 20
p
rm(p)

# similar barplot sex versus diseased with adjustment
p <- ggplot(df, aes(x = sex, y = count, fill = diseased))
p <- p + geom_bar(stat="identity", position = "fill") # relative frequency adding up to 1
p <- p + labs(title = "Barplot of disease status per sex", x = "Sex of the subject", y = "Proportion") # adding the axis labels and title
p <- p + theme(plot.title = element_text(size = 20), axis.title = element_text(size = 12), axis.text = element_text(size = 10))  # changing font size
p
rm(p)

# a barplot of avergae body weight (continguous) by sex and split by disease status
p <- ggplot(df, aes(x = sex, y = avg.bw, fill = diseased))
p <- p + geom_col(position = position_dodge()) # position_dodge means along each other
p <- p + labs(title = "Barplot of average body weight per sex", x = "Sex of the subject", y = "Proportion")
p <- p + theme(plot.title = element_text(size = 20), axis.title = element_text(size = 12), axis.text = element_text(size = 10))  # x-axis text size
p
rm(p)

# >>> TODO yourself: make adjustments to the last graph for font size, title name and legend name 

rm(df) # why remove df?

# An example of a scatterplot
# https://r-charts.com/correlation/scatter-plot-ggplot2/
p <- ggplot(sim.data, aes(x = age, y = bw))
p <- p + geom_point(aes(colour = diseased)) # position_dodge means along each other
p <- p + labs(title = "Scatterplot of body weight versus age", x = "Age (year)", y = "Body weight (kg)")
p <- p + theme(plot.title = element_text(size = 20), axis.title = element_text(size = 12), axis.text = element_text(size = 10))  # x-axis text size
p
rm(p)


############################
## Part 4 Testing associations (inferential statistics)
############################

# in this part one univariable approach is handled body weight ~ sex (body weight dependent of age)
# The research goal (Part 1) can be split in several questions.
# = 1. Is sex associated with body weight?
# some descriptives (but also graphs and tables are part of it, see above)

# intuitive conclusion for this research question based on descriptive statistics
attach(sim.data)
describeBy(bw, sex, na.rm = TRUE, mat = T, digits = 2)
detach(sim.data)

# boxplot bw by sex
p <- ggplot(sim.data, aes(x = sex, y = bw))
p <- p + geom_boxplot() + geom_jitter(width = 0.1)
p <- p + labs(title = "Boxplot of body weight by sex", x = "Sex of the subject", y = "Body weight of the Phoenix (kg)")
p
rm(p) # clean desk policy

# Choosing the statistical model;
# The outcome variable (bw) is continuous, the independent variable is a group (categorical/binary)
# this leads to a t-test which belongs to the family of linear models 
attach(sim.data)
t.test(bw ~ sex, var.equal = TRUE) 
detach(sim.data)

# Conclusion? Using the descriptive statistics, graphs and model results: how would you answer the research question?
# Explain in a simple way which every one can understand including your (grand) parents!


# >>> TODO yourself:  Run same model for 2. Is disease status related to body weight?
# Why cannot this model t-test) NOT be applied for bw ~ immuno.status?


# Let's run a general linear model for bw ~ sex  model
fit <- glm(bw ~ sex, data = sim.data, family = gaussian)
# Model summary
summary(fit) # compare these results with the results from the previous t.test(bw ~ sex, var.equal = TRUE)
# interpretation of estimate for intercept: mean of the reference group/category
# interpretation of other estimates: difference between each of other group means  and the mean of the reference group

# calculate the 95% confidence interval for the estimates
round(confint(fit), 2) # rounding on two digits
# 95% confidence interval: 95% probability the range of the confidence interval will contain the true difference between group means

# But we can do more: assess the validity of the model: analysis of the residuals
# normality?
qqnorm(resid(fit))
abline(0, sd(resid(fit)))
# constant variability / homoscedasticity?
plot(predict(fit), resid(fit))
abline(h = 0)
#
# Did the model show statistical significance? 
# Several options to choose the best model i.e.:
# -1. Akaike's Information Criterion (AIC) : smaller is better (more efficient) model.
# -2. p-value: Likelihood ratio test (LRT): p < 0.05 
# We assess the studied statistical association in total (not just the difference between 2 group means when there are more groups)
drop1(fit, test = "Chisq")
# is model with or without immuno.status best?
# When removing sex from the model (single term deletion), the AIC will be higher meaning information loss: model including sex is better
# Does this fit with the graphical presentation of this association and the summary statistics? Yes, it does fit. 
rm(fit) # clean desk policy

# >>> TODO yourself: Run similar general linear model approach (glm(bw ~ ..., family= gaussian)) also for 
# - 2. Is disease status related to body weight?
# - 3. Is immunological status related to body weight?
# - 4. Is age related to body weight?

# >>> TODO yourself: Study the difference between the model results of bw ~ disease and bw ~ age.
# Describe the difference in interpretation of the coefficients.


# = 5. But at the end of the day we want to study all explanatory variables together with body weight

# First: An intuitive conclusion for this research question based on descriptive statistics (see previous summary statistics, tables and graphs)
# Second: a large multidimensional model
# Modelling body weight with sex, disease status, immunological status and age together: multivariable model
# In the univariable models other factors/variables were not taken into account: unadjusted estimates
# In the following we take other variables into account: adjusted estimates
fit <- glm(bw ~ sex + diseased + immuno.status + age , data = sim.data, family = gaussian)

# study validity of the model by the residuals
# normality?
qqnorm(resid(fit))
abline(0, sd(resid(fit))) 
# This is great: residuals follow the solid line nicely = normal distribution of the residuals

# constant variability / homoscedasticity?
plot(predict(fit), resid(fit))
abline(h = 0)
# The variability should be scattered around 0 (zero) in a horizontal band (not widening, de- or increasing or other pattern)
# this is not very great but for this approach/example we accept this.

# Which variable show statistical association with outcome bw?
drop1(fit, test = "Chisq")
rm(fit)
# when removing sex or age from the model the AIC increases: loss of information
# when removing diseased or immuno.status from the model the AIC decreases: no loss of information
# 
# When the ultimate research goal is to study the disease status on body weight
# then we keep (forced in) disease status in the model at all times to answer the research question (estimate and 95% CI)
# otherwise we can stepwise simplify the model by removing the unnecessary (no) association.
# Let us remove the variable improving the model most: immuno.status

# remove immuno.status and rerun the model
fit <- glm(bw ~ sex + diseased + age , data = sim.data, family = gaussian)
# Model summary
summary(fit) 
# Which of remaining variables show statistical association with outcome bw?
drop1(fit, test = "Chisq")
# we can also remove diseased
rm(fit)

# remove diseased and rerun the model
fit <- glm(bw ~ sex + age , data = sim.data, family = gaussian)
# Model summary
summary(fit) 
# Which of remaining variables show statistical association with outcome bw?
drop1(fit, test = "Chisq")
# we can NOT simplify the model more without loosing information
# 95% confidence interval, interpretation?
round(confint(fit), 2)

rm(fit) # clean desk policy

# >>> TODO yourself: 
# - formulate the conclusion of this modelling approach
# - write some lines to present the results in a concise but meaningful text
# - can non-skilled readers understand your text?

# Calculation of the correlation coefficient  between bw and age
attach(sim.data)
cor(bw, age) # only correlation
cor.test(bw, age) # statistical test for correlation
detach(sim.data)

#
# Are explanatory variables associated or independent?
# = 5. Is sex associated with disease status? 
# = 6. Is immunological status associated with disease status?
# = 7. Is age associated with disease status?


# = 5. Is sex associated with disease status? 

# intuitve conclusion for this research question based on descriptive statistics
# make a contingency (cross) table
attach(sim.data)
table(sex, diseased)
prop.table(table(sex = sex, disease.status = diseased), margin = 1)
detach(sim.data)

## construct a barplot 
df <- sim.data %>% group_by(sex, diseased) %>% summarise(count = n())
df # show the constructed table in object df
# barplot sex versus diseased
p <- ggplot(df, aes(x = sex, y = count, fill = diseased)) # use the constructed object df
p <- p + geom_bar(stat="identity", position = "dodge") # a bar of each disease status by sex
p <- p + labs(title = "Barplot of disease status per sex", x = "Sex of the subject", y = "Frequency")
p <- p + labs(fill = "Disease status") # changing the legend name
p <- p + scale_x_discrete(labels=c("Female", "Male"))
p
rm(p)

# Choosing the statistical model;
# The outcome variable (diseased) is categorical (binary), the independent variable (sex) is a group (categorical/binary)
# this leads to a chi-square test and/or logistic regression
# chi-square test
attach(sim.data)
chisq.test(table(sex, diseased))
detach(sim.data)
# >>> TODO yourself: 
# - which sex has highest risk? 
# - assuming no association between sex and diseased, what would you expect 
#   for the risk to be diseased in male and females respectively? Equal risk in both sexes
# - calculate the odds p/(1-p) in the females and take the e-log of the odds. 
#    odds = 0.39/0.61 = 0.639; logodds = log(0.639) = -0.4473
# - calculate the odds p/(1-p) in the males and take the e-log of the odds. 
#    odds = 0.34/0.66 = 0.515; logodds = log(0.515) = -0.663
# - calculate the difference between both logodds, this is the estimate for the log OR

# logistic regression to assess the strength of association
fit <- glm(diseased ~ sex, data = sim.data, family = binomial)
# Model summary
summary(fit) 
# >>> TODO yourself: 
# - which sex is the reference category? Females
# - compare the obtained results with calculated log-odds and difference
# Has sex a statistical association with outcome disease status?
drop1(fit, test = "Chisq")
# The association between sex and disease status is weak and not very informative
# Calculating the odds ratio for sex with disease status. e^logOR
# obtain the coefficients:
coef(fit)
# exponentiate the estimates
exp(coef(fit))
# Calculate the 95% confidence interval for logodds and logOR:
confint(fit)
# exponentiate the 95% confidence interval for logodds and logOR:
round(exp(confint(fit)), 3)
rm(fit)

# >>> TODO yourself: Run similar analysis for the association between immunological status and age with disease status
# = 7. Is immunological status associated with disease status?
# = 8. Is age associated with disease status? But one problem: is the association (logOR) constant over time?
#      Study the discretized age as independent variable: cut(age, c(0, 4, 6, 8, 12)) (but limits are arbitrary)


# = 9. But at the end we want to study all explanatory variable together with disease status
# logistic regression to assess the strength of association
fit <- glm(diseased ~ sex + immuno.status + cut(age, c(0, 4, 6, 8, 12)), data = sim.data, family = binomial)
# Model summary
summary(fit) 
# Which variable has a statistical association with outcome disease status?
drop1(fit, test = "Chisq")
# The association of each of the independent variables with disease status is weak and not very informative
# the association with categorized variable age is the least (AIC improves most when deleted from the model)
rm(fit)

# remove age
fit <- glm(diseased ~ sex + immuno.status, data = sim.data, family = binomial)
# Model summary
summary(fit) 
# Which variable has a statistical association with outcome disease status?
drop1(fit, test = "Chisq")
rm(fit)

# when removing immuno.status from the model: loss of information  (AIC increases when variable is removed)
# When removing sex from the model the AIC decreases: no loss of information (AIC decreases when variable is removed)
# 
# When the ultimate research goal is to study the disease status on disease status
# then we keep (forced in) disease status in the model at all times to answer the research question (estimate and 95% CI)
# otherwise we can stepwise simplify the model by removing the unnecessary (no) association.

# Remove sex
fit <- glm(diseased ~ immuno.status, data = sim.data, family = binomial)
# Model summary
summary(fit) 
# Which variable has a statistical association with outcome disease status?
drop1(fit, test = "Chisq")

# The association of immuno status with disease status is weak and not very informative
# but this is the final model to answer the research question
# obtain the coefficients:
coef(fit)
# exponentiate the estimates
exp(coef(fit))
# Calculate the 95% confidence interval for logodds and logOR:
confint(fit)
# exponentiate the 95% confidence interval for logodds and logOR:
round(exp(confint(fit)), 3) # round the estimates
rm(fit)

# When also one or more of the independent variables remained in the model
# then we would obtain adjusted odds ratios: odds ratio adjusted of the other variables in the model.

# Survival analysis
# The age was measured at a certain moment in time and if the disease was present (diseased = 1) at that time

library(survival)
fit <- survfit(Surv(age, diseased == "Diseased") ~ sex, data = sim.data)
# Kaplan-Meier tables
summary(fit)
# Kaplan-Meier graph
plot(fit, lty = c(2,3))

# >>> TODO yourself: 
# - run the survival analysis also for immunological status and categorized age 

# cox proportinal hazards analysis
fit <- coxph(Surv(age, diseased == "Diseased") ~ sex, data = sim.data)
summary(fit)

# >>> TODO yourself: 
# - run the coxph analysis also for immunological status

# but at the end of the day we want all independent variables in one model
fit <- coxph(Surv(age, diseased == "Diseased") ~ sex + immuno.status, data = sim.data)
summary(fit)
drop1(fit, test = "Chisq")

# study the proportional hazards assumption (Should be constant over time)
cox.zph(fit)
par(mfrow = c(1, 2))
plot(cox.zph(fit))
par(mfrow = c(1, 1))


