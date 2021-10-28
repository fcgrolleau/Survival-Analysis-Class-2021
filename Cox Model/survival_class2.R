getwd() # tells you what your current working directory is
#setwd("/Users/francois/Desktop/TEAM METHODS/CCA Teaching/Parcours/Survival_analysis_FG_2020")

# load the dataset from your current working directory 
hfdata <- read.csv(file = "hf_survival_data.csv", header=TRUE, sep=',')


install.packages("survival")
install.packages("survminer")

library(survival)
library(survminer)

#######################################
#### Univariable Cox PH regression #### 
####################################### 

# Inspect what the Surv function does
Surv(fu_time, death)


# Fit a Cox PH model from hfdata with age as a linear predictor
cox <- coxph(Surv(fu_time, death) ~ age, data = hfdata) 
summary(cox)

# Fit a Cox PH model from hfdata with ethnicgroup as a categorical predictor
cox <- coxph(Surv(fu_time, death) ~ ethnicgroup, data = hfdata) 
summary(cox)

### … You’ll get just one coefficient for ethnicgroup. This is because unless 
### you tell it otherwise, R will assume that all your variables are continuous.
### Ethnicity is very obviously not a continuous variable, but R doesn’t know
### that unless you tell it! 

# Can also use “as.factor” rather than “factor”
ethnicgroup <- factor(hfdata[,"ethnicgroup"]) 
fu_time <- hfdata[,"fu_time"]
death <- hfdata[,"death"]

cox <- coxph(Surv(fu_time, death) ~ ethnicgroup)
summary(cox)

### 43 patients with missing ethnicgroup data were excluded. Let's consider their
### ethnic group unknown and refit the model

levels(ethnicgroup)<-c(levels(ethnicgroup),"8") # add level 8 to the factor
ethnicgroup[is.na(ethnicgroup)] <- "8" # Change NA to "None"

cox <- coxph(Surv(fu_time, death) ~ ethnicgroup)
summary(cox)

#######################################
### Multivariable Cox PH regression ### 
#######################################

# Insepect candidate predictors together or one by one as follows
summary(hfdata)

gender <- as.factor(hfdata[,"gender"]) # R calls categorical variables factors

t <- table(gender, exclude=NULL)
addmargins(t) # adds the total (a "sum" column)
round(100*prop.table(t),digits=1) # get %s rounded to 1dp
gender

copd <- as.factor(hfdata[,"copd"])
table(copd, useNA = "ifany")
prop.table(table(copd, useNA = "ifany"))*100

table(hfdata$prior_dnas, useNA = "ifany") 

table(hfdata$ethnicgroup, useNA = "ifany")

# Categorize data that is fundamentally not continuous
hfdata$ethnicgroup <- as.factor(hfdata$ethnicgroup)

# Fit a Multi variable Cox PH model
cox <- coxph(Surv(fu_time, death) ~
             age + gender + copd + prior_dnas + ethnicgroup,
             data = hfdata)

summary(cox)

### "gender1" and "ethnicgroup1" do not appear in the above output.
### This is because they are the reference categories for their respective
### variables. By default, R sets the reference category as the lowest value fo
### the variable, which is 1

### Watch out for patients with missing value too. There are 43 of them and these were
### simply excluded from the analysis... This can yield selection bias. The best thing to  
### do is proper imputation but this is not specific to survival analysis and beyond the  
### scope of this class.
