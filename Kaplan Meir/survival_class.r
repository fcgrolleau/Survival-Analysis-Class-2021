getwd() # tells you what your current working directory is
setwd("/Users/francois/Desktop/TEAM METHODS/CCA Teaching/Parcours/Survival_analysis_FG_2020")

# load the dataset from your current working directory 
hfdata <- read.csv(file = "hf_survival_data.csv", header=TRUE, sep=',')
dim(hfdata) # check the dataset dimensions
head(hfdata) # check the dataset first 6 rows 
hfdata[35:41,] # check rows 35 to 41 of the dataset 

#install.packages("survival")
#install.packages("ggplot")
#install.packages("survminer")

library(survival) # this is the cornerstone command for survival analysis in R
library(ggplot2) # newer package that does nice plots
library(survminer) # newer package that does nice kaplan meir tables and plots

gender <- as.factor(hfdata[,"gender"]) # R calls categorical variables factors
fu_time <- hfdata[,"fu_time"] # continuous variable (numeric) 
death <- hfdata[,"death"] # binary variable (numeric)

km_fit <- survfit(Surv(fu_time, death) ~ 1)
plot(km_fit)


plot(km_fit, conf.int = F, mark.time = T)

summary(km_fit, times = c(1:7,30,60,90*(1:10)))

km_gender_fit <- survfit(Surv(fu_time, death) ~ gender) 
plot(km_gender_fit)

survdiff(Surv(fu_time, death) ~ gender) # runs log-rank test by gender

hfdata$age_65plus <- ifelse(hfdata[,"age"]>=65,1,0) # dichotomise age
table(hfdata$age_65plus) # inspect the numbers - always a good idea
hfdata$age_65plus
table(hfdata$age, hfdata$age_65plus) # check - an even better idea...

survdiff(Surv(fu_time, death) ~ age_65plus, data = hfdata) # runs log-rank test by age>=65

# plot survival curves by age category (65 and above vs under 65)
survfit.obj<-survfit(Surv(fu_time, death) ~ age_65plus, data=hfdata)
plot(survfit.obj, col=c(4,2), mark.time=TRUE,
     xlab = "Days", ylab="Survival Probability",
     las=1)

# add a legend
legend(700, 1.05, c("under 65","65 and above"), lty = 1, col=c(4,2), bty="n")

# plotting with ggsurvplot (survminer package)
splots<-ggsurvplot(survfit.obj)
splots # first basic ggsurvplot

# we can make it even nicer
# for ploting details check http://www.sthda.com/english/wiki/survminer-r-package-survival-data-analysis-and-visualization
# or https://rpkgs.datanovia.com/survminer/index.html

splots<-ggsurvplot(survfit.obj,
                   ggtheme = theme_survminer() +
                             theme(plot.title = element_text(hjust = 0.5)),
                   title    = "Heart Failure Prognosis",
                   font.title=12,
                   legend.title = "Age 65 and over",
                   legend.labs = c("No", "Yes"),
                   pval=TRUE, pval.method = T,
                   risk.table = TRUE,
                   risk.table.fontsize = 5,
                   break.time.by = 120,
                   tables.theme = theme_cleantable(), 
                   tables.y.text = F)

splots$table <- splots$table + labs(title = "", subtitle = "No. at risk")
splots<- splots + labs(x  = "Days", y = "Overall Survival")

splots


