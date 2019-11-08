
library(table1)
library(tidyverse)
library(knitr)
library(tableone)
library(finalfit)
library(here)
library(mlr)

# Read data
NHANES2 <- readRDS(here("data","processing_data", "NHANES2.Rda"))


# Create table 1
## Format for variables name
labels <- list(
  variables=list(age="Age", sex1="Sex", race="Race/Ethnicity", 
                 US_born="Country of Birth", marital="Marital Status",
                 education="Education", HH_member="Number of household members",
                 hypertension="Hypertension", diabetes="Diabetes", 
                 HD="Heart Diseases", H_TCHOL="Total Cholesterol", Low_HDL="HDL", 
                 H_LDL="LDL",H_TG="Triglycerid",CKD="Chronic Kidney Disease", 
                 HAV="Hepatitis A", hepatitis='Other hepatitis'),
  groups= list("","Tuberculosis Infection"))

## Format for column names

strata <- c(list(Total=NHANES2), split(NHANES2, NHANES2$TB_Infection))

my.render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits=2), 
       c("","Mean (IQR)"=sprintf("%s (%s - %s)", MEDIAN, Q1, Q3)))}
my.render.cat <- function(x) {
  c("", sapply(stats.default(x), 
  function(y) with(y, sprintf("%d (%.1f %%)", FREQ, PCT))))
}
tablex<-table1(strata, labels, groupspan = c(1,2) ,render.continuous=my.render.cont, 
       render.categorical=my.render.cat)

saveRDS(tablex, file = here("results","tablex.rda"))


print(tablex)

?table1
# Create table1 by tableone package
myvar1 <- c("age","sex1","BMI","race","US_born", "marital","education","income",
            "HH_member", "hypertension","diabetes","HD", "H_TCHOL","Low_HDL", "H_LDL", "H_TG",
            "CKD", "HAV", "hepatitis")

table1.2 <- CreateTableOne(vars = myvar1, data=NHANES2, test=TRUE)
table1.2 <-print(table1.2, varLabels = TRUE)
saveRDS(table1.2, file = here("results","table1.2.Rda"))

# Create table 1 with stratification by infection status
table1.1 <- CreateTableOne(vars = myvar1, strata="TB_Infection", data=NHANES2,
                           test=TRUE)
table1.1<-print(table1.1, varLabels = TRUE)
saveRDS(table1.1, file = here("results","table1.1.Rda"))

# Create table 1 with stratifying TB infection, using finalfit package
explanatory <- myvar1
dependent <- "TB_Infection"

table1.3<-NHANES2 %>% 
  summary_factorlist(dependent, explanatory, p=TRUE, na_include = TRUE,
                     add_dependent_label = TRUE)

saveRDS(table1.3, file = here("results", "table1.3.Rda"))

NHANES3 <- NHANES2 %>% 
  select("TB_Infection","age","sex1","BMI","race","US_born", "marital","education","income",
         "HH_member", "hypertension","diabetes","HD", "H_TCHOL","Low_HDL", "H_LDL", "H_TG",
         "CKD", "HAV", "hepatitis")

#######################Regression##########################################
# Regression with multivariate and univariate for all variables
table2 <- NHANES2  %>%
  finalfit(dependent, explanatory, dependent_label_prefix = "")

# Multiple regresion analysis

## Set up
outcome <- NHANES3$TB_Infection
outcomename = "TB_Infection"
predictors <- NHANES3[,-1]
npred = ncol(predictors)

### sampling method for performance evaluation. I use 5-fold CV, and 5 times repeated
sampling_choice = makeResampleDesc("RepCV", reps=5, folds=5) 

## A null model
null <- rep("No", nrow(NHANES2))
measureACC(NHANES2$TB_Infection, null)

## Single predictor model
set.seed(12345)

learner_name="classif.binomial"
mylearner = makeLearner(learner_name, predict.type = "prob")

unifmat <- data.frame(variable=rep(0,npred), Accuracy=rep(0,npred))#contain result

for (nn in 1:npred){
  unidata = data.frame(TB_Infection=outcome, NHANES3[,nn+1])
  #Generate the task, define outcome and predictors to be fit
  mytask = makeClassifTask(id='unianalysis', data=unidata, target = outcomename, positive = "Yes")
  model=resample(mylearner, task = mytask, resampling = sampling_choice, show.info = FALSE, measures = mlr::acc)
  unifmat[nn,1]= names(predictors)[nn]
  unifmat[nn,2]= model$aggr
  }

kable(unifmat)

## Full model
set.seed(12345)
### do full model with cross-validation - to get an idea for the amount od over-fitting
### a full-model does
mytask = makeClassifTask(id="fullanalysis", data = NHANES3,target = outcomename, 
                         positive = "Yes")
fullmodel= resample(mylearner, task = mytask, resampling=sampling_choice, 
                    show.info = FALSE, measures = mlr::acc)
ACC_fullmodel=fullmodel$aggr[1]
print(ACC_fullmodel)

## Model selection
### To reduce the time to run the model, I only select predicts which are associated
### with TB infection (cut-off p-value 0.1). These predictors (p-value>0.1) are removed
### from model selection: BMI, income, hypertension, HD (heart disease), TCHOL, 
### HDL, LDL, TG, other hepatitis
NHANES4 <- NHANES2 %>% 
  select("TB_Infection","age","sex1","race","US_born", "marital","education",
         "HH_member","diabetes","CKD", "HAV")

outcome <- NHANES4$TB_Infection
outcomename = "TB_Infection"
predictors <- NHANES4[,-1]
npred = ncol(predictors)
mytask = makeClassifTask(id="selection", data = NHANES4,target = outcomename, 
                         positive = "Yes")

set.seed(12345)
tstart=proc.time()
select_methods= c("sbs","sfbs", "sfs","sffs")
resmat= data.frame(method=rep(0,4), Accuracy=rep(0,4), Model=rep(0,4))
ct=1

for (select_method in select_methods) {
  ctrl = makeFeatSelControlSequential(method = select_method)
  print(sprintf('doing subset selection with method %s', select_method))
  sfeat_res = selectFeatures(learner = mylearner, task = mytask, 
                             resampling = sampling_choice, control = ctrl,
                             show.info = FALSE, measures = mlr::acc)
  resmat[ct,1]= select_methods[ct]
  resmat[ct,2]= sfeat_res$y
  resmat[ct,3]= paste(as.vector(sfeat_res$x), collapse=',')
  ct = ct+1;
}

kable(resmat)
############################################################################
## After doing multivariate, only age is selected

## Create multivariate and univariate table
explanatory_multi <- "age"
table2 <- NHANES2  %>%
  finalfit(dependent, explanatory, explanatory_multi, dependent_label_prefix = "")
saveRDS(table2, file = here("results","table2.Rda"))
save(table1.3, table2, dependent, explanatory, file = here("results","out.rda"))



## Create plot for univariate model
#forest_univariate <- 
  NHANES3 %>%
  or_plot(dependent, explanatory, breaks = c())
  
?or_plot
  
  

