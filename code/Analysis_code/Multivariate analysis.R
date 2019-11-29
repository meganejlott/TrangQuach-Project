
library(here)
library(mlr)
library(tidyverse)
library(finalfit)
library(knitr)
# Read data
NHANES2 <- readRDS(here("data","processing_data", "NHANES2.Rda"))

NHANES3 <- NHANES2 %>% 
  select("TB_Infection","age","sex1","BMI","race","US_born", "marital","education","income",
         "HH_member", "hypertension","diabetes","HD", "H_TCHOL","Low_HDL", "H_LDL", "H_TG",
         "CKD", "HAV", "hepatitis")

#######################Regression##########################################

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
nullACC <-measureACC(NHANES2$TB_Infection, null)
saveRDS(nullACC, file = here("results", "nullACC.Rda"))


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

unisel <- saveRDS(unifmat,file = here("results", "unisel.Rda"))

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
saveRDS(ACC_fullmodel, file = here("results", "ACC_fullmodel.Rda"))

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

MultiSel <- resmat
saveRDS(MultiSel, file = here("results", "MultiSel.Rda"))


# Regression with multivariate and univariate for all variables
table2 <- NHANES2  %>%
  finalfit(dependent, explanatory, dependent_label_prefix = "")

############################################################################