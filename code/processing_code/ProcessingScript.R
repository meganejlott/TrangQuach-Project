

library(SASxport) # to import SAS data
library(tidyverse)
library(skimr)
library(nephro) # for GFR calculation
library(tableone)

# Import data
## demopgraphic data
demo <- read.xport("data/raw_data/DEMO_G.xpt") # Demographic information

## Examination data
TST <- read.xport("data/raw_data/TBX_G.xpt") # Outcome: Tuberculin skin test result
BP <- read.xport("data/raw_data/BPX_G.xpt") # Main exposure : hypertension
BMI <- read.xport("data/raw_data/BMX_G.xpt") # BMI measure

## Laboratory data
HDL <- read.xport("data/raw_data/HDL_G.xpt") # HDL cholesterol
TRIGLY <- read.xport("data/raw_data/TRIGLY_G.xpt") # Triglycerid and LDL
TCHOL <- read.xport("data/raw_data/TCHOL_G.xpt") # TOTAL CHOLESTEROL
HBA1C <- read.xport("data/raw_data/GHB_G.xpt") # HBA1C
HEPA <- read.xport("data/raw_data/HEPA_G.xpt") # Hepatitis A
HBV_S <- read.xport("data/raw_data/HEPB_S_G.xpt") # Hepatitis B surface
HBV_C <- read.xport("data/raw_data/HEPBD_G.xpt") # Hepatitis B core
HCV_E <- read.xport("data/raw_data/SSHCV_E.xpt") # Hepatitis C
HCV_G <- read.xport("data/raw_data/HEPC_G.xpt") # Hepatitis C
HEV <- read.xport("data/raw_data/HEPE_G.xpt") # Hepatitis E
HIV <- read.xport("data/raw_data/HIV_G.xpt") # HIV
OGTT <- read.xport("data/raw_data/OGTT_G.xpt") # Oral glucose tolerance test
GLU_G <- read.xport("data/raw_data/GLU_G.xpt") # Plasma fasting glucose & insulin
ALB_CR <- read.xport("data/raw_data/ALB_CR_G.xpt") # Albumin creatinin in urine
BIOPRO <- read.xport("data/raw_data/BIOPRO_G.xpt") # creatinine in serum

## Questionnaire
BPQ <- read.xport("data/raw_data/BPQ_G.xpt")#taking drug prescribed for HBP, cholesterol
ALQ <- read.xport("data/raw_data/ALQ_G.xpt") # Alcohol use
CDQ <- read.xport("data/raw_data/CDQ_G.xpt") # Cardiovascular health
SMQ <- read.xport("data/raw_data/SMQ_G.xpt") # smoking and cigarette use
DIQ <- read.xport("data/raw_data/DIQ_G.xpt") # Diabetes questionnaire
TBQ <- read.xport("data/raw_data/TBQ_G.xpt") # tuberculosis
MCQ <- read.xport("data/raw_data/MCQ_G.xpt") # chronic diseases history

# Merge these data by Respondent sequence number SEQN
mylist <- list(demo, TST, BP, BMI, HDL, TRIGLY,TCHOL, HBA1C, HEPA, HBV_S, HBV_C, HCV_E,
               HCV_G, HEV, HIV, OGTT, GLU_G, ALB_CR, BPQ, ALQ, CDQ, SMQ, DIQ, TBQ, MCQ,
               BIOPRO)
myvar <- c("SEQN", "TBDRUIND", "BPQ050A", "BPXSY1", "BPXSY2", "BPXSY3", "BPXSY4", 
            "BPXDI1", "BPXDI2","BPXDI3", "BPXDI4","RIAGENDR", "RIDAGEYR", "RIDRETH3", "DMDBORN4",
           "DMDCITZN", "DMDMARTL","DMDYRSUS","DMDEDUC2", "DMDEDUC3","INDHHIN2", "DMDHHSIZ", "BMXBMI",
           "LBDHDD", "LBXTR", "LBDLDL","LBXTC","BPQ100D", "LBDGLTSI", "LBDGLUSI",
           "GTDCODE","LBXGH", "DIQ050", "DIQ070","LBDHI","URXUMS", "URXUCR","URDACT","LBXSCR", "LBXHA",
           "LBXHBS", "LBXHBC", "LBDHBG", "LBDHD", "SSRIBA","LBDHCV", "LBXHCR",
           "LBDHEG", "LBDHEM", "ALQ101", "ALQ120Q", "CDQ001", "CDQ002", "CDQ003",
           "CDQ004", "CDQ005","CDQ006", "CDQ009D", "CDQ009E", "CDQ009F", "CDQ009G",
           "SMQ040", "TBQ040", "TBQ060", "MCQ160B", "MCQ160C", "MCQ160D", "MCQ160E", 
           "MCQ160F")



NHANES <- mylist %>% 
  reduce(full_join, by = "SEQN") %>% # merge all dataset
  select(myvar)  %>% # selected variables that will be used
  filter(RIDAGEYR >= 18 & ! is.na(TBDRUIND)) %>% # only included records with TST test and age >= 18
  mutate(TB_Infection = if_else(TBDRUIND >= 10, "Yes", "No")) %>% # Create outcome variable
  # calcuate the mean of systolic/diastolic blood pressure from 4 measures, and define hypertension
  ## BPQ050A is currently taking hypertension drug
  mutate(sys_mean= rowMeans(subset(NHANES ,select = c(BPXSY1,BPXSY2,BPXSY3,BPXSY4)), na.rm = TRUE)) %>%
  mutate(dia_mean= rowMeans(subset(NHANES ,select = c(BPXDI1,BPXDI2,BPXDI3,BPXDI4)), na.rm = TRUE)) %>%
  mutate(BP = if_else(sys_mean>=130 & dia_mean>=80, 1, 2)) %>% #hypertension is final var
  mutate(hypertension = case_when(BP %in% c(1,2) & BPQ050A==1 ~ 1,
                                  BP==1 & BPQ050A %in% c(2,9,NA) ~ 1,
                                  BP==2 & BPQ050A %in% c(2,9,NA) ~ 2)) %>%
  # History of Heart Disease
  mutate(HD = if_else(MCQ160B==1|MCQ160C==1|MCQ160D==1|MCQ160E==1|MCQ160F==1, 1, 2)) %>%
  # dyslipidemia
  mutate(TCHOL = if_else(LBXTC >= 240, "High", "Normal")) %>% # Final var
  mutate(H_TCHOL = case_when(BPQ100D==1 ~ "High",
                             (BPQ100D %in% c(2,9)|is.na(BPQ100D)) & TCHOL=="High" ~ "High",
                             (BPQ100D %in% c(2,9)|is.na(BPQ100D)) & TCHOL=="Normal" ~ "Normal")) %>%
  mutate(LDL = case_when(LBDLDL< 100 ~ "Normal",
                         LBDLDL>= 100 & LBDLDL<=159 & HD==1 ~ "High",
                         LBDLDL>= 100 & LBDLDL<=159 & (HD==2|is.na(HD)) ~ "Normal")) %>%
  mutate(H_LDL = case_when(BPQ100D==1 ~ "High", ## H_LDL is final var
                          (BPQ100D %in% c(2,9)|is.na(BPQ100D)) & LDL=="High" ~ "High",
                          (BPQ100D %in% c(2,9)|is.na(BPQ100D)) & LDL=="Normal" ~ "Normal")) %>%
  mutate(HDL = case_when(LBDHDD<40 & RIAGENDR==1 ~ "Low",
                         LBDHDD<50 & RIAGENDR==2 ~ "Low",
                         LBDHDD>40 & RIAGENDR==1 ~ "Normal",
                         LBDHDD>50 & RIAGENDR==2 ~ "Normal")) %>%
  mutate(Low_HDL = case_when(BPQ100D==1 ~ "Low",
                            (BPQ100D %in% c(2,9)|is.na(BPQ100D)) & HDL=="Normal" ~ "Normal",
                            (BPQ100D %in% c(2,9)|is.na(BPQ100D)) & HDL=="Low" ~ "Low")) %>%
  mutate(H_TG = case_when(BPQ100D==1 ~ "High",
                        (BPQ100D %in% c(2,9)|is.na(BPQ100D)) & LBXTR>= 200 ~ "High",
                        (BPQ100D %in% c(2,9)|is.na(BPQ100D)) & LBXTR< 200 ~ "Normal")) %>%
  # Diabetes
  mutate(FPG = if_else(LBDGLUSI>=7.0, 1, 2)) %>%
  mutate(OGTT = case_when(LBDGLTSI>=11.1 & GTDCODE==0 ~ 1,
                          LBDGLTSI<11.1 & GTDCODE==0 ~ 2,
                          GTDCODE==22 ~ 1)) %>%
  mutate(HBA1C = if_else(LBXGH>=6.5, 1,2)) %>%
  mutate(diabetes_drug= case_when(DIQ050==1 | DIQ070==1 ~ 1,
                                  DIQ050==2|DIQ070 %in% c(2,9)|is.na(DIQ070) ~ 2)) %>%
  mutate(DB= if_else(FPG==1|OGTT==1|HBA1C==1|diabetes_drug==1, 1,2)) %>%
  mutate(diabetes= case_when(is.na(DB) & (FPG==2|OGTT==2|HBA1C==2) ~ 2,
                             DB==1 ~ 1,
                             DB==2 ~ 2)) %>%
  # Hepatitis
  ## Hepatitis A
  mutate(HAV = if_else(LBXHA==1, "Yes", "No")) %>%
  ## Hepatitis B: chronic
  mutate(HBV = if_else(LBDHBG==1, "Yes", "No")) %>%
  ## Hepatitis C
  mutate(HCV= if_else(LBXHCR==1,"Yes","No")) %>%  # may skip this variable, because only 394 is tested
  ## Hepatitis D
  mutate(HDV = if_else(LBDHD==1, "Yes","No")) %>% #skip heptatis D because only 5 person is positive
  ## Hepatitis E
  mutate(HEV = if_else(LBDHEM==1, "Yes","No")) %>%#may skip this variable
  # HIV: skip because only 19 people positive
  # GFR: kidney chronic disease
  mutate(sex= if_else(RIAGENDR==1, 1,0)) %>%
  mutate(ethnic= if_else(RIDRETH3==4,1,0)) %>%
  mutate(GFR= MDRD4(LBXSCR,sex,RIDAGEYR,ethnic, method = "other")) %>%
  mutate(CKD= case_when(GFR>=90 ~ "Stage 1",
                        GFR>=60 & GFR<90 ~ "Stage 2",
                        GFR<=60 ~ "Stage >= 3")) %>%
  # demographic information
  mutate(sex1 = if_else(RIAGENDR==1, "Male", "Female")) %>%
  mutate(race= case_when(RIDRETH3==1 ~ "Mexican American",
                         RIDRETH3==2 ~ "Hispanic",
                         RIDRETH3==3 ~ "Whites",
                         RIDRETH3==4 ~ "Black",
                         RIDRETH3==6 ~ "Asian",
                         RIDRETH3==7 ~ "Others")) %>%
  mutate(US_born= if_else(DMDBORN4==1, "Yes","No")) %>%
  mutate(marital= if_else(DMDMARTL %in% c(1,6), "Live with partners", "Not live with partner")) %>%
  mutate(education= case_when(DMDEDUC2 %in% c(1,2) ~ "< High school",
                         is.na(DMDEDUC2) & (DMDEDUC3<12|DMDEDUC3 %in% c(55,66)) ~ "< High school",
                         is.na(DMDEDUC2) & (DMDEDUC3 %in% c(12,13,14)) ~ "High school",
                         is.na(DMDEDUC2) & (DMDEDUC3==15) ~ "Tertiary", 
                         is.na(DMDEDUC2) & DMDEDUC3<12 ~ "< High school",
                         DMDEDUC2==3 ~ "High school",
                         DMDEDUC2 %in% c(4,5) ~ "Tertiary")) %>%
  mutate(income= case_when(INDHHIN2 %in% c(1,2,3,4,13) ~ "<20,000",
                           INDHHIN2 %in% c(5,6,7,8,12) ~ "20,000 - <55,000",
                           INDHHIN2 %in% c(9,10,14) ~ "55,000 - <100,000",
                           INDHHIN2 %in% c(15) ~ ">= 100,000")) %>%
  mutate(HH_member= case_when(DMDHHSIZ %in% c(1,2) ~ "<= 2",
                              DMDHHSIZ>2 & DMDHHSIZ<=5 ~ "3-5",
                              DMDHHSIZ %in% c(6,7) ~ ">= 6")) %>%
  mutate(BMI= case_when(BMXBMI <18.5 ~ "Underweight",
                        BMXBMI>=18.5 & BMXBMI<24.9 ~ "Normal weight",
                        BMXBMI>=24.9 & BMXBMI<29.9 ~ "Overweight",
                        BMXBMI>=29.9 ~ "Overweight")) %>%
  mutate(age = as.numeric(RIDAGEYR))

myvar1 <- c("age","sex1","BMI","race","US_born", "marital","education","income",
            "HH_member", "hypertension","diabetes","HD", "H_TCHOL","Low_HDL", "H_LDL", "H_TG",
            "CKD", "HAV", "HBV", "HCV", "HDV", "HEV")


(table1.1 <- CreateTableOne(vars = myvar1,data=NHANES, test=FALSE))
(table1.2 <- CreateTableOne(vars = myvar1, strata= "TB_Infection", data=NHANES, test=TRUE))



library(table1)
table1( ~ age+sex1+BMI | TB_Infection, data=NHANES)




tabUnmatched <- CreateTableOne(vars = xvars, strata = "select", data=icd_psmean, test = FALSE)
print(tabUnmatched, smd=TRUE)





NHANES %>% count(is.na(BMXBMI))
NHANES %>% mean(DMDHHSIZ, na.rm = FALSE)

