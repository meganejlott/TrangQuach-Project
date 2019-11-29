
# install the latest version of finalfit package



library(tidyverse)
library(knitr)
library(dmm)
library(finalfit)
library(here)
library(mlr)


# Read data
NHANES2 <- readRDS(here("data","processing_data", "NHANES2.Rda"))

###########Using finalfit package to create table 1 and table 2#############
# Create table 1 with stratifying TB infection, using finalfit package
explanatory <- c("age","sex1","BMI","race","US_born", "marital","education","income",
                 "HH_member", "hypertension","diabetes","HD", "H_TCHOL","Low_HDL", 
                 "H_LDL", "H_TG","CKD", "HAV", "hepatitis")
dependent <- "TB_Infection"

table1.3<-NHANES2 %>% 
  summary_factorlist(dependent, explanatory, cont = "mean",
                     p=FALSE, na_include = TRUE,column = TRUE,
                     total_col = TRUE,add_dependent_label = TRUE, 
                     dependent_label_prefix = " ") 

saveRDS(table1.3, file = here("results","table1.3.Rda"))


## After doing multivariate, only age is selected.

## Create multivariate and univariate table
explanatory_multi <- "age"
table2 <- NHANES2  %>%
  finalfit(dependent, explanatory, explanatory_multi,column = FALSE, 
           add_dependent_label = TRUE,dependent_label_prefix = "")%>%
  ff_remove_p()

table2$`OR (univariable)`[is.na(table2$`OR (univariable)`)] <- "-"
table2$`OR (multivariable)`[is.na(table2$`OR (multivariable)`)] <- "-"


saveRDS(table2, file = here("results","table2.Rda"))



## Create forest plot for univariate model. Because there are a lot of variables,
## I only select variables with p-value<0.1
explanatory_plot <- c("age","sex1","race","US_born", "marital","education",
                      "HH_member", "diabetes","CKD", "HAV")
explanatory_plot1 <- c("age","sex1","race","US_born","education",
                       "CKD", "HAV")
## build the table with multivariate of these variables

table3 <- NHANES2  %>%
  finalfit(dependent, explanatory, explanatory_plot1,column = FALSE, 
           add_dependent_label = TRUE,dependent_label_prefix = "")%>%
  ff_remove_p()
saveRDS(table3, file = here("results","table3.Rda"))

table4 <- NHANES2  %>%
  finalfit(dependent, explanatory, explanatory_plot,column = FALSE, 
           add_dependent_label = TRUE,dependent_label_prefix = "")%>%
  ff_remove_p()
saveRDS(table4, file = here("results","table4.Rda"))



## build the figure 
forest<-NHANES2 %>%
  or_plot(dependent, explanatory_plot1,breaks = c(0.2,0.5,2,4,8),
          dependent_label = "", 
          plot_opts = list(xlab("OR, 95% CI"), 
          theme(axis.title.x = element_text(size=12), axis.text.x = element_text(size = 12))),
          prefix = "", suffix = "", table_text_size = 6)


graphics.off(); #close all graphics windows
ww=20; wh=2/3*ww; #window size, can be adjusted
windows(width=ww, height=wh)
ggsave(file="results/forest.tiff", plot=forest, dpi = 360, compression="lzw")
dev.off()

