# TrangQuach-Project

Please follow the step 0, 1, 2 below to reproduce the project.


# Overview

A template file and folder structure for a data analysis project/paper done with R/Rmarkdown/Github. 

# Pre-requisites

This is a template for a data analysis project using R, Rmarkdown (and variants, e.g. bookdown), Github and a reference manager that can handle bibtex (I recommend [Jabref](http://www.jabref.org/) or [Zotero](https://www.zotero.org/)). It is also assumed that you have a word processor installed (e.g. MS Word or [LibreOffice](https://www.libreoffice.org/)). You need that software stack to make use of this template.

# Template structure

* All data goes into the subfolders inside the `data` folder.
* All code goes into the `code` folder or subfolders.
* All results (figures, tables, computed values) go into `results` folder or subfolders.
* All products (manuscripts, supplement, presentation slides, web apps, etc.) go into `products` subfolders.
* See the various `readme.md` files in those folders for some more information.

# Template content 

* code: include the R code to produce the project. This folder contains two sub-folders

    + Analysis_code: code for analysis. The results of all analysis are stored in folder result. The Analysis_code includes two  R codes. First, AnalysisScript code contains my main analysis code to produce and save all results. Second, Multivariate analysis code is how I select the final model in my analysis.

    + Processing_code: code for cleaning and wrangling to produce the cleaned data which is used for analysis. The final output of this code is cleaned data, namely NHANES2.

* data

    + processing_data: contains the cleaned data, namely NHANES2
    
    + raw_data: contains the raw data and codebook. NHANES_full data is dataset which combined all raw data files. 

* Product: contains the manuscript or pptx to present the results of the project, and bitex file for references

    + manuscripts: contains manuscript (word document) for the project and Rmarkdown code to reproduce the manuscript word document. 

    + slides: contains the pptx
    
    + references: x.bib


## Instruction to reproduce the project

* Step 0: reproduce the cleaned data (NHANES2)

    + run processingScript in sub-folder processing_code to produce the cleaned data, namely NHANES2. NHANES2 is stored in processing_data.

* Step 1: reproduce the analysis 

    + run the AnalysisScript in sub-folder Analysis_code to do analyssis and get the results. The results of analysis (table1.3, table2, table3, table4, and forest) were stored in results folder. 
    
    + run the Multivariate analysis code in sub-folder Analysis_code to do multivariate analysis and get result files (ACC_fullmodel, nullACC, MultiSel, and unisel)

* Step 2: reproduce the manuscript

    + run the Manuscript Rmd file to reproduce the manuscript word document


