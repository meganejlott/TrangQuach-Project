# TrangQuach-Project

# Part 3 submission
My submission for part 3 is in sub-folder "manuscript" under "products" folder. I generated the word document, namely manuscript which contains the main findings.

Please follow instructions (step 0, 1, 2) at the end of this readme to reproduce the
analysis.

Thanks.


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

    + Analysis_code: code for analysis. The results of all analysis are stored in folder result

    + Processing_code: code for cleaning and wrangling to produce the cleaned data which is used for analysis. The final output of this code is cleaned data, namely NHANES2.

* data

    + processing_data: contains the cleaned data, namely NHANES2
    
    + raw_data: contains the raw data and codebook

* Product: contains the manuscript or pptx to present the results of the project

    + manuscript: contains manuscript (word document) for the project

    + slides: contains the pptx


## Instruction to reproduce the project

* Step 0: reproduce the cleaned data (NHANES2)

    + run processingScript in sub-folder processing_code to produce the cleaned data, namely NHANES2. NHANES will be stored in processing_data.

* Step 1: reproduce the analysis 

    + run the AnalysisScript in sub-folder Analysis_code to do analyssis and get the results. The results of analysis were stored in results folder.

* Step 2: reproduce the manuscript

    + run the Manuscript Rmd file to reproduce the manuscript word document


