---
title: "Final_Project_XianjieQue_BDM"
author: "XianjieQue"
date: "2025-04-14"
output:
  html_document:
    number_sections: yes
    toc: true        
    toc_depth: 3     
    toc_float: true  
    theme: cerulean
    keep_md: yes
---

# load packages


``` r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

``` r
library(ggplot2)
# install.packages("survey")
library(survey)
```

```
## Loading required package: grid
```

```
## Loading required package: Matrix
```

```
## Loading required package: survival
```

```
## 
## Attaching package: 'survey'
```

```
## The following object is masked from 'package:graphics':
## 
##     dotchart
```

``` r
# install.packages("Amelia")
library(Amelia)
```

```
## Loading required package: Rcpp
```

```
## ## 
## ## Amelia II: Multiple Imputation
## ## (Version 1.8.3, built: 2024-11-07)
## ## Copyright (C) 2005-2025 James Honaker, Gary King and Matthew Blackwell
## ## Refer to http://gking.harvard.edu/amelia/ for more information
## ##
```

``` r
library(VIM)
```

```
## Loading required package: colorspace
```

```
## VIM is ready to use.
```

```
## Suggestions and bug-reports can be submitted at: https://github.com/statistikat/VIM/issues
```

```
## 
## Attaching package: 'VIM'
```

```
## The following object is masked from 'package:datasets':
## 
##     sleep
```

``` r
library(ncvreg)
library(caret)
```

```
## Loading required package: lattice
```

```
## 
## Attaching package: 'caret'
```

```
## The following object is masked from 'package:survival':
## 
##     cluster
```

``` r
library(glmnet)
```

```
## Loaded glmnet 4.1-8
```

``` r
library(pROC)
```

```
## Type 'citation("pROC")' for a citation.
```

```
## 
## Attaching package: 'pROC'
```

```
## The following object is masked from 'package:colorspace':
## 
##     coords
```

```
## The following objects are masked from 'package:stats':
## 
##     cov, smooth, var
```

``` r
library(rlang)
# install.packages("gt")
library(gt)
```

# read dataset


``` r
prep <- read.csv("Preprocessed_data.csv")
str(prep)
```

```
## 'data.frame':	26757 obs. of  160 variables:
##  $ svy_id                            : int  12 21 57 66 117 121 170 175 183 209 ...
##  $ svy_weight_mec                    : num  95494 2911 102610 27490 52603 ...
##  $ svy_psu                           : int  2 2 2 1 2 1 1 1 1 1 ...
##  $ svy_strata                        : int  6 3 5 3 1 7 3 6 8 6 ...
##  $ svy_subpop_chol                   : int  1 1 1 0 0 1 0 0 0 1 ...
##  $ demo_age_cat                      : chr  "18 to 44" "18 to 44" "18 to 44" "18 to 44" ...
##  $ demo_race                         : chr  "Non-Hispanic White" "Hispanic" "Non-Hispanic White" "Hispanic" ...
##  $ demo_age_years                    : int  37 18 39 37 34 38 28 19 21 18 ...
##  $ demo_gender                       : chr  "Men" "Men" "Men" "Men" ...
##  $ bp_sys_mean                       : num  177 121 119 131 141 ...
##  $ bp_dia_mean                       : num  102 80 85.3 73.3 98 ...
##  $ bp_control_accaha                 : chr  "No" "No" "No" "No" ...
##  $ bp_med_use                        : chr  "Yes" "No" "No" "No" ...
##  $ htn_accaha                        : chr  "Yes" "Yes" "Yes" "Yes" ...
##  $ htn_aware                         : chr  "Yes" "No" "Yes" "No" ...
##  $ htn_resistant_jnc7                : chr  "No" "No" "No" "No" ...
##  $ htn_resistant_accaha              : chr  "No" "No" "No" "No" ...
##  $ htn_resistant_jnc7_thz            : chr  "No" "No" "No" "No" ...
##  $ htn_resistant_accaha_thz          : chr  "No" "No" "No" "No" ...
##  $ chol_measured_never               : chr  "Cholesterol has been measured previously" "Cholesterol has been measured previously" "Cholesterol has been measured previously" "" ...
##  $ chol_measured_last                : chr  "1 to 5 years ago" "" ">5 years ago (possibly never)" "" ...
##  $ chol_total                        : int  156 161 243 NA NA 167 NA NA NA 163 ...
##  $ chol_total_gteq_200               : chr  "No" "No" "Yes" "" ...
##  $ chol_total_gteq_240               : chr  "No" "No" "Yes" "" ...
##  $ chol_hdl                          : int  38 34 46 NA NA 39 NA NA NA 49 ...
##  $ chol_hdl_low                      : chr  "Yes" "Yes" "Yes" "" ...
##  $ chol_trig                         : int  146 120 97 NA NA 155 NA NA NA 122 ...
##  $ chol_trig_gteq_150                : chr  "No" "No" "No" "" ...
##  $ chol_ldl                          : num  90.9 104.2 179.3 NA NA ...
##  $ chol_ldl_5cat                     : chr  "70 to <100 mg/dL" "100 to <130 mg/dL" "130 to <190 mg/dL" "" ...
##  $ chol_ldl_lt_70                    : chr  "No" "No" "No" "" ...
##  $ chol_ldl_gteq_70                  : chr  "Yes" "Yes" "Yes" "" ...
##  $ chol_ldl_lt_100                   : chr  "Yes" "No" "No" "" ...
##  $ chol_ldl_gteq_100                 : chr  "No" "Yes" "Yes" "" ...
##  $ chol_ldl_gteq_190                 : chr  "No" "No" "No" "" ...
##  $ chol_ldl_persistent               : chr  "No" "No" "No" "" ...
##  $ chol_nonhdl                       : int  118 127 197 NA NA 128 NA NA NA 114 ...
##  $ chol_nonhdl_5cat                  : chr  "100 to <130 mg/dL" "100 to <130 mg/dL" "160 to <220 mg/dL" "" ...
##  $ chol_nonhdl_lt_100                : chr  "No" "No" "No" "" ...
##  $ chol_nonhdl_gteq_100              : chr  "Yes" "Yes" "Yes" "" ...
##  $ chol_nonhdl_gteq_220              : chr  "No" "No" "No" "" ...
##  $ chol_med_use                      : chr  "No" "No" "No" "" ...
##  $ chol_med_use_sr                   : chr  "No" "No" "No" "" ...
##  $ chol_med_statin                   : chr  "No" "No" "No" "" ...
##  $ chol_med_ezetimibe                : chr  "No" "No" "No" "" ...
##  $ chol_med_pcsk9i                   : chr  "No" "No" "No" "" ...
##  $ chol_med_bile                     : chr  "No" "No" "No" "" ...
##  $ chol_med_fibric_acid              : chr  "No" "No" "No" "" ...
##  $ chol_med_atorvastatin             : chr  "No" "No" "No" "" ...
##  $ chol_med_simvastatin              : chr  "No" "No" "No" "" ...
##  $ chol_med_rosuvastatin             : chr  "No" "No" "No" "" ...
##  $ chol_med_pravastatin              : chr  "No" "No" "No" "" ...
##  $ chol_med_pitavastatin             : chr  "No" "No" "No" "" ...
##  $ chol_med_fluvastatin              : chr  "No" "No" "No" "" ...
##  $ chol_med_lovastatin               : chr  "No" "No" "No" "" ...
##  $ chol_med_other                    : chr  "No" "No" "No" "" ...
##  $ chol_med_addon_use                : chr  "No" "No" "No" "" ...
##  $ chol_med_addon_recommended_ahaacc : chr  "No" "No" "No" "" ...
##  $ chol_med_statin_recommended_ahaacc: chr  "No" "No" "No" "" ...
##  $ chol_med_recommended_ever         : chr  "No" "No" "No" "" ...
##  $ ascvd_risk_vh_ahaacc              : chr  "No" "No" "No" "" ...
##  $ cc_smoke                          : chr  "Never" "" "Never" "Never" ...
##  $ cc_diabetes                       : chr  "No" "No" "No" "No" ...
##  $ cc_ckd                            : chr  "Yes" "No" "No" "No" ...
##  $ cc_acr                            : num  34.65 1.6 1.03 6.67 63.64 ...
##  $ cc_egfr                           : num  83.2 117.9 91.7 104.7 116.7 ...
##  $ cc_hba1c                          : num  5.2 5 4.9 4.8 6.1 5.3 4.9 5.4 4.8 5.1 ...
##  $ cc_egfr_lt60                      : chr  "No" "No" "No" "No" ...
##  $ cc_acr_gteq30                     : chr  "Yes" "No" "No" "No" ...
##  $ cc_cvd_mi                         : chr  "No" "" "No" "No" ...
##  $ cc_cvd_chd                        : chr  "No" "" "No" "No" ...
##  $ cc_cvd_stroke                     : chr  "No" "" "No" "No" ...
##  $ cc_cvd_ascvd                      : chr  "No" "No" "No" "No" ...
##  $ cc_cvd_hf                         : chr  "No" "" "No" "No" ...
##  $ cc_cvd_any                        : chr  "No" "No" "No" "No" ...
##  $ URXUMA                            : num  34.3 0.8 2.5 2.4 103.1 ...
##  $ URXUCR                            : num  99 50 243 36 162 172 196 239 189 212 ...
##  $ YEAR                              : int  1999 1999 1999 1999 1999 1999 1999 1999 1999 1999 ...
##  $ LBXSAL                            : num  4.7 4.8 4.3 4.8 4.7 4.6 4.7 5.1 5.5 4.8 ...
##  $ LBXSATSI                          : int  35 42 28 34 53 47 55 25 73 24 ...
##  $ LBXSASSI                          : int  17 26 21 33 27 29 30 25 31 21 ...
##  $ LBXSBU                            : int  20 12 24 10 18 15 13 14 8 13 ...
##  $ LBXSCA                            : num  8.8 9.3 9.2 9.4 9.9 9.4 9.3 9.3 10.2 9.4 ...
##  $ LBXSCH                            : int  151 159 238 213 266 165 188 132 229 159 ...
##  $ LBXSC3SI                          : int  26 22 25 22 26 25 24 26 25 26 ...
##  $ LBXSGTSI                          : int  32 47 27 20 54 69 33 26 60 9 ...
##  $ LBXSGL                            : int  75 83 100 86 122 92 88 92 84 88 ...
##  $ LBXSIR                            : int  63 91 69 144 120 112 105 52 81 119 ...
##  $ LBXSTP                            : num  7.2 7.7 7.1 7.9 8.3 7.8 7.5 8.4 8.6 7.3 ...
##  $ LBDSTPSI                          : int  72 77 71 79 83 78 75 84 86 73 ...
##  $ LBXSTR_x                          : int  140 107 97 190 179 164 154 85 135 105 ...
##  $ LBDSTRSI                          : num  1.58 1.21 1.09 2.15 2.02 ...
##  $ LBXSUA                            : num  5.7 7.9 8.2 6.9 6.5 9.8 4.4 8 6.4 6.4 ...
##  $ LBDSUASI                          : num  339 470 488 410 387 ...
##  $ LBXSNASI                          : num  141 137 140 141 137 ...
##  $ LBXSKSI                           : num  3.81 3.78 4.15 3.71 3.42 4.07 4.14 3.94 4.1 4.08 ...
##  $ LBXSCLSI                          : num  101.9 97.9 101.2 103.1 96.5 ...
##  $ LBXSOSSI                          : int  283 273 284 279 277 283 280 280 284 282 ...
##  $ LBXSGB                            : num  2.5 2.9 2.8 3.1 3.6 3.2 2.8 3.3 3.1 2.5 ...
##   [list output truncated]
```

``` r
demogra <- read.csv("Variable Dictionary/nhanes_Demographics_variables.csv")
str(demogra)
```

```
## 'data.frame':	1776 obs. of  8 variables:
##  $ Variable.Name        : chr  "AIALANG" "DMDBORN" "DMDCITZN" "DMDEDUC2" ...
##  $ Variable.Description : chr  "Language of the MEC ACASI Interview Instrument" "In what country {were you/was SP} born?" "{Are you/Is SP} a citizen of the United States? [Information about citizenship is being collected by the U.S. P"| __truncated__ "(SP Interview Version) What is the highest grade or level of school {you have/SP has} completed or the highest "| __truncated__ ...
##  $ Data.File.Name       : chr  "DEMO_D" "DEMO_D" "DEMO_D" "DEMO_D" ...
##  $ Data.File.Description: chr  "Demographic Variables & Sample Weights" "Demographic Variables & Sample Weights" "Demographic Variables & Sample Weights" "Demographic Variables & Sample Weights" ...
##  $ Begin.Year           : int  2005 2005 2005 2005 2005 2005 2005 2005 2005 2005 ...
##  $ EndYear              : int  2006 2006 2006 2006 2006 2006 2006 2006 2006 2006 ...
##  $ Component            : chr  "Demographics" "Demographics" "Demographics" "Demographics" ...
##  $ Use.Constraints      : chr  "" "" "" "" ...
```

``` r
dietary <- read.csv("Variable Dictionary/nhanes_Dietary_variables.csv")
str(dietary)
```

```
## 'data.frame':	7684 obs. of  8 variables:
##  $ Variable.Name        : chr  "DRALANG" "DRD020" "DRD030" "DRD040" ...
##  $ Variable.Description : chr  "The SP/Proxy spoke mostly:" "What time did you begin to eat/drink the meal/food." "Coded meal name" "List of places: Where did you eat this meal/food?" ...
##  $ Data.File.Name       : chr  "DRXIFF" "DRXIFF" "DRXIFF" "DRXIFF" ...
##  $ Data.File.Description: chr  "Dietary Interview - Individual Foods" "Dietary Interview - Individual Foods" "Dietary Interview - Individual Foods" "Dietary Interview - Individual Foods" ...
##  $ Begin.Year           : int  1999 1999 1999 1999 1999 1999 1999 1999 1999 1999 ...
##  $ EndYear              : int  2000 2000 2000 2000 2000 2000 2000 2000 2000 2000 ...
##  $ Component            : chr  "Dietary" "Dietary" "Dietary" "Dietary" ...
##  $ Use.Constraints      : logi  NA NA NA NA NA NA ...
```

``` r
lab <- read.csv("Variable Dictionary/nhanes_Laboratory_variables.csv")
str(lab)
```

```
## 'data.frame':	14510 obs. of  8 variables:
##  $ Variable.Name        : chr  "LBDBCDSI" "LBDBPBSI" "LBDTHGLC" "LBDTHGSI" ...
##  $ Variable.Description : chr  "Cadmium (nmol/L)" "Lead (umol/L)" "Mercury, total comment code" "Mercury, total (nmol/L )" ...
##  $ Data.File.Name       : chr  "PbCd_D" "PbCd_D" "PbCd_D" "PbCd_D" ...
##  $ Data.File.Description: chr  "Cadmium, Lead, & Total Mercury - Blood" "Cadmium, Lead, & Total Mercury - Blood" "Cadmium, Lead, & Total Mercury - Blood" "Cadmium, Lead, & Total Mercury - Blood" ...
##  $ Begin.Year           : int  2005 2005 2005 2005 2005 2005 2005 2005 2007 2007 ...
##  $ EndYear              : int  2006 2006 2006 2006 2006 2006 2006 2006 2008 2008 ...
##  $ Component            : chr  "Laboratory" "Laboratory" "Laboratory" "Laboratory" ...
##  $ Use.Constraints      : chr  "" "" "" "" ...
```

``` r
exam <- read.csv("Variable Dictionary/nhanes_examination_variables.csv")
str(exam)
```

```
## 'data.frame':	19149 obs. of  8 variables:
##  $ Variable.Name        : chr  "BPAARM" "BPACSZ" "BPAEN1" "BPAEN2" ...
##  $ Variable.Description : chr  "Arm selected:" "Cuff size (cm) (width X length)" "Enhancement used first reading" "Enhancement used second reading" ...
##  $ Data.File.Name       : chr  "BPX_D" "BPX_D" "BPX_D" "BPX_D" ...
##  $ Data.File.Description: chr  "Blood Pressure" "Blood Pressure" "Blood Pressure" "Blood Pressure" ...
##  $ Begin.Year           : int  2005 2005 2005 2005 2005 2005 2005 2005 2005 2005 ...
##  $ EndYear              : int  2006 2006 2006 2006 2006 2006 2006 2006 2006 2006 ...
##  $ Component            : chr  "Examination" "Examination" "Examination" "Examination" ...
##  $ Use.Constraints      : chr  "" "" "" "" ...
```

``` r
ques <- read.csv("Variable Dictionary/nhanes_Questionnaire_variables.csv")
str(ques)
```

```
## 'data.frame':	18755 obs. of  8 variables:
##  $ Variable.Name        : chr  "ACD010A" "ACD010B" "ACD010C" "ACD040" ...
##  $ Variable.Description : chr  "What language(s) {do you/does SP} usually speak at home?" "What language(s) {do you/does SP} usually speak at home?" "What language(s) {do you/does SP} usually speak at home?" "Now I'm going to ask you about language use. What language(s) {do you/does SP} usually speak at home?" ...
##  $ Data.File.Name       : chr  "ACQ_D" "ACQ_D" "ACQ_D" "ACQ_D" ...
##  $ Data.File.Description: chr  "Acculturation" "Acculturation" "Acculturation" "Acculturation" ...
##  $ Begin.Year           : int  2005 2005 2005 2005 2005 2005 2005 2005 2005 2005 ...
##  $ EndYear              : int  2006 2006 2006 2006 2006 2006 2006 2006 2006 2006 ...
##  $ Component            : chr  "Questionnaire" "Questionnaire" "Questionnaire" "Questionnaire" ...
##  $ Use.Constraints      : chr  "" "" "" "" ...
```

## add time variable "phase"


``` r
# save a raw data
prep_raw <- prep

# check how many obs are with hypertension
table(prep$htn_accaha)
```

```
## 
##   Yes 
## 26757
```

``` r
# actually all obs are "Yes"

# Add time variable as a covariate 
prep$phase <- ifelse(prep$Begin_Year >= "2013", "Falling", "Rising")
prep$phase <- relevel(factor(prep$phase), ref = "Rising")  
# change reference to "Rising" ("Before2013")

prep$phase <- as.factor(prep$phase)

prep$bp_control_accaha <- ifelse(prep$bp_control_accaha %in% c("Yes", 1), 1,
                              ifelse(prep$bp_control_accaha %in% c("No", 0), 0, NA))
```

## clean_fake_missing


``` r
clean_fake_missing <- function(df,
                                missing_strings = c("", " ", "NA", "N/A", "Refused", "Don't know"),
                                missing_numbers = c(7, 9, 77, 88, 99, 777, 888, 999, 7777, 8888, 9999),
                                verbose = TRUE,
                                save_csv = TRUE,
                                csv_path = "cleaned_data.csv") {
  df_cleaned <- df  

  for (col_name in names(df_cleaned)) {
    col <- df_cleaned[[col_name]]

    # for character
    if (is.character(col) || is.factor(col)) {
      before <- sum(is.na(col))
      col <- as.character(col)
      col[col %in% missing_strings] <- NA
      after <- sum(is.na(col))
      if (verbose && (after > before)) {
        cat(sprintf("Character cleaned: %-20s | NA: %d ➜ %d\n", col_name, before, after))
      }
      df_cleaned[[col_name]] <- col
    }

    # for numberic
    if (is.numeric(col) || is.integer(col)) {
      before <- sum(is.na(col))
      col[col %in% missing_numbers] <- NA
      after <- sum(is.na(col))
      if (verbose && (after > before)) {
        cat(sprintf("Numeric  cleaned: %-20s | NA: %d ➜ %d\n", col_name, before, after))
      }
      df_cleaned[[col_name]] <- col
    }
  }

  if (save_csv) {
    write.csv(df_cleaned, file = csv_path, row.names = FALSE)
    if (verbose) cat(sprintf("\n Cleaned data saved to: %s\n", csv_path))
  }
  
  return(df_cleaned)
}

prep <- clean_fake_missing(prep, csv_path = "prep_cleaned.csv")
```

```
## Numeric  cleaned: svy_id               | NA: 0 ➜ 3
## Numeric  cleaned: svy_strata           | NA: 0 ➜ 964
## Numeric  cleaned: demo_age_years       | NA: 0 ➜ 359
## Numeric  cleaned: bp_sys_mean          | NA: 0 ➜ 4
## Numeric  cleaned: bp_dia_mean          | NA: 0 ➜ 368
## Character cleaned: chol_measured_never  | NA: 0 ➜ 15694
## Character cleaned: chol_measured_last   | NA: 0 ➜ 15880
## Numeric  cleaned: chol_total           | NA: 15618 ➜ 15625
## Character cleaned: chol_total_gteq_200  | NA: 0 ➜ 15460
## Character cleaned: chol_total_gteq_240  | NA: 0 ➜ 15460
## Numeric  cleaned: chol_hdl             | NA: 15617 ➜ 15726
## Character cleaned: chol_hdl_low         | NA: 0 ➜ 15460
## Numeric  cleaned: chol_trig            | NA: 15626 ➜ 15863
## Character cleaned: chol_trig_gteq_150   | NA: 0 ➜ 15460
## Character cleaned: chol_ldl_5cat        | NA: 0 ➜ 15644
## Character cleaned: chol_ldl_lt_70       | NA: 0 ➜ 15460
## Character cleaned: chol_ldl_gteq_70     | NA: 0 ➜ 15460
## Character cleaned: chol_ldl_lt_100      | NA: 0 ➜ 15460
## Character cleaned: chol_ldl_gteq_100    | NA: 0 ➜ 15460
## Character cleaned: chol_ldl_gteq_190    | NA: 0 ➜ 15460
## Character cleaned: chol_ldl_persistent  | NA: 0 ➜ 15460
## Numeric  cleaned: chol_nonhdl          | NA: 15618 ➜ 15790
## Character cleaned: chol_nonhdl_5cat     | NA: 0 ➜ 15618
## Character cleaned: chol_nonhdl_lt_100   | NA: 0 ➜ 15460
## Character cleaned: chol_nonhdl_gteq_100 | NA: 0 ➜ 15460
## Character cleaned: chol_nonhdl_gteq_220 | NA: 0 ➜ 15460
## Character cleaned: chol_med_use         | NA: 0 ➜ 15460
## Character cleaned: chol_med_use_sr      | NA: 0 ➜ 15473
## Character cleaned: chol_med_statin      | NA: 0 ➜ 15460
## Character cleaned: chol_med_ezetimibe   | NA: 0 ➜ 15460
## Character cleaned: chol_med_pcsk9i      | NA: 0 ➜ 15460
## Character cleaned: chol_med_bile        | NA: 0 ➜ 15460
## Character cleaned: chol_med_fibric_acid | NA: 0 ➜ 15460
## Character cleaned: chol_med_atorvastatin | NA: 0 ➜ 15460
## Character cleaned: chol_med_simvastatin | NA: 0 ➜ 15460
## Character cleaned: chol_med_rosuvastatin | NA: 0 ➜ 15460
## Character cleaned: chol_med_pravastatin | NA: 0 ➜ 15460
## Character cleaned: chol_med_pitavastatin | NA: 0 ➜ 15460
## Character cleaned: chol_med_fluvastatin | NA: 0 ➜ 15460
## Character cleaned: chol_med_lovastatin  | NA: 0 ➜ 15460
## Character cleaned: chol_med_other       | NA: 0 ➜ 15460
## Character cleaned: chol_med_addon_use   | NA: 0 ➜ 15460
## Character cleaned: chol_med_addon_recommended_ahaacc | NA: 0 ➜ 15460
## Character cleaned: chol_med_statin_recommended_ahaacc | NA: 0 ➜ 15460
## Character cleaned: chol_med_recommended_ever | NA: 0 ➜ 15477
## Character cleaned: ascvd_risk_vh_ahaacc | NA: 0 ➜ 15460
## Character cleaned: cc_smoke             | NA: 0 ➜ 333
## Numeric  cleaned: cc_acr               | NA: 613 ➜ 661
## Numeric  cleaned: cc_hba1c             | NA: 1255 ➜ 1487
## Character cleaned: cc_egfr_lt60         | NA: 0 ➜ 1713
## Character cleaned: cc_acr_gteq30        | NA: 0 ➜ 613
## Character cleaned: cc_cvd_mi            | NA: 0 ➜ 434
## Character cleaned: cc_cvd_chd           | NA: 0 ➜ 437
## Character cleaned: cc_cvd_stroke        | NA: 0 ➜ 424
## Character cleaned: cc_cvd_hf            | NA: 0 ➜ 500
## Numeric  cleaned: URXUMA               | NA: 4740 ➜ 5000
## Numeric  cleaned: URXUCR               | NA: 4741 ➜ 5095
## Numeric  cleaned: LBXSATSI             | NA: 4347 ➜ 4539
## Numeric  cleaned: LBXSASSI             | NA: 4349 ➜ 4381
## Numeric  cleaned: LBXSBU               | NA: 4301 ➜ 6239
## Numeric  cleaned: LBXSCA               | NA: 4318 ➜ 5431
## Numeric  cleaned: LBXSCH               | NA: 4304 ➜ 4318
## Numeric  cleaned: LBXSGTSI             | NA: 4302 ➜ 4758
## Numeric  cleaned: LBXSGL               | NA: 4296 ➜ 5698
## Numeric  cleaned: LBXSIR               | NA: 4321 ➜ 5055
## Numeric  cleaned: LBXSTP               | NA: 4329 ➜ 6059
## Numeric  cleaned: LBDSTPSI             | NA: 4329 ➜ 5420
## Numeric  cleaned: LBXSTR_x             | NA: 4311 ➜ 4726
## Numeric  cleaned: LBXSUA               | NA: 4305 ➜ 4758
## Numeric  cleaned: LBXSNASI             | NA: 4301 ➜ 4302
## Numeric  cleaned: LBXSCLSI             | NA: 4302 ➜ 5219
## Numeric  cleaned: LBDSGBSI             | NA: 4330 ➜ 4331
## Numeric  cleaned: BMXWT                | NA: 4547 ➜ 4662
## Numeric  cleaned: BMXWAIST             | NA: 5397 ➜ 5573
## Numeric  cleaned: BPXSY1               | NA: 5939 ➜ 5954
## Numeric  cleaned: BPXDI1               | NA: 5939 ➜ 6759
## Numeric  cleaned: BPXSY2               | NA: 5836 ➜ 5848
## Numeric  cleaned: BPXDI2               | NA: 5836 ➜ 6643
## Numeric  cleaned: BPXSY3               | NA: 6027 ➜ 6036
## Numeric  cleaned: BPXDI3               | NA: 6027 ➜ 6831
## Numeric  cleaned: LBXWBCSI             | NA: 7546 ➜ 7907
## Numeric  cleaned: LBXLYPCT             | NA: 7591 ➜ 7594
## Numeric  cleaned: LBXMOPCT             | NA: 7591 ➜ 8037
## Numeric  cleaned: LBXNEPCT             | NA: 7591 ➜ 7600
## Numeric  cleaned: LBXEOPCT             | NA: 7591 ➜ 7622
## Numeric  cleaned: LBDLYMNO             | NA: 7591 ➜ 7592
## Numeric  cleaned: LBDNENO              | NA: 7591 ➜ 7716
## Numeric  cleaned: LBXHGB               | NA: 7546 ➜ 7551
## Numeric  cleaned: LBXPLTSI             | NA: 7546 ➜ 7556
## Numeric  cleaned: LBXMPSI              | NA: 7546 ➜ 8162
## Numeric  cleaned: SDDSRVYR             | NA: 4208 ➜ 9516
## Numeric  cleaned: RIDAGEYR             | NA: 4208 ➜ 4515
## Numeric  cleaned: DMDHHSIZ             | NA: 4208 ➜ 5204
## Numeric  cleaned: SDMVSTRA             | NA: 4208 ➜ 5172
## Numeric  cleaned: LBXGH                | NA: 5266 ➜ 5453
## Numeric  cleaned: SMQ020               | NA: 4519 ➜ 4538
## Numeric  cleaned: LBXTC                | NA: 1634 ➜ 1642
## Numeric  cleaned: LBDHDL               | NA: 22433 ➜ 22483
## Numeric  cleaned: LBDLDL               | NA: 17200 ➜ 17471
## Numeric  cleaned: LBXSTR_y             | NA: 11228 ➜ 11505
## Numeric  cleaned: FriedewaldLDL        | NA: 22605 ➜ 22625
## Numeric  cleaned: ldl_corrected        | NA: 12862 ➜ 12883
## 
##  Cleaned data saved to: prep_cleaned.csv
```

### check "chol_" variables


``` r
chol_vars <- grep("^chol_", names(prep), value = TRUE)
length(chol_vars) # 41
```

```
## [1] 41
```

``` r
missing_rates <- sapply(prep[chol_vars], function(col) {
  mean(is.na(col))
})

range(missing_rates) # 0.5777927 0.5934896
```

```
## [1] 0.5777927 0.5934896
```

``` r
# transform to a data.frame and order by missing rate (descending)
missing_df <- data.frame(
  Variable = names(missing_rates),
  MissingRate = missing_rates
) %>%
  arrange(desc(MissingRate))

print(missing_df)
```

```
##                                                              Variable
## chol_measured_last                                 chol_measured_last
## chol_trig                                                   chol_trig
## chol_nonhdl                                               chol_nonhdl
## chol_hdl                                                     chol_hdl
## chol_measured_never                               chol_measured_never
## chol_ldl_5cat                                           chol_ldl_5cat
## chol_ldl                                                     chol_ldl
## chol_total                                                 chol_total
## chol_nonhdl_5cat                                     chol_nonhdl_5cat
## chol_med_recommended_ever                   chol_med_recommended_ever
## chol_med_use_sr                                       chol_med_use_sr
## chol_total_gteq_200                               chol_total_gteq_200
## chol_total_gteq_240                               chol_total_gteq_240
## chol_hdl_low                                             chol_hdl_low
## chol_trig_gteq_150                                 chol_trig_gteq_150
## chol_ldl_lt_70                                         chol_ldl_lt_70
## chol_ldl_gteq_70                                     chol_ldl_gteq_70
## chol_ldl_lt_100                                       chol_ldl_lt_100
## chol_ldl_gteq_100                                   chol_ldl_gteq_100
## chol_ldl_gteq_190                                   chol_ldl_gteq_190
## chol_ldl_persistent                               chol_ldl_persistent
## chol_nonhdl_lt_100                                 chol_nonhdl_lt_100
## chol_nonhdl_gteq_100                             chol_nonhdl_gteq_100
## chol_nonhdl_gteq_220                             chol_nonhdl_gteq_220
## chol_med_use                                             chol_med_use
## chol_med_statin                                       chol_med_statin
## chol_med_ezetimibe                                 chol_med_ezetimibe
## chol_med_pcsk9i                                       chol_med_pcsk9i
## chol_med_bile                                           chol_med_bile
## chol_med_fibric_acid                             chol_med_fibric_acid
## chol_med_atorvastatin                           chol_med_atorvastatin
## chol_med_simvastatin                             chol_med_simvastatin
## chol_med_rosuvastatin                           chol_med_rosuvastatin
## chol_med_pravastatin                             chol_med_pravastatin
## chol_med_pitavastatin                           chol_med_pitavastatin
## chol_med_fluvastatin                             chol_med_fluvastatin
## chol_med_lovastatin                               chol_med_lovastatin
## chol_med_other                                         chol_med_other
## chol_med_addon_use                                 chol_med_addon_use
## chol_med_addon_recommended_ahaacc   chol_med_addon_recommended_ahaacc
## chol_med_statin_recommended_ahaacc chol_med_statin_recommended_ahaacc
##                                    MissingRate
## chol_measured_last                   0.5934896
## chol_trig                            0.5928542
## chol_nonhdl                          0.5901259
## chol_hdl                             0.5877341
## chol_measured_never                  0.5865381
## chol_ldl_5cat                        0.5846694
## chol_ldl                             0.5839967
## chol_total                           0.5839593
## chol_nonhdl_5cat                     0.5836977
## chol_med_recommended_ever            0.5784281
## chol_med_use_sr                      0.5782786
## chol_total_gteq_200                  0.5777927
## chol_total_gteq_240                  0.5777927
## chol_hdl_low                         0.5777927
## chol_trig_gteq_150                   0.5777927
## chol_ldl_lt_70                       0.5777927
## chol_ldl_gteq_70                     0.5777927
## chol_ldl_lt_100                      0.5777927
## chol_ldl_gteq_100                    0.5777927
## chol_ldl_gteq_190                    0.5777927
## chol_ldl_persistent                  0.5777927
## chol_nonhdl_lt_100                   0.5777927
## chol_nonhdl_gteq_100                 0.5777927
## chol_nonhdl_gteq_220                 0.5777927
## chol_med_use                         0.5777927
## chol_med_statin                      0.5777927
## chol_med_ezetimibe                   0.5777927
## chol_med_pcsk9i                      0.5777927
## chol_med_bile                        0.5777927
## chol_med_fibric_acid                 0.5777927
## chol_med_atorvastatin                0.5777927
## chol_med_simvastatin                 0.5777927
## chol_med_rosuvastatin                0.5777927
## chol_med_pravastatin                 0.5777927
## chol_med_pitavastatin                0.5777927
## chol_med_fluvastatin                 0.5777927
## chol_med_lovastatin                  0.5777927
## chol_med_other                       0.5777927
## chol_med_addon_use                   0.5777927
## chol_med_addon_recommended_ahaacc    0.5777927
## chol_med_statin_recommended_ahaacc   0.5777927
```

## split into two subsets based on chol == 0


``` r
prep_chol <- subset(prep, svy_subpop_chol == 1) # 11118, 161
prep_no_chol <- subset(prep, svy_subpop_chol == 0) # 15639, 161
```

# prep_chol

## unit conversions pair and keep SI only


``` r
get_unit_conversion_pairs <- function(data) {
  var_names <- names(data)
  lbx_vars <- grep("^LBX", var_names, value = TRUE)
  lbd_vars <- grep("^LBD", var_names, value = TRUE)
  
  get_core <- function(var) {
    core <- sub("^LBX", "", var)
    core <- sub("_[A-Za-z0-9]+$", "", core)
    return(core)
  }
  
  matched_pairs <- list()
  
  for (lbx in lbx_vars) {
    core <- get_core(lbx)
    matched <- grep(paste0("^LBD", core), lbd_vars, value = TRUE)
    if (length(matched) > 0) {
      matched_pairs[[length(matched_pairs) + 1]] <- c(lbx, matched[1])
    }
  }
  
  # output a data.frame
  matched_df <- do.call(rbind, matched_pairs)
  colnames(matched_df) <- c("LBX_variable", "LBD_variable")
  return(as.data.frame(matched_df, stringsAsFactors = FALSE))
}

matched_unit_pairs <- get_unit_conversion_pairs(prep_chol)
View(matched_unit_pairs) 
```


``` r
cor_results <- data.frame(
  LBX_variable = character(),
  LBD_variable = character(),
  correlation = numeric(),
  stringsAsFactors = FALSE
)

# calculate correlation coefficient
for (i in 1:nrow(matched_unit_pairs)) {
  lbx <- matched_unit_pairs$LBX_variable[i]
  lbd <- matched_unit_pairs$LBD_variable[i]
  
  if (all(c(lbx, lbd) %in% names(prep))) {
    r <- cor(prep[[lbx]], prep[[lbd]], use = "pairwise.complete.obs")
    cor_results <- rbind(cor_results, data.frame(
      LBX_variable = lbx,
      LBD_variable = lbd,
      correlation = round(r, 6),
      stringsAsFactors = FALSE
    ))
  } else {
    warning(paste("Missing variable in prep:", lbx, "or", lbd))
  }
}

cor_results
```

```
##   LBX_variable LBD_variable correlation
## 1       LBXSTP     LBDSTPSI           1
## 2     LBXSTR_x     LBDSTRSI           1
## 3       LBXSUA     LBDSUASI           1
## 4       LBXSGB     LBDSGBSI           1
## 5     LBXSTR_y     LBDSTRSI           1
```

### keep SI only


``` r
prep_chol <- prep_chol %>% select(-LBXSTP, -LBXSTR_x, -LBXSUA, -LBXSGB, -LBXSTR_y) # 11118, 156
```

## collinear >= 0.95 variables 

The variables (or variable with levels) LBDSTPSI, LBDSGBSI, SDDSRVYR, LBXGH, LBXSTR_y are perfectly collinear with another variable in the data.


``` r
# Step 1: Select all continuous variables (numeric) in prep
numeric_vars <- prep_chol[, sapply(prep_chol, is.numeric)]

# Step 2: Compute the correlation matrix between variables (ignoring missing values)
cor_matrix <- cor(numeric_vars, use = "pairwise.complete.obs")
```

```
## Warning in cor(numeric_vars, use = "pairwise.complete.obs"): the standard
## deviation is zero
```

``` r
# Step 3: Find pairs of variables with |correlation coefficient| >= 0.95 
collinear_pairs <- which(abs(cor_matrix) >= 0.95 & lower.tri(cor_matrix), arr.ind = TRUE)

# Step 4: Display completely collinear pairs of variable names
collinear_vars <- data.frame(
  var1 = rownames(cor_matrix)[collinear_pairs[, 1]],
  var2 = colnames(cor_matrix)[collinear_pairs[, 2]],
  r = cor_matrix[collinear_pairs]
)

print(collinear_vars)
```

```
##             var1           var2         r
## 1     svy_strata         svy_id 0.9903663
## 2           YEAR         svy_id 0.9939802
## 3    Survey_Year         svy_id 0.9841804
## 4       SDDSRVYR         svy_id 0.9920590
## 5       SDMVSTRA         svy_id 0.9884064
## 6     Begin_Year         svy_id 0.9841804
## 7       WTINT2YR svy_weight_mec 0.9972094
## 8       WTMEC2YR svy_weight_mec 1.0000000
## 9        SDMVPSU        svy_psu 1.0000000
## 10          YEAR     svy_strata 0.9943459
## 11   Survey_Year     svy_strata 0.9887755
## 12      SDDSRVYR     svy_strata 0.9923453
## 13      SDMVSTRA     svy_strata 1.0000000
## 14    Begin_Year     svy_strata 0.9887755
## 15      RIDAGEYR demo_age_years 0.9989222
## 16        BPXSY1    bp_sys_mean 0.9782211
## 17        BPXSY2    bp_sys_mean 0.9839424
## 18        BPXSY3    bp_sys_mean 0.9787903
## 19        LBXSCH     chol_total 0.9839118
## 20         LBXTC     chol_total 1.0000000
## 21        LBDHDL       chol_hdl 1.0000000
## 22      LBDSTRSI      chol_trig 0.9822510
## 23        LBDLDL       chol_ldl 0.9991425
## 24 FriedewaldLDL       chol_ldl 0.9975579
## 25 ldl_corrected       chol_ldl 1.0000000
## 26         LBXGH       cc_hba1c 1.0000000
## 27   Survey_Year           YEAR 1.0000000
## 28      SDDSRVYR           YEAR 1.0000000
## 29      SDMVSTRA           YEAR 0.9943459
## 30    Begin_Year           YEAR 1.0000000
## 31         LBXTC         LBXSCH 0.9839118
## 32        LBXHCT         LBXHGB 0.9646636
## 33      SDDSRVYR    Survey_Year 1.0000000
## 34      SDMVSTRA    Survey_Year 0.9943459
## 35    Begin_Year    Survey_Year 1.0000000
## 36      SDMVSTRA       SDDSRVYR 0.9923453
## 37    Begin_Year       SDDSRVYR 1.0000000
## 38      WTMEC2YR       WTINT2YR 0.9972094
## 39    Begin_Year       SDMVSTRA 0.9943459
## 40 FriedewaldLDL         LBDLDL 0.9981804
## 41 ldl_corrected         LBDLDL 0.9991425
## 42 ldl_corrected  FriedewaldLDL 0.9975579
```

``` r
write.csv(collinear_vars, "collinear_vars_0.95_prep_chol.csv", row.names = FALSE)
```


``` r
# calculate the correlation
r_hct <- cor(prep_chol$LBXHCT, prep_chol$bp_control_accaha, use = "complete.obs")
r_hgb <- cor(prep_chol$LBXHGB, prep_chol$bp_control_accaha, use = "complete.obs")

# print
cat("Correlation with bp_control_accaha:\n")
cat("LBXHCT:", round(r_hct, 4), "\n")
cat("LBXHGB:", round(r_hgb, 4), "\n")
```

### get_description


``` r
get_description <- function(varname, df) {
  result <- df %>%
    filter(Variable.Name == varname)
  new_name <- paste0(varname, "_descrip")
  assign(new_name, result, envir = .GlobalEnv)
}

get_description("WTMEC2YR", demogra)
get_description("svy_weight_mec", demogra)
get_description("LBXTC", lab)
get_description("chol_ldl", dietary)
get_description("LBDSTPSI", lab)
get_description("LBXSTR_x", lab)
get_description("BPAARM", exam)
```

### compare variables collinear = 1 


``` r
# prep[, c("LBXGH", "cc_hba1c")]

compare_collinear_vars <- function(data, var_pairs_df, print_n = TRUE) {
  results <- list()
  
  for (i in seq_len(nrow(var_pairs_df))) {
    col1 <- var_pairs_df$var1[i]
    col2 <- var_pairs_df$var2[i]
   
    if (!(col1 %in% names(data)) | !(col2 %in% names(data))) {
      warning("One of the columns not found: ", col1, " or ", col2)
      next
    }
    
    unequal_or_mismatched_na <- which(
      is.na(data[[col1]]) != is.na(data[[col2]]) |
      (!is.na(data[[col1]]) & !is.na(data[[col2]]) & data[[col1]] != data[[col2]])
    )
    
    mismatch_df <- data[unequal_or_mismatched_na, c(col1, col2)]
    
    if (print_n) {
      message(length(unequal_or_mismatched_na), 
              " row(s) where '", col1, "' and '", col2, "' are unequal or NA-mismatched.")
    }
    
    results[[paste(col1, col2, sep = "_vs_")]] <- mismatch_df
  }
  
  return(results)
}

results_collinear <- compare_collinear_vars(prep_chol, collinear_vars)

names(results_collinear)  

# results_collinear[["LBDSGBSI_vs_LBXSGB"]]  

# all.equal(prep$LBXSTP, prep$LBDSTPSI / 10) # LBDSTP derived by dividing LBDSTPSI by 10
```

## delete variables collinear >= 0.95


``` r
# delete collinear >= 0.95 variables 
prep_chol_1 <- prep_chol %>% select(-svy_weight_mec, -svy_strata, -svy_psu, -Begin_Year, -Survey_Year, -YEAR, -SDDSRVYR, -WTMEC2YR, -WTINT2YR, -SDMVPSU, -SDMVSTRA, -RIDAGEYR, -BPXSY1, -BPXSY2, -BPXSY3, -bp_sys_mean, -LBXSCH, -chol_total, -LBDHDL, -LBDLDL, -chol_ldl, -FriedewaldLDL, -LBDSTRSI, -LBXGH, -LBXHGB)

# delete other BP variables
prep_chol_1 <- prep_chol_1 %>% select(-BPXDI1, -BPXDI2, -BPXDI3, -bp_dia_mean) # 11118, 127

# prep_chol[, c("LBXSCH", "LBXTC")]
```

## delete variables that do not vary


``` r
constant_vars <- names(prep_chol_1)[sapply(prep_chol_1, function(col) {
  length(unique(na.omit(col))) == 1
})]

print(constant_vars) 
```

```
## [1] "svy_subpop_chol" "htn_accaha"      "RIDSTATR"
```

``` r
# svy_subpop_chol      htn_accaha        RIDSTATR 
#            "1"           "Yes"             "2" 

sapply(prep_chol_1[constant_vars], function(x) unique(na.omit(x)))
```

```
## svy_subpop_chol      htn_accaha        RIDSTATR 
##             "1"           "Yes"             "2"
```

``` r
write.csv(data.frame(Constant_Variables = constant_vars),
          "constant_vars_removed_prep_chol.csv", row.names = FALSE)

prep_chol_1 <- prep_chol_1[, !(names(prep_chol_1) %in% constant_vars)] # 11118, 124
```

## delete variables that barely change (e.g., a value that accounts for 99% of all variables)


``` r
# set threshold
dominant_threshold <- 0.99

# filter variables that barely change
low_variability_vars <- names(prep_chol_1)[sapply(prep_chol_1, function(col) {
  non_missing <- na.omit(col)
  if (length(non_missing) == 0) return(FALSE)
  max_prop <- max(table(non_missing)) / length(non_missing)
  return(max_prop >= dominant_threshold)
})]

print(low_variability_vars)
```

```
## [1] "chol_med_pcsk9i"       "chol_med_bile"         "chol_med_pitavastatin"
## [4] "chol_med_fluvastatin"  "chol_med_other"        "BPAARM"
```

``` r
# View the dominant values and proportions of these variables
sapply(prep_chol_1[low_variability_vars], function(x) {
  non_missing <- na.omit(x)
  top_val <- names(sort(table(non_missing), decreasing = TRUE))[1]
  top_prop <- max(table(non_missing)) / length(non_missing)
  paste0("Most frequent value: ", top_val, " (", round(top_prop * 100, 2), "%)")
})
```

```
##                    chol_med_pcsk9i                      chol_med_bile 
## "Most frequent value: No (99.96%)" "Most frequent value: No (99.68%)" 
##              chol_med_pitavastatin               chol_med_fluvastatin 
## "Most frequent value: No (99.95%)" "Most frequent value: No (99.75%)" 
##                     chol_med_other                             BPAARM 
## "Most frequent value: No (99.42%)"  "Most frequent value: 1 (99.22%)"
```

``` r
# delete 6 variables
prep_chol_1 <- prep_chol_1 %>% select(-chol_med_pcsk9i, -chol_med_bile, -chol_med_pitavastatin, -chol_med_fluvastatin, -chol_med_other, -BPAARM) # 11118, 118
```

## missing value


``` r
# Step 1: calculate the ratio
na_ratios <- sapply(prep_chol_1, function(col) mean(is.na(col)))
no_missing_cols <- names(na_ratios[na_ratios == 0])  # columns don't have NA
prep_chol_fixed <- prep_chol_1[, no_missing_cols] # 43

na_ratios <- na_ratios[na_ratios > 0]  # 75

# bins
bins <- cut(na_ratios, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE, right = FALSE)
bin_counts <- table(bins)

# plot
ggplot(data.frame(bin = names(bin_counts), count = as.vector(bin_counts)), aes(x = bin, y = count)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Missing Value Proportion Histogram for prep_chol",
       x = "Missing Rate Bin", y = "Number of Columns") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](Final_Project_XianjieQue_BDM_files/figure-html/missing value-1.png)<!-- -->

``` r
# Step 2: remove and retain columns
threshold <- 0.5
keep_cols <- names(na_ratios[na_ratios <= threshold])
drop_cols <- names(na_ratios[na_ratios > threshold])

kept_df <- data.frame(column = keep_cols, missing_rate = na_ratios[keep_cols]) # 75
dropped_df <- data.frame(column = drop_cols, missing_rate = na_ratios[drop_cols]) # 0

# save columns to csv
write.csv(kept_df, "kept_columns_prep_chol.csv", row.names = FALSE) 
write.csv(dropped_df, "dropped_columns_prep_chol.csv", row.names = FALSE) 

# Step 3: check continuous columns and categorical variables
prep_chol_kept <- prep_chol_1[, keep_cols] # 75

num_continuous <- sum(sapply(prep_chol_kept, is.numeric))
num_categorical <- sum(sapply(prep_chol_kept, function(col) is.factor(col) || is.character(col)))

cat("Number of continuous variables: ", num_continuous, "\n") # 64
```

```
## Number of continuous variables:  64
```

``` r
cat("Number of categorical variables: ", num_categorical, "\n") # 11
```

```
## Number of categorical variables:  11
```


## imputation NA

Warning in amcheck(x = x, m = m, idvars = numopts$idvars, priors = priors,  :
  The variable LBXTC is perfectly collinear with another variable in the data.
  
The resulting variance matrix was not invertible.   Please check your data for highly collinear variables.


``` r
# Step 1: split continuous and categorical
prep_cont <- prep_chol_kept[, sapply(prep_chol_kept, is.numeric)]
prep_cat  <- prep_chol_kept[, sapply(prep_chol_kept, function(x) is.factor(x) || is.character(x))]

prep_cont <- prep_cont %>% select(-chol_nonhdl)

# Step 2: EM imputation for continuous variables
amelia_result <- amelia(prep_cont, m = 1)  # advanced method: m = 5
```

```
## -- Imputation 1 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56
```

``` r
cont_imputed <- amelia_result$imputations[[1]]

# Step 3: KNN imputation for categorical variables
cat_imputed <- kNN(prep_cat, k = 5, imp_var = FALSE)

# Step 4: combine
prep_chol_imputed <- cbind(cont_imputed, cat_imputed, prep_chol_fixed) # 11118, 117
```

### check "LBXTC"


``` r
# Step 1: Extract non-missing rows containing "LBXTC"
df_non_na <- prep_cont %>% filter(!is.na(LBXTC))

# Step 2: Calculate the correlation coefficient with "LBXTC"
cor_vals <- sapply(df_non_na, function(x) {
  if (is.numeric(x)) cor(x, df_non_na$LBXTC, use = "complete.obs") else NA
})

# Step 3: Find variables with absolute value >= 0.9
collinear_vars_1 <- names(which(abs(cor_vals) >= 0.9 & names(cor_vals) != "LBXTC"))

# View Results
print(collinear_vars_1)
```

## collinear > 0.8 before Lasso and Logistics


``` r
# Step 1: Select all continuous variables (numeric) in prep_chol
numeric_vars <- prep_chol_imputed[, sapply(prep_chol_imputed, is.numeric)]

# Step 2: Compute the correlation matrix between variables (ignoring missing values)
cor_matrix <- cor(numeric_vars, use = "pairwise.complete.obs")

# Step 3: Find pairs of variables with |correlation coefficient| >= 0.8 
collinear_pairs <- which(abs(cor_matrix) >= 0.8 & lower.tri(cor_matrix), arr.ind = TRUE)

# Step 4: Display completely collinear pairs of variable names
collinear_vars <- data.frame(
  var1 = rownames(cor_matrix)[collinear_pairs[, 1]],
  var2 = colnames(cor_matrix)[collinear_pairs[, 2]],
  r = cor_matrix[collinear_pairs]
)

print(collinear_vars)
```

```
##             var1     var2          r
## 1         URXUMA   cc_acr  0.8816417
## 2         LBXSGL cc_hba1c  0.8488038
## 3         BMXBMI    BMXWT  0.8749749
## 4        BMXARMC    BMXWT  0.8800403
## 5       BMXWAIST    BMXWT  0.8922814
## 6        BMXARMC   BMXBMI  0.8679857
## 7       BMXWAIST   BMXBMI  0.8957542
## 8         BPACSZ  BMXARMC  0.8329874
## 9       LBXNEPCT LBXLYPCT -0.9381333
## 10       LBDEONO LBXEOPCT  0.8149621
## 11        LBXHCT LBXRBCSI  0.8150682
## 12 ldl_corrected    LBXTC  0.8349166
```

``` r
write.csv(collinear_vars, "collinear_vars_0.8_prep_chol.csv", row.names = FALSE)
```

## delete variables collinear >= 0.8


``` r
# delete collinear >= 0.8 variables 
prep_chol_imputed <- prep_chol_imputed %>% select(-URXUMA, -LBXSGL, -ldl_corrected, -BMXWT, -BMXARMC, -BPACSZ, -LBXLYPCT, -LBXRBCSI)

# keep: LBXTC, cc_acr, cc_hba1c, BMXBMI, BMXWAIST, LBXNEPCT, LBXHCT

# head(prep_imputed[, c("LBXSCH", "LBXTC")], 20)

sum(is.na(prep_chol_imputed)) # 11118, 109
```

```
## [1] 0
```

## Lasso to full dataset with interaction


``` r
prep_chol_imputed_2 <- prep_chol_imputed %>% select(-svy_id)  # 11118, 108

prep_chol_imputed_2$phase <- relevel(factor(prep_chol_imputed_2$phase), ref = "Rising")

# LASSO pipeline with interactions (excluding main effect of phase)
run_lasso_with_interaction <- function(data,
                                     response_var = "bp_control_accaha",
                                     save_dir = ".",
                                     dataset_name = "prep_chol_full") {
  set.seed(123)

  # Ensure phase is factor with correct reference level
  data$phase <- relevel(factor(data$phase), ref = "Rising")

  # Step 1: Prepare predictors (exclude response and phase)
  predictors <- setdiff(names(data), c(response_var, "phase"))

  # Step 2: Construct formula with interactions
  main_effects <- paste(predictors, collapse = " + ")
  interaction_terms <- paste(paste0(predictors, ":phase"), collapse = " + ")
  formula_string <- paste(response_var, "~", main_effects, "+", interaction_terms)

  # Step 3: Create full design matrix
  design_matrix <- model.matrix(as.formula(formula_string), data = data)[, -1]
  response <- data[[response_var]]

  # Step 4: Train/test split
  train_idx <- sample(1:nrow(data), 0.7 * nrow(data))
  x_train <- design_matrix[train_idx, ]
  y_train <- response[train_idx]
  x_test <- design_matrix[-train_idx, ]
  y_test <- response[-train_idx]

  # Step 5: Cross-validation with fixed folds
  foldid <- sample(rep(1:10, length.out = length(y_train)))
  cvfit <- cv.glmnet(x = x_train, y = y_train, family = "binomial", alpha = 1, foldid = foldid)
  best_lambda <- cvfit$lambda.min

  # Step 6: Predict on test set
  pred_prob <- as.vector(predict(cvfit, newx = x_test, s = best_lambda, type = "response"))
  pred_class <- ifelse(pred_prob > 0.5, 1, 0)
  acc <- mean(pred_class == y_test)
  auc_val <- auc(roc(y_test, pred_prob))

  # Step 7: Refit on full data using best lambda
  final_model <- glmnet(x = design_matrix, y = response, family = "binomial", alpha = 1, lambda = best_lambda)
  coef_final <- coef(final_model)
  selected_vars <- rownames(coef_final)[which(coef_final != 0)]
  selected_vars <- selected_vars[selected_vars != "(Intercept)"]

  # Step 8: Save results
  results_df <- data.frame(variable = selected_vars)
  summary_df <- data.frame(metric = c("accuracy", "auc"), value = c(acc, auc_val))

  write.csv(results_df, file = file.path(save_dir, paste0("lasso_selected_", dataset_name, ".csv")), row.names = FALSE)
  write.csv(summary_df, file = file.path(save_dir, paste0("lasso_summary_", dataset_name, ".csv")), row.names = FALSE)

  # Step 9: Save plots
  png(filename = file.path(save_dir, paste0("cv_curve_", dataset_name, ".png")), width = 800, height = 600)
  plot(cvfit, main = paste("LASSO CV -", dataset_name))
  dev.off()

  png(filename = file.path(save_dir, paste0("auc_curve_", dataset_name, ".png")), width = 800, height = 600)
  plot(roc(y_test, pred_prob), main = paste("AUC ROC -", dataset_name), col = "blue", lwd = 2)
  dev.off()

  # Step 10: Return
  return(list(model = final_model, selected = selected_vars, accuracy = acc, auc = auc_val))
}

result_lasso <- run_lasso_with_interaction(data = prep_chol_imputed_2)
```

```
## Setting levels: control = 0, case = 1
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = 0, case = 1
```

```
## Setting direction: controls < cases
```

## Build Logistic Regression Formula


``` r
# Step 1: extract interaction terms
lasso_selected_vars <- result_lasso[["selected"]]
interaction_vars <- grep(":phase", lasso_selected_vars, value = TRUE)
cat("interaction terms from Lasso: \n")
```

```
## interaction terms from Lasso:
```

``` r
print(interaction_vars)
```

```
##  [1] "chol_trig:phaseFalling"                        
##  [2] "URXUCR:phaseFalling"                           
##  [3] "LBDBANO:phaseFalling"                          
##  [4] "LBDSPH:phaseFalling"                           
##  [5] "cc_acr_gteq30Yes:phaseFalling"                 
##  [6] "demo_age_cat75+:phaseFalling"                  
##  [7] "demo_raceNon-Hispanic Black:phaseFalling"      
##  [8] "demo_raceNon-Hispanic White:phaseFalling"      
##  [9] "htn_resistant_jnc7Yes:phaseFalling"            
## [10] "chol_nonhdl_5cat100 to <130 mg/dL:phaseFalling"
## [11] "chol_med_rosuvastatinYes:phaseFalling"         
## [12] "chol_med_addon_useYes:phaseFalling"            
## [13] "cc_diabetesYes:phaseFalling"
```

``` r
# Step 2: build logistic regression formula
# main_effects <- unique(sub(":phase.*", "", interaction_vars))

main_effects <- c("chol_trig", "LBDSPH", "LBDSCR", "cc_egfr_lt60", "cc_acr_gteq30", "demo_age_cat", "demo_race", "htn_resistant_jnc7", "htn_resistant_jnc7_thz", "chol_nonhdl_5cat", "chol_med_rosuvastatin", "chol_med_addon_use", "cc_diabetes")

interaction_vars_2 <- paste0(main_effects, ":phase")

# all_vars<- unique(c(main_effects, interaction_vars_2))
# formula_str <- paste("bp_control_accaha ~", paste(all_vars, collapse = " + "))
formula_str <- paste("bp_control_accaha ~", paste(interaction_vars_2, collapse = " + "))
formula_obj <- as.formula(formula_str)
```

## Logistic with weight

Logistics with survey weights, using svyglm(), is intended to make inferences about the population, and representativeness needs to be considered.


``` r
prep_svy <- prep_chol_imputed %>%
  left_join(prep_chol %>% select(svy_id, svy_strata, svy_psu, svy_weight_mec), by = "svy_id")

prep_svy <- prep_svy %>%
  filter(!is.na(svy_psu) & !is.na(svy_strata) & !is.na(svy_weight_mec))

prep_svy <- prep_svy %>%
  mutate(bp_control_accaha = as.integer(bp_control_accaha))

design <- svydesign(ids = ~svy_psu,
                       strata = ~svy_strata,
                       weights = ~svy_weight_mec,
                       nest = TRUE,
                       data = prep_svy)

fit_svy <- svyglm(formula_obj,
                     design = design,
                     family = binomial())
```

```
## Warning in eval(family$initialize): non-integer #successes in a binomial glm!
```

``` r
summary(fit_svy)
```

```
## 
## Call:
## svyglm(formula = formula_obj, design = design, family = binomial())
## 
## Survey design:
## svydesign(ids = ~svy_psu, strata = ~svy_strata, weights = ~svy_weight_mec, 
##     nest = TRUE, data = prep_svy)
## 
## Coefficients:
##                                                  Estimate Std. Error t value
## (Intercept)                                    -2.2875238  0.1907966 -11.989
## chol_trig:phaseFalling                          0.0017045  0.0006002   2.840
## chol_trig:phaseRising                           0.0008822  0.0003798   2.323
## phaseFalling:LBDSPH                             0.4181112  0.1344063   3.111
## phaseRising:LBDSPH                             -0.1352863  0.0775957  -1.743
## phaseFalling:LBDSCR                             0.0008449  0.0007544   1.120
## phaseRising:LBDSCR                              0.0007139  0.0010053   0.710
## phaseFalling:cc_egfr_lt60Yes                    0.6657434  0.2137902   3.114
## phaseRising:cc_egfr_lt60Yes                     0.4625997  0.1450254   3.190
## phaseFalling:cc_acr_gteq30Yes                  -0.7133380  0.1584173  -4.503
## phaseRising:cc_acr_gteq30Yes                   -0.3829626  0.1079771  -3.547
## phaseFalling:demo_age_cat45 to 64               0.9400050  0.1674335   5.614
## phaseRising:demo_age_cat45 to 64                1.1073151  0.1228884   9.011
## phaseFalling:demo_age_cat65 to 74               0.9889886  0.1886435   5.243
## phaseRising:demo_age_cat65 to 74                1.2037237  0.1348839   8.924
## phaseFalling:demo_age_cat75+                    0.5402752  0.2485724   2.174
## phaseRising:demo_age_cat75+                     0.7448234  0.1525724   4.882
## phaseFalling:demo_raceNon-Hispanic Asian       -0.2479033  0.2096081  -1.183
## phaseRising:demo_raceNon-Hispanic Asian         0.0751484  0.3787286   0.198
## phaseFalling:demo_raceNon-Hispanic Black       -0.0961204  0.1473900  -0.652
## phaseRising:demo_raceNon-Hispanic Black         0.4403311  0.1593107   2.764
## phaseFalling:demo_raceNon-Hispanic White        0.4242448  0.1355177   3.131
## phaseRising:demo_raceNon-Hispanic White         0.6457496  0.1338500   4.824
## phaseFalling:demo_raceOther                     0.6098327  0.2715277   2.246
## phaseRising:demo_raceOther                      0.3571204  0.2164950   1.650
## phaseFalling:htn_resistant_jnc7Yes             -1.0951014  0.3377323  -3.243
## phaseRising:htn_resistant_jnc7Yes              -1.3247173  0.3450217  -3.840
## phaseFalling:htn_resistant_jnc7_thzYes          0.2282590  0.4245855   0.538
## phaseRising:htn_resistant_jnc7_thzYes           0.7011699  0.3853427   1.820
## phaseFalling:chol_nonhdl_5cat>= 220 mg/dL      -1.2685091  0.3631167  -3.493
## phaseRising:chol_nonhdl_5cat>= 220 mg/dL       -1.6612518  0.2888017  -5.752
## phaseFalling:chol_nonhdl_5cat100 to <130 mg/dL -0.2756573  0.1213037  -2.272
## phaseRising:chol_nonhdl_5cat100 to <130 mg/dL  -0.4118762  0.1581789  -2.604
## phaseFalling:chol_nonhdl_5cat130 to <160 mg/dL -0.8591546  0.1769788  -4.855
## phaseRising:chol_nonhdl_5cat130 to <160 mg/dL  -0.4942843  0.1529436  -3.232
## phaseFalling:chol_nonhdl_5cat160 to <220 mg/dL -0.8552715  0.1829502  -4.675
## phaseRising:chol_nonhdl_5cat160 to <220 mg/dL  -0.9153332  0.1552505  -5.896
## phaseFalling:chol_med_rosuvastatinYes           0.7933863  0.2493079   3.182
## phaseRising:chol_med_rosuvastatinYes            0.3537027  0.2923282   1.210
## phaseFalling:chol_med_addon_useYes             -1.3097482  0.6385205  -2.051
## phaseRising:chol_med_addon_useYes               0.6168484  0.3103741   1.987
## phaseFalling:cc_diabetesYes                     0.2653909  0.1312386   2.022
## phaseRising:cc_diabetesYes                      0.6337954  0.1030952   6.148
##                                                Pr(>|t|)    
## (Intercept)                                     < 2e-16 ***
## chol_trig:phaseFalling                         0.005331 ** 
## chol_trig:phaseRising                          0.021929 *  
## phaseFalling:LBDSPH                            0.002349 ** 
## phaseRising:LBDSPH                             0.083900 .  
## phaseFalling:LBDSCR                            0.265043    
## phaseRising:LBDSCR                             0.479015    
## phaseFalling:cc_egfr_lt60Yes                   0.002325 ** 
## phaseRising:cc_egfr_lt60Yes                    0.001831 ** 
## phaseFalling:cc_acr_gteq30Yes                  1.61e-05 ***
## phaseRising:cc_acr_gteq30Yes                   0.000564 ***
## phaseFalling:demo_age_cat45 to 64              1.37e-07 ***
## phaseRising:demo_age_cat45 to 64               4.91e-15 ***
## phaseFalling:demo_age_cat65 to 74              7.19e-07 ***
## phaseRising:demo_age_cat65 to 74               7.81e-15 ***
## phaseFalling:demo_age_cat75+                   0.031774 *  
## phaseRising:demo_age_cat75+                    3.39e-06 ***
## phaseFalling:demo_raceNon-Hispanic Asian       0.239347    
## phaseRising:demo_raceNon-Hispanic Asian        0.843062    
## phaseFalling:demo_raceNon-Hispanic Black       0.515595    
## phaseRising:demo_raceNon-Hispanic Black        0.006644 ** 
## phaseFalling:demo_raceNon-Hispanic White       0.002208 ** 
## phaseRising:demo_raceNon-Hispanic White        4.31e-06 ***
## phaseFalling:demo_raceOther                    0.026602 *  
## phaseRising:demo_raceOther                     0.101739    
## phaseFalling:htn_resistant_jnc7Yes             0.001547 ** 
## phaseRising:htn_resistant_jnc7Yes              0.000201 ***
## phaseFalling:htn_resistant_jnc7_thzYes         0.591880    
## phaseRising:htn_resistant_jnc7_thzYes          0.071399 .  
## phaseFalling:chol_nonhdl_5cat>= 220 mg/dL      0.000676 ***
## phaseRising:chol_nonhdl_5cat>= 220 mg/dL       7.29e-08 ***
## phaseFalling:chol_nonhdl_5cat100 to <130 mg/dL 0.024901 *  
## phaseRising:chol_nonhdl_5cat100 to <130 mg/dL  0.010423 *  
## phaseFalling:chol_nonhdl_5cat130 to <160 mg/dL 3.80e-06 ***
## phaseRising:chol_nonhdl_5cat130 to <160 mg/dL  0.001601 ** 
## phaseFalling:chol_nonhdl_5cat160 to <220 mg/dL 8.00e-06 ***
## phaseRising:chol_nonhdl_5cat160 to <220 mg/dL  3.75e-08 ***
## phaseFalling:chol_med_rosuvastatinYes          0.001875 ** 
## phaseRising:chol_med_rosuvastatinYes           0.228758    
## phaseFalling:chol_med_addon_useYes             0.042497 *  
## phaseRising:chol_med_addon_useYes              0.049230 *  
## phaseFalling:cc_diabetesYes                    0.045456 *  
## phaseRising:cc_diabetesYes                     1.15e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1.020193)
## 
## Number of Fisher Scoring iterations: 5
```

``` r
# save summary
summary_df_svy <- summary(fit_svy)$coefficients
summary_df_svy <- as.data.frame(summary_df_svy)
summary_df_svy$Variable <- rownames(summary_df_svy)
rownames(summary_df_svy) <- NULL
summary_df_svy <- summary_df_svy[, c("Variable", "Estimate", "Std. Error", "t value", "Pr(>|t|)")]

write.csv(summary_df_svy, "logistic_results_svyglm_interactions.csv", row.names = FALSE)
cat("Survey-weighted logistic summary saved as logistic_results_svyglm_interactions.csv\n")
```

```
## Survey-weighted logistic summary saved as logistic_results_svyglm_interactions.csv
```

``` r
# generate a table
signif_results <- summary_df_svy %>%
  filter(Variable != "(Intercept)", `Pr(>|t|)` <= 0.05) %>%
  arrange(`Pr(>|t|)`) %>%
  slice_head(n = 15)

table_with <- signif_results %>%
  gt() %>%
  fmt_number(columns = c("Estimate", "Std. Error", "t value", "Pr(>|t|)"),
             decimals = 4) %>%
  tab_header(
    title = "Significant Variables Selected from Subset with Cholesterol Data",
    subtitle = "Variables with p ≤ 0.05"
  )

gtsave(table_with, filename = "significant_vars_table.png")
```

```
## file:////var/folders/h3/hy3fs6yx0nn_hbtlbz0sc49m0000gn/T//Rtmp2cM03S/file8d672d706e8.html screenshot completed
```

## trend plot


``` r
df <- prep_chol_imputed %>%
  inner_join(prep_chol %>% select(Begin_Year, svy_id), by = "svy_id")

continuous_vars <- c("chol_trig", "LBDSPH", "LBDSCR")

categorical_vars <- c("cc_egfr_lt60", "cc_acr_gteq30", "demo_age_cat", "demo_race", "htn_resistant_jnc7", "htn_resistant_jnc7_thz", "chol_nonhdl_5cat", "chol_med_rosuvastatin", "chol_med_addon_use", "cc_diabetes")

# Create a save directory
output_dir <- "trend_plots_by_year"
dir.create(output_dir, showWarnings = FALSE)

# Continuous variables: mean for each year
for (var in continuous_vars) {
  p <- df %>%
    group_by(Begin_Year) %>%
    summarise(mean_val = mean(.data[[var]], na.rm = TRUE)) %>%
    ggplot(aes(x = Begin_Year, y = mean_val)) +
    geom_line(color = "black") +
    geom_point() +
    geom_vline(xintercept = 2013, linetype = "dashed", color = "red") +
    labs(title = paste("Trend of", var), y = "Mean", x = "Begin Year") +
    theme_minimal()

  ggsave(file.path(output_dir, paste0("trend_", var, ".png")), plot = p, width = 6, height = 4)
}

# Categorical variables: the proportion of each level in each year
for (var in categorical_vars) {
  df[[var]] <- as.factor(df[[var]])
  
  if (var %in% c("demo_age_cat", "chol_nonhdl_5cat", "demo_race")) {
    levels_list <- levels(df[[var]])
    
    for (lvl in levels_list) {
      trend_df <- df %>%
        filter(!is.na(.data[[var]])) %>%
        mutate(level_match = (.data[[var]] == lvl)) %>%
        group_by(Begin_Year) %>%
        summarise(prop = mean(level_match, na.rm = TRUE))
      
      p <- ggplot(trend_df, aes(x = Begin_Year, y = prop)) +
        geom_line(color = "black") +
        geom_point() +
        geom_vline(xintercept = 2013, linetype = "dashed", color = "red") +
        labs(title = paste("Proportion of", lvl, "in", var), y = "Proportion", x = "Begin Year") +
        theme_minimal()
      
      filename <- paste0("trend_", var, "_", gsub("[^a-zA-Z0-9]", "_", lvl), ".png")
      ggsave(file.path(output_dir, filename), plot = p, width = 6, height = 4)
    }
    
  } else {
    p <- df %>%
      filter(!is.na(.data[[var]])) %>%
      group_by(Begin_Year, !!sym(var)) %>%
      summarise(n = n(), .groups = "drop") %>%
      group_by(Begin_Year) %>%
      mutate(prop = n / sum(n)) %>%
      ggplot(aes(x = Begin_Year, y = prop, color = !!sym(var))) +
      geom_line() +
      geom_point() +
      geom_vline(xintercept = 2013, linetype = "dashed", color = "red") +
      labs(title = paste("Proportion of", var, "by Year"), y = "Proportion", x = "Begin Year") +
      theme_minimal()
    
    ggsave(file.path(output_dir, paste0("trend_", var, ".png")), plot = p, width = 6, height = 4)
  }  
}
```


# prep_no_chol

## unit conversions pair and keep SI only


``` r
get_unit_conversion_pairs <- function(data) {
  var_names <- names(data)
  lbx_vars <- grep("^LBX", var_names, value = TRUE)
  lbd_vars <- grep("^LBD", var_names, value = TRUE)
  
  get_core <- function(var) {
    core <- sub("^LBX", "", var)
    core <- sub("_[A-Za-z0-9]+$", "", core)
    return(core)
  }
  
  matched_pairs <- list()
  
  for (lbx in lbx_vars) {
    core <- get_core(lbx)
    matched <- grep(paste0("^LBD", core), lbd_vars, value = TRUE)
    if (length(matched) > 0) {
      matched_pairs[[length(matched_pairs) + 1]] <- c(lbx, matched[1])
    }
  }
  
  # output a data.frame
  matched_df <- do.call(rbind, matched_pairs)
  colnames(matched_df) <- c("LBX_variable", "LBD_variable")
  return(as.data.frame(matched_df, stringsAsFactors = FALSE))
}

matched_unit_pairs_no <- get_unit_conversion_pairs(prep_no_chol)
View(matched_unit_pairs_no) 
```


``` r
cor_results <- data.frame(
  LBX_variable = character(),
  LBD_variable = character(),
  correlation = numeric(),
  stringsAsFactors = FALSE
)

# calculate correlation coefficient
for (i in 1:nrow(matched_unit_pairs)) {
  lbx <- matched_unit_pairs$LBX_variable[i]
  lbd <- matched_unit_pairs$LBD_variable[i]
  
  if (all(c(lbx, lbd) %in% names(prep))) {
    r <- cor(prep[[lbx]], prep[[lbd]], use = "pairwise.complete.obs")
    cor_results <- rbind(cor_results, data.frame(
      LBX_variable = lbx,
      LBD_variable = lbd,
      correlation = round(r, 6),
      stringsAsFactors = FALSE
    ))
  } else {
    warning(paste("Missing variable in prep:", lbx, "or", lbd))
  }
}

cor_results
```

```
##   LBX_variable LBD_variable correlation
## 1       LBXSTP     LBDSTPSI           1
## 2     LBXSTR_x     LBDSTRSI           1
## 3       LBXSUA     LBDSUASI           1
## 4       LBXSGB     LBDSGBSI           1
## 5     LBXSTR_y     LBDSTRSI           1
```

### keep SI only_no


``` r
prep_no_chol <- prep_no_chol %>% select(-LBXSTP, -LBXSTR_x, -LBXSUA, -LBXSGB, -LBXSTR_y) # 15639, 156
```

## collinear >= 0.95 variables 

The variables (or variable with levels) LBDSTPSI, LBDSGBSI, SDDSRVYR, LBXGH, LBXSTR_y are perfectly collinear with another variable in the data.


``` r
# Step 1: Select all continuous variables (numeric) in prep
numeric_vars_no <- prep_no_chol[, sapply(prep_no_chol, is.numeric)]

# Step 2: Compute the correlation matrix between variables (ignoring missing values)
cor_matrix_no <- cor(numeric_vars_no, use = "pairwise.complete.obs")
```

```
## Warning in cor(numeric_vars_no, use = "pairwise.complete.obs"): the standard
## deviation is zero
```

``` r
# Step 3: Find pairs of variables with |correlation coefficient| >= 0.95 
collinear_pairs_no <- which(abs(cor_matrix_no) >= 0.95 & lower.tri(cor_matrix_no), arr.ind = TRUE)

# Step 4: Display completely collinear pairs of variable names
collinear_vars_no <- data.frame(
  var1 = rownames(cor_matrix_no)[collinear_pairs_no[, 1]],
  var2 = colnames(cor_matrix_no)[collinear_pairs_no[, 2]],
  r = cor_matrix_no[collinear_pairs_no]
)

print(collinear_vars_no)
```

```
##             var1           var2          r
## 1     svy_strata         svy_id  0.9902528
## 2           YEAR         svy_id  0.9939171
## 3    Survey_Year         svy_id  0.9844496
## 4       SDDSRVYR         svy_id  0.9919263
## 5       SDMVSTRA         svy_id  0.9878997
## 6     Begin_Year         svy_id  0.9844496
## 7       WTINT2YR svy_weight_mec  0.9971019
## 8       WTMEC2YR svy_weight_mec  1.0000000
## 9        SDMVPSU        svy_psu  1.0000000
## 10          YEAR     svy_strata  0.9941146
## 11   Survey_Year     svy_strata  0.9884949
## 12      SDDSRVYR     svy_strata  0.9920955
## 13      SDMVSTRA     svy_strata  1.0000000
## 14    Begin_Year     svy_strata  0.9884949
## 15      RIDAGEYR demo_age_years  0.9989441
## 16        BPXSY1    bp_sys_mean  0.9792641
## 17        BPXSY2    bp_sys_mean  0.9848387
## 18        BPXSY3    bp_sys_mean  0.9796386
## 19      chol_ldl     chol_total  0.9745387
## 20        LBXSCH     chol_total  0.9858922
## 21         LBXTC     chol_total  1.0000000
## 22        LBDHDL     chol_total -1.0000000
## 23        LBDLDL     chol_total  0.9729515
## 24 FriedewaldLDL     chol_total  1.0000000
## 25 ldl_corrected     chol_total  0.9745387
## 26        LBDHDL       chol_hdl  1.0000000
## 27 FriedewaldLDL       chol_hdl -1.0000000
## 28      LBDSTRSI      chol_trig  0.9657512
## 29        LBDHDL      chol_trig -1.0000000
## 30 FriedewaldLDL      chol_trig  1.0000000
## 31        LBXSCH       chol_ldl  0.9693725
## 32         LBXTC       chol_ldl  0.9745387
## 33        LBDHDL       chol_ldl -1.0000000
## 34        LBDLDL       chol_ldl  0.9995627
## 35 FriedewaldLDL       chol_ldl  1.0000000
## 36 ldl_corrected       chol_ldl  1.0000000
## 37        LBDHDL    chol_nonhdl -1.0000000
## 38        LBDLDL    chol_nonhdl  0.9637045
## 39 FriedewaldLDL    chol_nonhdl  1.0000000
## 40         LBXGH       cc_hba1c  1.0000000
## 41   Survey_Year           YEAR  1.0000000
## 42      SDDSRVYR           YEAR  1.0000000
## 43      SDMVSTRA           YEAR  0.9941146
## 44    Begin_Year           YEAR  1.0000000
## 45         LBXTC         LBXSCH  0.9850925
## 46        BPXSY3         BPXSY2  0.9520424
## 47        LBXHCT         LBXHGB  0.9637120
## 48      SDDSRVYR    Survey_Year  1.0000000
## 49      SDMVSTRA    Survey_Year  0.9941146
## 50    Begin_Year    Survey_Year  1.0000000
## 51      SDMVSTRA       SDDSRVYR  0.9920955
## 52    Begin_Year       SDDSRVYR  1.0000000
## 53      WTMEC2YR       WTINT2YR  0.9971019
## 54    Begin_Year       SDMVSTRA  0.9941146
## 55        LBDLDL         LBDHDL -1.0000000
## 56 FriedewaldLDL         LBDLDL  1.0000000
## 57 ldl_corrected         LBDLDL  0.9999847
## 58 ldl_corrected  FriedewaldLDL  0.9999986
```

``` r
write.csv(collinear_vars_no, "collinear_vars_0.95_prep_no_chol.csv", row.names = FALSE)
```

## delete variables collinear >= 0.95


``` r
# delete collinear >= 0.95 variables 
prep_no_chol_1 <- prep_no_chol %>% select(-svy_weight_mec, -svy_strata, -svy_psu, -Begin_Year, -Survey_Year, -YEAR, -SDDSRVYR, -SDMVSTRA, -WTMEC2YR, -WTINT2YR, -SDMVPSU, -RIDAGEYR, -BPXSY1, -BPXSY2, -BPXSY3, -bp_sys_mean, -LBXSCH, -chol_total, -chol_hdl, -LBDLDL, -chol_ldl, -FriedewaldLDL, -chol_trig, -LBXGH, -LBXHGB) 

# delete other BP variables
prep_no_chol_1 <- prep_no_chol_1 %>% select(-BPXDI1, -BPXDI2, -BPXDI3, -bp_dia_mean) # 15639, 127

# prep_no_chol[, c("LBXSCH", "LBXTC")]
```

## delete variables that do not vary


``` r
constant_vars_no <- names(prep_no_chol_1)[sapply(prep_no_chol_1, function(col) {
  length(unique(na.omit(col))) == 1
})]

print(constant_vars_no)  # 10 variables
```

```
##  [1] "svy_subpop_chol"       "htn_accaha"            "chol_ldl_gteq_190"    
##  [4] "chol_ldl_persistent"   "chol_nonhdl_gteq_220"  "chol_med_pcsk9i"      
##  [7] "chol_med_bile"         "chol_med_pitavastatin" "chol_med_fluvastatin" 
## [10] "RIDSTATR"
```

``` r
#      svy_subpop_chol            htn_accaha 
#                  "0"                 "Yes" 
#    chol_ldl_gteq_190   chol_ldl_persistent 
#                 "No"                  "No" 
# chol_nonhdl_gteq_220       chol_med_pcsk9i 
#                 "No"                  "No" 
#        chol_med_bile chol_med_pitavastatin 
#                 "No"                  "No" 
# chol_med_fluvastatin              RIDSTATR 
#                 "No"                   "2" 

sapply(prep_no_chol_1[constant_vars_no], function(x) unique(na.omit(x)))
```

```
##       svy_subpop_chol            htn_accaha     chol_ldl_gteq_190 
##                   "0"                 "Yes"                  "No" 
##   chol_ldl_persistent  chol_nonhdl_gteq_220       chol_med_pcsk9i 
##                  "No"                  "No"                  "No" 
##         chol_med_bile chol_med_pitavastatin  chol_med_fluvastatin 
##                  "No"                  "No"                  "No" 
##              RIDSTATR 
##                   "2"
```

``` r
write.csv(data.frame(Constant_Variables = constant_vars_no),
          "constant_vars_removed_prep_no_chol.csv", row.names = FALSE)

prep_no_chol_1 <- prep_no_chol_1[, !(names(prep_no_chol_1) %in% constant_vars_no)] # 15639, 117
```

## delete variables that barely change (e.g., a value that accounts for 99% of all variables)


``` r
# set threshold
dominant_threshold <- 0.99

# filter variables that barely change
low_variability_vars_no <- names(prep_no_chol_1)[sapply(prep_no_chol_1, function(col) {
  non_missing <- na.omit(col)
  if (length(non_missing) == 0) return(FALSE)
  max_prop <- max(table(non_missing)) / length(non_missing)
  return(max_prop >= dominant_threshold)
})]

print(low_variability_vars_no)
```

```
## [1] "BPAARM"
```

``` r
# View the dominant values and proportions of these variables
sapply(prep_no_chol_1[low_variability_vars_no], function(x) {
  non_missing <- na.omit(x)
  top_val <- names(sort(table(non_missing), decreasing = TRUE))[1]
  top_prop <- max(table(non_missing)) / length(non_missing)
  paste0("Most frequent value: ", top_val, " (", round(top_prop * 100, 2), "%)")
})
```

```
##                            BPAARM 
## "Most frequent value: 1 (99.08%)"
```

``` r
# delete 1 variable
prep_no_chol_1 <- prep_no_chol_1 %>% select(-BPAARM) # 15639, 116

#                            BPAARM 
# "Most frequent value: 1 (99.08%)" 
```

## missing value


``` r
# Step 1: calculate the ratio
na_ratios_no <- sapply(prep_no_chol_1, function(col) mean(is.na(col)))
no_missing_cols_no <- names(na_ratios_no[na_ratios_no == 0])  # columns don't have NA
prep_no_chol_fixed <- prep_no_chol_1[, no_missing_cols_no] # 15

na_ratios_no <- na_ratios_no[na_ratios_no > 0]  # 101

# bins
bins <- cut(na_ratios_no, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE, right = FALSE)
bin_counts <- table(bins)

# plot
ggplot(data.frame(bin = names(bin_counts), count = as.vector(bin_counts)), aes(x = bin, y = count)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Missing Value Proportion Histogram for prep_no_chol",
       x = "Missing Rate Bin", y = "Number of Columns") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](Final_Project_XianjieQue_BDM_files/figure-html/missing value no-1.png)<!-- -->

``` r
# Step 2: remove and retain columns
threshold <- 0.5
keep_cols_no <- names(na_ratios_no[na_ratios_no <= threshold]) # 68
drop_cols_no <- names(na_ratios_no[na_ratios_no > threshold]) # 33

kept_df_no <- data.frame(column = keep_cols_no, missing_rate = na_ratios_no[keep_cols_no])
dropped_df_no <- data.frame(column = drop_cols_no, missing_rate = na_ratios_no[drop_cols_no])

# save columns to csv
write.csv(kept_df_no, "kept_columns_prep_no_chol.csv", row.names = FALSE) # 68
write.csv(dropped_df_no, "dropped_columns_prep_no_chol.csv", row.names = FALSE) # 33

# Step 3: check continuous columns and categorical variables
prep_no_chol_kept <- prep_no_chol_1[, keep_cols_no] # 68

num_continuous_no <- sum(sapply(prep_no_chol_kept, is.numeric))
num_categorical_no <- sum(sapply(prep_no_chol_kept, function(col) is.factor(col) || is.character(col)))

cat("Number of continuous variables: ", num_continuous_no, "\n") # 61
```

```
## Number of continuous variables:  61
```

``` r
cat("Number of categorical variables: ", num_categorical_no, "\n") # 7
```

```
## Number of categorical variables:  7
```

## imputation NA


``` r
# Step 1: split continuous and categorical
prep_no_cont <- prep_no_chol_kept[, sapply(prep_no_chol_kept, is.numeric)]
prep_no_cat  <- prep_no_chol_kept[, sapply(prep_no_chol_kept, function(x) is.factor(x) || is.character(x))]

# Step 2: EM imputation for continuous variables
amelia_result_no <- amelia(prep_no_cont, m = 1)  # advanced method: m = 5
```

```
## -- Imputation 1 --
## 
##   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
##  21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
##  41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60
##  61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80
##  81 82 83 84 85 86 87 88 89 90
```

``` r
cont_imputed_no <- amelia_result_no$imputations[[1]]

# Step 3: KNN imputation for categorical variables
cat_imputed_no <- kNN(prep_no_cat, k = 5, imp_var = FALSE)

# Step 4: combine
prep_no_chol_imputed <- cbind(cont_imputed_no, cat_imputed_no, prep_no_chol_fixed) # 15639, 83
```

## collinear > 0.8 before Lasso and Logistics


``` r
# Step 1: Select all continuous variables (numeric) in prep_chol
numeric_vars_no <- prep_no_chol_imputed[, sapply(prep_no_chol_imputed, is.numeric)]

# Step 2: Compute the correlation matrix between variables (ignoring missing values)
cor_matrix_no <- cor(numeric_vars_no, use = "pairwise.complete.obs")

# Step 3: Find pairs of variables with |correlation coefficient| >= 0.8 
collinear_pairs_no <- which(abs(cor_matrix_no) >= 0.8 & lower.tri(cor_matrix_no), arr.ind = TRUE)

# Step 4: Display completely collinear pairs of variable names
collinear_vars_no <- data.frame(
  var1 = rownames(cor_matrix_no)[collinear_pairs_no[, 1]],
  var2 = colnames(cor_matrix_no)[collinear_pairs_no[, 2]],
  r = cor_matrix_no[collinear_pairs_no]
)

print(collinear_vars_no)
```

```
##        var1     var2          r
## 1  LBXSASSI LBXSATSI  0.8206713
## 2    BMXBMI    BMXWT  0.8709388
## 3   BMXARMC    BMXWT  0.8835169
## 4  BMXWAIST    BMXWT  0.8902057
## 5   BMXARMC   BMXBMI  0.8737932
## 6  BMXWAIST   BMXBMI  0.8923860
## 7  BMXWAIST  BMXARMC  0.8051673
## 8    BPACSZ  BMXARMC  0.8374339
## 9  LBXNEPCT LBXLYPCT -0.9380969
## 10  LBDEONO LBXEOPCT  0.8847718
## 11   LBXHCT LBXRBCSI  0.8178142
```

``` r
write.csv(collinear_vars_no, "collinear_vars_0.8_prep_no_chol.csv", row.names = FALSE)
```

## delete variables collinear >= 0.8


``` r
# delete collinear >= 0.8 variables 
prep_no_chol_imputed <- prep_no_chol_imputed %>% select(-LBXSATSI, -BMXWT, -BMXARMC, -BPACSZ, -LBXLYPCT, -LBDEONO, -LBXRBCSI)

# keep: LBXSASSI, BMXBMI, BMXWAIST, LBXNEPCT, LBXEOPCT, LBXHCT

# head(prep_no_chol_imputed[, c("LBXSCH", "LBXTC")], 20)

sum(is.na(prep_no_chol_imputed)) # 15639, 76
```

```
## [1] 0
```

## Lasso to full dataset with interaction


``` r
prep_no_chol_imputed_2 <- prep_no_chol_imputed %>% select(-svy_id)  # 15639, 75

prep_no_chol_imputed_2$phase <- relevel(factor(prep_no_chol_imputed_2$phase), ref = "Rising")

# LASSO pipeline with interactions (excluding "phase" main effect)
run_lasso_with_interaction <- function(data,
                                     response_var = "bp_control_accaha",
                                     save_dir = ".",
                                     dataset_name = "prep_no_chol_full") {
  set.seed(123)

  # Ensure phase is factor with correct reference level
  data$phase <- relevel(factor(data$phase), ref = "Rising")

  # Step 1: Prepare predictors (exclude response and phase)
  predictors <- setdiff(names(data), c(response_var, "phase"))

  # Step 2: Construct formula with interactions
  main_effects <- paste(predictors, collapse = " + ")
  interaction_terms <- paste(paste0(predictors, ":phase"), collapse = " + ")
  formula_string <- paste(response_var, "~", main_effects, "+", interaction_terms)

  # Step 3: Create full design matrix
  design_matrix <- model.matrix(as.formula(formula_string), data = data)[, -1]
  response <- data[[response_var]]

  # Step 4: Train/test split
  train_idx <- sample(1:nrow(data), 0.7 * nrow(data))
  x_train <- design_matrix[train_idx, ]
  y_train <- response[train_idx]
  x_test <- design_matrix[-train_idx, ]
  y_test <- response[-train_idx]

  # Step 5: Cross-validation with fixed folds
  foldid <- sample(rep(1:10, length.out = length(y_train)))
  cvfit <- cv.glmnet(x = x_train, y = y_train, family = "binomial", alpha = 1, foldid = foldid)
  best_lambda <- cvfit$lambda.min

  # Step 6: Predict on test set
  pred_prob <- as.vector(predict(cvfit, newx = x_test, s = best_lambda, type = "response"))
  pred_class <- ifelse(pred_prob > 0.5, 1, 0)
  acc <- mean(pred_class == y_test)
  auc_val <- auc(roc(y_test, pred_prob))

  # Step 7: Refit on full data using best lambda
  final_model <- glmnet(x = design_matrix, y = response, family = "binomial", alpha = 1, lambda = best_lambda)
  coef_final <- coef(final_model)
  selected_vars <- rownames(coef_final)[which(coef_final != 0)]
  selected_vars <- selected_vars[selected_vars != "(Intercept)"]

  # Step 8: Save results
  results_df <- data.frame(variable = selected_vars)
  summary_df <- data.frame(metric = c("accuracy", "auc"), value = c(acc, auc_val))

  write.csv(results_df, file = file.path(save_dir, paste0("lasso_selected_", dataset_name, ".csv")), row.names = FALSE)
  write.csv(summary_df, file = file.path(save_dir, paste0("lasso_summary_", dataset_name, ".csv")), row.names = FALSE)

  # Step 9: Save plots
  png(filename = file.path(save_dir, paste0("cv_curve_", dataset_name, ".png")), width = 800, height = 600)
  plot(cvfit, main = paste("LASSO CV -", dataset_name))
  dev.off()

  png(filename = file.path(save_dir, paste0("auc_curve_", dataset_name, ".png")), width = 800, height = 600)
  plot(roc(y_test, pred_prob), main = paste("AUC ROC -", dataset_name), col = "blue", lwd = 2)
  dev.off()

  # Step 10: Return
  return(list(model = final_model, selected = selected_vars, accuracy = acc, auc = auc_val))
}

result_lasso_no <- run_lasso_with_interaction(data = prep_no_chol_imputed_2)
```

```
## Setting levels: control = 0, case = 1
```

```
## Setting direction: controls < cases
```

```
## Setting levels: control = 0, case = 1
```

```
## Setting direction: controls < cases
```

## Build Logistic Regression Formula


``` r
# Step 1: extract interaction terms
lasso_selected_vars_no <- result_lasso_no[["selected"]]
interaction_vars_no <- grep(":phase", lasso_selected_vars_no, value = TRUE)
cat("interaction terms from Lasso: \n")
```

```
## interaction terms from Lasso:
```

``` r
print(interaction_vars_no)
```

```
##  [1] "cc_acr:phaseFalling"                     
##  [2] "URXUMA:phaseFalling"                     
##  [3] "URXUCR:phaseFalling"                     
##  [4] "LBXSGTSI:phaseFalling"                   
##  [5] "LBXSIR:phaseFalling"                     
##  [6] "LBDSTRSI:phaseFalling"                   
##  [7] "LBDMONO:phaseFalling"                    
##  [8] "LBDBANO:phaseFalling"                    
##  [9] "LBXPLTSI:phaseFalling"                   
## [10] "RIDEXMON:phaseFalling"                   
## [11] "INDFMPIR:phaseFalling"                   
## [12] "cc_smokeNever:phaseFalling"              
## [13] "cc_cvd_miYes:phaseFalling"               
## [14] "cc_cvd_chdYes:phaseFalling"              
## [15] "cc_cvd_hfYes:phaseFalling"               
## [16] "demo_age_cat45 to 64:phaseFalling"       
## [17] "demo_age_cat65 to 74:phaseFalling"       
## [18] "demo_raceNon-Hispanic Asian:phaseFalling"
## [19] "demo_raceNon-Hispanic Black:phaseFalling"
## [20] "demo_raceNon-Hispanic White:phaseFalling"
## [21] "demo_raceOther:phaseFalling"             
## [22] "demo_genderWomen:phaseFalling"           
## [23] "htn_resistant_accaha_thzYes:phaseFalling"
## [24] "cc_diabetesYes:phaseFalling"             
## [25] "cc_ckdYes:phaseFalling"                  
## [26] "cc_cvd_ascvdYes:phaseFalling"            
## [27] "cc_cvd_anyYes:phaseFalling"
```

``` r
# Step 2: build logistic regression formula
# main_effects <- unique(sub(":phase.*", "", interaction_vars))

main_effects_no <- c("cc_acr", "URXUMA", "LBXSGTSI", "LBXSIR", "LBXPLTSI", "LBDSCR", "cc_cvd_mi", "cc_cvd_chd", "cc_cvd_hf", "demo_age_cat", "demo_race", "demo_gender", "htn_resistant_accaha_thz", "cc_diabetes", "cc_ckd", "cc_cvd_any")

interaction_vars_2_no <- paste0(main_effects_no, ":phase")

# all_vars<- unique(c(main_effects, interaction_vars_2))
# formula_str <- paste("bp_control_accaha ~", paste(all_vars, collapse = " + "))

formula_str_no <- paste("bp_control_accaha ~", paste(interaction_vars_2_no, collapse = " + "))

formula_obj_no <- as.formula(formula_str_no)
```

## Logistic with weight

Logistics with survey weights, using svyglm(), is intended to make inferences about the population, and representativeness needs to be considered.


``` r
prep_svy_no <- prep_no_chol_imputed %>%
  left_join(prep_no_chol %>% select(svy_id, svy_strata, svy_psu, svy_weight_mec), by = "svy_id")

prep_svy_no <- prep_svy_no %>%
  filter(!is.na(svy_psu) & !is.na(svy_strata) & !is.na(svy_weight_mec))

prep_svy_no <- prep_svy_no %>%
  mutate(bp_control_accaha = as.integer(bp_control_accaha))

design_no <- svydesign(ids = ~svy_psu,
                       strata = ~svy_strata,
                       weights = ~svy_weight_mec,
                       nest = TRUE,
                       data = prep_svy_no)

fit_svy_no <- svyglm(formula_obj_no,
                     design = design_no,
                     family = binomial())
```

```
## Warning in eval(family$initialize): non-integer #successes in a binomial glm!
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

``` r
summary(fit_svy_no)
```

```
## 
## Call:
## svyglm(formula = formula_obj_no, design = design_no, family = binomial())
## 
## Survey design:
## svydesign(ids = ~svy_psu, strata = ~svy_strata, weights = ~svy_weight_mec, 
##     nest = TRUE, data = prep_svy_no)
## 
## Coefficients:
##                                            Estimate Std. Error t value Pr(>|t|)
## (Intercept)                              -3.2063089  0.1931702 -16.598  < 2e-16
## cc_acr:phaseFalling                      -0.0013242  0.0002909  -4.553 1.31e-05
## cc_acr:phaseRising                       -0.0008358  0.0002906  -2.876  0.00479
## phaseFalling:URXUMA                       0.0005221  0.0002074   2.518  0.01318
## phaseRising:URXUMA                        0.0003323  0.0001971   1.686  0.09448
## phaseFalling:LBXSGTSI                     0.0014436  0.0007301   1.977  0.05039
## phaseRising:LBXSGTSI                      0.0003422  0.0005493   0.623  0.53453
## phaseFalling:LBXSIR                      -0.0017153  0.0015773  -1.088  0.27906
## phaseRising:LBXSIR                       -0.0005083  0.0012595  -0.404  0.68727
## phaseFalling:LBXPLTSI                     0.0003935  0.0006316   0.623  0.53451
## phaseRising:LBXPLTSI                     -0.0007234  0.0005614  -1.288  0.20015
## phaseFalling:LBDSCR                       0.0061093  0.0014112   4.329 3.19e-05
## phaseRising:LBDSCR                        0.0057348  0.0009159   6.261 6.66e-09
## phaseFalling:cc_cvd_miYes                -0.2768188  0.2436998  -1.136  0.25834
## phaseRising:cc_cvd_miYes                  0.5227158  0.2001041   2.612  0.01019
## phaseFalling:cc_cvd_chdYes                0.5651137  0.2708444   2.086  0.03913
## phaseRising:cc_cvd_chdYes                -0.2309343  0.1947823  -1.186  0.23820
## phaseFalling:cc_cvd_hfYes                 0.4667092  0.2187660   2.133  0.03500
## phaseRising:cc_cvd_hfYes                  0.3635952  0.1655090   2.197  0.03002
## phaseFalling:demo_age_cat45 to 64         0.9288119  0.1607444   5.778 6.46e-08
## phaseRising:demo_age_cat45 to 64          1.0695198  0.1228995   8.702 2.55e-14
## phaseFalling:demo_age_cat65 to 74         1.1057757  0.1743086   6.344 4.48e-09
## phaseRising:demo_age_cat65 to 74          1.4092125  0.1494653   9.428 5.19e-16
## phaseFalling:demo_age_cat75+              0.6491812  0.1943001   3.341  0.00112
## phaseRising:demo_age_cat75+               0.8890176  0.1359669   6.538 1.74e-09
## phaseFalling:demo_raceNon-Hispanic Asian -0.2690360  0.1419974  -1.895  0.06063
## phaseRising:demo_raceNon-Hispanic Asian   0.2122025  0.2544342   0.834  0.40599
## phaseFalling:demo_raceNon-Hispanic Black  0.1303997  0.1351901   0.965  0.33677
## phaseRising:demo_raceNon-Hispanic Black   0.0966646  0.1015892   0.952  0.34332
## phaseFalling:demo_raceNon-Hispanic White  0.2971233  0.1309402   2.269  0.02511
## phaseRising:demo_raceNon-Hispanic White   0.1094866  0.0946127   1.157  0.24956
## phaseFalling:demo_raceOther               0.0232250  0.1998119   0.116  0.90767
## phaseRising:demo_raceOther               -0.5264633  0.2352967  -2.237  0.02717
## phaseFalling:demo_genderWomen             0.3289787  0.1045213   3.147  0.00209
## phaseRising:demo_genderWomen              0.3793539  0.0883531   4.294 3.67e-05
## phaseFalling:htn_resistant_accaha_thzYes -0.5781987  0.2548163  -2.269  0.02511
## phaseRising:htn_resistant_accaha_thzYes  -0.2373225  0.1776497  -1.336  0.18420
## phaseFalling:cc_diabetesYes               0.7818338  0.1322937   5.910 3.51e-08
## phaseRising:cc_diabetesYes                0.6984601  0.1010738   6.910 2.78e-10
## phaseFalling:cc_ckdYes                   -0.0868314  0.1178387  -0.737  0.46269
## phaseRising:cc_ckdYes                    -0.2397898  0.0888663  -2.698  0.00801
## phaseFalling:cc_cvd_anyYes                0.3274040  0.2206992   1.483  0.14066
## phaseRising:cc_cvd_anyYes                 0.2191020  0.1337310   1.638  0.10405
##                                             
## (Intercept)                              ***
## cc_acr:phaseFalling                      ***
## cc_acr:phaseRising                       ** 
## phaseFalling:URXUMA                      *  
## phaseRising:URXUMA                       .  
## phaseFalling:LBXSGTSI                    .  
## phaseRising:LBXSGTSI                        
## phaseFalling:LBXSIR                         
## phaseRising:LBXSIR                          
## phaseFalling:LBXPLTSI                       
## phaseRising:LBXPLTSI                        
## phaseFalling:LBDSCR                      ***
## phaseRising:LBDSCR                       ***
## phaseFalling:cc_cvd_miYes                   
## phaseRising:cc_cvd_miYes                 *  
## phaseFalling:cc_cvd_chdYes               *  
## phaseRising:cc_cvd_chdYes                   
## phaseFalling:cc_cvd_hfYes                *  
## phaseRising:cc_cvd_hfYes                 *  
## phaseFalling:demo_age_cat45 to 64        ***
## phaseRising:demo_age_cat45 to 64         ***
## phaseFalling:demo_age_cat65 to 74        ***
## phaseRising:demo_age_cat65 to 74         ***
## phaseFalling:demo_age_cat75+             ** 
## phaseRising:demo_age_cat75+              ***
## phaseFalling:demo_raceNon-Hispanic Asian .  
## phaseRising:demo_raceNon-Hispanic Asian     
## phaseFalling:demo_raceNon-Hispanic Black    
## phaseRising:demo_raceNon-Hispanic Black     
## phaseFalling:demo_raceNon-Hispanic White *  
## phaseRising:demo_raceNon-Hispanic White     
## phaseFalling:demo_raceOther                 
## phaseRising:demo_raceOther               *  
## phaseFalling:demo_genderWomen            ** 
## phaseRising:demo_genderWomen             ***
## phaseFalling:htn_resistant_accaha_thzYes *  
## phaseRising:htn_resistant_accaha_thzYes     
## phaseFalling:cc_diabetesYes              ***
## phaseRising:cc_diabetesYes               ***
## phaseFalling:cc_ckdYes                      
## phaseRising:cc_ckdYes                    ** 
## phaseFalling:cc_cvd_anyYes                  
## phaseRising:cc_cvd_anyYes                   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 0.9820799)
## 
## Number of Fisher Scoring iterations: 6
```

``` r
# save summary
summary_df_svy_no <- summary(fit_svy_no)$coefficients
summary_df_svy_no <- as.data.frame(summary_df_svy_no)
summary_df_svy_no$Variable <- rownames(summary_df_svy_no)
rownames(summary_df_svy_no) <- NULL
summary_df_svy_no <- summary_df_svy_no[, c("Variable", "Estimate", "Std. Error", "t value", "Pr(>|t|)")]

write.csv(summary_df_svy_no, "logistic_results_svyglm_interactions_no.csv", row.names = FALSE)
cat("Survey-weighted logistic summary saved as logistic_results_svyglm_interactions_no.csv\n")
```

```
## Survey-weighted logistic summary saved as logistic_results_svyglm_interactions_no.csv
```

``` r
# generate a table
signif_results_no <- summary_df_svy_no %>%
  filter(Variable != "(Intercept)", `Pr(>|t|)` <= 0.05) %>%
  arrange(`Pr(>|t|)`) %>%
  slice_head(n = 15)

table_without <- signif_results_no %>%
  gt() %>%
  fmt_number(columns = c("Estimate", "Std. Error", "t value", "Pr(>|t|)"),
             decimals = 4) %>%
  tab_header(
    title = "Significant Variables Selected from Subset with Cholesterol Data",
    subtitle = "Variables with p ≤ 0.05"
  )

gtsave(table_without, filename = "significant_vars_table_no.png")
```

```
## file:////var/folders/h3/hy3fs6yx0nn_hbtlbz0sc49m0000gn/T//Rtmp2cM03S/file8d64300a539.html screenshot completed
```

## trend plot


``` r
df_no <- prep_no_chol_imputed %>%
  inner_join(prep_no_chol %>% select(Begin_Year, svy_id), by = "svy_id")

continuous_vars_no <- c("cc_acr", "URXUMA", "LBXSGTSI", "LBXSIR", "LBXPLTSI", "LBDSCR")
categorical_vars_no <- c("cc_cvd_mi", "cc_cvd_chd", "cc_cvd_hf", "demo_age_cat", "demo_race", "demo_gender", "htn_resistant_accaha_thz", "cc_diabetes", "cc_ckd", "cc_cvd_any")

# Create a save directory
output_dir_no <- "trend_plots_by_year_no_chol"
dir.create(output_dir_no, showWarnings = FALSE)

# Continuous variables: mean for each year
for (var in continuous_vars_no) {
  p <- df_no %>%
    group_by(Begin_Year) %>%
    summarise(mean_val = mean(.data[[var]], na.rm = TRUE)) %>%
    ggplot(aes(x = Begin_Year, y = mean_val)) +
    geom_line(color = "black") +
    geom_point() +
    geom_vline(xintercept = 2013, linetype = "dashed", color = "red") +
    labs(title = paste("Trend of", var), y = "Mean", x = "Begin Year") +
    theme_minimal()

  ggsave(file.path(output_dir_no, paste0("trend_", var, ".png")), plot = p, width = 6, height = 4)
}

# Categorical variables: the proportion of each level in each year
for (var in categorical_vars_no) {
  df_no[[var]] <- as.factor(df_no[[var]])
  
  if (var %in% c("demo_age_cat", "demo_race")) {
    levels_list <- levels(df_no[[var]])
    
    for (lvl in levels_list) {
      trend_df <- df_no %>%
        filter(!is.na(.data[[var]])) %>%
        mutate(level_match = (.data[[var]] == lvl)) %>%
        group_by(Begin_Year) %>%
        summarise(prop = mean(level_match, na.rm = TRUE))
      
      p <- ggplot(trend_df, aes(x = Begin_Year, y = prop)) +
        geom_line(color = "black") +
        geom_point() +
        geom_vline(xintercept = 2013, linetype = "dashed", color = "red") +
        labs(title = paste("Proportion of", lvl, "in", var), y = "Proportion", x = "Begin Year") +
        theme_minimal()
      
      filename <- paste0("trend_", var, "_", gsub("[^a-zA-Z0-9]", "_", lvl), ".png")
      ggsave(file.path(output_dir_no, filename), plot = p, width = 6, height = 4)
    }
    
  } else {
    p <- df_no %>%
      filter(!is.na(.data[[var]])) %>%
      group_by(Begin_Year, !!sym(var)) %>%
      summarise(n = n(), .groups = "drop") %>%
      group_by(Begin_Year) %>%
      mutate(prop = n / sum(n)) %>%
      ggplot(aes(x = Begin_Year, y = prop, color = !!sym(var))) +
      geom_line() +
      geom_point() +
      geom_vline(xintercept = 2013, linetype = "dashed", color = "red") +
      labs(title = paste("Proportion of", var, "by Year"), y = "Proportion", x = "Begin Year") +
      theme_minimal()
    
    ggsave(file.path(output_dir_no, paste0("trend_", var, ".png")), plot = p, width = 6, height = 4)
  }  
}
```



``` r
sessionInfo()
```

```
## R version 4.4.1 (2024-06-14)
## Platform: x86_64-apple-darwin20
## Running under: macOS Monterey 12.7.5
## 
## Matrix products: default
## BLAS:   /Library/Frameworks/R.framework/Versions/4.4-x86_64/Resources/lib/libRblas.0.dylib 
## LAPACK: /Library/Frameworks/R.framework/Versions/4.4-x86_64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.0
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## time zone: America/Chicago
## tzcode source: internal
## 
## attached base packages:
## [1] grid      stats     graphics  grDevices utils     datasets  methods  
## [8] base     
## 
## other attached packages:
##  [1] gt_1.0.0         rlang_1.1.5      pROC_1.18.5      glmnet_4.1-8    
##  [5] caret_7.0-1      lattice_0.22-7   ncvreg_3.15.0    VIM_6.2.2       
##  [9] colorspace_2.1-1 Amelia_1.8.3     Rcpp_1.0.14      survey_4.4-2    
## [13] survival_3.8-3   Matrix_1.7-3     ggplot2_3.5.1    dplyr_1.1.4     
## 
## loaded via a namespace (and not attached):
##  [1] DBI_1.2.3            magrittr_2.0.3       e1071_1.7-16        
##  [4] compiler_4.4.1       systemfonts_1.2.2    vctrs_0.6.5         
##  [7] reshape2_1.4.4       stringr_1.5.1        pkgconfig_2.0.3     
## [10] shape_1.4.6.1        fastmap_1.2.0        labeling_0.4.3      
## [13] promises_1.3.2       rmarkdown_2.29       prodlim_2024.06.25  
## [16] ps_1.9.0             ragg_1.3.3           purrr_1.0.4         
## [19] xfun_0.52            cachem_1.1.0         jsonlite_2.0.0      
## [22] recipes_1.2.1        later_1.4.1          parallel_4.4.1      
## [25] R6_2.6.1             bslib_0.9.0          stringi_1.8.7       
## [28] vcd_1.4-13           ranger_0.17.0        parallelly_1.43.0   
## [31] car_3.1-3            boot_1.3-31          rpart_4.1.24        
## [34] lmtest_0.9-40        lubridate_1.9.4      jquerylib_0.1.4     
## [37] iterators_1.0.14     knitr_1.50           future.apply_1.11.3 
## [40] zoo_1.8-13           splines_4.4.1        nnet_7.3-20         
## [43] timechange_0.3.0     tidyselect_1.2.1     rstudioapi_0.17.1   
## [46] abind_1.4-8          yaml_2.3.10          timeDate_4041.110   
## [49] websocket_1.4.4      codetools_0.2-20     processx_3.8.6      
## [52] listenv_0.9.1        tibble_3.2.1         plyr_1.8.9          
## [55] withr_3.0.2          evaluate_1.0.3       foreign_0.8-90      
## [58] future_1.34.0        proxy_0.4-27         xml2_1.3.8          
## [61] pillar_1.10.2        carData_3.0-5        foreach_1.5.2       
## [64] stats4_4.4.1         generics_0.1.3       sp_2.2-0            
## [67] chromote_0.5.1       munsell_0.5.1        scales_1.3.0        
## [70] laeken_0.5.3         globals_0.16.3       class_7.3-23        
## [73] glue_1.8.0           tools_4.4.1          webshot2_0.1.2      
## [76] robustbase_0.99-4-1  data.table_1.17.0    ModelMetrics_1.2.2.2
## [79] gower_1.0.2          fs_1.6.5             mitools_2.4         
## [82] ipred_0.9-15         nlme_3.1-168         Formula_1.2-5       
## [85] cli_3.6.4            textshaping_1.0.0    lava_1.8.1          
## [88] gtable_0.3.6         DEoptimR_1.1-3-1     sass_0.4.9          
## [91] digest_0.6.37        farver_2.1.2         htmltools_0.5.8.1   
## [94] lifecycle_1.0.4      hardhat_1.4.1        MASS_7.3-65
```
