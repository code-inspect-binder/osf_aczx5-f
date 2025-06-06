## ****************************************************************
## ** TERRAIN AND TRAVEL ADVICE - EXAMINATION OF ATTENTION MODEL **
## ****************************************************************
##
## Fisher, Haegeli and Mair: 
## Travel and terrain advice statements in public avalanche bulletins: 
## Which backcountry recreationists use this information, what makes 
## it useful, and how can we make it better?
## April 23, 2022


## ---- 1) PREPARATION -----

## Wipe clean
rm(list = ls())

## Necessary packages
if (!require("emmeans")) {install.packages("emmeans"); require("emmeans")}

## ---- 2) LOADING OF DATA AND INITIAL PROCESSING ----

## Load data
load("TTA_Data_FinalModels.RData")


## ---- 3) CALCULATION OF ESTIMATED MARGINAL MEANS ----

## ---- Setting reference levels
RL_BullUseType <- "D"
RL_BackgsAvTraining <- "Intro"
RL_BackgrYrsOfExp <- "6-10 yrs"
RL_BackgrDaysPerYrNum <- 3  ## 11-20 days
RL_BullCountry <- "Can"
RL_cut <- "Cons|Large"


## ---- BullUseType ----
at_but <- list(cut = RL_cut, 
               BackgrAvTraining = RL_BackgsAvTraining,
               BackgrYrsOfExp = RL_BackgrYrsOfExp,
               BackgrDaysPerYrNum = RL_BackgrDaysPerYrNum,
               BullCountry = RL_BullCountry)
emmip(polr_atten, ~BullUseType, mode = "exc.prob", at = at_but, CIs = T)

## Consecutive
emmeans(polr_atten, list(consec ~ BullUseType), mode = "exc.prob", at = at_but)
## -> Used in Fig 2

## ---- Training ----
at_train <- list(cut = RL_cut, 
                 BullUseType = RL_BullUseType,
                 BackgrYrsOfExp = RL_BackgrYrsOfExp,
                 BackgrDaysPerYrNum = RL_BackgrDaysPerYrNum,
                 BullCountry = RL_BullCountry)
emmip(polr_atten, ~BackgrAvTraining, mode = "exc.prob", at = at_train, CIs = T)

## Consecutive
emmeans(polr_atten, list(consec ~ BackgrAvTraining), mode = "exc.prob", at = at_train)
## -> Used in Fig 2

## --- DaysPerYr ----
at_days <- list(cut = RL_cut, 
                BullUseType = RL_BullUseType,
                BackgrYrsOfExp = RL_BackgrYrsOfExp,
                BackgrAvTraining = RL_BackgsAvTraining,
                BackgrDaysPerYrNum = c(1, 2, 3, 4, 5),
                BullCountry = RL_BullCountry)
emmip(polr_atten, ~BackgrDaysPerYrNum, mode = "exc.prob", at = at_days, CIs = T)

## Consecutive
emmeans(polr_atten, list(consec ~ BackgrDaysPerYrNum), mode = "exc.prob", at = at_days)

## --- Country of residence ----
at_count <- list(cut = RL_cut, 
                 BullUseType = RL_BullUseType,
                 BackgrYrsOfExp = RL_BackgrYrsOfExp,
                 BackgrAvTraining = RL_BackgsAvTraining,
                 BackgrDaysPerYrNum = RL_BackgrDaysPerYrNum)
emmip(polr_atten, ~BullCountry, mode = "exc.prob", at = at_count, CIs = T)

## Consecutive
emmeans(polr_atten, list(consec ~ BullCountry), mode = "exc.prob", at = at_count)