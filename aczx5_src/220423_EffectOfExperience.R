## ********************************************************************
## ** TERRAIN AND TRAVEL ADVICE - COMPARISON OF EFFECT OF EXPERIENCE **
## ********************************************************************
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
if (!require("ordinal")) {install.packages("ordinal"); require("ordinal")}

## ---- 2) LOADING OF DATA AND INITIAL PROCESSING ----

## Load data
load("TTA_Data_FinalModels.RData")


## ---- 3) CALCULATION OF ESTIMATED MARGINAL MEANS: REFERENCE LEVELS ----

## Definition of reference levels
RL_BullUseType <- "D"
RL_TrvlAdv_Atten <- "Cons"
RL_BackgsAvTraining <- "Intro"
RL_BackgrYrsOfExp <- "6-10 yrs"
RL_BackgrDaysPerYr <- "11-20 days"
RL_Jargon <- "Less"
RL_Expl <- "No"
RL_cut_under <- "3|4"
RL_cut_recogn <- "3|4"

## ---- Understand model ----
at_under <- list(Jargon = RL_Jargon,
                 TrvlAdv_Atten = RL_TrvlAdv_Atten,
                 BackgrDaysPerYr = RL_BackgrDaysPerYr,
                 BackgrAvTraining = RL_BackgsAvTraining,
                 BullUseType = RL_BullUseType,
                 cut = RL_cut_under)
grid_under <- ref_grid(clmm_underjar, mode = "exc.prob", at = at_under)
emmip(grid_under, ~ BackgrYrsOfExp, mode = "exc.prob", at = at_under, CIs = T)

## Consecutive
emmeans(grid_under, specs = consec ~ BackgrYrsOfExp)


## ---- Recognition model ----
at_recogn <- list(Jargon = RL_Jargon,
                  BackgrDaysPerYr = RL_BackgrDaysPerYr,
                  BackgrAvTraining = RL_BackgsAvTraining,
                  BullUseType = RL_BullUseType,
                  cut = RL_cut_recogn)
grid_recogn <- ref_grid(clmm_recognjar, mode = "exc.prob", at = at_recogn)
emmip(grid_recogn, ~ BackgrYrsOfExp, mode = "exc.prob", at = at_recogn, CIs = T)

## Consecutive
emmeans(grid_recogn, specs = consec ~ BackgrYrsOfExp)



## ---- 4) ESTIMATING MODELS WITH LIN AND QUAD TRENDS ----

## ---- Understand model ----

table(data_underjar$BackgrYrsOfExp)
contrasts(data_underjar$BackgrYrsOfExp) <- contr.poly(5)

clmm_underjar_poly <- clmm(Under ~ BackgrAvTraining*Jargon + TrvlAdv_Atten + BackgrYrsOfExp + BackgrDaysPerYr + BullUseType + (1|Id) + (1|StatCode/VersionCode), 
                           data = data_underjar)
summary(clmm_underjar_poly)
## -> Lin, Quad, and Cubic term significant


## ---- Recognition model ----
table(data_recognjar$BackgrYrsOfExp)
contrasts(data_recognjar$BackgrYrsOfExp) <- contr.poly(5)

clmm_recognjar_poly <- clmm(Recogn ~ BackgrAvTraining*Jargon + BackgrYrsOfExp + BackgrDaysPerYr + BullUseType + (1|Id) + (1|StatCode/VersionCode), 
                       data = data_recognjar)
summary(clmm_recognjar_poly)
## -> 
