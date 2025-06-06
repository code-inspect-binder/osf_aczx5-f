## *******************************************************************
## ** TERRAIN AND TRAVEL ADVICE - EXAMINATION OF RECOGNITION MODELS **
## *******************************************************************
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

## Distribution of dependent variables
(temp <- table(data_recogn$Recogn))
round(100*prop.table(temp), 1)
rm(temp)




## ---- 3) CALCULATION OF ESTIMATED MARGINAL MEANS: REFERENCE LEVELS ----

## Definition of reference levels
RL_BullUseType <- "D"
RL_BackgsAvTraining <- "Intro"
RL_BackgrYrsOfExp <- "6-10 yrs"
RL_BackgrDaysPerYr <- "11-20 days"
RL_Jargon <- "Less"
RL_Expl <- "No"
RL_cut_clmm <- "3|4"
RL_cut_polr <- "Fairly|Very"


## ---- 3.1) CALCULATION OF ESTIMATED MARGINAL MEANS: JARGON ----

## ---- Bulletin user type ----
at_but <- list(Jargon = RL_Jargon,
               BackgrDaysPerYr = RL_BackgrDaysPerYr,
               BackgrAvTraining = RL_BackgsAvTraining,
               BackgrYrsOfExp = RL_BackgrYrsOfExp,
               cut = RL_cut_clmm)
grid_but <- ref_grid(clmm_recognjar, mode = "exc.prob", at = at_but)
emmip(grid_but, ~ BullUseType, mode = "exc.prob", at = at_but, CIs = T)

## Consecutive
emmeans(grid_but, specs = consec ~ BullUseType)


## ---- Days per winter ----
at_days <- list(Jargon = RL_Jargon,
                BackgrAvTraining = RL_BackgsAvTraining,
                BullUseType = RL_BullUseType,
                BackgrYrsOfExp = RL_BackgrYrsOfExp,
                cut = RL_cut_clmm)
grid_days <- ref_grid(clmm_recognjar, mode = "exc.prob", at = at_days)
emmip(grid_days, ~ BackgrDaysPerYr, mode = "exc.prob", at = at_days, CIs = T)

## Consecutive
emmeans(grid_days, specs = consec ~ BackgrDaysPerYr)


## ---- Experience ----
at_yrs <- list(Jargon = RL_Jargon,
               BackgrDaysPerYr = RL_BackgrDaysPerYr,
               BackgrAvTraining = RL_BackgsAvTraining,
               BullUseType = RL_BullUseType,
               cut = RL_cut_clmm)
grid_yrs <- ref_grid(clmm_recognjar, mode = "exc.prob", at = at_yrs)
emmip(grid_yrs, ~ BackgrYrsOfExp, mode = "exc.prob", at = at_yrs, CIs = T)

## Consecutive
emmeans(grid_yrs, specs = consec ~ BackgrYrsOfExp)


## ---- Jargon and training interaction ----
at_jarg <- list(Jargon = c("Less", "More"), 
                BackgrYrsOfExp = RL_BackgrYrsOfExp,
                BackgrDaysPerYr = RL_BackgrDaysPerYr,
                BullUseType = RL_BullUseType,
                cut = RL_cut_clmm)
grid_jarg <- ref_grid(clmm_recognjar, mode = "exc.prob", at = at_jarg)
emmip(grid_jarg, ~ Jargon*BackgrAvTraining, mode = "exc.prob", at = at_jarg, CIs = T)

## Pairwise
emmeans(grid_jarg, specs = pairwise ~ Jargon, by = "BackgrAvTraining")
## -> Figure 7
## Consecutive
emmeans(grid_jarg, specs = consec ~ BackgrAvTraining, by = "Jargon")




## ---- 3.2) CALCULATION OF ESTIMATED MARGINAL MEANS: EXPLANATION ----

## ---- Bulletin user type ----
at_but <- list(Expl = RL_Expl,
               BackgrDaysPerYr = RL_BackgrDaysPerYr,
               BackgrAvTraining = RL_BackgsAvTraining,
               BackgrYrsOfExp = RL_BackgrYrsOfExp,
               cut = RL_cut_polr)
grid_but <- ref_grid(polr_recognexp, mode = "exc.prob", at = at_but)
emmip(grid_but, ~ BullUseType, mode = "exc.prob", at = at_but, CIs = T)

## Consecutive
emmeans(grid_but, specs = consec ~ BullUseType)


## ---- Days per winter ----
at_days <- list(Expl = RL_Expl,
                BackgrAvTraining = RL_BackgsAvTraining,
                BullUseType = RL_BullUseType,
                BackgrYrsOfExp = RL_BackgrYrsOfExp,
                cut = RL_cut_polr)
grid_days <- ref_grid(polr_recognexp, mode = "exc.prob", at = at_days)
emmip(grid_days, ~ BackgrDaysPerYr, mode = "exc.prob", at = at_days, CIs = T)

## Consecutive
emmeans(grid_days, specs = consec ~ BackgrDaysPerYr)


## ---- Experience ----
at_yrs <- list(Expl = RL_Expl,
               BackgrDaysPerYr = RL_BackgrDaysPerYr,
               BackgrAvTraining = RL_BackgsAvTraining,
               BullUseType = RL_BullUseType,
               cut = RL_cut_polr)
grid_yrs <- ref_grid(polr_recognexp, mode = "exc.prob", at = at_yrs)
emmip(grid_yrs, ~ BackgrYrsOfExp, mode = "exc.prob", at = at_yrs, CIs = T)

## Consecutive
emmeans(grid_yrs, specs = consec ~ BackgrYrsOfExp)


## ---- Explanation and training interaction ----
at_expl <- list(Expl = c("No", "Yes"),
                BackgrYrsOfExp = RL_BackgrYrsOfExp,
                BackgrDaysPerYr = RL_BackgrDaysPerYr,
                BullUseType = RL_BullUseType,
                cut = RL_cut_polr)
grid_expl <- ref_grid(polr_recognexp, mode = "exc.prob", at = at_expl)
emmip(grid_expl, ~ Expl*BackgrAvTraining, mode = "exc.prob", at = at_expl, CIs = T)

## Pairwise
emmeans(grid_expl, specs = pairwise ~ Expl, by = "BackgrAvTraining")
## -> Figure 7
## Consecutive 
emmeans(grid_expl, specs = consec ~ BackgrAvTraining, by = "Expl")
