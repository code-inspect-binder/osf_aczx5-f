## ******************************************************************
## ** TERRAIN AND TRAVEL ADVICE - EXAMINATION OF USEFULNESS MODELS **
## ******************************************************************
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
(temp <- table(data_use$Use))
round(100*prop.table(temp), 1)
rm(temp)


## ---- 3) CALCULATION OF ESTIMATED MARGINAL MEANS: REFERENCE LEVELS ----

## Definition of reference levels
RL_Jargon <- "Less"
RL_Expl <- "No"
RL_UnderN <- "Easy"
RL_RecognN <- "Fairly"
RL_BackgsAvTraining <- "Intro"
RL_TrvlAdv_Atten <- "Cons"
RL_cut <- "3|4"

## ---- 3.1) CALCULATION OF ESTIMATED MARGINAL MEANS: JARGON ----

## ---- Understanding ----
at_under <- list(Jargon = RL_Jargon, 
                 RecognN = RL_RecognN,
                 BackgrAvTraining = RL_BackgsAvTraining,
                 TrvlAdv_Atten = RL_TrvlAdv_Atten,
                 cut = RL_cut)
grid_under <- ref_grid(clmm_usejar, mode = "exc.prob", at = at_under)
emmip(grid_under, ~ UnderN, mode = "exc.prob", at = at_under, CIs = T)

## Consecutive
emmeans(grid_under, specs = consec ~ UnderN, mode = "exc.prob")
## -> Figure 3


## ---- Recognition ----
at_recogn <- list(Jargon = RL_Jargon, 
                  UnderN = RL_UnderN,
                  RecognN = c("NotAtAll", "Somewhat", "Fairly", "Very", "Extr", "N/A"),
                  BackgrAvTraining = RL_BackgsAvTraining,
                  TrvlAdv_Atten = RL_TrvlAdv_Atten,
                  cut = RL_cut)
grid_recogn <- ref_grid(clmm_usejar, mode = "exc.prob", at = at_recogn)
emmip(grid_recogn, ~ RecognN, mode = "exc.prob", at = at_under, CIs = T)

## Consecutive
emmeans(grid_recogn, specs = consec ~ RecognN, mode = "exc.prob")
## -> Figure 3


## ---- Attention to travel advice ----
at_att <- list(Jargon = RL_Jargon, 
               UnderN = RL_UnderN,
               RecognN = RL_RecognN,
               BackgrAvTraining = RL_BackgsAvTraining,
               cut = RL_cut)
grid_att <- ref_grid(clmm_usejar, mode = "exc.prob", at = at_att)
emmip(grid_att, ~ TrvlAdv_Atten, mode = "exc.prob", at = at_under, CIs = T)

## Consecutive
emmeans(grid_att, specs = consec ~ TrvlAdv_Atten, mode = "exc.prob")


## ---- Interaction of training and jargon ----
at_jarg <- list(Jargon = c("More", "Less"), 
                UnderN = RL_UnderN,
                RecognN = RL_RecognN,
                TrvlAdv_Atten = RL_TrvlAdv_Atten,
                cut = RL_cut)
grid_jarg <- ref_grid(clmm_usejar, mode = "exc.prob", at = at_jarg)
emmip(grid_jarg, ~ Jargon*BackgrAvTraining, mode = "exc.prob", at = at_jarg, CIs = T)

## Pairwise
emmeans(grid_jarg, specs = pairwise ~ Jargon, by = "BackgrAvTraining")
## -> Fig 4


## ---- 3.2) CALCULATION OF ESTIMATED MARGINAL MEANS: EXPLANATION ----

## ---- Understanding ----
at_under <- list(Expl = RL_Expl,
                 UnderN = c("<=Diff", "SWDiff", "SWEasy", "Easy", "VEasy", "N/A"),
                 RecognN = RL_RecognN,
                 BackgrAvTraining = RL_BackgsAvTraining,
                 TrvlAdv_Atten = RL_TrvlAdv_Atten,
                 cut = RL_cut)
grid_under <- ref_grid(clmm_useexp, mode = "exc.prob", at = at_under)
emmip(grid_under, ~ UnderN, mode = "exc.prob", at = at_under, CIs = T)

## Consecutive
emmeans(grid_under, specs = consec ~ UnderN, mode = "exc.prob")


## ---- Recognition ----
at_recogn <- list(Expl = RL_Expl, 
                  UnderN = RL_UnderN,
                  RecognN = c("NotAtAll", "Somewhat", "Fairly", "Very", "Extr", "N/A"),
                  BackgrAvTraining = RL_BackgsAvTraining,
                  TrvlAdv_Atten = RL_TrvlAdv_Atten,
                  cut = RL_cut)
grid_recogn <- ref_grid(clmm_useexp, mode = "exc.prob", at = at_recogn)
emmip(grid_recogn, ~ RecognN, mode = "exc.prob", at = at_under, CIs = T)

## Consecutive
emmeans(grid_recogn, specs = consec ~ RecognN, mode = "exc.prob")


## ---- Attention to travel advice ----
at_att <- list(Expl = RL_Expl, 
               UnderN = RL_UnderN,
               RecognN = RL_RecognN,
               BackgrAvTraining = RL_BackgsAvTraining,
               cut = RL_cut)
grid_att <- ref_grid(clmm_useexp, mode = "exc.prob", at = at_att)
emmip(grid_att, ~ TrvlAdv_Atten, mode = "exc.prob", at = at_under, CIs = T)

## Consecutive
emmeans(grid_att, specs = consec ~ TrvlAdv_Atten, mode = "exc.prob")


## ---- Interaction of training and explanation ----
at_expl <- list(Expl = c("No", "Yes"), 
                UnderN = RL_UnderN,
                RecognN = RL_RecognN,
                TrvlAdv_Atten = RL_TrvlAdv_Atten,
                cut = RL_cut)
grid_expl <- ref_grid(clmm_useexp, mode = "exc.prob", at = at_expl)
emmip(grid_expl, ~ Expl*BackgrAvTraining, mode = "exc.prob", at = at_expl, CIs = T)

## Pairwise
emmeans(grid_expl, specs = pairwise ~ Expl, by = "BackgrAvTraining")
## -> Fig 4
