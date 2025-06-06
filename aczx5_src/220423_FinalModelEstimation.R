## ******************************************************
## ** TERRAIN AND TRAVEL ADVICE - ESTIMATION OF MODELS **
## ******************************************************
##
## Fisher, Haegeli and Mair: 
## Travel and terrain advice statements in public avalanche bulletins: 
## Which backcountry recreationists use this information, what makes 
## it useful, and how can we make it better?
## April 23, 2022


## ---- 1) PREPARATION -----

## Necessary packages
require(MASS)
if (!require("vcd")) {install.packages("vcd"); require("vcd")}
if (!require("car")) {install.packages("car"); require("car")}
if (!require("lme4")) {install.packages("lme4"); require("lme4")}
if (!require("effects")) {install.packages("effects"); require("effects")}
if (!require("emmeans")) {install.packages("emmeans"); require("emmeans")}
if (!require("ordinal")) {install.packages("ordinal"); require("ordinal")}
if (!require("bayesplot")) {install.packages("bayesplot"); require("bayesplot")}        ## For posterior predictive checks
if (!require("performance")) {install.packages("performance"); require("performance")}  ## For posterior predictive checks

## Function for showing parameter estimats with pvalues for polr models
showCoefWithPValue <- function(model) {
  coef <- coef(summary(model))
  pvalue <- pnorm(abs(coef[, "t value"]), lower.tail = FALSE) * 2
  coef <- cbind(coef, "pvalue" = pvalue)
  return(round(coef, 4))
}

## Switch for model estimation
Reestimate <- FALSE  ## Set to TRUE for reestimating all the models


## ---- 2) LOADING OF DATA AND INITIAL PROCESSING ----

## Load data
if (file.exists("TTA_Data_FinalModels.RData") & !Reestimate) {
  load("TTA_Data_FinalModels.RData")
} else {
  load("TTA_Data.RData")
}

## Eliminating Type A anf F from PartBkgr
table(PartBkgr$BullUseType, PartBkgr$TrvlAdv_Atten)
table(PartBkgr$BullUseType)
PartBkgr <- PartBkgr[PartBkgr$BullUseType != "A",]
PartBkgr <- PartBkgr[PartBkgr$BullUseType != "F",]
PartBkgr$BullUseType <- droplevels(PartBkgr$BullUseType)
table(PartBkgr$BullUseType)
table(PartBkgr$BullUseType, PartBkgr$TrvlAdv_Atten)

## Proportion of survey participants included in the analysis relative to how completed the survey
nrow(PartBkgr)/3668

## Eliminating Type A anf F from TrvAd
table(TrvAd$BullUseType)
TrvAd <- TrvAd[TrvAd$BullUseType != "A",]
TrvAd <- TrvAd[TrvAd$BullUseType != "F",]
TrvAd$BullUseType <- droplevels(TrvAd$BullUseType)
table(TrvAd$BullUseType)

## Adding model type column
TrvAd$ModelType[is.na(TrvAd$Jargon)] <- "Expl"
TrvAd$ModelType[!is.na(TrvAd$Jargon)] <- "Jarg"
TrvAd$ModelType <- factor(TrvAd$ModelType)
table(TrvAd$ModelType)


## ---- 2) PARTICIPANT OVERVIEW (Section 3.1) ----

nrow(PartBkgr)

## Gender
(temp <- table(PartBkgr$DemogrGender))
round(100*prop.table(temp),1)

## Age
(temp <- table(PartBkgr$DemogrAge))
round(100*prop.table(temp),1)

## Education
(temp <- table(PartBkgr$DemogrEduc))
round(100*prop.table(temp),1)

## Avalanche awareness training
(temp <- table(PartBkgr$BackgrAvTraining))
round(100*prop.table(temp),1)

## Primary backcountry activity
(temp <- table(PartBkgr$BackgrActivity_1))
round(100*prop.table(temp),1)

## Years of experience
(temp <- table(PartBkgr$BackgrYrsOfExp))
round(100*prop.table(temp),1)

## Bulletin user type
(temp <- table(PartBkgr$BullUseType))
round(100*prop.table(temp),1)

## Correlation between bulletin user type and avalanche awareness training
cor.test(as.numeric(PartBkgr$BullUseType), as.numeric(PartBkgr$BackgrAvTraining), method = "spearman")

## ---- Table 1: Bulletin user type versus avalanche awareness training ----
(temp <- table(PartBkgr$BullUseType, PartBkgr$BackgrAvTraining))
round(100*prop.table(temp, 1),1)

## Country
(temp <- table(PartBkgr$BullCountry))
round(100*prop.table(temp),1)

## Cleanup
rm(temp)


## ---- 3) ATTENTION MODEL (Section 3.2) ----

## Model data: all records
data_atten <- PartBkgr

## Number of records
nrow(data_atten)

## Distribution of response variable
plot(data_atten$TrvlAdv_Atten, main = "How much attention to you pay to TrvAd?")
(temp <- table(data_atten$TrvlAdv_Atten))
round(100*prop.table(temp), 1)
rm(temp)

## Setting contrasts
contrasts(data_atten$BullUseType) <- contr.treatment
contrasts(data_atten$BackgrAvTraining) <- contr.treatment
contrasts(data_atten$BackgrDaysPerYr) <- contr.treatment
contrasts(data_atten$BackgrYrsOfExp) <- contr.treatment

## Estimating model
if (Reestimate | !exists("polr_atten")) {
  polr_atten <- polr(TrvlAdv_Atten ~ BullUseType + BackgrAvTraining + BackgrDaysPerYrNum + BullCountry, 
                     data = data_atten)
}

## ---- Table 2: Parameter estimates of attention model ----
Anova(polr_atten)
showCoefWithPValue(polr_atten)

fit.eff <- allEffects(polr_atten)
plot(fit.eff, type = "probability", style = "stacked")
rm(fit.eff)

## Checking for colinearity issues
vif(polr_atten)
## -> All values are smaller than 5, which indicates that there are no issues

## Posterior predictive checks
n_sim <- 20
sim_yrep <- matrix(NA, nrow(data_atten), n_sim, dimnames = list(NULL, paste0("sim", 1:n_sim)))   ## initialize matrix
set.seed(123)
sim_yrep <- simulate(polr_atten, nsim = n_sim)

(sim_res <- apply(sim_yrep, 2, table))
summary(sim_res[4,])
table(data_atten$TrvlAdv_Atten)
## -> Simulations represent observed distribution of response categories well.
rm(sim_res, sim_yrep, n_sim)


## ---- 4) USEFULNESS MODELS (Section 3.3) ----

## Model data only includes cases where we have a understanding rating
data_use <- TrvAd[!is.na(TrvAd$Use),]
nrow(data_use)

## Variable distributions
plot(data_use$BullUseType, main = "Bulletin User Type")                 ## -> Decent distribution
plot(data_use$BackgrAvTraining, main = "Avalanche awareness Training")  ## -> Decent distribution
plot(data_use$BackgrDaysPerYr, main = "Days per Year")                  ## -> few 1-2 days: could combine with 3-10 days
plot(data_use$BackgrYrsOfExp, main = "Years of Experience")             ## -> Small but decent number of First year
plot(data_use$TrvlAdv_Atten, main = "Attention to Travel Advice")       ## -> Skewed but decent distribution 

plot(data_use$UnderN, main = "Ease of understanding")                   ## -> VDiff is very small
plot(data_use$RecognN, main = "Confidence in recognition")              ## -> fine!

(temp <- table(data_use$RecognN))
round(100*prop.table(temp), 1)

plot(data_use$Use, main = "Distribution of Usefulness")  ## -> not too bad
(temp <- table(data_use$Use))
round(100*prop.table(temp), 1)

## Simplifying of predictor variables
plot(data_use$BackgrDaysPerYr, main = "Backcountry days per winter")
data_use$BackgrDaysPerYr <- ordered(data_use$BackgrDaysPerYr, labels = c("1-10 days", "1-10 days", "11-20 days", "21-50 days", "50+ days"))
plot(data_use$BackgrDaysPerYr, main = "Backcountry days per winter")
(temp <- table(data_use$BackgrDaysPerYr))
round(100*prop.table(temp), 1)

plot(data_use$UnderN, main = "Ease of understanding")
data_use$UnderN <- factor(data_use$UnderN, labels = c("N/A", "<=Diff", "<=Diff", "SWDiff", "SWEasy", "Easy", "VEasy"))
plot(data_use$UnderN, main = "Ease of understanding")
(temp <- table(data_use$UnderN))
round(100*prop.table(temp), 1)

## Preparing dataset
contrasts(data_use$BullUseType) <- contr.treatment 
contrasts(data_use$BackgrAvTraining) <- contr.treatment
contrasts(data_use$BackgrDaysPerYr) <- contr.treatment
contrasts(data_use$BackgrYrsOfExp) <- contr.treatment
contrasts(data_use$TrvlAdv_Atten) <- contr.treatment
contrasts(data_use$UnderN) <- contr.treatment
contrasts(data_use$RecognN) <- contr.treatment

## Comparison of two datasets
(temp <- table(data_use$ModelType))
round(100*prop.table(temp), 1)
plot(Use ~ ModelType, data = data_use)
(temp <- table(data_use$ModelType, data_use$Use))
round(100*prop.table(temp, 1), 1)
kruskal.test(Use ~ ModelType, data = data_use)
rm(temp)


## ----- 4.1) USEFULNESS MODEL: JARGON -----

## Selecting relevant rows in data set
data_usejar <- data_use[data_use$ModelType == "Jarg",]
table(data_usejar$Jargon)

summary(data_usejar$UnderN)
data_usejar$UnderN <- droplevels(data_usejar$UnderN)
summary(data_usejar$RecognN)

## Model estimation
if (Reestimate | !exists("clmm_usejar")) {
  
  if (!exists("clmm_usejar1")) {
    clmm_usejar1 <- clmm(Use ~ BackgrAvTraining*Jargon + TrvlAdv_Atten + UnderN + RecognN + (1|Id) + (1|StatCode/VersionCode), data = data_usejar)
  }
  summary(clmm_usejar1)
  ## -> RESULTS: No effect of jargon main or interaction effect!
  
  if (!exists("clmm_usejar2")) {
    clmm_usejar2 <- clmm(Use ~ BackgrAvTraining + TrvlAdv_Atten + UnderN + RecognN + (1|Id) + (1|StatCode/VersionCode), data = data_usejar)
  }
  summary(clmm_usejar2)

  anova(clmm_usejar1, clmm_usejar2)
  ## -> More parsimonious model 2 better due to lower AIC

  ## We still use the model with the interactions becasue it does not affect the effects plots  
  clmm_usejar <- clmm_usejar1
  rm(clmm_usejar1, clmm_usejar2)

}

summary(clmm_usejar)
## -> No main and interaction effect for jargon.

## ---- Table B1: Parameter estimate of usefulness - jargon model ----
round(coef(summary(clmm_usejar)), 4)


## @PATRICK: Here it would be useful to have an omnibus test for the significance
##           of the different variables sliminat to the Anova() function from the 
##           car package.


## --- polr version of final model for additional model checks ----
if (Reestimate | !exists("polr_usejar")) {
  polr_usejar <- polr(Use ~ BackgrAvTraining + TrvlAdv_Atten + UnderN + RecognN, data = data_usejar)
}

## Type II Wald Statistics
Anova(polr_usejar) 

## Generalized variance inflation factors
vif(polr_usejar)
## -> everything is fine since GVIF^(1/(2*DF)) < 5

## Response category distributions
n_sim <- 50
sim_yrep <- matrix(NA, nrow(data_usejar), n_sim, dimnames = list(NULL, paste0("sim", 1:n_sim)))   ## initialize matrix
set.seed(123)
sim_yrep <- simulate(polr_usejar, nsim = n_sim)
(sim_res <- apply(sim_yrep, 2, table))
as.data.frame(apply(sim_res, 1, summary))[, c(3, 4, 2, 5, 1)]
table(data_usejar$Use)
## -> Looks good

## Clean up 
rm(list = ls(pattern = "sim_+"))
rm(n_sim)


## ----- 4.2) USEFULNESS MODEL: EXPLANATION -----

## Selecting relevant rows in data set
data_useexp <- data_use[data_use$ModelType == "Expl",]
table(data_useexp$Expl)

## Relevel UnderN so that it has the same reference level as the Jargon model
table(data_useexp$UnderN)
data_useexp$UnderN <- relevel(data_useexp$UnderN, ref = 2)

## Model with same final predictors as jargon model
if (Reestimate | !exists("clmm_useexp")) {
  clmm_useexp <- clmm(Use ~ BackgrAvTraining*Expl + TrvlAdv_Atten + UnderN + RecognN + (1|Id) + (1|StatCode/VersionCode), data = data_useexp)
}
summary(clmm_useexp)
## -> Marginal main effect for explanation, but significant interaction effect.

## ---- Table B1: Parameter estimate of usefulness - explanation model ----
round(coef(summary(clmm_useexp)), 4)


## @PATRICK: Here it would be useful to have an omnibus test for the significance
##           of the different variables sliminat to the Anova() function from the 
##           car package.


## --- polr version of final model for additional model checks ----
if (!exists("polr_useexp")) {
  polr_useexp <- polr(Use ~ BackgrAvTraining*Expl + TrvlAdv_Atten + UnderN + RecognN, data = data_useexp)
}
summary(polr_useexp)
showCoefWithPValue(polr_useexp)

## Type II Wald Statistics
Anova(polr_useexp) 
## -> All included variables significant

## Generalized variance inflation factors
vif(polr_useexp)
## -> everything is fine since GVIF^(1/(2*DF)) < 5

## Response category distributions
n_sim <- 50
sim_yrep <- matrix(NA, nrow(data_useexp), n_sim, dimnames = list(NULL, paste0("sim", 1:n_sim)))   ## initialize matrix
set.seed(123)
sim_yrep <- simulate(polr_useexp, nsim = n_sim)
(sim_res <- apply(sim_yrep, 2, table))
as.data.frame(apply(sim_res, 1, summary))[, c(3, 4, 2, 5, 1)]
table(data_useexp$Use)
## -> Looks good

## Clean up 
rm(list = ls(pattern = "sim_+"))
rm(n_sim)


## ---- 5) UNDERSTANDING MODEL ----

## Model data only includes cases where we have a understanding rating
data_under <- TrvAd[!is.na(TrvAd$Under),]
nrow(data_under)

## Variable distributions
plot(data_under$BullUseType, main = "Bulletin User Type")                 ## -> Decent distribution
plot(data_under$BackgrAvTraining, main = "Avalanche awareness Training")  ## -> Decent distribution
plot(data_under$BackgrDaysPerYr, main = "Days per Year")                  ## -> few 1-2 days: could combine with 3-10 days
plot(data_under$BackgrYrsOfExp, main = "Years of Experience")             ## -> Small but decent number of First year
plot(data_under$TrvlAdv_Atten, main = "Attention to Travel Advice")       ## -> Skewed but decent distribution 

plot(data_under$Under, main = "Distribution of Under")                    ## -> Very few VDiff ratings
(temp <- table(data_under$Under))
round(100*prop.table(temp), 1)

## Simplifying of predictor variables
table(data_under$BackgrDaysPerYr)
data_under$BackgrDaysPerYr <- ordered(data_under$BackgrDaysPerYr, labels = c("1-10 days", "1-10 days", "11-20 days", "21-50 days", "50+ days"))
table(data_under$BackgrDaysPerYr)

## Simplify response variable
table(data_under$Under)
data_under$Under <- ordered(data_under$Under, labels = c("<=Diff", "<=Diff", "SWDiff", "SWEasy", "Easy", "VEasy"))
table(data_under$Under)

## Preparing dataset
contrasts(data_under$BullUseType) <- contr.treatment 
contrasts(data_under$BackgrAvTraining) <- contr.treatment
contrasts(data_under$BackgrDaysPerYr) <- contr.treatment
contrasts(data_under$BackgrYrsOfExp) <- contr.treatment
contrasts(data_under$TrvlAdv_Atten) <- contr.treatment

## Comparison of two datasets
(temp <- table(data_under$ModelType))
round(100*prop.table(temp), 1)
plot(Use ~ ModelType, data = data_under)
(temp <- table(data_under$ModelType, data_under$Under))
round(100*prop.table(temp, 1), 1)
kruskal.test(Use ~ ModelType, data = data_under)
rm(temp)

## ----- 5.1) UNDERSTANDING MODEL: JARGON -----

## Selecting relevant rows in data set
data_underjar <- data_under[data_under$ModelType == "Jarg",]
table(data_underjar$Jargon)

## Estimating model
if (Reestimate | !exists("clmm_underjar")) {
  clmm_underjar <- clmm(Under ~ BackgrAvTraining*Jargon + TrvlAdv_Atten + BackgrYrsOfExp + BackgrDaysPerYr + BullUseType + (1|Id) + (1|StatCode/VersionCode), 
                        data = data_underjar)
}
summary(clmm_underjar)
## -> Main and interaction effect of jargon

## ---- Table B2: Parameter estimate of ease of understanding - for jargon model ----
round(coef(summary(clmm_underjar)), 4)


## @PATRICK: Here it would be useful to have an omnibus test for the significance
##           of the different variables sliminat to the Anova() function from the 
##           car package.


## --- polr version of final model for additional model checks ----
if (Reestimate | !exists("polr_underjar")) {
  polr_underjar <- polr(Under ~ BackgrAvTraining * Jargon + TrvlAdv_Atten + BackgrYrsOfExp + BackgrDaysPerYr + BullUseType,
                        data = data_underjar)
}

## Type II Wald statistics
Anova(polr_underjar)

## Variance inflation factors
vif(polr_underjar)
## -> everything is fine since GVIF^(1/(2*DF)) < 5

## Simulation for posterior predictive checks
n_sim <- 50
sim_yrep <- matrix(NA, nrow(data_underjar), n_sim, dimnames = list(NULL, paste0("sim", 1:n_sim)))   ## initialize matrix
set.seed(123)
sim_yrep <- simulate(polr_underjar, nsim = n_sim)
(sim_res <- apply(sim_yrep, 2, table))
as.data.frame(apply(sim_res, 1, summary))[, c(1, 3, 4, 2, 5)]
table(data_underjar$Under)
## -> Looks good

## Clean up 
rm(list = ls(pattern = "sim_+"))
rm(n_sim)


## ---- 5.2) UNDERSTANDING MODEL: EXPLANATION ----

## Selecting relevant rows in data set
data_underexp <- data_under[data_under$ModelType == "Expl",]
table(data_underexp$Expl)
## -> About half the size of the jargon model -> expect lower significance

## Estimating model
if (Reestimate | !exists("clmm_underexp")) {
  clmm_underexp <- clmm(Under ~ BackgrAvTraining*Expl + TrvlAdv_Atten + BackgrYrsOfExp + BackgrDaysPerYr + BullUseType + (1|Id) + (1|StatCode/VersionCode),
                        data = data_underexp)
}
summary(clmm_underexp)
## -> Interaction just marginally significant

## ---- Table B2: Parameter estimate of ease of understanding - jargon model ----
round(coef(summary(clmm_underexp)), 4)


## @PATRICK: Here it would be useful to have an omnibus test for the significance
##           of the different variables sliminat to the Anova() function from the 
##           car package.


## --- polr version of final model for additional model checks ----
if (Reestimate | !exists("polr_underexp")) {
  polr_underexp <- polr(Under ~ BackgrAvTraining * Expl + TrvlAdv_Atten + BackgrYrsOfExp + BackgrDaysPerYr + BullUseType, data = data_underexp)
}

## Type II Wald statistics
Anova(polr_underexp)

## Variance inflation factors
vif(polr_underexp)
## -> everything is fine since GVIF^(1/(2*DF)) < 5

## Simulation for posterior predictive checks
n_sim <- 50
sim_yrep <- matrix(NA, nrow(data_underexp), n_sim, dimnames = list(NULL, paste0("sim", 1:n_sim)))   ## initialize matrix
set.seed(123)
sim_yrep <- simulate(polr_underexp, nsim = n_sim)
(sim_res <- apply(sim_yrep, 2, table))
as.data.frame(apply(sim_res, 1, summary))[, c(1, 3, 4, 2, 5)]
table(data_underexp$Under)
## -> Looks good

## Clean up 
rm(list = ls(pattern = "sim_+"))
rm(n_sim)


## ---- 6) RECOGNITION MODEL ----

## Model data only includes cases where we have a understanding rating
data_recogn <- TrvAd[!is.na(TrvAd$Recogn),]
nrow(data_recogn)

## Variable distributions
plot(data_recogn$BullUseType, main = "Bulletin User Type")                 ## -> Decent distribution
plot(data_recogn$BackgrAvTraining, main = "Avalanche awareness Training")  ## -> Decent distribution
plot(data_recogn$BackgrDaysPerYr, main = "Days per Year")                  ## -> few 1-2 days: could combine with 3-10 days
plot(data_recogn$BackgrYrsOfExp, main = "Years of Experience")             ## -> Small but decent number of First year
plot(data_recogn$TrvlAdv_Atten, main = "Attention to Travel Advice")       ## -> Skewed but decent distribution 

plot(data_recogn$Recogn, main = "Distribution of Recogn")  ## -> not too bad
(temp <- table(data_recogn$Recogn))
round(100*prop.table(temp), 1)

## Simplifying of predictor variables
table(data_recogn$BackgrDaysPerYr)
data_recogn$BackgrDaysPerYr <- ordered(data_recogn$BackgrDaysPerYr, labels = c("1-10 days", "1-10 days", "11-20 days", "21-50 days", "50+ days"))
table(data_recogn$BackgrDaysPerYr)

## Preparing dataset
contrasts(data_recogn$BullUseType) <- contr.treatment 
contrasts(data_recogn$BackgrAvTraining) <- contr.treatment
contrasts(data_recogn$BackgrDaysPerYr) <- contr.treatment
contrasts(data_recogn$BackgrYrsOfExp) <- contr.treatment
contrasts(data_recogn$TrvlAdv_Atten) <- contr.treatment

table(data_recogn$Jargon)
table(data_recogn$Expl)

## Comparison of two datasets
(temp <- table(data_recogn$ModelType))
round(100*prop.table(temp), 1)
plot(Use ~ ModelType, data = data_recogn)
(temp <- table(data_recogn$ModelType, data_recogn$Recogn))
round(100*prop.table(temp, 1), 1)
kruskal.test(Use ~ ModelType, data = data_recogn)
rm(temp)


## ----- 6.1) RECOGNITION MODEL: JARGON -----

## Selecting relevant rows in data set
data_recognjar <- data_recogn[data_recogn$ModelType == "Jarg",]
table(data_recognjar$Jargon)

## Estimating model
if (Reestimate | !exists("clmm_recognjar")) {
  clmm_recognjar <- clmm(Recogn ~ BackgrAvTraining*Jargon + BackgrYrsOfExp + BackgrDaysPerYr + BullUseType + (1|Id) + (1|StatCode/VersionCode), 
                         data = data_recognjar)
}
summary(clmm_recognjar)
## -> Limited main effect for jargon, but significant IA effect

## ---- Table B3: Parameter estimate for confidence in recognition - jargon model ----
round(coef(summary(clmm_recognjar)), 4)


## @PATRICK: Here it would be useful to have an omnibus test for the significance
##           of the different variables sliminat to the Anova() function from the 
##           car package.


## --- polr version of final model for additional model checks ----
if (!exists("polr_recognjar")) {
  polr_recognjar <- polr(Recogn ~ BackgrAvTraining*Jargon + BackgrYrsOfExp + BackgrDaysPerYr + BullUseType, data = data_recognjar)
}

## Type II Wald Statistics
Anova(polr_recognjar) 
## -> All included variables significant

## Generalized variance inflation factors
vif(polr_recognjar)
## -> everything is fine since GVIF^(1/(2*DF)) < 5

## Response category distributions
n_sim <- 50
sim_yrep <- matrix(NA, nrow(data_recognjar), n_sim, dimnames = list(NULL, paste0("sim", 1:n_sim)))   ## initialize matrix
set.seed(123)
sim_yrep <- simulate(polr_recognjar, nsim = n_sim)

(sim_res <- apply(sim_yrep, 2, table))
as.data.frame(apply(sim_res, 1, summary))[, c(3, 4, 2, 5, 1)]
table(data_recognjar$Recogn)
## -> Looks good

## Clean up
rm(sim_res, sim_yrep, n_sim)


## ----- 6.2) RECOGNITION MODEL: EXPLANATION -----

## Selecting relevant rows in data set
data_recognexp <- data_recogn[!is.na(data_recogn$Expl),]
table(data_recognexp$Expl)

## Check for need for random effects
table(data_recognexp$StatCode, data_recognexp$VersionCode)
## -> Only a single StatCode with two version codes
## -> Naturally, each participant only saw this single statement once
##    NO NEED FOR ANY RANDOM EFFECTS

## Model with same final predictors as jargon model
if (Reestimate | !exists("polr_recognexp")) {
  polr_recognexp <- polr(Recogn ~ BackgrAvTraining*Expl + BackgrYrsOfExp + BackgrDaysPerYr + BullUseType, data = data_recognexp)
}
summary(polr_recognexp)
## -> Significant main and interaction effect

## ---- Table B3: Parameter estimate for confidence in recognition - explanation model ----
showCoefWithPValue(polr_recognexp)

## Type II Wald Statistics
Anova(polr_recognexp) 
## -> Interaction just marginally significant.

## Generalized variance inflation factors
vif(polr_recognexp)
## -> everything is fine since GVIF^(1/(2*DF)) < 5

## Response category distributions
n_sim <- 50
sim_yrep <- matrix(NA, nrow(data_recognexp), n_sim, dimnames = list(NULL, paste0("sim", 1:n_sim)))   ## initialize matrix
set.seed(123)
sim_yrep <- simulate(polr_recognexp, nsim = n_sim)
(sim_res <- apply(sim_yrep, 2, table))
as.data.frame(apply(sim_res, 1, summary))[, c(3, 4, 2, 5, 1)]
table(data_recognexp$Recogn)
## -> Looks good

## Clean up
rm(sim_res, sim_yrep, n_sim)


## ---- 7) SAVING OF MODELS ----

## Final cleanup
rm(Reestimate)
## Save all of the models and datasets
save.image(file = "TTA_Data_FinalModels.RData")
