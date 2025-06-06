## **************************************************
## ** TERRAIN AND TRAVEL ADVICE - FIG 2: ATTENTION **
## **************************************************
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

## Settings for figure
Export = TRUE
FileName = "Fig02_Atten_hires.png"
FigWidth = 15
FigHeight = 7
LineWidth = 1.5
SymbolSize = 1.5
YLim = c(30, 65)


## ---- 2) LOADING OF DATA AND INITIAL PROCESSING ----

## Load data
load("TTA_Data_FinalModels.RData")


## ---- 3) CALCULATION OF ESTIMATED MARGINAL MEANS ----

## Specific effects plots and estimated marginal means: BullUseType
at_but <- list(cut = "Cons|Large", 
               BackgrAvTraining = "Intro",
               BackgrYrsOfExp = "6-10 yrs",
               BackgrDaysPerYrNum = 3,  ## 11-20 days
               BullCountry = "Can")
emmip(polr_atten, ~BullUseType, mode = "exc.prob", at = at_but, CIs = T)
emmeans(polr_atten, list(consec ~ BullUseType), mode = "exc.prob", at = at_but)

## Specific effects plots and estimated marginal means: Training
at_train <- list(cut = "Cons|Large", 
                 BullUseType = "D",
                 BackgrYrsOfExp = "6-10 yrs",
                 BackgrDaysPerYrNum = 3,  ## 11-20 days
                 BullCountry = "Can")
emmip(polr_atten, ~BackgrAvTraining, mode = "exc.prob", at = at_train, CIs = T)
emmeans(polr_atten, list(consec ~ BackgrAvTraining), mode = "exc.prob", at = at_train)


## ---- 4) DRAWING OF FIGURE ----

## Setup
if (Export) png(filename = FileName, width = FigWidth, height = FigHeight, units = "cm", res = 300, pointsize = 6)

par(mfrow=c(1,2))
ColBUT1 <- "#e31a1c"
ColBUT2 <- "#fb9a99"
ColTrain1 <- "#1f78b4"
ColTrain2 <- "#a6cee3"

## Left panel: Bulletin user type
(emm <- emmeans(polr_atten, list(consec ~ BullUseType), mode = "exc.prob", at = at_but))
res <- summary(emm$`emmeans of BullUseType`)

plot(res$exc.prob*100, ylab = "Probability of selecting 'Large amount'", las = 1, ylim = YLim, xlim = c(0.75, 4.25), xaxt = "n",
     xlab = "Bulletin user type", main = "a) Bulletin User Type")
axis(1, at = c(1:4), labels = c("B", "C", "D", "E"))
grid()
arrows(x0 = c(1:4), y0 = res$asymp.LCL*100, y1 = res$asymp.UCL*100, angle = 90, code = 3, lwd = LineWidth, col = ColBUT2, length = 0.05)
lines(c(1:4), res$exc.prob*100, lty = 2, lwd = LineWidth, col = ColBUT1)
points(c(1:4), res$exc.prob*100, pch = 21, bg = ColBUT1, cex = SymbolSize)
text(c(1:4), res$exc.prob*100, labels = format(round(res$exc.prob*100, 1), nsmall = 1), adj = 0.5, 
     pos = 3, offset = 1, cex = 1, font = 2)

## Right panel: Avalanche training
(emm <- emmeans(polr_atten, list(consec ~ BackgrAvTraining), mode = "exc.prob", at = at_train))
res <- summary(emm$`emmeans of BackgrAvTraining`)

plot(res$exc.prob*100, ylab = "Probability of selecting 'Large amount'", las = 1, ylim = YLim, xlim = c(0.75, 4.25), xaxt = "n",
     xlab = "Levels of training", main = "b) Avalanche Training")
axis(1, at = c(1:4), labels = c("None", "Intro", "Adv", "Prof"))
grid()
arrows(x0 = c(1:4), y0 = res$asymp.LCL*100, y1 = res$asymp.UCL*100, angle = 90, code = 3, lwd = LineWidth, col = ColTrain2, length = 0.05)
lines(c(1:4), res$exc.prob*100, lty = 2, lwd = LineWidth, col = ColTrain1)
points(c(1:4), res$exc.prob*100, pch = 21, bg = ColTrain1, cex = SymbolSize)
text(c(1:4), res$exc.prob*100, labels = format(round(res$exc.prob*100, 1), nsmall = 1), adj = 0.5, 
     pos = 3, offset = 1, cex = 1, font = 2)

## Finish and reset
if (Export) dev.off()

par(mfrow=c(1,1))
par(mar=c(5.1,4.1,4.1,2.1))