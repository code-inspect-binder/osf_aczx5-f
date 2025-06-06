## ***********************************************************************************
## ** TERRAIN AND TRAVEL ADVICE - FIG 3: USEFULNESS - UNDERSTANDING AND RECOGNITION **
## ***********************************************************************************
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

## Settings for figure
Export = TRUE
FileName = "Fig03_Use_UnderRecogn_hires.png"
FigWidth = 15
FigHeight = 9
LineWidth = 1.5
SymbolSize = 1.5
YLim = c(0, 90)


## ---- 2) LOADING OF DATA AND INITIAL PROCESSING ----

## Load data
load("TTA_Data_FinalModels.RData")


## ---- 3) CALCULATION OF ESTIMATED MARGINAL MEANS ----

## Estimated marginal means for UnderN
table(data_usejar$UnderN)
at_under <- list(Jargon = "Less", 
                 UnderN = c("<=Diff", "SWDiff", "SWEasy", "Easy", "VEasy"),
                 RecognN = "Fairly",
                 BackgrAvTraining = "Intro",
                 TrvlAdv_Atten = "Cons",
                 cut = "3|4")
grid_under <- ref_grid(clmm_usejar, mode = "exc.prob", at = at_under)
(emm_under <- emmeans(grid_under, specs = consec ~ UnderN, mode = "exc.prob"))
emmip(grid_under, ~ UnderN, mode = "exc.prob", at = at_under, CIs = T)


## Estimated marginal means for RecognN
table(data_usejar$RecognN)
at_recogn <- list(Jargon = "Less", 
                  UnderN = "Easy",
                  RecognN = c("NotAtAll", "Somewhat", "Fairly", "Very", "Extr", "N/A"),
                  BackgrAvTraining = "Intro",
                  TrvlAdv_Atten = "Cons",
                  cut = "3|4")
grid_recogn <- ref_grid(clmm_usejar, mode = "exc.prob", at = at_recogn)
(emm_recogn <- emmeans(grid_recogn, specs = consec ~ RecognN, mode = "exc.prob"))
emmip(grid_recogn, ~ RecognN, mode = "exc.prob", at = at_under, CIs = T)


## ---- 4) DRAWING OF FIGURE ----

## Setup
if (Export) png(filename = FileName, width = FigWidth, height = FigHeight, units = "cm", res = 300, pointsize = 6)
par(mfrow=c(1,2))
par(mar=c(10.1,4.1,4.1,2.1))

ColUnder1 <- "#1f78b4"
ColUnder2 <- "#a6cee3"
ColRecogn1 <- "#33a02c"
ColRecogn2 <- "#b2df8a"

## Left panel: Understanding
XAxis1 <- c(1:5)
Index <- c(1, 3, 5, 7, 9)

plot_under <- summary(emm_under$emmeans)
ExcProp1 <- plot_under$exc.prob*100
ConfIntL1 <- plot_under$asymp.LCL*100
ConfIntU1 <- plot_under$asymp.UCL*100

plot(XAxis1, ExcProp1, ylab = "Probability of selecting 'Very/Extremely useful'", 
     las = 1, ylim = YLim, xlim = c(0.5, 5.5), xaxt='n', xlab= "", main = "a) Level of Understanding", cex.main = 1)
axis(1, at = c(1:5), labels = c("(Very) Difficult", "Somewhat Difficult", "Somewhat Easy", "Easy", "Very Easy"), las=2)
mtext("Level of Understanding", side=1, line = 8.5)
grid()
box()
arrows(XAxis1, y0 = ConfIntL1, y1 = ConfIntU1, angle = 90, code = 3, lwd = LineWidth, col = ColUnder2, length = 0.05)
lines(XAxis1, ExcProp1, lty = 2, lwd = LineWidth, col = ColUnder1)
points(XAxis1, ExcProp1, pch = 21, bg = ColUnder1, cex = SymbolSize)
text(XAxis1, ExcProp1, labels = format(round(ExcProp1, 1), nsmall = 1), adj = 0.5, pos = 3, 
     offset = 1, cex = 1, font = 2)


## Right panel: Recognition
XAxis2 <- c(1:6)
Index <- c(1, 3, 5, 7, 9, 11)

plot_recogn <- summary(emm_recogn$emmeans)
ExcProp2 <- plot_recogn$exc.prob*100
ConfIntL2 <- plot_recogn$asymp.LCL*100
ConfIntU2 <- plot_recogn$asymp.UCL*100

plot(XAxis2, ExcProp2, ylab = "Probability of selecting 'Very/Extremely useful'", 
     las = 1, ylim = YLim, xlim = c(0.5, 6.5), xaxt = "n", xlab = "", main = "b) Confidence in Recognition", cex.main = 1)
axis(1, at = c(1:6), labels = c("Not at all", "Somewhat", "Fairly", "Very", "Extremely", "N/A"), las=2)
mtext("Confidence in Recognition", side=1, line = 8.5)
grid()
box()
arrows(XAxis2, y0 = ConfIntL2, y1 = ConfIntU2, angle = 90, code = 3, lwd = LineWidth, col = ColRecogn2, length = 0.05)
lines(XAxis2[1:5], ExcProp2[1:5], lty = 2, lwd = LineWidth, col = ColRecogn1)
points(XAxis2, ExcProp2, pch = 21, bg = ColRecogn1, cex = SymbolSize)
text(XAxis2, ExcProp2, labels = format(round(ExcProp2, 1), nsmall = 1), adj = 0.5, pos = 3, 
     offset = 1, cex = 1, font = 2)

## Finish and reset
if (Export) dev.off()

par(mfrow=c(1,1))
par(mar=c(5.1,4.1,4.1,2.1))
