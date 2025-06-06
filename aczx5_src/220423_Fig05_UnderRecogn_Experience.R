## *******************************************************************
## ** TERRAIN AND TRAVEL ADVICE - FIG 5: UNDERSTANDING - EXPERIENCE **
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

## Settings for figure
Export = TRUE
FileName = "Fig08_UnderRecogn_Experience_hires.png"
FigWidth = 15
FigHeight = 7
LineWidth = 1.5
SymbolSize = 1.5


## ---- 2) LOADING OF DATA AND INITIAL PROCESSING ----

## Load data
load("TTA_Data_FinalModels.RData")


## ---- 3) CALCULATION OF ESTIMATED MARGINAL MEANS ----

## Understanding model
at_under_yrs <- list(Jargon = "Less",
                     TrvlAdv_Atten = "Cons",
                     BackgrDaysPerYr = "11-20 days",
                     BackgrAvTraining = "Intro",
                     BullUseType = "D",
                     cut = "3|4")
grid_under_yrs <- ref_grid(clmm_underjar, mode = "exc.prob", at = at_under_yrs)
(emm_under_yrs <- emmeans(grid_under_yrs, specs = consec ~ BackgrYrsOfExp))
plot_under_yrs <- summary(emm_under_yrs$emmeans)

## Recognition
at_recogn_yrs <- list(Jargon = "Less",
                      BackgrDaysPerYr = "11-20 days",
                      BackgrAvTraining = "Intro",
                      BullUseType = "D",
                      cut = "3|4")
grid_recogn_yrs <- ref_grid(clmm_recognjar, mode = "exc.prob", at = at_recogn_yrs)
(emm_recogn_yrs <- emmeans(grid_recogn_yrs, specs = consec ~ BackgrYrsOfExp))
plot_recogn_yrs <- summary(emm_recogn_yrs$emmeans)


## ---- 4) DRAWING OF FIGURE ----

## Setup
if (Export) png(filename = FileName, width = FigWidth, height = FigHeight, units = "cm", res = 300, pointsize = 6)
par(mfrow=c(1,2))
par(mar=c(5.1,4.1,4.1,2.1))

ColYrs1 <- "#e31a1c"
ColYrs2 <- "#fb9a99"


## ---- Left panel: understanding ----

XAxis <- c(1:5)
ExcProp <- plot_under_yrs$exc.prob*100
ConfIntL <- plot_under_yrs$asymp.LCL*100
ConfIntU <- plot_under_yrs$asymp.UCL*100

plot(XAxis, ExcProp, ylab = "Probability of selecting 'Easy'/'Very easy'", 
     las = 1, ylim = c(0,90), xlim = c(0.5, 5.5), xaxt = "n", xlab = "Number of years", main = "a) Level of Understanding")
axis(1, at = c(1:5), labels = c("First", "2-5", "6-10", "11-20", "20+"))
grid()
box()
arrows(XAxis, y0 = ConfIntL, y1 = ConfIntU, angle = 90, code = 3, lwd = LineWidth, col = ColYrs2, length = 0.05)
lines(XAxis, ExcProp, lty = 2, lwd = LineWidth, col = ColYrs1)
points(XAxis, ExcProp, pch = 21, bg = ColYrs1, cex = SymbolSize)
text(XAxis, ExcProp, labels = format(round(ExcProp, 1), nsmall = 1), adj = 0.5, pos = c(3, 3, 3, 3), 
     offset = 1, cex = 1, font = 2)


## ---- Right panel: Recognition ----

XAxis <- c(1:5)
ExcProp <- plot_recogn_yrs$exc.prob*100
ConfIntL <- plot_recogn_yrs$asymp.LCL*100
ConfIntU <- plot_recogn_yrs$asymp.UCL*100

plot(XAxis, ExcProp, ylab = "Probability of selecting 'Very/Extremely confident'", 
     las = 1, ylim = c(0, 90), xlim = c(0.5, 5.5), xaxt = "n", xlab = "Number of years", main = "b) Confidence in Recognizing")
axis(1, at = c(1:5), labels = c("First", "2-5", "6-10", "11-20", "20+"))
grid()
box()
arrows(XAxis, y0 = ConfIntL, y1 = ConfIntU, angle = 90, code = 3, lwd = LineWidth, col = ColYrs2, length = 0.05)
lines(XAxis, ExcProp, lty = 2, lwd = LineWidth, col = ColYrs1)
points(XAxis, ExcProp, pch = 21, bg = ColYrs1, cex = SymbolSize)
text(XAxis, ExcProp, labels = format(round(ExcProp, 1), nsmall = 1), adj = 0.5, pos = c(3, 3, 3, 3), 
     offset = 1, cex = 1, font = 2)


## Finish and reset
if (Export) dev.off()

par(mfrow=c(1,1))
par(mar=c(5.1,4.1,4.1,2.1))