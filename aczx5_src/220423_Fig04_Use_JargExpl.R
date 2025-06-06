## ****************************************************************************
## ** TERRAIN AND TRAVEL ADVICE - FIG 4: USEFULNESS - JARGON AND EXPLANATION **
## ****************************************************************************
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
Export <- TRUE
FileName <- "Fig04_Use_JargExpl_hires.png"
FigWidth <- 15
FigHeight <- 7
LineWidth <- 1.5
LineWidthSign <- 1.5
SymbolSize <- 1.5
YLim <- c(0, 90)


## ---- 2) LOADING OF DATA AND INITIAL PROCESSING ----

## Load data
load("TTA_Data_FinalModels.RData")


## ---- 3) CALCULATION OF ESTIMATED MARGINAL MEANS ----

## Tests for differences in response to jargon for plot
at_jarg <- list(Jargon = c("More", "Less"), 
                UnderN = "Easy",
                RecognN = "Fairly",
                TrvlAdv_Atten = "Cons",
                cut = "3|4")
grid_jarg <- ref_grid(clmm_usejar, mode = "exc.prob", at = at_jarg)
(emm_jarg <- emmeans(grid_jarg, specs = pairwise ~ Jargon, by = "BackgrAvTraining"))
plot_jarg <- summary(emm_jarg$emmeans)
cont_jarg <- summary(emm_jarg$contrasts)

## Tests for differences in response to explanation for plot
at_expl <- list(Expl = c("No", "Yes"), 
                UnderN = "Easy",
                RecognN = "Fairly",
                TrvlAdv_Atten = "Cons",
                cut = "3|4")
grid_expl <- ref_grid(clmm_useexp, mode = "exc.prob", at = at_expl)
(emm_expl <- emmeans(grid_expl, specs = pairwise ~ Expl, by = "BackgrAvTraining"))
plot_expl <- summary(emm_expl$emmeans)
cont_expl <- summary(emm_expl$contrasts)


## ---- 4) DRAWING OF FIGURE ----

## Setup
if (Export) png(filename = FileName, width = FigWidth, height = FigHeight, units = "cm", res = 300, pointsize = 6)
par(mfrow=c(1,2))
par(mar=c(5.1,4.1,4.1,2.1))

ColJarL1 <- "#1f78b4"
ColJarL2 <- "#a6cee3"
ColJarM1 <- "#33a02c"
ColJarM2 <- "#b2df8a"
ColExpN1  <- "#6a3d9a"
ColExpN2  <- "#cab2d6"
ColExpA1 <- "#ff7f00"
ColExpA2 <- "#fdbf6f"
Offset <- 0.1


## ---- Left panel: Effect of Jargon ----

## More Jargon
XAxis1 <- c(1:4) - Offset
Index <- c(1, 3, 5, 7)
ExcProp1 <- plot_jarg$exc.prob[Index]*100
ConfIntL1 <- plot_jarg$asymp.LCL[Index]*100
ConfIntU1 <- plot_jarg$asymp.UCL[Index]*100

plot(XAxis1, ExcProp1, ylab = "Probability of selecting 'Very/Extremely useful'", 
     las = 1, ylim = YLim, xlim = c(0.5, 4.5), xaxt = "n", xlab = "Levels of avalanche training", main = "a) Amount of Jargon")
axis(1, at = c(1:4), labels = c("None", "Intro", "Adv", "Prof"))
grid()
box()
arrows(XAxis1, y0 = ConfIntL1, y1 = ConfIntU1, angle = 90, code = 3, lwd = LineWidth, col = ColJarM2, length = 0.05)
lines(XAxis1, ExcProp1, lty = 2, lwd = LineWidth, col = ColJarM1)
points(XAxis1, ExcProp1, pch = 21, bg = ColJarM1, cex = SymbolSize)

## Less Jargon
XAxis2 <- c(1:4) + Offset
Index <- c(2, 4, 6, 8)
ExcProp2 <- plot_jarg$exc.prob[Index]*100
ConfIntL2 <- plot_jarg$asymp.LCL[Index]*100
ConfIntU2 <- plot_jarg$asymp.UCL[Index]*100

arrows(XAxis2, y0 = ConfIntL2, y1 = ConfIntU2, angle = 90, code = 3, lwd = LineWidth, col = ColJarL2, length = 0.05)
lines(XAxis2, ExcProp2, lty = 2, lwd = LineWidth, col = ColJarL1)
points(XAxis2, ExcProp2, pch = 21, bg = ColJarL1, cex = SymbolSize)

## Adding labels
text(XAxis1, ExcProp1, labels = format(round(ExcProp1, 1), nsmall = 1), adj = 0.5, pos = c(1, 1, 1, 1), offset = 1, cex = 1, font = 2)
text(XAxis2, ExcProp2, labels = format(round(ExcProp2, 1), nsmall = 1), adj = 0.5, pos = c(3, 3, 3, 3), offset = 1, cex = 1, font = 2)

## Adding significance marker
for (i in 1:nrow(cont_jarg)) {
  y <- (plot_jarg$exc.prob[i*2-1] + plot_jarg$exc.prob[i*2])/2
  if (cont_jarg$p.value[i] < 0.01) {
    points(i, y*100, pch = 8, bg = "black", lwd = LineWidthSign)
  } else if (cont_jarg$p.value[i] < 0.05) {
    points(i, y*100, pch = 4, bg = "black", lwd = LineWidthSign)
  } else if (cont_jarg$p.value[i] < 0.1) {
    points(i, y*100, pch = 3, bg = "black", lwd = LineWidthSign)
  }
}

## Legend
legend("bottom", c("More Jargon  ", "Less Jargon"), pch = 21, pt.bg = c(ColJarM1, ColJarL1), pt.cex = SymbolSize, bty = "n", horiz = T,
       x.intersp = 1.5, inset = 0.02)


## ---- Right panel: Effect of explanation ----

## No explanation
XAxis <- c(1:4) - Offset
Index <- c(1, 3, 5, 7)
ExcProp1 <- plot_expl$exc.prob[Index]*100
ConfIntL1 <- plot_expl$asymp.LCL[Index]*100
ConfIntL1[ConfIntL1 < 0] <- 0
ConfIntU1 <- plot_expl$asymp.UCL[Index]*100
ConfIntU1[ConfIntU1 > 100] <- 100

plot(XAxis1, ExcProp1, ylab = "Probability of selecting 'Very/Extremely useful'", 
     las = 1, ylim = YLim, xlim = c(0.5, 4.5), xaxt = "n", xlab = "Levels of avalanche training", main = "b) Added Explanation")
axis(1, at = c(1:4), labels = c("None", "Intro", "Adv", "Prof"))
grid()
box()
arrows(XAxis1, y0 = ConfIntL1, y1 = ConfIntU1, angle = 90, code = 3, lwd = LineWidth, col = ColExpN2, length = 0.05)
lines(XAxis1, ExcProp1, lty = 2, lwd = LineWidth, col = ColExpN1)
points(XAxis1, ExcProp1, pch = 21, bg = ColExpN1, cex = SymbolSize)

## Added explanation
XAxis2 <- c(1:4) + Offset
Index <- c(2, 4, 6, 8)
ExcProp2 <- plot_expl$exc.prob[Index]*100
ConfIntL2 <- plot_expl$asymp.LCL[Index]*100
ConfIntL2[ConfIntL2 < 0] <- 0
ConfIntU2 <- plot_expl$asymp.UCL[Index]*100
ConfIntU2[ConfIntU2 > 100] <- 100

arrows(XAxis2, y0 = ConfIntL2, y1 = ConfIntU2, angle = 90, code = 3, lwd = LineWidth, col = ColExpA2, length = 0.05)
lines(XAxis2, ExcProp2, lty = 2, lwd = LineWidth, col = ColExpA1)
points(XAxis2, ExcProp2, pch = 21, bg = ColExpA1, cex = SymbolSize)

## Adding labels
text(XAxis1, ExcProp1, labels = format(round(ExcProp1, 1), nsmall = 1), adj = 0.5, pos = c(1, 1, 1, 1), offset = 1, cex = 1, font = 2)
text(XAxis2, ExcProp2, labels = format(round(ExcProp2, 1), nsmall = 1), adj = 0.5, pos = c(3, 3, 3, 3), offset = 1, cex = 1, font = 2)

## Adding significance marker
for (i in 1:nrow(cont_expl)) {
  y <- (plot_expl$exc.prob[i*2-1] + plot_expl$exc.prob[i*2])/2
  if (cont_expl$p.value[i] < 0.01) {
    points(i, y*100, pch = 8, bg = "black", lwd = LineWidthSign)
  } else if (cont_expl$p.value[i] < 0.05) {
    points(i, y*100, pch = 4, bg = "black", lwd = LineWidthSign)
  } else if (cont_expl$p.value[i] < 0.1) {
    points(i, y*100, pch = 3, bg = "black", lwd = LineWidthSign)
  }
}

## Legend
legend("bottom", c("No Expl.  ", "Added Expl."), pch = 21, pt.bg = c(ColExpN1, ColExpA1), pt.cex = SymbolSize, bty = "n", horiz = T,
       x.intersp = 1.5, inset = 0.02)
box()

## Finish and reset
if (Export) dev.off()

par(mfrow=c(1,1))
par(mar=c(5.1,4.1,4.1,2.1))