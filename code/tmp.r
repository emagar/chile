# LOGIT ON WHETHER REPORT PROB AFFECTED BY URGENCY OF BILLS REFERRED TO any committee
# which boletines referred to any committee (retained to keep code structure like Hda version above)
sel <- 1:nrow(bills$info)
target <- bills$info$bol[sel]; #unnecesssary target <- target[duplicated(target)==FALSE]
##drop? billsHda <- bills$info[sel,] # subset bills referred to Hacienda (info element only)
#
# select relevant reports
sel <- which(allRep$bol %in% target)
repsAll <- allRep[sel,]
#
# select relevant urgencia chains
sel <- which(allUrg$chains$bol %in% target)
chainsAll <- allUrg$chains[sel,]
chainsAll[1,]
#
# create DVs
chainsAll$danyReportwiDeadline <- 0 # will receive info
chainsAll$dhdaReportwiDeadline <- 0 # will receive info
for (i in 1:length(target)){ # loop over boletines
    #i <- 1 # debug
    message(sprintf("loop %s of %s", i, length(target)))
    sel <- which(repsAll$bol %in% target[i])
    tmpRep <- repsAll[sel,]
    sel <- which(chainsAll$bol %in% target[i])
    tmpCh <- chainsAll[sel,]
    for (j in 1:nrow(tmpCh)){ # loop over chains
        #j <- 1 # debug
        hit <- which(tmpRep$date > tmpCh$on[j] & tmpRep$date <= tmpCh$deadline[j] & tmpRep$chamber==tmpCh$tramite[j]) # report came within deadline in same chamber
        if (length(hit)>0) tmpCh$danyReportwiDeadline <- 1
        if (sum(tmpRep$dHda[hit])>0) tmpCh$dhdaReportwiDeadline <- 1
    }
    chainsAll$danyReportwiDeadline[sel] <- tmpCh$danyReportwiDeadline
    chainsAll$dhdaReportwiDeadline[sel] <- tmpCh$dhdaReportwiDeadline
}
table(chainsAll$danyReportwiDeadline)
table(chainsAll$dhdaReportwiDeadline)
#
# create IVs
chainsAll$dactNow <- as.numeric(chainsAll$typeN==1 | chainsAll$typeN==1.4 | chainsAll$typeN==1.45 | chainsAll$typeN==1.5)
chainsAll$d2wk <-    as.numeric(chainsAll$typeN==2 | chainsAll$typeN==2.4 | chainsAll$typeN==2.45 | chainsAll$typeN==2.5)
chainsAll$d4wk <-    as.numeric(chainsAll$typeN==3 | chainsAll$typeN==3.4 | chainsAll$typeN==3.45 | chainsAll$typeN==3.5)
chainsAll$dextend <- as.numeric(chainsAll$typeN==1.4 | chainsAll$typeN==1.45 | chainsAll$typeN==2.4 | chainsAll$typeN==2.45 | chainsAll$typeN==3.4 | chainsAll$typeN==3.45)
chainsAll$dwithdr <- as.numeric(chainsAll$typeN==1.45 | chainsAll$typeN==1.5 | chainsAll$typeN==2.45 | chainsAll$typeN==2.5 | chainsAll$typeN==3.45 | chainsAll$typeN==3.5)
#
chainsAll$dmocion <- 0
chainsAll$pctcon <- 0
chainsAll$pctright <- 0
chainsAll$dom <- "."
for(i in 1:nrow(chainsAll)){
    #i <- 1 # debug
    message(sprintf("loop %s of %s", i, nrow(chainsAll)))
    target <- which(bills$info$bol %in% chainsAll$bol[i])
    chainsAll$dmocion[i] <- 1-bills$info$dmensaje[target]
    chainsAll$pctcon[i] <- bills$info$pctcon[target]
    chainsAll$pctright[i] <- bills$info$pctright[target]
    chainsAll$dom[i] <- bills$info$dom[target]
    # para versión con todos las comisiones, hay bills$info$drefAll
}
chainsAll$pctpdt <- chainsAll$pctcon; sel <- which(chainsAll$on >= dmy("11-3-2010", tz = "chile")); chainsAll$pctpdt[sel] <- chainsAll$pctright[sel]
chainsAll$dmocionAllOpp <- chainsAll$dmocionMix <- chainsAll$dmocionAllPdt <- chainsAll$dmocion
chainsAll$dmocionAllOpp[chainsAll$pctpdt>0] <- 0
chainsAll$dmocionAllPdt[chainsAll$pctpdt<100] <- 0
chainsAll$dmocionMix[chainsAll$pctpdt==0 | chainsAll$pctpdt==100] <- 0
#
# Add president's maj status in chamber
chainsAll$dmajDip <- chainsAll$dmajSen <- 0
# sen
tmp <- dmy(c("11-03-1990", "22-01-1999", "11-03-2000", "11-01-2002", "27-01-2005", "30-08-2005", "11-03-2006", "11-03-2010") , tz = "chile")
sel <- which(chainsAll$on >= tmp[1] & chainsAll$on < tmp[2]); chainsAll$dmajSen[sel] <- 0
sel <- which(chainsAll$on >= tmp[2] & chainsAll$on < tmp[3]); chainsAll$dmajSen[sel] <- 1 # tie coded as maj for pdt
sel <- which(chainsAll$on >= tmp[3] & chainsAll$on < tmp[4]); chainsAll$dmajSen[sel] <- 1 
sel <- which(chainsAll$on >= tmp[4] & chainsAll$on < tmp[5]); chainsAll$dmajSen[sel] <- 1 # tie coded as maj for pdt
sel <- which(chainsAll$on >= tmp[5] & chainsAll$on < tmp[6]); chainsAll$dmajSen[sel] <- 0
sel <- which(chainsAll$on >= tmp[6] & chainsAll$on < tmp[7]); chainsAll$dmajSen[sel] <- 1 # tie coded as maj for pdt
sel <- which(chainsAll$on >= tmp[7] & chainsAll$on < tmp[8]); chainsAll$dmajSen[sel] <- 1
sel <- which(chainsAll$on >= tmp[8] & chainsAll$on < tmp[9]); chainsAll$dmajSen[sel] <- 0
#
# dip: always maj=1 (2010-14 50%, coded 1)
chainsAll$dmajDip <- 1
#
# to end of pdtl term
tmp <- dmy(c("11-03-1994", "11-03-2000", "11-03-2006", "11-03-2010", "11-03-2014") , tz = "chile")
chainsAll$pterm <- NA
sel <- which(                          chainsAll$on < tmp[1]); chainsAll$pterm[sel] <- round((tmp[1] - chainsAll$on[sel]) * 100 / (365*4), digits = 0)
sel <- which(chainsAll$on >= tmp[1] & chainsAll$on < tmp[2]); chainsAll$pterm[sel] <- round((tmp[2] - chainsAll$on[sel]) * 100 / (365*6), digits = 0)
sel <- which(chainsAll$on >= tmp[2] & chainsAll$on < tmp[3]); chainsAll$pterm[sel] <- round((tmp[3] - chainsAll$on[sel]) * 100 / (365*6), digits = 0)
sel <- which(chainsAll$on >= tmp[3] & chainsAll$on < tmp[4]); chainsAll$pterm[sel] <- round((tmp[4] - chainsAll$on[sel]) * 100 / (365*4), digits = 0)
sel <- which(chainsAll$on >= tmp[4] & chainsAll$on < tmp[5]); chainsAll$pterm[sel] <- round((tmp[5] - chainsAll$on[sel]) * 100 / (365*4), digits = 0)
#
# to end of dip term
tmp <- dmy(c("11-03-1994", "11-03-1998", "11-03-2002", "11-03-2006", "11-03-2010", "11-03-2014") , tz = "chile")
chainsAll$dterm <- NA
sel <- which(                            chainsAll$on < tmp[1]); chainsAll$dterm[sel] <- round((tmp[1] - chainsAll$on[sel]) * 100 / (365*4), digits = 0)
sel <- which(chainsAll$on >= tmp[1] & chainsAll$on < tmp[2]); chainsAll$dterm[sel] <- round((tmp[2] - chainsAll$on[sel]) * 100 / (365*4), digits = 0)
sel <- which(chainsAll$on >= tmp[2] & chainsAll$on < tmp[3]); chainsAll$dterm[sel] <- round((tmp[3] - chainsAll$on[sel]) * 100 / (365*4), digits = 0)
sel <- which(chainsAll$on >= tmp[3] & chainsAll$on < tmp[4]); chainsAll$dterm[sel] <- round((tmp[4] - chainsAll$on[sel]) * 100 / (365*4), digits = 0)
sel <- which(chainsAll$on >= tmp[4] & chainsAll$on < tmp[5]); chainsAll$dterm[sel] <- round((tmp[5] - chainsAll$on[sel]) * 100 / (365*4), digits = 0)
sel <- which(chainsAll$on >= tmp[5] & chainsAll$on < tmp[6]); chainsAll$dterm[sel] <- round((tmp[6] - chainsAll$on[sel]) * 100 / (365*4), digits = 0)
#
# to end of sen term
tmp <- dmy(c("11-03-1994", "11-03-1998", "11-03-2006", "11-03-2014") , tz = "chile")
chainsAll$sterm <- NA
sel <- which(                          chainsAll$on < tmp[1]); chainsAll$sterm[sel] <- round((tmp[1] - chainsAll$on[sel]) * 100 / (365*8), digits = 0)
sel <- which(chainsAll$on >= tmp[1] & chainsAll$on < tmp[2]); chainsAll$sterm[sel] <- round((tmp[2] - chainsAll$on[sel]) * 100 / (365*8), digits = 0)
sel <- which(chainsAll$on >= tmp[2] & chainsAll$on < tmp[3]); chainsAll$sterm[sel] <- round((tmp[3] - chainsAll$on[sel]) * 100 / (365*8), digits = 0)
sel <- which(chainsAll$on >= tmp[3] & chainsAll$on < tmp[4]); chainsAll$sterm[sel] <- round((tmp[4] - chainsAll$on[sel]) * 100 / (365*8), digits = 0)
#
# to end of leg year
tmp <- dmy(c("10-03-1999", "10-03-2000", "10-03-2001", "10-03-2002", "10-03-2003", "10-03-2004", "10-03-2005", "10-03-2006", "10-03-2007", "10-03-2008", "10-03-2009", "10-03-2010", "10-03-2011", "10-03-2012", "10-03-2013", "10-03-2014") , tz = "chile")
chainsAll$legyr <- NA
sel <- which(                           chainsAll$on < tmp[1]);  chainsAll$legyr[sel] <- round((tmp[1]  - chainsAll$on[sel]) * 100 / 365, digits = 0)
sel <- which(chainsAll$on >= tmp[1]  & chainsAll$on < tmp[2]);  chainsAll$legyr[sel] <- round((tmp[2]  - chainsAll$on[sel]) * 100 / 365, digits = 0)
sel <- which(chainsAll$on >= tmp[2]  & chainsAll$on < tmp[3]);  chainsAll$legyr[sel] <- round((tmp[3]  - chainsAll$on[sel]) * 100 / 365, digits = 0)
sel <- which(chainsAll$on >= tmp[3]  & chainsAll$on < tmp[4]);  chainsAll$legyr[sel] <- round((tmp[4]  - chainsAll$on[sel]) * 100 / 365, digits = 0)
sel <- which(chainsAll$on >= tmp[4]  & chainsAll$on < tmp[5]);  chainsAll$legyr[sel] <- round((tmp[5]  - chainsAll$on[sel]) * 100 / 365, digits = 0)
sel <- which(chainsAll$on >= tmp[5]  & chainsAll$on < tmp[6]);  chainsAll$legyr[sel] <- round((tmp[6]  - chainsAll$on[sel]) * 100 / 365, digits = 0)
sel <- which(chainsAll$on >= tmp[6]  & chainsAll$on < tmp[7]);  chainsAll$legyr[sel] <- round((tmp[7]  - chainsAll$on[sel]) * 100 / 365, digits = 0)
sel <- which(chainsAll$on >= tmp[7]  & chainsAll$on < tmp[8]);  chainsAll$legyr[sel] <- round((tmp[8]  - chainsAll$on[sel]) * 100 / 365, digits = 0)
sel <- which(chainsAll$on >= tmp[8]  & chainsAll$on < tmp[9]);  chainsAll$legyr[sel] <- round((tmp[9]  - chainsAll$on[sel]) * 100 / 365, digits = 0)
sel <- which(chainsAll$on >= tmp[9]  & chainsAll$on < tmp[10]); chainsAll$legyr[sel] <- round((tmp[10] - chainsAll$on[sel]) * 100 / 365, digits = 0)
sel <- which(chainsAll$on >= tmp[10] & chainsAll$on < tmp[11]); chainsAll$legyr[sel] <- round((tmp[11] - chainsAll$on[sel]) * 100 / 365, digits = 0)
sel <- which(chainsAll$on >= tmp[11] & chainsAll$on < tmp[12]); chainsAll$legyr[sel] <- round((tmp[12] - chainsAll$on[sel]) * 100 / 365, digits = 0)
sel <- which(chainsAll$on >= tmp[12] & chainsAll$on < tmp[13]); chainsAll$legyr[sel] <- round((tmp[13] - chainsAll$on[sel]) * 100 / 365, digits = 0)
sel <- which(chainsAll$on >= tmp[13] & chainsAll$on < tmp[14]); chainsAll$legyr[sel] <- round((tmp[14] - chainsAll$on[sel]) * 100 / 365, digits = 0)
sel <- which(chainsAll$on >= tmp[14] & chainsAll$on < tmp[15]); chainsAll$legyr[sel] <- round((tmp[15] - chainsAll$on[sel]) * 100 / 365, digits = 0)
sel <- which(chainsAll$on >= tmp[15] & chainsAll$on < tmp[16]); chainsAll$legyr[sel] <- round((tmp[16] - chainsAll$on[sel]) * 100 / 365, digits = 0)
#
# legislature dummies (periodo)
tmp <- dmy(c("11-03-1994", "11-03-1998", "11-03-2002", "11-03-2006", "11-03-2010", "11-03-2014") , tz = "chile")
chainsAll$dleg90 <- chainsAll$dleg94 <- chainsAll$dleg98 <- chainsAll$dleg02 <- chainsAll$dleg06 <- chainsAll$dleg10 <- 0
chainsAll$legis <- 0
sel <- which(                         chainsAll$on < tmp[1]); chainsAll$dleg90[sel] <- 1; chainsAll$legis[sel] <- 1990
sel <- which(chainsAll$on >= tmp[1] & chainsAll$on < tmp[2]); chainsAll$dleg94[sel] <- 1; chainsAll$legis[sel] <- 1994
sel <- which(chainsAll$on >= tmp[2] & chainsAll$on < tmp[3]); chainsAll$dleg98[sel] <- 1; chainsAll$legis[sel] <- 1998
sel <- which(chainsAll$on >= tmp[3] & chainsAll$on < tmp[4]); chainsAll$dleg02[sel] <- 1; chainsAll$legis[sel] <- 2002
sel <- which(chainsAll$on >= tmp[4] & chainsAll$on < tmp[5]); chainsAll$dleg06[sel] <- 1; chainsAll$legis[sel] <- 2006
sel <- which(chainsAll$on >= tmp[5] & chainsAll$on < tmp[6]); chainsAll$dleg10[sel] <- 1; chainsAll$legis[sel] <- 2010
#
table(chainsAll$legis) # hay dos cadenas iniciadas el 10-3-1998... ¿cómo se han colado?
#
# bill introduced after ley orgánica relaxed urgency deadlines
chainsAll$dreform2010 <- 0
sel <- which(chainsAll$on >= dmy("1-7-2010", tz = "chile")); chainsAll$dreform2010[sel] <- 1
#
# chain in senate
chainsAll$dsen <- as.numeric(chainsAll$tramite=="sen")
#
fit1 <- glm(danyReportwiDeadline ~ d2wk + d4wk + dextend + dwithdr + dmocion                                    + dmajSen + dsen + pterm + legyr + dreform2010                   , data = chainsAll, family = binomial(link = logit))
fit2 <- glm(danyReportwiDeadline ~ d2wk + d4wk + dextend + dwithdr + dmocionAllPdt + dmocionMix + dmocionAllOpp + dmajSen + dsen + pterm + legyr + dreform2010, data = chainsAll, family = binomial(link = logit))
fit3 <- glm(danyReportwiDeadline ~ d2wk + d4wk + dextend + dwithdr + dmocionAllPdt + dmocionMix + dmocionAllOpp + dmajSen + dsen + pterm + legyr + dreform2010 + as.factor(legis), data = chainsAll, family = binomial(link = logit))
fit4 <- glm(danyReportwiDeadline ~ as.factor(typeN)                + dmocion                                    + dmajSen + dsen + pterm + legyr + dreform2010, data = chainsAll, family = binomial(link = logit))
fit5 <- glm(danyReportwiDeadline ~ as.factor(typeN)                + dmocionAllPdt + dmocionMix + dmocionAllOpp + dmajSen + dsen + pterm + legyr + dreform2010, data = chainsAll, family = binomial(link = logit))
## library(Zelig) # for clustered errors ### NOT WORKING, SEs IDENTICAL
## fit6 <- zelig(danyReportwiDeadline ~ as.factor(typeN) + dmocionAllPdt + dmocionMix + dmocionAllOpp + dmajSen + dsen + pterm + legyr + dreform2010, data = chainsAll, model = "logit", robust = TRUE, cluster = "legis")
#
summary(fit1)
round(table(chainsAll$typeN)/length(chainsAll$typeN), 2)
#
pred1 <- predict(fit1, type = "response"); pred1[pred1>=.5] <- 1; pred1[pred1<.5] <- 0
table(chainsAll$danyReportwiDeadline - pred1) / nrow(chainsHda) # pct correctly predicted
pred2 <- predict(fit2, type = "response"); pred2[pred2>=.5] <- 1; pred2[pred2<.5] <- 0
table(chainsAll$danyReportwiDeadline - pred2) / nrow(chainsHda) # pct correctly predicted
pred3 <- predict(fit3, type = "response"); pred3[pred3>=.5] <- 1; pred3[pred3<.5] <- 0
table(chainsAll$danyReportwiDeadline - pred3) / nrow(chainsHda) # pct correctly predicted
# export to latex
library(stargazer)
stargazer(fit2, fit3, title="Regression results", align=TRUE,
          covariate.labels=
 c("Act now, change deadline",
   "Act now, change deadline, withdraw",
   "Act now, withdraw",
   "2 week",
   "2 week, change deadline",
   "2 week, change deadline, withdraw",
   "2 week, withdraw",
   "4 week",
   "4 week, change deadline",
   "4 week, change deadline, withdraw",
   "4 week, withdraw",
   "Member bill",
   "Member bill, pres. coal.-sp.",
   "Member bill, mix.-sponsored",
   "Member bill, opp.-sponsored",
   "Senate majority",
   "Chain in Senate",
   "Pres.~term remaining",
   "Year remaining",
   "Post relax",
   "Constant")
          )
#
# periodize table
sel <- which(chainsAll$dleg98==1); round(table(chainsAll$typeN[sel])*100/length(chainsAll$typeN[sel]), 0); length(chainsAll$typeN[sel])
sel <- which(chainsAll$dleg02==1); round(table(chainsAll$typeN[sel])*100/length(chainsAll$typeN[sel]), 0); length(chainsAll$typeN[sel])
sel <- which(chainsAll$dleg06==1); round(table(chainsAll$typeN[sel])*100/length(chainsAll$typeN[sel]), 0); length(chainsAll$typeN[sel])
sel <- which(chainsAll$dleg10==1); round(table(chainsAll$typeN[sel])*100/length(chainsAll$typeN[sel]), 0); length(chainsAll$typeN[sel])


