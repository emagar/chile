summary(fit4)

sims4 <- with(tmpdat,
              data.frame(dsamePty=c(0,1),
                         dmultiRef=0,
#                         dmocion= 0,
                         dmocionAllOpp= 0,
                         dmocionMix   = 0,
                         dmocionAllPdt= 0,
                         drefHda=1,
#                         dmajSen=1,
                         dinSen=0,
                         legyrR=seq(from=(min(legyrR)-.05), to=(max(legyrR)+.05), length.out = 100),
#                         dreform2010=0,
                         netApprovR=median(netApprovR),
                         legis = 2010
                         )
              )
sims4$pr <- predict(fit4, newdata = sims4, type = "response")
sims4 <- cbind(sims4, predict(fit4, newdata = sims4, type="link", se=TRUE))
sims4 <- within(sims4, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
library(ggplot2)
gr <- "../graphs/"
#pdf (file = paste(gr, "predictedPr.pdf", sep = ""), width = 7, height = 4)
ggplot(sims3, aes(x = legyrR, y = PredictedProb)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = factor(dsamePty)), alpha = .2) +
  geom_line(aes(colour = factor(dsamePty)), size=1)
#dev.off()








## ESTE SIRVE DE EJEMPLO PARA LA CONFERENCIA DE CHUCHO---LO COMENTADO FUE LO PRIMERO, POCO A POCO VI QUE FORMULA ERA MUCHO MAS SENCILLA
## Corre justo arriba del logit, en chilbill.r
allCom.tmp <- list()
for (i in 1:I){
    message(sprintf("iteración %s of %s", i, I))
    #i <- 1 # debug
    tmp <- bills$hitos[[i]]$action[grep(".*[Pp]asa a [Cc]omisi[óo]n.*", bills$hitos[[i]]$action)]
    #tmp <- bills$hitos[[i]]$action[grep(".*[Cc]omisi[óo]n.*", bills$hitos[[i]]$action)] # versión más laxa, demasiado permisiva
    allCom.tmp[[i]] <- tmp
    #if (length(grep("[Hh]acienda", tmp))>0) bills$info$drefHda[i] <- 1
}
allCom.tmp <- unlist(allCom.tmp)
allCom.tmp <- gsub(pattern = "^(?:.*)Pasa a (?:la )*", replacement = "", allCom.tmp, ignore.case = TRUE)
## allCom.tmp <- gsub(pattern = "Ingreso de proyecto. (?:Cuenta:|Pasa a) ", replacement = "", allCom.tmp, ignore.case = TRUE)
## allCom.tmp <- gsub(pattern = "Cuent*a.*proyecto.+Pasa a ", replacement = "", allCom.tmp, ignore.case = TRUE)
## allCom.tmp <- gsub(pattern = "Cuenta.+Pasa a ", replacement = "", allCom.tmp, ignore.case = TRUE)
## allCom.tmp <- gsub(pattern = "Desarchivado.+Pasa a ", replacement = "", allCom.tmp, ignore.case = TRUE)
## allCom.tmp <- gsub(pattern = "Eximido del trámite ante Comisión.+Pasa a ", replacement = "", allCom.tmp, ignore.case = TRUE)
## allCom.tmp <- gsub(pattern = "(?:Nuevo p|P)rimer informe (?:de comisión|complementario). *(?:Cuenta de informe(?: de Comisión)*. *)*Pasa a ", replacement = "", allCom.tmp, ignore.case = TRUE)
## allCom.tmp <- gsub(pattern = "(?:Nuevo s|S)egundo informe (?:de comisión|complementario). (?:. )*Pasa a ", replacement = "", allCom.tmp, ignore.case = TRUE)
## allCom.tmp <- gsub(pattern = "Por acuerdo de la Sala pasa a ", replacement = "", allCom.tmp, ignore.case = TRUE)
## allCom.tmp <- gsub(pattern = "Nuevo informe de comisión. Pasa a ", replacement = "", allCom.tmp, ignore.case = TRUE)
## allCom.tmp <- gsub(pattern = "Oficio rechazo modificaciones a.+pasa a ", replacement = "", allCom.tmp, ignore.case = TRUE)
## allCom.tmp <- gsub(pattern = "Informe de comisión. Pasa a", replacement = "", allCom.tmp, ignore.case = TRUE)
## allCom.tmp <- gsub(pattern = "Discusión (?:general|particular).+pasa a ", replacement = "", allCom.tmp, ignore.case = TRUE)
## allCom.tmp <- gsub(pattern = "Discusión única. Rechazadas las modificaciones Pasa a", replacement = "", allCom.tmp, ignore.case = TRUE)
## allCom.tmp <- gsub(pattern = "Discusión única. Se aprueban unas y rechazan otras modificaciones de la Cámara. Pasa a", replacement = "", allCom.tmp, ignore.case = TRUE)
## allCom.tmp <- gsub(pattern = "^(?:. )*Pasa a ", replacement = "", allCom.tmp)

table(allCom.tmp)
head(allCom.tmp)

table(bills$info$drefHda)

summary(bills$info)

