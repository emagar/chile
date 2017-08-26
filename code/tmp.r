# Average marginal effects
# https://www.r-bloggers.com/probitlogit-marginal-effects-in-r/
mfxboot <- function(modform,dist,data,boot=1000,digits=3){
  x <- glm(modform, family=binomial(link=dist),data)
  # get marginal effects
  pdf <- ifelse(dist=="probit",
                mean(dnorm(predict(x, type = "link"))),
                mean(dlogis(predict(x, type = "link"))))
  marginal.effects <- pdf*coef(x)
  # start bootstrap
  bootvals <- matrix(rep(NA,boot*length(coef(x))), nrow=boot)
  set.seed(1111)
  for(i in 1:boot){
    samp1 <- data[sample(1:dim(data)[1],replace=T,dim(data)[1]),]
    x1 <- glm(modform, family=binomial(link=dist),samp1)
    pdf1 <- ifelse(dist=="probit",
                   mean(dnorm(predict(x, type = "link"))),
                   mean(dlogis(predict(x, type = "link"))))
    bootvals[i,] <- pdf1*coef(x1)
  }
  res <- cbind(marginal.effects,apply(bootvals,2,sd),marginal.effects/apply(bootvals,2,sd))
  if(names(x$coefficients[1])=="(Intercept)"){
    res1 <- res[2:nrow(res),]
    res2 <- matrix(as.numeric(sprintf(paste("%.",paste(digits,"f",sep=""),sep=""),res1)),nrow=dim(res1)[1])     
    rownames(res2) <- rownames(res1)
    } else {
    res2 <- matrix(as.numeric(sprintf(paste("%.",paste(digits,"f",sep=""),sep="")),nrow=dim(res)[1]))
    rownames(res2) <- rownames(res)
    }
  colnames(res2) <- c("marginal.effect","standard.error","z.ratio")  
  return(res2)
}


library(AER)
data(SwissLabor)
mfx1 <- mfxboot(participation ~ . + I(age^2),"probit",SwissLabor)
mfx2 <- mfxboot(participation ~ . + I(age^2),"logit",SwissLabor)
mfx3 <- mfxboot(participation ~ . + I(age^2),"probit",SwissLabor,boot=100,digits=4)
 
mfxdat <- data.frame(cbind(rownames(mfx1),mfx1))
mfxdat$me <- as.numeric(as.character(mfxdat$marginal.effect))
mfxdat$se <- as.numeric(as.character(mfxdat$standard.error))

mfx1

# coefplot
library(ggplot2)
ggplot(mfxdat, aes(V1, marginal.effect,ymin = me - 2*se,ymax= me + 2*se)) +
  scale_x_discrete('Variable') +
  scale_y_continuous('Marginal Effect',limits=c(-0.5,1)) +
  theme_bw() + 
  geom_errorbar(aes(x = V1, y = me),size=.3,width=.2) + 
  geom_point(aes(x = V1, y = me)) +
  geom_hline(yintercept=0) + 
  coord_flip()
## +
##   opts(title="Marginal Effects with 95% Confidence Intervals")

