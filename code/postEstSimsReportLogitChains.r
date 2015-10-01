## Intenté sacar simulaciones del logit de reported en cadenas. Lo hice con Zelig y los CIs se empalman gacho. Me quedé a medias
## de una versión MCMC, que no salió paero parecía llegar a la misma conclusión...

## Zelig simulation of model 1 ---> needs to be able to run fit1 in chillbill.r
library(Zelig)
z.fit1 <- zelig(dhdaReportwiDeadline ~ d2wk + d4wk + dextend + dwithdr + dmocion + dmajSen + dsen + ptermR + legyrR + dreform2010                   , data = chainsHda, model = "logit")
#
X <- data.frame(x = seq(from = 0, to = 4, by = .05)) # will receive sim data for plot
X$hi <- X$mid <- X$lo <- NA

pterm <- 100 -  X$x*100/4; pterm <- (pterm - attr(chainsHda$ptermR, which="scaled:center"))/attr(chainsHda$ptermR, which="scaled:scale")
legyr <- 100 - (X$x-as.integer(X$x))*100; legyr[length(legyr)] <- 0; legyr <- (legyr - attr(chainsHda$legyrR, which="scaled:center"))/attr(chainsHda$legyrR, which="scaled:scale")


for (i in 1:nrow(X)){
    #i <- 1 # debug
    message(sprintf("loop %s of %s", i, nrow(X)))
    # compute scaled term and year to input into equation
#    pterm <- 100 -  X$x[i]*100/4; pterm = (pterm - attr(chainsHda$ptermR, which="scaled:center"))/attr(chainsHda$ptermR, which="scaled:scale")
#    legyr <- 100 - (X$x[i]-as.integer(X$x[i]))*100; legyr = (pterm - attr(chainsHda$legyrR, which="scaled:center"))/attr(chainsHda$legyrR, which="scaled:scale")
    a <- pterm[i];
    b <- legyr[i];
    scen <- setx(z.fit1, 
            d2wk=1,
            d4wk=0,
            dextend=1,
            dwithdr=0,
            dmocion=1,
            dmajSen=0,
            dsen=1,
            ptermR=a,
            legyrR=b, 
            dreform2010=0
                 )
    out <- sim(z.fit1, x = scen, num=50000)
    out$qi[[1]]
    # Fill in predicted probabilities
    X$lo[i] <-  quantile(out$qi[[1]], .025)  # q.025 sim
    X$mid[i] <- quantile(out$qi[[1]], .5)    # median sim
    X$hi[i] <-  quantile(out$qi[[1]], .975)  # q.975 sim
    ## X$lo[i] <-  simulations$stats[["Expected Values: E(Y|X)"]][4] # q.025 sim
    ## X$mid[i] <- simulations$stats[["Expected Values: E(Y|X)"]][3] # median sim
    ## X$hi[i] <-  simulations$stats[["Expected Values: E(Y|X)"]][5] # q.975 sim
}

exec2wk <- X
memb2wk <- X
memb2wkExt <- X
membAct <- X

plot.ci()

plot(c(min(exec2wk$x), max(exec2wk$x)), c(0,1), type = "n")
## lines(exec2wk$x, exec2wk$hi, col="red")
## lines(exec2wk$x, exec2wk$mid, col="red")
## lines(exec2wk$x, exec2wk$lo, col="red")
lines(memb2wkExt$x, memb2wkExt$hi, col="blue")
lines(memb2wkExt$x, memb2wkExt$mid, col="blue")
lines(memb2wkExt$x, memb2wkExt$lo, col="blue")
lines(membAct$x, membAct$hi, col="red")
lines(membAct$x, membAct$mid, col="red")
lines(membAct$x, membAct$lo, col="red")
    
########################
### BAYESIAN VERSION ###
########################
#
### Packages for JAGS
library(R2jags)
### JAGS/BUGS models
model1 <- function(){
    for (i in 1:I){
        dhdaReportwiDeadline[i] ~ dbin (p[i], 1);
        logit(p[i]) <- b.cons*cons[i] +
                       b.d2wk*d2wk[i] +
                       b.d4wk*d4wk[i] +
                       b.dextend*dextend[i] +
                       b.dwithdr*dwithdr[i] +
                       b.dmocion*dmocion[i] +
                       b.dmajSen*dmajSen[i] +
                       b.dsen*dsen[i] +
                       b.ptermR*ptermR[i] +
                       b.legyrR*legyrR[i] +
                       b.dreform2010*dreform2010[i];
    }
    ## NON-INFORMATIVE PRIORS ##
    b.cons        ~ dnorm (0,0.01);
    b.d2wk        ~ dnorm (0,0.01);
    b.d4wk        ~ dnorm (0,0.01);
    b.dextend     ~ dnorm (0,0.01);
    b.dwithdr     ~ dnorm (0,0.01);
    b.dmocion     ~ dnorm (0,0.01);
    b.dmajSen     ~ dnorm (0,0.01);
    b.dsen        ~ dnorm (0,0.01);
    b.ptermR      ~ dnorm (0,0.01);
    b.legyrR      ~ dnorm (0,0.01);
    b.dreform2010 ~ dnorm (0,0.01);
}

# data prep
dhdaReportwiDeadline <- chainsHda$dhdaReportwiDeadline
cons <- rep(1, length(dhdaReportwiDeadline))
d2wk <- chainsHda$d2wk
d4wk <- chainsHda$d4wk
dextend <- chainsHda$dextend
dwithdr <- chainsHda$dwithdr
dmocion <- chainsHda$dmocion
dmajSen <- chainsHda$dmajSen
dsen <- chainsHda$dsen
ptermR <- as.vector(chainsHda$pterm)
legyrR <- as.vector(chainsHda$legyr)
dreform2010 <- chainsHda$dreform2010
I <- length(dreform2010); K <- 10

model1.data <- list("dhdaReportwiDeadline", "cons", "d2wk", "d4wk", "dextend", "dwithdr", "dmocion", "dmajSen", "dsen", "ptermR", "legyrR", "dreform2010", "I")
model1.inits <- function(){ list (b.cons=rnorm(1,0,.01), b.d2wk=rnorm(1,0,.1), b.d4wk=rnorm(1,0,.1), b.dextend=rnorm(1,0,.1), b.dwithdr=rnorm(1,0,.1), b.dmocion=rnorm(1,0,.1), b.dmajSen=rnorm(1,0,.1), b.dsen=rnorm(1,0,.1), b.ptermR=rnorm(1,0,.1), b.legyrR=rnorm(1,0,.1), b.dreform2010=rnorm(1,0,.1)) }
model1.parameters <- c("b.cons", "b.d2wk", "b.d4wk", "b.dextend", "b.dwithdr", "b.dmocion", "b.dmajSen", "b.dsen", "b.ptermR", "b.legyrR", "b.dreform2010")

## test ride
tmpRes <- jags(data=model1.data, inits=model1.inits, parameters.to.save = model1.parameters,
                 model.file=model1, n.chains=3,
                 n.iter=100, n.thin=10
                 )
## estimate
Res <- jags (data=model1.data, inits=model1.inits, model1.parameters,
                 model.file=model1, n.chains=3,
                 n.iter=20000, n.thin=10
                 )
rm(dhdaReportwiDeadline, cons, d2wk, d4wk, dextend, dwithdr, dmocion, dmajSen, dsen, ptermR, legyrR, dreform2010, I, K) # clean

## 
X <- data.frame(x = seq(from = 0, to = 4, by = .05)) # will receive sim data for plot
X$hi <- X$mid <- X$lo <- NA
pterm <- 100 -  X$x*100/4; pterm <- (pterm - attr(chainsHda$ptermR, which="scaled:center"))/attr(chainsHda$ptermR, which="scaled:scale")
legyr <- 100 - (X$x-as.integer(X$x))*100; legyr[length(legyr)] <- 0; legyr <- (legyr - attr(chainsHda$legyrR, which="scaled:center"))/attr(chainsHda$legyrR, which="scaled:scale")

for (i in 1:nrow(X)){
    #i <- 1 # debug
    message(sprintf("loop %s of %s", i, nrow(X)))
    a <- pterm[i];
    b <- legyr[i];
    scen <- c(
        cons=1,
        d2wk=1,
        d4wk=0,
        dextend=1,
        dwithdr=0,
        dmocion=1,
        dmajSen=0,
        dsen=1,
        ptermR=a,
        legyrR=b, 
        dreform2010=0
        )
    tmp <- Res$BUGSoutput$sims.matrix[,-12]
    tmp <- tmp[, c("b.cons", "b.d2wk", "b.d4wk", "b.dextend", "b.dwithdr", "b.dmocion", "b.dmajSen", "b.dsen", "b.ptermR", "b.legyrR", "b.dreform2010")] # sort columns
    tmp <- apply(tmp, 1, function(x) invlogit(sum(x*scen)))
    X[i,2:4] <- quantile(tmp, probs = c(.025,.5,.975))
}


plot(c(min(X$x), max(X$x)), c(0,1), type = "n")
lines(X$x, X$hi,  col="blue")
lines(X$x, X$mid, col="blue")
lines(X$x, X$lo,  col="blue")
