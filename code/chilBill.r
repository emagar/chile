rm(list=ls())

# data directory
datdir <- "/home/eric/Dropbox/data/latAm/chile/data/" 
setwd(datdir)

# prepare object with boletin filenames
files <- dir(paste(datdir, "boletines/", sep = "")) # reads existing files from directory
files <- files[grep("bol[0-9]+.*.txt", files)] # keeps only those named bol*.txt in list
files <- paste("boletines/", files, sep = "")
## files <- read.csv("boletines/1id-bl.csv", encoding = 'utf-8', stringsAsFactors = FALSE) # reads universe of filenames
## files <- files$bl
## files <- paste("boletines/bol", files, ".txt", sep = "")
#
# prepare object to receive bill histories
I <- length(files)
bills <- list(
    #info = data.frame(n=1:I), # matrix with basic bill info
    info = NULL,                        # matrix to receive basic bill info
    urgencias = vector("list", I),      # empty list with urgencias info to be systematized
    sponsors = vector("list", I),       # idem
    hitos = vector("list", I),
    votes = vector("list", I),
    veto = vector("list", I),
    reports = vector("list", I)
    )


# loop over files
library(lubridate)
for (i in 1:I){
#    i <- grep("1140", files) # debug: read one boletin
    print(paste("loop", i, "of", I))
    bol <- readLines( files[i], encoding = "utf-8" )
    bol <- gsub(pattern = "[\"]", replacement = "", bol) # some cleaning: removes double quotes inside text
                                        # get its summary info
    start <- grep(pattern = "emmStart Summary", x = bol)
    end <- grep(pattern = "Summary emmEnd", x = bol)
    chunk <- bol[start:end] # selects chunk with summary info
    chunk <- sub(pattern = "emmStart Summary.*,", replacement = "", chunk) # cleans
    chunk <- sub(pattern = ",Summary emmEnd"   , replacement = "", chunk) # cleans
                                        # plug info into bill histories object
    tmp <- chunk[grep("Legislatura:", chunk)]
    bill <- data.frame(leg = sub(pattern = "Leg.*: ([0-9]+)", replacement = "\\1", x = tmp))
                                        #
    tmp <- chunk[grep("Fecha de ingreso:", chunk)]
    tmp <- sub(pattern = "Fecha de ingreso: [a-zA-Z]+ (.*)", replacement = "\\1", x = tmp)
    tmp <- gsub(pattern = " de ", replacement = "-", x = tmp)
    tmp <- gsub(pattern = "enero"     , replacement = "1", x = tmp)
    tmp <- gsub(pattern = "febrero"   , replacement = "2", x = tmp)
    tmp <- gsub(pattern = "marzo"     , replacement = "3", x = tmp)
    tmp <- gsub(pattern = "abril"     , replacement = "4", x = tmp)
    tmp <- gsub(pattern = "mayo"      , replacement = "5", x = tmp)
    tmp <- gsub(pattern = "junio"     , replacement = "6", x = tmp)
    tmp <- gsub(pattern = "julio"     , replacement = "7", x = tmp)
    tmp <- gsub(pattern = "agosto"    , replacement = "8", x = tmp)
    tmp <- gsub(pattern = "septiembre", replacement = "9", x = tmp)
    tmp <- gsub(pattern = "octubre"   , replacement = "10", x = tmp)
    tmp <- gsub(pattern = "noviembre" , replacement = "11", x = tmp)
    tmp <- gsub(pattern = "diciembre" , replacement = "12", x = tmp)
    bill$dateIn <- dmy(tmp)
                                        #
    tmp <- chunk[grep("Estado:", chunk)]
    tmp <- sub(pattern = "Estado: (.*)", replacement = "\\1", x = tmp)
    bill$state <- tmp
                                        #
    bill$bol <- sub(pattern = ".*/bol(.*).txt", replacement = "\\1", files[i])
                                        #
    tmp <- chunk[grep("Refundido con:", chunk)]
    if (length(tmp) == 0){
        tmp <- "no"
    } else {
        tmp <- sub(pattern = ".*: (.*)", replacement = "\\1", tmp)
    }
    bill$refundido <- tmp
                                        #
    tmp <- chunk[grep("Materia:", chunk)]
    tmp <- sub(pattern = "Materia: (.*)", replacement = "\\1", tmp)
    bill$materia <- tmp
                                        #
    tmp <- chunk[grep("Iniciativa:", chunk)]
    tmp <- sub(pattern = "Iniciativa: (.*)", replacement = "\\1", tmp)
    tmp <- sub(pattern = "Mensaje", replacement = "1", tmp)
    tmp <- sub(pattern = "Moción", replacement = "0", tmp)
    bill$dmensaje <- as.numeric(tmp)
                                        #
    tmp <- chunk[grep("Cámara de origen:", chunk)]
    tmp <- sub(pattern = "Cámara de origen: (.*)", replacement = "\\1", tmp)
    tmp <- sub(pattern = ".*[Dd]iputados", replacement = "dip", tmp)
    tmp <- sub(pattern = ".*[Ss]enado", replacement = "sen", tmp)
    bill$init <- tmp
                                        #
                                        # find hitos tramitación
    start <- grep(pattern = "emmStart Hitos", x = bol)
    end <- grep(pattern = "Hitos emmEnd", x = bol)
    chunk <- bol[start:end] # selects chunk with info of interest
    chunk <- sub(pattern = "emmStart Hitos.*[0-9]+,", replacement = "", chunk) # cleans
    chunk <- sub(pattern = ",Hitos emmEnd"   , replacement = "", chunk) # cleans
                                        # verifies if there is a record or not
    tmp <- grep(pattern = "No record", chunk)
    bill$hasHitos <- ifelse(length(tmp)>0, "no", "yes")
    bills$hitos[[i]] <- chunk # pastes raw info for further processing
                                        #
                                        # find informes
    start <- grep(pattern = "emmStart Informes", x = bol)
    end <- grep(pattern = "Informes emmEnd", x = bol)
    chunk <- bol[start:end] # selects chunk with info of interest
    chunk <- sub(pattern = "emmStart Informes.*[0-9]+,", replacement = "", chunk) # cleans
    chunk <- sub(pattern = ",Informes emmEnd"   , replacement = "", chunk) # cleans
                                        # verifies if there is a record or not
    tmp <- grep(pattern = "No record", chunk)
    bill$hasReport <- ifelse(length(tmp)>0, "no", "yes")
    bills$reports[[i]] <- chunk # pastes raw info for further processing
                                        #
                                        # find urgencias
    start <- grep(pattern = "emmStart Urgencias", x = bol)
    end <- grep(pattern = "Urgencias emmEnd", x = bol)
    chunk <- bol[start:end] # selects chunk with info of interest
    chunk <- sub(pattern = "emmStart Urgencias.*[0-9]+,", replacement = "", chunk) # cleans
    chunk <- sub(pattern = ",Urgencias emmEnd"   , replacement = "", chunk) # cleans
                                        # verifies if there is a record or not
    tmp <- grep(pattern = "No record", chunk)
    bill$hasUrg <- ifelse(length(tmp)>0, "no", "yes")
    bills$urgencias[[i]] <- chunk # pastes raw info for further processing
                                        #
                                        # find autores
    start <- grep(pattern = "emmStart Autores", x = bol)
    end <- grep(pattern = "Autores emmEnd", x = bol)
    chunk <- bol[start:end] # selects chunk with info of interest
    chunk <- sub(pattern = "emmStart Autores.*[0-9]+,", replacement = "", chunk) # cleans
    chunk <- sub(pattern = ",Autores emmEnd"   , replacement = "", chunk) # cleans
                                        # verifies if there is a record or not
    tmp <- grep(pattern = "No record", chunk)
    bill$hasSpon <- ifelse(length(tmp)>0, "no", "yes")
    bills$sponsors[[i]] <- chunk # pastes raw info for further processing
                                        #
                                        # find votaciones
    start <- grep(pattern = "emmStart Votaciones", x = bol)
    end <- grep(pattern = "Votaciones emmEnd", x = bol)
    chunk <- bol[start:end] # selects chunk with info of interest
    chunk <- sub(pattern = "emmStart Votaciones.*[0-9]+,", replacement = "", chunk) # cleans
    chunk <- sub(pattern = ",Votaciones emmEnd"   , replacement = "", chunk) # cleans
                                        # verifies if there is a record or not
    tmp <- grep(pattern = "No record", chunk)
    bill$hasVot <- ifelse(length(tmp)>0, "no", "yes")
    bills$votes[[i]] <- chunk # pastes raw info for further processing
                                        #
                                        # find veto
    start <- grep(pattern = "emmStart Veto", x = bol)
    end <- grep(pattern = "Veto emmEnd", x = bol)
    chunk <- bol[start:end] # selects chunk with info of interest
    chunk <- sub(pattern = "emmStart Veto.*[0-9]+,", replacement = "", chunk) # cleans
    chunk <- sub(pattern = ",Veto emmEnd"   , replacement = "", chunk) # cleans
                                        # verifies if there is a record or not
    tmp <- grep(pattern = "No record", chunk)
    bill$hasVeto <- ifelse(length(tmp)>0, "no", "yes")
    bills$veto[[i]] <- chunk # pastes raw info for further processing
                                        #
                                        # add bill info to info data.frame
    if (i==1){
        bills$info <- bill
    } else {
        bills$info <- rbind(bills$info, bill)
    }
}

summary(bills)

####################################
# systematize hitos de tramitación #
####################################
bills$syst <- vector("list", I) # new slot to receive structured info
#
bills$info$debug <- 0 # debug prep
#
# pick one case
i <- 77

for (i in 1:200){
    message(sprintf("loop %s of %s", i, I))
    tmp <- bills$hitos[[i]]
    tmp <- gsub(pattern = ",", replacement = "", tmp) # drop commas
    tmp <- tmp[-1] # drop line with titles
    N <- length(tmp) # number of hitos
    output <- data.frame(rawText=tmp) # initialize output object
                                        # format dates
    tmp2 <- sub(pattern = "^([0-9]{2}[ .A-Za-z]+[0-9]{4}).*", replacement = "\\1", tmp, perl = TRUE)
    tmp2 <- sub(pattern = " de ", replacement = "/", tmp2, perl = TRUE)
    tmp2 <- sub(pattern = "[. ][de ]*", replacement = "./", tmp2, perl = TRUE)
    tmp2 <- gsub(pattern = "Ene.", replacement = "1", x = tmp2)
    tmp2 <- gsub(pattern = "Feb.", replacement = "2", x = tmp2)
    tmp2 <- gsub(pattern = "Mar.", replacement = "3", x = tmp2)
    tmp2 <- gsub(pattern = "Abr.", replacement = "4", x = tmp2)
    tmp2 <- gsub(pattern = "May.", replacement = "5", x = tmp2)
    tmp2 <- gsub(pattern = "Jun.", replacement = "6", x = tmp2)
    tmp2 <- gsub(pattern = "Jul.", replacement = "7", x = tmp2)
    tmp2 <- gsub(pattern = "Ago.", replacement = "8", x = tmp2)
    tmp2 <- gsub(pattern = "Sep.", replacement = "9", x = tmp2)
    tmp2 <- gsub(pattern = "Oct.", replacement = "10", x = tmp2)
    tmp2 <- gsub(pattern = "Nov.", replacement = "11", x = tmp2)
    tmp2 <- gsub(pattern = "Dic.", replacement = "12", x = tmp2)
    output$date <- dmy(tmp2, quiet = TRUE)
                                        #
    tmp <- sub(pattern = "^[0-9]{2}[ .A-Za-z]+[0-9]{4}(.*)", replacement = "\\1", tmp, perl = TRUE) # crops object
    tmp <- sub(pattern = "^[ ]+", replacement = "", tmp) # removes spaces at start
    output$ses <- (function(x){
        pat <- "^(\\d[/ 0-9ª]*) .*"
        ind <- grep(pat, x)
        x <- gsub(pat, "\\1", x)
        x[-ind] <- ""
        x
    })(tmp)
                                        #
    tmp <- sub(pattern = "^\\d[/ 0-9ª]* (.*)", replacement = "\\1", tmp, perl = TRUE) # crops object
                                        # returns text up to first "/" with trámite name
    output$tramite <- sub(pattern = "^(.*?)/.*", replacement = "\\1", tmp, perl = TRUE) # relies on non-greedy *?
    output$tramite <- sub(pattern = "Primer trámite constitucional[ ]*"   , replacement = "1ero", output$tramite, perl = TRUE)
    output$tramite <- sub(pattern = "Segundo trámite constitucional[ ]*"  , replacement = "2do", output$tramite, perl = TRUE)
    output$tramite <- sub(pattern = "Tercer trámite constitucional[ ]*"   , replacement = "3ero", output$tramite, perl = TRUE)
    output$tramite <- sub(pattern = ".*aprobaci[óo]n presidencial.*"      , replacement = "toPres", output$tramite, perl = TRUE)
    output$tramite <- sub(pattern = "Discusión veto.*"                    , replacement = "veto", output$tramite, perl = TRUE)
    output$tramite <- sub(pattern = ".*Tribunal Constitucional.*"         , replacement = "tribunal", output$tramite, perl = TRUE)
    output$tramite <- sub(pattern = ".*finalización.*"                    , replacement = "final", output$tramite, perl = TRUE)
    output$tramite <- sub(pattern = "Archivado.*"                         , replacement = "onHold", output$tramite, perl = TRUE)
    output$tramite <- sub(pattern = "Tramitación terminada.*"             , replacement = "ended", output$tramite, perl = TRUE)
    output$tramite <- sub(pattern = "Comisión Mixta.*"                    , replacement = "conf", output$tramite, perl = TRUE)
    output$tramite <- sub(pattern = "Disc[.] [Ii]nforme C[.]Mixta.*"      , replacement = "PostConf", output$tramite, perl = TRUE)
    output$tramite <- sub(pattern = ".*insistencia.*"                     , replacement = "PostConf", output$tramite, perl = TRUE)
                                        #
    tmp <- sub(pattern = "^.*?/(.*)", replacement = "\\1", tmp, perl = TRUE) # crops object
    tmp <- sub(pattern = "^[ ]+", replacement = "", tmp) # removes spaces at start
    output$chamber <- sub(pattern = "^(Senado|C. Diputados).*", replacement = "\\1", tmp, perl = TRUE)
    output$chamber <- sub(pattern = "^Senado"                                      , replacement = "sen", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = "^C. Diputados"                                , replacement = "dip", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*Congreso Pleno.*"                           , replacement = "con", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*Oficio.*"                                   , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = "^S[.]E[.] el [PV].*retira.*"                  , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*S[.]E[.] el [PV].*retira.*"                 , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = "^Cuenta [Cc]omunicación.*"                    , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = "^Cuenta del [Mm]ensaje.*"                     , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = "^Cuenta (?:en el Senado )*(?:de[l]* )*(?:un )*oficio.*"  , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = "^Archivado Cuenta oficio.*"                   , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*vuelve a [Cc]omisión.*"                     , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*indicaciones.*"                             , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*[Dd]iscusión particular.*"                     , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = "^Queda para.*"                                , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = "^Se rechaza el proyecto.*"                    , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = "^Cambia la tramitación.*"                     , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = "^Se suspende [la]{2} tramitación.*"           , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = "^Por acuerdo de la Sala.*"                    , replacement = ".", output$chamber, perl = TRUE)
                                        #
    tmp <- sub(pattern = "^(Senado|C. Diputados)[ ]+(.*)", replacement = "\\2", tmp, perl = TRUE) # crops object
    output$actionRaw <- sub(pattern = "(.*)[ ]+Ver$", replacement = "\\1", tmp, perl = TRUE) # removes Ver (download procedure missed link 2 relevant docs.)
                                        #
    bills$info$debug[i] <-
        max(sapply(output$chamber, function(x) nchar(x))) # counts the maximum n characters of string in output$chamber (shoud be 3) and plugs to info
    #bills$syst[
}

bills$info$debug[1:202]

#########################
# systematize urgencias #
#########################
work <- which(bills$info$hasUrg=="yes") # work only this in loop
#
# new slots for info
bills$info$nUrg <- 0
bills$info$n30 <- 0
bills$info$n15 <- 0
bills$info$n6 <- 0
bills$info$nRet <- 0

# neat function to compute urgencia dealines excluding week-ends
# solution 1 takes holidays other than weekends into account, which is a plus
library(timeDate)
deadline <- function(x, nBizDys = 6){ # function to process deadlines excluding week-ends and holidays... how do you change default=holidayNYSE with non-prepackaged holidays (eg., Chile's http://www.feriadoschilenos.cl/)?
    output <- Reduce(rbind, Map((function(x, howMuch = 15){
        x <- as.Date(x)
        days <- x + 1:(howMuch*2)
        Deadline <- days[isBizday(as.timeDate(days))][howMuch]
        data.frame(DateIn = x, Deadline, DayOfWeek = weekdays(Deadline),   
                   TimeDiff = difftime(Deadline, x))  # useful to get more info, if so wished
    }), x, howMuch = nBizDys))
    output$Deadline
}
#deadline(date.in, nBizDys=30) # example of use
#
## # solution 2 removes weekends only, still needs to be turned into function
## library(chron)
## deadline <- function(x, nDays=31) {
##     x1 <-seq(as.Date(x)+1, length.out=nDays*2, by='1 day')
##     data.frame(Start=x,End=x1[!is.weekend(x1)][nDays])
## }
## do.call(rbind, lapply(date.in, deadline))

# pick one case
i <- which(bills$info$bol=="999-15") # debug: one case with several urgencias
i <- work[1]
j <- 1

#for (i in work){
j <- j + 1
i <- work[j]
    print(paste("loop", which(work==i), "of", length(work)))
    #bills$info$bol[i] # debug
    tmp <- bills$urgencias[[i]]
    tmp <- tmp[-grep(pattern = "Fecha Inicio", tmp)] # remove header assuming it contains Fecha Inicio and may not be there
    U <- length(tmp)
    tmp <- gsub(pattern = "(de [0-9]+) ", replacement = "\\1,", tmp) # separates date(s) with a comma
    tmp2 <- nchar( gsub(pattern = "[^,]", replacement = "", tmp) )  # how many dates (commas) in each line
                                        # prepares dates
    tmp <- gsub(pattern = " de ", replacement = "/", tmp)
    tmp <- gsub(pattern = "Ene.", replacement = "1", x = tmp)
    tmp <- gsub(pattern = "Feb.", replacement = "2", x = tmp)
    tmp <- gsub(pattern = "Mar.", replacement = "3", x = tmp)
    tmp <- gsub(pattern = "Abr.", replacement = "4", x = tmp)
    tmp <- gsub(pattern = "May.", replacement = "5", x = tmp)
    tmp <- gsub(pattern = "Jun.", replacement = "6", x = tmp)
    tmp <- gsub(pattern = "Jul.", replacement = "7", x = tmp)
    tmp <- gsub(pattern = "Ago.", replacement = "8", x = tmp)
    tmp <- gsub(pattern = "Sep.", replacement = "9", x = tmp)
    tmp <- gsub(pattern = "Oct.", replacement = "10", x = tmp)
    tmp <- gsub(pattern = "Nov.", replacement = "11", x = tmp)
    tmp <- gsub(pattern = "Dic.", replacement = "12", x = tmp)
                                        #
    tmp <- gsub(pattern = ",[ ]+", replacement = ",", tmp) # drops spaces after commas
                                        #
    output <- data.frame(type=character(U)) # initialize output object
    output$on <- dmy(gsub(pattern = "^([0-9/]*),.*", "\\1", tmp, perl = TRUE), tz = "UTC") # adds date urgencia introduced
                                        #
    tmp3 <- sub(pattern = ".*(Sin urgencia).*", replacement = "\\1", tmp, perl = TRUE)
    tmp3 <- sub(pattern = ".*(Simple).*", replacement = "\\1", tmp3, perl = TRUE)
    tmp3 <- sub(pattern = ".*(Suma).*", replacement = "\\1", tmp3, perl = TRUE)
    tmp3 <- sub(pattern = ".*(Discusión inmediata).*", replacement = "\\1", tmp3, perl = TRUE)
    output$type <- tmp3
                                        #
                                        # when urgencia deadline is de jure (need to change if the bill is in Comisión mixta) --- check is Senado and Comisión mixta urgencias are included
    tmp3 <- sub(pattern = ".*(Sin urgencia).*", replacement = 0, tmp, perl = TRUE)
    tmp3 <- sub(pattern = ".*(Simple).*", replacement = 30, tmp3, perl = TRUE)
    tmp3 <- sub(pattern = ".*(Suma).*", replacement = 15, tmp3, perl = TRUE)
    tmp3 <- sub(pattern = ".*(Discusión inmediata).*", replacement = 6, tmp3, perl = TRUE)
    tmp3 <- as.numeric(tmp3)
                                        #
    output$deadline <- output$on # inherits format for NAs
    select <- which(tmp2!=0)
    if (length(select)>0){
        output$deadline[tmp3!=0] <- deadline(output$on[tmp3!=0], as.numeric(tmp3[tmp3!=0]))
    }
                                        #
                                        # urgencia retired?
    output$retir <- "no"
    select <- which(tmp2==2)
    if (length(select)>0){
        output$retir[tmp2==2] <- "yes"
    }
                                        # when urgencia was removed, if at all
    output$off <- output$deadline
    select <- which(tmp2==2)
    if (length(select)>0){
        output$off[tmp2==2] <- dmy(gsub(pattern = ".*[0-9],([0-9/]*),.*", "\\1", tmp[tmp2==2], perl = TRUE))
    }
                                        # clean dates
    select <- which(tmp2==1)
    if (length(select)>0){
        output$off[tmp2==1] <- NA
    }
                                        # drops instances of no urgencia, if any
    select <- which(output$type=="Sin urgencia")
    if (length(select)>0) {
        output <- output[-select,]
    }
                                        #
                                        # sort chrono
    output <- output[order(output$on),]
                                        #
                                        # remove "sin urgencia", if any
    select <- which(output$type=="Sin urgencia")
    if (length(select)>0) {
        output <- output[-select,]
    }
                                        # find and consolidate chains
    output$chain <- 0
    U <- nrow(output) # number of messages left
    if (U > 1){
        for (k in 2:U){
            if (output$retir[k-1]=="yes" & output$on[k]==output$off[k-1]){
                output$chain[k] <- 1
            }
        }
        tmp4 <- output$chain
        for (k in 2:U){
            tmp4[k] <- output$chain[k] + tmp4[k-1] * output$chain[k] # zero if no chain, else how many links
        }
        output$chain <- tmp4; rm(tmp4)
    ## output$shorten <- output$extend <- "no"
    ## output$newDeadline <- output$deadline
    ## output$change <- 0
    ## for (k in 2:U){
    ##     if (output$chain[k]==1){
    ##         output$retir[k-1] <- "no"
    ##         output$newDeadline[k-1] <- output$deadline[k]
    ##         if (output$deadline[k] >= output$deadline[k-1]){
    ##             output$extend[k-1] <- "yes"
    ##         } else {
    ##             output$shorten[k-1] <- "yes"
    ##         }
    ##         output$newDeadline[k] <- NA
    ##         output$change[k-1] <- as.numeric(output$newDeadline[k-1] - output$off[k-1]) *100 / as.numeric(output$deadline[k-1] - output$on[k-1])
    ##         output$off[k-1] <- NA
    ##     }
    }
    ## ##                                     # drop chains
    ## ## select <- which(output$chain==1)
    ## ## if (length(select)>0) {
    ## ##     output <- output[-select,]
    ## ## }
    ## ## output$chain <- NULL
output # debug
bills$info$bol[i] # debug
                                        #
                                        # plug into slot for systematized data
    if (nrow(output)>0){ # anything left after dropping sin urgencia?
        bills$syst[[i]] <- output # WILL NEED A NAME
        bills$info$nUrg[i] <- nrow(output)
        bills$info$n30[i] <- nrow(output[output$type=="Simple",])
        bills$info$n15[i] <- nrow(output[output$type=="Suma",])
        bills$info$n6[i] <- nrow(output[output$type=="Discusión inmediata",])
        bills$info$nRet[i] <- nrow(output[output$retir=="yes",])
    }
    if (nrow(output)==0){ # in case nothing left after dropping sin urgencia, change info
        bills$info$hasUrg[i] <- "no"
    }
#}
output
bills$info[i,]

ls()
rm(select, output, tmp, tmp2, tmp3)      # clean
bills$urgRaw <- bills$urgencias  # preserves raw info to verify nothing missed
bills$urgencias <- bills$syst    # data systematized (more work needed)
bills$syst <- NULL               # clean

save.image("tmp.RData")
bills$urgencias[[work[2]]]

bills$info$bol[2739]
bills$urgencias[2739]

str(bills$info)

head(bills)
table(bills$info$hasUrg)
bill

bills$bol[bills$hasVeto=="yes"]



# used to clean content copied with  htmlfox from camara.cl search of boletin numbers

rm(list = ls())
wd <- "~/Dropbox/data/latAm/chile/data/"
setwd(wd)

raw <- readLines( "boletines.txt", encoding = "utf-8" )
head(raw)

## raw <- gsub(pattern = "\xe1",   replacement = "á", raw)
## raw <- gsub(pattern = "&#225;", replacement = "á", raw)
## raw <- gsub(pattern = "\xe9",   replacement = "é", raw)
## raw <- gsub(pattern = "&#233;", replacement = "é", raw)
## raw <- gsub(pattern = "\xed",   replacement = "í", raw)
## raw <- gsub(pattern = "&#237;", replacement = "í", raw)
## raw <- gsub(pattern = "\xf3",   replacement = "ó", raw)
## raw <- gsub(pattern = "&#243;", replacement = "ó", raw)
## raw <- gsub(pattern = "\xfa",   replacement = "ú", raw)
## raw <- gsub(pattern = "&#250;", replacement = "ú", raw)
## #raw <- gsub(pattern = "\", replacement = "ñ", raw)
## raw <- gsub(pattern = "&#241;", replacement = "ñ", raw)
## raw <- gsub(pattern = "&#176;", replacement = "°", raw)

## writeLines(raw, "utf8.boletines.txt"    )

reg <- grep(pattern = "\t<td>", raw, perl = TRUE) # locates lines with ingreso, título, estado
#reg <- c(reg, reg+1) # next line in each has prmID
#reg <- reg[order(reg)] # sorts

raw.sm <- raw[reg]

dat <- data.frame(ord=1:length(reg))

tmp <- sub(pattern = ".*\t<td>(.*)</td><td>(.*)</td><td>(.*)</td><td>", replacement = "\\1", raw.sm)
head(tmp)
dat$dy <- as.numeric( sub(pattern = "([0-9]{1-2}) de .*", replacement = "\\1", tmp) )
tmp2 <- sub(pattern = ".* de (.*) de.*", replacement = "\\1", tmp)
    tmp2 <- sub(tmp2, pattern="[Ee]ne.", replacement="1")
    tmp2 <- sub(tmp2, pattern="[Ff]eb.", replacement="2")
    tmp2 <- sub(tmp2, pattern="[Mm]ar.", replacement="3")
    tmp2 <- sub(tmp2, pattern="[Aa]br.", replacement="4")
    tmp2 <- sub(tmp2, pattern="[Mm]ay.", replacement="5")
    tmp2 <- sub(tmp2, pattern="[Jj]un.", replacement="6")
    tmp2 <- sub(tmp2, pattern="[Jj]ul.", replacement="7")
    tmp2 <- sub(tmp2, pattern="[Aa]go.", replacement="8")
    tmp2 <- sub(tmp2, pattern="[Ss]ep.", replacement="9")
    tmp2 <- sub(tmp2, pattern="[Oo]ct.", replacement="10")
    tmp2 <- sub(tmp2, pattern="[Nn]ov.", replacement="11")
    tmp2 <- sub(tmp2, pattern="[Dd]ic.", replacement="12")
dat$mo <- as.numeric(tmp2); rm(tmp2)
dat$yr <- as.numeric( sub(pattern = ".* de ([0-9]+)", replacement = "\\1", tmp) )
#
dat$titulo <- sub(pattern = ".*\t<td>(.*)</td><td>(.*)</td><td>(.*)</td><td>", replacement = "\\2", raw.sm)
#
dat$estado17oct2014 <- sub(pattern = ".*\t<td>(.*)</td><td>(.*)</td><td>(.*)</td><td>", replacement = "\\3", raw.sm)
#
# next line has prnID
raw.sm <- raw[(reg+1)]
dat$url <- sub(pattern = ".*<a href=\"(.*)\">", replacement = "\\1", raw.sm)
head(dat$url)
dat$prmID <- sub(pattern = ".*prmID=([0-9]+)&.*", replacement = "\\1", dat$url)
dat$bolCom <- sub(pattern = ".*prmBL=(.*).*", replacement = "\\1", dat$url)
dat$boletin <- as.numeric( sub(pattern = "(.*)-.*", replacement = "\\1", dat$bolCom) )
dat$comision <- sub(pattern = ".*-(.*)", replacement = "\\1", dat$bolCom)

rm(tmp)

head(dat)

write.csv(dat, file="boletines.csv")

proyec2 <- read.csv("proyec2.csv")
colnames(proyec2)

boletines <- read.csv("boletines.csv")
colnames(boletines)

joint <- merge(x = proyec2, y = boletines, by = "boletin")

colnames(joint)
joint <- joint[, c("boletin", "comision", "bolCom", "url","sumario", "titulo", "note")]
write.csv(joint, "proy3.csv")


joint$prmID <- NULL
joint$note <- ""
joint$tmp <- as.numeric(as.character(joint$comis)) - joint$comision
joint$note[which(joint$tmp!=0)] <- paste("valparaiso data said comision =", joint$comis[which(joint$tmp!=0)])
joint$comis <- NULL

tmp <- joint[!is.na(joint$yrin), c("yrin","yr")]
tmp$yrin <- as.numeric(as.character(tmp$yrin))
tmp$dif <- tmp$yrin - tmp$yr
tmp[which(tmp$dif!=0),]


