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
grep("2397", files)
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
    #i <- grep("127-01", files) # debug: read one boletin
    message(sprintf("loop %s of %s", i, I))
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
    bill$dateIn <- dmy(tmp, tz = "chile")
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
    if (length(tmp)==0){
        tmpD <- chunk[-grep(pattern = "Sin urgencia", chunk)]
        bill$hasUrg <- ifelse(length(tmpD)==1, "no", "yes") # dropping sin urgencia lines, are there any remaining other than the date etc. heading?
    }
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
rm(bill, bol, chunk, end, files, i, start, tmp, tmpD)

####################################
# systematize hitos de tramitación #
####################################
#
nHitos <- rep(0,I)
bills$info$hasUrgHU <- "." # will receive urgencia info from hitos and urgencia tab
#bills$info$hasVetoH <- "." # will receive veto info from hitos
#
bills$info$debug <- 0 # debug prep --- can receive anything being checked in data
#
bills$hitosRaw <- bills$hitos # keeps raw info for future revision
#
sel <- 1:I
library(lubridate)
for (i in sel){
    message(sprintf("loop %s of %s", i, I))
    tmp <- bills$hitosRaw[[i]]
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
    output$date <- dmy(tmp2, quiet = TRUE, tz = "chile")
                                        #
    tmp <- sub(pattern = "^[0-9]{2}[ .A-Za-z]+[0-9]{4}(.*)", replacement = "\\1", tmp, perl = TRUE) # crops object
    tmp <- sub(pattern = "^[ ]+", replacement = "", tmp) # removes spaces at start of line
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
    output$tramite <- sub(pattern = "Discusión.*[Cc]ámara de [Oo]rigen.*" , replacement = "1ero", output$tramite, perl = TRUE)
    output$tramite <- sub(pattern = "Discusión.*[Cc]ámara [Rr]evisora.*"  , replacement = "2do", output$tramite, perl = TRUE)
    output$tramite <- sub(pattern = ".*Tribunal Constitucional.*"         , replacement = "tribunal", output$tramite, perl = TRUE)
    output$tramite <- sub(pattern = ".*finalización.*"                    , replacement = "final", output$tramite, perl = TRUE)
    output$tramite <- sub(pattern = "Archivado.*"                         , replacement = "onHold", output$tramite, perl = TRUE)
    output$tramite <- sub(pattern = "Tramitación terminada.*"             , replacement = "ended", output$tramite, perl = TRUE)
    output$tramite <- sub(pattern = "Comisión Mixta.*"                    , replacement = "conf", output$tramite, perl = TRUE)
    output$tramite <- sub(pattern = "Disc[.] [Ii]nforme C[.]Mixta.*"      , replacement = "PostConf", output$tramite, perl = TRUE)
    output$tramite <- sub(pattern = ".*insistencia.*"                     , replacement = "chOverride", output$tramite, perl = TRUE) 
    output$tramite <- sub(pattern = ".*Congreso Pleno.*"                  , replacement = "Cong", output$tramite, perl = TRUE)
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
    output$chamber <- sub(pattern = "^Archivado Cuenta oficio.*"                   , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*vuelve a [Cc]omisión.*"                     , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*indicaciones.*"                             , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*[Dd]iscusión particular.*"                  , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = "^Queda.*"                                     , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = "^Se rechaza el proyecto.*"                    , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = "^Cambia.*tramitación.*"                       , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = "^Se suspende [la]{2} tramitación.*"           , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = "^Por acuerdo de la Sala.*"                    , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*retir.*tramitación.*"    , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*[Aa]pr[uo]e*ba.*"  , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = "^Cuenta.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*Ratifica.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*desarchiv.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*[Dd]iscusión.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*[Rr]echazado.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*remite.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*dispone.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*retir.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*[Aa]rv*chj*iv.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*[Ss]oli[ci]{2}t.*archiv.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*suspende.*tramitación.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*[cC]omisión.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*oficio.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*rechaza.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*[Rr]etira.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*segunda discusión.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*general y particular.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*aprobación en particular.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*discutida en particular.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*ratifica.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*aplaza.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*acepta.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*[Tt]abla.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = "^Pasa a [Cc]omisión.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = "^Fracasa la sesión.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*petición.*[Cc]omisión.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*Sala.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*incluye.*proyecto.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = "LEY N.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*aprueba.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*Urgencia.*"                        , replacement = ".", output$chamber, perl = TRUE)
    output$chamber <- sub(pattern = ".*Decreto.*"                        , replacement = ".", output$chamber, perl = TRUE)
                                        #
    tmp <- sub(pattern = "^(Senado|C. Diputados)[ ]+(.*)", replacement = "\\2", tmp, perl = TRUE) # crops object
    output$action <- sub(pattern = "(.*)[ ]+Ver$", replacement = "\\1", tmp, perl = TRUE) # removes Ver (download procedure missed link 2 relevant docs.)
                                        #
    ## bills$info$debug[i] <-
    ##    min(sapply(output$chamber, function(x) nchar(x))) # counts the maximum n characters of string in output$chamber (shoud be 3) and plugs to info
                                        #
    bills$hitos[[i]] <- output # replaces raw object with data.frame
    nHitos[i] <- N
}
rm(i, N, output, sel, tmp, tmp2) # housecleaning

# infer missing chamber when possible
#
# for single-line hitos, a missing cannot be inferred (no cases, it seems)
#
sel <- which(nHitos==2) # start with cases with two-line hitos
tmpHasMissing <- tmpInferred <- rep(0,I)
for (i in sel){
    if (bills$hitos[[i]]$chamber[1]=="."){ # is 1st-line chamber missing?
        tmpHasMissing[i] <- 1
        if (bills$hitos[[i]]$chamber[2]!="."){ # if 2nd-line chamber not missing, infer line 1 with it
            bills$hitos[[i]]$chamber[1] <- bills$hitos[[i]]$chamber[2]
            tmpInferred[i] <- 1 # record that change occurred -- for verif
        }
    }
    if (bills$hitos[[i]]$chamber[2]=="."){ # is 2nd-line chamber missing?
        tmpHasMissing[i] <- 1
        if (bills$hitos[[i]]$chamber[2]!="."){ # if 1st-line chamber not missing, infer line 2 with it
            bills$hitos[[i]]$chamber[2] <- bills$hitos[[i]]$chamber[1]
            tmpInferred[i] <- 1 # record that change occurred -- for verif
        }
    }
}
#
sel <- which(nHitos>2) # pick now cases with three-or-more-line hitos
#i <- sel[3] # debug
for (i in sel){
    tmpLinesChMissing <- which(bills$hitos[[i]]$chamber==".")
    if (length(tmpLinesChMissing)==0){ # if no missing chambers, move on in loop
        next
    } else {
        tmpHasMissing[i] <- 1
        if (tmpLinesChMissing[1]==1){ # is 1st-line chamber missing?
            if (bills$hitos[[i]]$chamber[2]!="."){ # if 2nd-line chamber not missing, infer line 1 with it
                bills$hitos[[i]]$chamber[1] <- bills$hitos[[i]]$chamber[2]
                tmpInferred[i] <- 1 # record that change occurred -- for verif
                tmpLinesChMissing <- tmpLinesChMissing[-1] # crop object
            }
        }
    }
    if (length(tmpLinesChMissing)==0){ # if no missing chambers, move on in loop
        next
    } else {
        if (tmpLinesChMissing[length(tmpLinesChMissing)]==nHitos[i]){ # is last-line chamber missing?
            if (bills$hitos[[i]]$chamber[(nHitos[i]-1)]!="."){ # if next-to-last-line chamber not missing, infer last line with it
                bills$hitos[[i]]$chamber[nHitos[i]] <- bills$hitos[[i]]$chamber[(nHitos[i]-1)]
                tmpInferred[i] <- 1 # record that change occurred -- for verif
                tmpLinesChMissing <- tmpLinesChMissing[-length(tmpLinesChMissing)] # crop object
            }
        }
    }
    if (length(tmpLinesChMissing)==0){ # if no missing chambers, move on in loop
        next
    } else {
        for (j in tmpLinesChMissing){ # loop over lines with missing chamber
            if (bills$hitos[[i]]$chamber[(j-1)]!="."){ # is line above's camber non-missing?
                bills$hitos[[i]]$chamber[j] <- bills$hitos[[i]]$chamber[(j-1)] # if so, use it to infer missing chamber
                tmpInferred[i] <- 1 # record that change occurred -- for verif
            }
        }
    }
}

# infer comisión mixta and ejecutivo trámites --- approximates com mixta 
for (i in 1:I){
    message(sprintf("loop %s of %s", i, I))
    select <- grep(pattern = "[Cc]omisi[oó]n [Mm]ixta", bills$hitos[[i]]$action)
    bills$hitos[[i]]$chamber[select] <- "conf"
    sel1 <- grep(pattern = "[Oo]ficio de [Ll]ey al [Ee]jecutivo", bills$hitos[[i]]$action)
    sel2 <- grep(pattern = "[Oo]ficio.*[pP]ara promulgación.*[Pp]resident[ea].*", bills$hitos[[i]]$action)
    select <- union(sel1, sel2)
    bills$hitos[[i]]$chamber[select] <- "ejec"
}

# begin correcting tribunal constitucional steps
tmp <- rep(0, I)
for (i in 1:I){
    if (length(grep(pattern = "tribunal", x = bills$hitos[[i]]$tramite))>0) tmp[i] <- 1
}
sel <- which(tmp==1)
for (i in sel){
    bills$hitos[[i]]$chamber[bills$hitos[[i]]$tramite=="tribunal" & bills$hitos[[i]]$chamber!="ejec"] <- "trib"
}
# begin correcting veto steps
tmp <- rep(0, I)
for (i in 1:I){
    if (length(grep(pattern = "veto", x = bills$hitos[[i]]$tramite))>0) tmp[i] <- 1
}
sel <- which(tmp==1)
for (i in sel){
    bills$hitos[[i]]$chamber[bills$hitos[[i]]$tramite=="veto"] <- "veto"
}
# drop redundant veto entries
for (i in sel){
    vet <- grep("veto",bills$hitos[[i]]$chamber)
    if (length(vet)>1){
        vet <- vet[-1] # those that will be dropped
        vet <- min(vet):max(vet) # include anything sandwiched between veto for manipulation
        bills$ hitos[[i]]$chamber[vet] <- "drop"
    }
}

tmp <- rep(0,I)
for (i in 1:I){
    if (length(which(bills$hitos[[i]]$chamber=="."))==0){
        next
    } else {
        tmp[i] <- 1
    }
}
table(tmp) # NO MORE MISSING CHAMBERS
table(tmpHasMissing) # cases with missing chamber encoutered
table(tmpInferred)   # cases with missing chamber inferred
#
bills$info$nHitos <- nHitos
#
rm(i, j, sel, sel1, sel2, tmp, tmpHasMissing, tmpInferred, tmpLinesChMissing, nHitos, select)

# recode bill status
bills$info$status <- bills$info$state # duplicates to retain original
bills$info$dateOut <- "." # will record date published
bills$info$status <- sub(pattern = "Tramitación terminada.*[0-9]{2}/[0-9]{2}/[0-9]{4}.*", replacement = "statute", bills$info$status)
bills$info$status[grep("Tramitación terminada", bills$info$status)] <- "killed/withdrawn"
sel <- which(bills$info$status=="statute")
bills$info$dateOut[sel] <- sub(pattern = "Tramitación terminada.*([0-9]{2}/[0-9]{2}/[0-9]{4}).*", replacement = "\\1", bills$info$state[sel])
bills$info$dateOut <- sub(pattern = "Tramitación terminada.*", replacement = ".", bills$info$dateOut) # missing values
bills$info$status <- sub(pattern = "Primer trámite.*", replacement = "pending: 1er trámite", bills$info$status)
bills$info$status <- sub(pattern = "Segundo trámite.*", replacement = "pending: 2do trámite", bills$info$status)
bills$info$status <- sub(pattern = "Tercer trámite.*", replacement = "pending: 3er trámite", bills$info$status)
bills$info$status <- sub(pattern = "Archivado.*", replacement = "frozen", bills$info$status)
bills$info$status <- sub(pattern = ".*Mixta.*", replacement = "pending: conference", bills$info$status)
bills$info$status <- sub(pattern = ".*veto.*", replacement = "pending: veto", bills$info$status)
bills$info$status <- sub(pattern = ".*[Ii]nsistencia.*", replacement = "pending: 3er trámite", bills$info$status)
bills$info$status <- sub(pattern = ".*aprobaci[óo]n presidencial.*", replacement = "pending: to executive", bills$info$status)
bills$info$status <- sub(pattern = ".*finalización en Cámara.*", replacement = "pending", bills$info$status)
#
bills$info$dateOut <- dmy(bills$info$dateOut, quiet = TRUE, tz = "chile")
table(bills$info$status) # debug

# compact bicameral sequence with dates (may still miss comisión mixta, revise bills$hitos$chamber above)
bills$tramites <- sapply(1:I, function(x) NULL) # initializes empty list with I elements (unnamed; names would go where 1:I)
for (i in 1:I){
    message(sprintf("loop %s of %s", i, I))
    if (length(grep("drop", bills$hitos[[i]]$chamber))>0){ # manipulates hitos object in case there are "drop" lines
        tmp <- bills$hitos[[i]][-grep("drop", bills$hitos[[i]]$chamber),] # drops "drop" rows
    } else {
        tmp <- bills$hitos[[i]]
    }
    if (length(grep("ejec", tmp$chamber))>0){ # manipulates tmp object in case there are "ejec" lines
        # this block may be wrong: assumes that after "ejec", $chamber can only be "veto" or "trib", once each at most...
        selejec <- grep("ejec", tmp$chamber)
        if (length(selejec)>0){
            selejec <- selejec[1] # keep only first/only instance
            after <- selejec:nrow(tmp) # indices that will be mnipulated
            tmp2 <- tmp[after,] # rows to manipulate
            tmp2 <- tmp2[grep("ejec|veto|trib", tmp2$chamber),] # keeps only rows with ejec, veto, or trib...
            tmp2 <- tmp2[duplicated(tmp2$chamber)==FALSE,]      # ... removing duplicates
            tmp <- rbind(tmp[1:(selejec-1),], tmp2)             # re-builds object
        }
        # end block
    }
    these <- rep(1, nrow(tmp)) # object that will select cases to include
    dat.tram <- tmp[,c("date", "chamber")] # keeps only date and tramite in separate object
    if (nrow(tmp)>1){ # next block only if multiline <--- ALL MULTILINE, SEEN ABOVE 
        for (j in 2:length(these)){
            #hour(dat.tram$date[j]) <- ifelse(dat.tram$date[j]==dat.tram$date[j-1], hour(dat.tram$date[j]) + 1, hour(dat.tram$date[j])) # adding 1hr to same dates avoids overlaps
            these[j] <- ifelse(dat.tram$chamber[j]==dat.tram$chamber[j-1], 0, 1) # identify tramites different from previous row as 1s
        }
        dat.tram$to <- dat.tram$date  # duplicates date to retain format
        dat.tram <- dat.tram[these==1,] # compact sequence of tramites
        if (nrow(dat.tram)>1){
            dat.tram$to[1:(nrow(dat.tram)-1)] <- dat.tram$to[2:nrow(dat.tram)] # plug end of tramite -- multiline
        } else {
            dat.tram$to <- dmy("5/11/2014", tz = "chile")                                    # -- uniline assumed pending
        }
        if (bills$info$status[i]=="statute"){
            dat.tram$to[nrow(dat.tram)] <- bills$info$dateOut[i] # use date published if so
        } else {
            dat.tram$to[nrow(dat.tram)] <- dmy("5/11/2014", tz = "chile")      # else date when data downloaded (pending)
        }
        # minute(dat.tram$date) <- minute(dat.tram$date) +1 # adds 1 minute to remove overlap with last "to"
    }
    colnames(dat.tram)[1:2] <- c("from","tramite")
    if (nrow(bills$hitos[[i]])==1){
        dat.tram$to <- dmy("5/11/2014", tz = "chile")
    }
    bills$tramites[[i]] <- dat.tram
    bills$tramites[[i]]$period <- new_interval(bills$tramites[[i]]$from, bills$tramites[[i]]$to) # adds trámite duration
}
rm(dat.tram, i, j, sel, these, after, selejec, tmp2)

## loop over hitos in search of urgencia info
#
## # used to prove that text "urgencia" misses no case of discusión inmediata
## tmp <- 0
## for (i in 1:I){
##     message(sprintf("loop %s of %s", i, I))
##     tmp1 <- grep(pattern = "[Uu]rgencia", bills$hitos[[i]]$action)
##     tmp2 <- grep(pattern = "[Dd]iscusión [Ii]nmediata", bills$hitos[[i]]$action)
##     if (length(tmp2)>0 & length(tmp2[!(tmp2 %in% tmp1)])>1){ # for records with DI, subsets line numbers with DI but no U
##         tmp <- append(tmp, i) # which record
##     }
## }

for (i in 1:I){
    message(sprintf("loop %s of %s", i, I))
    tmp1 <- grep(pattern = "[Uu]rgencia", bills$hitos[[i]]$action)
    bills$hitos[[i]]$urg <- rep(".", nrow(bills$hitos[[i]]))
    if (length(tmp1)>0){ # for records with word urgencia
        tmp <- bills$hitos[[i]]$action[tmp1] # pick those lines
        # use greps to systematize urgencia action
        tmp <- sub(".*[Pp]asa.*\\(suma urgencia\\).*"                                              , replacement = "urg15 on", tmp, perl = TRUE)
        tmp <- sub(".*[Pp]asa.*\\(simple urgencia\\).*"                                            , replacement = "urg30 on", tmp, perl = TRUE)
        tmp <- sub(".*que retira y hace presente.*urgencia [Ss]imple.*"                            , replacement = "reset: urg30", tmp, perl = TRUE)
        tmp <- sub(".*que retira y hace presente.*urgencia [Ss]uma.*"                              , replacement = "reset: urg15", tmp, perl = TRUE)
        tmp <- sub(".*que retira y hace presente.*urgencia [Dd]iscusión inmediata.*"               , replacement = "reset: urg06", tmp, perl = TRUE)
        tmp <- sub("^Cuenta retiro y se hace presente.*urgencia [Ss]imple.*"                       , replacement = "reset: urg30", tmp, perl = TRUE)
        tmp <- sub(".*que( se)* retira.*urgencia.*"                                                , replacement = "urg off", tmp, perl = TRUE)
        tmp <- sub(".*que hace presente.*urgencia [Ss]imple.*"                                     , replacement = "urg30 on", tmp, perl = TRUE)
        tmp <- sub(".*que hace presente.*urgencia.*[Ss]uma.*"                                      , replacement = "urg15 on", tmp, perl = TRUE)
        tmp <- sub(".*que hace presente.*urgencia.*[Dd]iscusión [Ii]nmediata.*"                    , replacement = "urg06 on", tmp, perl = TRUE)
        tmp <- sub(".*Rep. hace presente.*urgencia.*[Ss]imple.*"                                   , replacement = "urg30 on", tmp, perl = TRUE)
        tmp <- sub(".*Rep. hace presente.*urgencia.*[Ss]uma.*"                                     , replacement = "urg15 on", tmp, perl = TRUE)
        tmp <- sub(".*Rep. hace presente.*urgencia.*[Dd]iscusión [Ii]nmediata.*"                   , replacement = "urg06 on", tmp, perl = TRUE)
        tmp <- sub(".*Hace presente.*urgencia.*[Dd]iscusión [Ii]nmediata.*"                        , replacement = "urg06 on", tmp, perl = TRUE)
        tmp <- sub(".*Asimismo hace presente.*urgencia.*[Dd]iscusión [Ii]nmediata.*"               , replacement = "urg06 on", tmp, perl = TRUE)
        tmp <- sub("^Cuenta [Mm]ensaje.*[Hh]ace presente.*urgencia.*[Dd]iscusi[óo]n [Ii]nmediata.*", replacement = "urg06 on", tmp, perl = TRUE)
        tmp <- sub("^Cuenta urgencia [Ss]imple.*"                                                  , replacement = "urg30 on", tmp, perl = TRUE)
        tmp <- sub(".*[Rr]et[iu][rt]a y.*hace presente.*urgencia.*[Ss]imple.*"                     , replacement = "reset: urg30", tmp, perl = TRUE)
        tmp <- sub(".*retira y hace presente.*urgencia.*[Ss]uma.*"                                 , replacement = "reset: urg15", tmp, perl = TRUE)
        tmp <- sub(".*[Rr]etira y hace presente.*urgencia.*[Dd]iscusión [Ii]nmediata.*"            , replacement = "reset: urg30", tmp, perl = TRUE)
        tmp <- sub(".*Corte Suprema.*urgencia.*"                                                   , replacement = "urg on @SC", tmp, perl = TRUE)
        tmp <- sub(".*Rep[.].*retir[oa].*urgencia.*"                                               , replacement = "urg off", tmp, perl = TRUE)
        tmp <- sub(".*República retira.*urgencia.*"                                                , replacement = "urg off", tmp, perl = TRUE)
        tmp <- sub(".*Ejecutivo.*retir[oa].*urgencia.*"                                            , replacement = "urg off", tmp, perl = TRUE)
        tmp <- sub("^Cuenta [Mm]ensaje.*retir[oa].*urgencia.*"                                     , replacement = "urg off", tmp, perl = TRUE)
        tmp <- sub("^Urgencia \\\\suma\\\\.*"                                                      , replacement = "urg15 on", tmp, perl = TRUE)
        tmp <- sub("Vicepresidente.*hace presente urgencia \\\\suma\\\\.*"                         , replacement = "urg15 on", tmp, perl = TRUE)
        tmp <- sub("^Urgencia \\\\discusión inmediata\\\\.*"                                       , replacement = "urg06 on", tmp, perl = TRUE)
        tmp <- sub("^Como la urgencia.*es de discusión inmediata.*"                                , replacement = "urg06 on", tmp, perl = TRUE)
        tmp <- sub(".*informe.*mensaje con urgencia suma.*"                                        , replacement = "urg06 still on", tmp, perl = TRUE)
        tmp <- sub(".*informe.*con urgencia simple.*"                                              , replacement = "urg30 still on", tmp, perl = TRUE)
        tmp <- sub(".*proyecto. con urgencia suma.*"                                               , replacement = "urg15 on", tmp, perl = TRUE)
        tmp <- sub(".*proyecto.*tiene urgencia.*[Dd]iscusión [Ii]nmediata.*"                       , replacement = "urg06 on", tmp, perl = TRUE)
        tmp <- sub(".*proyecto.*viene.*urgencia.*[Ss]uma.*"                                        , replacement = "urg15 on", tmp, perl = TRUE)
        tmp <- sub(".*proyecto.*tabla urgencia.*[Dd]iscusión [Ii]nmediata.*"                       , replacement = "urg06 on", tmp, perl = TRUE)
        tmp <- sub(".*solicitar al Presidente.*el retiro de la urgencia.*"                         , replacement = "ask urg off", tmp, perl = TRUE)
        tmp <- sub(".*ha aprobado el proyecto con urgencia.*"                                      , replacement = "urg30 on", tmp, perl = TRUE)
        bills$hitos[[i]]$urg[tmp1] <- tmp
    }
}

## loop over hitos in search of veto info
for (i in 1:I){
    message(sprintf("loop %s of %s", i, I))
    tmp1 <- grep(pattern = "[Vv]eto", bills$hitos[[i]]$action) # try Art. 70 (bicam veto orr)
    tmp2 <- grep(pattern = "([Aa]rt.|artículo) 73", bills$hitos[[i]]$action)
#    tmp3 <- grep(pattern = "insistencia", bills$hitos[[i]]$action) # do this in other loop to find bicameral overrule with president's request art. 68
    tmp1 <- union(tmp1, tmp2) # remove repeated lines
    bills$hitos[[i]]$vet <- rep(".", nrow(bills$hitos[[i]]))
    if (length(tmp1)>0){ 
        tmp <- bills$hitos[[i]]$action[tmp1] # which record
        tmp <- sub(".*(Vicep|Presidente|P. *de la* Re*p*.).*([Cc]omunica|manifiesta).*no (hará|hacer) uso.*([Aa]rt[.]|artículo) 73.*", replacement = "no veto", tmp)
        tmp <- sub(".*(Ejecutivo|President.).*([Cc]omunica|informa|señala).*no (har[áa]|hacer) uso.*([Aa]rt[.]|artículo) 73.*", replacement = "no veto", tmp)
        tmp <- sub(".*[Pp]resident[ea].*[Cc]omunica.*no hará uso.*(Art[.]|artículo) 70.*", replacement = ".", tmp) #art70=bicamOverrule
        tmp <- sub(".*P(dte)*[.] de la Rep[.].*[Cc]omun[ic]{2}a.*no hará uso.*[Vv]eto.*" , replacement = "no veto", tmp)
        tmp <- sub(".*Oficio.*resultado.*veto.*"                                         , replacement = "tell override outcome", tmp)   
        tmp <- sub(".*Oficio rechazo veto.*comunica rechazo e insistencia.*"             , replacement = "tell veto overridden", tmp)
        tmp <- sub(".*Oficio rechazo veto a Cámara.*"                                    , replacement = "tell veto sustained", tmp)
        tmp <- sub(".*Oficio rechazo insistencia a Cámara.*"                             , replacement = "no bicam overrule?", tmp)
        tmp <- sub(".*Oficio aprobación insistencia a Cámara.*"                          , replacement = "bicam overrule?", tmp)
        tmp <- sub(".*[Oo]ficio a Cámara.*aprobando insistencia.*"                       , replacement = "tell veto overridden", tmp)
        tmp <- sub(".*Oficio.*Comunica.*proyecto a.*Presidenta.*efectos.*artículo 73.*"  , replacement = ".", tmp)
        tmp <- sub(".*Oficio.*[Cc]onsulta.*(Presidente|[Ee]jecutivo).*(ejerc[ie][cr](io)*|hará uso).*(veto|73).*", replacement = "ask if will veto", tmp)
        tmp <- sub(".*Oficio.*[Aa]l*.*(President[ea]|[Ee]jecutivo).*([Aa]rt.|artículo) 73.*" , replacement = ".", tmp)
        tmp <- sub(".*Oficio.*[Vv]eto [Pp]residencial.*"                                     , replacement = "veto", tmp)
        tmp <- sub(".*Oficio.*Observaciones del Ejecutivo.*"                                 , replacement = "veto", tmp)
        tmp <- sub(".*Presidente.*[Cc]omunica.*resuelto hacer uso.*([Aa]rt[.]|artículo) 73.*", replacement = "veto", tmp)
        tmp <- sub(".*Oficio.*[Rr]emite proyecto( aprobado)*.*ejercer.*facultad de veto.*"   , replacement = ".", tmp)
        tmp <- sub(".*Oficio de ley.*[Pp]ara.*ejerc[ie][cr](io)*.*facultad.*[Vv]eto.*"       , replacement = ".", tmp)
        tmp <- sub(".*Oficio de [Cc]onsulta.*facultad.*[Vv]eto.*"                            , replacement = ".", tmp)
        tmp <- sub(".*Oficio de ley.*([Cc]onsulta|saber).*facultad.*[Vv]eto.*"               , replacement = ".", tmp)
        tmp <- sub(".*Oficio aprobación veto a Cámara.*"                                     , replacement = "tell veto sustained", tmp)
        tmp <- sub(".*eximir el veto del trámite.*"                                          , replacement = "override disc", tmp)
        tmp <- sub(".*[Cc]omunica texto aprobado.*ejerc[ie][cr](io)*.*veto.*"                , replacement = ".", tmp)
        tmp <- sub(".*[Cc]onsulta.*(Presidente|Ejecutivo).*ejerc[ie][cr](io)*.*veto.*"       , replacement = ".", tmp)
        tmp <- sub(".*Informe.*sobre veto presidencial.*tabla"                               , replacement = "override disc", tmp)
        tmp <- sub(".*Discusión veto.*[Ss]e aprueban.*observaciones.*con excepci.n.*"        , replacement = "veto sustained in part", tmp)
        tmp <- sub(".*Discusión veto.*[Ss]e aprueban.*observaciones.*"                       , replacement = "veto sustained", tmp)
        tmp <- sub(".*Discusión veto.*[Ss]egunda discusión.*"                                , replacement = "override disc", tmp)
        tmp <- sub(".*Discusión veto.*Se aprueba.*y rechazan.*"                              , replacement = "veto sustained in part", tmp)
        tmp <- sub(".*Discusión veto.*[Rr]echaza[dn][o]*.*"                                  , replacement = "veto overridden", tmp)
        tmp <- sub(".*Discusión veto.*[Pp]endiente su discusión.*"                           , replacement = "override disc", tmp)
        tmp <- sub(".*Discusión veto.*[Aa]probado*.*"                                        , replacement = "veto sustained", tmp) # VERIFY, ACCEPTED
        tmp <- sub("^Discusión veto$"                                                        , replacement = "override disc", tmp)
        tmp <- sub(".*Cuenta veto presidencial.*"                                            , replacement = "veto", tmp)
        tmp <- sub(".*Cuenta [Oo]ficio rechazo [Vv]eto.*"                                    , replacement = "override disc", tmp)
        tmp <- sub(".*Cuenta( del)* [Oo]ficio.*([Pp]resident[ea]|Ejecutivo).*no (hará|hacer*) uso.*veto.*", replacement = "no veto", tmp)
        tmp <- sub(".*Cuenta [Oo]ficio aprobación veto.*aprobado parcialmente.*"             , replacement = "veto sustained in part", tmp)
        tmp <- sub(".*Cuenta [Oo]ficio aprobación veto.*manda comunicar.*[Pp]resident[ea].*" , replacement = "veto sustained", tmp)
        tmp <- sub(".*Cuenta [Oo]ficio aprobación veto.*remite.*al proyecto.*"               , replacement = "override disc", tmp)
        tmp <- sub(".*Cuenta [Oo]ficio aprobación veto.*[Pp]asa a.*[Cc]omisión.*"            , replacement = "override disc", tmp)
        tmp <- sub("^Cuenta [Oo]ficio aprobación veto$"                                      , replacement = "override disc", tmp);
        bills$hitos[[i]]$vet[tmp1] <- tmp;
    }
    # adds info to describe hitos
    bills$hitos[[i]]$bol <- rep(bills$info$bol[i], times = nrow(bills$hitos[[i]]));
    bills$hitos[[i]]$durgPest <- rep(bills$info$hasUrg[i], times = nrow(bills$hitos[[i]]));
    bills$hitos[[i]]$dvetPest <- rep(bills$info$hasVeto[i], times = nrow(bills$hitos[[i]]));
    ## next block seeks inconsistencies between urgencia tab and urgencia info in hitos (spots very few post 2006)
    if (length(grep(pattern = "[^.]", bills$hitos[[i]]$urg))>0 & bills$info$hasUrg[i]=="yes"){         # both report urgencia
        tmp <- 1
    } else if (length(grep(pattern = "[^.]", bills$hitos[[i]]$urg))==0 & bills$info$hasUrg[i]=="yes"){ # hito misses urgencia
        tmp <- 2
    } else if (length(grep(pattern = "[^.]", bills$hitos[[i]]$urg))>0 & bills$info$hasUrg[i]=="no"){   # uTab misses urgencia
        tmp <- 3
    } else {                                                                                           # none report urgencia
        tmp <- 4
    }
    bills$hitos[[i]]$debug <- rep(tmp, times = nrow(bills$hitos[[i]]));
    bills$info$debug[i] <- tmp
    bills$info$hasUrgHU[i] <- ifelse( length(grep(pattern = "[^.]", bills$hitos[[i]]$urg))==0 & bills$info$hasUrg[i]=="no", "no", "yes" )
}
rm(i, tmp, tmp1, tmp2)

# preliminary analysis
library(lubridate)
# compare urgencia report in Urgencias and in Hitos
table(bills$info$debug[bills$info$dateIn>=dmy("1/3/1990", tz = "chile") & bills$info$dateIn<dmy("1/3/1994", tz = "chile")])
table(bills$info$debug[bills$info$dateIn>=dmy("1/3/1994", tz = "chile") & bills$info$dateIn<dmy("1/3/1998", tz = "chile")])
table(bills$info$debug[bills$info$dateIn>=dmy("1/3/1998", tz = "chile") & bills$info$dateIn<dmy("1/3/2002", tz = "chile")])
table(bills$info$debug[bills$info$dateIn>=dmy("1/3/2002", tz = "chile") & bills$info$dateIn<dmy("1/3/2006", tz = "chile")])
table(bills$info$debug[bills$info$dateIn>=dmy("1/3/2006", tz = "chile") & bills$info$dateIn<dmy("1/3/2010", tz = "chile")])
table(bills$info$debug[bills$info$dateIn>=dmy("1/3/2010", tz = "chile")])
table(bills$info$debug)
#
# crosstabs of urgencias and mensajes by yr
bills$info$legyr <- 0
bills$info$legyr[bills$info$dateIn>=dmy("1/3/1990", tz = "chile") & bills$info$dateIn<dmy("1/3/1991", tz = "chile")] <- 1
bills$info$legyr[bills$info$dateIn>=dmy("1/3/1991", tz = "chile") & bills$info$dateIn<dmy("1/3/1992", tz = "chile")] <- 2
bills$info$legyr[bills$info$dateIn>=dmy("1/3/1992", tz = "chile") & bills$info$dateIn<dmy("1/3/1993", tz = "chile")] <- 3
bills$info$legyr[bills$info$dateIn>=dmy("1/3/1993", tz = "chile") & bills$info$dateIn<dmy("1/3/1994", tz = "chile")] <- 4
bills$info$legyr[bills$info$dateIn>=dmy("1/3/1994", tz = "chile") & bills$info$dateIn<dmy("1/3/1995", tz = "chile")] <- 5
bills$info$legyr[bills$info$dateIn>=dmy("1/3/1995", tz = "chile") & bills$info$dateIn<dmy("1/3/1996", tz = "chile")] <- 6
bills$info$legyr[bills$info$dateIn>=dmy("1/3/1996", tz = "chile") & bills$info$dateIn<dmy("1/3/1997", tz = "chile")] <- 7
bills$info$legyr[bills$info$dateIn>=dmy("1/3/1997", tz = "chile") & bills$info$dateIn<dmy("1/3/1998", tz = "chile")] <- 8
bills$info$legyr[bills$info$dateIn>=dmy("1/3/1998", tz = "chile") & bills$info$dateIn<dmy("1/3/1999", tz = "chile")] <- 9
bills$info$legyr[bills$info$dateIn>=dmy("1/3/1999", tz = "chile") & bills$info$dateIn<dmy("1/3/2000", tz = "chile")] <- 10
bills$info$legyr[bills$info$dateIn>=dmy("1/3/2000", tz = "chile") & bills$info$dateIn<dmy("1/3/2001", tz = "chile")] <- 11
bills$info$legyr[bills$info$dateIn>=dmy("1/3/2001", tz = "chile") & bills$info$dateIn<dmy("1/3/2002", tz = "chile")] <- 12
bills$info$legyr[bills$info$dateIn>=dmy("1/3/2002", tz = "chile") & bills$info$dateIn<dmy("1/3/2003", tz = "chile")] <- 13
bills$info$legyr[bills$info$dateIn>=dmy("1/3/2003", tz = "chile") & bills$info$dateIn<dmy("1/3/2004", tz = "chile")] <- 14
bills$info$legyr[bills$info$dateIn>=dmy("1/3/2004", tz = "chile") & bills$info$dateIn<dmy("1/3/2005", tz = "chile")] <- 15
bills$info$legyr[bills$info$dateIn>=dmy("1/3/2005", tz = "chile") & bills$info$dateIn<dmy("1/3/2006", tz = "chile")] <- 16
bills$info$legyr[bills$info$dateIn>=dmy("1/3/2006", tz = "chile") & bills$info$dateIn<dmy("1/3/2007", tz = "chile")] <- 17
bills$info$legyr[bills$info$dateIn>=dmy("1/3/2007", tz = "chile") & bills$info$dateIn<dmy("1/3/2008", tz = "chile")] <- 18
bills$info$legyr[bills$info$dateIn>=dmy("1/3/2008", tz = "chile") & bills$info$dateIn<dmy("1/3/2009", tz = "chile")] <- 19
bills$info$legyr[bills$info$dateIn>=dmy("1/3/2009", tz = "chile") & bills$info$dateIn<dmy("1/3/2010", tz = "chile")] <- 20
bills$info$legyr[bills$info$dateIn>=dmy("1/3/2010", tz = "chile") & bills$info$dateIn<dmy("1/3/2011", tz = "chile")] <- 21
bills$info$legyr[bills$info$dateIn>=dmy("1/3/2011", tz = "chile") & bills$info$dateIn<dmy("1/3/2012", tz = "chile")] <- 22
bills$info$legyr[bills$info$dateIn>=dmy("1/3/2012", tz = "chile") & bills$info$dateIn<dmy("1/3/2013", tz = "chile")] <- 23
bills$info$legyr[bills$info$dateIn>=dmy("1/3/2013", tz = "chile") & bills$info$dateIn<dmy("1/3/2014", tz = "chile")] <- 24
#
for (i in 1:24){ # loop over legislative years
    sel <- which(bills$info$legyr==i); tmp <- table(bills$info$dmensaje[sel], bills$info$hasUrgH[sel], useNA = "ifany")
    print(cbind(round(prop.table(tmp, 1), digits = 2), margin.table(tmp, 1))) # crosstab with shares and column margins
}
tmp <- table(bills$info$dmensaje, bills$info$hasUrgH, useNA = "ifany") # whole period
print(cbind(round(prop.table(tmp, 1), digits = 2), margin.table(tmp, 1))) # crosstab with shares and column margins
#
sel <- which(bills$info$dateIn>=dmy("1/3/1990", tz = "chile") & bills$info$dateIn<dmy("1/3/2006", tz = "chile")) # period in Alemán and Navia
tmp <- table(bills$info$dmensaje[sel], bills$info$hasUrgH[sel], useNA = "ifany")
print(cbind(round(prop.table(tmp, 1), digits = 2), margin.table(tmp, 1))) # crosstab with shares and column margins
#
# by legislature
sel <- which(bills$info$dateIn>=dmy("1/3/1990", tz = "chile") & bills$info$dateIn<dmy("1/3/1994", tz = "chile")) # period in Alemán and Navia
tmp <- table(bills$info$dmensaje[sel], bills$info$hasUrgH[sel], useNA = "ifany")
print(cbind(round(prop.table(tmp, 1), digits = 2), margin.table(tmp, 1))) # crosstab with shares and column margins
sel <- which(bills$info$dateIn>=dmy("1/3/1994", tz = "chile") & bills$info$dateIn<dmy("1/3/1998", tz = "chile")) # period in Alemán and Navia
tmp <- table(bills$info$dmensaje[sel], bills$info$hasUrgH[sel], useNA = "ifany")
print(cbind(round(prop.table(tmp, 1), digits = 2), margin.table(tmp, 1))) # crosstab with shares and column margins
sel <- which(bills$info$dateIn>=dmy("1/3/1998", tz = "chile") & bills$info$dateIn<dmy("1/3/2002", tz = "chile")) # period in Alemán and Navia
tmp <- table(bills$info$dmensaje[sel], bills$info$hasUrgH[sel], useNA = "ifany")
print(cbind(round(prop.table(tmp, 1), digits = 2), margin.table(tmp, 1))) # crosstab with shares and column margins
sel <- which(bills$info$dateIn>=dmy("1/3/2002", tz = "chile") & bills$info$dateIn<dmy("1/3/2006", tz = "chile")) # period in Alemán and Navia
tmp <- table(bills$info$dmensaje[sel], bills$info$hasUrgH[sel], useNA = "ifany")
print(cbind(round(prop.table(tmp, 1), digits = 2), margin.table(tmp, 1))) # crosstab with shares and column margins
sel <- which(bills$info$dateIn>=dmy("1/3/2006", tz = "chile") & bills$info$dateIn<dmy("1/3/2010", tz = "chile")) # period in Alemán and Navia
tmp <- table(bills$info$dmensaje[sel], bills$info$hasUrgH[sel], useNA = "ifany")
print(cbind(round(prop.table(tmp, 1), digits = 2), margin.table(tmp, 1))) # crosstab with shares and column margins
sel <- which(bills$info$dateIn>=dmy("1/3/2006", tz = "chile") & bills$info$dateIn<dmy("1/3/2014", tz = "chile")) # period in Alemán and Navia
tmp <- table(bills$info$dmensaje[sel], bills$info$hasUrgH[sel], useNA = "ifany")
print(cbind(round(prop.table(tmp, 1), digits = 2), margin.table(tmp, 1))) # crosstab with shares and column margins

## # bind together hitos data.frames of different bills... export as csv to see it in excel
## library(plyr)
## sel <- which(bills$info$dateIn > dmy("28/2/2002"))
## ## sel <- runif(n=I); sel <- which(sel<.25) # process 25% randomly
## ## table(sel)
## tmp <- rbind.fill(bills$hitos[sel]) 
## head(tmp)
## #
## tmp2 <- tmp[, c("bol", "date", "tramite", "chamber", "urg", "vet", "durgPest", "dvetPest", "action", "debug")]
## tmp2$bolnum <- as.numeric(sub(pattern = "([0-9]+)-[0-9]+", replacement = "\\1", tmp2$bol))
## tmp2 <- tmp2[order(tmp2$bolnum),]
## tmp2$color <- 0; for (i in 2:nrow(tmp2)) tmp2$color[i] <- ifelse(tmp2$bolnum[i]==tmp2$bolnum[i-1], tmp2$color[i-1], 1 - tmp2$color[i-1])
## table(tmp2$color)
## write.csv(tmp2, file = "tmp.csv")

## table(tmp)

#########################
# systematize urgencias #
#########################
library(plyr)
library(lubridate)
bills$urgRaw <- bills$urgencias # preserve raw data
names(bills)[grep("urgencias", names(bills))] <- "urg" # shortens the object's name
bills$urg <- sapply(1:I, function(x) NULL) # initializes empty list with I elements (unnamed; names would go where 1:I)
#
work <- which(bills$info$hasUrg=="yes") # work only this in loop <-- OJO: there are 6 bills w urgencia in hitos only since 1998 (11 since 1990) GET THEM
#
# new slots for info # OJO: NEED TO ADJUST THESE
bills$info$nUrg <- 0
bills$info$nInChains <- 0
bills$info$nSimple <- 0
bills$info$nSuma <- 0
bills$info$nInmed <- 0
bills$info$nRet <- 0
#bills$info$nExtended <- 0
#bills$info$nShortened <- 0
#bills$info$nInDip <- 0
#bills$info$nInSen <- 0

# neat function to compute urgencia dealines excluding week-ends
# solution 1 takes holidays other than weekends into account, which is a plus
library(timeDate)
deadline <- function(x, nBizDys = 6){ # function to process deadlines excluding week-ends and holidays... how do you change default=holidayNYSE with non-prepackaged holidays (eg., Chile's http://www.feriadoschilenos.cl/)? NYSE: Jan1, MLKJan20, WashBdFeb17, GoodFdy, MemDyMay26, Jul4, LabDySep1, ThksNov2627, Dec25... Chile: Jan1, May1, DiscPdteCongMay21, Sep18-19, Dec25 (SemSta?)... check http://stackoverflow.com/questions/26777282/in-using-timedate-r-package-i-receive-an-error-when-specifying-gbnewyearseve
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

# redefine %within% to exclude upper bounds
"%my_within%" <- function(a,b) standardGeneric("%my_within%")
setGeneric("%my_within%")
#
setMethod("%my_within%", signature(b = "Interval"), function(a,b){
    if(!is.instant(a)) stop("Argument 1 is not a recognized date-time")
    a <- as.POSIXct(a)
    (as.numeric(a) - as.numeric(b@start) < b@.Data) & (as.numeric(a) - as.numeric(b@start) >= 0)
})
#
setMethod("%my_within%", signature(a = "Interval", b = "Interval"), function(a,b){
    a <- int_standardize(a)
    b <- int_standardize(b)
    start.in <- as.numeric(a@start) >= as.numeric(b@start) 
    end.in <- (as.numeric(a@start) + a@.Data) < (as.numeric(b@start) + b@.Data)
    start.in & end.in
})

save.image("tmp2.RData")

rm(list=ls())
datdir <- "/home/eric/Dropbox/data/latAm/chile/data/" 
setwd(datdir)
load(file = "tmp2.RData")
options(width = 150)
library(plyr)
library(lubridate)
library(timeDate)

# clean data from source
i <- which(bills$info$bol=="1484-01")
tmp <- bills$urgRaw[[i]]; tmp <- tmp[c(-2,-3,-5)]
bills$urgRaw[[i]] <- tmp
#
i <- which(bills$info$bol=="2296-18")
tmp <- bills$urgRaw[[i]]; tmp[3] <- "17 de Jul. de 2002   Simple 237-339  "; tmp <- tmp[-2]; 
bills$urgRaw[[i]] <- tmp
#
i <- which(bills$info$bol=="2347-15")
tmp <- bills$urgRaw[[i]]; tmp[11] <- "04 de Ago. de 1999 17 de Ago. de 1999 Suma 73-340 126-340"; tmp <- tmp[c(-2, -6:-9)]; 
bills$urgRaw[[i]] <- tmp
#
i <- which(bills$info$bol=="4639-11")
tmp <- bills$urgRaw[[i]]; tmp[2] <- "14 de Nov. de 2006 28 de Nov. de 2006 Suma 466-354 504-354"; tmp <- tmp[-4]; 
bills$urgRaw[[i]] <- tmp
#
i <- which(bills$info$bol=="2035-06")
tmp <- bills$urgRaw[[i]];
tmp[2] <- "02 de May. de 2001   Suma 0020501  "; tmp[4] <- "10 de Abr. de 2001 18 de Abr. de 2001 Suma 395-343 0020501"; 
bills$urgRaw[[i]] <- tmp
#
i <- which(bills$info$bol=="2121-04")
tmp <- bills$tramites[[i]]; tmp$to[2] <- dmy("20-01-1998", tz = "chile"); tmp$period[2] <- new_interval(tmp$from[2], tmp$to[2]); tmp <- tmp[-3:-5,]
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="114-06")
tmp <- bills$tramites[[i]]; tmp$to[2] <- tmp$to[3]; tmp$from[3] <- tmp$to[3]; tmp$to[3] <- tmp$to[4]; tmp$from[4] <- tmp$to[4]; tmp$tramite[3] <- "ejec"; tmp$tramite[4] <- "veto"; tmp$period <- new_interval(tmp$from, tmp$to) # ojo, infiero veto porque últ hito menciona ingreso de observaciones, sin más
#old tmp <- bills$tramites[[i]]; tmp$to[4] <- tmp$from[4] + days(1); tmp$period[4] <- new_interval(tmp$from[4], tmp$to[4]); tmp <- tmp[-5,]
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="1902-17")
tmp <- bills$tramites[[i]]; tmp$to[2] <- dmy("21-12-2000", tz = "chile"); tmp$from[3] <- dmy("21-12-2000", tz = "chile"); tmp$tramite[3] <- "dip"; tmp <- tmp[-4,]; tmp$period <- new_interval(tmp$from, tmp$to)
#old tmp <- bills$tramites[[i]]; tmp <- tmp[c(-3,-4),]
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="2036-11")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[4,]); tmp$to[4] <- tmp$from[4] + days(7); tmp$from[5] <- tmp$to[4]; tmp$tramite[5] <- "veto"; tmp$period <- new_interval(tmp$from, tmp$to) # ojo, infiero veto porque últ hito menciona ingreso de observaciones, sin más
#old tmp <- bills$tramites[[i]]; tmp$to[4] <- tmp$from[4] + days(1); tmp$from[5] <- tmp$to[4]; tmp$to[5] <- tmp$from[5] + days(1); tmp$period[4] <- new_interval(tmp$from[4], tmp$to[4]); tmp$period[5] <- new_interval(tmp$from[5], tmp$to[5]); 
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="2185-06")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[4,]); tmp$to[4] <- tmp$from[4] + days(7); tmp$from[5] <- tmp$to[4]; tmp$tramite[5] <- "veto"; tmp$period <- new_interval(tmp$from, tmp$to) # ojo, infiero veto porque últ hito menciona ingreso de observaciones, sin más
#old tmp <- bills$tramites[[i]]; tmp$to[4] <- tmp$from[4] + days(1); tmp$from[5] <- tmp$to[4]; tmp$to[5] <- tmp$from[5] + days(1); tmp$period[4] <- new_interval(tmp$from[4], tmp$to[4]); tmp$period[5] <- new_interval(tmp$from[5], tmp$to[5]); 
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="2361-23")
tmp <- bills$tramites[[i]]; tmp <- tmp[c(-4,-6:-11,-14,-15),]; tmp$tramite[5] <- "ejec"; tmp$to[4] <- tmp$from[5]; tmp$to[5] <- tmp$to[5] + days(1); tmp$from[6] <- tmp$to[5]; tmp$from[7] <- tmp$to[6]; tmp$period <- new_interval(tmp$from, tmp$to)
#old tmp <- bills$tramites[[i]]; tmp <- tmp[c(-4,-6:-11),]  ## OJO: AQUÍ ME ESTOY COMIENDO UN RENGLON DE MAS... LO ARREGLO MAS ABAJO
bills$tramites[[i]] <- tmp
#
## i <- which(bills$info$bol=="2496-15")
## tmp <- bills$tramites[[i]]; tmp$to[4] <- tmp$from[4] + days(1); tmp$from[5] <- tmp$to[4]; tmp$period[4] <- new_interval(tmp$from[4], tmp$to[4]); tmp$period[5] <- new_interval(tmp$from[5], tmp$to[5]); 
## bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="737-03")
tmp <- bills$tramites[[i]]; tmp <- tmp[-4:-5,]; tmp$to[3] <- tmp$from[4]; tmp$period <- new_interval(tmp$from, tmp$to); 
#old tmp <- bills$tramites[[i]]; tmp$to[7] <- tmp$from[7] + days(1); tmp$from[8] <- tmp$to[7]; tmp$to[8] <- tmp$from[8] + days(1); tmp$period[7] <- new_interval(tmp$from[7], tmp$to[7]); tmp$period[8] <- new_interval(tmp$from[8], tmp$to[8]); 
bills$tramites[[i]] <- tmp
#
# fill missing trámites from urg by hand
i <- which(bills$info$bol=="561-06")
bills$urgRaw[[i]][2] <-  "18 de Dic. de 1991   Suma    " # decía 18 agosto 1992
#
i <- which(bills$info$bol=="679-05")
bills$urgRaw[[i]][2] <-  "06 de May. de 1992   Suma    " # decía 1991
#
i <- which(bills$info$bol=="1077-05")
bills$urgRaw[[i]][2] <-  "17 de Dic. de 1993   Simple    " # decía 1992
#
i <- which(bills$info$bol=="1135-04")
bills$urgRaw[[i]][3] <-   "22 de Mar. de 1994 05 de Abr. de 1994 Suma 220394 050494" # decía 1992
#
i <- which(bills$info$bol=="1240-11")
bills$urgRaw[[i]][2] <- "14 de Jul. de 1994 02 de Ago. de 1994 Suma 190794 20894"
bills$urgRaw[[i]][4] <- "05 de Jul. de 1994 19 de Jul. de 1994 Suma 050794 190794"
#
i <- which(bills$info$bol=="1444-15")
bills$urgRaw[[i]][2] <-   "22 de Nov. de 1994  Simple"
#
i <- which(bills$info$bol=="1575-10")
bills$urgRaw[[i]][2] <-   "02 de May. de 2001  Simple 428-343  "
#
i <- which(bills$info$bol=="1738-04")
bills$urgRaw[[i]][2] <-    "05 de Oct. de 2001   Suma 2-345  " # decía 5 mar 2002, pareciera que urgencia es sobre tribunal const
#
i <- which(bills$info$bol=="1906-04")
bills$urgRaw[[i]][2] <-     "09 de Ago. de 1997   Suma    " # decía 1995
#
i <- which(bills$info$bol=="2265-01")
bills$urgRaw[[i]][8] <-     "02 de Sep. de 1999   Suma   25-340  " # decía 1997
#
i <- which(bills$info$bol=="2361-23")
bills$urgRaw[[i]][2] <- "28 de Ago. de 2004 31 de Ago. de 2004 Suma 286-351 333-351"
#old bills$tramites[[i]]$from[5] <- dmy("19/10/2004", tz = "chile")
#
i <- which(bills$info$bol=="2374-07")
bills$urgRaw[[i]][2] <- "04 de Nov. de 1999   Suma 110-342  "
#
i <- which(bills$info$bol=="2628-13")
bills$urgRaw[[i]][2] <- "04 de Ene. de 2002   Simple  170-345  "
#
i <- which(bills$info$bol=="2922-08")
bills$urgRaw[[i]][2] <-  "18 de Oct. de 2003 28 de Oct. de 2003 Suma 112-350 114-350"
bills$urgRaw[[i]][3] <- "15 de Ene. de 2004    Suma 365-350 "
#
i <- which(bills$info$bol=="3098-06")
bills$urgRaw[[i]][20] <- "08 de Jul. de 2003 05 de Ago. de 2003 Simple 101-346 157-349"
#
i <- which(bills$info$bol=="3190-04")
bills$urgRaw[[i]][4] <- "15 de Abr. de 2003   Simple 536-348  "
#
i <- which(bills$info$bol=="3406-03")
bills$urgRaw[[i]][3] <-  "04 de Nov. de 2003   Discusión inmediata 133-350  "
bills$tramites[[i]]$to[1] <- dmy("05/11/2003", tz = "chile")
bills$tramites[[i]]$from[2] <- dmy("05/11/2003", tz = "chile")
bills$tramites[[i]]$period <- new_interval(bills$tramites[[i]]$from, bills$tramites[[i]]$to)
#
i <- which(bills$info$bol=="3447-15")
bills$urgRaw[[i]] <- bills$urgRaw[[i]][-2]
#
i <- which(bills$info$bol=="3671-03")
bills$urgRaw[[i]][7] <- "21 de Jun. de 2005 02 de Ago. de 2005 Simple 58-353 142-353"
#
i <- which(bills$info$bol=="3885-07")
bills$urgRaw[[i]][10] <- "21 de Jun. de 2005 05 de Jul. de 2005 Discusión inmediata 57-353 89-353"
#
i <- which(bills$info$bol=="3899-05")
bills$urgRaw[[i]] <- gsub(pattern = "0005", replacement = "2005", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="4040-06")
bills$urgRaw[[i]] <- gsub(pattern = "0007", replacement = "2007", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="4545-11")
bills$urgRaw[[i]] <- gsub(pattern = "0007", replacement = "2007", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="4742-13")
bills$urgRaw[[i]] <- gsub(pattern = "0007", replacement = "2007", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="4813-06")
bills$urgRaw[[i]] <- gsub(pattern = "0007", replacement = "2007", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="4814-13")
bills$urgRaw[[i]] <- gsub(pattern = "0007", replacement = "2007", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="4900-27")
bills$urgRaw[[i]] <- gsub(pattern = "0007", replacement = "2007", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="4937-18")
bills$urgRaw[[i]] <- gsub(pattern = "0010", replacement = "2010", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="4970-04")
bills$urgRaw[[i]] <- gsub(pattern = "0007", replacement = "2007", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="5076-15")
bills$urgRaw[[i]] <- gsub(pattern = "0007", replacement = "2007", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="5080-11")
bills$urgRaw[[i]] <- gsub(pattern = "0007", replacement = "2007", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="5081-15")
bills$urgRaw[[i]] <- gsub(pattern = "0007", replacement = "2007", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="5083-04")
bills$urgRaw[[i]] <- gsub(pattern = "0010", replacement = "2010", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="5091-15")
bills$urgRaw[[i]] <- gsub(pattern = "0007", replacement = "2007", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="5114-10")
bills$urgRaw[[i]] <- gsub(pattern = "0007", replacement = "2007", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="5120-21")
bills$urgRaw[[i]] <- gsub(pattern = "0007", replacement = "2007", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="5148-05")
bills$urgRaw[[i]] <- gsub(pattern = "0007", replacement = "2007", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="5173-05")
bills$urgRaw[[i]] <- gsub(pattern = "0007", replacement = "2007", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="5308-18")
bills$urgRaw[[i]] <- gsub(pattern = "0010", replacement = "2010", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="5431-11")
bills$urgRaw[[i]] <- gsub(pattern = "0008", replacement = "2008", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="5687-23")
bills$urgRaw[[i]] <- gsub(pattern = "0010", replacement = "2010", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="5763-05")
bills$urgRaw[[i]] <- gsub(pattern = "0008", replacement = "2008", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="5898-07")
bills$urgRaw[[i]] <- gsub(pattern = "0010", replacement = "2010", bills$urgRaw[[i]])
#
## i <- which(bills$info$bol=="6110-24")
## bills$urgRaw[[i]] <- NULL # urgencias en nov 2014, post corte
## bills$urg[[i]] <- NULL
## bills$info$nUrg[i] <- bills$info$nSimple[i] <- 0
#
i <- which(bills$info$bol=="6189-06")
bills$urgRaw[[i]] <- gsub(pattern = "0010", replacement = "2010", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="6231-05")
bills$urgRaw[[i]] <- gsub(pattern = "2009", replacement = "2008", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="6252-09")
bills$urgRaw[[i]] <- gsub(pattern = "0010", replacement = "2010", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="6443-07")
tmp <- bills$tramites[[i]]; tmp <- tmp[-3,]; bills$tramites[[i]] <- tmp
##old bills$tramites[[i]]$to[4] <- dmy("04/06/2009", tz = "chile")
##old bills$tramites[[i]]$from[5] <- dmy("04/06/2009", tz = "chile")
##old bills$tramites[[i]]$to[5] <- dmy("12/06/2009", tz = "chile")
bills$urgRaw[[i]] <- gsub(pattern = "Ago.", replacement = "Jun.", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="6562-07")
bills$urgRaw[[i]] <- gsub(pattern = "0010", replacement = "2010", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="6582-11")
bills$urgRaw[[i]] <- gsub(pattern = "0010", replacement = "2010", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="6586-15")
bills$urgRaw[[i]] <- gsub(pattern = "0010", replacement = "2010", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="6628-06")
bills$urgRaw[[i]] <- gsub(pattern = "0010", replacement = "2010", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="6639-25")
bills$urgRaw[[i]] <- gsub(pattern = "0010", replacement = "2010", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="6691-07")
bills$urgRaw[[i]] <- gsub(pattern = "0010", replacement = "2010", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="6726-06")
bills$urgRaw[[i]] <- gsub(pattern = "0010", replacement = "2010", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="6739-02")
bills$urgRaw[[i]] <- gsub(pattern = "0010", replacement = "2010", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="6758-15")
bills$urgRaw[[i]] <- gsub(pattern = "0010", replacement = "2010", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="6759-10")
bills$urgRaw[[i]] <- gsub(pattern = "0010", replacement = "2010", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="6791-06")
bills$urgRaw[[i]] <- gsub(pattern = "0010", replacement = "2010", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="7141-08")
bills$urgRaw[[i]] <- gsub(pattern = "1211", replacement = "2011", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="7203-02")
bills$urgRaw[[i]] <- gsub(pattern = "Ago.", replacement = "Sep.", bills$urgRaw[[i]])
#
i <- which(bills$info$bol=="7854-07")
bills$urgRaw[[i]] <- gsub(pattern = "1211", replacement = "2011", bills$urgRaw[[i]])
#
# OJO: AQUÍ DEBERÉ BUSCAR SI "Ingreso observaciones" EN TRÁMITE DE APROBACIÓN PRESIDENCIAL, SIN MÁS POSTERIORMENTE, ES INDICADOR DE UN VETO ACEPTADO POR DEFAULT... SI FUERA EL CASO, MODIFICARÍA LOS TRAMITES...

# manipulate tramites to remove fake tercer trámite when revisora made no changes to bill
tramVerif <- rep(0, I) # alternative would be adding +1 after each change (and include cases that do not exhaust problem below)
tmp1 <- rep(0, I) # will receive dummy sin modificaciones pointing to índices that need manipulation
tmp2 <- rep(0, I) # will receive length tramites
for (i in 1:I){ 
    if (length(grep("Oficio aprobaci[óo]n sin modificaciones a.*de [Oo]rigen", bills$hitos[[i]]$action))>0) tmp1[i] <- 1
    tmp2[i] <- nrow(bills$tramites[[i]])
}
#
sel <- c(1:I)[tmp1==1 & tmp2==3] # select indices of hitos mentioning no amendment by 2nd chamber and with nrow(tramites)==3
for (i in sel){
    if (bills$tramites[[i]]$tramite[3]=="ejec"){
        tmp1[i] <- 0 # if third tramite is executive, needs no manipulation
        tramVerif[i] <- 1
    }
}
sel <- c(1:I)[tmp1==1 & tmp2==3] # select indices of hitos mentioning no amendment by 2nd chamber and with nrow(tramites)==3 that all need work
for (i in sel){
    bills$tramites[[i]]$tramite[3] <- "ejec" # change third entry to executive
    tmp1[i] <- 0
    tramVerif[i] <- 1
}
sel <- c(1:I)[tmp1==1 & tmp2==4] # select indices of hitos mentioning no amendment by 2nd chamber and with nrow(tramites)==4 that all need work
for (i in sel){
    if (bills$tramites[[i]]$tramite[4]=="ejec"){ # if fourth trámite is ejec
        bills$tramites[[i]] <- bills$tramites[[i]][-3,] # then drop third trámite
        bills$tramites[[i]]$to[2] <- bills$tramites[[i]]$from[3] # and arrange period in case needed
        tmp1[i] <- 0 # remove from manipulation dummy
        tramVerif[i] <- 1
    }
}
#
sel <- c(1:I)[tmp1==1 & tmp2==4] # select indices of hitos mentioning no amendment by 2nd chamber and with nrow(tramites)==4 that all need work
# change by hand
i <- which(bills$info$bol=="3120-10")
tmp <- bills$tramites[[i]]; tmp <- tmp[-3,]; tmp$to[2] <- tmp$from[3]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="339-10")
tmp <- bills$tramites[[i]]; tmp <- tmp[-3,]; tmp$to[2] <- tmp$from[3]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="476-07")
tmp <- bills$tramites[[i]]; tmp <- tmp[-3,]; tmp$to[2] <- tmp$from[3]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="6690-10")
tmp <- bills$tramites[[i]]; tmp$from[4] <- dmy("03-03-2011", tz = "chile"); tmp$tramite[3] <- "trib"; tmp$tramite[4] <- "ejec"; tmp$to[3] <- tmp$from[4]
bills$tramites[[i]] <- tmp
#
tmp1[sel] <- 0
tramVerif[sel] <- 1
#
sel <- c(1:I)[tmp1==1 & tmp2==2] # select indices of hitos mentioning no amendment by 2nd chamber and with nrow(tramites)==2 that all need work
# change by hand
i <- which(bills$info$bol=="140-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,], tmp[2,]); tmp$to[1] <- dmy("04-12-1990", tz = "chile"); tmp$from[2] <- tmp$to[1]; tmp$to[2] <- dmy("05-03-1991", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$to[3] <- tmp$from[3] + days(1); tmp$from[4] <- tmp$to[3]; tmp$tramite[2] <- "dip"; tmp$tramite[4] <- "veto"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="147-13")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("24-04-1991", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="1711-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("22-05-1996", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="218-05")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("20-12-1990", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="257-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("17-04-1991", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="258-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("05-11-1992", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="291-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("10-04-1991", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="292-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("01-09-1993", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="321-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("01-09-1993", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="322-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("02-07-1992", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="323-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("22-01-1992", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="337-07")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("12-08-1991", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="347-13")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("16-05-1991", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="3517-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("19-10-2004", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[2] <- "sen"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="356-04")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("14-06-1994", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="366-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("08-10-1992", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="367-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("28-01-1992", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="377-06")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("04-12-1991", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="379-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("12-09-1991", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="387-04")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("22-04-1993", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="388-07")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("15-12-1992", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="403-07")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("01-04-1992", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="417-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("24-07-1991", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="440-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("01-04-1992", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="657-02")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("28-05-1992", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="664-06")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("18-08-1992", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="665-06")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("18-08-1992", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="666-06")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("18-08-1992", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="671-06")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("17-06-1993", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="681-13")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("20-05-1992", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="697-13")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("16-09-1992", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="700-06")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("16-06-1992", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="716-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("15-09-1992", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="728-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("09-09-1992", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="729-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("15-06-1993", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="744-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("01-06-1994", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="753-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("01-02-1993", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="762-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("15-12-1992", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="768-04")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("22-04-1993", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="769-13")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("09-11-1992", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="770-05")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("25-08-1992", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="777-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("24-03-1993", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="778-07")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("21-12-1992", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="803-01")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("22-12-1994", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="822-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("22-12-1992", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="848-02")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("11-07-1994", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="852-05")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("19-11-1992", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="865-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("01-07-1993", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="866-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("15-06-1993", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="879-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("01-07-1993", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="880-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("28-04-1993", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="906-05")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("22-04-1993", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="931-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("24-05-1993", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="932-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("01-07-1993", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="933-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("07-09-1993", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="941-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("23-11-1993", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="951-06")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("07-04-1993", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="959-10")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$to[2] <- dmy("07-09-1995", tz = "chile"); tmp$from[3] <- tmp$to[2]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
tmp1[sel] <- 0 # remove indices
tramVerif[sel] <- 1
#
sel <- c(1:I)[tmp1==1 & tmp2==5] # select indices of hitos mentioning no amendment by 2nd chamber and with nrow(tramites)==5
#
for (i in sel){
    if (bills$tramites[[i]]$tramite[4]=="ejec"){ # fourth entry is exec
        tmp <- bills$tramites[[i]]
        tmp <- tmp[-3,] # drop third row
        tmp$to[2] <- tmp$from[3] # fix dates
        bills$tramites[[i]] <- tmp
        tmp1[i] <- 0 # remove from indices
        tramVerif[i] <- 1
    }
}
sel <- c(1:I)[tmp1==1 & tmp2==5] # select indices of hitos mentioning no amendment by 2nd chamber and with nrow(tramites)==5
#
for (i in sel){
    if (bills$tramites[[i]]$tramite[5]=="ejec"){ # fifth entry is exec
        tmp <- bills$tramites[[i]]
        tmp <- tmp[-3:-4,] # drop third and fourth rows
        tmp$to[2] <- tmp$from[3] # fix dates
        bills$tramites[[i]] <- tmp
        tmp1[i] <- 0 # remove from indices
        tramVerif[i] <- 1
    }
}
#
sel <- c(1:I)[tmp1==1 & tmp2==5] # select indices of hitos mentioning no amendment by 2nd chamber and with nrow(tramites)==5
#
# change by hand
i <- which(bills$info$bol=="113-11")
tmp <- bills$tramites[[i]]; tmp <- tmp[-3:-4,]; tmp$to[2] <- tmp$from[3]; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="5500-10")
tmp <- bills$tramites[[i]]; tmp <- tmp[-3:-4,]; tmp$to[2] <- tmp$from[3]; tmp <- rbind(tmp, tmp[3,]); tmp$tramite[4] <- "ejec"; tmp$to[3] <- dmy("06-10-2009", tz = "chile"); tmp$from[4] <- tmp$to[3]
bills$tramites[[i]] <- tmp
#
tmp1[sel] <- 0
tramVerif[sel] <- 1
#
sel <- c(1:I)[tmp1==1 & tmp2==6] # select indices of hitos mentioning no amendment by 2nd chamber and with nrow(tramites)==1 that all need work
for (i in sel){
    if (bills$tramites[[i]]$tramite[6]=="ejec"){ # sixth entry is exec
        tmp <- bills$tramites[[i]]
        tmp <- tmp[-3:-5,] # drop third row
        tmp$to[2] <- tmp$from[3] # fix dates
        bills$tramites[[i]] <- tmp
        tmp1[i] <- 0 # remove from indices
        tramVerif[i] <- 1
    }
}
#
sel <- c(1:I)[tmp1==1 & tmp2==6] # select indices of hitos mentioning no amendment by 2nd chamber and with nrow(tramites)==1 that all need work
#
# change by hand
i <- which(bills$info$bol=="2689-06")
tmp <- bills$tramites[[i]]; tmp$to[4] <- dmy("05-06-2001", tz = "chile"); tmp <- tmp[c(-3,-5,-6),]; tmp$to[2] <- tmp$from[3]
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="2718-02")
tmp <- bills$tramites[[i]]; tmp <- tmp[c(-3,-6),]; tmp$to[2] <- tmp$from[3]
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="2814-06")
tmp <- bills$tramites[[i]]; tmp <- tmp[c(-3,-6),]; tmp$to[2] <- tmp$from[3]
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="3178-07")
tmp <- bills$tramites[[i]]; tmp <- tmp[c(-3,-4),]; tmp$to[2] <- tmp$from[3]; tmp$to[4] <- dmy("04-03-2003", tz = "chile")
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="3729-13")
tmp <- bills$tramites[[i]]; tmp <- tmp[c(-3,-6),]; tmp$to[2] <- tmp$from[3]
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="4639-11")
tmp <- bills$tramites[[i]]; tmp <- tmp[c(-3,-4),]; tmp$to[2] <- tmp$from[3]
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="5099-07")
tmp <- bills$tramites[[i]]; tmp <- tmp[c(-3,-4),]; tmp$to[2] <- tmp$from[3]
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="5326-06")
tmp <- bills$tramites[[i]]; tmp <- tmp[c(-3,-6),]; tmp$to[2] <- tmp$from[3]
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="6349-06")
tmp <- bills$tramites[[i]]; tmp <- tmp[c(-3,-6),]; tmp$to[2] <- tmp$from[3]
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="6560-10")
tmp <- bills$tramites[[i]]; tmp$to[5] <- tmp$to[6]; tmp <- tmp[c(-3,-6),]; tmp$to[2] <- tmp$from[3]
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="8387-05")
tmp <- bills$tramites[[i]]; tmp <- tmp[c(-3,-4),]; tmp$to[2] <- tmp$from[3]
bills$tramites[[i]] <- tmp
#
tmp1[sel] <- 0
tramVerif[sel] <- 1
#
sel <- c(1:I)[tmp1==1 & tmp2==7] # select indices of hitos mentioning no amendment by 2nd chamber and with nrow(tramites)==1 that all need work
# change by hand
i <- which(bills$info$bol=="2465-06")
tmp <- bills$tramites[[i]]; tmp$to[6] <- tmp$to[7]; tmp <- tmp[c(-3,-5,-7),]; tmp$to[2] <- tmp$from[3]; tmp$to[3] <- tmp$from[4]
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="2677-06")
tmp <- bills$tramites[[i]]; tmp$to[6] <- tmp$to[7]; tmp <- tmp[c(-3,-5,-7),]; tmp$to[2] <- tmp$from[3]; tmp$to[3] <- tmp$from[4]
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="3595-05")
tmp <- bills$tramites[[i]]; tmp <- tmp[c(-3,-5,-6),]; tmp$to[2] <- tmp$from[3]; tmp$to[3] <- tmp$from[4]
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="4313-06")
tmp <- bills$tramites[[i]]; tmp <- tmp[c(-3,-4,-5),]; tmp$to[2] <- tmp$from[3]; tmp$to[3] <- tmp$from[4]
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="6733-06")
tmp <- bills$tramites[[i]]; tmp <- tmp[c(-3,-4,-5),]; tmp$to[2] <- tmp$from[3]; tmp$to[3] <- tmp$from[4]
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="8696-04")
tmp <- bills$tramites[[i]]; tmp <- tmp[c(-3,-4,-5),]; tmp$to[2] <- tmp$from[3]; tmp$to[3] <- tmp$from[4]
bills$tramites[[i]] <- tmp
#
tmp1[sel] <- 0
tramVerif[sel] <- 1
#
sel <- c(1:I)[tmp1==1 & tmp2>7] # select indices of hitos mentioning no amendment by 2nd chamber and with nrow(tramites)==1 that all need work
# change by hand
i <- which(bills$info$bol=="1577-10")
tmp <- bills$tramites[[i]]; tmp <- tmp[-3:-7,]; tmp$to[2] <- tmp$from[3]
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="2520-07")
tmp <- bills$tramites[[i]]; tmp$to[7] <- tmp$to[8]; tmp <- tmp[c(-3,-5,-6,-8),]; tmp$to[2] <- tmp$from[3]; tmp$to[3] <- tmp$from[4]
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="3265-07")
tmp <- bills$tramites[[i]]; tmp$to[8] <- tmp$to[9]; tmp <- tmp[c(-3,-4,-5,-7,-9),]; tmp$to[2] <- tmp$from[3]; tmp$to[3] <- tmp$from[4]
bills$tramites[[i]] <- tmp
#
i <- which(bills$info$bol=="4047-10")
tmp <- bills$tramites[[i]]; tmp$to[8] <- tmp$to[9]; tmp <- tmp[c(-4,-5,-6,-7,-9),]; tmp$to[2] <- tmp$from[3]; tmp$to[3] <- tmp$from[4]
bills$tramites[[i]] <- tmp
#
tmp1[sel] <- 0
tramVerif[sel] <- 1
#
# did single-tramite pass (they should not, else info missing)
tmp2 <- rep(0, I) # will receive length tramites revised
for (i in 1:I){ 
    tmp2[i] <- nrow(bills$tramites[[i]])
}
sel <- which(tmp2==1 & bills$info$status=="statute")
# change by hand
i <- which(bills$info$bol=="133-05")
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp, tmp); tmp$to[1] <- tmp$from[2] <- dmy("29-08-1990", tz = "chile"); tmp$to[2] <- tmp$from[3] <- dmy("30-08-1990", tz = "chile"); tmp$to[3] <- dmy("04-09-1990", tz = "chile"); tmp$tramite[1] <- "dip"; tmp$tramite[2] <- "sen"; 
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
i <- which(bills$info$bol=="1381-05") # a éste le estoy inventando las fechas...
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp, tmp); tmp$to[1] <- tmp$from[2] <- dmy("04-11-1994", tz = "chile"); tmp$to[2] <- tmp$from[3] <- dmy("04-12-1994", tz = "chile"); tmp$to[3] <- dmy("06-12-1994", tz = "chile"); tmp$tramite[1] <- "dip"; tmp$tramite[2] <- "sen"; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
i <- which(bills$info$bol=="397-05") # a éste le estoy inventando las fechas...
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp, tmp); tmp$to[1] <- tmp$from[2] <- dmy("04-08-1991", tz = "chile"); tmp$to[2] <- tmp$from[3] <- dmy("04-09-1991", tz = "chile"); tmp$to[3] <- dmy("10-09-1991", tz = "chile"); tmp$tramite[1] <- "dip"; tmp$tramite[2] <- "sen"; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
i <- which(bills$info$bol=="471-10") # a éste le estoy inventando las fechas...
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp, tmp); tmp$to[1] <- tmp$from[2] <- dmy("04-10-1991", tz = "chile"); tmp$to[2] <- tmp$from[3] <- dmy("04-11-1991", tz = "chile"); tmp$to[3] <- dmy("29-09-1991", tz = "chile"); tmp$tramite[1] <- "dip"; tmp$tramite[2] <- "sen"; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
i <- which(bills$info$bol=="642-10") # a éste le estoy inventando las fechas...
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp, tmp); tmp$to[1] <- tmp$from[2] <- dmy("26-03-1994", tz = "chile"); tmp$to[2] <- tmp$from[3] <- dmy("26-03-1995", tz = "chile"); tmp$to[3] <- dmy("26-03-1997", tz = "chile"); tmp$tramite[1] <- "dip"; tmp$tramite[2] <- "sen"; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
i <- which(bills$info$bol=="981-13") # a éste le estoy inventando las fechas...
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp, tmp); tmp$to[1] <- tmp$from[2] <- dmy("13-05-1993", tz = "chile"); tmp$to[2] <- tmp$from[3] <- dmy("23-05-1993", tz = "chile"); tmp$to[3] <- dmy("31-05-1997", tz = "chile"); tmp$tramite[1] <- "dip"; tmp$tramite[2] <- "sen"; tmp$tramite[3] <- "ejec"
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
sel <- which(tmp2==1 & bills$info$status=="pending: 3er trámite")
i <- which(bills$info$bol=="7486-01")
bills$info$status[i] <- "killed/withdrawn"
i <- which(bills$info$bol=="9036-07")
bills$info$status[i] <- "killed/withdrawn"
tramVerif[sel] <- 1
#
sel <- which(tmp2==1 & bills$info$status=="pending: 2do trámite")
for (i in sel){
    tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp); tmp$tramite[2] <- ifelse(tmp$tramite[1]=="dip", "sen", "dip")
    tmp$to[1] <- tmp$from[2] <- bills$hitos[[i]]$date[nrow(bills$hitos[[i]])] # take last date from hitos
    bills$tramites[[i]] <- tmp
}
tramVerif[sel] <- 1
#
tmp2 <- rep(0, I) # will receive length tramites revised
for (i in 1:I){ 
    tmp2[i] <- nrow(bills$tramites[[i]])
}
sel <- which(tmp2==1)
table(bills$info$status[sel])
tramVerif[sel] <- 1 # huge leap of faith? assume all coded frozen, killed, withdrawn, pending 1 with single trámite are correct...
#
sel <- which(tmp2==2 & bills$info$status=="statute") # two tramites that are statute
#
# drop tramite==dip or sen after third tramite  (does not exhaust problems in tmp1==1 cases, so will not code as tramVerif)
for (i in 1:I){
    check <- grep("dip|sen", bills$tramites[[i]]$tramite)
    drop <- check[check>3]
    if (length(drop)>0){
        tmp <- bills$tramites[[i]]
        tmp <- tmp[-drop,]
        bills$tramites[[i]] <- tmp
    }
}
#
# drop repeated conf
tmp1 <- rep(0, I) # will receive dummy conf repeated
for (i in 1:I){ 
    if (length(grep("conf", bills$tramites[[i]]$tramite))>1) tmp1[i] <- 1 # conf appears more than once
}
#
sel <- which(tmp1==1)
# drop repeated instances of conf (does not exhaust problems in tmp1==1 cases, so will not add tramVerif)
for (i in sel){
    tmp <- bills$tramites[[i]]
    drop <- which(tmp$tramite=="conf")
    tmp$to[min(drop)] <- tmp$to[max(drop)] # plug last date's to first
    drop <- min(drop):max(drop) # select everything in between
    drop <- drop[-1] # drop everything sandwiched between repeated conf (2nd inclusive)
    tmp <- tmp[-drop,]
    bills$tramites[[i]] <- tmp
}
#
# miscoded case
i <- which(bills$info$bol=="334-07") # a éste le estoy inventando las fechas...
tmp <- bills$tramites[[i]]; tmp$tramite[3] <- "dip"; tmp <- rbind(tmp, tmp[4,], tmp[4,]); tmp$tramite[5] <- "ejec"; tmp$tramite[6] <- "veto"
tmp$to[4] <- tmp$from[5] <- dmy("21-07-1992", tz = "chile"); tmp$to[5] <- tmp$from[6] <- dmy("08-10-1992", tz = "chile"); 
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
# cases missing return to iniciadora when project was modified by revisora
tmp1 <- rep(0,I)
tmp2 <- rep(0,I)
for (i in 1:I){
    if (length(grep(".*[Oo]ficio.*aprobación.*proyecto.*con modificaciones.*", bills$hitos[[i]]$action))>0) tmp1[i] <- 1
    tmp2[i] <- nrow(bills$tramites[[i]])
}
sel <- which(tmp1==1 & tmp2==2) # when two tramites only, one must be missing
for (i in sel){
    tmp <- bills$tramites[[i]]
    tmp <- rbind(tmp, tmp[1,])
    tmp$to[3] <- tmp$to[2] # take last date
    tmp$to[2] <- tmp$from[3] <- bills$hitos[[i]]$date[nrow(bills$hitos[[i]])] # take last date from hitos
}
tramVerif[sel] <- 1
#
sel <- which(tmp1==1 & tmp2>2) # when 3 or more tramites, third must be same as first
for (i in sel){
    i <- sel[2] # debug
    tmp <- bills$tramites[[i]]
#    if (tmp$tramite[3]==tmp$tramite[1]) next
    if (tmp$tramite[3]=="conf" | tmp$tramite[3]=="veto"){
        tmp <- rbind(tmp[1:2,], tmp[1,], tmp[3:tmp2[i],]) # adds row for missing return to iniciadora
        tmp$from[3] <- tmp$to[3] <- tmp$to[2] # picks date from last recorded trámite
    }
}
#
# if veto, must have exec trámite before
tmp1 <- rep(0,I)
tmp2 <- rep(0,I)
for (i in 1:I){
    if (length(grep("veto", bills$tramites[[i]]$tramite))>0 & length(grep("ejec", bills$tramites[[i]]$tramite))==0) tmp1[i] <- 1 # no ejec despite veto
    tmp2[i] <- nrow(bills$tramites[[i]])
}
sel <- which(tmp1==1 & bills$info$status=="statute")
for (i in sel){
    tmp <- bills$tramites[[i]]
    n <- grep("veto", tmp$tramite)
    tmp <- rbind(tmp[1:(n-1),], tmp[n,], tmp[n:tmp2[i],])
    tmp$tramite[n] <- "ejec"
    tmp$from[n] <- tmp$to[n-1]
    tmp$to[n] <- tmp$from[n+1]
    bills$tramites[[i]] <- tmp
}
# clean
i <- which(bills$info$bol=="1034-15")
tmp <- bills$tramites[[i]]
tmp <- tmp[-3,]
bills$tramites[[i]] <- tmp
#
# if statute, must have exec trámite
tmp1 <- rep(0,I)
tmp2 <- rep(0,I)
for (i in 1:I){
    if (length(grep("ejec", bills$tramites[[i]]$tramite))==0) tmp1[i] <- 1 # no ejec
    tmp2[i] <- nrow(bills$tramites[[i]])
}
sel <- which(tmp1==1 & bills$info$status=="statute")
for (i in sel){
    tmp <- bills$tramites[[i]]
    tmp <- rbind(tmp, tmp[tmp2[i],])
    tmp$tramite[tmp2[i]+1] <- "ejec";
    tmp$from[tmp2[i]+1] <- tmp$to[tmp2[i]+1] <- bills$hitos[[i]]$date[nrow(bills$hitos[[i]])] # take last date from hitos
    bills$tramites[[i]] <- tmp
}
tramVerif[sel] <- 1
#
# tribunal repeated, trámites dropped
tmp1 <- rep(0,I)
for (i in 1:I){
    if (length(grep("trib", bills$tramites[[i]]$tramite))>1) tmp1[i] <- 1 
}
sel <- which(tmp1==1)
for (i in sel){
#    i <- sel[3]; bills$info$bol[i] # debug
    tmp <- bills$tramites[[i]]
    drop <- grep("trib", tmp$tramite)
    drop <- drop[-1] # drops repeated instances of tribunal
    tmp <- tmp[-drop,]
    bills$tramites[[i]] <- tmp
}
#
# two tramites that are not statute taken as ok if trámites are dip and sen only
tmp2 <- rep(0,I)
for (i in 1:I){
    tmp2[i] <- nrow(bills$tramites[[i]])
}
sel <- which(tmp2==2 & bills$info$status!="statute")
table(tramVerif[sel]) # most still marked as not revised
#
tmp1 <- rep(0,I)
for (i in sel){
    tmp <- bills$tramites[[i]]
    if (length(grep("conf|ejec|veto|trib", tmp$tramite))>0) tmp1[i] <- 1 # should only have dip and sen
}
table(tmp1[sel]) # all ok
tramVerif[sel] <- 1
#
# by hand
i <- which(bills$info$bol=="171-02") 
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[2,]); tmp$tramite[2] <- "dip"; tmp$to[1] <- tmp$from[2] <- dmy("31-10-1990", tz = "chile"); tmp$to[2] <- tmp$from[3] <- dmy("05-12-1990", tz = "chile")
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
i <- which(bills$info$bol=="825-03") 
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp); tmp$tramite[2] <- "dip"; tmp$to[1] <- tmp$from[2] <- dmy("09-06-1993", tz = "chile"); tmp$to[2] <- tmp$from[3] <- dmy("04-08-1993", tz = "chile"); tmp$to[3] <- tmp$from[4] <- dmy("17-08-1993", tz = "chile")
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
i <- which(bills$info$bol=="2247-05") 
tmp <- bills$tramites[[i]]; tmp$tramite[2] <- "sen"; tmp$to[1] <- tmp$from[2] <- dmy("18-11-1998", tz = "chile"); tmp$to[2] <- tmp$from[3] <- dmy("19-11-1998", tz = "chile"); tmp$to[3] <- tmp$from[4]
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
i <- which(bills$info$bol=="5058-07") 
tmp <- bills$tramites[[i]]; tmp$to[1] <- tmp$to[3]; tmp <- tmp[1,]
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
i <- which(bills$info$bol=="674-14") 
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp[1,], tmp[1,], tmp); tmp$tramite[2] <- "dip"; tmp$tramite[5] <- "trib"; tmp$tramite[6] <- "ejec"; tmp$to[1] <- tmp$from[2] <- dmy("22-03-1993", tz = "chile"); tmp$to[2] <- tmp$from[3] <- dmy("04-10-1995", tz = "chile"); tmp$to[4] <- tmp$from[5] <- dmy("13-06-1996", tz = "chile")
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
i <- which(bills$info$bol=="2093-05") 
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp[1,], tmp[1,], tmp); tmp$tramite[2] <- "sen"; tmp$to[1] <- tmp$from[2] <- tmp$to[2] <- tmp$from[3] <- dmy("18-11-1997", tz = "chile"); tmp$to[3] <- tmp$from[4] <- tmp$to[4] <- tmp$from[5] <- dmy("25-11-1997", tz = "chile")
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
i <- which(bills$info$bol=="3993-05") 
tmp <- bills$tramites[[i]]; tmp$tramite[2] <- "sen"; 
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
i <- which(bills$info$bol=="603-13") 
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp[1,], tmp[1,], tmp); tmp$tramite[2] <- "dip"; tmp$to[1] <- tmp$from[2] <- dmy("07-09-1992", tz = "chile");  tmp$to[2] <- tmp$from[3] <- dmy("20-03-1993", tz = "chile");
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
i <- which(bills$info$bol=="7308-06") 
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp, tmp[5,]); tmp$tramite[5] <- "ejec"; tmp$from[5] <- tmp$to[4]; tmp$to[5] <- tmp$from[6] <- dmy("18-12-2012", tz = "chile")
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
i <- which(bills$info$bol=="1035-07") 
tmp <- bills$tramites[[i]]; tmp$tramite[5] <- "ejec"; tmp$tramite[6] <- "veto"; tmp$to[1] <- tmp$from[2] <- dmy("12-09-1995", tz = "chile"); tmp$to[4] <- tmp$from[5] <- dmy("04-07-2000", tz = "chile"); tmp$to[5] <- tmp$from[6] <- dmy("16-08-2000", tz = "chile")
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
i <- which(bills$info$bol=="111-06")
# didn't fix it, will be dropped in analysis
tramVerif[i] <- 1
#
i <- which(bills$info$bol=="1251-18") 
tmp <- bills$tramites[[i]]; tmp$tramite[3] <- "dip"; tmp$tramite[4] <- "ejec"; tmp$tramite[5] <- "veto"; tmp$tramite[6] <- "trib"; tmp$to[3] <- tmp$from[4] <- dmy("04-04-2000", tz = "chile"); tmp$to[4] <- tmp$from[5] <- dmy("04-05-2000", tz = "chile"); tmp$to[5] <- tmp$from[6] <- dmy("21-06-2000", tz = "chile"); tmp$to[6] <- dmy("05-08-2000", tz = "chile")
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
i <- which(bills$info$bol=="259-07")
# didn't fix it, will be dropped in analysis
tramVerif[i] <- 1
#
i <- which(bills$info$bol=="446-03")
# didn't fix it, will be dropped in analysis
tramVerif[i] <- 1
#
i <- which(bills$info$bol=="5724-26") 
tmp <- bills$tramites[[i]]; tmp$tramite[4] <- "ejec"; tmp$tramite[5] <- "veto"; tmp$tramite[6] <- "trib";  tmp$from[4] <- tmp$to[3] <- dmy("04-11-2009", tz = "chile"); tmp$to[4] <- tmp$from[5] <- dmy("10-11-2009", tz = "chile"); tmp$to[5] <- tmp$from[6] <- dmy("01-12-2009", tz = "chile"); 
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
i <- which(bills$info$bol=="628-11")
# didn't fix it, will be dropped in analysis
tramVerif[i] <- 1
#
i <- which(bills$info$bol=="111-06") 
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp[1,], tmp); tmp[2,] <- tmp[3,]; tmp$tramite[3] <- "dip"; tmp$from[3] <- tmp$to[2] <- dmy("09-11-1993", tz = "chile")
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
i <- which(bills$info$bol=="1502-02") 
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp[1,], tmp); tmp[2,] <- tmp[3,]; tmp$tramite[3] <- "dip"; tmp$to[2] <- tmp$from[3] <- dmy("17-11-1999", tz = "chile"); tmp$from[4] <- tmp$to[3] <- dmy("04-01-2000", tz = "chile")
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
i <- which(bills$info$bol=="1516-02") 
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp[1,], tmp); tmp[2,] <- tmp[3,]; tmp$tramite[3] <- "dip"; tmp$to[2] <- tmp$from[3] <- dmy("17-11-1999", tz = "chile"); tmp$from[4] <- tmp$to[3] <- dmy("04-01-2000", tz = "chile")
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
i <- which(bills$info$bol=="1867-06") 
tmp <- bills$tramites[[i]]; tmp[4,] <- tmp[3,]; tmp$tramite[3] <- "sen"; tmp$to[3] <- tmp$from[4] <- dmy("04-08-1999", tz = "chile"); tmp$to[4] <- dmy("04-11-2014", tz = "chile")
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
i <- which(bills$info$bol=="195-08") 
tmp <- bills$tramites[[i]]; tmp <- rbind(tmp[1,], tmp); tmp[2,] <- tmp[3,]; tmp$tramite[3] <- "dip"; tmp$to[2] <- tmp$from[3] <- dmy("05-12-1990", tz = "chile")
bills$tramites[[i]] <- tmp; tramVerif[i] <- 1
#
# check conf in 3rd trám to verify if not missing a trámite in origen (looking for "rechazo ideal de legislar" and for "oficio rechazo c. origen" would achieve clean revision)
tmp1 <- rep(0,I)
for (i in 1:I){
    tmp <- bills$tramites[[i]]
    if (length(grep("conf", tmp$tramite))>0){
        tmp1[i] <- grep("conf", tmp$tramite)
    }
}
sel <- which(tmp1==3)
tramVerif[i] <- 1 # checked most by hand, they look ok
# VERIFICACIÓN DE TRÁMITES CONTINÚARÁ MÁS ABAJO
#
# RE DO ALL PERIODS (NEED TO REVISE FROM AND TO...)
for (i in 1:I){
    message(sprintf("loop %s of %s", i, I))
    tmp <- bills$tramites[[i]]
    tmp$period <- new_interval(tmp$from, tmp$to)
    bills$tramites[[i]] <- tmp
}
# add tramite number to the object created above
for (i in 1:I){
    #i <- 1 # debug
    N <- nrow(bills$tramites[[i]])
    bills$tramites[[i]]$nTr <- 1:N # OJO: WILL BE RE-DONE BELOW; RETAINED HERE TO AVOID BREAKS IN CODE 
}
rm(N)
#
options(warn=2) # turns warnings into errors, which break the loop (use warn=1 to return to normal) 
for (i in work){
    #j <- j + 1 # debug
    #i <- work[j] # debug
    message(sprintf("processing record %s", i))
    #bills$info$bol[i] # debug
    tmp <- bills$urgRaw[[i]]
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
    output$on <- dmy(gsub(pattern = "^([0-9/]*),.*", "\\1", tmp, perl = TRUE), tz = "chile") # adds date urgencia introduced
                                        #
    tmp3 <- sub(pattern = ".*(Sin urgencia).*", replacement = "\\1", tmp, perl = TRUE)
    tmp3 <- sub(pattern = ".*(Simple).*", replacement = "\\1", tmp3, perl = TRUE)
    tmp3 <- sub(pattern = ".*(Suma).*", replacement = "\\1", tmp3, perl = TRUE)
    tmp3 <- sub(pattern = ".*(Discusión inmediata).*", replacement = "\\1", tmp3, perl = TRUE)
    output$type <- tmp3
## # use something like this to determine if urgencia happened while bill was in conf...
## bills$tramites[[i]]$period <- interval(bills$tramites[[i]]$from, bills$tramites[[i]]$to) # <- put this in tramites loop
## sel <- which( bills$tramites[[i]]$tramite == "conf" )
## if (length(sel)>0){
##     output$on %my_within% bills$tramites[[i]]$period
## #    output$on[sel] %my_within% bills$tramites[[i]]$period
## }
                                        #
                                        # when urgencia deadline is de jure (need to change if the bill is in Comisión mixta) --- check if Senado and Comisión mixta urgencias are included
    tmp3 <- sub(pattern = ".*(Sin urgencia).*", replacement = 0, tmp, perl = TRUE)
    tmp3 <- sub(pattern = ".*(Simple).*", replacement = 30, tmp3, perl = TRUE)
    tmp3 <- sub(pattern = ".*(Suma).*", replacement = 15, tmp3, perl = TRUE)
    tmp3 <- sub(pattern = ".*(Discusión inmediata).*", replacement = 6, tmp3, perl = TRUE)
    tmp3 <- as.numeric(tmp3)
    select <- which(output$on < dmy("3/7/2010", tz = "chile")) # change urgencias before constitutional reform
    tmp3[select] <- mapvalues(tmp3[select], from = c(6,15), to = c(3,10), warn_missing = FALSE)
                                        #
    output$deadline <- output$on # inherits format for NAs
    select <- which(tmp2!=0)
    if (length(select)>0){
        output$deadline[tmp3!=0] <- deadline(output$on[tmp3!=0], as.numeric(tmp3[tmp3!=0]))
    }
                                        #
                                        # urgencia retired?
    output$dcaduca <- 0; output$dcaduca[grep("CADUCA", tmp)] <- 1 # urgencias "caducadas" were not retired (eg 1035-07)
    output$dretir <- 0
    select <- which(tmp2==2)
    if (length(select)>0){
        output$dretir[tmp2==2 & output$dcaduca==0] <- 1
    }
                                        # when urgencia was removed, if at all
    output$off <- output$deadline
    select <- which(tmp2==2)
    if (length(select)>0){
        output$off[tmp2==2] <- dmy(gsub(pattern = ".*[0-9],([0-9/]*),.*", "\\1", tmp[tmp2==2], perl = TRUE), tz = "chile")
    }
    ##                                     # put NAs in off for urgencias not retired
    ## select <- which(tmp2==1)
    ## if (length(select)>0){
    ##     output$off[tmp2==1] <- NA
    ## }
                                        # drops instances of no urgencia, if any
    select <- which(output$type=="Sin urgencia")
    if (length(select)>0) {
        output <- output[-select,]
    }
                                        #
                                        # sort chrono
    output <- output[order(output$on),]
                                        #
                                        # find and consolidate chains
    output$chain <- 0
    U <- nrow(output) # number of messages left
    output$tramite <- "." # prepare to receive trámite
    output$trNum <- 0 # prepare to receive trámite number
    for(u in 1:U){  # plug trámite to output; Not sure what it does when 2+ trámites in same day (eg. 2121-04)
#        u <- u+1 # debug
        sel <- output$on[u] %my_within% bills$tramites[[i]]$period # in which period does date.on belong in?
#        sel # debug
        if (length(bills$tramites[[i]]$tramite[sel])==0){
            output$tramite[u] <- "check: no overlap"
        } else {
            output$tramite[u] <- bills$tramites[[i]]$tramite[sel]
            output$trNum[u] <- bills$tramites[[i]]$nTr[sel]
        }
    }
    if (U > 1){
        for (k in 2:U){
            if (output$dretir[k-1]==1 & output$on[k]==output$off[k-1]){
                output$chain[k] <- 1
            }
        }
        tmp4 <- output$chain
        for (k in 2:U){
            tmp4[k] <- output$chain[k] + tmp4[k-1] * output$chain[k] # zero if no chain, else how many links
        }
        output$chain <- tmp4; rm(tmp4)
        # verify that chain indeed happened in same trámite and not in next
        if (U>1){
            for (u in 2:U){
                if (output$tramite[u]!=output$tramite[u-1]){ # spot and recode false chains (not in same trámite)
                    output$chain[u] <- 0
                }
            }
        }
    }
    output$change <- 0 # by default, no change in deadline
    ## # bloc useful if urgencia chains were consolidated
    ## output$nshorten <- output$nextend <- 0
    ## output$newDeadline <- output$deadline; # by default it is the same, with nil change
    ## output$nlinks <- output$chain + 1 # will receive info if 
    if (U > 1){
        for (u in U:2){ ## reverse loop over urgencia messages (so that first link of multi-chain inherits downstream info)
            if (output$chain[u]!=0){                              # choose urgencies in chains
                output$dretir[u-1] <- 0                           # recode: upstream message was not retired
                output$change[u] <- as.numeric(output$deadline[u] - output$deadline[u-1])*100 / as.numeric(output$deadline[u-1] - output$on[u-1]) # % change new deadline -- OJO: necesita más info para ser preciso: quitar fechas en que el Congreso no estuvo en sesión, quitar días festivos del conteo de días etc.
                ## # bloc useful if chains were to be consolidated
                ## output$dnlinks[u-1] <- output$nlinks[u]           # plug nlinks upstream
                ## output$newDeadline[u-1] <- output$deadline[u]     # it just got a new deadline
                ## output$newDeadline[u-1] <- output$deadline[u]     # they just got a new deadline
                ## if (output$deadline[u] >= output$deadline[u-1]){
                ##     output$nextend[u-1] <- 1                      # either longer
                ## } else {
                ##     output$shorten[u-1] <- 1                      # or shorter
                ## }
                ## output$newDeadline[u] <- NA
                ## output$change[u-1] <- as.numeric(output$newDeadline[u-1] - output$off[u-1]) *100 / as.numeric(output$deadline[u-1] - output$on[u-1])
                ## output$off[u-1] <- NA
            }
        }
    }
    ## ##                                     # drop chains after consolidating info
    ## ## select <- which(output$chain==1)
    ## ## if (length(select)>0) {
    ## ##     output <- output[-select,]
    ## ##}
    ## ## output$chain <- NULL
    #
    bills$urg[[i]] <- output # plug systematized object back into database
                                        #
                                        # plug into slot for systematized data
    if (nrow(output)>0){ # anything left after dropping sin urgencia?
        bills$info$nUrg[i] <- U
        bills$info$nInChains[i] <- nrow(output[output$chain!=0,])
        bills$info$nSimple[i] <- nrow(output[output$type=="Simple",])
        bills$info$nSuma[i] <- nrow(output[output$type=="Suma",])
        bills$info$nInmed[i] <- nrow(output[output$type=="Discusión inmediata",])
        bills$info$nRet[i] <- nrow(output[output$dretir==1,])
    }
    if (nrow(output)==0){ # in case nothing left after dropping sin urgencia, change info
        bills$info$hasUrg[i] <- "no"
    }
}
#output # debug
#message(sprintf("i=%s bol=%s", i, bills$info$bol[i]))
options(warn=1)

# fill wrong trámites from urg by hand (single-day trámite missed by loop above)
i <- which(bills$info$bol=="279-03")
bills$urg[[i]]$tramite[1] <- "sen"; bills$urg[[i]]$trNum[1] <- 1
bills$urg[[i]]$tramite[2] <- "dip"; bills$urg[[i]]$trNum[2] <- 2
#
#old i <- which(bills$info$bol=="2361-23")
#old bills$urg[[i]]$tramite[25] <- "sen"; bills$urg[[i]]$trNum[25] <- 4
#
i <- which(bills$info$bol=="3190-04")
bills$urgRaw[[i]][4] <- "15 de Abr. de 2003   Simple 536-348  "
#

# fix tramite date by hand
i <- which(bills$info$bol=="6041-08")
bills$tramites[[i]]$to[3] <- bills$tramites[[i]]$from[4]; bills$tramites[[i]]$period <- new_interval(bills$tramites[[i]]$from, bills$tramites[[i]]$to)
#
rm(i, k, output, sel, select, tmp, tmp2, tmp3, u, U, work, check, drop, n, tmp1) # housecleaning

# WHICH TRÁMITE(S) RECEIVED AT LEAST ONE URGENCY: 1, 2, 3, 12, 13, 23, or 123 (0 if none)
bills$info$urgIn <- 0 # prepares column to receive which trámites had 1+ urgencies
sel <- which(bills$info$nUrg>0)
for (i in sel){
    tmp <- bills$urg[[i]]$trNum # trámite numbers with an urgency
    tmp[tmp>3] <- 3 # recode trNum as 1,2,3+
    tmp <- as.numeric(names(table(tmp))) # remove repeated numbers
    tmp <- paste(tmp, sep="", collapse = "")
    bills$info$urgIn[i] <- as.numeric(tmp)
}
rm(sel,tmp)
# BILL PASSED DUMMY
bills$info$dpassed <- 0
bills$info$dpassed[bills$info$status=="statute"] <- 1

# re-arrange columns in data.frame, dropping useless ones
bills$info <- bills$info[, c("bol", "legyr", "dateIn", "init", "dmensaje", "dpassed", "status", "dateOut", "urgIn", "nUrg", "nInChains", "nSimple", "nSuma", "nInmed", "nRet", "refundido", "leg", "state", "materia", "hasHitos", "hasReport", "hasUrg", "hasSpon", "hasVot", "hasVeto", "hasUrgHU", "nHitos")] 

# SORT BILLS AND OBJECTS (SEE MY http://stackoverflow.com/questions/27303945/sort-nested-lists-in-r)
tmp <- as.numeric( sub(pattern = "([0-9]+)-.*", replacement = "\\1", bills$info$bol) ); ord <- order(tmp)  # order: boletín w/o committee
bills <- lapply(bills, function(x, ord) {
      if (is.data.frame(x)) return(x[ord,])
      return(x[ord])
    },
    ord = ord
)
rm(tmp, ord)

# add titulo from csv file
tmp <- read.csv(file = paste(datdir, "proyec3.csv", sep = ""), stringsAsFactors = FALSE)
tmp <- tmp[,c("bl", "titulo")]; colnames(tmp)[1] <- "bol" # keep titulos and bol only
tmp2 <- merge(x = bills$info, y = tmp, by = "bol", all = FALSE)
tmp <- as.numeric(sub(pattern = "([0-9]+)-[0-9]+", replacement = "\\1", tmp2$bol))
tmp2 <- tmp2[order(tmp),]
table(tmp2$bol==bills$info$bol)
bills$info <- tmp2

## Add objects with session dates 1990-2014
library(lubridate)
ses <- read.csv(file = paste(datdir, "sesionesCamara.csv", sep = ""), stringsAsFactors = FALSE)
colnames(ses) <- c("legislatura","date","session","stat")
ses$txt <- ses$session # keep text
ses$session <- sub(pattern = "Sesión ([0-9].*) en .*", replacement = "\\1", ses$txt)
ses$session <- sub(pattern = "Sesión en ([Cc]ongreso pleno) en .*", replacement = "\\1", ses$session)
#
tmp <- ses$date
tmp <- gsub(pattern = " de ", replacement = "-", x = tmp)
tmp <- gsub(pattern = "Ene."     , replacement = "1", x = tmp)
tmp <- gsub(pattern = "Feb."   , replacement = "2", x = tmp)
tmp <- gsub(pattern = "Mar."     , replacement = "3", x = tmp)
tmp <- gsub(pattern = "Abr."     , replacement = "4", x = tmp)
tmp <- gsub(pattern = "May."      , replacement = "5", x = tmp)
tmp <- gsub(pattern = "Jun."     , replacement = "6", x = tmp)
tmp <- gsub(pattern = "Jul."     , replacement = "7", x = tmp)
tmp <- gsub(pattern = "Ago."    , replacement = "8", x = tmp)
tmp <- gsub(pattern = "Sep.", replacement = "9", x = tmp)
tmp <- gsub(pattern = "Oct."   , replacement = "10", x = tmp)
tmp <- gsub(pattern = "Nov." , replacement = "11", x = tmp)
tmp <- gsub(pattern = "Dic." , replacement = "12", x = tmp)
ses$date <- dmy(tmp, tz = "chile")
## # compare date in string to date column: ALL OK
## tmp <- ses$txt
## tmp <- sub(pattern = "Sesión [0-9].* en (.*)", replacement = "\\1", tmp)       # drop start
## tmp <- sub(pattern = "Sesión en [cC]ongreso pleno en (.*)", replacement = "\\1", tmp)       # drop start
## tmp <- sub(pattern = "(.*) de [0-9]+:.*:[0-9]+ hrs.", replacement = "\\1", tmp) # drop hours
## tmp <- sub(pattern = "(.*) a las.*hrs.", replacement = "\\1", tmp)              # drop hours
## tmp <- gsub(pattern = " de ", replacement = "-", x = tmp)
## tmp <- sub(pattern = ".* ([0-9]+[-].*)", replacement = "\\1", x = tmp)
## tmp <- gsub(pattern = "enero"     , replacement = "1", x = tmp)
## tmp <- gsub(pattern = "febrero"   , replacement = "2", x = tmp)
## tmp <- gsub(pattern = "marzo"     , replacement = "3", x = tmp)
## tmp <- gsub(pattern = "abril"     , replacement = "4", x = tmp)
## tmp <- gsub(pattern = "mayo"      , replacement = "5", x = tmp)
## tmp <- gsub(pattern = "junio"     , replacement = "6", x = tmp)
## tmp <- gsub(pattern = "julio"     , replacement = "7", x = tmp)
## tmp <- gsub(pattern = "agosto"    , replacement = "8", x = tmp)
## tmp <- gsub(pattern = "septiembre", replacement = "9", x = tmp)
## tmp <- gsub(pattern = "octubre"   , replacement = "10", x = tmp)
## tmp <- gsub(pattern = "noviembre" , replacement = "11", x = tmp)
## tmp <- gsub(pattern = "diciembre" , replacement = "12", x = tmp)
## tmp <- dmy(tmp, tz = "chile")
## table(tmp == ses$date)
#
# sort and keep dates only --- if something else needed it can be added from ses here
ses <- ses[order(ses$date, ses$session),]; ses$ddip <- 1 # prepare new data.frame with date (sen sessions will be added here)
tmp <- ses[,c("date","ddip")];
tmp <- tmp[duplicated(tmp)==FALSE, ] # drop repeated dates (days with 2nd+ session)
bills$sessions <- tmp
rm(ses, tmp, tmp2)
#
# add senado sessions
ses <- read.csv(file = paste(datdir, "sesionesSenado.csv", sep = ""), stringsAsFactors = FALSE)
colnames(ses) <- c("legislatura","sesion","tipo","fch")
tmp <- ses$fch
tmp <- sub(pattern = "(.*[12][90][901][0-9]).*", replacement = "\\1", tmp) # drop trailing spaces
tmp <- gsub(pattern = " de ", replacement = "-", x = tmp)
tmp <- sub(pattern = "[A-Za-záé]+\\W([0-9]{1,2}[-].*)", replacement = "\\1", tmp) # drop weekdays
tmp <- gsub(pattern = "Enero"     , replacement = "1", x = tmp)
tmp <- gsub(pattern = "Febrero"   , replacement = "2", x = tmp)
tmp <- gsub(pattern = "Marzo"     , replacement = "3", x = tmp)
tmp <- gsub(pattern = "Abril"     , replacement = "4", x = tmp)
tmp <- gsub(pattern = "Mayo"      , replacement = "5", x = tmp)
tmp <- gsub(pattern = "Junio"     , replacement = "6", x = tmp)
tmp <- gsub(pattern = "Julio"     , replacement = "7", x = tmp)
tmp <- gsub(pattern = "Agosto"    , replacement = "8", x = tmp)
tmp <- gsub(pattern = "Septiembre", replacement = "9", x = tmp)
tmp <- gsub(pattern = "Octubre"   , replacement = "10", x = tmp)
tmp <- gsub(pattern = "Noviembre" , replacement = "11", x = tmp)
tmp <- gsub(pattern = "Diciembre" , replacement = "12", x = tmp)
tmp <- dmy(tmp, tz = "chile")
ses$fch <- tmp # needed to recover cong pleno below
#
tmp <- tmp[order(tmp)]
tmp <- tmp[duplicated(tmp)==FALSE]
tmp <- data.frame(date = tmp, dsen = rep(1, length(tmp)))
#
tmp2 <- merge(x = bills$sessions, y = tmp, by = "date", all = TRUE); tmp2[is.na(tmp2)==TRUE] <- 0 # merge senado and diputado sessions
tmp2 <- tmp2[order(tmp2$date),]
#
bills$sessions <- tmp2 # paste object with both chambers' sessions
#
# recover Congreso pleno dates that are missing in dip data
sel <- which(ses$tipo=="Congreso pleno") # some of these sessions do not appear in diputados data, will add them
bills$sessions$ddip[which(bills$sessions$date %in% ses$fch[sel])] <- 1
#
rm(tmp, tmp2)
#
head(bills$sessions)
rm(i, ses, sel)

save.image(file="tmp.RData")
# export csv
bills$info$yrin <- year(bills$info$dateIn); bills$info$moin <- month(bills$info$dateIn); bills$info$dyin <- day(bills$info$dateIn);
#bills$outfo$yrout[] <- year(bills$outfo$dateOut); bills$outfo$moout <- month(bills$outfo$dateOut); bills$outfo$dyout <- day(bills$outfo$dateOut);
write.csv(bills$info, file = paste(datdir, "bills-info.csv", sep = ""))

rm(list=ls())
datdir <- "/home/eric/Dropbox/data/latAm/chile/data/" 
setwd(datdir)
load(file = "tmp.RData")
options(width = 150)

## CONTINUE REVISING TRÁMITES
#tmp1 <- rep(0, I) # will receive dummy sin modificaciones pointing to índices that need manipulation
tmp2 <- rep(0, I) # will receive length tramites
for (i in 1:I){ 
#    if (length(grep("Oficio aprobaci[óo]n sin modificaciones a.*de [Oo]rigen", bills$hitos[[i]]$action))>0) tmp1[i] <- 1
    tmp2[i] <- nrow(bills$tramites[[i]])
}
sel <- which(tmp2==1 & tramVerif==0) # pick single-trámite, non-revised
table(bills$info$dpassed[sel]) ## none have passed... infer that single trámite is right
tramVerif[sel] <- 1
#
sel <- which(tmp2==2 & tramVerif==0) # pick two-trámite, non-revised
table(bills$info$dpassed[sel]) # none have passed
tramVerif[sel] <- 1
# all should have dip and sen  only
tmp1 <- rep(0, I) 
for (i in 1:I){ 
    if (length(grep("dip|sen", bills$tramites[[i]]$tramite))!=2) tmp1[i] <- 1 # all should have dip or sen, nothing else
}
sel <- which(tmp2==2 & tramVerif==0 & tmp1==1)
# checked by hand: have redundant second trámite that needs to be dropped
for (i in sel){
    tmp <- bills$tramites[[i]]
    tmp <- tmp[1,]
    bills$tramites[[i]] <- tmp
}
#
# change by hand
i <- which(bills$info$bol=="356-04")
bills$info$status[i] <- "statute"; bills$info$dpassed[i] <- 1
tramVerif[i] <- 1
#
i <- which(bills$info$bol=="1282-10")
tmp <- bills$tramites[[i]]
tmp <- rbind(tmp, tmp[3,]); tmp$tramite[4] <- "veto"; tmp$to[3] <- tmp$from[3]; tmp$period <- new_interval(tmp$from, tmp$to); tmp$nTr[4] <- 4# assume vetoed
bills$tramites[[i]] <- tmp
tramVerif[i] <- 1
#
i <- which(bills$info$bol=="1285-10")
tmp <- bills$tramites[[i]]
tmp <- rbind(tmp, tmp[3,]); tmp$tramite[4] <- "veto"; tmp$to[3] <- tmp$from[3]; tmp$period <- new_interval(tmp$from, tmp$to); tmp$nTr[4] <- 4# assume vetoed
bills$tramites[[i]] <- tmp
tramVerif[i] <- 1
#
i <- which(bills$info$bol=="1778-07")
tmp <- bills$tramites[[i]]
tmp$to[1] <- tmp$to[3]; tmp <- tmp[1,]
bills$tramites[[i]] <- tmp
tramVerif[i] <- 1
#
i <- which(bills$info$bol=="1854-10")
tmp <- bills$tramites[[i]]
tmp <- rbind(tmp, tmp[3,]); tmp$tramite[4] <- "veto"; tmp$to[3] <- tmp$from[3]; tmp$period <- new_interval(tmp$from, tmp$to); tmp$nTr[4] <- 4# assume vetoed
bills$tramites[[i]] <- tmp
tramVerif[i] <- 1
#
i <- which(bills$info$bol=="2293-10")
tmp <- bills$tramites[[i]]
tmp <- tmp[-3,]; 
bills$tramites[[i]] <- tmp
bills$info$status[i] <- "killed/withdrawn"
tramVerif[i] <- 1
#
i <- which(bills$info$bol=="3119-10")
tmp <- bills$tramites[[i]]
tmp <- rbind(tmp, tmp[3,]); tmp$tramite[4] <- "veto"; tmp$to[3] <- tmp$from[3]; tmp$period <- new_interval(tmp$from, tmp$to); tmp$nTr[4] <- 4# assume vetoed
bills$tramites[[i]] <- tmp
tramVerif[i] <- 1
#
i <- which(bills$info$bol=="5115-10")
tmp <- bills$tramites[[i]]
tmp <- rbind(tmp, tmp[3,]); tmp$tramite[4] <- "veto"; tmp$to[3] <- tmp$from[3]; tmp$period <- new_interval(tmp$from, tmp$to); tmp$nTr[4] <- 4# assume vetoed
bills$tramites[[i]] <- tmp
tramVerif[i] <- 1
#
i <- which(bills$info$bol=="6649-10")
bills$info$status[i] <- "statute"; bills$info$dpassed[i] <- 1
tramVerif[i] <- 1
#
i <- which(bills$info$bol=="7160-10")
tmp <- bills$tramites[[i]]
tmp <- rbind(tmp, tmp[3,]); tmp$tramite[4] <- "veto"; tmp$to[3] <- tmp$from[3]; tmp$period <- new_interval(tmp$from, tmp$to); tmp$nTr[4] <- 4# assume vetoed
bills$tramites[[i]] <- tmp
tramVerif[i] <- 1
#
sel <- which(tmp2==3 & tramVerif==0 & bills$info$dpassed==0) # pick three-trámite, non-revised, that didn't pass; revised by hand, all ok
tramVerif[sel] <- 1
#
sel <- which(tmp2==3 & tramVerif==0 & bills$info$dpassed==1) # pick three-trámite, non-revised, that passed
tmp1 <- rep(1, I) 
for (i in sel){ 
    if (bills$tramites[[i]]$tramite[3]=="ejec") tmp1[i] <- 0 # all should have ejec as third trámite
}
length(which(tmp2==3 & tramVerif==0 & bills$info$dpassed==1 & tmp1==1))==0 # all have ejec as third, assume all ok
tramVerif[sel] <- 1
#
tmp2 <- rep(0, I) # will receive length tramites
for (i in 1:I){ 
#    if (length(grep("Oficio aprobaci[óo]n sin modificaciones a.*de [Oo]rigen", bills$hitos[[i]]$action))>0) tmp1[i] <- 1
    tmp2[i] <- nrow(bills$tramites[[i]])
}
#
sel <- which(tmp2==4 & tramVerif==0) # pick four-trámite, non-revised
# checked by hand, all seem ok
tramVerif[sel] <- 1
#
sel <- which(tmp2==5 & tramVerif==0) # pick five-trámite, non-revised
# checked by hand, all seem ok
tramVerif[sel] <- 1
#
sel <- which(tmp2==6 & tramVerif==0) # pick six-trámite, non-revised
# checked by hand, all seem ok
tramVerif[sel] <- 1
#
sel <- which(tmp2==7 & tramVerif==0) # pick seven-trámite, non-revised
# checked by hand, all seem ok
tramVerif[sel] <- 1
#
table(tramVerif) # ALL REVISED
rm(i, sel, tmp, tmp1, tmp2, tramVerif, vet) # clean
#
# SHOULD FILL GAPS IN TO:FROM AND RE DO ALL PERIODS

# head(force_tz(bills$sessions, "Chile")) # use this to force chile time zone while keeping the clock time, which may change the date

# exports csv of allUrg to process in plots.r
# extract urgencias object, unlisted to prepare urgencia-as-unit data
## ojo: hay objectos bills$urgencias, bills$urgRaw y bills$urg... uno sale sobrando, checar
tmp <- bills$urg
sel <- which(bills$info$nUrg>0) # bills with at least one urgencia
for (i in sel){
    tmp[[i]]$bol <- bills$info$bol[i]
}
tmp <- tmp[sel] # drop elements without urgency
#
library(plyr)
library(lubridate)
allUrg <- ldply(tmp, data.frame) # unlist the data.frames into one large data.frame
#
# drop urgencias after 10/3/2014
sel <- which(allUrg$on>dmy("10/03/2014", tz = "chile"))
allUrg <- allUrg[-sel,]
#
# add numeric type: 1,2,3 for di, su, si; 4.1,4.2,4.3 for resets of each type; 5.1,5.2,5.3 for retired of each type
allUrg$typeN <- 0; allUrg$typeN[allUrg$type=="Discusión inmediata"] <- 1; allUrg$typeN[allUrg$type=="Suma"] <- 2; allUrg$typeN[allUrg$type=="Simple"] <- 3;
tmp2 <- allUrg$typeN # will be used for retires
allUrg$typeN[allUrg$chain>0] <- 4 + allUrg$typeN[allUrg$chain>0]/10 # temp recode
# add messages retiring urgency
tmp <- allUrg[allUrg$dretir==1,]; tmp2 <- tmp2[allUrg$dretir==1]
tmp$typeN <- 5 + tmp2/10
tmp$on <- tmp$off
allUrg <- rbind(allUrg, tmp) # binds retiring messages for graph
table(allUrg$typeN)
#
# drop urgencias after 10/3/2014 (repeat since off messages were added)
sel <- which(allUrg$on>dmy("10/03/2014", tz = "chile"))
allUrg <- allUrg[-sel,]
save(bills, allUrg, file = paste(datdir, "allUrg.RData", sep = "")) # <--- export
rm(i, sel, tmp, tmp2)
# further transformations of allUrg in plots.r in preparation for graph

# drop bills initiated before 1/3/1998
library(lubridate)
drop <- -which(bills$info$dateIn<dmy("1/3/1998", tz = "chile"))
bills <- lapply(bills, function(x, drop) {
      if (is.data.frame(x)) return(x[drop,])
      return(x[drop])
    },
    drop = drop
)
rm(drop)
#
I <- nrow(bills$info) # update tot obs

# ADD POLICY DOMAIN (CODED FROM BOLETIN)
bills$info$ndom <- as.numeric(sub(pattern = "[0-9]+-([0-9]+)", replacement = "\\1", bills$info$bol))
bills$info$dom[bills$info$ndom==1] <- "agricultura"
bills$info$dom[bills$info$ndom==2] <- "defensa"
bills$info$dom[bills$info$ndom==3] <- "economía"
bills$info$dom[bills$info$ndom==4] <- "educación"
bills$info$dom[bills$info$ndom==5] <- "hacienda"
bills$info$dom[bills$info$ndom==6] <- "elecciones"
bills$info$dom[bills$info$ndom==7] <- "constitución"
bills$info$dom[bills$info$ndom==8] <- "minería"
bills$info$dom[bills$info$ndom==9] <- "obras púb"
bills$info$dom[bills$info$ndom==10] <- "rree"
bills$info$dom[bills$info$ndom==11] <- "salud"
bills$info$dom[bills$info$ndom==12] <- "medio ambiente"
bills$info$dom[bills$info$ndom==13] <- "trabajo"
bills$info$dom[bills$info$ndom==14] <- "vivienda"
bills$info$dom[bills$info$ndom==15] <- "telecom"
bills$info$dom[bills$info$ndom==16] <- "corg"
bills$info$dom[bills$info$ndom==17] <- "ddhh"
bills$info$dom[bills$info$ndom==18] <- "familia"
bills$info$dom[bills$info$ndom==19] <- "ciencia internet"
bills$info$dom[bills$info$ndom==20] <- "narco"
bills$info$dom[bills$info$ndom==21] <- "pesca"
bills$info$dom[bills$info$ndom==24] <- "monumentos"
bills$info$dom[bills$info$ndom==25] <- "narco"
bills$info$dom[bills$info$ndom==29] <- "deporte"
# MERGE THESE INTO NARROW INTEREST
## bills$info$dom[bills$info$ndom==22] <- "bomberos"
## bills$info$dom[bills$info$ndom==23] <- "turismo"
## bills$info$dom[bills$info$ndom==26] <- "pymes"
## bills$info$dom[bills$info$ndom==27] <- "extremos"
## bills$info$dom[bills$info$ndom==28] <- "discapacitados"
## bills$info$dom[bills$info$ndom==30] <- "juventud"
## bills$info$dom[bills$info$ndom==31] <- "discapacitados"
## bills$info$dom[bills$info$ndom==32] <- "3a edad"
## bills$info$dom[bills$info$ndom==33] <- "subsuelo"
bills$info$dom[bills$info$ndom==22] <- "narrow"
bills$info$dom[bills$info$ndom==23] <- "narrow"
bills$info$dom[bills$info$ndom==26] <- "narrow"
bills$info$dom[bills$info$ndom==27] <- "narrow"
bills$info$dom[bills$info$ndom==28] <- "narrow"
bills$info$dom[bills$info$ndom==30] <- "narrow"
bills$info$dom[bills$info$ndom==31] <- "narrow"
bills$info$dom[bills$info$ndom==32] <- "narrow"
bills$info$dom[bills$info$ndom==33] <- "narrow"
#
table(bills$info$dom)
#
# drop ndom (handy if selecting domains)
bills$info$ndom <- NULL

# Referred to Hacienda Committee (ie., needs appropriation)
bills$info$drefHda <- 0
for (i in 1:I){
    tmp <- bills$hitos[[i]]$action[grep(".*[Pp]asa a [Cc]omisi[óo]n.*", bills$hitos[[i]]$action)]
    if (length(grep("[Hh]acienda", tmp))>0) bills$info$drefHda[i] <- 1
}
table(bills$info$drefHda)

# NEED INDICATORS OF VOTING QUORUM!!

# FIND SPONSORS
# FILL MISSING SPONSORS
i <- which(bills$info$bol=="2428-06") # missing sponsors
tmp <- c("Nombre", "Laura Soto G.", "Joaquín Palma I.", "Aldo Cornejo G.", "Juan Bustos R.")
bills$sponsors[[i]] <- tmp
bills$info$hasSpon[i] <- "yes"
#
i <- which(bills$info$bol=="2866-06") # missing sponsors
tmp <- c("Nombre", "Carlos Recondo L.")
bills$sponsors[[i]] <- tmp
bills$info$hasSpon[i] <- "yes"
#
i <- which(bills$info$bol=="3085-11") # missing sponsors
tmp <- c("Nombre", "Nicolás Monckeberg D.", "Pedro Pablo Álvarez-Salamanca R.", "Rosauro Martínez L.")
bills$sponsors[[i]] <- tmp
bills$info$hasSpon[i] <- "yes"
#
i <- which(bills$info$bol=="3108-01") # missing sponsors
tmp <- c("Nombre", "Nicolás Monckeberg D.", "Pedro Pablo Álvarez-Salamanca R.", "Rosauro Martínez L.", "Galilea", "Bauer", "Cardemil", "Delmastro", "Palma", "Prieto", "Vargas")
bills$sponsors[[i]] <- tmp
bills$info$hasSpon[i] <- "yes"
#
i <- which(bills$info$bol=="3136-04") # missing sponsors
tmp <- c("Nombre", "Ulloa", "Dittborn", "Longueira", "Víctor Pérez", "Egaña", "Álvarez", "Hernández", "Navarro", "Norambuena")
bills$sponsors[[i]] <- tmp
bills$info$hasSpon[i] <- "yes"
#
i <- which(bills$info$bol=="3144-07") # missing sponsors
tmp <- c("Nombre", "Kast", "Varela", "Forni", "Alvarado", "Norambuena", "Ulloa", "Díaz", "Ibáñez", "Molina", "Cubillos")
bills$sponsors[[i]] <- tmp
bills$info$hasSpon[i] <- "yes"
#
i <- which(bills$info$bol=="3156-07") # missing sponsors
tmp <- c("Nombre", "Kast", "Cristi", "Paya", "Víctor Pérez", "Leay", "Molina", "Dittborn", "García-Huidobro", "Melero", "Uriarte", "Alvarado")
bills$sponsors[[i]] <- tmp
bills$info$hasSpon[i] <- "yes"
#
i <- which(bills$info$bol=="3362-07") # missing sponsors
tmp <- c("Nombre", "Montes", "Bustos", "Espinoza", "Burgos", "Aguiló", "Ceroni")
bills$sponsors[[i]] <- tmp
bills$info$hasSpon[i] <- "yes"
#
i <- which(bills$info$bol=="3363-07") # missing sponsors
tmp <- c("Nombre", "Montes", "Bustos", "Espinoza", "Burgos", "Aguiló", "Ceroni")
bills$sponsors[[i]] <- tmp
bills$info$hasSpon[i] <- "yes"
#
i <- which(bills$info$bol=="3364-07") # missing sponsors
tmp <- c("Nombre", "Errázuriz")
bills$sponsors[[i]] <- tmp
bills$info$hasSpon[i] <- "yes"
#
i <- which(bills$info$bol=="3365-15") # missing sponsors
tmp <- c("Nombre", "Accorsi")
bills$sponsors[[i]] <- tmp
bills$info$hasSpon[i] <- "yes"
#
i <- which(bills$info$bol=="3370-07") # missing sponsors
tmp <- c("Nombre", "Carmen Ibáñez", "Eliana Caraball", "Pía Guzmán", "Salaberry", "Moreira", "Uriarte", "Becker")
bills$sponsors[[i]] <- tmp
bills$info$hasSpon[i] <- "yes"
#
i <- which(bills$info$bol=="3372-04") # missing sponsors
tmp <- c("Nombre", "Muñoz")
bills$sponsors[[i]] <- tmp
bills$info$hasSpon[i] <- "yes"
#
i <- which(bills$info$bol=="3372-04") # missing sponsors
tmp <- c("Nombre", "Muñoz")
bills$sponsors[[i]] <- tmp
bills$info$hasSpon[i] <- "yes"
#
i <- which(bills$info$bol=="3375-04") # missing sponsors
tmp <- c("Nombre", "Ulloa", "Egaña", "Recondo", "Urrutia", "Pablo Galilea", "Vargas", "Melero")
bills$sponsors[[i]] <- tmp
bills$info$hasSpon[i] <- "yes"
#
i <- which(bills$info$bol=="3430-07") # missing sponsors
tmp <- c("Nombre", "Bayo", "Bertolino", "Delmastro", "García", "Hidalgo", "Vargas")
bills$sponsors[[i]] <- tmp
bills$info$hasSpon[i] <- "yes"
#
i <- which(bills$info$bol=="3773-06") # missing sponsors
tmp <- c("Nombre", "Hernán Larraín Fernandez", "Jaime Gazmuri Mujica")
bills$sponsors[[i]] <- tmp
bills$info$hasSpon[i] <- "yes"
#
# FIND SPONSORS
bills$sponsorsRaw <- bills$sponsors # keep original info
tmplook <- rep(0,I) # dummy pointing to names without party info
sel <- which(bills$info$dmensaje==0)
for (i in sel){
    message(sprintf("loop %s of %s", i, I))
    #i <- sel[2] # debug
    tmp <- bills$sponsorsRaw[[i]]
    n <- length(tmp) - 1
    bills$sponsors[[i]] <- data.frame( name=rep(NA,n), region=rep(NA,n), disn=rep(NA,n), party=rep(NA,n), list=rep(NA,n), regex=rep(NA,n) ) # empty frame
    if (length(grep("Región", tmp[1]))==0){
        bills$sponsors[[i]]$name <- tmp[-1] # drop title
        tmplook[i] <- 1 # mark: party needs to be searched
    } else {
        tmp <- tmp[-1] # drop title
        #sub(pattern = "(.*)((?:I|II|III|IV|V|VI|VII|VIII|IX|X|XI|XII|XIII|XIV|XV|RM) Región.*)N°([0-9+]) (.*)", replacement = "\\2", tmp)
        bills$sponsors[[i]]$name <- sub(pattern = "(.*) N°([0-9]+) (.*)", replacement = "\\1", tmp)
        bills$sponsors[[i]]$disn <- sub(pattern = "(.*) N°([0-9]+) (.*)", replacement = "\\2", tmp)
        bills$sponsors[[i]]$party <- sub(pattern = "(.*) N°([0-9]+) (.*)", replacement = "\\3", tmp)
    }
    # remove accents
    bills$sponsors[[i]]$name <- gsub(pattern = "á", replacement = "a", bills$sponsors[[i]]$name)
    bills$sponsors[[i]]$name <- gsub(pattern = "é", replacement = "e", bills$sponsors[[i]]$name)
    bills$sponsors[[i]]$name <- gsub(pattern = "í", replacement = "i", bills$sponsors[[i]]$name)
    bills$sponsors[[i]]$name <- gsub(pattern = "ó", replacement = "o", bills$sponsors[[i]]$name)
    bills$sponsors[[i]]$name <- gsub(pattern = "ú", replacement = "u", bills$sponsors[[i]]$name)
    bills$sponsors[[i]]$name <- gsub(pattern = "Á", replacement = "A", bills$sponsors[[i]]$name)
    bills$sponsors[[i]]$name <- gsub(pattern = "É", replacement = "E", bills$sponsors[[i]]$name)
    bills$sponsors[[i]]$name <- gsub(pattern = "Í", replacement = "I", bills$sponsors[[i]]$name)
    bills$sponsors[[i]]$name <- gsub(pattern = "Ó", replacement = "O", bills$sponsors[[i]]$name)
    bills$sponsors[[i]]$name <- gsub(pattern = "Ú", replacement = "U", bills$sponsors[[i]]$name)
    # regex matching names in any order
    tmp <- bills$sponsors[[i]]$name
    tmp <- sub(pattern = "^", replacement = "^(?=.*", x = tmp)
    tmp <- sub(pattern = "$", replacement = ").*?", x = tmp)
    tmp <- gsub(pattern = " ", replacement = ")(?=.*", x = tmp) 
    bills$sponsors[[i]]$regex <- tmp
}
#
# FILL LIST INFO
sel <- which(bills$info$dmensaje==0 & tmplook==0)
for (i in sel){
    #i <- sel[1] # debug
    bills$sponsors[[i]]$list <- mapvalues(bills$sponsors[[i]]$party, from = c("Independientes", "Izquierda Ciudadana", "Partido Comunista", "Partido Demócrata Cristiano", "Partido Por la Democracia", "Partido Radical Social Demócrata", "Partido Regionalista Independiente", "Partido Socialista", "Renovación Nacional", "Rubén Gajardo Chacón Región N° Partido Demócrata Cristiano", "Unión de Centro Progresista", "Unión Demócrata Independiente"), to = c("ind", "con", "con", "con", "con", "con", "reg", "con", "right", "con", "right", "right"), warn_missing = FALSE)
    bills$sponsors[[i]]$party <- mapvalues(bills$sponsors[[i]]$party, from = c("Independientes", "Izquierda Ciudadana", "Partido Comunista", "Partido Demócrata Cristiano", "Partido Por la Democracia", "Partido Radical Social Demócrata", "Partido Regionalista Independiente", "Partido Socialista", "Renovación Nacional", "Rubén Gajardo Chacón Región N° Partido Demócrata Cristiano", "Unión de Centro Progresista", "Unión Demócrata Independiente"), to = c("ind", "ic", "pcch", "dc", "ppd", "prsd", "pri", "ps", "rn", "dc", "ucp", "udi"), warn_missing = FALSE)
}
#
# import dip/sen names and parties to fill missing list
dip <- read.csv(paste("/home/eric/Dropbox/data/latAm/chile/composicionCamarasComs/dip.csv", sep = ""), stringsAsFactors = FALSE)
dip <- dip[,c("name","pty","list")]
sen <- read.csv(paste("/home/eric/Dropbox/data/latAm/chile/composicionCamarasComs/sen.csv", sep = ""), stringsAsFactors = FALSE)
sen <- sen[,c("name","pty","list")]
# merge
dip$from <- "dip"; sen$from <- "sen"; mcs <- rbind(dip, sen); rm(dip,sen)
# clean party labels
mcs$pty <- sub(pattern = "ind[-(: )]", replacement = "", mcs$pty) # make leaners party members
mcs$pty <- sub(pattern = "inst[,:] ", replacement = "", mcs$pty) # make leaners party members
mcs$pty[mcs$pty=="mas (ex ps)"] <- "ps"
mcs$pty[mcs$pty=="sd"] <- "prsd"
mcs$pty[mcs$pty=="ppd-ps"] <- "ps"
mcs$pty[mcs$pty=="psdp-pr"] <- "prsd"
# remove accents
mcs$name <- gsub(pattern = "á", replacement = "a", mcs$name)
mcs$name <- gsub(pattern = "é", replacement = "e", mcs$name)
mcs$name <- gsub(pattern = "í", replacement = "i", mcs$name)
mcs$name <- gsub(pattern = "ó", replacement = "o", mcs$name)
mcs$name <- gsub(pattern = "ú", replacement = "u", mcs$name)
mcs$name <- gsub(pattern = "Á", replacement = "A", mcs$name)
mcs$name <- gsub(pattern = "É", replacement = "E", mcs$name)
mcs$name <- gsub(pattern = "Í", replacement = "I", mcs$name)
mcs$name <- gsub(pattern = "Ó", replacement = "O", mcs$name)
mcs$name <- gsub(pattern = "Ú", replacement = "U", mcs$name)
# remove commas
mcs$name <- gsub(pattern = ",", replacement = "", mcs$name)
#
sel <- which(bills$info$dmensaje==0 & tmplook==1)
# find name, add party/list
for (i in sel){
    message(sprintf("loop %s of %s", i, I))
    #i <- sel[7]; j <- 1; bills$info$bol[i] # debug
    tmp <- bills$sponsors[[i]]$regex
    n <- length(tmp)
    for (j in 1:n){
        tmphits <- grep(pattern = tmp[j], x = mcs$name, perl = TRUE)
        if (length(tmphits)==0) next
        if (length(tmphits)==1){
            bills$sponsors[[i]]$pty <- mcs$pty[tmphits]
            bills$sponsors[[i]]$list <- mcs$list[tmphits]
            tmplook[i] <- 0
        } else {
            bills$sponsors[[i]]$pty <- paste(mcs$pty[tmphits], collapse = "-") # colapses many labels into one long
            bills$sponsors[[i]]$list <- paste(mcs$list[tmphits], collapse = "-") # colapses many labels into one long
            tmplook[i] <- 0
        }
    }
}

# COMPUTE CONCERTACIÓN AND RIGHT % SPONSORS
bills$info$pctcon <- bills$info$pctright <- NA
# exec-init 100 percent their list
sel <- which(bills$info$dmensaje==1)
bills$info$pctcon[sel] <- 100
bills$info$pctright[sel] <- 0
sel <- which(bills$info$dmensaje==1 & bills$info$dateIn>=dmy("1/3/2010", tz = "chile") & bills$info$dateIn<dmy("1/3/2014", tz = "chile")) # piñera
bills$info$pctcon[sel] <- 0
bills$info$pctright[sel] <- 100
# mc-init
sel <- which(bills$info$dmensaje==0)
for (i in sel){
    tmp <- bills$sponsors[[i]]$list
    n <- length(tmp)
    bills$info$pctcon[i] <- length(grep(pattern = "con", tmp)) * 100 / n
    bills$info$pctright[i] <- length(grep(pattern = "right", tmp)) * 100 / n
}
# round
bills$info$pctcon <- round(bills$info$pctcon, digits = 0)
bills$info$pctright <- round(bills$info$pctright, digits = 0)
#
rm(i, j, mcs, n, sel, tmp, tmphits, tmplook)

# SOME DESCRIPTIVES (Processed in separate spreadsheet descriptives.ods)
table(bills$info$dmensaje)
table(bills$info$dpassed)
table(bills$info$dpassed[bills$info$dmensaje==0])
table(bills$info$dpassed[bills$info$dmensaje==1])
tmp <- bills$info$nUrg; tmp[tmp>0] <- 1; table(tmp); round(table(tmp)*100/I,0) # <-- with at least one urgency message
table(tmp[bills$info$dmensaje==0])
table(tmp[bills$info$dmensaje==1])
#
table(bills$info$urgIn)
table(bills$info$urgIn[bills$info$dpassed==0])
table(bills$info$urgIn[bills$info$dpassed==1])
#
table(bills$info$urgIn[bills$info$dpassed==0 & bills$info$dmensaje==0])
table(bills$info$urgIn[bills$info$dpassed==1 & bills$info$dmensaje==0])
table(bills$info$urgIn[bills$info$dpassed==0 & bills$info$dmensaje==1])
table(bills$info$urgIn[bills$info$dpassed==1 & bills$info$dmensaje==1])
#
table(bills$info$nUrg)
#
# Whose leg-init bills are declared urgent?
# cut % sponsors into categories of Concertación sponsors
tmp <- cut(x = bills$info$pctcon, breaks = c(0, 1, 25, 50, 75, 99, 100), labels = c("0","1-25","25-50","51-75","76-99","100"), include.lowest = TRUE)
# select pre-piñera
sel <- which(bills$info$dmensaje==0 & bills$info$dateIn<dmy("1/3/2010", tz = "chile") & bills$info$nUrg>0)
round(table(tmp[sel]) / length(sel), digits = 2)
length(sel)
# select piñera
sel <- which(bills$info$dmensaje==0 & bills$info$dateIn>=dmy("1/3/2010", tz = "chile") & bills$info$dateIn<dmy("1/3/2014", tz = "chile")  & bills$info$nUrg>0) # piñera
round(table(tmp[sel]) / length(sel), digits = 2)
length(sel)
# select all
sel <- which(bills$info$dmensaje==0 & bills$info$nUrg>0)
round(table(tmp[sel]) / length(sel), digits = 2)
length(sel)

# BILL'S LAST TRÁMITE(S) AND PATH
## 2nd urgency is v, 3rd is w, anf so forth until z
## return to chamber 1 is 3
## A 1-o
## B 1-u
## C 1-2
## Ch u-o
## D u-2
## E 2-o
## F 2-v
## G 2-3
## H 2-y
## I 2-p
## J y-o
## K y-p
## L v-o
## M v-3
## N 3-o
## Ñ 3-w
## O 3-c
## P 3-z
## Q 3-p
## R z-o
## S z-p
## T w-o
## U w-c
## V c-o
## W c-x
## X c-p
## Y x-o
## Z x-p
##  recode 2-c-. = 2-3-c-.
##  recode 2-v-c-. = 2-v-3-c-.

## change nTr to 1=origen, 2=revisora, 3=origen.bis, 4=conf, 5=ejec, 6=veto, 7=trib, and urgIn accordingly
for (i in 1:I){
    message(sprintf("loop %s of %s", i, I))
    tmp <- bills$tramites[[i]]
    skip <- grep(pattern = "conf*|ejec|veto|trib", tmp$tramite)
    if (length(skip)>0){
        tmp1 <- tmp[-skip,]
    } else {
        tmp1 <- tmp
    }
    tmp1$nTr <- 1:nrow(tmp1)
    if (length(skip)>0){
        tmp[-skip,] <- tmp1
    } else {
        tmp <- tmp1
    }
    tmp$nTr[tmp$tramite=="conf"] <- 4
    tmp$nTr[tmp$tramite=="ejec"] <- 5
    tmp$nTr[tmp$tramite=="veto"] <- 6
    tmp$nTr[tmp$tramite=="trib"] <- 7
    bills$tram[[i]] <- tmp
}

tram <- bills$tramites # duplicate to remove tribunal and veto steps
for (i in 1:I){
    message(sprintf("loop %s of %s", i, I))
    tmp <- tram[[i]]
    drop <- union( grep(pattern = "trib", tmp$tramite), grep(pattern = "veto", tmp$tramite) )
    if (length(drop)>0){
        tmp <- tmp[-drop,]
        tmp$nTr <- 1:nrow(tmp)
    }
    tram[[i]] <- tmp
}

#
lastTram <- rep(NA,I) # no NAs should remain at the end
# will receive num of trámites
tmp1 <- rep(0,I)
for (i in 1:I){
#    if (max(bills$tramites[[i]]$nTr)==1) tmp1[i] <- 1
    tmp1[i] <- max(tram[[i]]$nTr)
}
table(tmp1)
#
# get five-tramite cases
sel <- which(tmp1==5)# & bills$info$nUrg==0)
tmp <- "start"
for (i in sel){
    tmp <- c(tmp, tram[[i]]$tramite[4])
}
table(tmp[-1]) # all end in ejec
sel <- which(tmp1==5 & 

# get single-tramite cases
sel <- which(tmp1==1 & bills$info$nUrg==0)
lastTram[sel] <- "B"
sel <- which(tmp1==1 & bills$info$nUrg==1)
lastTram[sel] <- "E"
#
table(bills$info$urgIn[lastTram=="E"]) # Ojo: los 3 ceros son urgencias de Bachelet II que quité más arriba...
#
# get two-tramite cases
sel <- which(tmp1==2 & bills$info$nUrg==0)
lastTram[sel] <- "G"
sel <- which(tmp1==2 & (bills$info$urgIn==2 | bills$info$urgIn==12))
lastTram[sel] <- "L"
#
# get three-tramite cases ending with ejec
tmp2 <- rep(0,I)
for (i in 1:I){
    if (tmp1[i]!=3) next
    if (tram[[i]]$tramite[3]=="ejec") tmp2[i] <- 1
}
sel <- which(tmp2==1)
lastTram[sel] <- "K"

table(bills$info$urgIn[lastTram=="K"])



table(lastTram)

table(bills$info$urgIn[bills$info$nUrg==1])

sel <- which(bills$info$urgIn==0 & bills$info$nUrg==1)
bills$info$bol[sel]


#see xtabs pre-packaged function
myXtab <- function(v1,v2){ # xtab with row% in cells and row totals
    select <- which(bills$info$dmensaje==0); # will use for all, change if needed
    zetab <- table(v1[select], v2[select], useNA = "no");
    message("MOCIONES")
    print(cbind(round(prop.table(zetab, 1), digits = 2), margin.table(zetab, 1))) # crosstab with shares and column margins
    select <- which(bills$info$dmensaje==1); # will use for all, change if needed
    zetab <- table(v1[select], v2[select], useNA = "no");
    message("MENSAJES")
    print(cbind(round(prop.table(zetab, 1), digits = 2), margin.table(zetab, 1))) # crosstab with shares and column margins
}

table(bills$info$status)

# has urgency v not
tmp <- bills$info$nUrg; tmp[tmp>0] <- 1; table(tmp); round(table(tmp)*100/I,0)
# since 1998
post98 <- rep(0,I); post98[which(bills$info$dateIn>=dmy("1/3/1998", tz = "chile"))] <- 1
table(tmp[post98==1])
myXtab(post98, tmp)

# has urgency (col) v passed (rows)
myXtab(bills$info$dpassed, tmp)
# where did urgency hit v passed
myXtab(bills$info$dpassed, bills$info$urgIn)

colnames(bills$info)
sel <- which(bills$info$init=="dip"); tmp <- bills$info[sel,] # dip-init
myXtab(tmp$dpassed, tmp$urgIn)
sel <- which(bills$info$init=="sen"); tmp <- bills$info[sel,] # sen-init
myXtab(tmp$dpassed, tmp$urgIn)


tmp <- bills$info$nUrg; tmp[bills$info$nUrg>9] <- 10 # nUrg with 10+ urgencias grouped 
myXtab(bills$info$init, tmp)
table(bills$info$nUrg)

table(bills$info$urgIn)

bills$info$dpassed <- 0; bills$info$dpassed[bills$info$status=="statute"] <- 1

table(bills$info$status)
table(bills$info$dmensaje)

    sel <- which(bills$info$legyr==i); tmp <- table(bills$info$dmensaje[sel], bills$info$hasUrgH[sel], useNA = "ifany")
    print(cbind(round(prop.table(tmp, 1), digits = 2), margin.table(tmp, 1))) # crosstab with shares and column margins
tmp <- table(bills$info$dmensaje, bills$info$hasUrgH, useNA = "ifany") # whole period
print(cbind(round(prop.table(tmp, 1), digits = 2), margin.table(tmp, 1))) # crosstab with shares and column margins



## # Esto está pendiente terminarlo: add bills$urg to bills with urgencia in hitos only
## library(lubridate)
## library(timeDate)
## library(plyr)
## sel <- which(bills$info$hasUrgHU=="yes" & bills$info$hasUrg=="no")
## bills$info$bol[sel]
## tmp <- bills$urg[[which(bills$info$bol=="4639-11")]]; tmp <- tmp[1,] # choose bill with urgencia to copy object format
## i <- sel[1] # loop over sel
## tmp2 <- bills$hitos[[i]]
## tmp2 <- tmp2[grep("[Uu]rgencia", tmp2$action), c("date","urg","chamber")]; colnames(tmp2) <- c("on","urg","tramite")
## tmp2$type <- "."; tmp3 <- tmp2$type
## select <- grep("urg30", tmp2$urg); tmp2$type[select] <- "Simple"; tmp3[select] <- 30
## select <- grep("urg15", tmp2$urg); tmp2$type[select] <- "Suma"; tmp3[select] <- 15
## select <- grep("urg6", tmp2$urg); tmp2$type[select] <- "Discusión inmediata"; tmp3[select] <- 6
## tmp3 <- as.numeric(tmp3)
## select <- which(tmp2$on < dmy("3/7/2010", tz = "chile")) # change urgencias before constitutional reform
## tmp3[select] <- mapvalues(tmp3[select], from = c(6,15), to = c(3,10), warn_missing = FALSE)
## tmp2$deadline <- tmp2$on # inherits format for NAs
## tmp2$deadline <- deadline(tmp2$on, tmp3)
## # en tmp2 falta dcaduca, dretir, off, chain, change





xx

### use for bicameral overrule loop "insistencia"
## tmp <- "."
## for (i in 1:I){
##     message(sprintf("loop %s of %s", i, I))
##     tmp1 <- grep(pattern = "insistencia", bills$hitos[[i]]$action) # do this in other loop to find bicameral overrule with president's request art. 68
##     tmp2 <- grep(pattern = "([Aa]rt.|artículo) 68", bills$hitos[[i]]$action) 
##     tmp1 <- union(tmp1, tmp2) # remove repeated lines
##     if (length(tmp1)>0){ 
##         tmp <- append(tmp, as.character(bills$hitos[[i]]$action[tmp1])) # which record
##     }
## }
## tmp <- sub(".*Oficio.*consulta.*(President[ea]|[Ee]jecutivo).*(Art.|artículo) 71.*insistencia.*"               , replacement = ".", tmp) #art71=bicamOverrule
## tmp <- sub(".*Oficio( de)* insistencia a Cámara.*"                                                             , replacement = ".", tmp) #art71=bicamOverrule
## tmp <- sub(".*Cuenta [Oo]ficio rechazo insistencia.*"                                                          , replacement = ".", tmp) #art71=bicamOverrule
## tmp <- sub(".*Cuenta [Oo]ficio aprobación insistencia.*"                                                       , replacement = ".", tmp) #art71=bicamOverrule
## tmp <- sub(".*Cuenta [Oo]ficio.*admisibilidad.*insistencia.*"                                                  , replacement = ".", tmp) #art71=bicamOverrule
## tmp <- sub(".*Cuenta [Oo]ficio insistencia del Ejecutivo.*"                                                    , replacement = ".", tmp) #art71=bicamOverrule
## tmp <- sub(".*Oficio( de)* consulta.*ejercer facultad.*insistencia.*"                                          , replacement = ".", tmp) #art71=bicamOverrule
## tmp <- sub(".*Oficio de.*Presidente.*uso.*([Aa]rt[.]|artículo) 68.*"                                           , replacement = ".", tmp) #art71=bicamOverrule
## tmp <- sub(".*[Oo]ficio de.*Cámara.*ha rechazado.*Ejecutivo.*uso.*([Aa]rt[.]|artículo) 68.*"                   , replacement = ".", tmp) #art71=bicamOverrule
## tmp <- sub(".*Sala fija un plazo.*solicitud de insistencia.*"                                                  , replacement = ".", tmp) #art71=bicamOverrule
## tmp <- sub(".*Discusión insistencia.*"                                                                         , replacement = ".", tmp) #art71=bicamOverrule


tmp <- 0
for (i in 1:I){
    message(sprintf("loop %s of %s", i, I))
    tmp1 <- grep(pattern = "comisión para primer informe Senado comunica", bills$hitos[[i]]$action)
    if (length(tmp1)>0){
        tmp <- append(tmp, i)
    }
}

i <- 5349
bills$hitos[[i]]$rawText

colnames(bills$hitos[[i]])   

sel <- which(bills$info$debug==1)
tmp <- rep(0,I)
for (i in sel){
    tmp[i] <- ifelse(bills$hitos[[i]]$chamber[1]==".", 1, 0)
}
i <- which(tmp==1)


table(bills$info$debug[sel]) # should all be 3

table(nHitos)
i <- sel[2]
bills$hitos[[i]]





ls()

table(bills$hitos[[i]]$chamber)
which(bills$hitos[[i]]$chamber==".")


HAY QUE LIMPIAR bills$hitos[[i]]$chamber (QUITAR HUECOS)



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


