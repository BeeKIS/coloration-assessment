rm(list = ls())

# set wd
#setwd("C:/Users/jernejb/Kmetijski inštitut Slovenije/BeeTeam - Dokumenti/STUDENTI/Bubnic/vsebina/phd_project/barva_riti/2022_slikeAbdomni_JernejBubnic/poskus inkubator/roi_med")
setwd("C:/Users/jernejb/Kmetijski inštitut Slovenije/BeeTeam - Dokumenti/STUDENTI/Bubnic/vsebina/phd_project/barva_riti/2022_slikeAbdomni_JernejBubnic/poskus inkubator/roi_half")
# load libraries

library(jpeg)
library(stats)
library(interp)
library(dplyr)
library(pracma)
library(tiff)
library(tibble)

# load images, dobim vekotor z imeni slik

images <- dir(pattern = "*.tif")

# naredim prazen list, kamor spravim vektor sivine za vsako sliko.

vecList <- list()

# v zanki grem cez vse slike, jih pretvorim v 3 matrike za RGB, jih povprecim, da dobim sivino
# ter nardim povprecje po stolpcu da dobim za vsako sliko en vektor

for (i in 1:length(images)){
  img <- readTIFF(images[i]) # objekt img je array

  #v matrikah bodo neke decimalne vrednosti, ki jih je eventuelno treba pomozit
  #z 255, ce zelim dobiti dejanske RGB vrednosti za piksle

  r <- (img[ , , 1])
  g <- (img[ , , 2])
  b <- (img[ , , 3])

  # ker so vse tri matrike enkaih dimenzij jih lahko sestejem, po principu
  # prvi element prve matrike + prvi element druge matrike + prvi element tretje matrike
  # naredim povprecje vseh 3 da dobim gray scale
  # primerni koeficienti za pretvorbo v gr so 0,299 za R, 0,587 za G in 0,114 za B.

  gr <- (r*0.299) + (g*0.587) + (b*0.114)

  #poberem proc vse vrstice in stolpce, ki vsebujejo samo 0, ce delam za polovice riti, 0 zamenjam z NA.

  gr <- gr[, -(which(colSums(gr) == 0))]
  gr <- gr[ -(which(rowSums(gr) == 0)),]
  gr[gr == 0] <- NA

  # poberem proc vse nasicene pixle, da gre proc odsev

  gr[gr >= 0.95] <- NA

  #naredim mediano po stolpcih, da pomečem ven autlajerje in dobim vektor

  grVec <- as.vector(apply(gr,2,mean, na.rm = TRUE))

  #vsak vektor posebej pospravim v list. vsak vektor v listu predstavlja eno sliko

  vecList[[i]]<-  grVec
}

# dolocim najdaljso dolzino vektorja

maxLen <- max(lengths(vecList))

# naredim df za zajem podatkov

colour <- data.frame(img = NA, coloration = NA, zap_st = NA)

# v tej zanki raztegnem vse vektorje na dolžino najdaljsega, ter zracunam
# povrsino pod krivuljo

# naredim se list s standardnimi na spline pofitanmi vektorji
vecListStand <- list()

 for (i in 1:length(vecList)){
   # interpolacija
   if (length(vecList[[i]]) < maxLen ){
     dolzina <- maxLen - length(vecList[[i]])

     # dobim željeno število vrednosti, ki je enko razliki med max dolzino vseh
     # vektorjev in dolzino obravnavange vektorja
     # argument n pove, koliko enakomernih točk se algoritem zmisli.
     # preveri, če je prav, da se zmišljujem točke, ki manjakjo do najdaljše dolžine, ali je pravilno da se zmislim št točk, ki je enako najdalši dolžini, mislim, da je to drugo pravilno.
     # se pravi mora biti argument n = maxLen in potem odpade vse kombiniranje interpoliranih vrednosti in originalnih

     interpoleted <- interp::aspline(vecList[[1]], n = dolzina, ties = "ordered")

     # skombinirm orignal vektor in interpolirane vrednosti

     original <- data.frame(x = (1: length(vecList[[i]])), y = vecList[[i]])

     newVal <- data.frame(x = interpoleted$x, y = interpoleted$y)
     standVec <- data.frame(x = NA, y = NA)
     standVec <- rbind(original, newVal)
     standVec <- arrange(standVec, x)

     # standVec je vektor inteprpoliranih sivih vrednost za vsako sliko

     standVec <- pull(standVec, y)

   } else {

     #naredim objekt standVec še za najdalšga

     standVec <- vecList[[i]]

   }

   # pofitam na spline, da zgladim šume

   spline <- smooth.spline(standVec)

   # zračunam površino pod krivuljo s pomočjo trapezoidnega pravila
   auc <- trapz(spline$x, spline$y)
   vecListStand[[i]]<-  as.vector(spline$y)
   # dodam še sample ID
   b <- c(1:length(images))
   colour <- rbind(colour, c( images[[i]], auc, as.numeric(b[i])))
 }

 colour <- na.omit(colour)
 write.csv(colour, file = "coloration_half.csv")
 # tuki zapišem celotne vektorje v df z vsemi vrednostmi, ce bi jih rabil za ml
 # #plot(x = 1:maxLen, y = vecListStand[[36]])
 #
 # zapišem interpolirane in na splaine pofitane vrednosti
 standDF <- data.frame(matrix(unlist(vecListStand), nrow=length(vecListStand), byrow=TRUE))

 #dodam samp_id in exp_group

 info <- readxl::read_xlsx("C:/Users/jernejb/Kmetijski inštitut Slovenije/BeeTeam - Dokumenti/STUDENTI/Bubnic/vsebina/phd_project/barva_riti/2022_slikeAbdomni_JernejBubnic/poskus inkubator/analize/colourMeasurments.xlsx")
 info <- info[-196,] #delit the row 196 since I don't have the col measurement

 standDF <- add_column(standDF, info$samp_ID, .before = 1)
 standDF <- add_column(standDF, info$samp_nr, .before = 1)
 colnames(standDF)[1] <- "samp_nr"
 colnames(standDF)[2] <- "exp_group"
 write.csv(standDF, file = "vectors_half.csv")



















