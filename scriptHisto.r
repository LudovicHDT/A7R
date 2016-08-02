######Script d'analyse des données calculées par MARscope#######



#Initialisation du script
library(gplots)
library(plyr)
setwd("/media/win_shared/data_sync/Documents/INRA_PC/Data_MARscope/")
fluxLum = read.table("Carac_Lusignan_2016-07-09_003.txt", header = TRUE, sep = ";", dec = ".")
attach(fluxLum)

#Sélection des données
capteur_in = subset(fluxLum, Id == 'a01' & Ray == '1')
capteur_01 = subset(fluxLum, Id == 'a01' & Ray == '2')
capteur_02 = subset(fluxLum, Id == 'a02' & Ray == '2')
capteur_03 = subset(fluxLum, Id == 'a03' & Ray == '2')
capteur_04 = subset(fluxLum, Id == 'a04' & Ray == '2')
capteur_05 = subset(fluxLum, Id == 'a05' & Ray == '2')
capteur_06 = subset(fluxLum, Id == 'a06' & Ray == '2')


##Premier jeu de graphiques

HEADER = c("Flux_photons", "UVA_BLEU", "BLEU_elargi", "BLEU_strict")
TITRE = c("Flux photonique moyen transmis", "UVA bleu moyen transmis", "Bleu élargi moyen transmis", "Bleu strict moyen transmis")
LABELS = c("Flux photonique [400; 700]", "UVA bleu [350; 500]", "Bleu élargi [400; 500]", "Bleu strict [445; 455]")
SENSORS = list(capteur_in, capteur_01, capteur_02, capteur_03, capteur_04, capteur_05, capteur_06)
k = c("a", "b", "c", "d", "e", "f", "g")
sd.s = c("h", "i", "j", "l", "m", "n", "o")


svg("1_barplot.svg", width = 15, height = 10, onefile = TRUE)
par(mfrow = c(2,2), oma = c(1,1,1,0), mar = c(5,5,5,1))

for(i in 1:4){
  for(j in 1:7){
    k[j] = mean(SENSORS[[j]][,HEADER[i]])
    sd.s[j] = sd(SENSORS[[j]][,HEADER[i]])
  }
  v = k[1]
  k = k[-1]
  sd.s = sd.s[-1]
  barplot2(as.numeric(k), space = 0, add = FALSE, axes = TRUE, xpd = FALSE,
	  plot.ci = TRUE, ci.l = as.numeric(k), ci.u = as.numeric(k) + as.numeric(sd.s), col = "lightblue",
	  main = TITRE[i], xlab = "Espèces", ylab = LABELS[i],
	  cex.main = 2, cex.lab = 1.5, cex.axis = 1.5)
  axis(1, at = seq(0.5, 5.5, 1), labels = c('F. excel', 'F. excel', 'A. cor', 'M. alba', 'A. cor', 'M. alba'))

  legend(x = "topright", legend = round(as.numeric(v),2), title = "Valeur de l'incident", bty = "n")
}

dev.off()



##Deuxième jeu de graphiques

HEADER = c("ROUGE_CLAIR", "ROUGE_SOMBRE", "RC_RS", "PHI")
TITRE = c("Rouge moyen transmis", "Infrarouge moyen transmis", "Rouge sur IR moyen", "Phi moyen")
LABELS = c("Rouge [350600; 700]", "IR [700; 800]", "Rouge/IR", "Phi")
SENSORS = list(capteur_in, capteur_01, capteur_02, capteur_03, capteur_04, capteur_05, capteur_06)
k = c("a", "b", "c", "d", "e", "f", "g")
sd.s = c("h", "i", "j", "l", "m", "n", "o")


svg("2_barplot.svg", width = 15, height = 10, onefile = TRUE)
par(mfrow = c(2,2), oma = c(1,1,1,0), mar = c(5,5,5,1))

for(i in 1:4){
  for(j in 1:7){
    k[j] = mean(SENSORS[[j]][,HEADER[i]])
    sd.s[j] = sd(SENSORS[[j]][,HEADER[i]])
  }
  v = k[1]
  k = k[-1]
  sd.s = sd.s[-1]
  barplot2(as.numeric(k), space = 0, add = FALSE, axes = TRUE, xpd = FALSE,
	  plot.ci = TRUE, ci.l = as.numeric(k), ci.u = as.numeric(k) + as.numeric(sd.s), col = "lightblue",
	  main = TITRE[i], xlab = "Espèces", ylab = LABELS[i],
	  cex.main = 2, cex.lab = 1.5, cex.axis = 1.5)
  axis(1, at = seq(0.5, 5.5, 1), labels = c('F. excel', 'F. excel', 'A. cor', 'M. alba', 'A. cor', 'M. alba'))

  legend(x = "topright", legend = round(as.numeric(v),2), title = "Valeur de l'incident", bty = "n")
}

dev.off()


HEADER = c("THETA", "RC_strict", "RS_strict", "ZETA")
TITRE = c("THETA moyen", "Rouge strict moyen transmis", "Infrarouge réduit moyen transmis", "ZETA")
LABELS = c("THETA", "Rouge strict [655; 665]", "IR réduit [725; 735]", "ZETA")
SENSORS = list(capteur_in, capteur_01, capteur_02, capteur_03, capteur_04, capteur_05, capteur_06)
k = c("a", "b", "c", "d", "e", "f", "g")
sd.s = c("h", "i", "j", "l", "m", "n", "o")


svg("3_barplot.svg", width = 15, height = 10, onefile = TRUE)
par(mfrow = c(2,2), oma = c(1,1,1,0), mar = c(5,5,5,1))

for(i in 1:4){
  for(j in 1:7){
    k[j] = mean(SENSORS[[j]][,HEADER[i]])
    sd.s[j] = sd(SENSORS[[j]][,HEADER[i]])
  }
  v = k[1]
  k = k[-1]
  sd.s = sd.s[-1]
  barplot2(as.numeric(k), space = 0, add = FALSE, axes = TRUE, xpd = FALSE,
	  plot.ci = TRUE, ci.l = as.numeric(k), ci.u = as.numeric(k) + as.numeric(sd.s), col = "lightblue",
	  main = TITRE[i], xlab = "Espèces", ylab = LABELS[i],
	  cex.main = 2, cex.lab = 1.5, cex.axis = 1.5)
  axis(1, at = seq(0.5, 5.5, 1), labels = c('F. excel', 'F. excel', 'A. cor', 'M. alba', 'A. cor', 'M. alba'))

  legend(x = "topright", legend = round(as.numeric(v),2), title = "Valeur de l'incident", bty = "n")
}

dev.off()



