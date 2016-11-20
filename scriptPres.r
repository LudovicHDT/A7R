##### Script d'analyse des données de MARscope #######


## Part of the A7R project, this script was intended to produce figures for report.
## Copyright (C) 2016  Ludovic Hondet

## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.

## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.


# Initialisation du script
library(tikzDevice)
setwd("/media/win_shared/data_sync/Documents/INRA_PC/Data_MARscope/")


## Création des nuages de points
fluxLum = read.table("Carac_Lusignan_2016-07-09_002.txt", header = TRUE,
		     sep = ";", dec = ".")
attach(fluxLum)



# Sélection des données
capteur_01 = subset(fluxLum, Id == 'a01')			# Capteur n°1 + incident
capteur_02 = subset(fluxLum, Id == 'a02')			# Capteur n°2 + incident
capteur_03 = subset(fluxLum, Id == 'a03' & Ray == '2')
capteur_04 = subset(fluxLum, Id == 'a04' & Ray == '2')
capteur_05 = subset(fluxLum, Id == 'a05' & Ray == '2')
capteur_06 = subset(fluxLum, Id == 'a06' & Ray == '2')


# Tracer les graphiques

tikz("rapport_plots.tex", width = 10, height = 4, onefile = TRUE, engine = "xetex")
par(mfrow = c(1,3), oma = c(1,1,1,0), mar = c(5,5,5,1))


HEADER = c("Flux_photons", "UVA_BLEU", "ZETA")
TITRE = c("PAR transmis", "UVA-bleu transmis", "ZETA transmis")
LABELS = c("PAR (µmol.m$^{-2}$.s$^{-1}$)", "Flux photonique UVA-bleu (µmol.m$^{-2}$.s$^{-1}$)", "ZETA")


for(i in 1:3){
plot(strptime(capteur_01$Date_Heure, "%H:%M:%S"), capteur_01[,HEADER[i]],
     col = capteur_01$Ray, pch = 19,
     main = TITRE[i],
     xlab = "Temps UTC", ylab = LABELS[i],
     cex.main = 2, cex.lab = 1.5, cex.axis = 1.5)
points(strptime(capteur_03$Date_Heure, "%H:%M:%S"), capteur_03[,HEADER[i]],
       col = "green", pch = 19)
points(strptime(capteur_04$Date_Heure, "%H:%M:%S"), capteur_04[,HEADER[i]],
       col = "blue", pch = 19)

bozo = c("Incident", "F. excelsior", "A. cordata", "M. alba")
couleurs = c("black", "red", "green", "blue")
legend(x = "topright", legend = bozo, fill = couleurs, bty !='n', bg = "white",
       cex = 1.5)
}
dev.off()


## Création des boxplots

fluxLum = read.table("Carac_Lusignan_2016-07-09_004.txt", header = TRUE,
		     sep = ";", dec = ".")
attach(fluxLum)

TR = subset(fluxLum, Id=='a01'&Ray=='2' | Id=='a02'&Ray=='2' | Id=='a03'&Ray=='2'
	    | Id=='a05'&Ray=='2' | Id=='a04'&Ray=='2' | Id=='a06' & Ray=='2')
mean_in = c(1396.27,375.8148,1.113237)

tikz("rapport_barplots.tex", width = 9, height = 3, onefile = TRUE, engine = "xetex")
par(mfrow = c(1,3), oma = c(1,1,1,0), mar = c(5,5,5,1))


HEADER = c("Flux_photons", "UVA_BLEU", "ZETA")
TITRE = c("PAR transmis", "UVA-bleu transmis", "ZETA transmis")
LABELS = c("PAR (µmol.m$^{-2}$.s$^{-1}$)", "UVA-bleu (µmol.m$^{-2}$.s$^{-1}$)", "ZETA")


for(i in 1:3){
  boxplot(TR[,HEADER[i]]~Id, data = TR, range = 0, main = TITRE[i],
	  xlab = "Espèces", ylab = LABELS[i],
	  names = c("F. exc","F. exc","A. cor","A. cor","M. al","M. al"))
  legend(x = "bottomleft", legend = mean_in[i], title = "Valeur de l'incident",
	 bty = "n")
  means = aggregate(TR[,HEADER[i]]~Id, data = TR, mean)
  means = as.list(means)
  points(1:6, means$`TR[, HEADER[i]]`, col = "red")
}
dev.off()

