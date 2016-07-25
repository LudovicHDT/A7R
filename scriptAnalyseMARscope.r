#####Script d'analyse des données calculées par MARscope#######



#Initialisation du script
setwd("/media/win_shared/data_sync/Documents/INRA_PC/Data_MARscope/")
fluxLum = read.table("Carac_Lusignan_2016-07-09_002.txt", header = TRUE, sep = ";", dec = ".")
attach(fluxLum)


#Sélection des données
capteur_01 = subset(fluxLum, Id == 'a01')			#Capteur n°1 + incident
capteur_02 = subset(fluxLum, Id == 'a02')			#Capteur n°2 + incident
capteur_03 = subset(fluxLum, Id == 'a03' & Ray == '2')
capteur_04 = subset(fluxLum, Id == 'a04' & Ray == '2')
capteur_05 = subset(fluxLum, Id == 'a05' & Ray == '2')
capteur_06 = subset(fluxLum, Id == 'a06' & Ray == '2')


##Premier jeu de graphiques

#Graphiques des flux photoniques

svg("1_flux_tot_+_bleu.svg", width = 15, height = 10, onefile = TRUE)
par(mfrow = c(2,4), oma = c(1, 1, 1, 0), mar = c(5, 5, 5, 1))

V = c("Flux_photons", "UVA_BLEU", "BLEU_elargi", "BLEU_strict")
TITRE = c("Flux photonique transmis", "UVA bleu transmis", "Bleu élargi transmis", "Bleu strict transmis")
LABELS = c("Flux photonique [400; 700]", "UVA bleu [350; 500]", "Bleu élargi [400; 500]", "Bleu strict [445; 455]")


for(i in 1:4){
plot(strptime(capteur_01$Date_Heure, "%H:%M:%S"), capteur_01[,V[i]],
     col = capteur_01$Ray, pch = 19,
     main = TITRE[i],
     xlab = "Temps", ylab = LABELS[i],
     cex.main = 2, cex.lab = 1.5, cex.axis = 1.5)
points(strptime(capteur_03$Date_Heure, "%H:%M:%S"), capteur_03[,V[i]], col = "green", pch = 19)
points(strptime(capteur_04$Date_Heure, "%H:%M:%S"), capteur_04[,V[i]], col = "blue", pch = 19)

bozo = c("Incident", "F. excelsior", "A. cordata", "M. alba")
couleurs = c("black", "red", "green", "blue")
legend(x = "topright", legend = bozo, fill = couleurs, bty !='n', bg = "white")

plot(strptime(capteur_02$Date_Heure, "%H:%M:%S"), capteur_02[,V[i]],
     col = capteur_02$Ray, pch = 19,
     main = TITRE[i],
     xlab = "Temps", ylab = LABELS[i],
     cex.main = 2, cex.lab = 1.5, cex.axis = 1.5)
points(strptime(capteur_05$Date_Heure, "%H:%M:%S"), capteur_05[,V[i]], col = "lightgreen", pch = 19)
points(strptime(capteur_06$Date_Heure, "%H:%M:%S"), capteur_06[,V[i]], col = "lightblue", pch = 19)

bozo = c("Incident", "F. excelsior", "A. cordata", "M. alba")
couleurs = c("black", "red", "lightgreen", "lightblue")
legend(x = "topright", legend = bozo, fill = couleurs, bty != 'n', bg = "white")
}

dev.off()


##Deuxième jeu de graphiques
svg("2_vert_+_rouge.svg", width = 15, height = 10, onefile = TRUE)
par(mfrow = c(2,4), oma = c(1,1,1,0), mar = c(5,5,5,1))

V = c("VERT", "ROUGE_CLAIR", "ROUGE_SOMBRE", "RC_RS")
TITRE = c("Vert transmis", "Rouge transmis", "Infrarouge transmis", "RC/RS")
LABELS = c("Vert [500; 600]", "Rouge visible [600; 700]", "Infrarouge [700; 800]", "RC/RS")

plot(strptime(capteur_01$Date_Heure, "%H:%M:%S"), capteur_01[,V[i]],
     col = capteur_01$Ray, pch = 19,
     main = TITRE[i],
     xlab = "Temps", ylab = LABELS[i],
     cex.main = 2, cex.lab = 1.5, cex.axis = 1.5)
points(strptime(capteur_03$Date_Heure, "%H:%M:%S"), capteur_03[,V[i]], col = "green", pch = 19)
points(strptime(capteur_04$Date_Heure, "%H:%M:%S"), capteur_04[,V[i]], col = "blue", pch = 19)

bozo = c("Incident", "F. excelsior", "A. cordata", "M. alba")
couleurs = c("black", "red", "green", "blue")
legend(x = "topright", legend = bozo, fill = couleurs, bty != 'n', bg = "white")

plot(strptime(capteur_02$Date_Heure, "%H:%M:%S"), capteur_02[,V[i]],
     col = capteur_02$Ray, pch = 19,
     main = TITRE[i],
     xlab = "Temps", ylab = LABELS[i],
     cex.main = 2, cex.lab = 1.5, cex.axis = 1.5)
points(strptime(capteur_05$Date_Heure, "%H:%M:%S"), capteur_05[,V[i]], col = "lightgreen", pch = 19)
points(strptime(capteur_06$Date_Heure, "%H:%M:%S"), capteur_06[,V[i]], col = "lightblue", pch = 19)

bozo = c("Incident", "F. excelsior", "A. cordata", "M. alba")
couleurs = c("black", "red", "lightgreen", "lightblue")
legend(x = "topright", legend = bozo, fill = couleurs, bty != 'n', bg = "white")


dev.off()


## Troisième jeu de graphiques
svg("3_infrarouge_+_theta.svg", width = 15, height = 10, onefile = TRUE)

#Graphiques rouge strict
par(mfrow = c(2,4), oma = c(1,1,1,0), mar = c(5,5,5,1))

V = c("RC_strict", "RS_strict", "ZETA", "THETA")
TITRE = c("Rouge strict transmis", "Infrarouge réduit", "Z", "Théta")
LABELS = c("Rouge strict [655; 665]", "Infrarouge réduit [725; 735]", "ZETA", "Théta")

plot(strptime(capteur_01$Date_Heure, "%H:%M:%S"), capteur_01[,V[i]],
     col = capteur_01$Ray, pch = 19,
     main = TITRE[i],
     xlab = "Temps", ylab = LABELS[i],
     cex.main = 2, cex.lab = 1.5, cex.axis = 1.5)
points(strptime(capteur_03$Date_Heure, "%H:%M:%S"), capteur_03[,V[i]], col = "green", pch = 19)
points(strptime(capteur_04$Date_Heure, "%H:%M:%S"), capteur_04[,V[i]], col = "blue", pch = 19)

bozo = c("Incident", "F. excelsior", "A. cordata", "M. alba")
couleurs = c("black", "red", "green", "blue")
legend(x = "topright", legend = bozo, fill = couleurs, bty != 'n', bg = "white")

plot(strptime(capteur_02$Date_Heure, "%H:%M:%S"), capteur_02[,V[i]],
     col = capteur_02$Ray, pch = 19,
     main = TITRE[i],
     xlab = "Temps", ylab = LABELS[i],
     cex.main = 2, cex.lab = 1.5, cex.axis = 1.5)
points(strptime(capteur_05$Date_Heure, "%H:%M:%S"), capteur_05[,V[i]], col = "lightgreen", pch = 19)
points(strptime(capteur_06$Date_Heure, "%H:%M:%S"), capteur_06[,V[i]], col = "lightblue", pch = 19)

bozo = c("Incident", "F. excelsior", "A. cordata", "M. alba")
couleurs = c("black", "red", "lightgreen", "lightblue")
legend(x = "topright", legend = bozo, fill = couleurs, bty != 'n', bg = "white")


dev.off()

