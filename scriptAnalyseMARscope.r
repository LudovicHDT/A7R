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


##Tracer les graphique

#Graphique des flux photoniques

svg("Flux_tot_+_bleu.svg", width = 15, height = 10, onefile = TRUE)

par(mfrow = c(2,4))
plot(strptime(capteur_01$Date_Heure, "%H:%M:%S"), capteur_01$Flux_photons,
     col = capteur_01$Ray, pch = 19,
     main = "Flux de photons transmis par arbre",
     xlab = "Temps", ylab = "Flux de photons")
points(strptime(capteur_03$Date_Heure, "%H:%M:%S"), capteur_03$Flux_photons, col = "green", pch = 19)
points(strptime(capteur_04$Date_Heure, "%H:%M:%S"), capteur_04$Flux_photons, col = "blue", pch = 19)

bozo = c("Incident", "F. excelsior", "A. cordata", "M. alba")
couleurs = c("black", "red", "green", "blue")
legend(x = "topright", legend = bozo, fill = couleurs, bty !='n', bg = "white")

plot(strptime(capteur_02$Date_Heure, "%H:%M:%S"), capteur_02$Flux_photons,
     col = capteur_02$Ray, pch = 19,
     main = "Flux de photons transmis par arbre",
     xlab = "Temps", ylab = "Flux de photons")
points(strptime(capteur_05$Date_Heure, "%H:%M:%S"), capteur_05$Flux_photons, col = "lightgreen", pch = 19)
points(strptime(capteur_06$Date_Heure, "%H:%M:%S"), capteur_06$Flux_photons, col = "lightblue", pch = 19)

bozo = c("Incident", "F. excelsior", "A. cordata", "M. alba")
couleurs = c("black", "red", "lightgreen", "lightblue")
legend(x = "topright", legend = bozo, fill = couleurs, bty != 'n', bg = "white")


#Graphique UVA_BLEU
plot(strptime(capteur_01$Date_Heure, "%H:%M:%S"), capteur_01$UVA_BLEU,
     col = capteur_01$Ray, pch = 19,
     main = "UVA bleu transmis par les arbres",
     xlab = "Temps", ylab = "UVA bleu [350; 500]")
points(strptime(capteur_03$Date_Heure, "%H:%M:%S"), capteur_03$UVA_BLEU, col = "green", pch = 19)
points(strptime(capteur_04$Date_Heure, "%H:%M:%S"), capteur_04$UVA_BLEU, col = "blue", pch = 19)

bozo = c("Incident", "F. excelsior", "A. cordata", "M. alba")
couleurs = c("black", "red", "green", "blue")
legend(x = "topright", legend = bozo, fill = couleurs, bty != 'n', bg = "white")

plot(strptime(capteur_02$Date_Heure, "%H:%M:%S"), capteur_02$UVA_BLEU,
     col = capteur_02$Ray, pch = 19,
     main = "UVA bleu  transmis par les arbres",
     xlab = "Temps", ylab = "UVA bleu [350; 500]")
points(strptime(capteur_05$Date_Heure, "%H:%M:%S"), capteur_05$UVA_BLEU, col = "lightgreen", pch = 19)
points(strptime(capteur_06$Date_Heure, "%H:%M:%S"), capteur_06$UVA_BLEU, col = "lightblue", pch = 19)

bozo = c("Incident", "F. excelsior", "A. cordata", "M. alba")
couleurs = c("black", "red", "lightgreen", "lightblue")
legend(x = "topright", legend = bozo, fill = couleurs, bty != 'n', bg = "white")


#Graphique bleu élargi
plot(strptime(capteur_01$Date_Heure, "%H:%M:%S"), capteur_01$BLEU_elargi,
     col = capteur_01$Ray, pch = 19,
     main = "Bleu élargi transmis par les arbres",
     xlab = "Temps", ylab = "Bleu élargi [400; 500]")
points(strptime(capteur_03$Date_Heure, "%H:%M:%S"), capteur_03$BLEU_elargi, col = "green", pch = 19)
points(strptime(capteur_04$Date_Heure, "%H:%M:%S"), capteur_04$BLEU_elargi, col = "blue", pch = 19)

bozo = c("Incident", "F. excelsior", "A. cordata", "M. alba")
couleurs = c("black", "red", "green", "blue")
legend(x = "topright", legend = bozo, fill = couleurs, bty != 'n', bg = "white")

plot(strptime(capteur_02$Date_Heure, "%H:%M:%S"), capteur_02$BLEU_elargi,
     col = capteur_02$Ray, pch = 19,
     main = "Bleu elargi transmis par les arbres",
     xlab = "Temps", ylab = "Bleu elargi [400; 500]")
points(strptime(capteur_05$Date_Heure, "%H:%M:%S"), capteur_05$BLEU_elargi, col = "lightgreen", pch = 19)
points(strptime(capteur_06$Date_Heure, "%H:%M:%S"), capteur_06$BLEU_elargi, col = "lightblue", pch = 19)

bozo = c("Incident", "F. excelsior", "A. cordata", "M. alba")
couleurs = c("black", "red", "lightgreen", "lightblue")
legend(x = "topright", legend = bozo, fill = couleurs, bty != 'n', bg = "white")


#Graphique bleu strict
plot(strptime(capteur_01$Date_Heure, "%H:%M:%S"), capteur_01$BLEU_strict,
     col = capteur_01$Ray, pch = 19,
     main = "Bleu strict transmis par les arbres",
     xlab = "Temps", ylab = "Bleu strict [445; 455]")
points(strptime(capteur_03$Date_Heure, "%H:%M:%S"), capteur_03$BLEU_strict, col = "green", pch = 19)
points(strptime(capteur_04$Date_Heure, "%H:%M:%S"), capteur_04$BLEU_strict, col = "blue", pch = 19)

bozo = c("Incident", "F. excelsior", "A. cordata", "M. alba")
couleurs = c("black", "red", "green", "blue")
legend(x = "topright", legend = bozo, fill = couleurs, bty != 'n', bg = "white")

plot(strptime(capteur_02$Date_Heure, "%H:%M:%S"), capteur_02$BLEU_strict,
     col = capteur_02$Ray, pch = 19,
     main = "Bleu strict transmis par les arbres",
     xlab = "Temps", ylab = "Bleu strict [445; 455]")
points(strptime(capteur_05$Date_Heure, "%H:%M:%S"), capteur_05$BLEU_strict, col = "lightgreen", pch = 19)
points(strptime(capteur_06$Date_Heure, "%H:%M:%S"), capteur_06$BLEU_strict, col = "lightblue", pch = 19)

bozo = c("Incident", "F. excelsior", "A. cordata", "M. alba")
couleurs = c("black", "red", "lightgreen", "lightblue")
legend(x = "topright", legend = bozo, fill = couleurs, bty != 'n', bg = "white")

dev.off()

##Deuxième jeu de graphiques

X11()
svg("Vert_+_rouge.svg", width = 15, height = 10, onefile = TRUE)

#Graphiques du vert transmis
par(mfrow = c(2,4))
plot(strptime(capteur_01$Date_Heure, "%H:%M:%S"), capteur_01$VERT,
     col = capteur_01$Ray, pch = 19,
     main = "Vert transmis par les arbres",
     xlab = "Temps", ylab = "Vert [500; 600]")
points(strptime(capteur_03$Date_Heure, "%H:%M:%S"), capteur_03$VERT, col = "green", pch = 19)
points(strptime(capteur_04$Date_Heure, "%H:%M:%S"), capteur_04$VERT, col = "blue", pch = 19)

bozo = c("Incident", "F. excelsior", "A. cordata", "M. alba")
couleurs = c("black", "red", "green", "blue")
legend(x = "topright", legend = bozo, fill = couleurs, bty != 'n', bg = "white")

plot(strptime(capteur_02$Date_Heure, "%H:%M:%S"), capteur_02$VERT,
     col = capteur_02$Ray, pch = 19,
     main = "Vert transmis par les arbres",
     xlab = "Temps", ylab = "Vert [500; 600]")
points(strptime(capteur_05$Date_Heure, "%H:%M:%S"), capteur_05$VERT, col = "lightgreen", pch = 19)
points(strptime(capteur_06$Date_Heure, "%H:%M:%S"), capteur_06$VERT, col = "lightblue", pch = 19)

bozo = c("Incident", "F. excelsior", "A. cordata", "M. alba")
couleurs = c("black", "red", "lightgreen", "lightblue")
legend(x = "topright", legend = bozo, fill = couleurs, bty != 'n', bg = "white")


#Graphiques du rouge clair
plot(strptime(capteur_01$Date_Heure, "%H:%M:%S"), capteur_01$ROUGE_CLAIR,
     col = capteur_01$Ray, pch = 19,
     main = "Rouge transmis par les arbres",
     xlab = "Temps", ylab = "Rouge [600; 700]")
points(strptime(capteur_03$Date_Heure, "%H:%M:%S"), capteur_03$ROUGE_CLAIR, col = "green", pch = 19)
points(strptime(capteur_04$Date_Heure, "%H:%M:%S"), capteur_04$ROUGE_CLAIR, col = "blue", pch = 19)

bozo = c("Incident", "F. excelsior", "A. cordata", "M. alba")
couleurs = c("black", "red", "green", "blue")
legend(x = "topright", legend = bozo, fill = couleurs, bty != 'n', bg = "white")

plot(strptime(capteur_02$Date_Heure, "%H:%M:%S"), capteur_02$ROUGE_CLAIR,
     col = capteur_02$Ray, pch = 19,
     main = "Rouge transmis par les arbres",
     xlab = "Temps", ylab = "Rouge [600; 700]")
points(strptime(capteur_05$Date_Heure, "%H:%M:%S"), capteur_05$ROUGE_CLAIR, col = "lightgreen", pch = 19)
points(strptime(capteur_06$Date_Heure, "%H:%M:%S"), capteur_06$ROUGE_CLAIR, col = "lightblue", pch = 19)

bozo = c("Incident", "F. excelsior", "A. cordata", "M. alba")
couleurs = c("black", "red", "lightgreen", "lightblue")
legend(x = "topright", legend = bozo, fill = couleurs, bty != 'n', bg = "white")


#Graphiques infrarouge
plot(strptime(capteur_01$Date_Heure, "%H:%M:%S"), capteur_01$ROUGE_SOMBRE,
     col = capteur_01$Ray, pch = 19,
     main = "Infrarouge transmis par les arbres",
     xlab = "Temps", ylab = "Infrarouge [700; 800]")
points(strptime(capteur_03$Date_Heure, "%H:%M:%S"), capteur_03$ROUGE_SOMBRE, col = "green", pch = 19)
points(strptime(capteur_04$Date_Heure, "%H:%M:%S"), capteur_04$ROUGE_SOMBRE, col = "blue", pch = 19)

bozo = c("Incident", "F. excelsior", "A. cordata", "M. alba")
couleurs = c("black", "red", "green", "blue")
legend(x = "topright", legend = bozo, fill = couleurs, bty != 'n', bg = "white")

plot(strptime(capteur_02$Date_Heure, "%H:%M:%S"), capteur_02$ROUGE_SOMBRE,
     col = capteur_02$Ray, pch = 19,
     main = "Infrarouge transmis par les arbres",
     xlab = "Temps", ylab = "Infrarouge [700; 800]")
points(strptime(capteur_05$Date_Heure, "%H:%M:%S"), capteur_05$ROUGE_SOMBRE, col = "lightgreen", pch = 19)
points(strptime(capteur_06$Date_Heure, "%H:%M:%S"), capteur_06$ROUGE_SOMBRE, col = "lightblue", pch = 19)

bozo = c("Incident", "F. excelsior", "A. cordata", "M. alba")
couleurs = c("black", "red", "lightgreen", "lightblue")
legend(x = "topright", legend = bozo, fill = couleurs, bty != 'n', bg = "white")


#Graphique RC/RS
plot(strptime(capteur_01$Date_Heure, "%H:%M:%S"), capteur_01$RC_RS,
     col = capteur_01$Ray, pch = 19,
     main = "Rouge par infrarouge des arbres",
     xlab = "Temps", ylab = "R/IR")
points(strptime(capteur_03$Date_Heure, "%H:%M:%S"), capteur_03$RC_RS, col = "green", pch = 19)
points(strptime(capteur_04$Date_Heure, "%H:%M:%S"), capteur_04$RC_RS, col = "blue", pch = 19)

bozo = c("Incident", "F. excelsior", "A. cordata", "M. alba")
couleurs = c("black", "red", "green", "blue")
legend(x = "topright", legend = bozo, fill = couleurs, bty != 'n', bg = "white")

plot(strptime(capteur_02$Date_Heure, "%H:%M:%S"), capteur_02$RC_RS,
     col = capteur_02$Ray, pch = 19,
     main = "Rouge par infrarouge des arbres",
     xlab = "Temps", ylab = "I/IR")
points(strptime(capteur_05$Date_Heure, "%H:%M:%S"), capteur_05$RC_RS, col = "lightgreen", pch = 19)
points(strptime(capteur_06$Date_Heure, "%H:%M:%S"), capteur_06$RC_RS, col = "lightblue", pch = 19)

bozo = c("Incident", "F. excelsior", "A. cordata", "M. alba")
couleurs = c("black", "red", "lightgreen", "lightblue")
legend(x = "topright", legend = bozo, fill = couleurs, bty != 'n', bg = "white")

dev.off()


## Troisième jeu de graphiques

X11()
svg("Infrarouge_+_theta.svg", width = 15, height = 10, onefile = TRUE)

#Graphiques rouge strict
par(mfrow = c(2,4))
plot(strptime(capteur_01$Date_Heure, "%H:%M:%S"), capteur_01$RC_strict,
     col = capteur_01$Ray, pch = 19,
     main = "Rouge strict transmis par les arbres",
     xlab = "Temps", ylab = "Rouge strict [655; 665]")
points(strptime(capteur_03$Date_Heure, "%H:%M:%S"), capteur_03$RC_strict, col = "green", pch = 19)
points(strptime(capteur_04$Date_Heure, "%H:%M:%S"), capteur_04$RC_strict, col = "blue", pch = 19)

bozo = c("Incident", "F. excelsior", "A. cordata", "M. alba")
couleurs = c("black", "red", "green", "blue")
legend(x = "topright", legend = bozo, fill = couleurs, bty != 'n', bg = "white")

plot(strptime(capteur_02$Date_Heure, "%H:%M:%S"), capteur_02$RC_strict,
     col = capteur_02$Ray, pch = 19,
     main = "Rouge strict transmis par les arbres",
     xlab = "Temps", ylab = "Rouge strict [655; 665]")
points(strptime(capteur_05$Date_Heure, "%H:%M:%S"), capteur_05$RC_strict, col = "lightgreen", pch = 19)
points(strptime(capteur_06$Date_Heure, "%H:%M:%S"), capteur_06$RC_strict, col = "lightblue", pch = 19)

bozo = c("Incident", "F. excelsior", "A. cordata", "M. alba")
couleurs = c("black", "red", "lightgreen", "lightblue")
legend(x = "topright", legend = bozo, fill = couleurs, bty != 'n', bg = "white")


#Graphiques infrarouge réduit
plot(strptime(capteur_01$Date_Heure, "%H:%M:%S"), capteur_01$RS_strict,
     col = capteur_01$Ray, pch = 19,
     main = "Infrarouge réduit transmis par les arbres",
     xlab = "Temps", ylab = "Infrarouge réduit [725; 735]")
points(strptime(capteur_03$Date_Heure, "%H:%M:%S"), capteur_03$RS_strict, col = "green", pch = 19)
points(strptime(capteur_04$Date_Heure, "%H:%M:%S"), capteur_04$RS_strict, col = "blue", pch = 19)

bozo = c("Incident", "F. excelsior", "A. cordata", "M. alba")
couleurs = c("black", "red", "green", "blue")
legend(x = "topright", legend = bozo, fill = couleurs, bty != 'n', bg = "white")

plot(strptime(capteur_02$Date_Heure, "%H:%M:%S"), capteur_02$RS_strict,
     col = capteur_02$Ray, pch = 19,
     main = "Infrarouge réduit transmis par les arbres",
     xlab = "Temps", ylab = "Infrarouge réduit [725; 735]")
points(strptime(capteur_05$Date_Heure, "%H:%M:%S"), capteur_05$RS_strict, col = "lightgreen", pch = 19)
points(strptime(capteur_06$Date_Heure, "%H:%M:%S"), capteur_06$RS_strict, col = "lightblue", pch = 19)

bozo = c("Incident", "F. excelsior", "A. cordata", "M. alba")
couleurs = c("black", "red", "lightgreen", "lightblue")
legend(x = "topright", legend = bozo, fill = couleurs, bty != 'n', bg = "white")


#Graphiques ZETA
plot(strptime(capteur_01$Date_Heure, "%H:%M:%S"), capteur_01$ZETA,
     col = capteur_01$Ray, pch = 19,
     main = "ZETA des arbres",
     xlab = "Temps", ylab = "ZETA")
points(strptime(capteur_03$Date_Heure, "%H:%M:%S"), capteur_03$ZETA, col = "green", pch = 19)
points(strptime(capteur_04$Date_Heure, "%H:%M:%S"), capteur_04$ZETA, col = "blue", pch = 19)

bozo = c("Incident", "F. excelsior", "A. cordata", "M. alba")
couleurs = c("black", "red", "green", "blue")
legend(x = "topright", legend = bozo, fill = couleurs, bty != 'n', bg = "white")

plot(strptime(capteur_02$Date_Heure, "%H:%M:%S"), capteur_02$ZETA,
     col = capteur_02$Ray, pch = 19,
     main = "ZETA des arbres",
     xlab = "Temps", ylab = "ZETA")
points(strptime(capteur_05$Date_Heure, "%H:%M:%S"), capteur_05$ZETA, col = "lightgreen", pch = 19)
points(strptime(capteur_06$Date_Heure, "%H:%M:%S"), capteur_06$ZETA, col = "lightblue", pch = 19)

bozo = c("Incident", "F. excelsior", "A. cordata", "M. alba")
couleurs = c("black", "red", "lightgreen", "lightblue")
legend(x = "topright", legend = bozo, fill = couleurs, bty != 'n', bg = "white")


#Graphiques théta
plot(strptime(capteur_01$Date_Heure, "%H:%M:%S"), capteur_01$THETA,
     col = capteur_01$Ray, pch = 19,
     main = "Théta des arbres",
     xlab = "Temps", ylab = "Théta")
points(strptime(capteur_03$Date_Heure, "%H:%M:%S"), capteur_03$THETA, col = "green", pch = 19)
points(strptime(capteur_04$Date_Heure, "%H:%M:%S"), capteur_04$THETA, col = "blue", pch = 19)

bozo = c("Incident", "F. excelsior", "A. cordata", "M. alba")
couleurs = c("black", "red", "green", "blue")
legend(x = "topright", legend = bozo, fill = couleurs, bty != 'n', bg = "white")

plot(strptime(capteur_02$Date_Heure, "%H:%M:%S"), capteur_02$THETA,
     col = capteur_02$Ray, pch = 19,
     main = "Théta des arbres",
     xlab = "Temps", ylab = "Théta")
points(strptime(capteur_05$Date_Heure, "%H:%M:%S"), capteur_05$THETA, col = "lightgreen", pch = 19)
points(strptime(capteur_06$Date_Heure, "%H:%M:%S"), capteur_06$THETA, col = "lightblue", pch = 19)

bozo = c("Incident", "F. excelsior", "A. cordata", "M. alba")
couleurs = c("black", "red", "lightgreen", "lightblue")
legend(x = "topright", legend = bozo, fill = couleurs, bty != 'n', bg = "white")

dev.off()

