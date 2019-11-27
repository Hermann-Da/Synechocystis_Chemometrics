###################################
### Loadings und Mittelwert Spectra plotten
###################################
# Bei "_d":
# Sample <- Sample_d

# Mittelwerte Berechnen, für mehrere Gruppen kopieren und A, B austauschen

for (x in 1:length(levels(Data$Groups))) {
  Data[[levels(Data$Groups)[x]]] <- apply(Data[[Type[Select]]][which(Data$Groups== levels(Data$Groups)[x]),], 
                                      2, FUN = mean)  
}


# X-Achse bestimmen, _d bei Ableitung
X_Variab = Wavelength

# My.export_start("InSitu_Mean")
plot(NULL,
     type = "l", xlab = "Wavenumber [1/cm]", # Keine Achsenbeschriftung, weil nur relativ zueinander wichtig ist. 
     font = 2,font.lab = 2, 
     ylab = "Relative Intensity [-] ",                # Außerdem sind es verschiedene Einheiten
     lab = c(20,15,10), 
     ylim = c(-0.9,2.6),
     xlim = Range, xaxs = "i", bty = "l")
abline(h = 0, lwd = 0.8, lty = "dashed")
grid(lwd = 0.8)
for (x in 1:length(levels(Data$Groups))) {
        lines(x= X_Variab, y =  Data[[levels(Data$Groups)[x]]], 
              col = unique(Data$Groups[!is.na(Data$Groups)])[x])
}
# lines(x = as.numeric(colnames(Data$Spectra_n)), y= (Data$PHB_n/4)-2.2, col = "royalblue")
lines(x= X_Variab[Sample], y = PCA$rotation[,2]*20, col = "royalblue", lty = "dashed")
legend(x = 1500, y = 2.7, legend=c(levels(Data$Groups), 
                                   paste("PC 2"," ","[",round(Data$Variance[2]*100,2),"%]", sep = "")),  
       col=c(unique(Data$Groups[!is.na(Data$Groups)]),"royalblue"), inset = 0.0001, bty = "n",
       horiz = FALSE, lty = c("solid","solid","solid","dashed"))
# My.export_end()

###################################
### Scores mit verschiedenen Symbolen
###################################
# Bei Points "pch" ändern für andere Symbole, siehe "points" EIntrag. 
# 
# Form <- c(15,20,17,18,3,4,6,8)
# 
# # My.export_start("Scores_Refined")
# plot(NULL,xlab = paste("Scores of", "PC",X_Ax, "[",round(Data$Variance[X_Ax]*100,1),"%]", sep = " "),
#      ylab = paste("Scores of","PC" ,Y_Ax, "[",round(Data$Variance[Y_Ax]*100,1),"%]", sep = " "),
#      col = Data$Groups[!is.na(Data$Groups)], lab = c(10,10,10),
#      font.lab = 2, font = 2, xlim = c(min(PCA$x[,X_Ax]),max(PCA$x[,X_Ax])),
#      ylim = c(min(PCA$x[,Y_Ax]),max(PCA$x[,Y_Ax]))
# )
# grid(lwd = 0.8)
# abline(h = 0, v = 0)
# for(g in 1:length(levels(PCA$Groups))){
#   points(x = PCA$x[which(PCA$Groups == levels(PCA$Groups)[g]),X_Ax],
#          y = PCA$x[which(PCA$Groups == levels(PCA$Groups)[g]),Y_Ax],
#          pch = Form[g], col = unique(PCA$Groups)[g])
# }
# legend("topleft", legend=levels(PCA$Groups),
#        pch=Form[1:length(levels(PCA$Groups))], col=unique(PCA$Groups), 
#        inset = 0.01, bty = "n", cex = 1.1)
# # My.export_end()

