#############################################
# Vorbehandlung auswählen
#############################################

# Welche Vorbehandlungen gibt es
Type <- c("Spectra","Spectra_b", "Spectra_d", "Spectra_SNV", "Spectra_n", "Spectra_norm" )
# Welche werden ausgewählt
Stats <- 5

# Wenn die abgeleiteten Spektren verwendet werden
# Sample <- Sample_d

#########################
# PCA 
#########################
# Scale = TRUE, damit wird die Korrelationsmatrix verwendet
# Scale = FALSE, damit wird die Kovarianzmatrix verwendet


PCA <- prcomp(Data[[Type[Stats]]][!is.na(Data$Groups),Sample],
                  center = TRUE,rank.=5, scale. = FALSE )

# Damit "NAs" entfernen, um die Farben richtig darzustellen
PCA$Groups <- Data$Groups[!is.na(Data$Groups)]

summary(PCA)

######################
# Erklärte Varianz pro Principal Component
#####################

# Varianz ausgeben
Data$Variance <- PCA$sdev^2/sum(PCA$sdev^2)


#My.export_start(paste("Variance",Type[Stats], sep = " "))

plot(x= 1:length(Data$Variance), y = cumsum(Data$Variance[1:length(Data$Variance)]*100),
     type = "b", xlab = "Number of Principal Components",
     ylab = "Explained Variance [%]",
     col = "dimgrey", pch = 21, bg = "darkgrey",
     font = 2, font.lab = 2,lab = c(10,20,20),
     xlim = c(1,20))
grid(lwd = 0.8)
#My.export_end()


########################
# Loadings ausgeben
#######################

# My.export_start(paste("Loadings_PC2",Type[Stats], sep = " "))

plot.load()

# My.export_end()

#####################
# Scorewerte plotten, überblicksmäßig
#####################


#My.export_start(paste("Scores_ges",Type[Stats], sep = " "))

pairs(PCA$x[,1:4],col=Data$Groups[!is.na(Data$Groups)], pch = 19)
legend("bottomright", legend=levels(Data$Groups[!is.na(Data$Groups)]), pch=19, 
       col=unique(Data$Groups[!is.na(Data$Groups)]), inset = 0.03,
       horiz = FALSE, bty = "n")

#My.export_end()

#####################
# Plotten der ScoreWerte einzelner PCA's
####################

PC <- c(1,2)
Choice <- 2
# My.export_start(paste("Scores_1_2",Type[Stats], sep = " "))
plot.scores(PCs = PC)

legend("topleft", legend=levels(PCA$Groups),
       pch=Form[1:length(levels(PCA$Groups))], col=unique(PCA$Groups), 
       inset = 0.01, bty = "n", cex = 1.1)
# My.export_end()

###################################
# Ausreißer identifizieren mit identify
###################################
# Entkommentieren und ausführen, gibt Indexzes der gewählten Punkte aus

# Ausreißer = identify( x = PCA$x[,X_Ax], y = PCA$x[,Y_Ax])


###################################
# Visualization of Scores with Boxplots
###################################
# Stichprobenumfang einbauen

# My.export_start(paste("Boxplot_PC2_Publ",Type[Stats], sep = " "))

boxplot(PCA$x[,PC[Choice]] ~ PCA$Groups, 
        xlab = paste("Cultures,","n=",floor(length(Data$Groups)/length(levels(Data$Groups))), sep = " "),
        ylab = paste("Scores of","PC" ,Choice, "[",round(Data$Variance[PC[Choice]]*100,2),"%]", sep = " "), 
          cex.lab = 1.2, cex.axis = 1.2, yaxt = "n", font.lab = 2)
abline(h = seq(from = round(min(PCA$x[,PC[Choice]])), to = round(max(PCA$x[,PC[Choice]])), by = 2), col = "darkgrey", lty = "dashed")
abline(h = 0, col = "black", lty = "solid")
axis(2,at = seq(from = round(min(PCA$x[,PC[Choice]])), to = round(max(PCA$x[,PC[Choice]])),by = 2), cex.axis = 1.2)
# My.export_end()

###################################
# Univariant statistics
###################################

# # ANOVA to test for general signigicance
# summary(aov(PCA$x[,2]~ PCA$Groups))
# # Test for statistical differences 
# TukeyHSD(aov(PCA$x[,2]~ PCA$Groups))

# Kruskal wallis test for general significance
print(kruskal.test(PCA$x[,Y_Ax]~ PCA$Groups))
# Games-Howell Algorithm for each group
# Levene test um diew Variance homogentity zu überprüfen
Games <- posthocTGH(y = PCA$x[,Y_Ax], x = PCA$Groups, method = "games-howell")
print(Games)
rm(Games)
###################################
# Hierarchical clustering Analysis
###################################

#My.export_start("paste("HCA",Type[Stats], sep = " ")")


HCA(Data[[Type[Stats]]][!is.na(Data$Groups),],Data$Groups[!is.na(Data$Groups)])
legend("topright", legend=levels(Data$Groups[!is.na(Data$Groups)]), 
       pch=16, col=unique(Data$Groups[!is.na(Data$Groups)]), inset = 0.01, bty = "n")

#My.export_end()

###################################
# PCA und Hierarchical clustering Analysis
###################################

#My.export_start(paste("PCA_HCA",Type[Stats], sep = " "))

HCA(Data = PCA$x[,2:3],Groups = Data$Groups[!is.na(Data$Groups)])
legend("topright", legend=levels(Data$Groups[!is.na(Data$Groups)]), 
       pch=16, col=unique(Data$Groups[!is.na(Data$Groups)]), inset = 0.01, bty = "n")
#My.export_end()
