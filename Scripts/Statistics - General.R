#############################################
# Chose pretreated spectra
#############################################

# Which preTreatment are available
Type <- c("Spectra","Spectra_norm" , "Spectra_b", "Spectra_d", "Spectra_SNV", "Spectra_n")
# Which are chosen
# Stats <- 5

# If the complete area is used
# Sample <- 1:length(colnames(Data[[Type[Stats]]]))

#########################
# PCA 
#########################
# Scale = TRUE, Correlation matrix is used
# Scale = FALSE, Covarianz matrix is used 


PCA <- prcomp(Data[[Type[Stats]]][!is.na(Data$Groups),Sample],
                  center = TRUE,rank.=5, scale. = FALSE )

# Remove NAs to display correct colours
PCA$Groups <- Data$Groups[!is.na(Data$Groups)]

summary(PCA)

######################
# Explaine Variance per PC
#####################

# Store Variance
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
# Plot loadings
#######################

# My.export_start(paste("Loadings_PC2",Type[Stats], sep = " "))

plot.load()

# My.export_end()

#####################
# Plot scores of first 4 Components, general overview
#####################


#My.export_start(paste("Scores_ges",Type[Stats], sep = " "))

pairs(PCA$x[,1:4],col=Data$Groups[!is.na(Data$Groups)], pch = 19)
legend("bottomright", legend=levels(Data$Groups[!is.na(Data$Groups)]), pch=19, 
       col=unique(Data$Groups[!is.na(Data$Groups)]), inset = 0.03,
       horiz = FALSE, bty = "n")

#My.export_end()

#####################
# Plot specific PCs
####################
# Vector specifying the forms used during the plotting
Form <- c(15,20,17,18,3,4,6,8)

# Which are to be plotted
PC <- c(1,2)

# My.export_start(paste("Scores_1_2",Type[Stats], sep = " "))
plot.scores(PCs = PC)

legend("topleft", legend=levels(PCA$Groups),
       pch=Form[1:length(levels(PCA$Groups))], col=unique(PCA$Groups), 
       inset = 0.01, bty = "n", cex = 1.1)
# My.export_end()

###################################
# Identify outliers
###################################
# Decomment and run, returns locators of selected points

# Outlier = identify( x = PCA$x[,PC[1]], y = PCA$x[,PC[2]])


###################################
# Visualization of Scores with Boxplots
###################################

# Which Component should be used
Choice <- 1

# My.export_start(paste("Boxplot_PC2_Publ",Type[Stats], sep = " "))

boxplot(PCA$x[,Choice] ~ PCA$Groups, 
        xlab = paste("Cultures,","n=",floor(length(Data$Groups)/length(levels(Data$Groups))), sep = " "),
        ylab = paste("Scores of","PC" ,Choice, "[",round(Data$Variance[2]*100,2),"%]", sep = " "), 
          cex.lab = 1.2, cex.axis = 1.2, yaxt = "n", font.lab = 2)
abline(h = seq(from = round(min(PCA$x[,Choice])), to = round(max(PCA$x[,Choice])), by = 2), col = "darkgrey", lty = "dashed")
abline(h = 0, col = "black", lty = "solid")
axis(2,at = seq(from = round(min(PCA$x[,Choice])), to = round(max(PCA$x[,Choice])),by = 2), cex.axis = 1.2)
# My.export_end()

###################################
# Univariant statistics
###################################

#######
# If variance homogenity is TRUE
######
# ANOVA to test for general signigicance
# summary(aov(PCA$x[,2]~ PCA$Groups))

# Test for statistical differences 
# TukeyHSD(aov(PCA$x[,2]~ PCA$Groups))

#######
# If variance homogenity is not given or FALSE
######

# Kruskal wallis test for general significance
print(kruskal.test(PCA$x[,Choice]~ PCA$Groups))
# Games-Howell Algorithm tests for differences between each group

# Levene to test for variance homogenity
Games <- posthocTGH(y = PCA$x[,Choice], x = PCA$Groups, method = "games-howell")
print(Games)
rm(Games)

###################################
# Hierarchical clustering Analysis
###################################

#My.export_start("paste("HCA",Type[Stats], sep = " ")")


HCA(Data[[Type[Stats]]][!is.na(Data$Groups),],Data$Groups[!is.na(Data$Groups)])
legend("topright", legend=levels(PCA$Groups), 
       pch=16, col=unique(PCA$Groups), inset = 0.01, bty = "n")

#My.export_end()

###################################
# PCA and Hierarchical clustering Analysis of the score values
###################################

#My.export_start(paste("PCA_HCA",Type[Stats], sep = " "))

HCA(Data = PCA$x[,Choice],Groups = PCA$Groups)
legend("topright", legend=levels(PCA$Groups), 
       pch=16, col=unique(PCA$Groups), inset = 0.01, bty = "n")
#My.export_end()
