###################################
### Plot loadings and median spectra
###################################
Type <- c("Spectra","Spectra_norm" , "Spectra_b", "Spectra_d", "Spectra_SNV", "Spectra_n")

# Find median spectrum for each group

for (x in 1:length(levels(Data$Groups))) {
  Data[[levels(Data$Groups)[x]]] <- apply(Data[[Type[Stats]]][which(Data$Groups== levels(Data$Groups)[x]),], 
                                      2, FUN = mean)  
}


# Define X-axis (used wavelength area)
X_Variab = colnames(Data[[Type[Stats]]])

###
# Plot
###

# My.export_start("InSitu_Mean")
plot(NULL,
     type = "l", xlab = "Wavenumber [1/cm]", # No Y-axis labels, since values are only relative
     font = 2,font.lab = 2, 
     ylab = "Relative Intensity [-] ",                
     lab = c(20,15,10), 
     ylim = c(min(Data[[Type[Stats]]]),max(Data[[Type[Stats]]])), # adjust for use
     xlim = Range, xaxs = "i", bty = "l")
abline(h = 0, lwd = 0.8, lty = "dashed")
grid(lwd = 0.8)
for (x in 1:length(levels(Data$Groups))) {
        lines(x= X_Variab, y =  Data[[levels(Data$Groups)[x]]], 
              col = unique(Data$Groups[!is.na(Data$Groups)])[x])
}
# Copy and change PC
# lines(x = as.numeric(colnames(Data$Spectra_n)), y= (Data$PHB_n/4)-2.2, col = "royalblue")
lines(x= X_Variab[Sample], y = PCA$rotation[,1]*10, col = "royalblue", lty = "dashed")

legend("topright", legend=c(levels(Data$Groups), 
                                   paste("PC 1"," ","[",round(Data$Variance[2]*100,2),"%]", sep = "")),  
       col=c(unique(Data$Groups[!is.na(Data$Groups)]),"royalblue"), inset = 0.0001, bty = "n",
       horiz = FALSE, lty = c("solid","solid","dashed")) # If more different lines are used append vector
# My.export_end()
