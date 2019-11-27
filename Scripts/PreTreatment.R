#########################
### Load Functions
#########################
source(file = "./Scripts/Functions.R") # Funktionsfile ausführen um Packages und Funktionen zu laden


################

Project <- "_Git_Hub_" # Um welche Daten handelt es sich, anpassbar
Version <- 000            # Versionsnummer für die Nachverfolgung, nicht verändern


#########################
### Import
#########################
# Gruppen <- string vector of the used groups
# must be part of the file name and be excplicit

Data <- My.Import(Location = "./Raw_data",Gruppen = c("A","B")) 

# Replace the names of the levels, for example for a publication
levels(Data$Groups) <- c("Strain A", "Strain B")

# Which preTreatment are available
Type <- c("Spectra","Spectra_b", "Spectra_d", "Spectra_SNV", "Spectra_n", "Spectra_norm" )

#########################
### Setting of the visualization parameter
#########################

# What is shown
Range <- c(max(Data$Wavenumber),min(Data$Wavenumber))
# Which wavennumbers are chosen
Sample <- c(1:length(Data$Wavenumber))

#########################
### Plot Original spectra
#########################

# My.export_start("Spectra_Original")

plot.spectra(Liste = Data,Spektren = "Spectra", area = Range)

# add legend, adjust position by modifying the first parameter

legend("topright", legend=unique(Data$Groups), pch=16, col=unique(Data$Groups), inset = 0.05, bty = "n")

# My.export_end()

#########################
### Normalizing
#########################
# Position = Wavenumber or area used as reference for the normalization
# Spektren <- which pretreated spectra are used 

Data$Spectra_norm <- Spec_norm(List = Data, Spektren = Type[1], Position = c(1550,1500))


#########################
### Baseline-correction
#########################
# Degree = order of the used polynomial 

Select <- 1

Baseline <- baseline(spectra = as.matrix(Data[[Type[Select]]]),method = 'modpolyfit',degree = 1)
# plot(Baseline)
# Extraction of the corrected spectra
Data$Spectra_b <- as.data.frame(getCorrected(Baseline))
colnames(Data$Spectra_b) <- colnames(Data$Spectra)
# Baseline wieder entfernen
rm(Baseline)

#############################################
# Derivative and smoothing (SyitzkyGolay)
#############################################
# m = Order of the derivative
# p = order of the used polynomial 
# w = Window used for the derivative
# delta.wav = optional vector used for smoothing

Data$Spectra_d <- as.data.frame(savitzkyGolay(Data[[Type[Select]]][,Sample], m = 1, p = 2, w = 5, delta.wav = 5))

#############################################
# SNV - Transformation
#############################################

Data$Spectra_SNV <- standardNormalVariate(Data[[Type[Select]]][,Sample])

#############################################
# detrend = Fitting a 2nd order polynomial and applying a SNV correction
#############################################

Data$Spectra_n <- detrend(Data[[Type[Select]]][,Sample], 
                          wav = as.numeric(colnames(Data[[Type[Select]]][,Sample])))

#############################################
# Plot differenly treated spectra
#############################################

# Which treatments are available
Type <- c("Spectra","Spectra_norm" , "Spectra_b", "Spectra_d", "Spectra_SNV", "Spectra_n")

# Which treatment is chosen
Select <- 6

# My.export_start(Type[Select])

plot.spectra(Data, Type[Select],area = Range)

legend("topright", legend=levels(Data$Groups[!is.na(Data$Groups)]), lty = "solid", 
       col=unique(Data$Groups[!is.na(Data$Groups)]), inset = 0.001, bty = "n",
       horiz = FALSE)
# My.export_end()



