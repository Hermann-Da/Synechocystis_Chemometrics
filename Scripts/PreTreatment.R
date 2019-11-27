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

Data <- My.Import(Location = "./Raw_data/191030_InSitu_PHB_192",Gruppen = c("A","B")) # Eindeutige Bennenung, in den File Namen vorhanden
levels(Data$Groups) <- c("0.805","8.558","8.596")
# Data <- My.Import(Location = "./Raw_data/6803",Gruppen = c("6A","6B","6C","6D"))
Data$PHB <- read.csv("./Raw_data/191030_InSitu_PHB_192/PHB_Mean_0.TXT",
                     sep = ",", dec = ".", header = FALSE)
Data$PHB <- as.numeric(Data$PHB[,2])
### Ersetzen der Namen der Levels für eine Publikation
levels(Data$Groups) <- c("A","B","C")

# Welche Vorbehandlungen gibt es
Type <- c("Spectra","Spectra_b", "Spectra_d", "Spectra_SNV", "Spectra_n", "Spectra_norm" )

#########################
### Festlegen der Default Darstellungsgrenzen und plotten der Originaspektren
#########################

# Was wird dargestellt
Range <- c(max(Data$Wavenumber),min(Data$Wavenumber))
# Welche Wellenzahlen werden ausgewählt
Sample <- c(1:length(Data$Wavenumber))

# My.export_start("Spectra_Original")

plot.spectra(Liste = Data,Spektren = "Spectra", area = Range)

# Legende dazugeben, Über ersten parameter die Position verändern

legend("topleft", legend=unique(Data$Groups), pch=16, col=unique(Data$Groups), inset = 0.05, bty = "n")
 abline(v = 250, lty = "dashed")
 abline(v = 100, lty = "dashed")

# My.export_end()

#########################
### Normierung
#########################
# Position = Wellenzahl an der man normieren will

Data$Spectra_norm <- Spec_norm(List = Data, Spektren = Type[1], Position = c(100,250))

#Data$Spectra_norm <- msc(as.matrix(Data$Spectra_b))

#########################
### Baseline-correction
#########################
# Degree = Ordnung des zur Korrekutr verwendeten Polynoms

Baseline <- baseline(spectra = as.matrix(Data[[Type[Select]]]),method = 'modpolyfit',degree = 1)
# plot(Baseline)
# Extrahierung der korrigierten Spektren
Data$Spectra_b <- as.data.frame(getCorrected(Baseline))
# Baseline wieder entfernen
rm(Baseline)

#############################################
#1.Ableitung & smoothing
#############################################
# m = Wievielte Ableitung 
# w = Intervall über dem abgeleitet wird
# s = Intervall über dem das Smoothing durchgeführt wird

Data$Spectra_d <- as.data.frame(gapDer(DData[[Type[Select]]][,Sample], m = 1,
                                 w = 5, s = 3))
# Wavenumber auf selbe Länge wie bei den abgeleiteten Spektren bringen 
# Bei 2. Ableitung, doppelte Anzahl entfernen und so weiter

# Data$Wavenumber_d <- Data$Wavenumber[-c(1:5,length(Data$Wavenumber)-(0:4))]

# Sample anpassen 
# Sample_d <- c(1:length(Data$Wavenumber_d))

#############################################
#Spektren vorbehandlung mit Standardnormalvarianz
#############################################
# Data$Spectra_b = basislinien korrigierte Spektren vorbehandeln
# Data$Spectra   = Originalspektren vorbehandeln

Data$Spectra_SNV <- standardNormalVariate(Data$Spectra_b[Sample])

#############################################
#Spektren vorbehandlung mit Detrend (Smoothing und Polynom 2. Ordnung fitting)
#############################################

Data$Spectra_n <- detrend(Data$Spectra[,Sample], wav = Data$Wavenumber[Sample])
Data$PHB_n <- detrend(Data$PHB[Sample], wav = Data$Wavenumber[Sample])

Data$Wavenumber_SNV <- as.numeric(colnames(Data$Spectra_n))
#############################################
# Spektren plotten
#############################################

Type <- c("Spectra","Spectra_b", "Spectra_d", "Spectra_SNV", "Spectra_n", "Spectra_norm" )

# Welche werden ausgewählt
Select <- 5

# My.export_start(Type[Select])

plot.spectra(Data, Type[Select],area = Range)
legend("topleft", legend=levels(Data$Groups[!is.na(Data$Groups)]), pch=16, 
       col=unique(Data$Groups[!is.na(Data$Groups)]), inset = 0.001, bty = "n",
       horiz = FALSE)
lines(x = as.numeric(colnames(Data$Spectra_n)), y= Data$PHB_n, col = "royalblue")
 # abline(v = 900, lty = "dashed") # Darstellen welche Bereiche zur PCA verwendet wurden
 # abline(v = 1280, lty = "dashed")
 # arrows(x0 = c(1515,1152,1005,955,875), # Mit Pfeilen wichtige Peaks vermerken
 #        x1 = c(1515,1152,1005,955,875),
 #        y0 = c(-0.6,-0.6,-1.1,1.1,0.8), y1 = c(0.0,0.0,-0.5,0.5,0.2),
 #        code = 2, col = "darkgreen", lwd = 1.3, length = 0.1) #code = 2 -> pfeil an y1
# My.export_end()



