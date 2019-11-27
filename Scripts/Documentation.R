###################
#Dokumentationsverlauf
###################
# Versionsnummer ist in der Export Funktion hinterlegt
# Wenn Änderungen durchgeführt werden -> Kopieren und ändern
# Dann Versionsnummer ändern


##############################################################################################
# Kopieren Anfang

Version <- "_001"

##################
# Spektralen Bereich einschränken 
##################

# Mit SNV oder detrend: 
## Wenn mehr Bereiche gewünscht sind, mehr Limits setzen und Sample anpassen

Limit1 <- which(Data$Wavenumber > 1800) # oberes Limit
Limit2 <- which(Data$Wavenumber < 400)  # Unteres Limit

Sample <- c(min(Limit1):max(Limit2))

# Hier sind nur die maximal und minimal Werte gefragt
Range <- c(Data$Wavenumber[min(Limit1)],Data$Wavenumber[max(Limit2)])

# Mit Ableitung

# Limit1 <- which(Data$Wavenumber_d > 1700)
# Limit2 <- which(Data$Wavenumber_d < 400)
# 
# Range <- c(Data$Wavenumber_d[min(Limit1)],Data$Wavenumber_d[max(Limit2)])
# 
# Sample_d <- c(min(Limit1):max(Limit2))

##################
# Entfernen von Ausreißern
##################

# Ausreißer mit Identify akzeptieren
# Hier notieren, damit die Modifikation nachvollziehbar ist 

#Ausreißer <- c()
#Data$Groups[Ausreißer] <- NA # Data$Groups ist verlinkt, NA Werte werden ausgelassen

# Kopieren Ende
##############################################################################################

##############################################################################################
# Kopieren Anfang

Version <- "_002"

##################
# Spektralen Bereich einschränken 
##################

Wavelength <- as.numeric(colnames(Data[[Type[Stats]]]))

# Mit SNV oder detrend: 
# 
  Limit1 <- which(Wavelength < 1150) # oberes Limit
#  Limit2 <- which(Wavelength < 400)  # Unteres Limit
# # 
  Sample <- Limit1
# # 
# # # Hier sind nur die maximal und minimal Werte gefragt
  Range <- c(Wavelength[min(Limit1)],Wavelength[max(Limit1)])
# # 
# # Mit Ableitung

# Mit Ableitung

# Limit1 <- which(Data$Wavenumber_d > 1700)
# Limit2 <- which(Data$Wavenumber_d < 400)
# 
# Range <- c(Data$Wavenumber_d[min(Limit1)],Data$Wavenumber_d[max(Limit2)])
# 
# Sample_d <- c(min(Limit1):max(Limit2))

##################
# Entfernen von Ausreißern
##################

# Ausreißer mit Identify akzeptieren
# Hier notieren, damit die Modifikation nachvollziehbar ist 

Ausreißer <- c(86)
Data$Groups[Ausreißer] <- NA # Data$Groups ist verlinkt, NA Werte werden ausgelassen

# Kopieren Ende
##############################################################################################

##############################################################################################
# Kopieren Anfang

Version <- "_003"

##################
# Spektralen Bereich einschränken 
##################

Wavelength <- as.numeric(colnames(Data[[Type[Stats]]]))
# Mit SNV oder detrend: 
## Wenn mehr Bereiche gewünscht sind, mehr Limits setzen und Sample anpassen

Limit1 <- which(Wavelength < 800) # oberes Limit
Limit2 <- which(Wavelength < 600)  # Unteres Limit

Sample <- c(min(Limit1):min(Limit2))

# Hier sind nur die maximal und minimal Werte gefragt
Range <- c(Wavelength[min(Limit1)],Wavelength[min(Limit2)])

# Mit Ableitung

# Limit1 <- which(Data$Wavenumber_d > 1700)
# Limit2 <- which(Data$Wavenumber_d < 400)
# 
# Range <- c(Data$Wavenumber_d[min(Limit1)],Data$Wavenumber_d[max(Limit2)])
# 
# Sample_d <- c(min(Limit1):max(Limit2))

##################
# Entfernen von Ausreißern
##################

# Ausreißer mit Identify akzeptieren
# Hier notieren, damit die Modifikation nachvollziehbar ist 

Ausreißer <- c(14,86)
Data$Groups[Ausreißer] <- NA # Data$Groups ist verlinkt, NA Werte werden ausgelassen

# Kopieren Ende
##############################################################################################

##############################################################################################
# Kopieren Anfang

Version <- "_004"

##################
# Spektralen Bereich einschränken 
##################

Wavelength <- as.numeric(colnames(Data[[Type[Stats]]]))

# Mit SNV oder detrend: 
# 
Limit1 <- which(Wavelength > 550) # oberes Limit
Limit2 <- which(Wavelength > 1450)  # Unteres Limit
# # 
Sample <- c(max(Limit1):max(Limit2))
# # 
# # # Hier sind nur die maximal und minimal Werte gefragt
Range <- c(Wavelength[max(Limit2)],Wavelength[max(Limit1)])
# # 
# # Mit Ableitung

# Mit Ableitung

# Limit1 <- which(Data$Wavenumber_d > 1700)
# Limit2 <- which(Data$Wavenumber_d < 400)
# 
# Range <- c(Data$Wavenumber_d[min(Limit1)],Data$Wavenumber_d[max(Limit2)])
# 
# Sample_d <- c(min(Limit1):max(Limit2))

##################
# Entfernen von Ausreißern
##################

# Ausreißer mit Identify akzeptieren
# Hier notieren, damit die Modifikation nachvollziehbar ist 

Ausreißer <- c(14,23)
Data$Groups[Ausreißer] <- NA # Data$Groups ist verlinkt, NA Werte werden ausgelassen

# Kopieren Ende
##############################################################################################

##############################################################################################
# Kopieren Anfang

Version <- "_005"

##################
# Spektralen Bereich einschränken 
##################

Wavelength <- as.numeric(colnames(Data[[Type[Stats]]]))

# Mit SNV oder detrend: 
# 
Limit1 <- which(Wavelength > 550) # oberes Limit
Limit2 <- which(Wavelength > 1500)  # Unteres Limit
# # 
Sample <- c(max(Limit1):max(Limit2))
# # 
# # # Hier sind nur die maximal und minimal Werte gefragt
Range <- c(Wavelength[max(Limit2)],Wavelength[max(Limit1)])
# # 
# # Mit Ableitung

# Mit Ableitung

# Limit1 <- which(Data$Wavenumber_d > 1700)
# Limit2 <- which(Data$Wavenumber_d < 400)
# 
# Range <- c(Data$Wavenumber_d[min(Limit1)],Data$Wavenumber_d[max(Limit2)])
# 
# Sample_d <- c(min(Limit1):max(Limit2))

##################
# Entfernen von Ausreißern
##################

# Ausreißer mit Identify akzeptieren
# Hier notieren, damit die Modifikation nachvollziehbar ist 

Ausreißer <- c(14,23)
Data$Groups[Ausreißer] <- NA # Data$Groups ist verlinkt, NA Werte werden ausgelassen

# Kopieren Ende
##############################################################################################
