###################
# Documentation of changes
###################
# Version number is also lodged in the export function
# If changes are made, copy the marked areas a
# Then change the version number


##############################################################################################
# Start copying 

Version <- "_001"

##################
# Restrict used spectral area
##################

# Which treatments are available
Type <- c("Spectra","Spectra_norm" , "Spectra_b", "Spectra_d", "Spectra_SNV", "Spectra_n")

# Which treatment is chosen
Stats <- 6

Spectral_range <- as.numeric(colnames(Data[[Type[Stats]]]))

# Set limits, if more a needed, append and modify Sample

Limit1 <- which(Spectral_range > 1700) # Upper Limit
Limit2 <- which(Spectral_range > 700)  # lower Limit

Sample <- c(min(Limit1):min(Limit2))

# Hier sind nur die maximal und minimal Werte gefragt
Range <- c(Spectral_range[min(Limit1)],Spectral_range[min(Limit2)])


##################
# Removal of outlier
##################

# Identification of outlier with the identify function in the score plots
# For a better traceability, removals are noted here:

Outlier <- c(22,30)
Data$Groups[Outlier] <- NA # Data$Groups are linked, NA values are not considered for evaluation

# End copying
##############################################################################################
