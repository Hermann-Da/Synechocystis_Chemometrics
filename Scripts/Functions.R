###############
##Load Packages
##############
# Require function to check if the packages are installed
# Packages <- lsit of the required packages
  # If needed append 
Packages <- c("prospectr","dendextend","baseline","pls","userfriendlyscience")

for (p in 1:length(Packages)) {
  test <- require(Packages[p], character.only = TRUE) #loads package or exports false 
  if(test == FALSE){
    install.packages(Packages[p]) #installiert Packages wenn nicht vorhanden
    library(Packages[p], character.only = TRUE)
  }else{
    library(Packages[p],character.only = TRUE)
  }
}

# Remove variables to keep the environment clear
rm(test) 
rm(Packages)
rm(p)


#########################
##Import Function
#########################
# Location: Path, where the TXT.files are located
# Gruppen: string vector of the used groups, must be part of file name and be explicit
# A maximum of 8 groups is possible based on the available colours

My.Import <- function(Location = "./Raw_data", Gruppen = Files) {
  
  # List of the TXT.files in location
  Files <- list.files(path = Location, pattern = ".TXT")
  Files1 <- c()
  for (x in 1:length(Gruppen)) {
    Files1 <- c(Files1,Files[grep(pattern = as.character(Gruppen[x]), x = Files)])
  }
  
  import <- function(data) {
    start <- getwd() ##store Start wd
    setwd(Location) ### setwd to Data file
    
    df <- try(read.csv(data, header = FALSE, 
                       sep = ",",              
                       dec = "." ))         
    setwd(start) ## back to normal
    return(df)  ### Output = df
  }
  
  # Import of the files
  Raw.list <- lapply(Files1,import)
  
  # combine list of data frames 
  Raw.data <- do.call("cbind", Raw.list)
  
  # Remove wavenumbers
  Spectra <- as.data.frame(t(Raw.data[,-c(which(colnames(Raw.data) == "V1"))]))
  
  # Extract wavenumbers and set as column names
  Wavenumber <- as.numeric(Raw.data[,1])
  colnames(Spectra) <- Wavenumber
  
  ### Test for a Gruppen vector, else the files are organised according to files vector
  if(Gruppen[1] != Files1[1]){
    ##### construct Groups vector
    Groups <- c(1:length(Files1))
    for (i in 1:length(Gruppen)){
      Pos <- c(grep(pattern = as.character(Gruppen[i]), x = Files1)) ##grep recognizes patterns in file names
      Groups[Pos] <- Gruppen[i] # appends vector
    }
  }else{
    Groups <- sub(pattern = ".TXT",x= Gruppen, replacement = "")
  }
  ##### Creation of a list containig spectra, wavenumbers and Groups 
  ###Zur Besseren Organisation
  OriginalData <- list(Wavenumber,Spectra,as.factor(Groups))
  
  names(OriginalData) <- c("Wavenumber","Spectra","Groups")
  
  # Output = list
  return(OriginalData)
}


#########################
## Export fucntion (png format)
#########################
# Default for resolution = 600 DPI
# title = Precise definition of the graphs, for example "Loadings"
# The file name is appended by the project name and the version number (c.f. Documentation script)

# Position in front of the graphs
My.export_start <- function(title) {
  setwd("./Graphs")
  png(filename = paste(title,Project,Version,".png", sep = ""), 
         height = 11, width = 15, units = "cm",
         res = 600, pointsize = 9) # for a power point presentation, set to around 11
}

# Position after the graphs
My.export_end <- function() {
  dev.off()  # Finishes export
  setwd("../") # Working directionary back to default 
}

#########################
# Hierarchical clustering analysis
#########################
# Data = Matrix of data (Variables in columns, samples in rows)
# Groups = Vektor der Gruppierung der Daten 

HCA <- function(Data,Groups) {
  d <- dist(Data,method = "euclidean") # create distance matrix
  hca <- hclust(d, method = "ward.D2") # construct histogram
  ## define colours and groups
  dend <- as.dendrogram(hca) # Transform into a format accessable by dendextend
  colors_to_use <- as.numeric(Groups)[order.dendrogram(dend)] # store colours according to the order of the histogram
  Label <- Groups # create labels
  labels_colors(dend) <- colors_to_use # colour labels
  labels(dend) <- Label[order.dendrogram(dend)] # order labels 
  plot(dend, font.lab = 2, ylab = "Heterogenity") # plot
  
}

#########################
# Plot loadings
#########################
# Plots the chosen principal components (PCs) in sequence
# Variab = Vector of the used spectral area
# Load = PCA object, which loadings are to be plotted
# PCs = String vektor detailing the PCs used for plotting
# Default: first three PCs

plot.load <- function(Variab = as.numeric(colnames(Data[[Type[Stats]]][,Sample])), Load = PCA,PCs = c("PC1","PC2","PC3")){
  
  Variance <- Load$sdev^2/sum(Load$sdev^2) # Calcualtion of the explained variance per component
  
  if(length(colnames(Data[[Type[Stats]]])) == length(Sample)){
    Variab <- as.numeric(colnames(Data[[Type[Stats]]]))
  }else{
    Variab <- as.numeric(colnames(Data[[Type[Stats]]][,Sample]))
  }
# Loop function plots all specified components
  for (i in 1:length(PCs)) {
    plot(x= Variab, y = Load$rotation[,PCs[i]],
         type = "l", xlab = "Wavenumber [1/cm]",
         ylab = paste("Loadings of",PCs[i], "[",round(Variance[i]*100,1),"%]", sep = " "), # explained variance is noted in the Y-axis label 
         font = 2,font.lab = 2,
         lab = c(20,15,10),
         col = "dimgrey",xlim = c(max(Variab),min(Variab)), xaxs = "i", bty = "l")
    abline(h = 0, lwd = 0.8, lty = "dashed") # Indicate zero line for an easier interpretation 
    grid(lwd = 0.8)
  }
  
}


#########################
# Plot scores
#########################
# Maximum of 8 groups
# Input = Raw Data (format = List)
# Result = PCA object, which scores are to be plotted
# PCs = which PCs are to be used

plot.scores <- function(Input = Data, Result = PCA, PCs = c(1,2)) {

 # Vector specifying the forms used during the plotting
  Form <- c(15,20,17,18,3,4,6,8)
  
  plot(NULL,xlab = paste("Scores of", "PC",PCs[1], "[",round(Input$Variance[PCs[1]]*100,1),"%]", sep = " "),
       ylab = paste("Scores of","PC" ,PCs[2], "[",round(Input$Variance[PCs[2]]*100,1),"%]", sep = " "),
       col = Input$Groups[!is.na(Input$Groups)], lab = c(10,10,10),
       font.lab = 2, font = 2, xlim = c(min(Result$x[,PCs[1]]),max(Result$x[,PCs[1]])),
       ylim = c(min(Result$x[,PCs[2]]),max(Result$x[,PCs[2]]))
  )
  grid(lwd = 0.8)
  abline(h = 0, v = 0)
  for(g in 1:length(levels(Result$Groups))){
    points(x = Result$x[which(Result$Groups == levels(Result$Groups)[g]),PCs[1]],
           y = Result$x[which(Result$Groups == levels(Result$Groups)[g]),PCs[2]],
           pch = Form[g], col = unique(Result$Groups)[g])
  }
  
}




#########################
# plot Spectra
#########################
# Liste = List containig spectra, wavenumbers and Groups 
# Spektren = Which spectra should be plotted (String)
# Wellenzahl = Vector of the used spectral area
# area = Which are should be shown
# Code = Order for the colours used
# Bereich = Which area should be used 

plot.spectra <- function(Liste,Spektren,area = c(max(Wellenzahl),min(Wellenzahl)), 
                         Code = Liste$Groups[!is.na(Liste$Groups)], 
                         Bereich = c(1:length(colnames(Liste[[Spektren]])))){
  # Store relevant spectra
  X <- Liste[[Spektren]][!is.na(Liste$Groups),Bereich]
  
  matplot(x = as.numeric(colnames(X)),
          y = t(as.matrix(X)),lty = 1, type = "l", col = Code,
          xlab = "Wavenumber [1/cm]", ylab = " ",
          font = 2, font.lab = 2,  ###font = 2 ist Fett gedruckt
          lab = c(20,15,10), xlim = area,
          ylim = c(min(X),max(X)), bty = "l", family = "sans", xaxs = "i")
  grid(lwd = 0.8)
}

#########################
# Normalize Spectra by a peak or area
#########################
# List = List containig spectra, wavenumbers and Groups 
# Spektren = Which spectra should be plotted (String)
# Position = Which intervall or spectral range

Spec_norm <- function(List = Data, Spektren = "Spectra",Wellenzahl = Data$Wavenumber, Position){
    X <- List[[Spektren]]
    Y <- as.numeric(apply(X[,c(which(Wellenzahl == Position[1]):which(Wellenzahl == Position[2]))],1,mean))
    X <- sweep(X,1,Y,"/")

  return(as.data.frame(X))
}




