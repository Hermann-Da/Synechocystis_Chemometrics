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
    library(Packages[p])
  }
}

# Entfernen von Variablen um das Environment zu reduzieren
rm(test) 
rm(Packages)
rm(p)


#########################
##Import Funktion
#########################
# Location: Der Pfad zu den Txt. files
# Gruppen: String Vektor der Gruppennamen (MÃ¼ssen in den File namen vorkommen)
# Maximal 8 Gruppen sind farbtechnisch moeglich 

My.Import <- function(Location = "./Raw_data", Gruppen = Files) {
  
  #Liste von Txt. Files in Location
  Files <- list.files(path = Location, pattern = ".TXT")
  Files1 <- c()
  for (x in 1:length(Gruppen)) {
    Files1 <- c(Files1,Files[grep(pattern = as.character(Gruppen[x]), x = Files)])
  }
  
  import <- function(data) {
    start <- getwd() ##store Start wd
    setwd(Location) ### setwd to Data file
    
    df <- try(read.csv(data, header = FALSE, ##data als Funktionsparameter und hier
                       sep = ",",           ## df ist ein Zwischenspeicher    
                       dec = "." ))          ## Try um Fehler zu Ã¼berspielen
    setwd(start) ## back to normal
    return(df)  ###Speicher in Variable Ã¼bertragen
  }
  
  #Auslesen der Dateien
  Raw.list <- lapply(Files1,import)
  
  #Liste an Dataframes in einen Ã¼berfÃ¼hren
  Raw.data <- do.call("cbind", Raw.list)
  
  # Wellenzahlen entfernen
  Spectra <- as.data.frame(t(Raw.data[,-c(which(colnames(Raw.data) == "V1"))]))
  
  # Wellenzahlen extrahieren und als Colnames setzen
  Wavenumber <- as.numeric(Raw.data[,1])
  colnames(Spectra) <- Wavenumber
  
  ###Ueberpruefen ob ein Gruppenvektor vorhanden ist, sonst Filenamen nehmen
  if(Gruppen[1] != Files1[1]){
    ##### Gruppenvektor erstellen
    Groups <- c(1:length(Files1))
    for (i in 1:length(Gruppen)){
      Pos <- c(grep(pattern = as.character(Gruppen[i]), x = Files1)) ##grep erkennt Bestandteile von Filenamen
      Groups[Pos] <- Gruppen[i] ###FÃ¼llt Vektor auf
    }
  }else{
    Groups <- sub(pattern = ".TXT",x= Gruppen, replacement = "")
  }
  #####Erstelle eine Liste mit Gruppen, Wavenumber, Spektren
  ###Zur Besseren Organisation
  OriginalData <- list(Wavenumber,Spectra,as.factor(Groups))
  
  names(OriginalData) <- c("Wavenumber","Spectra","Groups")
  
  return(OriginalData)
}


#########################
##Export Funktion fÃ¼r png Export
#########################
# Resolutiondefault =  600 DPI
# title = Genauere Bezeichnung der Grafik (String), z.B.: "Loadings"
# Der File name wird erweitert um die Projektbezeichnung und die version (Siehe Dokumentation script)

# Vor der zu exportierenden Grafik positionieren
My.export_start <- function(title) {
  setwd("./Graphs")
  png(filename = paste(title,Project,Version,".png", sep = ""), 
         height = 11, width = 15, units = "cm",
         res = 600, pointsize = 9) ##Fuer Power Point -> Pointsize auf ca. 11 stellen
}

# Nach der zu exportierenden Grafik positionieren
My.export_end <- function() {
  dev.off()  # Beendet Export
  setwd("../") # working directionary zurÃ¼ck auf default
}

#########################
#Hierarchical clustering analysis
#########################
# Data = Matrix der Daten (Variablen in columns, Proben in rows)
# Groups = Vektor der Gruppierung der Daten 

HCA <- function(Data,Groups) {
  d <- dist(Data,method = "euclidean") #Distanzmatrix erzeugen
  hca <- hclust(d, method = "ward.D2") #Histogramm erstellen
  ##Gruppen und Farben bestimmen
  dend <- as.dendrogram(hca) #FÃ¼rs dendextend-package verwendbar machen
  colors_to_use <- as.numeric(Groups)[order.dendrogram(dend)] #Farben mit der Ordnung des Histograms speichern
  Label <- Groups #Labels erstellen
  labels_colors(dend) <- colors_to_use # Labels einfÃ¤rben
  labels(dend) <- Label[order.dendrogram(dend)] #Labels ordnen 
  plot(dend, font.lab = 2, ylab = "Heterogenity") #plot
  
}

#########################
#Loadings plotten 
#########################
# Plottet nacheinander alle angegebenen PCS
# Default sind die ersten 3 PCs
# Variab = Vektor der verwendeten Wellenzahlen
# Load = PCA object, dessen Loadings geplottet werden sollen
# PCs = String vektor der zu plottenden Principal Components, default sind PC1 bis 3
# 

plot.load <- function(Variab = as.numeric(colnames(Data[[Type[Stats]]][,Sample])), Load = PCA,PCs = c("PC1","PC2","PC3")){
  
  Variance <- Load$sdev^2/sum(Load$sdev^2) # Berechnung der erklÃ¤rten varianz pro PC
  
  if(length(colnames(Data[[Type[Stats]]])) == length(Sample)){
    Variab <- as.numeric(colnames(Data[[Type[Stats]]]))
  }else{
    Variab <- as.numeric(colnames(Data[[Type[Stats]]][,Sample]))
  }
# Schleifenfunktion arbeitet die PCs ab  
  for (i in 1:length(PCs)) {
    plot(x= Variab, y = Load$rotation[,PCs[i]],
         type = "l", xlab = "Wavenumber [1/cm]",
         ylab = paste("Loadings of",PCs[i], "[",round(Variance[i]*100,1),"%]", sep = " "), # ErklÃ¤rte Varianz wird in der y- Achse vermerkt 
         font = 2,font.lab = 2,
         lab = c(20,15,10),
         col = "dimgrey",xlim = c(max(Variab),min(Variab)), xaxs = "i", bty = "l")
    abline(h = 0, lwd = 0.8, lty = "dashed") # Null Linie einfÃ¼gen zur besseren Interpretation
    grid(lwd = 0.8)
  }
  
}


#########################
# Scores plotten
#########################
# Maximal 8 Gruppen

plot.scores <- function(Input = Data, Result = PCA, PCs = c(1,2)) {

    x_Ax <- PCs[1]
    Y_Ax <- PCs[2]
    
  Form <- c(15,20,17,18,3,4,6,8)
  
  plot(NULL,xlab = paste("Scores of", "PC",X_Ax, "[",round(Input$Variance[X_Ax]*100,1),"%]", sep = " "),
       ylab = paste("Scores of","PC" ,Y_Ax, "[",round(Input$Variance[Y_Ax]*100,1),"%]", sep = " "),
       col = Input$Groups[!is.na(Input$Groups)], lab = c(10,10,10),
       font.lab = 2, font = 2, xlim = c(min(Result$x[,X_Ax]),max(Result$x[,X_Ax])),
       ylim = c(min(Result$x[,Y_Ax]),max(Result$x[,Y_Ax]))
  )
  grid(lwd = 0.8)
  abline(h = 0, v = 0)
  for(g in 1:length(levels(Result$Groups))){
    points(x = Result$x[which(Result$Groups == levels(Result$Groups)[g]),X_Ax],
           y = Result$x[which(Result$Groups == levels(Result$Groups)[g]),Y_Ax],
           pch = Form[g], col = unique(Result$Groups)[g])
  }
  
}




#########################
#Spektren plotten 
#########################
# Liste = Datenliste mit Wavenumber, Spectra, Groups
# Spektren = Welche Spektren sollen geplottet werden
# Wellenzahl = Vektor der Wellenzahlen
# area = Welcher Bereich soll geplottet werden
# Code = Faktor vektor, ordnet die Farben zu 
# Bereich = Welche Wellenzahlen werden ausgewÃ¤hlt

plot.spectra <- function(Liste,Spektren,area = c(max(Wellenzahl),min(Wellenzahl)), 
                         Code = Liste$Groups[!is.na(Liste$Groups)], 
                         Bereich = c(1:length(colnames(Liste[[Spektren]])))){
  # Spektren abspeichern 
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
#Spektren auf Peaks normieren 
#########################
# List = Liste aus der genommen wird, z.B.: Data
# Spektren = Welche Spektren normiert werden sollen
# Position = Welches Intervall bzw. welche Wellenzahl

Spec_norm <- function(List = Data, Spektren = "Spectra",Wellenzahl = Data$Wavenumber, Position){
    X <- List[[Spektren]]
    Y <- as.numeric(apply(X[,c(which(Wellenzahl == Position[1]):which(Wellenzahl == Position[2]))],1,mean))
    X <- sweep(X,1,Y,"/")

  return(as.data.frame(X))
}




