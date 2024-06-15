# Load libraries
library(bibliometrix)
library(tidyverse)

# File paths
file1 <- "C:/Users/derue/Documents/BAM/Thesis/Codes/scopus - mett.bib"
file2 <- "C:/Users/derue/Documents/BAM/Thesis/Codes/scopus - rappam.bib"
file3 <- "C:/Users/derue/Documents/BAM/Thesis/Codes/scopus - sop.bib"

# Convert each BibTeX file to a bibliographic dataframe
M1 <- convert2df(file = file1, dbsource = "scopus", format = "bibtex")
M2 <- convert2df(file = file2, dbsource = "scopus", format = "bibtex")
M3 <- convert2df(file = file3, dbsource = "scopus", format = "bibtex")

# List variables in each dataframe
names(M1)
names(M2)
names(M3)

# Drop column "PU" from M1 and M2
M1 <- subset(M1, select = -PU)
M2 <- subset(M2, select = -PU)

# Combine into a single dataframe
M <- rbind(M1, M2, M3)

# Remove duplicates based on title (TI)
M <- M %>% distinct(TI, .keep_all = TRUE)

# Perform bibliometric analysis
results <- biblioAnalysis(M, sep = ";")

# Summary of bibliometric analysis
options(width = 100)
S <- summary(object = results, k = 10, pause = FALSE)

# Plot bibliometric results
plot(x = results, k = 10, pause = FALSE)



## Keyword co-occurrences network ##
# Create keyword co-occurrences network
NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")

# Plot the network
net <- networkPlot(NetMatrix, 
                   normalize = "association", 
                   weighted = TRUE, 
                   n = 30, 
                   Title = "Keyword Co-occurrences", 
                   type = "fruchterman", 
                   size = TRUE,
                   edgesize = 5,
                   labelsize = 0.7)


## Co-word analysis ##
# Conceptual Structure using keywords (method="CA")
CS <- conceptualStructure(M, field = "ID", method = "CA", 
                          minDegree = 4, clust = 3, stemming = FALSE, 
                          labelsize = 10, documents = 10)
