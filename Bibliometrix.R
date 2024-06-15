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

# List variables
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

results <- biblioAnalysis(M, sep = ";")

options(width=100)
S <- summary(object = results, k = 10, pause = FALSE)

plot(x = results, k = 10, pause = FALSE)


## Significant journals
# Extract and count journals
journal_counts <- M %>% 
  count(SO) %>%
  arrange(desc(n))

# View top journals
top_journals <- head(journal_counts, 10)  # Adjust the number as per your requirement

# Plot top journals
ggplot(top_journals, aes(x = reorder(SO, n), y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Journals", y = "Frequency", title = "Top 10 Journals by Publication Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Sort by citation counts (TC) and select top publications
top_publications <- M %>% 
  arrange(desc(TC)) %>%
  select(TI, TC) %>%
  head(10)  # Adjust the number as per your requirement

# Print top publications
print(top_publications)

# Plot top publications by citation counts
ggplot(top_publications, aes(x = reorder(TI, TC), y = TC)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(x = "Publications", y = "Citation Counts", title = "Top 10 Publications by Citation Counts") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create keyword co-occurrences network
NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")

# Plot the network
net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 30, Title = "Keyword Co-occurrences", type = "fruchterman", size=T,edgesize = 5,labelsize=0.7)
