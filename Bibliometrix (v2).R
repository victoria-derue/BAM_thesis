library(bibliometrix)
library(bib2df)
library(tidyverse)
library(here)


# File paths
file_path <- here::here("scopus - full.bib")

## Bibliometrix
# Convert each BibTeX file to a bibliographic dataframe
M <- convert2df(file = file_path, dbsource = "scopus", format = "bibtex")

# List variables
names(M)

# Perform bibliometric analysis
results <- biblioAnalysis(M, sep = ";")

options(width=100)
S <- summary(object = results, k = 10, pause = FALSE)

plot(x = results, k = 10, pause = FALSE)


## Significant journals
# Summarize journal information: count articles and sum citations
journal_summary <- M %>%
  group_by(SO) %>%
  summarise(Number_of_Articles = n(),
            Total_Citations = sum(TC, na.rm = TRUE)) %>%
  arrange(desc(Number_of_Articles)) %>%
  rename(Journal = SO)

# Print the result
print(head(journal_summary, 10))

# Display the count of Journals by Number of Articles
journal_summary %>%
  count(Number_of_Articles) %>%
  arrange(desc(Number_of_Articles)) %>%
  print()


## Significant authors
# Summarize author information: count articles and sum citations
author_summary <- M %>%
  separate_rows(AU, sep = ";") %>% 
  group_by(AU) %>%
  summarise(Number_of_Articles = n(),
            Total_Citations = sum(TC, na.rm = TRUE)) %>%
  rename(Author = AU) %>%
  arrange(desc(Number_of_Articles))

# Print the result
print(head(author_summary, 10))

#h_index_data <- c(
#  "HOCKINGS M" = 36,
#  "GELDMANN J" = 29,
#  "BURGESS ND" = 76,
#  "COAD L" = 26,
#  "LEVERINGTON F" = 11,
#  "ANTHONY BP" = 10,
#  "DUDLEY N" = 34,
#  "AHMADIA GN" = 26,
#  "ANDRADI-BROWN DA" = 20,
# "COOK CN" = 32)

#h_index_df <- tibble(
#  Author = names(h_index_data),
#  H_index = as.integer(h_index_data))

#author_summary <- author_summary %>%
# left_join(h_index_df, by = "Author")

#print(author_summary)


# Display the count of Authors by Number of Articles
author_summary %>%
  count(Number_of_Articles) %>%
  arrange(desc(Number_of_Articles)) %>%
  print()


## Significant publications
# Summarize publications information: sum citations
publications_summary <- M %>%
  select(TI, AU, SO, TC) %>%
  group_by(TI, AU, SO) %>%
  summarise(Total_Citations = sum(TC, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(Total_Citations))

# Print the result
print(head(publications_summary, 10))

# Sum all the citations
sum(publications_summary$Total_Citations, na.rm = TRUE)