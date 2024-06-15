## Step 1: Install and Load Required Packages
library(pdftools)
library(tm)
library(textmineR)
library(topicmodels)
library(broom)
library(dplyr)
library(tidytext)
library(pdftools)
library(tm)
library(textmineR)
library(topicmodels)
library(ggplot2)
library(ldatuning)


## Step 2: Read the PDF
pdf_file <- "C:/Users/derue/Documents/BAM/Thesis/Codes/METT/Nugraha et al., 2024.pdf"
pdf_text <- pdf_text(pdf_file)


## Step 3: Preprocess the Text
# Convert the PDF text into a single character vector
pdf_text_combined <- paste(pdf_text, collapse = " ")

# Create a corpus
corpus <- Corpus(VectorSource(pdf_text_combined))

# Stopwords
english_stopwords <- readLines("https://slcladal.github.io/resources/stopwords_en.txt", encoding = "UTF-8")

custom_stopwords <- c("effectiveness", "ment",
                      "protected", "area", "areas", "pa", "pas" ,
                      "evaluation", "assessment", "methodology",
                      "mett", "management effectiveness tracking tool", "rmett",
                      "indonesia", "nepal", 
                      "online", "published", "fig", "table", "university", "cambridge", # references
                      "ramsar site", "ramsar", "site", "sites", # names of the areas in article 1
                      "jrrs", # names of the areas in  article 2
                      "scores", "score", 
                      "showed", "press", "element", "elements")

# Preprocess the corpus: convert to lowercase, remove punctuation, numbers, stopwords, and whitespace
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, english_stopwords)
corpus <- tm_map(corpus, removeWords, custom_stopwords)
#Corpus <- tm_map(Corpus, stemDocument, language = "en")
corpus <- tm_map(corpus, stripWhitespace)

# Read the second PDF file
pdf_file2 <- "C:/Users/derue/Documents/BAM/Thesis/Codes/METT/Thapa and Lindner, 2023.pdf"
pdf_text2 <- pdf_text(pdf_file2)

# Preprocess the text
pdf_text_combined2 <- paste(pdf_text2, collapse = " ")
corpus2 <- Corpus(VectorSource(pdf_text_combined2))

corpus2 <- tm_map(corpus2, content_transformer(tolower))
corpus2 <- tm_map(corpus2, removePunctuation)
corpus2 <- tm_map(corpus2, removeNumbers)
corpus2 <- tm_map(corpus2, removeWords, english_stopwords)
corpus2 <- tm_map(corpus2, removeWords, custom_stopwords)
corpus2 <- tm_map(corpus2, stripWhitespace)

# Combine the preprocessed text with the existing corpus
corpus <- c(corpus, corpus2)


## Step 4: Create Document-Term Matrix
dtm <- DocumentTermMatrix(corpus)
dtm <- removeSparseTerms(dtm, 0.99)  # Adjust the sparsity threshold as needed


## Step 5: Perform LDA
# Tuning the number of topics (k)
result <- FindTopicsNumber(
  dtm,
  topics = seq(from = 2, to = 100, by = 2),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  verbose = TRUE
)

#Plotting the results of the relevant metrics to determine the number of topics (k)
FindTopicsNumber_plot(result)

# Set the number of topics
num_topics <- 4  # Adjust the number of topics as needed

# Perform LDA
lda_model <- LDA(dtm, k = num_topics, control = list(seed = 1234))

# Get the results
topics <- tidy(lda_model, matrix = "beta")


## Step 6: Analyze the Topics
# Get top terms per topic
top_terms <- topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)


# Print top terms for each topic
print(top_terms)

## Step 7: Visualize the topics
top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "METT - Top Terms in Each Topic",
       x = "Term",
       y = "Beta") +
  theme_minimal()
