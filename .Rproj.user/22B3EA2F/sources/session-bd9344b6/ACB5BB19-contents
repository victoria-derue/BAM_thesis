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
# Read the PDF files
library(tm)

# Read the first PDF file
pdf_file1 <- "C:/Users/derue/Documents/BAM/Thesis/Codes/SoP/Accountability, Reporting, or Management Improvement Development of a State of the Parks Assessment System in New South Wales, Australia.pdf"
pdf_text1 <- pdf_text(pdf_file1)

# Read the second PDF file
pdf_file2 <- "C:/Users/derue/Documents/BAM/Thesis/Codes/sOp/Challenges and experiences in implementing a management effectiveness evaluation program in a protected area system.pdf"
pdf_text2 <- pdf_text(pdf_file2)

## Step 3: Preprocess the Text
# Combine all the text into a single vector
combined_text <- c(pdf_text1, pdf_text2)

# Create a corpus
corpus <- Corpus(VectorSource(combined_text))

# Stopwords
english_stopwords <- readLines("https://slcladal.github.io/resources/stopwords_en.txt", encoding = "UTF-8")

custom_stopwords <- c("mett", "score", 
                      "Philippines", "Philippine", "National",
                      "pa", "pas",
                      "Hockings")

# Preprocess the corpus: convert to lowercase, remove punctuation, numbers, stopwords, and whitespace
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, english_stopwords)

custom_stopwords <- c("national", "nsw", "sop", "wwf",
                      "state",
                      "hockings", "table")

corpus <- tm_map(corpus, removeWords, custom_stopwords)
corpus <- tm_map(corpus, stemDocument, language = "en")
corpus <- tm_map(corpus, stripWhitespace)


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

# Plotting the results of the relevant metrics to determine the number of topics (k)
FindTopicsNumber_plot(result)

# Set the number of topics
K <- 6

# Perform LDA
lda_model <- LDA(dtm, K , method="Gibbs", control=list(iter = 1000, verbose = 25))

# Get the results
topics <- tidy(lda_model, matrix = "beta")

## Step 6: Analyze the Topics
# Get top terms per topic
top_terms <- topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
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
  labs(title = "SoP - Top Terms in Each Topic",
       x = "Term",
       y = "Beta") +
  theme_minimal()

terms(lda_model, 10)
