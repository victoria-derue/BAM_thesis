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


## Step 2: Read the PDF
# Read the PDF files
pdf_file1 <- "C:/Users/derue/Documents/BAM/Thesis/Codes/RAPPAM/Assessing the effectiveness of protected area management in the Turkish Caucasus.pdf"
pdf_text1 <- pdf_text(pdf_file1)

# Read the second PDF file
pdf_file2 <- "C:/Users/derue/Documents/BAM/Thesis/Codes/RAPPAM/Assessing the Management Effectiveness of Three Protected Areas in Ghana.pdf"
pdf_text2 <- pdf_text(pdf_file2)

# Read the third PDF file
pdf_file3 <- "C:/Users/derue/Documents/BAM/Thesis/Codes/RAPPAM/Does the respondent's role affect the final value of management effectiveness The case of Brazilian marine protected areas.pdf"
pdf_text3 <- pdf_text(pdf_file3)

# Read the fourth PDF file
pdf_file4 <- "C:/Users/derue/Documents/BAM/Thesis/Codes/RAPPAM/Evaluating the Management Effectiveness of Five Protected Areas in Taiwan Using WWF's RAPPAM.pdf"
pdf_text4 <- pdf_text(pdf_file4)

# Read the fifth PDF file
pdf_file5 <- "C:/Users/derue/Documents/BAM/Thesis/Codes/RAPPAM/Evaluation of management effectiveness of protected areas in the Volta Basin, Ghana perspectives on the methodology for evaluation, protected area financing and community participation.pdf"
pdf_text5 <- pdf_text(pdf_file5)

# Read the sixth PDF file
pdf_file6 <- "C:/Users/derue/Documents/BAM/Thesis/Codes/RAPPAM/Expanding Staff Voice in Protected Area Management Effectiveness Assessments within Kenya’s Maasai Mara National Reserve.pdf"
pdf_text6 <- pdf_text(pdf_file6)

# Read the seventh PDF file
pdf_file7 <- "C:/Users/derue/Documents/BAM/Thesis/Codes/RAPPAM/Improving the management effectiveness and decision-making by stakeholders' perspectives A case study in a protected area from the Brazilian Atlantic Forest.pdf"
pdf_text7 <- pdf_text(pdf_file7)

# Read the eighth PDF file
pdf_file8 <- "C:/Users/derue/Documents/BAM/Thesis/Codes/RAPPAM/Management effectiveness and conservation prioritizing the protected areas using RAPPAM methodology (case study Khuzestan province).pdf"
pdf_text8 <- pdf_text(pdf_file8)

# Read the ninth PDF file
pdf_file9 <- "C:/Users/derue/Documents/BAM/Thesis/Codes/RAPPAM/Management effectiveness of a large marine protected area in Northeastern Brazil.pdf"
pdf_text9 <- pdf_text(pdf_file9)

# Read the tenth PDF file
pdf_file10 <- "C:/Users/derue/Documents/BAM/Thesis/Codes/RAPPAM/Rapid assessment of protected area management effectiveness in four countries.pdf"
pdf_text10 <- pdf_text(pdf_file10)

# Read the eleventh PDF file
pdf_file11 <- "C:/Users/derue/Documents/BAM/Thesis/Codes/RAPPAM/Setting priorities to avoid deforestation in Amazon protected areas are we choosing the right indicators.pdf"
pdf_text11 <- pdf_text(pdf_file11)

# Read the twelfth PDF file
pdf_file12 <- "C:/Users/derue/Documents/BAM/Thesis/Codes/RAPPAM/Temporal assessment of the management effectiveness of reef environments The role of marine protected areas in Brazil.pdf"
pdf_text12 <- pdf_text(pdf_file12)


## Step 3: Preprocess the Text
# Combine all the text into a single vector
combined_text <- c(
  pdf_text1, pdf_text2, pdf_text3, pdf_text4, pdf_text5, 
  pdf_text6, pdf_text7, pdf_text8, pdf_text9, pdf_text10,
  pdf_text11, pdf_text12
)

# Create a corpus
corpus <- Corpus(VectorSource(combined_text))

# Stopwords
english_stopwords <- readLines("https://slcladal.github.io/resources/stopwords_en.txt", encoding = "UTF-8")

# Preprocess the corpus: convert to lowercase, remove punctuation, numbers, stopwords, and whitespace
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, english_stopwords)

custom_stopwords <- c("pas", "mpa")

corpus <- tm_map(corpus, removeWords, custom_stopwords)
corpus <- tm_map(corpus, stemDocument, language = "en")
corpus <- tm_map(corpus, stripWhitespace)

## Step 4: Create Document-Term Matrix
dtm <- DocumentTermMatrix(corpus)
dtm <- removeSparseTerms(dtm, 0.99)  # Adjust the sparsity threshold as needed


## Step 5: Perform LDA
# Set the number of topics
num_topics <- 5  # Adjust the number of topics as needed

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
  labs(title = "RAPPAM - Top Terms in Each Topic",
       x = "Term",
       y = "Beta") +
  theme_minimal()
