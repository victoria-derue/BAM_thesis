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
pdf_file1 <- "C:/Users/derue/Documents/BAM/Thesis/Codes/METT/A global analysis of management capacity and ecological outcomes in terrestrial protected areas.pdf"
pdf_text1 <- pdf_text(pdf_file1)

# Read the second PDF file
pdf_file2 <- "C:/Users/derue/Documents/BAM/Thesis/Codes/METT/A preliminary assessment of protected area management within the WWF ‘Coastal East Africa’ priority place, Eastern Africa.pdf"
pdf_text2 <- pdf_text(pdf_file2)

# Read the third PDF file
pdf_file3 <- "C:/Users/derue/Documents/BAM/Thesis/Codes/METT/Assessing the effectiveness of a protected area network A case study of Bhutan.pdf"
pdf_text3 <- pdf_text(pdf_file3)

# Read the fourth PDF file
pdf_file4 <- "C:/Users/derue/Documents/BAM/Thesis/Codes/METT/Assessment of marine protected areas in the East China Sea using a management effectiveness tracking tool.pdf"
pdf_text4 <- pdf_text(pdf_file4)

# Read the fifth PDF file
pdf_file5 <- "C:/Users/derue/Documents/BAM/Thesis/Codes/METT/Assessment of the effectiveness of protected areas management in iran Case study in khojir national park.pdf"
pdf_text5 <- pdf_text(pdf_file5)

# Read the sixth PDF file
pdf_file6 <- "C:/Users/derue/Documents/BAM/Thesis/Codes/METT/Associating Management Effectiveness Scores to Conservation Activities A Study of Gbele Resource Reserve, Ghana.pdf"
pdf_text6 <- pdf_text(pdf_file6)

# Read the seventh PDF file
pdf_file7 <- "C:/Users/derue/Documents/BAM/Thesis/Codes/METT/Beyond Protected Areas Assessing Management Effectiveness of a Ramsar Site in Nepal.pdf"
pdf_text7 <- pdf_text(pdf_file7)

# Read the eighth PDF file
pdf_file8 <- "C:/Users/derue/Documents/BAM/Thesis/Codes/METT/Changes in protected area management effectiveness over time A global analysis.pdf"
pdf_text8 <- pdf_text(pdf_file8)

# Read the ninth PDF file
pdf_file9 <- "C:/Users/derue/Documents/BAM/Thesis/Codes/METT/Evaluating the effectiveness of protected area management in Indonesia.pdf"
pdf_text9 <- pdf_text(pdf_file9)

# Read the tenth PDF file
pdf_file10 <- "C:/Users/derue/Documents/BAM/Thesis/Codes/METT/Evaluating the management effectiveness of protected areas in Mongolia using the management effectiveness tracking tool.pdf"
pdf_text10 <- pdf_text(pdf_file10)

# Read the eleventh PDF file
pdf_file11 <- "C:/Users/derue/Documents/BAM/Thesis/Codes/METT/Lessons learned from 18 years of implementing the management effectiveness tracking tool (Mett) A perspective from the mett developers and implementers.pdf"
pdf_text11 <- pdf_text(pdf_file11)

# Read the twelfth PDF file
pdf_file12 <- "C:/Users/derue/Documents/BAM/Thesis/Codes/METT/Linking management effectiveness indicators to observed effects of protected areas on fire occurrence in the Amazon rainforest.pdf"
pdf_text12 <- pdf_text(pdf_file12)

# Read the thirteenth PDF file
pdf_file13 <- "C:/Users/derue/Documents/BAM/Thesis/Codes/METT/Management effectiveness assessment for Ecuador’s national parks.pdf"
pdf_text13 <- pdf_text(pdf_file13)

# Read the fourteenth PDF file
pdf_file14 <- "C:/Users/derue/Documents/BAM/Thesis/Codes/METT/Management resourcing and government transparency are key drivers of biodiversity outcomes in Southeast Asian protected areas.pdf"
pdf_text14 <- pdf_text(pdf_file14)

# Read the fifteenth PDF file
pdf_file15 <- "C:/Users/derue/Documents/BAM/Thesis/Codes/METT/Multidimensional indicators to improve management effectiveness monitoring of protected areas.pdf"
pdf_text15 <- pdf_text(pdf_file15)

# Read the sixteenth PDF file
pdf_file16 <- "C:/Users/derue/Documents/BAM/Thesis/Codes/METT/Plant and Other Forest Bioresource Utilization by Local Communities of Northern Negros Natural Park, Negros Island, Philippines.pdf"
pdf_text16 <- pdf_text(pdf_file16)

# Read the seventeenth PDF file
pdf_file17 <- "C:/Users/derue/Documents/BAM/Thesis/Codes/METT/Rapid assessment of management effectiveness of the zhangye national wetland park, Gansu Province, people’s republic of China.pdf"
pdf_text17 <- pdf_text(pdf_file17)

# Read the eighteenth PDF file
pdf_file18 <- "C:/Users/derue/Documents/BAM/Thesis/Codes/METT/Single-species conservation in a multiple-use landscape Current protection of the tiger range.pdf"
pdf_text18 <- pdf_text(pdf_file18)

# Read the nineteenth PDF file
pdf_file19 <- "C:/Users/derue/Documents/BAM/Thesis/Codes/METT/Southeast Asian protected areas are effective in conserving forest cover and forest carbon stocks compared to unprotected areas.pdf"
pdf_text19 <- pdf_text(pdf_file19)

# Read the twentieth PDF file
pdf_file20 <- "C:/Users/derue/Documents/BAM/Thesis/Codes/METT/Threats assessment and management of the Baua-Wangag Watershed Forest Reserve in Gonzaga, Cagayan Province, Philippines.pdf"
pdf_text20 <- pdf_text(pdf_file20)


## Step 3: Preprocess the Text
# Combine all the text into a single vector
combined_text <- c(pdf_text1, pdf_text2, pdf_text3, pdf_text4, pdf_text5, pdf_text6, pdf_text7,
                   pdf_text8, pdf_text9, pdf_text10, pdf_text11, #pdf_text12
                   pdf_text13,
                   pdf_text14, pdf_text15, pdf_text16, pdf_text17, pdf_text18, pdf_text19,
                   pdf_text20)


# Create a corpus
corpus <- Corpus(VectorSource(combined_text))

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
#corpus <- tm_map(corpus, removeWords, custom_stopwords)
Corpus <- tm_map(corpus, stemDocument, language = "en")
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
K <- 4

# Perform LDA
lda_model <- LDA(dtm, K , method="Gibbs", control=list(iter = 1000, verbose = 25))

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

terms(lda_model, 10)

