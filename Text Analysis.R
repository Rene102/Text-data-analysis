# Set the working directory to your folder
setwd("C:/Users/ual-laptop/Desktop/rrse/")

# Load the required packages
library(dplyr)
library(tm)
library(tidyverse) # 
library(stringr)  # for string manipulation
library(tidytext)
library(textstem) # for lemmatization
library(tm)
library(koRpus)
library(koRpus.lang.en)
library(textstem)
library(tm)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(tm)
library(tidytext)
library(wordcloud)
library(RColorBrewer)
library(ldatuning)
library(topicmodels)
library(topicmodels)
library(tidytext)
library(magrittr) 
library(dplyr)
# Define a function to clean HTML tags
clean_html <- function(text) {
  # Use regular expressions to remove HTML tags
  clean_text <- gsub("<[^>]+>", " ", text)
  return(clean_text)
}
filename <- "clustered_data01.csv"
MyData <- read.csv(filename, header = TRUE, sep = ",", encoding = "UTF-8", stringsAsFactors = FALSE)
readpercent <- function() {
  n <- as.integer(readline(prompt = "Enter a percentage (positive for head, negative for tail) [-100-100]: "))
  if (is.na(n) || !(grepl("^-?[0-9]+$", n)) || n > 100 || n < -100) {
    cat("Invalid percentage. Please enter a valid percentage between -100 and 100.\n")
    return(readpercent())
  }
  return(n)
}


statedPercent <- readpercent()
MyData$AwardedAmountToDate <- as.numeric(gsub("[^0-9.]", "", MyData$AwardedAmountToDate))
str(MyData$AwardedAmountToDate)

# histogram
GenerateAndSaveHistogram <- function(funding, statedPercent, numRows, topicDir) {
  if (statedPercent < 0) {
    color1 <- "gray"
    color2 <- "gray40"
    nInHead <- as.integer(((100 + statedPercent) / 100) * numRows)
    nInTail <- numRows - nInHead
    headPercent <- statedPercent + 100
    tailPercent <- abs(statedPercent)
  } else {
    color2 <- "gray"
    color1 <- "gray40"
    nInHead <- as.integer((statedPercent / 100) * numRows)
    nInTail <- numRows - nInHead
    headPercent <- statedPercent
    tailPercent <- 100 - statedPercent
  }
  
  cols <- c(rep(color1, times = nInHead), rep(color2, times = nInTail))
  png(paste(topicDir, "/award_amount_histogram.png", sep = ""), width = 5, height = 4, units = 'in', res = 300)
  hist(funding, breaks = 20, col = cols, main = NULL, xpd = FALSE, las = 1)
  legend("right",
         legend = c(paste("Head (", headPercent, "%)", sep = ""),
                    paste("Tail (", tailPercent, "%)", sep = "")),
         fill = c(color1, color2), cex = 0.9)
  title("Distribution of Award Amounts", cex.main = 0.95, line = -2)
  title(xlab = "Award Amount (in millions of dollars)", line = 0)
  title(ylab = "Frequency", line = 2)
  dev.off()
}

#  histogram
GenerateAndSaveHistogram(MyData$AwardedAmountToDate/1000000, statedPercent, nrow(MyData), "C:/Users/ual-laptop/Desktop/rrse/")
# Convert the text to UTF-8
MyData$Abstract <- iconv(MyData$Abstract, to = "UTF-8")

#  text processing
text_corpus <- Corpus(VectorSource(MyData$Abstract))

# Create a list of custom stop words
custom_stop_words <- c(
  "drones", "drone", "unmanned", "aerial", "vehicles", "flight",
  "emergency", "response", "scenarios", "safety", "privacy", "delivering", 
  "drone", "mission", "model", "project", "research"
)


text_corpus <- tm_map(text_corpus, content_transformer(tolower))
text_corpus <- tm_map(text_corpus, removePunctuation)
text_corpus <- tm_map(text_corpus, removeNumbers)
text_corpus <- tm_map(text_corpus, removeWords, custom_stop_words)
text_corpus <- tm_map(text_corpus, stripWhitespace)
processed_text <- sapply(text_corpus, as.character)


######

#  bigram work
nsf_funding <- tibble(id = as.character(MyData$AwardNumber),
                      abstract = MyData$Abstract,
                      amount = MyData$AwardedAmountToDate)

nsf_funding_bigram <- nsf_funding %>% 
  unnest_tokens(bigram, abstract, token = "ngrams", n = 2)

nsf_funding_bigram <- nsf_funding_bigram %>%
  separate(bigram, c("word1", "word2"), sep = " ")
nsf_funding_bigram <- nsf_funding_bigram %>%
  filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word)
nsf_funding_bigram <- nsf_funding_bigram %>%
  unite(bigram, word1, word2, sep = " ")

nsf_funding <- nsf_funding %>% 
  unnest_tokens(word, abstract)  %>% 
  anti_join(stop_words)
nsf_funding <- nsf_funding %>% filter(!is.na(word))

#  lemmatization
lemma_unique <- nsf_funding %>%
  select(word) %>%
  mutate(word_clean = str_replace_all(word,"\u2019s|'s","")) %>%
  filter(!duplicated(word_clean)) %>%
  filter(!is.na(word_clean)) %>%
  arrange(word)

lemma_unique$lemma <- lemmatize_words(lemma_unique$word_clean)
lemma_unique = within(lemma_unique, {
  lemma = ifelse(is.na(lemma), word_clean, lemma)  } )

lemma_unique <- nsf_funding %>%
  select(word) %>%
  mutate(word_clean = str_replace_all(word,"\u2019s|'s","")) %>%
  filter(!duplicated(word_clean)) %>%
  filter(!is.na(word_clean)) %>%
  arrange(word)
lemma_unique$lemma <- lemmatize_words(lemma_unique$word_clean)

lemma_unique = within(lemma_unique, {
  lemma = ifelse(is.na(lemma), word_clean, lemma)  } )

lemma_new <- lemma_unique
lemma_new <- left_join(nsf_funding, lemma_new, by = "word") %>%
  mutate(word = ifelse(is.na(lemma), word_clean, lemma))
lemma_new <- subset(lemma_new, select = c(id, word))


# Create a text corpus from the lemmatized words
myCorpus <- Corpus(VectorSource(lemma_new$word))
toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})
myCorpus <- tm_map(myCorpus, removeNumbers)
dtm <- DocumentTermMatrix(myCorpus)

# Calculate the frequency of each term in the document-term matrix
freq <- colSums(as.matrix(dtm))
ord <- order(freq, decreasing=TRUE)
freq[head(ord)]
freq[tail(ord)]

#
wf <- data.frame(term = names(freq), occurrences = freq)

# Create a bar graph of word frequencies
p <- ggplot(subset(wf, occurrences > 200), aes(term, occurrences))
p <- p + geom_bar(stat = "identity")
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot
print(p)

# Save the plot
ggsave("WordFreqBarGraph.png")


# Create a bar graph of word frequencies
p <- ggplot(subset(wf, occurrences > 350), aes(term, occurrences))
p <- p + geom_bar(stat = "identity")
p <- p + theme_minimal() # Use a theme
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), # Rotate and increase size of x-axis labels
               axis.text.y = element_text(size = 12), # Increase size of y-axis labels
               plot.title = element_text(size = 14, hjust = 0.5)) # Increase size of title
p <- p + labs(x = "Terms", y = "Occurrences", title = "Word Frequencies") # Add labels

# Display the plot
print(p)

# Create a bar graph of word frequencies
p <- ggplot(subset(wf, occurrences > 150 & occurrences < 200), aes(term, occurrences))
p <- p + geom_bar(stat = "identity")
p <- p + theme_minimal() # Use a theme
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), # Rotate and increase size of x-axis labels
               axis.text.y = element_text(size = 12), # Increase size of y-axis labels
               plot.title = element_text(size = 14, hjust = 0.5)) # Increase size of title
p <- p + labs(x = "Terms", y = "Occurrences", title = "Word Frequencies") # Add labels

# Display the plot
print(p)

ggsave("WordFreqBarGraph101.png")


library(wordcloud)
library(RColorBrewer)

# Set the seed for reproducibility
set.seed(42)

# Create a word cloud
png("FreqWordCloud.png", width = 1280, height = 800)
wordcloud(names(freq), freq, max.words = 100, scale = c(4, .2), colors = brewer.pal(6, "Dark2"))
dev.off()


wordcloud(names(freq), freq, max.words = 100, scale = c(4, .2), colors = brewer.pal(6, "Dark2"))
# Count the words
word_counts <- lemma_new %>%
  count(id, word, sort = TRUE) %>%
  ungroup()

# Create a document-term matrix
desc_dtm <- word_counts %>%
  cast_dtm(id, word, n)

# Calculate tf-idf
desc_tf_idf <- lemma_new %>% 
  count(id, word, sort = TRUE) %>%
  ungroup() %>%
  bind_tf_idf(word, id, n)

low_idf <- subset(desc_tf_idf, idf > 0.38 & idf < 2)
write.csv(low_idf, "low_idf.csv")
print(head(low_idf, 28))


# Calculate the frequency of each term
freq <- colSums(as.matrix(desc_dtm))
ord <- order(freq, decreasing = TRUE)
most_frequent <- freq[head(ord)]
print(most_frequent)
least_frequent <- freq[tail(ord)]
print(least_frequent)



# Set the seed for reproducibility
set.seed(42)

# Create a word cloud
png("TF-IDFWordCloud.png", width = 1280, height = 800)
wordcloud(names(freq), freq, min.freq = 250, scale = c(4, .2), colors = brewer.pal(6, "Dark2"))
dev.off()
wordcloud(names(freq), freq, min.freq = 150, scale = c(4, .2), colors = brewer.pal(6, "Dark2"))


library(ldatuning)
library(topicmodels)

# Find the optimal number of topics
result <- FindTopicsNumber(
  desc_dtm,
  topics = seq(from = 10, to = 20, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 1234),
  mc.cores = 4L,
  verbose = TRUE
)

# Save the plot to a PNG file
png("FindTopicNumber.png", width = 1280, height = 800)
FindTopicsNumber_plot(result)
dev.off()

# Display the plot
FindTopicsNumber_plot(result)

# Define a function to calculate the harmonic mean
harmonicMean <- function(logLikelihoods, precision = 2000L) {
  llMed <- median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods, prec = precision) + llMed))))
}




# Create a subset of data for faster testing
subset_size <- 100  # Adjust the subset size as needed
desc_dtm_subset <- desc_dtm[1:subset_size, ]

seqk <- seq(2, 40, 1)  # Use a smaller range for faster testing
burnin <- 100
iter <- 100
keep <- 10

# To measure overall execution time
start_time <- Sys.time()

fitted_many <- lapply(seqk, function(k) {
  # Display a message
  print(paste("Fitting model for k =", k))
  
  # Record start time for the current k
  k_start_time <- Sys.time()
  
  # Fit LDA model with adjusted parameters
  lda_model <- LDA(desc_dtm_subset, k = k, method = "Gibbs", control = list(burnin = burnin, iter = iter, keep = keep))
  
  # Calculate elapsed time for the current k
  k_elapsed_time <- Sys.time() - k_start_time
  print(paste("Elapsed time for k =", k_elapsed_time))
  
  # Return the fitted model
  return(lda_model)
})

# Calculate overall elapsed time
overall_elapsed_time <- Sys.time() - start_time
print(paste("Total elapsed time:", overall_elapsed_time))

# Load the required packages
library(Rmpfr)
library(ggplot2)
library(scales)

# Extract the log-likelihoods from each topic
logLiks_many <- lapply(fitted_many, function(L)  L@logLiks[-c(1:(burnin/keep))])

# Compute the harmonic means
hm_many <- sapply(logLiks_many, function(h) harmonicMean(h))

# Create a plot of the harmonic means
ldaplot <- ggplot(data.frame(seqk, hm_many), aes(x = seqk, y = hm_many)) +
  geom_path(lwd = 1.5) +
  theme(text = element_text(family = NULL),
        axis.title.y = element_text(vjust = 1, size = 16),
        axis.title.x = element_text(vjust = -.5, size = 16),
        axis.text = element_text(size = 16),
        plot.title = element_text(size = 20)) +
  scale_y_continuous(labels = comma) +
  xlab('Number of Topics') +
  ylab('Harmonic Mean') +
  annotate("text", x = 25, y = max(hm_many) + 10000, 
           label = paste("The optimal number of topics is", seqk[which.max(hm_many)])) +
  ggtitle(expression(atop("LDA Analysis of NSF Program", 
                          atop(italic("Harmonic Mean"), ""))))


png("LDAHamonicMean.png", width = 1280, height = 800)
print(ldaplot)
dev.off()
print(ldaplot)

NumTopics <- 21
desc_lda <- LDA(desc_dtm, method = "Gibbs", k = NumTopics, control = list(seed = 1234))
saveRDS(desc_lda, file = "desc_lda.rds")
tidy_lda <- tidy(desc_lda)

# Examine the top 10 terms for each topic
top_terms <- tidy_lda %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
# Load the required package
library(wordcloud)

# Get the top 10 terms for each topic
top10termsPerTopic <- terms(desc_lda, 10)
topicNames <- apply(top10termsPerTopic, 2, paste, collapse=" ")

# Get the posterior distributions
tmResult <- posterior(desc_lda)

# Visualize topics as word clouds
for (TopicNum in 1:NumTopics){
  set.seed(42)
  topicToViz <- TopicNum
  top100terms <- sort(tmResult$terms[topicToViz, ], decreasing = TRUE)[1:100]
  words <- names(top100terms)
  probabilities <- sort(tmResult$terms[topicToViz, ], decreasing = TRUE)[1:100]
  mycolors <- brewer.pal(8, "Dark2")
  
  # Save the word cloud to a PNG file
  png(paste0("WordCloud", TopicNum, ".png"), width = 1280, height = 800)
  wordcloud(words, probabilities, scale = c(4, .2), random.order = FALSE, color = mycolors)
  dev.off()
  
  # Display the word cloud
  wordcloud(words, probabilities, scale = c(4, .2), random.order = FALSE, color = mycolors)
}
# Load the required package
library(ggplot2)

# Save and display the top terms in each LDA topic
png("TermsLDAHistogram.png", width = 1280, height = 1600)
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  group_by(topic, term) %>%
  arrange(desc(beta)) %>%
  ungroup() %>%
  mutate(term = factor(paste(term, topic, sep = "__"), 
                       levels = rev(paste(term, topic, sep = "__")))) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  labs(title = "Top terms in each LDA topic",
       x = NULL, y = expression(beta)) +
  facet_wrap(~ topic, ncol = 3, scales = "free")
dev.off()

# Display the top 10 terms in each model
terms(desc_lda, 10)


lda_gamma <- tidy(desc_lda, matrix = "gamma")
lda_gamma

library(ggplot2)
library(tidyverse)
library(stringr)
library(data.table)

setDT(MyData, keep.rownames = TRUE)[]
colnames(MyData)[1] <- "FundingRank"
MyData$AwardNumber <- as.character(MyData$AwardNumber)

str(lda_gamma)
head(lda_gamma)
lda_gamma2 <- dplyr::left_join(lda_gamma, MyData, by = c("document" = "AwardNumber"))
lda_gamma2$FundingRank = as.numeric(lda_gamma2$FundingRank)
print(class(lda_gamma2$gamma))
print(class(lda_gamma2$AwardedAmountToDate))
lda_gamma2$AwardedAmountToDate <- as.numeric(gsub("[$,]", "", lda_gamma2$AwardedAmountToDate))
print(class(lda_gamma2$AwardedAmountToDate))

lda_gamma2 <- lda_gamma2 %>% mutate(worth = gamma * AwardedAmountToDate)

# Calculate the sum of the worth for each topic
topic_worth <- aggregate(lda_gamma2$worth, by = list(topic = lda_gamma2$topic), FUN = "sum")
colnames(topic_worth)[2] <- "WorthSum"

# Sort by most worthy of topic
topic_worth <- topic_worth %>% arrange(desc(WorthSum))

# Save topic_worth as CSV
setDT(topic_worth, keep.rownames = "TopicWorthRank")[]

lda_gamma3 <- left_join(lda_gamma2, topic_worth, 
                        select(x, TopicWorthRank), by = "topic") 
lda_gamma3$TopicWorthRank = as.numeric(lda_gamma3$TopicWorthRank)
lda_gamma3 <- na.omit(lda_gamma3)



p <- ggplot(lda_gamma3, aes(x = TopicWorthRank, y = FundingRank)) +
  geom_point(aes(size = gamma), shape = 23) +   # draw points with gamma values
  scale_size_continuous(limits = c(0, 0.01)) +  # adjust limits accordingly
  ylim(c(0, length(lda_gamma3$document))) +  # assuming document represents the maximum Funding Rank #
  xlim(c(1, max(lda_gamma3$TopicWorthRank))) +  # assuming TopicWorthRank is the maximum Topic #
  labs(subtitle = "Topic by Document", 
       x = "Topic #", 
       y = "Funding Rank",  # Assuming FundingRank represents the document order
       title = "Gamma Level sorted by Funding Level", 
       caption = "Topic Analysis") 
p
lda_gamma3 <- na.omit(lda_gamma3)
topicDir <- "C:/Users/ual-laptop/Desktop/rrse/"
ggsave(paste(topicDir, "/DocTopic.png", sep = ""), height = 10, device = "png")
png(paste(topicDir, "/TermsLDAHistogram.png", sep = ""), width = 1280, height = 1600)

while (!is.null(dev.list()))  dev.off()
p

# Check for NA values in FundingRank
print(summary(lda_gamma3$FundingRank))
print(any(is.na(lda_gamma3$FundingRank)))
print(summary(lda_gamma3$TopicWorthRank))
print(any(is.na(lda_gamma3$TopicWorthRank)))


nsf_funding <- data_frame(
  id = as.character(MyData$AwardNumber),
  abstract = MyData$Abstract,
  amount = MyData$AwardedAmountToDate
)
####

nsf_funding <- data.frame(
  id = as.character(MyData$AwardNumber),
  abstract = MyData$Abstract,
  amount = MyData$AwardedAmountToDate
)

# Assigning nsf_funding_join to nsf_funding
nsf_funding_join <- nsf_funding

# Assigning 'document' column in nsf_funding_join as character type
nsf_funding_join$document <- as.character(nsf_funding_join$id)

# Checking if 'abstract' column exists in funding_gamma
if ('abstract' %in% colnames(funding_gamma)) {
  # If 'abstract' column exists, perform subset operation to remove 'id' and 'abstract' columns
  funding_gamma <- subset(funding_gamma, select = -c(id, abstract))
} else {
  # If 'abstract' column does not exist, print a message
  print("Column 'abstract' not found in funding_gamma dataframe.")
}


funding_gamma <- merge(lda_gamma, nsf_funding_join, by="document", all.x = TRUE)
funding_gamma <- subset(funding_gamma, select=-c(id, abstract))

# Need to join lda_gamma with funding amount vector, that would be informative
doc_top <- funding_gamma %>%
  group_by(document) %>%
  top_n(5, gamma) %>%
  ungroup() %>%
  arrange(document, desc(gamma))

# Save to CSV
write.csv(doc_top, paste(topicDir,"/topTopics5Documents.csv", sep=""))

# Consensus document for each topic
document_topics <- doc_top %>%
  count(document, topic, amount) %>%
  group_by(topic) %>%
  top_n(1, n) %>%
  ungroup() %>%
  transmute(consensus = document, topic, amount)

# Print document_topics
document_topics


library(flexdashboard)
library(shiny)
library(dplyr)
library(stm)
install.packages("scales")
library(quanteda)
library(tidytext)
library(ggplot2)
library(scales)

# Visualize which documents have the highest probability of being generated from each topic
p <- funding_gamma %>% 
  mutate(document = factor(document, levels = rev(unique(document)))) %>%
  group_by(document) %>%
  top_n(1) %>%
  ungroup %>%
  ggplot(aes(reorder(document, amount), y=gamma, label = NA, fill = as.factor(topic))) +
  geom_col() +
  geom_text(aes(document, 0.01), hjust = 0,
            color = "white", size = .5) +
  scale_fill_manual(values = c("#F48024", "#0077CC", "#5FBA7D", 
                               "#8C60A7", "#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7", 
                               "#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD", 
                               "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D", 
                               "#8A7C64", "#599861", "#A48024", "#B077CC", "#CFBA7D", "#DFBA7D")) +
  scale_y_continuous(expand = c(0,0),
                     labels = percent_format()) +
  coord_flip() + 
  theme_minimal() +
  theme(axis.text.y=element_blank()) +
  labs(x = NULL, y = expression(gamma), fill = "Topic")  

# Save the plot
ggsave(paste(topicDir,"/TopicGammaMap.png", sep=""), height = 10, device = "png")

# Close all open graphics devices
while (!is.null(dev.list()))  dev.off()

# Print the plot
p

# Creating a heatmap to visualize the gamma probabilities across topics for each document
ggplot(funding_gamma, aes(x = topic, y = document, fill = gamma)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("#FFFFFF", "#3182BD", "#31A354", "#FDD0A2", "#FDAE6B", "#E6550D"),
                       na.value = "grey50", guide = "colorbar") +
  theme_minimal() +
  labs(x = "Topic", y = "Document", fill = expression(gamma)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "right") +
  ggtitle("Gamma Probabilities of Documents across Topics (Heatmap)")


# Load necessary libraries
library(ggplot2)

# Assuming you have the 'funding_gamma' dataframe with columns: 'topic', 'document', 'gamma'

# Creating a sample dataframe (replace this with your actual data)
funding_gamma <- data.frame(
  topic = rep(1:5, each = 5),
  document = rep(letters[1:5], times = 5),
  gamma = runif(25)
)

# Creating a heatmap to visualize the gamma probabilities across topics for each document
ggplot(funding_gamma, aes(x = topic, y = document, fill = gamma)) +
  geom_tile() +
  scale_fill_gradientn(
    colors = c("#FFFFFF", "#3182BD", "#31A354", "#FDD0A2", "#FDAE6B", "#E6550D"),
    na.value = "grey50", guide = "colorbar"
  ) +
  theme_minimal() +
  labs(x = "Topic", y = "Document", fill = expression(gamma)) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = "right"
  ) +
  ggtitle("Gamma Probabilities of Documents across Topics (Heatmap)")



# Load necessary libraries
library(ggplot2)

# Assuming you have the 'funding_gamma' dataframe with columns: 'topic', 'document', 'gamma'

# Creating a sample dataframe (replace this with your actual data)
funding_gamma <- data.frame(
  topic = rep(1:5, each = 5),
  document = rep(letters[1:5], times = 5),
  gamma = runif(25)
)

# Bar plot to visualize the gamma probabilities across topics for each document
ggplot(funding_gamma, aes(x = topic, y = gamma, fill = document)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#1F78B4", "#33A02C", "#FF7F00", "#E31A1C", "#6A3D9A")) +
  theme_minimal() +
  labs(x = "Topic", y = expression(gamma), fill = "Document") +
  ggtitle("Gamma Probabilities of Documents across Topics (Bar Plot)")



# Load necessary libraries
library(ggplot2)

# Assuming you have the 'lda_gamma' and 'nsf_funding_join' dataframes

# Merging dataframes
funding_gamma <- merge(lda_gamma, nsf_funding_join, by = "document", all.x = TRUE)
funding_gamma <- subset(funding_gamma, select = -c(id, abstract))

# Bar plot to visualize the gamma probabilities across topics for each document
ggplot(funding_gamma, aes(x = topic, y = gamma, fill = document)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#1F78B4", "#33A02C", "#FF7F00", "#E31A1C", "#6A3D9A")) +
  theme_minimal() +
  labs(x = "Topic", y = expression(gamma), fill = "Document") +
  ggtitle("Gamma Probabilities of Documents across Topics (Bar Plot)")


#####

library(topicmodels)
library(tidytext)
library(magrittr)
library(dplyr)
library(slam)
library(wordcloud)
library(ggplot2)
library(LDAvis)

required_libraries <- c("topicmodels", "slam", "LDAvis")
loaded_libraries <- lapply(required_libraries, function(lib) lib %in% installed.packages())

if (!all(loaded_libraries)) {
  # Install and load missing libraries
  install.packages(required_libraries[!loaded_libraries])
  sapply(required_libraries, require, character.only = TRUE)
}


# Perform LDA with a specified number of topics
NumTopics <- 21
desc_lda <- LDA(desc_dtm, method = "Gibbs", k = NumTopics, control = list(seed = 1234))
saveRDS(desc_lda, file = "desc_lda.rds")
tidy_lda <- tidy(desc_lda)

# Examine the top 10 terms for each topic
top_terms <- tidy_lda %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Visualize topics as word clouds and save as PNG files
tmResult <- posterior(desc_lda)
for (TopicNum in 1:NumTopics) {
  set.seed(42)
  topicToViz <- TopicNum
  top100terms <- sort(tmResult$terms[topicToViz, ], decreasing = TRUE)[1:100]
  words <- names(top100terms)
  probabilities <- sort(tmResult$terms[topicToViz, ], decreasing = TRUE)[1:100]
  mycolors <- brewer.pal(8, "Dark2")
  
  # Save the word cloud to a PNG file
  png(paste0("WordCloud", TopicNum, ".png"), width = 1280, height = 800)
  wordcloud(words, probabilities, scale = c(4, .2), random.order = FALSE, colors = mycolors)
  dev.off()
}

# Save the top terms in each LDA topic as a histogram in PNG format
png("TermsLDAHistogram.png", width = 1280, height = 1600)
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  group_by(topic, term) %>%
  arrange(desc(beta)) %>%
  ungroup() %>%
  mutate(term = factor(paste(term, topic, sep = "__"), 
                       levels = rev(paste(term, topic, sep = "__")))) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  labs(title = "Top terms in each LDA topic",
       x = NULL, y = expression(beta)) +
  facet_wrap(~ topic, ncol = 3, scales = "free")
dev.off()
####

# Function to convert LDA model to LDAvis
topicmodels2LDAvis <- function(lda_model) {
  
  post <- topicmodels::posterior(lda_model)
  
  vocab <- colnames(post$terms)
  vocab <- vocab[!(is.na(vocab) | nchar(vocab) == 0)]
  
  mat <- lda_model@wordassignments
  
  # Calculate term frequencies
  term_freq <- colSums(mat)
  
  # Ensure vocab and term freq match 
  if(length(vocab) != length(term_freq)) {
    stop("Vocab and term frequencies must have same length") 
  }
  
  LDAvis::createJSON(
    phi = post$terms[, vocab, drop = FALSE],  
    theta = post$topics,
    vocab = vocab,
    doc.length = rowSums(mat, na.rm = TRUE),
    term.frequency = term_freq
  )
  
}

# Usage
lda_json <- topicmodels2LDAvis(desc_lda)
#### 12

topicmodels2LDAvis <- function(x, ...) {
  post <- topicmodels::posterior(x)
  if (ncol(post[["topics"]]) < 3) stop("The model must contain > 2 topics")
  
  vocab <- colnames(post[["terms"]])
  if (any(is.na(vocab)) || any(nchar(vocab) == 0)) {
    vocab <- vocab[!(is.na(vocab) | nchar(vocab) == 0)]  # Remove NA and zero-length terms
  }
  
  mat <- x@wordassignments
  term_freq <- slam::col_sums(mat, na.rm = TRUE)
  term_freq <- term_freq[names(term_freq) %in% vocab]  # Filter term frequencies for existing vocab
  
  LDAvis::createJSON(
    phi = post[["terms"]][, vocab, drop = FALSE], 
    theta = post[["topics"]],
    vocab = vocab,
    doc.length = slam::row_sums(mat, na.rm = TRUE),
    term.frequency = term_freq
  )
}

lda_json <- topicmodels2LDAvis(desc_lda)

