file.exists("wordcloud_exam.R")
file.exists("feedback.txt")

# These lines check whether the two files `wordcloud_exam.R` and `feedback.txt` exist in the current working directory.

# PART 1 — DATA PREPARATION (25 pts)

# 1. Set working directory
setwd("C:/Users/beryl/Desktop/DM")
#this is to know if we are already in our set working directory
cat("Working directory:", getwd(), "\n")

# 2. Read the file using readLines()
#reads the file line-by-line and returns a character vector, each element is a line from the file.
raw_lines <- readLines("feedback.txt", encoding = "UTF-8")
#removes empty lines (lines that are exactly the empty string), which avoids processing blank entries later.
raw_lines <- raw_lines[raw_lines != ""]

# 3. If feedback.txt contains "Category|Text", split it

#The grepl function  checks if the pipe character `|` appears in the line; the pipe is escaped as `\\|` because `|` is special in regex.
#the strsplit(line) splits the line into components around the pipe. `[[1]]` extracts the first element (the resulting vector of parts).
# concatenates everything after the first part into a single `text` string in case the text itself contained extra `|` characters.
#- If the pipe is not present:
#  - returns `category = NA` and the `text` as the original `line`.


# PART 1 — DATA PREPARATION (25 pts)
# 1. Set working directory
setwd("C:/Users/beryl/Desktop/DM")
cat("Working directory:", getwd(), "\n")

# 2. Read the file using readLines()
raw_lines <- readLines("feedback.txt", encoding = "UTF-8")
raw_lines <- raw_lines[raw_lines != ""]

# 3. If feedback.txt contains "Category|Text", split it

split_line <- function(line) {
  if (grepl("\\|", line)) {
    parts <- strsplit(line, "\\|")[[1]]
    return(list(category = parts[1], text = paste(parts[-1], collapse = "")))
  } else {
    return(list(category = NA, text = line))
  }
}

#`lapply(raw_lines, split_line)` applies the `split_line` function to every line in `raw_lines`, returning a list of lists (each element contains `category` and `text`).
# `sapply(parsed, `[[`, "category")` extracts the `category` element from each parsed list; similar for `text`.
#`data.frame(...)` builds a data frame `feedback_df` with two columns: `category` and `text`. Using `stringsAsFactors = FALSE` avoids automatic conversion of strings to factors (important for text processing).

parsed <- lapply(raw_lines, split_line)
feedback_df <- data.frame(
  category = sapply(parsed, `[[`, "category"),
  text     = sapply(parsed, `[[`, "text"),
  stringsAsFactors = FALSE
)

# 4. Create a Corpus from imported text

all_texts <- trimws(feedback_df$text) #This trim the trailing whitespaces each text entry
all_texts <- all_texts[all_texts != ""]#this one naman removes the empty string produced after trimming
library(tm) #text mining utilities (Corpus, TermDocumentMatrix, tm_map)
library(SnowballC)#word stemming utilities.
library(wordcloud) #produces word clouds.
library(RColorBrewer)#provides color palettes for plots.
# creates a volatile corpus (in-memory) from the character vector `all_texts`. The corpus is the structure used by `tm` for text processing.
corpus <- VCorpus(VectorSource(all_texts))


# 5. Clean the text with required steps
clean_corpus <- function(corp) {
  corp <- tm_map(corp, content_transformer(tolower))           
  corp <- tm_map(corp, removeNumbers)                          
  corp <- tm_map(corp, removePunctuation)                     
  corp <- tm_map(corp, removeWords, stopwords("english"))      
  corp <- tm_map(corp, stripWhitespace)                        
  corp <- tm_map(corp, stemDocument, language = "english")  
}
all_texts <- trimws(feedback_df$text)
all_texts <- all_texts[all_texts != ""]
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(gridExtra)
library(grid)

corpus <- VCorpus(VectorSource(all_texts))

# 5. Clean the text with required steps
clean_corpus <- function(corp) {
  corp <- tm_map(corp, content_transformer(tolower))           # For lowercase
  corp <- tm_map(corp, removeNumbers)                          # For remove numbers
  corp <- tm_map(corp, removePunctuation)                      # For remove punctuation
  corp <- tm_map(corp, removeWords, stopwords("english"))      # For remove stopwords
  corp <- tm_map(corp, stripWhitespace)                        # For strip whitespace
  corp <- tm_map(corp, stemDocument, language = "english")     # For stem words

  return(corp)
}

corpus_clean <- clean_corpus(corpus)

# PART 2 — WORD FREQUENCY ANALYSIS (25 pts)

# 1. Create a Term–Document Matrix

# creates a matrix-like object where rows are terms (words) and columns are documents. Each cell counts occurrences of the word in the document.
tdm <- TermDocumentMatrix(corpus_clean) 
# converts the `TermDocumentMatrix` object to a regular R matrix for easier numeric operations.
tdm_mat <- as.matrix(tdm)

# 2. Convert matrix to word frequency table
#computes the total occurrences of each word across all documents; resulting vector has names that are the words.
word_freqs <- rowSums(tdm_mat)
#orders words by their total frequency, highest first.
word_freqs <- sort(word_freqs, decreasing = TRUE)
#is a data frame with columns `word` and `freq` (integer), suitable for printing and plotting.

tdm <- TermDocumentMatrix(corpus_clean)
tdm_mat <- as.matrix(tdm)

# 2. Convert matrix to word frequency table
word_freqs <- rowSums(tdm_mat)
word_freqs <- sort(word_freqs, decreasing = TRUE)
df_freq <- data.frame(
  word = names(word_freqs),
  freq = as.integer(word_freqs),
  row.names = NULL,
  stringsAsFactors = FALSE
)


#This top 10 display the first 10 rows. 
top10 <- head(df_freq, 10)
print(top10)#and this prints the top10 in the console for you to see highlevel results


#InterPretation
 # "The most frequent terms indicate the dominant themes mentioned across the customer feedback."
 # #"High-frequency words suggest common topics such as service quality, staff behavior, or general experience."
 # "These terms help show what areas of the experience customers repeatedly highlight."
 # "Rare words, by contrast, may represent isolated or unique issues that occurred only once."
 # "Overall, word frequency analysis helps identify primary concerns and frequently mentioned topics."


# PART 3 — WORD CLOUD GENERATION (30 pts)
  
#`main_df` filters words that appear at least twice; this avoids cluttering the main cloud with extremely rare words.
# If the dataset is very small and no words have freq ≥ 2, the code falls back to using `df_freq` directly so the word cloud still runs.

# 3. Display the top 10 most frequent words
top10 <- head(df_freq, 10)
print(top10)

write.csv(top10, "top10_table.csv", row.names = FALSE)
dev.off()

# 5. Write 3–5 sentences interpreting the results
interpretation <- c(
  "The most frequent terms indicate the dominant themes mentioned across the customer feedback.",
  "High-frequency words suggest common topics such as service quality, staff behavior, or general experience.",
  "These terms help show what areas of the experience customers repeatedly highlight.",
  "Rare words, by contrast, may represent isolated or unique issues that occurred only once.",
  "Overall, word frequency analysis helps identify primary concerns and frequently mentioned topics."
)
writeLines(interpretation, "interpretation.txt")

# PART 3 — WORD CLOUD GENERATION (30 pts)

main_df <- df_freq[df_freq$freq >= 2, , drop = FALSE]
if (nrow(main_df) == 0) {
  main_df <- df_freq   # fallback if very small dataset
}

#png(...)` opens the graphics device for `wordcloud_exam.png`.
#set.seed(123)` ensures reproducible random placement of words in the cloud.
#`words` supplies the words to display.
#`freq` supplies sizes (relative to frequency).
#- `min.freq = 2` ensures only words appearing at least twice are plotted.
#allows many words
#places highest frequency words near the center.
# sets fraction of words rotated 90° (10%)
#controls the largest and smallest font sizes (relative values).
#chooses a qualitative color palette.
#writes the PNG file.

png("wordcloud_exam.png", width = 800, height = 600)
set.seed(123)
wordcloud(
  words = main_df$word,
  freq = main_df$freq,
  min.freq = 2,
  max.words = 1000,
  random.order = TRUE,
  random.order = FALSE,

  rot.per = 0.1,
  scale = c(4, 0.4),
  colors = brewer.pal(8, "Dark2")
)
dev.off()

# PART 4 — ADVANCED TASK (20 pts)

# 1. Identify least frequent words (frequency = 1)


min_freq_present <- min(df_freq$freq)


rare_all <- df_freq[df_freq$freq == min_freq_present, , drop = FALSE]

# Select ONLY 5
rare_df <- head(rare_all[order(rare_all$freq), ], 5)

if (nrow(rare_df) > 0) {
  

min_freq_present <- min(df_freq$freq)
rare_df <- df_freq[df_freq$freq == min_freq_present, , drop = FALSE]

if (nrow(rare_df) > 0) {
  # If the least frequent words have a frequency > 1, adjust min.freq
  plot_min_freq <- min_freq_present 
  
  message("Found ", nrow(rare_df), " words with the lowest frequency (freq=", plot_min_freq, "). Generating cloud...")
  
# 2. Create separate word cloud for this lowest frequency group

  png("wordcloud_rare.png", width = 800, height = 600)
  set.seed(321)
  wordcloud(
    words = rare_df$word,
    freq = rare_df$freq,

    min.freq = min_freq_present,
    max.words = 5,
    random.order = FALSE,
    rot.per = 0.1,
    scale = c(4, 0.5),
    colors = brewer.pal(8, "Set1")
  )
  dev.off()
  
  message("Successfully created 'wordcloud_rare.png' with EXACTLY 5 rarest words.")
  
} else {
  message("No rare words available.")
}

