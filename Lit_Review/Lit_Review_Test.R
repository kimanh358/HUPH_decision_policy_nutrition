# Load required libraries
library(stringr)
library(dplyr)

# Paste or load your text
text <- readLines("Kim_Anh_Lit_Review.txt")  # or replace with a character string

# Combine lines into a single string
text_combined <- paste(text, collapse = " ")

# Extract in-text citations like (Author et al., 2020)
citations_paren <- str_match_all(text_combined, "\\(([^()]+? et al\\., \\d{4})\\)")[[1]][,2]

# Extract in-text citations like Author et al. (2020)
citations_inline <- str_match_all(text_combined, "([A-Z][a-z]+ et al\\.) \\((\\d{4})\\)")[[1]]
citations_inline_combined <- paste(citations_inline[,2], citations_inline[,3], sep = ", ")

# Combine and clean
all_citations <- c(citations_paren, citations_inline_combined)
all_citations <- str_trim(all_citations)
length(all_citations)

# Count unique citations
citation_counts <- table(all_citations)
sorted_counts <- sort(citation_counts, decreasing = TRUE)
sorted_counts
