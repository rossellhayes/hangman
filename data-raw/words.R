library(dplyr)
library(readr)

words <- read_delim(
  "http://corpus.leeds.ac.uk/frqc/internet-en.num", " ", skip = 4,
  col_names = c("rank", "freq", "word")
)

offensive <- read_lines(
  "https://www.cs.cmu.edu/~biglou/resources/bad-words.txt"
)

words <- words[1:5000, ] %>%
  filter(!grepl("[^a-z]", word), nchar(word) > 3, !word %in% offensive)

hangman_word_list <- words$word

usethis::use_data(hangman_word_list, internal = TRUE, overwrite = TRUE)
