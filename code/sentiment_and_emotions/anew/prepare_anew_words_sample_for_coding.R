# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #  
#
#' @title  Prepare words in ANEW dictionary for manual subjective coding
#' @author Hauke Licht
#' @date   2025-03-32
#
# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #

# setup ----

# load required packages
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(stringr)

data_path <- file.path("data", "dictionaries", "anew")

# read the data ----

fp <- file.path(data_path, "words_sample.tsv")

df <- read_tsv(fp)

# format for qualtrix ----

# https://www.qualtrics.com/support/survey-platform/survey-module/survey-tools/import-and-export-surveys/#PreparingAnAdvancedFormatTXTFile

create_single_choice_q <- function(question.text, choice.options, question.id=NULL, vertical=TRUE) {
  
  header <- "[[Question:MC:SingleAnswer]]"
  if (!vertical)
    header <- sub("\\]{2}$", ":Horizontal]]", header)
  
  if (!is.null(question.id)) {
    qid <- sprintf("[[ID:%s]]", as.character(question.id))
    header <- paste(header, qid, sep = "\n")
  }
  
  if (is.null(choice.options)) {
    options <- paste("[[Choices]]", choice.options, sep = "\n")
  } else {
    options <- paste(
      sprintf("[[Choice:%s]]", names(choice.options)), 
      unname(choice.options), 
      sep = "\n", 
      collapse = "\n"
    )
    options <- paste("\n[[AdvancedChoices]]", options, sep = "\n")
  }
  
  q_string <- paste(header, question.text, options, sep = "\n")
  
  return(q_string)
}

valence_choice_options <- setNames(rep(" ", 9), -4:4)
valence_choice_options["-4"] <- "extremeley negative"
valence_choice_options["4"] <- "extremeley positive"

arousal_choice_options <- setNames(rep(" ", 9), 0:8)
arousal_choice_options["0"] <- "extremeley calm/subdue"
arousal_choice_options["8"] <- "extremeley aroused/excited"

valence_question <- "How would you rate the <b><em>valence</em></b> of the following word? &ldquo;<b>%s</b>&rdquo;"
arousal_question <- "How would you rate the <b><em>arousal</em></b> of the following word? &ldquo;<b>%s</b>&rdquo;"

valence_q <- create_single_choice_q(sprintf(valence_question, "test"), valence_choice_options, question.id = "valence", vertical=FALSE)

word_to_questions <- function(word) {
  valence_q <- create_single_choice_q(sprintf(valence_question, word), valence_choice_options, question.id = paste0("valence-", word), vertical=FALSE)
  arousal_q <- create_single_choice_q(sprintf(arousal_question, word), arousal_choice_options, question.id = paste0("arousal-", word), vertical=FALSE)
  
  sprintf("%s\n\n\n%s\n\n[[PageBreak]]", valence_q, arousal_q)
}

convert_to_qualtrix_format <- function(words, instructions=NULL) {
  qs <- map_chr(words, word_to_questions)
  qs <- paste(qs, collapse = "\n\n\n")
  
  if (is.null(instructions)) {
    cont <- paste("[[AdvancedFormat]]", qs, sep = "\n\n\n")
  } else {
    instructions <- paste("[[Block:Instructions]]\n\n[[Text]]", instructions, "[[PageBreak]]", sep = "\n\n")
    cont <- paste("[[AdvancedFormat]]", instructions, qs, sep = "\n\n\n")    
  }
  return(cont)
}

txt <- convert_to_qualtrix_format(df$word)

fp <- file.path(data_path, "words_sample_qualtrix.txt")
if (!file.exists(fp))
  write_lines(txt, fp, sep = "\n\n")






instructions <- "
On the following pages you will be presented with **words in the ANEW dictionary**, a linguistic resource for dictionary-based sentiment and emotion analysis.
  
Please rate the valence and arousal of each word.
  
- **Valence** refers to the _positive_ or _negative_ nature of the word. A low score indicates an extremely negative word, while a high score indicates an extremely positive word.
- **Arousal** refers to the _calm_ or _excited_ nature of the word. A low score indicates an extremely calm word, while a high score indicates an extremely excited word.

Please rate each word based on your subjective interpretation of the word's meaning and connotation. 
If you are unsure about the meaning of a word, please look up the word in an online lexicon (e.g., [_Cambridge Dictionary_](https://dictionary.cambridge.org/)).
"

cat(markdown::markdownToHTML(instructions, fragment.only = TRUE))


