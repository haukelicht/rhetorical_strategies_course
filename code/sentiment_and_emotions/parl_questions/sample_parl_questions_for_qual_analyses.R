# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #  
#
#' @title  Annotate speeches in UK House of Commons question times sessions with
#'          ANEW dictionary keywords
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
# renv::install("quanteda@4.2.0")
library(quanteda)

data_path <- file.path("data")

# read the data ----

fp <- file.path(data_path, "attacks", "poljak_2022_attack_data.tsv")

df <- read_tsv(fp)

df <- df |> 
  filter(country == "UK", lang == "en") |> 
  select(-text_mt_google_old) 


{
set.seed(1234)
sampled <- df |> 
  # count(attack_binary, policy_attack, trait_attack, incivility) |> arrange(n)
  group_by(attack_binary, policy_attack, trait_attack, incivility) |> 
  sample_n(50) |> 
  ungroup() |> 
  sample_frac(1.0)
}

toks <- tokens(sampled$text, remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE)

# apply ANEW ----

fp <- file.path(data_path, "dictionaries", "anew", "ANEW2010All.txt")
anew <- read_tsv(fp, col_select = c(1, 3))
names(anew) <- tolower(names(anew))

# get list of ANEW words contained in each speech
anew_dict <- dictionary(as.list(setNames(paste0(anew$word), anew$word)))
matched <- tokens_lookup(toks, anew_dict, valuetype = "fixed", case_insensitive = TRUE)

anew_scores <- with(anew, setNames(valmn, word))

anew_scores[matched[[1]]]

sampled$scores <- map(matched, function(x, n) anew_scores[x])

sampled$score <- map_dbl(sampled$scores, ~mean(.x, na.rm = TRUE))

sampled |> 
  filter(speech_id == 21424) |> 
  select(text, scores, score)

hist(sampled$score)
  
# sample speeches ----

# apply ANEW ----

col_palette <- RColorBrewer::brewer.pal(11, "PiYG") 
# col_palette <- rev(col_palette)
anew_colors <- col_palette[as.integer(cut(anew_scores, breaks = 11))]
names(anew_colors) <- names(anew_scores)

css_style_fmr <- paste(
  "color: %s",
  "font-weight: 700",
  "text-shadow: -1px -1px 0 #000, 1px -1px 0 #000, -1px 1px 0 #000, 1px 1px 0 #000;",
  sep = "; "
)
  
anew_color_annotations <- sprintf(
  '<span style="color: %s; font-weight: 900;">%s</span>',
  # sprintf('<span style="%s">%%s</span>', css_style_fmr), 
  unname(anew_colors), 
  names(anew_colors)
)
names(anew_color_annotations) <- names(anew_scores)

speech_css <- paste(
  "background-color: #eab676",
  # "margin: 3ex",
  "padding: 3ex",
  "border: 1px solid black;",
  sep = "; "
)

{
set.seed(1234)
examples  <- sampled |> 
  filter(!is.na(score)) |>
  group_by(cut(score, 10)) |>
  sample_n(2) |>
  ungroup()
}

matched <- examples$text |> 
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) |> 
  tokens_lookup(anew_dict, valuetype = "fixed", case_insensitive = TRUE, append_key = TRUE)

htmls <- character()
for (i in 1:nrow(examples)) { # i = 1
  speech.id <- as.character(examples$speech_id[i])
  speech <- examples$text[i]
  score <-  examples$score[i]
  m <- matched[[i]]
  
  m <- unique(m)
  m <- strsplit(m, "/")
  
  repl <- anew_color_annotations[map_chr(m, 2)] # , map_chr(m, 1))
  repl <- str_replace_all(repl, setNames(map_chr(m, 1), paste0("(?<=>)", map_chr(m, 2), "(?=<)")))
  names(repl) <- paste0("\\b", map_chr(m, 1), "\\b")
  
  html <- stringr::str_replace_all(speech, repl)
  html <- paste0(
    sprintf("<hr>\n<p><b>Speech ID:</b> %s (score: %0.3f)</p>", speech.id, score), 
    sprintf('\n<div style="%s">\n', speech_css), 
    "<p>", html, "</p>\n",
    "</div><br/>\n\n"
  )
  
  htmls[speech.id] <- html
}

content <- markdown::markdownToHTML(paste(htmls, collapse = "\n"))

write_lines(content, "sampled_speeches.html")
  
