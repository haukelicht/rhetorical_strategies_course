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

# read and parse the responses ----

fp <- file.path(data_path, "responses.csv")

header <- read_csv(fp, n_max = 0)
header <- names(header)

df <- read_csv(fp, skip = 3, col_names = header)

df <- df |> 
  select(response_id = ResponseId, starts_with("valence-"), starts_with("arousal-")) |> 
  pivot_longer(cols = -response_id, names_to = c("dimension", "word"), names_sep = "-") |>
  pivot_wider(names_from = dimension, values_from = value) 

# compute averages ----
means <- df |> 
  group_by(word) |> 
  summarise(across(c(valence, arousal), ~mean(., na.rm = TRUE)))

hist(means$valence)
hist(means$arousal)

# cross-validate ----

anew <- read_tsv(file.path(data_path, "words_sample.tsv"), col_select = 1:3)

merged <- left_join(anew, means, by = "word") 

cor.test(merged$valmn, merged$valence)
cor.test(merged$aromn, merged$arousal)

