# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #  
#
#' @title  Sample wors from ANEW dictionary for manual subjective coding
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

fp <- file.path(data_path, "ANEW2010All.txt")

# read only columns word, mean valence rating and mean arousal rating 
df <- read_tsv(fp, col_select= c(1, 3, 5))

# bin mean ratings into five quantiles each
df <- df |> 
  rename_all(tolower) |>
  mutate(ntile = across(2:3, ~ntile(., 5))) |> 
  unnest_wider(ntile, names_sep = "_")

hist(df$valmn); range(df$valmn)
hist(df$aromn); range(df$aromn)

# distribution across combinations
with(df, table(ntile_valmn, ntile_aromn)) |> prop.table() |> round(2)

# sample 50 words ----

# sample 2 words from each combination
{
  set.seed(1234)
  sampled <- df |> 
    group_by(ntile_valmn, ntile_aromn) |> 
    sample_n(2) |> 
    ungroup() |> 
    sample_frac(1.0) 
}
  
sampled

# write to file ----

fp <- file.path(data_path, "words_sample.tsv")
write_tsv(sampled, fp)
