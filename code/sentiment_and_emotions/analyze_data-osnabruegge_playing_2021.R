# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #  
#
#' @title  Analyze data from Osnabrügge et al. (2021) "Playing to the Gallery: 
#'          Emotive Rhetoric in Parliaments", APSR, DOI: 10.1017/S0003055421000356
#' @author Hauke Licht
#' @date   2023-11-01
#' @note   This R script is mainly for illustrative purposes proving course
#'          participants with some helpful code snippets they might want
#'          to build on in their term papers.
#'         I reproduce some of the analyses and figures reported in the main 
#'          paper but not the actual word embedding and dictionary-based 
#'          measurement procedure.
#'         For these measurement aspects, it's likely better to consult the 
#'          replication materials of some of required readings for the session
#'          on sentiment and emotion measurement methods (e.g., Cochrane et 
#'          al. 2022, Widmann & Wich 2022, van Atteveldt et al. 2021, etc.)
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
library(dataverse)
library(ggplot2)
library(ggridges)
theme_set(theme_linedraw())

Sys.setenv("DATAVERSE_SERVER" = "dataverse.harvard.edu")

# download the data ----

Sys.setenv("DATAVERSE_ID" = "the_review") # set to APSR dataverse

## get files in dataverse
repo_doi <- "doi:10.7910/DVN/QDTLYV"
files <- dataset_files(repo_doi, version = "1.0")
file_names <- map_chr(files, "label")

## get 'uk_data.csv'
uk_data_file <- files[[which(file_names == "uk_data.csv")]]
df <- get_dataframe_by_id(uk_data_file$dataFile$id, .f = read_csv)
table(is.na(df$id_mp))

## emotive keywords
emotive_keywords_file <- files[[which(file_names == "emotive_uk.tab")]]
emotive_keywords <- get_dataframe_by_id(emotive_keywords_file$dataFile$id, .f = read_lines)
emotive_keywords <- gsub('"', "", emotive_keywords)

## neutral keywords
neutral_keywords_file <- files[[which(file_names == "neutral_uk.tab")]]
neutral_keywords <- get_dataframe_by_id(neutral_keywords_file$dataFile$id, .f = read_lines)
neutral_keywords <- gsub('"', "", neutral_keywords)

# clean the data ----

## get speaker IDs and fix some issues in the dataset
speakers <- df |> 
  filter(!is.na(id_mp)) |> 
  distinct(id_mp, last_name, first_name)

## check if speakers with `NA` IDs can be fixed
speakers_with_na_ids <- df |> 
  filter(is.na(id_mp)) |> 
  distinct(last_name, first_name)
speakers_with_na_ids |> 
  left_join(speakers)
# no =(

# to fix this issue, we add artificial speaker IDs where missing
max_id_ <- max(speakers$id_mp, na.rm = TRUE)
speakers_with_na_ids$id_mp <- max_id_+1:nrow(speakers_with_na_ids)

# re-join with speakers data frame
speakers <- speakers |> 
  # discard (to make sure no duplicated)
  anti_join(speakers_with_na_ids) |> 
  # add (to make complete)
  bind_rows(speakers_with_na_ids) |> 
  arrange(id_mp)

# re-join with dataset
df <- df |> 
  # discard obs with missing speaker ID 
  filter(!is.na(id_mp)) |> 
  bind_rows(
    # add discarded obs with fixed speaker IDs 
    df |> 
      filter(is.na(id_mp)) |> 
      select(-id_mp) |> 
      left_join(speakers_with_na_ids)
  ) |> 
  # bring back to original order
  arrange(id_speech)

## add categorical debate type indicator
debate_types <- c(
  "pm_questions" = "PMQ",
  "m_questions" = "Minister question time",
  "u_questions" = "Urgent question",
  "queen_debate_day1" = "Queen's Speech debate (1st day)",
  "queen_debate_others" = "Queen's Speech debate (other day)",
  "other" = "other debate"
)

df$debate_type <- with(
  df,
  factor(case_when(
    pm_questions == 1 ~ debate_types[1],
    m_questions == 1 ~ debate_types[2],
    u_questions == 1 ~ debate_types[3],
    queen_debate_day1 == 1 ~ debate_types[4],
    queen_debate_others == 1 ~ debate_types[5],
    other_debate == 1 ~ debate_types[6],
    TRUE ~ NA
  ), levels = debate_types, labels = debate_types)
)
df$debate_type <- relevel(df$debate_type, debate_types[6])

# illustration: how to apply the dictionary ----

# let's take the example of the first sentence listed in the paper's Table 2 
idx <- grep("vil happens when good people stand by and do nothing", df$text)
example <- df$text[idx]

# split into words at white space
words <- str_split(example, pattern = " ")[[1]]
# important: lowercase words
(words <- str_to_lower(words))
# remove words with 2 or fewer characters
words <- words[nchar(words) != 2]
# remove "stop words" (CAVEAT: unlcear which stopwords list they have used)
stopwords <- c("the", "and", "but", "for", "there", "i", "about", "when", "them", "are") # !?!?
words <- words[!words %in% stopwords]
length(words)

# words in neutral keywords
count_neutral <- sum(words %in% neutral_keywords)
share_neutral <- mean(words %in% neutral_keywords)
# words in emotive keywords
count_emotive <- sum(words %in% emotive_keywords)
share_emotive <- mean(words %in% emotive_keywords)

# final score 
length(words); count_neutral; count_emotive
df[idx, c("text", "words", "neutral_count", "emotive_count", "emotive_rhetoric")]

# IMPORTANT: unclear how they arrive at the 'emotive_rhetoric' score
#  In the paper, they say "We compute the level of emotive rhetoric by 
#   subtracting the percentage of neutral from the percentage of emotive 
#   words" (p. 890f.). But this doesn't check out:
(share_emotive-share_neutral)*100

# renv::install("stopwords")

# note: compare to replication data

# analyze/visualize ----

# emotiveness time seris by debate type
df |>
  # sample obs to speed up plotting
  group_by(
    debate_type
    , halfyear = sprintf("%d-%d", year(date), as.integer(month(date) > 6))
  ) |>
  summarise(emotive_rhetoric = mean(emotive_rhetoric)) |> 
  ggplot(aes(x = halfyear, y = emotive_rhetoric, group = debate_type, color = debate_type)) +
  geom_smooth(span = .4, se = FALSE) +
  scale_x_discrete(
    breaks = paste0(seq(2001, 2019, 2), "-0")
    , labels = seq(2001, 2019, 2)
  ) + 
  labs(
    x = "Halfyear"
    , y = "Average of emotiveness scores"
    , color = NULL
  )


# distribution of emotiveness scores by debate type
df |> 
  ggplot(aes(x = emotive_rhetoric, y = reorder(debate_type, emotive_rhetoric))) +
  geom_density_ridges(scale = .5, size = 0.25) +
  # # limit to [-50, 50] interval  
  # xlim(-50, 50) +
  geom_point(
    data = df |> 
      group_by(debate_type) |> 
      summarise(emotive_rhetoric = mean(emotive_rhetoric))
    , pch = "|"
    , color = "red" # <== red dash shows average
    , size = 4
  ) +
  labs(
    x = "emotive rhetoric"
    , y = NULL
  )
# note: the mean differences are super tiny (see regression below)
summary(feols(emotive_rhetoric ~ debate_type | id_mp, df, cluster = ~id_mp))


# relation between number of words in speech and emotiveness score
set.seed(1234)
df |>
  # sample obs to speed up plotting
  sample_frac(0.1) |> 
  ggplot(aes(x = words, y = emotive_rhetoric)) +
    geom_point(size = 0.1, alpha = 0.25) +
    scale_x_log10() +
    labs(
      y = "emotive rhetoric"
      , x = "# words"
    )
# note: shorter speeches are assigned more extreme values
summary(lm(log(words) ~ emotive_rhetoric + I(emotive_rhetoric^2), df))
# see: https://stats.stackexchange.com/a/25976



# distribution of emotiveness scores by topic
df |> 
  ggplot(aes(x = emotive_rhetoric, y = reorder(top_topic, emotive_rhetoric))) +
    geom_density_ridges(scale = .5, size = 0.25) +
    # xlim(-10, 10) +
    geom_point(
      data = df |> 
        group_by(top_topic) |> 
        summarise(emotive_rhetoric = mean(emotive_rhetoric))
      , pch = "|"
      , color = "red" # <== red dash shows average
      , size = 4
    ) +
    labs(
      x = "emotive rhetoric"
      , y = NULL
    )

# note: the mean differences are super tiny (see regression below)
summary(feols(emotive_rhetoric ~ top_topic | id_mp, df, cluster = ~id_mp))

# replicate the regression analyses ----

# translated from stata (see https://dataverse.harvard.edu/file.xhtml?fileId=4551016&version=1.0)

library(sandwich)
library(lmtest)
library(fixest)
library(broom)

## Models in Table 3 ----
m1 <- lm(
  emotive_rhetoric ~
   + pm_questions
   + queen_debate_day1
   + queen_debate_others
   + m_questions 
   + u_questions 
  , data = df
)
m1_coefs <- coeftest(m1, vcov = vcovCL, type = "HC1", cluster = ~id_mp)

m2 <- lm(
  emotive_rhetoric ~
   + pm_questions
   + queen_debate_day1
   + queen_debate_others
   + m_questions 
   + u_questions 
   + linear_trend
  , data = df
)
m2_coefs <- coeftest(m2, vcov = vcovCL, type = "HC1", cluster = ~id_mp)

m3 <- feols(
  emotive_rhetoric ~
   + pm_questions
   + queen_debate_day1
   + queen_debate_others
   + m_questions 
   + u_questions 
   + linear_trend
   | id_mp # <== MP fixed effects
  , data = df
  , cluster = ~id_mp
)
summary(m3)

m4 <- feols(
  emotive_rhetoric ~
    + pm_questions
    + queen_debate_day1
    + queen_debate_others
    + m_questions 
    + u_questions 
    + linear_trend
    + leader
    + prime_minister
    + senior_minister
    + shadow
    + cabinet
    + chair
    + government
    + female
    + age
    + electoral_cycle
    | party # <== party fixed effects
  , data = df
  , cluster = ~id_mp
)
summary(m4)

m5 <- feols(
  emotive_rhetoric ~
    + pm_questions
    + queen_debate_day1
    + queen_debate_others
    + m_questions 
    + u_questions 
    + linear_trend
    | id_mp # <== MP fixed effects
  , data = df
  , cluster = ~id_mp
  , weights = ~words
)
summary(m5)

these_vars <- names(m1$coefficients)[-1]

get_estimates <- function(model) {
  tab <- tidy(model, conf.int = TRUE)
  transmute(tab, term, estimate = sprintf("%+.03f [%+.03f, %+.03f]", estimate, conf.low, conf.high))
}

res <- bind_rows(
  "m1" = get_estimates(m1),
  "m2" = get_estimates(m2),
  "m3" = get_estimates(m3),
  "m4" = get_estimates(m4),
  "m5" = get_estimates(m5),
  .id = "model"
)

res_tab <- res |> 
  filter(term %in% c("(Intercept)", names(debate_types))) |> 
  pivot_wider(names_from = "model", values_from = "estimate") |> 
  mutate(
    term = factor(
      term
      , levels = c(names(debate_types), "(Intercept)")
      , labels = c(debate_types, "Constant")
    )
  ) |> 
  arrange(term)

res_tab
  
