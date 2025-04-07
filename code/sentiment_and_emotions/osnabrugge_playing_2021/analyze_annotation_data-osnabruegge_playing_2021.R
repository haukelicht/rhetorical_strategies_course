# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #  
#
#' @title  Analyze human codings of sample of speeches in corpus from 
#'          paper Osnabrügge et al. (2021) "Playing to the Gallery: Emotive 
#'          Rhetoric in Parliaments", APSR, DOI: 10.1017/S0003055421000356
#' @author Hauke Licht
#' @date   2023-11-15
#
# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #

# setup ----

# load required packages
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
theme_set(theme_linedraw())
library(irr)
library(broom)

data_path <- file.path("data", "osnabruegge_playing_2021")

# read data ---- 

# # Code to generate anonymized responses
# fp <- file.path(data_path, "sample_speeches_qualtrix", "responses.csv")
# col_names <- names(read_csv(fp, n_max = 1))
# resp <- read_csv(fp, skip = 3, col_names = col_names)
# fp <- file.path(data_path, "sample_speeches_qualtrix", "responses_anonymous.csv")
# resp |> 
#   select(
#     -RESPONDENT
#     , -IPAddress
#     , -starts_with("Recipient")
#     , -starts_with("Location")
#   ) |> 
#   write_csv(fp)

# read anonymized responses
fp <- file.path(data_path, "sample_speeches_qualtrix", "responses_anonymous.csv")
resp <- read_csv(fp)

# create a "mapping" of answer categories to numeric codes
answers_codes <- c(
  "very neutral" = -2,
  "somehwat neutral" = -1,
  "about as neutral as emotive" = 0,
  "somehwat emotive" = 1,
  "very emotive" = 2
)

# assess inter-coder agreement ----

resp_matrix <- resp |> # <== note: we use the native R pipe (instead of dplyr's %>%)
  # select all columns that start with "SPEECH" (record the emotiveness judgments)
  select(starts_with("SPEECH")) |> 
  # make columns character (instead of factor)
  mutate_all(as.character) |> 
  # replace answers with numeric codes
  mutate_all(~answers_codes[.]) |> 
  # convert to matrix
  as.matrix()

# remove speeches without judgments
resp_matrix <- resp_matrix[, colMeans(!is.na(resp_matrix)) > 0]

# compute Krippendorff's alpha
irr::kripp.alpha(resp_matrix, method = "ordinal")
# note: ideally should be ≥ 0.65

# reshape ratings into desired format ----

ratings <- resp |> 
  # select response ID and all columns that record coders' emotiveness judgments of speeches
  select(response_id = ResponseId, starts_with("SPEECH")) |> 
  # reshape from wide to long format
  pivot_longer(-1) |> 
  # discard cases of speeches a coder has not judged
  filter(!is.na(value)) |> 
  mutate(
    # extract speech ID from column names
    id_speech = as.double(sub("SPEECH", "", name))
    , name = NULL
    # convert answer categories to numeric codes
    , rating = answers_codes[value]
  ) |> 
  # discard cases of speeches a coder has not judged
  filter(!is.na(rating)) 

# aggregate ----

mean_ratings <- ratings |> 
  # group by speech ID
  group_by(id_speech) |> 
  # compute mean and SD of numeric ratings
  summarise(
    rating_mean = mean(rating)
    , rating_sd = sd(rating)
    , n_judgments = n()
  )

# combine with dictionary-based scores ----

# read the data with the dictionary-based scores
fp <- file.path(data_path, "sample_speeches_qualtrix.rds")
sampled <- read_rds(fp)

# combine them with the human-rating data
combined <- right_join(sampled, mean_ratings, by = "id_speech")

# analyze ----

# Q: how do their scores correlate with our judgments
with(combined, cor.test(emotive_rhetoric, rating_mean))
# A: correlation weak to modest

# NOTE: but mainly driven by positive association in subset of PMQ speeches
m <- lm(emotive_rhetoric ~ rating_mean:debate_type, data = combined)
broom::tidy(m, conf.int = TRUE)

# Q: would their finding replicate with human codings?
combined |> 
  ggplot(aes(x = rating_mean, y = reorder(debate_type, rating_mean))) +
  geom_boxplot() +
  labs(y = NULL, x = "level of emotiveness (average of human judgments)")
# A: we find a similar tendency, but ...
m <- lm(rating_mean ~ debate_type, data = combined)
broom::tidy(m, conf.int = TRUE)
# A: ... our N is too small (all confidence intervalls are wide and include 0)

