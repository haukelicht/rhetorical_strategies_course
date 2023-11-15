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

fp <- file.path(data_path, "sample_speeches_qualtrix", "responses_anonymous.csv")
resp <- read_csv(fp)

answers_codes <- c(
  "very neutral" = -2,
  "somehwat neutral" = -1,
  "about as neutral as emotive" = 0,
  "somehwat emotive" = 1,
  "very emotive" = 2
)

# assess inter-coder agreement ----

resp_matrix <- resp |> 
  select(starts_with("SPEECH")) |> 
  mutate_all(as.character) |> 
  mutate_all(~answers_codes[.]) |> 
  as.matrix()

# remove speeches without judgments
resp_matrix <- resp_matrix[, colMeans(!is.na(resp_matrix)) > 0]

# compute Krippendorff's alpha
irr::kripp.alpha(resp_matrix, method = "ordinal")
# note: should be ≥ 0.65

# reshape in desired format ----

ratings <- resp |> 
  select(response_id = ResponseId, starts_with("SPEECH")) |> 
  pivot_longer(-1) |> 
  filter(!is.na(value)) |> 
  mutate(
    id_speech = as.double(sub("SPEECH", "", name))
    , name = NULL
    , rating = answers_codes[value]
  ) |> 
  filter(!is.na(rating)) 

# aggregate ----

mean_ratings <- ratings |> 
  group_by(id_speech) |> 
  summarise(
    rating_mean = mean(rating)
    , rating_sd = sd(rating)
    , n_judgments = n()
  )

# combine with dictionary-based scores ----

fp <- file.path(data_path, "sample_speeches_qualtrix.rds")
sampled <- read_rds(fp)                

combined <- right_join(sampled, mean_ratings, by = "id_speech")

# analyze ----

# Q: how do their scores correlate with our judgments
with(combined, cor.test(emotive_rhetoric, rating_mean))
# correlation: weak to modest
# but mainly driven by positive association in subset of PMQ speeches
summary(lm(emotive_rhetoric ~ rating_mean:debate_type, data = combined))

# Q: would their finding replicate with human codings?
# maybe, but our N is too small
summary(lm(rating_mean ~ debate_type, data = combined))
combined |> 
  ggplot(aes(x = rating_mean, y = reorder(debate_type, rating_mean))) +
  geom_boxplot() +
  labs(y = NULL, x = "level of emotiveness (average of human judgments)")


