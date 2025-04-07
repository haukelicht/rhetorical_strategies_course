# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #  
#
#' @title  Sample speeches in corpus from paper Osnabrügge et al. (2021) 
#'          "Playing to the Gallery: Emotive Rhetoric in Parliaments", 
#'          APSR, DOI: 10.1017/S0003055421000356
#' @author Hauke Licht
#' @date   2023-11-01
#
# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #

# setup ----

# load required packages
library(readr)
library(dplyr)
library(dataverse)


# download the data ----
Sys.setenv("DATAVERSE_SERVER" = "dataverse.harvard.edu")
Sys.setenv("DATAVERSE_ID" = "the_review") # set to APSR dataverse

## get files in dataverse
repo_doi <- "doi:10.7910/DVN/QDTLYV"
files <- dataset_files(repo_doi, version = "1.0")
file_names <- map_chr(files, "label")

## get 'uk_data.csv'
uk_data_file <- files[[which(file_names == "uk_data.csv")]]
df <- get_dataframe_by_id(uk_data_file$dataFile$id, .f = read_csv)
table(is.na(df$id_mp))

# sample for validation against human judgments  ----

## add categorical debate type indicator
debate_types <- c(
  "pm_questions" = "PMQ",
  "m_questions" = "Minister question time",
  "u_questions" = "Urgent question",
  "queen_debate_day1" = "Queen's Speech debate (1st day)",
  "queen_debate_others" = "Queen's Speech debate (other day)",
  "other" = "other debate"
)

words_range <- quantile(df$words, c(.4, .6)) # should be  [34, 48]

set.seed(1234)
sampled <- df |> 
  filter(words >= words_range[1], words <= words_range[2]) |> 
  filter(
    debate_type %in% debate_types[c("pm_questions", "m_questions", "queen_debate_day1", "queen_debate_others")]
  ) |> 
  group_by(debate_type) |> 
  mutate(
    tile_ = ntile(emotive_rhetoric, 10)
  ) |> 
  group_by(debate_type, tile_) |> 
  sample_n(3) |> 
  ungroup() |> 
  select(id_speech, text, emotive_rhetoric, debate_type) |> 
  sample_frac(1.0)

convert_to_qualtrix_format <- function(x) {
  # see https://www.qualtrics.com/support/survey-platform/survey-module/survey-tools/import-and-export-surveys/#PreparingAnAdvancedFormatTXTFile
  paste(
    paste0("SPEECH", x$id_speech, ": Please rate the level of emotiveness of the following speech - ", dQuote(x$text))
    , ""
    , "very neutral"
    , "somehwat neutral"
    , "about as neutral as emotive"
    , "somehwat emotive"
    , "very emotive"
    , sep = "\n"
  )
}

lines <- map_chr(split(sampled, 1:nrow(sampled)), convert_to_qualtrix_format)

fp <- file.path("data", "osnabruegge_playing_2021", "sample_speeches_qualtrix.rds")
if (!file.exists(fp))
  write_rds(sampled, fp)

fp <- file.path("data", "osnabruegge_playing_2021", "sample_speeches_qualtrix.txt")
if (!file.exists(fp))
  write_lines(lines, fp, sep = "\n\n")

