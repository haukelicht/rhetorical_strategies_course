# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #  
#
#' @title  Illustration of how to download a corpus in the ParlSpeech2 dataverse
#' @author Hauke Licht
#' @date   2023-11-02
#' @note   The dataverse is here: https://dataverse.harvard.edu/dataverse/ParlSpeech
#
# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #

# setup ----

# load required packages
library(readr)
library(purrr)
library(dataverse)

Sys.setenv("DATAVERSE_SERVER" = "dataverse.harvard.edu")
Sys.setenv("DATAVERSE_ID" = "ParlSpeech") # set to ParlSpeech dataverse

# download ParlEE UK corpus data ----

# we work with the V2 dataset that records UK parl speeches

# URL: https://doi.org/10.7910/DVN/L4OAKN
repo_doi <- "doi:10.7910/DVN/L4OAKN"

# get dataverse information
files <- dataset_files(repo_doi, version = "1.0")

# get the names of the files in the dataverse
(file_names <- map_chr(files, "label"))

# get UK corpus (this will take some time to download)
(uk_data_file <- files[[grep("HouseOfCommons", file_names)]])
speech_corpus <- get_dataframe_by_id(uk_data_file$dataFile$id, .f = read_rds)
