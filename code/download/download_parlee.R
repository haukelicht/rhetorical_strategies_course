# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #  
#
#' @title  Illustration of how to download a corpus in the ParlEE dataverse
#' @author Hauke Licht
#' @date   2023-11-02
#' @note   The dataverse is here: https://dataverse.harvard.edu/dataverse/ParlEE
#
# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #

# setup ----

# load required packages
library(readr)
library(purrr)
library(dataverse)

Sys.setenv("DATAVERSE_SERVER" = "dataverse.harvard.edu")
Sys.setenv("DATAVERSE_ID" = "ParlEE") # set to ParlEE dataverse

# download ParlEE UK corpus data ----

# we work with the V1 dataset that records UK parl speeches

# URL: https://doi.org/10.7910/DVN/ZY3RV7
repo_doi <- "doi:10.7910/DVN/ZY3RV7"

# get dataverse information
files <- dataset_files(repo_doi, version = "1.0")

# get the names of the files in the dataverse
file_names <- map_chr(files, "label")

# get UK corpus
(uk_data_file <- files[[grep("UK", file_names)]])
speech_corpus <- get_dataframe_by_id(uk_data_file$dataFile$id, .f = read_csv)
