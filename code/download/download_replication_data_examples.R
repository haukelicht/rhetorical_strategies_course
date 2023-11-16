# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #  
#
#' @title  Illustration of how to download replication data from 
#'          Harvard dataverse and github
#' @author Hauke Licht
#' @date   2023-11-16
#
# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #

# setup ----

library(readr)
library(dplyr)

# download from Harvard Dataverse -----

#' @note: Many replication materials for articles published in poltical science
#'    journals are available through *Harvard Dataverse*:
#'     https://dataverse.harvard.edu/dataverse/harvard
#'    
#'    Specifically, many journals have their own "dataverses". Here some:
#'      
#'      - _American Political Science Review_ (APSR): https://dataverse.harvard.edu/dataverse/the_review
#'      - _Political Analysis_: https://dataverse.harvard.edu/dataverse/pan
#'      - _The Journal of Politics_ (JOP): https://dataverse.harvard.edu/dataverse/jop
#'      - _Political Science Research & Methods_ (PSRM): https://dataverse.harvard.edu/dataverse/PSRM
#'      
#'    Note: in the URLs listed above, the last bit behind the last "/" is called
#'     "Dataverse ID" -- we need this to automatically download files from 
#'     a journals dataverse.

# Example 1: dowloading with the file URL ----

#' @example In what follows, we will use the example of the article 
#' 
#'    Bestvater, S., & Monroe, B. (2023). Sentiment is Not Stance: 
#'        Target-Aware Opinion Classification for Political Text Analysis. 
#'        _Political Analysis_, 31(2), 235-256.
#'       
#'    The repository is https://doi.org/10.7910/DVN/MUYYG4

### 1. locate the file we want to download ----

# 1) go to https://doi.org/10.7910/DVN/MUYYG4
# 2) in the "Files" panel, click "Tree" to view the files
# 3) in the data folder, find and click on the file 'WM_tweets_groundtruth.tab'
#    (the README.md files notes that these are the human-coded WomensMarch tweets)
# 3) on the files page, go to the "Metadata" tab
# 4) get the value in the field "Download URL"
file_url <- "https://dataverse.harvard.edu/api/access/datafile/5374866"

### 2. download the file and read it into R ----

df <- read_tsv(file_url)  
# note: we use `read_tsv` because the file we want to download is a .tab file, i.e. "tab-separated"
#.      if it was a .csv file, we'd use `read_csv` instead

df

## Example 2: dowload with file persistent ID -----

#' @example In what follows, we will use the example of the article 
#' 
#'    Barberá, P., Boydstun, A. E., Linn, S., McMahon, R., & Nagler, J. (2021). 
#'       Automated Text Classification of News Articles: A Practical Guide. 
#'       Political Analysis, 29(1), 19–42.
#'       
#'    The repository is https://doi.org/10.7910/DVN/MXKRDE

## 1. load the 'dataverse' package and set the necessary environment variables ----
library(dataverse)
Sys.setenv("DATAVERSE_SERVER" = "dataverse.harvard.edu")
Sys.setenv("DATAVERSE_ID" = "pan") # set to Political Analysis dataverse ID !

## 2. locate the file we want to download ----

# 1) go to https://doi.org/10.7910/DVN/MXKRDE 
# 2) search for the file 'ground-truth-dataset-cf.tab'
# 3) on the files page, go to the "Metadata" tab
# 4) get the value in the field "File Persistent ID"
persistent_id <- "doi:10.7910/DVN/MXKRDE/EJTMLZ"

## 3. download the file and read it into R ----

df <- get_dataframe_by_doi(
  # use the file persistent ID to specify which file to download
  filedoi = persistent_id, 
  # pass the appropriate file reading function (from the readr package)
  .f = read_tsv 
  # note: we use `read_tsv` because the file we want to download is a .tab file, i.e. "tab-separated"
  #.      if it was a .csv file, we'd use `read_csv` instead
)

# Download from GitHub ----

#' @note: Github is a code sharing and open-source collaboration platform.
#'    Some researchers use it to store make available the replication materials
#'     
#' @example In what follows, we will use the example of the article 
#' 
#'    van Atteveldt, W., van der Velden, M. A. C. G. & Boukes, M. (2021) 
#'        The Validity of Sentiment Analysis: Comparing Manual Annotation, 
#'        Crowd-Coding, Dictionary Approaches, and Machine Learning Algorithms.
#'        _Communication Methods and Measures_, 15(2), 121-140.
#'       
#'    The repository is https://github.com/vanatteveldt/ecosent

## get the gold headline annotations ----

### 1. locate the files we want to download ----

# 1) go to https://github.com/vanatteveldt/ecosent
# 2) click on the "data" folder
# 3) get gold sentences' texts:  in the 'raw' subfolder, 
#    1. find the file 'gold_sentences.csv'
#    2. click on the file
#    3. click on the "Raw" button
#    4. copy the URL of the raw file
gold_sentences_texts_url <- "https://raw.githubusercontent.com/vanatteveldt/ecosent/master/data/raw/gold_sentences.csv"
# 3) get gold sentences' expert codings: in the intermediate 'subfolder'
#    1. find the file 'gold.csv'
#    2. click on the file
#    3. click on the "Raw" button
#    4. copy the URL of the raw file
gold_sentences_labels_url <- "https://raw.githubusercontent.com/vanatteveldt/ecosent/master/data/intermediate/gold.csv"

### 2. download the files and combine them ----

sentences_df <- read_csv(gold_sentences_texts_url)
labels_df <- read_csv(gold_sentences_labels_url)
# note: we use `read_csv` because the file we want to download is a .csv file
#.      if it was a .tsv or .tab file, we'd use `read_tsv` instead

# note: the labels are already at the headline level (i.e. experts' judgments 
#       have already been aggregated and disambigutated)
labels_df |> 
  # compute number of labels per headline
  group_by(id) |> 
  summarise(n_labels = n()) |> 
  # count numbers of labels per headlines
  count(n_labels)
# note: each of 284 headlines has only one label

# combine texts and labels
gold_df <- inner_join(labels_df, sentences_df, by = "id")

glimpse(gold_df)

## get human coders' headline annotations ----

# IMPORTANT: each sentences has been coded by muliple coders, so we need to aggregate them

### get the raw data ----

# 1) go to https://github.com/vanatteveldt/ecosent
# 2) click on the "data" folder
# 3) get gold sentences' texts:  in the 'raw' subfolder, 
#    1. find the file 'manual_coding.csv'
#    2. click on the file
#    3. click on the "Raw" button
#    4. copy the URL of the raw file
file_url <- "https://raw.githubusercontent.com/vanatteveldt/ecosent/master/data/intermediate/manual_coding.csv"

manual_codings_df <- read_csv(file_url)
# note: the labels are at the coder--headline level

# number of coded headlines
length(unique(manual_codings_df$id))

# check how many annotations per headline
manual_codings_df |> 
  # compute number of judgments per headline
  group_by(id) |> 
  summarise(n_judgments = n()) |> 
  # count numbers of judgments per headlines
  count(n_judgments)
# note: 149 headlines have been judged by only one coder, 24 by 2, 44 by 3, etc.


### aggregate codings into headine-level labels ----

table(manual_codings_df$value)
# note: label categories are -1 = negative, 0 = neutral, 1 = positive
#       they are nominal (at best ordinal), so we shouldn't just averaged ratings
#      instead we are going to find the plurality winner judgment for each headline

# let's see some of the headlines that have been coded by three coders
cases_with_three_codings <- manual_codings_df |> 
  # compute number of judgments per headline
  group_by(id) |> 
  filter(n() == 3) |> 
  pull(id) |> 
  unique()

filter(manual_codings_df, id %in% cases_with_three_codings[1:3])
# note: headlines with IDs 11191, 12363, 12367 have unambiguous codings
filter(manual_codings_df, id %in% cases_with_three_codings[4:6])
# note: headlines with IDs 12501, 15261, 18751 have disagreements but all a plurality winner
filter(manual_codings_df, id %in% "150282475")
# but this headline has  n o  plurality winner ==> we would want to discard it

# define a function that finds the majority winner, if it exists
find_plurality_winner <- function(judgments) {
  
  # if only one judgment, it's the plurality winner
  if (length(judgments) == 1)
    return(judgments[[1]])
  
  # tabulate judgments, in descending order of frequency (i.e., most frequent one first)
  tab <- sort(table(judgments), decreasing = TRUE)
  # if table has only one element, all judgments are the same
  if (length(tab) == 1)
    return(unique(judgments))
  
  # otherwise, there is a plurality winner if the most frequent judgment is 
  #  more frequent than the second most common judgment
  if (tab[1] > tab[2]) {
    # get the most frequent judgment (from the table element's name)
    winner <- names(tab[1])
    # convert to the class (e.g. numeric of the input data)
    class(winner) <- class(judgments)
    # and return
    return(winner)
  }
  
  # otherwise, there is no winner
  no_winner <- NA
  class(no_winner) <- class(judgments)
  return(no_winner)
}

# apply the function to the judgments
manual_labels_df <- manual_codings_df |> 
  # compute number of judgments per headline
  group_by(id) |> 
  summarise(
    n_judgments = n(),
    judgments = list(c(value))
  ) |> 
  mutate(
    label = sapply(judgments, find_plurality_winner, simplify = TRUE)
  )

count(manual_labels_df, label)
# note: only 9 cases where no plurality winner could be identified

# compare with gold standard/ground truth codings:
confusion_matrix <- manual_labels_df |> 
  filter(!is.na(label)) |> 
  select(id, manual = label) |> 
  inner_join(
    select(gold_df, id, gold = value)
    , by = "id"
  ) |> 
  with(table(manual, gold))

confusion_matrix
# compute accuracy
sum(diag(confusion_matrix))/sum(confusion_matrix)
# note: see https://gist.github.com/phil8192/e42c85397888292f861cf3e9389217bc for how to compute classification report
source("https://gist.githubusercontent.com/phil8192/e42c85397888292f861cf3e9389217bc/raw/708e5d3922c3e7a6971e5666b01e80b2f5275381/classification_report.R")
cr(confusion_matrix)
## get crowd coders' headline annotations ----

# IMPORTANT: each sentences has been coded by muliple coders, so we need to aggregate them

### get the raw data ----

# 1) go to https://github.com/vanatteveldt/ecosent
# 2) click on the "data" folder
# 3) get gold sentences' texts:  in the 'raw' subfolder, 
#    1. find the file 'crowdcodings.csv'
#    2. click on the file
#    3. click on the "Raw" button
#    4. copy the URL of the raw file
file_url <- "https://raw.githubusercontent.com/vanatteveldt/ecosent/master/data/intermediate/crowdcodings.csv"

crowd_codings_df <- read_csv(file_url)
# note: the labels are at the coder--headline level

# number of coded headlines
length(unique(crowd_codings_df$id))

# check how many annotations per headline
crowd_codings_df |> 
  # compute number of judgments per headline
  group_by(id) |> 
  summarise(n_judgments = n()) |> 
  # count numbers of judgments per headlines
  count(n_judgments)
# note: all headlines have been judged by five coders


### aggregate codings into headine-level labels ----

table(crowd_codings_df$value)
# note: label categories are -1 = negative, 0 = neutral, 1 = positive
#       they are nominal (at best ordinal), so we shouldn't just averaged ratings
#      instead we are going to find the plurality winner judgment for each headline

# apply plurality winner function to the judgments
crowd_labels_df <- crowd_codings_df |> 
  # compute number of judgments per headline
  group_by(id) |> 
  summarise(
    n_judgments = n(),
    judgments = list(c(value))
  ) |> 
  mutate(
    label = sapply(judgments, find_plurality_winner, simplify = TRUE)
  )

count(crowd_labels_df, label)
# note: only 12 cases where no plurality winner could be identified

# compare with gold standard/ground truth codings:
confusion_matrix <- crowd_labels_df |> 
  filter(!is.na(label)) |> 
  select(id, crowd = label) |> 
  inner_join(
    select(gold_df, id, gold = value)
    , by = "id"
  ) |> 
  with(table(crowd, gold))

confusion_matrix
# compute accuracy
sum(diag(confusion_matrix))/sum(confusion_matrix)
# note: see https://gist.github.com/phil8192/e42c85397888292f861cf3e9389217bc for how to compute classification report
cr(confusion_matrix)

