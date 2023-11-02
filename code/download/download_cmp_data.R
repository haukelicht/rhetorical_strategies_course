# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #  
#
#' @title  Download and clean manifesto corpus data
#' @author Hauke Licht
#' @date   2023-11-02 
#
# +~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~+~ #

# setup ----

## load required packages ----

library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(manifestoR)
library(countrycode)

## helper functions ----

#' \link[dplyr]{\code{as_tibble}} method for \link[manifestoR]{\code{ManifestoCorpus}} class
#' 
#' @param x A 'ManifestoCorpus' object.
#' 
#' @return a \link[tibble]{\code{tibble}} with 
#'      rows uniquely identifyied by column 'manifesto_id',
#'      list-column 'data' containing mainfesto text and codes, and
#'      all other columns recording manifesto meta data. 
as_tibble.ManifestoCorpus <- function(x) {
  
  if (!inherits(x, "ManifestoCorpus"))
    stop("No `as_tibble()` method implemented for object of class ", sQuote(class(x)[1]))
  
  man_sents <- as_tibble(map_dfr(as.list(x), "content", .id = "manifesto_id"))
  
  man_meta <- as_tibble(map_dfr(map(as.list(x), "meta"), as.data.frame.list, stringsAsFactors = FALSE))
  
  out <- left_join(man_sents, man_meta, by = "manifesto_id")
  
  out <- nest(out, data = names(man_sents)[-1])
  
  return(out)
}

#' \link[dplyr]{\code{as_tibble}} method for \link[manifestoR]{\code{ManifestoDocument}} class
#' 
#' @param x A 'ManifestoDocument' object.
#' 
#' @return a \link[tibble]{\code{tibble}} with 
#'      rows uniquely identifyied by column 'manifesto_id',
#'      list-column 'data' containing mainfesto text and codes, and
#'      all other columns recording manifesto meta data. 
as_tibble.ManifestoDocument <- function(x) {
  
  if (!inherits(x, "ManifestoDocument"))
    stop("No `as_tibble()` method implemented for object of class ", sQuote(class(x)[1]))
  
  out <- as_tibble(as.data.frame.list(as.list(x$meta)))
  out$data <- list(as_tibble(x$content))
  
  return(out)
}

## setup access to Manifesto Project API ----

# set Manifesto Project API key
# TODO: put your manifesto API key in the working directory (see ?mp_setapikey) 
mp_setapikey("manifesto_apikey.txt")

# list available versions
mp_coreversions()
# pick one (for reproducibility)
CMP_VERSION <- "MPDS2023a"

# download the data ----

## get CMP party-election data ----

cmp <- mp_maindataset(
  # use first 2020 release
  version = CMP_VERSION
  # don't use cache (bad for reproducebility because you may have another corpus version in caches)
  , cache = FALSE
  # don't load data for South American countries
  , south_america = FALSE
)

# ensure correct version
table(cmp$corpusversion)

## create table of countries ----

cmp_countries <- cmp %>% 
  group_by(countryname) %>% 
  summarise(
    oecdmember = any(as.logical(oecdmember))
    , eumember = any(as.logical(eumember))
    , elections = paste(unique(edate), collapse = ", ")
  ) %>% 
  transmute(
    country_iso2c = countryname(countryname, "iso2c")
    , country_iso3c = countryname(countryname, "iso3c")
    , country_name = countryname
    , country_name_un = countryname(countryname, "un.name.en")
    , continent = countryname(countryname, "continent")
    , region = countryname(countryname, "region")
    , region23 = countryname(countryname, "region23")
    , oecd_member = oecdmember
    , eu_member = eumember
    , elections
  )

# recode Northern Ireland
irl <- which(cmp_countries$country_name == "Ireland")
nirl <- which(cmp_countries$country_name == "Northern Ireland")

cmp_countries$continent[nirl] <- cmp_countries$continent[irl]
cmp_countries$region[nirl] <- cmp_countries$region[irl]
cmp_countries$region23[nirl] <- cmp_countries$region23[irl]

cmp_countries %>% filter(grepl("Ireland", country_name))

## get table of valid manifestos' key meta information ----

cmp_meta <- mp_metadata(select(cmp, party, date))

with(cmp_meta, table("annotated" = annotations, "valid" = !is.na(manifesto_id), useNA = "ifany"))

manifestos <- cmp_countries %>% 
  select(country_iso3c, country_name) %>% 
  left_join(
    transmute(cmp, oecd_member = as.logical(oecdmember), countryname, party, date, election_date = edate)
    , by = c("country_name" = "countryname")
  ) %>% 
  right_join(
    cmp_meta %>% 
      select(party, date, manifesto_id, language, annotations)
    , by = c("party", "date")
  )

valid_manifestos <- filter(manifestos, !is.na(manifesto_id))

## get manifestos of selected countries ---- 

countries <- c("DEU", "ESP", "GBR")

wanted <- valid_manifestos %>% 
  filter(country_iso3c %in% countries) |> 
  select(party, date)

cmp_subcorpus <- mp_corpus(wanted)

format(object.size(cmp_subcorpus), "Gb")

# convert to tibble (list-column data records annotated quasi-sentences)
cmp_subcorpus_df <- as_tibble(cmp_subcorpus)

# add further meta data to manifestos ----

# left-join relevant party-election meta data
cmp_subcorpus_df <- cmp_countries %>% 
  filter(country_iso3c %in% countries) %>% 
  select(country_iso3c, country_name) %>% 
  left_join(
    cmp %>% 
      select(
        countryname
        , date, election_date = edate
        , party_id = party, party_name_short = partyabbrev, party_name = partyname, parfam
        , progtype
        , datasetversion, id_perm
        , coderid
      )
    , by = c("country_name" = "countryname")
  ) %>% 
  right_join(cmp_subcorpus_df, by = c("party_id" = "party", "date")) %>% 
  arrange(country_iso3c, party_id, election_date)


## add info on which codebook (and coding scheme) was used and party family ----

# from Codebook (p. 5): https://manifesto-project.wzb.eu/down/data/2019a/codebooks/codebook_MPDataset_MPDS2019a.pdf
manual_used <- tibble::tribble(
  ~desc, ~label, ~value,
  "No manual","none","0",
  "Manual version 1","v1","1",
  "Manual version 2","v2","2",
  "Manual version 3","v3","3",
  "Manual version 4","v4","4",
  "Manual version 5","v5","5",
  "Not applicable","NA","998",
  "No information whether a handbook was used or not","DK","999",
)

# from Codebook (pp. 4-5)
parfam_codes <- tibble::tribble(
  ~value, ~label, ~desc,
  "10","ECO","Ecological parties",
  "20","LEF","Socialist or other left parties",
  "30","SOC","Social democratic parties",
  "40","LIB","Liberal parties",
  "50","CHR","Christian democratic parties (in Israel also Jewish parties)",
  "60","CON","Conservative parties",
  "70","NAT","Nationalist parties",
  "80","AGR","Agrarian parties",
  "90","ETH","Ethnic and regional parties",
  "95","SIP","Special issue parties",
  "98","DIV","Electoral alliances of diverse origin without dominant party",
  "999","MI","Missing information",
)

progtypes <- c(
  "Programme of a single party" = 1
  , "Programme of two or more parties" = 2
  , "Estimate" = 3
  , "Programme taken from main party of electoral coalition" = 4
  , "Average of all members of an electoral coalition" = 5
  , "General programme" = 6
  , "Party bloc programme" = 8
  , "programmes, electoral statistics are given for the party bloc" = 2
  , "Other programme type" = 9
  , "Missing programme" = 99
)

cmp_subcorpus_df <- cmp_subcorpus_df %>% 
  left_join(
    manual_used %>% 
      select(-desc) %>% 
      rename(handbook_version = label)
    , by = c("handbook" = "value")
  ) %>% 
  mutate(parfam = as.character(parfam)) %>% 
  left_join(
    parfam_codes %>% 
      select(-desc) %>% 
      rename(parfam_name_short = label)
    , by = c("parfam" = "value")
  ) %>% 
  left_join(
    rename(tibble::enframe(progtypes), progtype_name = name)
    , by = c("progtype" = "value")
  )
  
# create extra column indicating harmonized (backward-compatible) CMP categories
cmp_subcorpus_df <- cmp_subcorpus_df %>% 
  mutate(tmp_ = rep_len(1:8, nrow(.))) %>% 
  split(.$tmp_) %>% 
  map_dfr(
    function(s) {
      mutate(s, data = map(data, function(x) x %>% mutate(cmp_code_harmonized = cmp_code %>% recode_cee_codes %>% recode_v5_to_v4)))
    }
  ) %>% 
  select(-tmp_)

# finally: convert into quasi-sentence level dataset ----

coded_manifestos <- cmp_subcorpus_df %>% 
  unnest(data) %>% 
  group_by(manifesto_id) %>% 
  mutate(qs_id = sprintf("%s_%05d", manifesto_id, row_number())) %>% 
  ungroup() 
  
coded_manifestos
