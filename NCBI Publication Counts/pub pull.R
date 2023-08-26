# Detach all session packages and automate the working directory as source file location -----
# lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load needed libraries ----
library(tidyverse)
library(RISmed)
library(rentrez)
library(rcrossref)
library(lubridate)
library(fontawesome)
library(icons)
library(textclean)
library(powerjoin)

# Get icons ----
linkIcon <- 
  fontawesome::fa("link", 
     fill = "#89cbfd", 
     width = "2em")

doiIcon <- 
  icon_style(icons::academicons("doi"), 
             fill = "#F1B847",
             width = "2em")

pubmedIcon <- 
  icon_style(icons::academicons("pubmed"), 
             fill = "#4977A9",
             width = "2em")

# Get NCBI data ----

##  NCBI API key (both keys are the same) ----
# NCBIapi_key = "yourkeyhere" 
# set_entrez_key("yourkeyhere")

## Load NCBI pubstatus codebook data ----
ncbi_status <- 
  read_csv("pubstatus_codes.csv") %>%
  mutate(code = na_if(code, "unknown")) %>%
  mutate(code = as.numeric(code)) %>%
  mutate(code = coalesce(code, filter)) %>%
  select(-filter)

## Wrangle WVCTSI grant information ----
grantNumbers <- 
  c("5U54GM104942-04", 
    "5U54GM104942-03", 
    "3U54GM104942-03S1", 
    "3U54GM104942-03S2", 
    "2U54GM104942-02", 
    "1U54GM104942-01") %>%
  enframe(name = NULL) %>%
  rename(Grant = value) %>%
  separate(Grant, into = c('Type', 'Activity','Project','Year'), 
           sep = c(1,4,12,15)) %>%
  mutate(Year = str_remove(Year, "-")) %>%
  distinct() 

## Define search parameters ----

### Denote all search combinations ----
by_activity_project_type <- 
  grantNumbers %>%
  tidyr::expand(Type, Activity, Project)

by_activity_project_year <- 
  grantNumbers %>%
  tidyr::expand(Activity, Project, Year)

by_activity_project <- 
  grantNumbers %>%
  tidyr::expand(Activity, Project)

by_project_year <- 
  grantNumbers %>%
  tidyr::expand(Project, Year)

by_project <- 
  grantNumbers %>%
  tidyr::expand(Project)

### Get PMID information and send search ----
RISMedRecords <- 
  bind_rows(grantNumbers, 
            by_activity_project_type, 
            by_activity_project_year,
            by_activity_project,
            by_project_year,
            by_project) %>%
  mutate_all(~ replace_na(.x, "")) %>% 
  unite(Grant, Type, Activity, Project, Year, sep = " ") %>%
  mutate(Grant = str_trim(Grant)) %>%
  mutate(Grant = str_c(Grant, collapse = " OR ")) %>% 
  distinct() %>%
  paste0() %>%
  EUtilsSummary(retmax = 10000) %>%
  EUtilsGet()

### Create a results data frame ----
counts <-  
  data.frame('pmid' = PMID(RISMedRecords), 
             'type' = PublicationStatus(RISMedRecords), 
             'month' = MonthPubmed(RISMedRecords), 
             'year' = YearPubmed(RISMedRecords)) %>%
  distinct(pmid, .keep_all = TRUE) %>%
  mutate(quarter = case_when(
    (month == 1 | month == 2 | month == 3) ~ "Q3",
    (month == 4 | month == 5 | month == 6) ~ "Q4",
    (month == 7 | month == 8 | month == 9) ~ "Q1",
    (month == 10 | month == 11 | month == 12) ~ "Q2"
  )) %>%
  mutate(Timeframe = case_when(
    (year == 2009 | year == 2010 | year == 2011 | year == 2012 | year == 2013 | year == 2014 | year == 2015 | year == 2016 | (year == 2017 & (quarter == "Q3" | quarter == "Q4"))) ~ "Years 1-5",
    ((year == 2017 & (quarter == "Q1" | quarter == "Q2")) |  (year == 2018 & (quarter == "Q3" | quarter == "Q4"))) ~ "Year 6",
    ((year == 2018 & (quarter == "Q1" | quarter == "Q2")) |  (year == 2019 & (quarter == "Q3" | quarter == "Q4"))) ~ "Year 7",
    ((year == 2019 & (quarter == "Q1" | quarter == "Q2")) |  (year == 2020 & (quarter == "Q3" | quarter == "Q4"))) ~ "Year 8",
    ((year == 2020 & (quarter == "Q1" | quarter == "Q2")) |  (year == 2021 & (quarter == "Q3" | quarter == "Q4"))) ~ "Year 9",
    ((year == 2021 & (quarter == "Q1" | quarter == "Q2")) |  (year == 2022 & (quarter == "Q3" | quarter == "Q4"))) ~ "Year 10",
    ((year == 2022 & (quarter == "Q1" | quarter == "Q2")) |  (year == 2023 & (quarter == "Q3" | quarter == "Q4"))) ~ "Year 11"
    )
  ) %>%
  mutate(type = str_replace(type, "aheadofprint", "In press")) %>%
  mutate(type = str_replace(type, "epublish", "Published")) %>%
  mutate(type = str_replace(type, "ppublish", "Published")) %>%
  group_by(type, quarter, Timeframe) %>%
  count() %>%
  arrange(Timeframe, quarter) %>%
  pivot_wider(names_from = quarter, 
              values_from = c("n")) %>%
  ungroup() %>%
  mutate(Timeframe = factor(Timeframe, levels = c("Years 1-5", "Year 6", "Year 7", "Year 8", "Year 9", "Year 10", "Year 11"))) %>%
  mutate(type = factor(type, levels = c("Published", "In press"))) %>%
  arrange(Timeframe, type) %>%
  mutate_if(is.factor, as.character)  

# Construct summary table ----

## Calculate column sums ----
col_sums <- 
  counts %>%
  mutate(across(c(Q1, Q2, Q3, Q4), ~ replace_na(., 0))) %>%
  select_if(is.numeric) %>%
  summarise(across(where(is.numeric), sum)) %>%
  mutate(type = "Aggregate") %>%
  mutate(Timeframe = "")

counts_col_sums <- 
  bind_rows(counts, col_sums)

## Calculate row sums ----
row_sums <- 
  counts_col_sums %>%
  mutate(across(c(Q1, Q2, Q3, Q4), ~ replace_na(., 0))) %>%
  select_if(is.numeric) %>%
  rowwise() %>% 
  mutate(sum = sum(Q1, Q2, Q3, Q4)) %>%
  select(sum) %>%
  rename(Publications = sum)

## Build the table ----
pubcounts <- 
  bind_cols(counts_col_sums, row_sums) %>% 
  mutate(across(everything(), na_if, ""))  %>%
  mutate(Q1 = gsub("[[:punct:]]", "", Q1)) %>%
  mutate(Q2 = gsub("[[:punct:]]", "", Q2)) %>%
  mutate(Q3 = gsub("[[:punct:]]", "", Q3)) %>%
  mutate(Q4 = gsub("[[:punct:]]", "", Q4)) %>%
  rename(Status = type) %>%
  mutate(across(c(Q1, Q2, Q3, Q4), ~ replace_na(., "0"))) %>%
  mutate(across(c(Timeframe), ~ replace_na(., "")
    )
  )


# Construct list of pubs ----
## Pubmed pull ++ ----
  pubmedPull <- 
    tibble('title'= ArticleTitle(RISMedRecords),
           'journal' = Title(RISMedRecords), 
           'abstract'= AbstractText(RISMedRecords), 
           'pmid' = PMID(RISMedRecords), 
      #    'grants' = GrantID(RISMedRecords), 
           'type' = PublicationStatus(RISMedRecords), 
           'month' = MonthPubmed(RISMedRecords), 
           'year' = YearPubmed(RISMedRecords), 
           'issue' = Issue(RISMedRecords), 
           'volume' = Volume(RISMedRecords), 
           'pages' = MedlinePgn(RISMedRecords),
           'author' = Author(RISMedRecords),
           'affiliation' = Affiliation(RISMedRecords)) %>% 
    mutate(pmid = as.character(pmid)) %>%
    mutate(title = as.character(title)) %>%
    mutate(journal = as.character(journal)) %>%
    mutate(abstract = as.character(abstract)) %>%
    mutate(type = as.character(type)) %>%
 #  mutate(grants = as.character(grants)) %>%
 #  mutate(grants = str_replace_all(grants, ' ', '')) %>%
 #  mutate(grants = str_trim(grants)) %>%
    mutate(pmid = str_trim(pmid)) %>%
    mutate(title = str_trim(title)) %>%
    mutate(abstract = str_trim(abstract)) %>%
    mutate(issue = as.character(issue)) %>%
    mutate(volume = as.character(volume)) %>%
    mutate(pages = as.character(pages)) %>%
    mutate(quarter = case_when(
      (month == 1 | month == 2 | month == 3)  ~ "Third",
      (month == 4 | month == 5 | month == 6)  ~ "Fourth",
      (month == 7 | month == 8 | month == 9)  ~ "First",
      (month == 10 | month == 11 | month == 12)  ~ "Second"
      )
     ) 
    
### extract pmids ----
initialPmids <- 
  PMID(RISMedRecords)

initialYears <- 
  YearPubmed(RISMedRecords)

initialMonths <-
  MonthPubmed(RISMedRecords)

### dois ----
doiNumbers = list()

for(i in 1:length(unique(initialPmids)))
{
  print(i)
  
  doiNumbers[[i]] <- 
    tryCatch({
      data.frame(pmid = initialPmids[i], 
                 extract_from_esummary(entrez_summary(db = "pubmed", 
                                                      id = initialPmids[i]), 
                                       "articleids")) %>%
        filter(idtype == "pubmed" | idtype == "doi" | idtype == "pmcid") %>%
        select(pmid, value)}, 
        error=function(e) NA)
}

dois <- 
  do.call(rbind, doiNumbers) %>%
  tibble() %>% 
  rename(doi = value) %>%
  mutate(pmid = as.character(pmid)) %>%
  group_by(pmid) %>%
  mutate(grouped_id = row_number()) %>%
  spread(grouped_id, doi) %>%
  rename(pubmed = `1`) %>%
  rename(doi = `2`) %>%
  rename(pmc = `3`) %>%
  select(-pubmed) %>%
  mutate(pmc = case_when(
    str_detect(doi, "pmc-id") ~ doi,
    TRUE ~ pmc)
  ) %>%
  mutate(doi = case_when(
    str_detect(doi, "pmc-id") ~ "NA",
    TRUE ~ doi)
  ) %>%
  # mutate_at(vars(doi),list(doi = ~ replace(., str_detect(., "pmc-id"), NA_real_))) %>%
  mutate(embargo = str_extract(pmc, "embargo-date[^_]+$")) %>%
  mutate(embargo = str_remove_all(embargo, ";")) %>%
  mutate(embargo = str_remove_all(embargo, "embargo-date: ")) %>%
  select(-pmc) %>%
  mutate(embargo = str_replace_all(embargo,"/", "-")) %>%
  mutate(embargo = str_trim(embargo)) %>%
  mutate(embargo = format(as.Date(embargo, '%Y-%m-%d'), "%m/%d/%Y")) %>%
  rename_with(str_to_lower)

### citations ----
temp_cites = list()

for(i in 1:length(unique(initialPmids)))
  {
  print(i)
  temp_cites[[i]] <- 
    tryCatch({tibble(pmid = initialPmids[i], 
                     year = initialYears[i],
                     month = initialMonths[i],
                     citation_count = extract_from_esummary(entrez_summary(db="pubmed", 
                                                                           id=initialPmids[i]), 
                                                            "pmcrefcount"))}, 
             error=function(e) NA)
  }

citationsFull <- 
  do.call(rbind, temp_cites) %>%
  tibble() %>%
  mutate_at(.vars = c("citation_count"),
            list(~ifelse(.=="", 0, as.character(.)))) %>%
  mutate(citation_count = as.numeric(citation_count)) %>%
  mutate(pmid = as.character(pmid)) %>%
  rename_with(str_to_lower) %>%
  mutate(quarter = case_when(
    (month == 1 | month == 2 | month == 3) ~ "Q3",
    (month == 4 | month == 5 | month == 6) ~ "Q4",
    (month == 7 | month == 8 | month == 9) ~ "Q1",
    (month == 10 | month == 11 | month == 12) ~ "Q2"
  )) %>%
  mutate(Timeframe = case_when(
    (year == 2009 | year == 2010 | year == 2011 | year == 2012 | year == 2013 | year == 2014 | year == 2015 | year == 2016 | (year == 2017 & (quarter == "Q3" | quarter == "Q4"))) ~ "Years 1-5",
    ((year == 2017 & (quarter == "Q1" | quarter == "Q2")) |  (year == 2018 & (quarter == "Q3" | quarter == "Q4"))) ~ "Year 6",
    ((year == 2018 & (quarter == "Q1" | quarter == "Q2")) |  (year == 2019 & (quarter == "Q3" | quarter == "Q4"))) ~ "Year 7",
    ((year == 2019 & (quarter == "Q1" | quarter == "Q2")) |  (year == 2020 & (quarter == "Q3" | quarter == "Q4"))) ~ "Year 8",
    ((year == 2020 & (quarter == "Q1" | quarter == "Q2")) |  (year == 2021 & (quarter == "Q3" | quarter == "Q4"))) ~ "Year 9",
    ((year == 2021 & (quarter == "Q1" | quarter == "Q2")) |  (year == 2022 & (quarter == "Q3" | quarter == "Q4"))) ~ "Year 10",
    ((year == 2022 & (quarter == "Q1" | quarter == "Q2")) |  (year == 2023 & (quarter == "Q3" | quarter == "Q4"))) ~ "Year 11"
    )
  ) %>%
  select(pmid, Citations = citation_count, Timeframe)

citationsCounts <- 
  citationsFull %>%
  group_by(Timeframe) %>% 
  summarise(across(Citations, sum))

citations <-
  citationsFull %>%
  select(pmid, Citations)


### pubhistory ----
publicationHistory = list()

for(i in 1:length(unique(initialPmids)))
{
  print(i)
  publicationHistory[[i]] <- 
    tryCatch({tibble(pmid = initialPmids[i], 
                     pubhistory = extract_from_esummary(entrez_summary(db = "pubmed", 
                                                                       id = initialPmids[i]),
                                                        "history") %>%
                       as_tibble(.name_repair = "check_unique"))}, 
             error=function(e) NA)
}

pubhistory <- 
  do.call(rbind, publicationHistory) %>%
  as.data.frame.list() %>%
  rename(pubhistory = 'pubhistory.pubstatus') %>%
  rename(pubdate = 'pubhistory.date') %>%
  tibble() %>%
  mutate(pubdate=as.Date(pubdate, format = "%Y/%m/%d")) %>%
  group_by(pmid) %>%
  filter(pubdate == max(pubdate)) %>%
  ungroup() %>%
  left_join(ncbi_status, by = c("pubhistory" = "pubstatus")) %>%
  mutate(status = str_to_sentence(status)) %>%
  group_by(pmid) %>%
  filter(code == max(code)) %>%
  rename(pubstatus = pubhistory) %>%
  rename(description = status) %>%
  select(pmid, pubdate, pubstatus, description) %>%
  rename_with(str_to_lower)

### pmid linked information ----
pmidInformation <- 
  EUtilsGet(initialPmids)

### affiliations ----
affiliationInformation = list()

for(i in 1:length(unique(initialPmids)))
{
  print(i)
  affiliationInformation[[i]] <- 
    tibble(Affiliation(pmidInformation)[i]) %>%
    unlist() %>% 
    enframe(name = NULL) %>%
    mutate(pmid = PMID(pmidInformation)[i]) %>%
    rowid_to_column(var = "order") %>%
    rename(affiliation = value) 
}

affiliationPubmed <- 
  bind_rows(affiliationInformation)

### authors ----
authorCols <- 
  c(LastName = NA_character_,
    ForeName = NA_character_,
    Initials = NA_character_,
    order = NA_integer_, 
    pmid = NA_character_) 

authorInformation = list()

for(i in 1:length(unique(initialPmids)))
{
  print(i)
  authorInformation[[i]] <- 
    tibble(Author(pmidInformation)[i]) %>%
    unnest_legacy() %>%
    mutate(pmid = PMID(pmidInformation)[i]) %>% 
    select(-one_of("CollectiveName")) %>%
    add_column(!!!authorCols[setdiff(names(authorCols), names(.))], .name_repair = "minimal") %>%
    mutate(order = as.numeric(order))
}

authorPubmed <- 
  bind_rows(authorInformation) %>%
  select(names(authorCols)) %>%
  rename_with(str_to_lower)

### joined list * ----
pubmedList <-
  pubmedPull %>%
  mutate(across(everything(), as.character)) %>%
  power_left_join(dois, by = c("pmid"), conflict = coalesce_xy) %>%
  power_left_join(citations, by = c("pmid"), conflict = coalesce_xy) %>%
 # power_left_join(authorPubmed, by = c("pmid"), conflict = coalesce_xy) %>%
 # power_left_join(affiliationPubmed, by = c("pmid", "order"), conflict = coalesce_xy) %>%
  select(-c(author, affiliation)) %>%
  rename(cited = Citations) %>%
  distinct()

## Rentrez Pull ++ ----
RentrezRecords = list()

for(i in 1:length(unique(initialPmids)))
{
  print(i)
  
  # get ID of each search
  pubdata <-
    tryCatch({entrez_fetch(db = "pubmed", 
                           id = initialPmids[i],
                           rettype = "xml", 
                           parsed = TRUE)},
             error = function(e) NA)
  
  # convert to XML
  pub_list <- XML::xmlToList(pubdata)
  
  # retrieve LastName
  LastName <- 
    if (!is.null(pub_list$PubmedArticle$MedlineCitation$Article$AuthorList$Author$LastName)) { 
      pub_list$PubmedArticle$MedlineCitation$Article$AuthorList$Author$LastName
    } else if (!is.null(pub_list$PubmedBookArticle$BookDocument$AuthorList$Author$LastName)) {
      pub_list$PubmedBookArticle$BookDocument$AuthorList$Author$LastName
    } else {
      NA_character_
    }
  
  # retrieve ForeName
  ForeName <- 
    if (!is.null(pub_list$PubmedArticle$MedlineCitation$Article$AuthorList$Author$ForeName)) { 
      pub_list$PubmedArticle$MedlineCitation$Article$AuthorList$Author$ForeName
    } else if (!is.null(pub_list$PubmedBookArticle$BookDocument$AuthorList$Author$ForeName)) {
      pub_list$PubmedBookArticle$BookDocument$AuthorList$Author$ForeName
    } else {
      NA_character_
    }
  
  # retrieve Initials
  Initials <- 
    if (!is.null(pub_list$PubmedArticle$MedlineCitation$Article$AuthorList$Author$Initials)) { 
      pub_list$PubmedArticle$MedlineCitation$Article$AuthorList$Author$Initials
    } else if (!is.null(pub_list$PubmedBookArticle$BookDocument$AuthorList$Author$Initials)) {
      pub_list$PubmedBookArticle$BookDocument$AuthorList$Author$Initials
    } else {
      NA_character_
    }
  
  # retrieve orcid
  ORCID <- 
    if (!is.null(pub_list$PubmedArticle$MedlineCitation$Article$AuthorList$Author$Identifier$text)) { 
      pub_list$PubmedArticle$MedlineCitation$Article$AuthorList$Author$Identifier$text
    } else {
      NA_character_
    }
  
  # retrieve location
  Affiliation <- 
    if (!is.null(pub_list$PubmedArticle$MedlineCitation$Article$AuthorList$Author$AffiliationInfo$Affiliation)) { 
      pub_list$PubmedArticle$MedlineCitation$Article$AuthorList$Author$AffiliationInfo$Affiliation
    } else if (!is.null(pub_list$PubmedBookArticle$BookDocument$AuthorList$Author$AffiliationInfo$Affiliation)) {
      pub_list$PubmedBookArticle$BookDocument$AuthorList$Author$AffiliationInfo$Affiliation
    } else {
      NA_character_
    }
  
  # retrieve doi
  DOI <- 
    if (!is.null(pub_list$PubmedArticle$MedlineCitation$Article$ELocationID$text)) { 
      pub_list$PubmedArticle$MedlineCitation$Article$ELocationID$text
    } else if (!is.null(pub_list$PubmedBookArticle$BookDocument$ArticleIdList$ArticleId$text)) {
      pub_list$PubmedBookArticle$BookDocument$ArticleIdList$ArticleId$text
    } else {
      NA_character_
    }
  
  # retrieve pmid
  PMID <- 
    if (!is.null(pub_list$PubmedArticle$MedlineCitation$PMID$text)) { 
      pub_list$PubmedArticle$MedlineCitation$Article$ELocationID$text
    } else {
      NA_character_
    }
  
  # retrieve title
  Title <- 
    if (!is.null(pub_list$PubmedArticle$MedlineCitation$Article$ArticleTitle)) { 
      tryCatch({c(pub_list$PubmedArticle$MedlineCitation$Article$ArticleTitle) %>%
          cbind() %>% 
          as_tibble() %>% 
          rename(text = c(1)) %>%
          unnest_wider(col = c(text)) %>% 
          select(text = c(1)) %>%
          unnest(cols = c(text)) %>% 
          mutate(text = str_squish(text)) %>% 
          rowid_to_column() %>% 
          pivot_wider(names_from = rowid, 
                      values_from = text) %>% 
          unite(text, c(1:ncol(.)), sep = " ") %>% 
          pull()})
    } else if (!is.null(pub_list$PubmedBookArticle$BookDocument$ArticleTitle)) {
      tryCatch({c(pub_list$PubmedBookArticle$BookDocument$ArticleTitle) %>%
          cbind() %>% 
          as_tibble() %>% 
          rename(text = c(1)) %>%
          unnest_wider(col = c(text)) %>% 
          select(text = c(1)) %>%
          mutate(text = str_squish(text)) %>% 
          rowid_to_column() %>% 
          pivot_wider(names_from = rowid, 
                      values_from = text) %>% 
          unite(text, c(1:ncol(.)), sep = " ") %>% 
          pull()
      })
    } else {
      NA_character_
    }
  
  # retrieve abstract
  Abstract <- 
    if (!is.null(pub_list$PubmedArticle$MedlineCitation$Article$Abstract$AbstractText)) {
      tryCatch({c(pub_list$PubmedArticle$MedlineCitation$Article$Abstract$AbstractText) %>%
          cbind() %>% 
          as_tibble() %>% 
          rename(text = c(1)) %>%
          unnest_wider(col = c(text)) %>% 
          select(text = c(1)) %>%
          mutate(text = str_squish(text)) %>% 
          rowid_to_column() %>% 
          pivot_wider(names_from = rowid, 
                      values_from = text) %>% 
          unite(text, c(1:ncol(.)), sep = " ") %>% 
          pull()})
    } else if (!is.null(pub_list$PubmedBookArticle$BookDocument$Abstract$AbstractText)) {
      tryCatch({c(pub_list$PubmedBookArticle$BookDocument$Abstract$AbstractText) %>%
          cbind() %>% 
          as_tibble() %>% 
          rename(text = c(1)) %>%
          unnest_wider(col = c(text)) %>% 
          select(text = c(1)) %>% 
          mutate(text = str_squish(text)) %>% 
          rowid_to_column() %>% 
          pivot_wider(names_from = rowid, 
                      values_from = text) %>% 
          unite(text, c(1:ncol(.)), sep = " ") %>% 
          pull()})
    } else {
      NA_character_
    }
  
  # retrieve journal name
  Journal <- 
    if (!is.null(pub_list$PubmedArticle$MedlineCitation$Article$Journal$Title)) { 
      pub_list$PubmedArticle$MedlineCitation$Article$Journal$Title
    } else if (!is.null(pub_list$PubmedBookArticle$BookDocument$Book$BookTitle$text)) {
      pub_list$PubmedBookArticle$BookDocument$Book$BookTitle$text
    } else {
      NA_character_
    }
  
  # retrieve volume
  Volume <- 
    if (!is.null(pub_list$PubmedArticle$MedlineCitation$Article$Journal$JournalIssue$Volume)) { 
      pub_list$PubmedArticle$MedlineCitation$Article$Journal$JournalIssue$Volume
    } else {
      NA_character_
    }
  
  # retrieve issue
  Issue <-
    if (!is.null(pub_list$PubmedArticle$MedlineCitation$Article$Journal$JournalIssue$Issue)) { 
      pub_list$PubmedArticle$MedlineCitation$Article$Journal$JournalIssue$Issue
    } else {
      NA_character_
    }
  
  # retrieve pages
  Pages <- 
    if (!is.null(pub_list$PubmedArticle$MedlineCitation$Article$Pagination$MedlinePgn)) { 
      pub_list$PubmedArticle$MedlineCitation$Article$Pagination$MedlinePgn
    } else {
      NA_character_
    }
  
  # retrieve month
  Month <- 
    if (!is.null(pub_list$PubmedArticle$MedlineCitation$Article$Journal$JournalIssue$PubDate$Month)) { 
      pub_list$PubmedArticle$MedlineCitation$Article$Journal$JournalIssue$PubDate$Month
    } else if (!is.null(pub_list$PubmedBookArticle$PubmedBookData$History$PubMedPubDate$Month)) {
      pub_list$PubmedBookArticle$PubmedBookData$History$PubMedPubDate$Month
    } else {
      NA_character_
    }
  
  # retrieve year
  Year <- 
    if (!is.null(pub_list$PubmedArticle$MedlineCitation$Article$Journal$JournalIssue$PubDate$Year)) { 
      pub_list$PubmedArticle$MedlineCitation$Article$Journal$JournalIssue$PubDate$Year
    } else if (!is.null(pub_list$PubmedBookArticle$PubmedBookData$History$PubMedPubDate$Year)) {
      pub_list$PubmedBookArticle$PubmedBookData$History$PubMedPubDate$Year
    } else {
      NA_character_
    }
  
  # retrieve dissemenation mode
  Outlet <- 
    if (!is.null(pub_list$PubmedArticle$MedlineCitation$Article$PublicationTypeList$PublicationType$text)) { 
      pub_list$PubmedArticle$MedlineCitation$Article$PublicationTypeList$PublicationType$text
    } else if (!is.null(pub_list$PubmedBookArticle$BookDocument$PMID$text)) {
      "Book"
    } else {
      NA_character_
    }
  
  # retrieve status
  PublicationType <- 
    if (!is.null(pub_list$PubmedArticle$PubmedData$PublicationStatus)) { 
      pub_list$PubmedArticle$PubmedData$PublicationStatus
    } else if (!is.null(pub_list$PubmedBookArticle$PubmedBookData$PublicationStatus)) {
      pub_list$PubmedBookArticle$PubmedBookData$PublicationStatus
    } else {
      NA_character_
    }
  
  # needed columns
  cols <- c("initialPmids[i]" = NA_character_,
            Affiliation = NA_character_, 
            ForeName = NA_character_, 
            Initials = NA_character_, 
            LastName = NA_character_,
            ORCID = NA_character_, 
            DOI = NA_character_, 
            Journal = NA_character_, 
            Title = NA_character_, 
            Abstract = NA_character_, 
            Volume = NA_character_, 
            Issue = NA_character_, 
            Pages = NA_character_, 
            Month = NA_character_, 
            Year = NA_character_, 
            PublicationType = NA_character_,
            Outlet = NA_character_)
  
  # combine with pmid
  RentrezRecords[[i]] <- 
    tryCatch({
      tibble(initialPmids[i], Affiliation, ForeName, Initials, LastName, ORCID, DOI, Journal, Title, Abstract, Volume, Issue, Pages, Month, Year, PublicationType, Outlet) %>%
        mutate(across(everything(), as.character)) %>%
        add_column(!!!cols[setdiff(names(cols), names(.))], .name_repair = "minimal") %>%
        distinct(initialPmids[i], .keep_all = TRUE) %>%
        mutate(Abstract = as.character(Abstract)) %>%
        filter(PublicationType != "Preprint")})
  
  flush.console()
}

rentrezList <- 
  bind_rows(RentrezRecords) %>%
  rename(PMID = `initialPmids[i]`) %>%
  mutate(Month = case_when( 
    (Month %in% "Jan" | Month %in% "01") ~ "1",
    (Month %in% "Feb" | Month %in% "02") ~ "2",
    (Month %in% "Mar" | Month %in%"03") ~ "3",
    (Month %in% "Apr" | Month %in% "04") ~ "4",
    (Month %in% "May" | Month %in% "05") ~ "5",
    (Month %in% "Jun" | Month %in% "06") ~ "6",
    (Month %in% "Jul" | Month %in% "07") ~ "7",
    (Month %in% "Aug" | Month %in% "08") ~ "8",
    (Month %in% "Sep" | Month %in% "09") ~ "9",
    (Month %in% "Oct" | Month %in% "10") ~ "10",
    (Month %in% "Nov" | Month %in% "11") ~ "11",
    (Month %in% "Dec" | Month %in% "12") ~ "12",
    TRUE ~ as.character(Month)
    )
  ) %>%
  mutate(Month = as.numeric(Month))  %>%
  mutate(Year = as.numeric(Year)) %>%
  mutate(ORCID = str_remove_all(ORCID, "https://orcid.org/")) %>%
  mutate(ORCID = str_remove_all(ORCID, "http://orcid.org/")) %>%
  rename_with(str_to_lower) %>%
  rename(type = publicationtype) %>% 
  select(-c(lastname, forename, initials, affiliation)) %>%
  mutate(across(everything(), as.character)) %>%
  distinct()

# Join pulls ----
draftList <- 
  pubmedList %>%
  power_left_join(rentrezList, conflict = coalesce_xy) %>%
  distinct()

autoapas <-
  cr_cn(draftList$doi,
        format = "text",
        style = "apa",
        .progress = "time") %>% 
  purrr::map_depth(1, ~ifelse(is.null(.x), NA, .x)) %>%
  lapply(`[[`, 1) %>%
  unlist(recursive = FALSE) %>%
  as_tibble_col(column_name = "apa")

FullList <-
  bind_cols(draftList, autoapas) %>%
  mutate(type = str_replace(type, "epublish", "Published Paper (Online)")) %>%
  mutate(type = str_replace(type, "ppublish", "Published Paper (Print)")) %>%
  mutate(type = str_replace(type, "aheadofprint", "In Press")) %>%
  mutate(abstract = str_remove_all(abstract, regex(str_c(
    #     "\\b",
    c('NlmCategory=',
      'Label=',
      '"CONCLUSION"',
      '"BACKGROUND"',
      '"PURPOSE"',
      '"INTRODUCTION"',
      '"OBJECTIVE"',
      '"OBJECTIVES"',
      '"BACKGROUND AND PURPOSE"',
      '"AIM"',
      '"AIMS"',
      '"UNASSIGNED"',
      '"UNLABELLED"',
      '"METHOD"',
      '"METHODS"',
      '"RESULTS"',
      '"BACKGROUND AND OBJECTIVE"',
      '"BACKGROUND AND OBJECTIVES"',
      '"BACKGROUND/AIMS"',
      '"METHODS AND MATERIALS"',
      '"RATIONALE,AIMS AND OBJECTIVES"',
      '"NEW FINDINGS"',
      '"DISCUSSION"',
      '"CASE"',
      '"PURPOSE/OBJECTIVE\\(S\\)"',
      '"QUESTIONS/PURPOSES"',
      '"AIMS AND OBJECTIVES"',
      '"RATIONALE, AIMS AND OBJECTIVES"',
      '"INTRODUCTION/BACKGROUND"',
      '"CONCLUSIONS"',
      '"FINDINGS"',
      '"Importance"',
      '"Context"'), 
    #    "\\b", 
    collapse = '|'), 
    ignore_case = TRUE))) %>%
  mutate(abstract = str_remove_all(abstract, '\\:')) %>%
  mutate(abstract = str_squish(abstract)) %>%
  mutate(doilink = paste0('<a href="https://doi.org/', doi,'" target="_blank"><img src="doi-logo.png" alt="HTML tutorial" style="width:35px;height:35px;"></a>')) %>%
  rename(citation = apa) %>%
  arrange(year, quarter, citation) %>%
  mutate(quarter = case_when(
    quarter == "Third" ~ "Q3",
    quarter == "Fourth" ~ "Q4", 
    quarter == "First" ~ "Q1", 
    quarter == "Second" ~ "Q2"
    )
  ) %>%
  mutate(Timeframe = case_when(
    (year == 2009 | year == 2010 | year == 2011 | year == 2012 | year == 2013 | year == 2014 | year == 2015 | year == 2016 | (year == 2017 & (quarter == "Q3" | quarter == "Q4"))) ~ "years 1-5",
    ((year == 2017 & (quarter == "Q1" | quarter == "Q2")) |  (year == 2018 & (quarter == "Q3" | quarter == "Q4"))) ~ "Y6",
    ((year == 2018 & (quarter == "Q1" | quarter == "Q2")) |  (year == 2019 & (quarter == "Q3" | quarter == "Q4"))) ~ "Y7",
    ((year == 2019 & (quarter == "Q1" | quarter == "Q2")) |  (year == 2020 & (quarter == "Q3" | quarter == "Q4"))) ~ "Y8",
    ((year == 2020 & (quarter == "Q1" | quarter == "Q2")) |  (year == 2021 & (quarter == "Q3" | quarter == "Q4"))) ~ "Y9",
    ((year == 2021 & (quarter == "Q1" | quarter == "Q2")) |  (year == 2022 & (quarter == "Q3" | quarter == "Q4"))) ~ "Y10",
    ((year == 2022 & (quarter == "Q1" | quarter == "Q2")) |  (year == 2023 & (quarter == "Q3" | quarter == "Q4"))) ~ "Y11"
    )
  ) %>%
  unite(reporting, c(Timeframe, quarter), sep = "") %>%
  mutate(reporting = case_when(
    str_detect(reporting, 
               paste(c("years 1-5Q1", "years 1-5Q2", "years 1-5Q3", "years 1-5Q4"), collapse = '|')) ~ "Y1-5",
    TRUE ~ as.character(reporting)
    )
  ) %>%
  mutate(reporting = factor(
    reporting, 
    levels = c("Y1-5", 
               "Y6Q1", "Y6Q2", "Y6Q3", "Y6Q4",
               "Y7Q1", "Y7Q2", "Y7Q3", "Y7Q4",
               "Y8Q1", "Y8Q2", "Y8Q3", "Y8Q4",
               "Y9Q1", "Y9Q2", "Y9Q3", "Y9Q4",
               "Y10Q1", "Y10Q2", "Y10Q3", "Y10Q4",
               "Y11Q1", "Y11Q2", "Y11Q3", "Y11Q4")
        )
    ) %>%
  distinct() %>%
  droplevels()%>%
  mutate(quarter = case_when(
    (month == 1 | month == 2 | month == 3)  ~ "Third",
    (month == 4 | month == 5 | month == 6)  ~ "Fourth",
    (month == 7 | month == 8 | month == 9)  ~ "First",
    (month == 10 | month == 11 | month == 12)  ~ "Second"
  )
  ) %>%
  mutate(doilink = na_if(doilink, '<a href="https://doi.org/NA" target="_blank"><img src="doi-logo.png" alt="HTML tutorial" style="width:35px;height:35px;"></a>')); FullList

# Build APA 7th (if needed) ----
manualapas <- 
  FullList %>%
  power_left_join(authorPubmed, by = "pmid", conflict = coalesce_xy) %>%
  mutate(year = as.character(year)) %>%
  mutate(title = replace_html(title)) %>% 
  mutate(across(where(is.character), str_squish)) %>%
  arrange(title, order) %>%
  group_by(title, order) %>%
  arrange(year, month) %>% 
  mutate(FirstInitial = substr(initials, 1, 1)) %>%
  mutate(SecondInitial = substr(initials, 2, 2)) %>%
  mutate(ThirdInitial = substr(initials, 3, 3)) %>%
  mutate(FourthInitial = substr(initials, 4, 4)) %>%
  select(-initials) %>%
  ungroup() %>%
  mutate_at(vars(colnames(.)),
            .funs = list(~ifelse(.=="", NA, as.character(.)))) %>%
  unite(initials, c("FirstInitial", "SecondInitial", "ThirdInitial", "FourthInitial"), sep = ".") %>%
  mutate(initials = str_replace_all(initials, 'NA.?', '')) %>%
  select(pmid, lastname, initials, year, title, journal, volume, issue, pages, pmid, doi, quarter, year, cited) %>%
  unite(head, c(lastname, initials), sep = ", ") %>%
  mutate(tail = paste0("(", year, "). ", title, ", ", journal, ", ", volume, "(", issue, ")", ", ", pages, ". doi: ", doi)) %>%
  mutate(group_id = group_indices(.,pmid)) %>%
  group_by_at(vars(-head)) %>% 
  mutate(row_id = 1:n()) %>%
  spread(key = row_id, value = head) %>%
  unite(authors, c(13:ncol(.)), sep = ", ") %>%
  mutate(authors = stringr::str_replace_all(authors, 'NA,?', '')) %>%
  mutate(authors = gsub(',[^,]*$', '', authors)) %>%
  separate(authors, into = c("P1", "P3"), sep=",(?=[^,]+$)") %>%
  separate(P1, into = c("Main", "P2"), sep=",(?=[^,]+$)") %>%
  unite(Last, c("P2", "P3"), sep = ",", na.rm = TRUE) %>%
  unite(authors, c("Main", "Last"), sep = " &", na.rm = TRUE)  %>%
  unite(citation, c("authors", "tail"), sep = " ", na.rm = TRUE) %>%
  ungroup() %>%
  distinct() %>%
  droplevels() %>%
  mutate(pubmedlink = paste0('<a href="https://pubmed.ncbi.nlm.nih.gov/',pmid,'" target="_blank"><img src="pubmed-logo.png" alt="HTML tutorial" style="width:35px;height:35px;"></a>')) %>%
  select(pmid, citation, pubmedlink, cited); manualapas

# Final Join APA tables ---- 
reflist <- 
  FullList %>%
  mutate(cited = as.character(cited)) %>%
  power_left_join(manualapas, by = "pmid", conflict = coalesce_xy) %>%
  rename_with(str_to_title) %>%
  rename(`Publication Status` = Type) %>%
  rename(`Embargo Date` = Embargo) %>%
  rename(`ORCiD iD` = Orcid) %>%
  rename(DOI = Doi) %>%
  rename(`Pubmed PMID` = Pmid) %>%
  rename(`DOI Link` = Doilink) %>%
  rename(`Pubmed Link` = Pubmedlink) %>%
  rename(`Citation Count` = Cited) %>%
  select(Reporting, `Pubmed PMID`, `Pubmed Link`, `Publication Status`, Citation, DOI, `DOI Link`, Title, Journal, Abstract, Month, Year, Quarter, `Embargo Date`, `Citation Count`) %>%
  arrange(Year, Quarter);  reflist

# Update counts table ----
refcounts <-
  pubcounts %>%
  mutate(ping = case_when(
          str_detect(Status, "Published") ~ "1",
          TRUE ~ NA_character_)
         ) %>%
  left_join(citationsCounts %>%
              add_column(ping = "1")) %>%
  select(-ping) %>%
  mutate(across(everything(), as.character)) %>%
  mutate(across(c(Citations), ~ replace_na(., ""))); refcounts

write_csv(refcounts, "pubcountsTable.csv")
write_csv(reflist, "pubslistTable.csv")
  
# Save a local image of the pull
save.image()
