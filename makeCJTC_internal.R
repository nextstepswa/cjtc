rm(list = ls())
library(tidyverse)
library(kableExtra)
library(readxl)

# Raw data ----
cjtc_internal_raw <- read_excel("Data/Raw/cert_bureau_data_2025-09-25T20_44_26.690079789Z.xlsx", 
                       col_types = c("text", "text", "text", 
                                     "date", "date", "text", "text", "text", 
                                     "text", "text", "text", "text", "text", 
                                     "text", "text", "text", "text", "numeric", 
                                     "text", "text", "text", "text", "text", 
                                     "date", "text", "date", "date", "date", 
                                     "date", "date", "date", "date", "date", 
                                     "date", "date", "date", "text", "text"))

# Prep for analysis and merge with public DB ----

## Fix missing names with documents link when possible ----
## We need names to properly de-duplicate and match to public DB

cjtc_namefix <- cjtc_internal_raw %>%

  ## harmonize url entries for anomalies
  mutate(url = case_when(
    grepl("-PD", url) ~ sub("-PD", "PD", url),
    grepl("Police Department", url) ~ sub("Police Department", "PD", url),
    TRUE ~ url)
  ) %>%
    
  ## pull url names when possible
  mutate(
    first_name = case_when(
      !is.na(first_name) ~ first_name,
      is.na(first_name) & !is.na(url) ~ str_split_i(url, "-", 4),
      TRUE ~ "Missing"),
    
    last_name = case_when(
      !is.na(last_name) ~ last_name,
      is.na(last_name) & !is.na(url) ~ str_split_i(url, "-", 3),
      TRUE ~ "Missing")
  )
 
## Deduplicate ---- 
## The raw data contain many duplicate records, making it different than the public DB.
## * one case per RCW offense
## * additional cases for "tag" variable

## dedupe_all:
## "Case" defined as an name+case_number
## * names are NA in some cases, so we insert "Missing" token to preserve these records
## * note: if a case number is associated with multiple Missing names, the result may be incorrect
##   if it collapses across different unnamed persons

## Group by case/fname/lname -- combine info on all vars with summarize
## drop groups to get a non-grouped df
cjtc_internal_dedupe_all <- cjtc_namefix %>%
  mutate(across(contains("name"), ~replace_na(., "Missing"))) %>%
  group_by(case_number, first_name, last_name) %>%
  summarise(nrec = n(),
            across(everything(), ~paste(unique(.x), collapse="; ")), .groups = "drop") %>%
  mutate(internal_source = "dedupe_all") %>%
  mutate(across(where(is.character), ~if_else(. == "NA", NA, .))) # recover NAs


## Merge with public DB ----

load(file = "Data/cjtc_public.rda")
cjtc_public <- cjtc_public %>%
  mutate(public_source = "public all",
         case_number = case)

## Merge based on case_number, first_name and last_name:
merge_all <- full_join(cjtc_internal_dedupe_all, 
                       cjtc_public %>% select(case_number, 
                                              first_name, 
                                              last_name,
                                              public_source,
                                              public_legacy_casenum = legacy_case_number,
                                              public_offense = conduct_investigated,
                                              public_status = status, 
                                              public_outcome = case_determination,
                                              public_url = documents_link)
                       )

## Separate the results
public_only <- merge_all %>% filter(is.na(internal_source)) %>% mutate(public_source = "public_only")
internal_only <- merge_all %>% filter(is.na(public_source)) %>% mutate(internal_source = "internal_only")
match_casenum_name <- merge_all %>% 
  filter(!is.na(internal_source) & !is.na(public_source)) %>%
  mutate(match = "casenum_name")

## Public only ----
## Most are missing names
## Find residual matches using case number alone
### some match to multiple internal records
### some match to a unique internal record
### a handful of new cases in public were not included in 9/25/2024 internal PDF

public_only %>% count(last_name) %>% arrange(desc(n)) %>% print(n=30)

### Join residual public only to internal only by case number alone -- all but 5 records match
casenum_match_residual <- public_only %>%
  select(case_number, first_name, last_name, public_source) %>%
  left_join(., internal_only %>% select(-public_source), 
            by = join_by(case_number),
            suffix = c(".pub", ".int")) %>%
  mutate(match = "casenum")

#### Unmatched ----
# Remaining public only cases (2 casenum/5 records; rcd post-internal PDR?)
public_unmatched <- casenum_match_residual %>%
  filter(is.na(internal_source)) %>%
  select(case_number, first_name = first_name.pub, last_name = last_name.pub) %>%
  left_join(cjtc_public) %>%
  mutate(match = "unmatched",
         public_source = "public_only",
         internal_source = NA)

public_unmatched %>% select(case_number, last_name, first_name, date_received, status)

#### Multiply matched ----
# public only case_number matches to multiple internal only records
# 18 casenum/42 records; none tagged as officers:  ***ASK ABOUT THESE***
# These are currently NOT INCLUDED in the final dataset

casenum_match_mult <- casenum_match_residual %>% 
  filter(!is.na(internal_source)) %>%
  count(case_number, public_source) %>% 
  filter(n>1) %>%
  left_join(internal_only %>% select(-public_source)) %>%
  mutate(match = "casenum, non-unique")

casenum_match_mult %>% count(tag) %>% arrange(desc(n))
casenum_match_mult %>% group_by(case_number) %>% count() %>% arrange(desc(n)) %>% print(n=25)

#### Uniquely matched ----
# case_number-only matches to unique internal record
casenum_match_unique <- casenum_match_residual %>% 
  filter(!is.na(internal_source)) %>%  
  count(case_number, public_source) %>% 
  filter(n==1) %>%
  select(case_number, public_source)

### Unique match df based on casenum alone
match_casenum <- casenum_match_unique %>%
  left_join(internal_only %>% select(-contains("public"))) %>% 
  left_join(public_only %>% 
              select(
                case_number, 
                public_first_name = first_name, 
                public_last_last_name = last_name,
                public_legacy_casenum,
                public_offense,
                public_status, 
                public_outcome,
                public_url),
            join_by(case_number)
  ) %>%
  mutate(match = "casenum, unique")

### Add unique casenum only matches to final matched dataset
match_final <- bind_rows(
  match_casenum_name,
  match_casenum
)



## Internal-only ----

# Filter out casenum only matches from above to get internal_unmatched

casenum_matches <- bind_rows(casenum_match_unique %>% select(case_number),
                             casenum_match_mult %>% select(case_number) %>% distinct(case_number))

internal_unmatched <- internal_only %>%  
  anti_join(casenum_matches) %>%
  mutate(internal_source = "internal_only") %>%
  mutate(match = NA)

### Dx internal unmatched ----
# Lots of casenums with multiple records
internal_unmatched %>% group_by(case_number) %>% count() %>% with(., table(n))

# Some are linked to cases we have already matched
internal_unmatched_casenums <- internal_unmatched %>% 
  group_by(case_number) %>% count() %>%
  mutate(internal_source = "internal_unmatched")

match_final_casenums <- match_final %>% 
  group_by(case_number) %>% count() %>%
  mutate(source = "match final")

linked_internal_unmatched <- internal_unmatched_casenums %>%
  left_join(match_final_casenums) %>%
  filter(source == "match final")

# Not all Intake/Intake review status
internal_unmatched %>% count(status)

# Non-intake related cases include certified officers
internal_unmatched %>% filter(!grepl("Intake", status)) %>% count(tag) %>% arrange(desc(n))

# Officer cases:  All casenums from 2024, most closed
internal_unmatched %>% filter(!grepl("Intake", status) & grepl("Officer", tag)) %>% 
  select(case_number, first_name, last_name, tag, status, case_determination) %>% print(n=30)


# Final internal dedupe draft ----
## internal unmatched with grepl("Intake", status) | grepl("Officer", tag)
## matched cases

cjtc_internal_draft <- internal_unmatched %>% 
  filter(grepl("Intake", status) | grepl("Officer", tag)) %>%
  mutate(source = "internal unmatched") %>%
  bind_rows(match_final %>% mutate(source = "match_final"))
  
cjtc_internal_draft %>% count(source)
cjtc_internal_draft %>% with(., table(tag, source, useNA = "a"))

cjtc_internal_excluded <- anti_join(cjtc_internal_dedupe_all, cjtc_internal_draft,
                                    by = join_by(case_number, first_name, last_name))
  

# Harmonize ----
cjtc_internal <- cjtc_internal_draft %>%

  mutate(across(where(is.character), ~ ifelse(. == "", NA, .)),
    
  ## Case status and outcome ----
  # factor level order by sequence
  status = factor(status,
                  levels = c("Intake",
                            "Intake Review",
                            "Investigation",
                            "Admin Review",
                            "AAG (Legal)",
                            "Hearing Coordinator",
                            "Discipline - Post Hearing",
                            "Appeal - Post Discipline",
                            "Assistant Director Review",
                            "Settlement",
                            "Open - Pending Expiration",
                            "Closed"
                  )),
  
  status3 = factor(
    case_when(
      is.na(status) ~ NA,
      status == "Closed" ~ "Closed",
      grepl("Intake", status) ~ "Intake and review",
      TRUE ~ "Active investigation"
    ), 
    levels = c("Intake and review", "Active investigation", "Closed")),
  
  # Note open cases for outcome and outcat exclude Settlements
  outcome = case_when(
    #is.na(case_determination) ~ "Missing",
    grepl("Settl", case_determination) ~ "Settlement (still open)",
    grepl("Open", status3) ~ "Still open", 
    grepl("Admin", case_determination) ~ "Admin closure",
    grepl("Revoc", case_determination) ~ "Cert revoked",
    grepl("Discipline", case_determination) ~ "Disciplined",
    case_determination == "Decline" ~ "Decline to charge",
    grepl("Dismiss", case_determination) ~ "Dismissed",
    TRUE ~ "Other"), 
  
  outcome = fct_relevel(outcome, "Still open", after = Inf),
  #outcome = fct_relevel(outcome, "Missing", after = Inf),
  
  outcat = case_when(
    outcome == "Still open" ~ "Still Open",
    grepl("Duplicate", case_determination) ~ "Duplicate",
    grepl("Admin", case_determination) ~ "Admin Closure",
    grepl("Decline|Dismiss|Settl", case_determination) ~ "Declined/Dismissed/Settlement",
    grepl("Revo|Denied|Disc", case_determination) ~ "Revoked/Denied Cert/Disciplined",
    #outcome == "Missing" ~ "Missing",
    TRUE ~ "Other"),
  outcat = fct_relevel(outcat,
                       "Admin Closure",
                       "Duplicate",
                       "Declined/Dismissed/Settlement",
                       "Revoked/Denied Cert/Disciplined",
                       "Other",
                       "Still Open",
                       #"Missing"
                       ),
  
  revoke_type = if_else(grepl("Revoc", case_determination), case_determination, NA_character_),
  admin_close_type = if_else(grepl("Admin", case_determination), case_determination, NA_character_),

  ## Dates / Times / Durations ----
  
  ### some odd formats and duplications in case/date closed, use case_closed rather than date_closed
  date_closed = as.Date(case_closed, format = "%Y-%m-%d"),
  date_received = as.Date(date_received, format = "%Y-%m-%d"),
  
  yr_r = lubridate::year(date_received),
  yr_c = lubridate::year(date_closed),
  mo_r = lubridate::month(date_received),
  mo_c = lubridate::month(date_closed),
  yr_mo_r = paste0(yr_r, "-", mo_r),
  yr_mo_c = paste0(yr_c, "-", mo_c),
  
  
  ### Overall duration (if closed) / age (if open) / length (both) ----
  case_duration = if_else(is.na(date_closed), NA_real_, as.numeric(difftime(date_closed, date_received, units="weeks"))),
  case_age = if_else(is.na(date_closed), as.numeric(difftime(Sys.Date(), date_received, units="weeks")), NA_real_),
  case_length = ifelse(!is.na(case_duration), case_duration, case_age),
  
  case_duration_mo = round(case_duration/4),
  case_age_mo = round(case_duration/4),
  case_length_mo = round(case_length/4),
  
  case_duration_yr = round(case_duration/52, 1),
  case_age_yr = round(case_duration/52, 1),
  case_length_yr = round(case_length/52, 1),
  
  ### Rename interim dates for consistency
  date_intake_rev = as.Date(intake_reveiw_date, format = "%Y-%m-%d"),
  date_investigation = as.Date(investigation_asgnd_date, format = "%Y-%m-%d"),
  date_admin_rev = as.Date(administrative_review_date, format = "%Y-%m-%d"),
  date_to_aag = as.Date(to_aag_date, format = "%Y-%m-%d"),
  date_soc = as.Date(statement_of_charges_date, format = "%Y-%m-%d"),
  date_to_hc = as.Date(to_hearing_coordinator_date, format = "%Y-%m-%d"),
  date_hearing = as.Date(hearing_date, format = "%Y-%m-%d"),
  date_fo = as.Date(final_order_date, format = "%Y-%m-%d"),
  date_ad_rev = as.Date(assistant_director_review_date, format = "%Y-%m-%d"),
  date_ed_rev = as.Date(executive_director_review_date, format = "%Y-%m-%d"),
  date_dir_rev = if_else(is.na(date_ad_rev),  date_ed_rev, date_ad_rev),
  date_dir_rev_done = as.Date(ed_ad_review_complete_date, format = "%Y-%m-%d"),
  
  ### Time in sequential status pairs ----
  time_rec_intrev = difftime(date_intake_rev, date_received, units="weeks"),
  time_intrev_inv = difftime(date_investigation, date_intake_rev, units="weeks"),
  time_inv_admin = difftime(date_admin_rev, date_investigation, units="weeks"),
  time_admin_aag = difftime(date_to_aag, date_admin_rev, units="weeks"),
  time_aag_soc = difftime(date_soc, date_to_aag, units="weeks"),
  time_soc_hcoord = difftime(date_to_hc, date_soc, units="weeks"),
  time_hcoord_h = difftime(date_hearing, date_to_hc, units="weeks"),
  time_h_fo = difftime(date_fo, date_hearing, units="weeks"),
  time_dirrev_comp = difftime(date_dir_rev, date_dir_rev_done, units="weeks"),
  time_dirrev_clo = difftime(date_closed, date_dir_rev, units="weeks"),
  
  ### Last observed date and status ----
  last_date = case_when(
    !is.na(date_closed) ~ date_closed,
    !is.na(date_dir_rev) ~ date_dir_rev,
    !is.na(date_fo) ~ date_fo,
    !is.na(date_hearing) ~ date_hearing,
    !is.na(date_to_hc) ~ date_to_hc,
    !is.na(date_soc) ~ date_soc,
    !is.na(date_to_aag) ~ date_to_aag,
    !is.na(date_admin_rev) ~ date_admin_rev,
    !is.na(date_investigation) ~ date_investigation,
    !is.na(date_intake_rev) ~ date_intake_rev,
    !is.na(date_received) ~ date_received
  ),
  
  last_status_with_date = case_when(
    !is.na(date_closed) ~ "Closed",
    !is.na(date_dir_rev) ~ "Director review",
    !is.na(date_fo) ~ "Final order",
    !is.na(date_hearing) ~ "Hearing",
    !is.na(date_to_hc) ~ "Hearing coordinator",
    !is.na(date_soc) ~ "Statement of charges",
    !is.na(date_to_aag) ~ "AAG (Legal)",
    !is.na(date_admin_rev) ~ "Administrative review",
    !is.na(date_investigation) ~ "Investigation",
    !is.na(date_intake_rev) ~ "Intake review",
    !is.na(date_received) ~ "Received"
  ),
  last_status_with_date = factor(
    last_status_with_date, 
    levels=c(
      "Received",
      "Intake review",
      "Investigation",
      "Administrative review",
      "AAG (Legal)",
      "Statement of charges",
      "Hearing coordinator",
      "Hearing",
      "Final order",
      "Director review",
      "Closed")
  ),
  
  ### Period received and closed ----
  period_r = case_when(
    date_received < as.Date("2021-06-20") ~ "pre.5051",
    date_received > as.Date("2022-06-30") ~ "current",
    TRUE ~ "interim"),
  period_r = factor(period_r, levels = c("pre.5051", "interim", "current")),
  
  period_c = case_when(
    is.na(date_closed) ~ "no close date",
    date_closed < as.Date("2021-06-20") ~ "pre.5051",
    date_closed > as.Date("2022-06-30") ~ "current",
    TRUE ~ "interim"),
  period_c = factor(period_c, levels = c("pre.5051", "interim", "current", "no close date")),
    
  length_yr_tc = ifelse(
    case_length_yr > 5, 6, case_length_yr),
  
 
  
  ### Offense tracking ----
  ### raw internal format is one RCW per record
  ### dedupe format is like public DB: multiple RCW per record
  
  num_offenses = str_count(rcw_laws, ";") + 1,
  offense = case_when(
    str_count(rcw_laws, ";") == 1 ~ "2 offenses",
    str_count(rcw_laws, ";") == 2 ~ "3 offenses",
    str_count(rcw_laws, ";") == 3 ~ "4 offenses",
    str_count(rcw_laws, ";") == 4 ~ "5 offenses",
    str_count(rcw_laws, ";") == 5 ~ "6 offenses",
    str_count(rcw_laws, ";") == 6 ~ "7 offenses",
    str_count(rcw_laws, ";") == 7 ~ "8 offenses",
    str_count(rcw_laws, ";") == 8 ~ "9 offenses",
    str_count(rcw_laws, ";") == 9 ~ "10 offenses",
    TRUE ~ rcw_laws),
  offense = fct_relevel(factor(offense), "10 offenses", after = 8)
  
  )

# Offense list ----

# Create a list of all offenses that appear in the "rcw_laws" field and
# assign them the CJTC priority ranking
# case number and updated first/last name will be used to aggregate and merge back

offense_list_internal <- data.frame(
  cjtc_internal %>% select(case_number, last_name, first_name, period_r, status, num_offenses),

  # split the offense vector into separate columns
  str_split(cjtc_internal_draft$rcw_laws, ";", simplify=T)) %>%
  
  filter(!is.na(X1)) %>% # remove records with offense = NA
  pivot_longer(cols=X1:X10, names_to = "offense_num", values_to = "offense") %>%
  filter(offense != "NA" & offense != "") %>%
  
  # bind_rows(.,
  #           cjtc %>% 
  #             filter(num_offenses==1) %>% 
  #             select(period_r, offense_tot = num_offenses, offense = rcw_laws)) %>%
  
  mutate(offense = sub(" R", "R", offense),
         offense = sub("ofR", "of R", offense),
         offense = sub("seR", "se R", offense),
         
         offense_rcw = sub(" - .*", "", offense),
         offense_desc = if_else(!grepl("-", offense), NA_character_, sub(".* - ", "", offense))
  ) %>%
  mutate(
    offense_classification = factor(
      case_when(
        grepl("RCW 43.101.105\\(2\\)", offense) ~ "Mandatory",
        grepl("RCW 43.101.105\\(3\\)", offense) ~ "Discretionary",
        TRUE ~ "Not classified"),
      levels = c("Mandatory", "Discretionary", "Not classified")
    ),
    offense_priority_emp = case_when(
      grepl("RCW 43.101.105\\(2\\)\\(a\\)\\(i\\)\\(A\\)" , offense) ~ 1,
      grepl("RCW 43.101.105\\(2\\)\\(a\\)\\(i\\)\\(B\\)" , offense) ~ 1,
      grepl("RCW 43.101.105\\(2\\)\\(a\\)\\(ii\\)\\(B\\)" , offense) ~ 1,
      grepl("RCW 43.101.105\\(2\\)\\(c\\)\\(i\\)" , offense) ~ 1,
      grepl("RCW 43.101.105\\(2\\)\\(d\\)" , offense) ~ 1,
      grepl("RCW 43.101.105\\(2\\)\\(e\\)" , offense) ~ 1,
      grepl("RCW 43.101.105\\(3\\)\\(a\\)" , offense) ~ 1,
      grepl("RCW 43.101.105\\(3\\)\\(b\\)" , offense) ~ 1,
      grepl("RCW 43.101.105\\(3\\)\\(c\\)" , offense) ~ 1,
      grepl("RCW 43.101.105\\(3\\)\\(d\\)\\(i\\)" , offense) ~ 1,
      grepl("RCW 43.101.105\\(3\\)\\(d\\)\\(ii\\)" , offense) ~ 1,
      grepl("RCW 43.101.105\\(3\\)\\(d\\)\\(iii\\)" , offense) ~ 1,
      grepl("RCW 43.101.105\\(3\\)\\(e\\)" , offense) ~ 2,
      grepl("RCW 43.101.105\\(3\\)\\(f\\)" , offense) ~ 2,
      grepl("RCW 43.101.105\\(3\\)\\(g\\)" , offense) ~ 2,
      grepl("RCW 43.101.105\\(3\\)\\(h\\)" , offense) ~ 2,
      grepl("RCW 43.101.105\\(3\\)\\(i\\)" , offense) ~ 1,
      grepl("RCW 43.101.105\\(3\\)\\(j\\)\\(i\\)" , offense) ~ 1,
      grepl("RCW 43.101.105\\(3\\)\\(j\\)\\(ii\\)" , offense) ~ 1,
      grepl("RCW 43.101.105\\(3\\)\\(j\\)\\(iii\\)" , offense) ~ 1,
      grepl("RCW 43.101.105\\(3\\)\\(j\\)\\(iv\\)" , offense) ~ 2,
      grepl("RCW 43.101.105\\(3\\)\\(k\\)" , offense) ~ 4, # separated for misconduct
      grepl("RCW 43.101.105\\(3\\)\\(l\\)" , offense) ~ 5), # voluntary surrender
    offense_priority_sep = case_when(
      grepl("RCW 43.101.105\\(2\\)\\(a\\)\\(i\\)\\(A\\)" , offense) ~ 2,
      grepl("RCW 43.101.105\\(2\\)\\(a\\)\\(i\\)\\(B\\)" , offense) ~ 2,
      grepl("RCW 43.101.105\\(2\\)\\(a\\)\\(ii\\)\\(B\\)" , offense) ~ 2,
      grepl("RCW 43.101.105\\(2\\)\\(c\\)\\(i\\)" , offense) ~ 3,
      grepl("RCW 43.101.105\\(2\\)\\(d\\)" , offense) ~ 2,
      grepl("RCW 43.101.105\\(2\\)\\(e\\)" , offense) ~ 3,
      grepl("RCW 43.101.105\\(3\\)\\(a\\)" , offense) ~ 3,
      grepl("RCW 43.101.105\\(3\\)\\(b\\)" , offense) ~ 3,
      grepl("RCW 43.101.105\\(3\\)\\(c\\)" , offense) ~ 3,
      grepl("RCW 43.101.105\\(3\\)\\(d\\)\\(i\\)" , offense) ~ 3,
      grepl("RCW 43.101.105\\(3\\)\\(d\\)\\(ii\\)" , offense) ~ 3,
      grepl("RCW 43.101.105\\(3\\)\\(d\\)\\(iii\\)" , offense) ~ 3,
      grepl("RCW 43.101.105\\(3\\)\\(e\\)" , offense) ~ 3,
      grepl("RCW 43.101.105\\(3\\)\\(f\\)" , offense) ~ 3,
      grepl("RCW 43.101.105\\(3\\)\\(g\\)" , offense) ~ 3,
      grepl("RCW 43.101.105\\(3\\)\\(h\\)" , offense) ~ 3,
      grepl("RCW 43.101.105\\(3\\)\\(i\\)" , offense) ~ 3,
      grepl("RCW 43.101.105\\(3\\)\\(j\\)\\(i\\)" , offense) ~ 3,
      grepl("RCW 43.101.105\\(3\\)\\(j\\)\\(ii\\)" , offense) ~ 3,
      grepl("RCW 43.101.105\\(3\\)\\(j\\)\\(iii\\)" , offense) ~ 3,
      grepl("RCW 43.101.105\\(3\\)\\(j\\)\\(iv\\)" , offense) ~ 3,
      grepl("RCW 43.101.105\\(3\\)\\(k\\)" , offense) ~ 4, # separated for misconduct
      grepl("RCW 43.101.105\\(3\\)\\(l\\)" , offense) ~ 5) # voluntary surrender
  )

# Aggregate offense priority rankings back into case/lname/fname list
offense_by_case_internal <- offense_list_internal %>%
  group_by(case_number, last_name, first_name) %>%
  summarize(n_offenses = n(), # just a check to verify against num_offenses
            unique_offenses = paste(unique(offense_rcw), collapse = "; "),
            num_mandatory_offenses = sum(offense_classification=="Mandatory"),
            num_discretionary_offenses = sum(offense_classification=="Discretionary"),
            separated = sum(offense_priority_emp==4) + sum(offense_priority_sep==4),
            surrendered = sum(offense_priority_emp==5) + sum(offense_priority_sep==5),
            num_ranked_emp = sum(offense_priority_emp < 3),
            num_ranked_sep = sum(offense_priority_sep < 3),
            max_offense_priority_emp = min(offense_priority_emp, na.rm=T), # worst is lowest
            max_offense_priority_sep = min(offense_priority_sep, na.rm=T),
            .groups = "drop",
  ) %>%
  mutate(across(contains("max"), ~na_if(., Inf)))

# Merge offense priority rankings back on to cases
cjtc_internal <- left_join(cjtc_internal, offense_by_case_internal,
                         by = join_by(case_number, last_name, first_name))

# Save out ----
  
save(list=c("cjtc_internal_raw", "cjtc_internal",
            "offense_list_internal", "offense_by_case_internal" ),
     file="Data/cjtc_internal.rda")
