rm(list = ls())

library(tidyverse)
library(kableExtra)

#install.packages('RSocrata', repos ="https://chicago.r-universe.dev")
cjtc_public_raw <- RSocrata::read.socrata("https://data.wa.gov/resource/r5ki-dmfz.csv")

cjtc_public <- cjtc_public_raw %>%
  
  mutate(
    across(c(officer_name:conduct_investigated, documents_link), ~ ifelse(. == "", NA, .))
  ) %>%
    
  ## fix missing names with documents link when possible ----
  
  ### harmonize documents-link entries for anomalies
  mutate(documents_link = case_when(
    grepl("-PD", documents_link) ~ sub("-PD", "PD", documents_link),
    grepl("Police Department", documents_link) ~ sub("Police Department", "PD", documents_link),
    TRUE ~ documents_link)
  ) %>%
  
  ### pull missing names from documents_link when possible
  mutate(
    first_name = case_when(
      !is.na(officer_first_name) ~ officer_first_name,
      is.na(officer_first_name) & !is.na(documents_link) ~ str_split_i(documents_link, "-", 4),
      TRUE ~ "Missing"),
    
    last_name = case_when(
      !is.na(officer_name) ~ officer_name,
      is.na(officer_name) & !is.na(documents_link) ~ str_split_i(documents_link, "-", 3),
      TRUE ~ "Missing"),

    # fix problem cases (id'd during merge with internal data)
    first_name = if_else(first_name == "OBrien", "O'Brien", first_name),
    last_name = if_else(last_name == "Madkou", "Madkour", last_name),
    
    delete = if_else(case=="2024-0010920" & is.na(first_name), 1, 0) # dupe case Medelez
        ) %>%
  # delete dupe case
  filter(delete == 0) %>%
  select(-delete) %>%
  
  # mop up last NAs
  mutate(across(contains("name"), ~replace_na(., "Missing"))) %>%

  mutate(
    yr_r = lubridate::year(date_received),
    yr_c = lubridate::year(date_closed),
    case_duration = if_else(is.na(date_closed), 
                            NA_real_, 
                            as.numeric(difftime(date_closed, date_received, units="weeks"))),
    case_age = if_else(is.na(date_closed), 
                       as.numeric(difftime(Sys.Date(), date_received, units="weeks")), 
                       NA_real_),
    case_length = ifelse(!is.na(case_duration), case_duration, case_age),
    
    case_duration_mo = round(case_duration/4),
    case_age_mo = round(case_duration/4),
    case_length_mo = round(case_length/4),
    
    case_duration_yr = round(case_duration/52, 1),
    case_age_yr = round(case_duration/52, 1),
    case_length_yr = round(case_length/52, 1),
    
    # Stage specific time in status ##############################################################################
    ## There are 5 dates recorded: received, investigation, statement_of_charges, hearing, final_order, date_closed
    ## Cases take many different routes through this process, so may be closed without going through all states
    
    ## For admin closures we calculate date closed - date received
    ## For other states, we calculate the difference between 
    ##    each adjacent pair
    ##    entry and date_closed if adjacent date is missing
    ##    age in state if all later dates are missing
    
    ## Received 
    ### to admin closure (not forwarded for investigation)
    recv_adminclose_duration = if_else(grepl("Admin", case_determination), 
                                       as.numeric(difftime(date_closed, date_received, units="weeks")), 
                                       NA_real_),
    
    ### to assigned to investigator (if forwarded)
    recv_investigation_duration = as.numeric(difftime(investigation, date_received, units="weeks")),
    
    ### if still in state received
    recv_age = if_else(is.na(investigation) & is.na(date_closed), 
                       as.numeric(difftime(Sys.time(), date_received, units="weeks")), 
                       NA_real_),
    
    ## Investigation: 
    ### to statement of charges (soc)
    review_soc_duration = as.numeric(difftime(statement_of_charges, investigation, units="weeks")),
    
    ### to closure if closed w/o soc:  decline, settlement, voluntary surrender
    review_close_duration = if_else(is.na(statement_of_charges), 
                                    as.numeric(difftime(date_closed, investigation, units="weeks")), 
                                    NA_real_),
    
    ### if still in state investigation
    review_age = if_else(!is.na(investigation) & is.na(statement_of_charges) & is.na(date_closed), 
                         as.numeric(difftime(Sys.time(), investigation, units="weeks")), 
                         NA_real_),
    
    ## Statement of charges (soc):
    ### to hearing
    soc_hearing_duration = as.numeric(difftime(hearing, statement_of_charges, units="weeks")),
    
    ### to closure if closed w/o hearing:  summary judgement, settlement, voluntary surrender
    soc_close_duration = if_else(is.na(hearing), 
                                 as.numeric(difftime(date_closed, statement_of_charges, units="weeks")), 
                                 NA_real_),
    
    ### if still in state soc
    soc_age = if_else(!is.na(statement_of_charges) & is.na(hearing) & is.na(date_closed), 
                      as.numeric(difftime(Sys.time(), statement_of_charges, units="weeks")), 
                      NA_real_),
    
    
    ## Hearing 
    ### to final_order (fo)
    hearing_fo_duration = as.numeric(difftime(final_order, hearing, units="weeks")),
    
    ### to closure if closed w/o final order:  no cases so far, and may not be possible, but just in case
    hearing_close_duration = if_else(is.na(final_order), 
                                     as.numeric(difftime(date_closed, hearing, units="weeks")), 
                                     NA_real_),
    
    ### if still in state hearing -- none of these so far either
    hearing_age = if_else(!is.na(hearing) & is.na(final_order) & is.na(date_closed), 
                          as.numeric(difftime(Sys.time(), hearing, units="weeks")), 
                          NA_real_),
    
    
    ## Final order 
    ### to close
    fo_close_duration = as.numeric(difftime(date_closed, final_order, units="weeks")),
    
    ### if still in state final_order
    fo_age = if_else(!is.na(final_order) & is.na(date_closed), 
                     as.numeric(difftime(Sys.time(), final_order, units="weeks")), 
                     NA_real_),
    #############################################################################################    
    
    status = factor(status,
                    levels = c("Investigation", "Open - Pending Expiration",
                               "Discipline - Post Hearing", "Assistant Director Review",
                               "Closed")),
    
    status2 = factor(ifelse(status=="Closed", "Closed", "Open"), 
                     levels = c("Open", "Closed")),
    
    outcome = ifelse(status2=="Open", "Still Open", case_determination),
    outcome = fct_relevel(outcome, "Still Open", after = Inf),
    outcat = case_when(
      grepl("Revo|Denied", outcome) ~ "Revoked/Denied",
      grepl("Duplicate", outcome) ~ "Duplicate",
      grepl("Admin", outcome) ~ "Admin Closure",
      #grepl("Dismiss|Reinstated", outcome) ~ "Dismiss/Reinstate",
      grepl("Decline", outcome) ~ "Decline",
      grepl("Open", outcome) ~ "Still Open",
      is.na(outcome) ~ "Missing",
      TRUE ~ "Other"),
    outcat = fct_relevel(outcat,
                         "Admin Closure",
                         "Duplicate",
                         "Decline",
                         #"Dismiss/Reinstate",
                         "Revoked/Denied",
                         "Other",
                         "Still Open",
                         "Missing"),
    
    last_date = case_when(
      !is.na(date_closed) ~ date_closed,
      !is.na(final_order) ~ final_order,
      !is.na(hearing) ~ hearing,
      !is.na(statement_of_charges) ~ statement_of_charges,
      !is.na(investigation) ~ investigation,
      !is.na(date_received) ~ date_received
    ),
    
    last_status_with_date = case_when(
      !is.na(date_closed) ~ "date_closed",
      !is.na(final_order) ~ "final_order",
      !is.na(hearing) ~ "hearing",
      !is.na(statement_of_charges) ~ "statement_of_charges",
      !is.na(investigation) ~ "investigation",
      !is.na(date_received) ~ "date_received"
    ),
    last_status_with_date = factor(last_status_with_date, 
                                   levels=c("date_received", "investigation", "statement_of_charges",
                                            "hearing", "final_order", "date_closed")
    ),
    last_stage_reached = case_when(
      !is.na(hearing) ~ "hearing",
      !is.na(statement_of_charges) ~ "statement of charges",
      !is.na(investigation) | status == "Investigation" ~ "investigation",
      grepl("Admin", case_determination) ~ "admin closure",
      is.na(investigation) & grepl("Surrender", case_determination) ~ "intake revocation-surrender",
      TRUE ~ "irregular pattern"
    ),
    last_stage_reached = factor(last_stage_reached, 
                                levels=c("admin closure", "intake revocation-surrender", 
                                         "investigation", "statement of charges", "hearing",
                                         "irregular pattern")
                                ),
    
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
    
  # Offense tracking
      num_offenses = str_count(conduct_investigated, ";") + 1,
      offense = case_when(
        str_count(conduct_investigated, ";") == 1 ~ "2 offenses",
        str_count(conduct_investigated, ";") == 2 ~ "3 offenses",
        str_count(conduct_investigated, ";") == 3 ~ "4 offenses",
        str_count(conduct_investigated, ";") == 4 ~ "5 offenses",
        str_count(conduct_investigated, ";") == 5 ~ "6 offenses",
        str_count(conduct_investigated, ";") == 6 ~ "7 offenses",
        str_count(conduct_investigated, ";") == 7 ~ "8 offenses",
        str_count(conduct_investigated, ";") == 8 ~ "9 offenses",
        str_count(conduct_investigated, ";") == 9 ~ "10 offenses",
        TRUE ~ conduct_investigated),
      offense = fct_relevel(factor(offense), "10 offenses", after = 8)
    
  )

# Create a list of all offenses that appear in the "conduct_investigated" field and
# assign them the CJTC priority ranking
# case number and updated first/last name will be used to merge back

offense_list <- data.frame(
  case = cjtc_public$case,
  last_name = cjtc_public$last_name,
  first_name = cjtc_public$first_name,
  period_r = cjtc_public$period_r, 
  offense_tot = cjtc_public$num_offenses,
  
  # split the offense vector into separate columns
  str_split(cjtc_public$conduct_investigated, ";", simplify=T)) %>%
  
  filter(!is.na(X1)) %>% # remove records with offense = NA
  pivot_longer(cols=X1:X10, names_to = "offense_num", values_to = "offense") %>%
  filter(offense != "") %>%
  
  # bind_rows(.,
  #           cjtc %>% 
  #             filter(num_offenses==1) %>% 
  #             select(period_r, offense_tot = num_offenses, offense = conduct_investigated)) %>%
  
  mutate(offense = sub(" R", "R", offense),
         offense = sub("ofR", "of R", offense),
         offense = sub("seR", "se R", offense),
         
         offense_rcw = sub(" - .*", "", offense),
         offense_desc = if_else(!grepl("-", offense), NA_character_, sub(".* - ", "", offense))
  ) %>%
  mutate(
    offense_type = factor(
      case_when(
        grepl("RCW 43.101.105\\(2\\)", offense) ~ "Mandatory",
        grepl("RCW 43.101.105\\(3\\)\\(l\\)", offense) ~ "Surrender",    
        grepl("RCW 43.101.105\\(3\\)", offense) ~ "Discretionary",
        TRUE ~ "Unknown"),
      levels = c("Surrender", "Discretionary", "Mandatory")
    ),
    priority_emp = case_when(
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
      grepl("RCW 43.101.105\\(3\\)\\(l\\)" , offense) ~ 0),
    priority_sep = case_when(
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
      grepl("RCW 43.101.105\\(3\\)\\(l\\)" , offense) ~ 0)
  )

# Aggregate offense priority rankings back into case/lname/fname list
offense_by_case <- offense_list %>%
  group_by(case, last_name, first_name) %>%
  summarize(n_offenses = n(), # just a check to verify against num_offenses
            unique_offenses = paste(unique(offense_rcw), collapse = ";"),
            num_mandatory = sum(offense_type=="Mandatory"),
            num_discretionary = sum(offense_type=="Discretionary"),
            surrender = sum(offense_type=="Surrender"),
            num_ranked_emp = sum(!is.na(priority_emp)),
            num_ranked_sep = sum(!is.na(priority_sep)),
            max_priority_emp = max(priority_emp, na.rm=T),
            min_priority_emp = min(priority_emp, na.rm=T),
            max_priority_sep = max(priority_sep, na.rm=T),
            min_priority_sep = min(priority_sep, na.rm=T),
            .groups = "drop",
  ) %>%
  mutate(across(contains("max"), ~na_if(., -Inf))) %>%
  mutate(across(contains("min"), ~na_if(., Inf)))

# Merge offense priority rankings back on to cases
cjtc_public <- left_join(cjtc_public, offense_by_case,
                         by = join_by(case, last_name, first_name))


save(list=c("cjtc_public_raw", "cjtc_public", "offense_list", "offense_by_case"), 
     file="Data/cjtc_public.rda")
