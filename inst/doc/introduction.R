## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(dplyr)
library(magrittr)
library(msSPChelpR)
#Load synthetic dataset of patients with cancer to demonstrate package functions
data("us_second_cancer")

#This dataset is in long format, so each tumor is a separate row in the data
us_second_cancer

## -----------------------------------------------------------------------------
#filter for lung cancer
ids <- us_second_cancer %>%
  #detect ids with any lung cancer
  filter(t_site_icd == "C34") %>%
  select(fake_id) %>%
  as.vector() %>%
  unname() %>%
  unlist()

filtered_usdata <- us_second_cancer %>%
  #filter according to above detected ids with any lung cancer diagnosis
  filter(fake_id %in% ids) %>%
  arrange(fake_id)

filtered_usdata

## -----------------------------------------------------------------------------
renumbered_usdata <- filtered_usdata %>%
  renumber_time_id(new_time_id_var = "t_tumid", 
                   dattype = "seer",
                   case_id_var = "fake_id")

renumbered_usdata %>%
   select(fake_id, sex, t_site_icd, t_datediag, t_tumid)

## -----------------------------------------------------------------------------
usdata_wide <- renumbered_usdata %>%
  reshape_wide_tidyr(case_id_var = "fake_id", time_id_var = "t_tumid", timevar_max = 10)

#now the data is in the wide format as required by many package functions. 
#This means, each case is a row and several tumors per case ID are 
#add new columns to the data using the time_id as column name suffix.
usdata_wide

## -----------------------------------------------------------------------------

usdata_wide <- usdata_wide %>%
  dplyr::mutate(p_spc = dplyr::case_when(is.na(t_site_icd.2)   ~ "No SPC",
                         !is.na(t_site_icd.2)           ~ "SPC developed",
                         TRUE ~ NA_character_)) %>%
  #create the same information as numeric variable count_spc
  dplyr::mutate(count_spc = dplyr::case_when(is.na(t_site_icd.2)   ~ 1,
                            TRUE ~ 0))
usdata_wide %>%
   dplyr::select(fake_id, sex.1, p_spc, count_spc, t_site_icd.1, 
                 t_datediag.1, t_site_icd.2, t_datediag.2)


## -----------------------------------------------------------------------------
usdata_wide <- usdata_wide %>%
  pat_status(., fu_end = "2017-12-31", dattype = "seer",
             status_var = "p_status", life_var = "p_alive.1",
             spc_var = "p_spc", birthdat_var = "datebirth.1",
             lifedat_var = "datedeath.1", fcdat_var = "t_datediag.1",
             spcdat_var = "t_datediag.2", life_stat_alive = "Alive",
             life_stat_dead = "Dead", spc_stat_yes = "SPC developed",
             spc_stat_no = "No SPC", lifedat_fu_end = "2019-12-31",
             use_lifedatmin = FALSE, check = TRUE, 
             as_labelled_factor = TRUE)

usdata_wide %>%
   dplyr::select(fake_id, p_status, p_alive.1, datedeath.1, t_site_icd.1, t_datediag.1, 
                 t_site_icd.2, t_datediag.2)

#alternatively, you can impute the date of death using lifedatmin_var
usdata_wide %>%
  pat_status(., fu_end = "2017-12-31", dattype = "seer",
             status_var = "p_status", life_var = "p_alive.1",
             spc_var = "p_spc", birthdat_var = "datebirth.1",
             lifedat_var = "datedeath.1", fcdat_var = "t_datediag.1",
             spcdat_var = "t_datediag.2", life_stat_alive = "Alive",
             life_stat_dead = "Dead", spc_stat_yes = "SPC developed",
             spc_stat_no = "No SPC", lifedat_fu_end = "2019-12-31",
             use_lifedatmin = TRUE, lifedatmin_var = "p_dodmin.1", 
             check = TRUE, as_labelled_factor = TRUE)



## -----------------------------------------------------------------------------
usdata_wide <- usdata_wide %>%
  dplyr::filter(!p_status %in% c("NA - Patient not born before end of FU",
                                 "NA - Patient did not develop cancer before end of FU",
                                 "NA - Patient date of death is missing"))

usdata_wide %>%
  dplyr::count(p_status)


## -----------------------------------------------------------------------------
usdata_wide <- usdata_wide %>%
   calc_futime(., futime_var_new = "p_futimeyrs", fu_end = "2017-12-31",
               dattype = "seer", time_unit = "years", 
               lifedat_var = "datedeath.1", 
               fcdat_var = "t_datediag.1", spcdat_var = "t_datediag.2")

usdata_wide %>%
   dplyr::select(fake_id, p_status, p_futimeyrs, p_alive.1, datedeath.1, t_datediag.1, t_datediag.2)


## -----------------------------------------------------------------------------
sircalc_results <- usdata_wide %>%
  sir_byfutime(
    dattype = "seer",
    ybreak_vars = c("race.1", "t_dco.1"),
    xbreak_var = "none",
    futime_breaks = c(0, 1/12, 2/12, 1, 5, 10, Inf),
    count_var = "count_spc",
    refrates_df = us_refrates_icd2,
    calc_total_row = TRUE,
    calc_total_fu = TRUE,
    region_var = "registry.1",
    age_var = "fc_agegroup.1",
    sex_var = "sex.1",
    year_var = "t_yeardiag.1",
    race_var = "race.1",
    site_var = "t_site_icd.1", #using grouping by second cancer incidence
    futime_var = "p_futimeyrs",
    alpha = 0.05)

sircalc_results %>% print(n = 100)


## -----------------------------------------------------------------------------
#The summarize function is versatile. Here for example the summary with minimal output

sircalc_results %>%
  #summarize results across region, age, year and t_site
  summarize_sir_results(.,
                        summarize_groups = c("region", "age", "year", "race"),
                        summarize_site = TRUE,
                        output = "long",  output_information = "minimal",
                        add_total_row = "only",  add_total_fu = "no",
                        collapse_ci = FALSE,  shorten_total_cols = TRUE,
                        fubreak_var_name = "fu_time", ybreak_var_name = "yvar_name",
                        xbreak_var_name = "none", site_var_name = "t_site",
                        alpha = 0.05
                        ) %>%
  dplyr::select(-region, -age, -year, -race, -sex, -yvar_name)

## -----------------------------------------------------------------------------
sessionInfo()

