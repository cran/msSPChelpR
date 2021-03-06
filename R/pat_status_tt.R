
#' Calculate patient status at specific end of follow-up - tidytable version
#'
#' @param wide_df dataframe or data.table in wide format
#' @param fu_end end of follow-up in time format YYYY-MM-DD.
#' @param dattype Type of cancer registry data. Can be "seer" or "zfkd". Default is "zfkd".
#' @param life_var Name of variable containing life status. Will override dattype preset.  
#' @param spc_var Name of variable containing SPC status. Will override dattype preset.   
#' @param status_var Name of the newly calculated variable for patient status. Default is p_status.    
#' @param birthdat_var Name of variable containing Date of Birth. Will override dattype preset.    
#' @param lifedat_var Name of variable containing Date of Death. Will override dattype preset.
#' @param lifedatmin_var Name of variable containing the minimum Date of Death when true DoD is missing. Will override dattype preset. Will only  be used if use_lifedatmin = TRUE.     
#' @param fcdat_var Name of variable containing Date of Primary Cancer diagnosis. Will override dattype preset.     
#' @param spcdat_var Name of variable containing Date of SPC diagnosis Will override dattype preset.
#' @param life_stat_alive Value for alive status in life_var. Will override dattype preset.
#' @param life_stat_dead Value for dead status in life_var. Will override dattype preset.
#' @param spc_stat_yes Value for SPC occurred in spc_var. Will override dattype preset.
#' @param spc_stat_no Value for no SPC in spc_var. Will override dattype preset.
#' @param lifedat_fu_end Date of last FU of alive status in registry data. Will override dattype preset (2017-03-31 for zfkd; 2018-12-31 for seer).
#' @param use_lifedatmin If TRUE, option to use Date of Death from lifedatmin_var when DOD is missing. Default is FALSE.
#' @param check Check newly calculated variable p_status. Default is TRUE.    
#' @param as_labelled_factor If TRUE, output status_var as labelled factor variable. Default is FALSE.
#' @return wide_df
#' @export
#' @examples 
#' #load sample data
#' data("us_second_cancer")
#' 
#' #prep step - make wide data as this is the required format
#' usdata_wide <- us_second_cancer %>%
#'                     msSPChelpR::reshape_wide_tidyr(case_id_var = "fake_id", 
#'                     time_id_var = "SEQ_NUM", timevar_max = 10)
#'                     
#' #prep step - calculate p_spc variable
#' usdata_wide <- usdata_wide %>%
#'                  dplyr::mutate(p_spc = dplyr::case_when(is.na(t_site_icd.2)   ~ "No SPC",
#'                                                        !is.na(t_site_icd.2)   ~ "SPC developed",
#'                                                        TRUE ~ NA_character_)) %>%
#'                  dplyr::mutate(count_spc = dplyr::case_when(is.na(t_site_icd.2)   ~ 1,
#'                                                               TRUE ~ 0))
#'                                                               
#' #now we can run the function
#' msSPChelpR::pat_status_tt(usdata_wide, 
#'                        fu_end = "2017-12-31", 
#'                        dattype = "seer", 
#'                        status_var = "p_status", 
#'                        life_var = "p_alive.1", 
#'                        spc_var = NULL, 
#'                        birthdat_var = "datebirth.1", 
#'                        lifedat_var = "datedeath.1",
#'                        use_lifedatmin = FALSE, 
#'                        check = TRUE, 
#'                        as_labelled_factor = FALSE)
#'                        


pat_status_tt <- function(wide_df, fu_end = NULL, dattype = "zfkd", 
                          status_var = "p_status", life_var = NULL, spc_var = NULL, birthdat_var = NULL, lifedat_var = NULL, lifedatmin_var = NULL,
                          fcdat_var = NULL, spcdat_var = NULL, 
                          life_stat_alive = NULL, life_stat_dead = NULL, spc_stat_yes = NULL, spc_stat_no = NULL, lifedat_fu_end = NULL,
                          use_lifedatmin = FALSE, check = TRUE, as_labelled_factor = FALSE){
  
  status_var <- rlang::ensym(status_var)
  
  #setting default var names and values for SEER data
  
  if (dattype == "seer"){
    if(is.null(life_var)){
      life_var <- rlang::sym("STAT_REC.1")
    } else{
      life_var <- rlang::ensym(life_var)
    }
    if(is.null(spc_var)){
      spc_var <- rlang::sym("p_spc")
    } else{
      spc_var <- rlang::ensym(spc_var)
    }
    if(is.null(birthdat_var)){
      birthdat_var <- rlang::sym("p_datebirth")
    } else{
      birthdat_var <- rlang::ensym(birthdat_var)
    }
    if(is.null(lifedat_var)){
      lifedat_var <- rlang::sym("p_datedeath")
    } else{
      lifedat_var <- rlang::ensym(lifedat_var)
    }
    if(is.null(lifedatmin_var) & use_lifedatmin == TRUE){
      lifedatmin_var <- rlang::sym("p_dodmin")
    } else if(use_lifedatmin == TRUE){
      lifedatmin_var <- rlang::ensym(lifedatmin_var)
    }
    if(is.null(fcdat_var)){
      fcdat_var <- rlang::sym("t_datediag.1")
    } else{
      fcdat_var <- rlang::ensym(fcdat_var)
    }
    if(is.null(spcdat_var)){
      spcdat_var <- rlang::sym("t_datediag.2")
    } else{
      spcdat_var <- rlang::ensym(spcdat_var)
    }
    if(is.null(life_stat_alive)){
      life_stat_alive <- rlang::quo("Alive")
    } else{
      life_stat_alive <- rlang::enquo(life_stat_alive)
    }
    if(is.null(life_stat_dead)){
      life_stat_dead <- rlang::quo("Dead")
    } else{
      life_stat_dead <- rlang::enquo(life_stat_dead)
    }
    if(is.null(spc_stat_yes)){
      spc_stat_yes <- rlang::quo("SPC developed")
    } else{
      spc_stat_yes <- rlang::enquo(spc_stat_yes)
    }
    if(is.null(spc_stat_no)){
      spc_stat_no <- rlang::quo("No SPC")
    } else{
      spc_stat_no <- rlang::enquo(spc_stat_no)
    }
    if(is.null(lifedat_fu_end)){
      lifedat_fu_end <- rlang::quo("2016-12-31")
    } else{
      lifedat_fu_end <- rlang::enquo(lifedat_fu_end)
    }
  }
  
  #setting default var names and values for ZfKD data
  if (dattype == "zfkd"){
    if(is.null(life_var)){
      life_var <- rlang::sym("TOD.1")
    } else{
      life_var <- rlang::ensym(life_var)
    }
    if(is.null(spc_var)){
      spc_var <- rlang::sym("p_spc")
    } else{
      spc_var <- rlang::ensym(spc_var)
    }
    if(is.null(birthdat_var)){
      birthdat_var <- rlang::sym("GDIMP.1")
    } else{
      birthdat_var <- rlang::ensym(birthdat_var)
    }
    if(is.null(lifedat_var)){
      lifedat_var <- rlang::sym("SDIMP.1")
    } else{
      lifedat_var <- rlang::ensym(lifedat_var)
    }
    if(is.null(lifedatmin_var) & use_lifedatmin == TRUE){
      lifedatmin_var <- rlang::sym("p_dodmin")
    } else if(use_lifedatmin == TRUE){
      lifedatmin_var <- rlang::ensym(lifedatmin_var)
    }
    if(is.null(fcdat_var)){
      fcdat_var <- rlang::sym("DDIMP.1")
    } else{
      fcdat_var <- rlang::ensym(fcdat_var)
    }
    if(is.null(spcdat_var)){
      spcdat_var <- rlang::sym("DDIMP.2")
    } else{
      spcdat_var <- rlang::ensym(spcdat_var)
    }
    if(is.null(life_stat_alive)){
      life_stat_alive <- rlang::quo("no (patient alive)")
    } else{
      life_stat_alive <- rlang::enquo(life_stat_alive)
    }
    if(is.null(life_stat_dead)){
      life_stat_dead <- rlang::quo("yes (patient deceased)")
    } else{
      life_stat_dead <- rlang::enquo(life_stat_dead)
    }
    if(is.null(spc_stat_yes)){
      spc_stat_yes <- rlang::quo("SPC developed")
    } else{
      spc_stat_yes <- rlang::enquo(spc_stat_yes)
    }
    if(is.null(spc_stat_no)){
      spc_stat_no <- rlang::quo("No SPC")
    } else{
      spc_stat_no <- rlang::enquo(spc_stat_no)
    }
    if(is.null(lifedat_fu_end)){
      lifedat_fu_end <- rlang::quo("2017-03-31")
    } else{
      lifedat_fu_end <- rlang::enquo(lifedat_fu_end)
    }
  }
  
  #---- Checks start
  
  #check whether all required variables are defined and present in dataset
  defined_vars <- c(rlang::as_name(life_var), rlang::as_name(spc_var), rlang::as_name(lifedat_var), 
                    if(use_lifedatmin){rlang::as_name(lifedatmin_var)})
  
  not_found <- defined_vars[!(defined_vars %in% colnames(wide_df))]
  
  if(length(not_found) > 0) {
    rlang::abort(paste0("The following variables defined are not found in the provided dataframe: ", paste(not_found, collapse=", ")))
  }
  
  #check whether date was provided in correct format
  fu_end <- rlang::enquo(fu_end)
  if(!lubridate::is.Date(as.Date(rlang::eval_tidy(fu_end), date.format = "%y-%m-%d"))) {
    rlang::abort("You have not provided a correct Follow-up date in the format YYYY-MM-DD")
  }
  
  #make label for new variable
  statvar_label <- paste("Patient Status at end of follow-up", rlang::as_name(fu_end))
  
  #check whether spc_var is coherent with date (to catch cases where old p_spc is used and data is filtered afterwards)
  
  n_errorspc <- wide_df %>% 
    tidytable::filter.(
      #scenario 1 -> p_spc indicates SPC developed, but spcdat_var is empty
      (!!spc_var == !!spc_stat_yes & is.na(rlang::eval_tidy(!!spcdat_var))) |
        #secnario 2 -> p_spc indicates no SPC, but spcdat_var is filled
        (!!spc_var == !!spc_stat_no & !is.na(rlang::eval_tidy(!!spcdat_var)))
    ) %>%
    nrow()
  
  
  if(n_errorspc > 0){
    rlang::abort("`spc_var` and `spcdat_var` are inconsistent. Are you sure you have calculated `spc_var` after filtering and reshaping the dataset?")
  }
  
  #check if new and old status_var are the same --> message that id was overwritten
  
  if(rlang::as_name(status_var) %in% names(wide_df)){
    rlang::warn(paste0(rlang::as_name(status_var)," is already present in dataset. Variable has been overwritten with new values."))
  }
  
  
  #---- Calculate
  
  #enforce option use_lifedatmin == TRUE
  
  if(use_lifedatmin == TRUE){
    wide_df <- wide_df %>%
      #option use_lifedatmin option is used and date of death is missing -> replace by lifedatamin
      tidytable::mutate.(
        #copy old lifedat_var
        p_datedeath_orig = !!lifedat_var,
        !!lifedat_var := tidytable::case.(
          is.na(rlang::eval_tidy(!!lifedat_var)), !!lifedatmin_var,
          default = !!lifedat_var)
      ) 
  }
  
  #calculate new status_var variable and label it
  #todo: implement check on date of spc_diagnosis and date of birth and introduce new status.
  wide_df <- wide_df %>%
    tidytable::mutate.(!!status_var := tidytable::case.(
      #patient is not born before end of follow-up
      !!birthdat_var > !!fu_end, 97,
      #patient has not developed FC before end of follow-up
      !!fcdat_var > !!fu_end, 98,
      #patient date of death is missing
      !!life_var == !!life_stat_dead & is.na(rlang::eval_tidy(!!lifedat_var)) & !!lifedat_fu_end > !!fu_end, 99,
      #patient is alive after FC and before end of FU (independet of whether SPC has developed or not after FU)
      !!spc_var == !!spc_stat_no & !!life_var == !!life_stat_alive, 1,
      !!spc_var == !!spc_stat_no & !!life_var == !!life_stat_dead & !!lifedat_var > !!fu_end, 1,
      !!spc_var == !!spc_stat_yes & !!spcdat_var > !!fu_end & !!life_var == !!life_stat_alive, 1,
      !!spc_var == !!spc_stat_yes & !!spcdat_var > !!fu_end & !!life_var == !!life_stat_dead & !!lifedat_var > !!fu_end, 1,
      #patient is alive after SPC and before end of FU
      !!spc_var == !!spc_stat_yes & !!spcdat_var <= !!fu_end & !!life_var == !!life_stat_alive, 2,
      !!spc_var == !!spc_stat_yes & !!spcdat_var <= !!fu_end & !!life_var == !!life_stat_dead & !!lifedat_var > !!fu_end, 2,
      #patient is dead after FC and before end of FU
      !!spc_var == !!spc_stat_no & !!life_var == !!life_stat_dead & !!lifedat_var <= !!fu_end, 3,
      !!spc_var == !!spc_stat_no & !!life_var == !!life_stat_dead & is.na(rlang::eval_tidy(!!lifedat_var)) & !!lifedat_fu_end <= !!fu_end, 3,
      #patient is dead after SPC and before end of FU
      !!spc_var == !!spc_stat_yes & !!spcdat_var <= !!fu_end & !!life_var == !!life_stat_dead & !!lifedat_var <= !!fu_end, 4,
      !!spc_var == !!spc_stat_yes & !!spcdat_var <= !!fu_end & !!life_var == !!life_stat_dead & is.na(rlang::eval_tidy(!!lifedat_var)) & !!lifedat_fu_end <= !!fu_end, 4,
      default = NA_real_)) 
  
  #enforce option use_lifedatmin == TRUE - part 2
  
  if(use_lifedatmin == TRUE){
    wide_df <- wide_df %>%
      tidytable::mutate.(
        #replace temporary lifedat_var values with values from old lifedat_var
        !!lifedat_var := p_datedeath_orig
      ) %>%
      #remove p_datedeath_orig
      tidytable::select.(-tidyselect::all_of("p_datedeath_orig"))
  }
  
  wide_df <- wide_df%>%
    #label new variable
    sjlabelled::var_labels(!!status_var := !!statvar_label) %>%
    sjlabelled::val_labels(!!status_var := c("patient alive after FC (with or without following SPC after end of FU)" = 1,
                                             "patient alive after SPC" = 2,
                                             "patient dead after FC" = 3,
                                             "patient dead after SPC" = 4,
                                             "NA - patient not born before end of FU" = 97,
                                             "NA - patient did not develop cancer before end of FU" = 98,
                                             "NA - patient date of death is missing" = 99),
                           force.labels = TRUE)
  
  #enforce option as_labelled_factor = TRUE
  if(as_labelled_factor == TRUE){
    wide_df <- wide_df %>%
      tidytable::mutate.(!!status_var := sjlabelled::as_label(!!status_var, keep.labels=TRUE))
  }
  
  #----Checks end
  
  #conduct check on new variable
  if(check == TRUE){
    check_tab <- wide_df %>%
      tidytable::count.(!!life_var, !!status_var)
    
    print(check_tab)
    
    freq_tab <- wide_df %>%
      tidytable::count.(!!status_var)
    
    print(freq_tab)
    
  }
  
  #---- Return results
  
  return(wide_df)
  
}

