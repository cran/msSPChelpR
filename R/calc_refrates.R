#' 
#' Calculate age-, sex-, cohort-, region-specific incidence rates from a cohort
#'
#' @param df dataframe in long format
#' @param dattype can be "zfkd" or "seer" or NULL. Will set default variable names if dattype is "seer" or "zfkd". Default is NULL.
#' @param count_var variable to be counted as observed case. Should be 1 for case to be counted.
#' @param calc_totals option to calculate totals for all age-groups, all sexes, all years, all races, all sites. Default is FALSE.
#' @param fill_sites option to fill missing sites in observed with incidence rate of 0. Needs to define the coding system used. 
#'                   Can be either "no" for not filling missing sites. "icd2d" for ICD-O-3 2 digit (C00-C80), "icd3d" for ICD-O-3 3digit, 
#'                   "icd10gm2d" for ICD-10-GM 2-digit (C00-C97), "sitewho" for Site SEER WHO coding (no 1-89 categories), "sitewho_b"  for Site SEER WHO B recoding (no. 1-111 categories),
#'                   "sitewho_epi" for SITE SEER WHO coding with additional sums,  "sitewhogen" for SITE WHO coding with less categories to make compatible for international rates, 
#'                   "sitewho_num" for numeric coding of Site SEER WHO coding (no 1-89 categories), "sitewho_b_num"  for numeric coding of Site SEER WHO B recoding (no. 1-111 categories),
#'                   "sitewhogen_num" for numeric international rates, c("manual", char_vector) of sites manually defined
#' @param region_var variable in df that contains information on region where case was incident. Default is set if dattype is given.
#' @param age_var variable in df that contains information on age-group. Default is set if dattype is given.
#' @param sex_var variable in df that contains information on sex. Default is set if dattype is given.
#' @param year_var variable in df that contains information on year or year-period when case was incident. Default is set if dattype is given.
#' @param race_var optional argument, if rates should be calculated stratified by race. If you want to use this option, provide variable name of df that contains race information. If race_var is provided refpop_df needs to contain the variable "race".
#' @param site_var variable in df that contains information on ICD code of case diagnosis. Cases are usually the second cancers. Default is set if dattype is given.
#' @param refpop_df df where reference population data is defined. Only required if option futime = "refpop" is chosen. It is assumed that refpop_df has the columns 
#'                  "region" for region, "sex" for biological sex, "age" for age-groups (can be single ages or 5-year brackets), "year" for time period (can be single year or 5-year brackets), 
#'                  "population_pyar" for person-years at risk in the respective age/sex/year cohort.
#'                  refpop_df must use the same category coding of age, sex, region, year and site as age_var, sex_var, region_var, year_var and site_var.
#' @param quiet If TRUE, warnings and messages will be suppressed. Default is FALSE.
#' @return df
#' @importFrom rlang .data
#' @export 
#' @examples 
#' #load sample data
#' data("us_second_cancer")
#' data("population_us")
#' 
#' us_second_cancer %>%
#'   #create variable to indicate to be counted as case
#'   dplyr::mutate(is_case = 1) %>%
#'   #calculate refrates - warning: these are not realistic numbers, just showing functionality
#'   calc_refrates(dattype = "seer", , count_var = "is_case", refpop_df = population_us,
#'                region_var = "registry", age_var = "fc_agegroup", sex_var = "sex", 
#'                site_var = "t_site_icd")


calc_refrates <- function(df,                         
                          dattype = NULL,
                          count_var,
                          refpop_df,
                          calc_totals = FALSE,
                          fill_sites = "no",
                          region_var = NULL,
                          age_var = NULL,
                          sex_var = NULL,
                          year_var = NULL,
                          race_var = NULL,    #optional when matching by race is wanted
                          site_var = NULL,
                          quiet = FALSE) {
  
  # ---- 0 function basics ----
  
  ## --- 0a setting default parameters
  na_explicit <- "zzz_NA_explicit" # string for explicit NAs
  
  if(!(fill_sites[1] %in% c("no", "icd2d", "icd3d", "icd10gm2d", "sitewho", "sitewho_b",
                            "sitewho_num", "sitewho_b_num", "sitewho_epi", "sitewhogen",
                            "sitewhogen_num", "manual"))){
    if(!quiet){
      rlang::warn(c(
        "Parameter `fill_sites` must be \"icd2d\", \"icd3d\", \"icd10gm2d\", \"sitewho\", \"sitewho_b\", 
      \"sitewho_num\", \"sitewho_b_num\", \"sitewho_epi\", \"sitewhogen\", \"sitewhogen_num\", \"manual\" or \"no\".", 
        paste0("Default `fill_sites = \"no\"` will be used instead of: ", fill_sites),
        ""
      ))
    }
    fill_sites <- "no"
  }
  
  if((fill_sites[1]  %in% c("icd2d", "icd3d", "icd10gm2d", "sitewho", "sitewho_b",
                            "sitewho_num", "sitewho_b_num", "sitewho_epi", "sitewhogen",
                            "sitewhogen_num", "manual"))){
    fill <- TRUE
    if(fill_sites[1] == "icd2d"){
      sites_all <- c("C00", "C01", "C02", "C03", "C04", "C05", "C06", "C07", "C08", "C09", 
                     "C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19", 
                     "C20", "C21", "C22", "C23", "C24", "C25", "C26", 
                     "C30", "C31", "C32", "C33", "C34",               "C37", "C38", "C39", 
                     "C40", "C41", "C42",        "C44",               "C47", "C48", "C49", 
                     "C50", "C51", "C52", "C53", "C54", "C55", "C56", "C57", "C58", 
                     "C60", "C61", "C62", "C63", "C64", "C65", "C66", "C67", "C68", "C69", 
                     "C70", "C71", "C72", "C73", "C74", "C75", "C76", "C77", 
                     "C80")
    }
    if(fill_sites[1] == "icd3d"){
      sites_all <- c("C000", "C001", "C002", "C003", "C004", "C005", "C006", "C008", "C009",
                     "C019", "C020", "C021", "C022", "C023", "C024", "C028", "C029", "C030", "C031", "C039", 
                     "C040", "C041", "C048", "C049", "C050", "C051", "C052", "C058", "C059", "C060", "C061", 
                     "C062", "C068", "C069", "C079", "C080", "C081", "C088", "C089", "C090", "C091", "C098", 
                     "C099", "C100", "C101", "C102", "C103", "C104", "C108", "C109", "C110", "C111", "C112", 
                     "C113", "C118", "C119", "C129", "C130", "C131", "C132", "C138", "C139", "C140", "C142", 
                     "C148", "C150", "C151", "C152", "C153", "C154", "C155", "C158", "C159", "C160", "C161", 
                     "C162", "C163", "C164", "C165", "C166", "C168", "C169", "C170", "C171", "C172", "C173", 
                     "C178", "C179", "C180", "C181", "C182", "C183", "C184", "C185", "C186", "C187", "C188", 
                     "C189", "C199", "C209", "C210", "C211", "C212", "C218", "C220", "C221", "C239", "C240", 
                     "C241", "C248", "C249", "C250", "C251", "C252", "C253", "C254", "C257", "C258", "C259", 
                     "C260", "C268", "C269", "C300", "C301", "C310", "C311", "C312", "C313", "C318", "C319",
                     "C320", "C321", "C322", "C323", "C328", "C329", "C339", "C340", "C341", "C342", "C343",
                     "C348", "C349", "C379", "C380", "C381", "C382", "C383", "C384", "C388", "C390", "C398",
                     "C399", "C400", "C401", "C402", "C403", "C408", "C409", "C410", "C411", "C412", "C413",
                     "C414", "C418", "C419", "C420", "C421", "C422", "C423", "C424", "C440", "C441", "C442", 
                     "C443", "C444", "C445", "C446", "C447", "C448", "C449", "C470", "C471", "C472", "C473", 
                     "C474", "C475", "C476", "C478", "C479", "C480", "C481", "C482", "C488", "C490", "C491", 
                     "C492", "C493", "C494", "C495", "C496", "C498", "C499", "C500", "C501", "C502", "C503", 
                     "C504", "C505", "C506", "C508", "C509", "C510", "C511", "C512", "C518", "C519", "C529", 
                     "C530", "C531", "C538", "C539", "C540", "C541", "C542", "C543", "C548", "C549", "C559",
                     "C569", "C570", "C571", "C572", "C573", "C574", "C577", "C578", "C579", "C589", "C600",
                     "C601", "C602", "C608", "C609", "C619", "C620", "C621", "C629", "C630", "C631", "C632",
                     "C637", "C638", "C639", "C649", "C659", "C669", "C670", "C671", "C672", "C673", "C674",
                     "C675", "C676", "C677", "C678", "C679", "C680", "C681", "C688", "C689", "C690", "C691",
                     "C692", "C693", "C694", "C695", "C696", "C698", "C699", "C700", "C701", "C709", "C710", 
                     "C711", "C712", "C713", "C714", "C715", "C716", "C717", "C718", "C719", "C720", "C721", 
                     "C722", "C723", "C724", "C725", "C728", "C729", "C739", "C740", "C741", "C749", "C750", 
                     "C751", "C752", "C753", "C754", "C755", "C758", "C759", "C760", "C761", "C762", "C763", 
                     "C764", "C765", "C767", "C768", "C770", "C771", "C772", "C773", "C774", "C775", "C778", 
                     "C779", "C809")
    }
    if(fill_sites[1] == "icd10gm2d"){
      sites_all <- c("C00", "C01", "C02", "C03", "C04", "C05", "C06", "C07", "C08", "C09", 
                     "C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19", 
                     "C20", "C21", "C22", "C23", "C24", "C25", "C26", 
                     "C30", "C31", "C32", "C33", "C34",               "C37", "C38", "C39", 
                     "C40", "C41",        "C43", "C44", "C45", "C46", "C47", "C48", "C49", 
                     "C50", "C51", "C52", "C53", "C54", "C55", "C56", "C57", "C58", 
                     "C60", "C61", "C62", "C63", "C64", "C65", "C66", "C67", "C68", "C69", 
                     "C70", "C71", "C72", "C73", "C74", "C75", "C76",  
                     "C80", "C81", "C82", "C83", "C84", "C85", "C86",        "C88", 
                     "C90", "C91", "C92", "C93", "C94", "C95", "C96")
    }
    if(fill_sites[1] == "sitewho"){
      sites_all <- c("Lip", "Tongue", "Salivary Gland", "Floor of Mouth", "Gum and Other Mouth", "Nasopharynx", 
                     "Tonsil", "Oropharynx", "Hypopharynx", "Other Oral Cavity and Pharynx", "Esophagus", "Stomach", 
                     "Small Intestine", "Cecum", "Appendix", "Ascending Colon", "Hepatic Flexure", "Transverse Colon",
                     "Splenic Flexure", "Descending Colon", "Sigmoid Colon", "Large Intestine, NOS", 
                     "Rectosigmoid Junction", "Rectum", "Anus, Anal Canal and Anorectum", "Liver", 
                     "Intrahepatic Bile Duct", "Gallbladder", "Other Biliary", "Pancreas", "Retroperitoneum", 
                     "Peritoneum, Omentum and Mesentery", "Other Digestive Organs", "Nose, Nasal Cavity and Middle Ear", 
                     "Larynx", "Lung and Bronchus", "Pleura", "Trachea, Mediastinum and Other Respiratory Organs", 
                     "Bones and Joints", "Soft Tissue including Heart", "Melanoma of the Skin", 
                     "Other Non-Epithelial Skin", "Breast", "Cervix Uteri", "Corpus Uteri", "Uterus, NOS", 
                     "Ovary", "Vagina", "Vulva", "Other Female Genital Organs", "Prostate", "Testis", "Penis", 
                     "Other Male Genital Organs", "Urinary Bladder", "Kidney and Renal Pelvis", "Ureter", 
                     "Other Urinary Organs", "Eye and Orbit", "Brain", "Cranial Nerves Other Nervous System", 
                     "Thyroid", "Other Endocrine including Thymus", "Hodgkin - Nodal", "Hodgkin - Extranodal", 
                     "NHL - Nodal", "NHL - Extranodal", "Myeloma", "Acute Lymphocytic Leukemia", 
                     "Chronic Lymphocytic Leukemia", "Other Lymphocytic Leukemia", "Acute Myeloid Leukemia", 
                     "Chronic Myeloid Leukemia", "Acute Monocytic Leukemia", "Other Acute Leukemia", 
                     "Aleukemic, Subleukemic and NOS", "Miscellaneous", "Mesothelioma", "Kaposi Sarcoma", 
                     "Other Myeloid/Monocytic Leukemia")
    }
    if(fill_sites[1] == "sitewho_num"){
      sites_all <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 25, 26, 27, 
                     29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 
                     52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 68, 69, 71, 72, 73, 74, 75, 76, 
                     77, 78, 80, 83, 85, 86, 87, 88, 89)
    }
    if(fill_sites[1] == "sitewho_b"){
      sites_all <- c("Lip", "Tongue", "Salivary Gland", "Floor of Mouth", "Gum and Other Mouth", 
                     "Nasopharynx", "Tonsil", "Oropharynx", "Hypopharynx", "Other Oral Cavity and Pharynx", 
                     "Esophagus", "Stomach", "Small Intestine", "Cecum", "Appendix", "Ascending Colon", 
                     "Hepatic Flexure", "Transverse Colon", "Splenic Flexure", "Descending Colon", 
                     "Sigmoid Colon", "Large Intestine, NOS", "Rectosigmoid Junction", "Rectum", "Anus", 
                     "Anal Canal and Anorectum", "Liver", "Intrahepatic Bile Duct", "Gallbladder", 
                     "Other Biliary", "Pancreas", "Retroperitoneum", "Peritoneum, Omentum and Mesentery", 
                     "Other Digestive Organs", "Nose, Nasal Cavity and Middle Ear", "Larynx", 
                     "Lung and Bronchus", "Pleura", "Trachea", "Bones and Joints", 
                     "Soft Tissue including Heart", "Melanoma of the Skin", "Other Non-Epithelial Skin", 
                     "Cervix Uteri", "Corpus Uteri", "Uterus, NOS", "Ovary", "Vagina", "Vulva", 
                     "Other Female Genital Organs", "Prostate", "Testis", "Penis", "Other Male Genital Organs", 
                     "Urinary Bladder",           "Ureter", "Other Urinary Organs",           "Brain", 
                     "Cranial Nerves Other Nervous System", "Thyroid",          "Hodgkin - Nodal", 
                     "Hodgkin - Extranodal", "NHL - Nodal", "NHL - Extranodal", "Myeloma", 
                     "Acute Lymphocytic Leukemia", "Chronic Lymphocytic Leukemia", "Other Lymphocytic Leukemia", 
                     "Acute Myeloid Leukemia", "Chronic Myeloid Leukemia", "Acute Monocytic Leukemia", 
                     "Other Acute Leukemia", "Aleukemic, Subleukemic and NOS", "Miscellaneous", "Mesothelioma", 
                     "Kaposi Sarcoma", "Other Myeloid/Monocytic Leukemia", 
                     "Mediastinum and Other Respiratory Organs", "Female Breast", "Male Breast", "Kidney", 
                     "Renal Pelvis", "Eye and Orbit - Non-Melanoma", "Eye and Orbit - Melanoma", "Other Endocrine",
                     "Thymus", "Adrenal Gland")
    }
    if(fill_sites[1] == "sitewho_b_num"){
      sites_all <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 25, 26, 27, 29, 
                     30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 101, 42, 43, 44, 45, 47, 48, 49, 50, 51, 52, 53,
                     54, 55, 56, 57, 58, 60, 61, 63, 64, 65, 68, 69, 71, 72, 73, 74, 75, 76, 77, 78, 80, 83, 85, 
                     86, 87, 88, 89, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111)
    }
    if(fill_sites[1] == "sitewho_epi"){
      sites_all <- c("Acute Lymphocytic Leukemia" , "Acute Monocytic Leukemia" , "Acute Myeloid Leukemia" , 
                     "Aleukemic, Subleukemic and NOS" , "Anus, Anal Canal and Anorectum" , "Appendix" , 
                     "Ascending Colon", "Bones and Joints" , "Brain", "Brain and Other Nervous System" , 
                     "Breast" , "Cecum", "Cervix Uteri" , "Chronic Lymphocytic Leukemia" , 
                     "Chronic Myeloid Leukemia" , "Colon and Rectum" , "Colon excluding Rectum" , 
                     "Corpus and Uterus, NOS" , "Corpus Uteri" , "Cranial Nerves Other Nervous System", 
                     "Descending Colon" , "Digestive System" , "Endocrine System" , "Esophagus", 
                     "Eye and Orbit", "Female Genital System", "Floor of Mouth" , "Gallbladder", 
                     "Gum and Other Mouth", "Hepatic Flexure", "Hodgkin - Extranodal" , "Hodgkin - Nodal", 
                     "Hodgkin Lymphoma" , "Hypopharynx", "Intrahepatic Bile Duct" , "Kaposi Sarcoma" , 
                     "Kidney and Renal Pelvis", "Large Intestine, NOS" , "Larynx" , "Leukemia" , "Lip", 
                     "Liver", "Liver and Intrahepatic Bile Duct" , "Lung and Bronchus",
                     "Lymphocytic Leukemia" , "Lymphoma" , "Male Genital System", "Melanoma of the Skin" ,
                     "Mesothelioma" , "Miscellaneous", "Myeloid and Monocytic Leukemia" , "Myeloma", 
                     "Nasopharynx", "NHL - Extranodal" , "NHL - Nodal", "Non-Hodgkin Lymphoma" ,
                     "Nose, Nasal Cavity and Middle Ear", "Oral Cavity and Pharynx", "Oropharynx" ,
                     "Other Acute Leukemia" , "Other Biliary", "Other Digestive Organs" , 
                     "Other Endocrine including Thymus" , "Other Female Genital Organs", "Other Leukemia" ,
                     "Other Lymphocytic Leukemia" , "Other Male Genital Organs", 
                     "Other Myeloid/Monocytic Leukemia" , "Other Non-Epithelial Skin", 
                     "Other Oral Cavity and Pharynx", "Other Urinary Organs" , "Ovary", "Pancreas" ,
                     "Penis", "Peritoneum, Omentum and Mesentery", "Pleura" , "Prostate" , 
                     "Rectosigmoid Junction", "Rectum" , "Rectum and Rectosigmoid Junction" , 
                     "Respiratory System" , "Retroperitoneum", "Salivary Gland" , "Sigmoid Colon", 
                     "Skin excluding Basal and Squamous", "Small Intestine", "Soft Tissue including Heart", 
                     "Splenic Flexure", "Stomach", "Testis" , "Thyroid", "Tongue" , "Tonsil" , 
                     "Trachea, Mediastinum and Other Respiratory Organs", "Transverse Colon" , "Ureter" , 
                     "Urinary Bladder", "Urinary System" , "Uterus, NOS", "Vagina" , "Vulva")
    }
    if(fill_sites[1] == "sitewhogen"){
      sites_all <- c("Lip", "Tongue", "Gum and Other Mouth", "Floor of Mouth", "Salivary Gland", "Tonsil", 
                     "Oropharynx", "Nasopharynx", "Hypopharynx", "Other Oral Cavity and Pharynx", "Esophagus", 
                     "Stomach", "Small Intestine", "REC Colon", "Rectosigmoid Junction", "Rectum", 
                     "Anus, Anal Canal and Anorectum", "REC Liver and intrahepatic bile ducts", "Gallbladder", 
                     "Other Biliary", "Pancreas", "Other Digestive Organs", "Nose, Nasal Cavity and Middle Ear",
                     "Larynx", "REC Trachea, Other Respiratory Organs", "Lung and Bronchus", 
                     "Other Endocrine including Thymus", "REC Heart, Pleura", "Bones and Joints", 
                     "Melanoma of the Skin", "Other Non-Epithelial Skin", "Mesothelioma", "Kaposi Sarcoma", 
                     "REC Retroperitoneum and peritoneum", "Breast", "Vulva", "Vagina", "Cervix Uteri", 
                     "Corpus Uteri", "Uterus, NOS", "Ovary", "Other Female Genital Organs", "Penis", "Prostate",
                     "Testis", "Other Male Genital Organs", "Kidney and Renal Pelvis", "Ureter", 
                     "Urinary Bladder", "Other Urinary Organs", "Eye and Orbit", 
                     "Cranial Nerves Other Nervous System", "Brain", "Thyroid", "Miscellaneous", 
                     "REC Hodgkin's Disease", "REC Non-Hodgkin's lymphoma", "Myeloma", 
                     "REC Lymphocytic Leukemia", "REC Myeloid Leukemia", "REC Monocytic Leukemia", 
                     "REC Other Leukemia")
    }
    if(fill_sites[1] == "sitewhogen_num"){
      sites_all <- c(1, 2, 5, 4, 3, 7, 8, 6, 9, 10, 11, 12, 13, 915, 25, 26, 27, 929, 31, 32, 33, 36, 37, 38, 
                     941, 39, 66, 943, 42, 44, 45, 87, 88, 934, 46, 52, 51, 47, 48, 49, 50, 53, 56, 54, 55, 
                     57, 59, 60, 58, 61, 62, 64, 63, 65, 86, 968, 971, 73, 974, 977, 980, 983)
    }
    
    if(fill_sites[1] == "manual"){
      sites_all <- fill_sites[-1]
    } 
    
    sites_all <- as.character(sites_all)
    
  } else{
    fill <- FALSE
  }
  
  
  
  ## --- 0b getting and setting names / preferences
  
  count_var <- rlang::ensym(count_var)
  
  #race stratification option
  
  if(!is.null(race_var)){
    rs <- TRUE
  } else{
    rs <- FALSE
  }
  
  #if dattype is null, all relevant vars need to be provided
  if(is.null(dattype)){
    #test if any variable is not provided
    if(any(sapply(list(region_var,
                       age_var,
                       sex_var,
                       year_var,
                       site_var), is.null))){
      rlang::abort("If dattype is NULL, all variable names for `region_var`, `age_var`, `sex_var`, `year_var`, and `site_var` need to be provided.")
    }
  }
  
  
  if(!is.null(dattype)){
    # setting default var names and values for SEER data --> still need to update to final names!
    if (dattype == "seer") {
      if (is.null(region_var)) {
        region_var <- rlang::sym("p_region")
      } else{
        region_var <- rlang::ensym(region_var)
      }
      if (is.null(age_var)) {
        age_var <- rlang::sym("t_agegroupdiag")
      } else{
        age_var <- rlang::ensym(age_var)
      }
      if (is.null(sex_var)) {
        sex_var <- rlang::sym("SEX")
      } else{
        sex_var <- rlang::ensym(sex_var)
      }
      if (is.null(year_var)) {
        year_var <- rlang::sym("t_yeardiag")
      } else{
        year_var <- rlang::ensym(year_var)
      }
      if (is.null(site_var)) {
        site_var <- rlang::sym("t_icdcat")
      } else{
        site_var <- rlang::ensym(site_var)
      }
      if(rs){
        race_var <- rlang::ensym(race_var)
      }
    }
    
    
    #setting default var names and values for ZfKD data
    if (dattype == "zfkd") {
      if (is.null(region_var)) {
        region_var <- rlang::sym("p_region")
      } else{
        region_var <- rlang::ensym(region_var)
      }
      if (is.null(age_var)) {
        age_var <- rlang::sym("t_agegroupdiag")
      } else{
        age_var <- rlang::ensym(age_var)
      }
      if (is.null(sex_var)) {
        sex_var <- rlang::sym("SEX")
      } else{
        sex_var <- rlang::ensym(sex_var)
      }
      if (is.null(year_var)) {
        year_var <- rlang::sym("t_yeardiag")
      } else{
        year_var <- rlang::ensym(year_var)
      }
      if (is.null(site_var)) {
        site_var <- rlang::sym("t_icdcat")
      } else{
        site_var <- rlang::ensym(site_var)
      }
    }
  } else{
    # ensym if no dattype is given
    region_var <- rlang::ensym(region_var)
    age_var <- rlang::ensym(age_var)
    sex_var <- rlang::ensym(sex_var)
    year_var <- rlang::ensym(year_var)
    site_var <- rlang::ensym(site_var)
    if(rs){
      race_var <- rlang::ensym(race_var)
    }
  }  
  
  ## --- 0c checking input
  
  #CHK_vars: Check that all required variables are defined and present in dataset
  defined_vars <-
    c(
      rlang::as_string(region_var),
      rlang::as_string(age_var),
      rlang::as_string(sex_var),
      rlang::as_string(year_var),
      rlang::as_string(site_var),
      rlang::as_string(count_var),
      if(rs){rlang::as_string(race_var)}
    )
  
  not_found <- defined_vars[!(defined_vars %in% colnames(df))]
  
  
  if (length(not_found) > 0) {
    rlang::abort(c(
      "Columns are missing in `df`",
      "x" = paste0(paste(not_found, collapse = ", "), " are missing"),
      " "
    ))
  }
  
  #CHK_count: Check that count_var is in correct format
  
  if(!is.numeric(df[[rlang::as_name(count_var)]])){
    rlang::abort(c(
      "CHK_count] The column defined in `count_var` is not numeric.",
      "i" = paste0("You have used `count_var = \"", rlang::as_name(count_var), "\"`"),
      "Please make sure that the column of df defined as `count_var` is numeric and coded 1 for observed cases.",
      " "
    ))
  }
  
  if(!quiet &!( c(1) %in% (unique(df[[rlang::as_name(count_var)]])))){
    rlang::warn(c(
      "[CHK_count] The column defined in `count_var` does not contain any rows where count_var == 1. So no observed cases are found.",
      "i" = paste0("You have used `count_var = \"", rlang::as_name(count_var), "\"`"),
      "Please make sure that the column of df defined as `count_var` is numeric and coded 1 for observed cases.",
      " "
    ))
  }
  
  # create empty objects for possible warnings and errors
  
  problems_missing_refpop_strata_attr <- tidytable::tidytable()
  
  
  # ---- 1 data modifications ----
  
  ## --- 1a: prepare df
  
  # remove columns from data.frame that is not needed to save memory
  df <- df %>%
    tidytable::select(!!!rlang::syms(defined_vars))
  
  # change factors to character to avoid warning messages
  df <- df %>%
    tidytable::mutate(tidytable::across(.cols = where(is.factor), .fns = as.character))
  
  # remove all labels from df to avoid warning messages
  df[] <- lapply(df, function(x) { attributes(x) <- NULL; x })
  
  
  #make all important variables characters and make NAs explicit (for better matching)
  df <- df %>%
    tidytable::mutate(
      age = as.character(!!age_var),
      sex = as.character(!!sex_var),
      region = as.character(!!region_var),
      year = as.character(!!year_var),
      t_site = as.character(!!site_var),
      count_var = as.numeric(!!count_var)) %>%
    tidytable::mutate(tidytable::across(.cols = c(age, sex, region, year, t_site), 
                                          .fns = ~tidytable::replace_na(., na_explicit)))
  
  #prepare df for race stratification if needed
  if(rs){
    df <- df %>%
      tidytable::mutate(
        race = as.character(!!race_var)) %>%
      tidytable::mutate(tidytable::across(
        .cols = c(race), 
        .fns = ~tidytable::replace_na(., na_explicit)))
  }
  
  
  ## --- 1b: get used age, sex, region, year, t_site
  
  used_age    <- unique(df$age)
  used_sex    <- unique(df$sex)
  used_region <- unique(df$region)
  used_year   <- unique(df$year)
  used_t_site <- unique(df$t_site)
  if(rs){
    used_race <- unique(df$race)
  } else {
    used_race <- "none"
  }
  
  
  ## --- 1c: prepare refpop_df
  
  #make factor variables to character for better matching
  refpop_df <- refpop_df %>%
    tidytable::mutate(tidytable::across(.cols = where(is.factor), .fns = as.character))
  
  #remove attributes for better matching
  refpop_df[] <- lapply(refpop_df, function(x) { attributes(x) <- NULL; x })
  
  
  
  #prepare for race stratification option, if rs=TRUE
  if(rs){
    ##get available race levels from refpop_df and compare to used in df
    available_race <- unique(refpop_df$race)
    miss_race <- used_race[!used_race %in% available_race]
    ##take precautions for missing race data in df
    if(length(miss_race) > 0){
      rlang::warn(c(
        "[WARN Unknown Race] There are values from race missing in refrates_df.",
        "x" = "The following values for `race_var` present in the data are not availabe in refrates_df:",
        paste0(" -> ", miss_race),
        "!" = "For missing reference levels of race, no population can be found in refrates_df. Thus, rates will be overestimated.",
        "i" = "You have two options to fix this problem:",
        " " = "(1) clean race_var before running this function.",
        " " = "(2) run function again to calculate rates ignoring race by using `race_var = NULL`",
        " "
      ))
    }
  }
  
  
  if(!is.null(dattype)){
    #SEER only, if no race stratification is used, filter refpop_df so that only totals remain
    if(!rs & dattype == "seer"){
      refpop_df <- refpop_df %>%
        tidytable::filter(race == "Total - All races")
    }
  } else{
    #possibly dattype is null, but race_var might be present and `race_var = NULL`
    if(!rs & "race" %in% names(refpop_df)){
      refpop_df <- refpop_df %>%
        tidytable::filter(race == "Total - All races")
    }
  }
  
  ## --- 1d: prepare calc_totals option
  
  if(!is.logical(calc_totals)){
    if(!quiet){
      rlang::warn(c(
        "Parameter `calc_totals` should be logical (TRUE or FALSE).",
        "i" = "Default `calc_totals = FALSE` will be used instead.",
        " "))
    }
    calc_totals <- FALSE
  }
  
  if(calc_totals == TRUE){
    ct <- TRUE
  } else{
    ct <- FALSE
  }
  
  # ---- 2 analysis - calculate incidence ----
  ### Calculating Observed, then match with population_df and calculate incidence rates
  
  #2a calculate observed
  calc_count <- df %>%
    tidytable::summarize(incidence_cases = sum(.data$count_var, na.rm = TRUE), 
                          .by = tidyselect::all_of(c("age", "sex", "region", "year", "t_site",
                                                     if(rs){"race"}))
    )
  
  
  #enforce option fill
  
  if(fill == TRUE) {
    if(!quiet){
      rlang::inform(c(
        "Option `fill_sites == TRUE` is used.", 
        "i" = "This means empty strata will be filled for all combinations of used age, sex, year, region, race and sites.",
        " "
      ))
    }
    
    complete_vars_quo <- rlang::syms(c("age", "sex", "region", "year", 
                                       if(rs){"race"}))
    
    calc_count <- calc_count %>% #complete groups where i_observed = 0
      tidytable::complete(., !!!complete_vars_quo, t_site = !!sites_all) %>%
      tidytable::mutate(
        incidence_cases = tidytable::case_when(is.na(incidence_cases) ~ 0,
                                                TRUE ~ incidence_cases)) 
  } else{
    #if fill is not used, but totals are calculated, table also needs to be filled
    if(ct){
      if(!quiet){
        rlang::inform(c(
          "[INFO Totals] Option `calc_totals == TRUE` is used.", 
          "i" = "This means empty strata will be filled for all combinations of used age, sex, year, region, race and site to obtain correct totals for poulation.",
          " "
        ))
      }
      
      complete_vars_quo <- rlang::syms(c("age", "sex", "region", "year", 
                                         if(rs){"race"}, "t_site"))
      
      calc_count <- calc_count %>% #complete groups where i_observed = 0
        tidytable::complete(., !!!complete_vars_quo) %>%
        tidytable::mutate(
          incidence_cases = tidytable::case_when(is.na(incidence_cases) ~ 0,
                                                  TRUE ~ incidence_cases))
    }
  }
  
  #2b match population
  
  #CHK for missing strata in refpop_df
  used_strata <- calc_count %>%
    tidytable::distinct(tidyselect::all_of(c("age", "sex", "region", "year", if(rs){"race"})))
  
  missing_ref_strata <- used_strata %>%
    tidytable::anti_join(refpop_df, by = c("age", "sex", "region" , "year", if(rs){"race"}))
  
  if(nrow(missing_ref_strata) > 0){
    problems_missing_refpop_strata_attr <- tidytable::bind_rows(problems_missing_refpop_strata_attr, missing_ref_strata)
  }
  
  #do the matching
  calc_rates <- calc_count %>%
    tidytable::left_join(refpop_df, by = c("age", "sex", "region" , "year", if(rs){"race"})) %>%
    tidytable::select(-tidyselect::any_of(c("comment")))
  
  rm(calc_count)
  
  #cleanup for race option
  if(rs){
    calc_rates <- calc_rates %>%
      tidytable::mutate(race = tidytable::case_when(
        race == na_explicit ~ "Unknown",
        TRUE ~ race
      )) %>%
      tidytable::replace_na(list(
        population_pyar = 0,
        population_n_per_year = 0))
  }
  
  ### add totals
  
  if(ct){
    #"region", "age", "sex", "year", "race", "site"
    used_region <- unique(calc_rates$region) 
    
    sum_rate_reg <- calc_rates %>%
      tidytable::mutate(reg_group =  paste0("Total - All included regions: ", paste(used_region, collapse = ", "))) %>%
      tidytable::summarise(incidence_cases = sum(incidence_cases, na.rm = TRUE),
                            population_pyar = sum(population_pyar, na.rm = TRUE),
                            .by = tidyselect::all_of(c("age", "sex", "reg_group", "year", "t_site", if(rs){"race"}))) %>%
      tidytable::rename(region = reg_group) 
    
    calc_rates <- tidytable::bind_rows(calc_rates, sum_rate_reg)
    rm(sum_rate_reg)
    
    min_age <- min(calc_rates$age, na.rm = TRUE) 
    max_age <- max(calc_rates$age, na.rm = TRUE) 
    
    sum_rate_age <- calc_rates %>%
      tidytable::mutate(age_group =  paste0("Total - All included ages: ",  min_age, " - ", max_age)) %>%
      tidytable::summarise(incidence_cases = sum(incidence_cases, na.rm = TRUE),
                            population_pyar = sum(population_pyar, na.rm = TRUE),
                            .by = tidyselect::all_of(c("age_group", "sex", "region", "year", "t_site", if(rs){"race"}))) %>%
      tidytable::rename(age = age_group) 
    
    calc_rates <- tidytable::bind_rows(calc_rates, sum_rate_age)
    rm(sum_rate_age)
    
    
    sum_rate_sex <- calc_rates %>%
      tidytable::mutate(sex_group = "Total - All sexes") %>%
      tidytable::summarise(incidence_cases = sum(incidence_cases, na.rm = TRUE),
                            population_pyar = sum(population_pyar, na.rm = TRUE),
                            .by = tidyselect::all_of(c("age", "sex_group", "region", "year", "t_site", if(rs){"race"}))) %>%
      tidytable::rename(sex = sex_group) 
    
    calc_rates <- tidytable::bind_rows(calc_rates, sum_rate_sex)
    rm(sum_rate_sex)
    
    min_year <- min(calc_rates$year, na.rm = TRUE) 
    max_year <- max(calc_rates$year, na.rm = TRUE) 
    
    sum_rate_year <- calc_rates %>%
      tidytable::mutate(year_group = paste0("Total - All included years: ", min_year, " - ", max_year)) %>%
      tidytable::summarise(incidence_cases = sum(incidence_cases, na.rm = TRUE),
                            population_pyar = sum(population_pyar, na.rm = TRUE),
                            .by = tidyselect::all_of(c("age", "sex", "region", "year_group", "t_site", if(rs){"race"}))) %>%
      tidytable::rename(year = year_group) 
    
    calc_rates <- tidytable::bind_rows(calc_rates, sum_rate_year)
    rm(sum_rate_year)
    
    if(rs){
      used_race <- unique(calc_rates$race)
      
      sum_rate_race <- calc_rates %>%
        tidytable::mutate(race_group = paste0("Total - All included races: ", paste(used_race, collapse = ", "))) %>%
        tidytable::summarise(incidence_cases = sum(incidence_cases, na.rm = TRUE),
                              population_pyar = sum(population_pyar, na.rm = TRUE),
                              .by = tidyselect::all_of(c("age", "sex", "region", "year", "t_site", if(rs){"race_group"}))) %>%
        tidytable::rename(race = race_group) 
      
      calc_rates <- tidytable::bind_rows(calc_rates, sum_rate_race)
      rm(sum_rate_race)
    }
    
    used_site <- unique(calc_rates$t_site)
    
    sum_rate_site_cases <- calc_rates %>%
      tidytable::mutate(site_group = paste0("Total - All included cancer sites: ", paste(used_site, collapse = ", "))) %>%
      tidytable::summarise(incidence_cases = sum(incidence_cases, na.rm = TRUE),
                            .by = tidyselect::all_of(c("age", "sex", "region", "year", "site_group", if(rs){"race"}))) %>%
      tidytable::rename(t_site = site_group) 
    
    #calculate pyar separetly because you cannot add up across sites
    sum_rate_site_pyar <- calc_rates %>%
      tidytable::summarise(population_pyar = sum(population_pyar, na.rm = TRUE),
                            .by = tidyselect::all_of(c("age", "sex", "region", "year", "t_site", if(rs){"race"}))) %>%
      tidytable::distinct(tidyselect::all_of(c("age", "sex", "region", "year", if(rs){"race"}, "population_pyar")), .keep_all = TRUE) %>%
      tidytable::mutate(t_site = paste0("Total - All included cancer sites: ", paste(used_site, collapse = ", ")))
    
    #merge cases and pyar
    sum_rate_site <- sum_rate_site_cases %>%
      tidytable::left_join(sum_rate_site_pyar,
                            by = tidyselect::all_of(c("age", "sex", "region", "year", if(rs){"race"}, "t_site")))
    
    calc_rates <- tidytable::bind_rows(calc_rates, sum_rate_site)
    rm(sum_rate_site)
    
  } #end ct option
  
  
  ### 2e Calculate rates
  
  rates_pre <- calc_rates %>%
    tidytable::mutate(
      incidence_crude_rate = tidytable::case_when(
        .data$population_pyar > 0 ~ .data$incidence_cases / .data$population_pyar * 100000,
        TRUE                    ~ NA_real_),
      region = as.factor(region),
      sex = as.factor(sex)
    ) %>%
    tidytable::mutate(
      population_n_per_year = tidytable::case_when(
        stringr::str_detect(.data$year, "^Total - All included years") ~ NA_real_,        
        stringr::str_length(.data$year) == 4 ~ .data$population_pyar,
        stringr::str_length(.data$year) > 4  ~ .data$population_pyar / 5,
        TRUE ~ NA_real_)
    )
  
  
  
  rm(calc_rates)
  
  ## ---- 3 finalize output ----
  
  ##3a Labelling and sorting
  
  final_sort_var_quo <- rlang::syms(c("t_site", "region", "year", "sex", "age", if(rs){"race"}))
  
  
  #final arranging
  rates <- rates_pre %>%
    #sort variables
    tidytable::select(tidyselect::any_of(
      c("t_site", "region", "year", "sex", "age", if(rs){"race"}, 
        "incidence_cases", "incidence_crude_rate", "population_pyar", "population_n_per_year")
    )) %>%
    #sort dataset
    tidytable::arrange(!!!final_sort_var_quo)
  
  #write attributes for matched strata
  attr(rates, "strata_var_names") <- used_strata
  
  #write attributes for error and warning messages
  if(nrow(problems_missing_refpop_strata_attr) > 0){
    if(!quiet){
      rlang::inform(c(
        "[INFO Reference Population Missing] For some strata no population can be found.",
        "i" = paste0(nrow(problems_missing_refpop_strata_attr), " strata have no reference population in `refpop_df`"),
        " - Solution could be to add these strata to `refpop_df`.",
        "!" = "Check attribute `problems_missing_refpop_strata` of results to see what strata are affected.",
        " "
      ))
    }
    
    attr(rates, "problems_missing_refpop_strata") <- problems_missing_refpop_strata_attr
    
  }
  
  
  return(rates)
  
} 

