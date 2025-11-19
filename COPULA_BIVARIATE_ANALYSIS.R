
##------------------------- JOINT MODELING- BIVARIATE COPULA ---------------------------------
library(rio)
install_formats()
library(survey)
library(tidyverse)
library(GJRM)
library(sf)
library(patchwork)
library(ggspatial)
library(stars)
library(ggpubr)
library("psych") # to compute uncorrected kendal tau 
options(survey.lonely.psu = "average")


# read the data 

kid_path <- "/Users/rachidmuleia/Dropbox/DHS_DATA_2021/MZ_2022-23_DHS_05102024_917_55954/MZKR81SV/MZKR81FL.SAV"
household_pah <- "/Users/rachidmuleia/Dropbox/DHS_DATA_2021/MZ_2022-23_DHS_05102024_917_55954/MZHR81SV/MZHR81FL.SAV"
birth_df <- "/Users/rachidmuleia/Dropbox/DHS_DATA_2021/MZ_2022-23_DHS_05102024_917_55954/MZBR81SV/MZBR81FL.SAV"
pr_file <- "/Users/rachidmuleia/Dropbox/DHS_DATA_2021/MZ_2022-23_DHS_05102024_917_55954/MZPR81SV/MZPR81FL.SAV"
View(household_data)
save_tables <- "/Users/rachidmuleia/Dropbox/INS/ARTIGO_COPULA_BIVARIATE/TABELAS"
list.files("/Users/rachidmuleia/Dropbox/DHS_DATA_2021/MZ_2022-23_DHS_05102024_917_55954/")

kid_data <- import(kid_path)
household_data <- import(household_pah)
birth_data <- import(birth_df)
pr_df <- import(pr_file)

View(kid_data)


variable_names <- lapply(names(kid_data),\(x){
  attr(kid_data[, x], "label")
})

var_names <- do.call(rbind, variable_names)
var_names1 <- cbind(names(kid_data), var_names)
View(var_names1)



variable_names <- lapply(names(household_data),\(x){
  attr(household_data[, x], "label")
})

var_names <- do.call(rbind, variable_names)
var_names2 <- cbind(names(household_data), var_names)
View(var_names2)




variable_names <- lapply(names(birth_data),\(x){
  attr(birth_data[, x], "label")
})

var_names <- do.call(rbind, variable_names)
var_names3 <- cbind(names(birth_data), var_names)
View(var_names1)




variable_names <- lapply(names(birth_data),\(x){
  attr(birth_data[, x], "label")
})

variable_names <- lapply(names(pr_df),\(x){
  attr(pr_df[, x], "label")
})
var_names <- do.call(rbind, variable_names)
var_names4 <- cbind(names(pr_df), var_names)
View(var_names4)
# merge kid data and household data 
# first select the variables to be used in the analysis that are in the houshold dataset

household_data[, c("HV001", "HV002", "HV201B", "HV237", "HV230A")]
kid_data <- merge(kid_data, household_data[, c("HV001", "HV002", "HV201B", "HV237", "HV230A")],
                  by.x = c("V001", "V002"), by.y = c("HV001", "HV002"))


kid_df <- merge(kid_data, pr_df[, c("HV001", "HV002", "HV003","HML32" ,"HML35", "HA57", 
                                    "HC1", "HV042", "HV103", "HVIDX","HC60", "HC56", 
                                    "HV227", "HML1", "HV213", "HV214", "HV215","SH136A", "HML21")],
                  by.x = c("B16", "V001", "V002","V003"), by.y = c("HVIDX","HV001", "HV002", "HC60"))

kid_df <- kid_df |>
  mutate(
    anemia_response = case_when(
      HC56 < 80 ~ 1,
      HC56 >= 80 ~ 0
    ),
    
    
    malaria_response = case_when(
      HML35 == 1 ~ 1,
      HML35 == 0 ~ 0
    ),
    
    has_mosquito_net = dplyr::recode(HV227, "0" = "2_NO", "1" = "1_YES"),
    
    slept_under_net = case_when(
      HML21 == 0 | HV227 == 0 ~ "2_NO",
      HML21 == 1 ~ "1_YES"
    ),
    
    had_fever = case_when(
      H22 == 0 ~ "2_NO",
      H22 == 1 ~ "1_YES"
    ),
    
    had_diarreha = case_when(
      H11 == 0 ~ "2_NO",
      H11 %in% c(1,2) ~ "1_YES",
    ),
    number_mosquito_net = case_when(
      HML1 == 0 ~ "1_NONE",
      HML1 == 1 ~ "1",
      HML1 == 2  ~ "2",
      HML1 > 2 ~ "3+"
    ),
    
    floor_material = case_when(
      HV213 %in% c(10,20,12,21,22,11) ~ "1_natural_rudmentary",
      HV213 %in% c(32,30,34,31,33) ~ "2_morden"
    ),
    
    wall_material = case_when(
      HV214 %in% c(10, 11, 12, 20,23,24,21,22) ~ "1_natural_rudmentary",
      HV214 %in% c(30,31,32,33,24) ~ "2_morden"
    ),
    
    roof_material = case_when(
      HV215 %in% c(10, 11,12,20,22) ~ "1_natural_rudmentary",
      HV215 %in% c(30,31,33,34) ~ "2_morden"
    ),
    
    indoor_spraying = case_when(
      SH136A == 0 ~ "2_NO",
      SH136A == 1 ~ "1_YES"
    ),
    
    SEX_CHILD = dplyr::recode(B4, "1" = "1_MALE", "2" = "2_FEMALE"), 
    
    RELIGION = case_when(
      #V130 == 1 ~ "1_CHRISTIAN",
      V130 %in% c(1,3,4,5) ~ "1_Christian",
      V130 == 2 ~ "2_MUSLIM",
      V130 %in% c(6,96) ~ "3_OTHER_NORELIGION"
    ), 
    MOTHER_EDUCATION = case_when(
      V106 == 0 ~ "1_NO_EDUCATION",
      V106 == 1 ~ "2_PRIMARY",
      V106 %in% c(2,3) ~ "3_SECONDARY_HIGHER"
    ),
    
    MARITAL_STATUS = case_when(
      V501 == 0 ~ "1_SINGLE",
      V501 %in% c(1,2) ~ "2_MARRIED",
      V501 %in% c(3,4,5) ~ "3_WIDOWED_DIVORCE"
    ),
    RESIDENCE = dplyr::recode(V025, "1" = "1_URBAN", "2" = "2_RURAL"),
    
    REGION = case_when(
      V024  %in% c(1,2,3) ~ "1_NORTH",
      V024 %in% c(4,5,6,7) ~ "2_CENTER",
      V024 %in% c(8,9,10,11) ~ "3_SOUTH"
    ),
    
    WEALTH_INDEX = case_when(
      V190 %in% c(1,2) ~ "1_POORER",
      V190 == 3 ~ "2_MIDDLE",
      V190 %in% c(4,5) ~ "RICH",
    ),
    
    SOURCE_WATER = case_when(
      V113 %in% c(10, 11, 12, 13, 14, 20, 30, 40,51, 71, 31,43,62,21) ~ "1_IMPROVED",
      V113 %in% c(41, 42 , 61,32,96) ~ "2_UNIMPROVED"
    ),
    TYPE_SANITATION = case_when(
      V116 %in% c(10, 11, 12, 13, 15 ,20, 21, 22, 41) ~ "1_IMPROVED",
      
      V116 %in% c(14, 42, 43, 50, 51, 52, 53, 54,31,96,31,23) ~ "2_UNIMPROVED"
      
    ),
    
    MOTHER_OCCUPATION = dplyr::recode(V714, "1" = "1_YES", "0" = "2_N0"),
    
    
    DURATION_BREAST_FEED = case_when(
      M5 == 0 | M4 == 94 ~ "1_NEVER_BREAST",
      between(M5, 1,5) ~ "2_LESS_SIX_MONTH",
      between(M5, 6,35) ~ "3_MORE_SIX_MONTH",
      M5 == 93 ~ "4_EVER_BREAST_FED"
    ),
    
    RECEIVED_VITAMIN_A = case_when(
      H34 == 0 ~ "2_NO", 
      H34 == 1 ~ "1_YES"
    ),
    
    ACUTE_RESPIRATORY = case_when(
      (H31C %in% c(1,3)  & H31B == 1) ~ "1_YES",
      TRUE ~ "2_NO"
      
    ),
    
    DIARRHEA = dplyr::recode(H11, "2" = "1_YES", "0" = "2_NO"),
    
    AGE_CHILD = B19,
    
    FATHERS_AGE = V730,
    
    MOTHERS_AGE = V012,
    
    MOTHERS_AGE_CAT = case_when(
      between(MOTHERS_AGE, 15,24) ~ "1_15-24",
       between(MOTHERS_AGE,25,34) ~ "2_25-34",
      between(MOTHERS_AGE,35,49) ~ "3_35-49"
    ),
    
    WEIGHTS = V005/1000000,
    
    COMORBIDITY = case_when(
      ACUTE_RESPIRATORY == "1_YES" & DIARRHEA == "1_YES" ~ "1_YES",
      ACUTE_RESPIRATORY != "1_YES" | DIARRHEA == "1_YES" ~ "2_NO"
    ),
    
    HEAD_HOUSEHOLD = dplyr::recode( V151, "1" = "1_MALE", "2" = "2_FEMALE"),
    NUMBER_HOUSEHOLD = if_else(V136 < 5, "1_LESS_FIVA", "2_FIVE_MORE"),
    
    
    ENERGY_SOURCE = case_when(
      V161 %in% c(1, 2, 3, 4, 5, 6, 15,7,8) ~ "1_Modern_Clean",
      V161 %in% c(9, 10, 11, 12, 13, 14, 17,16) ~ "2_Traditional_Solid Biomass",
      TRUE ~ NA_character_
    ),
    
    ACCESS_MEDIA = case_when(
      V157 %in% c(1,2,3) |  V158 %in% c(1,2,3) | V159 %in% c(1,2,3) ~ "1_YES",
      TRUE ~ "2_NO"
    ),
    
    newspaper_never = if_else(V157 == 0,1,0),
    newspaper_less_week = if_else(V157 == 1,1,0),
    newspaper_once_week = if_else(V157 == 2, 1,0),
    newspaper_every_day = if_else(V157 == 3, 1, 0),
    
    television_never = if_else(V158 == 0,1,0),
    television_less_week = if_else(V158 == 1, 1, 0),
    television_once_week = if_else(V158 == 2, 1,0),
    television_every_day = if_else(V158 == 3,1,0),
    
    radio_never = if_else(V159 == 0,1,0),
    radio_less_week = if_else(V159 == 1,1,0),
    radio_once_week = if_else(V159 == 2, 1, 0),
    radio_every_day = if_else(V159 == 3, 1, 0),
    
    internet_never = if_else(V171B == 0, 1, 0),
    internet_less_week = if_else(V171B == 1, 1, 0),
    internet_once_week = if_else(V171B == 2, 1, 0),
    internet_every_day = if_else(V171B == 3, 1, 0),
    
    
    
    bcg = H2 %in% c(1, 2, 3),
    dpt1 = H3 %in% c(1, 2, 3),
    dpt2 = H5 %in% c(1, 2, 3),
    dpt3 = H7 %in% c(1, 2, 3),
    polio1 = H4 %in% c(1, 2, 3),
    polio2 = H6 %in% c(1, 2, 3),
    polio3 = H8 %in% c(1, 2, 3),
    measles = H9 %in% c(1, 2, 3),
    
   fully_vaccinated = bcg & dpt1 & dpt2 & dpt3 & polio1 & polio2 & polio3 & measles,
   FULL_VACCIN = case_when(
     fully_vaccinated == TRUE ~ "1_YES",
     fully_vaccinated == FALSE ~ "2_NO"
   ),
   
   NUMBER_CHILD_FIVE = case_when(
     V137 <3 ~ "1_Less_Three",
     V137 >=3 ~ "2_Greater_Three"
   ),
   
   DISTANCE_HOSPITAL = case_when(
     V467D == 1 ~ "1_BIG_PROBLEM",
     V467D == 2 ~ "2_NOT_BIG_PROBLEM"
   ),
   
   
   stunted = case_when(
     HW70 < -2 ~ "Stunted",
     HW70 >= -2 ~ "Not Stunted",
     TRUE ~ NA_character_
   ),
   wasted = case_when(
     HW71 < -2 ~ "Wasted",
     HW71 >= -2 ~ "Not Wasted",
     TRUE ~ NA_character_
   ),
   underweight = case_when(
     HW72 < -2 ~ "Underweight",
     HW72 >= -2 ~ "Not Underweight",
     TRUE ~ NA_character_
   ),
   
   ENOUGH_WATER = dplyr::recode(HV201B, "0" = "2_NO", "1" = "1_YES"),
   WATER_TREATMENT = dplyr::recode(HV237, "0" = "2_NO", "1" = "1_YES"),
   PLACE_WASH_HANDS = case_when(
     HV230A %in% c(1,2) ~ "1_OBSERVED",
     HV230A %in% c(3,4,5) ~ "2_NOT_OBSERVED"
   ),
   BIRTH_ORDER = case_when(
     between(BORD, 1,3 ) ~ "1_1-3",
     between(BORD, 4,6 ) ~ "2_4-6",
     BORD > 6 ~ "3_>6"
   )
   
   
  
    
    

)|> dplyr::filter(B5 == 1 & HV103 ==1 & HV042 == 1)



#construnct media access using PCA
media_index <- prcomp(~ newspaper_never + newspaper_less_week + newspaper_once_week +
                        newspaper_every_day + television_never + television_less_week +
                        television_once_week + television_every_day + radio_never +
                        radio_less_week + radio_once_week + radio_every_day + 
                        internet_never + internet_less_week + internet_once_week + internet_every_day,
                         kid_df, center = TRUE)

# extract the score to the define the media index 
media_data <- kid_df |>
  dplyr::select(newspaper_never , newspaper_less_week , newspaper_once_week ,
                  newspaper_every_day , television_never , television_less_week ,
                  television_once_week , television_every_day , radio_never ,
                  radio_less_week , radio_once_week , radio_every_day , 
                  internet_never , internet_less_week , internet_once_week , internet_every_day)

kid_df$media_score <-as.matrix(media_data)%*%t(t(media_index$rotation[,1]))

kid_df <- kid_df |>
  mutate(
    media_tertiles = ntile(media_score,3),

    media_access_cat = case_when(
      media_tertiles == 1 ~ "1_LOW",
      media_tertiles == 2~ "2_MODERATE",
      media_tertiles ==3 ~ "3_HIGH"
    ),
    media_access_cat1 = case_when(
      media_score < -1.4576326 ~ "1_LOW",
      between(media_score,-1.4576326,-0.485585) ~ "2_MODERATE",
      media_score > -0.485585 ~ "3_HIGH"
    )
  )


table(kid_df$media_access_cat1)


#---------- read the spatial data 

path_map <- '/Users/rachidmuleia/Dropbox/INS/ARTIGO_COPULA_BIVARIATE/TABELAS'
map_moz <- st_read(paste(path_map, "MZGE81FL.shp", sep = '/'))


#------------------merge survey data with spatial data ------------------------------------------------------

var_exp <- c("SEX_CHILD", "RELIGION", "MOTHER_EDUCATION",  "MARITAL_STATUS",
             "RESIDENCE", "REGION", "WEALTH_INDEX", "SOURCE_WATER", "TYPE_SANITATION", 
             "MOTHER_OCCUPATION", "RECEIVED_VITAMIN_A", "HEAD_HOUSEHOLD", "NUMBER_HOUSEHOLD", 
             "ENERGY_SOURCE", "AGE_CHILD", "MOTHERS_AGE", 
             "NUMBER_CHILD_FIVE", "malaria_response", "anemia_response", "WEIGHTS", 
             "DIARRHEA", "ACUTE_RESPIRATORY", "BIRTH_ORDER",
             "ENOUGH_WATER", "WATER_TREATMENT", "PLACE_WASH_HANDS", "media_access_cat1",
             "floor_material", "roof_material", "wall_material", "number_mosquito_net",
             "has_mosquito_net", "stunted", "underweight", "wasted", "slept_under_net", "had_fever", "had_diarreha") 




kid_df <- merge(kid_df[,c("V001", "V022",var_exp)], map_moz[, c("DHSCLUST", "LATNUM", "LONGNUM")], by.x = "V001", by.y = "DHSCLUST")



#------------------------- Sample description ------------------------------------------------------------
descriptive_pri<-function(data, surveydesign, vector_var,rd){
  prop_svy<-list()
  tab<-list()
  for(i in 1:length(vector_var)){
    prop_svy[[i]]=svymean(~data[,vector_var[i]], surveydesign, na.rm=TRUE)
    tab[[i]]<-table(data[,vector_var[i]])
  }
  conf_int<-lapply(prop_svy, confint)
  pop<-lapply(prop_svy,function(x){round(x*100,rd)})
  conf<-lapply(conf_int,function(x){round(x*100,rd)})
  vec<-list()
  vec_res<-list()
  for(i in 1: length(prop_svy)){
    
    vec[[i]]<-paste(pop[[i]][1:length(prop_svy[[i]])], ' (', paste(conf[[i]][,1],conf[[i]][,2],sep='-'), ')', sep='')
    vec_res[[i]]<-data.frame(Var=rep(vector_var[i],length(names(tab[[i]]))),categoria=names(tab[[i]]),N=tab[[i]],Est=t(t(vec[[i]])))
  }
  result<-do.call(rbind, vec_res)
  return(result)
}



kid_df <- kid_df |>
  mutate(comorbid = case_when(
    malaria_response == 1 & anemia_response == 1 ~ 1, 
    TRUE ~ 0
  ),
  comorbid = factor(comorbid, level = c(0,1), labels = c("2_NO", "1_YES"))
  ) |>
  filter( !is.na(malaria_response) & !is.na(anemia_response))

kid_desing <-svydesign(id=~V001, strata=~V022, data=kid_df, weights =~WEIGHTS, nest = TRUE)

var_explanatory <- c("SEX_CHILD", "RELIGION", "MOTHER_EDUCATION","MARITAL_STATUS",
                     "RESIDENCE", "REGION", "WEALTH_INDEX",
                     "MOTHER_OCCUPATION", 
                     "RECEIVED_VITAMIN_A", "HEAD_HOUSEHOLD" ,"NUMBER_HOUSEHOLD", "NUMBER_CHILD_FIVE",  "BIRTH_ORDER",
                     "ENOUGH_WATER",
                     "WATER_TREATMENT", "PLACE_WASH_HANDS" ,"stunted", "had_fever", "has_mosquito_net", "underweight")

sample_tab <- descriptive_pri(data = kid_df, surveydesign = kid_desing, vector_var = var_explanatory, rd = 1)

descriptive_pri(data = kid_df, surveydesign = kid_desing, vector_var = "malaria_response", rd = 1)
descriptive_pri(data = kid_df, surveydesign = kid_desing, vector_var = "anemia_response", rd = 1)



descriptive_pri(data = kid_df, surveydesign = kid_desing, vector_var = "comorbid", rd = 1)

rio::export(sample_tab, paste(save_tables, "DESCRIPTIVE_TABLE.xlsx", sep = '/'))

# ---------------------- compute the prevalence by selected covariates -----------------------------------

# function to compute the prevalence and chisquare test
cross_svy <- function(data, var_vec, response, tsdesign){
  
  if(length(na.omit(unique(data[,response]))) != 2) {
    stop("Error: response must have two categories")
  }
  
  list_for<- lapply(var_vec, \(x){
    as.formula(paste("~",paste(x, response, sep = "+")))
  })
  
  prop_svy <- lapply(list_for, \(x){
    round(prop.table(svytable(x, design = tsdesign),1)*100,1)
  })
  
  p_value <- lapply(list_for, \(x){
    svychisq(x, design = tsdesign)
  })
  
  raw_table <- lapply(var_vec, \(x){
    table(data[,x], data[,response])
  })
  
  
  p_value <- lapply(list_for, \(x){
    svychisq(x, design = tsdesign)
  })
  
  table_ls <- list()
  for(i in 1: length(var_vec)){
    col1<-paste(raw_table[[i]][,1], " (",  prop_svy[[i]][,1], ")",sep = "")
    
    col2<-paste(raw_table[[i]][,2], " (",  prop_svy[[i]][,2], ")",sep = "")
    
    table_ls[[i]] <- data.frame(var = rep(var_vec[i],length(rownames(raw_table[[i]]))),
                                category = rownames(raw_table[[i]]),
                                col1 = col1, col2 = col2, 
                                p_value = rep(round(p_value[[i]]$p.value,3),length(rownames(raw_table[[i]])) ))
  }
  
  

  
  table_descr <- do.call(rbind, table_ls)
  names(table_descr)[c(3,4)] <- sort(na.omit(unique(data[,response])))
  return(table_descr)
}


kid_df$malaria_response <- factor(kid_df$malaria_response, levels = c(0,1), labels = c("2_NO", "1_YES"))
kid_df$anemia_response <- factor(kid_df$anemia_response, levels = c(0,1), labels = c("2_NO", "1_YES"))
kid_df$media_access_cat1 <- factor(kid_df$media_access_cat1)
kid_df <- kid_df |>
  mutate(comorbid = case_when(
    malaria_response == "1_YES" & anemia_response == "1_YES" ~ 1, 
    TRUE ~ 0
  ))
kid_desing <-svydesign(id=~V001, strata=~V022, data=kid_df, weights =~WEIGHTS, nest = TRUE)


malaria_crosstab <- cross_svy(data = kid_df, var_vec = var_explanatory, response = "malaria_response", tsdesign = kid_desing)
anemia_crosstab <-cross_svy(data = kid_df, var_vec = var_explanatory, response = "anemia_response", tsdesign = kid_desing)

cross_svy(data = kid_df, var_vec = "malaria_response", response = "anemia_response", tsdesign = kid_desing)

prop.table(svytable(~malaria_response+anemia_response, design = kid_desing))


comorbidit_crosstab <-cross_svy(data = kid_df, var_vec = var_explanatory, response = "comorbid", tsdesign = kid_desing)



rio::export(malaria_crosstab , paste(save_tables, "TABELA_MALARIA.xlsx",sep = "/"))
rio::export(anemia_crosstab ,  paste(save_tables, "TABELA_ANEMIA.xlsx",sep = "/"))
rio::export(comorbidit_crosstab, paste(save_tables, "TABELA_COMORBIDIT.xlsx",sep = "/"))




#------------------------------- fit regression model - copula bivariate ----------------------------------------------------

# run bivariate logistic regression to select important covariate

var_exp <- c("SEX_CHILD", "RELIGION", "MOTHER_EDUCATION",  "MARITAL_STATUS",
             "RESIDENCE", "WEALTH_INDEX", 
             "MOTHER_OCCUPATION", "RECEIVED_VITAMIN_A", "HEAD_HOUSEHOLD", "NUMBER_HOUSEHOLD", 
             "AGE_CHILD", "MOTHERS_AGE", 
             "NUMBER_CHILD_FIVE", "malaria_response", "anemia_response", "WEIGHTS", 
              "BIRTH_ORDER",
             "ENOUGH_WATER", "WATER_TREATMENT", "PLACE_WASH_HANDS",
             "has_mosquito_net", "stunted", "underweight", "had_fever", "had_diarreha") 


kid_desing <-svydesign(id=~V001, strata=~V022, data=kid_df[, c("V001", "V022",var_exp) ], weights =~WEIGHTS, nest = TRUE)



# fit the bivariate logistic regressioxn for diarrhea
var_exp1 <- var_exp[-which(var_exp %in% c("malaria_response", "anemia_response", "WEIGHTS", "AGE_CHILD", "MOTHERS_AGE"))]
bivariate_model <- list()
for(i in 1:length(var_exp1)){
  bivariate_model[[i]] <- svyglm(as.formula(paste("malaria_response ~ ", var_exp1[i])), design = kid_desing, family =quasibinomial())
}

summ_model <- lapply(bivariate_model, summary)
var_selected <- c()
var_not_selected <- c()
for(i in 1:length(var_exp1)){
  if( any(summ_model[[i]]$coefficients[-1,4] < 0.25)){
    var_selected[i] <- var_exp1[i]
  } else {
    var_not_selected[i] <- var_exp1[i]
  }
}



var_marginal_malaria <- na.omit(var_selected)
#var_marginal_malaria <- var_marginal_malaria[-c(1,26,16,12,5,20,17,13,11,3)]


bivariate_model_anemia <- list()

for(i in 1:length(var_exp1)){
  bivariate_model_anemia[[i]] <- svyglm(as.formula(paste("anemia_response ~ ", var_exp1[i])), design = kid_desing, family =quasibinomial())
}

summ_model_ari <- lapply(bivariate_model_anemia, summary)
var_selected_ari <- c()
var_not_selected_ari <- c()
for(i in 1:length(var_exp1)){
  if( any(summ_model_ari[[i]]$coefficients[-1,4] < 0.25)){
    var_selected_ari[i] <- var_exp1[i]
  } else {
    var_not_selected_ari[i] <- var_exp1[i]
  }
}


var_marginal_anemia <- na.omit(var_selected_ari) 


var_marginal_anemia <-var_marginal_anemia[-c(17,18,19,20,21, 23,24,25,14,12,11,10,4)]

#------------------------------------------- fit the copula bivariate model --------------------------------------------



eq1 <- as.formula(paste("malaria_response~", paste(c(var_marginal_malaria, "has_mosquito_net"), collapse = "+"),"+s(AGE_CHILD)+s(MOTHERS_AGE)+s(LONGNUM, LATNUM)"))


eq2 <- as.formula(paste("anemia_response~", paste(var_marginal_anemia, collapse = "+"),"+s(AGE_CHILD)+s(MOTHERS_AGE)+s(LONGNUM, LATNUM)"))

fl <- list(eq1, eq2)

out <- gjrm(fl, margins = c("probit", "probit"), copula = "C0",
            model = "B" ,data = kid_df[,c("WEIGHTS", "wall_material", "MOTHERS_AGE_CAT","LATNUM", "LONGNUM",var_exp)],weights = kid_df[,"WEIGHTS"])
summary(out)

#copula_family <- c("N", "C0", "C90", "C180", "C270", "GAL0", "GAL90", 
#                   "GAL180", "GAL270", "J0", "J90", "J180", "J270", 
#                   "G0", "G90", "G180", "G270", "F", "AMH", "FGM", 
#                   "T", "PL", "HO")


copula_family <- c("N", "C0", "C90", "C180", "C270", "GAL0", "GAL90", 
                   "GAL180", "GAL270", "J0", "J90", "J180", "J270", 
                   "G0", "G90", "G180", "G270", "F", "AMH", "FGM", 
                   "T", "PL", "HO")
# model selection

model1 <- list()
for(i in 1:length(copula_family)){
  model1[[i]] <-gjrm(fl, margins = c("probit", "probit"), copula = copula_family[i],
                    model = "B",data = kid_df[,c("LONGNUM", "LATNUM",var_exp)], weights = kid_df[, "WEIGHTS"])
}

AIC_model1 <- unlist( lapply(model1, AIC)) 
BIC_model1 <- unlist( lapply(model1, BIC))
copula_family[which.min(AIC_model1)]
copula_family[which.min(BIC_model1)]
copula_family[order(BIC_model1)]
copula_family[order(AIC_model1)]

model2 <- list()
for(i in 1:length(copula_family)){
  model2[[i]] <-gjrm(fl, margins = c("logit", "logit"), copula = copula_family[i],
                    model = "B",data = kid_df[,c("LONGNUM", "LATNUM",var_exp1)],weights = kid_df[, "WEIGHTS"])
}

model2_final <-gjrm(fl, margins = c("logit", "logit"), copula = "G0",
                   model = "B",data = kid_df[,c("LONGNUM", "LATNUM",var_exp1, "malaria_response", "anemia_response", "AGE_CHILD", "MOTHERS_AGE")],weights = kid_df[, "WEIGHTS"])

AIC_model2 <- unlist( lapply(model2, AIC))
BIC_model2 <- unlist( lapply(model2, BIC))
copula_family[which.min(AIC_model2)]
copula_family[which.min(BIC_model2)]
copula_family[order(BIC_model2)]
copula_family[order(AIC_model2)]


model3 <- list()
for(i in 1:length(copula_family)){
  model3[[i]] <-gjrm(fl, margins = c("logit", "probit"), copula = copula_family[i],
                     model = "B" ,data = kid_df[,c("LONGNUM", "LATNUM",var_exp)], weights = kid_df[, "WEIGHTS"])
}


AIC_model3 <- unlist( lapply(model3, AIC))
copula_family[which.min(AIC_model3)]

model4 <- list()
for(i in 1:length(copula_family)){
  model4[[i]] <-gjrm(fl, margins = c("probit", "logit"), copula = copula_family[i],
                     model = "B" ,data = kid_df[,c("LONGNUM", "LATNUM",var_exp)],weights = kid_df[, "WEIGHTS"])
}

AIC_model4 <- unlist( lapply(model4, AIC))
copula_family[which.min(AIC_model4)]







# allow the theta to vary with covariate

eq3 <- ~ s(LONGNUM, LATNUM)
fl1 <- list(eq1,eq2, eq3)
model_cop_var <-gjrm(fl1, margins = c("logit", "logit"), copula = "G0",
                     model = "B",data = kid_df[,c("LONGNUM", "LATNUM",var_exp1,"malaria_response", "anemia_response", "AGE_CHILD", "MOTHERS_AGE")],weights = kid_df[, "WEIGHTS"])



#--------------------------------- EXTRACT THE OR ----------------------------------

# extract the odds ration and export to excel 

#summary_model <- summary(model2[[22]])
summary_model <- summary(model2_final)
rownames(summary_model$tableP1)

# function to extract OR 
extract_or_copula <- function(obj_summary,rd, rd.pvalue){
  or <- exp(obj_summary[,1])
  p_value <- obj_summary[,4]
  ci_l <- obj_summary[,1] - 1.96*obj_summary[,2]
  ci_s <- obj_summary[,1] + 1.96*obj_summary[,2]
  or_pvalue <- data.frame(variables =rownames(obj_summary), OR =  paste(round(or,rd), " (", round(exp(ci_l),2), "-", round(exp(ci_s),2), ")",sep = ""), p_value = round(p_value,rd.pvalue) )
  return(or_pvalue)
  
}

extract_or_copula(obj_summary = summary_model$tableP1, rd=1, rd.pvalue = 3)

malaria_marginal <- extract_or_copula(obj_summary = summary_model$tableP1, rd=1, rd.pvalue = 3)

anemia_marginal <- extract_or_copula(obj_summary = summary_model$tableP2, rd=1, rd.pvalue = 3)

# export to excel 


rio::export(malaria_marginal, paste(save_tables, "MALARIA_MARGINAL.xlsx",sep = "/"))

rio::export(anemia_marginal, paste(save_tables, "ANEMIA_MARGINAL.xlsx",sep = "/"))

#---------------------plot the non-linear effects -------------------------------

# extract the effect of age on malaria  to plot on ggplot 
smooth_data <- plot(model2_final, eq = 1,select = 1, term = "s(AGE_CHILD)", plot = FALSE)
smooth_data1 <- plot(model2_final, eq = 1,select = 2, term = "s(MOTHERS_AGE)", plot = FALSE)



df_smooth <- data.frame(
  x = smooth_data[[1]]$x,
  fit = smooth_data[[1]]$fit,
  se = smooth_data[[1]]$se
) %>%
  mutate(
    lower = fit - 1.96 * se,
    upper = fit + 1.96 * se
  )


df_smooth1 <- data.frame(
  x = smooth_data1[[2]]$x,
  fit = smooth_data1[[2]]$fit,
  se = smooth_data1[[2]]$se
) %>%
  mutate(
    lower = fit - 1.96 * se,
    upper = fit + 1.96 * se
  )


malaria_plot_child <- ggplot(df_smooth, aes(x = x, y = fit)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  annotate("text", x = 10, y = 2, label = "(a)", size = 4, color = "black")+
  labs(x = "Child's age in months", y = "Effect of age on malaria ") +
  ylim(-4, 2.5)+
  theme_bw()


malaria_plot_mother <- ggplot(df_smooth1, aes(x = x, y = fit)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  annotate("text", x = 10, y = 2, label = "(c)", size = 4, color = "black")+
  labs(x = "Mother's age in years", y = "Effect of age on malaria ") +
  ylim(-4, 2.5)+
  theme_bw()


# extract the effect of age on anemia  to plot on ggplot 
smooth_data <- plot(model2_final, eq = 2 ,select = 1, term = "s(AGE_CHILD)", plot = FALSE)
smooth_data1 <- plot(model2_final, eq = 2 ,select = 2, term = "s(MOTHERS_AGE)", plot = FALSE)

library(dplyr)

df_smooth <- data.frame(
  x = smooth_data[[1]]$x,
  fit = smooth_data[[1]]$fit,
  se = smooth_data[[1]]$se
) %>%
  mutate(
    lower = fit - 1.96 * se,
    upper = fit + 1.96 * se
  )


df_smooth_mother <- data.frame(
  x = smooth_data1[[2]]$x,
  fit = smooth_data1[[2]]$fit,
  se = smooth_data1[[2]]$se
) %>%
  mutate(
    lower = fit - 1.96 * se,
    upper = fit + 1.96 * se
  )


library(ggplot2)

anemia_plot_child <- ggplot(df_smooth, aes(x = x, y = fit)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  annotate("text", x = 10, y = 2, label = "(b)", size = 4, color = "black")+
  labs(x = "Child's age in months", y = "Effect of age on anemia") +
  ylim(-4, 2.5)+
  theme_bw()




anemia_plot_mother <- ggplot(df_smooth_mother, aes(x = x, y = fit)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  annotate("text", x = 10, y = 2, label = "(d)", size = 4, color = "black")+
  labs(x = "Mother's age in years", y = "Effect of age on anemia") +
  ylim(-4, 2.5)+
  theme_bw()

all_plot <- malaria_plot_child + anemia_plot_child+ malaria_plot_mother + anemia_plot_mother
all_plot 

# save 

ggsave(paste(save_tables, "smooth_effects.png", sep ="/"), plot = all_plot, width = 8, height = 8, dpi = 300)

#----------------------- predict the spatial effect ---------------------------
path_shape <- "/Users/rachidmuleia/Dropbox/PhD Material/OMS  Consultoria/Data Science/Consultoria OMS/Bases de Dados/SHAPEFILE"

shape_f <-  st_read(paste(path_shape,"I_CapacidadeAdaptativa.shp",sep = "/"), quiet = TRUE)

fit <- model2_final
# Create prediction grid

# Create a grid of spatial coordinates
lon_seq <- seq(min(kid_df$LONGNUM), max(kid_df$LONGNUM), length.out = 100)
lat_seq <- seq(min(kid_df$LATNUM), max(kid_df$LATNUM), length.out = 100)
grid <- expand.grid(LONGNUM = lon_seq, LATNUM = lat_seq)


path_map <- '/Users/rachidmuleia/Dropbox/INS/ARTIGO_SEX_DEBUT/DATA'
map <- st_read(paste(path_map, "MOZ-level_1.shp", sep = '/'))
map <- shape_f

# make a fishnet grid over the countries
grd <- st_make_grid(map, n = 200)
# visualize the grid
plot(grd)

# find which grid points intersect `polygons` (countries) 
# and create an index to subset from
index <- which(lengths(st_intersects(grd, map)) > 0)

# subset the grid to make a fishnet
fishnet <- grd[index]
grid <- st_coordinates(fishnet)

grid <- pred.loc1 <- as.data.frame(grid[, c('X', 'Y')])
names(grid) <- c("LONGNUM", "LATNUM")



grid <- grid |>
  mutate(
    #SEX_CHILD = "1_MALE",
    RELIGION  = unique(kid_df$RELIGION)[1],
    MOTHER_EDUCATION = unique(kid_df$MOTHER_EDUCATION)[1],
    MARITAL_STATUS = unique(kid_df$MARITAL_STATUS)[1],
    RESIDENCE = unique(kid_df$RESIDENCE)[1],
    REGION = unique(kid_df$REGION)[1],
    WEALTH_INDEX = unique(kid_df$WEALTH_INDEX)[1],
    #SOURCE_WATER = unique(kid_df$SOURCE_WATER)[1],
    #TYPE_SANITATION = unique(kid_df$TYPE_SANITATION)[1],
    MOTHER_OCCUPATION = unique(kid_df$MOTHER_OCCUPATION)[1],
    RECEIVED_VITAMIN_A = unique(kid_df$RECEIVED_VITAMIN_A)[1],
    HEAD_HOUSEHOLD = unique(kid_df$HEAD_HOUSEHOLD)[1],
    NUMBER_HOUSEHOLD = unique(kid_df$NUMBER_HOUSEHOLD)[1],
    NUMBER_CHILD_FIVE = unique(kid_df$NUMBER_CHILD_FIVE)[1],
    #ENERGY_SOURCE = unique(kid_df$ENERGY_SOURCE)[1],
    AGE_CHILD = mean(kid_df$AGE_CHILD,na.rm = TRUE ),
    MOTHERS_AGE = mean(kid_df$MOTHERS_AGE, na.rm = TRUE),
    #ACCESS_MEDIA = unique(kid_df$ACCESS_MEDIA)[1],
    #NUMBER_CHILD_FIVE = unique(kid_df$NUMBER_CHILD_FIVE)[1],
    BIRTH_ORDER = unique(kid_df$BIRTH_ORDER)[1],
    WATER_TREATMENT = unique(kid_df$WATER_TREATMENT)[1],
    ENOUGH_WATER = unique(kid_df$ENOUGH_WATER)[1],
    PLACE_WASH_HANDS = unique(kid_df$PLACE_WASH_HANDS)[1],
    #media_access_cat1 = unique(kid_df$media_access_cat)[1],
    #floor_material = unique(kid_df$floor_material)[2],
    #roof_material = unique(kid_df$roof_material)[1],
    #wall_material = unique(kid_df$wall_material)[1],
    had_fever = "1_YES",
    has_mosquito_net = "1_YES",
    stunted = "Not Stunted",
    underweight = "Not Underweight"
    
  )


# Add average or reference values for other covariates
for (var in setdiff(names(grid), c("LONGNUM", "LATNUM", "malaria_response", "anemia_response"))) {
  grid[[var]] <- mean(kid_df[[var]], na.rm = TRUE)
}




# Create an empty vector to store predictions
grid$pred_joint <- NA
grid$theta <- NA
grid$tau <- NA
# Loop over each row of the grid
for (i in 1:nrow(grid)) {
  copula_estimates <- copula.prob(model_cop_var, y1 = 1, y2 = 1, newdata = grid[i, ], theta = TRUE, tau = TRUE)
  grid$pred_joint[i] <- copula_estimates[1]
  grid$theta[i] <- copula_estimates[2]
  grid$tau[i] <- copula_estimates[3]
  
}

grid$pred_joint1 <- grid$pred_joint/100
# Add to your grid
grid$prob_malaria <- predict(model_cop_var, type = "response", newdata = grid, eq =1)
grid$prob_anemia <- predict(model_cop_var, type = "response", newdata = grid, eq =2)

logit(grid$prob_ari)

map1 <- ggplot(grid) + geom_tile(aes(x = LONGNUM, y = LATNUM, fill = prob_malaria))+
  geom_sf(data =st_cast(map, "MULTILINESTRING"),linewidth = 0.3)+
  coord_sf(lims_method = "geometry_bbox")+
  scale_fill_gradientn("resp",colours = terrain.colors(5))+
  #scale_fill_viridis_c()+
  annotate("text", x = 32, y = -12, label = "(a)", size = 4, color = "black")+
  theme_bw()+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.title = element_blank())



map2 <- ggplot(grid) + geom_tile(aes(x = LONGNUM, y = LATNUM, fill = prob_anemia))+
  geom_sf(data =st_cast(map, "MULTILINESTRING"), linewidth = 0.3)+
  coord_sf(lims_method = "geometry_bbox")+
  scale_fill_gradientn("resp",colours = terrain.colors(5))+
  annotate("text", x = 32, y = -12, label = "(b)", size = 4, color = "black")+
  theme_bw()+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.title = element_blank())


# plot the joint probability
map3 <- ggplot(grid) +
  geom_tile(aes(x = LONGNUM, y = LATNUM, fill = pred_joint1)) +
  geom_sf(data = st_cast(map, "MULTILINESTRING"),linewidth = 0.3) +
  coord_sf(lims_method = "geometry_bbox") +
  scale_fill_gradientn("resp",colours = terrain.colors(5))+
  annotate("text", x = 32, y = -12, label = "(c)", size = 4, color = "black")+
  theme_bw()+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.title = element_blank())




# Predict the kendal tau correlation 



# 2. Predict copula parameter (theta) over the grid

model_cop_var <- gjrm(
  formula = fl1,
  data = kid_df[,c("LONGNUM", "LATNUM",var_exp1,"malaria_response", "anemia_response", "AGE_CHILD", "MOTHERS_AGE")],weights = kid_df[, "WEIGHTS"],
  model = "B",
  copula = "G0",
  margins = c("probit", "probit")
)

theta_grid <- predict(model_cop_var, newdata = grid[, c("RESIDENCE", "LATNUM", "LONGNUM")], type = "theta", eq =3)

# 3. Compute Kendall's tau for Gumbel 
tau_grid <- 1-1/model_cop_var$theta

# 4. Visualize
library(ggplot2)
grid$dependece <- 2-2^(1/as.numeric(grid$theta))

# plot the tau parameter 
map4 <- ggplot(grid) +
  geom_tile(aes(x = LONGNUM, y = LATNUM, fill = tau)) +
  geom_sf(data = st_cast(map, "MULTILINESTRING"),linewidth = 0.3) +
  coord_sf(lims_method = "geometry_bbox") +
  scale_fill_gradientn("resp",colours = terrain.colors(5))+
  annotate("text", x = 32, y = -12, label = "(d)", size = 4, color = "black")+
  theme_bw()+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.title = element_blank())



# plot the tau parameter 
map5 <- ggplot(grid) +
  geom_tile(aes(x = LONGNUM, y = LATNUM, fill = dependece)) +
  geom_sf(data = st_cast(map, "MULTILINESTRING"),linewidth = 0.3) +
  coord_sf(lims_method = "geometry_bbox") +
  scale_fill_gradientn("resp",colours = terrain.colors(5))+
  annotate("text", x = 32, y = -12, label = "(d)", size = 4, color = "black")+
  theme_bw()+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.title = element_blank())



all_map <- (map1|map2)/ (map3 |map4)

ggsave(paste(save_tables, "maps_all.png", sep ="/"), plot = all_map, width = 8, height = 8,dpi = 300)


# mean spearman rho
theta_s <- fit1$theta.a
tau_s <- ((theta_s + 1) / (theta_s - 1)) - 
  (2 * theta_s * log(theta_s)) / ((theta_s - 1)^2)

theta_s <- 3.34
tau_s <- ((theta_s + 1) / (theta_s - 1)) - 
  (2 * theta_s * log(theta_s)) / ((theta_s - 1)^2)


theta_s <- 65.3
tau_s <- ((theta_s + 1) / (theta_s - 1)) - 
  (2 * theta_s * log(theta_s)) / ((theta_s - 1)^2)



library(ggplot2)
library(sf)
library(dplyr)

# Suponha que você tenha um data frame chamado `grid` com:
# LONGNUM, LATNUM, tau, malaria_prev, anemia_prev

# Criar uma variável de prevalência combinada
grid <- grid %>%
  mutate(combined_prev = prob_malaria + prob_anemia,
         highlight = ifelse(tau > 0.75 & prob_malaria < 0.05 & prob_anemia < 0.05, "Alta dependência, baixa ocorrência", NA))

# Mapa
ggplot(grid) +
  geom_tile(aes(x = LONGNUM, y = LATNUM, fill = tau)) +
  geom_point(aes(x = LONGNUM, y = LATNUM, size = combined_prev), color = "blue", alpha = 0.4) +
  geom_point(data = filter(grid, !is.na(highlight)),
             aes(x = LONGNUM, y = LATNUM),
             color = "green", size = 3, shape = 21, stroke = 1.5) +
  scale_fill_gradientn(name = "Tau", colours = terrain.colors(5)) +
  scale_size_continuous(name = "Prevalência combinada") +
  labs(title = "Mapa de Tau com Prevalência de Malária e Anemia",
       subtitle = "Regiões verdes têm alta dependência e baixa ocorrência") +
  theme_minimal()


