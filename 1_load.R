library(tidyverse)
library(here)
library(gdxrrw)
igdx("C:/GAMS/38")
#devtools::install_github("dill/beyonce")
library(beyonce)
library(tmap)
library(terra)
library(sf)

library(patchwork)

# Functions
# False for Condor run.
local =FALSE

reg_map <- st_read(here("Reg37/Reg37_aggregate.shp"))

## Load Functions
source(here("_Analysis/v.compare.R"))
source(here("_Analysis/glob.plot.R")) ### Creates bar graph of individual years. Compares to baseline scenario. 

source(here("_Analysis/glob.map.2020.R"))
source(here("_Analysis/glob.map.R"))
source(here("_Analysis/facet.plot.R")) ### Aggregates and explores items as line graphs over time. Compares scenarios.



figure_folder <- function(x){
  path = file.path(getwd(),'Plots',
                   x)
  print(path)
  return(path)
}



## Load GLOBIOM Results
#change this to 'Globiom_Output' Directory if not on austria server.
globiom_path <- here("Model/gdx/")
experiment_name = "output_trunk_fish_"

a6_file <- list.files(globiom_path, pattern = "a6_r1", full.names = TRUE)

version_all <- list.files(globiom_path) %>% #str_remove("output_trunk_fish_") %>% 
  lapply(function(x) str_split(x, "[\\.,\\_]")) %>% unlist() %>% as.numeric() %>% suppressWarnings() %>% sort() %>% unique %>% rev

recent_version <- version_all %>% max()



######

version_index <- recent_version  #306 # input recent_version or numerical index
#version_index <- 1446#1356#1255#recent_version #306 # input recent_version or numerical index
# version 1446,1320,1255 is publishable. 
######

# Sets

fish_final = c("DMRSF","SALMF","TUNAF","SHRIF","MLSCF",'MARNF','FRSHF','CEPHF','PELGF','CRSTF')
fish_raw = c("DMRS","FLTR","MENH","OTHM","OTHP","SALM","TUNA","OTHF","SHRI","OTHC","MLSC")

fmfo = c("FSHO","FSHM")

message("Loading GLOBIOM results from run number: ",version_index*(!local))

globiom_files <- list.files(globiom_path, pattern = paste0(experiment_name,version_index), full.names = TRUE)
globiom_files = if(local){
  file.path(globiom_path %>% str_remove("/gdx"),"output/Trunk_Fish_Condor.gdx")
}else{
  globiom_files[!str_detect(globiom_files,"merged")]
  }
# paste0(experiment_name,version_index,".gdx")
#globiom_files <- file.path(globiom_path,globiom_filename)

tidy_globiom <- function(var_name){
  lapply(globiom_files, function(x) {rgdx.param(x, var_name)}) %>% 
    bind_rows() %>% 
    droplevels() %>% 
    rename("Year"= contains("Year")) %>% 
    rename("Region"= contains("Region")) %>%
    rename("Product"= contains("Product")) %>%
    mutate(across(-!!as.symbol(var_name), toupper)) %>% 
    setNames(toupper(names(.)))
}

tidy_globiom("OUTPUT") %>%  
  rename("ITEM" = ".I4") %>% 
  saveRDS(here("_Analysis/output.RDS"))
# 

tidy_globiom("OUTPUT_AG") %>% 
  saveRDS(here("_Analysis/output_ag.RDS"))
# # 
# tidy_globiom("DemQuantity_Compare") %>%
#   dplyr::select(-REPORTITEM) %>% 
#   saveRDS(here("_Analysis/dem_compare.RDS"))

    tidy_globiom("FISH_AQUAFEED_QUANTITY_COMPARE") 
  

tidy_globiom("FISH_VAR_COMPARE") %>% 
  saveRDS(here("_Analysis/fish_compare.RDS"))

fish_var <- tidy_globiom("FISH_VAR_COMPARE")

tidy_globiom("FISH_CAPACITY_COMPARE") %>% 
  saveRDS(here("_Analysis/fish_capacity_compare.RDS"))

fish_capacity <- tidy_globiom("FISH_CAPACITY_COMPARE")

output <- readRDS(here("_Analysis/output.RDS"))

output_ag <- readRDS(here("_Analysis/output_ag.RDS"))

dem_compare <- readRDS(here("_Analysis/dem_compare.RDS"))

process_compare <- tidy_globiom("Process_Compare2")

fish_compare <- readRDS(here("_Analysis/fish_compare.RDS"))

fish_aquafeed_compare <- tidy_globiom('FISH_AQUAFEED_QUANTITY_COMPARE')

cap_scen_names <- c("BAU","MSY","MEY","AQUA", "FOLU_MSY", "FOLU_MSY_AQUA")

DData <-  tidy_globiom("DData_Compare")
DemQuantity <- tidy_globiom("DemQuantity_Compare")

region_map <- a6_file %>% rgdx.set("REGION37_COUNTRY_MAP") 

# fish_capacity <- tidy_globiom("FISH_VAR_COMPARE") %>% 
#   saveRDS(here("_Analysis/fish_var_compare.RDS"))

trade <- tidy_globiom("Trade_Compare")
fish_demand <- rgdx.param(a6_file, "DData_Compare") %>% 
  filter(CURVE == "Quantity") %>% 
  dplyr::select(-CURVE) %>% 
  filter(toupper(ALLPRODUCT) %in% fish_final)
fish_demand %>% 
  saveRDS(here("_Analysis/fish_demand.RDS"))

#### Process Compare

process_compare <- rgdx.param(a6_file, "Process_Compare2") %>% 
  saveRDS(here("_Analysis/process_compare.RDS"))




# Name Conversions

# Define scen_include function
scen_include <- function(x){
  raw_scen_names <- c("SCEN_DIET-10_CULTURE0_CAPTURE-10","SCEN_DIET0_CULTURE0_CAPTURE0","SCEN_DIET+10_CULTURE+50_CAPTURE+10")
  x %>% filter(ALLSCEN3 %in% raw_scen_names)
}

scen_pub_names <- matrix(ncol = 2,
                         byrow =TRUE,
                         data = c(
                           'SCEN_REF_2020',"Reference - 2020",
                           'SCEN_DIET-10_CULTURE0_CAPTURE-10', 'Diet -10 NonFed 0 Capture -10',
                            'SCEN_DIET-10_CULTURE0_CAPTURE0', 'Diet -10 NonFed 0 Capture 0',
                           'SCEN_DIET-10_CULTURE0_CAPTURE+10', 'Diet -10 NonFed 0 Capture +10',
                           'SCEN_DIET-10_CULTURE+50_CAPTURE-10', 'Diet -10 NonFed +50 Capture -10',
                           'SCEN_DIET-10_CULTURE+50_CAPTURE0', 'Diet -10 NonFed +50 Capture 0',
                           'SCEN_DIET-10_CULTURE+50_CAPTURE+10', 'Diet -10 NonFed +50 Capture +10',
                           'SCEN_DIET0_CULTURE0_CAPTURE-10', 'Diet 0 NonFed 0 Capture -10',
                           'SCEN_DIET0_CULTURE0_CAPTURE0', 'Diet 0 NonFed 0 Capture 0',
                           'SCEN_DIET0_CULTURE0_CAPTURE+10', 'Diet 0 NonFed 0 Capture +10',
                           'SCEN_DIET0_CULTURE+50_CAPTURE-10', 'Diet 0 NonFed +50 Capture -10',
                           'SCEN_DIET0_CULTURE+50_CAPTURE0', 'Diet 0 NonFed +50 Capture 0',
                           'SCEN_DIET0_CULTURE+50_CAPTURE+10', 'Diet 0 NonFed +50 Capture +10',
                           'SCEN_DIET+10_CULTURE0_CAPTURE-10', 'Diet +10 NonFed 0 Capture -10',
                           'SCEN_DIET+10_CULTURE0_CAPTURE0', 'Diet +10 NonFed 0 Capture 0',
                           'SCEN_DIET+10_CULTURE0_CAPTURE+10', 'Diet +10 NonFed 0 Capture +10',
                           'SCEN_DIET+10_CULTURE+50_CAPTURE-10', 'Diet +10 NonFed +50 Capture -10',
                           'SCEN_DIET+10_CULTURE+50_CAPTURE0', 'Diet +10 NonFed +50 Capture 0',
                           'SCEN_DIET+10_CULTURE+50_CAPTURE+10', 'Diet +10 NonFed +50 Capture +10'
                           # 'SCEN_DIET+10_CAPTURE+10', 'Diet + 10 Capture + 10',
                           # 'SCEN_DIET0_CAPTURE+10', 'Diet 0 Capture + 10',
                           # 'SCEN_DIET0_CAPTURE-10', 'Diet 0 Capture - 10',
                           # 'SCEN_DIET0_CAPTURE-20', 'Diet 0 Capture - 20',
                           # 'SCEN_DIET+10_CAPTURE-20', 'Diet + 10 Capture - 20',
                           # "SCEN_DIET+10_CULTURE+200_CAPTURE-10", 'Scen Diet + 10 NonFed + 200 Capture - 10',
                           # 'SCEN_DIET0_CULTURE0', 'Scen Diet 0 Culture 0',
                           # 'SCEN_DIET0_CULTURE+50', 'Scen Diet 0 Culture + 50',
                           # 'SCEN_DIET0_CULTURE+100', 'Scen Diet 0 Culture + 100',
                           # 'SCEN_DIET0_CULTURE+200', 'Scen Diet 0 Culture + 200',
                           # 'SCEN_DIET0_CULTURE+800', 'Scen Diet 0 Culture + 800',
                           # 'SCEN_DIET0_CULTURE+400', 'Scen Diet 0 Culture + 400',
                           # 'SCEN_DIET+10_CULTURE+10','Diet + 10 Culture +10 Capture 0',
                           # 'SCEN_DIET+10_CULTURE+10_CAPTURE-10','Diet + 10 Culture +10 Capture -10',
                           # 'SCEN_DIET0_CULTURE+10_CAPTURE-10','Diet 0 Culture +10 Capture -10',
                           # 'SCEN_BAU'  ,        'Business as Usual' ,
                           # 'SCEN_DIET_+05'  , 'Diets - More Fish',
                           # 'SCEN_DIET_LESS'  , 'Diets - Less Fish',
                           # 'SCEN_DIET_+10_NOSUB', 'Diets - More Fish w/o substitution',
                           # 'SCEN_AQFD_FMFO'  , 'Aquafeed - More FMFO',
                           # 'SCEN_AQFD_CROP'  , 'Aquafeed - More Crops',
                           # 'SCEN_CAP_MARI', 'Production - More Mariculture',
                           # 'SCEN_FCE_UP10', 'Technology - Feeding Efficiency +10%',
                           # 'SCEN_FCE_UP20', 'Technology - Feeding Efficiency +20%',
                           # 'SCEN_CAP_CULT', 'Production - More Cultivation',
                           # 'SCEN_CAP_WILD', 'Production - More Catch',
                           # 'SCEN_DIET_-05'  , 'Diets - -5 Fish',
                           # 'SCEN_DIET_+10'  , 'Diets - +10 Fish',
                           # 'SCEN_DIET_+20'  , 'Diets - +20 Fish',
                           # 'SCEN_DIET_-10'  , 'Diets - -10 Fish',
                           # 'SCEN_DIET_+50'  , 'Diets - +50 Fish',
                           # 'SCEN_DIET_+50_NOSUB'  , 'Diets - +50 Fish No Sub',
                           # 'SCEN_ALL_FISH'  , 'ALL FISH',
                           # 'SCEN_FCE_DOUBLE'  , 'Aquafeed - Feeding Efficiency x2',
                           # 'SCEN_DIET_-20'  , 'Diets - -20 Fish',
                           # 'SCEN_DIET_FREEZE', 'Diets - 2020 Fish',
                         )) %>% 
  as_tibble() %>% 
  setNames(c("ALLSCEN3","PUBSCEN")) %>% 
  mutate(across(PUBSCEN, factor))

pub_scen <- function(input_string) {
  # Convert the string to lower case and then capitalize the first letter of each word
  title_case_string <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", tolower(input_string), perl = TRUE)
  
  # Replace underscores with spaces
  title_case_string <- gsub("_", " ", title_case_string)
  
  # Replace hyphens and plus signs with space before and after
  title_case_string <- gsub("([-+])", " \\1 ", title_case_string)
  
  # Replace sequences of numbers with space-separated versions (optional)
  title_case_string <- gsub("([0-9])([A-Za-z])", "\\1 \\2", title_case_string)
  
  # Trim any leading or trailing whitespace and condense multiple spaces
  title_case_string <- trimws(gsub("\\s+", " ", title_case_string))
  
  return(title_case_string)
}



crop_pub_names <- matrix(ncol = 2,
                         byrow =TRUE,
                         data = c(
                           'CORN', 'Corn',
                           'RAPE', "Rapeseed",
                           'SOYA', 'Soy',
                           'WHEA', 'Wheat',
                           'FSHM', 'Fishmeal',
                           'FSHO', 'Fish Oil'
                         )) %>% 
  as_tibble() %>% 
  setNames(c("PRODUCT","PUBFEED")) %>% 
  mutate(PUBFEED = factor(PUBFEED, levels = PUBFEED[c(1:6)]))

fish_pub_names <- matrix(ncol = 2,
                         byrow =TRUE,
                         data = c(
                           'CEPHF', 'Cephalopods',
                           'CRSTF', "Other Crustaceans",
                           'DMRSF', 'Demersal',
                           'FRSHF', 'Other Freshwater',
                           'MARNF', 'Other Marine',
                           'MLSCF', 'Other Molluscs',
                           'PELGF', 'Other Pelagic',
                           'SHRIF', 'Shrimp',
                           'TUNAF', 'Tuna',
                           'SALMF', 'Salmon'
                         )) %>% 
  as_tibble() %>% 
  setNames(c("PRODUCT","Fish")) %>% 
  mutate(Fish = factor(Fish, levels = Fish#[c(1:6)]
                       ))

country_names <- read.csv(here("Data/Fish/Data/OtherData/country_name_map.csv"), fileEncoding="UTF-8-BOM") %>% 
  as_tibble() %>% 
  mutate(across(everything(),trimws)) %>% 
  mutate(across(everything(),factor))

output_2020 <- output %>% bind_rows(output %>% filter(ALLSCEN3 == 'SCEN_DIET0_CULTURE0_CAPTURE0') %>% 
                                      filter(YEAR %in% c(2020)) %>% dplyr::select(-YEAR) %>% 
                                      mutate(ALLSCEN3 = 'SCEN_REF_2020') %>% 
                                      expand_grid(YEAR = c('2000','2010','2020','2030','2040','2050')))

output_ag_2020 <- output_ag %>% bind_rows(output_ag %>% filter(ALLSCEN3 == 'SCEN_DIET0_CULTURE0_CAPTURE0') %>% 
                                             filter(YEAR %in% c(2020)) %>% dplyr::select(-YEAR) %>% 
                                             mutate(ALLSCEN3 = 'SCEN_REF_2020') %>% 
                                             expand_grid(YEAR = c('2000','2010','2020','2030','2040','2050')))


fish_var_2020 <- fish_var %>% bind_rows(fish_var %>% filter(ALLSCEN3 == 'SCEN_DIET0_CULTURE0_CAPTURE0') %>% 
                                          filter(YEAR %in% c(2020)) %>% dplyr::select(-YEAR) %>% 
                                          mutate(ALLSCEN3 = 'SCEN_REF_2020') %>% 
                                          expand_grid(YEAR = c('2000','2010','2020','2030','2040','2050')))



production_df_ <- output_ag_2020 %>% filter(VAR_ID == 'PROD',ITEM_AG %in% c('CRP','LSP',VAR_UNIT == '1000 T')) %>% 
  group_by(ITEM_AG,ALLSCEN3,YEAR) %>% 
  summarise(output = sum(OUTPUT_AG)) %>% 
  rename('TYPE' = 'ITEM_AG') %>% 
  bind_rows(
    fish_var_2020  %>% #  filter(ALLSCEN3 %in% c("SCEN_BAU", "SCEN_DIET_+05"#, "SCEN_CAP_WILD"           )) %>% 
      mutate(Inland = ifelse(as.numeric(FISHREG) < 10 | FISHSYST == "AQUA_F","Inland","Marine") ) %>% ################ FFFFIIIIXXX THIS
      mutate(GROUP = case_when(FISHSPEC %in% c("DMRS","MARN","SALM","TUNA","FRSH","PELG") ~ "Finfish",
                               FISHSPEC %in% c("SHRI","CRST") ~ "Crustaceans",
                               FISHSPEC %in% c("MLSC","CEPH") ~ "Molluscs"),
             TYPE = ifelse(FISHSYST == 'CATCH',"CATCH","CULTURE")) %>%
      group_by(ALLSCEN3, #TYPE, 
               TYPE, #Inland,
               YEAR#, GROUP
      ) %>% {products <<- .}  %>% 
      summarise(output = sum(FISH_VAR_COMPARE)) 
  ) %>% 
  left_join(scen_pub_names) %>%  ungroup()   %>% 
  mutate(DIET = ifelse(str_detect(ALLSCEN3,'DIET0'),'Baseline','Fish+10'),
         CAPACITY = case_when(
           str_detect(ALLSCEN3,'CAPTURE\\+') ~ 'Catch+10',
           str_detect(ALLSCEN3,'CAPTURE0') ~ 'Baseline',
           str_detect(ALLSCEN3,'CAPTURE\\-1') ~ 'Catch-10',
           str_detect(ALLSCEN3,'CAPTURE\\-2') ~ 'Catch-20',
           str_detect(ALLSCEN3,'CAPTURE\\-3') ~ 'Catch-30',
           str_detect(ALLSCEN3,'CULTURE0') ~ 'CEILING',
           str_detect(ALLSCEN3,'CULTURE\\+5') ~ 'NonFed+50',
           str_detect(ALLSCEN3,'CULTURE\\+10') ~ 'NonFed+100',
           str_detect(ALLSCEN3,'CULTURE\\+40') ~ 'NonFed+400',
           str_detect(ALLSCEN3,'CULTURE\\+20') ~ 'NonFed+200',
           str_detect(ALLSCEN3,'CULTURE\\+80') ~ 'NonFed+800',
           TRUE ~ "ERROR"
         )
  )

# scen_2020 <- products %>% filter(ALLSCEN3 == 'SCEN_DIET0_CAPTURE0') %>% 
#   filter(YEAR %in% c(2020)) %>% group_by(YEAR,GROUP,TYPE) %>% summarise(output = sum(FISH_VAR_COMPARE)) %>% 
#   mutate(ALLSCEN3 = 'REF_2020') 
pWASTE_ <- matrix(byrow = TRUE,data = c(  ## Other than CEPH, values are from Guerard, Sellos, Le Gal (2005) Fish and Shellfish Upgrading, Traceability
  'FRSHF'                    , 0.50,
  'CARPF'                    , 0.50,
  'SALMF'                    , 0.50,
  'DMRSF'                    , 0.50,
  'TUNAF'                    , 0.50,
  'PELGF'                    , 0.50,
  'BAITF'                    , 0.50,    ## Imputed from PELG
  'MARNF'                    , 0.50,
  'SHRIF'                    , 0.40,
  'CRSTF'                    , 0.70,
  'MLSCF'                    , 0.70,   ### No Source.
  'CEPHF'                    , 0.35     ## Koueta et al 2014 : https://link.springer.com/chapter/10.1007/978-94-017-8648-5_8
),ncol = 2) %>% 
  dplyr::as_tibble() %>% 
  stats::setNames(c("PRODUCT", "ITEMw")) %>% 
  dplyr::mutate(ITEMw = as.numeric(ITEMw))


tidy_globiom("DData_Compare") %>% 
  scen_include %>% 
  filter(PRODUCT %in% fish_final) %>% 
  # left_join(pWASTE_) %>% 
  mutate(value = DDATA_COMPARE,#/(1-ITEMw),
         FISHSPEC = str_sub(PRODUCT, 1, -2)) %>% ### INVERT BACK TO LIVE WEIGHT
  group_by(FISHSPEC,
           YEAR,ALLSCEN3) %>% 
  filter(CURVE == 'QUANTITY') %>% 
  summarise(ddata = value %>% sum) %>% 
  left_join(
tidy_globiom("FISH_VAR_COMPARE") %>% 
              group_by(FISHSPEC,YEAR) %>% 
  summarise(fish_var = sum(FISH_VAR_COMPARE)) %>% 
  left_join(pWASTE_ %>% mutate(FISHSPEC = PRODUCT %>% str_remove("F"))) %>% 
  dplyr::select(-PRODUCT) %>% 
  mutate(final_fish_var = fish_var*(1-ITEMw))) %>% 
  pivot_longer(-c(FISHSPEC,YEAR,ALLSCEN3)) %>% 
 
  bind_rows(
tidy_globiom("DemQuantity_Compare") %>% filter(PRODUCT %in% fish_final) %>% 
  # left_join(pWASTE_) %>% 
  mutate(value =DEMQUANTITY_COMPARE,#/(1-ITEMw),
         FISHSPEC = str_sub(PRODUCT, 1, -2)) %>% 
  group_by(FISHSPEC,
           YEAR,ALLSCEN3) %>% 
  #  filter(CURVE == 'QUANTITY') %>% 
  summarise(demQ = value %>% sum) %>% 
  pivot_longer(demQ)) %>% 
  bind_rows(
    tidy_globiom("FISH_CAPACITY_COMPARE") %>%
      group_by(FISHSPEC,YEAR,ALLSCEN3) %>%
      summarise(fish_capacity = sum(FISH_CAPACITY_COMPARE)) %>%
      left_join(pWASTE_ %>% mutate(FISHSPEC = PRODUCT %>% str_remove("F"))) %>%
      mutate(final_fish_capacity = fish_capacity*(1-ITEMw)) %>%
      dplyr::select(-PRODUCT) %>% 
  pivot_longer(-c(FISHSPEC,YEAR,ALLSCEN3))
  ) %>% 
  filter(name != 'fish_var', name != 'ITEMw', ALLSCEN3 == 'SCEN_DIET-10_CULTURE0_CAPTURE-10') %>% 
  
  ggplot()+
  geom_line(aes(x = YEAR, y = value, group = name,color = name) )+
  geom_point(aes(x = YEAR, y = value, group = name,color = name) )+
  
  facet_wrap(~FISHSPEC, scales = 'free')+
  theme_classic()

tidy_globiom("FISH_CAPACITY_COMPARE") %>% 
  group_by(FISHFEEDSYST,ALLSCEN3,YEAR) %>% 
  summarise(output = sum(FISH_CAPACITY_COMPARE)) %>% 
  left_join(scen_pub_names) %>% 
  # pub_scen() %>% 
  ggplot() + 
  geom_line(aes(x = YEAR,
                y = output/1000,
                # linetype = Source,
                group = PUBSCEN,
                col = PUBSCEN
  )) +
  geom_point(aes(x = YEAR,
                 y = output/1000,
                 # linetype = Source,
                 group =PUBSCEN,
                 col = PUBSCEN
  )) +
  theme_classic() +
  #facet_grid(TYPE~Inland) +
  # scale_y_log10() +
  # scale_y_continuous(breaks = c((-4:4)*20)) +
  scale_color_manual(values = beyonce_palette(40, type = 'continuous', n =11)[-c(4,5,6)],
                     name = 'Scenario') +
  scale_linetype_manual(values = c("solid", "dotted")) +
  # geom_hline(yintercept = 0) +
  ylab('Production (Mt)') +
  xlab(NULL) +
  facet_wrap(~FISHFEEDSYST,ncol = 1,scales = 'free')

# fish_capacity %>%  group_by(FISHSPEC,ALLYEAR) %>% 
#   summarise(fish_capacity = sum(value))
# tidy_globiom("FISH_CAPACITY_COMPARE") %>% group_by(FISHSPEC,YEAR) %>% 
#   summarise(fish_capacity = sum(FISH_CAPACITY_COMPARE))
fish_capacity %>% 
   dplyr::group_by(FISHSPEC,ALLYEAR) %>% 
  dplyr::summarise(supply = sum(value, na.rm = TRUE)) %>% 
  mutate(YEAR = ALLYEAR) %>% dplyr::select(-ALLYEAR) %>% 
  dplyr::left_join(
    fish_demand %>% 
      ### INVERT BACK TO LIVE WEIGHT
      dplyr::filter(CURVE == "Quantity") %>% 
      dplyr::rename(FISHSPEC = ALLPRODUCT) %>%
      dplyr::mutate(FISHSPEC = stringr::str_remove(FISHSPEC,"f"),
                    YEAR = ALLYEAR) %>% 
      dplyr::group_by(FISHSPEC,YEAR) %>% 
      
      dplyr::summarise(demand = sum(value, na.rm = TRUE)),
    by = join_by(FISHSPEC,YEAR)) %>% 
    dplyr::select(FISHSPEC,YEAR,demand,supply) %>% 
   left_join(pWASTE_) %>% mutate(final_fish = supply*(1-ITEMw)) %>% arrange(YEAR) %>% 
  filter(YEAR == 2010)
  

art_vars <- tidy_globiom("ARTVAR_COMPARE")

if(art_vars %>% nrow == 0){
  message("Congratulations, no artificial variables :)")
}else{
  message("Warning: Artificial variables are present.")
  message("Here they are!")
  print(art_vars)
}

region_map

trade_compare <- trade %>% mutate(REGION1 = tolower(REGION1),REGION2 = tolower(REGION2)) %>% filter(PRODUCT %in% fish_final) %>% 
  mutate(TRADE_COMPARE = ifelse(REGION1=='world',TRADE_COMPARE,-1*TRADE_COMPARE)) %>% 
  mutate(REGION = ifelse(REGION1 == 'world',REGION2,REGION1))
# 
# look = fish_var %>% mutate(ALLCOUNTRY = tolower(ALLCOUNTRY)) %>%
#   left_join(region_map %>% mutate(ALLCOUNTRY = tolower(ALLCOUNTRY),REGION = tolower(ANYREGION)), by = 'ALLCOUNTRY') %>% 
#   mutate(PRODUCT = paste0(FISHSPEC,'F')) %>% 
#   group_by(REGION,YEAR,PRODUCT) %>% 
#   
#   summarise(value_final = 0.5*sum(FISH_VAR_COMPARE)) %>% 
#   left_join(
#     DData %>% mutate(REGION = tolower(REGION)) %>% filter(PRODUCT %in% fish_final, CURVE == "QUANTITY"),
#     by = c("YEAR","PRODUCT","REGION")
#   ) %>% 
#   left_join(
#    DemQuantity %>% mutate(REGION = tolower(REGION)) %>% filter(PRODUCT %in% fish_final) %>% distinct(DEMQUANTITY_COMPARE,YEAR,REGION,PRODUCT),
#     by = c("YEAR","PRODUCT","REGION")
#   ) %>% 
#   left_join(trade_compare %>% distinct(TRADE_COMPARE,YEAR,REGION,PRODUCT)) %>% 
#   rowwise() %>% 
#   mutate(surplus = sum(value_final, -1* DDATA_COMPARE, TRADE_COMPARE,na.rm = TRUE)) %>% 
#   # filter(PRODUCT == "TUNAF", YEAR == 2020) %>% 
#   ungroup() %>% 
#   group_by(YEsave(list = ls(), file = "all_objects.RData")AR) %>% summarise(tot = sum(surplus,na.rm = TRUE))
save(list = ls(), file = paste0(version_index,".RData"))
