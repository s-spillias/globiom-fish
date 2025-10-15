## Map Variable Outputs

glob.map.2020 <- function(glob_df, var_id, colors_plot, item,
                          scen_baseline = "SCEN_BAU", 
                          facet = 'SCEN',
                          save = TRUE,
                          year = max(as.numeric(glob_df$YEAR))){
  year <<- year
  
  df_dat <- glob_df %>% ####### use 'globiom' or 'land_comp'
       filter(VAR_ID == var_id) %>% 
    rename("ITEM" = contains("ITEM"),
           "OUTPUT" = contains("OUTPUT"),
           "REGION" = contains("REGION"),
           "SCEN" = "ALLSCEN3")  %>% 
    {
      if(var_id == "EMIS"){
        filter(.,ITEM %in% c("AFFR","LUC","LSP","CRP"))
      } else {
        if(var_id == "WATR" | str_detect(var_id, "FRT")){
          mutate(., ITEM = case_when(ITEM == "CER" ~ "Cereals",
                                     ITEM == "OCR" ~ "Other Crops",
                                     ITEM == "OSD" ~ "Oilseeds",
                                     ITEM == "SGC" ~ "Sugarcane",
                                     TRUE ~ ""))
        } else {.}
      }
    } %>%  
    filter(REGION != "WORLD") 
  
  df_2020 <- df_dat %>% filter(YEAR == "2020") %>% dplyr::select(-YEAR) %>% rename("BASE_OUTPUT" = "OUTPUT")
  
  df_dat_ <- df_dat %>% 
    left_join(df_2020, by = names(df_2020)[!(names(df_2020) %in% c("BASE_OUTPUT"))]) %>% #group_by(REGION,ALLSCEN3,ITEM,YEAR) %>% 
  #  summarise(OUTPUT = sum(OUTPUT)) %>% 
    filter(YEAR == year) %>% 
    mutate(OUTPUT = OUTPUT - BASE_OUTPUT) %>% 
    # pivot_wider(names_from = ALLSCEN3, values_from = OUTPUT) %>% 
    # mutate(across(.cols = starts_with("SCEN"), .fns = list(
    #   delta_Baseline = function(x) {(x-!!as.symbol(scen_baseline))}),
    #   .names = "{.fn}.{.col}" )) %>%
    # dplyr::select(!starts_with("SCEN")) %>%
    # pivot_longer(cols = starts_with("delta_Baseline"), values_to = "delta_Baseline", names_to = "SCEN") %>%
    # mutate(SCEN = str_remove(SCEN, "delta_Baseline.") ) %>%
    # rename("OUTPUT"="delta_Baseline") %>%
    # filter(SCEN != scen_baseline) %>% 
  #  filter(OUTPUT != 0) %>% 
    mutate(ITEM = factor(ITEM, levels = unique(ITEM))) %>% 
    mutate(SCEN = factor(SCEN, levels = unique(SCEN)))
  
  unit <- df_dat$VAR_UNIT %>% unique()
  
  print(df_dat_$ITEM %>% unique)
  
  pick_col = colors_plot[1:length(df_dat_$ITEM %>% unique)] %>% 
    setNames(df_dat_$ITEM %>% unique)#%>% 
  
  pal = scale_fill_manual(name = paste0(var_id," Type"),
                          values = pick_col, 
                          guide = guide_legend(reverse = FALSE))
  
 # df_dat_ <<- df_dat_
map_face <- list()

df_dat_item <-  df_dat_ %>% 
  filter(ITEM %in% item) %>% 
  group_by(across(-ITEM)) %>% 
  summarise(OUTPUT = sum(OUTPUT))

print(df_dat_item)
 # for(i in 1:length(df_dat_$SCEN %>% unique)){
  #reg_name = names(globiom_df)[names(globiom_df) %>% str_detect(region %>% toupper)]
reg_map %>% 
  mutate(Region37 = toupper(Region37)) %>% 
 # setNames(c("OBJECTID","REGION","Shape_Leng","Shape_Area")) %>% 
 left_join(df_dat_item,
           by = c("Region37" = "REGION")) %>% 
  tm_shape(projection = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m" ) +
  tm_polygons(col = "OUTPUT",
              pal = colors_plot,
              border.col = "white",
              style = "cont",
              midpoint = 0) +
  tm_facets("SCEN", ncol = 2) +
  # tm_shape(sel_com_sp,
  #          # raster.downsample = FALSE,
  #          projection = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m" ,raster.warp = FALSE)+
  # tm_fill("Assumption",#col = file_name,
  #         style = "cat",
  #         palette = pal,
  #         title = "",
  #         labels = if(prio) {c("Minimum","Mean","Maximum")}else{c("Feasible","Suitable")},
  #         legend.is.portrait = FALSE) +
  tm_layout(
   # legend.position = c("bottom"),
    frame = FALSE,
    legend.text.size = 1.3,
    frame.lwd = NA, panel.label.bg.color = NA,
    legend.outside.position = "bottom",
    main.title = paste0(var_id, " - ",item, " - ",unit),
    main.title.size = 2,
    # main.title = names(to_map)[i], 
    main.title.position = "left"
  )  
#}

# tmap_arrange(map_face[[1]], 
#                map_face[[2]],
#                map_face[[3]],
#                map_face[[4]],
#                map_face[[5]],
#              ncol = 2
#                )
}

# region_mapping <- 
#   tibble(REGION = c("MIDEASTNORTHAFR" ,  "SUBSAHARANAFR"   ,  "FORMERSOVIETUNION", "LATINAMERICACARIB", "NORTHAMERICA"   ,  
#        "SOUTHASIA",         "EUROPE"       ,     "OCEANIA"      ,     "EASTERNASIA"   ,    "SOUTHEASTASIA" ),
#        GGI = c(
#          "MEN", "SSA","CIS"  ,"LAM","NAM", "SAS" ,"EUR","OCE","EAS", "SEA"       
#        ))

#glob.map(glob_df = output, var_id = "CALO", item = "BVMEAT", colors_plot = rev(beyonce_palette(33)[-2]) )
