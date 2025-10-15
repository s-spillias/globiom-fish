## Map Variable Outputs

glob.map <- function(glob_df, var_id, colors_plot, item,
                     scen,
                     scen_baseline = "SCEN_DIET0_CAPTURE0",
                     facet = 'SCEN',
                     make_facet = TRUE,
                     save = TRUE,
                     title = '',
                     legend = "insert text here",
                     base_year = 2050,
                     year = max(as.numeric(glob_df$YEAR))){
if(missing(item)){
    item = "ALL"
}
  
  df_base <- glob_df %>% filter(YEAR == base_year, ALLSCEN3 == scen_baseline) %>% 
    dplyr::select(-YEAR, -ALLSCEN3) %>% rename("BASE_OUTPUT" = "OUTPUT")
  
  df_dat <- glob_df %>% ####### use 'globiom' or 'land_comp'
    left_join(df_base) %>% 
    filter(YEAR == year) %>% 
    filter(VAR_ID == var_id) %>% 
    filter(ALLSCEN3 == scen) %>% 
    rename("ITEM" = contains("ITEM"),
           "OUTPUT" = starts_with("OUTPUT"),
           "REGION" = contains("REGION"))  %>% 
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

  
  df_dat_ <- df_dat %>% 
    {if(length(item) == 1 && item == "ALL"){mutate(.,ITEM = "ALL")}else{mutate(.,ITEM = as.character(ITEM))}} %>% 
    group_by(REGION,ALLSCEN3,ITEM,VAR_UNIT, BASE_OUTPUT) %>%
    summarise(OUTPUT = sum(OUTPUT)) %>% 
    mutate(OUTPUT = OUTPUT - BASE_OUTPUT) %>% 
    rename("SCEN" = "ALLSCEN3") %>% 
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
# print("df_dat_")
# print(df_dat_)
df_dat_item <-  df_dat_ %>% 
  filter(ITEM %in% item) %>% 
  group_by(REGION,!!as.symbol(facet)) %>% 
  summarise(OUTPUT = sum(OUTPUT)) %>% ungroup() %>% 
  mutate(across(where(is.factor),as.character)) %>% 
  complete(REGION,!!as.symbol(facet))
print(df_dat_item)

 # for(i in 1:length(df_dat_$SCEN %>% unique)){
  #reg_name = names(globiom_df)[names(globiom_df) %>% str_detect(region %>% toupper)]
df_final <- reg_map %>% 
  mutate(Region37 = toupper(Region37)) %>% 
 # setNames(c("OBJECTID","REGION","Shape_Leng","Shape_Area")) %>% 
 left_join(df_dat_item,
           by = c("Region37" = "REGION")) 
print(df_final)
plot_ob <- df_final %>% 
  mutate(facet_ = !!as.symbol(facet)) %>% 
  ggplot() +
  geom_col(aes(x = Region37, y = OUTPUT)) +
  facet_wrap(~ facet_)

map_ob <- df_final %>% 
  tm_shape(projection = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m" ) +
  tm_polygons(col = "OUTPUT",
              title = legend,
              pal = colors_plot,
              border.col = "white",
              colorNA = 'grey80',
              style = "cont",
              midpoint = 0) +
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
    legend.outside.position = "right" , legend.outside.size = .15,
    frame = FALSE,
    legend.text.size = 1.3,
    frame.lwd = NA, panel.label.bg.color = NA,
   # legend.position = "right",
    main.title = paste0(title),
   # main.title.size = 2,
 #  asp = 2.1,
    # main.title = names(to_map)[i], 
    main.title.position = "left"
  ) +
  {if(make_facet)(tm_facets(facet, ncol = 2))}

if(save){
  tmap_save(tm = map_ob,
            filename = figure_folder(paste0(var_id," - ",item[1],"_",year,"_",scen,"_","map.png")), 
            dpi = 300, 
            width = 7, 
            height = 6)
  

}

return(lst(map_ob,df_final,df_dat))
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
