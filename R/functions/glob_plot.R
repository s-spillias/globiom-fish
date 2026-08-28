# glob.plot() — stacked delta-from-baseline bar panels for Figure 4.
#
# For a given indicator (var_id: LAND, EMIS, WATR, FRTN) it sums each ITEM by
# scenario, subtracts the baseline scenario, and draws a horizontal stacked bar
# of the change from baseline. Sourced by R/make_figures.R, which supplies the
# global objects scen_pub_names and figure_folder().

  glob.plot <- function(glob_df,
                        var_id, 
                        colors_plot,
                        scen_baseline = "SCEN_BAU", 
                        year = max(as.numeric(glob_df$YEAR)),
                        title = "Insert Title Here",
                        save = TRUE){

    
       df_dat <- glob_df %>% ####### use 'globiom' or 'land_comp'
  filter(YEAR == year) %>% 
  filter(VAR_ID == var_id) %>% 
      rename("ITEM" = contains("ITEM"),
             "OUTPUT" = contains("OUTPUT"))  %>% 
      {
        if(var_id == "EMIS"){
          filter(.,ITEM %in% c("AFFR","LUC","LSP","CRP"))
        } else {
          if(var_id == "WATR" | str_detect(var_id, "FRT")){
           # mutate(., ITEM = case_when(ITEM == "CER" ~ "Cereals",
           #                            ITEM == "OCR" ~ "Other Crops",
           #                            ITEM == "OSD" ~ "Oilseeds",
           #                            ITEM == "SGC" ~ "Sugarcane",
           #                            TRUE ~ ""))
            mutate(., ITEM = var_id)
          } else {.}
          }
      } %>%  
      filter(REGION != "World") %>% 
    filter(ITEM != "")
    
  df_dat_ <- df_dat %>% group_by(ALLSCEN3,ITEM) %>% 
  summarise(OUTPUT = sum(OUTPUT)) %>% 
  pivot_wider(names_from = ALLSCEN3, values_from = OUTPUT) %>% 
  mutate(across(.cols = starts_with("SCEN"), .fns = list(
    delta_Baseline = function(x) {(x-!!as.symbol(scen_baseline))}),
    .names = "{.fn}.{.col}" )) %>%
  dplyr::select(!starts_with("SCEN")) %>%
  pivot_longer(cols = starts_with("delta_Baseline"), values_to = "delta_Baseline", names_to = "SCEN") %>%
  mutate(SCEN = str_remove(SCEN, "delta_Baseline.") ) %>%
  rename("OUTPUT"="delta_Baseline") %>% 
  filter(SCEN != scen_baseline) %>% 
    left_join(scen_pub_names %>% rename("SCEN" = "ALLSCEN3")) %>% 
    mutate(SCEN = PUBSCEN) %>% 
  filter(OUTPUT != 0) %>% 
  mutate(ITEM = factor(ITEM, levels = unique(ITEM))) %>% 
    mutate(SCEN = factor(SCEN, levels = unique(SCEN) %>% rev))

  unit <- df_dat$VAR_UNIT %>% unique()
  
 print(df_dat_$ITEM %>% unique)
 
pick_col = colors_plot[1:length(df_dat_$ITEM %>% unique)] %>% 
  setNames(df_dat_$ITEM %>% unique)#%>% 

pal = scale_fill_manual(name = paste0(var_id," Type"),
                        values = pick_col, 
                        guide = guide_legend(reverse = FALSE))

print(df_dat_, n=Inf)

plot_f <- ggplot(df_dat_) +
    geom_col(aes(x= SCEN,
                 y= OUTPUT, 
                 fill = ITEM), 
             position = "stack") +
    pal +
    theme_classic() +
    ylab(unit) +
    xlab(NULL) +
    coord_flip() +
    theme(legend.position = "right"
    )+
    geom_hline(yintercept = 0) +
  ggtitle(paste0(title, year))

if(save){
  out_png <- figure_folder(paste0(var_id, "_", year, ".png"))
  message("Saving: ", out_png)
  ggsave(out_png, dpi = 300, width = 8, height = 6, device = "png")
}
return(plot_f)
  }
