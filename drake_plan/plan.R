plan <- drake::drake_plan(  
  # Compile expert database
  compiled_plant_data = 
    compile_plant_data(answers_path = drake::file_in("raw_data/plant_data/expert_answers.csv"),
                       metadata_path = drake::file_in("raw_data/plant_data/metadata.csv")),
  
  # Extract summary data by species x community
  plant_spp_summary =
    summarise_plant_data(data = compiled_plant_data,
                         type = "species"),
  
  plot_spp_covers =
    plot_covers(data = plant_spp_summary,
                type = "species",
                outfile = file_out("outputs/plant_spp_covers.pdf"),
                width = 12,
                height = 6),
  
  # Extract summary data by community
  plant_community_summary =
    summarise_plant_data(data = compiled_plant_data,
                         type = "community"),
  
  plot_comm_covers =
    plot_covers(data = plant_community_summary,
                type = "community",
                outfile = file_out("outputs/plant_community_covers.pdf"),
                width = 7,
                height = 7),
  
  # Count number of experts by direction for species level
  plant_spp_directions =
    plant_direction_frequencies(data = compiled_plant_data,
                                type = "species"),
  
  plot_spp_directions =
    plot_plant_directions(data = plant_spp_directions,
                          type = "species",
                          outfile = file_out("outputs/plant_spp_directions.pdf"),
                          width = 8,
                          height = 5),
  
  # Count number of experts by direction for community level
  plant_community_directions =
    plant_direction_frequencies(data = compiled_plant_data,
                                type = "community"),
  
  plot_comm_directions =
    plot_plant_directions(data = plant_community_directions,
                          type = "community",
                          outfile = file_out("outputs/plant_community_directions.pdf"),
                          width = 6,
                          height = 6),
  
  plant_regression_data =
    append_plant_traits(data = plant_spp_summary,
                        trait_path = file_in("raw_data/plant_data/species_traits.csv")),
  
  plant_spp_trait_plots =
    plot_scatter(data = plant_regression_data,
                 response = "mean_AC",
                 predictor = c("Mean_ht_mm",
                               "Leaf_area_mm2",
                               "SLA_mm2mg1",
                               "Diaspore_mg",
                               "Dispersal_dist_m"),
                 predictor_labels = c("Mean~ht~(mm)",
                                      "Leaf~area~(mm^2)",
                                      "SLA~(mm^{2}*mg^{-1})",
                                      "Diaspore~(mg)",
                                      "Dispersal~dist~(m)"),
                 xlab ="Log10(traits values)",
                 ylab ="(Future - Current)/(Future + Current)",
                 alpha = 0.6,
                 ncol=2,
                 zero_line=TRUE,
                 scale= "free",
                 logx = TRUE,
                 show_correlation = TRUE,
                 outfile = file_out("outputs/plant_spp_traits.pdf"),
                 width = 7,
                 height = 7),
  
  plant_cat_traits_plots =
    plot_scatter(data = plant_regression_data,
                 response = "mean_AC",
                 predictor = c("Growth_form",
                               "Resprouter",
                               "Pollination",
                               "Dispersal_mode"),
                 predictor_labels = c("Growth~form",
                                      "Resprouter",
                                      "Pollinator",
                                      "Dispersal~mode"),
                 xlab ="Categorical traits",
                 ylab = "(Future - Current)/(Future + Current)",
                 alpha = 0.6,
                 ncol=2,
                 zero_line= TRUE,
                 scale="free",
                 logx = FALSE,
                 outfile = file_out("outputs/plant_cat_traits.pdf"),
                 width = 7,
                 height = 7),
  
  plant_envion_trait_plots =
    plot_scatter(data = plant_regression_data,
                 response = "mean_AC",
                 predictor = c("altitude_min",
                               "altitude_max",
                               "altitude_range",
                               "MAT_min",
                               "MAT_max",
                               "MAT_range",
                               "Extent",
                               "Area"),
                 predictor_labels = c("Min~altitude~(m)",
                                      "Max~altitude~(m)",
                                      "Altitude~range~(m)",
                                      "Min~MAT~(~degree*C)",
                                      "Max~MAT~(~degree*C)",
                                      "MAT~range~(~degree*C)",
                                      "Extent~occupied~(km^2)",
                                      "Area~occupied~(km^2)"),
                 
                 xlab ="Environmental traits",
                 ylab ="(Future - Current)/(Future + Current)",
                 alpha = 0.6,
                 ncol=2,
                 zero_line=TRUE,
                 scale = "free",
                 show_correlation = TRUE,
                 logx = TRUE,
                 outfile = file_out("outputs/plant_enviro_traits.pdf"),
                 width = 7,
                 height = 7),
  
  animal_data = 
    compile_animal_data(expert_file = file_in("raw_data/animal_data/animal_expert_answers.csv"), 
                        q_file = file_in("raw_data/animal_data/animal_question_meta.csv"), 
                        trait_file = file_in("raw_data/animal_data/animal_trait_data.csv")),
  
  animal_spp_summary_LowElev = 
    summarise_animal_data(data = animal_data, 
                          Q_IDs = c("1A", "1B"),
                          mass_convert = FALSE),
  
  animal_scatter_plots_LowElev =
    plot_animals(data = animal_spp_summary_LowElev,
                 add_labels = TRUE,
                 log_scale = FALSE,
                 facet_by = "Water_centric",
                 ylabel = "Future minimum elevation (m)",
                 xlabel = "Current minimum elevation (m)",
                 outfile = drake::file_out("outputs/animal_scatter_LowElev.pdf"),
                 width = 6,
                 height = 4),
                                      
  animal_spp_directions_LowElev =
    animal_spp_direction_frequencies(data = animal_data,
                                     Q_IDs = c("1A", "1B")),
  
  plot_animal_direction_water_LowElev =
    plot_animal_directions(data = animal_spp_directions_LowElev, 
                           facet_by = "Water_centric",
                           outfile = file_out("outputs/animal_water_LowElev.pdf"),
                           width = 7,
                           height = 7),
  
  animal_spp_summary_HighElev = 
    summarise_animal_data(data = animal_data, 
                          Q_IDs = c("2A", "2B"),
                          mass_convert = FALSE),
  
  animal_scatter_plots_HighElev =
    plot_animals(data = animal_spp_summary_HighElev,
                 add_labels = TRUE,
                 facet_by = "Water_centric",
                 log_scale = FALSE,
                 ylabel = "Future maximum elevation (m)",
                 xlabel = "Current maximum elevation (m)",
                 outfile = drake::file_out("outputs/animal_scatter_HighElev.pdf"),
                 width = 6,
                 height = 6),
  
  animal_spp_directions_HighElev =
    animal_spp_direction_frequencies(data = animal_data,
                                     Q_IDs = c("2A", "2B")),
  
  plot_animal_direction_water_HighElev =
    plot_animal_directions(data = animal_spp_directions_HighElev, 
                           facet_by = "Water_centric",
                           outfile = file_out("outputs/animal_water_directions_HighElev.pdf"),
                           width = 7,
                           height = 4),  
  
  animal_spp_summary_abundance = 
    summarise_animal_data(data = animal_data, 
                          Q_IDs = c("3A", "3B"),
                          mass_convert = FALSE) %>% 
    dplyr::filter(SPP_ID != 2), # Remove Bogong Moth for now
  
  
  animal_spp_summary_mass = 
    summarise_animal_data(data = animal_data, 
                          Q_IDs = c("3A", "3B"),
                          mass_convert = TRUE)  %>% # Remove Bogong Moth for now,
    dplyr::filter(SPP_ID != 2),
  
  animal_scatter_plots_3 =
    ggplot2::ggsave(cowplot::plot_grid(plot_animals(data = animal_spp_summary_abundance %>% filter(SPP_ID !=2),
                                    add_labels = TRUE,
                                    log_scale = TRUE,
                                    facet_by = "Water_centric",
                                    ylabel = expression(paste("Future abundance (100",~m^2, ")")),
                                    xlabel = expression(paste("Current abundance (100",~m^2, ")"))),
                       
                       plot_animals(data = animal_spp_summary_mass %>% filter(SPP_ID !=2),
                                    add_labels = TRUE,
                                    log_scale = TRUE,
                                    facet_by = "Water_centric",
                                    ylabel = expression(paste("Future mass (g/100",~m^2, ")")),
                                    xlabel = expression(paste("Current mass (g/100",~m^2, ")"))), 
                       nrow = 2, labels =LETTERS),
                    filename = file_out("outputs/animal_scatter3AB.pdf"),
                    width = 7,
                    height = 7),
  
  animal_spp_directions3 =
    animal_spp_direction_frequencies(data = animal_data,
                                     Q_IDs = c("3A", "3B")),
  
  plot_animal_direction_water3 =
    plot_animal_directions(data = animal_spp_directions3, 
                           facet_by = "Water_centric",
                           outfile = file_out("outputs/animal_water_directions3AB.pdf"),
                           width = 7,
                           height = 7)
)







