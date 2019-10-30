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
                          width = 12,
                          height = 7),
  
  # Count number of experts by direction for community level
  plant_community_directions =
    plant_direction_frequencies(data = compiled_plant_data,
                                type = "community"),
  
  plot_comm_directions =
    plot_plant_directions(data = plant_community_directions,
                          type = "community",
                          outfile = file_out("outputs/plant_community_directions.pdf"),
                          width = 7,
                          height = 7),
  
  plant_regression_data =
    append_plant_traits(data = plant_spp_summary,
                        trait_path = file_in("raw_data/plant_data/species_traits.csv")),
  
  plant_spp_trait_plots =
    plot_scatter(data = plant_regression_data,
                 response = "diff",
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
                 ylab ="Future - Current",
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
                 response = "diff",
                 predictor = c("Growth_form",
                               "Resprouter",
                               "Pollination",
                               "Dispersal_mode"),
                 predictor_labels = c("Growth~form",
                                      "Resprouter",
                                      "Pollinator",
                                      "Dispersal~mode"),
                 xlab ="Categorical traits",
                 ylab = "Future - Current",
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
                 response = "diff",
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
                 ylab ="Future - Current",
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
  
  animal_spp_summary_abundance = 
    summarise_animal_data(data = animal_data, 
                          Q_IDs = c("3A", "3B"),
                          mass_convert = FALSE),
  
  plot_animal_abun_summary_taxon =
    plot_animals(data = animal_spp_summary_abundance,
                 add_labels = TRUE,
                 colour_by = "Taxon",
                 outfile = file_out("outputs/animal_abun_taxon.pdf"),
                 ylabel = expression(paste("Future mass (100",~m^2, ")")),
                 xlabel = expression(paste("Current mass (100",~m^2, ")")),
                 width = 7, 
                 height = 7),
  
  plot_animal_abun_summary_water =
    plot_animals(data = animal_spp_summary_abundance,
                 add_labels = TRUE,
                 colour_by = "Water_centric",
                 outfile = file_out("outputs/animal_abun_water.pdf"),
                 ylabel = expression(paste("Future abundance (100",~m^2, ")")),
                 xlabel = expression(paste("Current abundance (100",~m^2, ")")),
                 width = 7, 
                 height = 7),
  
  animal_spp_summary_mass = 
    summarise_animal_data(data = animal_data, 
                          Q_IDs = c("3A", "3B"),
                          mass_convert = TRUE),
  
  plot_animal_mass_summary_taxon =
    plot_animals(data = animal_spp_summary_mass,
                 add_labels = TRUE,
                 colour_by = "Taxon",
                 outfile = file_out("outputs/animal_mass_taxon.pdf"),
                 ylabel = expression(paste("Future mass (g/100",~m^2, ")")),
                 xlabel = expression(paste("Current mass (g/100",~m^2, ")")),
                 width = 7, 
                 height = 7),
  
  plot_animal_mass_summary_water =
    plot_animals(data = animal_spp_summary_mass,
                 add_labels = TRUE,
                 colour_by = "Water_centric",
                 outfile = file_out("outputs/animal_mass_water.pdf"),
                 ylabel = expression(paste("Future mass (g/100",~m^2, ")")),
                 xlabel = expression(paste("Current mass (g/100",~m^2, ")")),
                 width = 7, 
                 height = 7),
  
  animal_spp_directions =
    animal_spp_direction_frequencies(data = animal_data,
                                     Q_IDs = c("3A", "3B")),
  
  plot_animal_direction_water =
    plot_animal_directions(data = animal_spp_directions, 
                           facet_by = "Water_centric",
                           outfile = file_out("outputs/animal_water_directions.pdf"),
                           width = 7,
                           height = 7),
  
  plot_animal_direction_taxon =
    plot_animal_directions(data = animal_spp_directions, 
                           facet_by = "Taxon",
                           outfile = file_out("outputs/animal_taxon_directions.pdf"),
                           width = 7,
                           height = 7),
  
  
  
  
)







