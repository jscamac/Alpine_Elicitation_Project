plan <- drake::drake_plan(  
  # Compile expert database
  compiled_plant_data = 
    compile_plant_data(answers_path = drake::file_in("raw_data/plant_data/expert_answers.csv"),
                       metadata_path = drake::file_in("raw_data/plant_data/metadata.csv")),
  # COMMUNITY DATA
  plant_community_summary =
    summarise_plant_data(data = compiled_plant_data,
                         type = "community"),
  
  plant_community_directions =
    plant_direction_frequencies(data = compiled_plant_data,
                                type = "community"),
  
  # FIG 1 - community responses
  fig1 = ggplot2::ggsave(plot = 
                           cowplot::plot_grid(
                             plot_covers(data = plant_community_summary,
                                         type = "community"),
                             plot_plant_directions(data = plant_community_directions,
                                                   type = "community"),
                             nrow=2,
                             align="v",
                             labels ="AUTO",
                             rel_heights = c(1, 1.25)),
                         width = 5,
                         height = 5,
                         filename = file_out("outputs/fig1.pdf"),
                         device=cairo_pdf, 
                         family ="Arial Unicode MS"),
  
  # SPECIES DATA
  plant_spp_summary =
    summarise_plant_data(data = compiled_plant_data,
                         type = "species",
                         trait_path = file_in("raw_data/plant_data/species_traits.csv")),

  # Count number of experts by direction for species level
  plant_spp_directions =
    plant_direction_frequencies(data = compiled_plant_data,
                                type = "species",
                                trait_path = file_in("raw_data/plant_data/species_traits.csv")),
  
  ## Fig 2 - Species responses
  fig2 = ggplot2::ggsave(plot = 
                           cowplot::plot_grid(
                             plot_covers(data = plant_spp_summary,
                                         type = "species"),
                             plot_plant_directions(data = plant_spp_directions,
                                                   type = "species"),
                             nrow=2,
                             align="v",
                             labels ="AUTO",
                             hjust =-3),
                         width = 11.69,
                         height = 8.27,
                         filename = file_out("outputs/fig2.pdf"),
                         device=cairo_pdf, 
                         family ="Arial Unicode MS"),
  
  
  ## ANIMAL DATA
  ## ABUNDANCE
  animal_data =
    compile_animal_data(expert_file = file_in("raw_data/animal_data/animal_expert_answers.csv"),
                        q_file = file_in("raw_data/animal_data/animal_question_meta.csv"),
                        trait_file = file_in("raw_data/animal_data/animal_trait_data.csv")),
  
  animal_abund_summary = 
    summarise_animal_data(data = animal_data, 
                          Q_IDs = c("3A", "3B"),
                          mass_convert = FALSE) %>% 
    dplyr::filter(SPP_ID != 2), # Remove Bogong Moth for now (too big an outlier)
  
  animal_abund_directions =
    animal_spp_direction_frequencies(data = animal_data,
                                     Q_IDs = c("3A", "3B")),
  # Fig 3 Animal abundance responses
  fig3 =
    ggplot2::ggsave(cowplot::plot_grid(
      plot_animals(data = animal_abund_summary %>% filter(SPP_ID !=2),
                   add_labels = TRUE,
                   log_scale = TRUE,
                   facet_by = "Water_centric",
                   ylabel = expression(paste("Future abundance (100",~m^2, ")")),
                   xlabel = expression(paste("Current abundance (100",~m^2, ")"))),
      plot_animal_directions(data = animal_abund_directions,
                             facet_by = "Water_centric"),
      nrow=2,
      align="v",
      labels ="AUTO",
      hjust =-3),
      width = 7.25,
      height = 7.25,
      filename = file_out("outputs/fig3.pdf"),
      device= cairo_pdf, 
      family ="Arial Unicode MS"),
  
  ## Elevation
  
  animal_elev_summary = 
    summarise_animal_data(data = animal_data, 
                          Q_IDs = c("1A", "1B"),
                          mass_convert = FALSE) %>%
    dplyr::mutate(elev_type = "Minimum elevation") %>%
    dplyr::select(-N) %>% # Need to remove N because some experts didn't answer both sets of Q's
    dplyr::bind_rows(summarise_animal_data(data = animal_data, 
                                           Q_IDs = c("2A", "2B"),
                                           mass_convert = FALSE) %>%
                       dplyr::mutate(elev_type = "Maximum elevation") %>%
                       dplyr::select(-N)) %>%
    dplyr::bind_rows(summarise_elevrange(data = animal_data) %>%
                       dplyr::mutate(elev_type = "Elevation range")) %>%
    dplyr::filter(!SPP_ID %in% c(20, 21)),
  
  # Fig 4 Animal elevation plots
  fig4 = 
    plot_animals(data = animal_elev_summary,facet_by = c("Water_centric", "elev_type"), 
                 ncol=3, log_scale = FALSE,
                 xlab ="Current elevation (m)",
                 ylab ="Future elevation (m)",
                 width = 7.25,
                 height = 6,
                 outfile = file_out("outputs/fig4.pdf")),
  # Fig 6 environmental traits
  fig6 =
    plot_scatter(data = plant_spp_summary,
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
                 ncol=3,
                 zero_line=TRUE,
                 scale = "free",
                 show_correlation = TRUE,
                 logx = TRUE,
                 outfile = file_out("outputs/fig6.pdf"),
                 width = 7,
                 height = 6),
  # Fig 7 - Species traits
  fig7 =
    plot_scatter(data = plant_spp_summary,
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
                 ncol=3,
                 zero_line=TRUE,
                 scale= "free",
                 logx = TRUE,
                 show_correlation = TRUE,
                 outfile = file_out("outputs/fig7.pdf"),
                 width = 7,
                 height = 6),
  # Fig supp
  supp_fig_cat_traits =
    plot_scatter(data = plant_spp_summary,
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
                 outfile = file_out("outputs/supp_fig_cat_traits.pdf"),
                 width = 7,
                 height = 7)
)







