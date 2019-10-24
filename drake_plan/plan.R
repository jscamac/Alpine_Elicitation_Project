plan <- drake::drake_plan(  
  # Compile expert database
  compiled_plant_data = 
    compile_plant_data(answers_path = drake::file_in("raw_data/expert_answers.csv"),
                       metadata_path = drake::file_in("raw_data/metadata.csv")),
  
  # Extract summary data by species x community
  plant_spp_summary =
    summarise_plant_data(data = compiled_plant_data,
                         type = "species"),
  
  plot_spp_covers =
    plot_covers(data = plant_spp_summary,
               type = "species",
               outfile = file_out("outputs/spp_covers.pdf"),
               width = 12,
               height = 6),
  
  # Extract summary data by community
  plant_community_summary =
    summarise_plant_data(data = compiled_plant_data,
                         type = "community"),
  
  plot_comm_covers =
    plot_covers(data = plant_community_summary,
               type = "community",
               outfile = file_out("outputs/community_covers.pdf"),
               width = 7,
               height = 7),
  
  # Count number of experts by direction for species level
  plant_spp_directions =
    direction_frequencies(data = compiled_plant_data,
                        type = "species"),
  
  plot_spp_directions =
    plot_stack(data = plant_spp_directions,
               type = "species",
               outfile = file_out("outputs/spp_directions.pdf"),
               width = 12,
               height = 7),
  
  # Count number of experts by direction for community level
  plant_community_directions =
    direction_frequencies(data = compiled_plant_data,
                          type = "community"),
  
  plot_comm_directions =
    plot_stack(data = plant_community_directions,
               type = "community",
               outfile = file_out("outputs/community_directions.pdf"),
               width = 7,
               height = 7)
)
  
  
  
  
  
  
  
  