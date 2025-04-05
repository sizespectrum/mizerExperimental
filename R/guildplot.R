
#' Plot Guild Relative Change Across Timescales  
#'
#' This function takes two mizerSim objects and calculates the relative % 
#' change in each given feeding guilde in the chosen year, short term (1/2 of the 
#' chosen year) and the long term (2x the chosen year) 
#' This function requires a dataframe in the environment titled guildparams - this dataframe should have
#' a column for minw (minimum weight of guild), maxw (maxmimum weight), guild (
#' the guild for the given weight), and a column for the species (which matches the mizersim species).
#' The mizerSim objects must also have a tmax of 2 * year2. 
#' 
#'
#' @param harvested A mizerSim object
#' @param unharvested A mizerSim object - to compare to.
#' @param year1 The lower year to plot in the range 
#' @param year2 The higher year to plot in the range
#'
#' @return A ggplot object that plots 3 bars per species - in the short, 
#' chosen and long time - it plots the relative biomass of each feeding guild 
#' in comparison to the unharvested.
#' 
#'
#' @examples
#' harvested <- project(NS_params)
#' unharvested <- project(NS_params)
#' guildplot(harvested, unharvested, 1, 2)
#'
#' @export
guildplot <- function(harvestedprojection, unharvestedprojection, year1, year2) {
  
  harvestedshort <- plotSpectra(harvestedprojection, time_range = max(1, round(year1 * (1/2))):max(1, round(year2 * (1/2))), return_data = TRUE)
  harvested <- plotSpectra(harvestedprojection, time_range = year1:year2, return_data = TRUE)
  harvestedlong <- plotSpectra(harvestedprojection, time_range = (year1 * 2):(year2 * 2), return_data = TRUE)
  
  unharvestedshort <- plotSpectra(unharvestedprojection, time_range = max(1, round(year1 * (1/2))):max(1, round(year2 * (1/2))), return_data = TRUE)
  unharvested <- plotSpectra(unharvestedprojection, time_range = year1:year2, return_data = TRUE)
  unharvestedlong <- plotSpectra(unharvestedprojection, time_range = (year1 * 2):(year2 * 2), return_data = TRUE)
  
  process_guilds <- function(mizerprojection) {
    
    
    assign_guild <- function(data, rules) {
      data <- data %>%
        mutate(Guild = NA_character_)  # Initialize Guild column with NA
      
      # Loop through each rule in the rules dataframe
      for (i in 1:nrow(rules)) {
        data <- data %>%
          mutate(
            #THIS CODE ASSUMES THAT ANYTHING UNDER W 0.05 IS PLANKTIVOROUS, AS IT IS VERY SMALL 
            Guild = ifelse(w < 0.05, "Plank",
                           ifelse(
                             is.na(Guild) & w >= rules$minw[i] & w < rules$maxw[i], 
                             rules$Feeding.guild[i], Guild)
            )
          )
      }
      
      return(data)
    }
    
    
    mizerprojection <- mizerprojection %>%
      group_by(Species) %>%
      group_modify(~ {
        species_data <- .x  
        
        species_name <- unique(species_data$Legend)
        
        species_rules <- guildparams %>%
          filter(Species == species_name)
        
        if (nrow(species_rules) == 0) {
          return(species_data)
        }
        
        assign_guild(species_data, species_rules)
        
      }) %>%
      ungroup() %>%
      #this next step takes out anything without an assigned guild, but you might not choose to do this
      #and then you can have a column of the change in biomass of species/sizes that we do not have guild rules for
      #this would be useful to observe where the biomass change is going, but would be confusing to interpret and explain.
      #(as its a possibility that all 3 guilds show a negative decrease, which looks like a decrease in biomass,
      #but may just be due to other sizes/species taking this biomass)
      drop_na(Guild)%>%
      group_by(Guild) %>%
      summarise(value = mean(value))
    
    return(mizerprojection)
    
  }
  
  #for the harvested - 
  guildsshort <- process_guilds(harvestedshort)
  guilds <- process_guilds(harvested)
  guildslong <- process_guilds(harvestedlong)
  #for the unharvested - 
  unguildsshort <- process_guilds(unharvestedshort)
  unguilds <- process_guilds(unharvested)
  unguildslong <- process_guilds(unharvestedlong)
  
  #now joining them together
  guildsshort$time <- "short"
  guilds$time <- "chosen"
  guildslong$time <- "long"
  unguildsshort$time <- "short"
  unguilds$time <- "chosen"
  unguildslong$time <- "long"
  
  joinedguilds <- bind_rows(guildsshort, guilds, guildslong) %>%
    group_by(Guild, time) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
  
  unjoinedguilds <- bind_rows(unguildsshort, unguilds, unguildslong) %>%
    group_by(Guild, time) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
  
  joinedguilds <- joinedguilds%>%
    full_join(unjoinedguilds, by = c("Guild", "time"),relationship = "many-to-many") %>%
    mutate(percentage_diff = ((value.x-value.y)/value.y))%>%
    select(Guild, time, percentage_diff)
  
  #this sets the colours correctly
  joinedguilds$time <- factor(joinedguilds$time, levels = c("short", "chosen", "long"))
  joinedguilds$fill_group <- interaction(joinedguilds$percentage_diff >= 0, joinedguilds$time)
  
  #plotting
  ggplot(joinedguilds, aes(x = Guild, y = percentage_diff, fill = fill_group)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    scale_fill_manual(values = c(
      "FALSE.short" = "#E76F51",  
      "FALSE.chosen" = "#E98C6B",  
      "FALSE.long" = "#F2A488",   
      "TRUE.short" = "#2FA4E7", 
      "TRUE.chosen" = "#2FA4E7cc",
      "TRUE.long" = "#2FA4E799" 
    )) +
    labs(title = "Percentage Change by Guild", 
         x = "Guild", 
         y = "Percentage Change") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 14, angle = 90, hjust = 1, vjust = 0.5),
      axis.text.y = element_text(size = 14),
      legend.position = "none",
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16)
    )
  
}
