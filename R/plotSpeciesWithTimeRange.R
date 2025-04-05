#This function plots the species plot - which the change in species for a given 
#year, and also for 2x in future and 1/2 year in the past.


#' Plot MizerSim Relative Biomass per Species Across Varying Timescales 
#'
#' This function takes two mizerSim objects and calculates the relative % 
#' change in each given species in the chosen year, short term (1/2 of the 
#' chosen year) and the long term (2x the chosen year). The mizerSim
#' objects must have tmax = 2 * chosentime2. 
#'
#' @param harvested A mizerSim object
#' @param unharvested A mizerSim object - to compare to.
#' @param chosentime1 The first year to plot in the range 
#' @param chosentime2 The last year to plot in the range
#'
#' @return A ggplot object that plots 3 bars per species - in the short, 
#' chosen and long time - it plots the relative biomass of each species in
#' comparison to the unharvested.
#' 
#'
#' @examples
#' harvested <- project(NS_params)
#' unharvested <- project(NS_params)
#' plotSpeciesWithTimeRange(harvested, unharvested, 1, 2)
#'
#' @export
plotSpeciesWithTimeRange <- function(harvestedprojection, unharvestedprojection, chosentime1, chosentime2) {
  
  #get the biomass of the species
  unharvestedbio <- getBiomass(unharvestedprojection) %>%
    .[chosentime1:chosentime2, ] %>% 
    melt() %>%
    group_by(sp) %>%
    summarize(value = mean(value, na.rm = TRUE))
  
  harvestedbio <- getBiomass(harvestedprojection) %>%
    .[chosentime1:chosentime2, ] %>% 
    melt() %>%
    group_by(sp) %>%
    summarize(value = mean(value, na.rm = TRUE))
  
  #calculate percentage change in species in the chosen year
  percentage_diff <-  harvestedbio %>%
    left_join(unharvestedbio, by = "sp") %>%
    mutate(percentage_diff = ((value.x - value.y) / value.y) * 100,
           Species = sp) %>%
    select(Species, percentage_diff) %>%
    filter(!Species %in% c("2", "4", "6", "8", "16", "17", "18", "19", "20", "Resource"))%>%
    mutate(class = "chosen")
  
  calculate_biomass_triples <- function(unharvestedprojection, harvestedprojection, year1, year2) {
    
    # Calculate unharvested biomass at different time points
    unharvestedbiotriple <- getBiomass(unharvestedprojection)
    
    #the range has to be 1-2, becuase if it is 1-1 it messes with the way the data is formatted.
    #i have also used ceiling here, because using round means they round to nearest even number, so some cases (11:13)
    #end up as 6:6 for the lowbiotrip - this does not work for the code format.
    lowunbiotrip <- unharvestedbiotriple[max(1, ceiling(year1 * (1/2))):max(2, ceiling(year2 * (1/2))), ] %>%
      melt() %>%
      group_by(sp) %>%
      summarize(value = mean(value, na.rm = TRUE))
    
    highunbiotrip <- unharvestedbiotriple[(year1 * 2):(year2 * 2), ] %>%
      melt() %>%
      group_by(sp) %>%
      summarize(value = mean(value, na.rm = TRUE))
    
    # Calculate harvested biomass at different time points
    harvestedbiotriple <- getBiomass(harvestedprojection)
    
    lowbiotrip <- harvestedbiotriple[max(1, ceiling(year1 * (1/2))):max(2, ceiling(year2 * (1/2))),] %>%
      melt() %>%
      group_by(sp) %>%
      summarize(value = mean(value, na.rm = TRUE))
    
    highbiotrip <- harvestedbiotriple[(year1 * 2):(year2 * 2), ] %>%
      melt() %>%
      group_by(sp) %>%
      summarize(value = mean(value, na.rm = TRUE))
    
    # Return the results as a list
    list(
      lowunbiotrip,
      highunbiotrip,
      lowbiotrip,
      highbiotrip
    )
  }
  
  #calculate percentage change in other years
  biorange <- calculate_biomass_triples(unharvestedprojection, harvestedprojection, chosentime1, chosentime2)
  
  #percentage_difflow <- percentdiff(biorange[[3]], biorange[[1]])
  #percentage_difflow$class <- "short"
  
  percentage_difflow <-  biorange[[3]] %>%
    left_join(biorange[[1]], by = "sp") %>%
    mutate(percentage_diff = ((value.x - value.y) / value.y) * 100,
           Species = sp) %>%
    select(Species, percentage_diff) %>%
    filter(!Species %in% c("2", "4", "6", "8", "16", "17", "18", "19", "20", "Resource"))%>%
    mutate(class = "short")
  
  #percentage_diffhigh <- percentdiff(biorange[[4]], biorange[[2]])
  #percentage_diffhigh$class <- "long"
  
  percentage_diffhigh <-  biorange[[4]] %>%
    left_join(biorange[[2]], by = "sp") %>%
    mutate(percentage_diff = ((value.x - value.y) / value.y) * 100,
           Species = sp) %>%
    select(Species, percentage_diff) %>%
    filter(!Species %in% c("2", "4", "6", "8", "16", "17", "18", "19", "20", "Resource"))%>%
    mutate(class = "long")
  
  percentage_diff <- rbind(percentage_difflow, percentage_diff, percentage_diffhigh)
  
  #now plot them together - the first lines sort out the colors of the bars
  percentage_diff$class <- factor(percentage_diff$class, levels = c("short", "chosen", "long"))
  percentage_diff$fill_group <- interaction(percentage_diff$percentage_diff >= 0, percentage_diff$class)
  
  ggplot(percentage_diff, aes(x = Species, y = percentage_diff, fill = fill_group)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    geom_hline(yintercept = 0, color = "grey", linetype = "dashed", size = 0.5)+
    labs(x = "Species", y = "Percentage Change") +
    scale_fill_manual(values = c(
      "FALSE.short" = "#E76F51",  
      "FALSE.chosen" = "#E98C6B",  
      "FALSE.long" = "#F2A488",   
      "TRUE.short" = "#2FA4E7", 
      "TRUE.chosen" = "#2FA4E7cc",
      "TRUE.long" = "#2FA4E799" 
    )) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 16, angle = 90, hjust = 1, vjust = 0.5),
      axis.text.y = element_text(size = 14),
      legend.position = "none",
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16)
    )
}
