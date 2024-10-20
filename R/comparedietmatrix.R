#'I am not sure if this code works as intended. It appears so, as I have checked it,
#'but the results are not what it is expected.
#'
#'I will also need to see if edge cases, where the timerange is 1:1, work.



#This function plots the diet matrix from the mizersim objects.
#' Plot Relative Diet Proportion of each Prey/Predator  
#'
#' This function takes two mizerSim objects and calculates the relative 
#' change in the proportion of a given prey species in a predators diet. This 
#' is done for every prey/predator in the model. 
#'
#' @param harvested A mizerSim object
#' @param unharvested A mizerSim object - to compare to.
#' @param chosentime The year range to plot (example 1:2).
#'
#' @return A ggplot object of a matrix of predator species on the X axis, 
#' prey species on the Y axis. The colour of the box indicates the change 
#' of the proportion in the predator's diet of the given prey species.
#' 
#'
#' @examples
#' harvested <- getBiomass(NS_sim)
#' unharvested <- getBiomass(NS_sim)
#' comparedietmatrix(harvested, unharvested, 5)
#'
#' @export
comparedietmatrix <- function(unharvestedprojection, harvestedprojection, timerange){
  
  #THE TIMERANGE SHOULD BE X:Y
  dietunharv <- getDiet(unharvestedprojection@params, 
                        n = apply(unharvestedprojection@n[timerange,,], c(2, 3), mean),
                        n_pp = apply(unharvestedprojection@n_pp[timerange,], 2, mean),
                        n_other = apply(unharvestedprojection@n_other[timerange,], 2, mean),
                        proportion = TRUE) %>%
    as.table()%>%
    as.data.frame()%>%
    group_by(predator, prey)%>%
    summarise(Proportion=mean(Freq))
  
  dietharv <- getDiet(unharvestedprojection@params, 
                      n = apply(unharvestedprojection@n[timerange,,], c(2, 3), mean),
                      n_pp = apply(unharvestedprojection@n_pp[timerange,], 2, mean),
                      n_other = apply(unharvestedprojection@n_other[timerange,], 2, mean),
                      proportion = TRUE) %>%
    as.table()%>%
    as.data.frame()%>%
    group_by(predator, prey)%>%
    summarise(Proportion=mean(Freq))
  
  joindiet <- left_join(dietharv, dietunharv, by = c("prey", "predator"))%>%
    mutate(Difference = ((Proportion.x - Proportion.y) / Proportion.y) * 100) %>%  # Calculate percentage change
    select(predator, prey, Difference)%>%
    filter(!predator %in% c("2", "4", "6", "8", "16", "17", "18", "19", "20", "Resource"), 
           !prey %in% c("2", "4", "6", "8", "16", "17", "18", "19", "20", "Resource"))
  
  dietplot <- ggplot(joindiet, aes(x = predator, y = prey, fill = Difference)) +
    geom_tile() +  
    scale_fill_gradient2() +  
    labs(x = "Predator",
         y = "Prey",
         fill = "Difference") +  
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 14),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16))
  
  
  return(dietplot)
  
}