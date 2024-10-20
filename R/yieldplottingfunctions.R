
#plotype can be "fleet" - plots two pie charts for the harvested and unharvested mizerSims.
#each pie chart show the % composition of the total yield from each fleet (or gears)
#plottype = "species" - plots two pie charts for the harvested and unharvested mizerSims.
#each pie chart shows the % composition of the total yield from each species
#plottype = "singlefleet" - plots a pie chart for each gear and each mizersim object 
#each plot shows the % species composition of the yield of a given gear. 

yieldplottingfunctions <- function(harvested, unharvested, timerange1, timerange2, plottype) {
  
  if (plottype == "fleet") {
    
    #getting yield per gear then reformating to work with rest of the code,
    #plus subsetting by year
    
    harv <- as.data.frame(as.table(getYieldGear(harvested)[c(timerange1, timerange2),,]))%>%
      group_by(gear)%>%
      summarise(value=mean(Freq))%>%
      subset(value>0)
    
    unharv <-as.data.frame(as.table(getYieldGear(unharvested)[c(timerange1, timerange2),,]))%>%
      group_by(gear)%>%
      summarise(value=mean(Freq))%>%
      subset(value>0)
    
    fig <- plot_ly()
    fig <- fig %>% add_pie(data = harv, labels = ~gear, values = ~value,
                           name = "harv", domain = list(row = 0, column = 0),
                           title = "Changed Strategy Yield")
    fig <- fig %>% add_pie(data = unharv, labels = ~gear, values = ~value,
                           name = "unharv", domain = list(row = 0, column = 1),
                           title = "Current Strategy Yield")
    
    fig <- fig %>% layout(showlegend = T,
                          grid = list(rows = 1, columns = 2),
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    fig
    
  } else if (plottype == "species") {
    
    harv <- as.data.frame(as.table(getYield(harvested)[c(timerange1, timerange2),]))%>%
      group_by(sp)%>%
      summarise(value=mean(Freq))%>%
      rename(gear = sp)%>%
      subset(value>0)
    
    unharv <- as.data.frame(as.table(getYield(unharvested)[c(timerange1, timerange2),]))%>%
      group_by(sp)%>%
      summarise(value=mean(Freq))%>%
      rename(gear = sp)%>%
      subset(value>0)
    
    fig <- plot_ly()
    fig <- fig %>% add_pie(data = harv, labels = ~gear, values = ~value,
                           name = "harv", domain = list(row = 0, column = 0),
                           title = "Changed Strategy Yield")
    fig <- fig %>% add_pie(data = unharv, labels = ~gear, values = ~value,
                           name = "unharv", domain = list(row = 0, column = 1),
                           title = "Current Strategy Yield")
    
    
    fig <- fig %>% layout(showlegend = T,
                          grid = list(rows = 1, columns = 2),
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    return(fig)
    
  } else if (plottype == "singlefleet") {
    
    harv <- as.data.frame(as.table(getYieldGear(harvested)[c(timerange1, timerange2),,]))
    unharv <-as.data.frame(as.table(getYieldGear(unharvested)[c(timerange1, timerange2),,]))
    
    harv <- split(harv, harv$gear)
    unharv <- split(unharv, unharv$gear)
    
    fig <- plot_ly()
    
    for (i in seq_along(harv)) {
      
      gear_name <- names(harv)[i] 
      
      #this is necessary as for some reason the survey gear is plotted differently, which messes with the layout.
      if (gear_name == "survey") {
        next 
      }
      
      averaged_data <- harv[[gear_name]] %>%
        group_by(sp) %>%
        summarise(value = mean(Freq))
      colnames(averaged_data) <- c("gear", "value")
      
      fig <- fig %>% add_pie(data = averaged_data, labels = ~gear, values = ~value,
                             title = list(text = paste("Changed", gear_name)),
                             textinfo = "none",
                             name = gear_name, domain = list(row = 0, column = i))
      
    }
    for (i in seq_along(unharv)) {
      
      gear_name <- names(unharv)[i] 
      
      if (gear_name == "survey") {
        next  
      }
      
      averaged_data <- unharv[[gear_name]] %>%
        group_by(sp) %>%
        summarise(value = mean(Freq))
      colnames(averaged_data) <- c("gear", "value")
      
      
      fig <- fig %>% add_pie(data = averaged_data, labels = ~gear, values = ~value,
                             title = gear_name,
                             textinfo = "none",
                             name = gear_name, domain = list(row = 1, column = i))
      
    }
    
    fig <- fig %>% layout(showlegend = T,
                          grid=list(rows=2, columns=length(harv)),
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    fig
    
  } else {
    
    stop("Invalid plot type specified")
  }
  
}