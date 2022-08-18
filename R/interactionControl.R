#' Controlling the interaction matrix in the tuning gadget
#' @inheritParams abundanceControl
#'
#' @export
interactionControl <- function(input, output, session, params,
                               params_old, flags, ...) {
    observe({
        req(input$interaction_resource)
        p <- isolate(params())
        sp <- isolate(input$sp)
        # The following req()a are required, otherwise changes in the sliders
        # do not trigger. I am not sure why.
        for (i in p@species_params$species[!is.na(p@A)]) {
            inter_var <- paste0("inter_", i)
            req(input[[inter_var]])
        }
        if (!identical(sp, flags$sp_old_inter)) {
            flags$sp_old_inter <- sp
            return()
        }
        
        if(!is.null(getComponent(p, "MR"))){
          res_df <- mizerMR::resource_interaction(p)
          
          for (iRes in 1:dim(res_df)[2]) {
            res_var <- paste0("inter_res", iRes)
            req(input[[inter_var]])
            mizerMR::resource_interaction(p)[sp,iRes] <- input[[res_var]]
          }
          p@species_params[sp, "interaction_resource"] <-
            input$interaction_resource
        } else {
        p@species_params[sp, "interaction_resource"] <-
            input$interaction_resource
        }
        
        for (i in p@species_params$species[!is.na(p@A)]) {
          inter_var <- paste0("inter_", i)
          p@interaction[sp, i] <- input[[inter_var]]
        }

        tuneParams_update_species(sp, p, params, params_old)
    })
}

#' @rdname interactionControl
#' @inheritParams abundanceControlUI
#' @export
interactionControlUI <- function(p, input) {
    sp <- p@species_params[input$sp, ]
    l1 <- list(
        tags$h3(tags$a(id = "interaction"), "Prey interactions"))
    
        if(!is.null(getComponent(p, "MR"))){
          res_df <- mizerMR::resource_interaction(p)
          for(iRes in 1:dim(res_df)[2]){
            res_var <- paste0("inter_res", iRes)
            res_name <- colnames(res_df)[iRes]
            l1 <- c(l1, list(
          sliderInput(res_var, res_name,
                      value = res_df[sp$species,iRes],
                      min = 0,
                      max = 1,
                      step = 0.001)
            ))
          }
          l1 <- c(l1, list(
            sliderInput("interaction_resource", "Resource",
                        value = sp$interaction_resource,
                        min = 0,
                        max = 1,
                        step = 0.001)
          ))
        } else {
          l1 <- c(l1, list(
        sliderInput("interaction_resource", "Resource",
                    value = sp$interaction_resource,
                    min = 0,
                    max = 1,
                    step = 0.001)
          ))
        }
    for (i in p@species_params$species[!is.na(p@A)]) {
        inter_var <- paste0("inter_", i)
        l1 <- c(l1, list(
            sliderInput(inter_var, i,
                        value = p@interaction[sp$species, i],
                        min = 0,
                        max = 1,
                        step = 0.001)
        ))
    }
    l1
}
