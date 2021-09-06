#' Controlling egg abundance in the tuning gadget
#' @param input .
#' @param output .
#' @param session .
#' @param params .
#' @param flags .
#' @param ... .
abundanceControl <- function(input, output, session, params, flags, ...) {
    observe({
        n0 <- req(input$n0)
        p <- isolate(params())
        sp <- isolate(input$sp)
        if (!identical(sp, flags$sp_old_n0)) {
            flags$sp_old_n0 <- sp
            return()
        }
        updateSliderInput(session, "n0",
                          min = signif(n0 / 10, 3),
                          max = signif(n0 * 2, 3))
        # rescale abundance to new egg density
        p@initial_n[sp, ] <- p@initial_n[sp, ] * n0 /
            p@initial_n[sp, p@w_min_idx[sp]]
        
        # Update the reactive params object
        params(p)
    })
}

#' @rdname abundanceControl
#' @param p The MizerParams object currently being tuned.
#' @param sp The single row from the pecies parameter data frame for the 
#'   currently selected species.
#' @return A tagList with a slider for the egg density
abundanceControlUI <- function(p, sp) {
    n0 <- p@initial_n[sp$species, p@w_min_idx[sp$species]]
    tagList(
        tags$h3(tags$a(id = "egg"), "Abundance"),
        sliderInput("n0", "Egg density",
                    value = n0,
                    min = signif(n0 / 10, 3),
                    max = signif(n0 * 2, 3),
                    step = n0 / 50))
}
