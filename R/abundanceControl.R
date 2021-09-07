#' Controlling egg abundance in the tuning gadget
#' @param input Reactive holding the inputs
#' @param output Reactive holding the outputs
#' @param session Shiny session
#' @param params Reactive value holding updated MizerParams object
#' @param params_old Reactive value holding non-updated MizerParams object
#' @param flags Environment holding flags to skip certain observers
#' @param ... Unused
abundanceControl <- function(input, output, session, params, params_old,
                             flags, ...) {
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
