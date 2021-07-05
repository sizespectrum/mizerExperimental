#' Set Beverton-Holt density dependence
#'
#' Takes a MizerParams object `params` with arbitrary density dependence and
#' returns a MizerParams object with Beverton-Holt density-dependence with your
#' chosen reproductive efficiency or your chosen maximal reproduction rate.
#' If you have tuned your `params` object to describe a particular steady state,
#' then setting the Beverton-Holt density dependence with this function will
#' leave you with the exact same steady state, because it only allows you to
#' choose among those Beverton-Holt reproduction curves that have the property
#' that the energy invested into reproduction by the mature individuals leads to
#' the reproduction rate that is required to maintain the given egg abundance.
#' By specifying one of the parameters `erepro`, `R_max` or `reproduction_level`
#' you pick the desired reproduction curve. More details of these parameters are
#' provided below.
#'
#' With Beverton-Holt density dependence the relation between the energy
#' invested into reproduction and the number of eggs hatched is determined
#' by two parameters: the reproductive efficiency `erepro` and the maximum
#' reproduction rate `R_max`.
#'
#' If no maximum is imposed on the reproduction rate
#' (\eqn{R_{max} = \infty}{R_max = Inf}) then the resulting density-independent
#' reproduction rate \eqn{R_{di}}{R_di} is proportional
#' to the total rate \eqn{E_R} at which energy is invested into reproduction,
#' \deqn{R_{di} = \frac{\text{erepro}}{2 w_{min}} E_R,}{R_di = (erepro/(2 w_min)) E_R,}
#' where the proportionality factor is given by the reproductive efficiency
#' `erepro` divided by the egg size `w_min` to convert energy to egg number and
#' divided by 2 to account for the two sexes.
#'
#' Imposing a finite maximum reproduction rate \eqn{R_{max}}{R_max} leads to a
#' non-linear relationship between energy invested and eggs hatched. This
#' density-dependent reproduction rate \eqn{R_{dd}}{R_dd} is given as
#' \deqn{R_{dd} = R_{di}
#' \frac{R_{max}}{R_{di} + R_{max}}.}{R_dd = R_di R_max/(R_di + R_max).}
#'
#' (All quantities in the above equations are species-specific but we dropped
#' the species index for simplicity.)
#'
#' The following plot illustrates the Beverton-Holt density dependence in the
#' reproduction rate for two different choices of parameters.
#'
#' ```{r Beverton-Holt-plot, echo = FALSE, fig.height = 2.5, fig.width = 5}
#' erepro <- 4
#' R_max <- 1
#' E_R <- seq(0, 2, by = 0.05)
#' R_di = erepro * E_R
#' R_dd <- R_di * R_max / (R_di + R_max)
#' df <- melt(data.frame(E_R, R_dd, R_di, R_max), id.vars = "E_R")
#' df <- df[df$value < 1.6, ]
#' df$dd <- "Low"
#'
#' erepro <- 1.5
#' R_max <- 3/2
#' R_di = erepro * E_R
#' R_dd <- R_di * R_max / (R_di + R_max)
#' df2 <- melt(data.frame(E_R, R_dd, R_di, R_max), id.vars = "E_R")
#' df2 <- df2[df2$value < 1.6, ]
#' df2$dd <- "High"
#'
#' ggplot(rbind(df, df2)) +
#'     geom_line(aes(x = E_R, y = value, linetype = variable,
#'                   colour = dd, size = dd)) +
#'     geom_point(aes(x = 5/4, y = 5/6), size = 2) +
#'     labs(linetype = "", size = "R_max", colour = "R_max") +
#'     scale_y_continuous(name = "Reproduction rate [eggs/year]",
#'                        breaks = c(5/6), labels = c("R_dd")) +
#'     scale_x_continuous(name = "Energy invested [g/year]",
#'                        breaks = c(5/4), labels = c("E_R")) +
#'     scale_size_manual(values = c(1, 0.5)) +
#'     scale_colour_manual(values = c("black", "blue")) +
#'     scale_linetype_manual(values = c("solid", "dashed", "dotted"))
#' ```
#'
#' This plot shows that a given energy \eqn{E_R} invested into reproduction can
#' lead to the same reproduction rate \eqn{R_{dd}}{R_dd} with different choices
#' of the parameters `R_max` and `erepro`. `R_max` determines the asymptote of
#' the curve and `erepro` its initial slope. A higher `R_max` coupled with a
#' lower `erepro` (black curves) can give the same value as a lower `R_max`
#' coupled with a higher `erepro` (blue curves).
#'
#' For the given initial state in the MizerParams object `params` one can
#' calculate the energy \eqn{E_R} that is invested into reproduction by the
#' mature individuals and the reproduction rate \eqn{R_{dd}}{R_dd} that is
#' required to keep the egg abundance constant. These two values determine the
#' location of the black dot in the above graph. You then only need one
#' parameter to select one curve from the family of Beverton-Holt curves going
#' through that point. This parameter can be `erepro` or `R_max`. Instead of
#' `R_max` you can alternatively specify the `reproduction_level` which is the
#' ratio between the density-dependent reproduction rate \eqn{R_{dd}}{R_dd} and
#' the maximal reproduction rate  \eqn{R_{max}}{R_max}.
#'
#' The parameter you provide can be either a vector, with one value for each
#' species, or a single value that is then used for all species. The
#' values for `R_max` must be larger than \eqn{R_{dd}}{R_dd} and can range
#' up to `Inf`. The values for the `reproduction_level` must be positive and
#' less than 1. The values for `erepro` must be large enough to allow the
#' required reproduction rate. They should also be smaller than 1 to be
#' physiologically sensible, but this is not enforced by the function.
#'
#' As can be seen in the graph above, choosing a lower value for `R_max` or a
#' higher value for `erepro` means that near the steady state the reproduction
#' will be less sensitive to a change in the energy invested into reproduction
#' and hence less sensitive to changes in the spawning stock biomass or its
#' energy income. As a result the species will also be less sensitive to
#' fishing, leading to a higher F_MSY.
#'
#' @param params A MizerParams object
#' @param erepro Reproductive efficiency for each species. See details.
#' @param R_max Maximum reproduction rate. See details.
#' @param reproduction_level Sets at which proportion of the maximum
#'   reproduction rate the actual reproduction takes place in the initial state.
#'   See details.
#' @param R_factor `r lifecycle::badge("deprecated")` Use
#'   `reproduction_level = 1 / R_factor` instead.
#'
#' @return A MizerParams object
#' @export
setBevertonHolt <- function(params, erepro, R_max, reproduction_level,
                            R_factor) {
    assert_that(is(params, "MizerParams"))
    no_sp <- nrow(params@species_params)
    # check that there are only two arguments
    if (hasArg("erepro") + hasArg("R_max") + hasArg("reproduction_level") +
        hasArg("R_factor") != 1) {
        stop("You should only provide `params` and one other argument.")
    }

    rdd <- getRDD(params)
    rdi <- getRDI(params)
    if (any(rdi == 0)) {
        stop("Some species have no reproduction.")
    }
    params@rates_funcs$RDD <- "BevertonHoltRDD"

    # Calculate required rdd
    mumu <- getMort(params)
    gg <- getEGrowth(params)
    rdd_new <- getRequiredRDD(params)

    if (!missing(erepro)) {
        if (length(erepro) != 1 && length(erepro) != no_sp) {
            stop("`erepro` needs to be a vector of length ", no_sp,
                 " or a single number.")
        }
        rdi_new <- rdi * erepro / params@species_params$erepro
        wrong <- rdi_new < rdd_new
        if (any(wrong)) {
            stop("For the following species the `erepro` you have provided",
                 "is too small: ", params@species_params$species[wrong])
        }
        r_max_new <- rdi_new * rdd_new / (rdi_new - rdd_new)
        r_max_new[is.nan(r_max_new)] <- Inf
        params@species_params$erepro <- erepro
        params@species_params$R_max <- r_max_new
        return(params)
    }

    # We now know that we are setting R_max, which however can have been
    # specified in different ways.
    if (!missing(reproduction_level)) {
        if (length(reproduction_level) != 1 && length(reproduction_level) != no_sp) {
            stop("`reproduction_level` needs to be a vector of length ", no_sp,
                 " or a single number.")
        }
        if (!all(reproduction_level > 0 & reproduction_level < 1)) {
            stop("The reproduction level must be a number strictly between 0 and 1.")
        }
        r_max_new <- rdd_new / reproduction_level
    }
    if (!missing(R_factor)) {
        if (length(R_factor) != 1 && length(R_factor) != no_sp) {
            stop("`R_factor` needs to be a vector of length ", no_sp,
                 " or a single number.")
        }
        if (!all(R_factor > 1)) {
            stop("The R_factor must be greater than 1.")
        }
        r_max_new <- rdd_new * R_factor
    }
    if (!missing(R_max)) {
        if (length(R_max) != 1 && length(R_max) != no_sp) {
            stop("`R_max` needs to be a vector of length ", no_sp,
                 " or a single number.")
        }
        wrong <- R_max < rdd_new
        if (any(wrong)) {
            stop("For the following species the `R_max` you have provided is
                 too small: ", params@species_params$species[wrong])
        }
        r_max_new <- R_max
    }

    rdi_new <- rdd_new / (1 - rdd_new / r_max_new)
    params@species_params$R_max <- r_max_new
    params@species_params$erepro <- params@species_params$erepro *
        rdi_new / rdi
    wrong <- params@species_params$erepro > 1
    if (any(wrong)) {
        warning("The following species require an unrealistic reproductive ",
                "efficiency greater than 1: ",
                params@species_params$species[wrong])
    }

    return(params)
}

getRequiredRDD <- function(params) {
    # Calculate required rdd
    mumu <- getMort(params)
    gg <- getEGrowth(params)
    rdd_new <- getRDD(params) # to get the right structure
    for (i in seq_len(nrow(params@species_params))) {
        gg0 <- gg[i, params@w_min_idx[i]]
        mumu0 <- mumu[i, params@w_min_idx[i]]
        DW <- params@dw[params@w_min_idx[i]]
        n0 <- params@initial_n[i, params@w_min_idx[i]]
        rdd_new[i] <- n0 * (gg0 + DW * mumu0)
    }
    rdd_new
}
