set_variable <- function(reser, values, variable) {
    if (length(values) != nrow(reser)) {
        var_names = c(E = "evaporation", W = "withdrawal", P = "precipitation")
        if (length(values) == 12) {
            if (attr(reser, "time_step") != "month")
                stop("Variable ", var_names[variable], " can be set by 12 values only for monthly data.")
            first_month = as.integer(format(reser$DTM[1], "%m"))
            values = rep(values, 2)[first_month:(first_month + 11)]
            values = rep_len(values, nrow(reser))
        }
        else {
            text_const = ifelse(variable == "W", " or one constant value", "")
            stop("Incorrect length of ", var_names[variable], " length of time series or 12 (monthly values)", text_const, " required.")
        }
    }
    reser[[variable]] = values
    return(reser)
}

#' @rdname set_evaporation.wateres
#' @export
set_evaporation <- function(reser, values, altitude) UseMethod("set_evaporation")

#' Evaporation setting or calculation
#'
#' For monthly data, sets or calculates time series of evaporation from the reservoir.
#'
#' @param reser A \code{wateres} object.
#' @param values A vector of monthly evaporation values in mm, either of length of reservoir time series, or 12 monthly values starting by January.
#' @param altitude Reservoir altitude (m.a.s.l.) used for calculation of monthly evaporation values according to the Czech Technical Standard
#'   ČSN 75 2405, where evaporation is a function of altitude for range from 100 to 1200 m.
#' @return A modified \code{wateres} object with evaporation time series added (denoted as \code{E}).
#' @details Evaporation is applied when calculating reservoir water balance. If no elevation-area-storage relationship is provided for the reservoir,
#'   evaporation only for flooded area related to the potential storage is assumed. Otherwise, evaporation is calculated for the area interpolated
#'   linearly by using the area-storage relationship (or area equal to the one of the limit value if storage fall out of the relationship limits).
#'
#'   An error occurs if data in \code{reser} are not monhtly.
#' @references ČSN 72 2405
#' @export
#' @examples
#' reser = data.frame(
#'     Q = c(0.078, 0.065, 0.168, 0.711, 0.154, 0.107, 0.068, 0.057, 0.07, 0.485, 0.252, 0.236,
#'           0.498, 0.248, 0.547, 0.197, 0.283, 0.191, 0.104, 0.067, 0.046, 0.161, 0.16, 0.094),
#'     DTM = seq(as.Date("2000-01-01"), by = "months", length.out = 24))
#' reser = as.wateres(reser, storage = 14.4e6, area = 754e3)
#' sry(reser, storage = 21e3, yield = 0.14)
#' reser = set_evaporation(reser, c(7, 14, 40, 62, 82, 96, 109, 102, 75, 48, 34, 13))
#' reser = set_evaporation(reser, altitude = 529)
#' sry(reser, storage = 21e3, yield = 0.14)
set_evaporation.wateres <- function(reser, values = NULL, altitude = NULL) {
    if (attr(reser, "time_step") != "month")
        stop("Evaporation can be set only for monthly data.")
    if (!is.null(altitude)) {
        E_annual = 4.957651e-5 * altitude ^ 2 - 0.3855958 * altitude + 871.19424
        E_monthly = c(0.01, 0.02, 0.06, 0.09, 0.12, 0.14, 0.16, 0.15, 0.11, 0.07, 0.05, 0.02)
        values = E_annual * E_monthly
    }
    reser = set_variable(reser, values, "E")
    return(reser)
}

#' @rdname set_withdrawal.wateres
#' @export
set_withdrawal <- function(reser, values) UseMethod("set_withdrawal")

#' Withdrawal setting
#'
#' Sets time series of withdrawal from the reservoir.
#'
#' @param reser A \code{wateres} object.
#' @param values A vector of withdrawal values in m3, either of length of reservoir time series, or 12 monthly values starting by January
#'    (for monthly data only), or one constant value.
#' @return A modified \code{wateres} object with withdrawal time series added (denoted as \code{W}).
#' @details Withdrawal is applied when calculating reservoir water balance after the yield and evaporation demands are satisfied.
#' @export
#' @examples
#' reser = data.frame(
#'     Q = c(0.078, 0.065, 0.168, 0.711, 0.154, 0.107, 0.068, 0.057, 0.07, 0.485, 0.252, 0.236,
#'           0.498, 0.248, 0.547, 0.197, 0.283, 0.191, 0.104, 0.067, 0.046, 0.161, 0.16, 0.094),
#'     DTM = seq(as.Date("2000-01-01"), by = "months", length.out = 24))
#' reser = as.wateres(reser, storage = 14.4e6, area = 754e3)
#' sry(reser, storage = 21e3, yield = 0.14)
#' reser = set_withdrawal(reser, c(7, 14, 40, 62, 82, 96, 109, 102, 75, 48, 34, 13))
#' sry(reser, storage = 21e3, yield = 0.14)
set_withdrawal.wateres <- function(reser, values) {
    if (length(values) == 1)
        values = rep(values, 12)
    reser = set_variable(reser, values, "W")
    return(reser)
}

#' @rdname set_precipitation.wateres
#' @export
set_precipitation <- function(reser, values) UseMethod("set_precipitation")

#' Precipitation setting
#'
#' Sets time series of precipitation on the reservoir area.
#'
#' @param reser A \code{wateres} object.
#' @param values A vector of precipitation values in mm, either of length of reservoir time series, or 12 monthly values starting by January
#'    (for monthly data only).
#' @return A modified \code{wateres} object with precipitation time series added (denoted as \code{P}).
#' @details Precipitation is applied when calculating reservoir water balance. When calculating precipitation volume, the flooded area related
#'   to the potential storage is always used.
#' @export
#' @examples
#' reser = data.frame(
#'     Q = c(0.078, 0.065, 0.168, 0.711, 0.154, 0.107, 0.068, 0.057, 0.07, 0.485, 0.252, 0.236,
#'           0.498, 0.248, 0.547, 0.197, 0.283, 0.191, 0.104, 0.067, 0.046, 0.161, 0.16, 0.094),
#'     DTM = seq(as.Date("2000-01-01"), by = "months", length.out = 24))
#' reser = as.wateres(reser, storage = 14.4e6, area = 754e3)
#' sry(reser, storage = 21e3, yield = 0.17)
#' reser = set_precipitation(reser, c(55, 40, 44, 43, 81, 72, 85, 84, 52, 54, 48, 58))
#' sry(reser, storage = 21e3, yield = 0.17)
set_precipitation.wateres <- function(reser, values) {
    reser = set_variable(reser, values, "P")
    return(reser)
}
