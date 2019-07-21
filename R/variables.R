# sets variable (water balance value stored in wateres object) or property (reservoir characteristics stored in attributes of the wateres object)
# values of properties are not dependent on time step (no conversions needed)
# is_storage_property set to TRUE will require and produce time series of length of reservoir variables plus one
# only_one_value only one constant value is required (e.g. for initial storage)
set_variable <- function(reser, values, variable, allow_one_value = FALSE, is_property = FALSE, is_storage_property = FALSE, only_one_value = FALSE) {
    if (allow_one_value && !only_one_value && length(values) == 1) {
        values = rep(values, 12)
    }
    required_length = ifelse(is_storage_property, nrow(reser) + 1, ifelse(only_one_value, 1, nrow(reser)))
    if (length(values) != required_length) {
        var_names = c(E = "evaporation", W = "wateruse", P = "precipitation")
        var_name = var_names[variable]
        if (is.na(var_name)) {
            var_name = variable
        }
        if (only_one_value) {
            stop("Property ", var_name, " has to be set as only one value.")
        }
        else if (length(values) == 12) {
            time_step = attr(reser, "time_step_unit")
            if (time_step == "hour")
                stop("Variable ", var_name, " cannot be set by 12 values for hourly data.")
            else if (time_step == "day") {
                if (is.null(reser$DTM))
                    stop("Variable ", var_name, " cannot be set for daily data without specified date.")
                else if (attr(reser, "time_step_len") != 1)
                    stop("Variable ", var_name, " cannot be set for data with an arbitrary length of daily time step.")
            }
            months = as.integer(format(reser$DTM, "%m"))
            if (is_storage_property) {
                last_dtm = seq(reser$DTM[nrow(reser)], by = paste0(time_step, "s"), length.out = 2)[2]
                months = c(months, as.integer(format(last_dtm, "%m")))
            }
            values = values[months]
            if (time_step == "day" && !is_property) {
                values = values / days_for_months(reser$DTM)
            }
        }
        else {
            text_const = ifelse(allow_one_value, " or one constant value", "")
            stop("Incorrect length of ", var_name, ": length of time series or 12 (monthly values)", text_const, " required.")
        }
    }
    if (is_property) {
        attr(reser, variable) = values
    }
    else {
        reser[[variable]] = values
    }
    return(reser)
}

#' @rdname set_evaporation.wateres
#' @export
set_evaporation <- function(reser, values, altitude, plant_cover) UseMethod("set_evaporation")

#' Evaporation setting or calculation
#'
#' For monthly or daily data, sets or calculates time series of evaporation from the reservoir.
#'
#' @param reser A \code{wateres} object.
#' @param values A vector of evaporation values in mm, either monthly or daily values of length of reservoir time series, or 12 monthly values starting by January.
#' @param altitude Reservoir altitude (m.a.s.l.) used for calculation of monthly evaporation values according to the Czech Technical Standard
#'   ČSN 75 2405, where evaporation is a function of altitude for range from 100 to 1200 m.
#' @param plant_cover Part of flooded area covered with plants, optional number between 0 and 0.75.
#' @return A modified \code{wateres} object with evaporation time series added (denoted as \code{E}).
#' @details Evaporation is applied when calculating reservoir water balance. If no elevation-area-storage relationship is provided for the reservoir,
#'   evaporation only for flooded area related to the potential storage is assumed. Otherwise, evaporation is calculated for the area interpolated
#'   linearly by using the area-storage relationship (or area equal to the one of the limit value if storage fall out of the relationship limits).
#'
#'   If the \code{plant_cover} argument is given, evaporation will be increased by plant transpiration as given by Vrána and Beran (1998). When the reservoir
#'   is getting empty, it is assumed that the plant cover area corresponds with the shallowest parts of the reservoir and therefore the coefficient increasing
#'   evaporation is adjusted accordingly.
#'
#'   An error occurs if data in \code{reser} are not in monthly or daily time step of length one or if no dates are associated with daily data.
#' @references ČSN 72 2405; Vrána, Karel, and Beran, Jan: Rybníky a účelové nádrže, ČVUT, Praha, 1998 (in Czech)
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
#' resul = calc_series(reser, storage = 21e3, yield = 0.14)
set_evaporation.wateres <- function(reser, values = NULL, altitude = NULL, plant_cover = NULL) {
    if (attr(reser, "time_step_unit") == "hour")
        stop("Evaporation cannot be set for hourly data.")
    if (!is.null(altitude)) {
        E_annual = 4.957651e-5 * altitude ^ 2 - 0.3855958 * altitude + 871.19424
        E_monthly = c(0.01, 0.02, 0.06, 0.09, 0.12, 0.14, 0.16, 0.15, 0.11, 0.07, 0.05, 0.02)
        values = E_annual * E_monthly
    }
    if (!is.null(plant_cover)) {
        if (plant_cover < 0 || plant_cover > 0.75)
            stop("Plant cover value has to be between 0 and 0.75.")
        attr(reser, "plant_cover") = plant_cover
    }
    reser = set_variable(reser, values, "E")
    return(reser)
}

#' @rdname set_wateruse.wateres
#' @export
set_wateruse <- function(reser, values) UseMethod("set_wateruse")

#' Water use setting
#'
#' Sets time series of water use for the reservoir.
#'
#' Water use is applied when calculating reservoir water balance in two ways: positive water use (release) is always added to the storage,
#' whereas negative water use (withdrawal) is considered after the yield and evaporation demands are satisfied.
#' @param reser A \code{wateres} object.
#' @param values A vector of water use values in m3, either monthly or daily of length of reservoir time series, or 12 monthly values
#'   starting by January (for monthly or daily time step of length one only), or one constant value. Positive values mean water release
#'   to the reservoir, negative withdrawal from the reservoir.
#' @return A modified \code{wateres} object with water use time series added (denoted as \code{W}).
#' @export
#' @examples
#' reser = data.frame(
#'     Q = c(0.078, 0.065, 0.168, 0.711, 0.154, 0.107, 0.068, 0.057, 0.07, 0.485, 0.252, 0.236,
#'           0.498, 0.248, 0.547, 0.197, 0.283, 0.191, 0.104, 0.067, 0.046, 0.161, 0.16, 0.094),
#'     DTM = seq(as.Date("2000-01-01"), by = "months", length.out = 24))
#' reser = as.wateres(reser, storage = 14.4e6, area = 754e3)
#' sry(reser, storage = 21e3, yield = 0.14)
#' reser = set_wateruse(reser, -1 * c(7, 14, 40, 62, 82, 96, 109, 102, 75, 48, 34, 13))
#' resul = calc_series(reser, storage = 21e3, yield = 0.14)
set_wateruse.wateres <- function(reser, values) {
    reser = set_variable(reser, values, "W", TRUE)
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
#' @param values A vector of precipitation values in mm, either monthly or daily of length of reservoir time series, or 12 monthly values
#'   starting by January (for monthly or daily data only).
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
#' resul = calc_series(reser, storage = 21e3, yield = 0.17)
set_precipitation.wateres <- function(reser, values) {
    reser = set_variable(reser, values, "P")
    return(reser)
}

#' @rdname set_property.wateres
#' @export
set_property <- function(reser, property_name, values) UseMethod("set_property")

#' Reservoir property setting
#'
#' Sets a value or time series of values of a reservoir property, i.e. characteristics which affect water balance calculation.
#'
#' @param reser A `wateres` object.
#' @param property_name One of \dQuote{storage} (maximum storage), \dQuote{storage_optim} (optimum storage), \dQuote{storage_initial} (initial storage),
#'   \dQuote{yield} (reservoir yield) or \dQuote{yield_max} (maximum yield relevant if optimum, but not maximum storage is exceeded).
#' @param values One constant value (mandatory for initial storage) or a vector of property values (in m3 for storages or m3.s-1 for yields), either monthly or daily
#'   of length of reservoir time series (or plus one in case of storages), or 12 monthly values starting by January (for monthly or daily data only).
#'   For storages, the values relate to the beginning of the particular month (e.g. the first of 12 values relates to the beginning of January, i.e. the end of December).
#' @return A modified `wateres` object with the property added as its attribute.
#' @details The reservoir properties are implemented as attributes of the `wateres` object.
#'
#'   If \dQuote{storage} is given, it replaces the value provided as the `storage` argument of the `as.wateres` function. However, \dQuote{yield} or \dQuote{storage_initial}
#'   given as a property is overriden by the `yield` or `initial_storage` argument of the `calc_series` function.
#' @export
#' @examples
#' reser = data.frame(
#'     Q = c(0.078, 0.065, 0.168, 0.711, 0.154, 0.107, 0.068, 0.057, 0.07, 0.485, 0.252, 0.236,
#'           0.498, 0.248, 0.547, 0.197, 0.283, 0.191, 0.104, 0.067, 0.046, 0.161, 0.16, 0.094),
#'     DTM = seq(as.Date("2000-01-01"), by = "months", length.out = 24))
#' reser = as.wateres(reser, storage = 14.4e6, area = 754e3)
#' reser = set_property(reser, "storage", 1e6 * c(10, 10, 10, 14, 14, 14, 12, 12, 12, 6, 6, 6))
#' resul = calc_series(reser, yield = 0.17)
#' @md
set_property.wateres <- function(reser, property_name, values) {
    if (!property_name %in% c("storage", "storage_optim", "storage_initial", "yield", "yield_max")) {
        stop("Unknown property '", property_name, "'.")
    }
    reser = set_variable(reser, values, property_name, TRUE, TRUE, property_name %in% c("storage", "storage_optim"), property_name %in% c("storage_initial"))
    return(reser)
}
