#' Characteristics and Simulations for Water Reservoirs
#'
#' Tools to calculate characteristics of water reservoirs and to perform simulations for them.
#'
#' A reservoir has to be created by \code{\link{as.wateres}} by using time series of dates
#' and corresponding flows. After that, characteristics for the reservoir can be calculated.
#'
#' @docType package
#' @name wateres
#' @import data.table
#' @importFrom Rcpp sourceCpp
#' @useDynLib wateres
#' @examples
#' reser = data.frame(
#'     Q = c(0.078, 0.065, 0.168, 0.711, 0.154, 0.107, 0.068, 0.057, 0.07, 0.485, 0.252, 0.236,
#'           0.498, 0.248, 0.547, 0.197, 0.283, 0.191, 0.104, 0.067, 0.046, 0.161, 0.16, 0.094),
#'     DTM = seq(as.Date("2000-01-01"), by = "months", length.out = 24))
#' reser = as.wateres(reser, Vpot = 14.4, area = 0.754)
#' reser = set_evaporation(reser, altitude = 529)
#' summary(reser)
#' sry(reser, reliab = 0.9, yield = 0.14)
#' prob_field = prob_field(reser, c(0.1, 0.9, 0.99), 0.14)
#' plot(prob_field, "storage")
#' plot(alpha_beta(reser))
NULL

.onUnload <- function (libpath) {
  library.dynam.unload("wateres", libpath)
}

days_in_month <- function(date) {
    mon = format(date, format = "%m")
    while (format(date, format = "%m") == mon) {
        date = date + 1
    }
    return(as.integer(format(date - 1, format = "%d")))
}

#' Water reservoir creation
#'
#' Creates a wateres object from provided time series.
#'
#' @param dframe A name of file containing table with data (including header) or directly data frame or data table.
#'   The data need to consist of monthly flows in m3.s-1 (\dQuote{Q} column) and dates (\dQuote{DTM} column).
#'   Alternatively, this can be a Bilan object where the dates and modelled or observed runoffs are read from.
#'   In that case, catchment area needs to be specified within the Bilan object.
#' @param Vpot Potential storage of the reservoir in millions of m3.
#' @param area Flooded area of the reservoir for the potential storage in km2.
#' @param observed Only when Bilan object is used; whether to read observed runoffs from the object (otherwise modelled are read).
#' @return A wateres object which is also of data.frame and data.table classes.
#' @details An error occurs if \dQuote{Q} or \dQuote{DTM} column is missing or \code{dframe} is of another class
#'   than \code{data.frame} or \code{data.table}.
#' @export
#' @examples
#' reser = data.frame(
#'     Q = c(0.078, 0.065, 0.168, 0.711, 0.154, 0.107, 0.068, 0.057, 0.07, 0.485, 0.252, 0.236,
#'           0.498, 0.248, 0.547, 0.197, 0.283, 0.191, 0.104, 0.067, 0.046, 0.161, 0.16, 0.094),
#'     DTM = seq(as.Date("2000-01-01"), by = "months", length.out = 24))
#' reser = as.wateres(reser, Vpot = 14.4, area = 0.754)
as.wateres <- function(dframe, Vpot, area, observed = FALSE) {
    if ("bilan" %in% class(dframe)) {
        if (requireNamespace("bilan", quietly = TRUE)) {
            catch_area = bilan::bil.get.area(dframe)
            if (!(catch_area > 0))
                stop("Catchment area needs to be specified when using Bilan data.")
            data = bilan::bil.get.data(dframe)
            Qvar = ifelse(observed, "R", "RM")
            days = sapply(data$DTM, days_in_month)
            dframe = data.frame(DTM = data$DTM, Q = data[[Qvar]] * catch_area / (24 * 3.6 * days))
        }
        else {
            stop("Bilan package is needed to load data from object of \"bilan\" class.")
        }
    }
    else if ("character" %in% class(dframe))
        dframe = read.table(dframe, header = TRUE)
    else if (!"data.frame" %in% class(dframe) && !"data.table" %in% class(dframe))
        stop("To create a reservoir, data.frame or data.table is required.")
    required_cols = c("Q", "DTM")
    for (colname in required_cols) {
        if (!colname %in% colnames(dframe))
            stop(paste0("To create a reservoir, ", colname, " column is required."))
    }
    dframe = dframe[, required_cols]
    dframe$DTM = as.Date(dframe$DTM)
    dframe$.days = sapply(dframe$DTM, days_in_month)
    dframe$Q = as.numeric(dframe$Q)
    class(dframe) = c("wateres", "data.table", "data.frame")
    attr(dframe, "Vpot") = Vpot
    attr(dframe, "area") = area
    return(dframe)
}

#' Water reservoir summary
#'
#' Calculates and shows characteristics of the reservoir.
#'
#' @param object A wateres object.
#' @param ... Further arguments are not used.
#' @param upper_limit An upper limit of yield (as multiple of the mean annual flow) for optimization as in the \code{\link{sry.wateres}} function.
#' @return A vector of reservoir characteristics:
#'   \item{Vpot}{potential reservoir storage in millions of m3 (given as a parameter of \code{\link{as.wateres}})}
#'   \item{Qn_max}{the maximum yield (m3.s-1) for 100\% reliability for given potential storage}
#'   \item{alpha}{level of development - ratio Qn_max to the mean annual flow}
#'   \item{m}{resilience index - a measure of flow variability calculated as (1 - alpha) / (standard deviation of annual flows / mean annual flow)}
#' @details The maximum yield is calculated by using the \code{\link{sry.wateres}} function for potential storage and reliability 1.
#'
#'   An error occurs if the range given by \code{upper_limit} does not contain value of 100\% reliability.
#' @export
#' @examples
#' reser = data.frame(
#'     Q = c(0.078, 0.065, 0.168, 0.711, 0.154, 0.107, 0.068, 0.057, 0.07, 0.485, 0.252, 0.236,
#'           0.498, 0.248, 0.547, 0.197, 0.283, 0.191, 0.104, 0.067, 0.046, 0.161, 0.16, 0.094),
#'     DTM = seq(as.Date("2000-01-01"), by = "months", length.out = 24))
#' reser = as.wateres(reser, Vpot = 14.4, area = 0.754)
#' summary(reser)
summary.wateres <- function(object, ..., upper_limit = 5) {
    Qn_max = sry(object, reliability = 1, empirical_rel = FALSE, upper_limit = upper_limit)$yield
    Qa = mean(object$Q)
    alpha = Qn_max / Qa
    Qyears = object[, list(Q = mean(Q)), by = year(DTM)]
    m = (1 - alpha) / (sd(Qyears$Q) / Qa)
    print(c(Vpot = attr(object, "Vpot"), Qn_max = Qn_max, alpha = alpha, m = m))
}

# bisection for monotonic function
bisection <- function(func, interval, max_iter = 500, tolerance = 1e-5, ...) {
    lower = min(interval)
    upper = max(interval)
    flower = func(lower, ...)
    fupper = func(upper, ...)
    if (flower == fupper && flower == 0)
        return(c(lower, 0))
    else if (flower * fupper > 0) {
        stop("Required reliability is not contained within the given interval.")
    }
    else {
        for (i in 1:max_iter) {
            if (upper - lower < tolerance)
                break
            mid = (lower + upper) / 2
            fmid = func(mid, ...)
            if (flower * fmid <= 0 && !(flower == 0 && fmid == 0)) {
                upper = mid
                fupper = fmid
            }
            else {
                lower = mid
                flower = fmid
            }
        }
        if (abs(flower) < abs(fupper))
            return(c(lower, flower))
        else
            return(c(upper, fupper))
    }
}

get_reser_variables <- function(reser) {
    tmp = list()
    for (var in c("E", "W")) {
        if (!var %in% names(reser))
            tmp[[var]] = rep(0, nrow(reser))
        else
            tmp[[var]] = reser[[var]]
    }
    return(tmp)
}

calc_reliability <- function(reser, storage_req, yield_req, empirical, throw_exceed) {
    tmp = get_reser_variables(reser)
    resul = .Call("calc_storage", PACKAGE = "wateres", reser$Q, reser$.days, tmp$E, tmp$W, yield_req, storage_req, attr(reser, "area"), throw_exceed)

    if (empirical)
        coeff = c(m = -0.3, n = 0.4)
    else
        coeff = c(m = 0, n = 0)
    reliab = (sum(resul$yield + .Machine$double.eps ^ 0.5 > yield_req) + coeff["m"]) / (length(resul$yield) + coeff["n"])
    names(reliab) = NULL
    return(reliab)
}

# absolute difference between calculated and required reliability used for optimization
calc_diff_reliability <- function(x, reser, storage_req, reliab_req, yield_req, empirical, throw_exceed) {
    if (missing(storage_req))
        storage_req = x
    else if (missing(yield_req))
        yield_req = x

    reliab = calc_reliability(reser, storage_req, yield_req, empirical, throw_exceed)
    return(reliab - reliab_req)
}

is_reliab_equal <- function(value, resul_value, reser, yield_req, empirical, throw_exceed) {
    if (calc_reliability(reser, value, yield_req, empirical, throw_exceed) == resul_value)
        return(0.5)
    else
        return(-1)
}

#' @rdname sry.wateres
#' @export
sry <- function(reser, storage, reliability, yield, empirical_rel, upper_limit, throw_exceed) UseMethod("sry")

#' Calculation of storage, reliability and yield
#'
#' Calculates one of the water reservoir storage, time-based reliability and yield (release) variable while the
#' two remaining values are provided.
#'
#' @param reser A wateres object.
#' @param storage A water reservoir storage value in millions of m3. (If missing together with reliability or yield, the default value
#'   equal to the potential volume of \code{reser} will be used. If only storage is missing, it will be optimized using reliability
#'   and yield.)
#' @param reliability A reliability value, cannot be less than zero or greater than maximum reliability value
#'   (depending on data and usage of empirical reliability). (If missing, it will be calculated using storage and yield.)
#' @param yield A required yield in m3.s-1, constant for all months. (If missing, it will be optimized using storage and reliability.)
#' @param empirical_rel Whether empirical probability (by (m - 0.3) / (n + 0.4) formula) will be used for calculation
#'   of reliability. If enabled, maximum reliability value will be less than 1.
#' @param upper_limit An upper limit for optimization of storage or yield given as multiple of potential volume of the reservoir
#'   (for storage) or as multiple of mean monthly flow (for yield).
#' @param throw_exceed Whether volume exceeding storage will be thrown or added to yield. This will affect calculated yield series,
#'   however resulting storage, reliability or yield value is not likely to be influenced.
#' @return A list consisting of:
#'   \item{storage}{storage value, optimized, equal to the \code{storage} argument or default (potential volume of \code{reser})}
#'   \item{reliability}{reliability value calculated for the given or optimized values of yield and storage}
#'   \item{yield}{yield value, optimized or equal to the \code{yield} argument}
#' @details To optimize the value of storage or yield, a simple bisection algorithm is applied. If the optimization fails because
#'   the required reliability is not contained within the provided interval, try to change its upper limit given as the \code{upper_limit}
#'   argument.
#'
#'   As the required reliability represents a range of storage or yield values, the smallest value of storage (or the greatest value
#'   of yield) is returned, considering some tolerance value of the optimization algorithm.
#' @export
#' @examples
#' reser = data.frame(
#'     Q = c(0.078, 0.065, 0.168, 0.711, 0.154, 0.107, 0.068, 0.057, 0.07, 0.485, 0.252, 0.236,
#'           0.498, 0.248, 0.547, 0.197, 0.283, 0.191, 0.104, 0.067, 0.046, 0.161, 0.16, 0.094),
#'     DTM = seq(as.Date("2000-01-01"), by = "months", length.out = 24))
#' reser = as.wateres(reser, Vpot = 14.4, area = 0.754)
#' sry(reser, reliab = 0.9, yield = 0.14)
#' sry(reser, storage = 0.041, yield = 0.14)
#' sry(reser, yield = 0.14)
#' sry(reser, storage = 0.041, reliab = 0.5)
sry.wateres <- function(reser, storage, reliability, yield, empirical_rel = TRUE, upper_limit = 5, throw_exceed = FALSE) {
    if (!missing(reliability)) {
        max_reliab = calc_reliability(reser, 1, 0, empirical_rel, throw_exceed)
        if (reliability > max_reliab || reliability < 0)
            stop("Invalid value of reliability.")
    }
    if (missing(storage) && (missing(reliability) || missing(yield)))
        storage = attr(reser, "Vpot")
    if (missing(storage) || missing(yield)) {
        upper_limit = ifelse(missing(storage), upper_limit * attr(reser, "Vpot"), upper_limit * mean(reser$Q))
        if (missing(storage)) {
            resul = bisection(calc_diff_reliability, c(0, upper_limit), reser = reser, reliab_req = reliability, yield_req = yield,
                empirical = empirical_rel, throw_exceed = throw_exceed)
            reliab_neighbour = calc_reliability(reser, resul[1] + 1e-5, yield, empirical_rel, throw_exceed)
            missing = "storage"
        }
        else {
            resul = bisection(calc_diff_reliability, c(0, upper_limit), reser = reser, reliab_req = reliability, storage_req = storage,
                empirical = empirical_rel, throw_exceed = throw_exceed)
            reliab_neighbour = calc_reliability(reser, storage, resul[1] - 1e-5, empirical_rel, throw_exceed)
            missing = "yield"
        }
        reliab_found = resul[2] + reliability
        # the greatest storage or the smallest yield for the same reliability found -> find the smallest or the greatest one
        if (reliab_found != reliab_neighbour) {
            if (missing == "storage") {
                resul = bisection(is_reliab_equal, c(0, resul[1]), resul_value = reliab_found, reser = reser, yield_req = yield,
                    empirical = empirical_rel, throw_exceed = throw_exceed)
            }
            else {
                resul = bisection(is_reliab_equal, c(resul[1], upper_limit), resul_value = reliab_found, reser = reser, storage_req = storage,
                    empirical = empirical_rel, throw_exceed = throw_exceed)
            }
        }
        assign(missing, resul[1])
    }
    reliability = calc_reliability(reser, storage, yield, empirical_rel, throw_exceed)
    return(list(storage = storage, reliability = reliability, yield = yield))
}
