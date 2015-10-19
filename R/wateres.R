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
#' reser = as.wateres(reser, storage = 14.4e6, area = 754e3)
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
#' @param storage Potential storage of the reservoir in m3.
#' @param area Flooded area of the reservoir for the potential storage in m2.
#' @param eas Elevation-area-storage relationship given as a data frame or data table with the three columns representing
#'   elevation (m.a.s.l.), area (m2) and storage (m3). If values of this three variables are not sorted and their orders
#'   differ, this argument will be ignored.
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
#' eas = data.frame(
#'     elevation = c(496, 502, 511, 520, 529), area = c(0, 58e3, 180e3, 424e3, 754e3),
#'     storage = c(0, 161e3, 1.864e6, 6.362e6, 14.400e6))
#' reser = as.wateres(reser, storage = 14.4e6, area = 754e3, eas = eas)
as.wateres <- function(dframe, storage, area, eas = NULL, observed = FALSE) {
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
    attr(dframe, "storage") = storage
    attr(dframe, "area") = area
    if (!is.null(eas)) {
        if (ncol(eas) != 3)
            warning("Incorrect number of columns for elevation-area-storage relationship.")
        else {
            ref_order = order(eas[[1]])
            if (!(identical(ref_order, order(eas[[2]])) && identical(ref_order, order(eas[[3]]))))
                warning("Elevation-area-storage values are not sorted correctly and will be ignored.")
            eas = as.data.table(eas)
            setnames(eas, c("elevation", "area", "storage"))
            if (!identical(ref_order, 1:length(ref_order)))
                setorder(eas, elevation, area, storage)
            attr(dframe, "eas") = eas
        }
    }
    return(dframe)
}

#' Water reservoir summary
#'
#' Calculates and shows characteristics of the reservoir.
#'
#' @param object A wateres object.
#' @param ... Further arguments passed to the \code{\link{sry.wateres}} function (as \code{prob_type} or \code{upper_limit}).
#' @param reliability A vector of reliability values passed to the \code{\link{sry.wateres}} function.
#' @return A data table of reservoir characteristics:
#'   \item{storage}{potential reservoir storage in m3 (given as a parameter of \code{\link{as.wateres}})}
#'   \item{reliability}{given reliability}
#'   \item{yield}{the maximum yield (m3.s-1) for given reliability and potential storage}
#'   \item{alpha}{level of development - ratio of yield to the mean annual flow}
#'   \item{m}{standardized net inflow - a measure of resilience calculated as (1 - alpha) / (standard deviation of annual flows / mean annual flow)}
#'   \item{resilience}{resilience calculated as number of continuous sequences of failures / total number of time steps with failures, NA for no failure}
#'   \item{vulnerability}{vulnerability (in m3) calculated as mean of monthly deficit volumes that represent maximum deficit of each failure period}
#'   \item{dimless_vulner}{dimensionless vulnerability, vulnerability value divided by yield value in volume units}
#' @details The maximum yield is calculated by using the \code{\link{sry.wateres}} function for potential storage and reliability 1.
#'
#'   An error occurs if the range given by \code{upper_limit} does not contain value of 100\% reliability or if an invalid reliability or probability
#'   type is given.
#' @seealso \code{\link{sry.wateres}} used for optimization of the yield for given reliability
#' @references Thomas A. McMahon, Adebayo J. Adeloye, Sen-Lin Zhou (2006): Understanding performance measures of reservoirs,
#'   Journal of Hydrology 324, 359–382.
#' @export
#' @examples
#' reser = data.frame(
#'     Q = c(0.078, 0.065, 0.168, 0.711, 0.154, 0.107, 0.068, 0.057, 0.07, 0.485, 0.252, 0.236,
#'           0.498, 0.248, 0.547, 0.197, 0.283, 0.191, 0.104, 0.067, 0.046, 0.161, 0.16, 0.094),
#'     DTM = seq(as.Date("2000-01-01"), by = "months", length.out = 24))
#' reser = as.wateres(reser, storage = 14.4e6, area = 754e3)
#' summary(reser, reliability = 1)
#' summary(reser, reliability = 0.95)
summary.wateres <- function(object, ..., reliability = "max") {
    as.data.table(t(sapply(reliability, summary_wateres, object, ...)))
}

# summary for one value of reliability
summary_wateres <- function(reliability, object, ...) {
    resul = sry(object, reliability = reliability, ..., get_series = TRUE)
    yield = resul$yield
    Qa = mean(object$Q)
    alpha = yield / Qa
    Qyears = object[, list(Q = mean(Q)), by = year(DTM)]
    m = (1 - alpha) / (sd(Qyears$Q) / Qa)

    failures = !(resul$series$yield + .Machine$double.eps ^ 0.5 > yield)
    failures_duration = sum(failures)
    if (failures_duration == 0) {
        resilience = vulnerability = dimless_vulner = NA
    }
    else {
        seq_begins = which(diff(failures) == 1) + 1
        if (failures[1]) # to count failure started in step 1
            seq_begins = c(1, seq_begins)
        seq_ends = which(diff(failures) == -1)
        if (failures[length(failures)])
            seq_ends = c(seq_ends, length(failures))
        failures_count = length(seq_begins)
        resilience = failures_count / failures_duration
        deficits = sapply(
            1:failures_count,
            function(i) { max(.Call("convert_m3", PACKAGE = "wateres", yield - resul$series$yield[seq_begins[i]:seq_ends[i]], object$.days[seq_begins[i]:seq_ends[i]], TRUE)) } )
        vulnerability = mean(deficits)
        dimless_vulner = vulnerability / (.Call("convert_m3", PACKAGE = "wateres", yield, 1, TRUE) * 30.5)
    }
    return(
        c(storage = attr(object, "storage"), reliability = resul$reliability, yield = yield, alpha = alpha, m = m, resilience = resilience,
        vulnerability = vulnerability, dimless_vulner = dimless_vulner))
}

#' @rdname fill_time.wateres
#' @export
fill_time <- function(reser, yield, begins, samples) UseMethod("fill_time")

#' Reservoir filling time
#'
#' Experimental calculation of filling time of the reservoir. A sample of time series is calculated based on the given beginning time steps (alternatively,
#'   these time steps are sampled randomly).
#'
#' @param reser A \code{wateres} object.
#' @param yield A required yield.
#' @param begins A vector of time steps that represent begins of time series to be simulated.
#' @param samples A number of time steps of begins to be randomly sampled.
#' @return A data table of filling times:
#'   \item{begin}{time steps when simulation of time series begins}
#'   \item{months}{number of months during that the reservoir changes from empty to full, or \code{NA} if the reservoir is not full and the end
#'     of the time series is reached}
#' @details If provided, the \code{begins} argument is used to calculate the time series. Alternatively, the \code{samples} argument is applied.
#' @export
#' @examples
#' reser = data.frame(
#'     Q = c(0.078, 0.065, 0.168, 0.711, 0.154, 0.107, 0.068, 0.057, 0.07, 0.485, 0.252, 0.236,
#'           0.498, 0.248, 0.547, 0.197, 0.283, 0.191, 0.104, 0.067, 0.046, 0.161, 0.16, 0.094),
#'     DTM = seq(as.Date("2000-01-01"), by = "months", length.out = 24))
#' reser = as.wateres(reser, storage = 4e5, area = 754e3)
#' fill = fill_time(reser, yield = 0.01)
fill_time.wateres <- function(reser, yield, begins = NULL, samples = 10) {
    if (is.null(begins)) {
        begins = sample(1:nrow(reser), samples)
        begins = c(1, begins)
    }
    fill_months = vector("numeric", length(begins))
    for (beg in 1:length(begins)) {
        tmp_reser = reser[begins[beg]:nrow(reser), ]
        attributes(tmp_reser) = attributes(reser)
        series = calc_series(tmp_reser, attr(tmp_reser, "storage"), yield, FALSE, initial_storage = 0)
        storage_full = !(series$storage < attr(tmp_reser, "storage"))
        if (all(!storage_full))
            fill_months[beg] = NA
        else
            fill_months[beg] = which(storage_full)[1] - 1
    }
    fill_months = fill_months[order(begins)]
    begins = begins[order(begins)]
    return(data.table(begin = begins, months = fill_months))
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

calc_series <- function(reser, storage_req, yield_req, throw_exceed, initial_storage = storage_req) {
    resul = .Call(
        "calc_storage", PACKAGE = "wateres", reser, yield_req, storage_req, initial_storage, throw_exceed)
    return(resul)
}

calc_reliability <- function(yield, yield_req, prob_type) {
    if (prob_type == "chegodayev")
        coeff = c(m = -0.3, n = 0.4)
    else if (prob_type == 4)
        coeff = c(m = 0, n = 0)
    else
        coeff = c(m = -1, n = -1)
    reliab = (sum(yield + .Machine$double.eps ^ 0.5 > yield_req) + coeff["m"]) / (length(yield) + coeff["n"])
    names(reliab) = NULL
    return(reliab)
}

# difference between calculated and required reliability used for optimization
calc_diff_reliability <- function(x, reser, storage_req, reliab_req, yield_req, prob_type, throw_exceed) {
    if (missing(storage_req))
        storage_req = x
    else if (missing(yield_req))
        yield_req = x

    series = calc_series(reser, storage_req, yield_req, throw_exceed)
    reliab = calc_reliability(series$yield, yield_req, prob_type)
    return(reliab - reliab_req)
}

is_reliab_equal <- function(value, resul_value, reser, storage_req, yield_req, prob_type, throw_exceed) {
    if (missing(storage_req))
        storage_req = value
    else
        yield_req = value
    series = calc_series(reser, storage_req, yield_req, throw_exceed)
    if (calc_reliability(series$yield, yield_req, prob_type) == resul_value)
        return(0.5)
    else
        return(-1)
}

#' @rdname sry.wateres
#' @export
sry <- function(reser, storage, reliability, yield, prob_type, upper_limit, throw_exceed, get_series) UseMethod("sry")

#' Calculation of storage, reliability and yield
#'
#' Calculates one of the water reservoir storage, time-based reliability and yield (release) variable while the
#' two remaining values are provided.
#'
#' @param reser A wateres object.
#' @param storage A water reservoir storage value in m3. (If missing together with reliability or yield, the default value
#'   equal to the potential volume of \code{reser} will be used. If only storage is missing, it will be optimized using reliability
#'   and yield.)
#' @param reliability A reliability value, cannot be less than zero or greater than maximum reliability value
#'   (depending on data and usage of empirical reliability). Alternatively, \dQuote{max} value can be used to set reliability to its maximum
#'   value. (If missing, reliability will be calculated using storage and yield.)
#' @param yield A required yield in m3.s-1, constant for all months. (If missing, it will be optimized using storage and reliability.)
#' @param prob_type Type of empirical probability used for calculation of reliability given as a number corresponding with the \code{type}
#'   argument of the \code{\link{quantile}} function or \dQuote{chegodayev} representing the (m - 0.3) / (n + 0.4) formula.
#'  For the numeric argument, currently only types 4 [m / n] and 7 [(m - 1) / (n - 1)] are supported.
#'
#'  Therefore, maximum reliability value can be less than 1 for certain types.
#' @param upper_limit An upper limit for optimization of storage or yield given as multiple of potential volume of the reservoir
#'   (for storage) or as multiple of mean monthly flow (for yield).
#' @param throw_exceed Whether volume exceeding storage will be thrown or added to yield. This will affect calculated yield series,
#'   however resulting storage, reliability or yield value is not likely to be influenced.
#' @param get_series Whether time series of reservoir balance variables will be returned.
#' @return A list consisting of:
#'   \item{storage}{storage value, optimized, equal to the \code{storage} argument or default (potential volume of \code{reser})}
#'   \item{reliability}{reliability value calculated for the given or optimized values of yield and storage}
#'   \item{yield}{yield value, optimized or equal to the \code{yield} argument}
#'   \item{series}{only if \code{get_series} is enabled, a list consisting of reservoir storage (m3), yield (m3.s-1), evaporation (m3)
#'     and withdrawal (m3) time series}
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
#' reser = as.wateres(reser, storage = 14.4e6, area = 754e3)
#' sry(reser, reliab = 0.9, yield = 0.14)
#' sry(reser, storage = 41e3, yield = 0.14)
#' sry(reser, yield = 0.14)
#' sry(reser, storage = 41e3, reliab = 0.5)
sry.wateres <- function(reser, storage, reliability, yield, prob_type = 7, upper_limit = 5, throw_exceed = FALSE, get_series = FALSE) {
    if (!prob_type %in% c(4, 7)) { # supported prob_types
        prob_types = "chegodayev"
        prob_type = prob_types[pmatch(prob_type, prob_types)]
        if (is.na(prob_type))
            stop("Invalid probability type.")
    }
    if (!missing(reliability)) {
        max_reliab = calc_reliability(rep(1, nrow(reser)), 0, prob_type)
        if (reliability == "max")
            reliability = max_reliab
        if (reliability > max_reliab || reliability < 0)
            stop("Invalid value of reliability.")
    }
    if (missing(storage) && (missing(reliability) || missing(yield)))
        storage = attr(reser, "storage")
    if (missing(storage) || missing(yield)) {
        upper_limit = ifelse(missing(storage), upper_limit * attr(reser, "storage"), upper_limit * mean(reser$Q))
        if (missing(storage)) {
            resul = bisection(calc_diff_reliability, c(0, upper_limit), reser = reser, reliab_req = reliability, yield_req = yield,
                prob_type = prob_type, throw_exceed = throw_exceed)
            series_neighbour = calc_series(reser, resul[1] + 1e-5, yield, throw_exceed)
            reliab_neighbour = calc_reliability(series_neighbour$yield, yield, prob_type)
            missing = "storage"
        }
        else {
            resul = bisection(calc_diff_reliability, c(0, upper_limit), reser = reser, reliab_req = reliability, storage_req = storage,
                prob_type = prob_type, throw_exceed = throw_exceed)
            series_neighbour = calc_series(reser, storage, resul[1] - 1e-5, throw_exceed)
            reliab_neighbour = calc_reliability(series_neighbour$yield, resul[1] - 1e-5, throw_exceed)
            missing = "yield"
        }
        reliab_found = resul[2] + reliability
        # the greatest storage or the smallest yield for the same reliability found -> find the smallest or the greatest one
        if (reliab_found != reliab_neighbour) {
            if (missing == "storage") {
                resul = bisection(is_reliab_equal, c(0, resul[1]), resul_value = reliab_found, reser = reser, yield_req = yield,
                    prob_type = prob_type, throw_exceed = throw_exceed)
            }
            else {
                resul = bisection(is_reliab_equal, c(resul[1], upper_limit), resul_value = reliab_found, reser = reser, storage_req = storage,
                    prob_type = prob_type, throw_exceed = throw_exceed)
            }
        }
        assign(missing, resul[1])
    }
    series = calc_series(reser, storage, yield, throw_exceed)
    reliability = calc_reliability(series$yield, yield, prob_type)
    resul = list(storage = storage, reliability = reliability, yield = yield)
    if (get_series)
        resul$series = series
    return(resul)
}
