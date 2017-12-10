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

days_in_month = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
days_for_months <- function(DTM) {
    mon = as.numeric(format(DTM, format = "%m"))
    year = as.numeric(format(DTM, format = "%Y"))
    days = days_in_month[mon]
    is_leap_feb = (mon == 2 & (year %% 4 == 0 & (year %% 100 != 0 | year %% 400 == 0)))
    days[is_leap_feb] = 29
    return(days)
}

calc_minutes <- function(time_step_len, time_step_unit, len, DTM) {
    minutes = rep(60, len) * time_step_len
    if (time_step_unit == "day") {
        minutes = 24 * minutes
    }
    else if (time_step_unit == "month") {
        minutes = days_for_months(DTM) * 24 * minutes
    }
    return(minutes)
}

#' Water reservoir creation
#'
#' Creates a wateres object from provided time series.
#'
#' @param dframe A name of file containing table with data (including header) or directly data frame or data table.
#'   The data need to consist of time series of flows in m3.s-1 (\dQuote{Q} column) and dates (\dQuote{DTM} column, required for monthly time step only).
#'   Optionally, time series of flows from intercatchment only can be given (\dQuote{QI} column). These intercatchment flows will be used when
#'   calculating a system of reservoirs by the \code{\link{calc_system}} function.
#'   Alternatively, \code{dframe} can be a Bilan object (in daily or monthly time step) where the dates and modelled or observed runoffs are read from.
#'   In that case, catchment area needs to be specified within the Bilan object.
#' @param storage Potential storage of the reservoir in m3.
#' @param area Flooded area of the reservoir for the potential storage in m2.
#' @param eas Elevation-area-storage relationship given as a data frame or data table with the three columns representing
#'   elevation (m.a.s.l.), area (m2) and storage (m3). If values of these three variables are not sorted and their orders
#'   differ or if they contain any NA value, this argument will be ignored.
#' @param observed Only when Bilan object is used; whether to read observed runoffs from the object (otherwise modelled are read).
#' @param time_step Time step length (optional, defaults to 1) and unit, entered as a unit name or a string joined by the hyphen (e.g. \dQuote{7-day}).
#'   Currently \dQuote{month}, \dQuote{day} and \dQuote{hour} values are supported. However, the length of monthly time step is allowed to be only 1.
#' @param id An identifier of the reservoir used for calculation of systems.
#' @param down_id The identifier of the nearest reservoir downstream (also used in systems).
#' @param title A reservoir title.
#' @return A wateres object which is also of data.frame and data.table classes.
#' @details An error occurs if any of the required columns is missing or \code{dframe} is of another class
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
as.wateres <- function(dframe, storage, area, eas = NULL, observed = FALSE, time_step = "month", id = NA, down_id = NA, title = NA) {
    ts_types = c("month", "day", "hour")
    if (grepl("-", time_step)) {
        splitted = strsplit(time_step, "-", fixed = TRUE)[[1]]
        time_step_len = suppressWarnings(as.integer(splitted[1]))
        if (is.na(time_step_len))
            stop("Invalid value of time step length.")
        time_step = splitted[2]
    }
    else
        time_step_len = 1L
    if ("data.frame" %in% class(dframe) && !is.null(dframe$DTM) && length(dframe$DTM) > 1) {
        diff_dates = difftime(dframe$DTM[2], dframe$DTM[1], units = "days")
        if ((time_step == "day" && diff_dates != time_step_len) ||
            (time_step == "month" && (diff_dates < 28 * time_step_len ||  diff_dates > 32 * time_step_len))) {
            stop("Given time step does correspond with the time step of dates in data frame.")
        }
    }
    time_step_unit = ts_types[pmatch(time_step, ts_types, 1)]
    if (is.na(time_step_unit))
        stop("Invalid value of time step unit.")
    if (time_step_len != 1 && time_step_unit == "month")
        stop("Monthly time step cannot be combined with an arbitrary length.")

    if ("bilan" %in% class(dframe)) {
        if (requireNamespace("bilan", quietly = TRUE)) {
            catch_area = bilan::bil.get.area(dframe)
            if (!(catch_area > 0))
                stop("Catchment area needs to be specified when using Bilan data.")
            data = bilan::bil.get.data(dframe)
            time_step_unit = bilan::bil.info(dframe, FALSE)$time_step
            time_step_len = 1
            Qvar = ifelse(observed, "R", "RM")
            if (time_step_unit == "month")
                days = days_for_months(data$DTM)
            else
                days = 1
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
    required_cols = c("Q")
    if (time_step_unit == "month")
        required_cols = c(required_cols, "DTM")
    for (colname in required_cols) {
        if (!colname %in% colnames(dframe))
            stop("To create a reservoir, ", colname, " column is required.")
    }
    optional_cols = c("DTM", "QI")
    all_cols = required_cols
    for (col_name in optional_cols) {
        if (col_name %in% colnames(dframe) && !col_name %in% required_cols)
            all_cols = c(all_cols, col_name)
    }
    dframe = as.data.frame(dframe) # remove data.table class to use data.frame subsetting
    dframe = as.data.frame(dframe[, all_cols]) # keep data.frame if only one required column
    colnames(dframe) = all_cols
    if ("DTM" %in% colnames(dframe))
        dframe$DTM = as.Date(dframe$DTM)
    dframe$minutes = calc_minutes(time_step_len, time_step_unit, nrow(dframe), dframe$DTM)
    dframe$Q = as.numeric(dframe$Q)
    class(dframe) = c("wateres", "data.table", "data.frame")
    for (attr_name in c("time_step_len", "time_step_unit", "storage", "area", "id", "down_id", "title")) {
        if (length(get(attr_name)) == 0)
            stop("Missing value of reservoir attribute '", attr_name, "'.")
        attr(dframe, attr_name) = get(attr_name)
    }
    if (!is.null(eas)) {
        if (ncol(eas) != 3)
            warning("Incorrect number of columns for elevation-area-storage relationship.")
        else if (anyNA(eas))
            warning("Elevation-area-storage relationship contains NAs and will be ignored.")
        else {
            ref_order = order(eas[[1]])
            if (!(identical(ref_order, order(eas[[2]])) && identical(ref_order, order(eas[[3]]))))
                warning("Elevation-area-storage values are not sorted correctly and will be ignored.")
            else {
                eas = as.data.table(eas)
                setnames(eas, c("elevation", "area", "storage"))
                if (!identical(ref_order, 1:length(ref_order)))
                    setorder(eas, elevation, area, storage)
                attr(dframe, "eas") = eas
            }
        }
    }
    return(dframe)
}

# converts input (date or index) to index of the given time series of dates
value_to_position <- function(value, DTM, time_step_len, time_step_unit, type, value_type) {
    if (is.null(value_type)) {
        value_date = try(as.Date(value), silent = TRUE)
        value_type = if (class(value_date) == "try-error") "index" else "date"
    }
    else if (value_type == "date") {
        value_date = as.Date(value)
    }
    if (value_type == "index") {
        value_pos = suppressWarnings(as.integer(value))
        if (is.na(value_pos))
            stop("Invalid ", type, " '", value, "' (date or index needed).")
    }
    else {
        if (is.null(DTM))
            stop(
                toupper(substr(type, 1, 1)), substr(type, 2, nchar(type)), " '", value,
                "' is of date format, but the reservoir series do not include dates.")
        tmp_dtm = DTM
        # expand time series to reach (or nearly reach) the given date
        if (value_date < DTM[1]) {
            # unique because DTM[1] is repeated
            tmp_dtm = unique(c(rev(seq(DTM[1], value_date, by = paste0("-", time_step_len, " ", time_step_unit))), tmp_dtm))
        }
        else if (value_date > DTM[length(DTM)]) {
            tmp_dtm = unique(c(tmp_dtm, seq(DTM[length(DTM)], value_date, by = paste(time_step_len, time_step_unit))))
        }
        if (type == "begin") {
            after_begin = which(tmp_dtm >= value_date)
             # given date can be first value after expanded time series
            value_pos = if (length(after_begin) == 0) length(tmp_dtm) + 1 else min(after_begin)
        }
        else {
            before_end = which(tmp_dtm <= value_date)
            value_pos = if (length(before_end) == 0) 0 else max(before_end)
        }
        value_pos = value_pos - which(tmp_dtm == DTM[1]) + 1
    }
    return(value_pos)
}

#' @rdname resize_input.wateres
#' @export
resize_input <- function(reser, begin, end, type) UseMethod("resize_input")

#' Reservoir input time series resize
#'
#' Resizes input time series of the reservoirs. Both shortening and expanding are supported. In case of expanding, input variables for the new
#' time steps are set to zero while dates and number of minutes are correctly filled in.
#'
#' @param reser A \code{wateres} object.
#' @param begin New time series begin entered as a value that can be converted to date or as a number representing index within the time series
#'   (can be less than 1 for expanding before the current series). Dates cannot be used if the \code{wateres} object does not contain dates (e.g.
#'   for hourly data).
#' @param end New time series end entered the same way as \code{begin}. The end value cannot be less than the begin value.
#' @param type Optional type of \code{begin} and \code{end} (one of \dQuote{index} or \dQuote{date}) to speed up the resize.
#' @return A \code{wateres} object with the resized series.
#' @export
#' @examples
#' reser = data.frame(
#'     Q = c(0.078, 0.065, 0.168, 0.711, 0.154, 0.107, 0.068, 0.057, 0.07, 0.485, 0.252, 0.236,
#'           0.498, 0.248, 0.547, 0.197, 0.283, 0.191, 0.104, 0.067, 0.046, 0.161, 0.16, 0.094),
#'     DTM = seq(as.Date("2000-01-01"), by = "months", length.out = 24))
#' reser = as.wateres(reser, storage = 4e5, area = 754e3)
#' reser = resize_input(reser, "2000-02-15", 20)
resize_input.wateres <- function(reser, begin = 1, end = nrow(reser), type = NULL) {
    type_begin = type_end = type
    if (!is.null(type)) {
        type_begin = if (missing(begin)) "index" else type
        type_end = if (missing(end)) "index" else type
    }
    begin_pos = value_to_position(begin, reser$DTM, attr(reser, "time_step_len"), attr(reser, "time_step_unit"), "begin", type_begin)
    end_pos = value_to_position(end, reser$DTM, attr(reser, "time_step_len"), attr(reser, "time_step_unit"), "end", type_end)
    if (end_pos < begin_pos)
        stop("End for reservoir resizing cannot be less than begin.")

    attrs_orig = attributes(reser)
    begin_pos_orig = begin_pos
    rows_before = if (begin_pos < 0) 1 - begin_pos else 0
    begin_pos = begin_pos + rows_before
    rows_after = if (end_pos > nrow(reser)) end_pos - nrow(reser) else 0
    end_pos = end_pos + rows_before

    if (rows_before > 0 || rows_after > 0) {
        tmp_coeff = begin_pos_orig / abs(begin_pos_orig)
        tmp_len = if (begin_pos_orig < 1) abs(begin_pos_orig) + 2 else begin_pos_orig
        begin_date = seq(
            reser$DTM[1], by = paste(tmp_coeff * attr(reser, "time_step_len"), attr(reser, "time_step_unit")), length.out = tmp_len)[tmp_len]

        for (type in c("before", "after")) {
            dt_name = paste0("dt_", type)
            assign(dt_name, data.table(matrix(0, nrow = get(paste0("rows_", type)), ncol = ncol(reser))))
            setnames(get(dt_name), colnames(reser))
        }
        if (!is.null(reser$DTM)) {
            class(dt_before$DTM) = class(dt_after$DTM) = "Date"
        }
        reser = rbindlist(list(dt_before, reser, dt_after))
    }
    reser = reser[begin_pos:end_pos, ]

    for (attr_name in names(attrs_orig)) {
        if (!attr_name %in% names(attributes(reser)))
            attr(reser, attr_name) = attrs_orig[[attr_name]]
    }
    class(reser) = c("wateres", class(reser))

    if (rows_before > 0 || rows_after > 0) {
        if (!is.null(reser$DTM))
            reser$DTM = seq(begin_date, by = paste(attr(reser, "time_step_len"), attr(reser, "time_step_unit")), length.out = nrow(reser))
        reser$minutes = calc_minutes(attr(reser, "time_step_len"), attr(reser, "time_step_unit"), nrow(reser), reser$DTM)
    }
    return(reser)
}

#' Water reservoir summary
#'
#' Calculates characteristics of the reservoir.
#'
#' @param object A \code{wateres} object.
#' @param ... Further arguments passed to the \code{\link{sry.wateres}} function (as \code{storage}, \code{yield}, \code{prob_type} or \code{upper_limit}).
#' @param reliability A vector of reliability values passed to the \code{\link{sry.wateres}} function.
#' @param get_series Whether time series of reservoir balance variables for the given reliabilites will be returned.
#' @return A data table of reservoir characteristics:
#'   \item{storage}{reservoir storage in m3, given or the minimum storage calculated for given reliability and yield}
#'   \item{reliability}{given or calculated reliability}
#'   \item{yield}{the maximum yield (m3.s-1), given or calculated for given reliability and potential storage}
#'   \item{alpha}{level of development - ratio of yield to the mean annual flow}
#'   \item{m}{standardized net inflow - a measure of resilience calculated as (1 - alpha) / (standard deviation of annual flows / mean annual flow)}
#'   \item{resilience}{resilience calculated as number of continuous sequences of failures / total number of time steps with failures, NA for no failure}
#'   \item{vulnerability}{vulnerability (in m3) calculated as mean of monthly deficit volumes that represent maximum deficit of each failure period}
#'   \item{dimless_vulner}{dimensionless vulnerability, vulnerability value divided by yield value in volume units}
#'
#'   If the \code{get_series} argument is \code{TRUE}, a list is returned instead. The list consists of:
#'   \item{chars}{the table of characteristics described above}
#'   \item{series}{a list of time series of water reservoir variables for the given reliabilities; list names are identical with the values of
#'     the \code{reliability} argument}
#' @details The maximum yield or the minimum storage is calculated by using the \code{\link{sry.wateres}} function for given storage or yield and reliability.
#'
#'   To calculate reliability for given storage and yield without any optimization, provide all the three arguments: storage, reliability (will be ignored)
#'   and yield.
#'
#'   An error occurs if the range given by \code{upper_limit} does not contain value of the given reliability or if an invalid reliability value or probability
#'   type is given.
#' @seealso \code{\link{sry.wateres}} used for optimization of the yield or storage for given reliability and for calculation of reliability
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
summary.wateres <- function(object, ..., reliability = "max", get_series = FALSE) {
    resul = sapply(reliability, summary_wateres, object, get_series, ...)
    if (!get_series) {
        return(as.data.table(t(resul)))
    }
    else {
        series = list()
        for (rel in seq(length(resul), 2, -2)) {
            nominal_rel = as.character(reliability[rel / 2])
            series[[nominal_rel]] = resul[[rel]]
            resul[[rel]] = NULL
        }
        char_names = names(resul[[1]])
        chars = as.data.table(matrix(unlist(resul), ncol = length(char_names), byrow = TRUE))
        colnames(chars) = char_names
        return(list(chars = chars, series = series))
    }
}

# summary for one value of reliability
summary_wateres <- function(reliability, object, get_series, ...) {
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
            function(i) { max(.Call("convert_m3", PACKAGE = "wateres", yield - resul$series$yield[seq_begins[i]:seq_ends[i]], object$minutes[seq_begins[i]:seq_ends[i]], TRUE)) } )
        vulnerability = mean(deficits)
        dimless_vulner = vulnerability / (.Call("convert_m3", PACKAGE = "wateres", yield, 1, TRUE) * 30.5 * 24 * 60)
    }
    chars = c(storage = resul$storage, reliability = resul$reliability, yield = yield, alpha = alpha, m = m, resilience = resilience,
        vulnerability = vulnerability, dimless_vulner = dimless_vulner)
    if (get_series)
        return(list(chars = chars, series = resul$series))
    else
        return(chars)
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
        series = calc_series(tmp_reser, attr(tmp_reser, "storage"), yield, FALSE, initial_storage = 0, complex_properties = FALSE)
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
# returns pair of resulting values or string describing function values when bisection is not feasible
bisection <- function(func, interval, max_iter = 500, tolerance = 1e-5, ...) {
    lower = min(interval)
    upper = max(interval)
    flower = func(lower, ...)
    fupper = func(upper, ...)
    if (flower < 0 && fupper < 0)
        return("both negative")
    else if (flower == fupper || (flower > 0 && fupper > 0))
        return("both positive or both zero")
    else if (flower * fupper == 0)
        return(ifelse(flower > 0 || fupper > 0, "positive and zero", "negative and zero"))
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

#' @rdname calc_series.wateres
#' @export
calc_series <- function(
    reser, storage, yield, throw_exceed, initial_storage, initial_level, initial_pos, last_pos, get_level, till_def, first_def_pos,
    storage_optim, yield_max, complex_properties) UseMethod("calc_series")

#' Calculation of reservoir time series
#'
#' Calculates time series of water balance variables for the reservoir. If provided in the \code{reser} object, the precipitation, evaporation,
#' water use or transfer variables are applied.
#'
#' @param reser A \code{wateres} object.
#' @param storage A maximum reservoir storage in m3, either a value of fixed maximum storage or a vector
#'   of the length of the reservoir series plus one. If not given, taken from the \code{reser} object.
#' @param yield A required yield in m3.s-1, either a value of fixed yield or a vector of the same length as the reservoir series.
#' @param throw_exceed Whether volume exceeding storage will be thrown or added to yield.
#' @param initial_storage A value of initial reservoir storage in m3. If not specified, the reservoir is considered to be full.
#' @param initial_level A value of initial water level in m.a.s.l. If specified and elevation-area-storage relationship is not provided within the
#'   \code{reser} object, it will be ignored; otherwise the \code{initial_storage} argument will be ignored.
#' @param initial_pos An index of time series where the calculation starts. If greater than one, returned time series will be shorter than the input.
#' @param last_pos An index of time series where the calculation stops.
#' @param get_level Whether to obtain water level series for calculated storages. It is ignored if no elevation-area-storage relationship
#'   is provided within the \code{reser} object.
#' @param till_def If TRUE, the calculation will stop at time step when any deficit occurs provided that this time step is greater or equal to \code{first_def_pos}.
#' @param first_def_pos If the \code{till_def} argument is TRUE, it means the first index of time step when a deficit will stop the calculation.
#' @param storage_optim An optimum reservoir storage in m3, i.e. the value which will be tried to be not exceeded if this is allowed by the value of `max_yield`.
#'   Either a value of fixed optimum storage or a vector of the length of the reservoir series plus one.
#' @param yield_max A maximum value of yield (in m3.s-1) that can be obtained from storage below the maximum value (the `storage` argument). Hence, if
#'   the reservoir storage exceeds the `storage` value and `throw_exceed` is not set to `TRUE`, the resulting yield may be greater than `yield_max`.
#'   The maximum yield can be interpreted as an outflow capacity of the reservoir in case that its dam is not overflown.
#'   Either a value of fixed yield or a vector of the same length as the reservoir series.
#' @param complex_properties If FALSE, constant values of `storage` and `yield` will be required and `storage_optim` and `yield_max` will not be considered.
#'   This is needed for calculation with generated values, as it is done e.g. in the [sry.wateres] function.
#' @return A \code{wateres_series} object which is a data table with water balance variables: inflow (in m3.s-1), storage (in m3), yield (in m3.s-1),
#'   precipitation, evaporation, water use, deficits and transfer (in m3). The deficits represent the missing volume which would satisfy the remaining
#'   sum of yield and withdrawal demands. There is the water transfer only in case of non-zero values, resulting from calculations of a reservoir system.
#'   Positive values of transfer mean additional inflow whereas negative represent additional withdrawal.
#'   Additionally, water levels are included if the \code{get_level} argument is TRUE.
#' @details When calculating water balance, a simple explicit method is applied. Finally, the initial time step of storage is omitted
#'   to get a time series of the same length as for other variables.
#'
#'   If one of `storage_optim` or `yield_max` is missing while the second is specified, both values are set to NULL and thus they are not considered in the calculation.
#' @seealso \code{\link{plot.wateres_series}} for plotting the time series
#' @export
#' @examples
#' reser = data.frame(
#'     Q = c(0.078, 0.065, 0.168, 0.711, 0.154, 0.107, 0.068, 0.057, 0.07, 0.485, 0.252, 0.236,
#'           0.498, 0.248, 0.547, 0.197, 0.283, 0.191, 0.104, 0.067, 0.046, 0.161, 0.16, 0.094),
#'     DTM = seq(as.Date("2000-01-01"), by = "months", length.out = 24))
#' reser = as.wateres(reser, storage = 14.4e6, area = 754e3)
#' resul = calc_series(reser, 14.4e6, 0.14)
#' @md
calc_series.wateres <- function(
    reser, storage = attr(reser, "storage"), yield = attr(reser, "yield"), throw_exceed = FALSE, initial_storage = storage[1], initial_level,
    initial_pos = 1, last_pos = nrow(reser), get_level = FALSE, till_def = FALSE, first_def_pos = initial_pos,
    storage_optim = attr(reser, "storage_optim"), yield_max = attr(reser, "yield_max"), complex_properties = TRUE) {

    if (!complex_properties) {
        if (length(storage) != 1 || length(yield) != 1) {
            stop("For calculation of series without complex properties, storage and yield have to be constant.")
        }
        if (!is.null(storage_optim) || !is.null(yield_max)) {
            warning("Optimum storage and maximum yield will not be considered in calculation without complex properties.")
        }
    }
    if ((is.null(storage_optim) && !is.null(yield_max)) || (is.null(yield_max) && !is.null(storage_optim))) {
        warning("Both optimum storage and maximum yield have to be specified (set to NULL for this calculation).")
        storage_optim = yield_max = NULL
    }
    required_series_length = list(yield = nrow(reser), yield_max = nrow(reser), storage = nrow(reser) + 1, storage_optim = nrow(reser) + 1)
    for (variable in c("yield", "yield_max", "storage", "storage_optim")) {
        if (length(get(variable)) == 1) {
            assign(variable, rep(get(variable), required_series_length[[variable]]))
        }
        else if (!is.null(get(variable)) && length(get(variable)) != required_series_length[[variable]]) {
            stop("Time series of ", variable, " must correspond with the length of the reservoir series (expected ",
                required_series_length[[variable]], " time steps).")
        }
    }
    if (!is.null(yield_max)) {
        if (any(yield_max < yield))  {
            stop("Yield cannot be greater than maximum yield.")
        }
    }
    if (!is.null(storage_optim)) {
        if (any(storage_optim > storage))  {
            stop("Optimum storage cannot be greater than maximum storage.")
        }
    }

    eas = attr(reser, "eas")
    if (!missing(initial_level)) {
        if (is.null(eas))
            warning("Initial level is ignored because of missing elevation-area-storage relationship.")
        else
            initial_storage = approx(eas$elevation, eas$storage, initial_level, rule = 2)$y
    }

    resul = .Call("calc_storage", PACKAGE = "wateres", reser, yield, yield_max, storage, storage_optim, initial_storage, initial_pos, last_pos, throw_exceed,
        till_def, first_def_pos)
    resul = as.data.table(resul)
    if (get_level && !is.null(eas)) {
        resul = cbind(resul, level = approx(eas$storage, eas$elevation, resul$storage, rule = 2)$y)
    }
    class(resul) = c("wateres_series", class(resul))
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

    series = calc_series(reser, storage_req, yield_req, throw_exceed, complex_properties = FALSE)
    reliab = calc_reliability(series$yield, yield_req, prob_type)
    return(reliab - reliab_req)
}

is_reliab_equal <- function(value, resul_value, reser, storage_req, yield_req, prob_type, throw_exceed) {
    if (missing(storage_req))
        storage_req = value
    else
        yield_req = value
    series = calc_series(reser, storage_req, yield_req, throw_exceed, complex_properties = FALSE)
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
#' Calculates one of the water reservoir characteristics: storage, time-based reliability or yield (release) while the
#' two remaining values are provided.
#'
#' @param reser A \code{wateres} object.
#' @param storage A water reservoir storage value in m3. (If missing together with reliability or yield, the default value
#'   equal to the potential volume of \code{reser} will be used. If only storage is missing, it will be optimized using reliability
#'   and yield.)
#' @param reliability A reliability value, cannot be less than zero or greater than maximum reliability value
#'   (depending on data and probability type). Alternatively, \dQuote{max} value can be used to set reliability to its maximum
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
#'   \item{series}{only if \code{get_series} is enabled, time series of water balance variables as returned by the
#'     \code{\link{calc_series.wateres}} function}
#' @details If all the three values are provided, the reliability value will be ignored and it will be calculated for the provided
#'   storage and yield.
#'
#'   To optimize the value of storage or yield, a simple bisection algorithm is applied. If the optimization fails because
#'   the required reliability is not contained within the provided interval, try to change its upper limit given as the \code{upper_limit}
#'   argument.
#'
#'   As the required reliability represents a range of storage or yield values, the smallest value of storage (or the greatest value
#'   of yield) is returned, considering some tolerance value of the optimization algorithm.
#'
#'   When optimizing the storage or yield value, a value which produces reliability closest to the required reliability is selected,
#'   hence the resulting reliability can be less or greater than the required one.
#'
#'   If the calculated reliability is even for the zero storage value greater than the required reliability, the zero storage and
#'   the corresponding reliability will be returned. Contrary to this, if the calculated reliability is for the optimized yield greater,
#'   the optimization fails as the reliability can be decreased by increasing the upper limit.
#' @seealso \code{\link{calc_series.wateres}} used for calculation of time series of water balance variables
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
    bisection_failed_text = "Required reliability is not contained within the given interval. Increase the upper limit."
    if (missing(storage) || missing(yield)) {
        upper_limit = ifelse(missing(storage), upper_limit * attr(reser, "storage"), upper_limit * mean(reser$Q))
        if (missing(storage)) {
            resul = bisection(calc_diff_reliability, c(0, upper_limit), reser = reser, reliab_req = reliability, yield_req = yield,
                prob_type = prob_type, throw_exceed = throw_exceed)
            if (length(resul) == 1) {
                if (resul == "both negative")
                    stop(bisection_failed_text)
                else if (resul == "negative and zero") {
                    resul = c(upper_limit, 0)
                    reliab_neighbour = -1 # storage can be decreased in the next bisection -> arbitrary different reliab_neighbour
                }
                else { # minimum storage always zero in other cases
                    tmp_series = calc_series(reser, 0, yield, throw_exceed, complex_properties = FALSE)
                    resul = c(0, calc_reliability(tmp_series$yield, yield, prob_type) - reliability)
                }
            }
            if (!exists("reliab_neighbour")) {
                series_neighbour = calc_series(reser, resul[1] + 1e-5, yield, throw_exceed, complex_properties = FALSE)
                reliab_neighbour = calc_reliability(series_neighbour$yield, yield, prob_type)
            }
            missing = "storage"
        }
        else {
            resul = bisection(calc_diff_reliability, c(0, upper_limit), reser = reser, reliab_req = reliability, storage_req = storage,
                prob_type = prob_type, throw_exceed = throw_exceed)
            if (length(resul) == 1) {
                # even if the yield is zero, an yield greater than the upper limit could give the same reliability
                if (resul != "negative and zero")
                    stop(bisection_failed_text)
                else {
                    resul = c(0, max_reliab - reliability)
                    reliab_neighbour = -1 # yield can be increased in the next bisection -> arbitrary different reliab_neighbour
                }
            }
            else {
                series_neighbour = calc_series(reser, storage, resul[1] - 1e-5, throw_exceed, complex_properties = FALSE)
                reliab_neighbour = calc_reliability(series_neighbour$yield, resul[1] - 1e-5, prob_type)
            }
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
            if (length(resul) == 1) {
                if (resul == "both positive or both zero") {
                    if (missing == "storage") # storage decreased to zero (with reliability slightly less than given one)
                        resul = c(0, 0.5)
                    else # for yield increase to the upper limit which can be further extended
                        stop(bisection_failed_text)
                }
                else # should not happen
                    stop("Additional bisection failed.")
            }
        }
        assign(missing, resul[1])
    }
    series = calc_series(reser, storage, yield, throw_exceed, complex_properties = FALSE)
    reliability = calc_reliability(series$yield, yield, prob_type)
    resul = list(storage = storage, reliability = reliability, yield = yield)
    if (get_series)
        resul$series = series
    return(resul)
}
