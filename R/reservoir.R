#' Characteristics and Simulations for Water Reservoirs
#'
#' Tools to calculate characteristics of water reservoirs and to perform simulations for them.
#'
#' A reservoir has to be created by \code{\link{as.reservoir}} by using time series of dates
#' and corresponding flows. After that, characteristics for the reservoir can be calculated.
#'
#' @docType package
#' @name reservoir
#' @import data.table
#' @examples
#' reser = data.frame(
#'     Q = c(0.078, 0.065, 0.168, 0.711, 0.154, 0.107, 0.068, 0.057, 0.07, 0.485, 0.252, 0.236,
#'           0.498, 0.248, 0.547, 0.197, 0.283, 0.191, 0.104, 0.067, 0.046, 0.161, 0.16, 0.094),
#'     DTM = seq(as.Date("2000-01-01"), by = "months", length.out = 24))
#' reser = as.reservoir(reser, Vpot = 11.1)
#' summary(reser, Qn_coeff = c(0.1, 5, 0.1))
NULL

days_in_month <- function(date) {
    mon = format(date, format = "%m")
    while (format(date, format = "%m") == mon) {
        date = date + 1
    }
    return(as.integer(format(date - 1, format = "%d")))
}

#' Water reservoir creation
#'
#' Creates a reservoir object from provided time series.
#'
#' @param dframe A name of file containing table with data (including header) or directly data frame or data table.
#'   The data need to consist of monthly flows in m3.s-1 (\dQuote{Q} column) and dates (\dQuote{DTM} column).
#'   Alternatively, this can be a Bilan object where the dates and modelled or observed runoffs are read from.
#'   In that case, catchment area needs to be specified within the Bilan object.
#' @param Vpot Potential storage of the reservoir in millions of m3.
#' @param observed Only when Bilan object is used; whether to read observed runoffs from the object (otherwise modelled are read).
#' @return A reservoir object which is also of data.frame and data.table classes.
#' @details An error occurs if \dQuote{Q} or \dQuote{DTM} column is missing or \code{dframe} is of another class
#'   than \code{data.frame} or \code{data.table}.
#' @export
#' @examples
#' reser = data.frame(
#'     Q = c(0.078, 0.065, 0.168, 0.711, 0.154, 0.107, 0.068, 0.057, 0.07, 0.485, 0.252, 0.236,
#'           0.498, 0.248, 0.547, 0.197, 0.283, 0.191, 0.104, 0.067, 0.046, 0.161, 0.16, 0.094),
#'     DTM = seq(as.Date("2000-01-01"), by = "months", length.out = 24))
#' reser = as.reservoir(reser, Vpot = 11.1)
as.reservoir <- function(dframe, Vpot, observed = FALSE) {
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
    dframe$Q = as.numeric(dframe$Q)
    class(dframe) = c("reservoir", "data.table", "data.frame")
    attr(dframe, "Vpot") = Vpot
    return(dframe)
}

# calculates changes in reservoir storage from monthly discharges Q [m3/s]
# Qn is a designed yield value
max_deficit <- function(Q, Qn) {
    deltaQ = Q - Qn
    n = length(deltaQ)
    storage = vector("numeric", n)
    for (j in 1:(n - 1)) {
        storage[j + 1] = min(0, storage[j] + deltaQ[j + 1])
    }
    return(-min(storage) * 30.5 * 3600 * 24 / 1e6)
}

indices <- function(reser, Qn) {
    Qa = mean(reser$Q)
    alpha = Qn / Qa
    Qyears = reser[, list(Q = mean(Q)), by = year(DTM)]
    m = (1 - alpha) / (sd(Qyears$Q) / Qa)
    return(c(alpha = alpha, m = m))
}

#' Reservoir summary
#'
#' Calculates and shows characteristics of the reservoir.
#'
#' @param object A reservoir object.
#' @param ... Further arguments are not used.
#' @param Qn_coeff Begin, end and step value of coefficients used to calculate designed yields to be tested.
#'   The designed yields are equal to the mean annual flow multiplied by the coefficient.
#' @return A vector of reservoir characteristics:
#'   \item{Vpot}{potential reservoir storage in millions of m3 (given as a parameter of \code{\link{as.reservoir}})}
#'   \item{Qn_max}{the maximum yield (m3.s-1) for 100\% reliability for given potential storage}
#'   \item{alpha}{level of development - ratio Qn_max to the mean annual flow}
#'   \item{m}{resilience index - a measure of flow variability calculated as (1 - alpha) / (standard deviation of annual flows / mean annual flow)}
#' @details An error occurs if potential storage \code{Vpot} is out of the range given by \code{Qn_coeff} so that
#'   value of Qn_max cannot be interpolated.
#' @export
#' @examples
#' reser = data.frame(
#'     Q = c(0.078, 0.065, 0.168, 0.711, 0.154, 0.107, 0.068, 0.057, 0.07, 0.485, 0.252, 0.236,
#'           0.498, 0.248, 0.547, 0.197, 0.283, 0.191, 0.104, 0.067, 0.046, 0.161, 0.16, 0.094),
#'     DTM = seq(as.Date("2000-01-01"), by = "months", length.out = 24))
#' reser = as.reservoir(reser, Vpot = 11.1)
#' summary(reser, Qn_coeff = c(0.1, 5, 0.1))
summary.reservoir <- function(object, ..., Qn_coeff = c(0.1, 1.2, 0.05)) {
    Qn = seq(Qn_coeff[1], Qn_coeff[2], by = Qn_coeff[3]) * mean(object$Q)

    Vz = sapply(1:length(Qn), function(i) { max_deficit(object$Q, Qn[i]) })

    Qn_max = approx(Vz, Qn, xout = attr(object, "Vpot"))$y
    if (is.na(Qn_max))
        stop("Qn_max for potential volume cannot be interpolated. Increase the range given by Qn_coeff.")

    print(c(Vpot = attr(object, "Vpot"), Qn_max = Qn_max, indices(object, Qn_max)))
}
