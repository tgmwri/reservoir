#' Plot of reservoir time series
#'
#' Plots time series of chosen water balance variables for the reservoir, by using the \code{ggplot2} package.
#'
#' @param x A \code{wateres_series} object with reservoir variables as returned by \code{\link{calc_series.wateres}}.
#' @param reser A corresponding \code{wateres} object whose series of number of minutes will be used.
#' @param type A type of variables to be plotted: \dQuote{storage}, \dQuote{level} or \dQuote{flow} which plots all of the remaining variables.
#' @param begin A time step to begin the plot.
#' @param end A time step to end the plot.
#' @param filename A file name where the plot will be saved. If not specified, the plot will be printed to the current device.
#' @param width Plot width in inches (or a unit specified by the \code{units} argument).
#' @param height Plot height in inches (or a unit specified by the \code{units} argument).
#' @param ... Further arguments passed to the \code{\link[ggplot2:ggsave]{ggsave}} function saving the plot to a file.
#' @return A \code{ggplot} object.
#' @details Series of flows with all values equal to zero are ignored.
#'
#'   An error occurs if length of the time series of results (\code{x}) does not equal to the length of the reservoir data (\code{reser}).
#' @seealso \code{\link{calc_series.wateres}} used for calculation of reservoir time series
#' @export
#' @examples
#' reser = data.frame(
#'     Q = c(0.078, 0.065, 0.168, 0.711, 0.154, 0.107, 0.068, 0.057, 0.07, 0.485, 0.252, 0.236,
#'           0.498, 0.248, 0.547, 0.197, 0.283, 0.191, 0.104, 0.067, 0.046, 0.161, 0.16, 0.094),
#'     DTM = seq(as.Date("2000-01-01"), by = "months", length.out = 24))
#' reser = as.wateres(reser, storage = 14.4e6, area = 754e3)
#' plot(calc_series(reser, 14.4e6, 0.14), reser)
plot.wateres_series <- function(x, reser, type = "flow", begin = 1, end = nrow(x), filename = NULL, width = 8, height = 6, ...) {
    check_plot_pkgs()
    if (nrow(x) != nrow(reser))
        stop("The length of time series of results (", nrow(x), ") does not equal to the length of reservoir data (", nrow(reser), ").")

    types = c("flow", "storage", "level")
    units = c(storage = "mil. m\u00b3", flow = "m\u00b3.s\u207b\u00b9", level = "m.a.s.l.")
    type = types[pmatch(type, types, 1)]
    if (type == "flow") {
        vars = colnames(x)[!(colnames(x) %in% c("storage", "level"))]
    }
    else {
        vars = type
        if (!vars %in% colnames(x))
            stop("The required '", vars, "' is not available in time series data. Recalculate the time series with proper settings.")
    }

    series = x[begin:end, vars, with = FALSE]
    if (type == "flow") {
        series = series[, vars[!sapply(vars, function(var) { all(series[[var]] == 0) })], with = FALSE]
        vars_to_convert = c("precipitation", "evaporation", "wateruse", "deficit", "transfer")
        for (var in vars_to_convert[vars_to_convert %in% colnames(series)])
            set(series, j = var, value = .Call("convert_m3", PACKAGE = "wateres", series[[var]], reser$minutes, FALSE))
    }

    series = series[, ts := 1:nrow(series)]
    mseries = reshape2::melt(series, id = "ts")
    if (type == "storage")
        mseries$value = mseries$value / 1e6
    p = ggplot2::ggplot(mseries, ggplot2::aes(x = ts, y = value, colour = variable)) + ggplot2::geom_line()
    p = p + ggplot2::scale_x_continuous("time step") + ggplot2::scale_y_continuous(paste0(type, " [", units[type], "]"))
    p = p + ggplot2::scale_colour_discrete(name = "variable", labels = levels(mseries$variable))
    p = p + ggplot2::theme(legend.position = "bottom")

    save_plot_file(p, filename, width, height, ...)
    return(p)
}

#' @rdname prob_field.wateres
#' @export
prob_field <- function(reser, probs, yield, storage, throw_exceed) UseMethod("prob_field")

#' Calculation of probability fields
#'
#' Calculates monthly values of storage, yield and level (if possible) for given probabilities by using the \code{\link{quantile}} function
#'   with the default settings.
#'
#' @param reser A wateres object.
#' @param probs A vector of required probability values.
#' @param yield A value of yield to be used for calculation of storages in the reservoir.
#' @param storage A water reservoir storage value in m3, the default value is equal to the potential volume of \code{reser}.
#' @param throw_exceed Whether volume exceeding storage will be thrown or added to yield (see also \code{\link{sry.wateres}}).
#' @return A \code{wateres_prob_field} object which is a list consisting of:
#'   \item{quantiles}{data.table containing storage, yield and level values for months and given probabilities. Level values are available
#'     only if the elevation-area-storage relationship for the reservoir has been provided (as an argument of \code{\link{as.wateres}}).}
#'   \item{probs}{given probability values in percent as characters}
#' @details An error occurs if time step of data in \code{reser} is not one month.
#' @seealso \code{\link{plot.wateres_prob_field}} for plotting the results
#' @export
#' @examples
#' reser = data.frame(
#'     Q = c(0.078, 0.065, 0.168, 0.711, 0.154, 0.107, 0.068, 0.057, 0.07, 0.485, 0.252, 0.236,
#'           0.498, 0.248, 0.547, 0.197, 0.283, 0.191, 0.104, 0.067, 0.046, 0.161, 0.16, 0.094),
#'     DTM = seq(as.Date("2000-01-01"), by = "months", length.out = 24))
#' reser = as.wateres(reser, storage = 14.4e6, area = 754e3)
#' prob_field = prob_field(reser, c(0.9, 0.95, 0.99), 0.14)
prob_field.wateres <- function(reser, probs, yield, storage = attr(reser, "storage"), throw_exceed = FALSE) {
    if (attr(reser, "time_step_unit") != "month")
        stop("Probability fields can be calculated only for monthly data.")

    calc_resul = calc_series(reser, storage, yield, throw_exceed, get_level = TRUE)

    vars = c("storage", "yield")
    if ("level" %in% colnames(calc_resul))
        vars = c(vars, "level")

    reser = cbind(reser, calc_resul[, vars, with = FALSE])
    var_quant = list()
    for (var in vars) {
        var_mon = reser[, get(var), by = month(DTM)]
        setnames(var_mon, 2, var)
        var_quant[[var]] = tapply(var_mon[[var]], var_mon$month, quantile, 1 - probs)
    }

    prob_names = paste0(probs * 100, "%")
    quantiles = as.data.table(matrix(NA, 12, 1 + length(vars) * length(probs)))
    quantiles = quantiles[, names(quantiles) := lapply(.SD, as.numeric)]
    setnames(quantiles, 1:ncol(quantiles), c("month", paste(rep(vars, each = length(prob_names)), prob_names, sep = "_")))
    quantiles = quantiles[, month := 1:12]

    for (var in vars) {
        tmp_pos = which(gsub(var, "", names(quantiles), fixed = TRUE) != names(quantiles))
        for (mon in 1:12)
            quantiles[month == mon, names(quantiles)[tmp_pos] := as.list(var_quant[[var]][[mon]])]
    }
    resul = list(quantiles = quantiles, probs = prob_names)
    class(resul) = c("wateres_prob_field", class(resul))
    return(resul)
}

check_plot_pkgs <- function() {
    for (pkg_name in c("ggplot2")) {
        if (!requireNamespace(pkg_name, quietly = FALSE))
            stop("To produce a plot, ", pkg_name, " package needs to be installed.")
    }
}

save_plot_file <- function(p, filename, width, height, ...) {
    if (!is.null(filename))
        ggplot2::ggsave(filename = filename, width = width, height = height, ...)
    else
        print(p)
}

#' Plot of probability field
#'
#' Plots monthly values of storage, yield or level stored in a given \code{wateres_prob_field} object, by using the \code{ggplot2} package.
#'
#' @param x A \code{wateres_prob_field} object.
#' @param type Type of values to be plotted (\dQuote{storage}, \dQuote{yield} or \dQuote{level}).
#' @param filename A file name where the plot will be saved. If not specified, the plot will be printed to the current device.
#' @param width Plot width in inches (or a unit specified by the \code{units} argument).
#' @param height Plot height in inches (or a unit specified by the \code{units} argument).
#' @param ... Further arguments passed to the \code{\link[ggplot2:ggsave]{ggsave}} function saving the plot to a file.
#' @return A \code{ggplot} object.
#' @details An error occurs if levels are required but they are not contained in the \code{wateres_prob_field} object.
#' @export
#' @examples
#' reser = data.frame(
#'     Q = c(0.078, 0.065, 0.168, 0.711, 0.154, 0.107, 0.068, 0.057, 0.07, 0.485, 0.252, 0.236,
#'           0.498, 0.248, 0.547, 0.197, 0.283, 0.191, 0.104, 0.067, 0.046, 0.161, 0.16, 0.094),
#'     DTM = seq(as.Date("2000-01-01"), by = "months", length.out = 24))
#' reser = as.wateres(reser, storage = 14.4e6, area = 754e3)
#' prob_field = prob_field(reser, c(0.9, 0.95, 0.99), 0.14)
#' plot(prob_field, "storage")
plot.wateres_prob_field <- function(x, type = "storage", filename = NULL, width = 8, height = 6, ...) {
    check_plot_pkgs()

    types = c("storage", "yield", "level")
    units = c(storage = "mil. m\u00b3", yield = "m\u00b3.s\u207b\u00b9", level = "m.a.s.l.")
    type = types[pmatch(type, types, 1)]
    quant = copy(x$quantiles)
    col_to_remove = names(quant) == gsub(type, "", names(quant), fixed = TRUE)
    if (type == "level" && all(col_to_remove))
        stop("The given probability field does not contain levels.")
    col_to_remove[1] = FALSE # month
    quant = quant[, names(quant)[col_to_remove] := NULL]

    mquant = reshape2::melt(quant, id = "month")
    if (type == "storage")
        mquant$value = mquant$value / 1e6

    p = ggplot2::ggplot(mquant, ggplot2::aes(x = month, y = value, colour = variable)) + ggplot2::geom_line()
    p = p + ggplot2::scale_x_discrete() + ggplot2::scale_y_continuous(paste0(type, " [", units[type], "]"))
    p = p + ggplot2::scale_colour_discrete(name = "probability", labels = x$probs)
    p = p + ggplot2::theme(legend.position = "bottom")

    save_plot_file(p, filename, width, height, ...)
    return(p)
}

#' @rdname alpha_beta.wateres
#' @export
alpha_beta <- function(reser, alphas, max_beta, reliability, ...) UseMethod("alpha_beta")

#' Calculation of alpha and beta characteristics
#'
#' Calculates pairs of alpha (level of development) and beta (ratio of storage and volume of annual flow) characteristics of the reservoir
#' for given reliabilities.
#'
#' @param reser A \code{wateres} object.
#' @param alphas A vector of alpha values, i.e. coefficients by which mean annual flow will be multiplied. Usually the interval between 0 and 1 is used.
#' @param max_beta A maximum value of calculated beta to be considered, greater values will be ignored.
#' @param reliability A vector of reliability values passed to the \code{\link{sry.wateres}} function.
#' @param ... Further arguments passed to the \code{\link{sry.wateres}} function (as \code{prob_type} or \code{upper_limit}).
#' @return A \code{wateres_alpha_beta} object which is a data.table consisting of:
#'   \item{alpha}{level of development, given as the \code{alphas} argument}
#'   \item{beta}{ratio of storage representing the given reliability and volume of mean annual flow}
#'   \item{reliability}{given reliability values (may differ from reliabilities calculated by \code{\link{sry.wateres}})}
#' @details An error occurs if the range given by \code{upper_limit} does not contain the given value of reliability. In that case, try to increase
#'   the \code{upper_limit} argument.
#' @seealso \code{\link{plot.wateres_alpha_beta}} for plotting the results, \code{\link{sry.wateres}} for calculation of a reservoir storage for the
#'   given yield and reliability
#' @export
#' @examples
#' reser = data.frame(
#'     Q = c(0.078, 0.065, 0.168, 0.711, 0.154, 0.107, 0.068, 0.057, 0.07, 0.485, 0.252, 0.236,
#'           0.498, 0.248, 0.547, 0.197, 0.283, 0.191, 0.104, 0.067, 0.046, 0.161, 0.16, 0.094),
#'     DTM = seq(as.Date("2000-01-01"), by = "months", length.out = 24))
#' reser = as.wateres(reser, storage = 14.4e6, area = 754e3)
#' alpha_beta = alpha_beta(reser)
alpha_beta.wateres <- function(reser, alphas = seq(0, 1, 0.02), max_beta = 2, reliability = "max", ...) {
    resul = rbindlist(lapply(reliability, alpha_beta_wateres, reser, alphas, max_beta, ...))
    resul$reliability = as.factor(resul$reliability)
    class(resul) = c("wateres_alpha_beta", class(resul))
    return(resul)
}

alpha_beta_wateres <- function(reliability, reser, alphas, max_beta, ...) {
    Q_annual = mean(reser$Q)
    yields = alphas * Q_annual
    resul = sapply(1:length(yields), function(i) { resul = sry(reser, reliability = reliability, yield = yields[i], ...); c(NA, resul$storage, NA) })
    resul = as.data.table(t(resul))
    setnames(resul, c("alpha", "beta", "reliability"))
    resul$alpha = alphas
    resul[, reliability := as.character(reliability)]
    resul$reliability = as.character(reliability)
    resul$beta = resul$beta / (.Call("convert_m3", PACKAGE = "wateres", Q_annual, 1, TRUE) * 365.25 * 24 * 60)
    betas_max_pos = which(resul$beta > max_beta)
    if (length(betas_max_pos) > 0)
        resul = resul[1:(betas_max_pos[1] - 1), ]
    return(resul)
}

#' Plot of alpha and beta characteristics
#'
#' Plots characteristics for given reliabilities stored in a \code{wateres_alpha_beta} object, by using the \code{ggplot2} package.
#'
#' @param x A \code{wateres_alpha_beta} object.
#' @param filename A file name where the plot will be saved. If not specified, the plot will be printed to the current device.
#' @param width Plot width in inches (or a unit specified by the \code{units} argument).
#' @param height Plot height in inches (or a unit specified by the \code{units} argument).
#' @param ... Further arguments passed to the \code{\link[ggplot2:ggsave]{ggsave}} function saving the plot to a file.
#' @return A \code{ggplot} object.
#' @export
#' @examples
#' reser = data.frame(
#'     Q = c(0.078, 0.065, 0.168, 0.711, 0.154, 0.107, 0.068, 0.057, 0.07, 0.485, 0.252, 0.236,
#'           0.498, 0.248, 0.547, 0.197, 0.283, 0.191, 0.104, 0.067, 0.046, 0.161, 0.16, 0.094),
#'     DTM = seq(as.Date("2000-01-01"), by = "months", length.out = 24))
#' reser = as.wateres(reser, storage = 14.4e6, area = 754e3)
#' alpha_beta = alpha_beta(reser)
#' plot(alpha_beta)
plot.wateres_alpha_beta <- function(x, filename = NULL, width = 8, height = 6, ...) {
    check_plot_pkgs()

    p = ggplot2::ggplot(x, ggplot2::aes(x = beta, y = alpha, colour = reliability)) + ggplot2::geom_line()
    p = p + ggplot2::scale_x_continuous("beta [\u2013]") + ggplot2::scale_y_continuous("alpha [\u2013]")
    p = p + ggplot2::scale_colour_discrete(name = "reliability", labels = levels(x$reliability))
    p = p + ggplot2::theme(legend.position = "bottom")

    save_plot_file(p, filename, width, height, ...)
    return(p)
}
