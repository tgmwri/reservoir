#' @rdname prob_field.wateres
#' @export
prob_field <- function(reser, probs, yield, storage, throw_exceed) UseMethod("prob_field")

#' Calculation of probability fields
#'
#' Calculates monthly values of storage and yield for given probabilities by using the \code{\link{quantile}} function with the default settings.
#'
#' @param reser A wateres object.
#' @param probs A vector of required probability values.
#' @param yield A value of yield to be used for calculation of storages in the reservoir.
#' @param storage A water reservoir storage value in millions of m3, the default value is equal to the potential volume of \code{reser}.
#' @param throw_exceed Whether volume exceeding storage will be thrown or added to yield (see also \code{\link{sry.wateres}}).
#' @return A \code{wateres_prob_field} object which is a list consisting of:
#'   \item{quantiles}{data.table containing storage and yield values for months and given probabilities}
#'   \item{probs}{given probability values in percent as characters}
#' @seealso \code{\link{plot.wateres_prob_field}} for plotting the results
#' @export
#' @examples
#' reser = data.frame(
#'     Q = c(0.078, 0.065, 0.168, 0.711, 0.154, 0.107, 0.068, 0.057, 0.07, 0.485, 0.252, 0.236,
#'           0.498, 0.248, 0.547, 0.197, 0.283, 0.191, 0.104, 0.067, 0.046, 0.161, 0.16, 0.094),
#'     DTM = seq(as.Date("2000-01-01"), by = "months", length.out = 24))
#' reser = as.wateres(reser, Vpot = 14.4)
#' prob_field = prob_field(reser, c(0.9, 0.95, 0.99), 0.14)
prob_field.wateres <- function(reser, probs, yield, storage = attr(reser, "Vpot"), throw_exceed = FALSE) {
    calc_resul = .Call("calc_storage", PACKAGE = "wateres", reser$Q, reser$.days, yield, storage, throw_exceed)

    reser = cbind(reser, storage = calc_resul$storage[2:length(calc_resul$storage)], yield = calc_resul$yield)
    var_quant = list()
    for (var in c("storage", "yield")) {
        var_mon = reser[, get(var), by = month(DTM)]
        setnames(var_mon, 2, var)
        var_quant[[var]] = tapply(var_mon[[var]], var_mon$month, quantile, probs)
    }

    prob_names = names(var_quant$storage[[1]])
    quantiles = as.data.table(matrix(NA, 12, 1 + 2 * length(probs)))
    quantiles = quantiles[, names(quantiles) := lapply(.SD, as.numeric)]
    setnames(quantiles, 1:ncol(quantiles), c("month", paste0("storage_", prob_names), paste0("yield_", prob_names)))
    quantiles = quantiles[, month := 1:12]

    for (var in c("storage", "yield")) {
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
            stop(paste0("To produce a plot, ", pkg_name, " package needs to be installed."))
    }
}

#' Plot of probability field
#'
#' Plots monthly values of storage or yield stored in a given \code{wateres_prob_field} object, by using the \code{ggplot2} package.
#'
#' @param x A \code{wateres_prob_field} object.
#' @param type Type of values to be plotted (\dQuote{storage} or \dQuote{yield}).
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
#' reser = as.wateres(reser, Vpot = 14.4)
#' prob_field = prob_field(reser, c(0.9, 0.95, 0.99), 0.14)
#' plot(prob_field, "storage")
plot.wateres_prob_field <- function(x, type = "storage", filename = NULL, width = 8, height = 6, ...) {
    check_plot_pkgs()

    types = c("storage", "yield")
    units = c(storage = "mil. m\u00b3", yield = "m\u00b3.s\u207b\u00b9")
    type = types[pmatch(type, types, 1)]
    quant = copy(x$quantiles)
    col_to_remove = names(quant) == gsub(type, "", names(quant), fixed = TRUE)
    col_to_remove[1] = FALSE # month
    quant = quant[, names(quant)[col_to_remove] := NULL]

    mquant = reshape2::melt(quant, id = "month")
    if (type == "storage")
        mquant$value = mquant$value / 1e6

    p = ggplot2::ggplot(mquant, ggplot2::aes(x = month, y = value, colour = variable)) + ggplot2::geom_line()
    p = p + ggplot2::scale_x_discrete() + ggplot2::scale_y_continuous(paste0(type, " [", units[type], "]"))
    p = p + ggplot2::scale_colour_discrete(name = "probability", labels = x$probs)
    p = p + ggplot2::theme(legend.position = "bottom")

    if (!is.null(filename))
        ggplot2::ggsave(filename = filename, width = width, height = height, ...)
    else
        print(p)
    return(p)
}
