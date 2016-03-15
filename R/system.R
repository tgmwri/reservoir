#' System of water reservoirs creation
#'
#' Creates an object of system of provided wateres reservoirs.
#'
#' @param ... Objects of the \code{wateres} class representing reservoirs of the system. The \code{id} attribute of each reservoir needs
#'   to be specified and needs to have a unique value.
#' @return A \code{wateres_system} object which is also of list class.
#' @details An error occurs if any of reservoir IDs is missing or is duplicate.
#' @seealso \code{\link{check.wateres_system}} for checking validity of the system
#' @export
#' @examples
#' period = seq(as.Date("2000-01-01"), by = "months", length.out = 24)
#' riv_data = data.frame(
#'     Q =  c(0.111, 0.339, 0.723, 0.165, 0.14, 0.088, 0.098, 0.052, 0.034, 0.022, 0.152, 0.162,
#'         0.156, 0.19, 0.259, 0.142, 0.075, 0.054, 0.118, 0.119, 0.267, 0.105, 0.194, 0.126),
#'     DTM = period)
#' riv = as.wateres(riv_data, 14.4e6, 754e3, id = "riv", id_down = "thar")
#' thar_data = data.frame(
#'     Q =  c(9.275, 32.586, 64.53, 16.702, 12.749, 9.646, 6.748, 6.645, 4.018, 3.523, 3.118, 4.009,
#'         7.137, 20.377, 47.467, 15.501, 8.199, 7.014, 7.086, 6.769, 9.038, 4.859, 12.006, 22.218),
#'     DTM = period)
#' thar = as.wateres(thar_data, 41.3e6, 2672e3, id = "thar")
#' sys = as.system(riv, thar)
as.system <- function(...) {
    system = list(...)
    for (res in 1:length(system)) {
        curr_id = attr(system[[res]], "id")
        if (is.na(curr_id))
            stop(paste0("Missing ID for reservoir given at position ", res, "."))
        if (curr_id %in% names(system))
            stop(paste0("Duplicate ID of reservoirs given at positions ", which(curr_id == names(system)[1]), " and ", res, "."))
        names(system)[res] = curr_id
    }
    class(system) = c("wateres_system", "list")
    return(system)
}

