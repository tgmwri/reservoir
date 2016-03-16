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
#' riv = as.wateres(riv_data, 14.4e6, 754e3, id = "riv", down_id = "thar")
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

# finds recursively the bottom reservoir within a given system
find_bottom_id <- function(system, id, found_ids) {
    next_down = attr(system[[id]], "down_id")
    if (next_down %in% found_ids)
        stop("There is a cycle in the system of reservoirs.")
    else if (is.na(next_down))
        return(id)
    else {
        found_ids = c(found_ids, next_down)
        return(find_bottom_id(system, next_down, found_ids))
    }
}

# remove reservoir from system including adjusting down_ids
remove_reser <- function(system, res_no) {
    # remove from downstream reservoirs
    system = lapply(
        system, function(res, id_to_remove, id_to_replace) {
            curr_id_down = attr(res, "down_id")
            if (!is.na(curr_id_down) && curr_id_down == id_to_remove)
                attr(res, "down_id") = id_to_replace
            return(res)
        },
        attr(system[[res_no]], "id"), attr(system[[res_no]], "down_id"))
    system[[res_no]] = NULL
    return(system)
}

#' @rdname check.wateres_system
#' @export
check <- function(system) UseMethod("check")

#' Check of system of reservoirs
#'
#' Checks if reservoirs in the system are correctly interconnected and if their time series share the same period.
#'
#' @param system A \code{wateres_system} object.
#' @return A \code{wateres_system} object with modified or removed reservoirs (which do not conform to the check rules).
#' @details A warning is given if:
#'   \itemize{
#'     \item{any downstream reservoir does not exist in the system (NA is set instead of the downstream ID
#'       in the system to be returned),}
#'     \item{a reservoir does not contain monthly data (it is removed and downstream IDs of affected reservoirs are adjusted),}
#'     \item{a reservoir is not connected to the first reservoir in the system (it is removed),}
#'     \item{a reservoir has not dates of time series identical to the first reservoir in the system (its time series are shortened
#'       or the reservoir is removed completely in case of no intersection).}
#'   }
#'   An error occurs if the system structure forms a cycle.
#' @export
#' @examples
#' period = seq(as.Date("2000-01-01"), by = "months", length.out = 24)
#' riv_data = data.frame(
#'     Q =  c(0.111, 0.339, 0.723, 0.165, 0.14, 0.088, 0.098, 0.052, 0.034, 0.022, 0.152, 0.162,
#'         0.156, 0.19, 0.259, 0.142, 0.075, 0.054, 0.118, 0.119, 0.267, 0.105, 0.194, 0.126),
#'     DTM = period)
#' riv = as.wateres(riv_data, 14.4e6, 754e3, id = "riv", down_id = "thar")
#' thar_data = data.frame(
#'     Q =  c(9.275, 32.586, 64.53, 16.702, 12.749, 9.646, 6.748, 6.645, 4.018, 3.523, 3.118, 4.009,
#'         7.137, 20.377, 47.467, 15.501, 8.199, 7.014, 7.086, 6.769, 9.038, 4.859, 12.006, 22.218),
#'     DTM = period)
#' thar = as.wateres(thar_data, 41.3e6, 2672e3, id = "thar")
#' sys = as.system(riv, thar)
#' sys_modif = check(sys)
check.wateres_system <- function(system) {
    for (res in 1:length(system)) {
        curr_id_down = attr(system[[res]], "down_id")
        if (!is.na(curr_id_down)) {
            if (!curr_id_down %in% names(system)) {
                warning(paste0("Reservoir '", attr(system[[res]], "id"), "': downstream reservoir '", curr_id_down, "' does not exist."))
                attr(system[[res]], "down_id") = NA
            }
        }
    }
    for (res in length(system):1) {
        if (attr(system[[res]], "time_step") != "month") {
            warning(paste0("Reservoir '", attr(system[[res]], "id"), "' will not be used because it does not contain monthly data."))
            system = remove_reser(system, res)
        }
    }
    bottom_id = find_bottom_id(system, attr(system[[1]], "id"), c())
    for (res in length(system):1) {
        curr_id = attr(system[[res]], "id")
        tmp_found_ids = curr_id
        curr_bottom_id = find_bottom_id(system, curr_id, tmp_found_ids)
        if (curr_bottom_id != bottom_id) {
            warning(paste0("Reservoir '", curr_id, "' will not be used because it is not connected to the reservoir '", bottom_id, "'."))
            system = remove_reser(system, res)
        }
    }
    common_ts = as.character(system[[1]]$DTM)
    for (res in length(system):min(length(system), 2)) {
        tmp_common = intersect(common_ts, as.character(system[[res]]$DTM))
        if (length(tmp_common) == 0) {
            warning(paste0("Reservoir '", attr(system[[res]], "id"), "' will not be used because of different dates of time series."))
            system = remove_reser(system, res)
        }
        else
            common_ts = tmp_common
    }
    for (res in 1:length(system)) {
        if (length(system[[res]]$DTM) != length(common_ts)) {
            warning(paste0("Time series for reservoir '", attr(system[[res]], "id"), "' will be shortened to common period for all reservoirs."))
            tmp_attrs = attributes(system[[res]])
            system[[res]] = system[[res]][system[[res]]$DTM %in% as.Date(common_ts), ]
            attributes(system[[res]]) = tmp_attrs
        }
    }
    return(system)
}
