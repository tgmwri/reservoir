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
find_bottom_id <- function(system, id, found_ids, all = FALSE) {
    next_down = attr(system[[id]], "down_id")
    if (next_down %in% found_ids)
        stop("There is a cycle in the system of reservoirs.")
    else if (is.na(next_down)) {
        if (all)
            return(found_ids)
        else
            return(id)
    }
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
            system[[res]] = resize_input(system[[res]], common_ts[1], common_ts[length(common_ts)])
        }
    }
    return(system)
}

# calculates simply each reservoir in the system by using its input data
calc_single <- function(system, yields, init_pos = 1, resul = NULL, only_part_ts = FALSE) {
    if (is.null(resul))
        resul = list()
    min_last_pos = nrow(system[[1]])
    for (res in 1:length(system)) {
        curr_id = attr(system[[res]], "id")
        if (only_part_ts) {
            # previous time step needs to be calculated because transfers were set there but the balance was not recalculated with them
            # therefore the next deficit which will stop the calculation (first_def_pos) cannot be in the same time
            init_pos_prev = if (init_pos == 1) 1 else init_pos - 1
            tmp_resul = calc_series(
                system[[res]], yield = yields[curr_id],
                initial_storage = if (init_pos > 2) resul[[curr_id]]$storage[init_pos - 2] else attr(system[[res]], "storage"),
                initial_pos = init_pos_prev,
                last_pos = min_last_pos, till_def = TRUE, first_def_pos = init_pos)

            last_pos = init_pos_prev - 1 + nrow(tmp_resul)
            if (is.null(resul[[curr_id]])) {
                resul[[curr_id]] = tmp_resul
                if (nrow(resul[[curr_id]]) < nrow(system[[res]])) {
                    tmp_dt = as.data.table(matrix(nrow = nrow(system[[res]]) - nrow(tmp_resul), ncol = ncol(tmp_resul)))
                    setnames(tmp_dt, names(tmp_resul))
                    resul[[curr_id]] = rbind(resul[[curr_id]], tmp_dt)
                }
            }
            else {
                if (ncol(tmp_resul) > ncol(resul[[curr_id]])) {
                    resul[[curr_id]]$transfer = 0
                }
                else if (ncol(tmp_resul) < ncol(resul[[curr_id]])) {
                    tmp_resul$transfer = 0
                }
                resul[[curr_id]][init_pos_prev:last_pos] = tmp_resul
            }
            if (last_pos < min_last_pos)
                min_last_pos = last_pos
        }
        else {
            resul[[curr_id]] = calc_series(system[[res]], yield = yields[curr_id])
        }
    }
    return(resul)
}

# find upper reservoirs for each reservoir in the system
set_up_ids <- function(system) {
    for (res in 1:length(system)) {
        curr_down = attr(system[[res]], "down_id")
        if (!is.na(curr_down)) {
            attr(system[[curr_down]], "up_ids") = c(attr(system[[curr_down]], "up_ids"), names(system)[res])
        }
    }
    return(system)
}

# calculates maximum transferred volume for reservoirs in the system
# starting from leaves, water exceeding deficits moved downwards and accumulated in the bottom reservoir to be redistributed later
calc_max_transfer <- function(system, resers_done, series, def_pos) {
    bottom_id = find_bottom_id(system, names(system)[[1]], c())
    new_resers_done = c()
    for (curr_res in system) {
        curr_id = attr(curr_res, "id")
        if (curr_id %in% resers_done)
            next
        curr_up = attr(curr_res, "up_ids")
        if (is.null(curr_up) || all(curr_up %in% resers_done)) {
            if (curr_id != bottom_id) {
                curr_down = attr(curr_res, "down_id")
                curr_max_transfer = series[[curr_id]]$storage[def_pos] + sum(attr(system[[curr_id]], "from_up")) # current storage and transfer from upper reservoirs
                if (curr_max_transfer > 0) {
                    if (series[[curr_id]]$deficit[def_pos] > 0) { # storage is zero
                        def_ratio = series[[curr_id]]$deficit[def_pos] / curr_max_transfer
                    }
                    else { # storage greater than zero
                        def_ratio = 0
                    }
                    for (res_id in names(system)) { # only upstream reservoirs are in from_up, so it is safe to copy all of them (rest is 0)
                        attr(system[[curr_down]], "from_up")[res_id] = attr(system[[curr_down]], "from_up")[res_id] + (1 - def_ratio) * attr(system[[curr_id]], "from_up")[res_id]
                        attr(system[[curr_id]], "from_up")[res_id] = attr(system[[curr_id]], "from_up")[res_id] - (1 - def_ratio) * attr(system[[curr_id]], "from_up")[res_id]
                    }
                    attr(system[[curr_down]], "from_up")[curr_id] = attr(system[[curr_down]], "from_up")[curr_id] + series[[curr_id]]$storage[def_pos]
                }
            }
            new_resers_done = c(new_resers_done, curr_id)
        }
    }
    resers_done = c(resers_done, new_resers_done)
    if (all.equal(sort(names(system)), sort(resers_done)) == TRUE)
        return(system)
    else
        calc_max_transfer(system, resers_done, series, def_pos)
}

# set transfers recursively for time steps with deficit from given time step to the end
set_transfers_from_pos <- function(system, yields, init_pos, series = NULL) {
    series = calc_single(system, yields, init_pos, series, only_part_ts = TRUE)
    defs_sum = rowSums(as.data.frame(lapply(series, function(x) { x$deficit })))
    def_pos = which(defs_sum > 0)
    def_pos = def_pos[def_pos >= init_pos]
    if (length(def_pos) > 0) {
        def_pos = def_pos[1]
        for (res in 1:length(system)) {
            attr(system[[res]], "from_up") = vector("numeric", length(system))
            names(attr(system[[res]], "from_up")) = names(system)
        }
        system = calc_max_transfer(system, c(), series, def_pos)

        # return the exceeding transfered volume from the bottom reservoir
        bottom_res = find_bottom_id(system, names(system)[[1]], c())
        sum_from_up = sum(attr(system[[bottom_res]], "from_up"))
        if (sum_from_up > 0)
            attr(system[[bottom_res]], "from_up") = attr(system[[bottom_res]], "from_up") * min(1, series[[bottom_res]]$deficit[def_pos] / sum_from_up)

        # add transfers to input time series
        for (res_id in names(system)) {
            for (from_res_id in names(attr(system[[res_id]], "from_up"))) {
                curr_from_up = attr(system[[res_id]], "from_up")[from_res_id]
                if (curr_from_up > 0) {
                    system[[res_id]]$T[def_pos] = system[[res_id]]$T[def_pos] + curr_from_up
                    system[[from_res_id]]$T[def_pos] = system[[from_res_id]]$T[def_pos] - curr_from_up
                }
            }
        }
        set_transfers_from_pos(system, yields, def_pos + 1, series)
    }
    else {
        return(system)
    }
}

#' @rdname calc_deficits.wateres_system
#' @export
calc_deficits <- function(system, yields) UseMethod("calc_deficits")

#' Calculation of system of reservoirs with respect to deficits
#'
#' Calculates time series of variables for reservoirs in the system in order to estimate influence of the system on deficit volumes.
#' Two variants are outputted: firstly, each reservoir is calculated independently of the system; secondly, water transfers decreasing
#' deficit volumes are added to the system.
#'
#' The purpose of this system calculation is a rough estimation of deficit volumes elimination by the system. Therefore, water storage
#' is just redistributed within the system. Reservoir outflows are not considered as inflow to the reservoirs downwards; input time series of inflow
#' for individual reservoirs are used instead.
#'
#' The water redistribution is carried out independently for the time steps when deficit in any of the reservoirs occurs. Future time steps are not
#' considered, i.e. it is possible that deficit decrease in a time step will cause deficit increase in the next time step.
#'
#' For a given time step, the following algorithm is applied:
#'
#' For each reservoir, the maximum water volume available to be transferred is determined (starting from top reservoirs and continuing with reservoirs
#' whose upper reservoirs have been already processed).
#' \itemize{
#'   \item{If sum of current reservoir storage and potential transfer is greater than zero, this sum is equal to the potential transfer from
#'         the current reservoir to the reservoir downwards.}
#'   \item{Otherwise, there is a deficit which can be satisfied by the potential transfer from upper reservoirs; then the exceeding water is equal to the potential transfer
#'         to the reservoir downwards. }}
#' Finally, volume resulting from all potential transfers is collected in the bottom reservoir. If it is greater than the bottom reservoir deficit,
#' the exceeding part is redistributed back to the corresponding source reservoirs. The only criterion of the redistribution is volume of the transfer, distance
#' between reservoirs is not taken into account. (The same redistribution method is applied when information about source reservoir is determined --
#' this happens if there is a potential transfer from another reservoir than bottom whose deficit is zero or has been completely satisfied.)
#'
#' If provided in the reservoir object in the \code{system}, the time series of water balance variables are included the same way
#' as in the \code{\link{calc_series}} function. Other arguments of this function are set to their default values.
#'
#' The system structure is checked by the \code{\link{check}} function before the calculation starts.
#'
#' @param system A \code{wateres_system} object.
#' @param yields A vector of required fixed yield values in m3.s-1, its names have to correspond with the names of the reservoirs in the system.
#' @return A list consisting of two items: \code{single} with the results for independently calculated reservoirs and \code{system} with the results
#'   for the system. Each of the items is a list of the \code{wateres_series} objects for individual reservoirs. The object contains the water
#'   balance variables returned by the \code{\link{calc_series}} functions. Moreover, \code{transfer} variable is added for the system results if has
#'   non-zero value at least in one time step.
#' @seealso \code{\link{calc_series}} for calculating individual reservoirs
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
#' resul = calc_deficits(sys, c(riv = 0.14, thar = 8))
calc_deficits.wateres_system <- function(system, yields) {
    system = check(system)
    system = set_up_ids(system)

    yields = yields[names(yields) %in% names(system)]
    if (anyNA(yields) || length(yields) < length(system))
        stop("Yields are not provided for all reservoirs in the system.")

    resul = list(single = list(), system = list())
    resul$single = calc_single(system, yields = yields)

    # set transfer variable to be filled in set_transfers_from_pos
    for (res in 1:length(system)) {
        system[[res]]$T = 0
    }
    system = set_transfers_from_pos(system, yields, 1)
    resul$system = calc_single(system, yields = yields)
    return(resul)
}
