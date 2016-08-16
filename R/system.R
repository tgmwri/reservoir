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
#'     \item{a reservoir does not contain data of time step identical with the time step of the first reservoir
#'       (then it is removed and downstream IDs of affected reservoirs are adjusted),}
#'     \item{a reservoir is not connected to the first reservoir in the system (it is removed),}
#'     \item{for daily or hourly data, a reservoir has time series of length different from time series of the first reservoir,}
#'     \item{for monthly data, a reservoir has not dates of time series identical to the first reservoir in the system
#'       (its time series are shortened or the reservoir is removed completely in case of no intersection).}
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
        if (attr(system[[res]], "time_step") != attr(system[[1]], "time_step")) {
            warning(paste0(
                "Reservoir '", attr(system[[res]], "id"), "' will not be used because it does not contain data of time step '",
                attr(system[[1]], "time_step"), "'."))
            system = remove_reser(system, res)
        }
        else if (is.null(system[[res]]$DTM) && nrow(system[[res]]) != nrow(system[[1]])) {
            warning(paste0("Reservoir '", attr(system[[res]], "id"), "' will not be used because length of its time series differs from the first reservoir."))
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
    if (!is.null(system[[1]]$DTM)) {
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
    }
    return(system)
}

# calculates simply each reservoir in the system by using its input data
calc_single <- function(system, yields, initial_storages, init_pos = 1, resul = NULL, only_part_ts = FALSE) {
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
                initial_storage = if (init_pos > 2) resul[[curr_id]]$storage[init_pos - 2] else initial_storages[curr_id],
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
            resul[[curr_id]] = calc_series(system[[res]], yield = yields[curr_id], initial_storage = initial_storages[curr_id])
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

# goes through all of the reservoirs starting from leaves and continuing to the bottom
traverse <- function(system, resers_done, inner_function, series, def_pos, use_attr_series = TRUE) {
    bottom_id = find_bottom_id(system, names(system)[[1]], c())
    new_resers_done = c()
    for (curr_res in system) {
        curr_id = attr(curr_res, "id")
        if (curr_id %in% resers_done)
            next
        curr_up = attr(curr_res, "up_ids")
        if (is.null(curr_up) || all(curr_up %in% resers_done)) {
            system = inner_function(system, series, def_pos, curr_id, bottom_id)
            if (use_attr_series)
                series = attr(system, "series")
            new_resers_done = c(new_resers_done, curr_id)
        }
    }
    resers_done = c(resers_done, new_resers_done)
    if (all.equal(sort(names(system)), sort(resers_done)) == TRUE)
        return(system)
    else
        traverse(system, resers_done, inner_function, series, def_pos, use_attr_series)
}

# calculates inflows to reservoirs from upper reservoirs plus intercatchment
calc_inflows_inner <- function(system, series, def_pos, curr_id, bottom_id, recalc_series = TRUE) {
    curr_up = attr(system[[curr_id]], "up_ids")
    if (!is.null(curr_up)) {
        tmp_len = nrow(system[[curr_id]])
        sum_up_Q = sum_up_yield = vector("numeric", tmp_len - def_pos + 1)
        for (up_id in curr_up) {
            if (recalc_series)
                series[[up_id]] = calc_series(system[[up_id]], yield = attr(system, "yields")[up_id], initial_storage = attr(system, "initial_storages")[up_id])
            sum_up_Q = sum_up_Q + system[[up_id]]$Q[def_pos:tmp_len]
            sum_up_yield = sum_up_yield + series[[up_id]]$yield[def_pos:tmp_len]
        }
        intercatch_Q = system[[curr_id]]$Q[def_pos:tmp_len] - sum_up_Q
        if (any(intercatch_Q < 0)) {
            stop("Negative inflow from an intercatchment for the reservoir '", curr_id, "' (time step ", which(intercatch_Q < 0)[1], ") is not allowed.")
        }
        system[[curr_id]]$I[def_pos:tmp_len] = sum_up_yield + intercatch_Q
    }
    return(system)
}

calc_inflows <- function(system, resers_done, series, def_pos) {
    traverse(system, resers_done, calc_inflows_inner, series, def_pos, FALSE)
}

calc_max_transfer_inner <- function(system, series, def_pos, curr_id, bottom_id) {
    # set inflows for current reservoir (influenced by new values of upstreams reservoirs)
    if (attr(system, "calc_type") != "single_transfer")
        system = calc_inflows_inner(system, series, def_pos, curr_id, bottom_id, FALSE)

    # calculate the current reservoir (input transfers have been set in previous calculation of upstream reservoirs)
    series[[curr_id]] = calc_series(system[[curr_id]], yield = attr(system, "yields")[curr_id], initial_storage = attr(system, "initial_storages")[curr_id])

    # set final transfers for current current reservoir and transfer to the downstream reservoir
    # amount from upstream reservoirs
    transfer_from_up = 0
    if (!is.null(series[[curr_id]]$transfer[def_pos]))
        transfer_from_up = series[[curr_id]]$transfer[def_pos]

    # amount available to be transferred to the downstream reservoir
    # calculated from resulting series that includes also transfer from upstream reservoir
    yield_excess = (series[[curr_id]]$yield[def_pos] - attr(system, "yields")[[curr_id]]) * 60 * system[[curr_id]]$minutes[def_pos]
    transfer_to_down = series[[curr_id]]$storage[def_pos] + max(yield_excess, 0) - series[[curr_id]]$deficit[def_pos]

    if (transfer_from_up > 0 || transfer_to_down > 0) {
        def_ratio = max(min((transfer_from_up - transfer_to_down) / transfer_from_up, 1), 0)
        res_down = attr(system[[curr_id]], "down_id")
        is_bottom = is.na(res_down)
        res_all = names(system)[names(system) != curr_id]

        # only upstream reservoirs are in from_up, so it is safe to copy all of them (rest is 0)
        if (!is_bottom) {
            res_all = res_all[res_all != res_down]
            attr(system[[res_down]], "from_up")[res_all] = attr(system[[res_down]], "from_up")[res_all] + (1 - def_ratio) * attr(system[[curr_id]], "from_up")[res_all]
            attr(system[[res_down]], "from_up")[curr_id] = attr(system[[res_down]], "from_up")[curr_id] + max(transfer_to_down - transfer_from_up, 0)
        }
        attr(system[[curr_id]], "from_up")[res_all] = attr(system[[curr_id]], "from_up")[res_all] - (1 - def_ratio) * attr(system[[curr_id]], "from_up")[res_all]

        # add transfers to input
        for (res_id in names(system)) {
            system[[res_id]]$T[def_pos] = 0
            for (from_res_id in names(attr(system[[res_id]], "from_up"))) {
                curr_from_up = attr(system[[res_id]], "from_up")[from_res_id]
                if (curr_from_up > 0) {
                    system[[res_id]]$T[def_pos] = system[[res_id]]$T[def_pos] + curr_from_up
                    system[[from_res_id]]$T[def_pos] = system[[from_res_id]]$T[def_pos] - curr_from_up
                }
            }
        }
        # recalculate the current reservoir for final transfers (to be used for downward reservoir)
        if (!is_bottom) {
            series[[curr_id]] = calc_series(system[[curr_id]], yield = attr(system, "yields")[curr_id], initial_storage = attr(system, "initial_storages")[curr_id])
        }
    }
    attr(system, "series") = series
    return(system)
}

# calculates maximum transferred volume for reservoirs in the system
# starting from leaves, water exceeding deficits moved downwards and accumulated in the bottom reservoir to be redistributed later
calc_max_transfer <- function(system, resers_done, series, def_pos) {
    traverse(system, resers_done, calc_max_transfer_inner, series, def_pos)
}

# set transfers recursively for time steps with deficit from given time step to the end
set_transfers_from_pos <- function(system, yields, initial_storages, init_pos, series = NULL) {
    series = calc_single(system, yields, initial_storages, init_pos, series, only_part_ts = TRUE)
    if (attr(system, "calc_type") != "single_transfer")
        system = calc_inflows(system, c(), series, 1) # initial calculation of inflows because then it starts from def_pos
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
        series = attr(system, "series")
        set_transfers_from_pos(system, yields, initial_storages, def_pos + 1, series)
    }
    else {
        return(system)
    }
}

#' @rdname calc_system.wateres_system
#' @export
calc_system <- function(system, yields, initial_storages, types) UseMethod("calc_system")

#' Calculation of system of reservoirs
#'
#' Calculates time series of variables for reservoirs organized in a system. Four types of calculation are available, depending on whether
#' inflows from upstream reservoirs and water transfer between reservoirs are considered.
#'
#' The types of calculation selected as the \code{types} argument are as follows:
#' \itemize{
#'   \item{\code{single_plain} - reservoirs are calculated independently of the system.}
#'   \item{\code{single_transfer} - as above, and water transfer is added to decrease deficit volumes; a fictional scenario for testing purposes.}
#'   \item{\code{system_plain} - reservoirs are calculated within the system, i.e. reservoir inflow consists of yield of corresponding upstream
#'     reservoirs and runoff from the intercatchment (derived from the original inflow series to reservoirs).}
#'   \item{\code{system_transfer} - as above, and water transfer is added.}}
#'
#' The water transfer (redistribution) is carried out independently for the time steps when deficit in any of the reservoirs occurs. Future time steps are not
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
#' @param initial_storages A vector of initial reservoir storages in m3 whose names correspond to the reservoirs names. If missing, all reservoirs
#'   are considered to be full initially.
#' @param types A vector of types of calculation whose valid values are \dQuote{single_plain}, \dQuote{system_plain}, \dQuote{single_transfer} and
#'   \dQuote{system_transfer} (see details).
#' @return A list consisting of items corresponding with the values of the \code{types} argument. Each of the items is a list of the \code{wateres_series}
#'   objects for individual reservoirs. The object contains the water balance variables returned by the \code{\link{calc_series}} functions.
#'   Moreover, \code{transfer} variable is added for the system results if has non-zero value at least in one time step.
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
#' resul = calc_system(sys, c(riv = 0.14, thar = 8))
calc_system.wateres_system <- function(system, yields, initial_storages, types = c("single_plain", "system_plain")) {
    system = check(system)
    system = set_up_ids(system)

    if (missing(initial_storages)) {
        initial_storages = sapply(names(system), function(res) { attr(system[[res]], "storage") })
    }
    for (arg in c("yields", "initial_storages")) {
        values = get(arg)
        values = values[names(values) %in% names(system)]
        if (anyNA(values) || length(values) < length(system))
            stop(paste0("Argument '", arg, "' does not provide values for all reservoirs in the system."))
    }
    attr(system, "yields") = yields
    attr(system, "initial_storages") = initial_storages

    initialize_input <- function(system, res, var, is) {
        if (is)
            system[[res]][[var]] = 0
        else if (!is.null(system[[res]][[var]]))
            system[[res]][[var]] = NULL
        return(system)
    }

    resul = list()
    all_types = c("single_plain", "system_plain", "system_transfer",  "single_transfer")
    for (ct in types) {
        matched = pmatch(ct, all_types, -1)
        if (matched == -1)
            stop("Unknown or ambiguous value '", ct, "' of argument 'types'.")
        else
            ct = all_types[matched]

        attr(system, "calc_type") = ct
        for (res in 1:length(system)) {
            system = initialize_input(system, res, "T", grepl("transfer", ct))
            if (!is.null(attr(system[[res]], "up_ids"))) {
                system = initialize_input(system, res, "I", grepl("system", ct))
            }
        }

        if (ct == "system_plain") {
            series = calc_single(system, yields, initial_storages)
            system = calc_inflows(system, c(), series, 1)
        }
        else if (grepl("transfer", ct)) {
            system = set_transfers_from_pos(system, yields, initial_storages, 1)
        }
        resul[[ct]] = calc_single(system, yields, initial_storages)
    }
    return(resul)
}
