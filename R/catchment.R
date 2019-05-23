# finds ID of the first reservoir situated downstreams from the place at the branch of branch_id identifier and at the position given by connect_to_part
get_first_down_reservoir <- function(res_data, branches, branch_id, connect_to_part) {
    if (is.na(branch_id)) {
        return("outlet") # TDD check keyword
    }

    res_data_branch = res_data[res_data$branch_id == branch_id,]
    if (nrow(res_data_branch) < 1) { # branch with no reservoir
        return(get_first_down_reservoir(res_data, branches, branches[[branch_id]]$down_id, branches[[branch_id]]$connect_to_part))
    }
    else {
        res_data_branch = res_data_branch[order(res_data_branch$part),]
        pos = which(connect_to_part <= res_data_branch$part)

        if (length(pos) == 0) { # connected below the last reservoir of the branch
            return(get_first_down_reservoir(res_data, branches, branches[[branch_id]]$down_id, branches[[branch_id]]$connect_to_part))
        }
        else {
            return(res_data_branch[pos, "id"])
        }
    }
}

#' Catchment with water reservoirs creation
#'
#' Creates an object of system of provided wateres reservoirs organized in a catchment.
#'
#' @param id A catchment ID.
#' @param down_id An ID of catchment located downwards.
#' @param data A data frame containing time series for the catchment: dates as `DTM`, runoff in mm as `R`,
#'   optionally precipitation in mm as `P`, potential evapotranspiration in mm as `PET` and water use in mm
#'   as `WU` (see details how the water use is handled).
#' @param area Whole catchment area in km2.
#' @param res_data A data frame containing columns describing catchment reservoirs: `storage` means potential
#'   storage in m3, `area` flooded area for the storage, `part` is area of the reservoir catchment relative
#'   to the whole catchment area, `branch_id` an ID of the reservoir branch (they have to be provided in the
#'   `branches` argument) and `id` an identifier of the reservoir. Optional column `part_wateruse` contains
#'   parts of water use assigned to individual reservoirs (see also details).
#' @param branches A list of individual branches with reservoirs; list names correspond to reservoir IDs.
#'   Each branch is represented by a list consisting of an ID of the downstream branch (`down_id`; NA for
#'   the main branch in the catchment) and a point where the branch is connected to the downstream branch
#'   (`connect_to_part`; not relevant for the main branch). The connection point is given as a catchment
#'   area (relative to the whole catchment area) of the downstream branch after the junction with this branch,
#'   i.e. including the area of the connecting branch.
#' @param main_branch An ID of the main branch, i.e. inflow from upstream catchments goes to this branch.
#' @param res_wateruse A list of time series of water use in m3 for individual reservoirs. The list names
#'   corresponds to reservoir IDs (for catchment outlet, use `outlet`). If not NULL, this overrides
#'   any water use data given in the `data` argument.
#' @param res_properties A list whose keys are property names (as used in [`set_property`]) containing lists
#'   whose keys are reservoir IDs and contain corresponding values.
#' @return A `catchment` object which is also of list class.
#' @details Water use can be given directly as time series for reservoirs in m3 (as the `res_wateruse` argument) or
#'   for the whole catchment in mm (within the `data` argument). If the direct series are given, the water use in
#'   `data` is ignored. Water use for the whole catchment is divided to individual reservoirs (or to the outlet).
#'   By default, the division is done proportionally to the area of intercatchment belonging to the reservoir or outlet.
#'   However, the default divison can be overriden by setting the `part_wateruse` column of the `res_data`
#'   argument. Sum of the parts has to be lesser than 1, the rest to 1 represents the part for the outlet.
#'
#'   An error occurs if there is a branch (downstream or in `res_data`) which has not been provided
#'   in the `branches` list or if branch connecting point does not comply with catchment parts (if sum of part
#'   of the connecting branch and part of the corresponding reservoir at the downstream branch is greater than
#'   part for the connecting point).
#' @export
#' @md
#' @examples
#' data_catch = data.frame(DTM = seq(as.Date("1982-11-01"), length.out = 7, by = "day"), PET = rep(0.5, 7), R = rep(24 * 3.6, 7), P = rep(1, 7))
#' res_data = data.frame(
#'     storage = c(1e7, 1e7, 1e7), area = c(1e4, 1e4, 1e4), part = c(0.25, 0.25, 0.5), branch_id = c("main", "lateral", "lateral"), id = c("M1", "L1", "L2"))
#' branches = list(main = list(down_id = NA), lateral = list(down_id = "main", connect_to_part = 0.8))
#' catch = as.catchment(id = "C1", down_id = "C2", data = data_catch, area = 100, res_data = res_data, branches = branches, main_branch = "main")
as.catchment <- function(id, down_id, data, area, res_data, branches, main_branch, res_wateruse = NULL, res_properties = NULL) {
    # TDD check data, res_data
    data$Q = data$R * 1e3 * area / (24 * 3600) # TDD general time step, dtto as.wateres
    if (!is.null(data$WU)) {
        data$WU = data$WU * 1e3 * area
    }
    res_data$id = as.character(res_data$id)

    branches_from_res = unique(as.character(res_data$branch_id))
    if (!(main_branch %in% names(branches))) {
        stop("Main branch '", main_branch, "' is not available in the list of branches.")
    }
    if (!all(branches_from_res %in% names(branches))) {
        stop("Not all branches given in reservoir properties are available in the list of branches.")
    }
    down_branches = sapply(branches, function(branch) { return(branch$down_id) })
    if (!all(down_branches[!is.na(down_branches)] %in% names(branches))) {
        stop("Not all branches given as downstream branches are available in the list of branches.")
    }

    res_branches = list()
    for (b in 1:length(branches)) {
        branch = branches[[b]]
        branch_id = names(branches)[[b]]

        res_dframe = res_data[res_data$branch_id == branch_id,]
        if (nrow(res_dframe) < 1) {
            warning("Branch '", branch_id, "' is not used for any reservoir.")
            next
        }
        res_dframe = res_dframe[order(res_dframe$part),]
        if (!is.na(branch$down_id)) {
            if (max(res_dframe$part) > branch$connect_to_part) {
                stop("Branch '", branch_id, "' cannot take larger part of catchment than corresponding part of downstream branch.")
            }
            res_dframe_down = res_data[res_data$branch_id == branch$down_id & res_data$part < branch$connect_to_part,]
            if (nrow(res_dframe_down) > 0 && max(res_dframe_down$part) + max(res_dframe$part) > branch$connect_to_part) {
                stop("Branch '", branch_id, "' is connected to branch '", branch$down_id, "' that way that areas of reservoirs of these branches are invalid.")
            }
        }
        res_dframe$down_id = NA
        for (res in 1:nrow(res_dframe)) {
            if (res + 1 <= nrow(res_dframe)) {
                res_dframe[res, "down_id"] = paste0(id, "_", res_dframe[res + 1, "id"])
            }
            else {
                res_dframe[res, "down_id"] = paste0(id, "_", get_first_down_reservoir(res_data, branches, branch_id, res_dframe$part + 1e-7)) # epsilon to get the next reservoir, not the same
            }
        }
        res_branches[[branch_id]] = res_dframe
    }

    reservoirs = list()
    for (branch in names(branches)) {
        res_branch = res_branches[[branch]]
        if (is.null(res_branch)) {
            next
        }
        for (res in 1:nrow(res_branch)) {
            curr_res = res_branch[res,]
            curr_res$id = paste0(id, "_", curr_res$id)
            curr_res_ts = data.frame(DTM = data$DTM, Q = data$Q * curr_res$part)
            reservoirs[[curr_res$id]] = as.wateres(curr_res_ts, curr_res$storage, curr_res$area, time_step = "day", id = curr_res$id, down_id = curr_res$down_id)
            if (!is.null(data$PET)) {
                reservoirs[[curr_res$id]] = set_evaporation(reservoirs[[curr_res$id]], data$PET)
            }
            if (!is.null(data$P)) {
                reservoirs[[curr_res$id]] = set_precipitation(reservoirs[[curr_res$id]], data$P)
            }
            if (!is.null(res_properties)) {
                id_without_catch = gsub(paste0(id, "_"), "", curr_res$id, fixed = TRUE)
                for (property in names(res_properties)) {
                    if (!is.null(res_properties[[property]][[id_without_catch]])) {
                        reservoirs[[curr_res$id]] = set_property(reservoirs[[curr_res$id]], property, res_properties[[property]][[id_without_catch]])
                    }
                }
            }
            attr(reservoirs[[curr_res$id]], "branch_id") = branch
        }
    }
    reservoirs[[paste0(id, "_outlet")]] = as.wateres(data.frame(DTM = data$DTM, Q = data$Q), 0, 0, time_step = "day")
    attr(reservoirs[[paste0(id, "_outlet")]], "id") = paste0(id, "_outlet")
    attr(reservoirs[[paste0(id, "_outlet")]], "down_id") = NA
    attr(reservoirs[[paste0(id, "_outlet")]], "branch") = main_branch

    if (!is.null(res_wateruse)) {
        for (res in 1:length(reservoirs)) {
            curr_res_id = attr(reservoirs[[res]], "id")
            id_without_catch = gsub(paste0(id, "_"), "", curr_res_id, fixed = TRUE)
            if (!is.null(res_wateruse[[id_without_catch]])) {
                reservoirs[[res]] = set_wateruse(reservoirs[[res]], res_wateruse[[id_without_catch]])
            }
        }
    }
    else if (!is.null(data$WU)) {
        if (!is.null(res_data$part_wateruse)) {
            if (sum(res_data$part_wateruse) > 1) {
                stop("Sum of parts for water use cannot be greater than 1.")
            }
            part_inter = c(res_data$part_wateruse, 1 - sum(res_data$part_wateruse))
            names(part_inter) = paste0(id, "_", c(res_data$id, "outlet"))
        }
        else {
            part_inter = part_total = c(res_data$part, 1)
            names(part_inter) = names(part_total) = paste0(id, "_", c(res_data$id, "outlet"))

            for (reser in reservoirs) {
                curr_down_id = attr(reser, "down_id")
                if (!is.na(curr_down_id)) {
                    part_inter[curr_down_id] = part_inter[curr_down_id] - part_total[attr(reser, "id")]
                }
            }
        }
        for (res in 1:length(reservoirs)) {
            reservoirs[[res]] = set_wateruse(reservoirs[[res]], data$WU * part_inter[attr(reservoirs[[res]], "id")])
        }
    }

    attr(reservoirs, "id") = id
    attr(reservoirs, "down_id") = down_id
    if (length(res_branches[[main_branch]]) > 0) {
        first_main_res = res_branches[[main_branch]]$id[1]
    }
    else {
        first_main_res = "outlet"
    }
    attr(reservoirs, "main_branch") = main_branch
    attr(reservoirs, "first_main_res") = paste0(id, "_", first_main_res)
    class(reservoirs) = c("catchment", "list")
    return(reservoirs)
}

#' Creation of system of catchments
#'
#' Creates system of catchments with reservoirs.
#'
#' @param ... Objects of `catchment` class.
#' @details An error occurs if a given downstream catchment is not available.
#' @export
#' @md
#' @examples
#' data_catch = data.frame(DTM = seq(as.Date("1982-11-01"), length.out = 7, by = "day"), PET = rep(0.5, 7), R = rep(24 * 3.6, 7), P = rep(1, 7))
#' res_data_c1 = data.frame(
#'     storage = c(1e7, 1e7, 1e7), area = c(1e4, 1e4, 1e4), part = c(0.25, 0.25, 0.5), branch_id = c("main", "lateral", "lateral"), id = c("M1", "L1", "L2"))
#' branches = list(main = list(down_id = NA), lateral = list(down_id = "main", connect_to_part = 0.8))
#' res_data_c2 = res_data_c1
#' res_data_c2$storage = res_data_c2$storage * 2
#'
#' catch1 = as.catchment(id = "C1", down_id = "C2", data = data_catch, area = 100, res_data = res_data_c1, branches = branches, main_branch = "main")
#' catch2 = as.catchment(id = "C2", down_id = NA, data = data_catch, area = 200, res_data = res_data_c2, branches = branches, main_branch = "main")
#' catch_system = as.catchment_system(catch1, catch2)
as.catchment_system <- function(...) {
    catchments = list(...) # TDD check unique catchment ID, reservoir names
    down_ids_to_process = c()
    for (catch in 1:length(catchments)) {    
        names(catchments)[catch] = attr(catchments[[catch]], "id")
        if (!is.na(attr(catchments[[catch]], "down_id"))) {
            down_ids_to_process = c(down_ids_to_process, attr(catchments[[catch]], "down_id"))
        }
    }
    
    processed_catchs = c()
    while (length(processed_catchs) < length(catchments)) {
        for (catch in 1:length(catchments)) {
            curr_catch_id = attr(catchments[[catch]], "id")
            curr_catch_down_id = attr(catchments[[catch]], "down_id")
            if (!(curr_catch_id %in% down_ids_to_process)) {
                curr_Q_outlet = catchments[[curr_catch_id]][[paste0(curr_catch_id, "_outlet")]]$Q
                for (res_down in names(catchments[[curr_catch_down_id]])) {
                    if (attr(catchments[[curr_catch_down_id]][[res_down]], "branch") == attr(catchments[[curr_catch_down_id]], "main_branch")) {
                        catchments[[curr_catch_down_id]][[res_down]]$Q = catchments[[curr_catch_down_id]][[res_down]]$Q + curr_Q_outlet
                    }
                }
                if (!is.na(curr_catch_down_id)) {
                    if (!(curr_catch_down_id %in% names(catchments))) {
                        stop("Catchment '", curr_catch_down_id, "' defined as downstream of '", curr_catch_id, "' is not available.")
                    }
                    attr(catchments[[curr_catch_id]][[paste0(curr_catch_id, "_outlet")]], "down_id") = attr(catchments[[curr_catch_down_id]], "first_main_res")
                }
                
                down_ids_to_process = down_ids_to_process[-which(down_ids_to_process == curr_catch_down_id)[1]]
                processed_catchs = c(processed_catchs, curr_catch_id)
            }
        }
    }
    all_reservoirs = list()
    for (catch in 1:length(catchments)) {    
        for (res in 1:length(catchments[[catch]])) {
            all_reservoirs[[names(catchments[[catch]])[res]]] = catchments[[catch]][[res]]
        }
    }  
    names(all_reservoirs) = NULL
    result = do.call(as.system, all_reservoirs)
    attr(result, "catchment_names") = names(catchments)
    class(result) = c("catchment_system", "wateres_system", "list")
    return(result)
}

#' @rdname calc_catchment_system.catchment_system
#' @export
calc_catchment_system <- function(system, yields, initial_storages, output_vars) UseMethod("calc_catchment_system")

#' Calculation of system of catchments with reservoirs
#'
#' Calculates time series of variables for reservoirs organized in a system of catchments.
#'
#' @param system A `catchment_system` object.
#' @param yields A vector of required fixed yield values in m3.s-1, its names have to correspond with the names of the reservoirs in the system.
#'   If not provided, their values are taken from the `yield` property of that reservoirs (if available).
#' @param initial_storages A vector of initial reservoir storages in m3 whose names correspond to the reservoirs names. If NULL, all reservoirs
#'   are considered to be full initially.
#' @param output_vars A vector of variables whose time series are returned. Variable names are those returned by `calc_series`
#'   (`inflow`, `storage`, `yield`, `precipitation`, `evaporation`, `wateruse` and `deficit`).
#' @return A list of data frames with time series for individual reservoirs.
#' @details No transfers between reservoirs are considered (`system_plain` option of [`calc_system`] is used).
#' @seealso [`calc_system`] for inner function calculating the reservoir system, [`calc_series`] for returned time series
#' @export
#' @md
#' @examples
#' data_catch = data.frame(DTM = seq(as.Date("1982-11-01"), length.out = 7, by = "day"), PET = rep(0.5, 7), R = rep(24 * 3.6, 7), P = rep(1, 7))
#' res_data_c1 = data.frame(
#'     storage = c(1e7, 1e7, 1e7), area = c(1e4, 1e4, 1e4), part = c(0.25, 0.25, 0.5), branch_id = c("main", "lateral", "lateral"), id = c("M1", "L1", "L2"))
#' branches = list(main = list(down_id = NA), lateral = list(down_id = "main", connect_to_part = 0.8))
#' res_data_c2 = res_data_c1
#' res_data_c2$storage = res_data_c2$storage * 2
#'
#' catch1 = as.catchment(id = "C1", down_id = "C2", data = data_catch, area = 100, res_data = res_data_c1, branches = branches, main_branch = "main")
#' catch2 = as.catchment(id = "C2", down_id = NA, data = data_catch, area = 200, res_data = res_data_c2, branches = branches, main_branch = "main")
#' catch_system = as.catchment_system(catch1, catch2)
#'
#' yields = c(C1_M1 = 25, C1_L1 = 25, C1_L2 = 25, C2_M1 = 25, C2_L1 = 25, C2_L2 = 200)
#' resul = calc_catchment_system(catch_system, yields, output_vars = c("storage", "yield", "precipitation"))
calc_catchment_system.catchment_system <- function(system, yields = NULL, initial_storages = NULL, output_vars = c("storage", "yield")) {
    catch_names = attr(system, "catchment_names")
    yields[paste0(catch_names, "_outlet")] = 0
    if (!is.null(initial_storages)) {
        initial_storages[paste0(catch_names, "_outlet")] = 0
    }
    resul = calc_system(system, yields, initial_storages, types = "system_plain")$system_plain
    resul_by_catchs = list()
    for (r in 1:length(resul)) {
        res = resul[[r]]
        catch_res_id = names(resul)[r]
        catch_id = unlist(strsplit(catch_res_id, "_", fixed = TRUE))[1]
        res_id = gsub(paste0(catch_id, "_"), "", catch_res_id, fixed = TRUE)
        for (out_var in output_vars) {
            if (out_var %in% names(res)) {
                if (is.null(resul_by_catchs[[catch_id]])) {
                    resul_by_catchs[[catch_id]] = data.frame(out_var = resul[[catch_res_id]][[out_var]])
                }
                else {
                    resul_by_catchs[[catch_id]] = cbind(resul_by_catchs[[catch_id]], resul[[catch_res_id]][[out_var]])
                }
                names(resul_by_catchs[[catch_id]])[length(resul_by_catchs[[catch_id]])] = paste0(res_id, "_", out_var)
            }
        }
    }
    return(resul_by_catchs)
}
