# data - DTM, R, PET
# area - in km2
as.catchment <- function(id, down_id, data, area, res_data) {
    # TDD check data, res_data
    data$Q = data$R * 1e3 * area / (24 * 3600) # TDD general time step, dtto as.wateres

    res_branches = list(main = res_data[res_data$is_main,], lateral = res_data[!res_data$is_main,])
    res_branches = lapply(res_branches, function(res_dframe) {
        if (nrow(res_dframe) < 1) {
            return(NULL)
        }
        res_dframe = res_dframe[order(res_dframe$part),]
        res_dframe$down_id = NA
        for (res in 1:nrow(res_dframe)) {
            if (res + 1 <= nrow(res_dframe)) {
                res_dframe[res, "down_id"] = paste0(id, "_", res_dframe[res + 1, "id"])
            }
            else {
                res_dframe[res, "down_id"] = paste0(id, "_outlet") # TDD check reserved keyword
            }
        }
        return(res_dframe)
    })
    
    reservoirs = list()
    for (branch in c("main", "lateral")) {
        res_branch = res_branches[[branch]]
        if (is.null(res_branch)) {
            next
        }
        for (res in 1:nrow(res_branch)) {
            curr_res = res_branch[res,]
            curr_res$id = paste0(id, "_", curr_res$id)
            curr_res_ts = data.frame(DTM = data$DTM, Q = data$Q * curr_res$part)
            reservoirs[[curr_res$id]] = as.wateres(curr_res_ts, curr_res$storage, curr_res$area, time_step = "day", id = curr_res$id, down_id = curr_res$down_id)
            reservoirs[[curr_res$id]] = set_evaporation(reservoirs[[curr_res$id]], data$PET)
            attr(reservoirs[[curr_res$id]], "is_main") = branch == "main"
            # TDD set wateruse etc.
        }
    }
    reservoirs[[paste0(id, "_outlet")]] = as.wateres(data.frame(DTM = data$DTM, Q = data$Q), 0, 0, time_step = "day")
    attr(reservoirs[[paste0(id, "_outlet")]], "is_main") = TRUE
    attr(reservoirs[[paste0(id, "_outlet")]], "id") = paste0(id, "_outlet")
    attr(reservoirs[[paste0(id, "_outlet")]], "down_id") = NA
    attr(reservoirs, "id") = id
    attr(reservoirs, "down_id") = down_id
    if (length(res_branches$main) > 0) {
        first_main_res = res_branches$main$id[1]
    }
    else {
        first_main_res = "outlet"
    }
    attr(reservoirs, "first_main_res") = paste0(id, "_", first_main_res)
    class(reservoirs) = c("catchment", "list")
    return(reservoirs)
}

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
                    if (attr(catchments[[curr_catch_down_id]][[res_down]], "is_main")) {
                        catchments[[curr_catch_down_id]][[res_down]]$Q = catchments[[curr_catch_down_id]][[res_down]]$Q + curr_Q_outlet
                    }
                }
                if (!is.na(attr(catchments[[catch]], "down_id"))) {
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
    class(result) = c("catchment_system", "wateres_system", "list")
    return(result)
}

calc_catchment_system <- function(system, yields) {
    return(calc_system(system, yields, types = "system_plain"))
}
