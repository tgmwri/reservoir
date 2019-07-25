context("calculation of system of reservoirs defined by catchments")

data_catch = data.frame(DTM = seq(as.Date("1982-11-01"), length.out = 7, by = "day"), PET = rep(0.5, 7), R = rep(24 * 3.6, 7), P = rep(1, 7))
res_data_c1 = data.frame(
    storage = c(1e7, 1e7, 1e7), area = c(1e4, 1e4, 1e4), part = c(0.25, 0.25, 0.5), branch_id = c("main", "lateral", "lateral"), id = c("M1", "L1", "L2"),
    stringsAsFactors = FALSE)
branches = list(main = list(down_id = NA), lateral = list(down_id = "main", connect_to_part = 0.8))

test_that("simple system of catchment reservoirs is calculated", {
    res_data_c2 = res_data_c1
    res_data_c2$storage = res_data_c2$storage * 2

    catch1 = as.catchment(id = "C1", down_id = "C2", data = data_catch, area = 100, res_data = res_data_c1, branches = branches, main_branch = "main")
    catch2 = as.catchment(id = "C2", down_id = NA, data = data_catch, area = 200, res_data = res_data_c2, branches = branches, main_branch = "main")
    catch_system = as.catchment_system(catch1, catch2)

    yields = c(C1_M1 = 25, C1_L1 = 25, C1_L2 = 25, C2_M1 = 25, C2_L1 = 25, C2_L2 = 200)
    resul = calc_catchment_system(catch_system, yields, output_vars = c("inflow", "storage", "yield", "precipitation", "evaporation", "wateruse", "deficit"))

    expect_equal(as.data.frame(resul$C1[, 1:7]), data.frame(M1_inflow = rep(25, 7), M1_storage = rep(1e7, 7), M1_yield = rep(25.00006, 7), M1_precipitation = rep(10, 7), M1_evaporation = rep(5, 7), M1_wateruse = rep(0, 7), M1_deficit = rep(0, 7)), tolerance = 1e-5)
    tmp_df = as.data.frame(resul$C1[, 1:7])
    names(tmp_df) = gsub("M1", "L1", names(tmp_df), fixed = TRUE)
    expect_equal(as.data.frame(resul$C1[, 8:14]), tmp_df)
    expect_equal(as.data.frame(resul$C1[, 15:21]), data.frame(L2_inflow = rep(50.00006, 7), L2_storage = rep(1e7, 7), L2_yield = rep(50.00012, 7), L2_precipitation = rep(10, 7), L2_evaporation = rep(5, 7), L2_wateruse = rep(0, 7), L2_deficit = rep(0, 7)), tolerance = 1e-5)
    expect_equal(as.data.frame(resul$C1[, 22:28]), data.frame(outlet_inflow = rep(100.0002, 7), outlet_storage = rep(0, 7), outlet_yield = rep(100.0002, 7), outlet_precipitation = rep(0, 7), outlet_evaporation = rep(0, 7), outlet_wateruse = rep(0, 7), outlet_deficit = rep(0, 7)), tolerance = 1e-5)
    expect_equal(as.data.frame(resul$C2[, 1:7]), data.frame(M1_inflow = rep(150.0002, 7), M1_storage = rep(2e7, 7), M1_yield = rep(150.0002, 7), M1_precipitation = rep(10, 7), M1_evaporation = rep(5, 7), M1_wateruse = rep(0, 7), M1_deficit = rep(0, 7)), tolerance = 1e-5)
    expect_equal(as.data.frame(resul$C2[, 8:14]), data.frame(L1_inflow = rep(50, 7), L1_storage = rep(2e7, 7), L1_yield = rep(50.00006, 7), L1_precipitation = rep(10, 7), L1_evaporation = rep(5, 7), L1_wateruse = rep(0, 7), L1_deficit = rep(0, 7)), tolerance = 1e-5)
    expect_equal(as.data.frame(resul$C2[, 15:21]), data.frame(L2_inflow = rep(100.0001, 7), L2_storage = c(11360010, 2720020, rep(0, 5)), L2_yield = c(rep(200, 2), 131.4818, rep(100, 4)), L2_precipitation = rep(10, 7), L2_evaporation = rep(5, 7), L2_wateruse = rep(0, 7), L2_deficit = c(rep(0, 2), 5919970, rep(8639990, 4))), tolerance = 1e-5)
    expect_equal(as.data.frame(resul$C2[, 22:28]), data.frame(outlet_inflow = c(rep(400.0002, 2), 331.4821, rep(300.0003, 4)), outlet_storage = rep(0, 7), outlet_yield = c(rep(400.0002, 2), 331.4821, rep(300.0003, 4)), outlet_precipitation = rep(0, 7), outlet_evaporation = rep(0, 7), outlet_wateruse = rep(0, 7), outlet_deficit = rep(0, 7)), tolerance = 1e-5)

    initial_storages = c(C1_M1 = 1e7, C1_L1 = 1e7, C1_L2 = 1e7, C2_M1 = 0, C2_L1 = 2e7, C2_L2 = 2e7)
    resul_init = calc_catchment_system(catch_system, yields, initial_storages, output_vars = c("inflow", "storage", "yield", "precipitation", "evaporation", "wateruse", "deficit"))
    expect_equal(resul_init$C1, resul$C1)
    expect_equal(resul_init$C2[, 8:21], resul$C2[, 8:21])
    expect_equal(as.data.frame(resul_init$C2[, 1:7]), data.frame(M1_inflow = rep(150.0002, 7), M1_storage = c(10800020, rep(2e7, 6)), M1_yield = c(25, 43.51898, rep(150.0002, 5)), M1_precipitation = rep(10, 7), M1_evaporation = rep(5, 7), M1_wateruse = rep(0, 7), M1_deficit = rep(0, 7)), tolerance = 1e-5)
    expect_equal(as.data.frame(resul_init$C2[, 22:28]), data.frame(outlet_inflow = c(275, 293.519, 331.4821, rep(300.0003, 4)), outlet_storage = rep(0, 7), outlet_yield = c(275, 293.519, 331.4821, rep(300.0003, 4)), outlet_precipitation = rep(0, 7), outlet_evaporation = rep(0, 7), outlet_wateruse = rep(0, 7), outlet_deficit = rep(0, 7)), tolerance = 1e-5)

    # catchments given as a list
    catch_system2 = as.catchment_system(list(catch1, catch2))
    resul2 = calc_catchment_system(catch_system2, yields, output_vars = c("inflow", "storage", "yield", "precipitation", "evaporation", "wateruse", "deficit"))
    expect_equal(resul2, resul)
})

test_that("system with no main or lateral reservoir is calculated", {
    res_data_c1$branch_id = rep("main", 3)
    res_data_c2 = res_data_c1
    res_data_c2$storage = res_data_c2$storage * 2
    res_data_c2$branch_id = rep("lateral", 3)

    catch1 = expect_warning(as.catchment(id = "C1", down_id = "C2", data = data_catch, area = 100, res_data = res_data_c1, branches = branches, main_branch = "main"), "'lateral' is not used for any reservoir")
    catch2 = as.catchment(id = "C2", down_id = NA, data = data_catch, area = 200, res_data = res_data_c2, branches = branches, main_branch = "main")
    catch_system = as.catchment_system(catch1, catch2)

    yields = c(C1_M1 = 25, C1_L1 = 25, C1_L2 = 25, C2_M1 = 25, C2_L1 = 25, C2_L2 = 200)
    resul = calc_catchment_system(catch_system, yields)
})

test_that("invalid inputs for catchment are recognized", {
    make_catchment <- function() {
        as.catchment(id = "C1", down_id = "C2", data = data_catch, area = 100, res_data = res_data_c1, branches = branches, main_branch = "main")
    }
    res_data_c1$branch_id[3] = "lateral_unavailable"
    expect_error(make_catchment(), "Not all branches given in reservoir properties are available")
    res_data_c1$branch_id[3] = "lateral"
    branches$lateral$down_id = "lateral_unavailable"
    expect_error(make_catchment(), "Not all branches given as downstream branches are available")
    branches$lateral$down_id = "main"
    branches$lateral$connect_to_part = 0.4
    expect_error(make_catchment(), "cannot take larger part of catchment")
    branches$lateral$connect_to_part = 0.5
    expect_error(make_catchment(), "areas of reservoirs of these branches are invalid")
    branches$lateral$connect_to_part = 0.8
    expect_error(as.catchment_system(make_catchment()), "'C2' defined as downstream of 'C1' is not available")
})

test_that("structure of reservoirs is created correctly", {
    res_data_c1 = data.frame(
        storage = rep(1e7, 5), area = rep(1e2, 5), part = c(0.1, 0.9, 0.1, 0.3, 0.1), branch_id = c("main", "main", "lateral", "lateral", "small"),
        id = c("M1", "M2", "L1", "L2", "S1"))
    branches = list(main = list(down_id = NA), lateral = list(down_id = "main", connect_to_part = 0.7), small = list(down_id = "lateral", connect_to_part = 0.5))
    catch1 = as.catchment(id = "C1", down_id = "C2", data = data_catch, area = 100, res_data = res_data_c1, branches = branches, main_branch = "main")

    get_attributes <- function(reser, attrs) {
        sapply(attrs, function(attr) {
            attr(reser, attr)
        })
    }
    expect_true(all(get_attributes(catch1$C1_M1, c("id", "down_id", "branch_id")) == c("C1_M1", "C1_M2", "main")))
    expect_true(all(get_attributes(catch1$C1_M2, c("id", "down_id", "branch_id")) == c("C1_M2", "C1_outlet", "main")))
    expect_true(all(get_attributes(catch1$C1_L1, c("id", "down_id", "branch_id")) == c("C1_L1", "C1_L2", "lateral")))
    expect_true(all(get_attributes(catch1$C1_L2, c("id", "down_id", "branch_id")) == c("C1_L2", "C1_M2", "lateral")))
    expect_true(all(get_attributes(catch1$C1_S1, c("id", "down_id", "branch_id")) == c("C1_S1", "C1_M2", "small")))
})

test_that("water use is included to catchments", {
    make_catchment <- function(...) {
        as.catchment(id = "C1", down_id = NA, data = data_catch, area = 100, res_data = res_data_c1, branches = branches, main_branch = "main", ...)
    }
    data_catch$WU = rep(4, 7)
    data_catch$WU[5:7] = -1 * data_catch$WU[5:7]
    catch_system = as.catchment_system(make_catchment())

    yields = c(C1_M1 = 25, C1_L1 = 25, C1_L2 = 25)
    resul = calc_catchment_system(catch_system, yields, output_vars = c("storage", "yield", "wateruse"))
    expect_equal(resul$C1$M1_wateruse, c(rep(1e5, 4), rep(-1e5, 3)))
    expect_equal(resul$C1$M1_storage, c(rep(1e7, 4), 9900005, 9800010, 9700015))
    expect_equal(resul$C1$M1_yield, c(rep(26.15747, 4), rep(25, 3)), tolerance = 1e-5)
    expect_equal(resul$C1$L1_wateruse, c(rep(1e5, 4), rep(-1e5, 3)))
    expect_equal(resul$C1$L2_wateruse, c(rep(1e5, 4), rep(-1e5, 3)))
    expect_equal(resul$C1$outlet_yield, c(rep(104.62980, 4), rep(97.68524, 3)), tolerance = 1e-5)
    expect_equal(resul$C1$outlet_wateruse, c(rep(1e5, 4), rep(-1e5, 3)))

    res_data_c1$part_wateruse = c(0.5, 0.4, 0.15)
    expect_error(make_catchment(), "Sum of parts for water use cannot be greater than 1")
    res_data_c1$part_wateruse = c(0.5, 0.1, 0.15)
    catch_system = as.catchment_system(make_catchment())
    resul = calc_catchment_system(catch_system, yields, output_vars = c("storage", "yield", "wateruse"))
    expect_equal(resul$C1$M1_wateruse, c(rep(2e5, 4), rep(-2e5, 3)))
    expect_equal(resul$C1$M1_storage, c(rep(1e7, 4), 9800005, 9600010, 9400015))
    expect_equal(resul$C1$M1_yield, c(rep(27.31487, 4), rep(25, 3)), tolerance = 1e-5)
    expect_equal(resul$C1$L1_wateruse, c(rep(4e4, 4), rep(-4e4, 3)))
    expect_equal(resul$C1$L2_wateruse, c(rep(6e4, 4), rep(-6e4, 3)))
    expect_equal(resul$C1$outlet_yield, c(rep(104.62980, 4), rep(98.14821, 3)), tolerance = 1e-5)
    expect_equal(resul$C1$outlet_wateruse, c(rep(1e5, 4), rep(-1e5, 3)))

    res_wateruse = list(M1 = c(rep(1e5, 4), rep(-1e5, 3)), L1 = c(rep(3e4, 4), rep(-3e4, 3)), L2 = c(rep(5e4, 4), rep(-5e4, 3)), outlet = c(rep(1e5, 4), rep(-1e5, 3)))
    catch_system = as.catchment_system(make_catchment(res_wateruse = res_wateruse))
    resul = calc_catchment_system(catch_system, yields, output_vars = c("storage", "yield", "wateruse"))
    expect_equal(resul$C1$M1_wateruse, c(rep(1e5, 4), rep(-1e5, 3)))
    expect_equal(resul$C1$M1_storage, c(rep(1e7, 4), 9900005, 9800010, 9700015))
    expect_equal(resul$C1$M1_yield, c(rep(26.15747, 4), rep(25, 3)), tolerance = 1e-5)
    expect_equal(resul$C1$L1_wateruse, c(rep(3e4, 4), rep(-3e4, 3)))
    expect_equal(resul$C1$L2_wateruse, c(rep(5e4, 4), rep(-5e4, 3)))
    expect_equal(resul$C1$outlet_yield, c(rep(103.24091, 4), rep(98.26395, 3)), tolerance = 1e-5)
    expect_equal(resul$C1$outlet_wateruse, c(rep(1e5, 4), rep(-1e5, 3)))
})

test_that("reservoir properties are applied", {
    res_properties = list(storage_optim = list(M1 = 8e6, L1 = 8e6, L2 = 3e6), yield = list(M1 = 30, L1 = 30, L2 = 60), yield_max = list(M1 = 40, L1 = 40, L2 = 70))
    catch1 = as.catchment(id = "C1", down_id = NA, data = data_catch, area = 100, res_data = res_data_c1, branches = branches, main_branch = "main", res_properties = res_properties)
    catch_system = as.catchment_system(catch1)
    resul = calc_catchment_system(catch_system, output_vars = c("storage", "yield"))
    expect_equal(resul$C1$M1_storage, c(8704005, 8000000, 7568005, 7136010, 6704015, 6272020, 5840025))
    expect_equal(resul$C1$M1_yield, c(40, 33.14826, rep(30, 5)), tolerance = 1e-5)
    expect_equal(resul$C1$L1_storage, resul$C1$M1_storage)
    expect_equal(resul$C1$L1_yield, resul$C1$M1_yield)
    expect_equal(resul$C1$L2_storage, c(9568005, 8544020, 7248025, 5952030, 4656035, 3360040, 2928045))
    expect_equal(resul$C1$L2_yield, c(rep(70, 6), 60), tolerance = 1e-5)

    # yields in property overriden by yields in argument
    yields = c(C1_M1 = 25, C1_L1 = 25, C1_L2 = 25)
    resul = expect_warning(calc_catchment_system(catch_system, yields, output_vars = c("storage", "yield")), "Yield given as property of reservoir 'C1_M1' will be overriden")
    expect_equal(resul$C1$M1_storage, c(8704005, rep(8e6, 6)))
    expect_equal(resul$C1$M1_yield, c(40, 33.14826, rep(25.00006, 5)), tolerance = 1e-5)
})

test_that("initial storage from properties is used", {
    res_properties = list(storage_initial = list(M1 = 5e6, L1 = 5e6, L2 = 5e6))
    catch1 = as.catchment(id = "C1", down_id = NA, data = data_catch, area = 100, res_data = res_data_c1, branches = branches, main_branch = "main", res_properties = res_properties)
    catch_system = as.catchment_system(catch1)
    resul = calc_catchment_system(catch_system, yield = c(C1_M1 = 30, C1_L1 = 30, C1_L2 = 60), output_vars = c("storage"))
    expect_equal(resul$C1$M1_storage, c(4568005, 4136010, 3704015, 3272020, 2840025, 2408030, 1976035))
})

test_that("warning about unused branch is shown", {
    catch = expect_warning(
        as.catchment(id = "C1", down_id = NA, data = data_catch, area = 100, res_data = NULL, branches = branches, main_branch = "main"),
        "Branch 'lateral' is not used for any reservoir")
})

test_that("catchment with no reservoir is included", {
    catch1 = as.catchment(id = "C1", down_id = "C2", data = data_catch, area = 100, res_data = res_data_c1, branches = branches, main_branch = "main")
    catch2 = as.catchment(id = "C2", down_id = NA, data = data_catch, area = 200, res_data = NULL, branches = list(main = list(down_id = NA)), main_branch = "main")
    catch_system = as.catchment_system(catch1, catch2)

    yields = c(C1_M1 = 25, C1_L1 = 25, C1_L2 = 25, C2_M1 = 25, C2_L1 = 25, C2_L2 = 200)
    resul = calc_catchment_system(catch_system, yields)
    expect_equal(resul$C1$M1_yield, rep(25.00006, 7), tolerance = 1e-5)
    expect_equal(resul$C2$outlet_yield, rep(300.0002, 7), tolerance = 1e-5)
})

test_that("catchment inputs in data.table produces no warning", {
    library(data.table)
    res_data_c1 = data.table(
        storage = c(1e7), area = c(1e4), part = c(0.25), branch_id = c("main"), id = c("M1"))
    branches = list(main = list(down_id = NA))
    expect_warning(as.catchment(id = "C1", down_id = NA, data = data_catch, area = 100, res_data = res_data_c1, branches = branches, main_branch = "main"), regexp = NA)
})

test_that("catchment inflows are calculated correctly", {
    branches = list(main = list(down_id = NA))
    catch1 = as.catchment(id = "C1", down_id = "C3", data = data_catch, area = 100, res_data = NULL, branches = branches, main_branch = "main")
    catch2 = as.catchment(id = "C2", down_id = "C3", data = data_catch, area = 100, res_data = NULL, branches = branches, main_branch = "main")
    catch3 = as.catchment(id = "C3", down_id = NA, data = data_catch, area = 200, res_data = NULL, branches = branches, main_branch = "main")

    catch_system = as.catchment_system(catch1, catch3, catch2)

    resul = calc_catchment_system(catch_system, output_vars = "yield")
    expect_equal(c(resul$C1$outlet_yield, resul$C2$outlet_yield, resul$C3$outlet_yield), c(rep(100, 14), rep(400, 7)))
})

test_that("branches are not needed to be given if only one is there", {
    catch1 = as.catchment(id = "C1", down_id = "C3", data = data_catch, area = 100, res_data = NULL, branches = NULL, main_branch = "main")
    catch2 = as.catchment(id = "C2", down_id = "C3", data = data_catch, area = 100, res_data = NULL, main_branch = "main")
    catch3 = as.catchment(id = "C3", down_id = NA, data = data_catch, area = 200, res_data = NULL, main_branch = "main")

    catch_system = as.catchment_system(catch1, catch3, catch2)

    resul = calc_catchment_system(catch_system, output_vars = "yield")
    expect_equal(c(resul$C1$outlet_yield, resul$C2$outlet_yield, resul$C3$outlet_yield), c(rep(100, 14), rep(400, 7)))
})

test_that("first main reservoir is correct if reservoir data are as data.table", {
    catch1 = as.catchment(id = "C1", down_id = "C2", data = data_catch, area = 100, res_data = res_data_c1, branches = branches, main_branch = "main")
    res_data_c2 = data.table(storage = 1e7, area = 1e4, part = 0.25, branch_id = "lateral", id = "L1")
    branches_c2 = list(main = list(down_id = NA), lateral = list(down_id = "main", connect_to_part = 0.8))
    catch2 = as.catchment(id = "C2", down_id = NA, data = data_catch, area = 100, res_data = res_data_c2, branches = branches_c2, main_branch = "main")
    expect_equal(attr(catch2, "first_main_res"), "C2_outlet")
})

test_that("downstream reservoirs are correct if reservoir data are as data.table", {
    catch1 = as.catchment(id = "C1", down_id = "C2", data = data_catch, area = 100, res_data = data.table(res_data_c1), branches = branches, main_branch = "main")
    expect_equal(attr(catch1$C1_L2, "down_id"), "C1_outlet")
})

test_that("routing of catchment outflow is calculated", {
    catch1 = as.catchment(id = "C1", down_id = NA, data = data_catch, area = 100, res_data = res_data_c1, branches = branches, main_branch = "main")
    catch1 = set_routing(catch1, "lag", list(lag_time = 3200))
    catch_system = as.catchment_system(catch1)
    resul = calc_catchment_system(catch_system,  yield = c(C1_M1 = 30, C1_L1 = 30, C1_L2 = 60), output_vars = c("yield", "yield_unrouted"))
    expect_equal(resul$C1$outlet_yield, c(0, 0, 89.444444, rep(115, 4)))
    expect_equal(resul$C1$outlet_yield_unrouted, rep(115, 7))
})
