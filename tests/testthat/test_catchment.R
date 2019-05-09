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
    resul = calc_catchment_system(catch_system, yields)
    resul = resul$system_plain

    expect_equal(as.data.frame(resul$C1_M1), data.frame(inflow = rep(25, 7), storage = rep(1e7, 7), yield = rep(25.00006, 7), precipitation = rep(10, 7), evaporation = rep(5, 7), wateruse = rep(0, 7), deficit = rep(0, 7)), tolerance = 1e-5)
    expect_equal(as.data.frame(resul$C1_L1), as.data.frame(resul$C1_M1))
    expect_equal(as.data.frame(resul$C1_L2), data.frame(inflow = rep(50.00006, 7), storage = rep(1e7, 7), yield = rep(50.00012, 7), precipitation = rep(10, 7), evaporation = rep(5, 7), wateruse = rep(0, 7), deficit = rep(0, 7)), tolerance = 1e-5)
    expect_equal(as.data.frame(resul$C1_outlet), data.frame(inflow = rep(100.0002, 7), storage = rep(0, 7), yield = rep(100.0002, 7), precipitation = rep(0, 7), evaporation = rep(0, 7), wateruse = rep(0, 7), deficit = rep(0, 7)), tolerance = 1e-5)
    expect_equal(as.data.frame(resul$C2_M1), data.frame(inflow = rep(150.0002, 7), storage = rep(2e7, 7), yield = rep(150.0002, 7), precipitation = rep(10, 7), evaporation = rep(5, 7), wateruse = rep(0, 7), deficit = rep(0, 7)), tolerance = 1e-5)
    expect_equal(as.data.frame(resul$C2_L1), data.frame(inflow = rep(50, 7), storage = rep(2e7, 7), yield = rep(50.00006, 7), precipitation = rep(10, 7), evaporation = rep(5, 7), wateruse = rep(0, 7), deficit = rep(0, 7)), tolerance = 1e-5)
    expect_equal(as.data.frame(resul$C2_L2), data.frame(inflow = rep(100.0001, 7), storage = c(11360010, 2720020, rep(0, 5)), yield = c(rep(200, 2), 131.4818, rep(100, 4)), precipitation = rep(10, 7), evaporation = rep(5, 7), wateruse = rep(0, 7), deficit = c(rep(0, 2), 5919970, rep(8639990, 4))), tolerance = 1e-5)
    expect_equal(as.data.frame(resul$C2_outlet), data.frame(inflow = c(rep(400.0002, 2), 331.4821, rep(300.0003, 4)), storage = rep(0, 7), yield = c(rep(400.0002, 2), 331.4821, rep(300.0003, 4)), precipitation = rep(0, 7), evaporation = rep(0, 7), wateruse = rep(0, 7), deficit = rep(0, 7)), tolerance = 1e-5)
})

test_that("system with no main or lateral reservoir is calculated", {
    res_data_c1$branch_id = rep("main", 3)
    res_data_c2 = res_data_c1
    res_data_c2$storage = res_data_c2$storage * 2
    res_data_c2$branch_id = rep("lateral", 3)

    catch1 = expect_warning(as.catchment(id = "C1", down_id = "C2", data = data_catch, area = 100, res_data = res_data_c1, branches = branches, main_branch = "main"), "'lateral' is not used for any reservoir")
    catch2 = expect_warning(as.catchment(id = "C2", down_id = NA, data = data_catch, area = 200, res_data = res_data_c2, branches = branches, main_branch = "main"), "'main' is not used for any reservoir")
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
