context("calculation of system of reservoirs")

test_that("system of four hourly empty reservoirs is calculated", {
    res_a1 = as.wateres(data.frame(Q = c(5, 7, 9, 4, 3, 3)), 1e7, 1e2, id = "A1", down_id = "B", time_step = "day")
    res_a2 = as.wateres(data.frame(Q = c(7, 11, 12, 6, 5, 4)), 1e7, 1e2, id = "A2", down_id = "B", time_step = "day")
    res_b = as.wateres(data.frame(Q = c(18, 24, 25, 15, 14, 23)), 1e7, 1e2, id = "B", down_id = "C", time_step = "day")
    res_c = as.wateres(data.frame(Q = c(25, 33, 33, 22, 23, 32)), 1e7, 1e2, id = "C", time_step = "day")

    sys = as.system(res_a1, res_a2, res_b, res_c)
    yields = c(A1 = 4, A2 = 7, B = 20, C = 29)
    initial_storages = c(A1 = 0, A2 = 0, B = 0, C = 0)
    resul = calc_system(sys, yields, initial_storages, c("single_plain", "system_plain", "single_transfer", "system_transfer"))

    # intercatchment flows directly given
    inter_res_a1 = as.wateres(data.frame(Q = c(5, 7, 9, 4, 3, 3)), 1e7, 1e2, id = "A1", down_id = "B", time_step = "day")
    inter_res_a2 = as.wateres(data.frame(Q = c(7, 11, 12, 6, 5, 4)), 1e7, 1e2, id = "A2", down_id = "B", time_step = "day")
    inter_res_b = as.wateres(data.frame(Q = rep(NA, 6), QI = c(6, 6, 4, 5, 6, 16)), 1e7, 1e2, id = "B", down_id = "C", time_step = "day")
    inter_res_c = as.wateres(data.frame(Q = rep(NA, 6), QI = c(7, 9, 8, 7, 9, 9)), 1e7, 1e2, id = "C", time_step = "day")

    inter_sys = as.system(inter_res_a1, inter_res_a2, inter_res_b, inter_res_c)
    inter_resul = calc_system(inter_sys, yields, initial_storages, types = c("system_plain", "system_transfer"))
    expect_equal(resul[c("system_plain", "system_transfer")], inter_resul)

    sin_plain = resul$single_plain
    sin_transfer = resul$single_transfer
    sys_plain = resul$system_plain
    sys_transfer = resul$system_transfer

    expect_equal(sin_plain$A1$storage, 86400 * c(1, 4, 9, 9, 8, 7))
    expect_equal(sin_plain$A1$yield, rep(4, 6))
    expect_equal(sin_plain$A1$deficit, rep(0, 6))
    expect_equal(sin_plain$A2$storage, 86400 * c(0, 4, 9, 8, 6, 3))
    expect_equal(sin_plain$A2$yield, rep(7, 6))
    expect_equal(sin_plain$A2$deficit, rep(0, 6))
    expect_equal(sin_plain$B$storage, 86400 * c(0, 4, 9, 4, 0, 3))
    expect_equal(sin_plain$B$yield, c(18, 20, 20, 20, 18, 20))
    expect_equal(sin_plain$B$deficit, 86400 * c(2, 0, 0, 0, 2, 0))
    expect_equal(sin_plain$C$storage, 86400 * c(0, 4, 8, 1, 0, 3))
    expect_equal(sin_plain$C$yield, c(25, 29, 29, 29, 24, 29))
    expect_equal(sin_plain$C$deficit, 86400 * c(4, 0, 0, 0, 5, 0))

    expect_equal(sin_transfer$A1$storage, 86400 * c(0, 3, 8, 8, 3.23077, 2.23077), tolerance = 1e-5)
    expect_equal(sin_transfer$A1$yield, rep(4, 6))
    expect_equal(sin_transfer$A1$deficit, rep(0, 6))
    expect_equal(sin_transfer$A1$transfer, 86400 * c(-1, 0, 0, 0, -3.76923, 0), tolerance = 1e-5)
    expect_equal(sin_transfer$A2$storage, 86400 * c(0, 4, 9, 8, 2.76923, 0), tolerance = 1e-5)
    expect_equal(sin_transfer$A2$yield, c(rep(7, 5), 6.76923), tolerance = 1e-5)
    expect_equal(sin_transfer$A2$deficit, 86400 * c(rep(0, 5), 0.23077), tolerance = 1e-5)
    expect_equal(sin_transfer$A2$transfer, 86400 * c(0, 0, 0, 0, -3.23077, 0), tolerance = 1e-5)
    expect_equal(sin_transfer$B$storage, 86400 * c(0, 4, 9, 4, 0, 3))
    expect_equal(sin_transfer$B$yield, c(19, rep(20, 5)))
    expect_equal(sin_transfer$B$deficit, 86400 * c(1, rep(0, 5)))
    expect_equal(sin_transfer$B$transfer, 86400 * c(1, 0, 0, 0, 2, 0))
    expect_equal(sin_transfer$C$storage, 86400 * c(0, 4, 8, 1, 0, 3))
    expect_equal(sin_transfer$C$yield, c(25, rep(29, 5)))
    expect_equal(sin_transfer$C$deficit, 86400 * c(4, rep(0, 5)))
    expect_equal(sin_transfer$C$transfer, 86400 * c(rep(0, 4), 5, 0))

    expect_equal(sys_plain$A1$inflow, c(5, 7, 9, 4, 3, 3))
    expect_equal(sys_plain$A1$storage, 86400 * c(1, 4, 9, 9, 8, 7))
    expect_equal(sys_plain$A1$yield, rep(4, 6))
    expect_equal(sys_plain$A1$deficit, rep(0, 6))
    expect_equal(sys_plain$A2$inflow, c(7, 11, 12, 6, 5, 4))
    expect_equal(sys_plain$A2$storage, 86400 * c(0, 4, 9, 8, 6, 3))
    expect_equal(sys_plain$A2$yield, rep(7, 6))
    expect_equal(sys_plain$A2$deficit, rep(0, 6))
    expect_equal(sys_plain$B$inflow, c(17, 17, 15, 16, 17, 27))
    expect_equal(sys_plain$B$storage, 86400 * c(rep(0, 5), 7))
    expect_equal(sys_plain$B$yield, c(17, 17, 15, 16, 17, 20))
    expect_equal(sys_plain$B$deficit, 86400 * c(3, 3, 5, 4, 3, 0))
    expect_equal(sys_plain$C$inflow, c(24, 26, 23, 23, 26, 29))
    expect_equal(sys_plain$C$storage, 86400 * rep(0, 6))
    expect_equal(sys_plain$C$yield, c(24, 26, 23, 23, 26, 29))
    expect_equal(sys_plain$C$deficit, 86400 * c(5, 3, 6, 6, 3, 0))

    expect_equal(sys_transfer$A1$inflow, c(5, 7, 9, 4, 3, 3))
    expect_equal(sys_transfer$A1$storage, 86400 * c(0, 1.71429, 3.83673, 0.54811, 0, 0), tolerance = 1e-5)
    expect_equal(sys_transfer$A1$yield, c(rep(4, 4), 3.548105, 3))
    expect_equal(sys_transfer$A1$deficit, 86400 * c(rep(0, 4), 0.45189504, 1))
    expect_equal(sys_transfer$A1$transfer, 86400 * c(-1, -1.28571, -2.87755, -3.28862, 0, 0), tolerance = 1e-5)
    expect_equal(sys_transfer$A2$inflow, c(7, 11, 12, 6, 5, 4))
    expect_equal(sys_transfer$A2$storage, 86400 * c(0, 2.28571, 4.16327, 0.45189, 0, 0), tolerance = 1e-5)
    expect_equal(sys_transfer$A2$yield, c(rep(7, 4), 5.45189, 4), tolerance = 1e-5)
    expect_equal(sys_transfer$A2$deficit, 86400 * c(rep(0, 4), 1.54811, 3), tolerance = 1e-5)
    expect_equal(sys_transfer$A2$transfer, 86400 * c(0, -1.71429, -3.12245, -2.71138, 0, 0), tolerance = 1e-5)
    expect_equal(sys_transfer$B$inflow, c(17, 17, 15, 16, 15, 23))
    expect_equal(sys_transfer$B$storage, 86400 * c(rep(0, 5), 3))
    expect_equal(sys_transfer$B$yield, c(18, 20, 20, 20, 15, 20))
    expect_equal(sys_transfer$B$deficit, 86400 * c(2, rep(0, 3), 5, 0))
    expect_equal(sys_transfer$B$transfer, 86400 * c(1, 3, 5, 4, 0, 0))
    expect_equal(sys_transfer$C$inflow, c(25, 29, 28, 27, 24, 29))
    expect_equal(sys_transfer$C$storage, 86400 * rep(0, 6))
    expect_equal(sys_transfer$C$yield, c(25, rep(29, 3), 24, 29))
    expect_equal(sys_transfer$C$deficit, 86400 * c(4, rep(0, 3), 5, 0))
    expect_equal(sys_transfer$C$transfer, 86400 * c(0, 0, 1, 2, 0, 0))
})

test_that("system with incorrectly given intercatchment flows is checked", {
    inter_res_a1 = as.wateres(data.frame(Q = c(5, 7, 9, 4, 3, 3)), 1e7, 1e2, id = "A1", down_id = "B", time_step = "day")
    inter_res_a2 = as.wateres(data.frame(Q = c(7, 11, 12, 6, 5, 4)), 1e7, 1e2, id = "A2", down_id = "B", time_step = "day")
    inter_res_b = as.wateres(data.frame(Q = rep(NA, 6), QI = c(6, 6, 4, 5, 6, 16)), 1e7, 1e2, id = "B", down_id = "C", time_step = "day")
    inter_res_c = as.wateres(data.frame(Q = c(25, 33, 33, 22, 23, 32)), 1e7, 1e2, id = "C", time_step = "day")

    inter_sys = as.system(inter_res_a1, inter_res_a2, inter_res_b, inter_res_c)
    inter_yields = c(A1 = 4, A2 = 7, B = 20, C = 29)
    inter_initial_storages = c(A1 = 0, A2 = 0, B = 0, C = 0)
    expect_error(calc_system(inter_sys, inter_yields, inter_initial_storages, types = "system_plain"), "Missing inflow from an intercatchment")

    inter_res_b = as.wateres(data.frame(Q = rep(1, 6), QI = c(6, 6, 4, 5, 6, 16)), 1e7, 1e2, id = "B", down_id = "C", time_step = "day")
    inter_sys = as.system(inter_res_a1, inter_res_a2, inter_res_b, inter_res_c)
    expect_warning(calc_system(inter_sys, inter_yields, inter_initial_storages, types = "system_plain"), "Intercatchment flow (QI column) is given inconsistently", fixed = TRUE)
})

riv = as.wateres("rivendell.txt", 14.4e6, 754e3, id = "riv", down_id = "thar")
thar = as.wateres("tharbad.txt", 41.3e6, 2672e3, id = "thar")

test_that("system object is created", {
    expect_equivalent(as.system(riv, thar), readRDS("system_creation.rds"))
})

test_that("system object is not created if IDs are not correct", {
    riv_no_id = as.wateres("rivendell.txt", 14.4e6, 754e3)
    expect_error(as.system(riv_no_id))
    expect_error(as.system(riv, riv))
})

test_that("check of system gives appopriate errors and warnings", {
    riv_no_down = as.wateres("rivendell.txt", 14.4e6, 754e3, id = "riv", down_id = "lond")
    expect_warning(check(as.system(riv_no_down, thar)), "downstream reservoir 'lond' does not exist")

    rivh = as.wateres("rivendell_1h.txt", 14.4e6, 754e3, time_step = "hour", id = "rivh")
    expect_warning(check(as.system(rivh, thar)), "does not contain data of time step '1-hour'")

    thar_cycle = as.wateres("tharbad.txt", 41.3e6, 2672e3, id = "thar", down_id = "riv")
    expect_error(check(as.system(riv, thar_cycle)), "There is a cycle")

    riv_no_conn = as.wateres("rivendell.txt", 14.4e6, 754e3, id = "riv")
    expect_warning(check(as.system(riv_no_conn, thar)), "is not connected")

    expect_warning(check(as.system(riv, thar)), "will be shortened")

    riv_diff_period = as.wateres("rivendell.txt", 14.4e6, 754e3, id = "riv", down_id = "thar")
    riv_diff_period$DTM = seq(as.Date("2020-01-15"), by = "month", length.out = nrow(riv_diff_period))
    expect_warning(check(as.system(riv_diff_period, thar)), "because of different dates")
})

test_that("downstream ID is adjusted if a reservoir is removed", {
    riv_to_rivh = as.wateres("rivendell.txt", 14.4e6, 754e3, id = "riv", down_id = "rivh")
    riv_to_rivh = resize_input(riv_to_rivh, "1981-01-15")
    rivh = as.wateres("rivendell_1h.txt", 14.4e6, 754e3, time_step = "hour", id = "rivh", down_id = "thar")
    sys = as.system(riv_to_rivh, rivh, thar)
    expect_equal(attr(sys[["riv"]], "down_id"), "rivh")
    expect_warning(sys <- check(sys), "does not contain data of time step '1-month'")
    expect_equal(attr(sys[["riv"]], "down_id"), "thar")
})

test_that("system is adjusted after check", {
    riv = as.wateres("rivendell.txt", 14.4e6, 754e3, id = "riv", down_id = "rivh")
    riv2 = as.wateres("rivendell.txt", 14.4e6, 754e3, id = "riv2")
    rivh = as.wateres("rivendell_1h.txt", 14.4e6, 754e3, time_step = "hour", id = "rivh", down_id = "thar")
    thar = as.wateres("tharbad.txt", 41.3e6, 2672e3, id = "thar")
    sys = as.system(riv, riv2, rivh, thar)
    expect_warning(sys <- check(sys))
    expect_equivalent(sys, readRDS("system_check.rds"))
})

riv_wateruse = -4e5
riv_data = read.table("rivendell.txt", colClasses = c("Date", "numeric"), header = TRUE)
riv_data = riv_data[riv_data$DTM > as.Date("1981-01-01"), ]
riv = as.wateres(riv_data, 14.4e6, 754e3, id = "A1", down_id = "B")
riv = set_wateruse(riv, rep(riv_wateruse, nrow(riv)))
riv_mrf = 0.033

riv_paralel = as.wateres(riv_data, 14.4e6, 754e3, id = "A2", down_id = "B")
riv_paralel = set_wateruse(riv_paralel, rep(riv_wateruse, nrow(riv_paralel)))

riv2_wateruse = riv_wateruse * 2.12
riv2_data = riv_data
riv2_data$Q = riv2_data$Q * 2.12
riv2 = as.wateres(riv2_data, 14.4e6, 754e3, id = "B", down_id = "C")
riv2 = set_wateruse(riv2, rep(riv2_wateruse, nrow(riv2)))
riv2_mrf = 0.033

thar_wateruse = -1e7
thar = as.wateres("tharbad.txt", 41.3e6, 2672e3, id = "C")
thar = resize_input(thar, "1981-01-01")
thar = set_wateruse(thar, rep(thar_wateruse, nrow(thar)))
thar_mrf = 2.718

test_that("system is not calculated without yields", {
    system = as.system(riv, riv_paralel, riv2, thar)
    yields = c(A1 = riv_mrf, A2 = riv_mrf, B3 = riv2_mrf, C = thar_mrf)
    expect_error(calc_system(system, yields), "Argument 'yields' does not provide values")
})

test_that("system is not calculated if wrong calculation type is given", {
    system = as.system(riv, riv2, thar)
    yields = c(A1 = riv_mrf, B = riv2_mrf, C = thar_mrf)
    expect_error(calc_system(system, yields, types = "system"), "Unknown or ambiguous value 'system'")
    expect_error(calc_system(system, yields, types = "nesmysl"), "Unknown or ambiguous value 'nesmysl'")
})

test_that("system of four reservoirs is calculated", {
    system = as.system(riv, riv_paralel, riv2, thar)
    yields = c(A1 = riv_mrf, A2 = riv_mrf, B = riv2_mrf, C = thar_mrf)
    expect_equivalent(
        calc_system(
            system, yields, types = c("single_plain", "system_plain", "system_transfer", "single_transfer")),
        readRDS("system_4reser.rds"))

    riv2$W[108] = riv2$W[108] - 4537238.1 # decrease water use to get deficit -9e5 in reservoir B for single_plain
    system = as.system(riv, riv_paralel, riv2, thar)
    yields = c(A1 = riv_mrf, A2 = riv_mrf, B = riv2_mrf, C = thar_mrf)
    expect_equivalent(
        calc_system(
            system, yields, types = c("single_plain", "system_plain", "system_transfer", "single_transfer")),
        readRDS("system_4reser_defB.rds"))
})

test_that("system with deficit in the first time is calculated", {
    reser_data = data.frame(Q =  rep(1, 6), DTM = seq(as.Date("2000-01-01"), by = "months", length.out = 6))
    reser_A = as.wateres(reser_data, 14.4e6, 754e3, id = "A", down_id = "B")
    reser_B = as.wateres(reser_data, 0, 754e3, id = "B")
    system = as.system(reser_A, reser_B)
    resul = calc_system(system, yields = c(A = 2, B = 2), types = c("single_plain", "system_plain", "single_transfer", "system_transfer"))

    expect_equivalent(resul$single_plain$A$storage, c(11721600, 9216000, 6537600, 3945600, 1267200, 0))
    expect_equivalent(resul$single_plain$A$deficit, c(rep(0, 5), 1324800))
    expect_equivalent(resul$single_plain$B$storage, rep(0, 6))
    expect_equivalent(resul$single_plain$B$deficit, c(2678400, 2505600, 2678400, 2592000, 2678400, 2592000))

    expect_equivalent(resul$single_transfer$A$storage, c(9043200, 4032000, rep(0, 4)))
    expect_equivalent(resul$single_transfer$A$deficit, c(rep(0, 3), 2592000, 2678400, 2592000))
    expect_equivalent(resul$single_transfer$A$transfer, c(-2678400, -2505600, -1353600, rep(0, 3)))
    expect_equivalent(resul$single_transfer$B$storage, rep(0, 6))
    expect_equivalent(resul$single_transfer$B$deficit, c(rep(0, 2), 1324800, 2592000, 2678400, 2592000))
    expect_equivalent(resul$single_transfer$B$transfer, c(2678400, 2505600, 1353600, rep(0, 3)))

    expect_equivalent(resul$system_plain$A$storage, c(11721600, 9216000, 6537600, 3945600, 1267200, 0))
    expect_equivalent(resul$system_plain$A$yield, c(rep(2, 5), 1.48888889))
    expect_equivalent(resul$system_plain$A$deficit, c(rep(0, 5), 1324800))
    expect_equivalent(resul$system_plain$B$inflow, c(rep(2, 5), 1.48888889))
    expect_equivalent(resul$system_plain$B$storage, rep(0, 6))
    expect_equivalent(resul$system_plain$B$yield, c(rep(2, 5), 1.48888889))
    expect_equivalent(resul$system_plain$B$deficit, c(rep(0, 5), 1324800))

    expect_equivalent(resul$system_transfer$A$transfer, rep(0, 6))
    expect_equivalent(resul$system_transfer$B$transfer, rep(0, 6))
    resul$system_transfer$A$transfer = NULL
    resul$system_transfer$B$transfer = NULL
    expect_equivalent(resul$system_transfer, resul$system_plain)
})
