context("calculation of system of reservoirs")

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
    expect_warning(check(as.system(rivh, thar)), "does not contain monthly data")

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
    rivh = as.wateres("rivendell_1h.txt", 14.4e6, 754e3, time_step = "hour", id = "rivh", down_id = "thar")
    sys = as.system(riv_to_rivh, rivh, thar)
    expect_equal(attr(sys[["riv"]], "down_id"), "rivh")
    sys = check(sys)
    expect_equal(attr(sys[["riv"]], "down_id"), "thar")
})

test_that("system is adjusted after check", {
    riv = as.wateres("rivendell.txt", 14.4e6, 754e3, id = "riv", down_id = "rivh")
    riv2 = as.wateres("rivendell.txt", 14.4e6, 754e3, id = "riv2")
    rivh = as.wateres("rivendell_1h.txt", 14.4e6, 754e3, time_step = "hour", id = "rivh", down_id = "thar")
    thar = as.wateres("tharbad.txt", 41.3e6, 2672e3, id = "thar")
    sys = as.system(riv, riv2, rivh, thar)
    expect_equivalent(check(sys), readRDS("system_check.rds"))
})

riv_wateruse = -4e5
riv_data = read.table("rivendell.txt", colClasses = c("Date", "numeric"), header = TRUE)
riv_data = riv_data[riv_data$DTM > as.Date("1981-01-01"), ]
riv = as.wateres(riv_data, 14.4e6, 754e3, id = "A1", down_id = "B")
riv = set_wateruse(riv, rep(riv_wateruse, nrow(riv)))
riv_mrf = 0.033

riv_paralel = as.wateres(riv_data, 14.4e6, 754e3, id = "A2", down_id = "B")
riv_paralel = set_wateruse(riv_paralel, rep(riv_wateruse, nrow(riv_paralel)))

riv2_wateruse = riv_wateruse * 1.12
riv2_data = riv_data
riv2_data$Q = riv2_data$Q * 1.12
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
    expect_error(calc_deficits(system, yields), "Yields are not provided")
})

test_that("deficits for system of four reservoirs are calculated", {
    system = as.system(riv, riv_paralel, riv2, thar)
    yields = c(A1 = riv_mrf, A2 = riv_mrf, B = riv2_mrf, C = thar_mrf)
    expect_equivalent(calc_deficits(system, yields), readRDS("system4_deficits.rds"))

    riv2$W[108] = riv2$W[108] - 5845586.9 # decrease water use to get deficit -9e5 in reservoir B
    system = as.system(riv, riv_paralel, riv2, thar)
    yields = c(A1 = riv_mrf, A2 = riv_mrf, B = riv2_mrf, C = thar_mrf)
    expect_equivalent(calc_deficits(system, yields), readRDS("system4_deficits_B.rds"))
})

test_that("deficits for system with deficit in the first time are calculated", {
    reser_data = data.frame(Q =  rep(1, 6), DTM = seq(as.Date("2000-01-01"), by = "months", length.out = 6))
    reser_A = as.wateres(reser_data, 14.4e6, 754e3, id = "A", down_id = "B")
    reser_B = as.wateres(reser_data, 0, 754e3, id = "B")
    system = as.system(reser_A, reser_B)
    resul = calc_deficits(system, yields = c(A = 2, B = 2))

    expect_equivalent(resul$single$A$storage, c(11721600, 9216000, 6537600, 3945600, 1267200, 0))
    expect_equivalent(resul$single$A$deficit, c(rep(0, 5), 1324800))
    expect_equivalent(resul$single$B$storage, rep(0, 6))
    expect_equivalent(resul$single$B$deficit, c(2678400, 2505600, 2678400, 2592000, 2678400, 2592000))
    expect_equivalent(resul$system$A$storage, c(9043200, 4032000, rep(0, 4)))
    expect_equivalent(resul$system$A$deficit, c(rep(0, 3), 2592000, 2678400, 2592000))
    expect_equivalent(resul$system$A$transfer, c(-2678400, -2505600, -1353600, rep(0, 3)))
    expect_equivalent(resul$system$B$storage, rep(0, 6))
    expect_equivalent(resul$system$B$deficit, c(rep(0, 2), 1324800, 2592000, 2678400, 2592000))
    expect_equivalent(resul$system$B$transfer, c(2678400, 2505600, 1353600, rep(0, 3)))
})
