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

riv_wateruse = 4e5
riv_data = read.table("rivendell.txt", colClasses = c("Date", "numeric"), header = TRUE)
riv_data = riv_data[riv_data$DTM > as.Date("1981-01-01"), ]
riv = as.wateres(riv_data, 14.4e6, 754e3, id = "A1", down_id = "B")
riv = set_withdrawal(riv, rep(riv_wateruse, nrow(riv)))
riv_mrf = 0.033

riv_paralel = as.wateres(riv_data, 14.4e6, 754e3, id = "A2", down_id = "B")
riv_paralel = set_withdrawal(riv_paralel, rep(riv_wateruse, nrow(riv_paralel)))

riv2_wateruse = riv_wateruse * 1.12
riv2_data = riv_data
riv2_data$Q = riv2_data$Q * 1.12
riv2 = as.wateres(riv2_data, 14.4e6, 754e3, id = "B", down_id = "C")
riv2 = set_withdrawal(riv2, rep(riv2_wateruse, nrow(riv2)))
riv2_mrf = 0.033

thar_wateruse = 1e7
thar_data = read.table("tharbad.txt", colClasses = c("Date", "numeric"), header = TRUE)
thar_data = thar_data[thar_data$DTM > as.Date("1981-01-01"), ]
thar = as.wateres(thar_data, 41.3e6, 2672e3, id = "C")
thar = set_withdrawal(thar, rep(thar_wateruse, nrow(thar)))
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

    riv2$W[108] = riv2$W[108] + 5845586.9 # increase water use to get deficit 9e5 in reservoir B
    system = as.system(riv, riv_paralel, riv2, thar)
    yields = c(A1 = riv_mrf, A2 = riv_mrf, B = riv2_mrf, C = thar_mrf)
    expect_equivalent(calc_deficits(system, yields), readRDS("system4_deficits_B.rds"))
})
