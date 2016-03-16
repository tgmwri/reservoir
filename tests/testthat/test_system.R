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


