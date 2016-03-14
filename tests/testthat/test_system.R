context("calculation of system of reservoirs")

riv = as.wateres("rivendell.txt", 14.4e6, 754e3, id = "riv", id_down = "thar")
thar = as.wateres("tharbad.txt", 41.3e6, 2672e3, id = "thar")

test_that("system object is created", {
    expect_equivalent(as.system(riv, thar), readRDS("system_creation.rds"))
})

test_that("system object is not created if IDs are not correct", {
    riv_no_id = as.wateres("rivendell.txt", 14.4e6, 754e3)
    expect_error(as.system(riv_no_id))
    expect_error(as.system(riv, riv))
})
