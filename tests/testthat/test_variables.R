context("setting variables")

riv = as.wateres("rivendell.txt", 14.4e6, 754e3)
monthly_evap = c(7, 14, 40, 62, 82, 96, 109, 102, 75, 48, 34, 13)
monthly_with = c(23, 31, 35, 33, 30, 42, 47, 33, 27, 22, 24, 32) * 1e3

test_that("evaporation values are calculated for altitude", {
    riv = set_evaporation(riv, altitude = 529)
    expect_equivalent(
        riv$E, rep_len(c(6.81087601935, 13.62175203870, 40.86525611609, 61.29788417414,
        81.73051223219, 95.35226427089, 108.97401630959, 102.16314029024,
        74.91963621284, 47.67613213544, 34.05438009675, 13.6217520387), nrow(riv)))
})

test_that("evaporation values are set", {
    riv = set_evaporation(riv, rep_len(monthly_evap, nrow(riv)))
    expect_equivalent(riv$E, rep_len(monthly_evap, nrow(riv)))
})

test_that("evaporation monthly values are set", {
    riv = set_evaporation(riv, monthly_evap)
    expect_equivalent(riv$E, rep_len(monthly_evap, nrow(riv)))
})

test_that("evaporation values of incorrect length are rejected", {
    expect_error(set_evaporation(riv, monthly_evap[1:5]))
})

test_that("evaporation with incorrect plant cover is rejected", {
    expect_error(set_evaporation(riv, monthly_evap, plant_cover = -1), "has to be between 0 and 0.75")
    expect_error(set_evaporation(riv, monthly_evap, plant_cover = 0.8), "has to be between 0 and 0.75")
})

test_that("water use values are set", {
    # time series
    riv = set_wateruse(riv, rep_len(monthly_with, nrow(riv)))
    expect_equivalent(riv$W, rep_len(monthly_with, nrow(riv)))
    # monthly values
    riv = set_wateruse(riv, monthly_with)
    expect_equivalent(riv$W, rep_len(monthly_with, nrow(riv)))
    # constant value
    riv = set_wateruse(riv, monthly_with[1])
    expect_equivalent(riv$W, rep(monthly_with[1], nrow(riv)))
})

test_that("water use values of incorrect length are rejected", {
    expect_error(set_wateruse(riv, monthly_with[1:5]))
})

monthly_precip = c(55, 40, 44, 43, 81, 72, 85, 84, 52, 54, 48, 58)

test_that("precipitation monthly values are set", {
    riv = set_precipitation(riv, monthly_precip)
    expect_equivalent(riv$P, rep_len(monthly_precip, nrow(riv)))
})
