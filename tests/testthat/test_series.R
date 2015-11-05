context("storage, yield, precipitation, evaporation and withdrawal time series")

eas = data.frame(
    elevation = c(496, 499, 502, 505, 508, 511, 514, 517, 520, 523, 526, 529),
    area = c(0, 5e3, 58e3, 90e3, 133e3, 180e3, 253e3, 347e3, 424e3, 483e3, 538e3, 754e3),
    storage = c(0, 3e3, 161e3, 530e3, 1.085e6, 1.864e6, 2.943e6, 4.439e6, 6.362e6, 8.626e6, 11.175e6, 14.4e6))

riv = as.wateres("rivendell.txt", 14.4e6, 754e3)

test_that("storage, yield, precipitation, evaporation and withdrawal time series are calculated", {
    resul = calc_series(riv, 41e3, 0.06, FALSE)
    resul_throw = calc_series(riv, 41e3, 0.06, TRUE)
    expect_equivalent(resul, readRDS("series.rds"))
    expect_equivalent(resul_throw, readRDS("series_throw.rds"))
    riv = set_evaporation(riv, altitude = 529)
    resul_evaporation = calc_series(riv, 14.4e6, 0.14, FALSE)
    expect_equivalent(resul_evaporation, readRDS("series_evaporation.rds"))
    riv = set_withdrawal(riv, c(23, 31, 35, 33, 30, 42, 47, 33, 27, 22, 24, 32) * 1e3)
    resul_with = calc_series(riv, 14.4e6, 0.14, FALSE)
    expect_equivalent(resul_with, readRDS("series_withdrawal.rds"))
    riv = set_precipitation(riv, c(55, 40, 44, 43, 81, 72, 85, 84, 52, 54, 48, 58))
    resul_precip = calc_series(riv, 14.4e6, 0.14, FALSE)
    expect_equivalent(resul_precip, readRDS("series_precipitation.rds"))
})

test_that("series with evaporation depending on E-A-S relationship are calculated", {
    riv = as.wateres("rivendell.txt", 14.4e6, 754e3, eas = eas)
    riv = set_evaporation(riv, altitude = 529)
    resul = calc_series(riv, 14.4e6, 0.14, FALSE)
    expect_equivalent(resul, readRDS("series_evaporation_eas.rds"))
})

test_that("series for hourly data are calculated", {
    rivh = as.wateres("rivendell_1h.txt", 14.4e6, 754e3, eas = eas, time_step = "hour")
    expect_error(set_evaporation(rivh, altitude = 529))
    expect_error(set_precipitation(rivh, 1:12))
    resul = calc_series(rivh, 14.4e6, 0.14, FALSE, 14e6, get_level = TRUE)
    expect_equivalent(resul, readRDS("series_hourly.rds"))
})

reser = data.frame(
    Q = c(0.078, 0.065, 0.168, 0.711, 0.154, 0.107, 0.068, 0.057, 0.07, 0.485, 0.252, 0.236,
        0.498, 0.248, 0.547, 0.197, 0.283, 0.191, 0.104, 0.067, 0.046, 0.161, 0.16, 0.094),
    DTM = seq(as.Date("2000-01-01"), by = "months", length.out = 24))
reser = as.wateres(reser, storage = 14.4e6, area = 754e3)

test_that("withdrawal without evaporation is calculated", {
    reser = set_withdrawal(reser, c(23, 31, 35, 33, 30, 42, 47, 33, 27, 22, 24, 32) * 1e3)
    resul = calc_series(reser, 21e3, 0.14, FALSE)
    expect_equivalent(resul$withdrawal,
        c(0, 0, 35000, 33000, 30000, 0, 0, 0, 0, 22000, 24000, 32000, 23000, 31000, 35000, 33000, 30000, 42000, 0, 0, 0, 22000, 24000, 0))
})
