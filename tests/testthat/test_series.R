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

rivh = as.wateres("rivendell_1h.txt", 14.4e6, 754e3, eas = eas, time_step = "hour")

test_that("series for hourly data are calculated", {
    expect_error(set_evaporation(rivh, altitude = 529))
    expect_error(set_precipitation(rivh, 1:12))
    resul = calc_series(rivh, 14.4e6, 0.14, FALSE, 14e6, get_level = TRUE)
    expect_equivalent(resul, readRDS("series_hourly.rds"))
})

rivd = as.wateres("rivendell_1d.txt", 14.4e6, 754e3, eas = eas, time_step = "day")

test_that("series for daily data are calculated", {
    rivd = set_evaporation(rivd, altitude = 529)
    expect_equivalent(rivd$E[1:90], c(rep(0.219705678, 31), rep(0.4864911442, 28), rep(1.3182340683, 31)))
    resul = calc_series(rivd, 14.4e6, 0.14, FALSE, 14e6, get_level = TRUE)
    expect_equivalent(resul, readRDS("series_daily.rds"))
    rivd$DTM = NULL
    expect_error(set_evaporation(rivd, altitude = 529), "without specified date")
})

test_that("series with variable yield are calculated", {
    expect_error(calc_series(rivh, 14.4e6, yield = rep(0.14, 3), FALSE, 14e6, get_level = TRUE))
    resul = calc_series(rivh, 14.4e6, yield = rep(c(0.14, 2, 0.5), each = 8), FALSE, 14e6, get_level = TRUE)
    expect_equivalent(resul, readRDS("series_var_yield.rds"))
})

test_that("series with specified initial conditions are calculated", {
    resul_storage = calc_series(rivh, yield = 1, initial_storage = 5e6, get_level = TRUE)
    expect_equivalent(resul_storage, readRDS("series_init_storage.rds"))
    resul_level = calc_series(rivh, yield = 1, initial_level = 512, get_level = TRUE)
    expect_equivalent(resul_level, readRDS("series_init_level.rds"))
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

test_that("series with transfer are calculated", {
    transfer = c(988841.6, 883737.6, 32864.0)
    transfer_pos = c(108, 118, 131)
    riv_wateruse = 4e5
    riv = as.wateres("rivendell.txt", storage = 14.4e6, area = 754e3, id = "riv", down_id = "thar")
    riv = resize_input(riv, "1981-01-01")
    riv = set_withdrawal(riv, rep(riv_wateruse, nrow(riv)))
    riv$T = rep(0, nrow(riv))
    riv$T[transfer_pos] = -transfer
    riv_mrf = 0.033
    riv_resul = calc_series(riv, yield = riv_mrf)
    resul_orig = readRDS("series_transfer.rds")
    expect_equivalent(riv_resul, resul_orig$riv)

    thar_wateruse = 1e7
    thar = as.wateres("tharbad.txt", 41.3e6, 2672e3, id = "thar")
    thar = resize_input(thar, "1981-01-01")
    thar = set_withdrawal(thar, rep(thar_wateruse, nrow(thar)))
    thar$T = rep(0, nrow(riv))
    thar$T[transfer_pos] = transfer
    thar_mrf = 2.718
    thar_resul = calc_series(thar, yield = thar_mrf)
    expect_equivalent(thar_resul, resul_orig$thar)
})

test_that("transfer greater than storage is decreased", {
    riv = as.wateres("rivendell.txt", storage = 14.4e6, area = 754e3)
    riv = resize_input(riv, "1981-01-01", "1981-12-01")
    riv = set_withdrawal(riv, rep(4e5, nrow(riv)))
    riv$T = c(-15249091, rep(0, nrow(riv) - 1))
    riv_resul = calc_series(riv, yield = 0.033)
    expect_equivalent(c(0, 0.033, 0, 0, 400000, 0, -14249091), as.numeric(riv_resul[1, ]))
    expect_equivalent(c(0, 0.033, 0, 0, 309657.6, 90342.4, 0), as.numeric(riv_resul[2, ]))
})

test_that("negative wateruse is effective also if storage is zero", {
    reser_data = data.frame(Q =  rep(1, 6), DTM = seq(as.Date("2000-01-01"), by = "months", length.out = 6))
    reser = as.wateres(reser_data, 7e6, 754e3)
    reser = set_withdrawal(reser, -1e6)
    resul = calc_series(reser, yield = 2)

    expect_equivalent(resul$storage, c(5321600, 3816000, 2137600, 545600, 0, 0))
    expect_equivalent(resul$yield, c(rep(2, 4), 1.577060932, 1.385802469))
    expect_equivalent(resul$withdrawal, rep(-1e6, 6))
    expect_equivalent(resul$deficit, c(rep(0, 4), 1132800, 1592000))
})
