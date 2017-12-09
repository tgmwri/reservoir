context("storage, yield, precipitation, evaporation and water use time series")

test_that("series of simple reservoir for given storage are calculated", {
    simple_reser = as.wateres(data.frame(Q = c(rep(2, 12), rep(0.5, 12)), DTM = seq(as.Date("2000-01-01"), by = "months", length.out = 24)), storage = 1e7, area = 1e4)
    resul = calc_series(simple_reser, 1e7, 1, FALSE)
    expect_equal(resul$inflow, c(rep(2, 12), rep(0.5, 12)))
    expect_equal(resul$storage, c(rep(1e7, 12), 1e2 * c(86608, 74512, 61120, 48160, 34768, 21808, 8416), rep(0, 5)))
    expect_equal(resul$yield, c(rep(2, 12), rep(1, 7), 0.8142174, rep(0.5, 4)), tol = 1e-5)

    expect_error(calc_series(simple_reser, c(rep(2e7, 12), rep(1e7, 12)), 1, FALSE), "must correspond with the length of the reservoir series")
    resul = calc_series(simple_reser, c(rep(2e7, 13), rep(1e7, 12)), 1, FALSE)
    expect_equal(resul$inflow, c(rep(2, 12), rep(0.5, 12)))
    expect_equal(resul$storage, c(rep(2e7, 12), 1e7, 1e2 * c(87904, 74512, 61552, 48160, 35200, 21808, 8416), rep(0, 4)))
    expect_equal(resul$yield, c(rep(2, 12), 4.2335723, rep(1, 7), 0.8246914, rep(0.5, 3)), tol = 1e-5)

    # storage specified as property
    reser_data = data.frame(Q = c(rep(2, 12), rep(0.5, 6)), DTM = seq(as.Date("2000-01-01"), by = "months", length.out = 18))
    storage_var = 1e6 * c(10, 10, 10, 14, 14, 14, 12, 12, 12, 6, 6, 6, 10, 10, 10, 14, 14, 14, 12)
    reser = as.wateres(reser_data, storage = 1e7, area = 1e4)
    reser = set_property(reser, "storage", storage_var)
    resul = calc_series(reser, yield = 1)

    reser2 = as.wateres(reser_data, storage = 1e7, area = 1e4)
    resul2 = calc_series(reser2, yield = 1, storage = storage_var)
    expect_equal(resul, resul2)
})

test_that("series of simple reservoir for optimum storage and maximum yield are calculated", {
    simple_reser = as.wateres(data.frame(Q = c(rep(2, 12), rep(0.5, 12)), DTM = seq(as.Date("2000-01-01"), by = "months", length.out = 24)), storage = 1e7, area = 1e4)
    resul = calc_series(simple_reser, storage = 2e7, yield = 1, storage_optim = c(rep(2e7, 6), rep(1e7, 19)), yield_max = 3)

    expect_equal(resul$inflow, c(rep(2, 12), rep(0.5, 12)))
    expect_equal(resul$storage, c(rep(2e7, 5), 1e2 * c(174080, 147296, 120512), rep(1e7, 4), 1e2 * c(86608, 74512, 61120, 48160, 34768, 21808, 8416), rep(0, 5)))
    expect_equal(resul$yield, c(rep(2, 5), rep(3, 3), 2.791358, rep(2, 3), rep(1, 7), 0.814217, rep(0.5, 4)), tol = 1e-5)

    # including water use
    simple_reser = set_wateruse(simple_reser, c(rep(0, 11), -5e6))
    resul = calc_series(simple_reser, storage = 2e7, yield = 1, storage_optim = c(rep(2e7, 6), rep(1e7, 19)), yield_max = 3)

    expect_equal(resul$inflow, c(rep(2, 12), rep(0.5, 12)))
    expect_equal(resul$storage, c(rep(2e7, 5), 1e2 * c(174080, 147296, 120512), rep(1e7, 3), 1e2 * c(76784, 63392, 51296, 37904, 24944, 11552), rep(0, 7)))
    expect_equal(resul$yield, c(rep(2, 5), rep(3, 3), 2.791358, rep(2, 2), rep(1, 6), 0.945679, rep(0.5, 6)), tol = 1e-5)

    # yield greater than maximum yield due to exceeding of maximum storage
    simple_reser = as.wateres(data.frame(Q = c(rep(4, 12), rep(0.5, 12)), DTM = seq(as.Date("2000-01-01"), by = "months", length.out = 24)), storage = 1e7, area = 1e4)
    resul = calc_series(simple_reser, storage = 2e7, yield = 1, storage_optim = c(rep(2e7, 6), rep(1e7, 19)), yield_max = 3)

    expect_equal(resul$inflow, c(rep(4, 12), rep(0.5, 12)))
    expect_equal(resul$storage, c(rep(2e7, 12), 1e2 * c(133040, 1e5, 86608, 73648, 60256, 47296, 33904, 20512, 7552), rep(0, 3)))
    expect_equal(resul$yield, c(rep(4, 12), 3, 1.8657407, rep(1, 7), 0.781959, rep(0.5, 2)), tol = 1e-5)

    # yield greater than maximum yield due to exceeding of maximum storage (starting from empty reservoir)
    simple_reser = as.wateres(data.frame(Q = c(rep(2, 8), rep(4, 4), rep(0.5, 12)), DTM = seq(as.Date("2000-01-01"), by = "months", length.out = 24)),
        storage = 1e7, area = 1e4)
    resul = calc_series(simple_reser, storage = 2e7, yield = 1, storage_optim = 1e7, yield_max = 3, initial_storage = 0)

    expect_equal(resul$inflow, c(rep(2, 8), rep(4, 4), rep(0.5, 12)))
    expect_equal(resul$storage, c(1e2 * c(26784, 51840, 78624), rep(1e7, 5), 1e2 * c(125920, 152704, 178624, 2e5, 133040, 1e5, 86608, 73648, 60256, 47296, 33904, 20512, 7552),
        rep(0, 3)))
    expect_equal(resul$yield, c(rep(1, 3), 1.1753086, rep(2, 4), rep(3, 3), 3.201911, 3, 1.865740, rep(1, 7), 0.781959, rep(0.5, 2)), tol = 1e-5)

    # yield_max missing
    expect_warning(calc_series(simple_reser, yield = 1, storage = 1e7, storage_optim = 5e6), "optimum storage and maximum yield have to be specified")
})

eas = data.frame(
    elevation = c(496, 499, 502, 505, 508, 511, 514, 517, 520, 523, 526, 529),
    area = c(0, 5e3, 58e3, 90e3, 133e3, 180e3, 253e3, 347e3, 424e3, 483e3, 538e3, 754e3),
    storage = c(0, 3e3, 161e3, 530e3, 1.085e6, 1.864e6, 2.943e6, 4.439e6, 6.362e6, 8.626e6, 11.175e6, 14.4e6))

riv = as.wateres("rivendell.txt", 14.4e6, 754e3)

test_that("storage, yield, precipitation, evaporation and water use time series are calculated", {
    resul = calc_series(riv, 41e3, 0.06, FALSE)
    resul_throw = calc_series(riv, 41e3, 0.06, TRUE)
    expect_equivalent(resul, readRDS("series.rds"))
    expect_equivalent(resul_throw, readRDS("series_throw.rds"))
    riv = set_evaporation(riv, altitude = 529)
    resul_evaporation = calc_series(riv, 14.4e6, 0.14, FALSE)
    expect_equivalent(resul_evaporation, readRDS("series_evaporation.rds"))
    riv = set_wateruse(riv, c(23, 31, 35, 33, 30, 42, 47, 33, 27, 22, 24, 32) * -1e3)
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

test_that("series with evaporation and plant cover are calculated", {
    eas = data.frame(elevation = c(500, 510, 520), area = c(0, 50, 100), storage = c(0, 500, 1000))
    reser = as.wateres(data.frame(Q = c(1, 1, 1, 1, 1)), 1e3, 1e2, time_step = "day", eas = eas)
    reser = set_evaporation(reser, rep(10, 5), plant = 11/30)

    resul = calc_series(reser, yield = 0.9)
    expect_equal(resul$storage, rep(1e3, 5))
    expect_equal(resul$yield, rep(0.9999872, 5), tol = 1e-5)
    expect_equal(resul$evaporation, rep(1.1, 5))

    resul = calc_series(reser, yield = 1.0005)
    expect_equal(resul$storage, c(955.70000, 911.46143, 867.28204, 823.15969, 779.09335), tol = 1e-5)
    expect_equal(resul$yield, rep(1.0005, 5))
    expect_equal(resul$evaporation, c(1.1, 1.038569, 0.979394, 0.922343, 0.866340), tol = 1e-5)
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

test_that("water use without evaporation is calculated", {
    reser = set_wateruse(reser, c(23, 31, 35, 33, 30, 42, 47, 33, 27, 22, 24, 32) * -1e3)
    resul = calc_series(reser, 21e3, 0.14, FALSE)
    expect_equivalent(resul$wateruse,
        -1 * c(0, 0, 35000, 33000, 30000, 0, 0, 0, 0, 22000, 24000, 32000, 23000, 31000, 35000, 33000, 30000, 42000, 0, 0, 0, 22000, 24000, 0))
})

test_that("series with transfer are calculated", {
    transfer = c(988841.6, 883737.6, 32864.0)
    transfer_pos = c(108, 118, 131)
    riv_wateruse = -4e5
    riv = as.wateres("rivendell.txt", storage = 14.4e6, area = 754e3, id = "riv", down_id = "thar")
    riv = resize_input(riv, "1981-01-01")
    riv = set_wateruse(riv, rep(riv_wateruse, nrow(riv)))
    riv$T = 0
    riv$T[transfer_pos] = -transfer
    riv_mrf = 0.033
    riv_resul = calc_series(riv, yield = riv_mrf)
    resul_orig = readRDS("series_transfer.rds")
    expect_equivalent(riv_resul, resul_orig$riv)

    thar_wateruse = -1e7
    thar = as.wateres("tharbad.txt", 41.3e6, 2672e3, id = "thar")
    thar = resize_input(thar, "1981-01-01")
    thar = set_wateruse(thar, rep(thar_wateruse, nrow(thar)))
    thar$T = 0
    thar$T[transfer_pos] = transfer
    thar_mrf = 2.718
    thar_resul = calc_series(thar, yield = thar_mrf)
    expect_equivalent(thar_resul, resul_orig$thar)
})

test_that("transfer greater than storage is decreased", {
    riv = as.wateres("rivendell.txt", storage = 14.4e6, area = 754e3)
    riv = resize_input(riv, "1981-01-01", "1981-12-01")
    riv = set_wateruse(riv, -4e5)
    riv$T = c(-15249091, rep(0, nrow(riv) - 1))
    riv_resul = calc_series(riv, yield = 0.033)
    expect_equivalent(c(0.126, 0, 0.033, 0, 0, -400000, 0, -14249091), as.numeric(riv_resul[1, ]))
    expect_equivalent(c(0.161, 0, 0.033, 0, 0, -309657.6, 90342.4, 0), as.numeric(riv_resul[2, ]))
})

test_that("negative wateruse is effective also if storage is zero", {
    reser_data = data.frame(Q =  rep(1, 6), DTM = seq(as.Date("2000-01-01"), by = "months", length.out = 6))
    reser = as.wateres(reser_data, 7e6, 754e3)
    reser = set_wateruse(reser, 1e6)
    resul = calc_series(reser, yield = 2)

    expect_equivalent(resul$storage, c(5321600, 3816000, 2137600, 545600, 0, 0))
    expect_equivalent(resul$yield, c(rep(2, 4), 1.577060932, 1.385802469))
    expect_equivalent(resul$wateruse, rep(1e6, 6))
    expect_equivalent(resul$deficit, c(rep(0, 4), 1132800, 1592000))
})

test_that("series for different time steps are calculated", {
    Q_df = data.frame(Q = c(5, 7, 9, 4, 3, 3))
    resul_storage = list(
        `1h` = c(9992800, 9992800, 1e+07, 9989200, 9974800, 9960400), `2h` = c(9985600, 9985600, 1e+07, 9978400, 9949600, 9920800),
        `4h` = c(9971200, 9971200, 1e+07, 9956800, 9899200, 9841600), `8h` = c(9942400, 9942400, 1e+07, 9913600, 9798400, 9683200))
    for (h in c(1, 2, 4, 8))
        expect_equal(resul_storage[[paste0(h, "h")]], calc_series(as.wateres(Q_df, 1e7, 1e2, time_step = paste(h, "hour", sep = "-")), yield = 7)$storage)
})
