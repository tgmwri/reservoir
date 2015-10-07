context("data loading")

test_that("data are loaded from Bilan object", {
    library(bilan)
    bil = bil.new("m")
    bil.read.file(bil, "rivendell.dat", c("P", "R", "T"))
    riv = as.wateres(bil, 14.4, 0.754, observed = TRUE)
    expect_equivalent(riv$Q[1:12], c(0.07799990666, 0.06500006200, 0.16799987866, 0.71100096451,
        0.15399985999, 0.10699987461, 0.06800014001, 0.05699987866, 0.06999987461, 0.48499990666,
        0.25200004823, 0.23600001867))
})

context("characteristics calculated by summary function")

riv = as.wateres("rivendell.txt", 14.4, 0.754)

test_that("characteristics are calculated correctly", {
    chars = summary(riv)
    expect_equivalent(chars["Vpot"], 14.4)
    expect_equivalent(chars["Qn_max"], 0.145018882520)
    expect_equivalent(chars["alpha"], 0.921783447266)
    expect_equivalent(chars["m"], 0.311966575415)
    expect_true(is.na(chars["resilience"]))
})

test_that("characteristics are calculated for given reliability", {
    chars = summary(riv, reliability = 0.95)
    expect_equivalent(chars["Vpot"], 14.4)
    expect_equivalent(chars["Qn_max"], 0.1557741525525)
    expect_equivalent(chars["alpha"], 0.9901471645571)
    expect_equivalent(chars["m"], 0.0392980158775)
    expect_equivalent(chars["resilience"], 0.272727272727)
})

context("storage, yield, evaporation and withdrawal time series")

test_that("storage, yield, evaporation and withdrawal time series are calculated", {
    resul = calc_series(riv, 0.041, 0.06, FALSE)
    resul_throw = calc_series(riv, 0.041, 0.06, TRUE)
    expect_equivalent(resul, readRDS("series.rds"))
    expect_equivalent(resul_throw, readRDS("series_throw.rds"))
    riv = set_evaporation(riv, altitude = 529)
    resul_evaporation = calc_series(riv, 14.4, 0.14, FALSE)
    expect_equivalent(resul_evaporation, readRDS("series_evaporation.rds"))
    riv = set_withdrawal(riv, c(23, 31, 35, 33, 30, 42, 47, 33, 27, 22, 24, 32) * 1e3)
    resul_with = calc_series(riv, 14.4, 0.14, FALSE)
    expect_equivalent(resul_with, readRDS("series_withdrawal.rds"))
})

test_that("withdrawal without evaporation is calculated", {
    reser = data.frame(
        Q = c(0.078, 0.065, 0.168, 0.711, 0.154, 0.107, 0.068, 0.057, 0.07, 0.485, 0.252, 0.236,
            0.498, 0.248, 0.547, 0.197, 0.283, 0.191, 0.104, 0.067, 0.046, 0.161, 0.16, 0.094),
        DTM = seq(as.Date("2000-01-01"), by = "months", length.out = 24))
    reser = as.wateres(reser, Vpot = 14.4, area = 0.754)
    reser = set_withdrawal(reser, c(23, 31, 35, 33, 30, 42, 47, 33, 27, 22, 24, 32) * 1e3)
    resul = calc_series(reser, 0.021, 0.14, FALSE)
    expect_equivalent(resul$withdrawal,
        c(0, 0, 35000, 33000, 30000, 0, 0, 0, 0, 22000, 24000, 32000, 23000, 31000, 35000, 33000, 30000, 42000, 0, 0, 0, 22000, 24000, 0))
})

context("storage-reliability-yield relationship")

test_that("storage for reliability and yield is optimized", {
    sry = sry(riv, reliab = 0.5, yield = 0.14)
    expect_equivalent(sry$storage, 0.0401792228222)
    expect_equivalent(sry$reliability, 0.499621326871)
    expect_equivalent(sry$yield, 0.14)
    sry = sry(riv, reliab = 0.5, yield = 0.14, empirical_rel = FALSE)
    expect_equivalent(sry$storage, 0.0401773452759)
    expect_equivalent(sry$reliability, 0.5)
    expect_equivalent(sry$yield, 0.14)
    # increase of upper limit of storage needed
    sry = sry(riv, reliab = 0.7, yield = 0.7, upper = 100)
    expect_equivalent(sry$storage, 1318.80657971)
    expect_equivalent(sry$reliability, 0.700318085429)
    expect_equivalent(sry$yield, 0.7)
})

test_that("invalid reliability is rejected", {
    expect_error(sry(riv, reliab = -0.5, yield = 0.14))
    expect_error(sry(riv, reliab = 1, yield = 0.14))
})

test_that("reliability for storage and yield is calculated", {
    sry = sry(riv, storage = 0.041, yield = 0.14)
    expect_equivalent(sry$storage, 0.041)
    expect_equivalent(sry$reliability, 0.499621326871)
    expect_equivalent(sry$yield, 0.14)

    # default storage value
    sry = sry(riv, yield = 0.14)
    expect_equivalent(sry$storage, 14.4)
    expect_equivalent(sry$reliability, 0.999469857619)
    expect_equivalent(sry$yield, 0.14)

    # evaporation applied
    riv = set_evaporation(riv, altitude = 529)
    sry = sry(riv, storage = 0.041, yield = 0.14)
    expect_equivalent(sry$storage, 0.041)
    expect_equivalent(sry$reliability, 0.433732202363)
    expect_equivalent(sry$yield, 0.14)

    # withdrawal applied
    riv = set_withdrawal(riv, c(23, 31, 35, 33, 30, 42, 47, 33, 27, 22, 24, 32) * 1e3)
    sry = sry(riv, storage = 0.041, yield = 0.14)
    expect_equivalent(sry$storage, 0.041)
    expect_equivalent(sry$reliability, 0.430702817328)
    expect_equivalent(sry$yield, 0.14)
})

test_that("yield for storage and reliability is optimized", {
    sry = sry(riv, storage = 0.041, reliab = 0.5)
    expect_equivalent(sry$storage, 0.041)
    expect_equivalent(sry$reliability, 0.500378673129)
    expect_equivalent(sry$yield, 0.139881646821)
    sry = sry(riv, storage = 14.4, reliab = 1, empirical_rel = FALSE)
    expect_equivalent(sry$storage, 14.4)
    expect_equivalent(sry$reliability, 1)
    expect_equivalent(sry$yield, 0.14501888252)
})

test_that("yield is optimized with the option to throw exceeding volume", {
    sry1 = sry(riv, storage = 0.041, reliab = 0.88)
    sry2 = sry(riv, storage = 0.041, reliab = 0.88, throw_exceed = TRUE)
    expect_equivalent(sry1$storage, 0.041)
    expect_equivalent(sry1$reliability, 0.880566495002)
    expect_equivalent(sry1$yield, 0.0618148688114)
    expect_equivalent(sry1, sry2)
})
