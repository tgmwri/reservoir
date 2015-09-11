context("data loading")

test_that("data are loaded from Bilan object", {
    library(bilan)
    bil = bil.new("m")
    bil.read.file(bil, "rivendell.dat", c("P", "R", "T"))
    riv = as.wateres(bil, 14.4, observed = TRUE)
    expect_equivalent(riv$Q[1:12], c(0.07799990666, 0.06500006200, 0.16799987866, 0.71100096451,
        0.15399985999, 0.10699987461, 0.06800014001, 0.05699987866, 0.06999987461, 0.48499990666,
        0.25200004823, 0.23600001867))
})

context("characteristics calculated by summary function")

riv = as.wateres("rivendell.txt", 14.4)

test_that("characteristics are calculated correctly", {
    chars = summary(riv, Qn_coeff = c(0.1, 1, 0.05))
    expect_equivalent(chars["Vpot"], 14.4)
    expect_equivalent(chars["Qn_max"], 0.1446833785)
    expect_equivalent(chars["alpha"], 0.9196508835)
    expect_equivalent(chars["m"], 0.3204723019)
})

context("storage and yield time series")

test_that("storage and yield time series are calculated", {
    resul = .Call("calc_storage", PACKAGE = "wateres", riv$Q, riv$.days, 0.06, 0.041, FALSE)
    resul_throw = .Call("calc_storage", PACKAGE = "wateres", riv$Q, riv$.days, 0.06, 0.041, TRUE)
    expect_equivalent(
        resul$storage[1:100], c(41000, 41000, 41000, 41000, 41000, 41000, 41000, 41000, 32964.8,
        41000, 41000, 41000, 41000, 41000, 41000, 41000, 41000, 41000,
        41000, 41000, 41000, 4712.00000000001, 41000, 41000, 41000, 41000,
        41000, 41000, 41000, 41000, 41000, 41000, 41000, 41000, 41000,
        41000, 41000, 41000, 41000, 41000, 41000, 41000, 41000, 41000,
        0, 0, 41000, 41000, 41000, 41000, 41000, 41000, 41000, 41000,
        41000, 35643.2, 0, 41000, 41000, 41000, 41000, 41000, 41000,
        41000, 41000, 41000, 41000, 11537.6, 0, 41000, 41000, 41000,
        41000, 41000, 41000, 41000, 41000, 41000, 41000, 41000, 41000,
        41000, 11537.6, 41000, 41000, 41000, 41000, 41000, 41000, 41000,
        41000, 35643.2, 24929.6, 41000, 0, 41000, 41000, 41000, 41000, 41000))
    expect_equivalent(resul$storage[1:100], resul_throw$storage[1:100])
    expect_equivalent(
        resul$yield[1:100], c(0.078, 0.065, 0.168, 0.711, 0.154, 0.107, 0.068, 0.06, 0.0669,
        0.485, 0.252, 0.236, 0.498, 0.248, 0.547, 0.197, 0.283, 0.191,
        0.104, 0.067, 0.06, 0.147451612903226, 0.16, 0.094, 0.132, 0.405,
        0.408, 0.292, 0.144, 0.099, 0.102, 0.183, 0.069, 0.262, 0.089,
        0.132, 0.135, 0.191, 0.22, 0.581, 0.134, 0.093, 0.06, 0.0563076463560335,
        0.033, 0.201692353643967, 0.42, 0.194, 0.154, 0.219, 0.45, 0.259,
        0.125, 0.092, 0.06, 0.0513076463560335, 0.0701820987654321, 0.295,
        0.458, 0.266, 0.24, 0.259, 0.458, 0.141, 0.14, 0.08, 0.06, 0.0513076463560335,
        0.222182098765432, 0.124, 0.235, 0.132, 0.139, 0.15, 0.398, 0.499,
        0.158, 0.134, 0.338, 0.113, 0.075, 0.06, 0.145633333333333, 0.162,
        0.136, 0.195, 0.285, 0.508, 0.133, 0.09, 0.06, 0.06, 0.0938,
        0.0533076463560335, 0.0871820987654321, 0.104, 0.104, 0.094,
        0.171, 0.814))
    expect_equivalent(
        resul_throw$yield[1:100], c(0.06, 0.06, 0.06, 0.06, 0.06, 0.06, 0.06, 0.06, 0.06, 0.06,
        0.06, 0.06, 0.06, 0.06, 0.06, 0.06, 0.06, 0.06, 0.06, 0.06, 0.06,
        0.06, 0.06, 0.06, 0.06, 0.06, 0.06, 0.06, 0.06, 0.06, 0.06, 0.06,
        0.06, 0.06, 0.06, 0.06, 0.06, 0.06, 0.06, 0.06, 0.06, 0.06, 0.06,
        0.0563076463560335, 0.033, 0.06, 0.06, 0.06, 0.06, 0.06, 0.06,
        0.06, 0.06, 0.06, 0.06, 0.0513076463560335, 0.06, 0.06, 0.06,
        0.06, 0.06, 0.06, 0.06, 0.06, 0.06, 0.06, 0.06, 0.0513076463560335,
        0.06, 0.06, 0.06, 0.06, 0.06, 0.06, 0.06, 0.06, 0.06, 0.06, 0.06,
        0.06, 0.06, 0.06, 0.06, 0.06, 0.06, 0.06, 0.06, 0.06, 0.06, 0.06,
        0.06, 0.06, 0.06, 0.0533076463560335, 0.06, 0.06, 0.06, 0.06,
        0.06, 0.06))
})

context("storage-reliability-yield relationship")

test_that("storage for reliability and yield is optimized", {
    sry = sry(riv, reliab = 0.5, yield = 0.14)
    expect_equivalent(sry$storage, 0.0410853830921)
    expect_equivalent(sry$reliability, 0.499621326871)
    expect_equivalent(sry$yield, 0.14)
    sry = sry(riv, reliab = 0.5, yield = 0.14, empirical_rel = FALSE)
    expect_equivalent(sry$storage, 0.0413683875684)
    expect_equivalent(sry$reliability, 0.5)
    expect_equivalent(sry$yield, 0.14)
    # reliability > 1 if absoluteness of difference not reflected
    sry = sry(riv, reliab = 0.7, yield = 0.7)
    expect_equivalent(sry$storage, 13.5975849471)
    expect_equivalent(sry$reliability, 0.0134050287792)
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
})

test_that("yield for storage and reliability is optimized", {
    sry = sry(riv, storage = 0.041, reliab = 0.5)
    expect_equivalent(sry$storage, 0.041)
    expect_equivalent(sry$reliability, 0.500378673129)
    expect_equivalent(sry$yield, 0.13934106881)
})

test_that("yield is optimized with the option to throw exceeding volume", {
    sry1 = sry(riv, storage = 0.041, reliab = 0.88)
    sry2 = sry(riv, storage = 0.041, reliab = 0.88, throw_exceed = TRUE)
    expect_equivalent(sry1$storage, 0.041)
    expect_equivalent(sry1$reliability, 0.882838533778)
    expect_equivalent(sry1$yield, 0.0608845435905)
    expect_equivalent(sry1, sry2)
})
