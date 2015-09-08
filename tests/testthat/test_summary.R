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

context("storage-reliability-yield relationship")

test_that("storage for reliability and yield is optimized", {
    sry = sry(riv, reliab = 0.5, yield = 0.14)
    expect_equivalent(sry$storage, 0.0410853830921)
    expect_equivalent(sry$reliability, 0.500378673129)
    expect_equivalent(sry$yield, 0.14)
    sry = sry(riv, reliab = 0.5, yield = 0.14, empirical_rel = FALSE)
    expect_equivalent(sry$storage, 0.0413683875684)
    expect_equivalent(sry$reliability, 0.5)
    expect_equivalent(sry$yield, 0.14)
})

test_that("invalid reliability is rejected", {
    expect_error(sry(riv, reliab = -0.5, yield = 0.14))
    expect_error(sry(riv, reliab = 1, yield = 0.14))
})

test_that("reliability for storage and yield is calculated", {
    riv = as.wateres("rivendell.txt", 14.4)
    sry = sry(riv, storage = 0.041, yield = 0.14)
    expect_equivalent(sry$storage, 0.041)
    expect_equivalent(sry$reliability, 0.499621326871)
    expect_equivalent(sry$yield, 0.14)
})

test_that("yield for storage and reliability is optimized", {
    riv = as.wateres("rivendell.txt", 14.4)
    sry = sry(riv, storage = 0.041, reliab = 0.5)
    expect_equivalent(sry$storage, 0.041)
    expect_equivalent(sry$reliability, 0.500378673129)
    expect_equivalent(sry$yield, 0.13934106881)
})

