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

test_that("characteristics are calculated correctly", {
    riv = read.table("rivendell.txt", header = TRUE)
    riv = as.wateres(riv, 14.4)
    chars = summary(riv, Qn_coeff = c(0.1, 1, 0.05))
    expect_equivalent(chars["Vpot"], 14.4)
    expect_equivalent(chars["Qn_max"], 0.1446833785)
    expect_equivalent(chars["alpha"], 0.9196508835)
    expect_equivalent(chars["m"], 0.3204723019)
})

context("storage-reliability-yield relationship")

test_that("storage for reliability and yield is optimized", {
    riv = read.table("rivendell.txt", header = TRUE)
    riv = as.wateres(riv, 14.4)
    sry = sry(riv, reliab = 0.5, yield = 0.14)
    expect_equivalent(sry$storage, 0.59823188717)
    expect_equivalent(sry$reliability, 0.501136019388)
    expect_equivalent(sry$yield, 0.14)
})

test_that("invalid reliability is rejected", {
    riv = read.table("rivendell.txt", header = TRUE)
    riv = as.wateres(riv, 14.4)
    expect_error(sry(riv, reliab = -0.5, yield = 0.14))
    expect_error(sry(riv, reliab = 1, yield = 0.14))
})

