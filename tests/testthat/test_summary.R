context("data loading")

test_that("data are loaded from Bilan object", {
    library(bilan)
    bil = bil.new("m")
    bil.read.file(bil, "rivendell.dat", c("P", "R", "T"))
    riv = as.wateres(bil, 14.4e6, 754e3, observed = TRUE)
    expect_equivalent(riv$Q[1:12], c(0.07799990666, 0.06500006200, 0.16799987866, 0.71100096451,
        0.15399985999, 0.10699987461, 0.06800014001, 0.05699987866, 0.06999987461, 0.48499990666,
        0.25200004823, 0.23600001867))
})

eas = data.frame(
    elevation = c(496, 499, 502, 505, 508, 511, 514, 517, 520, 523, 526, 529),
    area = c(0, 5e3, 58e3, 90e3, 133e3, 180e3, 253e3, 347e3, 424e3, 483e3, 538e3, 754e3),
    storage = c(0, 3e3, 161e3, 530e3, 1.085e6, 1.864e6, 2.943e6, 4.439e6, 6.362e6, 8.626e6, 11.175e6, 14.4e6))

test_that("elevation-area-storage relationship is set", {
    expect_warning(as.wateres("rivendell.txt", 14.4e6, 754e3, eas = data.frame(elev = 529)))
    riv = as.wateres("rivendell.txt", 14.4e6, 754e3, eas = eas)
    expect_equivalent(attr(riv, "eas"), eas)
    eas$storage[2] = 2e5
    expect_warning(as.wateres("rivendell.txt", 14.4e6, 754e3, eas = eas))
    orig_eas = eas
    eas = eas[c(1, 3:2, 4:nrow(eas)),]
    riv = as.wateres("rivendell.txt", 14.4e6, 754e3, eas = eas)
    expect_equivalent(attr(riv, "eas"), orig_eas)
})

context("characteristics calculated by summary function")

riv = as.wateres("rivendell.txt", 14.4e6, 754e3)

test_that("characteristics are calculated correctly", {
    chars = summary(riv)
    expect_equivalent(chars$storage, 14.4e6)
    expect_equivalent(chars$reliability, 1)
    expect_equivalent(chars$yield, 0.145018882520)
    expect_equivalent(chars$alpha, 0.921783447266)
    expect_equivalent(chars$m, 0.311966575415)
    expect_true(is.na(chars$resilience))
    expect_true(is.na(chars$vulnerability))
    expect_true(is.na(chars$dimless_vulner))
    chars_ch = summary(riv, prob_type = "ch")
    expect_equivalent(chars_ch$reliability, 0.999469857619)
    expect_equivalent(chars_ch[, reliability := NULL], chars[, reliability := NULL])
})

test_that("characteristics are calculated for given reliability and storage", {
    chars = summary(riv, reliability = 0.95)
    expect_equivalent(chars$storage, 14.4e6)
    expect_equivalent(chars$reliability, 0.949962092494)
    expect_equivalent(chars$yield, 0.1557741525525)
    expect_equivalent(chars$alpha, 0.9901471645571)
    expect_equivalent(chars$m, 0.0392980158775)
    expect_equivalent(chars$resilience, 0.272727272727)
    expect_equivalent(chars$vulnerability, 252139.72249)
    expect_equivalent(chars$dimless_vulner, 0.614231792124)
})

test_that("characteristics are calculated for given reliability and yield", {
    chars = summary(riv, reliability = 0.95, yield = 0.14)
    expect_equivalent(chars$storage, 2503180.76138)
    expect_equivalent(chars$reliability, 0.949962092494)
    expect_equivalent(chars$yield, 0.14)
    expect_equivalent(chars$alpha, 0.889881926922)
    expect_equivalent(chars$m, 0.439205730099)
    expect_equivalent(chars$resilience, 0.287878787879)
    expect_equivalent(chars$vulnerability, 227977.768421)
    expect_equivalent(chars$dimless_vulner, 0.617946505608)
})

test_that("characteristics are calculated for vector of reliabilities", {
    chars = summary(riv, reliability = c(0.5, 0.7, 0.9))
    expect_equivalent(chars$storage, rep(14.4e6, 3))
    expect_equivalent(chars$reliability, c(0.499620924943, 0.699772554966, 0.899924184989))
    expect_equivalent(chars$yield, c(0.213269035375, 0.185025217469, 0.16306729083))
    expect_equivalent(chars$alpha,c(1.355601858231, 1.17607569322, 1.036504535587))
    expect_equivalent(chars$m, c(-1.418317351576, -0.70227757562, -0.145598272436))
    expect_equivalent(chars$resilience, c(0.160606060606, 0.209595959596, 0.280303030303))
    expect_equivalent(chars$vulnerability, c(384376.37714, 295149.73258, 245221.73623))
    expect_equivalent(chars$dimless_vulner, c(0.683935745794, 0.605337932228, 0.570661436397))
})

test_that("fill times are calculated", {
    set.seed(446)
    fill = fill_time(riv, yield = 0.14, samples = 10)
    expect_equal(fill$begin, c(1, 78, 104, 117, 133, 526, 731, 914, 1099, 1116, 1244))
    expect_equal(fill$months, c(87, 128, 104, 91, 109, 444, 244, 217, NA, NA, NA))
})

reser = data.frame(
    Q = c(0.078, 0.065, 0.168, 0.711, 0.154, 0.107, 0.068, 0.057, 0.07, 0.485, 0.252, 0.236,
        0.498, 0.248, 0.547, 0.197, 0.283, 0.191, 0.104, 0.067, 0.046, 0.161, 0.16, 0.094),
    DTM = seq(as.Date("2000-01-01"), by = "months", length.out = 24))
reser = as.wateres(reser, storage = 14.4e6, area = 754e3)

test_that("summary for short time series is calculated", {
    chars = summary(reser, reliability = 0.95) # failure in last time step
    expect_equivalent(as.numeric(chars), c(14.4e6, 0.95652173913, 0.453715758324, 2.15755462646, -28.4899573928, 1, 963407.750510, 0.805772962576))
})

context("storage-reliability-yield relationship")

test_that("storage for reliability and yield is optimized", {
    sry = sry(riv, reliab = 0.5, yield = 0.14, prob_type = "ch")
    expect_equivalent(sry$storage, 40175.9600915)
    expect_equivalent(sry$reliability, 0.499621326871)
    expect_equivalent(sry$yield, 0.14)
    # increase of upper limit of storage needed
    sry = sry(riv, reliab = 0.7, yield = 0.7, upper = 100, prob_type = "ch")
    expect_equivalent(sry$storage, 1318806579.71)
    expect_equivalent(sry$reliability, 0.700318085429)
    expect_equivalent(sry$yield, 0.7)
})

test_that("invalid reliability is rejected", {
    expect_error(sry(riv, reliab = -0.5, yield = 0.14))
    expect_error(sry(riv, reliab = 1, yield = 0.14, prob_type = "ch"))
})

test_that("different probability types can be used", {
    expect_error(sry(riv, reliab = 1, yield = 0.14, prob_type = 11))
    expect_error(sry(riv, reliab = 1, yield = 0.14, prob_type = "pus"))
    sry = sry(riv, reliab = 0.5, yield = 0.14, prob_type = 4)
    expect_equivalent(sry$storage, 40175.9600927)
    expect_equivalent(sry$reliability, 0.5)
    expect_equivalent(sry$yield, 0.14)
    sry = sry(riv, reliab = 0.5, yield = 0.14, prob_type = 7)
    expect_equivalent(sry$storage, 40175.9600915)
    expect_equivalent(sry$reliability, 0.499620924943)
    expect_equivalent(sry$yield, 0.14)
})

test_that("reliability for storage and yield is calculated", {
    sry = sry(riv, storage = 41e3, yield = 0.14, prob_type = "ch")
    expect_equivalent(sry$storage, 41e3)
    expect_equivalent(sry$reliability, 0.499621326871)
    expect_equivalent(sry$yield, 0.14)

    # default storage value
    sry = sry(riv, yield = 0.14, prob_type = "ch")
    expect_equivalent(sry$storage, 14.4e6)
    expect_equivalent(sry$reliability, 0.999469857619)
    expect_equivalent(sry$yield, 0.14)

    # evaporation applied
    riv = set_evaporation(riv, altitude = 529)
    sry = sry(riv, storage = 41e3, yield = 0.14, prob_type = "ch")
    expect_equivalent(sry$storage, 41e3)
    expect_equivalent(sry$reliability, 0.433732202363)
    expect_equivalent(sry$yield, 0.14)

    # withdrawal applied
    riv = set_withdrawal(riv, c(23, 31, 35, 33, 30, 42, 47, 33, 27, 22, 24, 32) * 1e3)
    sry = sry(riv, storage = 41e3, yield = 0.14, prob_type = "ch")
    expect_equivalent(sry$storage, 41e3)
    expect_equivalent(sry$reliability, 0.430702817328)
    expect_equivalent(sry$yield, 0.14)
})

test_that("yield for storage and reliability is optimized", {
    sry = sry(riv, storage = 41e3, reliab = 0.5, prob_type = "ch")
    expect_equivalent(sry$storage, 41e3)
    expect_equivalent(sry$reliability, 0.500378673129)
    expect_equivalent(sry$yield, 0.139881646821)
    sry = sry(riv, storage = 14.4e6, reliab = 1, prob_type = 4)
    expect_equivalent(sry$storage, 14.4e6)
    expect_equivalent(sry$reliability, 1)
    expect_equivalent(sry$yield, 0.14501888252)
})

test_that("yield is optimized with the option to throw exceeding volume", {
    sry1 = sry(riv, storage = 41e3, reliab = 0.88, prob_type = "ch")
    sry2 = sry(riv, storage = 41e3, reliab = 0.88, prob_type = "ch", throw_exceed = TRUE)
    expect_equivalent(sry1$storage, 41e3)
    expect_equivalent(sry1$reliability, 0.880566495002)
    expect_equivalent(sry1$yield, 0.0618148688114)
    expect_equivalent(sry1, sry2)
})

test_that("storage-reliability-yield is calculated for corner cases of bisection", {
    # both negative - storage (for yield cannot be both negative as for 0 yield is always max. reliability)
    expect_error(sry(riv, reliability = 0.95, yield = 20, prob_type = "ch", upper = 1))
    # both positive - storage 0
    sry = sry(riv, reliability = 0.95, yield = 0.01, prob_type = "ch", upper = 1)
    expect_equivalent(sry, list(storage = 0, reliability = 0.9971978188, yield = 0.01))
    # both positive - yield
    expect_error(sry(riv, reliability = 0.95, storage = 1e6, prob_type = "ch", upper = 0.5))
    # both equal positive - storage 0
    sry = sry(riv, reliability = 0.95, yield = 0.01, prob_type = "ch", upper = 1e-4)
    expect_equivalent(sry, list(storage = 0, reliability = 0.9971978188, yield = 0.01))
    # both equal positive - yield
    expect_error(sry(riv, reliability = 0.95, storage = 1e6, prob_type = "ch", upper = 1e-4))
    # both equal zero - storage 0
    sry = sry(riv, reliability = "max", yield = 0.001, prob_type = "ch", upper = 1e-4)
    expect_equivalent(sry, list(storage = 0, reliability = 0.9994698576, yield = 0.001))
    # both equal zero - yield
    expect_error(sry(riv, reliability = "max", storage = 1e6, prob_type = "ch", upper = 1e-4))
    # zero and negative - storage upper - then decreased in the additional bisection
    sry = sry(riv, reliability = "max", yield = 0.1, prob_type = "ch", upper = 1)
    expect_equivalent(sry, list(storage = 2474927.96008955, reliability = 0.9994698576, yield = 0.1))
    # zero and negative - yield 0 - then increased in the additional bisection
    sry = sry(riv, reliability = "max", storage = 1e6, prob_type = "ch", upper = 1)
    expect_equivalent(sry, list(storage = 1e6, reliability = 0.9994698576, yield = 0.07089385265))
    # zero and positive - storage 0
    sry = sry(riv, reliability = 0.63594365343835201898, yield = 0.1, prob_type = "ch", upper = 1)
    expect_equivalent(sry, list(storage = 0, reliability = 0.6359436534, yield = 0.1))
    # zero and positive - yield
    expect_error(sry(riv, reliability = 0.75030293850348372953, storage = 1e6, prob_type = "ch", upper = 1))
    # in additional bisection storage decreased to zero with reliability slightly less than required one
    sry = sry(riv, reliab = 0.6363636364, yield = 0.1, prob_type = 4)
    expect_equivalent(sry, list(storage = 0, reliability = 0.6363636364, yield = 0.1))
})
