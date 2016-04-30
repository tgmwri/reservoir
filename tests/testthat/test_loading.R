context("data loading")

test_that("data are loaded from Bilan object", {
    library(bilan)
    bil = bil.new("m")
    bil.read.file(bil, "rivendell.dat", c("P", "R", "T"))
    riv = as.wateres(bil, 14.4e6, 754e3, observed = TRUE)
    expect_equivalent(riv$Q[1:12], c(0.07799990666, 0.06500006200, 0.16799987866, 0.71100096451,
        0.15399985999, 0.10699987461, 0.06800014001, 0.05699987866, 0.06999987461, 0.48499990666,
        0.25200004823, 0.23600001867))
    expect_equivalent(riv$DTM[1:12], seq(as.Date("1901-01-01"), by = "month", length.out = 12))
    expect_equivalent(riv$minutes[1:12], c(44640, 40320, 44640, 43200, 44640, 43200, 44640, 44640, 43200, 44640, 43200, 44640))

    bild = bil.new("d")
    bil.read.file(bild, "rivendell.dat", c("P", "R", "T")) # monthly data to daily just to test loading
    rivd = as.wateres(bild, 14.4e6, 754e3, observed = TRUE)
    expect_equivalent(rivd$Q[1:12], c(2.41799710648148, 1.82000173611111, 5.20799623842593, 21.3300289351852,
        4.77399565972222, 3.20999623842593, 2.10800434027778, 1.76699623842593, 2.09999623842593, 15.0349971064815,
        7.56000144675926, 7.3160005787037))
    expect_equivalent(rivd$DTM[1:12], seq(as.Date("1901-01-01"), by = "day", length.out = 12))
    expect_equivalent(rivd$minutes[1:12], rep(1440, 12))
})

test_that("data are loaded from data table", {
    library(data.table)
    riv_data = as.data.table(read.table("rivendell.txt", header = TRUE))
    riv = as.wateres(riv_data, 14.4e6, 754e3)
    expect_equivalent(riv$Q[1:12], c(0.078, 0.065, 0.168, 0.711, 0.154, 0.107, 0.068, 0.057, 0.07,
        0.485, 0.252, 0.236))
})

eas = data.frame(
    elevation = c(496, 499, 502, 505, 508, 511, 514, 517, 520, 523, 526, 529),
    area = c(0, 5e3, 58e3, 90e3, 133e3, 180e3, 253e3, 347e3, 424e3, 483e3, 538e3, 754e3),
    storage = c(0, 3e3, 161e3, 530e3, 1.085e6, 1.864e6, 2.943e6, 4.439e6, 6.362e6, 8.626e6, 11.175e6, 14.4e6))

test_that("elevation-area-storage relationship is set", {
    expect_warning(as.wateres("rivendell.txt", 14.4e6, 754e3, eas = data.frame(elev = 529)))
    riv = as.wateres("rivendell.txt", 14.4e6, 754e3, eas = eas)
    expect_equivalent(attr(riv, "eas"), eas)
    orig_eas = eas
    eas$storage[2] = 2e5
    expect_warning(as.wateres("rivendell.txt", 14.4e6, 754e3, eas = eas))
    eas$storage[2] = NA
    expect_warning(as.wateres("rivendell.txt", 14.4e6, 754e3, eas = eas))
    eas = orig_eas[c(1, 3:2, 4:nrow(eas)),]
    riv = as.wateres("rivendell.txt", 14.4e6, 754e3, eas = eas)
    expect_equivalent(attr(riv, "eas"), orig_eas)
})

test_that("reservoir input time series are resized", {
    dtm = seq(as.Date("1901-01-15"), as.Date("1903-12-15"), "month")
    reser_data = data.frame(Q = rep(4.46, length(dtm)), DTM = dtm)
    reser = as.wateres(reser_data, 14e6, 7e3)
    reserh = as.wateres(data.frame(Q = reser_data$Q), 14e6, 7e3, time_step = "hour")

    expect_error(resize_input(reser, "a"), "date or index needed")
    expect_error(resize_input(reserh, "1981-01-01"), "series do not include dates")
    expect_error(resize_input(reser, "1981-01-01", "1979-01-01"), "cannot be less than begin")

    resul = resize_input(reser, -5)
    expect_equivalent(resul$DTM, seq(as.Date("1900-07-15"), by = "month", length.out = 42))
    expect_equivalent(resul$Q, c(rep(0, 6), rep(4.46, 36)))
    expect_equal(class(resul), c("wateres", "data.table", "data.frame"))
    expect_equal(names(attributes(resul)), c("names", "class", "row.names", ".internal.selfref",
        "time_step", "storage", "area", "id", "down_id", "title"))

    resul = resize_input(reser, "1902-01-01", "1903-04-01")
    expect_equivalent(resul$DTM, seq(as.Date("1902-01-15"), by = "month", length.out = 15))
    expect_equivalent(resul$Q, rep(4.46, 15))

    resul = resize_input(reser, "1900-01-01", "1903-04-01")
    expect_equivalent(resul$DTM, seq(as.Date("1900-01-15"), by = "month", length.out = 39))
    expect_equivalent(resul$Q, c(rep(0, 12), rep(4.46, 27)))

    resul = resize_input(reser, "1901-01-01", "1905-01-01")
    expect_equivalent(resul$DTM, seq(as.Date("1901-01-15"), by = "month", length.out = 48))
    expect_equivalent(resul$Q, c(rep(4.46, 36), rep(0, 12)))

    resul = resize_input(reser, "1904-01-01", "1907-06-01")
    expect_equivalent(resul$DTM, seq(as.Date("1904-01-15"), by = "month", length.out = 41))
    expect_equivalent(resul$Q, rep(0, 41))
})
