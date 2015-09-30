context("probability fields")

riv = as.wateres("rivendell.txt", 14.4)

test_that("probability fields are calculated", {
    prob_field = prob_field(riv, c(0.1, 0.9, 0.99), 0.14)
    expect_equivalent(prob_field, readRDS("prob_field.rds"))
})
