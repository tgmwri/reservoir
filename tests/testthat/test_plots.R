context("data for plots")

riv = as.wateres("rivendell.txt", 14.4, 0.754)
eas = data.frame(
    elevation = c(496, 499, 502, 505, 508, 511, 514, 517, 520, 523, 526, 529),
    area = c(0, 0.005, 0.058, 0.09, 0.133, 0.18, 0.253, 0.347, 0.424, 0.483, 0.538, 0.754),
    storage = c(0.000, 0.003, 0.161, 0.530, 1.085, 1.864, 2.943, 4.439, 6.362, 8.626, 11.175, 14.400))

test_that("probability fields are calculated", {
    prob_field = prob_field(riv, c(0.1, 0.9, 0.99), 0.14)
    expect_equivalent(prob_field, readRDS("prob_field.rds"))
    expect_error(plot(prob_field, "level")) # no levels in probability field
    riv = as.wateres("rivendell.txt", 14.4, 0.754, eas = eas)
    prob_field_eas = prob_field(riv, c(0.1, 0.9, 0.99), 0.14)
    expect_equivalent(prob_field_eas, readRDS("prob_field_eas.rds"))
})

test_that("alphas and betas are calculated", {
    alpha_beta = alpha_beta(riv, upper = 10)
    expect_equivalent(alpha_beta, readRDS("alpha_beta.rds"))
})

