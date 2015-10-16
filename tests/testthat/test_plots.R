context("data for plots")

riv = as.wateres("rivendell.txt", 14.4e6, 754e3)
eas = data.frame(
    elevation = c(496, 499, 502, 505, 508, 511, 514, 517, 520, 523, 526, 529),
    area = c(0, 5e3, 58e3, 90e3, 133e3, 180e3, 253e3, 347e3, 424e3, 483e3, 538e3, 754e3),
    storage = c(0, 3e3, 161e3, 530e3, 1.085e6, 1.864e6, 2.943e6, 4.439e6, 6.362e6, 8.626e6, 11.175e6, 14.4e6))

test_that("probability fields are calculated", {
    prob_field = prob_field(riv, c(0.1, 0.9, 0.99), 0.14)
    expect_equivalent(prob_field, readRDS("prob_field.rds"))
    expect_error(plot(prob_field, "level")) # no levels in probability field
    riv = as.wateres("rivendell.txt", 14.4e6, 754e3, eas = eas)
    prob_field_eas = prob_field(riv, c(0.1, 0.9, 0.99), 0.14)
    expect_equivalent(prob_field_eas, readRDS("prob_field_eas.rds"))
})

test_that("alphas and betas are calculated", {
    alpha_beta = alpha_beta(riv, upper = 10)
    expect_equivalent(alpha_beta, readRDS("alpha_beta.rds"))
})

test_that("alphas and betas for more reliabiliites are calculated", {
    alpha_beta = alpha_beta(riv, upper = 10, reliability = c(0.9, 0.95, 1), prob_type = 4, alphas = seq(0.4, 1, 0.02))
    expect_equivalent(alpha_beta, readRDS("alpha_beta_reliabs.rds"))
})
