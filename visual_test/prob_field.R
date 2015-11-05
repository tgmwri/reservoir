vcontext("probability fields")

library(ggplot2)

eas = data.frame(
    elevation = c(496, 499, 502, 505, 508, 511, 514, 517, 520, 523, 526, 529),
    area = c(0, 5e3, 58e3, 90e3, 133e3, 180e3, 253e3, 347e3, 424e3, 483e3, 538e3, 754e3),
    storage = c(0, 3e3, 161e3, 530e3, 1.085e6, 1.864e6, 2.943e6, 4.439e6, 6.362e6, 8.626e6, 11.175e6, 14.4e6))
riv = as.wateres("tests/testthat/rivendell.txt", 14.4e6, 754e3, eas = eas)
prob_field = prob_field(riv, c(0.1, 0.9, 0.99), 0.14)
plot(prob_field, "storage")
save_vtest("storage probability field")

plot(prob_field, "yield")
save_vtest("yield probability field")

plot(prob_field, "level")
save_vtest("level probability field")

alpha_beta = alpha_beta(riv, upper = 10)
plot(alpha_beta)
save_vtest("alpha beta plot")

alpha_beta = alpha_beta(riv, upper = 10, reliability = c(0.9, 0.95, 1), prob_type = 4, alphas = seq(0, 1, 0.02))
plot(alpha_beta)
save_vtest("alpha beta plot for more reliabilities")

riv = set_evaporation(riv, altitude = 529)
series = calc_series(riv, 14.4e6, 0.14, FALSE)
plot(series, riv)
save_vtest("time series")

end_vcontext()
