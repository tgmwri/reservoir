vcontext("probability fields")

library(ggplot2)

eas = data.frame(
    elevation = c(496, 499, 502, 505, 508, 511, 514, 517, 520, 523, 526, 529),
    area = c(0, 0.005, 0.058, 0.09, 0.133, 0.18, 0.253, 0.347, 0.424, 0.483, 0.538, 0.754),
    storage = c(0.000, 0.003, 0.161, 0.530, 1.085, 1.864, 2.943, 4.439, 6.362, 8.626, 11.175, 14.400))
riv = as.wateres("tests/testthat/rivendell.txt", 14.4, 0.754, eas = eas)
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

end_vcontext()
