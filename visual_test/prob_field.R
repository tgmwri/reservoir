vcontext("probability fields")

library(ggplot2)

riv = as.wateres("tests/testthat/rivendell.txt", 14.4)
prob_field = prob_field(riv, c(0.1, 0.9, 0.99), 0.14)
plot(prob_field, "storage")
save_vtest("storage probability field")

plot(prob_field, "yield")
save_vtest("yield probability field")

alpha_beta = alpha_beta(riv)
plot(alpha_beta)
save_vtest("alpha beta plot")

end_vcontext()
