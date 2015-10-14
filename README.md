# wateres

An R package focused on calculation of characteristics and performing
simulations for water reservoirs.

## Installation

```
library(devtools)
install_github("tgmwri/wateres")
```
If you live with unfriendly network settings and the installation above
fails, try to use
```
library(devtools)
install_git("https://github.com/tgmwri/wateres.git")
```

## Getting started

Create a `wateres` object from vectors of reservoir inflow and corresponding
dates (an example input file is located in `tests/testthat`) and then
use its methods to get some characteristics and estimations:
```
library(wateres)
riv = as.wateres("rivendell.txt", 14.4e6, 754e3)
riv = set_evaporation(riv, altitude = 529)
chars = summary(riv)
sry = sry(riv, reliab = 0.5, yield = 0.14)
prob_field = prob_field(riv, c(0.1, 0.9, 0.99), 0.14)
plot(prob_field, "storage")
plot(alpha_beta(riv))
```
