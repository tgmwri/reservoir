# wateres

An R package focused on calculation of characteristics and performing
simulations for water reservoirs.

## Installation

```{r}
library(devtools)
install_github("tgmwri/wateres")
```
If you live with unfriendly network settings and the installation above
fails, try to use
```{r}
library(devtools)
install_git("https://github.com/tgmwri/wateres.git")
```

## Getting started

Create a `wateres` object from vectors of reservoir inflow and corresponding
dates (an example input file is located in `tests/testthat`) and then
use its methods to get some characteristics and estimations:
```{r}
library(wateres)
riv = as.wateres("rivendell.txt", 14.4e6, 754e3)
riv = set_evaporation(riv, altitude = 529)
series = calc_series(riv, yield = 0.14)
plot(series, riv, "flow")
plot(series, riv, "storage")
chars = summary(riv)
sry = sry(riv, reliab = 0.5, yield = 0.14)
prob_field = prob_field(riv, c(0.1, 0.9, 0.99), 0.14)
plot(prob_field, "storage")
plot(alpha_beta(riv))
```

## Use cases
The package can be used to calculate:
- long-term water balance of reservoirs and their systems,
- water reservoir characteristics to estimate its efectiveness,
- deficit volumes for individual catchments and reservoirs
and their systems,
- flood wave transformation.

## Further reading
Current capabilities of the package are described in the
[Reservoir Characteristics](http://lapv.vuv.cz/wateres/vignettes/reservoir.html)
and [System of Reservoirs](http://lapv.vuv.cz/wateres/vignettes/system.html) vignettes.
