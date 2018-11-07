
# Correlated Pseudo Marginal Monte Carlo
## cpmmc

An R package to perform the Correlated Pseudo Marginal Method, see paper detailing methodology [here](https://arxiv.org/abs/1511.04992)

Undertaken as part of the 1 week OxWaSP module: “Statistical Computing and Computational Statistics”

In partnership with Yuxi Jiang, OxWaSP- University of Warwick

[Report](https://github.com/JTT94/cpmmc/blob/master/vignettes/cpmmc.pdf)
[Presentation](https://github.com/JTT94/cpmmc/blob/master/Correlated_Pseudo_Marginal_Monte_Carlo.pdf)

--------------------------------------

This package can be installed as follows:

```R
devtools::install_github("JTT94/cpmmc")
```

Vignette help can be found as follows:

```R
library(cpmmc)
vignette('cpmmc', package='cpmmc')
```

Example code:

```R
# Random Effect CPM (specific case inherited from cpmmc class)
rem_cpm <- normal_random_effect_model(data,
                                      theta_0 = theta_0,
                                      u_0 = u_0,
                                      rho = rho
                                      )
# Run models for nsim iterations
rem_cpm <- run_chain(rem_cpm, chain_length = nsim)
```
