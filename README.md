# Install
```
if (!requireNamespace("devtools", quietly = TRUE))
library(devtools)
devtools::install_github('genmor/asmidx')
```
## asmidx
`asmidx` is a package written for R to assess draft genome quality. The main function call `asmidx()` is shiny GUI where the user can upload a dataframe of assembly metrics—this can be anything the user thinks is relevant to assessing assembly quality. In `asmidx()` the user can choose any set of numerical metrics to create a ranking. To create a ranking, the user can choose a metric to be either maximized (e.g., N50, number of BUSCO complete) or minimized (e.g., L50, number of BUSCO missing). Each metric is then feature normalized from 0–1 (worst–best) and a row mean is calculated from these values and multiplied by 100 to output a normalized score. Each draft assembly is ranked by this normalized score.

Each metric can also be weighted by any positive value defined by the user, and a weighted output can also be output. `asmidx()` will create a lollipop plot and output a table (normalized and weighted), which the user can save to their device.
