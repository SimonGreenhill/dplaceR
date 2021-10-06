# dplaceR - R functions for DPLACE

## Install

You can install dplaceR directly from GitHub using devtools:

```r
devtools::install_github("SimonGreenhill/dplaceR", dependencies = TRUE)
```

## Load Trees

```r
# load trees as-is
trees.mcct <- load_trees('gray_et_al2009')
trees.posterior <- load_trees('gray_et_al2009', type="posterior")

# load trees and rename to glottocodes
trees.glottocode <- load_trees('gray_et_al2009', renameto="glottocode")

# load trees and rename to society IDS
trees.socid <- load_trees('gray_et_al2009', renameto="soc_ids")  # or xd_ids
```


