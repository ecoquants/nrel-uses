# docs

All online documentation will be created here, using [bookdown](bookdown.org/yihui/bookdown).

## Technical

Regenerate report:

```r
# prep layers
source("scripts/prep_layers.R")

# generate report
rmarkdown::render("docs/nrel-uses.Rmd", "all")
```