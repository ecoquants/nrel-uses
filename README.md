# nrel-uses
Competing ocean uses work with National Renewable Energy Lab (NREL)

Here are other related Github repositories (in order from oldest to newest):
* [**nrelutils**](https://github.com/ecoquants/nrelutils): R package to facilitate nrel-uses analysis
* [**nrel-docker**](https://github.com/ecoquants/nrel-docker): full software stack to run the App, containerized using Docker
* [**nreluseblocks**](https://github.com/ecoquants/nreluseblocks): latest analysis by lease blocks (vs raster cells), as an R package

## Interactive App

The [Shiny](http://shiny.rstudio.com/) application (code in [app](https://github.com/ecoquants/nrel-uses/tree/master/app) folder) provides an interactive web interface to data layers, particularly for implementing alternative weights and sliders to ocean use layers for identifying areas of conflict.

[![](app/images/app_screen.png)](https://ecoquants.shinyapps.io/nrel-uses/)

### Running the App Online

You can find this app online here:

[ecoquants.shiny.io/nrel-uses](https://ecoquants.shinyapps.io/nrel-uses/)

### Running the App Locally with R

You can also directly run the app from a local desktop instance of the [R]() statistical programming language as long as you already have the libraries installed that are listed at the top of the [global.R](https://github.com/ecoquants/nrel-uses/blob/master/app/global.R#L1-L9) file (e.g. `install.packages("shiny")`).

```r
shiny::runGitHub('ecoquants/nrel-uses', subdir='app')
```
