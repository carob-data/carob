# Carob

<img align="right" width="350" height="350" src="https://github.com/carob-data/carob/blob/main/misc/img/carob.png">

*Carob* creates reproducible workflows that standardize primary agricultural research data from experiments and surveys. Standardization includes the use of a common file format, variable names, units and accepted values according to the [terminag](https://github.com/controvoc/terminag) standard. Standardized data sets are aggregated into larger collections that can be used in further research. We do this by writing an *R* script for each individual dataset. See the [website](https://carob-data.org) for more information.

Carob is an open access *Extract, Transform, and Load* (ETL) framework supported by [CGIAR](https://www.cgiar.org/initiative/excellence-in-agronomy/) to support predictive analytics (machine learning, artifical intelligence) and other types of data analysis. 

Contributions are welcome from anyone, and they can be made via pull-requests. Feel free to improve these scripts, or provide new ones. See the instructions on how to write a Carob script described [here](https://carob-data.org/contribute/index.html). You can also [raise an issues](https://github.com/carob-data/carob/issues). A good place to discover new data sets is the [Gardian](https://gardian.bigdata.cgiar.org/) website or our [to-do list](https://carob-data.org/contribute/todo.html). 

### Get the data

Standardized data can be downloaded from [carob-data.org](http://carob-data.org) (data with a [CC license](https://carob-data.org/data/licenses.html) only), or with *R* package [caramba](https://github.com/carob-data/caramba). 

You can also compile your own version by cloning this repo and running 

```
remotes::install_github("carob-data/carobiner")
ff <- carobiner::make_carob(path)
```

where `path` is the folder of the cloned repo (e.g. `"d:/github/carob"`)

