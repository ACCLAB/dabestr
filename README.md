# dabestr

dabestr is a package for **D**ta **A**nalysis using **B**ootstrap-Coupled **EST**imation in R.

## About
[Estimation statistics](https://en.wikipedia.org/wiki/Estimation_statistics "Estimation Stats on Wikipedia") is a [simple framework](https://thenewstatistics.com/itns/ "Introduction to the New Statistics") that avoids the [pitfalls](https://www.nature.com/articles/nmeth.3288 "The fickle P value generates irreproducible results, Halsey et al 2015") of significance testing. It uses familiar statistical concepts: means, mean differences, and error bars. More importantly, it focuses on the effect size of one's experiment/intervention, as opposed to a false dichotomy engendered by P values.

An estimation plot has two key features.

1. It presents all datapoints as a swarmplot, which orders each point to display the underlying distribution.

2. It presents the effect size as a bootstrap 95% confidence interval on a separate but aligned axes.


## Installation
```r
install.packages("dabestr")
```

## Usage

```r
# Some basic code usage coming soon!
my_plot <- dabestr.plot(...)
```

## How to Cite

**Moving beyond P values: Everyday data analysis with estimation plots**

Joses Ho, Tayfun Tumkaya, Sameer Aryal, Hyungwon Choi, Adam Claridge-Chang

https://doi.org/10.1101/377978

## dabest In Other Languages

dabestr is also available in [Python](https://github.com/ACCLAB/DABEST-python "DABEST-Python on Github") and [Matlab](https://github.com/ACCLAB/DABEST-Matlab "DABEST-Matlab on Github").

## Bugs

Where to report on the Github tracker?

## Contributing

All contributions are welcome. Please fork this Github repo and open a pull request.


