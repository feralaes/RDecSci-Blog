# CE Frontier
Fernando Alarid-Escudero  
`r format(Sys.time(), '%B %d, %Y')`  



# Overview
In this post I will show how to plot the cost-effectiveness frontier with the strategies from a cost-effectiveness analysis. We will use a recently published efficient algorithm to find the strategies that form the efficient frontier. Once we know which strategies are in the frontier we will plot all the strategies and the draw the frontier.

# Finding and plotting the frontier
To find the frontier we will use a recently published algorithm written by my colleagues [Sze-Chuan Suen and Jeremy Golhaber-Fiebert at MDM journal][@Suen2015].[@Suen2015] 

We will use the dataset provided by the authors. We will load the data an generate a matrix out of it

```r
ce.mat <- as.matrix(read.csv("Data/CostQalyInputFile.csv", header = TRUE))
```

We will use a wrapper function of the algorithm called `getFrontier` to find the frontier. This function can be found in the `Functions.R` file in this repository. **Note**: The code used in this function to find the frontier is heavily based on the authors code provided as a supplementary material in the paper. For more details about the algorithm I suggest the reader to take a peak to the published article.

This function returns the strategies that form the frontier and plots the frontier with basic `R` plot functionality.


```r
ce.frontier <- getFrontier(ce.mat)
```

![](CE-Frontier_files/figure-html/Get-frontier-1.png) 

Frontier is formed by strategies: 8, 52, 70, 89, 93, 97


# Ploting the frontier using `ggplot`
Although the basic graph is informative in the sense that it displays in red the strategies that form the frontier, we are interested in both enhancing the quality of the graph using `ggplot` and draw the frontier.

To do this we will need to load `ggplot2` package

```r
require(ggplot2)
require(scales) # For label formatting in ggplot
```


![](CE-Frontier_files/figure-html/ce-frontier-1.png) 

Because the original dataset is to big, we will randomly sample 15 strategies and use them as our total set of strategies to find the frontier.

```r
set.seed(3877) # Set seed for random sampling
ce.mat.sub <- ce.mat[sample(1:100, 15), ]
```

And redo the whole analysis


```r
ce.sub.frontier <- getFrontier(ce.mat.sub)
```

![](CE-Frontier_files/figure-html/Get-frontier-reduced-1.png) 

```r
plotFrontier(ce.mat.sub, ce.sub.frontier)
```

![](CE-Frontier_files/figure-html/Get-frontier-reduced-2.png) 

Frontier is formed by strategies: 31, 34, 89

[@Suen2015]: http://mdm.sagepub.com/cgi/doi/10.1177/0272989X15583496
# References
