getFrontier <- function(CEmat, maxWTP = Inf, plot=TRUE){
  # Name: getFrontier
  # Goal: Find the CEA frontier, up to a given WTP level, by 
  #       identifying strategies with the highest NMB
  # Originally written by: Sze-Chuan Suen on Apr 30, 2015
  # Wrapped by: Fernando Alarid-Escudero on July 20, 2015 
  # Notes: 
  #    ~Frontier strategies are displaced on the R output screen and 
  #      plotted in red on the scatter plot.  
  #
  #    ~User needs to provide a csv file of costs and QALYs 
  #	    (CostQalyInputFile_online_supp.csv) inside the folder specified    
  #	    below (inputFolder). The CSV should have three columns (labeled 
  #     in first row) in this order: 
  #      Strategy number, costs, and QALYs.
  #
  #    ~User can specify the maximum willingness-to-pay level to 
  #      consider (maxWTP).  Can be Inf for infinity.
  #
  #    ~QALY-reducing strategies will be on the frontier if they save
  #      enough money (script assumes maximum willingness to save money 
  #      per QALY lost is equivalent to maximum willingness to pay per QALY
  #      gained). If the user does not wish to consider such policies as
  #      being on the frontier, do not include strategies with negative 
  #      QALYs in the input csv file.
  #
  #    ~Script does not use the one-line code cited in the text
  #      as the max function is slow. This implementation is
  #      faster and methodologically does the same thing.
  #
  #    ~May take a few minutes if thousands of strategies and 
  #       processing resources are low.  Please be patient.
  #
  #    Please cite article if this code is used.
  #
  # USER INPUTS:
  #inputFolder <- "CostEffectivenessFrontier_MDM/"
  #maxWTP <- Inf        # any positive value or Inf
  
  ## Clean everythng from workspace
  #rm(list=ls())
  ####################################################################
  ####################################################################
  
  # check for duplicated strategies
  dups <- CEmat[c(duplicated(CEmat[,2:3]) | duplicated(CEmat[,2:3], fromLast = TRUE)),1]
  
  # initialize some variables
  costsCol <- 2; qalyCol <- 3
  numStrat <- nrow(CEmat)
  
  # find WTP levels to test so that all strategies on frontier will be captured
  # this means testing on either side of all NMB intersections, which are just all the pairwise ICERs
  ICERmat <- matrix(1, numStrat, numStrat)
  for (i in 1:numStrat ) {
    indexStrat <- matrix(1, numStrat, 3)
    indexStrat[,costsCol] <- indexStrat[,costsCol]*CEmat[i,costsCol]
    indexStrat[,qalyCol] <- indexStrat[,qalyCol]*CEmat[i,qalyCol]
    delCostQalys <- CEmat - indexStrat
    ICERmat[,i] <- delCostQalys[,costsCol] / delCostQalys[,qalyCol]
  }  
  intersections <- sort(unique(c(ICERmat)))
  intersections <- intersections[is.finite(intersections)]
  WTPtestPoints <- c(0, intersections [intersections >= 0 & intersections <= maxWTP ], maxWTP)
  
  # Find the strategy with the max NMB at each of the WTP test points
  indiciesOfMax <- vector()
  NMBmat <- matrix(0, numStrat, length(WTPtestPoints))
  for (i in 1:length(WTPtestPoints) ) {
    NMBmat[,i] <- (WTPtestPoints[i]*CEmat[,qalyCol]) - CEmat[,costsCol]
  }
  if (is.infinite(maxWTP)) {
    #WTP of infinity means costs are not considered
    NMBmat[,length(WTPtestPoints)] = CEmat[,qalyCol] - (0*CEmat[,costsCol]); 
  }
  maxVals <- apply(NMBmat, 2, max)  #find strategy that maximizes NMB at each WTP
  for (i in 1:length(WTPtestPoints) ) {  #find all strategies that match max at each WTP
    indiciesOfMax <- c(indiciesOfMax,which( NMBmat[,i] == maxVals[i]))
  }
  frontier <- unique(indiciesOfMax)  #find strategy that maximizes NMB at each WTP
  
  if(plot){
    # display out: make plot and print to output screen
    plot(CEmat[frontier,qalyCol], CEmat[frontier,costsCol], col = 'red', pch = 16)
    points(CEmat[,qalyCol], CEmat[,costsCol])
  }
  if (length(dups)>0){
    warning("Strategies have the same costs and benefits (displayed above)")
    print(dups)
  }
  sprintf("Frontier is formed by strategies: %s", paste( sort(CEmat[frontier,1]), collapse=" "))
  
  return(frontier)
}

plotFrontier <- function(CEmat, frontier, 
                         ncol = 1,
                         coord.flip = F,
                         txtsize = 12)
{
  # A function to plot CE frontier
  # USER INPUTS:
  #   CEmat: A CE matrix arranged as: Col1: Strategy; Col2: Cost; Col3: Effectiveness
  #   frontier: An indicator vector with strategies that form the frontier
  # Create a dataframe from matrix
  require(scales)
  CEmat.df <- data.frame(CEmat)
  colnames(CEmat.df)[3] <- "Effectiveness"
  n.strategies <- nrow(CEmat.df)
  # Make Strategies as factor
  CEmat.df$Strategy <- as.factor(CEmat.df$Strategy)
  #
  if (coord.flip == T){
    ggplot(CEmat.df, aes(Effectiveness, Cost)) +
      geom_point(aes(color = Strategy, shape = Strategy), size = 4) + 
      coord_flip() +
      ggtitle("Cost-Effectiveness Frontier") +
      geom_point(data = CEmat.df[frontier,], 
                 aes(Effectiveness, Cost, shape = Strategy, color = Strategy), size = 4) +
      geom_line(data = CEmat.df[frontier,], aes(Effectiveness, Cost)) +
      scale_x_continuous(labels = comma) +
      scale_y_continuous(labels = comma) +
      scale_shape_manual(values = 0:(n.strategies-1)) +
      scale_color_hue(l=50) + 
      guides(shape = guide_legend(ncol = ncol)) +
      theme_bw() +
      theme(title = element_text(face = "bold", size = txtsize+2),
            axis.title.x = element_text(face = "bold", size = txtsize),
            axis.title.y = element_text(face = "bold", size = txtsize),
            axis.text.y = element_text(size = txtsize),
            axis.text.x = element_text(size = txtsize))
  } else {
    ggplot(CEmat.df, aes(Effectiveness, Cost)) +
      geom_point(aes(color = Strategy, shape = Strategy), size = 4) + 
      ggtitle("Cost-Effectiveness Frontier") +
      geom_point(data = CEmat.df[frontier,], 
                 aes(Effectiveness, Cost, shape = Strategy, color = Strategy), size = 4) +
      geom_line(data = CEmat.df[frontier,], aes(Effectiveness, Cost)) +
      scale_x_continuous(labels = comma) +
      scale_y_continuous(labels = comma) +
      scale_shape_manual(values = 0:(n.strategies-1)) +
      scale_color_hue(l=50) + 
      guides(shape = guide_legend(ncol = ncol)) +
      theme_bw() +
      theme(title = element_text(face = "bold", size = txtsize+2),
            axis.title.x = element_text(face = "bold", size = txtsize),
            axis.title.y = element_text(face = "bold", size = txtsize),
            axis.text.y = element_text(size = txtsize),
            axis.text.x = element_text(size = txtsize))
  }
}

#### Formatting Functions ####
number_ticks <- function(n) {function(limits) pretty(limits, n)} #Function for number of ticks in ggplot
grid_arrange_shared_legend <- function(...) {
  # Source: https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, lapply(plots, function(x)
      x + theme(legend.position="none"))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
}
arrange_grob_shared_legend <- function(...) {
  # My modification using arrangeGrob instead, Works Better!!!
  plots <- list(haz.gg, Surv.gg)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  arrangeGrob(
    do.call(arrangeGrob, c(lapply(plots, function(x)
      x + theme(legend.position="none")), ncol = 2)), # Modified to plot 2 columns
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight)) # main = "Extrapolation of Survival"
}