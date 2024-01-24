library(smoof)
library(ecr)
library(ggplot2)
library(logger)

set.seed(42)

calculate_mins <- function(n) {
  fn <- makeAckleyFunction(dimensions = n)
  
  PRS_mins <-
    replicate(50,
      min(replicate(1000, fn(runif(n, min = -32.768, max = 32.768))))
    )
  
  lower = getLowerBoxConstraints(fn)
  upper = getUpperBoxConstraints(fn)
  
  GA_mins <-
    replicate(50,
      ecr(
        fitness.fun = fn,
        n.dim = n,
        representation = "float",
        mu = 50,
        lambda = 100,
        lower = lower,
        upper = upper,
        mutator = setup(mutGauss, sdev = 2, lower = lower, upper = upper)
      )$best.y
    )
  log_info("Done.")
  return(list(PRS = PRS_mins, GA = GA_mins))
}

results <- lapply(c(2, 10, 20), calculate_mins)

data_to_plot <- do.call(rbind, lapply(1:length(results), function(i) {
  n <- c(2, 10, 20)[i]
  data.frame(
    Method = factor(rep(c("PRS", "GA"), each = 50), levels = c("PRS", "GA")),
    Min_Value = c(results[[i]]$PRS, results[[i]]$GA),
    Dimension = as.factor(n)
  )
}))

pdf(file="ackley_boxplot.pdf")

ggplot(data_to_plot, aes(x = Dimension, y = Min_Value, fill = Method)) +
  geom_boxplot() +
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  labs(title = "Znalezione minima n-wymiarowej funkcji Ackley'a", 
       x = "wymiar (n)", 
       y = "minimum")

dev.off()
