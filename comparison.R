library(smoof)
library(ecr)
library(ggplot2)
library(logger)
library(extrafont)

# font_import(pattern = "lmodern*")
loadfonts()
set.seed(42)

calculate_mins <- function(n) {
  fn <- makeAckleyFunction(dimensions = n)
  # fn <- makeRosenbrockFunction(dimensions = n)
  
  lower = getLowerBoxConstraints(fn)
  upper = getUpperBoxConstraints(fn)
  
  PRS_mins <-
    replicate(50,
      min(replicate(1000, fn(runif(n, min = lower[[1]], max = upper[[1]]))))
    )
  
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
    method = factor(rep(c("PRS", "GA"), each = 50), levels = c("PRS", "GA")),
    min_value = c(results[[i]]$PRS, results[[i]]$GA),
    dimension = as.factor(n)
  )
}))

ggplot(data_to_plot, aes(x = dimension, y = min_value, fill = method)) +
  geom_boxplot() +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        text = element_text(size=10, family="LM Roman 10"),
        plot.title = element_text(hjust = 0.5)) +
  labs(
    title = "Znalezione minima n-wymiarowej funkcji Ackley'a",
    # title = "Znalezione minima n-wymiarowej funkcji Rosenbrock'a",
    x = "wymiar (n)",
    y = "minimum",
    fill = "metoda")
