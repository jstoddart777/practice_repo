# Monte Carlo Numerical Integration ----
## Coder: Jared Stoddart
## Create a Monte Carlo Numerical Integration ----

# Step 1: Load Packages ----
## Needed Packages: tidyverse, ggplot2
library(tidyverse)
library(ggplot2)
# Step 2: Run Monte Carlo Simulation with coordinateGen Function ----
sim_data <- coordinateGen(10, 0, 20, 0, 0.16)
# Step 3: Flag point locations ----
sim_data <- sim_data |>
  mutate(
    curveFunc = dchisq(x, df = 5),
    Flag = ifelse(y <= curveFunc, "on/below", "above")
  )
# Step 4: Calculate numerical integration estimate ----
boxArea = 20 * 0.16
propBelow <- mean(sim_data$Flag == "on/below")
estIntegration <- boxArea * propBelow
# Step 5: Format text for display ----
estLabel <- paste("Est. Numerical Integration = ", round(estIntegration, 4))
# Step 6: Create Plot ----
plot10 <- ggplot(
    data = sim_data,
    mapping = aes(
     x = x,
      y = y,
   ) 
  ) +
  geom_point(
    mapping = aes(color = Flag)
  ) +
  stat_function(
   fun = dchisq,
   args = list(df = 5),
   xlim = c(0, 20)
   ) +
  scale_color_manual(
    values = c("#BC204B","#99CC00")
  ) +
  labs(
   title = paste("Monte Carlo Integration Example, n =", nrow(sim_data)),
    subtitle = estLabel,
   x = "x",
   y = "y",
   color = "Flag"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom"
  )