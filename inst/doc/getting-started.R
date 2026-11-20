## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

## ----setup--------------------------------------------------------------------
library(mosaicresid)
library(ggplot2)

## -----------------------------------------------------------------------------
ggplot(infert) +
  geom_mosaic_resid(
    aes(x = factor(pmin(spontaneous, 2), labels = c("Zero", "One", "2 or more")),
        y = factor(case, labels = c("Control", "Case")))) +
  theme_minimal() +
  labs(title = "Prior Spontaneous Abortions vs Infertility Case Status",
       x = "Spontaneous abortions", y = NULL)

## -----------------------------------------------------------------------------
ggplot(Titanic) +
  geom_mosaic_resid(aes(x = Class, y = Survived)) +
  labs(title = "Titanic Survival Rate by Class", x = "Class") +
  theme_minimal()

## -----------------------------------------------------------------------------
titanic_counts <- aggregate(Freq ~ Class + Survived, as.data.frame(Titanic), sum)
names(titanic_counts)[names(titanic_counts) == "Freq"] <- "passengers"

ggplot(titanic_counts) +
  geom_mosaic_resid(aes(x = Class, y = Survived, weight = passengers)) +
  labs(title = "Titanic Survival Rate by Class (pre-aggregated counts)", x = "Class") +
  theme_minimal()

## -----------------------------------------------------------------------------
cars <- data.frame(
  cylinders    = factor(mtcars$cyl,  labels = c("4 cylinders", "6 cylinders", "8 cylinders")),
  transmission = factor(mtcars$am,   labels = c("Automatic", "Manual")),
  gears        = factor(mtcars$gear, labels = c("3 gears", "4 gears", "5 gears"))
)

## -----------------------------------------------------------------------------
ggplot(cars) +
  geom_mosaic_resid(aes(x = cylinders, y = transmission)) +
  scale_fill_gradient2(low = "purple", mid = "white", high = "darkgreen") +
  theme_minimal() +
  labs(title = "Cylinders vs Transmission", x = NULL, y = NULL)

## -----------------------------------------------------------------------------
ggplot(cars) +
  geom_mosaic_resid(
    aes(x = cylinders, y = gears),
    spacing = 0.02,
    min_label_width = 0.04,
    min_label_height = 0.06,
    label.size = 2.5
  ) +
  theme_minimal() +
  labs(title = "Cylinders vs Gears", x = NULL, y = NULL)

## -----------------------------------------------------------------------------
ggplot(cars) +
  geom_mosaic_resid(
    aes(x = cylinders, y = transmission),
    dir = c("h", "v"),   # swap which variable splits vertically first
    type = "deviance"    # pearson (default), deviance, or FT (Freeman-Tukey)
  ) +
  theme_minimal() +
  labs(title = "Cylinders vs Transmission (deviance residuals, swapped splits)",
       x = NULL, y = NULL)

## -----------------------------------------------------------------------------
compute_mosaic_residuals(cars, "cylinders", "transmission")

