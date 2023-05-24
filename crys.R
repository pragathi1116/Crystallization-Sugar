install.packages("FrF2")
library(FrF2)
library(ggplot2)
factors <- c("Temperature", "StirringRate", "ImpurityLevel", "Concentration")
levels <- list(
  Temperature = c("Low", "Medium", "High"),
  StirringRate = c( 1, 2),
  ImpurityLevel = c(1, 2, 3, 4),
  Concentration = c("Low", "Medium", "High")
)
design <- expand.grid(levels)
for (factor in factors) {
  design[, factor] <- as.numeric(factor(design[, factor], levels = levels[[factor]]))
}
design$Temperature <- rep(c(25, 35, 45), length.out = nrow(design))
design$StirringRate <- rep(c(1, 2), length.out = nrow(design))
design$ImpurityLevel <- rep(c(1, 2, 3, 4), length.out = nrow(design))
design$Concentration <- rep(c(5, 10, 15), length.out = nrow(design))
design$CrystalSize <- NA
design$GrowthRate <- NA
measure_crystal_size <- function(temperature, stirringRate, impurityLevel, concentration) {
  crystalSize <- 10 + temperature * 0.5 + stirringRate * 2 - impurityLevel * 1.5 + concentration * 0.8
  return(crystalSize)
}
measure_growth_rate <- function(temperature, stirringRate, impurityLevel, concentration) {
  growthRate <- 5 + temperature * 0.3 + stirringRate * 1.5 - impurityLevel * 1 + concentration * 0.6
  return(growthRate)
}
  
for (i in 1:nrow(design)) {
  temperature <- design$Temperature[i]
  stirringRate <- design$StirringRate[i]
  impurityLevel <- design$ImpurityLevel[i]
  concentration <- design$Concentration[i]
  
  crystalSize <- measure_crystal_size(temperature, stirringRate, impurityLevel, concentration)
  growthRate <- measure_growth_rate(temperature, stirringRate, impurityLevel, concentration)
  
  design$CrystalSize[i] <- crystalSize
  design$GrowthRate[i] <- growthRate
}
summary(design$CrystalSize)
summary(design$GrowthRate)
design$Temperature <- as.factor(design$Temperature)
ggplot(design, aes(x = Temperature, y = CrystalSize, group = Temperature)) +
  geom_boxplot()
ggplot(design, aes(x = StirringRate, y = GrowthRate, group = StirringRate )) +
  geom_boxplot()
anova_crystal <- aov(CrystalSize ~ Temperature + StirringRate + ImpurityLevel + Concentration, data = design)
summary(anova_crystal)
anova_growth <- aov(GrowthRate ~ Temperature + StirringRate + ImpurityLevel + Concentration, data = design)
summary(anova_growth)
ggplot(design, aes(x = Temperature, y = CrystalSize, color = StirringRate)) +
  geom_point() +
  facet_grid(ImpurityLevel ~ Concentration)
ggplot(design, aes(x = Temperature, y = GrowthRate, fill = Concentration)) +
  geom_bar(stat = "identity") +
  facet_wrap(~StirringRate)
ggplot(design, aes(x = Temperature, y = CrystalSize, fill = factor(Concentration))) +
  geom_boxplot() +
  facet_wrap(~ StirringRate) +
  xlab("Temperature") +
  ylab("Crystal Size") +
  ggtitle("Sugar Crystallization: Crystal Size by Factors") +
  theme_minimal()