predSoyPricePerYear <- function(Exports,TotalSupply,Imports,Residual, lev = .95, int = 'c') {
  new <- data.frame(x1prime = 1/Exports, x2prime = sqrt(TotalSupply), x3prime = log(Imports + 1)^2, x4prime = log(Residual))
  predict(reg11, new, se.fit = T, interval = int, level = lev)
}

predSoyPricePerYear(Exports = 1875, TotalSupply = 5001,Imports =  20, Residual = 127, int = 'c')

SoybeanSupplyDisappearance <- read_excel("~/SoybeanSupplyDisappearance.xlsx")
View(SoybeanSupplyDisappearance)

#y(soybean price)
#x2(Exports)
#x3(Total Supply)
#x4(Imports)
#x5(Residual)

x1 <- SoybeanSupplyDisappearance$Exports
x2 <- SoybeanSupplyDisappearance$TotalSupply
x3 <- SoybeanSupplyDisappearance$Imports
x4 <- SoybeanSupplyDisappearance$Residual

x1prime <- 1/x1
x2prime <- sqrt(x2)
x3prime <- log(x3+1)^2
x4prime <- log(x4)

reg11 <- lm(y ~ x1prime + x2prime + x3prime + x4prime)


