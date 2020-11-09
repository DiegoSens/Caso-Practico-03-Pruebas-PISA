library(tidyverse)
library(broom) # modelos en df
library(flextable) # Tablas formateadas
library(mgcv) # estimar gam
library(reshape2) # melt
library(readr)
library(ggplot2)
library(imputeTS)

pisa <- read.csv("pisasci2006.csv", sep = ";")
View(pisa)

#Sustitución de NAs por valores medios
pisa <- na_mean(pisa)

#Ver si hay duplicados
duplicated(pisa)
nrow(pisa[duplicated(pisa$Country),])


# Examine the pisa data frame
head(pisa)
par(mfrow=c(2,3))
ggplot(pisa, aes(Issues, Overall)) +
  geom_point(color = "purple")

ggplot(pisa, aes(Explain, Overall)) +
  geom_point(color = "purple")

ggplot(pisa, aes(Evidence, Overall)) +
  geom_point(color = "purple")

ggplot(pisa, aes(Interest, Overall)) +
  geom_point(color = "purple")

ggplot(pisa, aes(Support, Overall)) +
  geom_point(color = "purple")

ggplot(pisa, aes(Income, Overall)) +
  geom_point(color = "purple")

ggplot(pisa, aes(Health, Overall)) +
  geom_point(color = "purple")

ggplot(pisa, aes(Edu, Overall)) +
  geom_point(color = "purple")

ggplot(pisa, aes(HDI, Overall)) +
  geom_point(color = "purple")
par(mfrow=c(1,1))

summary(pisa)

# Fit a linear model
lm_mod <- lm(Overall~Income, data = pisa)

width(flextable(tidy(lm_mod)), width = 1.5)

width(flextable(glance(lm_mod)), width = 1.5)

# Visualize the model
termplot(lm_mod, partial.resid = TRUE, se = TRUE)

baseplot1 <- ggplot(data = pisa, mapping = aes(x = Overall, y = Income)) +
  layer(geom = "point",stat = "identity",position = "identity") +
  theme_bw() + theme(legend.key = element_blank())
baseplot1

knots <- c(0,50,100,150)
pisa$X1 <- pmax(0, pisa$Interest - knots[1])
pisa$X2 <- pmax(0, pisa$Support - knots[2])
pisa$X3 <- pmax(0, pisa$Evidence - knots[3])
pisa$X4 <- pmax(0, pisa$Explain - knots[4])
pisa

#Linear splines
lsp <- lm(Overall ~ Income + X1 + X2 + X3 + X4, data = pisa)
summary(lsp)

newdat <- data.frame(Income = seq(0,60,0.01))
newdat$X1 <- pmax(0, newdat$Income - knots[1])
newdat$X2 <- pmax(0, newdat$Income - knots[2])
newdat$X3 <- pmax(0, newdat$Income - knots[3])
newdat$X4 <- pmax(0, newdat$Income - knots[4])
newdat$linear <- predict(lsp, newdata = newdat)

#Quadratic splines
qsp <- lm(Overall ~ Income + I(Income^2) + I(X1^2) + I(X2^2) + I(X3^2) + I(X4^2), data = pisa)
summary(qsp)
newdat$quadratic <- predict(qsp, newdata = newdat)

#Cubic splines
csp <- lm(Overall ~ Income + I(Income^2) + I(Income^3) + I(X1^3) + I(X2^3) + I(X3^3) + I(X4^3), data = pisa)
summary(csp)
newdat$cubic <- predict(csp, newdata = newdat)

#Plot splines
newdatMelt <- melt(data          = newdat,
                   id.vars       = c("Income",paste0("X",1:4)),
                   variable.name = "spline",
                   value.name    = "value")

baseplot1 +
  layer(geom = "line", data = newdatMelt,stat = "identity", position = "identity",
        mapping = aes(x = Income, y = value, color = spline)) +
  facet_wrap( ~ spline, ncol = 1)


# Fit the model
gam_mod <- gam(Overall ~ s(Income) + s(Support) + s(Edu) + s(Health),   data = pisa)
summary(gam_mod)

# Plot the results
plot(gam_mod, residuals = TRUE, pch = 1)

coef(gam_mod)



# Modelo GAM
gam_mod2 <- gam(Overall ~ s(Income)+ s(Edu) + Support + Health + HDI,   data = pisa)
summary(gam_mod2)

# Representación gráfica

plot(gam_mod2, residuals = TRUE, pch = 1)

coef(gam_mod2)

anova(gam_mod, gam_mod2, test='F')


# Estimación de grados de libertad por Cross-Validation
fit1 <- smooth.spline(pisa$Interest, pisa$Overall, cv=TRUE)
fit1

fit2 <- smooth.spline(pisa$Support, pisa$Overall, cv=TRUE)
fit2

fit3 <- smooth.spline(pisa$Income, pisa$Overall, cv=TRUE)
fit3

fit4 <- smooth.spline(pisa$Health, pisa$Overall, cv=TRUE)
fit4

fit5 <- smooth.spline(pisa$Edu, pisa$Overall, cv=TRUE)
fit5

fit6 <- smooth.spline(pisa$HDI, pisa$Overall, cv=TRUE)
fit6

# Complexity
# Fit a GAM with 3 basis functions
gam_mod_k3 <- gam(Overall ~ s(Income, k = 3) + s(Edu, k = 3), data = pisa)

# Fit with 20 basis functions
gam_mod_k20 <- gam(Overall ~ s(Income, k = 20)+ s(Edu, k = 20), data = pisa)

# Visualize the GAMs
plot(gam_mod_k3, residuals = TRUE, pch = 1)
plot(gam_mod_k20, residuals = TRUE, pch = 1)


# Fix the smoothing parameter at 0.1
gam_mod_s1 <- gam(Overall ~ s(Income), data = pisa, sp = 0.01)

# Fix the smoothing parameter at 0.0001
gam_mod_s2 <- gam(Overall ~ s(Income), data = pisa, sp = 0.0001)

# Plot both models
plot(gam_mod_s1, residuals = TRUE, pch = 1)
plot(gam_mod_s2, residuals = TRUE, pch = 1)


gam_mod_sk <- gam(Overall ~ s(Income, k = 50), data = pisa, sp = 0.0001)

# Visualize the model
plot(gam_mod_sk, residuals = TRUE, pch = 1)


# Checking
gam.check(gam_mod_sk)
gam.check(gam_mod_s1)
gam.check(gam_mod_s2)
gam.check(gam_mod_k3)
gam.check(gam_mod_k20)
