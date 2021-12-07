library(JM)
library(lattice)

# lmeFit <- lme(
#   fixed = log(serBilir) ~ bs(year, 4, Boundary.knots = c(0, 15)),
#   random = list(
#     id = pdDiag(form=~bs(year,4, Boundary.knots = c(0,15)))
#   ),
#   data = pbc2
# )

# Nonlinear model
lmeFit <- lme(log(serBilir) ~ drug*(year + I(year^2)), random = ~ year + I(year^2)|id, data = pbc2)

# Linear model
#lmeFit <- lme(log(serBilir) ~ drug*(year), random = ~ year|id, data = pbc2)
coxFit <- coxph(Surv(years, status2) ~ drug + prothrombin, data = pbc2.id, x = TRUE)

jmFit <- jointModel(lmeFit, coxFit, timeVar = "year", method = "weibull-AFT-GH")

ND <- pbc2[pbc2$id == 2, ]
survPreds <- vector("list", nrow(ND))
for (i in 1:nrow(ND)) {
  set.seed(123)
  survPreds[[i]] <- survfitJM(jmFit, newdata = ND[1:i, ])
}

# plot of the dynamic survival probabilities
png("model_predictions.png")
par(mfrow = c(2, 2), oma = c(0, 2, 0, 2))
for (i in c(1,3,5,7)) {
  plot(survPreds[[i]], estimator = "median", conf.int = TRUE,
       include.y = TRUE, main = paste("Follow-up time:",
                                      round(survPreds[[i]]$last.time, 1)))
}
mtext("log serum bilirubin", side = 2, line = -1, outer = TRUE)
mtext("Survival Probability", side = 4, line = -1, outer = TRUE)
dev.off()

