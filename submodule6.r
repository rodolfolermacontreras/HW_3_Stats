# See GLM chapter 9.

# install.packages(c("GLMsData", "statmod"))
library(GLMsData)
library(statmod)

#
# Logistic regression
#
data(turbines)

# Note that there are multiple equivalent ways of expressing the same model
# in code.
# See GLM chapter 9.2 for more information.
# Here, we scale the predictor to 1000s of hours of operations for convenience
# when interpreting the model parameters.
# Note: logit is the default link for the Binomial family.
turbines_model <- glm(
    Fissures / Turbines ~ I(Hours / 1000),
    weights = Turbines, family = "binomial",
    data = turbines
)
turbines_model_summary <- summary(turbines_model)
turbines_model_summary

# Coefficients.
turbines_model$coefficient # Additive effect on log-odds.
exp(turbines_model$coefficient) # Multiplicative effect on odds.

# Deviance goodness of fit test.
pchisq(
    turbines_model_summary$deviance,
    turbines_model_summary$df.residual,
    lower.tail = FALSE
)

# Model diagnostics.
# Standardized quantile residuals vs. variance-stabilized fitted values.
# png(
#     "classes/img/class6-turbines-logistic-diagnostics.png",
#     width = 800, height = 600
# )
par(mfcol = c(2, 2))
turbines_model_q_resid <- qresid(turbines_model)
turbines_model_q_resid_std <- (
    turbines_model_q_resid / sqrt(1 - hatvalues(turbines_model))
)
scatter.smooth(
    turbines_model_q_resid_std ~ asin(sqrt(fitted(turbines_model))),
    col = "grey",
    las = 1,
    ylab = "Standardized quantile residuals",
    xlab = "Variance-stabilized fitted values",
    cex.lab = 1.5
)
abline(a = 0, b = 0, lty = "dashed")

# Standardized quantile residuals vs. the predictor (Hours).
scatter.smooth(
    turbines_model_q_resid_std ~ turbines$Hours,
    col = "grey",
    las = 1,
    ylab = "Standardized quantile residuals",
    xlab = "Hours",
    cex.lab = 1.5
)
abline(a = 0, b = 0, lty = "dashed")

# Linear predictor part of the model vs. working responses.
turbines_model_work_resp <- (
    resid(turbines_model, type = "working") + turbines_model$linear.predictor
)
scatter.smooth(
    turbines_model$linear.predictor ~ turbines_model_work_resp,
    col = "grey",
    las = 1,
    ylab = "Linear predictor",
    xlab = "Working responses",
    cex.lab = 1.5
)
abline(0, 1, lty = "dashed")

# Quantile-quantile plot.
qqnorm(turbines_model_q_resid, las = 1, pch = 19, cex.lab = 1.5)
qqline(turbines_model_q_resid, lty = "dashed")
# dev.off()

# Infuential observations and outliers.
turbines_model_im <- influence.measures(turbines_model)
summary(turbines_model_im)

#
# Poisson regression
#
data(danishlc)

lc_model <- glm(
    Cases ~ offset(log(Pop)) + City + Age,
    family = "poisson",
    data = danishlc
)
lc_model_summary <- summary(lc_model)
lc_model_summary

# Coefficients.
lc_model$coefficient
exp(lc_model$coefficient)

# Deviance goodness of fit test.
pchisq(
    lc_model_summary$deviance,
    lc_model_summary$df.residual,
    lower.tail = FALSE
)

# Model diagnostics.
# Standardized quantile residuals vs. variance-stabilized fitted values.
# png(
#     "classes/img/class6-danishlc-poisson-diagnostics.png",
#     width = 800, height = 600
# )
par(mfrow = c(1, 3))
lc_model_q_resid <- qresid(lc_model)
lc_model_q_resid_std <- (
    lc_model_q_resid / sqrt(1 - hatvalues(lc_model))
)
scatter.smooth(
    lc_model_q_resid_std ~ sqrt(fitted(lc_model)),
    col = "grey",
    las = 1,
    ylab = "Standardized quantile residuals",
    xlab = "Variance-stabilized fitted values",
    cex.lab = 1.5
)
abline(a = 0, b = 0, lty = "dashed")

# Linear predictor part of the model vs. working responses.
lc_model_work_resp <- (
    resid(lc_model, type = "working") + lc_model$linear.predictor
)
scatter.smooth(
    lc_model$linear.predictor ~ lc_model_work_resp,
    col = "grey",
    las = 1,
    ylab = "Linear predictor",
    xlab = "Working responses",
    cex.lab = 1.5
)
abline(0, 1, lty = "dashed")

# Quantile-quantile plot.
qqnorm(lc_model_q_resid, las = 1, pch = 19, cex.lab = 1.5)
qqline(lc_model_q_resid, lty = "dashed")
# dev.off()

# Infuential observations and outliers.
lc_model_im <- influence.measures(lc_model)
summary(lc_model_im)
