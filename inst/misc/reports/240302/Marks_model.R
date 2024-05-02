#########
# ADMIN #
#########
report_directoy <- "inst/misc/reports/231201/"


#########
# MODEL #
#########

# the model has two inputs, x1 and x2
# the model has two outputs, mortality and incremental net benefit (inb)
# we have mortality data that we can use to calibrate the model
# we are going to use the predicted inb to make our decision

model.function <- function(.x1, .x2) {
  .model.mortality <- .x1 + .x2
  .inb <- .x1 * 100
  return(list(model.mortality = .model.mortality, inb = .inb))
}

##########
# PRIORS #
##########

# Priors on model parameters, x1 and x2
# Bivariate Normal

m1 <- 0
m2 <- 190
mu <- c(m1, m2)

v1 <- 1000
v2 <- 100
rho <- 0
Sigma <- matrix(c(v1, rho * v1 * v2, rho * v1 * v2, v2), 2, 2)

set.seed(1)
n <- 1e6
x <- MASS::mvrnorm(n, mu, Sigma)
x1 <- x[, 1]
x2 <- x[, 2]

#########################################################
# GENERATE CALIBRATION TARGET FROM DUMMY MORTALITY DATA #
#########################################################

# generate dummy mortality data
d.mean <- 200
d.var <- 1000
d.sd <- sqrt(d.var)
d.sd
d.n <- 100
set.seed(10)
d <- rnorm(d.n, d.mean, d.sd)
d

# compute calibration target from dummy data
target.mortality.mean <- mean(d)
target.mortality.mean

target.mortality.sd <- sd(d)
target.mortality.sd

#####################################
# DEFINE LIKELIHOOD FOR CALIBRATION #
#####################################

# We assume a Normal likelihood

likelihood <- function(.parameter, .target.mean, .target.se) {
  dnorm(.target.mean, .parameter, .target.se)
}

#######################################
# RUN MODEL AT PRIOR VALUES OF INPUTS #
#######################################

prior.output <- model.function(x1, x2)
prior.inb <- prior.output$inb
prior.evpi <- mean(pmax(prior.inb, 0)) - max(mean(prior.inb), 0)
prior.evpi

########################
# CALCULATE LIKELIHOOD #
########################

likelihood.values <- likelihood(prior.output$model.mortality,
                                .target.mean = target.mortality.mean,
                                .target.se = target.mortality.sd / sqrt(d.n))

##############################
# GENERATE POSTERIOR SAMPLES #
##############################

# Generate posterior by resampling x1 and x2 with weights equal to the likelihood values

posterior.index <- sample(1:n, n, replace = TRUE, prob = likelihood.values)
posterior.x <- x[posterior.index, ]

posterior.output <- model.function(posterior.x[, 1], posterior.x[, 2])

posterior.inb <- posterior.output$inb

mean(posterior.inb)
sd(posterior.inb)
posterior.evpi <- mean(pmax(posterior.inb, 0)) - max(mean(posterior.inb), 0)
posterior.evpi

#########
# PLOTS #
#########

# Prior for inputs

bivariate.density.prior <- MASS::kde2d(x1, x2, n = 50)

dev.new()
persp(bivariate.density.prior,
      theta = 20, phi = 25, expand = 0.6, ticktype = 'detailed',
      xlab = "x1", ylab = "x2", zlab = "",
      main = "Prior density for inputs x1 and x2")

# Save the parameters' posterior distribution:
savePlot(filename = paste0(report_directoy, "1_x1_x2_prior"), type = "tiff")

# Posterior for inputs
# note the induced correlation

bivariate.density.posterior <- MASS::kde2d(posterior.x[, 1], posterior.x[, 2], n = 50)

dev.new()
persp(bivariate.density.posterior,
      theta = 20, phi = 25, expand = 0.6, ticktype = 'detailed',
      xlab = "x1", ylab = "x2", zlab = "",
      main = "Posterior density for inputs x1 and x2")

# Save the parameters' posterior distribution:
savePlot(filename = paste0(report_directoy, "1_x1_x2_post"), type = "tiff")

# Prior and posterior for INB output

dev.new()
den.prior <- density(prior.inb)
den.post <- density(posterior.inb)
plot(den.prior,
     xlim = c(min(den.prior$x, den.post$x), max(den.prior$x, den.post$x)),
     ylim = c(min(den.prior$y, den.post$y), max(den.prior$y, den.post$y)),
     main = "Prior and posterior distributions for INB",
     xlab = "INB"
)

lines(den.post, col = 2, lty = 2)
legend(x = "topleft", legend = c("Prior", "Posterior"), col = c(1, 2), lty = c(1, 2))

# Save the INB:
savePlot(filename = paste0(report_directoy, "1_pre_post_inmb"), type = "tiff")

#####

# SOME THINGS TO DO

# Try changing the size of the calibration dataset to see what happens
# work out how to compute the expected value of reducing the calibration target standard error to zero
# what is the expected value of sample information for a specific size of calibration dataset?




