% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mStats.R
\name{regress}
\alias{regress}
\alias{predict.regress}
\alias{plot.regress}
\alias{ladder}
\alias{hettest}
\alias{linkTest}
\title{Linear Regression Model}
\usage{
regress(model, vce = FALSE, digits = 5)

\method{predict}{regress}(object, ...)

\method{plot}{regress}(x, ...)

ladder(data, var)

hettest(regress, studentize = FALSE)

linkTest(model, vce = FALSE, digits = 5)
}
\arguments{
\item{model}{glm or lm model}

\item{vce}{if \code{TRUE}, robust standard errors are calculated.}

\item{digits}{specify rounding of numbers. See \code{\link{round}}.}

\item{object}{a model object for which prediction is desired.}

\item{...}{additional arguments affecting the predictions produced.}

\item{x}{the coordinates of points in the plot. Alternatively, a
      single plotting structure, function or \emph{any \R object with a
        \code{plot} method} can be provided.}

\item{data}{dataset}

\item{var}{variable name}

\item{regress}{output from \code{regress}}

\item{studentize}{logical.
If set to \code{TRUE} Koenker's studentized version
of the test statistic will be used.}
}
\value{
a list containing
\enumerate{
\item \code{info} - info and error tables
\item \code{reg} - regression table
\item \code{model} - raw model output from \code{lm()}
\item \code{fit} - formula for fitting the model
\item \code{lbl} - variable labels for further processing in \code{summary}.
}
}
\description{
\code{regress()} produces summary of the model
with coefficients and 95\% Confident Intervals.

\code{`predict.regress`} a S3 method for \code{predict} to generate
statistics related to the prediction of the linear model using the output
from the \code{regress} function of the \code{mStats}.

\code{`plot.regress`} is a S3 method for \code{plot()} to create
graphs for checking diagnostics of linear model using the output from
the \code{regress} function of the \code{mStats}.

\code{`ladder`} converts a variable into a normally
distributed one.

\code{`hettest`} performs the Breusch-Pagan test
for heteroskedasticity.
It presents evidence against the
null hypothesis that t=0 in Var(e)=sigma^2 exp(zt).
The formula are based on the \code{bptest} function
in \code{lmtest} package.

\code{`linkTest`} determines whether a model in R is
'well specified' using the \code{STATA}'s \code{linkTest}.
}
\details{
\code{regress} is based on \code{\link{lm}}. All statistics presented
in the function's output are derivatives of \code{\link{lm}},
except AIC value which is obtained from \code{\link{AIC}}.
It uses \code{lm()} function to run the model.

\strong{Outputs}

Outputs can be divided into three parts.
\enumerate{
\item \verb{Info of the model}:
Here provides number of observations (Obs.), F value, p-value
from F test,
R Squared value, Adjusted R Squared value, square root of mean square
error
(Root MSE) and AIC value.
\item \code{Errors}:
Outputs from \code{anova(model)} is tabulated here. SS, DF and MS indicate
sum of square of errors, degree of freedom and mean of square of errors.
\item \verb{Regression Output}:
Coefficients from summary of model are tabulated here along with 95\\%
confidence interval.
}

\strong{using Robust Standard Errors}

if heteroskedasticity is present in our data sample,
the ordinary least square (OLS) estimator will remain unbiased
and consistent,
but not efficient. The estimated OLS standard errors
will be biased and cannot be solved with a larger sample size.
To remedy this, robust standard errors can be used to adjusted
standard errors.

The \code{regress} uses sandwich estimator to estimate Huber-White's standard
errors. The calculation is based on the tutorial by Kevin Goulding.

\deqn{Variance of Robust = (N / N - K) (X'X)^(-1)
 \sum{Xi X'i ei^2} (X'X)^(-1)}

where N =  number of observations, and K =  the number of regressors
(including the intercept). This returns a Variance-covariance (VCV)
matrix
where the diagonal elements are the estimated heteroskedasticity-robust
coefficient
variances — the ones of interest. Estimated coefficient standard errors
are the square root of these diagonal elements.

\code{`predict.regress`} generates an original data with statistics for model
diagnostics:
\enumerate{
\item \code{fitted} (Fitted values)
\item \code{resid} (Residuals)
\item \code{std.resid} (Studentized Residuals)
\item \code{hat} (leverage)
\item \code{sigma}
\item \code{cooksd} (Cook's Distance)
}

The \verb{Breusch-Pagan test} fits a linear regression model
to the residuals of a linear regression model
(by default the same explanatory variables are taken as
in the main regression model) and rejects if too
much of the variance is explained by the additional
explanatory variables. Under \eqn{H_0} the test statistic
of the Breusch-Pagan test follows a chi-squared distribution
with \code{parameter} (the number of regressors without
the constant in the model) degrees of freedom.

The code for \code{`linkTest`} has been modified from Keith Chamberlain's linktext.
www.ChamberlainStatistics.com
https://gist.github.com/KeithChamberlain/8d9da515e73a27393effa3c9fe571c3f
}
\note{
Credits to Kevin Goulding, The Tarzan Blog.
}
\examples{

fit <- lm(Ozone ~ Wind, data = airquality)
regress(fit)

\dontrun{
## labelling variables
airquality2 <- label(airquality, Ozone = "Ozone level", Wind = "Wind Speed")
fit2 <- lm(Ozone ~ Wind, data = airquality2)
reg <- regress(fit2)
str(reg)
}


\dontrun{
predict(reg)
}


\dontrun{
plot(reg)
}


ladder(airquality, Ozone)


\dontrun{
hettest(reg)
}


\dontrun{
linkTest(fit)
}

}
\references{
T.S. Breusch & A.R. Pagan (1979),
A Simple Test for Heteroscedasticity and Random
Coefficient Variation.
\emph{Econometrica} \bold{47}, 1287--1294

R. Koenker (1981), A Note on Studentizing a Test for
Heteroscedasticity. \emph{Journal of Econometrics}
\bold{17}, 107--112.

W. Krämer & H. Sonnberger (1986),
\emph{The Linear Regression Model under Test}.
Heidelberg: Physics
}
\author{
Email: \email{dr.myominnoo@gmail.com}

Website: \url{https://myominnoo.github.io/}
}
