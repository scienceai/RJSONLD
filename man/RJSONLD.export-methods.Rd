\docType{methods}
\name{RJSONLD.export}
\alias{RJSONLD.export}
\alias{RJSONLD.export,ANY-method}
\alias{RJSONLD.export,aov-method}
\alias{RJSONLD.export,aovlist-method}
\alias{RJSONLD.export,glm-method}
\alias{RJSONLD.export,htest-method}
\alias{RJSONLD.export,lm-method}
\alias{RJSONLD.export,lme-method}
\alias{RJSONLD.export,lme4-method}
\title{Export results of standard statistical functions to semantic JSON-LD format}
\usage{
RJSONLD.export(object, path)

\S4method{RJSONLD.export}{ANY}(object, path)

\S4method{RJSONLD.export}{lm}(object, path)

\S4method{RJSONLD.export}{lme}(object, path)

\S4method{RJSONLD.export}{lme4}(object, path)

\S4method{RJSONLD.export}{glm}(object, path)

\S4method{RJSONLD.export}{aov}(object, path)

\S4method{RJSONLD.export}{aovlist}(object, path)

\S4method{RJSONLD.export}{htest}(object, path)
}
\arguments{
  \item{object}{object to be exported.}

  \item{path}{relative path where JSON-LD file is to be created.
  exported.}
}
\description{
Make your results of standard statistical analysis browsable and reproducible by exporting them into JSON-LD, following a standardized vocabulary (http://standardanalytics.io/stats). This vocabulary is still at a draft stage: provide feedback, suggestions and extenstions at https://github.com/standard-analytics/terms. This module currently supports the current functions: lm, lme, lmer, glm, aov, chis.test, t.test, cor.test, prop.test.
Make your results of standard statistical analysis browsable and reproducible by exporting them into JSON-LD, following a standardized vocabulary (http://standardanalytics.io/stats). This vocabulary is still at a draft stage: provide feedback, suggestions and extenstions at https://github.com/standard-analytics/RJSONLD.
}
\examples{
RJSONLD.export(lm(iris$Petal.Length~iris$Sepal.Length),"irisLM.jsonld")
RJSONLD.export(aov(iris$Petal.Length~iris$Species),"irisANOVA.jsonld")
RJSONLD.export(aov(iris$Petal.Length~iris$Species*iris$Sepal.Length),"irisANCOVA.jsonld")
}

