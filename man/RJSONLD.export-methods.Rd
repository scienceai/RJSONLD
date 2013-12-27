\docType{methods}
\name{RJSONLD.export}
\alias{RJSONLD.export}
\alias{RJSONLD.export,ANY-method}
\alias{RJSONLD.export,aov-method}
\alias{RJSONLD.export,aovlist-method}
\alias{RJSONLD.export,glm-method}
\alias{RJSONLD.export,htest-method}
\alias{RJSONLD.export,lm-method}
\title{Export results of standard statistical functions to semantic JSON-LD format}
\usage{
RJSONLD.export(object, path)

\S4method{RJSONLD.export}{ANY}(object, path)

\S4method{RJSONLD.export}{lm}(object, path)

\S4method{RJSONLD.export}{glm}(object, path)

\S4method{RJSONLD.export}{aov}(object, path)

\S4method{RJSONLD.export}{aovlist}(object, path)

\S4method{RJSONLD.export}{htest}(object, path)
}
\arguments{
  \item{object}{object to be exported.}

  \item{path}{relative where JSON-LD file is to be created
  exported.}
}
\description{
Simply reformats standard statistical function outputs into
JSON-LD format, abiding with statistical vocabulary
described in https://github.com/standard-analytics/Schemas
}
\examples{
RJSONLD.export(lm(iris$Petal.Length~iris$Sepal.Length),"irisLM.json")
RJSONLD.export(aov(iris$Petal.Length~iris$Species),"irisANOVA.json")
RJSONLD.export(aov(iris$Petal.Length~iris$Species*iris$Sepal.Length),"irisANCOVA.json")
}

