#################################################################################
#' Export results of standard statistical functions to semantic JSON-LD format
#'
#' Make your results of standard statistical analysis browsable and reproducible
#' by exporting them into JSON-LD, following a standardized vocabulary
#' (http://standardanalytics.io/stats). This vocabulary is still at a draft stage:
#' provide feedback, suggestions and extenstions at https://github.com/standard-analytics/terms
#' This module currently supports the current functions: lm, lme, lmer, glm, aov,
#' chisq.test, t.test, cor.test, prop.test, pairwise.t.test, TukeyHSD, glm.nb,
#' stan, anova, lrtest.
#'
#' @param object object to be exported.
#'
#' @param path relative path where JSON-LD file is to be created.
#' exported.
#'
#' @return NULL
#'
#'
#' @export
#' @docType methods
#' @rdname RJSONLD.export-methods
#'
#' @examples
#' RJSONLD.export(lm(iris$Petal.Length~iris$Sepal.Length),"irisLM.jsonld")
#' RJSONLD.export(aov(iris$Petal.Length~iris$Species),"irisANOVA.jsonld")
#' RJSONLD.export(aov(iris$Petal.Length~iris$Species*iris$Sepal.Length),"irisANCOVA.jsonld")
setGeneric("RJSONLD.export", function(object, path){
  standardGeneric("RJSONLD.export")
})

#' @rdname RJSONLD.export-methods
setMethod("RJSONLD.export", "ANY", function(object, path){
  print('unrecognized format')
})

push <- function(l, x) {
  assign(l, append(eval(as.name(l)), x), envir=parent.frame())
}

#' @rdname RJSONLD.export-methods
setMethod("RJSONLD.export", "lm", function(object, path){
  summary <- summary(object)
  coef <- coef(summary)
  res <- list( `@context` = list( `@vocab` = 'http://standardanalytics.io/stats/'),
               `@type` = 'LinearModel',
               modelFormula = deparse(object$call$formula, width.cutoff = 500L),
               modelVariable = list()
  )
  indfactor = 0
  for(i in 1:length(rownames(attr(object$terms,'factors')))){
    res$modelVariable[[i]] <- list(
      `@type` = 'Variable',
      name=rownames(attr(object$terms,'factors'))[i]
    )
    if(attr(object$terms,"dataClasses")[[i]]=="factor"){
      indfactor = indfactor + 1
      res$modelVariable[[i]]$isCategorical = 'true'
      res$modelVariable[[i]]$levels = object$xlevels[[indfactor]]
      res$modelVariable[[i]]$factorConstrast = list()
      if(object$contrasts[[indfactor]]=="contr.treatment"){
        n = length(object$xlevels[[indfactor]])
        res$modelVariable[[i]]$factorConstrast[[1]] <- list(
          name = paste(rownames(attr(object$terms,'factors'))[i],'-Intercept',sep=""),
          value = c(1,rep(0,n-1))
          )
        for(j in 2:n){
          res$modelVariable[[i]]$factorConstrast[[j]] <- list(
            name = paste(rownames(attr(object$terms,'factors'))[i],j-1,sep=""),
            value = c(-1,rep(0,j-2),1,rep(0,n-j))
          )
        }
      }
    }
  }
  res$modelFit <- list(
    `@type` = 'FitnessOptimization',
    fitnessCriterion = 'least squares',
    r2 = summary$r.squared,
    adjr2 = summary$adj.r.squared,
    nData = length(object$fitted.values),
    nParameters = length(object$coefficients),
    fTest = list(
      `@type`= 'FTest',
      testStatistic = summary$fstatistic[[1]],
      dfNum = summary$fstatistic[[2]],
      dfDenom = summary$fstatistic[[3]],
      pValue = pf(summary$fstatistic[[1]], summary$fstatistic[[2]], summary$fstatistic[[3]], lower.tail=FALSE)
    ),
    optimalParameter = list()
  )
  terms = names(summary$aliased)
  for(i in 1:nrow(coef)){
    res$modelFit$optimalParameter[[i]]<-list(
      `@type`= 'Parameter',
      name = terms[i],
      estimate = list(
        `@type`= 'Statistic',
        value = coef[i,][[1]],
        standardError = coef[i,][[2]],
        statisticalTest = list(
          `@type`= 'TTest',
          testStatistic = coef[i,][[3]],
          df = summary$df[[2]],
          pValue = coef[i,][[4]]
          )
        )
    )
  }
  indRow = 1
  indFactor = 1
  nFactors = length(object$xlevels)

  # all intercepts
  contrastlist = c()
  for(k in 1:length(res$modelVariable)){
    if(!is.na(match("isCategorical",names(res$modelVariable[[k]])))){
      contrastlist = c(contrastlist,res$modelVariable[[k]]$factorConstrast[[1]]$name)
    }
  }
  res$modelFit$optimalParameter[[indRow]]$contrast <- contrastlist
  indRow = indRow + 1

  # first order terms and 2nd order with only one categorical factor
  for(i in 1:length(colnames(attr(object$terms,"factors")))){
    if(sum(attr(object$terms,"factors")[,i])==1){
      ind = match(1,attr(object$terms,"factors")[,i])
      if(attr(object$terms,"dataClasses")[[ind]]=="factor"){
        for(j in 2:length(object$xlevels[[indFactor]])){
          # fill with intercepts
          contrastlist = c()
          for(k in 1:length(res$modelVariable)){
            if(!is.na(match("isCategorical",names(res$modelVariable[[k]])))){
              contrastlist = c(contrastlist,res$modelVariable[[k]]$factorConstrast[[1]]$name)
            }
          }
          contrastlist[indFactor]<-res$modelVariable[[i+1]]$factorConstrast[[j]]$name
          res$modelFit$optimalParameter[[indRow]]$contrast <- contrastlist
          indRow = indRow + 1
        }
        indFactor = indFactor + 1
      } else {
        indRow = indRow + 1
      }
    }
    if(sum(attr(object$terms,"factors")[,i])==2){
      ind1 = which(attr(object$terms,"factors")[,i] %in% 1)[1]
      ind2 = which(attr(object$terms,"factors")[,i] %in% 1)[2]
      test = FALSE
      if((attr(object$terms,"dataClasses")[[ind1]]=="factor")&(attr(object$terms,"dataClasses")[[ind2]]!="factor")){
        ind = ind1
        test = TRUE
      }
      if((attr(object$terms,"dataClasses")[[ind2]]=="factor")&(attr(object$terms,"dataClasses")[[ind1]]!="factor")){
        ind = ind2
        test = TRUE
      }
      if(test){
        for(j in 2:length(object$xlevels[[indFactor]])){
          # fill with intercepts
          contrastlist = c()
          for(k in 1:length(res$modelVariable)){
            if(!is.na(match("isCategorical",names(res$modelVariable[[k]])))){
              contrastlist = c(contrastlist,res$modelVariable[[k]]$factorConstrast[[1]]$name)
            }
          }
          contrastlist[indFactor]<-res$modelVariable[[i+1]]$factorConstrast[[j]]$name
          res$modelFit$optimalParameter[[indRow]]$contrast <- contrastlist
          indRow = indRow + 1
        }
        indFactor = indFactor + 1
      }
    }
  }

  # second order terms
  for(i in 1:length(colnames(attr(object$terms,"factors")))){
    if(sum(attr(object$terms,"factors")[,i])==2){
      ind1 = which(attr(object$terms,"factors")[,i] %in% 1)[1]
      ind2 = which(attr(object$terms,"factors")[,i] %in% 1)[2]
      test = FALSE
      if((attr(object$terms,"dataClasses")[[ind1]]=="factor")&(attr(object$terms,"dataClasses")[[ind2]]=="factor")){
        test = TRUE
      }
      if(test){
        indFactor1 = match(rownames(attr(object$terms,"factors"))[ind1],names(object$xlevels))
        indFactor2 = match(rownames(attr(object$terms,"factors"))[ind2],names(object$xlevels))
        for(j1 in 2:length(object$xlevels[[indFactor1]])){
          for(j2 in 2:length(object$xlevels[[indFactor2]])){
            # fill with intercepts
            contrastlist = c()
            for(k in 1:length(res$modelVariable)){
              if(!is.na(match("isCategorical",names(res$modelVariable[[k]])))){
                contrastlist = c(contrastlist,res$modelVariable[[k]]$factorConstrast[[1]]$name)
              }
            }
            contrastlist[indFactor1]<-res$modelVariable[[ind1]]$factorConstrast[[j1]]$name
            contrastlist[indFactor2]<-res$modelVariable[[ind2]]$factorConstrast[[j2]]$name
            res$modelFit$optimalParameter[[indRow]]$contrast <- contrastlist
            indRow = indRow + 1
          }
        }
        indFactor = indFactor + 1
      } else {
        indRow = indRow + 1
      }
    }
  }

  res$fitResidual <- list(
    list(
      `@type`= c('Statistic', 'Min', 'Quantile'),
      name = 'Min',
      value = quantile(summary$residuals)[[1]],
      percentile=0
    ),
    list(
      `@type`= c('Statistic', 'Quantile'),
      name = '1Q',
      value = quantile(summary$residuals)[[2]],
      percentile=25
    ),
    list(
      `@type`= c('Statistic', 'Median', 'Quantile'),
      name = 'Median',
      value = quantile(summary$residuals)[[3]],
      percentile=50
    ),
    list(
      `@type`= c('Statistic', 'Quantile'),
      name = '3Q',
      value = quantile(summary$residuals)[[4]],
      percentile=75
    ),
    list(
      `@type`= c('Statistic', 'Max', 'Quantile'),
      name = 'Max',
      value = quantile(summary$residuals)[[5]],
      percentile=100
    )
  )

  cat(gsub('"true"','true',gsub("\t","  ",toJSON(res,pretty=1))),file=path)
})



#' @rdname RJSONLD.export-methods
setMethod("RJSONLD.export", "lme", function(object, path){
  summary <- summary(object)
  res <- list( `@context` = list( `@vocab` = 'http://standardanalytics.io/stats/'),
               `@type` = 'LinearModel',
               modelFormula = paste(deparse(object$call$fixed), '+', substr(deparse(object$call$random), 2, nchar(deparse(object$call$random)))) ,
               loglik = summary$loglik,
               AIC = summary$AIC,
               BIC = summary$BIC,
               modelCoefficients = list()
  )
  terms = names(summary$fixDF$terms)
  for(i in 1:length(terms)){
    res$modelCoefficients[[i]]<-list(
      name = terms[i],
      estimate = summary$tTable[i,][[1]],
      stdError = summary$tTable[i,][[2]],
      statisticalTest = list(
        `@type` = 'TTest',
        testStatistic = summary$tTable[i,][[4]],
        df = summary$tTable[i,][[3]],
        pValue = summary$tTable[i,][[5]]
      )
    )
  }
  cat(gsub("\t","  ",toJSON(res,pretty=1)),file=path)
})



#' @rdname RJSONLD.export-methods
setMethod("RJSONLD.export", "lme4", function(object, path){
  summary <- summary(object)
  coef <- coef(summary)
  res <- list( `@context` = list( `@vocab` = 'http://standardanalytics.io/stats/'),
               `@type` = 'LinearModel',
               modelFormula = deparse(summary$call$formula) ,
               loglik = summary$loglik[1],
               AIC = summary$AICtab[1],
               modelCoefficients = list()
  )
  terms = rownames(summary$coefficients)
  for(i in 1:length(terms)){
    res$modelCoefficients[[i]]<-list(
      name = terms[i],
      estimate = summary$coefficients[i,][[1]],
      stdError = summary$coefficients[i,][[2]],
      statisticalTest = list(
        `@type` = 'TTest',
        testStatistic = summary$coefficients[i,][[3]]
      )
    )
  }
  cat(gsub("\t","  ",toJSON(res,pretty=1)),file=path)
})



#' @rdname RJSONLD.export-methods
setMethod("RJSONLD.export", c("glm"), function(object, path){
  summary <- summary(object)
  coef <- coef(summary)
  res <- list( `@context` = list( `@vocab` = 'http://standardanalytics.io/stats/'),
               `@type` = 'LinearModel',
               modelFormula = deparse(object$call$formula, width.cutoff = 500L),
               family = object$family$family,
               link = object$family$link,
               modelVariable = list()
  )
  indfactor = 0
  for(i in 1:length(rownames(attr(object$terms,'factors')))){
    res$modelVariable[[i]] <- list(
      `@type` = 'Variable',
      name=rownames(attr(object$terms,'factors'))[i]
    )
    if(attr(object$terms,"dataClasses")[[i]]=="factor"){
      indfactor = indfactor + 1
      res$modelVariable[[i]]$isCategorical = 'true'
      res$modelVariable[[i]]$levels = object$xlevels[[indfactor]]
      res$modelVariable[[i]]$factorConstrast = list()
      if(object$contrasts[[indfactor]]=="contr.treatment"){
        n = length(object$xlevels[[indfactor]])
        res$modelVariable[[i]]$factorConstrast[[1]] <- list(
          name = paste(rownames(attr(object$terms,'factors'))[i],'-Intercept',sep=""),
          value = c(1,rep(0,n-1))
        )
        for(j in 2:n){
          res$modelVariable[[i]]$factorConstrast[[j]] <- list(
            name = paste(rownames(attr(object$terms,'factors'))[i],j-1,sep=""),
            value = c(-1,rep(0,j-2),1,rep(0,n-j))
          )
        }
      }
    }
  }

  res$modelFit <- list(
    `@type` = 'FitnessOptimization',
    fitnessCriterion = 'log-likelihood',
    aic = summary$aic,
    deviance = summary$deviance,
    nData = length(object$fitted.values),
    nParameters = length(object$coefficients),
    optimalParameter = list()
  )

  terms = names(summary$aliased)
  for(i in 1:nrow(coef)){
    res$modelFit$optimalParameter[[i]]<-list(
      `@type`= 'Parameter',
      name = terms[i],
      estimate = list(
        `@type`= 'Statistic',
        value = coef[i,][[1]],
        standardError = coef[i,][[2]],
        statisticalTest = list(
          `@type`= 'TTest',
          testStatistic = coef[i,][[3]],
          df = summary$df[[2]],
          pValue = coef[i,][[4]]
        )
      )
    )
    if (length(grep("t value",   names(coef[i,])))){
      res$modelFit$optimalParameter[[i]]$estimate$statisticalTest$`@type` <- 'TTest'
    } else {
      res$modelFit$optimalParameter[[i]]$estimate$statisticalTest$`@type` <- 'ZTest'
    }
  }

  indRow = 1
  indFactor = 1
  nFactors = length(object$xlevels)

  # all intercepts
  contrastlist = c()
  for(k in 1:length(res$modelVariable)){
    if(!is.na(match("isCategorical",names(res$modelVariable[[k]])))){
      contrastlist = c(contrastlist,res$modelVariable[[k]]$factorConstrast[[1]]$name)
    }
  }
  res$modelFit$optimalParameter[[indRow]]$contrast <- contrastlist
  indRow = indRow + 1

  # first order terms and 2nd order with only one categorical factor
  for(i in 1:length(colnames(attr(object$terms,"factors")))){
    if(sum(attr(object$terms,"factors")[,i])==1){
      ind = match(1,attr(object$terms,"factors")[,i])
      if(attr(object$terms,"dataClasses")[[ind]]=="factor"){
        for(j in 2:length(object$xlevels[[indFactor]])){
          # fill with intercepts
          contrastlist = c()
          for(k in 1:length(res$modelVariable)){
            if(!is.na(match("isCategorical",names(res$modelVariable[[k]])))){
              contrastlist = c(contrastlist,res$modelVariable[[k]]$factorConstrast[[1]]$name)
            }
          }
          contrastlist[indFactor]<-res$modelVariable[[i+1]]$factorConstrast[[j]]$name
          res$modelFit$optimalParameter[[indRow]]$contrast <- contrastlist
          indRow = indRow + 1
        }
        indFactor = indFactor + 1
      } else {
        indRow = indRow + 1
      }
    }
    if(sum(attr(object$terms,"factors")[,i])==2){
      ind1 = which(attr(object$terms,"factors")[,i] %in% 1)[1]
      ind2 = which(attr(object$terms,"factors")[,i] %in% 1)[2]
      test = FALSE
      if((attr(object$terms,"dataClasses")[[ind1]]=="factor")&(attr(object$terms,"dataClasses")[[ind2]]!="factor")){
        ind = ind1
        test = TRUE
      }
      if((attr(object$terms,"dataClasses")[[ind2]]=="factor")&(attr(object$terms,"dataClasses")[[ind1]]!="factor")){
        ind = ind2
        test = TRUE
      }
      if(test){
        for(j in 2:length(object$xlevels[[indFactor]])){
          # fill with intercepts
          contrastlist = c()
          for(k in 1:length(res$modelVariable)){
            if(!is.na(match("isCategorical",names(res$modelVariable[[k]])))){
              contrastlist = c(contrastlist,res$modelVariable[[k]]$factorConstrast[[1]]$name)
            }
          }
          contrastlist[indFactor]<-res$modelVariable[[i+1]]$factorConstrast[[j]]$name
          res$modelFit$optimalParameter[[indRow]]$contrast <- contrastlist
          indRow = indRow + 1
        }
        indFactor = indFactor + 1
      }
    }
  }

  # second order terms
  for(i in 1:length(colnames(attr(object$terms,"factors")))){
    if(sum(attr(object$terms,"factors")[,i])==2){
      ind1 = which(attr(object$terms,"factors")[,i] %in% 1)[1]
      ind2 = which(attr(object$terms,"factors")[,i] %in% 1)[2]
      test = FALSE
      if((attr(object$terms,"dataClasses")[[ind1]]=="factor")&(attr(object$terms,"dataClasses")[[ind2]]=="factor")){
        test = TRUE
      }
      if(test){
        indFactor1 = match(rownames(attr(object$terms,"factors"))[ind1],names(object$xlevels))
        indFactor2 = match(rownames(attr(object$terms,"factors"))[ind2],names(object$xlevels))
        for(j1 in 2:length(object$xlevels[[indFactor1]])){
          for(j2 in 2:length(object$xlevels[[indFactor2]])){
            # fill with intercepts
            contrastlist = c()
            for(k in 1:length(res$modelVariable)){
              if(!is.na(match("isCategorical",names(res$modelVariable[[k]])))){
                contrastlist = c(contrastlist,res$modelVariable[[k]]$factorConstrast[[1]]$name)
              }
            }
            contrastlist[indFactor1]<-res$modelVariable[[ind1]]$factorConstrast[[j1]]$name
            contrastlist[indFactor2]<-res$modelVariable[[ind2]]$factorConstrast[[j2]]$name
            res$modelFit$optimalParameter[[indRow]]$contrast <- contrastlist
            indRow = indRow + 1
          }
        }
        indFactor = indFactor + 1
      } else {
        indRow = indRow + 1
      }
    }
  }

  res$fitResidual <- list(
    list(
      `@type`= c('Statistic', 'Min', 'Quantile'),
      name = 'Min',
      value = summary$deviance.resid[[1]],
      percentile=0
      ),
    list(
      `@type`= c('Statistic', 'Quantile'),
      name = '1Q',
      value = summary$deviance.resid[[5]],
      percentile=25
    ),
    list(
      `@type`= c('Statistic', 'Median', 'Quantile'),
      name = 'Median',
      value = summary$deviance.resid[[7]],
      percentile=50
    ),
    list(
      `@type`= c('Statistic', 'Quantile'),
      name = '3Q',
      value = summary$deviance.resid[[4]],
      percentile=75
    ),
    list(
      `@type`= c('Statistic', 'Max', 'Quantile'),
      name = 'Max',
      value = summary$deviance.resid[[2]],
      percentile=100
    )
  )

  cat(gsub("\t","  ",toJSON(res,pretty=1)),file=path)

})



# TODO: this is same as glm, define the method only once

#' @rdname RJSONLD.export-methods
setMethod("RJSONLD.export", c("negbin"), function(object, path){
  summary <- summary(object)
  coef <- coef(summary)
  res <- list( `@context` = list( `@vocab` = 'http://standardanalytics.io/stats/'),
               `@type` = 'LinearModel',
               modelFormula = deparse(object$call$formula, width.cutoff = 500L),
               family = object$family$family,
               link = object$family$link,
               modelVariable = list()
  )
  indfactor = 0
  for(i in 1:length(rownames(attr(object$terms,'factors')))){
    res$modelVariable[[i]] <- list(
      `@type` = 'Variable',
      name=rownames(attr(object$terms,'factors'))[i]
    )
    if(attr(object$terms,"dataClasses")[[i]]=="factor"){
      indfactor = indfactor + 1
      res$modelVariable[[i]]$isCategorical = 'true'
      res$modelVariable[[i]]$levels = object$xlevels[[indfactor]]
      res$modelVariable[[i]]$factorConstrast = list()
      if(object$contrasts[[indfactor]]=="contr.treatment"){
        n = length(object$xlevels[[indfactor]])
        res$modelVariable[[i]]$factorConstrast[[1]] <- list(
          name = paste(rownames(attr(object$terms,'factors'))[i],'-Intercept',sep=""),
          value = c(1,rep(0,n-1))
        )
        for(j in 2:n){
          res$modelVariable[[i]]$factorConstrast[[j]] <- list(
            name = paste(rownames(attr(object$terms,'factors'))[i],j-1,sep=""),
            value = c(-1,rep(0,j-2),1,rep(0,n-j))
          )
        }
      }
    }
  }

  res$modelFit <- list(
    `@type` = 'FitnessOptimization',
    fitnessCriterion = 'log-likelihood',
    aic = summary$aic,
    deviance = summary$deviance,
    nData = length(object$fitted.values),
    nParameters = length(object$coefficients),
    optimalParameter = list()
  )

  terms = names(summary$aliased)
  for(i in 1:nrow(coef)){
    res$modelFit$optimalParameter[[i]]<-list(
      `@type`= 'Parameter',
      name = terms[i],
      estimate = list(
        `@type`= 'Statistic',
        value = coef[i,][[1]],
        standardError = coef[i,][[2]],
        statisticalTest = list(
          `@type`= 'TTest',
          testStatistic = coef[i,][[3]],
          df = summary$df[[2]],
          pValue = coef[i,][[4]]
        )
      )
    )
    if (length(grep("t value",   names(coef[i,])))){
      res$modelFit$optimalParameter[[i]]$estimate$statisticalTest$`@type` <- 'TTest'
    } else {
      res$modelFit$optimalParameter[[i]]$estimate$statisticalTest$`@type` <- 'ZTest'
    }
  }

  indRow = 1
  indFactor = 1
  nFactors = length(object$xlevels)

  # all intercepts
  contrastlist = c()
  for(k in 1:length(res$modelVariable)){
    if(!is.na(match("isCategorical",names(res$modelVariable[[k]])))){
      contrastlist = c(contrastlist,res$modelVariable[[k]]$factorConstrast[[1]]$name)
    }
  }
  res$modelFit$optimalParameter[[indRow]]$contrast <- contrastlist
  indRow = indRow + 1

  # first order terms and 2nd order with only one categorical factor
  for(i in 1:length(colnames(attr(object$terms,"factors")))){
    if(sum(attr(object$terms,"factors")[,i])==1){
      ind = match(1,attr(object$terms,"factors")[,i])
      if(attr(object$terms,"dataClasses")[[ind]]=="factor"){
        for(j in 2:length(object$xlevels[[indFactor]])){
          # fill with intercepts
          contrastlist = c()
          for(k in 1:length(res$modelVariable)){
            if(!is.na(match("isCategorical",names(res$modelVariable[[k]])))){
              contrastlist = c(contrastlist,res$modelVariable[[k]]$factorConstrast[[1]]$name)
            }
          }
          contrastlist[indFactor]<-res$modelVariable[[i+1]]$factorConstrast[[j]]$name
          res$modelFit$optimalParameter[[indRow]]$contrast <- contrastlist
          indRow = indRow + 1
        }
        indFactor = indFactor + 1
      } else {
        indRow = indRow + 1
      }
    }
    if(sum(attr(object$terms,"factors")[,i])==2){
      ind1 = which(attr(object$terms,"factors")[,i] %in% 1)[1]
      ind2 = which(attr(object$terms,"factors")[,i] %in% 1)[2]
      test = FALSE
      if((attr(object$terms,"dataClasses")[[ind1]]=="factor")&(attr(object$terms,"dataClasses")[[ind2]]!="factor")){
        ind = ind1
        test = TRUE
      }
      if((attr(object$terms,"dataClasses")[[ind2]]=="factor")&(attr(object$terms,"dataClasses")[[ind1]]!="factor")){
        ind = ind2
        test = TRUE
      }
      if(test){
        for(j in 2:length(object$xlevels[[indFactor]])){
          # fill with intercepts
          contrastlist = c()
          for(k in 1:length(res$modelVariable)){
            if(!is.na(match("isCategorical",names(res$modelVariable[[k]])))){
              contrastlist = c(contrastlist,res$modelVariable[[k]]$factorConstrast[[1]]$name)
            }
          }
          contrastlist[indFactor]<-res$modelVariable[[i+1]]$factorConstrast[[j]]$name
          res$modelFit$optimalParameter[[indRow]]$contrast <- contrastlist
          indRow = indRow + 1
        }
        indFactor = indFactor + 1
      }
    }
  }

  # second order terms
  for(i in 1:length(colnames(attr(object$terms,"factors")))){
    if(sum(attr(object$terms,"factors")[,i])==2){
      ind1 = which(attr(object$terms,"factors")[,i] %in% 1)[1]
      ind2 = which(attr(object$terms,"factors")[,i] %in% 1)[2]
      test = FALSE
      if((attr(object$terms,"dataClasses")[[ind1]]=="factor")&(attr(object$terms,"dataClasses")[[ind2]]=="factor")){
        test = TRUE
      }
      if(test){
        indFactor1 = match(rownames(attr(object$terms,"factors"))[ind1],names(object$xlevels))
        indFactor2 = match(rownames(attr(object$terms,"factors"))[ind2],names(object$xlevels))
        for(j1 in 2:length(object$xlevels[[indFactor1]])){
          for(j2 in 2:length(object$xlevels[[indFactor2]])){
            # fill with intercepts
            contrastlist = c()
            for(k in 1:length(res$modelVariable)){
              if(!is.na(match("isCategorical",names(res$modelVariable[[k]])))){
                contrastlist = c(contrastlist,res$modelVariable[[k]]$factorConstrast[[1]]$name)
              }
            }
            contrastlist[indFactor1]<-res$modelVariable[[ind1]]$factorConstrast[[j1]]$name
            contrastlist[indFactor2]<-res$modelVariable[[ind2]]$factorConstrast[[j2]]$name
            res$modelFit$optimalParameter[[indRow]]$contrast <- contrastlist
            indRow = indRow + 1
          }
        }
        indFactor = indFactor + 1
      } else {
        indRow = indRow + 1
      }
    }
  }

  res$fitResidual <- list(
    list(
      `@type`= c('Statistic', 'Min', 'Quantile'),
      name = 'Min',
      value = summary$deviance.resid[[1]],
      percentile=0
    ),
    list(
      `@type`= c('Statistic', 'Quantile'),
      name = '1Q',
      value = summary$deviance.resid[[5]],
      percentile=25
    ),
    list(
      `@type`= c('Statistic', 'Median', 'Quantile'),
      name = 'Median',
      value = summary$deviance.resid[[7]],
      percentile=50
    ),
    list(
      `@type`= c('Statistic', 'Quantile'),
      name = '3Q',
      value = summary$deviance.resid[[4]],
      percentile=75
    ),
    list(
      `@type`= c('Statistic', 'Max', 'Quantile'),
      name = 'Max',
      value = summary$deviance.resid[[2]],
      percentile=100
    )
  )

  cat(gsub("\t","  ",toJSON(res,pretty=1)),file=path)

})




#' @rdname RJSONLD.export-methods
setMethod("RJSONLD.export", "aov", function(object, path){
  summary <- summary(object)
  terms <- attr(object$terms,'term.labels')
  coef <- coef(summary)
  res <- list( `@context` = list( `@vocab` = 'http://standardanalytics.io/stats/'),
               `@type` = 'LinearModel',
               modelFormula = deparse(object$call$formula, width.cutoff = 500L),
               modelVariable = list()
  )
  indfactor = 0
  for(i in 1:length(rownames(attr(object$terms,'factors')))){
    res$modelVariable[[i]] <- list(
      `@type` = 'Variable',
      name=rownames(attr(object$terms,'factors'))[i]
    )
    if(attr(object$terms,"dataClasses")[[i]]=="factor"){
      indfactor = indfactor + 1
      res$modelVariable[[i]]$isCategorical = 'true'
      res$modelVariable[[i]]$levels = object$xlevels[[indfactor]]
      res$modelVariable[[i]]$factorConstrast = list()
      if(object$contrasts[[indfactor]]=="contr.treatment"){
        n = length(object$xlevels[[indfactor]])
        res$modelVariable[[i]]$factorConstrast[[1]] <- list(
          name = paste(rownames(attr(object$terms,'factors'))[i],'-Intercept',sep=""),
          value = c(1,rep(0,n-1))
        )
        for(j in 2:n){
          res$modelVariable[[i]]$factorConstrast[[j]] <- list(
            name = paste(rownames(attr(object$terms,'factors'))[i],j-1,sep=""),
            value = c(-1,rep(0,j-2),1,rep(0,n-j))
          )
        }
      }
    }
  }

  res$modelFit <- list(
    `@type` = 'FitnessOptimization',
    fitnessCriterion = 'least squares',
    nData = length(object$fitted.values),
    nParameters = length(object$coefficients),
    anovaFactor = list(),
    anovaResidual = list()
  )

  for (i in 1:length(terms)){
    res$modelFit$anovaFactor[[i]]<-list(
      `@type` = 'AnovaFactor',
      name = terms[i],
      factorIndex = i,
      sumOfSquares = summary[[1]][['Sum Sq']][[i]],
      sumOfSquaresType ="I",
      meanOfSquares = summary[[1]][['Mean Sq']][[i]],
      statisticalTest = list(
        `@type` = 'FTest',
        testStatistic = summary[[1]][['F value']][[i]],
        dfNum = summary[[1]]$Df[[i]],
        dfDenom = summary[[1]][[1]][[length(summary[[1]][[1]])]],
        pValue = summary[[1]][['Pr(>F)']][[i]]
      )
    )
  }
  res$modelFit$anovaResidual <- list(
    `@type` = 'ANOVAResidual',
    sumOfSquares = summary[[1]][[2]][[length(summary[[1]][[2]])]],
    meanOfSquares = summary[[1]][[3]][[length(summary[[1]][[3]])]]
  )
  cat(gsub("\t","  ",toJSON(res,pretty=1)),file=path)
})


setOldClass("aovlist")
#' @rdname RJSONLD.export-methods
setMethod("RJSONLD.export", "aovlist", function(object, path){
  res <- list( `@context` = list( `@vocab` = 'http://standardanalytics.io/stats/'),
               `@type` = 'LinearModel',
               modelFormula = substr(attr(object,'call')[2],1,nchar(attr(object,'call')[2])),
               modelVariable = list()
  )
  indfactor = 0

  for(i in 1:(length(attr(object,"xlevels"))+1)){
    res$modelVariable[[i]] <- list(
      `@type` = 'Variable',
      name=rownames(attr(attr(object,'terms'),'factors'))[i]
    )
    if( i > 1){#attr(object$Within$terms,"dataClasses")[[i]]=="factor"){ # Categorical by default
      indfactor = indfactor + 1
      res$modelVariable[[i]]$isCategorical = 'true'
      res$modelVariable[[i]]$levels = attr(object,"xlevels")[[indfactor]]
      res$modelVariable[[i]]$factorConstrast = list()
      if(attr(object,"contrasts")[[indfactor]]=="contr.treatment"){
        n = length(attr(object,"xlevels")[[indfactor]])
        res$modelVariable[[i]]$factorConstrast[[1]] <- list(
          name = paste(rownames(attr(attr(object,'terms'),'factors'))[i],'-Intercept',sep=""),
          value = c(1,rep(0,n-1))
        )
        for(j in 2:n){
          res$modelVariable[[i]]$factorConstrast[[j]] <- list(
            name = paste(rownames(attr(attr(object,'terms'),'factors'))[i],j-1,sep=""),
            value = c(-1,rep(0,j-2),1,rep(0,n-j))
          )
        }
      }
    }
  }

  ncoefficients = 0
  for(indStratum in 1:length(names(object))){
    ncoefficients = ncoefficients + length(object[[i]]$coefficients)
  }

  res$modelFit <- list(
    `@type` = 'FitnessOptimization',
    fitnessCriterion = 'least squares',
    nData = length(object[[2]]$fitted.values),
    nParameters = ncoefficients,
    anovaFactor = list(),
    anovaResidual = list()
  )

  for(indStratum in 2:length(names(object))){
    terms <- attr(object[[indStratum]]$terms,'term.labels')
    summary = summary(object[[indStratum]])
    if(length(rownames(summary[[1]]))>1){
      for (i in 1:(length(rownames(summary[[1]]))-1)){
        res$modelFit$anovaFactor[[length(res$modelFit$anovaFactor)+1]]<-list(
          `@type` = 'AnovaFactor',
          errorStratum = names(object)[indStratum],
          name = terms[i],
          factorIndex = i,
          sumOfSquares = summary[[1]][['Sum Sq']][[i]],
          sumOfSquaresType ="I",
          meanOfSquares = summary[[1]][['Mean Sq']][[i]],
          statisticalTest = list(
            `@type` = 'FTest',
            testStatistic = summary[[1]][['F value']][[i]],
            dfNum = summary[[1]]$Df[[i]],
            dfDenom = summary[[1]][[1]][[length(summary[[1]][[1]])]],
            pValue = summary[[1]][['Pr(>F)']][[i]]
          )
        )
      }
    }
    res$modelFit$anovaResidual[[length(res$modelFit$anovaResidual)+1]] <- list(
      `@type` = 'ANOVAResidual',
      errorStratum = names(object)[indStratum],
      sumOfSquares = summary[[1]][[2]][[length(summary[[1]][[2]])]],
      meanOfSquares = summary[[1]][[3]][[length(summary[[1]][[3]])]]
    )
  }

  cat(gsub("\t","  ",toJSON(res,pretty=1)),file=path)
})


#' @rdname RJSONLD.export-methods
setMethod("RJSONLD.export", "anova", function(object, path){
  summary <- summary(object)
  res <- list( `@context` = list( `@vocab` = 'http://standardanalytics.io/stats/'),
               `@type` = 'ModelSelection',
               modelSet = list(),
               selectionCriterion = list()
  )
  for(i in 1:2){
    res$modelSet[[i]] <- list(
      name=paste("Model ",attr(object,"row.names")[[i]], sep=""),
      description=str_split(str_split(attr(object,"heading")[[2]],'\n')[[1]][i],': ')[[1]][2]
    )
  }
  if(length(grep('ratio',attr(object,"heading")[1]))>0){
    res$selectionCriterion[[1]] <- list(
      `@type` = 'Statistic',
      name = 'loglik-ratio',
      statisticalTest = list(
        `@type` = 'ChisqTest',
        testStatistic = object$Chisq[2],
        df = object['#Df'][[1]][2],
        pValue = object[['Pr(>Chisq)']][2]
      )
    )
  } else if(length(grep('Analysis of Variance',attr(object,"heading")[1]))>0){
    res$selectionCriterion[[1]] <- list(
      `@type` = 'AnovaRow',
      sumOfSquares = object$RSS[2],
      statisticalTest = list(
        `@type` = 'FTest',
        testStatistic = object$F[2],
        dfNum = object$Df,
        dfDenom = object$Res.Df[2],
        pValue = object[['Pr(>F)']][2]
      )
    )
  } else {
    print('The selection criterion is not recognized. Contributions to the RJSONLD package are welcome')
  }
  cat(gsub("\t","  ",toJSON(res,pretty=1)),file=path)
})

setOldClass("htest")
#' @rdname RJSONLD.export-methods
setMethod("RJSONLD.export", "htest", function(object, path){
  summary <- summary(object)
  if(length(grep("correlation",   object$method))){
    res <- list( `@context` = list( `@vocab` = 'http://standardanalytics.io/stats/'),
                 `@type` = c('Correlation','Statistic'),
                 description = paste('Correlation between ',str_split(object$data.name," and ")[[1]][1],' and ',strsplit(object$data.name," and ")[[1]][2],sep=''),
                 valueReference = list(
                   list( `@type`='Variable', name=strsplit(object$data.name," and ")[[1]][1] ),
                   list( `@type`='Variable', name=strsplit(object$data.name," and ")[[1]][2] )
                 ),
                 value = object$estimate[[1]],
                 confidenceInterval = list(
                   list( `@type`=c('Statistic','Quantile'), percentile=2.5, value=object$conf.int[1] ),
                   list( `@type`=c('Statistic','Quantile'), percentile=97.5, value=object$conf.int[2] )
                 ),
                 statisticalTest = list(
                   `@type` = 'CorTest',
                   method = list(
                     description= object$method,
                     sameAs="http://stat.ethz.ch/R-manual/R-patched/library/stats/html/cor.test.html"
                   ),
                   alternative = object$alternative,
                   testStatistic = object$statistic[[1]],
                   df = object$parameter[[1]],
                   pValue = object$p.value[[1]]
                 )
    )
  } else if(length(grep("proportions ",   object$method))){
    res <- list( `@context` = list( `@vocab` = 'http://standardanalytics.io/stats/'),
                 `@type` = c('Proportion','Statistic'),
                 description = object$data.name,
                 value = c(),
                 statisticalTest = list(
                   `@type` = 'ChiSquareTest',
                   method = list(
                     description= object$method,
                     sameAs="http://stat.ethz.ch/R-manual/R-patched/library/stats/html/prop.test.html"
                   ),
                   alternative = object$alternative,
                   testStatistic = object$statistic[[1]],
                   df = object$parameter[[1]],
                   pValue = object$p.value[[1]]
                 )
    )
    #    for (i in 1:length(object$estimate)){
    #      res$valueReference[[length(res$valueReference)+1]]= list( `@type`='Variable', name=names(object$estimate)[i] )
    #    }
    for (i in 1:length(object$estimate)){
      res$value[i] = object$estimate[[i]]
    }
  } else if(length(grep("t-test",   object$method))){
    if (length(object$estimate) == 1){
      res <- list( `@context` = list( `@vocab` = 'http://standardanalytics.io/stats/'),
                   `@type` = c('MeanDifference','Statistic'),
                   description = paste('T-test between ',object$data.name),
                   valueReference = list(
                     list( `@type`='Variable', name=strsplit(object$data.name," and ")[[1]][1] ),
                     list( `@type`='Variable', name=strsplit(object$data.name," and ")[[1]][2] )
                   ),
                   value = c(object$estimate[[1]]),
                   confidenceInterval = list(
                     list( `@type`=c('Statistic','Quantile'), percentile=2.5, value=object$conf.int[1] ),
                     list( `@type`=c('Statistic','Quantile'), percentile=97.5, value=object$conf.int[2] )
                   ),
                   statisticalTest = list(
                     `@type` = 'TTest',
                     method = list(
                       description= object$method,
                       sameAs="http://stat.ethz.ch/R-manual/R-patched/library/stats/html/t.test.html"
                     ),
                     alternative = object$alternative,
                     testStatistic = object$statistic[[1]],
                     df = object$parameter[[1]],
                     pValue = object$p.value[[1]]
                   )
      )
    } else {
      res <- list( `@context` = list( `@vocab` = 'http://standardanalytics.io/stats/'),
                   `@type` = c('Mean','Statistic'),
                   description = paste('T-test between ',object$data.name),
                   valueReference = list(
                     list( `@type`='Variable', name=strsplit(object$data.name," and ")[[1]][1] ),
                     list( `@type`='Variable', name=strsplit(object$data.name," and ")[[1]][2] )
                   ),
                   value = c(object$estimate[[1]],object$estimate[[2]]),
                   confidenceInterval = list(
                     list( `@type`=c('Statistic','Quantile'), percentile=2.5, value=object$conf.int[1] ),
                     list( `@type`=c('Statistic','Quantile'), percentile=97.5, value=object$conf.int[2] )
                   ),
                   statisticalTest = list(
                     `@type` = 'TTest',
                     method = list(
                       description= object$method,
                       sameAs="http://stat.ethz.ch/R-manual/R-patched/library/stats/html/t.test.html"
                     ),
                     alternative = object$alternative,
                     testStatistic = object$statistic[[1]],
                     df = object$parameter[[1]],
                     pValue = object$p.value[[1]]
                   )
      )
    }
  } else if(length(grep("Chi-squared",   object$method))){
    res <- list( `@context` = list( `@vocab` = 'http://standardanalytics.io/stats/'),
                 `@type` = c('Statistic'),
                 description = paste('Contingency table of ',names(dimnames(object$observed)[1]),' among ',sep=''),
                 valueReference = c(),
                 statisticalTest = list(
                   `@type` = 'ChiSquareTest',
                   method = list(
                     description= object$method,
                     sameAs="http://stat.ethz.ch/R-manual/R-patched/library/stats/html/chisq.test.html"
                   ),
                   alternative = object$alternative,
                   testStatistic = object$statistic[[1]],
                   df = object$parameter[[1]],
                   pValue = object$p.value[[1]]
                 )
    )
    for (i in 1:(length(colnames(object$observed))-2)){
      res$description = paste(res$description,colnames(object$observed)[i],', ')
    }
    res$description = paste(res$description,colnames(object$observed)[(length(colnames(object$observed))-1)],sep='')
    res$description = paste(res$description,' and ',colnames(object$observed)[(length(colnames(object$observed)))],sep='')
    for (i in 1:length(colnames(object$observed))){
      res$valueReference[[i]]= list( `@type`='Variable', name=colnames(object$observed)[i] )
    }
  } else {
    res <- list( `@context` = list( `@vocab` = 'http://standardanalytics.io/stats/'),
                 `@type` = 'StatisticalTest',
                 description = object$data.name[[1]],
                 testStatistic = object$statistic[[1]],
                 df = object$parameter[[1]],
                 pValue = object$p.value[[1]]
    )
  }
  cat(gsub("\t","  ",toJSON(res,pretty=1)),file=path)
})


setOldClass("pairwise.htest")
#' @rdname RJSONLD.export-methods
setMethod("RJSONLD.export", "pairwise.htest", function(object, path){
  summary <- summary(object)
  varname = strsplit(object$data.name," and ")[[1]][2]
  res <- list( `@context` = list( `@vocab` = 'http://standardanalytics.io/stats/'),
               `@type` = 'MultipleComparison',
               method = list(
                 description= paste(object$method,' (',object$p.adjust.method,')',sep=''),
                 sameAs="http://stat.ethz.ch/R-manual/R-patched/library/stats/html/pairwise.t.test.html"
               ),
               comparison=c()
  )
  for(i in 1:(length(rownames(object$p.value)))){
    for(j in 1:i){
      res$comparison[[length(res$comparison)+1]] <- list(
        `@type` = 'Statistic',
        valueReference = list(
          list( `@type` = 'Variable', name = paste(varname,i+1,sep='') ),
          list( `@type` = 'Variable', name = paste(varname,j,sep='') )
        ),
        statisticalTest = list(
          pValue = object$p.value[i,j],
          attype = 'StatisticalTest'
        )
      )
    }
  }
  cat(gsub('attype','@type',gsub("\t","  ",toJSON(res,pretty=1))),file=path)
})

setOldClass("TukeyHSD")
#' @rdname RJSONLD.export-methods
setMethod("RJSONLD.export", "TukeyHSD", function(object, path){
  summary <- summary(object)
  res <- list( `@context` = list( `@vocab` = 'http://standardanalytics.io/stats/'),
               `@type` = 'MultipleComparison',
               method = list(
                 description= 'Tukey multiple comparisons of means',
                 sameAs="http://stat.ethz.ch/R-manual/R-patched/library/stats/html/pairwise.t.test.html"
               ),
               comparison=c()
  )
  for(i in 1:(length(rownames(object$method)))){
    res$comparison[[length(res$comparison)+1]] <- list(
      `@type` = 'Statistic',
      description = rownames(object$method)[i],
      valueReference = list(
        list( `@type` = 'Variable', name = strsplit(rownames(object$method)[i],'-')[[1]][1] ),
        list( `@type` = 'Variable', name = strsplit(rownames(object$method)[i],'-')[[1]][2] )
      ),
      value = object$method[i,1],
      confidenceInterval = list(
        list( `@type` = c('Variable','Quantile'),
              percentile=2.5,
              value = object$method[i,2]
        ),
        list( `@type` = c('Variable','Quantile'),
              percentile=97.5,
              value = object$method[i,3]
        )
      ),
      statisticalTest = list(
        pValue = object$method[i,4],
        attype = 'StatisticalTest'
      )
    )
  }
  cat(gsub('attype','@type',gsub("\t","  ",toJSON(res,pretty=1))),file=path)
})


setOldClass("pairwise.htest")
#' @rdname RJSONLD.export-methods
setMethod("RJSONLD.export", "pairwise.htest", function(object, path){
  summary <- summary(object)
  varname = strsplit(object$data.name," and ")[[1]][2]
  res <- list( `@context` = list( `@vocab` = 'http://standardanalytics.io/stats/'),
               `@type` = 'MultipleComparison',
               method = list(
                 description= paste(object$method,' (',object$p.adjust.method,')',sep=''),
                 sameAs="http://stat.ethz.ch/R-manual/R-patched/library/stats/html/pairwise.t.test.html"
               ),
               comparison=c()
  )
  for(i in 1:(length(rownames(object$p.value)))){
    for(j in 1:i){
      res$comparison[[length(res$comparison)+1]] <- list(
        `@type` = 'Statistic',
        valueReference = list(
          list( `@type` = 'Variable', name = paste(varname,i+1,sep='') ),
          list( `@type` = 'Variable', name = paste(varname,j,sep='') )
        ),
        statisticalTest = list(
          pValue = object$p.value[i,j],
          attype = 'StatisticalTest'
        )
      )
    }
  }
  cat(gsub('attype','@type',gsub("\t","  ",toJSON(res,pretty=1))),file=path)
})

setOldClass("TukeyHSD")
#' @rdname RJSONLD.export-methods
setMethod("RJSONLD.export", "TukeyHSD", function(object, path){
  summary <- summary(object)
  res <- list( `@context` = list( `@vocab` = 'http://standardanalytics.io/stats/'),
               `@type` = 'MultipleComparison',
               method = list(
                 description= 'Tukey multiple comparisons of means',
                 sameAs="http://stat.ethz.ch/R-manual/R-patched/library/stats/html/pairwise.t.test.html"
               ),
               comparison=c()
  )
  for(i in 1:(length(rownames(object$method)))){
    res$comparison[[length(res$comparison)+1]] <- list(
        `@type` = 'Statistic',
        description = rownames(object$method)[i],
        valueReference = list(
          list( `@type` = 'Variable', name = strsplit(rownames(object$method)[i],'-')[[1]][1] ),
          list( `@type` = 'Variable', name = strsplit(rownames(object$method)[i],'-')[[1]][2] )
        ),
        value = object$method[i,1],
        confidenceInterval = list(
          list( `@type` = c('Variable','Quantile'),
                percentile=2.5,
                value = object$method[i,2]
          ),
          list( `@type` = c('Variable','Quantile'),
                percentile=97.5,
                value = object$method[i,3]
          )
        ),
        statisticalTest = list(
          pValue = object$method[i,4],
          attype = 'StatisticalTest'
        )
      )
  }
  cat(gsub('attype','@type',gsub("\t","  ",toJSON(res,pretty=1))),file=path)
})



#' @rdname RJSONLD.export-methods
setMethod("RJSONLD.export", "stanfit", function(object, path){
  res <- list( `@context` = list( `@vocab` = 'http://standardanalytics.io/stats/'),
               `@type` = 'BayesianHierarchicalModel',
               modelFormula = object@stanmodel@model_code,
               modelVariable = list()
  )
  summary = summary(object)

  # Extract code for parameters
  openBrackets = 1
  indstart = str_locate(object@stanmodel@model_code[[1]], 'parameters')[2] + 2
  parDef  =''
  temp = unlist(strsplit(substr(object@stanmodel@model_code[[1]],indstart+1,nchar(object@stanmodel@model_code[[1]])), split=""))
  i = 1
  while(openBrackets != 0){
    if(temp[i] == '}'){
      openBrackets = openBrackets - 1
    } else if(temp[i] == '{'){
      openBrackets = openBrackets + 1
    }
    parDef = paste(parDef,temp[i],sep='')
    i = i+1
  }

  # Extract code for model
  openBrackets = 1
  indstart = str_locate(object@stanmodel@model_code[[1]], 'model')[2] + 2
  modelDef  =''
  temp = unlist(strsplit(substr(object@stanmodel@model_code[[1]],indstart+1,nchar(object@stanmodel@model_code[[1]])), split=""))
  i = 1
  while(openBrackets != 0){
    if(temp[i] == '}'){
      openBrackets = openBrackets - 1
    } else if(temp[i] == '{'){
      openBrackets = openBrackets + 1
    }
    modelDef = paste(modelDef,temp[i],sep='')
    i = i+1
  }

  # Extract parameters that are defined in parameters section
  allPars = names(object@sim$samples[[1]])
  basePars = list()
  for (i in 1:length(allPars)){
  	if(is.na(str_locate(names(object@sim$samples[[1]])[i],'\\[')[1])){
  		tmpname = names(object$sim$samples[[1]])[i]
  	} else {
	  	tmpname = 	substr(names(object$sim$samples[[1]])[i],1,str_locate(names(object$sim$samples[[1]])[i],'\\[')[1]-1)
  	}
  	if(!is.na(str_locate(parDef,tmpname)[1])){
  		basePars = c(basePars,names(object$sim$samples[[1]])[i])
  	}
  }

  # Extract variables
  tmp = str_split(modelDef,'\n')
  for(i in 2:(length(tmp[[1]])-1)){
    tmp2 = str_split(tmp[[1]][i],'~')
    if (!(gsub(" ","",tmp2[[1]][1]) %in% basePars)){
      res$modelVariable[[length(res$modelVariable)+1]] <- list(
        `@type` = 'Variable',
        name = gsub(" ","",tmp2[[1]][1])
        )
    }
  }


  distributionParametersTerminology = list()
  normal <- c('mean','sigma')
  cauchy <- c('median','sigma')
  gamma <- c('alpha','beta')
  distributionParametersTerminology$normal <- normal
  distributionParametersTerminology$cauchy <- cauchy
  distributionParametersTerminology$gamma <- gamma

  trim <- function (x) gsub("^\\s+|\\s+$","",x)

  npars = length(basePars)
  modelParameter = rep(list(list()),npars)
  for (i in 1:npars){
  	prior = list(
      distributionParameter <- list( a = 'a')
      )

  	# locate line that defines the prior in the model section
  	if(is.na(str_locate(basePars[[i]],'\\[')[1])){
  		basename = basePars[i]
  	} else {
	  	basename = 	substr(basePars[[i]],1,str_locate(basePars[[i]],'\\[')[1]-1)
  	}
  	line = ''
  	for (j in 1:length(str_split(modelDef,'\n')[[1]])){
  		# be careful, there needs to be a space between parameter name and ~
  		if(!is.na(str_locate(str_split(modelDef,'\n')[[1]][[j]],paste(basename," ~",sep=''))[1])){
  			line = str_split(modelDef,'\n')[[1]][[j]]
  		}
  	}

  	distributionParameter <- list()
  	# get distribution name
  	if (line!=''){
    	indstart = str_locate(line,'~')+1
  		indend = indstart + str_locate(substr(line,indstart,nchar(line)),'\\(')[1]-2
  		priorName = trim(substr(line,indstart,indend))
  		prior$name <- priorName
  		# get distribution arguments
  		openPars = 1
		  indstart = indstart - 1 + str_locate(substr(line,indstart,nchar(line)), '\\(')[2]
      args  =''
      temp = unlist(strsplit(substr(line,indstart+1,nchar(line)), split=""))
      j = 0
      while(openPars != 0){
        	if(temp[j+1] == ')'){
           		openPars = openPars - 1
         	} else if(temp[j+1] == '('){
           		openPars = openPars + 1
         	}
         	args = paste(args,temp[j],sep='')
        	j = j+1
       	}
       	args = strsplit(args,split=',')
       	if(priorName %in% names(distributionParametersTerminology)){
	       	for(j in 1:length(args[[1]])){
		       	tmp = list()
		       	if (!is.na(suppressWarnings(as.numeric(args[[1]][j])))){
	    			tmp <- setNames(c(distributionParametersTerminology[[priorName]][j],args[[1]][j]),c('name','value'))
		       	} else {
	    			tmp <- setNames(c(distributionParametersTerminology[[priorName]][j],args[[1]][j]),c('name','valueReference'))
		       	}
    			distributionParameter[[length(distributionParameter)+1]] = tmp
       		}
       	} else {
       		'Warning: unrecognized prior distribution. Pull requests welcome'
       	}

  	} else {
  		prior$name = 'uniform'
  	}

  	# locate line that defines this parameter in the parameter section
  	if(is.na(str_locate(basePars[[i]],'\\[')[1])){
  		basename = basePars[i]
  	} else {
	  	basename = 	substr(basePars[[i]],1,str_locate(basePars[[i]],'\\[')[1]-1)
  	}
  	line = ''
  	for (j in 1:length(str_split(parDef,'\n')[[1]])){
  		# be careful, there needs to be a space between parameter name and ~
  		if(!is.na(str_locate(str_split(parDef,'\n')[[1]][[j]],basename[[1]])[1])){
  			line = str_split(parDef,'\n')[[1]][[j]]
  		}
  	}
  	if(!is.na(str_locate(line,'lower=')[1])){
  		tmp = list()
    	tmp <- setNames(c('lower',substr(str_extract(line,'lower(.*)>'),7,nchar(str_extract(line,'lower(.*)>'))-1)),c('name','value'))
    	distributionParameter[[length(distributionParameter)+1]] = tmp
  	} else {
  		tmp = list()
    	tmp <- setNames(c('lower','-inf'),c('name','value'))
    	distributionParameter[[length(distributionParameter)+1]] = tmp
  	}
  	if(!is.na(str_locate(line,'upper=')[1])){
  		tmp = list()
    	tmp <- setNames(c('upper',substr(str_extract(line,'upper(.*)>'),7,nchar(str_extract(line,'upper(.*)>'))-1)),c('name','value'))
    	distributionParameter[[length(distributionParameter)+1]] = tmp
  	} else {
  		tmp = list()
    	tmp <- setNames(c('upper','inf'),c('name','value'))
    	distributionParameter[[length(distributionParameter)+1]] = tmp
  	}

  	prior$distributionParameter <- list()
  	prior$distributionParameter <- distributionParameter

  	basename = basePars[[i]]
  	modelParameter[[i]] <- list(
      `@type` = 'Parameter',
      name = basename,
      prior = list(
        name = paste(basename,"-prior",sep=""),
        description = toJSON(prior,pretty=1)
      )
    )
  }
  res$modelParameter = modelParameter


  res$modelFit <- list()
  modelFit = list(
    `@type` = 'MCMC',
    method = list(
      description = "No U-Turn Sampler",
      sameAs = "http://arxiv.org/abs/1111.4246"
    ),
    parameterInference = list(),
    effectiveSampleSize = list(),
    rhat = list()
  )

  for (i in 1:npars){
    modelFit$parameterInference[[length(modelFit$parameterInference)+1]] <- list(
      `@type` = c('Statistic','Mean'),
      name = 'mean',
      valueReference = list(
        name = basePars[i]
      ),
      value = summary$summary[i,1]
    )
    modelFit$parameterInference[[length(modelFit$parameterInference)+1]] <- list(
      `@type` = c('Statistic','StandardDeviation'),
      name = 'sd',
      valueReference = list(
        name = basePars[i]
      ),
      value = summary$summary[i,3]
    )

    modelFit$parameterInference[[length(modelFit$parameterInference)+1]] <- list(
      `@type` = c('Statistic'),
      name = 'se_mean',
      valueReference = list(
        name = basePars[i]
      ),
      value = summary$summary[i,2]
    )

    modelFit$parameterInference[[length(modelFit$parameterInference)+1]] <- list(
      `@type` = c('Statistic','Quantile'),
      name = '2.5%',
      percentile = 2.5,
      valueReference = list(
        name = basePars[i]
      ),
      value = summary$summary[i,4]
    )

    modelFit$parameterInference[[length(modelFit$parameterInference)+1]] <- list(
      `@type` = c('Statistic','Quantile'),
      name = '25%',
      percentile = 25,
      valueReference = list(
        name = basePars[i]
      ),
      value = summary$summary[i,5]
    )

    modelFit$parameterInference[[length(modelFit$parameterInference)+1]] <- list(
      `@type` = c('Statistic','Quantile','Median'),
      name = '50%',
      percentile = 50,
      valueReference = list(
        name = basePars[i]
      ),
      value = summary$summary[i,6]
    )

    modelFit$parameterInference[[length(modelFit$parameterInference)+1]] <- list(
      `@type` = c('Statistic','Quantile'),
      name = '75%',
      percentile = 75,
      valueReference = list(
        name = basePars[i]
      ),
      value = summary$summary[i,7]
    )

    modelFit$parameterInference[[length(modelFit$parameterInference)+1]] <- list(
      `@type` = c('Statistic','Quantile'),
      name = '97.5%',
      percentile = 97.5,
      valueReference = list(
        name = basePars[i]
      ),
      value = summary$summary[i,8]
    )

    modelFit$effectiveSampleSize[[length(modelFit$effectiveSampleSize)+1]] <- list(
      valueReference = list( name = basePars[i] ),
      value = summary$summary[i,9]
    )

    modelFit$rhat[[length(modelFit$rhat)+1]] <- list(
      valueReference = list( name = basePars[i] ),
      value = summary$summary[i,10]
    )

  }

  res$modelFit <- modelFit

  output = toJSON(res,pretty=1)
  output = gsub("\t","  ",output)
  output = gsub("atcontext","@context",output)
  output = gsub("atid","@id",output)
  output = gsub("attype","@type",output)
  cat(output,file=path)
})
