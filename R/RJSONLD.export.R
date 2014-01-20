#################################################################################
#' Export results of standard statistical functions to semantic JSON-LD format
#'
#' Make your results of standard statistical analysis browsable and reproducible 
#' by exporting them into JSON-LD, following a standardized vocabulary 
#' (http://standardanalytics.io/stats). This vocabulary is still at a draft stage:
#' provide feedback, suggestions and extenstions at https://github.com/standard-analytics/RJSONLD
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

#' @rdname RJSONLD.export-methods
setMethod("RJSONLD.export", "lm", function(object, path){
  summary <- summary(object)
  coef <- coef(summary)
  res <- list( `@context` = list( `@vocab` = 'http://standardanalytics.io/stats/'),
               `@type` = 'LinearModel',
               modelFormula = deparse(object$call$formula),
               r2 = summary$r.squared,
               adjr2 = summary$adj.r.squared,
               fRatioTest = list(
                 `@type`= 'FTest',
                 testStatistic = summary$fstatistic[[1]],
                 dfNum = summary$fstatistic[[2]],
                 dfDenom = summary$fstatistic[[3]],
                 pValue = pf(summary$fstatistic[[1]], summary$fstatistic[[2]], summary$fstatistic[[3]], lower.tail=FALSE)
              ),
              modelCoefficients = list()
  )
  terms = names(summary$aliased)
  for(i in 1:nrow(coef)){
    res$modelCoefficients[[i]]<-list(
      name = terms[i],
      estimate = coef[i,][[1]],
      stdError = coef[i,][[2]],
      statTest = list(
        `@type` = 'TTest',
        testStatistic = coef[i,][[3]],
        df = summary$df[[2]],
        pValue = coef[i,][[4]]
      )
    )
  }
  cat(gsub("\t","  ",toJSON(res,pretty=1)),file=path)
})


#' @rdname RJSONLD.export-methods
setMethod("RJSONLD.export", "glm", function(object, path){
  summary <- summary(object)
  coef <- coef(summary)
  names <- names(unlist(summary))
  cpt <- 0
  terms <- rep("",length(names))
  for (i in 1:length(names)){
    if(length(grep("aliased.",names[i]))){
      cpt <- cpt + 1
      terms[cpt]<- substr(names[i],9,nchar(names[i]))
    }
  }
  res <- list( `@context` = list( `@vocab` = 'http://standardanalytics.io/stats/'),
               `@type` = 'GeneralizedLinearModel',
               modelFormula = deparse(object$call$formula),
               aic = summary$aic,
               family = object$family$family,
               modelCoefficients = list()
  )
  for (i in 1:cpt){
    res$modelCoefficients[[i]]<-list(
      name = terms[i],
      estimate = coef[i,][[1]],
      stdError = coef[i,][[2]],
      statTest = list(
        testStatistic = coef[i,][[3]],
        pValue = coef[i,][[4]]
      )
    )
    if (length(grep("t value",   names(coef[i,])))){
      res$modelCoefficients[[i]]$statTest$`@type` <- 'TTest'
    } else {
      res$modelCoefficients[[i]]$statTest$`@type` <- 'ZTest'
    }
  }
  cat(gsub("\t","  ",toJSON(res,pretty=1)),file=path)
})


#' @rdname RJSONLD.export-methods
setMethod("RJSONLD.export", "aov", function(object, path){
  summary <- summary(object)
  terms <- attr(object$terms,'term.labels')
  res <- list( `@context` = list( `@vocab` = 'http://standardanalytics.io/stats/'),
               `@type` = 'LinearModel',
               modelFormula = deparse(object$call$formula),
               anova = list()
  )
  for (i in 1:length(terms)){
    res$anova[[i]]<-list(
      `@type` = 'ANOVAFactor',
      name = terms[i],
      sumSq = summary[[1]][['Sum Sq']][[i]],
      meanSq = summary[[1]][['Mean Sq']][[i]],
      statTest = list(
        `@type` = 'FTest',
        testStatistic = summary[[1]][['F value']][[i]],
        dfNum = summary[[1]]$Df[[i]],
        dfDenom = summary[[1]][[1]][[length(summary[[1]][[1]])]],
        pValue = summary[[1]][['Pr(>F)']][[i]]
      )
    )
  }  
  res$anova[[length(terms)+1]] <- list(
    `@type` = 'ANOVAResidual',
    sumSq = summary[[1]][[2]][[length(summary[[1]][[2]])]],
    meanSq = summary[[1]][[3]][[length(summary[[1]][[3]])]]
  )
  cat(gsub("\t","  ",toJSON(res,pretty=1)),file=path)
})


setOldClass("aovlist")
#' @rdname RJSONLD.export-methods
setMethod("RJSONLD.export", "aovlist", function(object, path){
  res <- list( `@context` = list( `@vocab` = 'http://standardanalytics.io/stats/'),
               `@type` = 'LinearModel',
               modelFormula = deparse(attr(object,'call')$formula),
               anova = list()
  )
  cpt = 0
  eff <- eff.aovlist(object)
  for (s in 2:length(object)){
    terms <- colnames(eff)[eff[s-1,]==1]
    summary <- summary(object[[s]])
    if(length(terms)>0){
      for (i in 1:length(terms)){
        cpt = cpt + 1
        res$anova[[cpt]] <- list(
          `@type` = 'ANOVAFactor',
          name = terms[i],
          errorStratum = names(object)[[s]],
          sumSq = summary[[1]][['Sum Sq']][[i]],
          meanSq = summary[[1]][['Mean Sq']][[i]],
          statTest = list(
            `@type` = 'FTest',
            testStatistic = summary[[1]][['F value']][[i]],
            dfNum = summary[[1]]$Df[[i]],
            dfDenom = summary[[1]][[1]][[length(summary[[1]][[1]])]],
            pValue = summary[[1]][['Pr(>F)']][[i]]
          )
        )
      }
    }  
    cpt = cpt + 1
    res$anova[[cpt]] <- list(
      `@type` = 'ANOVAResidual',
      errorStratum = names(object)[[s]],
      sumSq = summary[[1]][[2]][[length(summary[[1]][[2]])]],
      meanSq = summary[[1]][[3]][[length(summary[[1]][[3]])]]
    )
  }
  cat(gsub("\t","  ",toJSON(res,pretty=1)),file=path)
})

setOldClass("htest")
#' @rdname RJSONLD.export-methods
setMethod("RJSONLD.export", "htest", function(object, path){
  summary <- summary(object)
  if(length(grep("correlation",   object$method))){
    res <- list( `@context` = list( `@vocab` = 'http://standardanalytics.io/stats/'),
                 `@type` = 'Correlation',
                 covariate1 = strsplit(object$data.name," and ")[[1]][1],
                 covariate2 = strsplit(object$data.name," and ")[[1]][2],
                 estimate = object$estimate[[1]],
                 statTest = list(
                   `@type` = 'TTest',
                   testStatistic = object$statistic[[1]],
                   df = object$parameter[[1]],
                   pValue = object$p.value[[1]]
                 )
    )
  } else if(length(grep("proportions ",   object$method))){
    res <- list( `@context` = list( `@vocab` = 'http://standardanalytics.io/stats/'),
                 `@type` = 'Proportion',
                 estimate = object$estimate[[1]],
                 statTest = list(
                   `@type` = 'ChisqTest',
                   testStatistic = object$statistic[[1]],
                   df = object$parameter[[1]],
                   pValue = object$p.value[[1]]
                 )
    )
  } else {
    res <- list( `@context` = list( `@vocab` = 'http://standardanalytics.io/stats/'),
                 `@type` = 'StatTest',
                 description = object$data.name[[1]],
                 testStatistic = object$statistic[[1]],
                 df = object$parameter[[1]],
                 pValue = object$p.value[[1]]
    )
  }
  cat(gsub("\t","  ",toJSON(res,pretty=1)),file=path)
})
