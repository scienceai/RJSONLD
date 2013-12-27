############################################################################
#' Export results of standard statistical functions to semantic JSON-LD format
#'
#' Simply reformats standard statistical function outputs into
#' JSON-LD format, abiding with statistical vocabulary described in 
#' https://github.com/standard-analytics/Schemas
#'
#' @param object object to be exported.
#'
#' @param path relative where JSON-LD file is to be created
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
#' RJSONLD.export(lm(iris$Petal.Length~iris$Sepal.Length),"irisLM.json")
#' RJSONLD.export(aov(iris$Petal.Length~iris$Species),"irisANOVA.json")
#' RJSONLD.export(aov(iris$Petal.Length~iris$Species*iris$Sepal.Length),"irisANCOVA.json")
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
  res <- list()
  res$formula <- deparse(object$call$formula)
  res$R2 <- summary$r.squared
  res$adjR2 <- summary$adj.r.squared
  res$FTest <- list()
  res$FTest$statistic <- summary$fstatistic[[1]]
  res$FTest$dfNum <- summary$fstatistic[[2]]
  res$FTest$dfDenom <- summary$fstatistic[[3]]
  res$FTest$pValue <- pf(summary$fstatistic[[1]], summary$fstatistic[[2]], summary$fstatistic[[3]], lower.tail=FALSE)
  res$coefficients <- list()
  terms = names(summary$aliased)
  for(i in 1:nrow(coef)){
    res$coefficients[[i]]<-list()
    res$coefficients[[i]]$term <- terms[i]
    res$coefficients[[i]]$estimate <- coef[i,][[1]]
    res$coefficients[[i]]$stdError <- coef[i,][[2]]
    res$coefficients[[i]]$tTest <- list()
    res$coefficients[[i]]$tTest$statistic <- coef[i,][[3]]
    res$coefficients[[i]]$tTest$df <- summary$df[[2]]
    res$coefficients[[i]]$tTest$pValue <- coef[i,][[4]]
  }
  cat(toJSON(res,pretty=1),file=path)
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
  res <- list()
  res$formula <- deparse(object$call$formula)
  res$AIC <- summary$aic
  res$deviance <- object$deviance
  res$family <- object$family$family
  res$coefficients <- list()
  for (i in 1:cpt){
    res$coefficients[[i]]<-list()
    res$coefficients[[i]]$term <- terms[i]
    res$coefficients[[i]]$estimate <- coef[i,][[1]]
    res$coefficients[[i]]$stdError <- coef[i,][[2]]
    if (length(grep("t value",   names(coef[i,])))){
      res$coefficients[[i]]$tTest <- list()
      res$coefficients[[i]]$tTest$statistic <- coef[i,][[3]]
      res$coefficients[[i]]$tTest$pValue <- coef[i,][[4]]
    } else {
      res$coefficients[[i]]$zTest <- list()
      res$coefficients[[i]]$zTest$statistic <- coef[i,][[3]]
      res$coefficients[[i]]$zTest$pValue <- coef[i,][[4]]
    }
  }
  cat(toJSON(res,pretty=1),file=path)
})


#' @rdname RJSONLD.export-methods
setMethod("RJSONLD.export", "aov", function(object, path){
  summary <- summary(object)
  terms <- attr(object$terms,'term.labels')
  res <- list()
  res$formula <- deparse(object$call$formula)
  res$errorStrata <- list()
  res$errorStrata[[1]] <- list()
  res$errorStrata[[1]]$effects <- list()
  for (i in 1:length(terms)){
    res$errorStrata[[1]]$effects[[i]]<-list()
    res$errorStrata[[1]]$effects[[i]]$term <- terms[i]
    res$errorStrata[[1]]$effects[[i]]$df <- summary[[1]]$Df[[i]]
    res$errorStrata[[1]]$effects[[i]]$sumSq <- summary[[1]][['Sum Sq']][[i]]
    res$errorStrata[[1]]$effects[[i]]$meanSq <- summary[[1]][['Mean Sq']][[i]]
    res$errorStrata[[1]]$effects[[i]]$FTest <- list()
    res$errorStrata[[1]]$effects[[i]]$FTest$statistic <- summary[[1]][['F value']][[i]]
    res$errorStrata[[1]]$effects[[i]]$FTest$dfNum <- summary[[1]]$Df[[i]]
    res$errorStrata[[1]]$effects[[i]]$FTest$dfDenom <- summary[[1]][[1]][[length(summary[[1]][[1]])]]
    res$errorStrata[[1]]$effects[[i]]$FTest$pValue <- summary[[1]][['Pr(>F)']][[i]]
  }
  res$errorStrata[[1]]$residuals <- list()
  res$errorStrata[[1]]$residuals$df <- summary[[1]][[1]][[length(summary[[1]][[1]])]]
  res$errorStrata[[1]]$residuals$sumSq <- summary[[1]][[2]][[length(summary[[1]][[2]])]]
  res$errorStrata[[1]]$residuals$meanSq <- summary[[1]][[3]][[length(summary[[1]][[3]])]]
  cat(toJSON(res,pretty=1),file=path)
})


setOldClass("aovlist")
#' @rdname RJSONLD.export-methods
setMethod("RJSONLD.export", "aovlist", function(object, path){
  res <- list()
  res$formula <- deparse(attr(object,'call')$formula)
  res$errorStrata <- list()
  eff <- eff.aovlist(object)
  for (s in 2:length(object)){
    res$errorStrata[[s-1]] <- list()
    res$errorStrata[[s-1]]$error <- names(object)[[s]]
    res$errorStrata[[s-1]]$effects <- list()    
    terms <- colnames(eff)[eff[s-1,]==1]
    summary <- summary(object[[s]])
    if(length(terms)>0){
      for (i in 1:length(terms)){
        res$errorStrata[[s-1]]$effects[[i]]<-list()
        res$errorStrata[[s-1]]$effects[[i]]$term <- terms[i]
        res$errorStrata[[s-1]]$effects[[i]]$df <- summary[[1]]$Df[[i]]
        res$errorStrata[[s-1]]$effects[[i]]$sumSq <- summary[[1]][['Sum Sq']][[i]]
        res$errorStrata[[s-1]]$effects[[i]]$meanSq <- summary[[1]][['Mean Sq']][[i]]
        res$errorStrata[[s-1]]$effects[[i]]$FTest <- list()
        res$errorStrata[[s-1]]$effects[[i]]$FTest$statistic <- summary[[1]][['F value']][[i]]
        res$errorStrata[[s-1]]$effects[[i]]$FTest$dfNum <- summary[[1]]$Df[[i]]
        res$errorStrata[[s-1]]$effects[[i]]$FTest$dfDenom <- summary[[1]][[1]][[length(summary[[1]][[1]])]]
        res$errorStrata[[s-1]]$effects[[i]]$FTest$pValue <- summary[[1]][['Pr(>F)']][[i]]
      }      
    }
    res$errorStrata[[s-1]]$residuals <- list()
    res$errorStrata[[s-1]]$residuals$df <- summary[[1]][[1]][[length(summary[[1]][[1]])]]
    res$errorStrata[[s-1]]$residuals$sumSq <- summary[[1]][[2]][[length(summary[[1]][[2]])]]
    res$errorStrata[[s-1]]$residuals$meanSq <- summary[[1]][[3]][[length(summary[[1]][[3]])]]
  }
  cat(toJSON(res,pretty=1),file=path)
})

setOldClass("htest")
#' @rdname RJSONLD.export-methods
setMethod("RJSONLD.export", "htest", function(object, path){
  res <- list()
  summary <- summary(object)
  if(length(grep("correlation",   object$method))){
    res$covariate1 <- strsplit(object$data.name," and ")[[1]][1]
    res$covariate2 <- strsplit(object$data.name," and ")[[1]][2]
    res$cor <- object$estimate[[1]]
    res$tTest <- list()
    res$tTest$statistic <- object$statistic[[1]]
    res$tTest$df <- object$parameter[[1]]
    res$tTest$pValue <- object$p.value[[1]]
  } else if(length(grep("proportions test",   object$method))){
    res$props <- object$estimate[[1]]
    res$chisqTest <- list()
    res$chisqTest$statistic <- object$statistic[[1]]
    res$chisqTest$df <- object$parameter[[1]]
    res$chisqTest$pValue <- object$p.value[[1]]
  } else {
    res$object <- object$data.name[[1]]
    res$statistic <- object$statistic[[1]]
    res$df <- object$parameter[[1]]
    res$pValue <- object$p.value[[1]]
  }
  cat(toJSON(res,pretty=1),file=path)
})
