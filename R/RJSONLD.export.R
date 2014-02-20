#################################################################################
#' Export results of standard statistical functions to semantic JSON-LD format
#'
#' Make your results of standard statistical analysis browsable and reproducible 
#' by exporting them into JSON-LD, following a standardized vocabulary 
#' (http://standardanalytics.io/stats). This vocabulary is still at a draft stage:
#' provide feedback, suggestions and extenstions at https://github.com/standard-analytics/terms
#' This module currently supports the current functions: lm, lme, lmer, glm, aov,
#' chis.test, t.test, cor.test, prop.test.
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
               modelFormula = deparse(object$call$formula, width.cutoff = 500L),
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
      statTest = list(
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
      statTest = list(
        `@type` = 'TTest',
        testStatistic = summary$coefficients[i,][[3]]
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
               modelFormula = deparse(object$call$formula, width.cutoff = 500L),
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
               modelFormula = deparse(object$call$formula, width.cutoff = 500L),
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
               modelFormula = deparse(attr(object,'call')$formula, width.cutoff = 500L),
               anova = list()
  )
  cpt = 0
  eff <- eff.aovlist(object)
  for (s in 2:length(object)){
    terms <- colnames(eff)[eff[s-1,]>0]
    summary <- summary(object[[s]])
    if(length(terms)>0){
      print(s)
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
      cpt = cpt + 1
      res$anova[[cpt]] <- list(
        `@type` = 'ANOVAResidual',
        errorStratum = names(object)[[s]],
        sumSq = summary[[1]][[2]][[length(summary[[1]][[2]])]],
        meanSq = summary[[1]][[3]][[length(summary[[1]][[3]])]]
      )
    }  
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



#' @rdname RJSONLD.export-methods
setMethod("RJSONLD.export", "rstan", function(object, path){
  res <- list( `@context` = list( `@vocab` = 'http://standardanalytics.io/stats/'),
               `@type` = 'StatisticalModel',
               modelFormula = object@stanmodel@model_code
  )
  
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
  allPars = names(fit@sim$samples[[1]])
  basePars = list()
  for (i in 1:length(allPars)){
  	if(is.na(str_locate(names(fit@sim$samples[[1]])[i],'\\[')[1])){
  		tmpname = names(fit@sim$samples[[1]])[i]
  	} else {
	  	tmpname = 	substr(names(fit@sim$samples[[1]])[i],1,str_locate(names(fit@sim$samples[[1]])[i],'\\[')[1]-1)  
  	}
  	if(!is.na(str_locate(parDef,tmpname)[1])){
  		basePars = c(basePars,names(fit@sim$samples[[1]])[i])
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
  	prior = list()
  	
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
  	
  	distributionParameters <- list()
  	# get distribution name
  	if (line!=''){
    	indstart = str_locate(line,'~')+1
  		indend = indstart + str_locate(substr(line,indstart,nchar(line)),'\\(')[1]-2
  		priorName = trim(substr(line,indstart,indend))
  		prior <- setNames(priorName,'name')
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
    			tmp <- setNames(c(distributionParametersTerminology[[priorName]][j],args[[1]][j]),c('name','value'))
    			distributionParameters[[length(distributionParameters)+1]] = tmp
       		}
       	} else {
       		'Warning: unrecognized prior distribution. Pull requests welcome'
       	}
  
  	} else {
  		prior <- setNames('uniform','name')
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
    	distributionParameters[[length(distributionParameters)+1]] = tmp
  	} else {
  		tmp = list()
    	tmp <- setNames(c('lower','-inf'),c('name','value'))
    	distributionParameters[[length(distributionParameters)+1]] = tmp
  	}
  	if(!is.na(str_locate(line,'upper=')[1])){
  		tmp = list()
    	tmp <- setNames(c('upper',substr(str_extract(line,'upper(.*)>'),7,nchar(str_extract(line,'upper(.*)>'))-1)),c('name','value'))
    	distributionParameters[[length(distributionParameters)+1]] = tmp
  	} else {
  		tmp = list()
    	tmp <- setNames(c('upper','inf'),c('name','value'))
    	distributionParameters[[length(distributionParameters)+1]] = tmp
  	}

  	prior$distributionParameters <- list()
  	prior$distributionParameters <- distributionParameters
  	
  	basename = basePars[[i]]
  	modelParameter[[i]]$name <- basename
  	modelParameter[[i]]$prior <- prior
  }
  res$modelParameter = modelParameter
  
  
  res$modelFit <- list()
  modelFit = list(
    mcmcAlgorithm = fit@stan_args[[1]]$algorithm
    )
  for (k in 1:length(fit@sim$samples)){
    tmp = as.data.frame(matrix(nrow = length(fit@sim$samples[[1]][[fit@model_pars[1]]]), ncol = npars))
    for (i in 1:npars){
      tmp[,i]=fit@sim$samples[[k]][[basePars[[i]]]]
    }
    write.table(tmp, file=paste(dirname(path),'/mcmcTrace_',as.character(k),'.csv',sep=''),col.names=basePars, sep=",",row.names = F)
    modelFit$mcmcTrace <- list(
        name = paste('mcmcTrace_',as.character(k),sep=''),
		atcontext = list( xsd<- "http://www.w3.org/2001/XMLSchema#")
      )
	for (i in 1:npars){
      modelFit$mcmcTrace$atcontext[[basePars[[i]]]] <- list(
        atid = paste("_:",names(fit@sim$samples[[k]])[i],sep=''),
        attype = "xsd:float"
      )
    }
    modelFit$mcmcTrace$distribution = {
    	contentPath = paste(dirname(path),'/mcmcTrace_',as.character(k),'.csv',sep='')
    }
    modelFit$name <- paste('chain_',as.character(k),sep='')
    res$modelFit[[k]] <- modelFit
  }
  cat(gsub("\t","  ",toJSON(res,pretty=1)),file=path)
})
