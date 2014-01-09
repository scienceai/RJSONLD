RJSONLD
=======

Make your results of standard statistical analysis **browsable** and **transparent** by exporting them into JSON-LD, following a [standardized 
vocabulary](http://standardanalytics.io/stats).

*Version: 1.1*

Dependencies: RJSONIO, formula.tools

Usage
=======
Insert a call to ``RJSONLD.export`` for every result you wish to export, along with
the relative path of the file you wish to create:

	result = lm(iris$Petal.Length~iris$Sepal.Length)
	RJSONLD.export(result,"irisLM.jsonld")

will create the following JSON-LD file:

	{
	  "@context" : { "@vocab" : "http://standardanalytics.io/stats" },
	  "@type" : "LinearModel",
	  "modelFormula" : "iris$Petal.Length ~ iris$Sepal.Length",
	  "r2" : 0.75995,
	  "adjr2" : 0.75833,
	  "fRatioTest" : {
	    "@type" : "FTest",
	    "statistic" : 468.55,
	    "dfNum" : 1,
	    "dfDenom" : 148,
	    "pValue" : 1.0387e-47
	  },
	  "modelCoefficients" : [
	    {
		  "name" : "(Intercept)",
		  "estimate" : -7.1014,
		  "stdError" : 0.50666,
		  "statTest" : {
		    "@type" : "TTest",
		    "statistic" : -14.016,
		    "df" : 148,
		    "pValue" : 6.1336e-29
		  }
	    },
	    {
		  "name" : "iris$Sepal.Length",
		  "estimate" : 1.8584,
		  "stdError" : 0.085856,
		  "statTest" : {
		    "@type" : "TTest",
		    "statistic" : 21.646,
		    "df" : 148,
		    "pValue" : 1.0387e-47
		  }
	    }
	  ]
	}


License 
=======

 MIT