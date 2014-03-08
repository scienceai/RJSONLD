RJSONLD
=======

Make your results of standard statistical analysis **browsable** and **reproducible** by exporting them into JSON-LD, following a [standardized
vocabulary](http://standardanalytics.io/stats).

*Version: 1.0.3*

Dependencies: RJSONIO, formula.tools, stringr, rstan, lmtest

Usage
=======
Insert a call to ``RJSONLD.export`` for every result you wish to export, along with
the relative path of the file you wish to create:

	result = lm(iris$Petal.Length~iris$Sepal.Length)
	RJSONLD.export(result,"irisLM.jsonld")

will create the following JSON-LD file:

	{
	  "@context" : {
	    "@vocab" : "http://standardanalytics.io/stats/"
	  },
	  "@type" : "LinearModel",
	  "modelFormula" : "iris$Petal.Length ~ iris$Sepal.Length",
	  "modelVariable" : [
	    {
	      "@type" : "Variable",
	      "name" : "iris$Petal.Length"
	    },
	    {
	      "@type" : "Variable",
	      "name" : "iris$Sepal.Length"
	    }
	  ],
	  "modelFit" : {
	    "@type" : "FitnessOptimization",
	    "fitnessCriterion" : "least squares",
	    "r2" : 0.75995,
	    "adjr2" : 0.75833,
	    "nData" : 150,
	    "nParameters" : 2,
	    "fTest" : {
	      "@type" : "FTest",
	      "testStatistic" : 468.55,
	      "dfNum" : 1,
	      "dfDenom" : 148,
	      "pValue" : 1.0387e-47
	    },
	    "optimalParameter" : [
	      {
	        "@type" : "Parameter",
	        "name" : "(Intercept)",
	        "estimate" : {
	          "@type" : "Statistic",
	          "value" : -7.1014,
	          "standardError" : 0.50666,
	          "statisticalTest" : {
	            "@type" : "TTest",
	            "testStatistic" : -14.016,
	            "df" : 148,
	            "pValue" : 6.1336e-29
	          }
	        }
	      },
	      {
	        "@type" : "Parameter",
	        "name" : "iris$Sepal.Length",
	        "estimate" : {
	          "@type" : "Statistic",
	          "value" : 1.8584,
	          "standardError" : 0.085856,
	          "statisticalTest" : {
	            "@type" : "TTest",
	            "testStatistic" : 21.646,
	            "df" : 148,
	            "pValue" : 1.0387e-47
	          }
	        }
	      }
	    ]
	  },
	  "fitResidual" : [
	    {
	      "@type" : [
	        "Statistic",
	        "Min",
	        "Quantile"
	      ],
	      "name" : "Min",
	      "value" : -2.4775,
	      "percentile" : 0
	    },
	    {
	      "@type" : [
	        "Statistic",
	        "Quantile"
	      ],
	      "name" : "1Q",
	      "value" : -0.59072,
	      "percentile" : 25
	    },
	    {
	      "@type" : [
	        "Statistic",
	        "Median",
	        "Quantile"
	      ],
	      "name" : "Median",
	      "value" : -0.0066844,
	      "percentile" : 50
	    },
	    {
	      "@type" : [
	        "Statistic",
	        "Quantile"
	      ],
	      "name" : "3Q",
	      "value" : 0.60484,
	      "percentile" : 75
	    },
	    {
	      "@type" : [
	        "Statistic",
	        "Max",
	        "Quantile"
	      ],
	      "name" : "Max",
	      "value" : 2.4951,
	      "percentile" : 100
	    }
	  ]
	}




License
=======

 MIT
