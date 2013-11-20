## Business analytics using Shiny

Interactive business analytics using [R](http://www.r-project.org/) and [Shiny](http://www.rstudio.com/shiny/). Developed by Vincent Nijs (vnijs at rady.ucsd.edu). 

### Todo:

- Use dplyr to explore and summarize data
- Use knitr to log analysis output
- etc. etc.

### Install the marketing analytics app to run locally

- Required: [R](http://cran.rstudio.com/), version 3.0.2 or later
- Required: [Shiny](http://www.rstudio.com/shiny/), version 0.8.0 or later
- Required: A modern browser (e.g., Chrome, Firefox, or Safari). Internet Explorer is not supported.
- Suggested: [Rstudio](http://www.rstudio.com/ide/download/)

To get the app click the 'Download ZIP' button and unzip the file to, for example, your Desktop. When you start the app for the first time a number of required packages will be installed. To start the app, copy and paste the command below into the R(studio) terminal (assuming you unzipped to your Desktop):

	# on windows
	shiny::runApp('~/../Desktop/radyant/')

 	# on mac
	shiny::runApp('~/Desktop/radyant/')

### License

The radyant package is licensed under the GPLv3. See the files listed below for additional details.

- COPYING - radyant package license (GPLv3)
- NOTICE - Copyright notices for additional included software
