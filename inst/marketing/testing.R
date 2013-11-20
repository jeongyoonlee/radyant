require(shiny)

output <- list()

setwd('/Users/vnijs/radyant/inst/marketing')
source('global.R', local = TRUE)
source('server.R', local = TRUE)
source('radyant.R', local = TRUE)
source('ui.R', local = TRUE)

	# source data & analysis tools
flist_analysis <- sourceDirectory('tools/analysis', recursive = TRUE)
flist_data <- sourceDirectory('tools/data', recursive = TRUE)

output$tester <- reactive({
	regression()
})

output$tester()


# test <- matrix(rnorm(400),ncol = 4)
# test <- cbind(test,test) + matrix(rnorm(800, 0, .3), ncol = 8)
# cor(test)
# test <- data.frame(test)
# colnames(test) <- paste0('X',1:8)
# head(test)
# save(test, file = '~/Desktop/test.rda')


# might the below work for testing?
# just load some parts of the app and make sure the work as expected 
# with specific data and input parameters?
library(shiny)
runApp(list(
  ui = pageWithSidebar(
    # ...
  ),
  server = function(input, output, session) {
    # ...
  }
))


# maybe combined with stopApp()
# https://groups.google.com/forum/#!msg/shiny-discuss/QegGiX9qBcg/d00NvOIKWRYJ

# the url parse thing also seems to offer the opportunity to (re)set a bunch of parameters in the input 
# list


example(mean)