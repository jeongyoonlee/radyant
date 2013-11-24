# variable selection in the datatabs views
output$vizvars1 <- renderUI({
	cols <- varnames()
	if(is.null(cols)) return()
	# selectInput(inputId = "vizvars1", label = "X-variable", choices = as.list(cols), selected = NULL, multiple = FALSE)
	selectInput(inputId = "vizvars1", label = "X-variable", choices = as.list(cols), selected = names(cols)[1], multiple = input$viz_multiple)
})

# variable selection
output$vizvars2 <- renderUI({
	cols <- varnames()
	if(is.null(cols)) return()
	if(is.null(input$vizvars1)) return()

	dat <- getdata()
	# if(!input$vizvars1 %in% colnames(dat)) return()
	if(sum(input$vizvars1 %in% colnames(dat)) != length(input$vizvars1)) return()
	if(is.Date(dat[,input$vizvars1])) {
		# selectInput(inputId = "vizvars2", label = "Y-variable", choices = as.list(cols[-which(cols == input$vizvars1)]), selected = "", multiple = TRUE)
		selectInput(inputId = "vizvars2", label = "Y-variable", choices = cols, selected = "", multiple = TRUE)
	} else {
		# selectInput(inputId = "vizvars2", label = "Y-variable", choices = c("None" = "",as.list(cols[-which(cols == input$vizvars1)])), selected = "", multiple = FALSE)
		# selectInput(inputId = "vizvars2", label = "Y-variable", choices = c("None" = "",as.list(cols)), selected = "", multiple = input$viz_multiple)
		# selectInput(inputId = "vizvars2", label = "Y-variable", choices = c("None" = "",as.list(cols)), selected = "None", multiple = input$viz_multiple)
		selectInput(inputId = "vizvars2", label = "Y-variable", choices = c("None" = "",as.list(cols)), selected = "", multiple = FALSE)
	}
})

output$viz_color <- renderUI({
	cols <- varnames()
	if(is.null(cols)) return()
	if(length(input$vizvars2) > 1) return()
	selectInput('viz_color', 'Color', c('None'="", as.list(cols)))
})

output$viz_facet_row <- renderUI({
	cols <- varnames()
	if(is.null(cols)) return()
	if(length(input$vizvars2) > 1) return()
	# isFct <- sapply(getdata(), is.factor || is.integer)
	isFct <- sapply(getdata(), is.factor)
 	cols <- cols[isFct]
	selectInput('viz_facet_row', 'Facet row', c(None='.', as.list(cols)))
})

output$viz_facet_col <- renderUI({
	cols <- varnames()
	if(is.null(cols)) return()
	if(length(input$vizvars2) > 1) return()
	# isFct <- sapply(getdata(), is.factor || is.integer)
	isFct <- sapply(getdata(), is.factor)
 	cols <- cols[isFct]
	selectInput('viz_facet_col', 'Facet col', c(None='.', as.list(cols)))
})

ui_Visualize <- function() {
	list(wellPanel(
		  checkboxInput('viz_multiple', 'Multiple plots', value = FALSE),
			uiOutput("vizvars1"),
			uiOutput("vizvars2"),
			conditionalPanel(condition = "input.vizvars2 != ''",
			  uiOutput("viz_color"),
				conditionalPanel(condition = "input.viz_multiple == false",
				  uiOutput("viz_facet_row"),
				  uiOutput("viz_facet_col")
				),
			  checkboxInput('viz_line', 'Line', value = FALSE),
			  checkboxInput('viz_loess', 'Loess', value = FALSE),
			  checkboxInput('viz_jitter', 'Jitter', value = FALSE)
			),
	    returnTextInput("viz_select", "Subset (e.g., mpg > 20 & vs == 1)", ''),
			div(class="row-fluid",
	    	div(class="span6",numericInput("viz_plot_height", label = "Plot height:", min = 100, step = 50, value = 650)),
	      div(class="span6", numericInput("viz_plot_width", label = "Plot width:", min = 100, step = 50, value = 650))
	    )
		),
	  helpModal('Visualize','visualize',includeMarkdown("tools/help/visualize.md"))
  )
}

viz_plot_width <- function() {
	if(is.null(input$viz_plot_width)) return(650)
 	input$viz_plot_width
}

viz_plot_height <- function() {
	if(is.null(input$viz_plot_height)) return(650)
	if(input$viz_multiple) { 
		nrPlots <- length(input$vizvars1)
		ifelse(nrPlots > 1, return(325 * ceiling(nrPlots / 2)), return(650))
	} else {
 		return(input$viz_plot_height)
	}
}

output$visualize <- renderPlot({
	if(is.null(input$datasets) || is.null(input$vizvars1) || is.null(input$vizvars2)) return()
	if(input$datatabs != 'Visualize') return()

		# inspired by Joe Cheng's ggplot2 browser app http://www.youtube.com/watch?feature=player_embedded&v=o2B5yJeEl1A#!
	dat <- getdata()
	# if(!input$vizvars1 %in% colnames(dat)) return()
	if(sum(input$vizvars1 %in% colnames(dat)) != length(input$vizvars1)) return()

  if(input$viz_select != '') {
    selcom <- input$viz_select
    selcom <- gsub(" ", "", selcom)
    seldat <- try(do.call(subset, list(dat,parse(text = selcom))), silent = TRUE)
    if(!is(seldat, 'try-error')) {
      if(is.data.frame(seldat)) {
        dat <- seldat
        seldat <- NULL
      }
    }
  }

	# dat <- mtcars
	# input <- list()
	# input$vizvars1 <- colnames(dat)[1:4]
	# sum(input$vizvars1 %in% colnames(dat))
	# length(input$vizvars1)

	plots <- list()

	if(input$vizvars2 == "") {

		# p <- ggplot(dat, aes_string(x=input$vizvars1)) + geom_histogram(fill = 'blue', alpha=.3) 
		# p <- ggplot(dat, aes_string(x=input$vizvars1)) + geom_histogram(alpha=.3) 
		for(i in input$vizvars1) 
			plots[[i]] <- ggplot(dat, aes_string(x=i)) + geom_histogram()

			# plots[[i]] <- ggplot(dat, aes_string(x=i)) + geom_histogram(colour = 'black', fill = 'blue', alpha = .1)
			# plots[[i]] <- ggplot(dat, aes_string(x=input$vizvars1)) + geom_histogram(colour = 'black', fill = 'blue', alpha = .1)
			# plots[[i]] <- ggplot(dat, aes_string(x = i)) + geom_histogram()

			# updateCheckboxInput(session = session, inputId = "viz_jitter", label = "Jitter", value = TRUE)
			# if(length(plots) > 1) {
			# 	plotHeight <- 325 * ceiling(length(plots) / 2)
  		#  	updateNumericInput(session = session, "viz_plot_height", label = "Plot height:", min = 100, step = 50, value = plotHeight)
  		# }

		p <- suppressMessages(do.call(grid.arrange, c(plots, list(ncol = min(2,length(plots))))))

   		# return(suppressWarnings(suppressMessages(print(p))))
	} else {

		# if(!input$vizvars2 %in% colnames(dat)) return()
		if(sum(input$vizvars2 %in% colnames(dat)) != length(input$vizvars2)) return()

		for(i in input$vizvars1) {
			for(j in input$vizvars2) { 

				# if(is.Date(dat[,input$vizvars1[1]])) {
				# if(is.Date(dat[,i]])) {

				# 	if(length(input$vizvars2) > 1) {
				# 		mdat <- melt(dat[,c(input$vizvars1,input$vizvars2)],id=input$vizvars1)
				# 		p <- ggplot(mdat,aes_string(x=input$vizvars1,y="value",colour="variable",group="variable")) + geom_line()
				# 	} else {
				# 	  p <- ggplot(dat, aes_string(x=input$vizvars1, y=input$vizvars2)) + geom_line()
				# 	}
				# } else {

					# if(is.factor(dat[,input$vizvars1])) {
					if(is.factor(dat[,i])) {
					 	# updateCheckboxInput(session = session, inputId = "viz_jitter", label = "Jitter", value = TRUE)
					 	# updateCheckboxInput(session = session, inputId = "viz_line", label = "Line", value = FALSE)
					 	# updateCheckboxInput(session = session, inputId = "viz_loess", label = "Loess", value = FALSE)
					  # p <- ggplot(dat, aes_string(x=i, y=j, fill=i)) + geom_boxplot(alpha = .3)

					  if(is.factor(dat[,j])) {
					  	plots[[i]] <- ggplot(dat, aes_string(x=i, fill=j)) + geom_bar(position = "fill", alpha=.3) + 
					  		labs(list(y = ""))
					  } else {
						  plots[[i]] <- ggplot(dat, aes_string(x=i, y=j, fill=i)) + geom_boxplot(alpha = .3)
						}
				  } else if(is.factor(dat[,j])) {

					 	# updateCheckboxInput(session = session, inputId = "viz_line", label = "Line", value = FALSE)
					 	# updateCheckboxInput(session = session, inputId = "viz_loess", label = "Loess", value = FALSE)

					  plots[[i]] <- ggplot(dat, aes_string(x=j, y=i, fill=j)) + geom_boxplot(alpha = .3) +
					 	 coord_flip()

				  } else {
				  	# p <- ggplot(dat, aes_string(x=input$vizvars1, y=input$vizvars2)) + geom_point()
					  plots[[i]] <- ggplot(dat, aes_string(x=i, y=j)) + geom_point()
				  }
				# }

		    # if (input$viz_color != '') {
				  # plots[[i]] <- plots[[i]] + aes_string(color=input$viz_color) + scale_fill_brewer()
		    # }

		    if(!input$viz_multiple) {
			    facets <- paste(input$viz_facet_row, '~', input$viz_facet_col)
			    if (facets != '. ~ .')
					  plots[[i]] <- plots[[i]] + facet_grid(facets)
				}
		    
				if(!(is.factor(dat[,i]) & is.factor(dat[,j]))) {
			    if (input$viz_jitter) plots[[i]] <- plots[[i]] + geom_jitter()
				} 

				if(!is.factor(dat[,i]) & !is.factor(dat[,j])) {
			    if (input$viz_color != '') plots[[i]] <- plots[[i]] + aes_string(color=input$viz_color) + scale_fill_brewer()
			    if (input$viz_line) plots[[i]] <- plots[[i]] + geom_smooth(method = "lm", fill = 'blue', alpha = .1, size = .75, linetype = "dashed", colour = 'black')
			    if (input$viz_loess) plots[[i]] <- plots[[i]] + geom_smooth(span = 1, size = .75, linetype = "dotdash")
			    if (input$viz_jitter) plots[[i]] <- plots[[i]] + geom_jitter()
			  } 

			}
		}

		p <- suppressMessages(do.call(grid.arrange, c(plots, list(ncol = min(2,length(plots))))))
	}

  suppressWarnings(suppressMessages(print(p)))

}, width = viz_plot_width, height = viz_plot_height)

