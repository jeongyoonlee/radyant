options(rstudio.markdownToHTML =
  function(inputFile, outputFile) {     
    require(markdown)
    markdownToHTML(inputFile, outputFile, stylesheet='mymd.css')  
  }
)

helpfiles <- list.files(".",pattern = "*.Rmd")

for(hf in helpfiles) {
	knit2html(hf, options = "")
}
