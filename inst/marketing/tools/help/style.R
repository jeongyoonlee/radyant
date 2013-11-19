options(rstudio.markdownToHTML =
  function(inputFile, outputFile) {     
    require(markdown)
    markdownToHTML(inputFile, outputFile, options = c(""), stylesheet='mymd.css')  
  }
)

# require(knitr)
# helpfiles <- list.files(".", pattern = "*.Rmd")
# for(hf in helpfiles) {
# 	knit2html(hf, options = "", stylesheet = "mymd.css")
# }
