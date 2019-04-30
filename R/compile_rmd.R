## ADB


## Compile Rmarkdown report

# compile_rmd function ----
compile_rmd <- function(file) {
  rmarkdown::render(
    input = here('rmds', paste(file, '.rmd', sep = '')),
    output_file = here('reports', paste(file, '.html', sep = ''))
  )
}
