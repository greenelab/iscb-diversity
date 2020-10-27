rnotebooks <- list.files(path = ".", pattern = '.Rmd')

for (notebooki in rnotebooks){
  print(notebooki)
  rmarkdown::render(notebooki, 
                    output_dir = 'docs',
                    output_yaml = '_output.yaml')  
}
