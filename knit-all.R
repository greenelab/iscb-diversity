# local_path <- '.' # without docker
local_path <- 'repo' # with docker

# on docker:
rnotebooks <- list.files(path = local_path, 
                         pattern = '.Rmd',
                         full.names = TRUE)

for (notebooki in rnotebooks){
  print(notebooki)
  rmarkdown::render(notebooki, 
                    output_dir = file.path(local_path, 'docs'),
                    output_yaml = '_output.yaml')  
}

# beepr::beep()
