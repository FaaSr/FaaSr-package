read_line_yml <- function(n, file_path){
  con = file(file_path, "r")
  count=1
  while (TRUE) {
    line = readLines(con, n = 1)
    if (count==n){
      close(con)
      return(trimws(line))
    }
    if (length(line)== 0) {
      break
    }
    count=count+1
  }
  close(con)
}

test_that("create_yml_file", {
  ## this function creates a yaml file named with action name
  ## Input is container name and actionname
  
  ## First, it is required to create a directory for workflow file.
  dir_path <- ".github/workflows/"
  dir.create(dir_path, recursive=TRUE)
  
  ## Run the function with arguments
  ## In this example, I'll create yml file with container image "my_image" and action name "my_action"
  container_image <- "my_image"
  action_name <- "my_action"
  faasr_register_workflow_github_create_yml_file(container_image, action_name)
  
  ## check the yml file exists. File name should be "action_name" + ".yml"
  ## In this example, it should be "my_action.yml"
  expect_true(file.exists(".github/workflows/my_action.yml"))
  
  ## Check the line no.19. Line 19 isn't guaranteed to have a container image data but it may have it.
  ## Use the function above: read_line_yml(n, file_path). "file_path" is where the file is and "n" is the line number.
  ## Result should be the character value: "container: my_image"
  result <- read_line_yml(19, ".github/workflows/my_action.yml")
  
  expect_equal(result, "container: my_image")
  
  ####################################################################################################
  ####################################################################################################
  ## Change the name and check.
  container_image <- "your_image"
  action_name <- "your_action"
  faasr_register_workflow_github_create_yml_file(container_image, action_name)
  
  expect_true(file.exists(".github/workflows/your_action.yml"))
  
  result <- read_line_yml(19, ".github/workflows/your_action.yml")
  expect_equal(result, "container: your_image")
  
  
  ####################################################################################################
  ####################################################################################################
  
  ## After all the test, remove the file & directory
  unlink(".github", recursive=TRUE, force=TRUE)
})
