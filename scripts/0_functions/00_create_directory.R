# Create a new directory and print new directory name 
create_directory <- function(directory_path) {
  if (!file.exists(directory_path)) {
    dir.create(directory_path, recursive = TRUE)
    cat("Directory created:", directory_path, "\n")
  }
}