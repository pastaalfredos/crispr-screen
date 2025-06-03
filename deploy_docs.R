# MAKE SURE TO RUN THIS FILE WHILE IN WD crispr-screen
# Remove crispr-screen/docs before running

# Load pkgdown
if (!requireNamespace("pkgdown", quietly = TRUE)) {
  install.packages("pkgdown")
}
library(pkgdown)

# Define paths
root_dir <- normalizePath(".")
pkg_dir <- "dataprocessor"
docs_src <- file.path(pkg_dir, "docs")
docs_dest <- file.path(root_dir, "docs")

# Build site
pkgdown::build_site(pkg = pkg_dir)

# Move new docs to root
file.rename(docs_src, docs_dest)

# Remove leftover empty folder
if (dir.exists(docs_src)) {
  unlink(docs_src, recursive = TRUE)
}

# Git commit
system("git add docs")
system('git commit -m "Auto-update pkgdown site"')
system("git push")
