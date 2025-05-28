# Build a RStudio-based image
FROM rocker/tidyverse

# Set working directory
WORKDIR /home/rstudio/crisper-ko-project


# Copy R dependency files into the container
COPY requirements.R ./
# Copy R files into the container
COPY ./dataprocessor ./dataprocessor

# Install R packages
RUN Rscript requirements.R


