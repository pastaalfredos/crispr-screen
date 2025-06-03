# crispr-screen
Project from course Advanced bioinformatics (5MO127) at Umeå University.  
The purpose of this project is to develop a model to analyze and optimize sgRNA design for CRISPR knockout screening.  

Authored by Alfred Lindqvist

Software used:  
RStudio 2025.05.0+496  
R 4.4.5 

🧬 **RShiny App (last update Jun 2 2025)**: [Visit the website to view the model's progress!](https://pastaalfredos.shinyapps.io/crispr-app/)

## Installation 
To install the package from Github:
 1. Copy the URL from the repository
 2. Open a terminal and navigate to an appropriate directory
 3. Run the following command

```bash
git clone URL
```

### Local installation
Install the required R packages by running:
```bash
Rscript requirements.R
```

To install the development version:
```bash
install.packages("devtools")
devtools::install_github("pastaalfredos/crispr-screen", subdir = "dataprocessor")
```

### How to use
The script for the machine learning pipeline is ran through *dataprocessor/R/rf_model.R*. This returns a file *results.rds* that contains various model data.  
Draw the same plots as shown in the RShiny app by running *dataprocessor/R/model_eval.R*.  
If you want to modify the data used in the model, you can generate a new file *ml_data.rda* in the script *dataprocessor/R/data_prep.R*.

#### Experimental features
Alternatively, try running an elastic net model in *dataprocessor/R/net_model.R*. This is currently unoptimized. 

### Docker
Containerize the R package using Docker. A Dockerfile can be found in the downloaded repository.

#### Build Docker image
To build the Docker image, run the following command in the project root directory.
```bash
docker build -t crisper-ko-screening .
```

#### Run the container
To run the container, run the following commands.
```bash
docker run -d 
-p 8787:8787
-e PASSWORD=YOURPASSWORD
crisper-ko-screening
```

#### RStudio server access
Navigate to http://localhost:8787 
The username is by default set to rstudio.

### Data
You can download the sgRNA data sets used in the project from the following links:
```bash
https://static-content.springer.com/esm/art%3A10.1186%2Fs13059-020-1940-8/MediaObjects/13059_2020_1940_MOESM3_ESM.xlsx
https://media.addgene.org/cms/filer_public/a4/b8/a4b8d181-c489-4dd7-823a-fe267fd7b277/human_geckov2_library_a_09mar2015.csv
https://media.addgene.org/cms/filer_public/2d/8b/2d8baa42-f5c8-4b63-9c6c-bd98f333b29e/human_geckov2_library_b_09mar2015.csv
```