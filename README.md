# crispr-screen
Project from course Advanced bioinformatics (5MO127) at Umeå University.  
The purpose of this project is to analyze and optimize  sgRNA design for CRISPR knockout screening.  
Authored by Alfred Lindqvist

Software used:
RStudio 2025.05.0+496
R 4.4.3


## Installation 
To install the package from Github:
 1. Copy the URL from the repository
 2. Open a terminal and navigate to an appropriate directory
 3. Run the following command

```bash
git clone URL
```

### Local installation
Insert info

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

