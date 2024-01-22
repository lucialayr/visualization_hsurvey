# Use an R base image with a specific R version
FROM rocker/r-ver:4.2.2

# Copy the Dockerfile and set the working directory
COPY . /app
WORKDIR /app

# Install R packages versions that were used for the analysis
RUN R -e "install.packages(c('tidyverse', 'reshape2', 'waffle', 'cowplot', 'ggforce', 'showtext'), version = c('1.3.2', '1.4.4', '0.7.0', '1.1.1', '0.4.1', '3.0'), repos='https://cran.rstudio.com/')"

# Start an interactive shell by default #this should allow to be able to manually execute makefile
CMD ["R"]
