FROM rocker/r-ver:4.2.2

RUN apt-get update && apt-get install -y \
  libcurl4-openssl-dev \
  libglpk-dev \
  libgmp-dev \
  libicu-dev \
  libssl-dev \
  libxml2-dev \
  make \
  pandoc \
  zlib1g-dev

# Build arg to use extra cores
ARG N_CPUS=1

ARG GITHUB_PAT

ENV GITHUB_PAT $GITHUB_PAT

ENV RENV_VERSION 0.15.5
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}', , Ncpus = $N_CPUS)"

WORKDIR /project
# COPY ../../../renv.lock renv.lock
COPY renv.lock renv.lock

ENV RENV_PATHS_LIBRARY renv/library

RUN R -e "options(timeout = 300);renv::restore()"

ENTRYPOINT ["R"]
