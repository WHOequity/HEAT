FROM heat-app-base:0.1

# Build arg to use extra cores
ARG N_CPUS=1

#### App as Package ####
COPY '.' /HEAT

RUN R -e "install.packages('remotes', Ncpus = $N_CPUS)"

RUN R -e "remotes::install_local('/HEAT', upgrade = 'never', Ncpus = $N_CPUS)"

EXPOSE 8080

CMD ["Rscript", "-e", "library(heat); heat::heatApp(launch.browser = FALSE)"]

