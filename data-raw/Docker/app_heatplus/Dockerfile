FROM heat-app-base:0.1

# Build arg to use extra cores
ARG N_CPUS=1

#### App as Package ####
WORKDIR /usr/src/app
COPY '/HEAT-development' /heat
COPY '/HEAT-Plus-development' /heatplus


RUN R -e "install.packages('remotes', Ncpus = $N_CPUS)"

RUN R -e "remotes::install_local('/heat', upgrade = 'never', Ncpus = $N_CPUS)"
RUN R -e "remotes::install_local('/heatplus', upgrade = 'never', Ncpus = $N_CPUS)"

COPY '/HEAT-Plus-development/config.yml' config.yml

EXPOSE 3838

CMD ["Rscript", "-e", "library(heatplus); options(heat.plus.portable = TRUE); heatplus::heatPlusApp(launch.browser = FALSE)"]
