FROM rocker/r-ver:4.4.1

RUN apt-get update -qq && apt-get install -y ;libssl-dev libcurl4-gnutls-dev

RUN R -e "install.packages('tidyverse')"
RUN R -e "install.packages('plumber')"
RUN R -e "install.packages('caret')"

COPY API.R API.R

EXPOSE 8000

ENTRYPOINT ["R", "-e",\
            "pr<-plumber::plumb('API.R'); pr$run(host='0.0.0.0',port=8000)"]