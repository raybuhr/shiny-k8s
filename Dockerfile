FROM rocker/shiny

RUN rm -rf /srv/shiny-server/*

RUN apt-get update && apt-get install -y \
    libssl-dev \
    libxml2-dev

RUN install2.r --error \
    ggplot2 \
    data.table

COPY wine-explorer /srv/shiny-server

CMD ["/usr/bin/shiny-server.sh"]
