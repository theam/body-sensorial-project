FROM ubuntu:16.04

WORKDIR /app

ADD . /app

# RUN apt-get install software-properties-common
# RUN add-apt-repository ppa:mc3man/trusty-media
# RUN apt-get update
RUN set -x \
    && apt-get -qq update \
    && apt-get install -y software-properties-common \
    && add-apt-repository ppa:mc3man/xerus-media \
    && apt-get update \
    && apt-get dist-upgrade \
    && apt-get install -y --no-install-recommends \
        ffmpeg \
        sox \
        r-base \
        curl \
    && curl -sSL https://get.haskellstack.org/ | sh \
    && stack setup \
    && R -e "install.packages('data.table', repos='http://cran.rstudio.com/')" \
    && R -e "install.packages('zoo', repos='http://cran.rstudio.com/')" \
    && cd data-combinator \
    && stack build \
    && stack install cabal-install \
    && stack exec data-combinator -- all \
    && cd ../visualizer-server \
    && stack build

EXPOSE 8081

CMD ["cd visualizer-server && stack exec visualizer-server"]
