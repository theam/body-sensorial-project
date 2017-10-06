FROM haskell:8

WORKDIR /app

ADD . /app

RUN sed -i "s/jessie main/jessie main contrib non-free/" /etc/apt/sources.list
RUN echo "deb http://http.debian.net/debian jessie-backports main contrib non-free" >> /etc/apt/sources.list
RUN apt-get -qq update\
    && apt-get install -y \
        python \
        python-pip \
        python-dev \
        gfortran \
        ffmpeg \
        sox \
        curl \
        libgmp-dev \
        libblas-dev \
        liblapack-dev \
        xz-utils \
        build-essential \
    && pip install numpy scipy pandas \
    && stack setup

RUN stack build
RUN stack exec bsp -- combine --dataDirectory resources/data

EXPOSE 8081

CMD stack exec bsp -- serve --port 8081

