FROM ubuntu:20.04

RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y tzdata
# RUN apt-get update && apt-get install -y software-properties-common
RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
    make cmake libcgal-dev g++

COPY src src
RUN mkdir src/build
RUN cd src/build && cmake .. && make