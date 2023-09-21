FROM ubuntu:20.04

ARG NUM_BUILD_CORES

RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y tzdata
RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
    cmake g++ git make python3

RUN git config --global http.sslverify false
RUN git clone https://github.com/aibasel/downward.git downward

RUN cd downward && ./build.py -j${NUM_BUILD_CORES}
RUN echo "export PS1=\"CS695.P3: \\w $ \""  >> ~/.bashrc

# Get the benchmark pddl files
RUN git clone https://github.com/aibasel/downward-benchmarks.git downward/pddl-benchmark-examples

WORKDIR /downward