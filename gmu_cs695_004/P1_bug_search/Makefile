## ==== Core Arguments and Parameters ====
MAJOR ?= 1
MINOR ?= 0
VERSION = $(MAJOR).$(MINOR)
APP_NAME ?= cs695-p01-cgal
NUM_BUILD_CORES ?= $(shell grep -c ^processor /proc/cpuinfo)

# Docker args
DOCKER_FILE_DIR = "."
DOCKERFILE = ./Dockerfile
IMAGE_NAME = ${APP_NAME}

.PHONY: build
build:
	@echo "Building the Docker container"
	@docker build -t ${IMAGE_NAME}:${VERSION} \
		--build-arg NUM_BUILD_CORES=$(NUM_BUILD_CORES) \
		-f ./${DOCKERFILE} .

.PHONY: rebuild
rebuild:
	@docker build -t ${IMAGE_NAME}:${VERSION} --no-cache \
		--build-arg NUM_BUILD_CORES=$(NUM_BUILD_CORES) \
		-f ./${DOCKERFILE} .

.PHONY: run-cgal
run-cgal: build
	@echo "Running CGAL..."
	@rm -rf results && mkdir -p results
	@docker run --init -it --rm \
		--volume="$(shell pwd)/results:/results/:rw" \
		${IMAGE_NAME}:${VERSION} \
		/bin/bash -c "cd /src && build/vd_2_point_location > /results/voronoi_lines.txt"
	@echo "Done generating Voronoi lines."

.PHONY: shell
shell:
	@docker run --init -it --rm \
		${IMAGE_NAME}:${VERSION} \
		bash
