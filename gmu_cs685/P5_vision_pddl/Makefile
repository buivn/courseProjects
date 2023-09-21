## ==== Core Arguments and Parameters ====
MAJOR ?= 0
MINOR ?= 1
VERSION = $(MAJOR).$(MINOR)
APP_NAME ?= ppddl
NUM_BUILD_CORES ?= $(shell grep -c ^processor /proc/cpuinfo)
NUM_BUILD_CORES := $(if $(NUM_BUILD_CORES),$(NUM_BUILD_CORES),4)

# Docker args
DOCKER_FILE_DIR = "."
DOCKERFILE = ./Dockerfile
IMAGE_NAME = ${APP_NAME}
PDDL_DIR = $(shell pwd)/pddl

.PHONY: build
build:
	@echo "Building the Docker container ($(NUM_BUILD_CORES) threads)"
	# See here for security privelage details: https://askubuntu.com/a/1264921
	@docker build -t ${IMAGE_NAME}:${VERSION} \
		--build-arg NUM_BUILD_CORES=$(NUM_BUILD_CORES) \
		-f ./${DOCKERFILE} .

.PHONY: rebuild
rebuild:
	@docker build -t ${IMAGE_NAME}:${VERSION} --no-cache \
		--build-arg NUM_BUILD_CORES=$(NUM_BUILD_CORES) \
		-f ./${DOCKERFILE} .

.PHONY: downward
downward: build
	@docker run --init -it --rm \
		--volume="$(PDDL_DIR):/downward/pddl:rw" \
		${IMAGE_NAME}:${VERSION} \
		bash

.PHONY: shell
shell:
	@docker run --init -it --rm \
		${IMAGE_NAME}:${VERSION} \
		bash
