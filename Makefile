GNATFLAGS ?=
GLFW_VERSION ?=3
GPRBUILD = gprbuild ${GNATFLAGS} -p

WINDOWING_BACKEND := windows
UNAME := $(shell uname)
ifeq ($(UNAME), Darwin)
  WINDOWING_BACKEND := quartz
endif
ifeq ($(UNAME), Linux)
  WINDOWING_BACKEND := x11
endif

WINDOWING_SYSTEM := -XWindowing_System=${WINDOWING_BACKEND}
GLFW_VERSION := -XGLFW_Version=${GLFW_VERSION}
LIBRARY_TYPE ?= static

all: compile

bin/generate: src/generator/generate.adb src/generator/specs.adb \
              src/generator/specs.ads src/generator/tokenization.adb \
							src/generator/tokenization.ads
	${GPRBUILD} -P generate.gpr

generate: bin/generate
	bin/generate

compile:
	mkdir -p lib
	mkdir -p obj
	${GPRBUILD} -P opengl-glfw.gpr ${WINDOWING_SYSTEM} ${GLFW_VERSION}

clean:
	rm -rf ./obj ./bin ./lib

tests:
	mkdir -p bin
	${GPRBUILD} -P glfw_test.gpr ${WINDOWING_SYSTEM} ${GLFW_VERSION}
	${GPRBUILD} -P opengl_test.gpr ${WINDOWING_SYSTEM} ${GLFW_VERSION}

.PHONY: generate compile clean tests