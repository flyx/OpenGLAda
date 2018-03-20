GNATFLAGS ?=
GLFW_VERSION ?=3
GPRBUILD = gprbuild ${GNATFLAGS} -p

WINDOWING_BACKEND := windows
GENERATE_EXE := bin/generate.exe
UNAME := $(shell uname)
ifeq ($(UNAME), Darwin)
  WINDOWING_BACKEND := quartz
  GENERATE_EXE := bin/generate
endif
ifeq ($(UNAME), Linux)
  WINDOWING_BACKEND := x11
  GENERATE_EXE := bin/generate 
endif

WINDOWING_SYSTEM := -XWindowing_System=${WINDOWING_BACKEND}
GLFW_VERSION := -XGLFW_Version=${GLFW_VERSION}
LIBRARY_TYPE ?= static

all: compile

${GENERATE_EXE}: src/generator/generate.adb src/generator/specs.adb \
                 src/generator/specs.ads src/generator/tokenization.adb \
                 src/generator/tokenization.ads
	${GPRBUILD} -P generate.gpr

generate: ${GENERATE_EXE}
	${GENERATE_EXE}

compile:
	mkdir -p lib
	mkdir -p obj
	${GPRBUILD} -P openglada.gpr ${WINDOWING_SYSTEM} ${GLFW_VERSION}

install: compile
	${GPRINSTALL} openglada.gpr ${WINDOWING_SYSTEM} ${GLFW_VERSION}

clean:
	rm -rf ./obj ./bin ./lib

tests:
	mkdir -p bin
	${GPRBUILD} -P opengl-glfw-test.gpr ${WINDOWING_SYSTEM} ${GLFW_VERSION}
	${GPRBUILD} -P opengl-test.gpr ${WINDOWING_SYSTEM} ${GLFW_VERSION}

.PHONY: generate compile install clean tests
