GNATFLAGS ?=
GPRBUILD = gprbuild ${GNATFLAGS} -p

GL_BACKEND := windows
UNAME := $(shell uname)
ifeq ($(UNAME), Darwin)
  GL_BACKEND := mac
endif
ifeq ($(UNAME), Linux)
  GL_BACKEND := linux
endif

compile:
	mkdir -p lib
	mkdir -p obj
	${GPRBUILD} -P glfw.gpr -XGL_Backend=${GL_BACKEND}

all: compile

clean:
	rm -rf ./obj ./bin ./lib

tests:
	mkdir -p bin
	${GPRBUILD} -P glfw_test.gpr -XGL_Backend=${GL_BACKEND}
	${GPRBUILD} -P opengl_test.gpr -XGL_Backend=${GL_BACKEND}

.PHONY: tests
