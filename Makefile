ifndef PREFIX
  PREFIX=$(dir $(shell dirname `which gnatls`))
endif
LIBDIR ?= ${PREFIX}/lib
DESTDIR ?=
GNATFLAGS ?=
ADA_PROJECT_DIR ?= ${PREFIX}/lib/gnat
GPRBUILD = gprbuild ${GNATFLAGS} -p

GL_BACKEND := Windows
UNAME := $(shell uname)
ifeq ($(UNAME), Darwin)
  GL_BACKEND := MacOSX
endif
ifeq ($(UNAME), Linux)
  GL_BACKEND := Linux
endif

compile:
	mkdir -p lib
	mkdir -p obj
	${GPRBUILD} -P glfw.gpr -XGL_Backend=${GL_BACKEND}

uninstall:
	rm -rf ${DESTDIR}/${PREFIX}/include/openglada ${DESTDIR}/${LIBDIR}/openglada ${DESTDIR}/${ADA_PROJECT_DIR}/opengl.gpr ${DESTDIR}/${ADA_PROJECT_DIR}/glfw.gpr

install: compile uninstall
	mkdir -p ${DESTDIR}/${PREFIX}/include/openglada
	mkdir -p ${DESTDIR}/${LIBDIR}/openglada
	mkdir -p ${DESTDIR}/${ADA_PROJECT_DIR}

	cp -r lib/* ${DESTDIR}/${LIBDIR}/openglada

	cp -f src/cl.ad* ${DESTDIR}/${PREFIX}/include/openglada
	cp -f src/cl-*.ad* ${DESTDIR}/${PREFIX}/include/openglada
	chmod -w ${DESTDIR}/${PREFIX}/include/openglada/*.ad?
	cp glfw.gpr ${DESTDIR}/${ADA_PROJECT_DIR}
	cp opengl.gpr ${DESTDIR}/${ADA_PROJECT_DIR}
all: compile

clean:
	rm -rf ./obj ./bin ./lib

tests:
	mkdir -p bin
	${GPRBUILD} -P glfw_test.gpr -XGL_Backend=${GL_BACKEND}
	${GPRBUILD} -P opengl_test.gpr -XGL_Backend=${GL_BACKEND}

.PHONY: tests
