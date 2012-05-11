---
layout: default
title: API Documentation
weight: 1
---

# API Documentation

## Introduction

OpenGLAda provides an API that aims to be similar to the standard OpenGL API you
might be familiar with. As a thick binding, the library introduces some additional
concepts and features which are explained here. On this page, you find some general
information on the structure and concepts of the API. The documentation of the packages
provided by OpenGLAda can be accessed from the navigation menu to the left.

This is not a standalone documentation. For a detailed description of the behaviour of
OpenGL, you have to consult the
[OpenGL Reference Pages](http://www.opengl.org/sdk/docs/man/). This documentation only
describes how OpenGL features are wrapped by OpenGLAda.

## OpenGL Versions

OpenCLAda aims to provide the feature set of OpenGL 3.2. It may be extended to provide
functionality of newer versions in the future.

OpenGL 3.0 has deprecated a lot of functionality that was part of earlier versions.
OpenGLAda provides some of this older functionality that seems to be important enough
to be inlcuded, but not all of it is available. If you discover that some
OpenGL 2.1 functionality isn't available in OpenGLAda, you should look for the
alternatives OpenGL 3.0 offers.

## Types

Most API entry points in OpenGL are provided for a number of different numeric types, like
`GLbyte`, `GLshort`, `GLint`, `GLuint`, `GLfloat`, `GLdouble` and so on. The incoming
types make no difference to OpenGL. The values will be converted to whatever format OpenGL
uses internally (this may depend on the implementation).

To avoid the the massive overhead of providing every functionality for multiple numeric
types, OpenGLAda provides every operation for only one value type. For vertices, this is
`GL.Real`, which is a double-precision floating point type. Other operations expect more
specialized types, like e.g. `GL.Colors.Component`, which is a single-precision value
constrained to `0.0 .. 1.0`. All types try to implement constraints defined in the OpenGL
specification.

## Exception Handling

OpenGL provides a polling mechanism for discovering errors, i.e. you have to ask OpenGL
whether an error has occured. OpenGLAda does this automatically in most calls that are
relayed to OpenGL. If it discovers an error, it raises an appropriate exception (see 
[GL](/OpenGLAda/doc/GL.html) for details). This behaviour can be disabled for performance
reasons, but then you won't notice any errors coming from OpenGL.

## Runtime Binding

All features that were introduced in OpenGL 1.2 or later will not be expected to be
available. OpenGLAda queries the neccessary function pointers at runtime and raises a
`Feature_Not_Supported_Exception` when a function is not available from the system's
OpenGL implementation.

## Getting Started

Reading lots of documentation can be exhausting, so here is some code that gives you a
basic idea of how to use OpenGLAda:

{% highlight ada %}
with GL.Immediate; use GL.Immediate;
with GL.Vectors;   use GL.Vectors;
with GL.Colors;    use GL.Colors;
with GL.Matrices;  use GL.Matrices;

with Glfw.Display;
with Glfw.Events;

procedure Glfw_Test.Immediate is
   use type GL.Real;
begin
   Glfw.Init;

   Glfw.Display.Open (Mode => Glfw.Display.Window);

   Projection.Load_Identity;
   Projection.Apply_Orthogonal (-1.0, 1.0, -1.0, 1.0, -1.0, 1.0);

   Set_Color (GL.Colors.Color'(1.0, 0.0, 0.0, 0.0));

   while not Glfw.Events.Keys.Pressed (Glfw.Events.Keys.Esc) and
         Glfw.Display.Opened loop
      Projection.Push;
      
      -- TODO: Fancy stuff here
      
      Projection.Pop;

      GL.Flush;

      Glfw.Display.Swap_Buffers;

      delay 0.1;
      Glfw.Events.Poll_Events;
   end loop;

   Glfw.Terminate_Glfw;

end Glfw_Test.Immediate;
{% endhighlight %}