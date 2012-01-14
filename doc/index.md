---
layout: default
title: API Documentation
---

# API Documentation

## Introduction

As a think binding, OpenGLAda provides an API that is somewhat distinct from the standard
OpenGL API you might be familiar with. On this page, you find some general information
on the structure and design concepts of the API. The documentation of the packages
provided by OpenGLAda can be accessed from the navigation menu to the left.

This is not a standalone documentation. For a detailed description of the behaviour of
OpenGL, you have to consult the [OpenGL Reference Pages](http://www.opengl.org/sdk/docs/man/). This documentation only points out the differences between OpenGLAda and standard
OpenGL.

## OpenGL Versions

OpenCLAda aims to provide the feature set of OpenGL 3.2. It may be extended to provide
functionality of newer versions in the future.

OpenGL 3.0 has deprecated a lot of functionality that was part of earlier versions.
OpenGLAda provides some of this functionality that seems to be important enough to be
inlcuded, but not all of this deprecated functionality is included. If you find that some
OpenGL 2.1 functionality isn't available in OpenGLAda, you should look for the
alternatives OpenGL 3.0 offers.

## Types

Most API entry points in OpenGL are provided for a number of different numeric types, like
`GLbyte`, `GLshort`, `GLint`, `GLuint`, `GLfloat`, `GLdouble` and so on. On the OpenGL
side, it makes no differences in which types you pack your numeric values. The values will
be converted to the format OpenGL uses internally (this may depend on the implementation).

To avoid the the massive overhead of providing every functionality for multiple numeric
types, OpenGLAda provides only one basic numeric type, `GL.Real`. It is a double precision
floating point type. On modern hardware, you surely have enough RAM so that you won't miss
the possibility to use smaller numeric data types.

## Exception Handling

OpenGL provides a polling mechanism for discovering errors, i.e. you have to ask OpenGL
whether an error has occured. OpenGLAda does this automatically in most calls that are
relayed to OpenGL. If it discovers an error, it raises an appropriate exception (see 
[GL](/OpenGLAda/doc/GL.html) for details). This behaviour can be disabled, but then, you
won't notice any errors coming from OpenGL.

## Getting Started

Reading lots of documentation can be exhausting, so here is some code that gives you a
basic idea of how to use OpenGLAda:

{% highlight ada %}
with GL.Immediate; use GL.Immediate;
with GL.Vectors;   use GL.Vectors;
with GL.Colors;    use GL.Colors;
with GL.Matrices;  use GL.Matrices;

with Glfw.Display;

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
   end loop;

   Glfw.Terminate_Glfw;

end Glfw_Test.Immediate;
{% endhighlight %}