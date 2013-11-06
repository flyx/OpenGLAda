---
layout: default
title: API Documentation
weight: 1
---

# API Documentation

## Introduction

OpenGLAda provides an API that aims to be similar to the standard OpenGL API you
might be familiar with. As a thick binding, the library introduces some additional
concepts and features. On this page, you find some general information on the structure
and concepts of the OpenGLAda API. The documentation of the packages
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

Some API entry points in OpenGL are provided for a number of different numeric types, like
`GLbyte`, `GLshort`, `GLint`, `GLuint`, `GLfloat`, `GLdouble` and so on. The incoming
types often make no difference to OpenGL. The values will be converted to whatever
format OpenGL uses internally (this may depend on the implementation). These entry
points may not be wrapped for all available types in OpenGLAda. For example, color
values are always single-precision floating point values. Other types, like vectors, are
provided for multiple base types.

Keep in mind that the size and type of values passed to OpenGL only matters on the
OpenGL side if you directly pass them to shaders or write them into buffers. As mentioned
above, other calls will convert the values to the internally used type.

## Exception Handling

OpenGL provides a polling mechanism for discovering errors, i.e. you have to ask OpenGL
whether an error has occured. OpenGLAda can do this automatically in most calls that are
relayed to OpenGL. If this feature is enabled, an appropriate exception is raised any time
OpenGL sets an error flag. You can disable this behavior by setting the scenario variable
`Auto_Exceptions` to `disabled` when compiling OpenGLAda.

## Runtime Binding

All features that were introduced in OpenGL 1.2 or later will not be expected to be
available. OpenGLAda queries the neccessary function pointers at runtime and raises a
`Feature_Not_Supported_Exception` when a function is not available from the system's
OpenGL implementation.

## Getting Started

Reading lots of documentation can be exhausting, so here is some code that gives you a
basic idea of how to use OpenGLAda:

<?prettify lang=ada?>

    with GL.Buffers;         use GL.Buffers;
    with GL.Colors;          use GL.Colors;
    with GL.Fixed.Immediate; use GL.Fixed.Immediate;
    with GL.Fixed.Matrix;    use GL.Fixed.Matrix;
    with GL.Types;           use GL.Types;
    use GL.Fixed;
    
    with Glfw.Display;
    with Glfw.Events.Keys;
    
    procedure GL_Test.Immediate is
       use GL.Types.Doubles;
    begin
       Glfw.Init;
    
       Glfw.Display.Open (Mode  => Glfw.Display.Window,
                          Width => 500, Height => 500);
    
       Projection.Load_Identity;
       Projection.Apply_Orthogonal (-1.0, 1.0, -1.0, 1.0, -1.0, 1.0);
    
       while not Glfw.Events.Keys.Pressed (Glfw.Events.Keys.Esc) and
             Glfw.Display.Opened loop
          Clear (Buffer_Bits'(others => True));
    
          Projection.Push;
    
          for I in 1 .. 12 loop
             declare
                Token : Input_Token := Start (Line_Strip);
             begin
                Set_Color (GL.Colors.Color'(1.0, 0.0, 0.0, 0.0));
                Token.Add_Vertex (Vector4'(0.1, 0.4, 0.0, 1.0));
                Token.Add_Vertex (Vector4'(0.1, 0.6, 0.0, 1.0));
                Token.Add_Vertex (Vector4'(-0.1, 0.6, 0.0, 1.0));
                Token.Add_Vertex (Vector4'(-0.1, 0.4, 0.0, 1.0));
             end;
             Projection.Apply_Rotation (360.0 / 12.0, 0.0, 0.0, 1.0);
          end loop;
    
          Projection.Pop;
          Projection.Apply_Rotation (0.8, 0.0, 0.0, 1.0);
    
          GL.Flush;
    
          Glfw.Display.Swap_Buffers;
    
          Glfw.Events.Poll_Events;
       end loop;
    
       Glfw.Terminate_Glfw;
    
    end GL_Test.Immediate;