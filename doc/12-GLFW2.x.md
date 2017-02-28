---
layout : default
title : API - GLFW 2.x Binding
packages :
  - GLFW (2.x)
api: true
permalink: glfw-v2.html
---

# The GLFW 2.x Binding

This is a binding for the [GLFW][1] library, version `2.x`. Most subprograms
map directly to the GLFW C API, so you can use the GLFW API documentation as
guide. The binding has two primary subpackages:

## `GLFW.Display`

This is the interface to the GLFW window. It provides functionality to
open, manipulate and close a windowed or fullscreen OpenGL surface with
front and back buffer.

## `GLFW.Events`

This Package contains the child packages `Mouse`, `Keys` and `Joysticks` to
access human interface devices.

## Excluded Features

GLFW 2.x has a number of features that are not included in this binding.
These are:

 * Threading. This includes threads, mutexes and condition variables. Since
   Ada has language-level threading support, it makes no sense to include
   these features in the binding.
 * Image and texture loading. There are specialized libraries for doing this.
   (__TODO__: When a binding to one of those has been added, link to that
   here )

## Supported GLFW versions

This binding has been tested with GLFW versions `2.7.6` and `2.7.7`. It
should work with older versions, but there is no guarantee.


 [1]: http://www.glfw.org/