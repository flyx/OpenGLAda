---
layout : default
title : API - The package GL.Objects
packages :
  - GL.Objects
versions:
  - 2.1
  - 3.0
weight: 5
---

# The package `GL.Objects`

This package is basically a wrapper for all OpenGL APIs that define and use objects.

## Overview

In OpenGL, an object is a set of properties. The OpenGL API for accessing objects
is purely based on the __state machine__ paradigm: An object has to be __bound__
(meaning it has to be made the active object) in order to interact with it. For each
object class (e.g. textures), there can only one object be active. The properties
of this object can be accessed and modified with functions that do not take a
reference to the object, as they automatically interact with the active object.

OpenCLAda basically works the same way. Designing an alternative, simpler API has
been considered and deem not to be suitable for a wrapper library. However, the
handling of objects and targets is designed in a more object-oriented way.

## OpenGLAda's approach to objects

In OpenGLAda, OpenGL objects are tagged types derived from the abstract `GL_Object`
type defined in this package. They implement reference counting to automatically
delete unused objects from OpenGL memory. This is achieved with controlled types.
So, actually, all object types are just references and should be handles as such.
This guide will subsequently refer to instances of these tagged types as
_object references_, while _OpenGL objects_ are the real allocated memory on the
graphics card.

The targets where objects can be bound are also implemented as tagged types.
However, you cannot create any targed instances. Instead, the available target
instances are provided as constants in the corresponding package.

All targets implement a procedure `Bind`, which binds the object referred by an
object reference to the target as raw OpenGL would do it. Additionally, `Bind`
protects the OpenGL object from deletion - as long as an object is bound, it will
not be deleted, even if there are no more references to the object in your code.

## Usage

When you define a new variable holding an OpenGL object reference and do
not initialize it, it automatically gets assigned a unique name from OpenGL.
Don't worry - there will be no object created as long as you do not bind your
object reference.

To use the object, you have to bind it to a target and use the methods of this
target to configure the currently bound object. The code might look like this:

{% highlight ada %}
declare
   use GL.Objects.Textures;
   My_Texture, My_Other_Texture : Texture;
begin
   -- Bind the texture to a target
   -- (Note that the texture's type is determined by the target it get
   -- bound to. You should not bind the texture to another target afterwards)
   Texture_2D.Bind (My_Texture);
   
   Texture_2D.Set_Minifying_Filter (Linear_Mipmap_Nearest);
   -- do something useful here
   -- ...
   
   Texture_2D.Bind (My_Other_Texture);
   
   -- if you did not copy the value of My_Texture, the OpenGL texture
   -- that is referenced by My_Texture gets destroyed at this point.
   -- The object referenced by My_Other_Texture lives on, as it is
   -- the currently bound texture to the target Texture_2D.
end;
{% endhighlight %}

You can query a reference to the currently bound object by calling
`Current_[Descriptor]` on a target, where `[Descriptor]` is the type of the
actual object (e.g. _Texture_).  

## Low-level access

If for some reason you need to access the name of the object that is used in
OpenGL, you can retrieve it with `Raw_Id`. The raw ID might be useful when
you interact with other OpenGL-related libraries.