---
layout : default
title : API - The package GL.Objects
packages :
  - GL.Objects
weight: 7
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
been considered and deemed not to be suitable for a wrapper library. However, the
handling of objects and targets is designed in a more object-oriented way.

## OpenGLAda's approach to objects

In OpenGLAda, OpenGL objects are tagged types derived from the abstract `GL_Object`
type defined in this package. They implement reference counting to automatically
delete unused objects from OpenGL memory. This is achieved with controlled types.
So, actually, all object types are just references and should be handled as such.
This guide will subsequently refer to instances of these tagged types as
_object references_, while _OpenGL objects_ are the real allocated memory on the
graphics card.

The targets where objects can be bound are also implemented as tagged types.
However, you cannot create any target instances. Instead, the available target
instances are provided as constants in the corresponding package.

All targets implement a procedure `Bind`, which binds the object referred by an
object reference to the target, as raw OpenGL would do it. Additionally, `Bind`
protects the OpenGL object from deletion - as long as an object is bound, it will
not be deleted, even if there are no more references to the object in your code.

## Usage

When you declare a new variable holding an OpenGL object reference, it will
be uninitialized. Calling `Bind` with this reference will result in an exception.
To initialize your reference, you have to call `Initialize_Id` on it. This will
query an unused ID from OpenGL which will subsequently be used internally to refer
to the OpenGL object. Some important notes on this:

 * You cannot initialize the ID before creating an OpenGL context. This is the
   main reason why IDs are not automatically initialized when the reference is
   created - this would prevent you from declaring any object references at
   library level.
 * You can check the status of an object reference at any time with the function
   `Initialized`.
 * You can manually delete an object by calling `Delete_Id`. If you don't, the
   object will be automatically destroyed when the reference count of the
   object reaches 0.
 * The actual OpenGL object will be created when you bind the reference to a
   target for the first time.
 * Do not confuse `Initialize_Id` with `Initialize`, which is inherited from
   `Ada.Finalization.Controlled` and mustn't be called manually.

To use the object, you have to bind it to a target and use the methods of this
target to configure the currently bound object. The code might look like this:

<?prettify lang=ada?>

    declare
       use GL.Objects.Textures;
       My_Texture, My_Other_Texture : Texture;
    begin
       -- Create IDs for the textures
       My_Texture.Initialize_Id;
       My_Other_Texture.Initialize_Id;
    
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

You can query a reference to the currently bound object by calling
`Current_[Descriptor]` on a target, where `[Descriptor]` is the type of the
actual object (e.g. _Texture_).  

## Low-level access

If for some reason you need to access the name of the object that is used in
OpenGL, you can retrieve it with `Raw_Id`. The raw ID might be useful when
you interact with other OpenGL-related libraries.