---
layout : default
title : API - The package GL.Objects
packages :
  - GL.Objects
  - GL.Objects.Textures
versions:
  - 2.1
  - 3.0
weight: 5
---

# The package `GL.Objects`

This package is basically a wrapper for all OpenGL APIs that define and use objects.

## Introduction: How OpenGL handles objects

In OpenGL, an object is a set of properties. The OpenGL API for accessing objects
is purely based on the __state machine__ paradigm: An object has to be __bound__
(meaning it has to be made the active object) in order to interact with it. For each
object class (e.g. textures), there can only one object be active. The properties
of this object can be accessed and modified with functions that do not take a
reference to the object, as they automatically interact with the active object.

Another curiosity is that we actually never see the object we create and interact
with. All OpenGL provides is a (numeric) name for that object. We tell OpenGL what
to do with a texture with some name, and OpenGL decides whether and when to actually
create an object with that name.

Let's have a look at how this API typically gets used in C / C++:

{% highlight c++ %}
void setupTexture() {
   GLuint texture;

   // get an unused name for the texture
   glGenTextures(1, &texture);

   // bind the name as current 2D texture.
   // note that before this call, it was not determined which object the name would
   // actually refer to - we could as well have made it a 3D texture.
   glBindTexture(GL_TEXTURE_2D, texture);
   
   // set some property value of our object
   // not that we do not reference the name of the object - we only refer to the
   // object class (being GL_TEXTURE_2D) while knowing that our object currently
   // is the active object and therefore gets assigned this property
   glTexParameter(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_NEAREST);

   // ...
}
{% endhighlight %}

## OpenGLAda's approach to objects

OpenGL's API for objects is rather confusing (my personal optinion), so OpenGLAda
tries to provide a higher-level interface for interacting with objects.

The goals of OpenGLAda's approach to object interaction are:

 * Use object orientation as paradigm instead of OpenGL's state-machine paradigm.
 * Automatically handle memory management: When the last reference to an OpenGL
   object is deleted, the OpenGL object gets destroyed.
 * Keep in mind that the higher level interface to the OpenGL API should not employ
   mechanisms that would cause a performance loss - usually, performance is a
   critical attribute of code that uses OpenGL.

In OpenGLAda, OpenGL objects are tagged types derived from the abstract `GL_Object`
type defined in this package. They implement reference counting to automatically
delete unused objects from OpenGL memory. This is achieved with controlled types.
So, actually, all object types are just references and should be handles as such.

## Usage

When you define a new variable holding an OpenGL object references and do
not initialize it, it automatically gets assigned a unique name from OpenGL.
Don't worry - there will be no object created as long as you do not bind your
object reference. Any call to an operation on the object will cause it to be
bound as active object. You should keep this in mind, as is it a side effect
of all these operations. You can also explicitly call `Bind`.

The code above might look like this with OpenGLAda:

{% highlight ada %}
declare
   use GL.Objects.Textures;
   My_Texture : Texture (Kind => Texture_1D);
begin
   -- at this call, the texture automatically gets bound
   My_Texture.Set_Minifying_Filter (Linear_Mipmap_Nearest);
   -- do something useful here
   -- ...
   
   -- if you did not copy the value of My_Texture, the OpenGL texture
   -- that has been created for My_Texture gets destroyed at this point.
end;
{% endhighlight %}

## Low-level access

If for some reason you need to access the name of the object that is used in
OpenGL, you can retrieve it with `Raw_Id`. This function is an exception to
the rule that every operation on the object will cause it to get bound - it
will not bind the object or do anything else with it.

The raw ID might be useful when you interact with other OpenGL-related
libraries.