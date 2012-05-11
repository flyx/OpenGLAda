---
layout : default
title : API - The package GL.Objects.Textures
packages :
  - GL.Objects.Textures
versions:
  - 2.1
weight: 6
---

# The package `GL.Objects.Textures`

***TODO:*** Most of the contents here became obsolete with
the introduction of `GL.Objects`. Write something useful here.

The package provides setters and getters for parameters of the texture targets.
You can look up the meaning and impact of those parameters in the corresponding
section in the [OpenGL API reference](http://www.opengl.org/sdk/docs/man/xhtml/glTexParameter.xml).

{* The following code example defines a texture ID, binds it to the 2D texture target,
and sets some properties of the target. It then configures the environment and
loads an empty texture. After doing something useful with the texture,
it unregisters it again implicitly at the end of life of the variable `My_Id`. *}

{* TODO: update this code to use the new GL.Objects API
declare
   My_Id : GL.Textures.Tex_2D_Id;
   use GL.Textures;
begin
   Texture_2D.Bind (My_Id);
   Texture_2D.Set_X_Wrapping (Repeat);
   Texture_2D.Set_Y_Wrapping (Mirrored_Repeat);
   Texture_2D.Set_Magnifying_Filter (Linear);
   
   Environment.Set_Tex_Function (Environment.Replace);
   
   Loader_2D.Load_Empty_Texture (Target          => Loader_2D.TX_2D,
                                 Level           => 0,
                                 Internal_Format => Pixel_Data.RGBA,
                                 Width           => 400,
                                 Height          => 400,
                                 Border          => False,
                                 Format          => Pixel_Data.RGBA,
                                 Data_Type       => Pixel_Data.Float);
   
   -- ... do something useful here
end;

*}