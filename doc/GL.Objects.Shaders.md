---
layout : default
title : API - Working with Shaders
packages :
  - GL.Objects.Shaders
  - GL.Objects.Programs
weight : 9
---

# Working with Shaders

Shaders replace OpenGL's fixed function pipeline that has been deprecated with OpenGL 3.0.
To use shaders, you have to load their source files and compile them. Then you have to 
attach them to a program object and link this object. Finally, you have to tell OpenGL
to use that program to process your input.

## The package `GL.Objects.Shaders`

There are multiple types of shaders. Unlike buffer objects, a shader always is of one
specific type and cannot be re-used as a shader of another type. The available shader
types are contained in the enumeration `Shader_Type`. When you create a `Shader` object,
you have to set its type to the discriminant `Kind`.

Usually, your shader source is available as a text file. There is a helper procedure in
the package `GL.Files` named `Load_Shader_Source_From_File` which loads a shader file
directly to OpenGL. This is more efficient than the procedure `Set_Source`, as you do
not have to load the file into a String manually. Mind that
`Load_Shader_Source_From_File` uses the current directory as base path to interpret the
given file path.

`Compile` does not raise an exception when the compilation fails. Instead, you have to
query the status of the compilation with `Compile_Status`. It returns `False` if an
error occured during the compilation. You can query the compilation log with `Info_Log`.

## The package `GL.Objects.Programs`

After you attached your shaders with `Attach`, linking the program works similar to
compiling a shader. Call `Use_Program` to use the program for processing in OpenGL.

The program object also gives you access to the uniform and attribute parameters of your
shaders. use `Uniform_Location` to query the internal name of a uniform parameter
(which is and integer value). You can then interact with the parameter with the
procedures in `GL.Uniforms`.

Interaction with attribute parameters is not fully implemented yet.

## Basic example

<?prettify lang=ada?>

    declare
       Vertex_Shader   : GL.Objects.Shaders.Shader
         (Kind => GL.Objects.Shaders.Vertex_Shader);
       Fragment_Shader : GL.Objects.Shaders.Shader
         (Kind => GL.Objects.Shaders.Fragment_Shader);
       Program         : GL.Objects.Programs.Program;
    begin
    
       -- load and compile shaders
       GL.Files.Load_Shader_Source_From_File (Vertex_Shader, "vertex.glsl");
       GL.Files.Load_Shader_Source_From_File (Fragment_Shader, "fragment.glsl");
       
       Vertex_Shader.Compile;
       Fragment_Shader.Compile;
       
       if not Vertex_Shader.Compile_Status then
          Ada.Text_IO.Put_Line ("Compilation of vertex shader failed. log:");
          Ada.Text_IO.Put_Line (Vertex_Shader.Info_Log);
       end if;
       if not Fragment_Shader.Compile_Status then
          Ada.Text_IO.Put_Line ("Compilation of fragment shader failed. log:");
          Ada.Text_IO.Put_Line (Fragment_Shader.Info_Log);
       end if;
       
       -- set up program
       Program.Attach (Vertex_Shader);
       Program.Attach (Fragment_Shader);
       Program.Link;
       if not Program.Link_Status then
          Ada.Text_IO.Put_Line ("Program linking failed. Log:");
          Ada.Text_IO.Put_Line (Program.Info_Log);
          return;
       end if;
       Program.Use_Program;
       
       -- do something useful here
    end;