--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with GL.Objects.Shaders;

-- This package is an addition to the original OpenGL API and simplifies
-- the handling of ressources located in files, like shader sources.
package GL.Files is

   procedure Load_Shader_Source_From_File (Object : Objects.Shaders.Shader;
                                           File_Name : String);

end GL.Files;
