--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with GL.Objects.Lists;

package GL.Objects.Shaders.Lists is
  new GL.Objects.Lists (Shader, Create_From_Id);
pragma Preelaborate (GL.Objects.Shaders.Lists);
