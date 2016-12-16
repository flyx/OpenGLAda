with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use  Ada.Text_IO;

with  Interfaces.C.Pointers;

with GL.Attributes;
with GL.Objects.Buffers;
with GL.Objects.Vertex_Arrays;
with GL.Types; use  GL.Types;

with Shaders_Program;

package body Utilities is
  --  generic package parameters of Interfaces.C.Pointers:
  --  type Index is (<>);
  --  type tElement is private;
  --  type Element_Array is array (Index range <>) of aliased Element;
   type tPoint_Index is new int;                          --   type Index is (<>)
   type tPoint_Coord is new Single;                     --   type tElement
   type tPoints_Array is array (tPoint_Index range <>) of aliased tPoint_Coord; --  type Element_Array

   Default_Terminator : tPoint_Coord := 0.0;

--    package pPointers is new Interfaces.C.Pointers(tPoint_Index, tPoint_Coord, tPoints_Array, Default_Terminator);
--    procedure Load_Vertex_Buffer is new gl.Objects.Buffers.Load_To_Buffer(pPointers);

    --  Buffer_Target is a private type not meant for instantiation by the user.
    --  Use the constant GL.Objects.Buffers.Element_Array_Buffer where Buffer_Target is needed.

    function Startup(Window : Window_Types.tWindow;
                     Rendering_Program : out GL.Objects.Programs.Program) return Boolean is

     --   use GL.Objects.Buffers;

        Vertex_Array             : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
      --  Vertex_Buffer           : GL.Objects.Buffers.Buffer;
    begin

    --    Vertex_Buffer.Initialize_Id;                          -- glGenBuffers(1, &vbo)  with vbo (buffer ID) = 0;
    --    Bind(ARRAY_BUFFER, Vertex_Buffer);     -- Set current buffer, glBindBuffer(GL_ARRAY_BUFFER, vbo);
    --    Load_Vertex_Buffer(ARRAY_BUFFER, Points, Static_Draw);

        Vertex_Array.Initialize_Id;
        Vertex_Array.Bind;
    --    GL.Attributes.Enable_Vertex_Attrib_Array(0);
    --    Bind(ARRAY_BUFFER, Vertex_Buffer);
    --    GL.Attributes.Set_Vertex_Attrib_Pointer(0, 3, GL.Types.Single_Type, 0, 0);

        return Shaders_Program.Make_Shader_Program(Window, Rendering_Program);
    end;
end Utilities;
