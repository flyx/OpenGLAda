--  part of OpenGLAda, (c) 2022 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

private with GL.Low_Level;

package GL.Memory is
   pragma Preelaborate;

   type Barrier_Bits is record
      Vertex_Attrib_Array  : Boolean := False;
      Element_Array        : Boolean := False;
      Uniform              : Boolean := False;
      Texture_Fetch        : Boolean := False;

      Shader_Image_Access  : Boolean := False;
      Command              : Boolean := False;
      Pixel_Buffer         : Boolean := False;
      Texture_Update       : Boolean := False;
      Buffer_Update        : Boolean := False;
      Framebuffer          : Boolean := False;
      Transform_Feedback   : Boolean := False;
      Atomic_Counter       : Boolean := False;
      Shader_Storage       : Boolean := False;
      Client_Mapped_Buffer : Boolean := False;
      Query_Buffer         : Boolean := False;

      Unused               : Boolean := False;
   end record;
   pragma Convention (C, Barrier_Bits);

   procedure Barrier (Bits : Barrier_Bits);

   procedure Barrier_By_Region (Bits : Barrier_Bits);

private
   for Barrier_Bits use record
      Vertex_Attrib_Array  at 0 range 0 .. 0;
      Element_Array        at 0 range 1 .. 1;
      Uniform              at 0 range 2 .. 2;
      Texture_Fetch        at 0 range 3 .. 3;

      Shader_Image_Access  at 0 range 5 .. 5;
      Command              at 0 range 6 .. 6;
      Pixel_Buffer         at 0 range 7 .. 7;
      Texture_Update       at 0 range 8 .. 8;
      Buffer_Update        at 0 range 9 .. 9;
      Framebuffer          at 0 range 10 .. 10;
      Transform_Feedback   at 0 range 11 .. 11;
      Atomic_Counter       at 0 range 12 .. 12;
      Shader_Storage       at 0 range 13 .. 13;
      Client_Mapped_Buffer at 0 range 14 .. 14;
      Query_Buffer         at 0 range 15 .. 15;

      Unused               at 0 range 16 .. 31;
   end record;
   for Barrier_Bits'Size use Low_Level.Bitfield'Size;
end GL.Memory;

