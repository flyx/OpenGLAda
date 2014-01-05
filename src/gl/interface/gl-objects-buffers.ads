--------------------------------------------------------------------------------
-- Copyright (c) 2012, Felix Krause <contact@flyx.org>
--
-- Permission to use, copy, modify, and/or distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
--------------------------------------------------------------------------------

with Interfaces.C.Pointers;

private with GL.Low_Level.Enums;

package GL.Objects.Buffers is
   pragma Preelaborate;
   
   type Buffer_Usage is (Stream_Draw, Stream_Read, Stream_Copy,
                         Static_Draw, Static_Read, Static_Copy,
                         Dynamic_Draw, Dynamic_Read, Dynamic_Copy);
   
   type Buffer_Target (<>) is tagged limited private;
   
   type Buffer is new GL_Object with private;
   
   procedure Bind (Target : Buffer_Target; Object : Buffer'Class);
   
   function Current_Object (Target : Buffer_Target) return Buffer'Class;
   
   generic
      with package Pointers is new Interfaces.C.Pointers (<>);
   procedure Load_To_Buffer (Target : Buffer_Target;
                             Data   : Pointers.Element_Array;
                             Usage  : Buffer_Usage);
   
   -- Use this instead of Load_To_Buffer when you don't want to copy any data
   procedure Allocate (Target : Buffer_Target; Number_Of_Bytes: Long;
                       Usage  : Buffer_Usage);
   
   generic
      with package Pointers is new Interfaces.C.Pointers (<>);
   procedure Map (Target : in out Buffer_Target; Access_Type : Access_Kind;
                  Pointer : out Pointers.Pointer);
   procedure Unmap (Target : in out Buffer_Target);
   
   generic
      with package Pointers is new Interfaces.C.Pointers (<>);
   function Pointer (Target : Buffer_Target) return Pointers.Pointer;
   
   generic
      with package Pointers is new Interfaces.C.Pointers (<>);
   procedure Get_Sub_Data (Target : in out Buffer_Target;
                           Offset : Types.Size;
                           Data   : in out Pointers.Element_Array);
   
   function Access_Type (Target : Buffer_Target) return Access_Kind;
   function Mapped      (Target : Buffer_Target) return Boolean;
   function Size        (Target : Buffer_Target) return Size;
   function Usage       (Target : Buffer_Target) return Buffer_Usage;
   
   
   procedure Draw_Elements (Mode : Connection_Mode; Count : Types.Size;
                            Index_Type : Unsigned_Numeric_Type);
   
   overriding
   procedure Initialize_Id (Object : in out Buffer);
   
   overriding
   procedure Delete_Id (Object : in out Buffer);
   
   procedure Invalidate_Data (Object : in out Buffer);   
   procedure Invalidate_Sub_Data (Object : in out Buffer;
                                  Offset, Length : Long_Size);
   
   Array_Buffer              : constant Buffer_Target;
   Element_Array_Buffer      : constant Buffer_Target;
   Pixel_Pack_Buffer         : constant Buffer_Target;
   Pixel_Unpack_Buffer       : constant Buffer_Target;
   Uniform_Buffer            : constant Buffer_Target;
   Texture_Buffer            : constant Buffer_Target;
   Transform_Feedback_Buffer : constant Buffer_Target;
   Copy_Read_Buffer          : constant Buffer_Target;
   Copy_Write_Buffer         : constant Buffer_Target;
   Draw_Indirect_Buffer      : constant Buffer_Target;
   Atomic_Counter_Buffer     : constant Buffer_Target;
   
private
   for Buffer_Usage use (Stream_Draw  => 16#88E0#,
                         Stream_Read  => 16#88E1#,
                         Stream_Copy  => 16#88E2#,
                         Static_Draw  => 16#88E4#,
                         Static_Read  => 16#88E5#,
                         Static_Copy  => 16#88E6#,
                         Dynamic_Draw => 16#88E8#,
                         Dynamic_Read => 16#88E9#,
                         Dynamic_Copy => 16#88EA#);
   for Buffer_Usage'Size use Low_Level.Enum'Size;

   type Buffer_Target (Kind : Low_Level.Enums.Buffer_Kind) is
     tagged limited null record;

   type Buffer is new GL_Object with null record;
   
   Array_Buffer              : constant Buffer_Target
     := Buffer_Target'(Kind => Low_Level.Enums.Array_Buffer);
   Element_Array_Buffer      : constant Buffer_Target
     := Buffer_Target'(Kind => Low_Level.Enums.Element_Array_Buffer);
   Pixel_Pack_Buffer         : constant Buffer_Target
     := Buffer_Target'(Kind => Low_Level.Enums.Pixel_Pack_Buffer);
   Pixel_Unpack_Buffer       : constant Buffer_Target
     := Buffer_Target'(Kind => Low_Level.Enums.Pixel_Unpack_Buffer);
   Uniform_Buffer            : constant Buffer_Target
     := Buffer_Target'(Kind => Low_Level.Enums.Uniform_Buffer);
   Texture_Buffer            : constant Buffer_Target
     := Buffer_Target'(Kind => Low_Level.Enums.Texture_Buffer);
   Transform_Feedback_Buffer : constant Buffer_Target
     := Buffer_Target'(Kind => Low_Level.Enums.Transform_Feedback_Buffer);
   Copy_Read_Buffer          : constant Buffer_Target
     := Buffer_Target'(Kind => Low_Level.Enums.Copy_Read_Buffer);
   Copy_Write_Buffer         : constant Buffer_Target
     := Buffer_Target'(Kind => Low_Level.Enums.Copy_Write_Buffer);
   Draw_Indirect_Buffer      : constant Buffer_Target
     := Buffer_Target'(Kind => Low_Level.Enums.Draw_Indirect_Buffer);
   Atomic_Counter_Buffer     : constant Buffer_Target
     := Buffer_Target'(Kind => Low_Level.Enums.Atomic_Counter_Buffer);
end GL.Objects.Buffers;
