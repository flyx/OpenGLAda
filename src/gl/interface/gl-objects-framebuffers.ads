--------------------------------------------------------------------------------
-- Copyright (c) 2013, Felix Krause <contact@flyx.org>
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

with GL.Buffers;
with GL.Objects.Renderbuffers;
with GL.Objects.Textures;

private with GL.Low_Level.Enums;

package GL.Objects.Framebuffers is
   pragma Preelaborate;

   type Framebuffer_Status is (Undefined, Complete, Incomplete_Attachment,
                               Incomplete_Missing_Attachment,
                               Incomplete_Draw_Buffer, Incomplete_Read_Buffer,
                               Unsupported, Incomplete_Multisample,
                               Incomplete_Layer_Targets);

   type Attachment_Point is (Depth_Stencil_Attachment,
                             Color_Attachment_0, Color_Attachment_1,
                             Color_Attachment_2, Color_Attachment_3,
                             Color_Attachment_4, Color_Attachment_5,
                             Color_Attachment_6, Color_Attachment_7,
                             Color_Attachment_8, Color_Attachment_9,
                             Color_Attachment_10, Color_Attachment_11,
                             Color_Attachment_12, Color_Attachment_13,
                             Color_Attachment_14, Color_Attachment_15,
                             Depth_Attachment, Stencil_Attachment);

   type Attachment_List is array (Positive range <>) of Attachment_Point;

   type Framebuffer_Target (<>) is tagged limited private;

   function Status (Target : Framebuffer_Target) return Framebuffer_Status;

   procedure Attach_Renderbuffer (Target : Framebuffer_Target;
                                  Attachment : Attachment_Point;
                                  Object : Renderbuffers.Renderbuffer'Class);

   procedure Attach_Texture (Target :  Framebuffer_Target;
                             Attachment : Attachment_Point;
                             Object : Textures.Texture'Class;
                             Level  : Textures.Mipmap_Level);

   procedure Invalidate (Target : in out Framebuffer_Target;
                         Attachments : Attachment_List);

   procedure Invalidate_Sub (Target        : in out Framebuffer_Target;
                             Attachments   : Attachment_List;
                             X, Y          : Int;
                             Width, Height : Size);

   procedure Set_Default_Width (Target : in out Framebuffer_Target;
                                Value  : Size);
   function Default_Width (Target : Framebuffer_Target) return Size;
   function Max_Framebuffer_Width return Size;

   procedure Set_Default_Height (Target : in out Framebuffer_Target;
                                 Value  : Size);
   function Default_Height      (Target : Framebuffer_Target) return Size;
   function Max_Framebuffer_Height return Size;

   procedure Set_Default_Layers (Target : in out Framebuffer_Target;
                                 Value  : Size);
   function Default_Layers      (Target : Framebuffer_Target) return Size;
   function Max_Framebuffer_Layers return Size;

   procedure Set_Default_Samples (Target : in out Framebuffer_Target;
                                  Value  : Size);
   function Default_Samples
     (Target : Framebuffer_Target) return Size;
   function Max_Framebuffer_Samples return Size;

   procedure Set_Default_Fixed_Sample_Locactions
     (Target : in out Framebuffer_Target; Value : Boolean);
   function Default_Fixed_Sample_Locations (Target : Framebuffer_Target)
                                            return Boolean;

   -- copy from current Read to current Draw framebuffer
   procedure Blit (Src_X0, Src_Y0, Src_X1, Src_Y1,
                   Dst_X0, Dst_Y0, Dst_X1, Dst_Y1 : Int;
                   Mask : Buffers.Buffer_Bits;
                   Filter : Textures.Magnifying_Function);

   type Framebuffer is new GL_Object with private;

   overriding
   procedure Initialize_Id (Object : in out Framebuffer);

   overriding
   procedure Delete_Id (Object : in out Framebuffer);

   procedure Bind (Target : Framebuffer_Target;
                   Object : Framebuffer'Class);

   function Current (Target : Framebuffer_Target) return Framebuffer'Class;

   Read_Target  : constant Framebuffer_Target;
   Draw_Target : constant Framebuffer_Target;
   Read_And_Draw_Target : constant Framebuffer_Target;

   Default_Framebuffer : constant Framebuffer;
private
   for Framebuffer_Status use (Undefined                     => 16#8219#,
                               Complete                      => 16#8CD5#,
                               Incomplete_Attachment         => 16#8CD6#,
                               Incomplete_Missing_Attachment => 16#8CD7#,
                               Incomplete_Draw_Buffer        => 16#8CDB#,
                               Incomplete_Read_Buffer        => 16#8CDC#,
                               Unsupported                   => 16#8CDD#,
                               Incomplete_Multisample        => 16#8D56#,
                               Incomplete_Layer_Targets      => 16#8DA8#);
   for Framebuffer_Status'Size use Low_Level.Enum'Size;

   for Attachment_Point use (Depth_Stencil_Attachment => 16#821A#,
                             Color_Attachment_0       => 16#8CE0#,
                             Color_Attachment_1       => 16#8CE1#,
                             Color_Attachment_2       => 16#8CE2#,
                             Color_Attachment_3       => 16#8CE3#,
                             Color_Attachment_4       => 16#8CE4#,
                             Color_Attachment_5       => 16#8CE5#,
                             Color_Attachment_6       => 16#8CE6#,
                             Color_Attachment_7       => 16#8CE7#,
                             Color_Attachment_8       => 16#8CE8#,
                             Color_Attachment_9       => 16#8CE9#,
                             Color_Attachment_10      => 16#8CEA#,
                             Color_Attachment_11      => 16#8CEB#,
                             Color_Attachment_12      => 16#8CEC#,
                             Color_Attachment_13      => 16#8CED#,
                             Color_Attachment_14      => 16#8CEE#,
                             Color_Attachment_15      => 16#8CEF#,
                             Depth_Attachment         => 16#8D00#,
                             Stencil_Attachment       => 16#8D01#);
   for Attachment_Point'Size use Low_Level.Enum'Size;

   pragma Convention (C, Attachment_List);

   type Framebuffer is new GL_Object with null record;

   type Framebuffer_Target (Kind : Low_Level.Enums.Framebuffer_Kind) is
     tagged limited null record;

   Read_Target  : constant Framebuffer_Target :=
     Framebuffer_Target'(Kind => Low_Level.Enums.Read);
   Draw_Target : constant Framebuffer_Target :=
     Framebuffer_Target'(Kind => Low_Level.Enums.Draw);
   Read_And_Draw_Target : constant Framebuffer_Target :=
     Framebuffer_Target'(Kind => Low_Level.Enums.Read_Draw);

   Default_Framebuffer : constant Framebuffer :=
     Framebuffer'(GL_Object with null record);
end GL.Objects.Framebuffers;
