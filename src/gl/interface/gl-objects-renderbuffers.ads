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

with GL.Pixels;

with GL.Low_Level.Enums;

package GL.Objects.Renderbuffers is
   pragma Preelaborate;

   type Renderbuffer_Target (<>) is tagged limited private;

   procedure Allocate (Object : Renderbuffer_Target;
                       Format : Pixels.Internal_Format;
                       Width, Height : Size;
                       Samples : Size := 0);

   function Width  (Object : Renderbuffer_Target) return Size;
   function Height (Object : Renderbuffer_Target) return Size;
   function Internal_Format (Object : Renderbuffer_Target)
                             return Pixels.Internal_Format;
   function Red_Size     (Object : Renderbuffer_Target) return Size;
   function Green_Size   (Object : Renderbuffer_Target) return Size;
   function Blue_Size    (Object : Renderbuffer_Target) return Size;
   function Alpha_Size   (Object : Renderbuffer_Target) return Size;
   function Depth_Size   (Object : Renderbuffer_Target) return Size;
   function Stencil_Size (Object : Renderbuffer_Target) return Size;

   function Raw_Kind (Object : Renderbuffer_Target)
                      return Low_Level.Enums.Renderbuffer_Kind;

   Active_Renderbuffer : constant Renderbuffer_Target;

   type Renderbuffer is new GL_Object with private;

   procedure Bind (Target : Renderbuffer_Target; Object : Renderbuffer'Class);

   function Current (Target : Renderbuffer_Target) return Renderbuffer'Class;

   overriding
   procedure Initialize_Id (Object : in out Renderbuffer);

   overriding
   procedure Delete_Id (Object : in out Renderbuffer);

   No_Renderbuffer : constant Renderbuffer;
private

   type Renderbuffer is new GL_Object with null record;

   type Renderbuffer_Target (Kind : Low_Level.Enums.Renderbuffer_Kind) is
     tagged limited null record;

   Active_Renderbuffer : constant Renderbuffer_Target
     := Renderbuffer_Target'(Kind => Low_Level.Enums.Renderbuffer);

   No_Renderbuffer : constant Renderbuffer :=
     Renderbuffer'(GL_Object with null record);
end GL.Objects.Renderbuffers;
