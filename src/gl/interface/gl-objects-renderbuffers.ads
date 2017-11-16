--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

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

   No_Renderbuffer : constant Renderbuffer;
private

   type Renderbuffer is new GL_Object with null record;

   overriding
   procedure Internal_Create_Id (Object : Renderbuffer; Id : out UInt);

   overriding
   procedure Internal_Release_Id (Object : Renderbuffer; Id : UInt);

   type Renderbuffer_Target (Kind : Low_Level.Enums.Renderbuffer_Kind) is
     tagged limited null record;

   Active_Renderbuffer : constant Renderbuffer_Target
     := Renderbuffer_Target'(Kind => Low_Level.Enums.Renderbuffer);

   No_Renderbuffer : constant Renderbuffer :=
     Renderbuffer'(Ada.Finalization.Controlled with Reference =>
                     Reference_To_Null_Object'Access);
end GL.Objects.Renderbuffers;
