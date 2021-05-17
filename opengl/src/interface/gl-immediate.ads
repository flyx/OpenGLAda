--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with Ada.Finalization;

with GL.Types.Colors;

-- This package provides functions to directly insert vertices, colors, normals
-- etc. into the pipeline. Note that these functions have been deprecated with
-- OpenGL 3, you should use VBOs instead.
package GL.Immediate is
   pragma Preelaborate;

   use GL.Types;
   use GL.Types.Doubles;

   type Input_Token (<>) is new Ada.Finalization.Limited_Controlled with private;

   overriding procedure Finalize (Token : in out Input_Token);

   function Start (Mode : Connection_Mode) return Input_Token;

   procedure Add_Vertex (Token : Input_Token; Vertex : Vector2);
   procedure Add_Vertex (Token : Input_Token; Vertex : Vector3);
   procedure Add_Vertex (Token : Input_Token; Vertex : Vector4);

   procedure Set_Color (Value : Colors.Color);
   function Current_Color return Colors.Color;

   procedure Set_Secondary_Color (Value : Colors.Color);
   function Current_Secondary_Color return Colors.Color;

   procedure Set_Fog_Distance (Value : Double);
   function Current_Fog_Distance return Double;

   procedure Set_Normal (Value : Vector3);
   function Current_Normal return Vector3;

   procedure Set_Texture_Coordinates (Value : Vector2);
   procedure Set_Texture_Coordinates (Value : Vector3);
   procedure Set_Texture_Coordinates (Value : Vector4);
   function Current_Texture_Coordinates return Vector4;


private
   type Input_Token (Mode : Connection_Mode) is
     new Ada.Finalization.Limited_Controlled with record
      Finalized : Boolean := True;
   end record;
end GL.Immediate;
