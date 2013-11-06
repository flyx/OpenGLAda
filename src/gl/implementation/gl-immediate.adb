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

with GL.API.Doubles;
with GL.Enums.Getter;

package body GL.Immediate is
   Error_Checking_Suspended : Boolean := False;

   overriding procedure Finalize (Token : in out Input_Token) is
   begin
      if not Token.Finalized then
         API.GL_End;
         Error_Checking_Suspended := False;
         Raise_Exception_On_OpenGL_Error;
         Token.Finalized := True;
      end if;
   end Finalize;

   function Start (Mode : Connection_Mode) return Input_Token is
   begin
      API.GL_Begin (Mode);
      Error_Checking_Suspended := True;
      return Input_Token'(Ada.Finalization.Limited_Controlled with
                            Mode => Mode, Finalized => False);
   end Start;

   procedure Add_Vertex (Token : Input_Token; Vertex : Vector2) is
      pragma Unreferenced (Token);
   begin
      API.Doubles.Vertex2 (Vertex);
   end Add_Vertex;
   
   procedure Add_Vertex (Token : Input_Token; Vertex : Vector3) is
      pragma Unreferenced (Token);
   begin
      API.Doubles.Vertex3 (Vertex);
   end Add_Vertex;
   
   procedure Add_Vertex (Token : Input_Token; Vertex : Vector4) is
      pragma Unreferenced (Token);
   begin
      API.Doubles.Vertex4 (Vertex);
   end Add_Vertex;

   procedure Set_Color (Value : Colors.Color) is
   begin
      API.Color (Value);
      if not Error_Checking_Suspended then
         Raise_Exception_On_OpenGL_Error;
      end if;
   end Set_Color;

   function Current_Color return Colors.Color is
      Ret : Colors.Color;
   begin
      API.Get_Color (Enums.Getter.Current_Color, Ret);
      if not Error_Checking_Suspended then
         Raise_Exception_On_OpenGL_Error;
      end if;
      return Ret;
   end Current_Color;

   procedure Set_Fog_Distance (Value : Double) is
   begin
      API.Fog_Coord (Value);
      if not Error_Checking_Suspended then
         Raise_Exception_On_OpenGL_Error;
      end if;
   end Set_Fog_Distance;

   function Current_Fog_Distance return Double is
      Value : aliased Double;
   begin
      API.Get_Double (Enums.Getter.Current_Fog_Coord, Value'Access);
      if not Error_Checking_Suspended then
         Raise_Exception_On_OpenGL_Error;
      end if;
      return Value;
   end Current_Fog_Distance;

   procedure Set_Normal (Value : Vector3) is
   begin
      API.Doubles.Normal (Value);
      if not Error_Checking_Suspended then
         Raise_Exception_On_OpenGL_Error;
      end if;
   end Set_Normal;

   function Current_Normal return Vector3 is
      Value : Vector3;
   begin
      API.Get_Double (Enums.Getter.Current_Normal, Value (X)'Access);
      if not Error_Checking_Suspended then
         Raise_Exception_On_OpenGL_Error;
      end if;
      return Value;
   end Current_Normal;

   procedure Set_Texture_Coordinates (Value : Vector4) is
   begin
      API.Doubles.Tex_Coord4 (Value);
      if not Error_Checking_Suspended then
         Raise_Exception_On_OpenGL_Error;
      end if;
   end Set_Texture_Coordinates;
   
   procedure Set_Texture_Coordinates (Value : Vector3) is
   begin
      API.Doubles.Tex_Coord3 (Value);
      if not Error_Checking_Suspended then
         Raise_Exception_On_OpenGL_Error;
      end if;
   end Set_Texture_Coordinates;
   
   procedure Set_Texture_Coordinates (Value : Vector2) is
   begin
      API.Doubles.Tex_Coord2 (Value);
      if not Error_Checking_Suspended then
         Raise_Exception_On_OpenGL_Error;
      end if;
   end Set_Texture_Coordinates;

   function Current_Texture_Coordinates return Vector4 is
      Value : Vector4;
   begin
      API.Get_Double (Enums.Getter.Current_Texture_Coords, Value (X)'Access);
      if not Error_Checking_Suspended then
         Raise_Exception_On_OpenGL_Error;
      end if;
      return Value;
   end Current_Texture_Coordinates;

end GL.Immediate;
