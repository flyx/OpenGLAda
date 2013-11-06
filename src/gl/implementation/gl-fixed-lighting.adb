-------------------------------------------------------------------------------
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
-------------------------------------------------------------------------------

with GL.Enums.Getter;
with GL.API.Singles;
with GL.API.Ints;

package body GL.Fixed.Lighting is
   use type Toggles.Toggle_State;
   
   Light_Model_Enabled  : aliased constant Int := 1;
   Light_Model_Disabled : aliased constant Int := 0;

   function Light (Index : Light_Index) return Light_Object is
   begin
      return Lights (Index).all;
   end Light;

   procedure Enable_Lighting is
   begin
      Toggles.Enable (Toggles.Lighting);
   end Enable_Lighting;
      
   procedure Disable_Lighting is
   begin
      Toggles.Disable (Toggles.Lighting);
   end Disable_Lighting;
      
   function Lighting_Enabled return Boolean is
   begin
      return Toggles.State (Toggles.Lighting) = Toggles.Enabled;
   end Lighting_Enabled;

   procedure Enable_Local_Viewer is
   begin
      API.Light_Model_Toggles (Enums.Local_Viewer, Light_Model_Enabled'Access);
      Raise_Exception_On_OpenGL_Error;
   end Enable_Local_Viewer;
      
   procedure Disable_Local_Viewer is
   begin
      API.Light_Model_Toggles (Enums.Local_Viewer, Light_Model_Disabled'Access);
      Raise_Exception_On_OpenGL_Error;
   end Disable_Local_Viewer;
   
   function Local_Viewer_Enabled return Boolean is
      Value : aliased Low_Level.Bool;
   begin
      API.Get_Boolean (Enums.Getter.Light_Model_Local_Viewer, Value'Access);
      Raise_Exception_On_OpenGL_Error;
      return Boolean (Value);
   end Local_Viewer_Enabled;

   procedure Enable_Two_Side is
   begin
      API.Light_Model_Toggles (Enums.Two_Side, Light_Model_Enabled'Access);
      Raise_Exception_On_OpenGL_Error;
   end Enable_Two_Side;
      
   procedure Disable_Two_Side is
   begin
      API.Light_Model_Toggles (Enums.Two_Side, Light_Model_Disabled'Access);
      Raise_Exception_On_OpenGL_Error;
   end Disable_Two_Side;
   
   function Two_Side_Enabled return Boolean is
      Value : aliased Low_Level.Bool;
   begin
      API.Get_Boolean (Enums.Getter.Light_Model_Two_Side, Value'Access);
      Raise_Exception_On_OpenGL_Error;
      return Boolean (Value);
   end Two_Side_Enabled;

   procedure Set_Global_Ambient_Light (Value : Colors.Color) is
   begin
      API.Light_Model_Color (Enums.Ambient, Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Global_Ambient_Light;
      
   function Global_Ambient_Light return Colors.Color is
      Value : Colors.Color;
   begin
      API.Get_Color (Enums.Getter.Light_Model_Ambient, Value);
      Raise_Exception_On_OpenGL_Error;
      return Value;
   end Global_Ambient_Light;

   procedure Set_Color_Control (Value : Color_Control) is
      Aliased_Value : aliased constant Color_Control := Value;
   begin
      API.Light_Model_Color_Control (Enums.Color_Control, Aliased_Value'Access);
      Raise_Exception_On_OpenGL_Error;
   end Set_Color_Control;
      
   function Current_Color_Control return Color_Control is
      Value : aliased Color_Control;
   begin
      API.Get_Color_Control (Enums.Getter.Light_Model_Color_Control,
                             Value'Access);
      Raise_Exception_On_OpenGL_Error;
      return Value;
   end Current_Color_Control;

   procedure Set_Shade_Model (Value : Shade_Model) is
   begin
      API.Shade_Model (Value);
      Raise_Exception_On_OpenGL_Error;
   end Set_Shade_Model;
      
   function Current_Shade_Model return Shade_Model is
      Value : aliased Shade_Model;
   begin
      API.Get_Shade_Model (Enums.Getter.Shade_Model, Value'Access);
      Raise_Exception_On_OpenGL_Error;
      return Value;
   end Current_Shade_Model;

   procedure Enable (Source : Light_Object) is
   begin
      Toggles.Enable (Source.Identifier);
   end Enable;
   
   procedure Disable (Source : Light_Object) is
   begin
      Toggles.Disable (Source.Identifier);
   end Disable;
   
   function Enabled (Source : Light_Object) return Boolean is
   begin
      return Toggles.State (Source.Identifier) = Toggles.Enabled;
   end Enabled;

   procedure Set_Ambient (Source : Light_Object; Color : Colors.Color) is
   begin
      API.Light_Color (Source.Identifier, Enums.Ambient, Color);
      Raise_Exception_On_OpenGL_Error;
   end Set_Ambient;
   
   function Ambient (Source : Light_Object) return Colors.Color is
      Value : Colors.Color;
   begin
      API.Get_Light_Color (Source.Identifier, Enums.Ambient, Value);
      Raise_Exception_On_OpenGL_Error;
      return Value;
   end Ambient;

   procedure Set_Diffuse (Source : Light_Object; Color : Colors.Color) is
   begin
      API.Light_Color (Source.Identifier, Enums.Diffuse, Color);
      Raise_Exception_On_OpenGL_Error;
   end Set_Diffuse;
   
   function Diffuse (Source : Light_Object) return Colors.Color is
      Value : Colors.Color;
   begin
      API.Get_Light_Color (Source.Identifier, Enums.Diffuse, Value);
      Raise_Exception_On_OpenGL_Error;
      return Value;
   end Diffuse;

   procedure Set_Specular (Source : Light_Object; Color : Colors.Color) is
   begin
      API.Light_Color (Source.Identifier, Enums.Specular, Color);
      Raise_Exception_On_OpenGL_Error;
   end Set_Specular;
   
   function Specular (Source : Light_Object) return Colors.Color is
      Value : Colors.Color;
   begin
      API.Get_Light_Color (Source.Identifier, Enums.Specular, Value);
      Raise_Exception_On_OpenGL_Error;
      return Value;
   end Specular;

   procedure Set_Position (Source : Light_Object;
                           Position : Types.Singles.Vector4) is
   begin
      API.Singles.Light_Position (Source.Identifier, Enums.Position, Position);
      Raise_Exception_On_OpenGL_Error;
   end Set_Position;
      
   procedure Set_Position (Source : Light_Object;
                           Position : Types.Ints.Vector4) is
   begin
      API.Ints.Light_Position (Source.Identifier, Enums.Position, Position);
      Raise_Exception_On_OpenGL_Error;
   end Set_Position;
      
   function Position (Source : Light_Object) return Types.Singles.Vector4 is
      Value : Types.Singles.Vector4;
   begin
      API.Singles.Get_Light_Position (Source.Identifier, Enums.Position, Value);
      Raise_Exception_On_OpenGL_Error;
      return Value;
   end Position;

   procedure Set_Spot_Direction (Source : Light_Object;
                                 Direction : Types.Singles.Vector3) is
   begin
      API.Singles.Light_Direction (Source.Identifier, Enums.Spot_Direction,
                                   Direction);
      Raise_Exception_On_OpenGL_Error;
   end Set_Spot_Direction;
      
   function Spot_Direction (Source : Light_Object) return Types.Singles.Vector3 is
      Value : Singles.Vector3;
   begin
      API.Singles.Get_Light_Direction (Source.Identifier, Enums.Spot_Direction,
                                       Value);
      Raise_Exception_On_OpenGL_Error;
      return Value;
   end Spot_Direction;
   
end GL.Fixed.Lighting;
