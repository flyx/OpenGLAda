--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with GL.Types.Colors;

private with GL.Low_Level;
private with GL.Toggles;

package GL.Fixed.Lighting is
   pragma Preelaborate;

   type Color_Control is (Single_Color, Separate_Specular_Color);
   type Shade_Model is (Flat, Smooth);

   ----------------------------------------------------------------------------
   --                     Global Lighting Configuration                      --
   ----------------------------------------------------------------------------

   procedure Enable_Lighting;
   procedure Disable_Lighting;
   function Lighting_Enabled return Boolean;

   procedure Enable_Local_Viewer;
   procedure Disable_Local_Viewer;
   function Local_Viewer_Enabled return Boolean;

   procedure Enable_Two_Side;
   procedure Disable_Two_Side;
   function Two_Side_Enabled return Boolean;

   procedure Set_Global_Ambient_Light (Value : Colors.Color);
   function Global_Ambient_Light return Colors.Color;

   procedure Set_Color_Control (Value : Color_Control);
   function Current_Color_Control return Color_Control;

   procedure Set_Shade_Model (Value : Shade_Model);
   function Current_Shade_Model return Shade_Model;

   ----------------------------------------------------------------------------
   --                             Light Objects                              --
   ----------------------------------------------------------------------------

   type Light_Object (<>) is tagged private;
   type Light_Index is range 0 .. 7;

   procedure Enable (Source : Light_Object);
   procedure Disable (Source : Light_Object);
   function Enabled (Source : Light_Object) return Boolean;

   procedure Set_Ambient (Source : Light_Object; Color : Colors.Color);
   function Ambient (Source : Light_Object) return Colors.Color;

   procedure Set_Diffuse (Source : Light_Object; Color : Colors.Color);
   function Diffuse (Source : Light_Object) return Colors.Color;

   procedure Set_Specular (Source : Light_Object; Color : Colors.Color);
   function Specular (Source : Light_Object) return Colors.Color;

   procedure Set_Position (Source : Light_Object;
                           Position : Types.Singles.Vector4);
   function Position (Source : Light_Object) return Types.Singles.Vector4;

   procedure Set_Spot_Direction (Source : Light_Object;
                                 Direction : Types.Singles.Vector3);
   function Spot_Direction (Source : Light_Object) return Types.Singles.Vector3;

   -- TBD: spot exponent, spot cutoff, attenduation

   function Light (Index : Light_Index) return Light_Object;

private
   type Light_Object (Identifier : Toggles.Toggle) is tagged null record;

   for Color_Control use (Single_Color            => 16#81F9#,
                          Separate_Specular_Color => 16#81FA#);
   for Color_Control'Size use Low_Level.Enum'Size;

   for Shade_Model use (Flat   => 16#1D00#,
                        Smooth => 16#1D01#);
   for Shade_Model'Size use Low_Level.Enum'Size;

end GL.Fixed.Lighting;
