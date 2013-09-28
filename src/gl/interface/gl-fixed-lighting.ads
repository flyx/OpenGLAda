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
   
   procedure Set_Position (Source : Light_Object; Position : Types.Singles.Vector4);
   procedure Set_Position (Source : Light_Object; Position : Types.Ints.Vector4);
   function Position (Source : Light_Object) return Types.Singles.Vector4;
   
   procedure Set_Spot_Direction (Source : Light_Object;
                                 Direction : Types.Singles.Vector3);
   function Spot_Direction (Source : Light_Object) return Types.Singles.Vector3;
   
   -- TBD: spot exponent, spot cutoff, attenduation
   
   function Light (Index : Light_Index) return Light_Object;
   
private
   type Light_Object (Identifier : Toggles.Toggle) is tagged null record;
   
   Lights : constant array (Light_Index) of access constant Light_Object :=
      (0 => new Light_Object'(Identifier => Toggles.Light0),
       1 => new Light_Object'(Identifier => Toggles.Light1),
       2 => new Light_Object'(Identifier => Toggles.Light2),
       3 => new Light_Object'(Identifier => Toggles.Light3),
       4 => new Light_Object'(Identifier => Toggles.Light4),
       5 => new Light_Object'(Identifier => Toggles.Light5),
       6 => new Light_Object'(Identifier => Toggles.Light6),
       7 => new Light_Object'(Identifier => Toggles.Light7));
   
   for Color_Control use (Single_Color            => 16#81F9#,
                          Separate_Specular_Color => 16#81FA#);
   for Color_Control'Size use Low_Level.Enum'Size;
   
   for Shade_Model use (Flat   => 16#1D00#,
                        Smooth => 16#1D01#);
   for Shade_Model'Size use Low_Level.Enum'Size;
      
end GL.Fixed.Lighting;