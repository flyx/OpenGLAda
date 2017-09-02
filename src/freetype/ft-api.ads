
with System;

package FT.API is
   pragma Preelaborate;

   type Face_Ptr is new System.Address;
   type Glyph_Slot_Ptr is new System.Address;
   type Library_Ptr is new System.Address;

   type Render_Mode is (Render_Mode_Normal, Render_Mode_Light,
                           Render_Mode_Mono, Render_Mode_LCD,
                           Render_Mode_LCD_V, Render_Mode_Max);
end FT.API;
