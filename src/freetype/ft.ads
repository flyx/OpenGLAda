
with System;

with Interfaces.C; use Interfaces.C;

with GL.Types;

package FT is
   pragma Preelaborate;

   type Render_Mode is (Render_Mode_Normal, Render_Mode_Light,
                           Render_Mode_Mono, Render_Mode_LCD,
                           Render_Mode_LCD_V, Render_Mode_Max);
   type Memory is private;
   type Stream is private;

   subtype FT_Bool is unsigned_char;
   subtype FT_Error is GL.Types.int;
   subtype FT_String is char;
   subtype FT_ULong is unsigned_long;

   FT_Exception : exception;

private
   type Memory is new System.Address;
   type Stream is new System.Address;
end FT;
