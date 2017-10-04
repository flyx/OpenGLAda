--------------------------------------------------------------------------------
-- Copyright (c) 2012, Felix Krause <flyx@isobeef.org>
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

with GL.Objects.Programs;
with GL.Types;
with GL.Types.Colors;
with GL.Uniforms;

package FT.OGL is
   type Character_Record is private;

   function Character_Data_To_String (Char : Character;
                                      Data : Character_Record) return String;
   procedure Initialize_Font_Data (Font_File : String);
   procedure Print_Character_Metadata (Data : Character_Record);
   procedure Render_Text (Render_Program : GL.Objects.Programs.Program;
                          Text   : String; X, Y, Scale : GL.Types.Single;
                          Colour : GL.Types.Colors.Basic_Color;
                          Texture_ID, Projection_Matrix_ID, Colour_ID : GL.Uniforms.Uniform;
                          Projection_Matrix : GL.Types.Singles.Matrix4);
private
   type Character_Record is record
      Texture   : GL.Objects.Textures.Texture;
      Width     : GL.Types.Int := 0;
      Rows      : GL.Types.Int := 0;
      Left      : GL.Types.Int := 0;
      Top       : GL.Types.Int := 0;
      Advance_X : GL.Types.Int := 0;
   end record;

end FT.OGL;
