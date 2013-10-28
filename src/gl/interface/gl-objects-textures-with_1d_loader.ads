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

with System;

generic
   type Base (<>) is new Texture_Proxy with private;
package GL.Objects.Textures.With_1D_Loader is
   pragma Preelaborate;

   type Target is new Base with null record;

   procedure Load_Empty_Texture
     (Object: Target; Level : Mipmap_Level;
      Internal_Format : Pixel_Data.Internal_Format;
      Width : Types.Size);

   type Fillable_Target is new With_1D_Loader.Target with null record;

   procedure Load_From_Data
     (Object : Fillable_Target; Level : Mipmap_Level;
      Internal_Format : Pixel_Data.Internal_Format;
      Width : Types.Size;
      Source_Format : Pixel_Data.Format;
      Source_Type   : Pixel_Data.Data_Type;
      Source        : System.Address);
end GL.Objects.Textures.With_1D_Loader;
