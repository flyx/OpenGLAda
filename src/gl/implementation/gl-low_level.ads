--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with GL.Types;

package GL.Low_Level is
   pragma Preelaborate;

   use GL.Types;

   -- This package contains some low-level types that are used by the raw C
   -- interface of the OpenGL API. They are converted to types that are easier
   -- to handle by the wrapper and thus are not needed for using the wrapper.
   -- However, they might be used by other APIs that use OpenGL and thus are
   -- exposed publicly here.


   -- Boolean with the representation used by the OpenGL API (unsigned char).
   -- Is converted to a standard Boolean by the wrapper.
   type Bool is new Boolean;

   -- This type is never used directly. However, enumerations refer to it for
   -- defining their Size attribute.
   subtype Enum is C.unsigned;

   -- Bitfields are usually converted to a record with Boolean fields in the
   -- wrapper. However, for interacting with the OpenGL library, these records
   -- are converted back to the raw Bitfield type (by means of
   -- Unchecked_Conversion). Using the record directly with the C interface
   -- requires it to have the C_Pass_By_Value conversion, which for some reason
   -- breaks linking on Windows with StdCall convention (possibly a GNAT bug).
   subtype Bitfield is C.unsigned;

   type Single_Array is array (Positive range <>) of aliased Single;
   type Double_Array is array (Positive range <>) of aliased Double;

   type Int_Array  is array (Positive range <>) of aliased Int;
   type UInt_Array is array (Positive range <>) of aliased UInt;

   -- These types totally are not pointers. No idea why they are named like this.
   subtype IntPtr is C.long;
   subtype SizeIPtr is C.long;

   type Char_Access_Array is array (Size range <>) of access C.char;

   -- used in API calls
   type Size_Access is access all Types.Size;
   type Bool_Access is access all Bool;
   subtype Zero is Int range 0 .. 0;
private
   for Bool use (False => 0, True => 1);
   for Bool'Size use C.unsigned_char'Size;

   pragma Convention (C, Single_Array);
   pragma Convention (C, Double_Array);
   pragma Convention (C, Int_Array);
   pragma Convention (C, UInt_Array);
   pragma Convention (C, Size_Access);
   pragma Convention (C, Bool_Access);
   pragma Convention (C, Char_Access_Array);
end GL.Low_Level;
