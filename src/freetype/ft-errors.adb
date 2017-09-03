
with Ada.Containers.Ordered_Maps;

package body FT.Errors is

   type String_36 is new String (1 .. 36);

   package Error_Map_Package is new
       Ada.Containers.Ordered_Maps (Natural, String_36);
   Error_List : Error_Map_Package.Map;

   function Error (Code : GL.Types.Int) return String is
   begin
   if Error_List.Contains (Natural (Code)) then
      return FT.FT_Error'Image (Code) & " " &
          String ( Error_List.Element (Natural (Code)));
   else
     return "Error code " & GL.Types.Int'Image (Code) & " not implemented yet.";
   end if;
   end Error;

begin
   Error_List.Include (16#0000#, "No error                            ");
   --  generic errors
   Error_List.Include (16#0001#, "Cannot Open Resource                ");
   Error_List.Include (16#0002#, "Unknown File Format                 ");
   Error_List.Include (16#0003#, "Invalid File Format                 ");
   Error_List.Include (16#0004#, "Invalid FreeType version            ");
   Error_List.Include (16#0005#, "module version is too low           ");
   Error_List.Include (16#0006#, "invalid argument                    ");
   Error_List.Include (16#0007#, "unimplemented feature               ");
   Error_List.Include (16#0008#, "broken table                        ");
   Error_List.Include (16#0009#, "broken offset within table          ");
   Error_List.Include (16#000A#, "array allocation size too large     ");
   Error_List.Include (16#000B#, "missing module                      ");
   Error_List.Include (16#000C#, "missing property                    ");
   --  glyph/character errors
   Error_List.Include (16#0010#, "invalid glyph index                 ");
   Error_List.Include (16#0011#, "invalid character code              ");
   Error_List.Include (16#0012#, "unsupported glyph image format      ");
   Error_List.Include (16#0013#, "cannot render this glyph format     ");
   Error_List.Include (16#0014#, "invalid outline                     ");
   Error_List.Include (16#0015#, "invalid composite glyph             ");
   Error_List.Include (16#0016#, "too many hints                      ");
   Error_List.Include (16#0017#, "invalid pixel size                  ");
   --  handle errors
   Error_List.Include (16#0020#, "invalid object handle               ");
   Error_List.Include (16#0021#, "invalid library handle              ");
   Error_List.Include (16#0022#, "invalid module handle               ");
   Error_List.Include (16#0023#, "invalid face handle                 ");
   Error_List.Include (16#0024#, "invalid size handle                 ");
   Error_List.Include (16#0025#, "invalid glyph slot handle           ");
   Error_List.Include (16#0026#, "invalid charmap handle              ");
   Error_List.Include (16#0027#, "invalid cache manager handle        ");
   Error_List.Include (16#0028#, "invalid stream handle               ");
   --  driver errors
   Error_List.Include (16#0030#, "too many modules                    ");
   Error_List.Include (16#0031#, "too many extensions                 ");
   -- memory errors
   Error_List.Include (16#0040#, "out of memory                       ");
   Error_List.Include (16#0041#, "unlisted object                     ");
   --  stream errors
   Error_List.Include (16#0051#, "cannot open stream                  ");
   Error_List.Include (16#0052#, "invalid stream seek                 ");
   Error_List.Include (16#0053#, "invalid stream skip                 ");
   Error_List.Include (16#0054#, "invalid stream read                 ");
   Error_List.Include (16#0055#, "invalid stream operation            ");
   Error_List.Include (16#0056#, "invalid frame operation             ");
   Error_List.Include (16#0057#, "nested frame access                 ");
   Error_List.Include (16#0058#, "invalid frame read                  ");

end FT.Errors;
