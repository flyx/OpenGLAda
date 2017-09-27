
with Ada.Containers.Ordered_Maps;

package body FT.Errors is

   type String_43 is new String (1 .. 43);

   package Error_Map_Package is new
       Ada.Containers.Ordered_Maps (Natural, String_43);
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
   Error_List.Include (16#0000#, "No error                                   ");
   --  generic errors
   Error_List.Include (16#0001#, "Cannot Open Resource                       ");
   Error_List.Include (16#0002#, "Unknown File Format                        ");
   Error_List.Include (16#0003#, "Invalid File Format                        ");
   Error_List.Include (16#0004#, "Invalid FreeType version                   ");
   Error_List.Include (16#0005#, "module version is too low                  ");
   Error_List.Include (16#0006#, "invalid argument                           ");
   Error_List.Include (16#0007#, "unimplemented feature                      ");
   Error_List.Include (16#0008#, "broken table                               ");
   Error_List.Include (16#0009#, "broken offset within table                 ");
   Error_List.Include (16#000A#, "array allocation size too large            ");
   Error_List.Include (16#000B#, "missing module                             ");
   Error_List.Include (16#000C#, "missing property                           ");
   --  glyph/character errors
   Error_List.Include (16#0010#, "invalid glyph index                        ");
   Error_List.Include (16#0011#, "invalid character code                     ");
   Error_List.Include (16#0012#, "unsupported glyph image format             ");
   Error_List.Include (16#0013#, "cannot render this glyph format            ");
   Error_List.Include (16#0014#, "invalid outline                            ");
   Error_List.Include (16#0015#, "invalid composite glyph                    ");
   Error_List.Include (16#0016#, "too many hints                             ");
   Error_List.Include (16#0017#, "invalid pixel size                         ");
   --  handle errors
   Error_List.Include (16#0020#, "invalid object handle                      ");
   Error_List.Include (16#0021#, "invalid library handle                     ");
   Error_List.Include (16#0022#, "invalid module handle                      ");
   Error_List.Include (16#0023#, "invalid face handle                        ");
   Error_List.Include (16#0024#, "invalid size handle                        ");
   Error_List.Include (16#0025#, "invalid glyph slot handle                  ");
   Error_List.Include (16#0026#, "invalid charmap handle                     ");
   Error_List.Include (16#0027#, "invalid cache manager handle               ");
   Error_List.Include (16#0028#, "invalid stream handle                      ");
   --  driver errors
   Error_List.Include (16#0030#, "too many modules                           ");
   Error_List.Include (16#0031#, "too many extensions                        ");
   -- memory errors
   Error_List.Include (16#0040#, "out of memory                              ");
   Error_List.Include (16#0041#, "unlisted object                            ");
   --  stream errors
   Error_List.Include (16#0051#, "cannot open stream                         ");
   Error_List.Include (16#0052#, "invalid stream seek                        ");
   Error_List.Include (16#0053#, "invalid stream skip                        ");
   Error_List.Include (16#0054#, "invalid stream read                        ");
   Error_List.Include (16#0055#, "invalid stream operation                   ");
   Error_List.Include (16#0056#, "invalid frame operation                    ");
   Error_List.Include (16#0057#, "nested frame access                        ");
   Error_List.Include (16#0058#, "invalid frame read                         ");
   --  raster errors
   Error_List.Include (16#0060#, "raster uninitialized                       ");
   Error_List.Include (16#0061#, "raster corrupted                           ");
   Error_List.Include (16#0062#, "raster overflow                            ");
   Error_List.Include (16#0063#, "negative height while rastering            ");
   --  cache errors
   Error_List.Include (16#0070#, "too many registered caches                 ");
   --  TrueType and SFNT errors
   Error_List.Include (16#0080#, "invalid opcode                             ");
   Error_List.Include (16#0081#, "too few arguments                          ");
   Error_List.Include (16#0082#, "stack overflow                             ");
   Error_List.Include (16#0083#, "code overflow                              ");
   Error_List.Include (16#0084#, "bad argument                               ");
   Error_List.Include (16#0085#, "division by zero                           ");
   Error_List.Include (16#0086#, "invalid reference                          ");
   Error_List.Include (16#0087#, "found debug opcode                         ");
   Error_List.Include (16#0088#, "found ENDF opcode in execution stream      ");
   Error_List.Include (16#0089#, "nested DEFS                                ");
   Error_List.Include (16#008A#, "invalid code range                         ");
   Error_List.Include (16#008B#, "execution context too long                 ");
   Error_List.Include (16#008C#, "too many function definitions              ");
   Error_List.Include (16#008D#, "too many instruction definitions           ");
   Error_List.Include (16#008E#, "SFNT font table missing                    ");
   Error_List.Include (16#008F#, "horizontal header (hhea) table missing     ");
   Error_List.Include (16#0090#, "locations (loca) table missing             ");
   Error_List.Include (16#0091#, "name table missing                         ");
   Error_List.Include (16#0092#, "character map (cmap) table missing         ");
   Error_List.Include (16#0093#, "horizontal metrics (hmtx) table missing    ");
   Error_List.Include (16#0094#, "PostScript (post) table missing            ");
   Error_List.Include (16#0095#, "invalid horizontal metrics                 ");
   Error_List.Include (16#0096#, "invalid character map (cmap) format        ");
   Error_List.Include (16#0097#, "invalid ppem value                         ");
   Error_List.Include (16#0098#, "invalid vertical metrics                   ");
   Error_List.Include (16#0099#, "could not find context                     ");
   Error_List.Include (16#009A#, "invalid PostScript (post) table format     ");
   Error_List.Include (16#009B#, "invalid PostScript (post) table            ");
   Error_List.Include (16#009C#, "found FDEF or IDEF opcode in glyf bytecode ");
   --  CFF, CID, and Type 1 errors
   Error_List.Include (16#00A0#, "opcode syntax error                        ");
   Error_List.Include (16#00A1#, "argument stack underflow                   ");
   Error_List.Include (16#00A2#, "ignore                                     ");
   Error_List.Include (16#00A3#, "no Unicode glyph name found                ");
   Error_List.Include (16#00A4#, "glyph too big for hinting                  ");
   --  BDF errors
   Error_List.Include (16#00B0#, "`STARTFONT' field missing                  ");
   Error_List.Include (16#00B1#, "`FONT' field missing                       ");
   Error_List.Include (16#00B2#, "`SIZE' field missing                       ");
   Error_List.Include (16#00B3#, "`FONTBOUNDINGBOX' field missing            ");
   Error_List.Include (16#00B4#, "`CHARS' field missing                      ");
   Error_List.Include (16#00B5#, "`STARTCHAR' field missing                  ");
   Error_List.Include (16#00B6#, "`ENCODING' field missing                   ");
   Error_List.Include (16#00B7#, "`BBX' field missing                        ");
   Error_List.Include (16#00B8#, "`BBX' too big                              ");
   Error_List.Include (16#00B9#, "Font header corrupted or missing fields    ");
   Error_List.Include (16#00BA#, "Font glyphs corrupted or missing fields    ");
end FT.Errors;
