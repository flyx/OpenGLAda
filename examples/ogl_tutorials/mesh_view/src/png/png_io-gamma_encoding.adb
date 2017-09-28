-- $Id: png_io-gamma_encoding.adb,v 1.5 2009/05/04 19:31:14 sangwine Exp $
---------------------------------------------------------------------
---------------------------------------------------------------------
-- PNG_IO  - Ada95 Portable Network Graphics Input/Output Package  --
--                                                                 --
--                http://png-io.sourceforge.net/                   --
--                                                                 --
--        Copyright (c) 1999-2009 Dr Stephen J. Sangwine           --
--                                S.Sangwine@IEEE.org              --
--                                                                 --
-- This software was created by Stephen J. Sangwine. He hereby     --
-- asserts his Moral Right to be identified as author of this      --
-- software.                                                       --
---------------------------------------------------------------------
---------------------------------------------------------------------
-- PNG_IO is free software; you can redistribute it and/or modify  --
-- it under the terms of the GNU General Public License as         --
-- published by the Free Software Foundation; either version 3 of  --
-- the License, or (at your option) any later version.             --
--                                                                 --
-- PNG_IO is distributed in the hope that it will be useful, but   --
-- WITHOUT ANY WARRANTY; without even the implied warranty of      --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    --
-- GNU General Public License for more details.                    --
--                                                                 --
-- You should have received a copy of the GNU General Public       --
-- License along with this software (in the file gpl.txt).         --
-- If not access  http://www.gnu.org/licenses/                     --
---------------------------------------------------------------------
---------------------------------------------------------------------

with Ada.Numerics.Elementary_Functions;

package body PNG_IO.Gamma_Encoding is

   pragma Assert (Input_Sample'Pos (Input_Sample'First) = 0);
   pragma Assert (Output_Sample'Pos (Output_Sample'First) = 0);

   -- The gamma lookup table (below) handles a double conversion, from the
   -- input gamma to linearized pixels, and then to the output gamma. Either
   -- (or indeed both!) of the gamma values may be unity (i.e. 100_000). The
   -- two stage calculation is merged here into one stage. The ISO standard
   -- Sections 12.2 and 13.13 explains how the conversion is done. Simply
   -- put, the pixel values from the input file are converted to floats in the
   -- range 0.0 .. 1.0, the two gamma exponents are applied, and the result is
   -- then scaled into the output range.

   type Lookup_Table is array (Input_Sample) of Output_Sample;

   LUT : Lookup_Table; -- This is initialised below during elaboration.

   function To_Output_Gamma (I : Input_Sample) return Output_Sample is
   begin
      return LUT (I);
   end To_Output_Gamma;

begin

   Initialise_LUT : declare

      IG : constant Float := 1.0e5 / Float (Input_Gamma);
      OG : constant Float := 1.0e5 / Float (Output_Gamma);

      Exponent : constant Float := Float (IG) / Float (OG);

      use Ada.Numerics.Elementary_Functions; -- For the ** operator.

      IL : constant Float := Float (Input_Sample'Pos (Input_Sample'Last));
      OL : constant Float := Float (Output_Sample'Pos (Output_Sample'Last));

   begin

      for P in LUT'Range loop
         LUT (P) := Output_Sample'Val (Integer (((Float (Input_Sample'Pos (P)) / IL)**Exponent) * OL));
      end loop;

   end Initialise_LUT;

end PNG_IO.Gamma_Encoding;
