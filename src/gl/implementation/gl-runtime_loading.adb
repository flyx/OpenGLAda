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

with Ada.Unchecked_Conversion;

package body GL.Runtime_Loading is
   use type Function_Maps.Cursor;
   use type System.Address;

   procedure Load_Function_To_Map (Function_Name : String;
                                   Position : out Function_Maps.Cursor) is
      Inserted : Boolean;
   begin
      Loaded.Insert (Key      => Function_Name,
                     New_Item => Raw_Subprogram_Reference (Function_Name),
                     Position => Position,
                     Inserted => Inserted);
   end Load_Function_To_Map;
   pragma Inline (Load_Function_To_Map);

   function Available (Function_Name : String) return Boolean is
      Position : Function_Maps.Cursor := Loaded.Find (Function_Name);
   begin
      if Position = Function_Maps.No_Element then
         Load_Function_To_Map (Function_Name, Position);
      end if;
      return Function_Maps.Element (Position) /= System.Null_Address;
   end Available;

   function Load (Function_Name : String) return Function_Reference is
      function As_Function_Reference is new Ada.Unchecked_Conversion (
        Source => System.Address, Target => Function_Reference
      );

      Position : Function_Maps.Cursor := Loaded.Find (Function_Name);
   begin
      if Position = Function_Maps.No_Element then
         Load_Function_To_Map (Function_Name, Position);
         if Position = Function_Maps.No_Element then
            Load_Function_To_Map (Function_Name & "ARB", Position);
            if Position = Function_Maps.No_Element then
               Load_Function_To_Map (Function_Name & "EXT", Position);
            end if;
         end if;
      end if;
      return As_Function_Reference (Function_Maps.Element (Position));
   end Load;
   
   function Function_Without_Params return Return_Type is
      type Function_Reference is
        access function return Return_Type;
      pragma Convention (StdCall, Function_Reference);
      
      function Load_Function is new Load (Function_Reference);
      
      Reference : constant Function_Reference := Load_Function (Function_Name);
   begin
      if Reference = null then
         raise Feature_Not_Supported_Exception with Function_Name;
      else
         return Reference.all;
      end if;
   end Function_Without_Params;

   function Function_With_1_Param (Param1 : Param1_Type) return Return_Type is
      type Function_Reference is
        access function (Param1 : Param1_Type) return Return_Type;
      pragma Convention (StdCall, Function_Reference);

      function Load_Function is new Load (Function_Reference);

      Reference : constant Function_Reference := Load_Function (Function_Name);
   begin
      if Reference = null then
         raise Feature_Not_Supported_Exception with Function_Name;
      else
         return Reference (Param1);
      end if;
   end Function_With_1_Param;

   function Function_With_2_Params (Param1 : Param1_Type;
                                    Param2 : Param2_Type)
                                   return Return_Type is
     type Function_Reference is
       access function (Param1 : Param1_Type; Param2 : Param2_Type)
       return Return_Type;
     pragma Convention (StdCall, Function_Reference);

     function Load_Function is new Load (Function_Reference);
     Reference : constant Function_Reference := Load_Function (Function_Name);
   begin
     if Reference = null then
        raise Feature_Not_Supported_Exception with Function_Name;
     else
        return Reference (Param1, Param2);
     end if;
   end Function_With_2_Params;
   
   function Function_With_3_Params (Param1 : Param1_Type;
                                    Param2 : Param2_Type;
                                    Param3 : Param3_Type)
                                    return Return_Type is
      type Function_Reference is
        access function (Param1 : Param1_Type; Param2 : Param2_Type;
                         Param3 : Param3_Type) return Return_Type;
      pragma Convention (StdCall, Function_Reference);
      
      function Load_Function is new Load (Function_Reference);
      Reference : constant Function_Reference := Load_Function (Function_Name);
   begin
      if Reference = null then
         raise Feature_Not_Supported_Exception with Function_Name;
      else
         return Reference.all (Param1, Param2, Param3);
      end if;
   end Function_With_3_Params;
   
   function Array_Getter_With_4_Params (Param1 : Param1_Type;
                                        Max_Size : Types.Size)
                                        return Array_Type is
      use type Types.Size;
      
      type Procedure_Reference is
        access procedure (Param1 : Param1_Type; Max : Types.Size;
                          Returned_Size : in out Types.Size;
                          Values : in out Array_Type);
      pragma Convention (StdCall, Procedure_Reference);
      
      function Load_Function is new Load (Procedure_Reference);
      Reference : constant Procedure_Reference
        := Load_Function (Procedure_Name);
      
      Actual_Size : Types.Size := 0;
   
      Ret : Array_Type (1 .. Max_Size);
   begin
      if Reference = null then
         raise Feature_Not_Supported_Exception with Procedure_Name;
      end if;
      
      Reference.all (Param1, Max_Size, Actual_Size, Ret);
      if Actual_Size /= Max_Size then
         return Ret (1 .. Actual_Size);
      else
         return Ret;
      end if;
   end Array_Getter_With_4_Params;
      

   procedure Procedure_Without_Params is
      type Procedure_Reference is
        access procedure;
      pragma Convention (StdCall, Procedure_Reference);

      function Load_Procedure is new Load (Procedure_Reference);
      Reference : constant Procedure_Reference
        := Load_Procedure (Procedure_Name);
   begin
      if Reference = null then
         raise Feature_Not_Supported_Exception with Procedure_Name;
      else
         Reference.all;
      end if;
   end Procedure_Without_Params;

   procedure Procedure_With_1_Param (Param1 : Param1_Type) is
      type Procedure_Reference is
        access procedure (Param1 : Param1_Type);
      pragma Convention (StdCall, Procedure_Reference);

      function Load_Procedure is new Load (Procedure_Reference);
      Reference : constant Procedure_Reference
        := Load_Procedure (Procedure_Name);
   begin
      if Reference = null then
         raise Feature_Not_Supported_Exception with Procedure_Name;
      else
         Reference (Param1);
      end if;
   end Procedure_With_1_Param;

   procedure Procedure_With_2_Params (Param1 : Param1_Type;
                                      Param2 : Param2_Type) is
      type Procedure_Reference is
        access procedure (Param1 : Param1_Type; Param2 : Param2_Type);
      pragma Convention (StdCall, Procedure_Reference);

      function Load_Procedure is new Load (Procedure_Reference);
      Reference : constant Procedure_Reference 
        := Load_Procedure (Procedure_Name);
   begin
      if Reference = null then
        raise Feature_Not_Supported_Exception with Procedure_Name;
      else
        Reference (Param1, Param2);
      end if;
   end Procedure_With_2_Params;

   procedure Procedure_With_3_Params (Param1 : Param1_Type;
                                      Param2 : Param2_Type;
                                      Param3 : Param3_Type) is
      type Procedure_Reference is
        access procedure (Param1 : Param1_Type; Param2 : Param2_Type;
                          Param3 : Param3_Type);
      pragma Convention (StdCall, Procedure_Reference);

      function Load_Procedure is new Load (Procedure_Reference);
      Reference : constant Procedure_Reference
        := Load_Procedure (Procedure_Name);
   begin
      if Reference = null then
        raise Feature_Not_Supported_Exception with Procedure_Name;
      else
        Reference (Param1, Param2, Param3);
      end if;
   end Procedure_With_3_Params;

   procedure Procedure_With_4_Params (Param1 : Param1_Type;
                                      Param2 : Param2_Type;
                                      Param3 : Param3_Type;
                                      Param4 : Param4_Type) is
      type Procedure_Reference is
        access procedure (Param1 : Param1_Type; Param2 : Param2_Type;
                          Param3 : Param3_Type; Param4 : Param4_Type);
      pragma Convention (StdCall, Procedure_Reference);

      function Load_Procedure is new Load (Procedure_Reference);
      Reference : constant Procedure_Reference
        := Load_Procedure (Procedure_Name);
   begin
      if Reference = null then
        raise Feature_Not_Supported_Exception with Procedure_Name;
      else
        Reference (Param1, Param2, Param3, Param4);
      end if;
   end Procedure_With_4_Params;
   
   procedure Procedure_With_5_Params (Param1 : Param1_Type;
                                      Param2 : Param2_Type;
                                      Param3 : Param3_Type;
                                      Param4 : Param4_Type;
                                      Param5 : Param5_Type) is
      type Procedure_Reference is
        access procedure (Param1 : Param1_Type; Param2 : Param2_Type;
                          Param3 : Param3_Type; Param4 : Param4_Type;
                          Param5 : Param5_Type);
      pragma Convention (StdCall, Procedure_Reference);
      
      function Load_Procedure is new Load (Procedure_Reference);
      Reference : constant Procedure_Reference
        := Load_Procedure (Procedure_Name);
   begin
      if Reference = null then
         raise Feature_Not_Supported_Exception with Procedure_Name;
      else 
         Reference (Param1, Param2, Param3, Param4, Param5);
      end if;
   end Procedure_With_5_Params;

   procedure Procedure_With_6_Params (Param1 : Param1_Type;
                                      Param2 : Param2_Type;
                                      Param3 : Param3_Type;
                                      Param4 : Param4_Type;
                                      Param5 : Param5_Type;
                                      Param6 : Param6_Type) is
      type Procedure_Reference is
        access procedure (Param1 : Param1_Type; Param2 : Param2_Type;
                          Param3 : Param3_Type; Param4 : Param4_Type;
                          Param5 : Param5_Type; Param6 : Param6_Type);
      pragma Convention (StdCall, Procedure_Reference);

      function Load_Procedure is new Load (Procedure_Reference);
      Reference : constant Procedure_Reference
        := Load_Procedure (Procedure_Name);
   begin
      if Reference = null then
         raise Feature_Not_Supported_Exception with Procedure_Name;
      else
         Reference (Param1, Param2, Param3, Param4, Param5, Param6);
      end if;
   end Procedure_With_6_Params;
   
   procedure Procedure_With_7_Params (Param1 : Param1_Type;
                                      Param2 : Param2_Type;
                                      Param3 : Param3_Type;
                                      Param4 : Param4_Type;
                                      Param5 : Param5_Type;
                                      Param6 : Param6_Type;
                                      Param7 : Param7_Type) is
      type Procedure_Reference is
        access procedure (Param1 : Param1_Type; Param2 : Param2_Type;
                          Param3 : Param3_Type; Param4 : Param4_Type;
                          Param5 : Param5_Type; Param6 : Param6_Type;
                          Param7 : Param7_Type);
      pragma Convention (StdCall, Procedure_Reference);
      
      function Load_Procedure is new Load (Procedure_Reference);
      Reference : constant Procedure_Reference
        := Load_Procedure (Procedure_Name);
   begin
      if Reference = null then
         raise Feature_Not_Supported_Exception with Procedure_Name;
      else
         Reference.all (Param1, Param2, Param3, Param4, Param5, Param6, Param7);
      end if;
   end Procedure_With_7_Params;
   
   procedure Procedure_With_8_Params (Param1 : Param1_Type;
                                      Param2 : Param2_Type;
                                      Param3 : Param3_Type;
                                      Param4 : Param4_Type;
                                      Param5 : Param5_Type;
                                      Param6 : Param6_Type;
                                      Param7 : Param7_Type;
                                      Param8 : Param8_Type) is
      type Procedure_Reference is
        access procedure (Param1 : Param1_Type; Param2 : Param2_Type;
                          Param3 : Param3_Type; Param4 : Param4_Type;
                          Param5 : Param5_Type; Param6 : Param6_Type;
                          Param7 : Param7_Type; Param8 : Param8_Type);
      pragma Convention (StdCall, Procedure_Reference);
      
      function Load_Procedure is new Load (Procedure_Reference);
      Reference : constant Procedure_Reference
        := Load_Procedure (Procedure_Name);
   begin
      if Reference = null then
         raise Feature_Not_Supported_Exception with Procedure_Name;
      else
         Reference.all (Param1, Param2, Param3, Param4,
                        Param5, Param6, Param7, Param8);
      end if;
   end Procedure_With_8_Params;
   
   procedure Procedure_With_10_Params (Param1 : Param1_Type;
                                       Param2 : Param2_Type;
                                       Param3 : Param3_Type;
                                       Param4 : Param4_Type;
                                       Param5 : Param5_Type;
                                       Param6 : Param6_Type;
                                       Param7 : Param7_Type;
                                       Param8 : Param8_Type;
                                       Param9 : Param9_Type;
                                       Param10 : Param10_Type) is
      type Procedure_Reference is
      access procedure (Param1 : Param1_Type; Param2 : Param2_Type;
                        Param3 : Param3_Type; Param4 : Param4_Type;
                        Param5 : Param5_Type; Param6 : Param6_Type;
                        Param7 : Param7_Type; Param8 : Param8_Type;
                        Param9 : Param9_Type; Param10 : Param10_Type);
      pragma Convention (StdCall, Procedure_Reference);
      
      function Load_Procedure is new Load (Procedure_Reference);
      Reference : constant Procedure_Reference
        := Load_Procedure (Procedure_Name);
   begin
      if Reference = null then
         raise Feature_Not_Supported_Exception with Procedure_Name;
      else
         Reference (Param1, Param2, Param3, Param4, Param5, Param6, Param7,
                    Param8, Param9, Param10);
      end if;
   end Procedure_With_10_Params;

   procedure Array_Proc_With_2_Params (Param1 : Size_Type;
                                       Param2 : Array_Type) is
      type Procedure_Reference is
        access procedure (Param1 : Size_Type;
                          Param2 : Array_Type);
      pragma Convention (StdCall, Procedure_Reference);
      
      function Load_Procedure is new Load (Procedure_Reference);
      Reference : constant Procedure_Reference
        := Load_Procedure (Procedure_Name);
   begin
      if Reference = null then
         raise Feature_Not_Supported_Exception with Procedure_Name;
      else
         Reference (Param1, Param2);
      end if;
   end Array_Proc_With_2_Params;
   
   procedure Array_Proc_With_3_Params (Param1 : Param1_Type;
                                       Param2 : Size_Type;
                                       Param3 : Array_Type) is
      type Procedure_Reference is
        access procedure (Param1 : Param1_Type; Param2 : Size_Type;
                          Param3 : Array_Type);
      pragma Convention (StdCall, Procedure_Reference);
      
      function Load_Procedure is new Load (Procedure_Reference);
      Reference : constant Procedure_Reference
        := Load_Procedure (Procedure_Name);
   begin
      if Reference = null then
         raise Feature_Not_Supported_Exception with Procedure_Name;
      else
         Reference (Param1, Param2, Param3);
      end if;
   end Array_Proc_With_3_Params;

   procedure Getter_With_2_Params (Param1 : Param1_Type;
                                   Value  : in out Value_Type) is
      type Procedure_Reference is
         access procedure (Param1 : Param1_Type;
                           Param2 : in out Value_Type);
      pragma Convention (StdCall, Procedure_Reference);
      
      function Load_Procedure is new Load (Procedure_Reference);
      Reference : constant Procedure_Reference
        := Load_Procedure (Procedure_Name);
   begin
      if Reference = null then
         raise Feature_Not_Supported_Exception with Procedure_Name;
      else
         Reference (Param1, Value);
      end if;
   end Getter_With_2_Params;

   procedure Getter_With_3_Params (Param1 : Param1_Type;
                                   Param2 : Param2_Type;
                                   Value  : in out Value_Type) is
      type Procedure_Reference is
        access procedure (Param1 : Param1_Type; Param2 : Param2_Type;
                          Param3 : in out Value_Type);
      pragma Convention (StdCall, Procedure_Reference);

      function Load_Procedure is new Load (Procedure_Reference);
      Reference : constant Procedure_Reference
        := Load_Procedure (Procedure_Name);
   begin
      if Reference = null then
        raise Feature_Not_Supported_Exception with Procedure_Name;
      else
        Reference (Param1, Param2, Value);
      end if;
   end Getter_With_3_Params;
   
   procedure Getter_With_4_Params (Param1 : Param1_Type;
                                   Param2 : Param2_Type;
                                   Param3 : Param3_Type;
                                   Value  : in out Value_Type) is
      type Procedure_Reference is
        access procedure (Param1 : Param1_Type; Param2 : Param2_Type;
                          Param3 : Param3_Type; Param4 : in out Value_Type);
      pragma Convention (StdCall, Procedure_Reference);
      
      function Load_Procedure is new Load (Procedure_Reference);
      Reference : constant Procedure_Reference
        := Load_Procedure (Procedure_Name);
   begin
      if Reference = null then
         raise Feature_Not_Supported_Exception with Procedure_Name;
      else
         Reference.all (Param1, Param2, Param3, Value);
      end if;
   end Getter_With_4_Params;

   procedure String_Getter_With_4_Params (Param1      : Param1_Type;
                                          Buffer_Size : Size_Type;
                                          Length      : out Size_Type;
                                          Value       : Interfaces.C.Strings.chars_ptr) is
      type Procedure_Reference is
        access procedure (Param1 : Param1_Type; Buffer_Size : Size_Type;
                          Length : out Size_Type;
						  Value : Interfaces.C.Strings.chars_ptr);
      pragma Convention (StdCall, Procedure_Reference);
      
      function Load_Procedure is new Load (Procedure_Reference);
      Reference : constant Procedure_Reference
        := Load_Procedure (Procedure_Name);
   begin
      if Reference = null then
         raise Feature_Not_Supported_Exception with Procedure_Name;
      else
         Reference (Param1, Buffer_Size, Length, Value);
      end if;
   end String_Getter_With_4_Params;

end GL.Runtime_Loading;
