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

with Ada.Unchecked_Conversion;

package body GL.Low_Level.Loader is
   use type Function_Maps.Cursor;
   use type System.Address;

   -- Implementation is platform-dependent
   procedure Load_Function_To_Map (GL_Function_Name : String;
                                   Position : out Function_Maps.Cursor)
                                  is separate;

   function Available (GL_Function_Name : String) return Boolean is
      Position : Function_Maps.Cursor := Loaded.Find (GL_Function_Name);
   begin
      if Position = Function_Maps.No_Element then
         Load_Function_To_Map (GL_Function_Name, Position);
      end if;
      return Function_Maps.Element (Position) /= System.Null_Address;
   end Available;

   function Load (GL_Function_Name : String) return Function_Reference is
      function As_Function_Reference is new Ada.Unchecked_Conversion (
        Source => System.Address, Target => Function_Reference
      );

      Position : Function_Maps.Cursor := Loaded.Find (GL_Function_Name);
   begin
      if Position = Function_Maps.No_Element then
         Load_Function_To_Map (GL_Function_Name, Position);
      end if;
      return As_Function_Reference (Function_Maps.Element (Position));
   end Load;

   function Function_With_1_Param (Param1 : Param1_Type) return Return_Type is
      type Function_Reference is
        access function (Param1 : Param1_Type) return Return_Type;
      pragma Convention (StdCall, Function_Reference);

      function Load_Function is new Load (Function_Reference);

      Reference : Function_Reference := Load_Function (GL_Function_Name);
   begin
      if Reference = null then
         raise Feature_Not_Supported_Exception;
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
     Reference : Function_Reference := Load_Function (GL_Function_Name);
   begin
     if Reference = null then
        raise Feature_Not_Supported_Exception;
     else
        return Reference (Param1, Param2);
     end if;
   end Function_With_2_Params;
   
   procedure Procedure_Without_Params is
      type Procedure_Reference is
        access procedure;
      pragma Convention (StdCall, Procedure_Reference);
      
      function Load_Procedure is new Load (Procedure_Reference);
      Reference : Procedure_Reference := Load_Procedure (GL_Procedure_Name);
   begin
      if Reference = null then
         raise Feature_Not_Supported_Exception;
      else
         Reference.all;
      end if;
   end Procedure_Without_Params;

   procedure Procedure_With_1_Param (Param1 : Param1_Type) is
      type Procedure_Reference is
        access procedure (Param1 : Param1_Type);
      pragma Convention (StdCall, Procedure_Reference);

      function Load_Procedure is new Load (Procedure_Reference);
      Reference : Procedure_Reference := Load_Procedure (GL_Procedure_Name);
   begin
      if Reference = null then
         raise Feature_Not_Supported_Exception;
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
      Reference : Procedure_Reference := Load_Procedure (GL_Procedure_Name);
   begin
      if Reference = null then
        raise Feature_Not_Supported_Exception;
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
      Reference : Procedure_Reference := Load_Procedure (GL_Procedure_Name);
   begin
      if Reference = null then
        raise Feature_Not_Supported_Exception;
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
      Reference : Procedure_Reference := Load_Procedure (GL_Procedure_Name);
   begin
      if Reference = null then
        raise Feature_Not_Supported_Exception;
      else
        Reference (Param1, Param2, Param3, Param4);
      end if;
   end Procedure_With_4_Params;
      
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
      Reference : Procedure_Reference := Load_Procedure (GL_Procedure_Name);
   begin
      if Reference = null then
         raise Feature_Not_Supported_Exception;
      else
         Reference (Param1, Param2, Param3, Param4, Param5, Param6);
      end if;
   end Procedure_With_6_Params;
   
   procedure Getter_With_3_Params (Param1 : Param1_Type;
                                   Param2 : Param2_Type;
                                   Value  : in out Value_Type) is
      type Procedure_Reference is
        access procedure (Param1 : Param1_Type; Param2 : Param2_Type;
                          Param3 : in out Value_Type);
      pragma Convention (StdCall, Procedure_Reference);
   
      function Load_Procedure is new Load (Procedure_Reference);
      Reference : Procedure_Reference := Load_Procedure (GL_Procedure_Name);
   begin
      if Reference = null then
        raise Feature_Not_Supported_Exception;
      else
        Reference (Param1, Param2, Value);
      end if;
   end Getter_With_3_Params;
   

end GL.Low_Level.Loader;
