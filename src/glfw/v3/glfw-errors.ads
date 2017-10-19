--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

package Glfw.Errors is
   type Kind is (Not_Initialized,
                 No_Current_Context,
                 Invalid_Enum,
                 Invalid_Value,
                 Out_Of_Memory,
                 API_Unavailable,
                 Version_Unavailable,
                 Platform_Error,
                 Format_Unavailable);
   for Kind use (Not_Initialized => 16#00010001#,
                 No_Current_Context  => 16#00010002#,
                 Invalid_Enum        => 16#00010003#,
                 Invalid_Value       => 16#00010004#,
                 Out_Of_Memory       => 16#00010005#,
                 API_Unavailable     => 16#00010006#,
                 Version_Unavailable => 16#00010007#,
                 Platform_Error      => 16#00010008#,
                 Format_Unavailable  => 16#00010009#);
   for Kind'Size use Interfaces.C.int'Size;

   type Callback is access procedure (Error : Kind; Description : String);

   procedure Set_Callback (Handler : Callback);
end Glfw.Errors;
