--  part of OpenGLAda, (c) 2021 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with Interfaces.C.Strings;
with System.Address_To_Access_Conversions;

with GL.API;
with GL.Enums.Getter;

package body GL.Debug is
   procedure Message_Insert (Source : Message_Source;
                             M_Type : Message_Type;
                             ID     : UInt;
                             Severity : Message_Severity;
                             Message : String) is
      Var_Msg : String := Message;
   begin
      GL.API.Debug_Message_Insert
        (Source, M_Type, ID, Severity, Message'Length, Var_Msg);
      Raise_Exception_On_OpenGL_Error;
   end Message_Insert;

   procedure Push_Group (Source  : Message_Source;
                         ID      : UInt;
                         Message : String) is
      Var_Msg : String := Message;
   begin
      GL.API.Push_Debug_Group (Source, ID, Message'Length, Var_Msg);
   end Push_Group;

   procedure Pop_Group is
   begin
      GL.API.Pop_Debug_Group.all;
   end Pop_Group;

   procedure Message_Control (Source   : Message_Source_Restriction;
                              M_Type   : Message_Type_Restriction;
                              Severity : Message_Severity_Restriction;
                              IDs      : UInt_Array;
                              Enabled  : Boolean) is
   begin
      GL.API.Debug_Message_Control
        (Source, M_Type, Severity, IDs'Length, IDs, Low_Level.Bool (Enabled));
      Raise_Exception_On_OpenGL_Error;
   end Message_Control;

   function Next_Message
     (Source  : out Message_Source; M_Type   : out Message_Type;
      ID      : out UInt;           Severity : out Message_Severity)
      return String is
      Length : aliased Int;
      Lengths : aliased Size;
   begin
      GL.API.Get_Integer
        (Enums.Getter.Debug_Next_Logged_Message_Length, Length'Access);
      return Ret : String (1 .. Integer (Length)) do
         declare
            Ignored : constant UInt :=
              GL.API.Get_Debug_Message_Log
                (1, Ret'Length, Source, M_Type, ID, Severity, Lengths, Ret);
         begin
            Raise_Exception_On_OpenGL_Error;
         end;
      end return;
   end Next_Message;

   function Logged_Messages return Int is
      Value : aliased Int;
   begin
      GL.API.Get_Integer (Enums.Getter.Debug_Logged_Messages, Value'Access);
      return Value;
   end Logged_Messages;

   package Conv is new System.Address_To_Access_Conversions
     (Message_Receiver'Class);

   procedure Call_Message_Receiver
     (Source     : Message_Source; M_Type   : Message_Type;
      ID         : UInt;           Severity : Message_Severity;
      Length     : Size;           Message  : Interfaces.C.Strings.chars_ptr;
      User_Param : System.Address);
   pragma Convention (C, Call_Message_Receiver);

   procedure Call_Message_Receiver
     (Source     : Message_Source; M_Type   : Message_Type;
      ID         : UInt;           Severity : Message_Severity;
      Length     : Size;           Message  : Interfaces.C.Strings.chars_ptr;
      User_Param : System.Address) is

   begin
      Conv.To_Pointer (User_Param).Receive
        (Source, M_Type, ID, Severity,
         Interfaces.C.Strings.Value (Message, Interfaces.C.size_t (Length)));
   end Call_Message_Receiver;

   procedure Message_Callback (Receiver : access Message_Receiver'Class) is
   begin
      if Receiver = null then
         GL.API.Debug_Message_Callback (null, System.Null_Address);
      else
         GL.API.Debug_Message_Callback
           (Call_Message_Receiver'Access, Conv.To_Address (Receiver));
      end if;
   end Message_Callback;

   procedure Set_Stream
     (Object : in out Stream_Logger;
      Stream : access Ada.Streams.Root_Stream_Type'Class) is
   begin
      Object.Stream := Stream;
   end Set_Stream;

   function Message_Source_Str
     (Value : Message_Source) return String is
     (case Value is
      when Api             => "     Api       ",
      when Window_System   => " Window System ",
      when Shader_Compiler => "Shader Compiler",
      when Third_Party     => "  Third Party  ",
      when Application     => "  Application  ",
      when Other           => "     Other     ");

   function Message_Type_Str
     (Value : Message_Type) return String is
     (case Value is
      when Error               => "       Error       ",
      when Deprecated_Behavior => "Deprecated Behavior",
      when Undefined_Behavior  => "Undefined Behavior ",
      when Portability         => "    Portability    ",
      when Performance         => "    Performance    ",
      when Other               => "       Other       ",
      when Marker              => "       Marker      ",
      when Push_Group          => "    Push Group     ",
      when Pop_Group           => "     Pop Group     ");

   function Message_Severity_Str
     (Value : Message_Severity) return String is
     (case Value is
      when Notification => "Notification",
      when High         => "    High    ",
      when Medium       => "   Medium   ",
      when Low          => "    Low     ");

   overriding procedure Receive
     (Object   : Stream_Logger;
      Source   : Message_Source;
      M_Type   : Message_Type;
      ID       : UInt;
      Severity : Message_Severity;
      Message  : String) is
   begin
      if Object.Stream /= null then
         String'Write (Object.Stream, "[");
         String'Write (Object.Stream, Message_Source_Str (Source));
         String'Write (Object.Stream, "/");
         String'Write (Object.Stream, Message_Type_Str (M_Type));
         String'Write (Object.Stream, "/");
         String'Write (Object.Stream, Message_Severity_Str (Severity));
         String'Write (Object.Stream, "] (#");
         String'Write (Object.Stream, ID'Img);
         String'Write (Object.Stream, ") ");
         String'Write (Object.Stream, Message);
         Character'Write (Object.Stream, Character'Val (10));
      end if;
   end Receive;
end GL.Debug;
