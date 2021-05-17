--  part of OpenGLAda, (c) 2021 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with Ada.Streams;

with GL.Types;
with GL.Low_Level;

package GL.Debug is
   pragma Preelaborate;
   
   use GL.Types;
   
   type Message_Source_Restriction is
     (Dont_Care, Api, Window_System, Shader_Compiler, Third_Party, Application,
      Other);
   for Message_Source_Restriction use (Dont_Care        => 16#1100#,
                                       Api              => 16#8246#,
                                       Window_System    => 16#8247#,
                                       Shader_Compiler  => 16#8248#,
                                       Third_Party      => 16#8249#,
                                       Application      => 16#824A#,
                                       Other            => 16#824B#);
   for Message_Source_Restriction'Size use Low_Level.Enum'Size;
   
   subtype Message_Source is Message_Source_Restriction range Api .. Other;
   
   type Message_Type_Restriction is
     (Dont_Care, Error, Deprecated_Behavior, Undefined_Behavior, Portability,
      Performance, Other, Marker, Push_Group, Pop_Group);
   for Message_Type_Restriction use (Dont_Care             => 16#1100#,
                                     Error                 => 16#824C#,
                                     Deprecated_Behavior   => 16#824D#,
                                     Undefined_Behavior    => 16#824E#,
                                     Portability           => 16#824F#,
                                     Performance           => 16#8250#,
                                     Other                 => 16#8251#,
                                     Marker                => 16#8268#,
                                     Push_Group            => 16#8269#,
                                     Pop_Group             => 16#826A#);
   for Message_Type_Restriction'Size use Low_Level.Enum'Size;
   
   subtype Message_Type is Message_Type_Restriction range Error .. Pop_Group;
   
   type Message_Severity_Restriction is
     (Dont_Care, Notification, High, Medium, Low);
   for Message_Severity_Restriction use (Dont_Care    => 16#1100#,
                                         Notification => 16#826B#,
                                         High         => 16#9146#,
                                         Medium       => 16#9147#,
                                         Low          => 16#9148#);
   for Message_Severity_Restriction'Size use Low_Level.Enum'Size;
   
   subtype Message_Severity is Message_Severity_Restriction
     range Notification .. Low;
 
   type Message_Receiver is interface;
   
   procedure Receive (Object   : Message_Receiver;
                      Source   : Message_Source;
                      M_Type   : Message_Type;
                      ID       : UInt;
                      Severity : Message_Severity;
                      Message  : String) is abstract;
   
   procedure Message_Callback (Receiver : access Message_Receiver'Class);
   
   procedure Message_Insert (Source : Message_Source;
                             M_Type : Message_Type;
                             ID     : UInt;
                             Severity : Message_Severity;
                             Message : String);
   
   procedure Push_Group (Source  : Message_Source;
                         ID      : UInt;
                         Message : String);
   procedure Pop_Group;
   
   procedure Message_Control (Source   : Message_Source_Restriction;
                              M_Type   : Message_Type_Restriction;
                              Severity : Message_Severity_Restriction;
                              IDs      : UInt_Array;
                              Enabled  : Boolean);
  
   function Next_Message
     (Source  : out Message_Source; M_Type   : out Message_Type;
      ID      : out UInt;           Severity : out Message_Severity)
      return String;
   
   function Logged_Messages return Int;
   
   
   type Stream_Logger is new Message_Receiver with private;
   
   procedure Set_Stream
     (Object : in out Stream_Logger;
      Stream : access Ada.Streams.Root_Stream_Type'Class);
   
   overriding procedure Receive
     (Object   : Stream_Logger;
      Source   : Message_Source;
      M_Type   : Message_Type;
      ID       : UInt;
      Severity : Message_Severity;
      Message  : String);
private
   type Stream_Logger is new Message_Receiver with record
      Stream : access Ada.Streams.Root_Stream_Type'Class := null;
      Depth : Natural := 0;
   end record;
end GL.Debug;
