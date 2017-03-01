with Ada.Text_IO;

private with Ada.Containers.Vectors;
private with Ada.Containers.Indefinite_Vectors;
private with Ada.Strings.Unbounded;

package Specs is
   -- This package implements parsing and applying .spec files.
   -- These are used to easily generate parts of the low-level OpenGL
   -- binding code. This package is not part of the OpenGLAda API nor
   -- implementation - it is only used for generating part of its
   -- source code.

   type Processor is limited private;
   type Spec is private;

   No_Spec : constant Spec;
   Parsing_Error : exception;

   procedure Parse_File (Proc : in out Processor; Path : String);

   function First (Proc : Processor) return Spec;

   function Next (Proc : Processor; Cur : Spec) return Spec;

   procedure Write_API (Proc : Processor; Cur : Spec;
                        Dir_Path : String);

   procedure Write_Init (Proc : Processor; Dir_Path : String);

   procedure Write_Wrapper_Table (Proc : Processor;
                                  Dir_Path, Interface_Folder : String);
private
   use Ada.Strings.Unbounded;

   type Param_Mode is
     (Mode_In, Mode_Out, Mode_In_Out, Mode_Access, Mode_Access_Constant);

   package String_Lists is new Ada.Containers.Indefinite_Vectors
     (Positive, String);

   type Parameter is record
      Mode : Param_Mode;
      Names : String_Lists.Vector;
      Type_Name : Unbounded_String;
   end record;

   package Param_Lists is new Ada.Containers.Vectors
      (Positive, Parameter);

   type Signature is record
      Params : Param_Lists.Vector;
      Return_Type : Unbounded_String;
   end record;

   package Sig_Lists is new Ada.Containers.Vectors (Positive, Signature);

   type Body_Item_Kind is (Copy, Static, Dynamic);

   type Body_Item (Kind : Body_Item_Kind) is record
      case Kind is
      when Copy => To_Copy : Unbounded_String;
      when Static =>
         S_Name, S_GL_Name : Unbounded_String;
         Sigs : Sig_Lists.Vector;
      when Dynamic =>
         D_Name, D_GL_Name : Unbounded_String;
         Sig_Id : Positive;
      end case;
   end record;

   package Item_Lists is
     new Ada.Containers.Indefinite_Vectors (Positive, Body_Item);

   package Wrapper_Lists is new Ada.Containers.Indefinite_Vectors
     (Positive, String_Lists.Vector, String_Lists."=");

   type Spec_Data is record
      Name, File_Base_Name : Unbounded_String;
      Withs : String_Lists.Vector;
      Uses  : String_Lists.Vector;
      Items : Item_Lists.Vector;
      Wrappers : Wrapper_Lists.Vector;
   end record;

   type Spec is new Natural;
   subtype Valid_Spec is Spec range 1 .. Spec'Last;

   No_Spec : constant Spec := 0;

   package Spec_Lists is new Ada.Containers.Vectors (Valid_Spec, Spec_Data);

   type Processor is record
      Dynamic_Subprogram_Types : Sig_Lists.Vector;
      List : Spec_Lists.Vector;
   end record;
end Specs;