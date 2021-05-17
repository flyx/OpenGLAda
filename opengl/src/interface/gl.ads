--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

with Interfaces.C;

package GL is
   pragma Preelaborate;

   package C renames Interfaces.C;

   -----------------------------------------------------------------------------
   --                                 Basics                                  --
   -----------------------------------------------------------------------------

   -- this is an OpenGLAda-specific procedure that must be called once at
   -- startup and loads all function pointers for post-1.1 OpenGL functionality.
   -- it is idempotent (i.e. can be called multiple times without further
   -- effect).
   procedure Init;

   procedure Flush;
   procedure Finish;

   -- index types for vectors and matrices
   type Index_Homogeneous is (X, Y, Z, W);
   subtype Index_3D is Index_Homogeneous range X .. Z;
   subtype Index_2D is Index_Homogeneous range X .. Y;

   -- raised when a function that is not available for the current context
   -- is called.
   Feature_Not_Supported_Exception : exception;

   -- raised when OpenGLAda does not support a certain OpenGL feature
   -- (either because it's too new and has not yet been wrapped, or because
   -- it's so deprecated that you shouldn't use it anyway)
   Not_Implemented_Exception : exception;
private

   -----------------------------------------------------------------------------
   --                           Internal functions                            --
   -----------------------------------------------------------------------------

   procedure Raise_Exception_On_OpenGL_Error;
   pragma Inline (Raise_Exception_On_OpenGL_Error);

end GL;
