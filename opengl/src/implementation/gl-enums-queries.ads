--  part of OpenGLAda, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "COPYING"

private with GL.Low_Level;

package GL.Enums.Queries is
   pragma Preelaborate;

   -- Texture_Kind is declared in GL.Low_Level.Enums to be accessible for
   -- OpenCLAda

   type Parameter is (Time_Elapsed, Samples_Passed, Any_Samples_Passed,
                         Transform_Feedback_Primitives_Written);

   -- needs to be declared here because of subtypes
   for Parameter use (Time_Elapsed                          => 16#88BF#,
                      Samples_Passed                        => 16#8914#,
                      Any_Samples_Passed                    => 16#8C2F#,
                      Transform_Feedback_Primitives_Written => 16#8C88#);
   for Parameter'Size use Low_Level.Enum'Size;

private

end GL.Enums.Queries;
