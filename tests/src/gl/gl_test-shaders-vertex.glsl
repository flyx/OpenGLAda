#version 120

varying vec4 checkerboard_pos;

void main(void)
{
   vec4 v = ftransform();
   
   gl_Position = v;
   checkerboard_pos = (gl_Vertex + 1) * 10;
   gl_FrontColor = gl_Color;
}