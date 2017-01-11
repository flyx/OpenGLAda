#version 410

 in vec4 vs_colour;
 out vec4 fragment_colour;

void main()
{
   fragment_colour = vs_colour;
   //  fragment_colour = vec4(0.5, 0.0, 0.5, 1.0);
}
