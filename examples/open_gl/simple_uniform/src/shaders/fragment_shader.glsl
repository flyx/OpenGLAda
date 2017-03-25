#version 410

uniform vec3 triangle_colour;

out vec4 Frag_Colour;

void main()
{
    Frag_Colour = vec4(triangle_colour, 1.0);
}
