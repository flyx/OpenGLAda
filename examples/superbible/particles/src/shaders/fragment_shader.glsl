#version 410

layout (location = 0) out vec4 o_color;

in vec4 particle_colour;

void main(void)
{
    o_color = particle_colour;
}
