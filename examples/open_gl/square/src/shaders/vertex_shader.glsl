#version 410

in vec2 position;
in vec3 colour;

out vec3 triangle_colour;

void main()
{
    triangle_colour = colour;
    gl_Position = vec4(position, 0.0, 1.0);
}
