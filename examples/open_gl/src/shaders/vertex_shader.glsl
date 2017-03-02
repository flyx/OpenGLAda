#version 410

in vec2 position;
in vec3 colour;
in vec2 image_coords;

out vec3 triangle_colour;
out vec2 image_coords_vs;

void main()
{
    triangle_colour = colour;
    image_coords_vs = image_coords;
    gl_Position = vec4(position, 0.0, 1.0);
}
