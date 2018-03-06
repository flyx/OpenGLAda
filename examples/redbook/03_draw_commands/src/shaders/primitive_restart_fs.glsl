#version 410 core

in vec4 vs_fs_colour;

layout (location = 0) out vec4 colour;

void main(void)
{
    colour = vs_fs_colour;
}
