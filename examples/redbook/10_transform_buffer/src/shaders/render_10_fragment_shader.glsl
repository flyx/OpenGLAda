#version 410 core

layout (location = 0) out vec4 colour;

in vec3 vs_fs_normal;

uniform vec4 pass_colour;

void main(void)
    {
    colour = pass_colour * (0.2 + pow(abs(vs_fs_normal.z), 4.0)) + vec4(1.0, 1.0, 1.0, 0.0) * pow(abs(vs_fs_normal.z), 37.0);
    }
