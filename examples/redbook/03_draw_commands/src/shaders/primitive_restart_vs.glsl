#version 410 core

uniform mat4 model_matrix;
uniform mat4 projection_matrix;

layout (location = 0) in vec4 position;
layout (location = 1) in vec4 colour;

out vec4 vs_fs_colour;

void main(void)
{
    vs_fs_colour = colour;
    gl_Position = projection_matrix * (model_matrix * position);
}
