#version 410 core

layout (location = 0) in vec4 position;
layout (location = 1) in vec3 normal;

out vec3 vs_fs_normal;

uniform mat4 model_matrix;
uniform mat4 projection_matrix;

void main(void)
    {
    vs_fs_normal = normal;
    gl_Position = position;
    }
