#version 410 core

layout (location = 0) in vec4 position;
layout (location = 1) in vec3 normal;

out vec4 world_space_position1;
out vec3 vs_fs_normal;

uniform mat4 model_matrix;
uniform mat4 projection_matrix;

void main(void)
    {
    // vec4 pos = (model_matrix * (position * vec4(1.0, 1.0, 1.0, 1.0)));
    vec4 pos = model_matrix * position;
    world_space_position1 = pos;
    vs_fs_normal = normalize((model_matrix * vec4(normal, 0.0)).xyz);
    gl_Position = projection_matrix * pos;
    }
