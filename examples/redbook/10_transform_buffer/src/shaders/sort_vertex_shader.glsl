#version 410 core

layout (location = 0) in vec4 position;
layout (location = 1) in vec3 normal;

out vec3 vs_normal;

uniform mat4 model_matrix;

uniform samplerBuffer geometry_tbo;
uniform float time_step = 0.02;

void main(void)
    {
    vs_normal = (model_matrix * vec4(normal, 0.0)).xyz;
    gl_Position = model_matrix * position;
    }
