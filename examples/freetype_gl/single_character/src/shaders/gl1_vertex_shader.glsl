#version 410 core

layout(location = 0) in vec4 vertices;  // <vec2 position, vec2 texture>

out vec2 texture_coords;
uniform mat4 projection_matrix;

void main()
{
    // gl_Position = projection_matrix * vec4(vertices.x, vertices.y, 0.0, 1.0);
    gl_Position = vec4(vertices.xy, 0.0, 1.0);
    texture_coords = vec2(vertices.zw);
}
