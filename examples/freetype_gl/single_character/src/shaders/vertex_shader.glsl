#version 410

//  in vec2 position;
in vec4 vertex;  // <vec2 position, vec2 texture>

out vec2 texture_coords;
uniform mat4 projection_matrix;

void main()
{
    // gl_Position = vec4(position, 0.0, 1.0);
    // texcoord = position * vec2(0.5) + vec2(0.5);
    gl_Position = projection_matrix * vec4(vertex.xy, 0.0, 1.0);
    texture_coords = vertex.zw;
}
