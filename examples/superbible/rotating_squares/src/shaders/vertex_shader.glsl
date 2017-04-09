#version 410

 in vec4 position;

 out VS_OUT
{
    vec4 colour;
} vs_out;

uniform mat4 mv_matrix;
uniform mat4 projection_matrix;

void main()
{
    gl_Position = projection_matrix * mv_matrix * position;
    vs_out.colour = 2.0 * position + vec4(0.5, 0.5, 0.5, 0.0);
}
