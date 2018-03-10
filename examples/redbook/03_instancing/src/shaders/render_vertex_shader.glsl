#version 410 core

layout (location = 0) in vec4 position;
layout (location = 1) in vec3 normal;
layout (location = 2) in vec4 colour;

out vec4 world_space_position;
out vec3 vs_fs_normal;

uniform mat4 view_matrix;
uniform mat4 projection_matrix;

// These are the texture buffer objects that hold per-instance colors and per-instance model matrices
uniform samplerBuffer colour_tbo;
uniform samplerBuffer model_matrix_tbo;

// The output of the vertex shader (matched to the fragment shader)
out VERTEX
{
    vec3    normal;
    vec4    colour;
} vertex;

void main(void)
    {
     // Use gl_InstanceID to obtain the instance color from the color TBO\n"
     vec4 colour = texelFetch(colour_tbo, gl_InstanceID);
        // Generating the model matrix is more complex because you can't
        // store mat4 data in a TBO. Instead, we need to store each matrix
        // as four vec4 variables and assemble the matrix in the shader.
        // First, fetch the four columns of the matrix (remember, matrices are
        // stored in memory in column-primary order).
        vec4 col1 = texelFetch(model_matrix_tbo, gl_InstanceID * 4);
        vec4 col2 = texelFetch(model_matrix_tbo, gl_InstanceID * 4 + 1);
        vec4 col3 = texelFetch(model_matrix_tbo, gl_InstanceID * 4 + 2);
        vec4 col4 = texelFetch(model_matrix_tbo, gl_InstanceID * 4 + 3);
        // Now assemble the four columns into a matrix.
        mat4 model_matrix = mat4(col1, col2, col3, col4);
        // Now construct a model-view matrix from the uniform view matrix
        // and the per-instance model matrix.
        mat4 model_view_matrix = view_matrix * model_matrix;

        // Transform position by the model-view matrix then by the projection matrix.
        gl_Position = projection_matrix * (model_view_matrix * position);
        // Transform the normal by the upper-left-3x3-submatrix of the
        // model-view matrix
        vertex.normal = mat3(model_view_matrix) * normal;
        // Pass the per-instance colour through to the fragment shader.
        vertex.colour = colour;
    }
