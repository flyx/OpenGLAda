#version 430
layout(local_size_x = 32) in;
layout(std430, binding = 0) buffer data {
   float values[];
};

void main() {
   int x = int(gl_GlobalInvocationID.x);
   values[x] = 2 * values[x];
}
