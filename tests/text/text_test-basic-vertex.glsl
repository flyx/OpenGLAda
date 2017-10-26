#version 410 core
layout(location = 0) in vec2 vertex;
uniform mat4 transformation;
uniform vec2 dimensions;
out vec2 uv;
void main() {
  gl_Position = transformation * vec4(vertex.x * dimensions.x,
      vertex.y * dimensions.y, 0.0, 1.0);
  uv = vertex;
}
