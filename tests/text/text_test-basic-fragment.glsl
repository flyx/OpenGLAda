#version 410 core
in vec2 uv;
out vec4 color;
uniform sampler2D texSampler;
void main() {
  color = vec4(1.0, 0.0, 0.0, texture(texSampler, uv).r);
}