#version 120

varying vec4 checkerboard_pos;

void main(void)
{   
   if (mod(floor(checkerboard_pos.x) + floor(checkerboard_pos.y), 2) == 0) {
      gl_FragColor = gl_Color;
   } else {
      gl_FragColor = vec4(1.0);
   }
}