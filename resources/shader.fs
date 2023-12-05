#version 330 core

uniform sampler2D uFont;
uniform sampler2DArray uFg, uBg;
uniform usampler2DArray uGlyphs;
uniform vec2 fontSize, termDimensions, fontTextureDimensions, glyphTextureDimensions;
uniform int numLayers;

in vec2 vTextureCoord;
out vec4 outcolor;

void main(void) {
  // width and height of the screen in pixels
  float screenWidth = fontSize.x * termDimensions.x;
  float screenHeight = fontSize.y * termDimensions.y;
  ivec2 termXY = ivec2(floor(vTextureCoord.x * termDimensions.x),
                       floor(vTextureCoord.y * termDimensions.y));
  ivec2 charSize = ivec2(fontSize.x, fontSize.y);
  // row,column -> glyph texture uvs
  // look up in uGlphs the index in the font texture to use for the fragment

  vec4 result = vec4(0);
  int numLayers = 1;
  for (int i = 0; i < numLayers; i++) {
      ivec3 termXYZ = ivec3(termXY.x, termXY.y, i);
      uvec3 glyphXYT = texelFetch(uGlyphs, termXYZ, 0).xyz;
      int glyphType = int(glyphXYT.t);
      uvec2 fontIndex = glyphXYT.xy;
      ivec2 fontXY = ivec2(int(fontIndex.x) * charSize.x, int(fontIndex.y) * charSize.y);
      // calc the position of the fragment relative to the terminal cell
      ivec2 charXY = ivec2(fract(vTextureCoord.x * termDimensions.x) * charSize.x,
                           (-fract(vTextureCoord.y * termDimensions.y) + 1) * charSize.y);
      vec4 fnt = texelFetch(uFont, fontXY + charXY, 0);

      vec4 fg  = texelFetch(uFg, termXYZ, 0);
      vec4 bg  = texelFetch(uBg, termXYZ, 0);

    if (glyphType == 0) {
        result = mix(bg, fg, fnt.r);
    } else {
        result += fnt * fnt.a + (result * (1.0 - fnt.a));
    }
  }
  outcolor = result;
}
