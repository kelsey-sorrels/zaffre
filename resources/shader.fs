#version 330 core

uniform sampler2D uFont, uFg, uBg;
uniform usampler2D uGlyphs;
uniform vec2 fontSize, termDimensions, fontTextureDimensions, glyphTextureDimensions;

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
  uvec2 fontIndex = texelFetch(uGlyphs, termXY, 0).xy;
  ivec2 fontXY = ivec2(int(fontIndex.x) * charSize.x, int(fontIndex.y) * charSize.y);
  // calc the position of the fragment relative to the terminal cell
  ivec2 charXY = ivec2(fract(vTextureCoord.x * termDimensions.x) * charSize.x,
                       (-fract(vTextureCoord.y * termDimensions.y) + 1) * charSize.y);
  vec4 fnt = texelFetch(uFont, fontXY + charXY, 0);

  vec4 fg  = texelFetch(uFg, termXY, 0);
  vec4 bg  = texelFetch(uBg, termXY, 0);

  outcolor = mix(bg, fg, fnt.r);
}
