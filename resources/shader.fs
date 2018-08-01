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
  ivec2 termXY = ivec2(floor(vTextureCoord.x * termDimensions.x),
                       floor(vTextureCoord.y * termDimensions.y));
  ivec2 charSize = ivec2(fontSize.x, fontSize.y);
  // row,column -> glyph texture uvs
  // look up in uGlphs the index in the font texture to use for the fragment

  vec4 result = vec4(0);
  //int numLayers = 1;
  for (uint i = 0u; i < uint(numLayers); i++) {
    ivec3 termXYZ = ivec3(termXY.x, termXY.y, (i + 0u));
    uvec3 glyphXYT = texelFetch(uGlyphs, termXYZ, 0).xyz;
    uint glyphType = glyphXYT.z;
    ivec2 fontIndex = ivec2(glyphXYT.xy);
    ivec2 fontXY = ivec2(int(fontIndex.x) * charSize.x, int(fontIndex.y) * charSize.y);
    // calc the position of the fragment relative to the terminal cell
    ivec2 charXY = ivec2(fract(vTextureCoord.x * termDimensions.x) * charSize.x,
                         (-fract(vTextureCoord.y * termDimensions.y) + 1) * charSize.y);
    vec4 fnt = texelFetch(uFont, fontXY + charXY, 0);

    vec4 fg  = texelFetch(uFg, termXYZ, 0);
    vec4 bg  = texelFetch(uBg, termXYZ, 0);
    uint r = uint(256u * result.r);
    switch (glyphType) {
      case 0u:
        //result.r = 0.2;
        break;
      case 1u:
        result = mix(bg, fg, fnt.r);
        //result.r = 0.5;
        break;
      case 2u:
        // from https://www.w3.org/TR/compositing-1/#csscompositingrules_CSS
        vec4 m = mix(bg, fg, fnt.a);
        result = m  +  result * (1.0 - m.a);
        result.a = 0.5;
        break;
    }
    //result.r = ((r | (glyphType * 10u)  << (2u * i))/ 256u);
    //result.r = 1.0;
    //result.a = 1.0;
  }
  outcolor = result;
}
