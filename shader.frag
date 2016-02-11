      const int TEXTURE_NIL = 0;
      const int TEXTURE_RAW = 1;
      const int TEXTURE_PANO = 2;
      const int TEXTURE_PANO_SPLIT = 3;

      varying vec3 world;
      varying vec2 texUV;

      uniform int prevTextureType;
      uniform sampler2D prevTextureOne;
      uniform vec2 prevTextureScaleOne;
      uniform vec2 prevTextureOffsetOne;
      uniform sampler2D prevTextureTwo;
      uniform vec2 prevTextureScaleTwo;
      uniform vec2 prevTextureOffsetTwo;
      uniform mat4 prevTextureInvTransform;

      uniform int textureType;
      uniform sampler2D textureOne;
      uniform vec2 textureScaleOne;
      uniform vec2 textureOffsetOne;
      uniform sampler2D textureTwo;
      uniform vec2 textureScaleTwo;
      uniform vec2 textureOffsetTwo;
      uniform mat4 textureInvTransform;

      uniform float textureInterpolation;

      const float pi = 3.141592653589793;

      vec2 worldUV(mat4 invTransform) {
        vec3 rayDir = normalize((invTransform * vec4(world, 1.0)).xyz);

        float theta = acos(rayDir.y);
        float phi = atan(-rayDir.z, rayDir.x);

        float panoX = mod(3.0 * pi / 2.0 - phi, 2.0 * pi) / pi; // [0..2[
        float panoY = theta / pi; // [0..1[

        return vec2(panoX, panoY);
      }

      vec4 samplePano(sampler2D texture, vec2 scale, vec2 offset, mat4 invTransform) {
        vec2 uv = worldUV(invTransform);
        uv.x /= 2.0;

        return texture2D(texture, uv * scale + offset);
      }

      vec4 sampleSplitPano(sampler2D textureLeft, vec2 scaleLeft, vec2 offsetLeft, sampler2D textureRight, vec2 scaleRight, vec2 offsetRight, mat4 invTransform) {
        vec2 baseUV = worldUV(invTransform);
        bool isLeft = baseUV.x < 1.0;
        vec2 uv = vec2(fract(baseUV.x), baseUV.y) * (isLeft ? scaleLeft : scaleRight) + (isLeft ? offsetLeft : offsetRight);

        if (isLeft) {
          return texture2D(textureLeft, uv);
        } else {
          return texture2D(textureRight, uv);
        }
      }

      vec4 sample(
        int textureType,
        sampler2D textureOne,
        vec2 scaleOne,
        vec2 offsetOne,
        sampler2D textureTwo,
        vec2 scaleTwo,
        vec2 offsetTwo,
        mat4 invTransform) {

        return
               textureType == TEXTURE_RAW ? texture2D(textureOne, texUV)
          :    textureType == TEXTURE_PANO ? samplePano(textureOne, scaleOne, offsetOne, invTransform)
          :    textureType == TEXTURE_PANO_SPLIT ? sampleSplitPano(textureOne, scaleOne, offsetOne, textureTwo, scaleTwo, offsetTwo, invTransform)
          : /* textureType == TEXTURE_NIL */ vec4(float(0x1a) / 255.0, float(0x21) / 255.0, float(0x22) / 255.0, 1.0);
      }

      void main(void)
      {
        vec4 prevColor = sample(
          prevTextureType,
          prevTextureOne,
          prevTextureScaleOne,
          prevTextureOffsetOne,
          prevTextureTwo,
          prevTextureScaleTwo,
          prevTextureOffsetTwo,
          prevTextureInvTransform);

        vec4 currColor = sample(
          textureType,
          textureOne,
          textureScaleOne,
          textureOffsetOne,
          textureTwo,
          textureScaleTwo,
          textureOffsetTwo,
          textureInvTransform);

        gl_FragColor = mix(prevColor, currColor, textureInterpolation);
}
