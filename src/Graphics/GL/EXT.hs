--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.EXT
-- Copyright   :  (c) Sven Panne 2016
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- A convenience module, combining all raw modules containing EXT extensions.
--
--------------------------------------------------------------------------------

module Graphics.GL.EXT (
  module Graphics.GL.EXT.ABGR,
  module Graphics.GL.EXT.BGRA,
  module Graphics.GL.EXT.BindableUniform,
  module Graphics.GL.EXT.BlendColor,
  module Graphics.GL.EXT.BlendEquationSeparate,
  module Graphics.GL.EXT.BlendFuncSeparate,
  module Graphics.GL.EXT.BlendMinmax,
  module Graphics.GL.EXT.BlendSubtract,
  module Graphics.GL.EXT.CMYKA,
  module Graphics.GL.EXT.ClipVolumeHint,
  module Graphics.GL.EXT.ColorSubtable,
  module Graphics.GL.EXT.CompiledVertexArray,
  module Graphics.GL.EXT.Convolution,
  module Graphics.GL.EXT.CoordinateFrame,
  module Graphics.GL.EXT.CopyTexture,
  module Graphics.GL.EXT.CullVertex,
  module Graphics.GL.EXT.DebugLabel,
  module Graphics.GL.EXT.DebugMarker,
  module Graphics.GL.EXT.DepthBoundsTest,
  module Graphics.GL.EXT.DirectStateAccess,
  module Graphics.GL.EXT.DrawBuffers2,
  module Graphics.GL.EXT.DrawInstanced,
  module Graphics.GL.EXT.DrawRangeElements,
  module Graphics.GL.EXT.FogCoord,
  module Graphics.GL.EXT.FourTwoTwoPixels,
  module Graphics.GL.EXT.FramebufferBlit,
  module Graphics.GL.EXT.FramebufferMultisample,
  module Graphics.GL.EXT.FramebufferMultisampleBlitScaled,
  module Graphics.GL.EXT.FramebufferObject,
  module Graphics.GL.EXT.FramebufferSRGB,
  module Graphics.GL.EXT.GPUProgramParameters,
  module Graphics.GL.EXT.GPUShader4,
  module Graphics.GL.EXT.GeometryShader4,
  module Graphics.GL.EXT.Histogram,
  module Graphics.GL.EXT.IndexArrayFormats,
  module Graphics.GL.EXT.IndexFunc,
  module Graphics.GL.EXT.IndexMaterial,
  module Graphics.GL.EXT.LightTexture,
  module Graphics.GL.EXT.MultiDrawArrays,
  module Graphics.GL.EXT.Multisample,
  module Graphics.GL.EXT.PackedDepthStencil,
  module Graphics.GL.EXT.PackedFloat,
  module Graphics.GL.EXT.PackedPixels,
  module Graphics.GL.EXT.PalettedTexture,
  module Graphics.GL.EXT.PixelBufferObject,
  module Graphics.GL.EXT.PixelTransform,
  module Graphics.GL.EXT.PointParameters,
  module Graphics.GL.EXT.PolygonOffset,
  module Graphics.GL.EXT.PolygonOffsetClamp,
  module Graphics.GL.EXT.ProvokingVertex,
  module Graphics.GL.EXT.RasterMultisample,
  module Graphics.GL.EXT.RescaleNormal,
  module Graphics.GL.EXT.SecondaryColor,
  module Graphics.GL.EXT.SeparateShaderObjects,
  module Graphics.GL.EXT.SeparateSpecularColor,
  module Graphics.GL.EXT.ShaderImageLoadStore,
  module Graphics.GL.EXT.SharedTexturePalette,
  module Graphics.GL.EXT.StencilClearTag,
  module Graphics.GL.EXT.StencilTwoSide,
  module Graphics.GL.EXT.StencilWrap,
  module Graphics.GL.EXT.Subtexture,
  module Graphics.GL.EXT.Texture,
  module Graphics.GL.EXT.Texture3D,
  module Graphics.GL.EXT.TextureArray,
  module Graphics.GL.EXT.TextureBufferObject,
  module Graphics.GL.EXT.TextureCompressionLATC,
  module Graphics.GL.EXT.TextureCompressionRGTC,
  module Graphics.GL.EXT.TextureCompressionS3TC,
  module Graphics.GL.EXT.TextureCubeMap,
  module Graphics.GL.EXT.TextureEnvCombine,
  module Graphics.GL.EXT.TextureEnvDot3,
  module Graphics.GL.EXT.TextureFilterAnisotropic,
  module Graphics.GL.EXT.TextureFilterMinmax,
  module Graphics.GL.EXT.TextureInteger,
  module Graphics.GL.EXT.TextureLODBias,
  module Graphics.GL.EXT.TextureMirrorClamp,
  module Graphics.GL.EXT.TextureObject,
  module Graphics.GL.EXT.TexturePerturbNormal,
  module Graphics.GL.EXT.TextureSNorm,
  module Graphics.GL.EXT.TextureSRGB,
  module Graphics.GL.EXT.TextureSRGBDecode,
  module Graphics.GL.EXT.TextureSharedExponent,
  module Graphics.GL.EXT.TextureSwizzle,
  module Graphics.GL.EXT.TimerQuery,
  module Graphics.GL.EXT.TransformFeedback,
  module Graphics.GL.EXT.VertexArray,
  module Graphics.GL.EXT.VertexArrayBGRA,
  module Graphics.GL.EXT.VertexAttrib64Bit,
  module Graphics.GL.EXT.VertexShader,
  module Graphics.GL.EXT.VertexWeighting,
  module Graphics.GL.EXT.X11SyncObject
) where

import Graphics.GL.EXT.ABGR
import Graphics.GL.EXT.BGRA
import Graphics.GL.EXT.BindableUniform
import Graphics.GL.EXT.BlendColor
import Graphics.GL.EXT.BlendEquationSeparate
import Graphics.GL.EXT.BlendFuncSeparate
import Graphics.GL.EXT.BlendMinmax
import Graphics.GL.EXT.BlendSubtract
import Graphics.GL.EXT.CMYKA
import Graphics.GL.EXT.ClipVolumeHint
import Graphics.GL.EXT.ColorSubtable
import Graphics.GL.EXT.CompiledVertexArray
import Graphics.GL.EXT.Convolution
import Graphics.GL.EXT.CoordinateFrame
import Graphics.GL.EXT.CopyTexture
import Graphics.GL.EXT.CullVertex
import Graphics.GL.EXT.DebugLabel
import Graphics.GL.EXT.DebugMarker
import Graphics.GL.EXT.DepthBoundsTest
import Graphics.GL.EXT.DirectStateAccess
import Graphics.GL.EXT.DrawBuffers2
import Graphics.GL.EXT.DrawInstanced
import Graphics.GL.EXT.DrawRangeElements
import Graphics.GL.EXT.FogCoord
import Graphics.GL.EXT.FourTwoTwoPixels
import Graphics.GL.EXT.FramebufferBlit
import Graphics.GL.EXT.FramebufferMultisample
import Graphics.GL.EXT.FramebufferMultisampleBlitScaled
import Graphics.GL.EXT.FramebufferObject
import Graphics.GL.EXT.FramebufferSRGB
import Graphics.GL.EXT.GPUProgramParameters
import Graphics.GL.EXT.GPUShader4
import Graphics.GL.EXT.GeometryShader4
import Graphics.GL.EXT.Histogram
import Graphics.GL.EXT.IndexArrayFormats
import Graphics.GL.EXT.IndexFunc
import Graphics.GL.EXT.IndexMaterial
import Graphics.GL.EXT.LightTexture
import Graphics.GL.EXT.MultiDrawArrays
import Graphics.GL.EXT.Multisample
import Graphics.GL.EXT.PackedDepthStencil
import Graphics.GL.EXT.PackedFloat
import Graphics.GL.EXT.PackedPixels
import Graphics.GL.EXT.PalettedTexture
import Graphics.GL.EXT.PixelBufferObject
import Graphics.GL.EXT.PixelTransform
import Graphics.GL.EXT.PointParameters
import Graphics.GL.EXT.PolygonOffset
import Graphics.GL.EXT.PolygonOffsetClamp
import Graphics.GL.EXT.ProvokingVertex
import Graphics.GL.EXT.RasterMultisample
import Graphics.GL.EXT.RescaleNormal
import Graphics.GL.EXT.SecondaryColor
import Graphics.GL.EXT.SeparateShaderObjects
import Graphics.GL.EXT.SeparateSpecularColor
import Graphics.GL.EXT.ShaderImageLoadStore
import Graphics.GL.EXT.SharedTexturePalette
import Graphics.GL.EXT.StencilClearTag
import Graphics.GL.EXT.StencilTwoSide
import Graphics.GL.EXT.StencilWrap
import Graphics.GL.EXT.Subtexture
import Graphics.GL.EXT.Texture
import Graphics.GL.EXT.Texture3D
import Graphics.GL.EXT.TextureArray
import Graphics.GL.EXT.TextureBufferObject
import Graphics.GL.EXT.TextureCompressionLATC
import Graphics.GL.EXT.TextureCompressionRGTC
import Graphics.GL.EXT.TextureCompressionS3TC
import Graphics.GL.EXT.TextureCubeMap
import Graphics.GL.EXT.TextureEnvCombine
import Graphics.GL.EXT.TextureEnvDot3
import Graphics.GL.EXT.TextureFilterAnisotropic
import Graphics.GL.EXT.TextureFilterMinmax
import Graphics.GL.EXT.TextureInteger
import Graphics.GL.EXT.TextureLODBias
import Graphics.GL.EXT.TextureMirrorClamp
import Graphics.GL.EXT.TextureObject
import Graphics.GL.EXT.TexturePerturbNormal
import Graphics.GL.EXT.TextureSNorm
import Graphics.GL.EXT.TextureSRGB
import Graphics.GL.EXT.TextureSRGBDecode
import Graphics.GL.EXT.TextureSharedExponent
import Graphics.GL.EXT.TextureSwizzle
import Graphics.GL.EXT.TimerQuery
import Graphics.GL.EXT.TransformFeedback
import Graphics.GL.EXT.VertexArray
import Graphics.GL.EXT.VertexArrayBGRA
import Graphics.GL.EXT.VertexAttrib64Bit
import Graphics.GL.EXT.VertexShader
import Graphics.GL.EXT.VertexWeighting
import Graphics.GL.EXT.X11SyncObject
