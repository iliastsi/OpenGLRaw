--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F58
-- Copyright   :  (c) Sven Panne 2016
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- Raw functions from the
-- <http://www.opengl.org/registry/ OpenGL registry>.
--
--------------------------------------------------------------------------------

module Graphics.GL.Functions.F58 (
  glStencilFillPathInstancedNV,
  glStencilFillPathNV,
  glStencilFunc,
  glStencilFuncSeparate,
  glStencilFuncSeparateATI,
  glStencilMask,
  glStencilMaskSeparate,
  glStencilOp,
  glStencilOpSeparate,
  glStencilOpSeparateATI,
  glStencilOpValueAMD,
  glStencilStrokePathInstancedNV,
  glStencilStrokePathNV,
  glStencilThenCoverFillPathInstancedNV,
  glStencilThenCoverFillPathNV,
  glStencilThenCoverStrokePathInstancedNV,
  glStencilThenCoverStrokePathNV,
  glStopInstrumentsSGIX,
  glStringMarkerGREMEDY,
  glSubpixelPrecisionBiasNV,
  glSwizzleEXT,
  glSyncTextureINTEL,
  glTagSampleBufferSGIX,
  glTangent3bEXT,
  glTangent3bvEXT,
  glTangent3dEXT,
  glTangent3dvEXT,
  glTangent3fEXT,
  glTangent3fvEXT,
  glTangent3iEXT,
  glTangent3ivEXT,
  glTangent3sEXT,
  glTangent3svEXT,
  glTangentPointerEXT,
  glTbufferMask3DFX,
  glTessellationFactorAMD,
  glTessellationModeAMD,
  glTestFenceAPPLE,
  glTestFenceNV,
  glTestObjectAPPLE
) where

import Control.Monad.IO.Class ( MonadIO(..) )
import Foreign.Marshal.Error ( throwIf )
import Foreign.Ptr ( Ptr, FunPtr, nullFunPtr )
import System.IO.Unsafe ( unsafePerformIO )

import Graphics.GL.Foreign
import Graphics.GL.GetProcAddress ( getProcAddress )
import Graphics.GL.Types

getCommand :: String -> IO (FunPtr a)
getCommand cmd =
  throwIfNullFunPtr ("unknown OpenGL command " ++ cmd) $ getProcAddress cmd

throwIfNullFunPtr :: String -> IO (FunPtr a) -> IO (FunPtr a)
throwIfNullFunPtr = throwIf (== nullFunPtr) . const

-- glStencilFillPathInstancedNV ------------------------------------------------

glStencilFillPathInstancedNV
  :: MonadIO m
  => GLsizei -- ^ @numPaths@.
  -> GLenum -- ^ @pathNameType@ of type @PathElementType@.
  -> Ptr a -- ^ @paths@ pointing to @COMPSIZE(numPaths,pathNameType,paths)@ elements of type @PathElement@.
  -> GLuint -- ^ @pathBase@ of type @Path@.
  -> GLenum -- ^ @fillMode@ of type @PathFillMode@.
  -> GLuint -- ^ @mask@ of type @MaskedStencilValue@.
  -> GLenum -- ^ @transformType@ of type @PathTransformType@.
  -> Ptr GLfloat -- ^ @transformValues@ pointing to @COMPSIZE(numPaths,transformType)@ elements of type @GLfloat@.
  -> m ()
glStencilFillPathInstancedNV v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn711 ptr_glStencilFillPathInstancedNV v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glStencilFillPathInstancedNV #-}
ptr_glStencilFillPathInstancedNV :: FunPtr (GLsizei -> GLenum -> Ptr a -> GLuint -> GLenum -> GLuint -> GLenum -> Ptr GLfloat -> IO ())
ptr_glStencilFillPathInstancedNV = unsafePerformIO $ getCommand "glStencilFillPathInstancedNV"

-- glStencilFillPathNV ---------------------------------------------------------

glStencilFillPathNV
  :: MonadIO m
  => GLuint -- ^ @path@ of type @Path@.
  -> GLenum -- ^ @fillMode@ of type @PathFillMode@.
  -> GLuint -- ^ @mask@ of type @MaskedStencilValue@.
  -> m ()
glStencilFillPathNV v1 v2 v3 = liftIO $ dyn712 ptr_glStencilFillPathNV v1 v2 v3

{-# NOINLINE ptr_glStencilFillPathNV #-}
ptr_glStencilFillPathNV :: FunPtr (GLuint -> GLenum -> GLuint -> IO ())
ptr_glStencilFillPathNV = unsafePerformIO $ getCommand "glStencilFillPathNV"

-- glStencilFunc ---------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glStencilFunc.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glStencilFunc.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glStencilFunc.xhtml OpenGL 4.x>.
glStencilFunc
  :: MonadIO m
  => GLenum -- ^ @func@ of type [StencilFunction](Graphics-GL-Groups.html#StencilFunction).
  -> GLint -- ^ @ref@ of type @StencilValue@.
  -> GLuint -- ^ @mask@ of type @MaskedStencilValue@.
  -> m ()
glStencilFunc v1 v2 v3 = liftIO $ dyn604 ptr_glStencilFunc v1 v2 v3

{-# NOINLINE ptr_glStencilFunc #-}
ptr_glStencilFunc :: FunPtr (GLenum -> GLint -> GLuint -> IO ())
ptr_glStencilFunc = unsafePerformIO $ getCommand "glStencilFunc"

-- glStencilFuncSeparate -------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glStencilFuncSeparate.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glStencilFuncSeparate.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glStencilFuncSeparate.xhtml OpenGL 4.x>.
glStencilFuncSeparate
  :: MonadIO m
  => GLenum -- ^ @face@ of type @StencilFaceDirection@.
  -> GLenum -- ^ @func@ of type [StencilFunction](Graphics-GL-Groups.html#StencilFunction).
  -> GLint -- ^ @ref@ of type @StencilValue@.
  -> GLuint -- ^ @mask@ of type @MaskedStencilValue@.
  -> m ()
glStencilFuncSeparate v1 v2 v3 v4 = liftIO $ dyn713 ptr_glStencilFuncSeparate v1 v2 v3 v4

{-# NOINLINE ptr_glStencilFuncSeparate #-}
ptr_glStencilFuncSeparate :: FunPtr (GLenum -> GLenum -> GLint -> GLuint -> IO ())
ptr_glStencilFuncSeparate = unsafePerformIO $ getCommand "glStencilFuncSeparate"

-- glStencilFuncSeparateATI ----------------------------------------------------

glStencilFuncSeparateATI
  :: MonadIO m
  => GLenum -- ^ @frontfunc@ of type [StencilFunction](Graphics-GL-Groups.html#StencilFunction).
  -> GLenum -- ^ @backfunc@ of type [StencilFunction](Graphics-GL-Groups.html#StencilFunction).
  -> GLint -- ^ @ref@ of type @ClampedStencilValue@.
  -> GLuint -- ^ @mask@ of type @MaskedStencilValue@.
  -> m ()
glStencilFuncSeparateATI v1 v2 v3 v4 = liftIO $ dyn713 ptr_glStencilFuncSeparateATI v1 v2 v3 v4

{-# NOINLINE ptr_glStencilFuncSeparateATI #-}
ptr_glStencilFuncSeparateATI :: FunPtr (GLenum -> GLenum -> GLint -> GLuint -> IO ())
ptr_glStencilFuncSeparateATI = unsafePerformIO $ getCommand "glStencilFuncSeparateATI"

-- glStencilMask ---------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glStencilMask.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glStencilMask.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glStencilMask.xhtml OpenGL 4.x>.
glStencilMask
  :: MonadIO m
  => GLuint -- ^ @mask@ of type @MaskedStencilValue@.
  -> m ()
glStencilMask v1 = liftIO $ dyn2 ptr_glStencilMask v1

{-# NOINLINE ptr_glStencilMask #-}
ptr_glStencilMask :: FunPtr (GLuint -> IO ())
ptr_glStencilMask = unsafePerformIO $ getCommand "glStencilMask"

-- glStencilMaskSeparate -------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glStencilMaskSeparate.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glStencilMaskSeparate.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glStencilMaskSeparate.xhtml OpenGL 4.x>.
glStencilMaskSeparate
  :: MonadIO m
  => GLenum -- ^ @face@ of type @StencilFaceDirection@.
  -> GLuint -- ^ @mask@ of type @MaskedStencilValue@.
  -> m ()
glStencilMaskSeparate v1 v2 = liftIO $ dyn16 ptr_glStencilMaskSeparate v1 v2

{-# NOINLINE ptr_glStencilMaskSeparate #-}
ptr_glStencilMaskSeparate :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glStencilMaskSeparate = unsafePerformIO $ getCommand "glStencilMaskSeparate"

-- glStencilOp -----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glStencilOp.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glStencilOp.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glStencilOp.xhtml OpenGL 4.x>.
glStencilOp
  :: MonadIO m
  => GLenum -- ^ @fail@ of type [StencilOp](Graphics-GL-Groups.html#StencilOp).
  -> GLenum -- ^ @zfail@ of type [StencilOp](Graphics-GL-Groups.html#StencilOp).
  -> GLenum -- ^ @zpass@ of type [StencilOp](Graphics-GL-Groups.html#StencilOp).
  -> m ()
glStencilOp v1 v2 v3 = liftIO $ dyn714 ptr_glStencilOp v1 v2 v3

{-# NOINLINE ptr_glStencilOp #-}
ptr_glStencilOp :: FunPtr (GLenum -> GLenum -> GLenum -> IO ())
ptr_glStencilOp = unsafePerformIO $ getCommand "glStencilOp"

-- glStencilOpSeparate ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glStencilOpSeparate.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glStencilOpSeparate.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glStencilOpSeparate.xhtml OpenGL 4.x>.
glStencilOpSeparate
  :: MonadIO m
  => GLenum -- ^ @face@ of type @StencilFaceDirection@.
  -> GLenum -- ^ @sfail@ of type [StencilOp](Graphics-GL-Groups.html#StencilOp).
  -> GLenum -- ^ @dpfail@ of type [StencilOp](Graphics-GL-Groups.html#StencilOp).
  -> GLenum -- ^ @dppass@ of type [StencilOp](Graphics-GL-Groups.html#StencilOp).
  -> m ()
glStencilOpSeparate v1 v2 v3 v4 = liftIO $ dyn53 ptr_glStencilOpSeparate v1 v2 v3 v4

{-# NOINLINE ptr_glStencilOpSeparate #-}
ptr_glStencilOpSeparate :: FunPtr (GLenum -> GLenum -> GLenum -> GLenum -> IO ())
ptr_glStencilOpSeparate = unsafePerformIO $ getCommand "glStencilOpSeparate"

-- glStencilOpSeparateATI ------------------------------------------------------

-- | This command is an alias for 'glStencilOpSeparate'.
glStencilOpSeparateATI
  :: MonadIO m
  => GLenum -- ^ @face@ of type @StencilFaceDirection@.
  -> GLenum -- ^ @sfail@ of type [StencilOp](Graphics-GL-Groups.html#StencilOp).
  -> GLenum -- ^ @dpfail@ of type [StencilOp](Graphics-GL-Groups.html#StencilOp).
  -> GLenum -- ^ @dppass@ of type [StencilOp](Graphics-GL-Groups.html#StencilOp).
  -> m ()
glStencilOpSeparateATI v1 v2 v3 v4 = liftIO $ dyn53 ptr_glStencilOpSeparateATI v1 v2 v3 v4

{-# NOINLINE ptr_glStencilOpSeparateATI #-}
ptr_glStencilOpSeparateATI :: FunPtr (GLenum -> GLenum -> GLenum -> GLenum -> IO ())
ptr_glStencilOpSeparateATI = unsafePerformIO $ getCommand "glStencilOpSeparateATI"

-- glStencilOpValueAMD ---------------------------------------------------------

glStencilOpValueAMD
  :: MonadIO m
  => GLenum -- ^ @face@ of type @StencilFaceDirection@.
  -> GLuint -- ^ @value@.
  -> m ()
glStencilOpValueAMD v1 v2 = liftIO $ dyn16 ptr_glStencilOpValueAMD v1 v2

{-# NOINLINE ptr_glStencilOpValueAMD #-}
ptr_glStencilOpValueAMD :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glStencilOpValueAMD = unsafePerformIO $ getCommand "glStencilOpValueAMD"

-- glStencilStrokePathInstancedNV ----------------------------------------------

glStencilStrokePathInstancedNV
  :: MonadIO m
  => GLsizei -- ^ @numPaths@.
  -> GLenum -- ^ @pathNameType@ of type @PathElementType@.
  -> Ptr a -- ^ @paths@ pointing to @COMPSIZE(numPaths,pathNameType,paths)@ elements of type @PathElement@.
  -> GLuint -- ^ @pathBase@ of type @Path@.
  -> GLint -- ^ @reference@ of type @StencilValue@.
  -> GLuint -- ^ @mask@ of type @MaskedStencilValue@.
  -> GLenum -- ^ @transformType@ of type @PathTransformType@.
  -> Ptr GLfloat -- ^ @transformValues@ pointing to @COMPSIZE(numPaths,transformType)@ elements of type @GLfloat@.
  -> m ()
glStencilStrokePathInstancedNV v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn715 ptr_glStencilStrokePathInstancedNV v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glStencilStrokePathInstancedNV #-}
ptr_glStencilStrokePathInstancedNV :: FunPtr (GLsizei -> GLenum -> Ptr a -> GLuint -> GLint -> GLuint -> GLenum -> Ptr GLfloat -> IO ())
ptr_glStencilStrokePathInstancedNV = unsafePerformIO $ getCommand "glStencilStrokePathInstancedNV"

-- glStencilStrokePathNV -------------------------------------------------------

glStencilStrokePathNV
  :: MonadIO m
  => GLuint -- ^ @path@ of type @Path@.
  -> GLint -- ^ @reference@ of type @StencilValue@.
  -> GLuint -- ^ @mask@ of type @MaskedStencilValue@.
  -> m ()
glStencilStrokePathNV v1 v2 v3 = liftIO $ dyn637 ptr_glStencilStrokePathNV v1 v2 v3

{-# NOINLINE ptr_glStencilStrokePathNV #-}
ptr_glStencilStrokePathNV :: FunPtr (GLuint -> GLint -> GLuint -> IO ())
ptr_glStencilStrokePathNV = unsafePerformIO $ getCommand "glStencilStrokePathNV"

-- glStencilThenCoverFillPathInstancedNV ---------------------------------------

glStencilThenCoverFillPathInstancedNV
  :: MonadIO m
  => GLsizei -- ^ @numPaths@.
  -> GLenum -- ^ @pathNameType@.
  -> Ptr a -- ^ @paths@.
  -> GLuint -- ^ @pathBase@.
  -> GLenum -- ^ @fillMode@.
  -> GLuint -- ^ @mask@.
  -> GLenum -- ^ @coverMode@.
  -> GLenum -- ^ @transformType@.
  -> Ptr GLfloat -- ^ @transformValues@.
  -> m ()
glStencilThenCoverFillPathInstancedNV v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn716 ptr_glStencilThenCoverFillPathInstancedNV v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glStencilThenCoverFillPathInstancedNV #-}
ptr_glStencilThenCoverFillPathInstancedNV :: FunPtr (GLsizei -> GLenum -> Ptr a -> GLuint -> GLenum -> GLuint -> GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glStencilThenCoverFillPathInstancedNV = unsafePerformIO $ getCommand "glStencilThenCoverFillPathInstancedNV"

-- glStencilThenCoverFillPathNV ------------------------------------------------

glStencilThenCoverFillPathNV
  :: MonadIO m
  => GLuint -- ^ @path@.
  -> GLenum -- ^ @fillMode@.
  -> GLuint -- ^ @mask@.
  -> GLenum -- ^ @coverMode@.
  -> m ()
glStencilThenCoverFillPathNV v1 v2 v3 v4 = liftIO $ dyn717 ptr_glStencilThenCoverFillPathNV v1 v2 v3 v4

{-# NOINLINE ptr_glStencilThenCoverFillPathNV #-}
ptr_glStencilThenCoverFillPathNV :: FunPtr (GLuint -> GLenum -> GLuint -> GLenum -> IO ())
ptr_glStencilThenCoverFillPathNV = unsafePerformIO $ getCommand "glStencilThenCoverFillPathNV"

-- glStencilThenCoverStrokePathInstancedNV -------------------------------------

glStencilThenCoverStrokePathInstancedNV
  :: MonadIO m
  => GLsizei -- ^ @numPaths@.
  -> GLenum -- ^ @pathNameType@.
  -> Ptr a -- ^ @paths@.
  -> GLuint -- ^ @pathBase@.
  -> GLint -- ^ @reference@.
  -> GLuint -- ^ @mask@.
  -> GLenum -- ^ @coverMode@.
  -> GLenum -- ^ @transformType@.
  -> Ptr GLfloat -- ^ @transformValues@.
  -> m ()
glStencilThenCoverStrokePathInstancedNV v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn718 ptr_glStencilThenCoverStrokePathInstancedNV v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glStencilThenCoverStrokePathInstancedNV #-}
ptr_glStencilThenCoverStrokePathInstancedNV :: FunPtr (GLsizei -> GLenum -> Ptr a -> GLuint -> GLint -> GLuint -> GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glStencilThenCoverStrokePathInstancedNV = unsafePerformIO $ getCommand "glStencilThenCoverStrokePathInstancedNV"

-- glStencilThenCoverStrokePathNV ----------------------------------------------

glStencilThenCoverStrokePathNV
  :: MonadIO m
  => GLuint -- ^ @path@.
  -> GLint -- ^ @reference@.
  -> GLuint -- ^ @mask@.
  -> GLenum -- ^ @coverMode@.
  -> m ()
glStencilThenCoverStrokePathNV v1 v2 v3 v4 = liftIO $ dyn719 ptr_glStencilThenCoverStrokePathNV v1 v2 v3 v4

{-# NOINLINE ptr_glStencilThenCoverStrokePathNV #-}
ptr_glStencilThenCoverStrokePathNV :: FunPtr (GLuint -> GLint -> GLuint -> GLenum -> IO ())
ptr_glStencilThenCoverStrokePathNV = unsafePerformIO $ getCommand "glStencilThenCoverStrokePathNV"

-- glStopInstrumentsSGIX -------------------------------------------------------

glStopInstrumentsSGIX
  :: MonadIO m
  => GLint -- ^ @marker@.
  -> m ()
glStopInstrumentsSGIX v1 = liftIO $ dyn12 ptr_glStopInstrumentsSGIX v1

{-# NOINLINE ptr_glStopInstrumentsSGIX #-}
ptr_glStopInstrumentsSGIX :: FunPtr (GLint -> IO ())
ptr_glStopInstrumentsSGIX = unsafePerformIO $ getCommand "glStopInstrumentsSGIX"

-- glStringMarkerGREMEDY -------------------------------------------------------

glStringMarkerGREMEDY
  :: MonadIO m
  => GLsizei -- ^ @len@.
  -> Ptr a -- ^ @string@ pointing to @len@ elements of type @a@.
  -> m ()
glStringMarkerGREMEDY v1 v2 = liftIO $ dyn260 ptr_glStringMarkerGREMEDY v1 v2

{-# NOINLINE ptr_glStringMarkerGREMEDY #-}
ptr_glStringMarkerGREMEDY :: FunPtr (GLsizei -> Ptr a -> IO ())
ptr_glStringMarkerGREMEDY = unsafePerformIO $ getCommand "glStringMarkerGREMEDY"

-- glSubpixelPrecisionBiasNV ---------------------------------------------------

glSubpixelPrecisionBiasNV
  :: MonadIO m
  => GLuint -- ^ @xbits@.
  -> GLuint -- ^ @ybits@.
  -> m ()
glSubpixelPrecisionBiasNV v1 v2 = liftIO $ dyn3 ptr_glSubpixelPrecisionBiasNV v1 v2

{-# NOINLINE ptr_glSubpixelPrecisionBiasNV #-}
ptr_glSubpixelPrecisionBiasNV :: FunPtr (GLuint -> GLuint -> IO ())
ptr_glSubpixelPrecisionBiasNV = unsafePerformIO $ getCommand "glSubpixelPrecisionBiasNV"

-- glSwizzleEXT ----------------------------------------------------------------

glSwizzleEXT
  :: MonadIO m
  => GLuint -- ^ @res@.
  -> GLuint -- ^ @in@.
  -> GLenum -- ^ @outX@ of type @VertexShaderCoordOutEXT@.
  -> GLenum -- ^ @outY@ of type @VertexShaderCoordOutEXT@.
  -> GLenum -- ^ @outZ@ of type @VertexShaderCoordOutEXT@.
  -> GLenum -- ^ @outW@ of type @VertexShaderCoordOutEXT@.
  -> m ()
glSwizzleEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn720 ptr_glSwizzleEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glSwizzleEXT #-}
ptr_glSwizzleEXT :: FunPtr (GLuint -> GLuint -> GLenum -> GLenum -> GLenum -> GLenum -> IO ())
ptr_glSwizzleEXT = unsafePerformIO $ getCommand "glSwizzleEXT"

-- glSyncTextureINTEL ----------------------------------------------------------

glSyncTextureINTEL
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> m ()
glSyncTextureINTEL v1 = liftIO $ dyn2 ptr_glSyncTextureINTEL v1

{-# NOINLINE ptr_glSyncTextureINTEL #-}
ptr_glSyncTextureINTEL :: FunPtr (GLuint -> IO ())
ptr_glSyncTextureINTEL = unsafePerformIO $ getCommand "glSyncTextureINTEL"

-- glTagSampleBufferSGIX -------------------------------------------------------

glTagSampleBufferSGIX
  :: MonadIO m
  => m ()
glTagSampleBufferSGIX = liftIO $ dyn10 ptr_glTagSampleBufferSGIX

{-# NOINLINE ptr_glTagSampleBufferSGIX #-}
ptr_glTagSampleBufferSGIX :: FunPtr (IO ())
ptr_glTagSampleBufferSGIX = unsafePerformIO $ getCommand "glTagSampleBufferSGIX"

-- glTangent3bEXT --------------------------------------------------------------

-- | The vector equivalent of this command is 'glTangent3bvEXT'.
glTangent3bEXT
  :: MonadIO m
  => GLbyte -- ^ @tx@.
  -> GLbyte -- ^ @ty@.
  -> GLbyte -- ^ @tz@.
  -> m ()
glTangent3bEXT v1 v2 v3 = liftIO $ dyn36 ptr_glTangent3bEXT v1 v2 v3

{-# NOINLINE ptr_glTangent3bEXT #-}
ptr_glTangent3bEXT :: FunPtr (GLbyte -> GLbyte -> GLbyte -> IO ())
ptr_glTangent3bEXT = unsafePerformIO $ getCommand "glTangent3bEXT"

-- glTangent3bvEXT -------------------------------------------------------------

glTangent3bvEXT
  :: MonadIO m
  => Ptr GLbyte -- ^ @v@ pointing to @3@ elements of type @GLbyte@.
  -> m ()
glTangent3bvEXT v1 = liftIO $ dyn37 ptr_glTangent3bvEXT v1

{-# NOINLINE ptr_glTangent3bvEXT #-}
ptr_glTangent3bvEXT :: FunPtr (Ptr GLbyte -> IO ())
ptr_glTangent3bvEXT = unsafePerformIO $ getCommand "glTangent3bvEXT"

-- glTangent3dEXT --------------------------------------------------------------

-- | The vector equivalent of this command is 'glTangent3dvEXT'.
glTangent3dEXT
  :: MonadIO m
  => GLdouble -- ^ @tx@ of type @CoordD@.
  -> GLdouble -- ^ @ty@ of type @CoordD@.
  -> GLdouble -- ^ @tz@ of type @CoordD@.
  -> m ()
glTangent3dEXT v1 v2 v3 = liftIO $ dyn38 ptr_glTangent3dEXT v1 v2 v3

{-# NOINLINE ptr_glTangent3dEXT #-}
ptr_glTangent3dEXT :: FunPtr (GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glTangent3dEXT = unsafePerformIO $ getCommand "glTangent3dEXT"

-- glTangent3dvEXT -------------------------------------------------------------

glTangent3dvEXT
  :: MonadIO m
  => Ptr GLdouble -- ^ @v@ pointing to @3@ elements of type @CoordD@.
  -> m ()
glTangent3dvEXT v1 = liftIO $ dyn39 ptr_glTangent3dvEXT v1

{-# NOINLINE ptr_glTangent3dvEXT #-}
ptr_glTangent3dvEXT :: FunPtr (Ptr GLdouble -> IO ())
ptr_glTangent3dvEXT = unsafePerformIO $ getCommand "glTangent3dvEXT"

-- glTangent3fEXT --------------------------------------------------------------

-- | The vector equivalent of this command is 'glTangent3fvEXT'.
glTangent3fEXT
  :: MonadIO m
  => GLfloat -- ^ @tx@ of type @CoordF@.
  -> GLfloat -- ^ @ty@ of type @CoordF@.
  -> GLfloat -- ^ @tz@ of type @CoordF@.
  -> m ()
glTangent3fEXT v1 v2 v3 = liftIO $ dyn40 ptr_glTangent3fEXT v1 v2 v3

{-# NOINLINE ptr_glTangent3fEXT #-}
ptr_glTangent3fEXT :: FunPtr (GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glTangent3fEXT = unsafePerformIO $ getCommand "glTangent3fEXT"

-- glTangent3fvEXT -------------------------------------------------------------

glTangent3fvEXT
  :: MonadIO m
  => Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @CoordF@.
  -> m ()
glTangent3fvEXT v1 = liftIO $ dyn41 ptr_glTangent3fvEXT v1

{-# NOINLINE ptr_glTangent3fvEXT #-}
ptr_glTangent3fvEXT :: FunPtr (Ptr GLfloat -> IO ())
ptr_glTangent3fvEXT = unsafePerformIO $ getCommand "glTangent3fvEXT"

-- glTangent3iEXT --------------------------------------------------------------

-- | The vector equivalent of this command is 'glTangent3ivEXT'.
glTangent3iEXT
  :: MonadIO m
  => GLint -- ^ @tx@.
  -> GLint -- ^ @ty@.
  -> GLint -- ^ @tz@.
  -> m ()
glTangent3iEXT v1 v2 v3 = liftIO $ dyn42 ptr_glTangent3iEXT v1 v2 v3

{-# NOINLINE ptr_glTangent3iEXT #-}
ptr_glTangent3iEXT :: FunPtr (GLint -> GLint -> GLint -> IO ())
ptr_glTangent3iEXT = unsafePerformIO $ getCommand "glTangent3iEXT"

-- glTangent3ivEXT -------------------------------------------------------------

glTangent3ivEXT
  :: MonadIO m
  => Ptr GLint -- ^ @v@ pointing to @3@ elements of type @GLint@.
  -> m ()
glTangent3ivEXT v1 = liftIO $ dyn43 ptr_glTangent3ivEXT v1

{-# NOINLINE ptr_glTangent3ivEXT #-}
ptr_glTangent3ivEXT :: FunPtr (Ptr GLint -> IO ())
ptr_glTangent3ivEXT = unsafePerformIO $ getCommand "glTangent3ivEXT"

-- glTangent3sEXT --------------------------------------------------------------

-- | The vector equivalent of this command is 'glTangent3svEXT'.
glTangent3sEXT
  :: MonadIO m
  => GLshort -- ^ @tx@.
  -> GLshort -- ^ @ty@.
  -> GLshort -- ^ @tz@.
  -> m ()
glTangent3sEXT v1 v2 v3 = liftIO $ dyn44 ptr_glTangent3sEXT v1 v2 v3

{-# NOINLINE ptr_glTangent3sEXT #-}
ptr_glTangent3sEXT :: FunPtr (GLshort -> GLshort -> GLshort -> IO ())
ptr_glTangent3sEXT = unsafePerformIO $ getCommand "glTangent3sEXT"

-- glTangent3svEXT -------------------------------------------------------------

glTangent3svEXT
  :: MonadIO m
  => Ptr GLshort -- ^ @v@ pointing to @3@ elements of type @GLshort@.
  -> m ()
glTangent3svEXT v1 = liftIO $ dyn45 ptr_glTangent3svEXT v1

{-# NOINLINE ptr_glTangent3svEXT #-}
ptr_glTangent3svEXT :: FunPtr (Ptr GLshort -> IO ())
ptr_glTangent3svEXT = unsafePerformIO $ getCommand "glTangent3svEXT"

-- glTangentPointerEXT ---------------------------------------------------------

glTangentPointerEXT
  :: MonadIO m
  => GLenum -- ^ @type@ of type @TangentPointerTypeEXT@.
  -> GLsizei -- ^ @stride@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(type,stride)@ elements of type @a@.
  -> m ()
glTangentPointerEXT v1 v2 v3 = liftIO $ dyn46 ptr_glTangentPointerEXT v1 v2 v3

{-# NOINLINE ptr_glTangentPointerEXT #-}
ptr_glTangentPointerEXT :: FunPtr (GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glTangentPointerEXT = unsafePerformIO $ getCommand "glTangentPointerEXT"

-- glTbufferMask3DFX -----------------------------------------------------------

glTbufferMask3DFX
  :: MonadIO m
  => GLuint -- ^ @mask@.
  -> m ()
glTbufferMask3DFX v1 = liftIO $ dyn2 ptr_glTbufferMask3DFX v1

{-# NOINLINE ptr_glTbufferMask3DFX #-}
ptr_glTbufferMask3DFX :: FunPtr (GLuint -> IO ())
ptr_glTbufferMask3DFX = unsafePerformIO $ getCommand "glTbufferMask3DFX"

-- glTessellationFactorAMD -----------------------------------------------------

glTessellationFactorAMD
  :: MonadIO m
  => GLfloat -- ^ @factor@.
  -> m ()
glTessellationFactorAMD v1 = liftIO $ dyn79 ptr_glTessellationFactorAMD v1

{-# NOINLINE ptr_glTessellationFactorAMD #-}
ptr_glTessellationFactorAMD :: FunPtr (GLfloat -> IO ())
ptr_glTessellationFactorAMD = unsafePerformIO $ getCommand "glTessellationFactorAMD"

-- glTessellationModeAMD -------------------------------------------------------

glTessellationModeAMD
  :: MonadIO m
  => GLenum -- ^ @mode@.
  -> m ()
glTessellationModeAMD v1 = liftIO $ dyn4 ptr_glTessellationModeAMD v1

{-# NOINLINE ptr_glTessellationModeAMD #-}
ptr_glTessellationModeAMD :: FunPtr (GLenum -> IO ())
ptr_glTessellationModeAMD = unsafePerformIO $ getCommand "glTessellationModeAMD"

-- glTestFenceAPPLE ------------------------------------------------------------

glTestFenceAPPLE
  :: MonadIO m
  => GLuint -- ^ @fence@ of type @FenceNV@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glTestFenceAPPLE v1 = liftIO $ dyn273 ptr_glTestFenceAPPLE v1

{-# NOINLINE ptr_glTestFenceAPPLE #-}
ptr_glTestFenceAPPLE :: FunPtr (GLuint -> IO GLboolean)
ptr_glTestFenceAPPLE = unsafePerformIO $ getCommand "glTestFenceAPPLE"

-- glTestFenceNV ---------------------------------------------------------------

glTestFenceNV
  :: MonadIO m
  => GLuint -- ^ @fence@ of type @FenceNV@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glTestFenceNV v1 = liftIO $ dyn273 ptr_glTestFenceNV v1

{-# NOINLINE ptr_glTestFenceNV #-}
ptr_glTestFenceNV :: FunPtr (GLuint -> IO GLboolean)
ptr_glTestFenceNV = unsafePerformIO $ getCommand "glTestFenceNV"

-- glTestObjectAPPLE -----------------------------------------------------------

glTestObjectAPPLE
  :: MonadIO m
  => GLenum -- ^ @object@ of type @ObjectTypeAPPLE@.
  -> GLuint -- ^ @name@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glTestObjectAPPLE v1 v2 = liftIO $ dyn477 ptr_glTestObjectAPPLE v1 v2

{-# NOINLINE ptr_glTestObjectAPPLE #-}
ptr_glTestObjectAPPLE :: FunPtr (GLenum -> GLuint -> IO GLboolean)
ptr_glTestObjectAPPLE = unsafePerformIO $ getCommand "glTestObjectAPPLE"

