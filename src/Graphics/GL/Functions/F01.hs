--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F01
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

module Graphics.GL.Functions.F01 (
  glAccum,
  glAccumxOES,
  glActiveProgramEXT,
  glActiveShaderProgram,
  glActiveShaderProgramEXT,
  glActiveStencilFaceEXT,
  glActiveTexture,
  glActiveTextureARB,
  glActiveVaryingNV,
  glAlphaFragmentOp1ATI,
  glAlphaFragmentOp2ATI,
  glAlphaFragmentOp3ATI,
  glAlphaFunc,
  glAlphaFuncQCOM,
  glAlphaFuncx,
  glAlphaFuncxOES,
  glApplyFramebufferAttachmentCMAAINTEL,
  glApplyTextureEXT,
  glAreProgramsResidentNV,
  glAreTexturesResident,
  glAreTexturesResidentEXT,
  glArrayElement,
  glArrayElementEXT,
  glArrayObjectATI,
  glAsyncMarkerSGIX,
  glAttachObjectARB,
  glAttachShader,
  glBegin,
  glBeginConditionalRender,
  glBeginConditionalRenderNV,
  glBeginConditionalRenderNVX,
  glBeginFragmentShaderATI,
  glBeginOcclusionQueryNV,
  glBeginPerfMonitorAMD,
  glBeginPerfQueryINTEL,
  glBeginQuery,
  glBeginQueryARB,
  glBeginQueryEXT,
  glBeginQueryIndexed,
  glBeginTransformFeedback
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

-- glAccum ---------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glAccum.xml OpenGL 2.x>.
glAccum
  :: MonadIO m
  => GLenum -- ^ @op@ of type [AccumOp](Graphics-GL-Groups.html#AccumOp).
  -> GLfloat -- ^ @value@ of type @CoordF@.
  -> m ()
glAccum v1 v2 = liftIO $ dyn0 ptr_glAccum v1 v2

{-# NOINLINE ptr_glAccum #-}
ptr_glAccum :: FunPtr (GLenum -> GLfloat -> IO ())
ptr_glAccum = unsafePerformIO $ getCommand "glAccum"

-- glAccumxOES -----------------------------------------------------------------

glAccumxOES
  :: MonadIO m
  => GLenum -- ^ @op@.
  -> GLfixed -- ^ @value@.
  -> m ()
glAccumxOES v1 v2 = liftIO $ dyn1 ptr_glAccumxOES v1 v2

{-# NOINLINE ptr_glAccumxOES #-}
ptr_glAccumxOES :: FunPtr (GLenum -> GLfixed -> IO ())
ptr_glAccumxOES = unsafePerformIO $ getCommand "glAccumxOES"

-- glActiveProgramEXT ----------------------------------------------------------

glActiveProgramEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> m ()
glActiveProgramEXT v1 = liftIO $ dyn2 ptr_glActiveProgramEXT v1

{-# NOINLINE ptr_glActiveProgramEXT #-}
ptr_glActiveProgramEXT :: FunPtr (GLuint -> IO ())
ptr_glActiveProgramEXT = unsafePerformIO $ getCommand "glActiveProgramEXT"

-- glActiveShaderProgram -------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glActiveShaderProgram.xhtml OpenGL 4.x>.
glActiveShaderProgram
  :: MonadIO m
  => GLuint -- ^ @pipeline@.
  -> GLuint -- ^ @program@.
  -> m ()
glActiveShaderProgram v1 v2 = liftIO $ dyn3 ptr_glActiveShaderProgram v1 v2

{-# NOINLINE ptr_glActiveShaderProgram #-}
ptr_glActiveShaderProgram :: FunPtr (GLuint -> GLuint -> IO ())
ptr_glActiveShaderProgram = unsafePerformIO $ getCommand "glActiveShaderProgram"

-- glActiveShaderProgramEXT ----------------------------------------------------

glActiveShaderProgramEXT
  :: MonadIO m
  => GLuint -- ^ @pipeline@.
  -> GLuint -- ^ @program@.
  -> m ()
glActiveShaderProgramEXT v1 v2 = liftIO $ dyn3 ptr_glActiveShaderProgramEXT v1 v2

{-# NOINLINE ptr_glActiveShaderProgramEXT #-}
ptr_glActiveShaderProgramEXT :: FunPtr (GLuint -> GLuint -> IO ())
ptr_glActiveShaderProgramEXT = unsafePerformIO $ getCommand "glActiveShaderProgramEXT"

-- glActiveStencilFaceEXT ------------------------------------------------------

glActiveStencilFaceEXT
  :: MonadIO m
  => GLenum -- ^ @face@ of type @StencilFaceDirection@.
  -> m ()
glActiveStencilFaceEXT v1 = liftIO $ dyn4 ptr_glActiveStencilFaceEXT v1

{-# NOINLINE ptr_glActiveStencilFaceEXT #-}
ptr_glActiveStencilFaceEXT :: FunPtr (GLenum -> IO ())
ptr_glActiveStencilFaceEXT = unsafePerformIO $ getCommand "glActiveStencilFaceEXT"

-- glActiveTexture -------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glActiveTexture.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glActiveTexture.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glActiveTexture.xhtml OpenGL 4.x>.
glActiveTexture
  :: MonadIO m
  => GLenum -- ^ @texture@ of type @TextureUnit@.
  -> m ()
glActiveTexture v1 = liftIO $ dyn4 ptr_glActiveTexture v1

{-# NOINLINE ptr_glActiveTexture #-}
ptr_glActiveTexture :: FunPtr (GLenum -> IO ())
ptr_glActiveTexture = unsafePerformIO $ getCommand "glActiveTexture"

-- glActiveTextureARB ----------------------------------------------------------

-- | This command is an alias for 'glActiveTexture'.
glActiveTextureARB
  :: MonadIO m
  => GLenum -- ^ @texture@ of type @TextureUnit@.
  -> m ()
glActiveTextureARB v1 = liftIO $ dyn4 ptr_glActiveTextureARB v1

{-# NOINLINE ptr_glActiveTextureARB #-}
ptr_glActiveTextureARB :: FunPtr (GLenum -> IO ())
ptr_glActiveTextureARB = unsafePerformIO $ getCommand "glActiveTextureARB"

-- glActiveVaryingNV -----------------------------------------------------------

glActiveVaryingNV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> Ptr GLchar -- ^ @name@ pointing to @COMPSIZE(name)@ elements of type @GLchar@.
  -> m ()
glActiveVaryingNV v1 v2 = liftIO $ dyn5 ptr_glActiveVaryingNV v1 v2

{-# NOINLINE ptr_glActiveVaryingNV #-}
ptr_glActiveVaryingNV :: FunPtr (GLuint -> Ptr GLchar -> IO ())
ptr_glActiveVaryingNV = unsafePerformIO $ getCommand "glActiveVaryingNV"

-- glAlphaFragmentOp1ATI -------------------------------------------------------

glAlphaFragmentOp1ATI
  :: MonadIO m
  => GLenum -- ^ @op@ of type @FragmentOpATI@.
  -> GLuint -- ^ @dst@.
  -> GLuint -- ^ @dstMod@.
  -> GLuint -- ^ @arg1@.
  -> GLuint -- ^ @arg1Rep@.
  -> GLuint -- ^ @arg1Mod@.
  -> m ()
glAlphaFragmentOp1ATI v1 v2 v3 v4 v5 v6 = liftIO $ dyn6 ptr_glAlphaFragmentOp1ATI v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glAlphaFragmentOp1ATI #-}
ptr_glAlphaFragmentOp1ATI :: FunPtr (GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ())
ptr_glAlphaFragmentOp1ATI = unsafePerformIO $ getCommand "glAlphaFragmentOp1ATI"

-- glAlphaFragmentOp2ATI -------------------------------------------------------

glAlphaFragmentOp2ATI
  :: MonadIO m
  => GLenum -- ^ @op@ of type @FragmentOpATI@.
  -> GLuint -- ^ @dst@.
  -> GLuint -- ^ @dstMod@.
  -> GLuint -- ^ @arg1@.
  -> GLuint -- ^ @arg1Rep@.
  -> GLuint -- ^ @arg1Mod@.
  -> GLuint -- ^ @arg2@.
  -> GLuint -- ^ @arg2Rep@.
  -> GLuint -- ^ @arg2Mod@.
  -> m ()
glAlphaFragmentOp2ATI v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn7 ptr_glAlphaFragmentOp2ATI v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glAlphaFragmentOp2ATI #-}
ptr_glAlphaFragmentOp2ATI :: FunPtr (GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ())
ptr_glAlphaFragmentOp2ATI = unsafePerformIO $ getCommand "glAlphaFragmentOp2ATI"

-- glAlphaFragmentOp3ATI -------------------------------------------------------

glAlphaFragmentOp3ATI
  :: MonadIO m
  => GLenum -- ^ @op@ of type @FragmentOpATI@.
  -> GLuint -- ^ @dst@.
  -> GLuint -- ^ @dstMod@.
  -> GLuint -- ^ @arg1@.
  -> GLuint -- ^ @arg1Rep@.
  -> GLuint -- ^ @arg1Mod@.
  -> GLuint -- ^ @arg2@.
  -> GLuint -- ^ @arg2Rep@.
  -> GLuint -- ^ @arg2Mod@.
  -> GLuint -- ^ @arg3@.
  -> GLuint -- ^ @arg3Rep@.
  -> GLuint -- ^ @arg3Mod@.
  -> m ()
glAlphaFragmentOp3ATI v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 = liftIO $ dyn8 ptr_glAlphaFragmentOp3ATI v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12

{-# NOINLINE ptr_glAlphaFragmentOp3ATI #-}
ptr_glAlphaFragmentOp3ATI :: FunPtr (GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ())
ptr_glAlphaFragmentOp3ATI = unsafePerformIO $ getCommand "glAlphaFragmentOp3ATI"

-- glAlphaFunc -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glAlphaFunc.xml OpenGL 2.x>.
glAlphaFunc
  :: MonadIO m
  => GLenum -- ^ @func@ of type [AlphaFunction](Graphics-GL-Groups.html#AlphaFunction).
  -> GLfloat -- ^ @ref@.
  -> m ()
glAlphaFunc v1 v2 = liftIO $ dyn0 ptr_glAlphaFunc v1 v2

{-# NOINLINE ptr_glAlphaFunc #-}
ptr_glAlphaFunc :: FunPtr (GLenum -> GLfloat -> IO ())
ptr_glAlphaFunc = unsafePerformIO $ getCommand "glAlphaFunc"

-- glAlphaFuncQCOM -------------------------------------------------------------

glAlphaFuncQCOM
  :: MonadIO m
  => GLenum -- ^ @func@.
  -> GLclampf -- ^ @ref@.
  -> m ()
glAlphaFuncQCOM v1 v2 = liftIO $ dyn9 ptr_glAlphaFuncQCOM v1 v2

{-# NOINLINE ptr_glAlphaFuncQCOM #-}
ptr_glAlphaFuncQCOM :: FunPtr (GLenum -> GLclampf -> IO ())
ptr_glAlphaFuncQCOM = unsafePerformIO $ getCommand "glAlphaFuncQCOM"

-- glAlphaFuncx ----------------------------------------------------------------

glAlphaFuncx
  :: MonadIO m
  => GLenum -- ^ @func@.
  -> GLfixed -- ^ @ref@.
  -> m ()
glAlphaFuncx v1 v2 = liftIO $ dyn1 ptr_glAlphaFuncx v1 v2

{-# NOINLINE ptr_glAlphaFuncx #-}
ptr_glAlphaFuncx :: FunPtr (GLenum -> GLfixed -> IO ())
ptr_glAlphaFuncx = unsafePerformIO $ getCommand "glAlphaFuncx"

-- glAlphaFuncxOES -------------------------------------------------------------

glAlphaFuncxOES
  :: MonadIO m
  => GLenum -- ^ @func@.
  -> GLfixed -- ^ @ref@ of type @ClampedFixed@.
  -> m ()
glAlphaFuncxOES v1 v2 = liftIO $ dyn1 ptr_glAlphaFuncxOES v1 v2

{-# NOINLINE ptr_glAlphaFuncxOES #-}
ptr_glAlphaFuncxOES :: FunPtr (GLenum -> GLfixed -> IO ())
ptr_glAlphaFuncxOES = unsafePerformIO $ getCommand "glAlphaFuncxOES"

-- glApplyFramebufferAttachmentCMAAINTEL ---------------------------------------

glApplyFramebufferAttachmentCMAAINTEL
  :: MonadIO m
  => m ()
glApplyFramebufferAttachmentCMAAINTEL = liftIO $ dyn10 ptr_glApplyFramebufferAttachmentCMAAINTEL

{-# NOINLINE ptr_glApplyFramebufferAttachmentCMAAINTEL #-}
ptr_glApplyFramebufferAttachmentCMAAINTEL :: FunPtr (IO ())
ptr_glApplyFramebufferAttachmentCMAAINTEL = unsafePerformIO $ getCommand "glApplyFramebufferAttachmentCMAAINTEL"

-- glApplyTextureEXT -----------------------------------------------------------

glApplyTextureEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type @LightTextureModeEXT@.
  -> m ()
glApplyTextureEXT v1 = liftIO $ dyn4 ptr_glApplyTextureEXT v1

{-# NOINLINE ptr_glApplyTextureEXT #-}
ptr_glApplyTextureEXT :: FunPtr (GLenum -> IO ())
ptr_glApplyTextureEXT = unsafePerformIO $ getCommand "glApplyTextureEXT"

-- glAreProgramsResidentNV -----------------------------------------------------

glAreProgramsResidentNV
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @programs@ pointing to @n@ elements of type @GLuint@.
  -> Ptr GLboolean -- ^ @residences@ pointing to @n@ elements of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glAreProgramsResidentNV v1 v2 v3 = liftIO $ dyn11 ptr_glAreProgramsResidentNV v1 v2 v3

{-# NOINLINE ptr_glAreProgramsResidentNV #-}
ptr_glAreProgramsResidentNV :: FunPtr (GLsizei -> Ptr GLuint -> Ptr GLboolean -> IO GLboolean)
ptr_glAreProgramsResidentNV = unsafePerformIO $ getCommand "glAreProgramsResidentNV"

-- glAreTexturesResident -------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glAreTexturesResident.xml OpenGL 2.x>.
glAreTexturesResident
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @textures@ pointing to @n@ elements of type @Texture@.
  -> Ptr GLboolean -- ^ @residences@ pointing to @n@ elements of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glAreTexturesResident v1 v2 v3 = liftIO $ dyn11 ptr_glAreTexturesResident v1 v2 v3

{-# NOINLINE ptr_glAreTexturesResident #-}
ptr_glAreTexturesResident :: FunPtr (GLsizei -> Ptr GLuint -> Ptr GLboolean -> IO GLboolean)
ptr_glAreTexturesResident = unsafePerformIO $ getCommand "glAreTexturesResident"

-- glAreTexturesResidentEXT ----------------------------------------------------

glAreTexturesResidentEXT
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @textures@ pointing to @n@ elements of type @Texture@.
  -> Ptr GLboolean -- ^ @residences@ pointing to @n@ elements of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glAreTexturesResidentEXT v1 v2 v3 = liftIO $ dyn11 ptr_glAreTexturesResidentEXT v1 v2 v3

{-# NOINLINE ptr_glAreTexturesResidentEXT #-}
ptr_glAreTexturesResidentEXT :: FunPtr (GLsizei -> Ptr GLuint -> Ptr GLboolean -> IO GLboolean)
ptr_glAreTexturesResidentEXT = unsafePerformIO $ getCommand "glAreTexturesResidentEXT"

-- glArrayElement --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glArrayElement.xml OpenGL 2.x>.
glArrayElement
  :: MonadIO m
  => GLint -- ^ @i@.
  -> m ()
glArrayElement v1 = liftIO $ dyn12 ptr_glArrayElement v1

{-# NOINLINE ptr_glArrayElement #-}
ptr_glArrayElement :: FunPtr (GLint -> IO ())
ptr_glArrayElement = unsafePerformIO $ getCommand "glArrayElement"

-- glArrayElementEXT -----------------------------------------------------------

-- | This command is an alias for 'glArrayElement'.
glArrayElementEXT
  :: MonadIO m
  => GLint -- ^ @i@.
  -> m ()
glArrayElementEXT v1 = liftIO $ dyn12 ptr_glArrayElementEXT v1

{-# NOINLINE ptr_glArrayElementEXT #-}
ptr_glArrayElementEXT :: FunPtr (GLint -> IO ())
ptr_glArrayElementEXT = unsafePerformIO $ getCommand "glArrayElementEXT"

-- glArrayObjectATI ------------------------------------------------------------

glArrayObjectATI
  :: MonadIO m
  => GLenum -- ^ @array@ of type [EnableCap](Graphics-GL-Groups.html#EnableCap).
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type @ScalarType@.
  -> GLsizei -- ^ @stride@.
  -> GLuint -- ^ @buffer@.
  -> GLuint -- ^ @offset@.
  -> m ()
glArrayObjectATI v1 v2 v3 v4 v5 v6 = liftIO $ dyn13 ptr_glArrayObjectATI v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glArrayObjectATI #-}
ptr_glArrayObjectATI :: FunPtr (GLenum -> GLint -> GLenum -> GLsizei -> GLuint -> GLuint -> IO ())
ptr_glArrayObjectATI = unsafePerformIO $ getCommand "glArrayObjectATI"

-- glAsyncMarkerSGIX -----------------------------------------------------------

glAsyncMarkerSGIX
  :: MonadIO m
  => GLuint -- ^ @marker@.
  -> m ()
glAsyncMarkerSGIX v1 = liftIO $ dyn2 ptr_glAsyncMarkerSGIX v1

{-# NOINLINE ptr_glAsyncMarkerSGIX #-}
ptr_glAsyncMarkerSGIX :: FunPtr (GLuint -> IO ())
ptr_glAsyncMarkerSGIX = unsafePerformIO $ getCommand "glAsyncMarkerSGIX"

-- glAttachObjectARB -----------------------------------------------------------

-- | This command is an alias for 'glAttachShader'.
glAttachObjectARB
  :: MonadIO m
  => GLhandleARB -- ^ @containerObj@ of type @handleARB@.
  -> GLhandleARB -- ^ @obj@ of type @handleARB@.
  -> m ()
glAttachObjectARB v1 v2 = liftIO $ dyn14 ptr_glAttachObjectARB v1 v2

{-# NOINLINE ptr_glAttachObjectARB #-}
ptr_glAttachObjectARB :: FunPtr (GLhandleARB -> GLhandleARB -> IO ())
ptr_glAttachObjectARB = unsafePerformIO $ getCommand "glAttachObjectARB"

-- glAttachShader --------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glAttachShader.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glAttachShader.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glAttachShader.xhtml OpenGL 4.x>.
glAttachShader
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLuint -- ^ @shader@.
  -> m ()
glAttachShader v1 v2 = liftIO $ dyn3 ptr_glAttachShader v1 v2

{-# NOINLINE ptr_glAttachShader #-}
ptr_glAttachShader :: FunPtr (GLuint -> GLuint -> IO ())
ptr_glAttachShader = unsafePerformIO $ getCommand "glAttachShader"

-- glBegin ---------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glBegin.xml OpenGL 2.x>.
glBegin
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> m ()
glBegin v1 = liftIO $ dyn4 ptr_glBegin v1

{-# NOINLINE ptr_glBegin #-}
ptr_glBegin :: FunPtr (GLenum -> IO ())
ptr_glBegin = unsafePerformIO $ getCommand "glBegin"

-- glBeginConditionalRender ----------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glBeginConditionalRender.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glBeginConditionalRender.xhtml OpenGL 4.x>.
glBeginConditionalRender
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @mode@ of type @TypeEnum@.
  -> m ()
glBeginConditionalRender v1 v2 = liftIO $ dyn15 ptr_glBeginConditionalRender v1 v2

{-# NOINLINE ptr_glBeginConditionalRender #-}
ptr_glBeginConditionalRender :: FunPtr (GLuint -> GLenum -> IO ())
ptr_glBeginConditionalRender = unsafePerformIO $ getCommand "glBeginConditionalRender"

-- glBeginConditionalRenderNV --------------------------------------------------

-- | This command is an alias for 'glBeginConditionalRender'.
glBeginConditionalRenderNV
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @mode@ of type @TypeEnum@.
  -> m ()
glBeginConditionalRenderNV v1 v2 = liftIO $ dyn15 ptr_glBeginConditionalRenderNV v1 v2

{-# NOINLINE ptr_glBeginConditionalRenderNV #-}
ptr_glBeginConditionalRenderNV :: FunPtr (GLuint -> GLenum -> IO ())
ptr_glBeginConditionalRenderNV = unsafePerformIO $ getCommand "glBeginConditionalRenderNV"

-- glBeginConditionalRenderNVX -------------------------------------------------

glBeginConditionalRenderNVX
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> m ()
glBeginConditionalRenderNVX v1 = liftIO $ dyn2 ptr_glBeginConditionalRenderNVX v1

{-# NOINLINE ptr_glBeginConditionalRenderNVX #-}
ptr_glBeginConditionalRenderNVX :: FunPtr (GLuint -> IO ())
ptr_glBeginConditionalRenderNVX = unsafePerformIO $ getCommand "glBeginConditionalRenderNVX"

-- glBeginFragmentShaderATI ----------------------------------------------------

glBeginFragmentShaderATI
  :: MonadIO m
  => m ()
glBeginFragmentShaderATI = liftIO $ dyn10 ptr_glBeginFragmentShaderATI

{-# NOINLINE ptr_glBeginFragmentShaderATI #-}
ptr_glBeginFragmentShaderATI :: FunPtr (IO ())
ptr_glBeginFragmentShaderATI = unsafePerformIO $ getCommand "glBeginFragmentShaderATI"

-- glBeginOcclusionQueryNV -----------------------------------------------------

glBeginOcclusionQueryNV
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> m ()
glBeginOcclusionQueryNV v1 = liftIO $ dyn2 ptr_glBeginOcclusionQueryNV v1

{-# NOINLINE ptr_glBeginOcclusionQueryNV #-}
ptr_glBeginOcclusionQueryNV :: FunPtr (GLuint -> IO ())
ptr_glBeginOcclusionQueryNV = unsafePerformIO $ getCommand "glBeginOcclusionQueryNV"

-- glBeginPerfMonitorAMD -------------------------------------------------------

glBeginPerfMonitorAMD
  :: MonadIO m
  => GLuint -- ^ @monitor@.
  -> m ()
glBeginPerfMonitorAMD v1 = liftIO $ dyn2 ptr_glBeginPerfMonitorAMD v1

{-# NOINLINE ptr_glBeginPerfMonitorAMD #-}
ptr_glBeginPerfMonitorAMD :: FunPtr (GLuint -> IO ())
ptr_glBeginPerfMonitorAMD = unsafePerformIO $ getCommand "glBeginPerfMonitorAMD"

-- glBeginPerfQueryINTEL -------------------------------------------------------

glBeginPerfQueryINTEL
  :: MonadIO m
  => GLuint -- ^ @queryHandle@.
  -> m ()
glBeginPerfQueryINTEL v1 = liftIO $ dyn2 ptr_glBeginPerfQueryINTEL v1

{-# NOINLINE ptr_glBeginPerfQueryINTEL #-}
ptr_glBeginPerfQueryINTEL :: FunPtr (GLuint -> IO ())
ptr_glBeginPerfQueryINTEL = unsafePerformIO $ getCommand "glBeginPerfQueryINTEL"

-- glBeginQuery ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glBeginQuery.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glBeginQuery.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glBeginQuery.xhtml OpenGL 4.x>.
glBeginQuery
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLuint -- ^ @id@.
  -> m ()
glBeginQuery v1 v2 = liftIO $ dyn16 ptr_glBeginQuery v1 v2

{-# NOINLINE ptr_glBeginQuery #-}
ptr_glBeginQuery :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glBeginQuery = unsafePerformIO $ getCommand "glBeginQuery"

-- glBeginQueryARB -------------------------------------------------------------

-- | This command is an alias for 'glBeginQuery'.
glBeginQueryARB
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLuint -- ^ @id@.
  -> m ()
glBeginQueryARB v1 v2 = liftIO $ dyn16 ptr_glBeginQueryARB v1 v2

{-# NOINLINE ptr_glBeginQueryARB #-}
ptr_glBeginQueryARB :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glBeginQueryARB = unsafePerformIO $ getCommand "glBeginQueryARB"

-- glBeginQueryEXT -------------------------------------------------------------

glBeginQueryEXT
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLuint -- ^ @id@.
  -> m ()
glBeginQueryEXT v1 v2 = liftIO $ dyn16 ptr_glBeginQueryEXT v1 v2

{-# NOINLINE ptr_glBeginQueryEXT #-}
ptr_glBeginQueryEXT :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glBeginQueryEXT = unsafePerformIO $ getCommand "glBeginQueryEXT"

-- glBeginQueryIndexed ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glBeginQueryIndexed.xhtml OpenGL 4.x>.
glBeginQueryIndexed
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLuint -- ^ @index@.
  -> GLuint -- ^ @id@.
  -> m ()
glBeginQueryIndexed v1 v2 v3 = liftIO $ dyn17 ptr_glBeginQueryIndexed v1 v2 v3

{-# NOINLINE ptr_glBeginQueryIndexed #-}
ptr_glBeginQueryIndexed :: FunPtr (GLenum -> GLuint -> GLuint -> IO ())
ptr_glBeginQueryIndexed = unsafePerformIO $ getCommand "glBeginQueryIndexed"

-- glBeginTransformFeedback ----------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glBeginTransformFeedback.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glBeginTransformFeedback.xhtml OpenGL 4.x>.
glBeginTransformFeedback
  :: MonadIO m
  => GLenum -- ^ @primitiveMode@.
  -> m ()
glBeginTransformFeedback v1 = liftIO $ dyn4 ptr_glBeginTransformFeedback v1

{-# NOINLINE ptr_glBeginTransformFeedback #-}
ptr_glBeginTransformFeedback :: FunPtr (GLenum -> IO ())
ptr_glBeginTransformFeedback = unsafePerformIO $ getCommand "glBeginTransformFeedback"

