--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F02
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

module Graphics.GL.Functions.F02 (
  glBeginTransformFeedbackEXT,
  glBeginTransformFeedbackNV,
  glBeginVertexShaderEXT,
  glBeginVideoCaptureNV,
  glBindAttribLocation,
  glBindAttribLocationARB,
  glBindBuffer,
  glBindBufferARB,
  glBindBufferBase,
  glBindBufferBaseEXT,
  glBindBufferBaseNV,
  glBindBufferOffsetEXT,
  glBindBufferOffsetNV,
  glBindBufferRange,
  glBindBufferRangeEXT,
  glBindBufferRangeNV,
  glBindBuffersBase,
  glBindBuffersRange,
  glBindFragDataLocation,
  glBindFragDataLocationEXT,
  glBindFragDataLocationIndexed,
  glBindFragDataLocationIndexedEXT,
  glBindFragmentShaderATI,
  glBindFramebuffer,
  glBindFramebufferEXT,
  glBindFramebufferOES,
  glBindImageTexture,
  glBindImageTextureEXT,
  glBindImageTextures,
  glBindLightParameterEXT,
  glBindMaterialParameterEXT,
  glBindMultiTextureEXT,
  glBindParameterEXT,
  glBindProgramARB,
  glBindProgramNV,
  glBindProgramPipeline,
  glBindProgramPipelineEXT,
  glBindRenderbuffer,
  glBindRenderbufferEXT,
  glBindRenderbufferOES
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

-- glBeginTransformFeedbackEXT -------------------------------------------------

-- | This command is an alias for 'glBeginTransformFeedback'.
glBeginTransformFeedbackEXT
  :: MonadIO m
  => GLenum -- ^ @primitiveMode@.
  -> m ()
glBeginTransformFeedbackEXT v1 = liftIO $ dyn4 ptr_glBeginTransformFeedbackEXT v1

{-# NOINLINE ptr_glBeginTransformFeedbackEXT #-}
ptr_glBeginTransformFeedbackEXT :: FunPtr (GLenum -> IO ())
ptr_glBeginTransformFeedbackEXT = unsafePerformIO $ getCommand "glBeginTransformFeedbackEXT"

-- glBeginTransformFeedbackNV --------------------------------------------------

-- | This command is an alias for 'glBeginTransformFeedback'.
glBeginTransformFeedbackNV
  :: MonadIO m
  => GLenum -- ^ @primitiveMode@.
  -> m ()
glBeginTransformFeedbackNV v1 = liftIO $ dyn4 ptr_glBeginTransformFeedbackNV v1

{-# NOINLINE ptr_glBeginTransformFeedbackNV #-}
ptr_glBeginTransformFeedbackNV :: FunPtr (GLenum -> IO ())
ptr_glBeginTransformFeedbackNV = unsafePerformIO $ getCommand "glBeginTransformFeedbackNV"

-- glBeginVertexShaderEXT ------------------------------------------------------

glBeginVertexShaderEXT
  :: MonadIO m
  => m ()
glBeginVertexShaderEXT = liftIO $ dyn10 ptr_glBeginVertexShaderEXT

{-# NOINLINE ptr_glBeginVertexShaderEXT #-}
ptr_glBeginVertexShaderEXT :: FunPtr (IO ())
ptr_glBeginVertexShaderEXT = unsafePerformIO $ getCommand "glBeginVertexShaderEXT"

-- glBeginVideoCaptureNV -------------------------------------------------------

glBeginVideoCaptureNV
  :: MonadIO m
  => GLuint -- ^ @video_capture_slot@.
  -> m ()
glBeginVideoCaptureNV v1 = liftIO $ dyn2 ptr_glBeginVideoCaptureNV v1

{-# NOINLINE ptr_glBeginVideoCaptureNV #-}
ptr_glBeginVideoCaptureNV :: FunPtr (GLuint -> IO ())
ptr_glBeginVideoCaptureNV = unsafePerformIO $ getCommand "glBeginVideoCaptureNV"

-- glBindAttribLocation --------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glBindAttribLocation.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glBindAttribLocation.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glBindAttribLocation.xhtml OpenGL 4.x>.
glBindAttribLocation
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLuint -- ^ @index@.
  -> Ptr GLchar -- ^ @name@.
  -> m ()
glBindAttribLocation v1 v2 v3 = liftIO $ dyn18 ptr_glBindAttribLocation v1 v2 v3

{-# NOINLINE ptr_glBindAttribLocation #-}
ptr_glBindAttribLocation :: FunPtr (GLuint -> GLuint -> Ptr GLchar -> IO ())
ptr_glBindAttribLocation = unsafePerformIO $ getCommand "glBindAttribLocation"

-- glBindAttribLocationARB -----------------------------------------------------

-- | This command is an alias for 'glBindAttribLocation'.
glBindAttribLocationARB
  :: MonadIO m
  => GLhandleARB -- ^ @programObj@ of type @handleARB@.
  -> GLuint -- ^ @index@.
  -> Ptr GLcharARB -- ^ @name@.
  -> m ()
glBindAttribLocationARB v1 v2 v3 = liftIO $ dyn19 ptr_glBindAttribLocationARB v1 v2 v3

{-# NOINLINE ptr_glBindAttribLocationARB #-}
ptr_glBindAttribLocationARB :: FunPtr (GLhandleARB -> GLuint -> Ptr GLcharARB -> IO ())
ptr_glBindAttribLocationARB = unsafePerformIO $ getCommand "glBindAttribLocationARB"

-- glBindBuffer ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glBindBuffer.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glBindBuffer.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glBindBuffer.xhtml OpenGL 4.x>.
glBindBuffer
  :: MonadIO m
  => GLenum -- ^ @target@ of type @BufferTargetARB@.
  -> GLuint -- ^ @buffer@.
  -> m ()
glBindBuffer v1 v2 = liftIO $ dyn16 ptr_glBindBuffer v1 v2

{-# NOINLINE ptr_glBindBuffer #-}
ptr_glBindBuffer :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glBindBuffer = unsafePerformIO $ getCommand "glBindBuffer"

-- glBindBufferARB -------------------------------------------------------------

-- | This command is an alias for 'glBindBuffer'.
glBindBufferARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @BufferTargetARB@.
  -> GLuint -- ^ @buffer@.
  -> m ()
glBindBufferARB v1 v2 = liftIO $ dyn16 ptr_glBindBufferARB v1 v2

{-# NOINLINE ptr_glBindBufferARB #-}
ptr_glBindBufferARB :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glBindBufferARB = unsafePerformIO $ getCommand "glBindBufferARB"

-- glBindBufferBase ------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glBindBufferBase.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glBindBufferBase.xhtml OpenGL 4.x>.
glBindBufferBase
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLuint -- ^ @index@.
  -> GLuint -- ^ @buffer@.
  -> m ()
glBindBufferBase v1 v2 v3 = liftIO $ dyn17 ptr_glBindBufferBase v1 v2 v3

{-# NOINLINE ptr_glBindBufferBase #-}
ptr_glBindBufferBase :: FunPtr (GLenum -> GLuint -> GLuint -> IO ())
ptr_glBindBufferBase = unsafePerformIO $ getCommand "glBindBufferBase"

-- glBindBufferBaseEXT ---------------------------------------------------------

-- | This command is an alias for 'glBindBufferBase'.
glBindBufferBaseEXT
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLuint -- ^ @index@.
  -> GLuint -- ^ @buffer@.
  -> m ()
glBindBufferBaseEXT v1 v2 v3 = liftIO $ dyn17 ptr_glBindBufferBaseEXT v1 v2 v3

{-# NOINLINE ptr_glBindBufferBaseEXT #-}
ptr_glBindBufferBaseEXT :: FunPtr (GLenum -> GLuint -> GLuint -> IO ())
ptr_glBindBufferBaseEXT = unsafePerformIO $ getCommand "glBindBufferBaseEXT"

-- glBindBufferBaseNV ----------------------------------------------------------

-- | This command is an alias for 'glBindBufferBase'.
glBindBufferBaseNV
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLuint -- ^ @index@.
  -> GLuint -- ^ @buffer@.
  -> m ()
glBindBufferBaseNV v1 v2 v3 = liftIO $ dyn17 ptr_glBindBufferBaseNV v1 v2 v3

{-# NOINLINE ptr_glBindBufferBaseNV #-}
ptr_glBindBufferBaseNV :: FunPtr (GLenum -> GLuint -> GLuint -> IO ())
ptr_glBindBufferBaseNV = unsafePerformIO $ getCommand "glBindBufferBaseNV"

-- glBindBufferOffsetEXT -------------------------------------------------------

glBindBufferOffsetEXT
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLuint -- ^ @index@.
  -> GLuint -- ^ @buffer@.
  -> GLintptr -- ^ @offset@ of type @BufferOffset@.
  -> m ()
glBindBufferOffsetEXT v1 v2 v3 v4 = liftIO $ dyn20 ptr_glBindBufferOffsetEXT v1 v2 v3 v4

{-# NOINLINE ptr_glBindBufferOffsetEXT #-}
ptr_glBindBufferOffsetEXT :: FunPtr (GLenum -> GLuint -> GLuint -> GLintptr -> IO ())
ptr_glBindBufferOffsetEXT = unsafePerformIO $ getCommand "glBindBufferOffsetEXT"

-- glBindBufferOffsetNV --------------------------------------------------------

-- | This command is an alias for 'glBindBufferOffsetEXT'.
glBindBufferOffsetNV
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLuint -- ^ @index@.
  -> GLuint -- ^ @buffer@.
  -> GLintptr -- ^ @offset@ of type @BufferOffset@.
  -> m ()
glBindBufferOffsetNV v1 v2 v3 v4 = liftIO $ dyn20 ptr_glBindBufferOffsetNV v1 v2 v3 v4

{-# NOINLINE ptr_glBindBufferOffsetNV #-}
ptr_glBindBufferOffsetNV :: FunPtr (GLenum -> GLuint -> GLuint -> GLintptr -> IO ())
ptr_glBindBufferOffsetNV = unsafePerformIO $ getCommand "glBindBufferOffsetNV"

-- glBindBufferRange -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glBindBufferRange.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glBindBufferRange.xhtml OpenGL 4.x>.
glBindBufferRange
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLuint -- ^ @index@.
  -> GLuint -- ^ @buffer@.
  -> GLintptr -- ^ @offset@ of type @BufferOffset@.
  -> GLsizeiptr -- ^ @size@ of type @BufferSize@.
  -> m ()
glBindBufferRange v1 v2 v3 v4 v5 = liftIO $ dyn21 ptr_glBindBufferRange v1 v2 v3 v4 v5

{-# NOINLINE ptr_glBindBufferRange #-}
ptr_glBindBufferRange :: FunPtr (GLenum -> GLuint -> GLuint -> GLintptr -> GLsizeiptr -> IO ())
ptr_glBindBufferRange = unsafePerformIO $ getCommand "glBindBufferRange"

-- glBindBufferRangeEXT --------------------------------------------------------

-- | This command is an alias for 'glBindBufferRange'.
glBindBufferRangeEXT
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLuint -- ^ @index@.
  -> GLuint -- ^ @buffer@.
  -> GLintptr -- ^ @offset@ of type @BufferOffset@.
  -> GLsizeiptr -- ^ @size@ of type @BufferSize@.
  -> m ()
glBindBufferRangeEXT v1 v2 v3 v4 v5 = liftIO $ dyn21 ptr_glBindBufferRangeEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glBindBufferRangeEXT #-}
ptr_glBindBufferRangeEXT :: FunPtr (GLenum -> GLuint -> GLuint -> GLintptr -> GLsizeiptr -> IO ())
ptr_glBindBufferRangeEXT = unsafePerformIO $ getCommand "glBindBufferRangeEXT"

-- glBindBufferRangeNV ---------------------------------------------------------

-- | This command is an alias for 'glBindBufferRange'.
glBindBufferRangeNV
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLuint -- ^ @index@.
  -> GLuint -- ^ @buffer@.
  -> GLintptr -- ^ @offset@ of type @BufferOffset@.
  -> GLsizeiptr -- ^ @size@ of type @BufferSize@.
  -> m ()
glBindBufferRangeNV v1 v2 v3 v4 v5 = liftIO $ dyn21 ptr_glBindBufferRangeNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glBindBufferRangeNV #-}
ptr_glBindBufferRangeNV :: FunPtr (GLenum -> GLuint -> GLuint -> GLintptr -> GLsizeiptr -> IO ())
ptr_glBindBufferRangeNV = unsafePerformIO $ getCommand "glBindBufferRangeNV"

-- glBindBuffersBase -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glBindBuffersBase.xhtml OpenGL 4.x>.
glBindBuffersBase
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLuint -- ^ @first@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @buffers@ pointing to @count@ elements of type @GLuint@.
  -> m ()
glBindBuffersBase v1 v2 v3 v4 = liftIO $ dyn22 ptr_glBindBuffersBase v1 v2 v3 v4

{-# NOINLINE ptr_glBindBuffersBase #-}
ptr_glBindBuffersBase :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLuint -> IO ())
ptr_glBindBuffersBase = unsafePerformIO $ getCommand "glBindBuffersBase"

-- glBindBuffersRange ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glBindBuffersRange.xhtml OpenGL 4.x>.
glBindBuffersRange
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLuint -- ^ @first@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @buffers@ pointing to @count@ elements of type @GLuint@.
  -> Ptr GLintptr -- ^ @offsets@ pointing to @count@ elements of type @GLintptr@.
  -> Ptr GLsizeiptr -- ^ @sizes@ pointing to @count@ elements of type @GLsizeiptr@.
  -> m ()
glBindBuffersRange v1 v2 v3 v4 v5 v6 = liftIO $ dyn23 ptr_glBindBuffersRange v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glBindBuffersRange #-}
ptr_glBindBuffersRange :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLuint -> Ptr GLintptr -> Ptr GLsizeiptr -> IO ())
ptr_glBindBuffersRange = unsafePerformIO $ getCommand "glBindBuffersRange"

-- glBindFragDataLocation ------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glBindFragDataLocation.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glBindFragDataLocation.xhtml OpenGL 4.x>.
glBindFragDataLocation
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLuint -- ^ @color@.
  -> Ptr GLchar -- ^ @name@ pointing to @COMPSIZE(name)@ elements of type @GLchar@.
  -> m ()
glBindFragDataLocation v1 v2 v3 = liftIO $ dyn18 ptr_glBindFragDataLocation v1 v2 v3

{-# NOINLINE ptr_glBindFragDataLocation #-}
ptr_glBindFragDataLocation :: FunPtr (GLuint -> GLuint -> Ptr GLchar -> IO ())
ptr_glBindFragDataLocation = unsafePerformIO $ getCommand "glBindFragDataLocation"

-- glBindFragDataLocationEXT ---------------------------------------------------

-- | This command is an alias for 'glBindFragDataLocation'.
glBindFragDataLocationEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLuint -- ^ @color@.
  -> Ptr GLchar -- ^ @name@ pointing to @COMPSIZE(name)@ elements of type @GLchar@.
  -> m ()
glBindFragDataLocationEXT v1 v2 v3 = liftIO $ dyn18 ptr_glBindFragDataLocationEXT v1 v2 v3

{-# NOINLINE ptr_glBindFragDataLocationEXT #-}
ptr_glBindFragDataLocationEXT :: FunPtr (GLuint -> GLuint -> Ptr GLchar -> IO ())
ptr_glBindFragDataLocationEXT = unsafePerformIO $ getCommand "glBindFragDataLocationEXT"

-- glBindFragDataLocationIndexed -----------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glBindFragDataLocationIndexed.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glBindFragDataLocationIndexed.xhtml OpenGL 4.x>.
glBindFragDataLocationIndexed
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLuint -- ^ @colorNumber@.
  -> GLuint -- ^ @index@.
  -> Ptr GLchar -- ^ @name@.
  -> m ()
glBindFragDataLocationIndexed v1 v2 v3 v4 = liftIO $ dyn24 ptr_glBindFragDataLocationIndexed v1 v2 v3 v4

{-# NOINLINE ptr_glBindFragDataLocationIndexed #-}
ptr_glBindFragDataLocationIndexed :: FunPtr (GLuint -> GLuint -> GLuint -> Ptr GLchar -> IO ())
ptr_glBindFragDataLocationIndexed = unsafePerformIO $ getCommand "glBindFragDataLocationIndexed"

-- glBindFragDataLocationIndexedEXT --------------------------------------------

-- | This command is an alias for 'glBindFragDataLocationIndexed'.
glBindFragDataLocationIndexedEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLuint -- ^ @colorNumber@.
  -> GLuint -- ^ @index@.
  -> Ptr GLchar -- ^ @name@.
  -> m ()
glBindFragDataLocationIndexedEXT v1 v2 v3 v4 = liftIO $ dyn24 ptr_glBindFragDataLocationIndexedEXT v1 v2 v3 v4

{-# NOINLINE ptr_glBindFragDataLocationIndexedEXT #-}
ptr_glBindFragDataLocationIndexedEXT :: FunPtr (GLuint -> GLuint -> GLuint -> Ptr GLchar -> IO ())
ptr_glBindFragDataLocationIndexedEXT = unsafePerformIO $ getCommand "glBindFragDataLocationIndexedEXT"

-- glBindFragmentShaderATI -----------------------------------------------------

glBindFragmentShaderATI
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> m ()
glBindFragmentShaderATI v1 = liftIO $ dyn2 ptr_glBindFragmentShaderATI v1

{-# NOINLINE ptr_glBindFragmentShaderATI #-}
ptr_glBindFragmentShaderATI :: FunPtr (GLuint -> IO ())
ptr_glBindFragmentShaderATI = unsafePerformIO $ getCommand "glBindFragmentShaderATI"

-- glBindFramebuffer -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glBindFramebuffer.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glBindFramebuffer.xhtml OpenGL 4.x>.
glBindFramebuffer
  :: MonadIO m
  => GLenum -- ^ @target@ of type @FramebufferTarget@.
  -> GLuint -- ^ @framebuffer@.
  -> m ()
glBindFramebuffer v1 v2 = liftIO $ dyn16 ptr_glBindFramebuffer v1 v2

{-# NOINLINE ptr_glBindFramebuffer #-}
ptr_glBindFramebuffer :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glBindFramebuffer = unsafePerformIO $ getCommand "glBindFramebuffer"

-- glBindFramebufferEXT --------------------------------------------------------

glBindFramebufferEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type @FramebufferTarget@.
  -> GLuint -- ^ @framebuffer@.
  -> m ()
glBindFramebufferEXT v1 v2 = liftIO $ dyn16 ptr_glBindFramebufferEXT v1 v2

{-# NOINLINE ptr_glBindFramebufferEXT #-}
ptr_glBindFramebufferEXT :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glBindFramebufferEXT = unsafePerformIO $ getCommand "glBindFramebufferEXT"

-- glBindFramebufferOES --------------------------------------------------------

glBindFramebufferOES
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLuint -- ^ @framebuffer@.
  -> m ()
glBindFramebufferOES v1 v2 = liftIO $ dyn16 ptr_glBindFramebufferOES v1 v2

{-# NOINLINE ptr_glBindFramebufferOES #-}
ptr_glBindFramebufferOES :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glBindFramebufferOES = unsafePerformIO $ getCommand "glBindFramebufferOES"

-- glBindImageTexture ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glBindImageTexture.xhtml OpenGL 4.x>.
glBindImageTexture
  :: MonadIO m
  => GLuint -- ^ @unit@.
  -> GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLboolean -- ^ @layered@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLint -- ^ @layer@.
  -> GLenum -- ^ @access@.
  -> GLenum -- ^ @format@.
  -> m ()
glBindImageTexture v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn25 ptr_glBindImageTexture v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glBindImageTexture #-}
ptr_glBindImageTexture :: FunPtr (GLuint -> GLuint -> GLint -> GLboolean -> GLint -> GLenum -> GLenum -> IO ())
ptr_glBindImageTexture = unsafePerformIO $ getCommand "glBindImageTexture"

-- glBindImageTextureEXT -------------------------------------------------------

glBindImageTextureEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLboolean -- ^ @layered@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLint -- ^ @layer@.
  -> GLenum -- ^ @access@.
  -> GLint -- ^ @format@.
  -> m ()
glBindImageTextureEXT v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn26 ptr_glBindImageTextureEXT v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glBindImageTextureEXT #-}
ptr_glBindImageTextureEXT :: FunPtr (GLuint -> GLuint -> GLint -> GLboolean -> GLint -> GLenum -> GLint -> IO ())
ptr_glBindImageTextureEXT = unsafePerformIO $ getCommand "glBindImageTextureEXT"

-- glBindImageTextures ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glBindImageTextures.xhtml OpenGL 4.x>.
glBindImageTextures
  :: MonadIO m
  => GLuint -- ^ @first@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @textures@ pointing to @count@ elements of type @GLuint@.
  -> m ()
glBindImageTextures v1 v2 v3 = liftIO $ dyn27 ptr_glBindImageTextures v1 v2 v3

{-# NOINLINE ptr_glBindImageTextures #-}
ptr_glBindImageTextures :: FunPtr (GLuint -> GLsizei -> Ptr GLuint -> IO ())
ptr_glBindImageTextures = unsafePerformIO $ getCommand "glBindImageTextures"

-- glBindLightParameterEXT -----------------------------------------------------

glBindLightParameterEXT
  :: MonadIO m
  => GLenum -- ^ @light@ of type [LightName](Graphics-GL-Groups.html#LightName).
  -> GLenum -- ^ @value@ of type [LightParameter](Graphics-GL-Groups.html#LightParameter).
  -> m GLuint
glBindLightParameterEXT v1 v2 = liftIO $ dyn28 ptr_glBindLightParameterEXT v1 v2

{-# NOINLINE ptr_glBindLightParameterEXT #-}
ptr_glBindLightParameterEXT :: FunPtr (GLenum -> GLenum -> IO GLuint)
ptr_glBindLightParameterEXT = unsafePerformIO $ getCommand "glBindLightParameterEXT"

-- glBindMaterialParameterEXT --------------------------------------------------

glBindMaterialParameterEXT
  :: MonadIO m
  => GLenum -- ^ @face@ of type [MaterialFace](Graphics-GL-Groups.html#MaterialFace).
  -> GLenum -- ^ @value@ of type [MaterialParameter](Graphics-GL-Groups.html#MaterialParameter).
  -> m GLuint
glBindMaterialParameterEXT v1 v2 = liftIO $ dyn28 ptr_glBindMaterialParameterEXT v1 v2

{-# NOINLINE ptr_glBindMaterialParameterEXT #-}
ptr_glBindMaterialParameterEXT :: FunPtr (GLenum -> GLenum -> IO GLuint)
ptr_glBindMaterialParameterEXT = unsafePerformIO $ getCommand "glBindMaterialParameterEXT"

-- glBindMultiTextureEXT -------------------------------------------------------

glBindMultiTextureEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type @TextureUnit@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLuint -- ^ @texture@ of type @Texture@.
  -> m ()
glBindMultiTextureEXT v1 v2 v3 = liftIO $ dyn29 ptr_glBindMultiTextureEXT v1 v2 v3

{-# NOINLINE ptr_glBindMultiTextureEXT #-}
ptr_glBindMultiTextureEXT :: FunPtr (GLenum -> GLenum -> GLuint -> IO ())
ptr_glBindMultiTextureEXT = unsafePerformIO $ getCommand "glBindMultiTextureEXT"

-- glBindParameterEXT ----------------------------------------------------------

glBindParameterEXT
  :: MonadIO m
  => GLenum -- ^ @value@ of type @VertexShaderParameterEXT@.
  -> m GLuint
glBindParameterEXT v1 = liftIO $ dyn30 ptr_glBindParameterEXT v1

{-# NOINLINE ptr_glBindParameterEXT #-}
ptr_glBindParameterEXT :: FunPtr (GLenum -> IO GLuint)
ptr_glBindParameterEXT = unsafePerformIO $ getCommand "glBindParameterEXT"

-- glBindProgramARB ------------------------------------------------------------

glBindProgramARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ProgramTargetARB@.
  -> GLuint -- ^ @program@.
  -> m ()
glBindProgramARB v1 v2 = liftIO $ dyn16 ptr_glBindProgramARB v1 v2

{-# NOINLINE ptr_glBindProgramARB #-}
ptr_glBindProgramARB :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glBindProgramARB = unsafePerformIO $ getCommand "glBindProgramARB"

-- glBindProgramNV -------------------------------------------------------------

-- | This command is an alias for 'glBindProgramARB'.
glBindProgramNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type @VertexAttribEnumNV@.
  -> GLuint -- ^ @id@.
  -> m ()
glBindProgramNV v1 v2 = liftIO $ dyn16 ptr_glBindProgramNV v1 v2

{-# NOINLINE ptr_glBindProgramNV #-}
ptr_glBindProgramNV :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glBindProgramNV = unsafePerformIO $ getCommand "glBindProgramNV"

-- glBindProgramPipeline -------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glBindProgramPipeline.xhtml OpenGL 4.x>.
glBindProgramPipeline
  :: MonadIO m
  => GLuint -- ^ @pipeline@.
  -> m ()
glBindProgramPipeline v1 = liftIO $ dyn2 ptr_glBindProgramPipeline v1

{-# NOINLINE ptr_glBindProgramPipeline #-}
ptr_glBindProgramPipeline :: FunPtr (GLuint -> IO ())
ptr_glBindProgramPipeline = unsafePerformIO $ getCommand "glBindProgramPipeline"

-- glBindProgramPipelineEXT ----------------------------------------------------

glBindProgramPipelineEXT
  :: MonadIO m
  => GLuint -- ^ @pipeline@.
  -> m ()
glBindProgramPipelineEXT v1 = liftIO $ dyn2 ptr_glBindProgramPipelineEXT v1

{-# NOINLINE ptr_glBindProgramPipelineEXT #-}
ptr_glBindProgramPipelineEXT :: FunPtr (GLuint -> IO ())
ptr_glBindProgramPipelineEXT = unsafePerformIO $ getCommand "glBindProgramPipelineEXT"

-- glBindRenderbuffer ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glBindRenderbuffer.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glBindRenderbuffer.xhtml OpenGL 4.x>.
glBindRenderbuffer
  :: MonadIO m
  => GLenum -- ^ @target@ of type @RenderbufferTarget@.
  -> GLuint -- ^ @renderbuffer@.
  -> m ()
glBindRenderbuffer v1 v2 = liftIO $ dyn16 ptr_glBindRenderbuffer v1 v2

{-# NOINLINE ptr_glBindRenderbuffer #-}
ptr_glBindRenderbuffer :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glBindRenderbuffer = unsafePerformIO $ getCommand "glBindRenderbuffer"

-- glBindRenderbufferEXT -------------------------------------------------------

glBindRenderbufferEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type @RenderbufferTarget@.
  -> GLuint -- ^ @renderbuffer@.
  -> m ()
glBindRenderbufferEXT v1 v2 = liftIO $ dyn16 ptr_glBindRenderbufferEXT v1 v2

{-# NOINLINE ptr_glBindRenderbufferEXT #-}
ptr_glBindRenderbufferEXT :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glBindRenderbufferEXT = unsafePerformIO $ getCommand "glBindRenderbufferEXT"

-- glBindRenderbufferOES -------------------------------------------------------

glBindRenderbufferOES
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLuint -- ^ @renderbuffer@.
  -> m ()
glBindRenderbufferOES v1 v2 = liftIO $ dyn16 ptr_glBindRenderbufferOES v1 v2

{-# NOINLINE ptr_glBindRenderbufferOES #-}
ptr_glBindRenderbufferOES :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glBindRenderbufferOES = unsafePerformIO $ getCommand "glBindRenderbufferOES"

