--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F30
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

module Graphics.GL.Functions.F30 (
  glGetTexParameterxv,
  glGetTexParameterxvOES,
  glGetTextureHandleARB,
  glGetTextureHandleNV,
  glGetTextureImage,
  glGetTextureImageEXT,
  glGetTextureLevelParameterfv,
  glGetTextureLevelParameterfvEXT,
  glGetTextureLevelParameteriv,
  glGetTextureLevelParameterivEXT,
  glGetTextureParameterIiv,
  glGetTextureParameterIivEXT,
  glGetTextureParameterIuiv,
  glGetTextureParameterIuivEXT,
  glGetTextureParameterfv,
  glGetTextureParameterfvEXT,
  glGetTextureParameteriv,
  glGetTextureParameterivEXT,
  glGetTextureSamplerHandleARB,
  glGetTextureSamplerHandleNV,
  glGetTextureSubImage,
  glGetTrackMatrixivNV,
  glGetTransformFeedbackVarying,
  glGetTransformFeedbackVaryingEXT,
  glGetTransformFeedbackVaryingNV,
  glGetTransformFeedbacki64_v,
  glGetTransformFeedbacki_v,
  glGetTransformFeedbackiv,
  glGetTranslatedShaderSourceANGLE,
  glGetUniformBlockIndex,
  glGetUniformBufferSizeEXT,
  glGetUniformIndices,
  glGetUniformLocation,
  glGetUniformLocationARB,
  glGetUniformOffsetEXT,
  glGetUniformSubroutineuiv,
  glGetUniformdv,
  glGetUniformfv,
  glGetUniformfvARB,
  glGetUniformi64vARB
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

-- glGetTexParameterxv ---------------------------------------------------------

glGetTexParameterxv
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLfixed -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfixed@.
  -> m ()
glGetTexParameterxv v1 v2 v3 = liftIO $ dyn163 ptr_glGetTexParameterxv v1 v2 v3

{-# NOINLINE ptr_glGetTexParameterxv #-}
ptr_glGetTexParameterxv :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
ptr_glGetTexParameterxv = unsafePerformIO $ getCommand "glGetTexParameterxv"

-- glGetTexParameterxvOES ------------------------------------------------------

glGetTexParameterxvOES
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLfixed -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfixed@.
  -> m ()
glGetTexParameterxvOES v1 v2 v3 = liftIO $ dyn163 ptr_glGetTexParameterxvOES v1 v2 v3

{-# NOINLINE ptr_glGetTexParameterxvOES #-}
ptr_glGetTexParameterxvOES :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
ptr_glGetTexParameterxvOES = unsafePerformIO $ getCommand "glGetTexParameterxvOES"

-- glGetTextureHandleARB -------------------------------------------------------

glGetTextureHandleARB
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> m GLuint64
glGetTextureHandleARB v1 = liftIO $ dyn414 ptr_glGetTextureHandleARB v1

{-# NOINLINE ptr_glGetTextureHandleARB #-}
ptr_glGetTextureHandleARB :: FunPtr (GLuint -> IO GLuint64)
ptr_glGetTextureHandleARB = unsafePerformIO $ getCommand "glGetTextureHandleARB"

-- glGetTextureHandleNV --------------------------------------------------------

glGetTextureHandleNV
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> m GLuint64
glGetTextureHandleNV v1 = liftIO $ dyn414 ptr_glGetTextureHandleNV v1

{-# NOINLINE ptr_glGetTextureHandleNV #-}
ptr_glGetTextureHandleNV :: FunPtr (GLuint -> IO GLuint64)
ptr_glGetTextureHandleNV = unsafePerformIO $ getCommand "glGetTextureHandleNV"

-- glGetTextureImage -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetTexImage.xhtml OpenGL 4.x>.
glGetTextureImage
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLenum -- ^ @format@.
  -> GLenum -- ^ @type@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr a -- ^ @pixels@.
  -> m ()
glGetTextureImage v1 v2 v3 v4 v5 v6 = liftIO $ dyn415 ptr_glGetTextureImage v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glGetTextureImage #-}
ptr_glGetTextureImage :: FunPtr (GLuint -> GLint -> GLenum -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glGetTextureImage = unsafePerformIO $ getCommand "glGetTextureImage"

-- glGetTextureImageEXT --------------------------------------------------------

glGetTextureImageEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @pixels@ pointing to @COMPSIZE(target,level,format,type)@ elements of type @a@.
  -> m ()
glGetTextureImageEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn416 ptr_glGetTextureImageEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glGetTextureImageEXT #-}
ptr_glGetTextureImageEXT :: FunPtr (GLuint -> GLenum -> GLint -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glGetTextureImageEXT = unsafePerformIO $ getCommand "glGetTextureImageEXT"

-- glGetTextureLevelParameterfv ------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetTexLevelParameter.xhtml OpenGL 4.x>.
glGetTextureLevelParameterfv
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLfloat -- ^ @params@.
  -> m ()
glGetTextureLevelParameterfv v1 v2 v3 v4 = liftIO $ dyn417 ptr_glGetTextureLevelParameterfv v1 v2 v3 v4

{-# NOINLINE ptr_glGetTextureLevelParameterfv #-}
ptr_glGetTextureLevelParameterfv :: FunPtr (GLuint -> GLint -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetTextureLevelParameterfv = unsafePerformIO $ getCommand "glGetTextureLevelParameterfv"

-- glGetTextureLevelParameterfvEXT ---------------------------------------------

glGetTextureLevelParameterfvEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetTextureLevelParameterfvEXT v1 v2 v3 v4 v5 = liftIO $ dyn418 ptr_glGetTextureLevelParameterfvEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetTextureLevelParameterfvEXT #-}
ptr_glGetTextureLevelParameterfvEXT :: FunPtr (GLuint -> GLenum -> GLint -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetTextureLevelParameterfvEXT = unsafePerformIO $ getCommand "glGetTextureLevelParameterfvEXT"

-- glGetTextureLevelParameteriv ------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetTexLevelParameter.xhtml OpenGL 4.x>.
glGetTextureLevelParameteriv
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @params@.
  -> m ()
glGetTextureLevelParameteriv v1 v2 v3 v4 = liftIO $ dyn419 ptr_glGetTextureLevelParameteriv v1 v2 v3 v4

{-# NOINLINE ptr_glGetTextureLevelParameteriv #-}
ptr_glGetTextureLevelParameteriv :: FunPtr (GLuint -> GLint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetTextureLevelParameteriv = unsafePerformIO $ getCommand "glGetTextureLevelParameteriv"

-- glGetTextureLevelParameterivEXT ---------------------------------------------

glGetTextureLevelParameterivEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetTextureLevelParameterivEXT v1 v2 v3 v4 v5 = liftIO $ dyn271 ptr_glGetTextureLevelParameterivEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetTextureLevelParameterivEXT #-}
ptr_glGetTextureLevelParameterivEXT :: FunPtr (GLuint -> GLenum -> GLint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetTextureLevelParameterivEXT = unsafePerformIO $ getCommand "glGetTextureLevelParameterivEXT"

-- glGetTextureParameterIiv ----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetTexParameter.xhtml OpenGL 4.x>.
glGetTextureParameterIiv
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @params@.
  -> m ()
glGetTextureParameterIiv v1 v2 v3 = liftIO $ dyn334 ptr_glGetTextureParameterIiv v1 v2 v3

{-# NOINLINE ptr_glGetTextureParameterIiv #-}
ptr_glGetTextureParameterIiv :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetTextureParameterIiv = unsafePerformIO $ getCommand "glGetTextureParameterIiv"

-- glGetTextureParameterIivEXT -------------------------------------------------

glGetTextureParameterIivEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetTextureParameterIivEXT v1 v2 v3 v4 = liftIO $ dyn363 ptr_glGetTextureParameterIivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetTextureParameterIivEXT #-}
ptr_glGetTextureParameterIivEXT :: FunPtr (GLuint -> GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetTextureParameterIivEXT = unsafePerformIO $ getCommand "glGetTextureParameterIivEXT"

-- glGetTextureParameterIuiv ---------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetTexParameter.xhtml OpenGL 4.x>.
glGetTextureParameterIuiv
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLuint -- ^ @params@.
  -> m ()
glGetTextureParameterIuiv v1 v2 v3 = liftIO $ dyn375 ptr_glGetTextureParameterIuiv v1 v2 v3

{-# NOINLINE ptr_glGetTextureParameterIuiv #-}
ptr_glGetTextureParameterIuiv :: FunPtr (GLuint -> GLenum -> Ptr GLuint -> IO ())
ptr_glGetTextureParameterIuiv = unsafePerformIO $ getCommand "glGetTextureParameterIuiv"

-- glGetTextureParameterIuivEXT ------------------------------------------------

glGetTextureParameterIuivEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> Ptr GLuint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLuint@.
  -> m ()
glGetTextureParameterIuivEXT v1 v2 v3 v4 = liftIO $ dyn420 ptr_glGetTextureParameterIuivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetTextureParameterIuivEXT #-}
ptr_glGetTextureParameterIuivEXT :: FunPtr (GLuint -> GLenum -> GLenum -> Ptr GLuint -> IO ())
ptr_glGetTextureParameterIuivEXT = unsafePerformIO $ getCommand "glGetTextureParameterIuivEXT"

-- glGetTextureParameterfv -----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetTexParameter.xhtml OpenGL 4.x>.
glGetTextureParameterfv
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLfloat -- ^ @params@.
  -> m ()
glGetTextureParameterfv v1 v2 v3 = liftIO $ dyn349 ptr_glGetTextureParameterfv v1 v2 v3

{-# NOINLINE ptr_glGetTextureParameterfv #-}
ptr_glGetTextureParameterfv :: FunPtr (GLuint -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetTextureParameterfv = unsafePerformIO $ getCommand "glGetTextureParameterfv"

-- glGetTextureParameterfvEXT --------------------------------------------------

glGetTextureParameterfvEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetTextureParameterfvEXT v1 v2 v3 v4 = liftIO $ dyn421 ptr_glGetTextureParameterfvEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetTextureParameterfvEXT #-}
ptr_glGetTextureParameterfvEXT :: FunPtr (GLuint -> GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetTextureParameterfvEXT = unsafePerformIO $ getCommand "glGetTextureParameterfvEXT"

-- glGetTextureParameteriv -----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetTexParameter.xhtml OpenGL 4.x>.
glGetTextureParameteriv
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @params@.
  -> m ()
glGetTextureParameteriv v1 v2 v3 = liftIO $ dyn334 ptr_glGetTextureParameteriv v1 v2 v3

{-# NOINLINE ptr_glGetTextureParameteriv #-}
ptr_glGetTextureParameteriv :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetTextureParameteriv = unsafePerformIO $ getCommand "glGetTextureParameteriv"

-- glGetTextureParameterivEXT --------------------------------------------------

glGetTextureParameterivEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetTextureParameterivEXT v1 v2 v3 v4 = liftIO $ dyn363 ptr_glGetTextureParameterivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetTextureParameterivEXT #-}
ptr_glGetTextureParameterivEXT :: FunPtr (GLuint -> GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetTextureParameterivEXT = unsafePerformIO $ getCommand "glGetTextureParameterivEXT"

-- glGetTextureSamplerHandleARB ------------------------------------------------

glGetTextureSamplerHandleARB
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLuint -- ^ @sampler@.
  -> m GLuint64
glGetTextureSamplerHandleARB v1 v2 = liftIO $ dyn422 ptr_glGetTextureSamplerHandleARB v1 v2

{-# NOINLINE ptr_glGetTextureSamplerHandleARB #-}
ptr_glGetTextureSamplerHandleARB :: FunPtr (GLuint -> GLuint -> IO GLuint64)
ptr_glGetTextureSamplerHandleARB = unsafePerformIO $ getCommand "glGetTextureSamplerHandleARB"

-- glGetTextureSamplerHandleNV -------------------------------------------------

glGetTextureSamplerHandleNV
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLuint -- ^ @sampler@.
  -> m GLuint64
glGetTextureSamplerHandleNV v1 v2 = liftIO $ dyn422 ptr_glGetTextureSamplerHandleNV v1 v2

{-# NOINLINE ptr_glGetTextureSamplerHandleNV #-}
ptr_glGetTextureSamplerHandleNV :: FunPtr (GLuint -> GLuint -> IO GLuint64)
ptr_glGetTextureSamplerHandleNV = unsafePerformIO $ getCommand "glGetTextureSamplerHandleNV"

-- glGetTextureSubImage --------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetTextureSubImage.xhtml OpenGL 4.x>.
glGetTextureSubImage
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLint -- ^ @xoffset@.
  -> GLint -- ^ @yoffset@.
  -> GLint -- ^ @zoffset@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLenum -- ^ @format@.
  -> GLenum -- ^ @type@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr a -- ^ @pixels@.
  -> m ()
glGetTextureSubImage v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 = liftIO $ dyn423 ptr_glGetTextureSubImage v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12

{-# NOINLINE ptr_glGetTextureSubImage #-}
ptr_glGetTextureSubImage :: FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glGetTextureSubImage = unsafePerformIO $ getCommand "glGetTextureSubImage"

-- glGetTrackMatrixivNV --------------------------------------------------------

glGetTrackMatrixivNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type @VertexAttribEnumNV@.
  -> GLuint -- ^ @address@.
  -> GLenum -- ^ @pname@ of type @VertexAttribEnumNV@.
  -> Ptr GLint -- ^ @params@ pointing to @1@ element of type @GLint@.
  -> m ()
glGetTrackMatrixivNV v1 v2 v3 v4 = liftIO $ dyn351 ptr_glGetTrackMatrixivNV v1 v2 v3 v4

{-# NOINLINE ptr_glGetTrackMatrixivNV #-}
ptr_glGetTrackMatrixivNV :: FunPtr (GLenum -> GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetTrackMatrixivNV = unsafePerformIO $ getCommand "glGetTrackMatrixivNV"

-- glGetTransformFeedbackVarying -----------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGetTransformFeedbackVarying.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetTransformFeedbackVarying.xhtml OpenGL 4.x>.
glGetTransformFeedbackVarying
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLuint -- ^ @index@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLsizei -- ^ @size@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLenum -- ^ @type@ pointing to @1@ element of type @GLenum@.
  -> Ptr GLchar -- ^ @name@ pointing to @bufSize@ elements of type @GLchar@.
  -> m ()
glGetTransformFeedbackVarying v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn307 ptr_glGetTransformFeedbackVarying v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glGetTransformFeedbackVarying #-}
ptr_glGetTransformFeedbackVarying :: FunPtr (GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLsizei -> Ptr GLenum -> Ptr GLchar -> IO ())
ptr_glGetTransformFeedbackVarying = unsafePerformIO $ getCommand "glGetTransformFeedbackVarying"

-- glGetTransformFeedbackVaryingEXT --------------------------------------------

-- | This command is an alias for 'glGetTransformFeedbackVarying'.
glGetTransformFeedbackVaryingEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLuint -- ^ @index@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLsizei -- ^ @size@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLenum -- ^ @type@ pointing to @1@ element of type @GLenum@.
  -> Ptr GLchar -- ^ @name@ pointing to @bufSize@ elements of type @GLchar@.
  -> m ()
glGetTransformFeedbackVaryingEXT v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn307 ptr_glGetTransformFeedbackVaryingEXT v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glGetTransformFeedbackVaryingEXT #-}
ptr_glGetTransformFeedbackVaryingEXT :: FunPtr (GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLsizei -> Ptr GLenum -> Ptr GLchar -> IO ())
ptr_glGetTransformFeedbackVaryingEXT = unsafePerformIO $ getCommand "glGetTransformFeedbackVaryingEXT"

-- glGetTransformFeedbackVaryingNV ---------------------------------------------

glGetTransformFeedbackVaryingNV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLuint -- ^ @index@.
  -> Ptr GLint -- ^ @location@ pointing to @1@ element of type @GLint@.
  -> m ()
glGetTransformFeedbackVaryingNV v1 v2 v3 = liftIO $ dyn424 ptr_glGetTransformFeedbackVaryingNV v1 v2 v3

{-# NOINLINE ptr_glGetTransformFeedbackVaryingNV #-}
ptr_glGetTransformFeedbackVaryingNV :: FunPtr (GLuint -> GLuint -> Ptr GLint -> IO ())
ptr_glGetTransformFeedbackVaryingNV = unsafePerformIO $ getCommand "glGetTransformFeedbackVaryingNV"

-- glGetTransformFeedbacki64_v -------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetTransformFeedback.xhtml OpenGL 4.x>.
glGetTransformFeedbacki64_v
  :: MonadIO m
  => GLuint -- ^ @xfb@.
  -> GLenum -- ^ @pname@.
  -> GLuint -- ^ @index@.
  -> Ptr GLint64 -- ^ @param@.
  -> m ()
glGetTransformFeedbacki64_v v1 v2 v3 v4 = liftIO $ dyn425 ptr_glGetTransformFeedbacki64_v v1 v2 v3 v4

{-# NOINLINE ptr_glGetTransformFeedbacki64_v #-}
ptr_glGetTransformFeedbacki64_v :: FunPtr (GLuint -> GLenum -> GLuint -> Ptr GLint64 -> IO ())
ptr_glGetTransformFeedbacki64_v = unsafePerformIO $ getCommand "glGetTransformFeedbacki64_v"

-- glGetTransformFeedbacki_v ---------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetTransformFeedback.xhtml OpenGL 4.x>.
glGetTransformFeedbacki_v
  :: MonadIO m
  => GLuint -- ^ @xfb@.
  -> GLenum -- ^ @pname@.
  -> GLuint -- ^ @index@.
  -> Ptr GLint -- ^ @param@.
  -> m ()
glGetTransformFeedbacki_v v1 v2 v3 v4 = liftIO $ dyn364 ptr_glGetTransformFeedbacki_v v1 v2 v3 v4

{-# NOINLINE ptr_glGetTransformFeedbacki_v #-}
ptr_glGetTransformFeedbacki_v :: FunPtr (GLuint -> GLenum -> GLuint -> Ptr GLint -> IO ())
ptr_glGetTransformFeedbacki_v = unsafePerformIO $ getCommand "glGetTransformFeedbacki_v"

-- glGetTransformFeedbackiv ----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetTransformFeedback.xhtml OpenGL 4.x>.
glGetTransformFeedbackiv
  :: MonadIO m
  => GLuint -- ^ @xfb@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @param@.
  -> m ()
glGetTransformFeedbackiv v1 v2 v3 = liftIO $ dyn334 ptr_glGetTransformFeedbackiv v1 v2 v3

{-# NOINLINE ptr_glGetTransformFeedbackiv #-}
ptr_glGetTransformFeedbackiv :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetTransformFeedbackiv = unsafePerformIO $ getCommand "glGetTransformFeedbackiv"

-- glGetTranslatedShaderSourceANGLE --------------------------------------------

glGetTranslatedShaderSourceANGLE
  :: MonadIO m
  => GLuint -- ^ @shader@.
  -> GLsizei -- ^ @bufsize@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLchar -- ^ @source@.
  -> m ()
glGetTranslatedShaderSourceANGLE v1 v2 v3 v4 = liftIO $ dyn331 ptr_glGetTranslatedShaderSourceANGLE v1 v2 v3 v4

{-# NOINLINE ptr_glGetTranslatedShaderSourceANGLE #-}
ptr_glGetTranslatedShaderSourceANGLE :: FunPtr (GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
ptr_glGetTranslatedShaderSourceANGLE = unsafePerformIO $ getCommand "glGetTranslatedShaderSourceANGLE"

-- glGetUniformBlockIndex ------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGetUniformBlockIndex.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetUniformBlockIndex.xhtml OpenGL 4.x>.
glGetUniformBlockIndex
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> Ptr GLchar -- ^ @uniformBlockName@ pointing to @COMPSIZE()@ elements of type @GLchar@.
  -> m GLuint
glGetUniformBlockIndex v1 v2 = liftIO $ dyn426 ptr_glGetUniformBlockIndex v1 v2

{-# NOINLINE ptr_glGetUniformBlockIndex #-}
ptr_glGetUniformBlockIndex :: FunPtr (GLuint -> Ptr GLchar -> IO GLuint)
ptr_glGetUniformBlockIndex = unsafePerformIO $ getCommand "glGetUniformBlockIndex"

-- glGetUniformBufferSizeEXT ---------------------------------------------------

glGetUniformBufferSizeEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> m GLint
glGetUniformBufferSizeEXT v1 v2 = liftIO $ dyn427 ptr_glGetUniformBufferSizeEXT v1 v2

{-# NOINLINE ptr_glGetUniformBufferSizeEXT #-}
ptr_glGetUniformBufferSizeEXT :: FunPtr (GLuint -> GLint -> IO GLint)
ptr_glGetUniformBufferSizeEXT = unsafePerformIO $ getCommand "glGetUniformBufferSizeEXT"

-- glGetUniformIndices ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGetUniformIndices.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetUniformIndices.xhtml OpenGL 4.x>.
glGetUniformIndices
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLsizei -- ^ @uniformCount@.
  -> Ptr (Ptr GLchar) -- ^ @uniformNames@ pointing to @COMPSIZE(uniformCount)@ elements of type @Ptr GLchar@.
  -> Ptr GLuint -- ^ @uniformIndices@ pointing to @COMPSIZE(uniformCount)@ elements of type @GLuint@.
  -> m ()
glGetUniformIndices v1 v2 v3 v4 = liftIO $ dyn428 ptr_glGetUniformIndices v1 v2 v3 v4

{-# NOINLINE ptr_glGetUniformIndices #-}
ptr_glGetUniformIndices :: FunPtr (GLuint -> GLsizei -> Ptr (Ptr GLchar) -> Ptr GLuint -> IO ())
ptr_glGetUniformIndices = unsafePerformIO $ getCommand "glGetUniformIndices"

-- glGetUniformLocation --------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetUniformLocation.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetUniformLocation.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetUniformLocation.xhtml OpenGL 4.x>.
glGetUniformLocation
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> Ptr GLchar -- ^ @name@.
  -> m GLint
glGetUniformLocation v1 v2 = liftIO $ dyn310 ptr_glGetUniformLocation v1 v2

{-# NOINLINE ptr_glGetUniformLocation #-}
ptr_glGetUniformLocation :: FunPtr (GLuint -> Ptr GLchar -> IO GLint)
ptr_glGetUniformLocation = unsafePerformIO $ getCommand "glGetUniformLocation"

-- glGetUniformLocationARB -----------------------------------------------------

-- | This command is an alias for 'glGetUniformLocation'.
glGetUniformLocationARB
  :: MonadIO m
  => GLhandleARB -- ^ @programObj@ of type @handleARB@.
  -> Ptr GLcharARB -- ^ @name@.
  -> m GLint
glGetUniformLocationARB v1 v2 = liftIO $ dyn311 ptr_glGetUniformLocationARB v1 v2

{-# NOINLINE ptr_glGetUniformLocationARB #-}
ptr_glGetUniformLocationARB :: FunPtr (GLhandleARB -> Ptr GLcharARB -> IO GLint)
ptr_glGetUniformLocationARB = unsafePerformIO $ getCommand "glGetUniformLocationARB"

-- glGetUniformOffsetEXT -------------------------------------------------------

glGetUniformOffsetEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> m GLintptr -- ^ of type @BufferOffset@.
glGetUniformOffsetEXT v1 v2 = liftIO $ dyn429 ptr_glGetUniformOffsetEXT v1 v2

{-# NOINLINE ptr_glGetUniformOffsetEXT #-}
ptr_glGetUniformOffsetEXT :: FunPtr (GLuint -> GLint -> IO GLintptr)
ptr_glGetUniformOffsetEXT = unsafePerformIO $ getCommand "glGetUniformOffsetEXT"

-- glGetUniformSubroutineuiv ---------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetUniformSubroutine.xhtml OpenGL 4.x>.
glGetUniformSubroutineuiv
  :: MonadIO m
  => GLenum -- ^ @shadertype@.
  -> GLint -- ^ @location@.
  -> Ptr GLuint -- ^ @params@ pointing to @1@ element of type @GLuint@.
  -> m ()
glGetUniformSubroutineuiv v1 v2 v3 = liftIO $ dyn75 ptr_glGetUniformSubroutineuiv v1 v2 v3

{-# NOINLINE ptr_glGetUniformSubroutineuiv #-}
ptr_glGetUniformSubroutineuiv :: FunPtr (GLenum -> GLint -> Ptr GLuint -> IO ())
ptr_glGetUniformSubroutineuiv = unsafePerformIO $ getCommand "glGetUniformSubroutineuiv"

-- glGetUniformdv --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetUniform.xhtml OpenGL 4.x>.
glGetUniformdv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> Ptr GLdouble -- ^ @params@ pointing to @COMPSIZE(program,location)@ elements of type @GLdouble@.
  -> m ()
glGetUniformdv v1 v2 v3 = liftIO $ dyn430 ptr_glGetUniformdv v1 v2 v3

{-# NOINLINE ptr_glGetUniformdv #-}
ptr_glGetUniformdv :: FunPtr (GLuint -> GLint -> Ptr GLdouble -> IO ())
ptr_glGetUniformdv = unsafePerformIO $ getCommand "glGetUniformdv"

-- glGetUniformfv --------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetUniform.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetUniform.xhtml OpenGL 4.x>.
glGetUniformfv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(program,location)@ elements of type @GLfloat@.
  -> m ()
glGetUniformfv v1 v2 v3 = liftIO $ dyn431 ptr_glGetUniformfv v1 v2 v3

{-# NOINLINE ptr_glGetUniformfv #-}
ptr_glGetUniformfv :: FunPtr (GLuint -> GLint -> Ptr GLfloat -> IO ())
ptr_glGetUniformfv = unsafePerformIO $ getCommand "glGetUniformfv"

-- glGetUniformfvARB -----------------------------------------------------------

-- | This command is an alias for 'glGetUniformfv'.
glGetUniformfvARB
  :: MonadIO m
  => GLhandleARB -- ^ @programObj@ of type @handleARB@.
  -> GLint -- ^ @location@.
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(programObj,location)@ elements of type @GLfloat@.
  -> m ()
glGetUniformfvARB v1 v2 v3 = liftIO $ dyn432 ptr_glGetUniformfvARB v1 v2 v3

{-# NOINLINE ptr_glGetUniformfvARB #-}
ptr_glGetUniformfvARB :: FunPtr (GLhandleARB -> GLint -> Ptr GLfloat -> IO ())
ptr_glGetUniformfvARB = unsafePerformIO $ getCommand "glGetUniformfvARB"

-- glGetUniformi64vARB ---------------------------------------------------------

glGetUniformi64vARB
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> Ptr GLint64 -- ^ @params@ pointing to @COMPSIZE(program,location)@ elements of type @GLint64@.
  -> m ()
glGetUniformi64vARB v1 v2 v3 = liftIO $ dyn433 ptr_glGetUniformi64vARB v1 v2 v3

{-# NOINLINE ptr_glGetUniformi64vARB #-}
ptr_glGetUniformi64vARB :: FunPtr (GLuint -> GLint -> Ptr GLint64 -> IO ())
ptr_glGetUniformi64vARB = unsafePerformIO $ getCommand "glGetUniformi64vARB"

