--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F64
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

module Graphics.GL.Functions.F64 (
  glTextureStorageSparseAMD,
  glTextureSubImage1D,
  glTextureSubImage1DEXT,
  glTextureSubImage2D,
  glTextureSubImage2DEXT,
  glTextureSubImage3D,
  glTextureSubImage3DEXT,
  glTextureView,
  glTextureViewEXT,
  glTextureViewOES,
  glTrackMatrixNV,
  glTransformFeedbackAttribsNV,
  glTransformFeedbackBufferBase,
  glTransformFeedbackBufferRange,
  glTransformFeedbackStreamAttribsNV,
  glTransformFeedbackVaryings,
  glTransformFeedbackVaryingsEXT,
  glTransformFeedbackVaryingsNV,
  glTransformPathNV,
  glTranslated,
  glTranslatef,
  glTranslatex,
  glTranslatexOES,
  glUniform1d,
  glUniform1dv,
  glUniform1f,
  glUniform1fARB,
  glUniform1fv,
  glUniform1fvARB,
  glUniform1i,
  glUniform1i64ARB,
  glUniform1i64NV,
  glUniform1i64vARB,
  glUniform1i64vNV,
  glUniform1iARB,
  glUniform1iv,
  glUniform1ivARB,
  glUniform1ui,
  glUniform1ui64ARB,
  glUniform1ui64NV
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

-- glTextureStorageSparseAMD ---------------------------------------------------

glTextureStorageSparseAMD
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLenum -- ^ @target@.
  -> GLenum -- ^ @internalFormat@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLsizei -- ^ @layers@.
  -> GLbitfield -- ^ @flags@.
  -> m ()
glTextureStorageSparseAMD v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn767 ptr_glTextureStorageSparseAMD v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glTextureStorageSparseAMD #-}
ptr_glTextureStorageSparseAMD :: FunPtr (GLuint -> GLenum -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLsizei -> GLbitfield -> IO ())
ptr_glTextureStorageSparseAMD = unsafePerformIO $ getCommand "glTextureStorageSparseAMD"

-- glTextureSubImage1D ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glTexSubImage1D.xhtml OpenGL 4.x>.
glTextureSubImage1D
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLint -- ^ @xoffset@.
  -> GLsizei -- ^ @width@.
  -> GLenum -- ^ @format@.
  -> GLenum -- ^ @type@.
  -> Ptr a -- ^ @pixels@.
  -> m ()
glTextureSubImage1D v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn768 ptr_glTextureSubImage1D v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glTextureSubImage1D #-}
ptr_glTextureSubImage1D :: FunPtr (GLuint -> GLint -> GLint -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glTextureSubImage1D = unsafePerformIO $ getCommand "glTextureSubImage1D"

-- glTextureSubImage1DEXT ------------------------------------------------------

glTextureSubImage1DEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLsizei -- ^ @width@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @pixels@ pointing to @COMPSIZE(format,type,width)@ elements of type @a@.
  -> m ()
glTextureSubImage1DEXT v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn769 ptr_glTextureSubImage1DEXT v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glTextureSubImage1DEXT #-}
ptr_glTextureSubImage1DEXT :: FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glTextureSubImage1DEXT = unsafePerformIO $ getCommand "glTextureSubImage1DEXT"

-- glTextureSubImage2D ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glTexSubImage2D.xhtml OpenGL 4.x>.
glTextureSubImage2D
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLint -- ^ @xoffset@.
  -> GLint -- ^ @yoffset@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLenum -- ^ @format@.
  -> GLenum -- ^ @type@.
  -> Ptr a -- ^ @pixels@.
  -> m ()
glTextureSubImage2D v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn770 ptr_glTextureSubImage2D v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glTextureSubImage2D #-}
ptr_glTextureSubImage2D :: FunPtr (GLuint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glTextureSubImage2D = unsafePerformIO $ getCommand "glTextureSubImage2D"

-- glTextureSubImage2DEXT ------------------------------------------------------

glTextureSubImage2DEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @yoffset@ of type @CheckedInt32@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @pixels@ pointing to @COMPSIZE(format,type,width,height)@ elements of type @a@.
  -> m ()
glTextureSubImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO $ dyn771 ptr_glTextureSubImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10

{-# NOINLINE ptr_glTextureSubImage2DEXT #-}
ptr_glTextureSubImage2DEXT :: FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glTextureSubImage2DEXT = unsafePerformIO $ getCommand "glTextureSubImage2DEXT"

-- glTextureSubImage3D ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glTexSubImage3D.xhtml OpenGL 4.x>.
glTextureSubImage3D
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
  -> Ptr a -- ^ @pixels@.
  -> m ()
glTextureSubImage3D v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 = liftIO $ dyn91 ptr_glTextureSubImage3D v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11

{-# NOINLINE ptr_glTextureSubImage3D #-}
ptr_glTextureSubImage3D :: FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glTextureSubImage3D = unsafePerformIO $ getCommand "glTextureSubImage3D"

-- glTextureSubImage3DEXT ------------------------------------------------------

glTextureSubImage3DEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @yoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @zoffset@ of type @CheckedInt32@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @pixels@ pointing to @COMPSIZE(format,type,width,height,depth)@ elements of type @a@.
  -> m ()
glTextureSubImage3DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 = liftIO $ dyn772 ptr_glTextureSubImage3DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12

{-# NOINLINE ptr_glTextureSubImage3DEXT #-}
ptr_glTextureSubImage3DEXT :: FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glTextureSubImage3DEXT = unsafePerformIO $ getCommand "glTextureSubImage3DEXT"

-- glTextureView ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glTextureView.xhtml OpenGL 4.x>.
glTextureView
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLenum -- ^ @target@.
  -> GLuint -- ^ @origtexture@.
  -> GLenum -- ^ @internalformat@.
  -> GLuint -- ^ @minlevel@.
  -> GLuint -- ^ @numlevels@.
  -> GLuint -- ^ @minlayer@.
  -> GLuint -- ^ @numlayers@.
  -> m ()
glTextureView v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn773 ptr_glTextureView v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glTextureView #-}
ptr_glTextureView :: FunPtr (GLuint -> GLenum -> GLuint -> GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> IO ())
ptr_glTextureView = unsafePerformIO $ getCommand "glTextureView"

-- glTextureViewEXT ------------------------------------------------------------

-- | This command is an alias for 'glTextureView'.
glTextureViewEXT
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLenum -- ^ @target@.
  -> GLuint -- ^ @origtexture@.
  -> GLenum -- ^ @internalformat@.
  -> GLuint -- ^ @minlevel@.
  -> GLuint -- ^ @numlevels@.
  -> GLuint -- ^ @minlayer@.
  -> GLuint -- ^ @numlayers@.
  -> m ()
glTextureViewEXT v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn773 ptr_glTextureViewEXT v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glTextureViewEXT #-}
ptr_glTextureViewEXT :: FunPtr (GLuint -> GLenum -> GLuint -> GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> IO ())
ptr_glTextureViewEXT = unsafePerformIO $ getCommand "glTextureViewEXT"

-- glTextureViewOES ------------------------------------------------------------

-- | This command is an alias for 'glTextureView'.
glTextureViewOES
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLenum -- ^ @target@.
  -> GLuint -- ^ @origtexture@.
  -> GLenum -- ^ @internalformat@.
  -> GLuint -- ^ @minlevel@.
  -> GLuint -- ^ @numlevels@.
  -> GLuint -- ^ @minlayer@.
  -> GLuint -- ^ @numlayers@.
  -> m ()
glTextureViewOES v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn773 ptr_glTextureViewOES v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glTextureViewOES #-}
ptr_glTextureViewOES :: FunPtr (GLuint -> GLenum -> GLuint -> GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> IO ())
ptr_glTextureViewOES = unsafePerformIO $ getCommand "glTextureViewOES"

-- glTrackMatrixNV -------------------------------------------------------------

glTrackMatrixNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type @VertexAttribEnumNV@.
  -> GLuint -- ^ @address@.
  -> GLenum -- ^ @matrix@ of type @VertexAttribEnumNV@.
  -> GLenum -- ^ @transform@ of type @VertexAttribEnumNV@.
  -> m ()
glTrackMatrixNV v1 v2 v3 v4 = liftIO $ dyn774 ptr_glTrackMatrixNV v1 v2 v3 v4

{-# NOINLINE ptr_glTrackMatrixNV #-}
ptr_glTrackMatrixNV :: FunPtr (GLenum -> GLuint -> GLenum -> GLenum -> IO ())
ptr_glTrackMatrixNV = unsafePerformIO $ getCommand "glTrackMatrixNV"

-- glTransformFeedbackAttribsNV ------------------------------------------------

glTransformFeedbackAttribsNV
  :: MonadIO m
  => GLsizei -- ^ @count@.
  -> Ptr GLint -- ^ @attribs@ pointing to @COMPSIZE(count)@ elements of type @GLint@.
  -> GLenum -- ^ @bufferMode@.
  -> m ()
glTransformFeedbackAttribsNV v1 v2 v3 = liftIO $ dyn775 ptr_glTransformFeedbackAttribsNV v1 v2 v3

{-# NOINLINE ptr_glTransformFeedbackAttribsNV #-}
ptr_glTransformFeedbackAttribsNV :: FunPtr (GLsizei -> Ptr GLint -> GLenum -> IO ())
ptr_glTransformFeedbackAttribsNV = unsafePerformIO $ getCommand "glTransformFeedbackAttribsNV"

-- glTransformFeedbackBufferBase -----------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glTransformFeedbackBufferBase.xhtml OpenGL 4.x>.
glTransformFeedbackBufferBase
  :: MonadIO m
  => GLuint -- ^ @xfb@.
  -> GLuint -- ^ @index@.
  -> GLuint -- ^ @buffer@.
  -> m ()
glTransformFeedbackBufferBase v1 v2 v3 = liftIO $ dyn102 ptr_glTransformFeedbackBufferBase v1 v2 v3

{-# NOINLINE ptr_glTransformFeedbackBufferBase #-}
ptr_glTransformFeedbackBufferBase :: FunPtr (GLuint -> GLuint -> GLuint -> IO ())
ptr_glTransformFeedbackBufferBase = unsafePerformIO $ getCommand "glTransformFeedbackBufferBase"

-- glTransformFeedbackBufferRange ----------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glTransformFeedbackBufferRange.xhtml OpenGL 4.x>.
glTransformFeedbackBufferRange
  :: MonadIO m
  => GLuint -- ^ @xfb@.
  -> GLuint -- ^ @index@.
  -> GLuint -- ^ @buffer@.
  -> GLintptr -- ^ @offset@.
  -> GLsizeiptr -- ^ @size@ of type @BufferSize@.
  -> m ()
glTransformFeedbackBufferRange v1 v2 v3 v4 v5 = liftIO $ dyn776 ptr_glTransformFeedbackBufferRange v1 v2 v3 v4 v5

{-# NOINLINE ptr_glTransformFeedbackBufferRange #-}
ptr_glTransformFeedbackBufferRange :: FunPtr (GLuint -> GLuint -> GLuint -> GLintptr -> GLsizeiptr -> IO ())
ptr_glTransformFeedbackBufferRange = unsafePerformIO $ getCommand "glTransformFeedbackBufferRange"

-- glTransformFeedbackStreamAttribsNV ------------------------------------------

glTransformFeedbackStreamAttribsNV
  :: MonadIO m
  => GLsizei -- ^ @count@.
  -> Ptr GLint -- ^ @attribs@ pointing to @count@ elements of type @GLint@.
  -> GLsizei -- ^ @nbuffers@.
  -> Ptr GLint -- ^ @bufstreams@ pointing to @nbuffers@ elements of type @GLint@.
  -> GLenum -- ^ @bufferMode@.
  -> m ()
glTransformFeedbackStreamAttribsNV v1 v2 v3 v4 v5 = liftIO $ dyn777 ptr_glTransformFeedbackStreamAttribsNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glTransformFeedbackStreamAttribsNV #-}
ptr_glTransformFeedbackStreamAttribsNV :: FunPtr (GLsizei -> Ptr GLint -> GLsizei -> Ptr GLint -> GLenum -> IO ())
ptr_glTransformFeedbackStreamAttribsNV = unsafePerformIO $ getCommand "glTransformFeedbackStreamAttribsNV"

-- glTransformFeedbackVaryings -------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glTransformFeedbackVaryings.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glTransformFeedbackVaryings.xhtml OpenGL 4.x>.
glTransformFeedbackVaryings
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLsizei -- ^ @count@.
  -> Ptr (Ptr GLchar) -- ^ @varyings@ pointing to @count@ elements of type @Ptr GLchar@.
  -> GLenum -- ^ @bufferMode@.
  -> m ()
glTransformFeedbackVaryings v1 v2 v3 v4 = liftIO $ dyn778 ptr_glTransformFeedbackVaryings v1 v2 v3 v4

{-# NOINLINE ptr_glTransformFeedbackVaryings #-}
ptr_glTransformFeedbackVaryings :: FunPtr (GLuint -> GLsizei -> Ptr (Ptr GLchar) -> GLenum -> IO ())
ptr_glTransformFeedbackVaryings = unsafePerformIO $ getCommand "glTransformFeedbackVaryings"

-- glTransformFeedbackVaryingsEXT ----------------------------------------------

-- | This command is an alias for 'glTransformFeedbackVaryings'.
glTransformFeedbackVaryingsEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLsizei -- ^ @count@.
  -> Ptr (Ptr GLchar) -- ^ @varyings@ pointing to @count@ elements of type @Ptr GLchar@.
  -> GLenum -- ^ @bufferMode@.
  -> m ()
glTransformFeedbackVaryingsEXT v1 v2 v3 v4 = liftIO $ dyn778 ptr_glTransformFeedbackVaryingsEXT v1 v2 v3 v4

{-# NOINLINE ptr_glTransformFeedbackVaryingsEXT #-}
ptr_glTransformFeedbackVaryingsEXT :: FunPtr (GLuint -> GLsizei -> Ptr (Ptr GLchar) -> GLenum -> IO ())
ptr_glTransformFeedbackVaryingsEXT = unsafePerformIO $ getCommand "glTransformFeedbackVaryingsEXT"

-- glTransformFeedbackVaryingsNV -----------------------------------------------

glTransformFeedbackVaryingsNV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint -- ^ @locations@ pointing to @count@ elements of type @GLint@.
  -> GLenum -- ^ @bufferMode@.
  -> m ()
glTransformFeedbackVaryingsNV v1 v2 v3 v4 = liftIO $ dyn779 ptr_glTransformFeedbackVaryingsNV v1 v2 v3 v4

{-# NOINLINE ptr_glTransformFeedbackVaryingsNV #-}
ptr_glTransformFeedbackVaryingsNV :: FunPtr (GLuint -> GLsizei -> Ptr GLint -> GLenum -> IO ())
ptr_glTransformFeedbackVaryingsNV = unsafePerformIO $ getCommand "glTransformFeedbackVaryingsNV"

-- glTransformPathNV -----------------------------------------------------------

glTransformPathNV
  :: MonadIO m
  => GLuint -- ^ @resultPath@ of type @Path@.
  -> GLuint -- ^ @srcPath@ of type @Path@.
  -> GLenum -- ^ @transformType@ of type @PathTransformType@.
  -> Ptr GLfloat -- ^ @transformValues@ pointing to @COMPSIZE(transformType)@ elements of type @GLfloat@.
  -> m ()
glTransformPathNV v1 v2 v3 v4 = liftIO $ dyn445 ptr_glTransformPathNV v1 v2 v3 v4

{-# NOINLINE ptr_glTransformPathNV #-}
ptr_glTransformPathNV :: FunPtr (GLuint -> GLuint -> GLenum -> Ptr GLfloat -> IO ())
ptr_glTransformPathNV = unsafePerformIO $ getCommand "glTransformPathNV"

-- glTranslated ----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTranslate.xml OpenGL 2.x>.
glTranslated
  :: MonadIO m
  => GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> GLdouble -- ^ @z@.
  -> m ()
glTranslated v1 v2 v3 = liftIO $ dyn38 ptr_glTranslated v1 v2 v3

{-# NOINLINE ptr_glTranslated #-}
ptr_glTranslated :: FunPtr (GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glTranslated = unsafePerformIO $ getCommand "glTranslated"

-- glTranslatef ----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTranslate.xml OpenGL 2.x>.
glTranslatef
  :: MonadIO m
  => GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> m ()
glTranslatef v1 v2 v3 = liftIO $ dyn40 ptr_glTranslatef v1 v2 v3

{-# NOINLINE ptr_glTranslatef #-}
ptr_glTranslatef :: FunPtr (GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glTranslatef = unsafePerformIO $ getCommand "glTranslatef"

-- glTranslatex ----------------------------------------------------------------

glTranslatex
  :: MonadIO m
  => GLfixed -- ^ @x@.
  -> GLfixed -- ^ @y@.
  -> GLfixed -- ^ @z@.
  -> m ()
glTranslatex v1 v2 v3 = liftIO $ dyn106 ptr_glTranslatex v1 v2 v3

{-# NOINLINE ptr_glTranslatex #-}
ptr_glTranslatex :: FunPtr (GLfixed -> GLfixed -> GLfixed -> IO ())
ptr_glTranslatex = unsafePerformIO $ getCommand "glTranslatex"

-- glTranslatexOES -------------------------------------------------------------

glTranslatexOES
  :: MonadIO m
  => GLfixed -- ^ @x@.
  -> GLfixed -- ^ @y@.
  -> GLfixed -- ^ @z@.
  -> m ()
glTranslatexOES v1 v2 v3 = liftIO $ dyn106 ptr_glTranslatexOES v1 v2 v3

{-# NOINLINE ptr_glTranslatexOES #-}
ptr_glTranslatexOES :: FunPtr (GLfixed -> GLfixed -> GLfixed -> IO ())
ptr_glTranslatexOES = unsafePerformIO $ getCommand "glTranslatexOES"

-- glUniform1d -----------------------------------------------------------------

glUniform1d
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLdouble -- ^ @x@.
  -> m ()
glUniform1d v1 v2 = liftIO $ dyn780 ptr_glUniform1d v1 v2

{-# NOINLINE ptr_glUniform1d #-}
ptr_glUniform1d :: FunPtr (GLint -> GLdouble -> IO ())
ptr_glUniform1d = unsafePerformIO $ getCommand "glUniform1d"

-- glUniform1dv ----------------------------------------------------------------

glUniform1dv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLdouble -- ^ @value@ pointing to @count*1@ elements of type @GLdouble@.
  -> m ()
glUniform1dv v1 v2 v3 = liftIO $ dyn781 ptr_glUniform1dv v1 v2 v3

{-# NOINLINE ptr_glUniform1dv #-}
ptr_glUniform1dv :: FunPtr (GLint -> GLsizei -> Ptr GLdouble -> IO ())
ptr_glUniform1dv = unsafePerformIO $ getCommand "glUniform1dv"

-- glUniform1f -----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glUniform.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniform1f
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLfloat -- ^ @v0@.
  -> m ()
glUniform1f v1 v2 = liftIO $ dyn782 ptr_glUniform1f v1 v2

{-# NOINLINE ptr_glUniform1f #-}
ptr_glUniform1f :: FunPtr (GLint -> GLfloat -> IO ())
ptr_glUniform1f = unsafePerformIO $ getCommand "glUniform1f"

-- glUniform1fARB --------------------------------------------------------------

-- | This command is an alias for 'glUniform1f'.
glUniform1fARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLfloat -- ^ @v0@.
  -> m ()
glUniform1fARB v1 v2 = liftIO $ dyn782 ptr_glUniform1fARB v1 v2

{-# NOINLINE ptr_glUniform1fARB #-}
ptr_glUniform1fARB :: FunPtr (GLint -> GLfloat -> IO ())
ptr_glUniform1fARB = unsafePerformIO $ getCommand "glUniform1fARB"

-- glUniform1fv ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glUniform.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniform1fv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @value@ pointing to @count*1@ elements of type @GLfloat@.
  -> m ()
glUniform1fv v1 v2 v3 = liftIO $ dyn783 ptr_glUniform1fv v1 v2 v3

{-# NOINLINE ptr_glUniform1fv #-}
ptr_glUniform1fv :: FunPtr (GLint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glUniform1fv = unsafePerformIO $ getCommand "glUniform1fv"

-- glUniform1fvARB -------------------------------------------------------------

-- | This command is an alias for 'glUniform1fv'.
glUniform1fvARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @value@ pointing to @count*1@ elements of type @GLfloat@.
  -> m ()
glUniform1fvARB v1 v2 v3 = liftIO $ dyn783 ptr_glUniform1fvARB v1 v2 v3

{-# NOINLINE ptr_glUniform1fvARB #-}
ptr_glUniform1fvARB :: FunPtr (GLint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glUniform1fvARB = unsafePerformIO $ getCommand "glUniform1fvARB"

-- glUniform1i -----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glUniform.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniform1i
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLint -- ^ @v0@.
  -> m ()
glUniform1i v1 v2 = liftIO $ dyn266 ptr_glUniform1i v1 v2

{-# NOINLINE ptr_glUniform1i #-}
ptr_glUniform1i :: FunPtr (GLint -> GLint -> IO ())
ptr_glUniform1i = unsafePerformIO $ getCommand "glUniform1i"

-- glUniform1i64ARB ------------------------------------------------------------

glUniform1i64ARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLint64 -- ^ @x@.
  -> m ()
glUniform1i64ARB v1 v2 = liftIO $ dyn784 ptr_glUniform1i64ARB v1 v2

{-# NOINLINE ptr_glUniform1i64ARB #-}
ptr_glUniform1i64ARB :: FunPtr (GLint -> GLint64 -> IO ())
ptr_glUniform1i64ARB = unsafePerformIO $ getCommand "glUniform1i64ARB"

-- glUniform1i64NV -------------------------------------------------------------

glUniform1i64NV
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLint64EXT -- ^ @x@.
  -> m ()
glUniform1i64NV v1 v2 = liftIO $ dyn785 ptr_glUniform1i64NV v1 v2

{-# NOINLINE ptr_glUniform1i64NV #-}
ptr_glUniform1i64NV :: FunPtr (GLint -> GLint64EXT -> IO ())
ptr_glUniform1i64NV = unsafePerformIO $ getCommand "glUniform1i64NV"

-- glUniform1i64vARB -----------------------------------------------------------

glUniform1i64vARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint64 -- ^ @value@ pointing to @count*1@ elements of type @GLint64@.
  -> m ()
glUniform1i64vARB v1 v2 v3 = liftIO $ dyn786 ptr_glUniform1i64vARB v1 v2 v3

{-# NOINLINE ptr_glUniform1i64vARB #-}
ptr_glUniform1i64vARB :: FunPtr (GLint -> GLsizei -> Ptr GLint64 -> IO ())
ptr_glUniform1i64vARB = unsafePerformIO $ getCommand "glUniform1i64vARB"

-- glUniform1i64vNV ------------------------------------------------------------

glUniform1i64vNV
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint64EXT -- ^ @value@ pointing to @count*1@ elements of type @GLint64EXT@.
  -> m ()
glUniform1i64vNV v1 v2 v3 = liftIO $ dyn787 ptr_glUniform1i64vNV v1 v2 v3

{-# NOINLINE ptr_glUniform1i64vNV #-}
ptr_glUniform1i64vNV :: FunPtr (GLint -> GLsizei -> Ptr GLint64EXT -> IO ())
ptr_glUniform1i64vNV = unsafePerformIO $ getCommand "glUniform1i64vNV"

-- glUniform1iARB --------------------------------------------------------------

-- | This command is an alias for 'glUniform1i'.
glUniform1iARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLint -- ^ @v0@.
  -> m ()
glUniform1iARB v1 v2 = liftIO $ dyn266 ptr_glUniform1iARB v1 v2

{-# NOINLINE ptr_glUniform1iARB #-}
ptr_glUniform1iARB :: FunPtr (GLint -> GLint -> IO ())
ptr_glUniform1iARB = unsafePerformIO $ getCommand "glUniform1iARB"

-- glUniform1iv ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glUniform.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniform1iv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint -- ^ @value@ pointing to @count*1@ elements of type @GLint@.
  -> m ()
glUniform1iv v1 v2 v3 = liftIO $ dyn788 ptr_glUniform1iv v1 v2 v3

{-# NOINLINE ptr_glUniform1iv #-}
ptr_glUniform1iv :: FunPtr (GLint -> GLsizei -> Ptr GLint -> IO ())
ptr_glUniform1iv = unsafePerformIO $ getCommand "glUniform1iv"

-- glUniform1ivARB -------------------------------------------------------------

-- | This command is an alias for 'glUniform1iv'.
glUniform1ivARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint -- ^ @value@ pointing to @count*1@ elements of type @GLint@.
  -> m ()
glUniform1ivARB v1 v2 v3 = liftIO $ dyn788 ptr_glUniform1ivARB v1 v2 v3

{-# NOINLINE ptr_glUniform1ivARB #-}
ptr_glUniform1ivARB :: FunPtr (GLint -> GLsizei -> Ptr GLint -> IO ())
ptr_glUniform1ivARB = unsafePerformIO $ getCommand "glUniform1ivARB"

-- glUniform1ui ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniform1ui
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLuint -- ^ @v0@.
  -> m ()
glUniform1ui v1 v2 = liftIO $ dyn789 ptr_glUniform1ui v1 v2

{-# NOINLINE ptr_glUniform1ui #-}
ptr_glUniform1ui :: FunPtr (GLint -> GLuint -> IO ())
ptr_glUniform1ui = unsafePerformIO $ getCommand "glUniform1ui"

-- glUniform1ui64ARB -----------------------------------------------------------

glUniform1ui64ARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLuint64 -- ^ @x@.
  -> m ()
glUniform1ui64ARB v1 v2 = liftIO $ dyn790 ptr_glUniform1ui64ARB v1 v2

{-# NOINLINE ptr_glUniform1ui64ARB #-}
ptr_glUniform1ui64ARB :: FunPtr (GLint -> GLuint64 -> IO ())
ptr_glUniform1ui64ARB = unsafePerformIO $ getCommand "glUniform1ui64ARB"

-- glUniform1ui64NV ------------------------------------------------------------

glUniform1ui64NV
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLuint64EXT -- ^ @x@.
  -> m ()
glUniform1ui64NV v1 v2 = liftIO $ dyn791 ptr_glUniform1ui64NV v1 v2

{-# NOINLINE ptr_glUniform1ui64NV #-}
ptr_glUniform1ui64NV :: FunPtr (GLint -> GLuint64EXT -> IO ())
ptr_glUniform1ui64NV = unsafePerformIO $ getCommand "glUniform1ui64NV"

