--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F70
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

module Graphics.GL.Functions.F70 (
  glVertexArrayParameteriAPPLE,
  glVertexArrayRangeAPPLE,
  glVertexArrayRangeNV,
  glVertexArraySecondaryColorOffsetEXT,
  glVertexArrayTexCoordOffsetEXT,
  glVertexArrayVertexAttribBindingEXT,
  glVertexArrayVertexAttribDivisorEXT,
  glVertexArrayVertexAttribFormatEXT,
  glVertexArrayVertexAttribIFormatEXT,
  glVertexArrayVertexAttribIOffsetEXT,
  glVertexArrayVertexAttribLFormatEXT,
  glVertexArrayVertexAttribLOffsetEXT,
  glVertexArrayVertexAttribOffsetEXT,
  glVertexArrayVertexBindingDivisorEXT,
  glVertexArrayVertexBuffer,
  glVertexArrayVertexBuffers,
  glVertexArrayVertexOffsetEXT,
  glVertexAttrib1d,
  glVertexAttrib1dARB,
  glVertexAttrib1dNV,
  glVertexAttrib1dv,
  glVertexAttrib1dvARB,
  glVertexAttrib1dvNV,
  glVertexAttrib1f,
  glVertexAttrib1fARB,
  glVertexAttrib1fNV,
  glVertexAttrib1fv,
  glVertexAttrib1fvARB,
  glVertexAttrib1fvNV,
  glVertexAttrib1hNV,
  glVertexAttrib1hvNV,
  glVertexAttrib1s,
  glVertexAttrib1sARB,
  glVertexAttrib1sNV,
  glVertexAttrib1sv,
  glVertexAttrib1svARB,
  glVertexAttrib1svNV,
  glVertexAttrib2d,
  glVertexAttrib2dARB,
  glVertexAttrib2dNV
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

-- glVertexArrayParameteriAPPLE ------------------------------------------------

glVertexArrayParameteriAPPLE
  :: MonadIO m
  => GLenum -- ^ @pname@ of type @VertexArrayPNameAPPLE@.
  -> GLint -- ^ @param@.
  -> m ()
glVertexArrayParameteriAPPLE v1 v2 = liftIO $ dyn55 ptr_glVertexArrayParameteriAPPLE v1 v2

{-# NOINLINE ptr_glVertexArrayParameteriAPPLE #-}
ptr_glVertexArrayParameteriAPPLE :: FunPtr (GLenum -> GLint -> IO ())
ptr_glVertexArrayParameteriAPPLE = unsafePerformIO $ getCommand "glVertexArrayParameteriAPPLE"

-- glVertexArrayRangeAPPLE -----------------------------------------------------

glVertexArrayRangeAPPLE
  :: MonadIO m
  => GLsizei -- ^ @length@.
  -> Ptr a -- ^ @pointer@ pointing to @length@ elements of type @a@.
  -> m ()
glVertexArrayRangeAPPLE v1 v2 = liftIO $ dyn260 ptr_glVertexArrayRangeAPPLE v1 v2

{-# NOINLINE ptr_glVertexArrayRangeAPPLE #-}
ptr_glVertexArrayRangeAPPLE :: FunPtr (GLsizei -> Ptr a -> IO ())
ptr_glVertexArrayRangeAPPLE = unsafePerformIO $ getCommand "glVertexArrayRangeAPPLE"

-- glVertexArrayRangeNV --------------------------------------------------------

glVertexArrayRangeNV
  :: MonadIO m
  => GLsizei -- ^ @length@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(length)@ elements of type @a@.
  -> m ()
glVertexArrayRangeNV v1 v2 = liftIO $ dyn260 ptr_glVertexArrayRangeNV v1 v2

{-# NOINLINE ptr_glVertexArrayRangeNV #-}
ptr_glVertexArrayRangeNV :: FunPtr (GLsizei -> Ptr a -> IO ())
ptr_glVertexArrayRangeNV = unsafePerformIO $ getCommand "glVertexArrayRangeNV"

-- glVertexArraySecondaryColorOffsetEXT ----------------------------------------

glVertexArraySecondaryColorOffsetEXT
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @buffer@.
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [ColorPointerType](Graphics-GL-Groups.html#ColorPointerType).
  -> GLsizei -- ^ @stride@.
  -> GLintptr -- ^ @offset@.
  -> m ()
glVertexArraySecondaryColorOffsetEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn834 ptr_glVertexArraySecondaryColorOffsetEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glVertexArraySecondaryColorOffsetEXT #-}
ptr_glVertexArraySecondaryColorOffsetEXT :: FunPtr (GLuint -> GLuint -> GLint -> GLenum -> GLsizei -> GLintptr -> IO ())
ptr_glVertexArraySecondaryColorOffsetEXT = unsafePerformIO $ getCommand "glVertexArraySecondaryColorOffsetEXT"

-- glVertexArrayTexCoordOffsetEXT ----------------------------------------------

glVertexArrayTexCoordOffsetEXT
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @buffer@.
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [TexCoordPointerType](Graphics-GL-Groups.html#TexCoordPointerType).
  -> GLsizei -- ^ @stride@.
  -> GLintptr -- ^ @offset@.
  -> m ()
glVertexArrayTexCoordOffsetEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn834 ptr_glVertexArrayTexCoordOffsetEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glVertexArrayTexCoordOffsetEXT #-}
ptr_glVertexArrayTexCoordOffsetEXT :: FunPtr (GLuint -> GLuint -> GLint -> GLenum -> GLsizei -> GLintptr -> IO ())
ptr_glVertexArrayTexCoordOffsetEXT = unsafePerformIO $ getCommand "glVertexArrayTexCoordOffsetEXT"

-- glVertexArrayVertexAttribBindingEXT -----------------------------------------

glVertexArrayVertexAttribBindingEXT
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @attribindex@.
  -> GLuint -- ^ @bindingindex@.
  -> m ()
glVertexArrayVertexAttribBindingEXT v1 v2 v3 = liftIO $ dyn102 ptr_glVertexArrayVertexAttribBindingEXT v1 v2 v3

{-# NOINLINE ptr_glVertexArrayVertexAttribBindingEXT #-}
ptr_glVertexArrayVertexAttribBindingEXT :: FunPtr (GLuint -> GLuint -> GLuint -> IO ())
ptr_glVertexArrayVertexAttribBindingEXT = unsafePerformIO $ getCommand "glVertexArrayVertexAttribBindingEXT"

-- glVertexArrayVertexAttribDivisorEXT -----------------------------------------

glVertexArrayVertexAttribDivisorEXT
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @index@.
  -> GLuint -- ^ @divisor@.
  -> m ()
glVertexArrayVertexAttribDivisorEXT v1 v2 v3 = liftIO $ dyn102 ptr_glVertexArrayVertexAttribDivisorEXT v1 v2 v3

{-# NOINLINE ptr_glVertexArrayVertexAttribDivisorEXT #-}
ptr_glVertexArrayVertexAttribDivisorEXT :: FunPtr (GLuint -> GLuint -> GLuint -> IO ())
ptr_glVertexArrayVertexAttribDivisorEXT = unsafePerformIO $ getCommand "glVertexArrayVertexAttribDivisorEXT"

-- glVertexArrayVertexAttribFormatEXT ------------------------------------------

glVertexArrayVertexAttribFormatEXT
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @attribindex@.
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@.
  -> GLboolean -- ^ @normalized@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLuint -- ^ @relativeoffset@.
  -> m ()
glVertexArrayVertexAttribFormatEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn831 ptr_glVertexArrayVertexAttribFormatEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glVertexArrayVertexAttribFormatEXT #-}
ptr_glVertexArrayVertexAttribFormatEXT :: FunPtr (GLuint -> GLuint -> GLint -> GLenum -> GLboolean -> GLuint -> IO ())
ptr_glVertexArrayVertexAttribFormatEXT = unsafePerformIO $ getCommand "glVertexArrayVertexAttribFormatEXT"

-- glVertexArrayVertexAttribIFormatEXT -----------------------------------------

glVertexArrayVertexAttribIFormatEXT
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @attribindex@.
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@.
  -> GLuint -- ^ @relativeoffset@.
  -> m ()
glVertexArrayVertexAttribIFormatEXT v1 v2 v3 v4 v5 = liftIO $ dyn832 ptr_glVertexArrayVertexAttribIFormatEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexArrayVertexAttribIFormatEXT #-}
ptr_glVertexArrayVertexAttribIFormatEXT :: FunPtr (GLuint -> GLuint -> GLint -> GLenum -> GLuint -> IO ())
ptr_glVertexArrayVertexAttribIFormatEXT = unsafePerformIO $ getCommand "glVertexArrayVertexAttribIFormatEXT"

-- glVertexArrayVertexAttribIOffsetEXT -----------------------------------------

glVertexArrayVertexAttribIOffsetEXT
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @buffer@.
  -> GLuint -- ^ @index@.
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type @VertexAttribEnum@.
  -> GLsizei -- ^ @stride@.
  -> GLintptr -- ^ @offset@.
  -> m ()
glVertexArrayVertexAttribIOffsetEXT v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn838 ptr_glVertexArrayVertexAttribIOffsetEXT v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glVertexArrayVertexAttribIOffsetEXT #-}
ptr_glVertexArrayVertexAttribIOffsetEXT :: FunPtr (GLuint -> GLuint -> GLuint -> GLint -> GLenum -> GLsizei -> GLintptr -> IO ())
ptr_glVertexArrayVertexAttribIOffsetEXT = unsafePerformIO $ getCommand "glVertexArrayVertexAttribIOffsetEXT"

-- glVertexArrayVertexAttribLFormatEXT -----------------------------------------

glVertexArrayVertexAttribLFormatEXT
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @attribindex@.
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@.
  -> GLuint -- ^ @relativeoffset@.
  -> m ()
glVertexArrayVertexAttribLFormatEXT v1 v2 v3 v4 v5 = liftIO $ dyn832 ptr_glVertexArrayVertexAttribLFormatEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexArrayVertexAttribLFormatEXT #-}
ptr_glVertexArrayVertexAttribLFormatEXT :: FunPtr (GLuint -> GLuint -> GLint -> GLenum -> GLuint -> IO ())
ptr_glVertexArrayVertexAttribLFormatEXT = unsafePerformIO $ getCommand "glVertexArrayVertexAttribLFormatEXT"

-- glVertexArrayVertexAttribLOffsetEXT -----------------------------------------

glVertexArrayVertexAttribLOffsetEXT
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @buffer@.
  -> GLuint -- ^ @index@.
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@.
  -> GLsizei -- ^ @stride@.
  -> GLintptr -- ^ @offset@ of type @BufferOffset@.
  -> m ()
glVertexArrayVertexAttribLOffsetEXT v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn838 ptr_glVertexArrayVertexAttribLOffsetEXT v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glVertexArrayVertexAttribLOffsetEXT #-}
ptr_glVertexArrayVertexAttribLOffsetEXT :: FunPtr (GLuint -> GLuint -> GLuint -> GLint -> GLenum -> GLsizei -> GLintptr -> IO ())
ptr_glVertexArrayVertexAttribLOffsetEXT = unsafePerformIO $ getCommand "glVertexArrayVertexAttribLOffsetEXT"

-- glVertexArrayVertexAttribOffsetEXT ------------------------------------------

glVertexArrayVertexAttribOffsetEXT
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @buffer@.
  -> GLuint -- ^ @index@.
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type @VertexAttribPointerType@.
  -> GLboolean -- ^ @normalized@.
  -> GLsizei -- ^ @stride@.
  -> GLintptr -- ^ @offset@.
  -> m ()
glVertexArrayVertexAttribOffsetEXT v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn839 ptr_glVertexArrayVertexAttribOffsetEXT v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glVertexArrayVertexAttribOffsetEXT #-}
ptr_glVertexArrayVertexAttribOffsetEXT :: FunPtr (GLuint -> GLuint -> GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> GLintptr -> IO ())
ptr_glVertexArrayVertexAttribOffsetEXT = unsafePerformIO $ getCommand "glVertexArrayVertexAttribOffsetEXT"

-- glVertexArrayVertexBindingDivisorEXT ----------------------------------------

glVertexArrayVertexBindingDivisorEXT
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @bindingindex@.
  -> GLuint -- ^ @divisor@.
  -> m ()
glVertexArrayVertexBindingDivisorEXT v1 v2 v3 = liftIO $ dyn102 ptr_glVertexArrayVertexBindingDivisorEXT v1 v2 v3

{-# NOINLINE ptr_glVertexArrayVertexBindingDivisorEXT #-}
ptr_glVertexArrayVertexBindingDivisorEXT :: FunPtr (GLuint -> GLuint -> GLuint -> IO ())
ptr_glVertexArrayVertexBindingDivisorEXT = unsafePerformIO $ getCommand "glVertexArrayVertexBindingDivisorEXT"

-- glVertexArrayVertexBuffer ---------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glBindVertexBuffer.xhtml OpenGL 4.x>.
glVertexArrayVertexBuffer
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @bindingindex@.
  -> GLuint -- ^ @buffer@.
  -> GLintptr -- ^ @offset@.
  -> GLsizei -- ^ @stride@.
  -> m ()
glVertexArrayVertexBuffer v1 v2 v3 v4 v5 = liftIO $ dyn833 ptr_glVertexArrayVertexBuffer v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexArrayVertexBuffer #-}
ptr_glVertexArrayVertexBuffer :: FunPtr (GLuint -> GLuint -> GLuint -> GLintptr -> GLsizei -> IO ())
ptr_glVertexArrayVertexBuffer = unsafePerformIO $ getCommand "glVertexArrayVertexBuffer"

-- glVertexArrayVertexBuffers --------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glBindVertexBuffers.xhtml OpenGL 4.x>.
glVertexArrayVertexBuffers
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @first@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @buffers@.
  -> Ptr GLintptr -- ^ @offsets@.
  -> Ptr GLsizei -- ^ @strides@.
  -> m ()
glVertexArrayVertexBuffers v1 v2 v3 v4 v5 v6 = liftIO $ dyn840 ptr_glVertexArrayVertexBuffers v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glVertexArrayVertexBuffers #-}
ptr_glVertexArrayVertexBuffers :: FunPtr (GLuint -> GLuint -> GLsizei -> Ptr GLuint -> Ptr GLintptr -> Ptr GLsizei -> IO ())
ptr_glVertexArrayVertexBuffers = unsafePerformIO $ getCommand "glVertexArrayVertexBuffers"

-- glVertexArrayVertexOffsetEXT ------------------------------------------------

glVertexArrayVertexOffsetEXT
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @buffer@.
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [VertexPointerType](Graphics-GL-Groups.html#VertexPointerType).
  -> GLsizei -- ^ @stride@.
  -> GLintptr -- ^ @offset@.
  -> m ()
glVertexArrayVertexOffsetEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn834 ptr_glVertexArrayVertexOffsetEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glVertexArrayVertexOffsetEXT #-}
ptr_glVertexArrayVertexOffsetEXT :: FunPtr (GLuint -> GLuint -> GLint -> GLenum -> GLsizei -> GLintptr -> IO ())
ptr_glVertexArrayVertexOffsetEXT = unsafePerformIO $ getCommand "glVertexArrayVertexOffsetEXT"

-- glVertexAttrib1d ------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>. The vector equivalent of this command is 'glVertexAttrib1dv'.
glVertexAttrib1d
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLdouble -- ^ @x@.
  -> m ()
glVertexAttrib1d v1 v2 = liftIO $ dyn841 ptr_glVertexAttrib1d v1 v2

{-# NOINLINE ptr_glVertexAttrib1d #-}
ptr_glVertexAttrib1d :: FunPtr (GLuint -> GLdouble -> IO ())
ptr_glVertexAttrib1d = unsafePerformIO $ getCommand "glVertexAttrib1d"

-- glVertexAttrib1dARB ---------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib1dvARB'. This command is an alias for 'glVertexAttrib1d'.
glVertexAttrib1dARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLdouble -- ^ @x@.
  -> m ()
glVertexAttrib1dARB v1 v2 = liftIO $ dyn841 ptr_glVertexAttrib1dARB v1 v2

{-# NOINLINE ptr_glVertexAttrib1dARB #-}
ptr_glVertexAttrib1dARB :: FunPtr (GLuint -> GLdouble -> IO ())
ptr_glVertexAttrib1dARB = unsafePerformIO $ getCommand "glVertexAttrib1dARB"

-- glVertexAttrib1dNV ----------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib1dvNV'. This command is an alias for 'glVertexAttrib1d'.
glVertexAttrib1dNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLdouble -- ^ @x@.
  -> m ()
glVertexAttrib1dNV v1 v2 = liftIO $ dyn841 ptr_glVertexAttrib1dNV v1 v2

{-# NOINLINE ptr_glVertexAttrib1dNV #-}
ptr_glVertexAttrib1dNV :: FunPtr (GLuint -> GLdouble -> IO ())
ptr_glVertexAttrib1dNV = unsafePerformIO $ getCommand "glVertexAttrib1dNV"

-- glVertexAttrib1dv -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttrib1dv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLdouble -- ^ @v@ pointing to @1@ element of type @GLdouble@.
  -> m ()
glVertexAttrib1dv v1 v2 = liftIO $ dyn828 ptr_glVertexAttrib1dv v1 v2

{-# NOINLINE ptr_glVertexAttrib1dv #-}
ptr_glVertexAttrib1dv :: FunPtr (GLuint -> Ptr GLdouble -> IO ())
ptr_glVertexAttrib1dv = unsafePerformIO $ getCommand "glVertexAttrib1dv"

-- glVertexAttrib1dvARB --------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib1dv'.
glVertexAttrib1dvARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLdouble -- ^ @v@ pointing to @1@ element of type @GLdouble@.
  -> m ()
glVertexAttrib1dvARB v1 v2 = liftIO $ dyn828 ptr_glVertexAttrib1dvARB v1 v2

{-# NOINLINE ptr_glVertexAttrib1dvARB #-}
ptr_glVertexAttrib1dvARB :: FunPtr (GLuint -> Ptr GLdouble -> IO ())
ptr_glVertexAttrib1dvARB = unsafePerformIO $ getCommand "glVertexAttrib1dvARB"

-- glVertexAttrib1dvNV ---------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib1dv'.
glVertexAttrib1dvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLdouble -- ^ @v@ pointing to @1@ element of type @GLdouble@.
  -> m ()
glVertexAttrib1dvNV v1 v2 = liftIO $ dyn828 ptr_glVertexAttrib1dvNV v1 v2

{-# NOINLINE ptr_glVertexAttrib1dvNV #-}
ptr_glVertexAttrib1dvNV :: FunPtr (GLuint -> Ptr GLdouble -> IO ())
ptr_glVertexAttrib1dvNV = unsafePerformIO $ getCommand "glVertexAttrib1dvNV"

-- glVertexAttrib1f ------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>. The vector equivalent of this command is 'glVertexAttrib1fv'.
glVertexAttrib1f
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLfloat -- ^ @x@.
  -> m ()
glVertexAttrib1f v1 v2 = liftIO $ dyn842 ptr_glVertexAttrib1f v1 v2

{-# NOINLINE ptr_glVertexAttrib1f #-}
ptr_glVertexAttrib1f :: FunPtr (GLuint -> GLfloat -> IO ())
ptr_glVertexAttrib1f = unsafePerformIO $ getCommand "glVertexAttrib1f"

-- glVertexAttrib1fARB ---------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib1fvARB'. This command is an alias for 'glVertexAttrib1f'.
glVertexAttrib1fARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLfloat -- ^ @x@.
  -> m ()
glVertexAttrib1fARB v1 v2 = liftIO $ dyn842 ptr_glVertexAttrib1fARB v1 v2

{-# NOINLINE ptr_glVertexAttrib1fARB #-}
ptr_glVertexAttrib1fARB :: FunPtr (GLuint -> GLfloat -> IO ())
ptr_glVertexAttrib1fARB = unsafePerformIO $ getCommand "glVertexAttrib1fARB"

-- glVertexAttrib1fNV ----------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib1fvNV'. This command is an alias for 'glVertexAttrib1f'.
glVertexAttrib1fNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLfloat -- ^ @x@.
  -> m ()
glVertexAttrib1fNV v1 v2 = liftIO $ dyn842 ptr_glVertexAttrib1fNV v1 v2

{-# NOINLINE ptr_glVertexAttrib1fNV #-}
ptr_glVertexAttrib1fNV :: FunPtr (GLuint -> GLfloat -> IO ())
ptr_glVertexAttrib1fNV = unsafePerformIO $ getCommand "glVertexAttrib1fNV"

-- glVertexAttrib1fv -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttrib1fv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLfloat -- ^ @v@ pointing to @1@ element of type @GLfloat@.
  -> m ()
glVertexAttrib1fv v1 v2 = liftIO $ dyn377 ptr_glVertexAttrib1fv v1 v2

{-# NOINLINE ptr_glVertexAttrib1fv #-}
ptr_glVertexAttrib1fv :: FunPtr (GLuint -> Ptr GLfloat -> IO ())
ptr_glVertexAttrib1fv = unsafePerformIO $ getCommand "glVertexAttrib1fv"

-- glVertexAttrib1fvARB --------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib1fv'.
glVertexAttrib1fvARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLfloat -- ^ @v@ pointing to @1@ element of type @GLfloat@.
  -> m ()
glVertexAttrib1fvARB v1 v2 = liftIO $ dyn377 ptr_glVertexAttrib1fvARB v1 v2

{-# NOINLINE ptr_glVertexAttrib1fvARB #-}
ptr_glVertexAttrib1fvARB :: FunPtr (GLuint -> Ptr GLfloat -> IO ())
ptr_glVertexAttrib1fvARB = unsafePerformIO $ getCommand "glVertexAttrib1fvARB"

-- glVertexAttrib1fvNV ---------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib1fv'.
glVertexAttrib1fvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLfloat -- ^ @v@ pointing to @1@ element of type @GLfloat@.
  -> m ()
glVertexAttrib1fvNV v1 v2 = liftIO $ dyn377 ptr_glVertexAttrib1fvNV v1 v2

{-# NOINLINE ptr_glVertexAttrib1fvNV #-}
ptr_glVertexAttrib1fvNV :: FunPtr (GLuint -> Ptr GLfloat -> IO ())
ptr_glVertexAttrib1fvNV = unsafePerformIO $ getCommand "glVertexAttrib1fvNV"

-- glVertexAttrib1hNV ----------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib1hvNV'.
glVertexAttrib1hNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLhalfNV -- ^ @x@ of type @Half16NV@.
  -> m ()
glVertexAttrib1hNV v1 v2 = liftIO $ dyn843 ptr_glVertexAttrib1hNV v1 v2

{-# NOINLINE ptr_glVertexAttrib1hNV #-}
ptr_glVertexAttrib1hNV :: FunPtr (GLuint -> GLhalfNV -> IO ())
ptr_glVertexAttrib1hNV = unsafePerformIO $ getCommand "glVertexAttrib1hNV"

-- glVertexAttrib1hvNV ---------------------------------------------------------

glVertexAttrib1hvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLhalfNV -- ^ @v@ pointing to @1@ element of type @Half16NV@.
  -> m ()
glVertexAttrib1hvNV v1 v2 = liftIO $ dyn844 ptr_glVertexAttrib1hvNV v1 v2

{-# NOINLINE ptr_glVertexAttrib1hvNV #-}
ptr_glVertexAttrib1hvNV :: FunPtr (GLuint -> Ptr GLhalfNV -> IO ())
ptr_glVertexAttrib1hvNV = unsafePerformIO $ getCommand "glVertexAttrib1hvNV"

-- glVertexAttrib1s ------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>. The vector equivalent of this command is 'glVertexAttrib1sv'.
glVertexAttrib1s
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLshort -- ^ @x@.
  -> m ()
glVertexAttrib1s v1 v2 = liftIO $ dyn845 ptr_glVertexAttrib1s v1 v2

{-# NOINLINE ptr_glVertexAttrib1s #-}
ptr_glVertexAttrib1s :: FunPtr (GLuint -> GLshort -> IO ())
ptr_glVertexAttrib1s = unsafePerformIO $ getCommand "glVertexAttrib1s"

-- glVertexAttrib1sARB ---------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib1svARB'. This command is an alias for 'glVertexAttrib1s'.
glVertexAttrib1sARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLshort -- ^ @x@.
  -> m ()
glVertexAttrib1sARB v1 v2 = liftIO $ dyn845 ptr_glVertexAttrib1sARB v1 v2

{-# NOINLINE ptr_glVertexAttrib1sARB #-}
ptr_glVertexAttrib1sARB :: FunPtr (GLuint -> GLshort -> IO ())
ptr_glVertexAttrib1sARB = unsafePerformIO $ getCommand "glVertexAttrib1sARB"

-- glVertexAttrib1sNV ----------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib1svNV'. This command is an alias for 'glVertexAttrib1s'.
glVertexAttrib1sNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLshort -- ^ @x@.
  -> m ()
glVertexAttrib1sNV v1 v2 = liftIO $ dyn845 ptr_glVertexAttrib1sNV v1 v2

{-# NOINLINE ptr_glVertexAttrib1sNV #-}
ptr_glVertexAttrib1sNV :: FunPtr (GLuint -> GLshort -> IO ())
ptr_glVertexAttrib1sNV = unsafePerformIO $ getCommand "glVertexAttrib1sNV"

-- glVertexAttrib1sv -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttrib1sv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLshort -- ^ @v@ pointing to @1@ element of type @GLshort@.
  -> m ()
glVertexAttrib1sv v1 v2 = liftIO $ dyn829 ptr_glVertexAttrib1sv v1 v2

{-# NOINLINE ptr_glVertexAttrib1sv #-}
ptr_glVertexAttrib1sv :: FunPtr (GLuint -> Ptr GLshort -> IO ())
ptr_glVertexAttrib1sv = unsafePerformIO $ getCommand "glVertexAttrib1sv"

-- glVertexAttrib1svARB --------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib1sv'.
glVertexAttrib1svARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLshort -- ^ @v@ pointing to @1@ element of type @GLshort@.
  -> m ()
glVertexAttrib1svARB v1 v2 = liftIO $ dyn829 ptr_glVertexAttrib1svARB v1 v2

{-# NOINLINE ptr_glVertexAttrib1svARB #-}
ptr_glVertexAttrib1svARB :: FunPtr (GLuint -> Ptr GLshort -> IO ())
ptr_glVertexAttrib1svARB = unsafePerformIO $ getCommand "glVertexAttrib1svARB"

-- glVertexAttrib1svNV ---------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib1sv'.
glVertexAttrib1svNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLshort -- ^ @v@ pointing to @1@ element of type @GLshort@.
  -> m ()
glVertexAttrib1svNV v1 v2 = liftIO $ dyn829 ptr_glVertexAttrib1svNV v1 v2

{-# NOINLINE ptr_glVertexAttrib1svNV #-}
ptr_glVertexAttrib1svNV :: FunPtr (GLuint -> Ptr GLshort -> IO ())
ptr_glVertexAttrib1svNV = unsafePerformIO $ getCommand "glVertexAttrib1svNV"

-- glVertexAttrib2d ------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>. The vector equivalent of this command is 'glVertexAttrib2dv'.
glVertexAttrib2d
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> m ()
glVertexAttrib2d v1 v2 v3 = liftIO $ dyn220 ptr_glVertexAttrib2d v1 v2 v3

{-# NOINLINE ptr_glVertexAttrib2d #-}
ptr_glVertexAttrib2d :: FunPtr (GLuint -> GLdouble -> GLdouble -> IO ())
ptr_glVertexAttrib2d = unsafePerformIO $ getCommand "glVertexAttrib2d"

-- glVertexAttrib2dARB ---------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib2dvARB'. This command is an alias for 'glVertexAttrib2d'.
glVertexAttrib2dARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> m ()
glVertexAttrib2dARB v1 v2 v3 = liftIO $ dyn220 ptr_glVertexAttrib2dARB v1 v2 v3

{-# NOINLINE ptr_glVertexAttrib2dARB #-}
ptr_glVertexAttrib2dARB :: FunPtr (GLuint -> GLdouble -> GLdouble -> IO ())
ptr_glVertexAttrib2dARB = unsafePerformIO $ getCommand "glVertexAttrib2dARB"

-- glVertexAttrib2dNV ----------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib2dvNV'. This command is an alias for 'glVertexAttrib2d'.
glVertexAttrib2dNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> m ()
glVertexAttrib2dNV v1 v2 v3 = liftIO $ dyn220 ptr_glVertexAttrib2dNV v1 v2 v3

{-# NOINLINE ptr_glVertexAttrib2dNV #-}
ptr_glVertexAttrib2dNV :: FunPtr (GLuint -> GLdouble -> GLdouble -> IO ())
ptr_glVertexAttrib2dNV = unsafePerformIO $ getCommand "glVertexAttrib2dNV"

