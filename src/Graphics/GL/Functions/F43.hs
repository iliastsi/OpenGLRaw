--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F43
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

module Graphics.GL.Functions.F43 (
  glMultiTexParameterIivEXT,
  glMultiTexParameterIuivEXT,
  glMultiTexParameterfEXT,
  glMultiTexParameterfvEXT,
  glMultiTexParameteriEXT,
  glMultiTexParameterivEXT,
  glMultiTexRenderbufferEXT,
  glMultiTexSubImage1DEXT,
  glMultiTexSubImage2DEXT,
  glMultiTexSubImage3DEXT,
  glNamedBufferData,
  glNamedBufferDataEXT,
  glNamedBufferPageCommitmentARB,
  glNamedBufferPageCommitmentEXT,
  glNamedBufferStorage,
  glNamedBufferStorageEXT,
  glNamedBufferSubData,
  glNamedBufferSubDataEXT,
  glNamedCopyBufferSubDataEXT,
  glNamedFramebufferDrawBuffer,
  glNamedFramebufferDrawBuffers,
  glNamedFramebufferParameteri,
  glNamedFramebufferParameteriEXT,
  glNamedFramebufferReadBuffer,
  glNamedFramebufferRenderbuffer,
  glNamedFramebufferRenderbufferEXT,
  glNamedFramebufferSampleLocationsfvARB,
  glNamedFramebufferSampleLocationsfvNV,
  glNamedFramebufferTexture,
  glNamedFramebufferTexture1DEXT,
  glNamedFramebufferTexture2DEXT,
  glNamedFramebufferTexture3DEXT,
  glNamedFramebufferTextureEXT,
  glNamedFramebufferTextureFaceEXT,
  glNamedFramebufferTextureLayer,
  glNamedFramebufferTextureLayerEXT,
  glNamedProgramLocalParameter4dEXT,
  glNamedProgramLocalParameter4dvEXT,
  glNamedProgramLocalParameter4fEXT,
  glNamedProgramLocalParameter4fvEXT
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

-- glMultiTexParameterIivEXT ---------------------------------------------------

glMultiTexParameterIivEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type @TextureUnit@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [TextureParameterName](Graphics-GL-Groups.html#TextureParameterName).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedInt32@.
  -> m ()
glMultiTexParameterIivEXT v1 v2 v3 v4 = liftIO $ dyn321 ptr_glMultiTexParameterIivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexParameterIivEXT #-}
ptr_glMultiTexParameterIivEXT :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glMultiTexParameterIivEXT = unsafePerformIO $ getCommand "glMultiTexParameterIivEXT"

-- glMultiTexParameterIuivEXT --------------------------------------------------

glMultiTexParameterIuivEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type @TextureUnit@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [TextureParameterName](Graphics-GL-Groups.html#TextureParameterName).
  -> Ptr GLuint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLuint@.
  -> m ()
glMultiTexParameterIuivEXT v1 v2 v3 v4 = liftIO $ dyn358 ptr_glMultiTexParameterIuivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexParameterIuivEXT #-}
ptr_glMultiTexParameterIuivEXT :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLuint -> IO ())
ptr_glMultiTexParameterIuivEXT = unsafePerformIO $ getCommand "glMultiTexParameterIuivEXT"

-- glMultiTexParameterfEXT -----------------------------------------------------

-- | The vector equivalent of this command is 'glMultiTexParameterfvEXT'.
glMultiTexParameterfEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type @TextureUnit@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [TextureParameterName](Graphics-GL-Groups.html#TextureParameterName).
  -> GLfloat -- ^ @param@ of type @CheckedFloat32@.
  -> m ()
glMultiTexParameterfEXT v1 v2 v3 v4 = liftIO $ dyn562 ptr_glMultiTexParameterfEXT v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexParameterfEXT #-}
ptr_glMultiTexParameterfEXT :: FunPtr (GLenum -> GLenum -> GLenum -> GLfloat -> IO ())
ptr_glMultiTexParameterfEXT = unsafePerformIO $ getCommand "glMultiTexParameterfEXT"

-- glMultiTexParameterfvEXT ----------------------------------------------------

glMultiTexParameterfvEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type @TextureUnit@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [TextureParameterName](Graphics-GL-Groups.html#TextureParameterName).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedFloat32@.
  -> m ()
glMultiTexParameterfvEXT v1 v2 v3 v4 = liftIO $ dyn320 ptr_glMultiTexParameterfvEXT v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexParameterfvEXT #-}
ptr_glMultiTexParameterfvEXT :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glMultiTexParameterfvEXT = unsafePerformIO $ getCommand "glMultiTexParameterfvEXT"

-- glMultiTexParameteriEXT -----------------------------------------------------

-- | The vector equivalent of this command is 'glMultiTexParameterivEXT'.
glMultiTexParameteriEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type @TextureUnit@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [TextureParameterName](Graphics-GL-Groups.html#TextureParameterName).
  -> GLint -- ^ @param@ of type @CheckedInt32@.
  -> m ()
glMultiTexParameteriEXT v1 v2 v3 v4 = liftIO $ dyn563 ptr_glMultiTexParameteriEXT v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexParameteriEXT #-}
ptr_glMultiTexParameteriEXT :: FunPtr (GLenum -> GLenum -> GLenum -> GLint -> IO ())
ptr_glMultiTexParameteriEXT = unsafePerformIO $ getCommand "glMultiTexParameteriEXT"

-- glMultiTexParameterivEXT ----------------------------------------------------

glMultiTexParameterivEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type @TextureUnit@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [TextureParameterName](Graphics-GL-Groups.html#TextureParameterName).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedInt32@.
  -> m ()
glMultiTexParameterivEXT v1 v2 v3 v4 = liftIO $ dyn321 ptr_glMultiTexParameterivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexParameterivEXT #-}
ptr_glMultiTexParameterivEXT :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glMultiTexParameterivEXT = unsafePerformIO $ getCommand "glMultiTexParameterivEXT"

-- glMultiTexRenderbufferEXT ---------------------------------------------------

glMultiTexRenderbufferEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type @TextureUnit@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLuint -- ^ @renderbuffer@.
  -> m ()
glMultiTexRenderbufferEXT v1 v2 v3 = liftIO $ dyn29 ptr_glMultiTexRenderbufferEXT v1 v2 v3

{-# NOINLINE ptr_glMultiTexRenderbufferEXT #-}
ptr_glMultiTexRenderbufferEXT :: FunPtr (GLenum -> GLenum -> GLuint -> IO ())
ptr_glMultiTexRenderbufferEXT = unsafePerformIO $ getCommand "glMultiTexRenderbufferEXT"

-- glMultiTexSubImage1DEXT -----------------------------------------------------

glMultiTexSubImage1DEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type @TextureUnit@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLsizei -- ^ @width@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @pixels@ pointing to @COMPSIZE(format,type,width)@ elements of type @a@.
  -> m ()
glMultiTexSubImage1DEXT v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn568 ptr_glMultiTexSubImage1DEXT v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glMultiTexSubImage1DEXT #-}
ptr_glMultiTexSubImage1DEXT :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glMultiTexSubImage1DEXT = unsafePerformIO $ getCommand "glMultiTexSubImage1DEXT"

-- glMultiTexSubImage2DEXT -----------------------------------------------------

glMultiTexSubImage2DEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type @TextureUnit@.
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
glMultiTexSubImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO $ dyn569 ptr_glMultiTexSubImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10

{-# NOINLINE ptr_glMultiTexSubImage2DEXT #-}
ptr_glMultiTexSubImage2DEXT :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glMultiTexSubImage2DEXT = unsafePerformIO $ getCommand "glMultiTexSubImage2DEXT"

-- glMultiTexSubImage3DEXT -----------------------------------------------------

glMultiTexSubImage3DEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type @TextureUnit@.
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
glMultiTexSubImage3DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 = liftIO $ dyn570 ptr_glMultiTexSubImage3DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12

{-# NOINLINE ptr_glMultiTexSubImage3DEXT #-}
ptr_glMultiTexSubImage3DEXT :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glMultiTexSubImage3DEXT = unsafePerformIO $ getCommand "glMultiTexSubImage3DEXT"

-- glNamedBufferData -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glBufferData.xhtml OpenGL 4.x>.
glNamedBufferData
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLsizeiptr -- ^ @size@ of type @BufferSize@.
  -> Ptr a -- ^ @data@.
  -> GLenum -- ^ @usage@.
  -> m ()
glNamedBufferData v1 v2 v3 v4 = liftIO $ dyn571 ptr_glNamedBufferData v1 v2 v3 v4

{-# NOINLINE ptr_glNamedBufferData #-}
ptr_glNamedBufferData :: FunPtr (GLuint -> GLsizeiptr -> Ptr a -> GLenum -> IO ())
ptr_glNamedBufferData = unsafePerformIO $ getCommand "glNamedBufferData"

-- glNamedBufferDataEXT --------------------------------------------------------

glNamedBufferDataEXT
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLsizeiptr -- ^ @size@.
  -> Ptr a -- ^ @data@ pointing to @COMPSIZE(size)@ elements of type @a@.
  -> GLenum -- ^ @usage@ of type @VertexBufferObjectUsage@.
  -> m ()
glNamedBufferDataEXT v1 v2 v3 v4 = liftIO $ dyn571 ptr_glNamedBufferDataEXT v1 v2 v3 v4

{-# NOINLINE ptr_glNamedBufferDataEXT #-}
ptr_glNamedBufferDataEXT :: FunPtr (GLuint -> GLsizeiptr -> Ptr a -> GLenum -> IO ())
ptr_glNamedBufferDataEXT = unsafePerformIO $ getCommand "glNamedBufferDataEXT"

-- glNamedBufferPageCommitmentARB ----------------------------------------------

glNamedBufferPageCommitmentARB
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLintptr -- ^ @offset@.
  -> GLsizeiptr -- ^ @size@.
  -> GLboolean -- ^ @commit@.
  -> m ()
glNamedBufferPageCommitmentARB v1 v2 v3 v4 = liftIO $ dyn572 ptr_glNamedBufferPageCommitmentARB v1 v2 v3 v4

{-# NOINLINE ptr_glNamedBufferPageCommitmentARB #-}
ptr_glNamedBufferPageCommitmentARB :: FunPtr (GLuint -> GLintptr -> GLsizeiptr -> GLboolean -> IO ())
ptr_glNamedBufferPageCommitmentARB = unsafePerformIO $ getCommand "glNamedBufferPageCommitmentARB"

-- glNamedBufferPageCommitmentEXT ----------------------------------------------

glNamedBufferPageCommitmentEXT
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLintptr -- ^ @offset@.
  -> GLsizeiptr -- ^ @size@.
  -> GLboolean -- ^ @commit@.
  -> m ()
glNamedBufferPageCommitmentEXT v1 v2 v3 v4 = liftIO $ dyn572 ptr_glNamedBufferPageCommitmentEXT v1 v2 v3 v4

{-# NOINLINE ptr_glNamedBufferPageCommitmentEXT #-}
ptr_glNamedBufferPageCommitmentEXT :: FunPtr (GLuint -> GLintptr -> GLsizeiptr -> GLboolean -> IO ())
ptr_glNamedBufferPageCommitmentEXT = unsafePerformIO $ getCommand "glNamedBufferPageCommitmentEXT"

-- glNamedBufferStorage --------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glBufferStorage.xhtml OpenGL 4.x>.
glNamedBufferStorage
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLsizeiptr -- ^ @size@ of type @BufferSize@.
  -> Ptr a -- ^ @data@ pointing to @size@ elements of type @a@.
  -> GLbitfield -- ^ @flags@.
  -> m ()
glNamedBufferStorage v1 v2 v3 v4 = liftIO $ dyn573 ptr_glNamedBufferStorage v1 v2 v3 v4

{-# NOINLINE ptr_glNamedBufferStorage #-}
ptr_glNamedBufferStorage :: FunPtr (GLuint -> GLsizeiptr -> Ptr a -> GLbitfield -> IO ())
ptr_glNamedBufferStorage = unsafePerformIO $ getCommand "glNamedBufferStorage"

-- glNamedBufferStorageEXT -----------------------------------------------------

-- | This command is an alias for 'glNamedBufferStorage'.
glNamedBufferStorageEXT
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLsizeiptr -- ^ @size@ of type @BufferSize@.
  -> Ptr a -- ^ @data@ pointing to @size@ elements of type @a@.
  -> GLbitfield -- ^ @flags@.
  -> m ()
glNamedBufferStorageEXT v1 v2 v3 v4 = liftIO $ dyn573 ptr_glNamedBufferStorageEXT v1 v2 v3 v4

{-# NOINLINE ptr_glNamedBufferStorageEXT #-}
ptr_glNamedBufferStorageEXT :: FunPtr (GLuint -> GLsizeiptr -> Ptr a -> GLbitfield -> IO ())
ptr_glNamedBufferStorageEXT = unsafePerformIO $ getCommand "glNamedBufferStorageEXT"

-- glNamedBufferSubData --------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glBufferSubData.xhtml OpenGL 4.x>.
glNamedBufferSubData
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLintptr -- ^ @offset@.
  -> GLsizeiptr -- ^ @size@ of type @BufferSize@.
  -> Ptr a -- ^ @data@ pointing to @COMPSIZE(size)@ elements of type @a@.
  -> m ()
glNamedBufferSubData v1 v2 v3 v4 = liftIO $ dyn362 ptr_glNamedBufferSubData v1 v2 v3 v4

{-# NOINLINE ptr_glNamedBufferSubData #-}
ptr_glNamedBufferSubData :: FunPtr (GLuint -> GLintptr -> GLsizeiptr -> Ptr a -> IO ())
ptr_glNamedBufferSubData = unsafePerformIO $ getCommand "glNamedBufferSubData"

-- glNamedBufferSubDataEXT -----------------------------------------------------

-- | This command is an alias for 'glNamedBufferSubData'.
glNamedBufferSubDataEXT
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLintptr -- ^ @offset@.
  -> GLsizeiptr -- ^ @size@ of type @BufferSize@.
  -> Ptr a -- ^ @data@ pointing to @COMPSIZE(size)@ elements of type @a@.
  -> m ()
glNamedBufferSubDataEXT v1 v2 v3 v4 = liftIO $ dyn362 ptr_glNamedBufferSubDataEXT v1 v2 v3 v4

{-# NOINLINE ptr_glNamedBufferSubDataEXT #-}
ptr_glNamedBufferSubDataEXT :: FunPtr (GLuint -> GLintptr -> GLsizeiptr -> Ptr a -> IO ())
ptr_glNamedBufferSubDataEXT = unsafePerformIO $ getCommand "glNamedBufferSubDataEXT"

-- glNamedCopyBufferSubDataEXT -------------------------------------------------

glNamedCopyBufferSubDataEXT
  :: MonadIO m
  => GLuint -- ^ @readBuffer@.
  -> GLuint -- ^ @writeBuffer@.
  -> GLintptr -- ^ @readOffset@.
  -> GLintptr -- ^ @writeOffset@.
  -> GLsizeiptr -- ^ @size@.
  -> m ()
glNamedCopyBufferSubDataEXT v1 v2 v3 v4 v5 = liftIO $ dyn174 ptr_glNamedCopyBufferSubDataEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glNamedCopyBufferSubDataEXT #-}
ptr_glNamedCopyBufferSubDataEXT :: FunPtr (GLuint -> GLuint -> GLintptr -> GLintptr -> GLsizeiptr -> IO ())
ptr_glNamedCopyBufferSubDataEXT = unsafePerformIO $ getCommand "glNamedCopyBufferSubDataEXT"

-- glNamedFramebufferDrawBuffer ------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glDrawBuffer.xhtml OpenGL 4.x>.
glNamedFramebufferDrawBuffer
  :: MonadIO m
  => GLuint -- ^ @framebuffer@.
  -> GLenum -- ^ @buf@.
  -> m ()
glNamedFramebufferDrawBuffer v1 v2 = liftIO $ dyn15 ptr_glNamedFramebufferDrawBuffer v1 v2

{-# NOINLINE ptr_glNamedFramebufferDrawBuffer #-}
ptr_glNamedFramebufferDrawBuffer :: FunPtr (GLuint -> GLenum -> IO ())
ptr_glNamedFramebufferDrawBuffer = unsafePerformIO $ getCommand "glNamedFramebufferDrawBuffer"

-- glNamedFramebufferDrawBuffers -----------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glDrawBuffers.xhtml OpenGL 4.x>.
glNamedFramebufferDrawBuffers
  :: MonadIO m
  => GLuint -- ^ @framebuffer@.
  -> GLsizei -- ^ @n@.
  -> Ptr GLenum -- ^ @bufs@.
  -> m ()
glNamedFramebufferDrawBuffers v1 v2 v3 = liftIO $ dyn282 ptr_glNamedFramebufferDrawBuffers v1 v2 v3

{-# NOINLINE ptr_glNamedFramebufferDrawBuffers #-}
ptr_glNamedFramebufferDrawBuffers :: FunPtr (GLuint -> GLsizei -> Ptr GLenum -> IO ())
ptr_glNamedFramebufferDrawBuffers = unsafePerformIO $ getCommand "glNamedFramebufferDrawBuffers"

-- glNamedFramebufferParameteri ------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glFramebufferParameteri.xhtml OpenGL 4.x>.
glNamedFramebufferParameteri
  :: MonadIO m
  => GLuint -- ^ @framebuffer@.
  -> GLenum -- ^ @pname@.
  -> GLint -- ^ @param@.
  -> m ()
glNamedFramebufferParameteri v1 v2 v3 = liftIO $ dyn488 ptr_glNamedFramebufferParameteri v1 v2 v3

{-# NOINLINE ptr_glNamedFramebufferParameteri #-}
ptr_glNamedFramebufferParameteri :: FunPtr (GLuint -> GLenum -> GLint -> IO ())
ptr_glNamedFramebufferParameteri = unsafePerformIO $ getCommand "glNamedFramebufferParameteri"

-- glNamedFramebufferParameteriEXT ---------------------------------------------

glNamedFramebufferParameteriEXT
  :: MonadIO m
  => GLuint -- ^ @framebuffer@ of type @Framebuffer@.
  -> GLenum -- ^ @pname@ of type @FramebufferParameterName@.
  -> GLint -- ^ @param@.
  -> m ()
glNamedFramebufferParameteriEXT v1 v2 v3 = liftIO $ dyn488 ptr_glNamedFramebufferParameteriEXT v1 v2 v3

{-# NOINLINE ptr_glNamedFramebufferParameteriEXT #-}
ptr_glNamedFramebufferParameteriEXT :: FunPtr (GLuint -> GLenum -> GLint -> IO ())
ptr_glNamedFramebufferParameteriEXT = unsafePerformIO $ getCommand "glNamedFramebufferParameteriEXT"

-- glNamedFramebufferReadBuffer ------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glReadBuffer.xhtml OpenGL 4.x>.
glNamedFramebufferReadBuffer
  :: MonadIO m
  => GLuint -- ^ @framebuffer@.
  -> GLenum -- ^ @src@.
  -> m ()
glNamedFramebufferReadBuffer v1 v2 = liftIO $ dyn15 ptr_glNamedFramebufferReadBuffer v1 v2

{-# NOINLINE ptr_glNamedFramebufferReadBuffer #-}
ptr_glNamedFramebufferReadBuffer :: FunPtr (GLuint -> GLenum -> IO ())
ptr_glNamedFramebufferReadBuffer = unsafePerformIO $ getCommand "glNamedFramebufferReadBuffer"

-- glNamedFramebufferRenderbuffer ----------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glFramebufferRenderbuffer.xhtml OpenGL 4.x>.
glNamedFramebufferRenderbuffer
  :: MonadIO m
  => GLuint -- ^ @framebuffer@.
  -> GLenum -- ^ @attachment@.
  -> GLenum -- ^ @renderbuffertarget@.
  -> GLuint -- ^ @renderbuffer@.
  -> m ()
glNamedFramebufferRenderbuffer v1 v2 v3 v4 = liftIO $ dyn574 ptr_glNamedFramebufferRenderbuffer v1 v2 v3 v4

{-# NOINLINE ptr_glNamedFramebufferRenderbuffer #-}
ptr_glNamedFramebufferRenderbuffer :: FunPtr (GLuint -> GLenum -> GLenum -> GLuint -> IO ())
ptr_glNamedFramebufferRenderbuffer = unsafePerformIO $ getCommand "glNamedFramebufferRenderbuffer"

-- glNamedFramebufferRenderbufferEXT -------------------------------------------

glNamedFramebufferRenderbufferEXT
  :: MonadIO m
  => GLuint -- ^ @framebuffer@ of type @Framebuffer@.
  -> GLenum -- ^ @attachment@ of type @FramebufferAttachment@.
  -> GLenum -- ^ @renderbuffertarget@ of type @RenderbufferTarget@.
  -> GLuint -- ^ @renderbuffer@ of type @Renderbuffer@.
  -> m ()
glNamedFramebufferRenderbufferEXT v1 v2 v3 v4 = liftIO $ dyn574 ptr_glNamedFramebufferRenderbufferEXT v1 v2 v3 v4

{-# NOINLINE ptr_glNamedFramebufferRenderbufferEXT #-}
ptr_glNamedFramebufferRenderbufferEXT :: FunPtr (GLuint -> GLenum -> GLenum -> GLuint -> IO ())
ptr_glNamedFramebufferRenderbufferEXT = unsafePerformIO $ getCommand "glNamedFramebufferRenderbufferEXT"

-- glNamedFramebufferSampleLocationsfvARB --------------------------------------

glNamedFramebufferSampleLocationsfvARB
  :: MonadIO m
  => GLuint -- ^ @framebuffer@.
  -> GLuint -- ^ @start@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @v@.
  -> m ()
glNamedFramebufferSampleLocationsfvARB v1 v2 v3 v4 = liftIO $ dyn575 ptr_glNamedFramebufferSampleLocationsfvARB v1 v2 v3 v4

{-# NOINLINE ptr_glNamedFramebufferSampleLocationsfvARB #-}
ptr_glNamedFramebufferSampleLocationsfvARB :: FunPtr (GLuint -> GLuint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glNamedFramebufferSampleLocationsfvARB = unsafePerformIO $ getCommand "glNamedFramebufferSampleLocationsfvARB"

-- glNamedFramebufferSampleLocationsfvNV ---------------------------------------

glNamedFramebufferSampleLocationsfvNV
  :: MonadIO m
  => GLuint -- ^ @framebuffer@.
  -> GLuint -- ^ @start@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @v@.
  -> m ()
glNamedFramebufferSampleLocationsfvNV v1 v2 v3 v4 = liftIO $ dyn575 ptr_glNamedFramebufferSampleLocationsfvNV v1 v2 v3 v4

{-# NOINLINE ptr_glNamedFramebufferSampleLocationsfvNV #-}
ptr_glNamedFramebufferSampleLocationsfvNV :: FunPtr (GLuint -> GLuint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glNamedFramebufferSampleLocationsfvNV = unsafePerformIO $ getCommand "glNamedFramebufferSampleLocationsfvNV"

-- glNamedFramebufferTexture ---------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glFramebufferTexture.xhtml OpenGL 4.x>.
glNamedFramebufferTexture
  :: MonadIO m
  => GLuint -- ^ @framebuffer@.
  -> GLenum -- ^ @attachment@.
  -> GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> m ()
glNamedFramebufferTexture v1 v2 v3 v4 = liftIO $ dyn576 ptr_glNamedFramebufferTexture v1 v2 v3 v4

{-# NOINLINE ptr_glNamedFramebufferTexture #-}
ptr_glNamedFramebufferTexture :: FunPtr (GLuint -> GLenum -> GLuint -> GLint -> IO ())
ptr_glNamedFramebufferTexture = unsafePerformIO $ getCommand "glNamedFramebufferTexture"

-- glNamedFramebufferTexture1DEXT ----------------------------------------------

glNamedFramebufferTexture1DEXT
  :: MonadIO m
  => GLuint -- ^ @framebuffer@ of type @Framebuffer@.
  -> GLenum -- ^ @attachment@ of type @FramebufferAttachment@.
  -> GLenum -- ^ @textarget@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLuint -- ^ @texture@ of type @Texture@.
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> m ()
glNamedFramebufferTexture1DEXT v1 v2 v3 v4 v5 = liftIO $ dyn577 ptr_glNamedFramebufferTexture1DEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glNamedFramebufferTexture1DEXT #-}
ptr_glNamedFramebufferTexture1DEXT :: FunPtr (GLuint -> GLenum -> GLenum -> GLuint -> GLint -> IO ())
ptr_glNamedFramebufferTexture1DEXT = unsafePerformIO $ getCommand "glNamedFramebufferTexture1DEXT"

-- glNamedFramebufferTexture2DEXT ----------------------------------------------

glNamedFramebufferTexture2DEXT
  :: MonadIO m
  => GLuint -- ^ @framebuffer@ of type @Framebuffer@.
  -> GLenum -- ^ @attachment@ of type @FramebufferAttachment@.
  -> GLenum -- ^ @textarget@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLuint -- ^ @texture@ of type @Texture@.
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> m ()
glNamedFramebufferTexture2DEXT v1 v2 v3 v4 v5 = liftIO $ dyn577 ptr_glNamedFramebufferTexture2DEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glNamedFramebufferTexture2DEXT #-}
ptr_glNamedFramebufferTexture2DEXT :: FunPtr (GLuint -> GLenum -> GLenum -> GLuint -> GLint -> IO ())
ptr_glNamedFramebufferTexture2DEXT = unsafePerformIO $ getCommand "glNamedFramebufferTexture2DEXT"

-- glNamedFramebufferTexture3DEXT ----------------------------------------------

glNamedFramebufferTexture3DEXT
  :: MonadIO m
  => GLuint -- ^ @framebuffer@ of type @Framebuffer@.
  -> GLenum -- ^ @attachment@ of type @FramebufferAttachment@.
  -> GLenum -- ^ @textarget@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLuint -- ^ @texture@ of type @Texture@.
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @zoffset@ of type @CheckedInt32@.
  -> m ()
glNamedFramebufferTexture3DEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn578 ptr_glNamedFramebufferTexture3DEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glNamedFramebufferTexture3DEXT #-}
ptr_glNamedFramebufferTexture3DEXT :: FunPtr (GLuint -> GLenum -> GLenum -> GLuint -> GLint -> GLint -> IO ())
ptr_glNamedFramebufferTexture3DEXT = unsafePerformIO $ getCommand "glNamedFramebufferTexture3DEXT"

-- glNamedFramebufferTextureEXT ------------------------------------------------

glNamedFramebufferTextureEXT
  :: MonadIO m
  => GLuint -- ^ @framebuffer@ of type @Framebuffer@.
  -> GLenum -- ^ @attachment@ of type @FramebufferAttachment@.
  -> GLuint -- ^ @texture@ of type @Texture@.
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> m ()
glNamedFramebufferTextureEXT v1 v2 v3 v4 = liftIO $ dyn576 ptr_glNamedFramebufferTextureEXT v1 v2 v3 v4

{-# NOINLINE ptr_glNamedFramebufferTextureEXT #-}
ptr_glNamedFramebufferTextureEXT :: FunPtr (GLuint -> GLenum -> GLuint -> GLint -> IO ())
ptr_glNamedFramebufferTextureEXT = unsafePerformIO $ getCommand "glNamedFramebufferTextureEXT"

-- glNamedFramebufferTextureFaceEXT --------------------------------------------

glNamedFramebufferTextureFaceEXT
  :: MonadIO m
  => GLuint -- ^ @framebuffer@ of type @Framebuffer@.
  -> GLenum -- ^ @attachment@ of type @FramebufferAttachment@.
  -> GLuint -- ^ @texture@ of type @Texture@.
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @face@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> m ()
glNamedFramebufferTextureFaceEXT v1 v2 v3 v4 v5 = liftIO $ dyn579 ptr_glNamedFramebufferTextureFaceEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glNamedFramebufferTextureFaceEXT #-}
ptr_glNamedFramebufferTextureFaceEXT :: FunPtr (GLuint -> GLenum -> GLuint -> GLint -> GLenum -> IO ())
ptr_glNamedFramebufferTextureFaceEXT = unsafePerformIO $ getCommand "glNamedFramebufferTextureFaceEXT"

-- glNamedFramebufferTextureLayer ----------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glFramebufferTextureLayer.xhtml OpenGL 4.x>.
glNamedFramebufferTextureLayer
  :: MonadIO m
  => GLuint -- ^ @framebuffer@.
  -> GLenum -- ^ @attachment@.
  -> GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLint -- ^ @layer@.
  -> m ()
glNamedFramebufferTextureLayer v1 v2 v3 v4 v5 = liftIO $ dyn580 ptr_glNamedFramebufferTextureLayer v1 v2 v3 v4 v5

{-# NOINLINE ptr_glNamedFramebufferTextureLayer #-}
ptr_glNamedFramebufferTextureLayer :: FunPtr (GLuint -> GLenum -> GLuint -> GLint -> GLint -> IO ())
ptr_glNamedFramebufferTextureLayer = unsafePerformIO $ getCommand "glNamedFramebufferTextureLayer"

-- glNamedFramebufferTextureLayerEXT -------------------------------------------

glNamedFramebufferTextureLayerEXT
  :: MonadIO m
  => GLuint -- ^ @framebuffer@ of type @Framebuffer@.
  -> GLenum -- ^ @attachment@ of type @FramebufferAttachment@.
  -> GLuint -- ^ @texture@ of type @Texture@.
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @layer@ of type @CheckedInt32@.
  -> m ()
glNamedFramebufferTextureLayerEXT v1 v2 v3 v4 v5 = liftIO $ dyn580 ptr_glNamedFramebufferTextureLayerEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glNamedFramebufferTextureLayerEXT #-}
ptr_glNamedFramebufferTextureLayerEXT :: FunPtr (GLuint -> GLenum -> GLuint -> GLint -> GLint -> IO ())
ptr_glNamedFramebufferTextureLayerEXT = unsafePerformIO $ getCommand "glNamedFramebufferTextureLayerEXT"

-- glNamedProgramLocalParameter4dEXT -------------------------------------------

-- | The vector equivalent of this command is 'glNamedProgramLocalParameter4dvEXT'.
glNamedProgramLocalParameter4dEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @target@ of type @ProgramTarget@.
  -> GLuint -- ^ @index@.
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> GLdouble -- ^ @z@.
  -> GLdouble -- ^ @w@.
  -> m ()
glNamedProgramLocalParameter4dEXT v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn581 ptr_glNamedProgramLocalParameter4dEXT v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glNamedProgramLocalParameter4dEXT #-}
ptr_glNamedProgramLocalParameter4dEXT :: FunPtr (GLuint -> GLenum -> GLuint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glNamedProgramLocalParameter4dEXT = unsafePerformIO $ getCommand "glNamedProgramLocalParameter4dEXT"

-- glNamedProgramLocalParameter4dvEXT ------------------------------------------

glNamedProgramLocalParameter4dvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @target@ of type @ProgramTarget@.
  -> GLuint -- ^ @index@.
  -> Ptr GLdouble -- ^ @params@ pointing to @4@ elements of type @GLdouble@.
  -> m ()
glNamedProgramLocalParameter4dvEXT v1 v2 v3 v4 = liftIO $ dyn366 ptr_glNamedProgramLocalParameter4dvEXT v1 v2 v3 v4

{-# NOINLINE ptr_glNamedProgramLocalParameter4dvEXT #-}
ptr_glNamedProgramLocalParameter4dvEXT :: FunPtr (GLuint -> GLenum -> GLuint -> Ptr GLdouble -> IO ())
ptr_glNamedProgramLocalParameter4dvEXT = unsafePerformIO $ getCommand "glNamedProgramLocalParameter4dvEXT"

-- glNamedProgramLocalParameter4fEXT -------------------------------------------

-- | The vector equivalent of this command is 'glNamedProgramLocalParameter4fvEXT'.
glNamedProgramLocalParameter4fEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @target@ of type @ProgramTarget@.
  -> GLuint -- ^ @index@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> GLfloat -- ^ @w@.
  -> m ()
glNamedProgramLocalParameter4fEXT v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn582 ptr_glNamedProgramLocalParameter4fEXT v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glNamedProgramLocalParameter4fEXT #-}
ptr_glNamedProgramLocalParameter4fEXT :: FunPtr (GLuint -> GLenum -> GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glNamedProgramLocalParameter4fEXT = unsafePerformIO $ getCommand "glNamedProgramLocalParameter4fEXT"

-- glNamedProgramLocalParameter4fvEXT ------------------------------------------

glNamedProgramLocalParameter4fvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @target@ of type @ProgramTarget@.
  -> GLuint -- ^ @index@.
  -> Ptr GLfloat -- ^ @params@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glNamedProgramLocalParameter4fvEXT v1 v2 v3 v4 = liftIO $ dyn367 ptr_glNamedProgramLocalParameter4fvEXT v1 v2 v3 v4

{-# NOINLINE ptr_glNamedProgramLocalParameter4fvEXT #-}
ptr_glNamedProgramLocalParameter4fvEXT :: FunPtr (GLuint -> GLenum -> GLuint -> Ptr GLfloat -> IO ())
ptr_glNamedProgramLocalParameter4fvEXT = unsafePerformIO $ getCommand "glNamedProgramLocalParameter4fvEXT"

