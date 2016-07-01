--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F09
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

module Graphics.GL.Functions.F09 (
  glCompressedTexImage3DOES,
  glCompressedTexSubImage1D,
  glCompressedTexSubImage1DARB,
  glCompressedTexSubImage2D,
  glCompressedTexSubImage2DARB,
  glCompressedTexSubImage3D,
  glCompressedTexSubImage3DARB,
  glCompressedTexSubImage3DOES,
  glCompressedTextureImage1DEXT,
  glCompressedTextureImage2DEXT,
  glCompressedTextureImage3DEXT,
  glCompressedTextureSubImage1D,
  glCompressedTextureSubImage1DEXT,
  glCompressedTextureSubImage2D,
  glCompressedTextureSubImage2DEXT,
  glCompressedTextureSubImage3D,
  glCompressedTextureSubImage3DEXT,
  glConservativeRasterParameterfNV,
  glConvolutionFilter1D,
  glConvolutionFilter1DEXT,
  glConvolutionFilter2D,
  glConvolutionFilter2DEXT,
  glConvolutionParameterf,
  glConvolutionParameterfEXT,
  glConvolutionParameterfv,
  glConvolutionParameterfvEXT,
  glConvolutionParameteri,
  glConvolutionParameteriEXT,
  glConvolutionParameteriv,
  glConvolutionParameterivEXT,
  glConvolutionParameterxOES,
  glConvolutionParameterxvOES,
  glCopyBufferSubData,
  glCopyBufferSubDataNV,
  glCopyColorSubTable,
  glCopyColorSubTableEXT,
  glCopyColorTable,
  glCopyColorTableSGI,
  glCopyConvolutionFilter1D,
  glCopyConvolutionFilter1DEXT
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

-- glCompressedTexImage3DOES ---------------------------------------------------

-- | This command is an alias for 'glCompressedTexImage3D'.
glCompressedTexImage3DOES
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLint -- ^ @level@.
  -> GLenum -- ^ @internalformat@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLint -- ^ @border@.
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @data@ pointing to @imageSize@ elements of type @a@.
  -> m ()
glCompressedTexImage3DOES v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn147 ptr_glCompressedTexImage3DOES v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glCompressedTexImage3DOES #-}
ptr_glCompressedTexImage3DOES :: FunPtr (GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedTexImage3DOES = unsafePerformIO $ getCommand "glCompressedTexImage3DOES"

-- glCompressedTexSubImage1D ---------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glCompressedTexSubImage1D.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glCompressedTexSubImage1D.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glCompressedTexSubImage1D.xhtml OpenGL 4.x>.
glCompressedTexSubImage1D
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLsizei -- ^ @width@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @data@ pointing to @imageSize@ elements of type @CompressedTextureARB@.
  -> m ()
glCompressedTexSubImage1D v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn148 ptr_glCompressedTexSubImage1D v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glCompressedTexSubImage1D #-}
ptr_glCompressedTexSubImage1D :: FunPtr (GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedTexSubImage1D = unsafePerformIO $ getCommand "glCompressedTexSubImage1D"

-- glCompressedTexSubImage1DARB ------------------------------------------------

-- | This command is an alias for 'glCompressedTexSubImage1D'.
glCompressedTexSubImage1DARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLsizei -- ^ @width@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @data@ pointing to @imageSize@ elements of type @CompressedTextureARB@.
  -> m ()
glCompressedTexSubImage1DARB v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn148 ptr_glCompressedTexSubImage1DARB v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glCompressedTexSubImage1DARB #-}
ptr_glCompressedTexSubImage1DARB :: FunPtr (GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedTexSubImage1DARB = unsafePerformIO $ getCommand "glCompressedTexSubImage1DARB"

-- glCompressedTexSubImage2D ---------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glCompressedTexSubImage2D.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glCompressedTexSubImage2D.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glCompressedTexSubImage2D.xhtml OpenGL 4.x>.
glCompressedTexSubImage2D
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @yoffset@ of type @CheckedInt32@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @data@ pointing to @imageSize@ elements of type @CompressedTextureARB@.
  -> m ()
glCompressedTexSubImage2D v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn149 ptr_glCompressedTexSubImage2D v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glCompressedTexSubImage2D #-}
ptr_glCompressedTexSubImage2D :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedTexSubImage2D = unsafePerformIO $ getCommand "glCompressedTexSubImage2D"

-- glCompressedTexSubImage2DARB ------------------------------------------------

-- | This command is an alias for 'glCompressedTexSubImage2D'.
glCompressedTexSubImage2DARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @yoffset@ of type @CheckedInt32@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @data@ pointing to @imageSize@ elements of type @CompressedTextureARB@.
  -> m ()
glCompressedTexSubImage2DARB v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn149 ptr_glCompressedTexSubImage2DARB v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glCompressedTexSubImage2DARB #-}
ptr_glCompressedTexSubImage2DARB :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedTexSubImage2DARB = unsafePerformIO $ getCommand "glCompressedTexSubImage2DARB"

-- glCompressedTexSubImage3D ---------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glCompressedTexSubImage3D.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glCompressedTexSubImage3D.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glCompressedTexSubImage3D.xhtml OpenGL 4.x>.
glCompressedTexSubImage3D
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @yoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @zoffset@ of type @CheckedInt32@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @data@ pointing to @imageSize@ elements of type @CompressedTextureARB@.
  -> m ()
glCompressedTexSubImage3D v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 = liftIO $ dyn150 ptr_glCompressedTexSubImage3D v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11

{-# NOINLINE ptr_glCompressedTexSubImage3D #-}
ptr_glCompressedTexSubImage3D :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedTexSubImage3D = unsafePerformIO $ getCommand "glCompressedTexSubImage3D"

-- glCompressedTexSubImage3DARB ------------------------------------------------

-- | This command is an alias for 'glCompressedTexSubImage3D'.
glCompressedTexSubImage3DARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @yoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @zoffset@ of type @CheckedInt32@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @data@ pointing to @imageSize@ elements of type @CompressedTextureARB@.
  -> m ()
glCompressedTexSubImage3DARB v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 = liftIO $ dyn150 ptr_glCompressedTexSubImage3DARB v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11

{-# NOINLINE ptr_glCompressedTexSubImage3DARB #-}
ptr_glCompressedTexSubImage3DARB :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedTexSubImage3DARB = unsafePerformIO $ getCommand "glCompressedTexSubImage3DARB"

-- glCompressedTexSubImage3DOES ------------------------------------------------

-- | This command is an alias for 'glCompressedTexSubImage3D'.
glCompressedTexSubImage3DOES
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLint -- ^ @level@.
  -> GLint -- ^ @xoffset@.
  -> GLint -- ^ @yoffset@.
  -> GLint -- ^ @zoffset@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLenum -- ^ @format@.
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @data@ pointing to @imageSize@ elements of type @a@.
  -> m ()
glCompressedTexSubImage3DOES v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 = liftIO $ dyn150 ptr_glCompressedTexSubImage3DOES v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11

{-# NOINLINE ptr_glCompressedTexSubImage3DOES #-}
ptr_glCompressedTexSubImage3DOES :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedTexSubImage3DOES = unsafePerformIO $ getCommand "glCompressedTexSubImage3DOES"

-- glCompressedTextureImage1DEXT -----------------------------------------------

glCompressedTextureImage1DEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @internalformat@ of type @TextureInternalFormat@.
  -> GLsizei -- ^ @width@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @bits@ pointing to @imageSize@ elements of type @a@.
  -> m ()
glCompressedTextureImage1DEXT v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn151 ptr_glCompressedTextureImage1DEXT v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glCompressedTextureImage1DEXT #-}
ptr_glCompressedTextureImage1DEXT :: FunPtr (GLuint -> GLenum -> GLint -> GLenum -> GLsizei -> GLint -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedTextureImage1DEXT = unsafePerformIO $ getCommand "glCompressedTextureImage1DEXT"

-- glCompressedTextureImage2DEXT -----------------------------------------------

glCompressedTextureImage2DEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @internalformat@ of type @TextureInternalFormat@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @bits@ pointing to @imageSize@ elements of type @a@.
  -> m ()
glCompressedTextureImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn152 ptr_glCompressedTextureImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glCompressedTextureImage2DEXT #-}
ptr_glCompressedTextureImage2DEXT :: FunPtr (GLuint -> GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedTextureImage2DEXT = unsafePerformIO $ getCommand "glCompressedTextureImage2DEXT"

-- glCompressedTextureImage3DEXT -----------------------------------------------

glCompressedTextureImage3DEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @internalformat@ of type @TextureInternalFormat@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @bits@ pointing to @imageSize@ elements of type @a@.
  -> m ()
glCompressedTextureImage3DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO $ dyn153 ptr_glCompressedTextureImage3DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10

{-# NOINLINE ptr_glCompressedTextureImage3DEXT #-}
ptr_glCompressedTextureImage3DEXT :: FunPtr (GLuint -> GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedTextureImage3DEXT = unsafePerformIO $ getCommand "glCompressedTextureImage3DEXT"

-- glCompressedTextureSubImage1D -----------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glCompressedTexSubImage1D.xhtml OpenGL 4.x>.
glCompressedTextureSubImage1D
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLint -- ^ @xoffset@.
  -> GLsizei -- ^ @width@.
  -> GLenum -- ^ @format@.
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @data@.
  -> m ()
glCompressedTextureSubImage1D v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn154 ptr_glCompressedTextureSubImage1D v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glCompressedTextureSubImage1D #-}
ptr_glCompressedTextureSubImage1D :: FunPtr (GLuint -> GLint -> GLint -> GLsizei -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedTextureSubImage1D = unsafePerformIO $ getCommand "glCompressedTextureSubImage1D"

-- glCompressedTextureSubImage1DEXT --------------------------------------------

glCompressedTextureSubImage1DEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLsizei -- ^ @width@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @bits@ pointing to @imageSize@ elements of type @a@.
  -> m ()
glCompressedTextureSubImage1DEXT v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn155 ptr_glCompressedTextureSubImage1DEXT v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glCompressedTextureSubImage1DEXT #-}
ptr_glCompressedTextureSubImage1DEXT :: FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedTextureSubImage1DEXT = unsafePerformIO $ getCommand "glCompressedTextureSubImage1DEXT"

-- glCompressedTextureSubImage2D -----------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glCompressedTexSubImage2D.xhtml OpenGL 4.x>.
glCompressedTextureSubImage2D
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLint -- ^ @xoffset@.
  -> GLint -- ^ @yoffset@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLenum -- ^ @format@.
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @data@.
  -> m ()
glCompressedTextureSubImage2D v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn156 ptr_glCompressedTextureSubImage2D v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glCompressedTextureSubImage2D #-}
ptr_glCompressedTextureSubImage2D :: FunPtr (GLuint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedTextureSubImage2D = unsafePerformIO $ getCommand "glCompressedTextureSubImage2D"

-- glCompressedTextureSubImage2DEXT --------------------------------------------

glCompressedTextureSubImage2DEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @yoffset@ of type @CheckedInt32@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @bits@ pointing to @imageSize@ elements of type @a@.
  -> m ()
glCompressedTextureSubImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO $ dyn157 ptr_glCompressedTextureSubImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10

{-# NOINLINE ptr_glCompressedTextureSubImage2DEXT #-}
ptr_glCompressedTextureSubImage2DEXT :: FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedTextureSubImage2DEXT = unsafePerformIO $ getCommand "glCompressedTextureSubImage2DEXT"

-- glCompressedTextureSubImage3D -----------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glCompressedTexSubImage3D.xhtml OpenGL 4.x>.
glCompressedTextureSubImage3D
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
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @data@.
  -> m ()
glCompressedTextureSubImage3D v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 = liftIO $ dyn158 ptr_glCompressedTextureSubImage3D v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11

{-# NOINLINE ptr_glCompressedTextureSubImage3D #-}
ptr_glCompressedTextureSubImage3D :: FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedTextureSubImage3D = unsafePerformIO $ getCommand "glCompressedTextureSubImage3D"

-- glCompressedTextureSubImage3DEXT --------------------------------------------

glCompressedTextureSubImage3DEXT
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
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @bits@ pointing to @imageSize@ elements of type @a@.
  -> m ()
glCompressedTextureSubImage3DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 = liftIO $ dyn159 ptr_glCompressedTextureSubImage3DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12

{-# NOINLINE ptr_glCompressedTextureSubImage3DEXT #-}
ptr_glCompressedTextureSubImage3DEXT :: FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedTextureSubImage3DEXT = unsafePerformIO $ getCommand "glCompressedTextureSubImage3DEXT"

-- glConservativeRasterParameterfNV --------------------------------------------

glConservativeRasterParameterfNV
  :: MonadIO m
  => GLenum -- ^ @pname@.
  -> GLfloat -- ^ @value@.
  -> m ()
glConservativeRasterParameterfNV v1 v2 = liftIO $ dyn0 ptr_glConservativeRasterParameterfNV v1 v2

{-# NOINLINE ptr_glConservativeRasterParameterfNV #-}
ptr_glConservativeRasterParameterfNV :: FunPtr (GLenum -> GLfloat -> IO ())
ptr_glConservativeRasterParameterfNV = unsafePerformIO $ getCommand "glConservativeRasterParameterfNV"

-- glConvolutionFilter1D -------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glConvolutionFilter1D.xml OpenGL 2.x>.
glConvolutionFilter1D
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ConvolutionTarget@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @image@ pointing to @COMPSIZE(format,type,width)@ elements of type @a@.
  -> m ()
glConvolutionFilter1D v1 v2 v3 v4 v5 v6 = liftIO $ dyn131 ptr_glConvolutionFilter1D v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glConvolutionFilter1D #-}
ptr_glConvolutionFilter1D :: FunPtr (GLenum -> GLenum -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glConvolutionFilter1D = unsafePerformIO $ getCommand "glConvolutionFilter1D"

-- glConvolutionFilter1DEXT ----------------------------------------------------

-- | This command is an alias for 'glConvolutionFilter1D'.
glConvolutionFilter1DEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ConvolutionTargetEXT](Graphics-GL-Groups.html#ConvolutionTargetEXT).
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @image@ pointing to @COMPSIZE(format,type,width)@ elements of type @a@.
  -> m ()
glConvolutionFilter1DEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn131 ptr_glConvolutionFilter1DEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glConvolutionFilter1DEXT #-}
ptr_glConvolutionFilter1DEXT :: FunPtr (GLenum -> GLenum -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glConvolutionFilter1DEXT = unsafePerformIO $ getCommand "glConvolutionFilter1DEXT"

-- glConvolutionFilter2D -------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glConvolutionFilter2D.xml OpenGL 2.x>.
glConvolutionFilter2D
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ConvolutionTarget@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @image@ pointing to @COMPSIZE(format,type,width,height)@ elements of type @a@.
  -> m ()
glConvolutionFilter2D v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn160 ptr_glConvolutionFilter2D v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glConvolutionFilter2D #-}
ptr_glConvolutionFilter2D :: FunPtr (GLenum -> GLenum -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glConvolutionFilter2D = unsafePerformIO $ getCommand "glConvolutionFilter2D"

-- glConvolutionFilter2DEXT ----------------------------------------------------

-- | This command is an alias for 'glConvolutionFilter2D'.
glConvolutionFilter2DEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ConvolutionTargetEXT](Graphics-GL-Groups.html#ConvolutionTargetEXT).
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @image@ pointing to @COMPSIZE(format,type,width,height)@ elements of type @a@.
  -> m ()
glConvolutionFilter2DEXT v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn160 ptr_glConvolutionFilter2DEXT v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glConvolutionFilter2DEXT #-}
ptr_glConvolutionFilter2DEXT :: FunPtr (GLenum -> GLenum -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glConvolutionFilter2DEXT = unsafePerformIO $ getCommand "glConvolutionFilter2DEXT"

-- glConvolutionParameterf -----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glConvolutionParameter.xml OpenGL 2.x>.
glConvolutionParameterf
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ConvolutionTarget@.
  -> GLenum -- ^ @pname@ of type @ConvolutionParameter@.
  -> GLfloat -- ^ @params@ of type @CheckedFloat32@.
  -> m ()
glConvolutionParameterf v1 v2 v3 = liftIO $ dyn161 ptr_glConvolutionParameterf v1 v2 v3

{-# NOINLINE ptr_glConvolutionParameterf #-}
ptr_glConvolutionParameterf :: FunPtr (GLenum -> GLenum -> GLfloat -> IO ())
ptr_glConvolutionParameterf = unsafePerformIO $ getCommand "glConvolutionParameterf"

-- glConvolutionParameterfEXT --------------------------------------------------

-- | This command is an alias for 'glConvolutionParameterf'.
glConvolutionParameterfEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ConvolutionTargetEXT](Graphics-GL-Groups.html#ConvolutionTargetEXT).
  -> GLenum -- ^ @pname@ of type [ConvolutionParameterEXT](Graphics-GL-Groups.html#ConvolutionParameterEXT).
  -> GLfloat -- ^ @params@ of type @CheckedFloat32@.
  -> m ()
glConvolutionParameterfEXT v1 v2 v3 = liftIO $ dyn161 ptr_glConvolutionParameterfEXT v1 v2 v3

{-# NOINLINE ptr_glConvolutionParameterfEXT #-}
ptr_glConvolutionParameterfEXT :: FunPtr (GLenum -> GLenum -> GLfloat -> IO ())
ptr_glConvolutionParameterfEXT = unsafePerformIO $ getCommand "glConvolutionParameterfEXT"

-- glConvolutionParameterfv ----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glConvolutionParameter.xml OpenGL 2.x>.
glConvolutionParameterfv
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ConvolutionTarget@.
  -> GLenum -- ^ @pname@ of type @ConvolutionParameter@.
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedFloat32@.
  -> m ()
glConvolutionParameterfv v1 v2 v3 = liftIO $ dyn132 ptr_glConvolutionParameterfv v1 v2 v3

{-# NOINLINE ptr_glConvolutionParameterfv #-}
ptr_glConvolutionParameterfv :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glConvolutionParameterfv = unsafePerformIO $ getCommand "glConvolutionParameterfv"

-- glConvolutionParameterfvEXT -------------------------------------------------

-- | This command is an alias for 'glConvolutionParameterfv'.
glConvolutionParameterfvEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ConvolutionTargetEXT](Graphics-GL-Groups.html#ConvolutionTargetEXT).
  -> GLenum -- ^ @pname@ of type [ConvolutionParameterEXT](Graphics-GL-Groups.html#ConvolutionParameterEXT).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedFloat32@.
  -> m ()
glConvolutionParameterfvEXT v1 v2 v3 = liftIO $ dyn132 ptr_glConvolutionParameterfvEXT v1 v2 v3

{-# NOINLINE ptr_glConvolutionParameterfvEXT #-}
ptr_glConvolutionParameterfvEXT :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glConvolutionParameterfvEXT = unsafePerformIO $ getCommand "glConvolutionParameterfvEXT"

-- glConvolutionParameteri -----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glConvolutionParameter.xml OpenGL 2.x>.
glConvolutionParameteri
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ConvolutionTarget@.
  -> GLenum -- ^ @pname@ of type @ConvolutionParameter@.
  -> GLint -- ^ @params@ of type @CheckedInt32@.
  -> m ()
glConvolutionParameteri v1 v2 v3 = liftIO $ dyn62 ptr_glConvolutionParameteri v1 v2 v3

{-# NOINLINE ptr_glConvolutionParameteri #-}
ptr_glConvolutionParameteri :: FunPtr (GLenum -> GLenum -> GLint -> IO ())
ptr_glConvolutionParameteri = unsafePerformIO $ getCommand "glConvolutionParameteri"

-- glConvolutionParameteriEXT --------------------------------------------------

-- | This command is an alias for 'glConvolutionParameteri'.
glConvolutionParameteriEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ConvolutionTargetEXT](Graphics-GL-Groups.html#ConvolutionTargetEXT).
  -> GLenum -- ^ @pname@ of type [ConvolutionParameterEXT](Graphics-GL-Groups.html#ConvolutionParameterEXT).
  -> GLint -- ^ @params@ of type @CheckedInt32@.
  -> m ()
glConvolutionParameteriEXT v1 v2 v3 = liftIO $ dyn62 ptr_glConvolutionParameteriEXT v1 v2 v3

{-# NOINLINE ptr_glConvolutionParameteriEXT #-}
ptr_glConvolutionParameteriEXT :: FunPtr (GLenum -> GLenum -> GLint -> IO ())
ptr_glConvolutionParameteriEXT = unsafePerformIO $ getCommand "glConvolutionParameteriEXT"

-- glConvolutionParameteriv ----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glConvolutionParameter.xml OpenGL 2.x>.
glConvolutionParameteriv
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ConvolutionTarget@.
  -> GLenum -- ^ @pname@ of type @ConvolutionParameter@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedInt32@.
  -> m ()
glConvolutionParameteriv v1 v2 v3 = liftIO $ dyn133 ptr_glConvolutionParameteriv v1 v2 v3

{-# NOINLINE ptr_glConvolutionParameteriv #-}
ptr_glConvolutionParameteriv :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glConvolutionParameteriv = unsafePerformIO $ getCommand "glConvolutionParameteriv"

-- glConvolutionParameterivEXT -------------------------------------------------

-- | This command is an alias for 'glConvolutionParameteriv'.
glConvolutionParameterivEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ConvolutionTargetEXT](Graphics-GL-Groups.html#ConvolutionTargetEXT).
  -> GLenum -- ^ @pname@ of type [ConvolutionParameterEXT](Graphics-GL-Groups.html#ConvolutionParameterEXT).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedInt32@.
  -> m ()
glConvolutionParameterivEXT v1 v2 v3 = liftIO $ dyn133 ptr_glConvolutionParameterivEXT v1 v2 v3

{-# NOINLINE ptr_glConvolutionParameterivEXT #-}
ptr_glConvolutionParameterivEXT :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glConvolutionParameterivEXT = unsafePerformIO $ getCommand "glConvolutionParameterivEXT"

-- glConvolutionParameterxOES --------------------------------------------------

glConvolutionParameterxOES
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @pname@.
  -> GLfixed -- ^ @param@.
  -> m ()
glConvolutionParameterxOES v1 v2 v3 = liftIO $ dyn162 ptr_glConvolutionParameterxOES v1 v2 v3

{-# NOINLINE ptr_glConvolutionParameterxOES #-}
ptr_glConvolutionParameterxOES :: FunPtr (GLenum -> GLenum -> GLfixed -> IO ())
ptr_glConvolutionParameterxOES = unsafePerformIO $ getCommand "glConvolutionParameterxOES"

-- glConvolutionParameterxvOES -------------------------------------------------

glConvolutionParameterxvOES
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLfixed -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfixed@.
  -> m ()
glConvolutionParameterxvOES v1 v2 v3 = liftIO $ dyn163 ptr_glConvolutionParameterxvOES v1 v2 v3

{-# NOINLINE ptr_glConvolutionParameterxvOES #-}
ptr_glConvolutionParameterxvOES :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
ptr_glConvolutionParameterxvOES = unsafePerformIO $ getCommand "glConvolutionParameterxvOES"

-- glCopyBufferSubData ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glCopyBufferSubData.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glCopyBufferSubData.xhtml OpenGL 4.x>.
glCopyBufferSubData
  :: MonadIO m
  => GLenum -- ^ @readTarget@.
  -> GLenum -- ^ @writeTarget@.
  -> GLintptr -- ^ @readOffset@ of type @BufferOffset@.
  -> GLintptr -- ^ @writeOffset@ of type @BufferOffset@.
  -> GLsizeiptr -- ^ @size@ of type @BufferSize@.
  -> m ()
glCopyBufferSubData v1 v2 v3 v4 v5 = liftIO $ dyn164 ptr_glCopyBufferSubData v1 v2 v3 v4 v5

{-# NOINLINE ptr_glCopyBufferSubData #-}
ptr_glCopyBufferSubData :: FunPtr (GLenum -> GLenum -> GLintptr -> GLintptr -> GLsizeiptr -> IO ())
ptr_glCopyBufferSubData = unsafePerformIO $ getCommand "glCopyBufferSubData"

-- glCopyBufferSubDataNV -------------------------------------------------------

-- | This command is an alias for 'glCopyBufferSubData'.
glCopyBufferSubDataNV
  :: MonadIO m
  => GLenum -- ^ @readTarget@.
  -> GLenum -- ^ @writeTarget@.
  -> GLintptr -- ^ @readOffset@ of type @BufferOffset@.
  -> GLintptr -- ^ @writeOffset@ of type @BufferOffset@.
  -> GLsizeiptr -- ^ @size@ of type @BufferSize@.
  -> m ()
glCopyBufferSubDataNV v1 v2 v3 v4 v5 = liftIO $ dyn164 ptr_glCopyBufferSubDataNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glCopyBufferSubDataNV #-}
ptr_glCopyBufferSubDataNV :: FunPtr (GLenum -> GLenum -> GLintptr -> GLintptr -> GLsizeiptr -> IO ())
ptr_glCopyBufferSubDataNV = unsafePerformIO $ getCommand "glCopyBufferSubDataNV"

-- glCopyColorSubTable ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glCopyColorSubTable.xml OpenGL 2.x>.
glCopyColorSubTable
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ColorTableTarget@.
  -> GLsizei -- ^ @start@.
  -> GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> m ()
glCopyColorSubTable v1 v2 v3 v4 v5 = liftIO $ dyn165 ptr_glCopyColorSubTable v1 v2 v3 v4 v5

{-# NOINLINE ptr_glCopyColorSubTable #-}
ptr_glCopyColorSubTable :: FunPtr (GLenum -> GLsizei -> GLint -> GLint -> GLsizei -> IO ())
ptr_glCopyColorSubTable = unsafePerformIO $ getCommand "glCopyColorSubTable"

-- glCopyColorSubTableEXT ------------------------------------------------------

-- | This command is an alias for 'glCopyColorSubTable'.
glCopyColorSubTableEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ColorTableTarget@.
  -> GLsizei -- ^ @start@.
  -> GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> m ()
glCopyColorSubTableEXT v1 v2 v3 v4 v5 = liftIO $ dyn165 ptr_glCopyColorSubTableEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glCopyColorSubTableEXT #-}
ptr_glCopyColorSubTableEXT :: FunPtr (GLenum -> GLsizei -> GLint -> GLint -> GLsizei -> IO ())
ptr_glCopyColorSubTableEXT = unsafePerformIO $ getCommand "glCopyColorSubTableEXT"

-- glCopyColorTable ------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glCopyColorTable.xml OpenGL 2.x>.
glCopyColorTable
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ColorTableTarget@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> m ()
glCopyColorTable v1 v2 v3 v4 v5 = liftIO $ dyn166 ptr_glCopyColorTable v1 v2 v3 v4 v5

{-# NOINLINE ptr_glCopyColorTable #-}
ptr_glCopyColorTable :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLsizei -> IO ())
ptr_glCopyColorTable = unsafePerformIO $ getCommand "glCopyColorTable"

-- glCopyColorTableSGI ---------------------------------------------------------

-- | This command is an alias for 'glCopyColorTable'.
glCopyColorTableSGI
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ColorTableTargetSGI](Graphics-GL-Groups.html#ColorTableTargetSGI).
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> m ()
glCopyColorTableSGI v1 v2 v3 v4 v5 = liftIO $ dyn166 ptr_glCopyColorTableSGI v1 v2 v3 v4 v5

{-# NOINLINE ptr_glCopyColorTableSGI #-}
ptr_glCopyColorTableSGI :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLsizei -> IO ())
ptr_glCopyColorTableSGI = unsafePerformIO $ getCommand "glCopyColorTableSGI"

-- glCopyConvolutionFilter1D ---------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glCopyConvolutionFilter1D.xml OpenGL 2.x>.
glCopyConvolutionFilter1D
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ConvolutionTarget@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> m ()
glCopyConvolutionFilter1D v1 v2 v3 v4 v5 = liftIO $ dyn166 ptr_glCopyConvolutionFilter1D v1 v2 v3 v4 v5

{-# NOINLINE ptr_glCopyConvolutionFilter1D #-}
ptr_glCopyConvolutionFilter1D :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLsizei -> IO ())
ptr_glCopyConvolutionFilter1D = unsafePerformIO $ getCommand "glCopyConvolutionFilter1D"

-- glCopyConvolutionFilter1DEXT ------------------------------------------------

-- | This command is an alias for 'glCopyConvolutionFilter1D'.
glCopyConvolutionFilter1DEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ConvolutionTargetEXT](Graphics-GL-Groups.html#ConvolutionTargetEXT).
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> m ()
glCopyConvolutionFilter1DEXT v1 v2 v3 v4 v5 = liftIO $ dyn166 ptr_glCopyConvolutionFilter1DEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glCopyConvolutionFilter1DEXT #-}
ptr_glCopyConvolutionFilter1DEXT :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLsizei -> IO ())
ptr_glCopyConvolutionFilter1DEXT = unsafePerformIO $ getCommand "glCopyConvolutionFilter1DEXT"

