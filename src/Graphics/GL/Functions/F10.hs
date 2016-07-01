--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F10
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

module Graphics.GL.Functions.F10 (
  glCopyConvolutionFilter2D,
  glCopyConvolutionFilter2DEXT,
  glCopyImageSubData,
  glCopyImageSubDataEXT,
  glCopyImageSubDataNV,
  glCopyImageSubDataOES,
  glCopyMultiTexImage1DEXT,
  glCopyMultiTexImage2DEXT,
  glCopyMultiTexSubImage1DEXT,
  glCopyMultiTexSubImage2DEXT,
  glCopyMultiTexSubImage3DEXT,
  glCopyNamedBufferSubData,
  glCopyPathNV,
  glCopyPixels,
  glCopyTexImage1D,
  glCopyTexImage1DEXT,
  glCopyTexImage2D,
  glCopyTexImage2DEXT,
  glCopyTexSubImage1D,
  glCopyTexSubImage1DEXT,
  glCopyTexSubImage2D,
  glCopyTexSubImage2DEXT,
  glCopyTexSubImage3D,
  glCopyTexSubImage3DEXT,
  glCopyTexSubImage3DOES,
  glCopyTextureImage1DEXT,
  glCopyTextureImage2DEXT,
  glCopyTextureLevelsAPPLE,
  glCopyTextureSubImage1D,
  glCopyTextureSubImage1DEXT,
  glCopyTextureSubImage2D,
  glCopyTextureSubImage2DEXT,
  glCopyTextureSubImage3D,
  glCopyTextureSubImage3DEXT,
  glCoverFillPathInstancedNV,
  glCoverFillPathNV,
  glCoverStrokePathInstancedNV,
  glCoverStrokePathNV,
  glCoverageMaskNV,
  glCoverageModulationNV
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

-- glCopyConvolutionFilter2D ---------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glCopyConvolutionFilter2D.xml OpenGL 2.x>.
glCopyConvolutionFilter2D
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ConvolutionTarget@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glCopyConvolutionFilter2D v1 v2 v3 v4 v5 v6 = liftIO $ dyn167 ptr_glCopyConvolutionFilter2D v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glCopyConvolutionFilter2D #-}
ptr_glCopyConvolutionFilter2D :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> IO ())
ptr_glCopyConvolutionFilter2D = unsafePerformIO $ getCommand "glCopyConvolutionFilter2D"

-- glCopyConvolutionFilter2DEXT ------------------------------------------------

-- | This command is an alias for 'glCopyConvolutionFilter2D'.
glCopyConvolutionFilter2DEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ConvolutionTargetEXT](Graphics-GL-Groups.html#ConvolutionTargetEXT).
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glCopyConvolutionFilter2DEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn167 ptr_glCopyConvolutionFilter2DEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glCopyConvolutionFilter2DEXT #-}
ptr_glCopyConvolutionFilter2DEXT :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> IO ())
ptr_glCopyConvolutionFilter2DEXT = unsafePerformIO $ getCommand "glCopyConvolutionFilter2DEXT"

-- glCopyImageSubData ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glCopyImageSubData.xhtml OpenGL 4.x>.
glCopyImageSubData
  :: MonadIO m
  => GLuint -- ^ @srcName@.
  -> GLenum -- ^ @srcTarget@.
  -> GLint -- ^ @srcLevel@.
  -> GLint -- ^ @srcX@.
  -> GLint -- ^ @srcY@.
  -> GLint -- ^ @srcZ@.
  -> GLuint -- ^ @dstName@.
  -> GLenum -- ^ @dstTarget@.
  -> GLint -- ^ @dstLevel@.
  -> GLint -- ^ @dstX@.
  -> GLint -- ^ @dstY@.
  -> GLint -- ^ @dstZ@.
  -> GLsizei -- ^ @srcWidth@.
  -> GLsizei -- ^ @srcHeight@.
  -> GLsizei -- ^ @srcDepth@.
  -> m ()
glCopyImageSubData v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 = liftIO $ dyn168 ptr_glCopyImageSubData v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15

{-# NOINLINE ptr_glCopyImageSubData #-}
ptr_glCopyImageSubData :: FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> IO ())
ptr_glCopyImageSubData = unsafePerformIO $ getCommand "glCopyImageSubData"

-- glCopyImageSubDataEXT -------------------------------------------------------

-- | This command is an alias for 'glCopyImageSubData'.
glCopyImageSubDataEXT
  :: MonadIO m
  => GLuint -- ^ @srcName@.
  -> GLenum -- ^ @srcTarget@.
  -> GLint -- ^ @srcLevel@.
  -> GLint -- ^ @srcX@.
  -> GLint -- ^ @srcY@.
  -> GLint -- ^ @srcZ@.
  -> GLuint -- ^ @dstName@.
  -> GLenum -- ^ @dstTarget@.
  -> GLint -- ^ @dstLevel@.
  -> GLint -- ^ @dstX@.
  -> GLint -- ^ @dstY@.
  -> GLint -- ^ @dstZ@.
  -> GLsizei -- ^ @srcWidth@.
  -> GLsizei -- ^ @srcHeight@.
  -> GLsizei -- ^ @srcDepth@.
  -> m ()
glCopyImageSubDataEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 = liftIO $ dyn168 ptr_glCopyImageSubDataEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15

{-# NOINLINE ptr_glCopyImageSubDataEXT #-}
ptr_glCopyImageSubDataEXT :: FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> IO ())
ptr_glCopyImageSubDataEXT = unsafePerformIO $ getCommand "glCopyImageSubDataEXT"

-- glCopyImageSubDataNV --------------------------------------------------------

glCopyImageSubDataNV
  :: MonadIO m
  => GLuint -- ^ @srcName@.
  -> GLenum -- ^ @srcTarget@.
  -> GLint -- ^ @srcLevel@.
  -> GLint -- ^ @srcX@.
  -> GLint -- ^ @srcY@.
  -> GLint -- ^ @srcZ@.
  -> GLuint -- ^ @dstName@.
  -> GLenum -- ^ @dstTarget@.
  -> GLint -- ^ @dstLevel@.
  -> GLint -- ^ @dstX@.
  -> GLint -- ^ @dstY@.
  -> GLint -- ^ @dstZ@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> m ()
glCopyImageSubDataNV v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 = liftIO $ dyn168 ptr_glCopyImageSubDataNV v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15

{-# NOINLINE ptr_glCopyImageSubDataNV #-}
ptr_glCopyImageSubDataNV :: FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> IO ())
ptr_glCopyImageSubDataNV = unsafePerformIO $ getCommand "glCopyImageSubDataNV"

-- glCopyImageSubDataOES -------------------------------------------------------

-- | This command is an alias for 'glCopyImageSubData'.
glCopyImageSubDataOES
  :: MonadIO m
  => GLuint -- ^ @srcName@.
  -> GLenum -- ^ @srcTarget@.
  -> GLint -- ^ @srcLevel@.
  -> GLint -- ^ @srcX@.
  -> GLint -- ^ @srcY@.
  -> GLint -- ^ @srcZ@.
  -> GLuint -- ^ @dstName@.
  -> GLenum -- ^ @dstTarget@.
  -> GLint -- ^ @dstLevel@.
  -> GLint -- ^ @dstX@.
  -> GLint -- ^ @dstY@.
  -> GLint -- ^ @dstZ@.
  -> GLsizei -- ^ @srcWidth@.
  -> GLsizei -- ^ @srcHeight@.
  -> GLsizei -- ^ @srcDepth@.
  -> m ()
glCopyImageSubDataOES v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 = liftIO $ dyn168 ptr_glCopyImageSubDataOES v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15

{-# NOINLINE ptr_glCopyImageSubDataOES #-}
ptr_glCopyImageSubDataOES :: FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> IO ())
ptr_glCopyImageSubDataOES = unsafePerformIO $ getCommand "glCopyImageSubDataOES"

-- glCopyMultiTexImage1DEXT ----------------------------------------------------

glCopyMultiTexImage1DEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type @TextureUnit@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @internalformat@ of type @TextureInternalFormat@.
  -> GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> m ()
glCopyMultiTexImage1DEXT v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn169 ptr_glCopyMultiTexImage1DEXT v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glCopyMultiTexImage1DEXT #-}
ptr_glCopyMultiTexImage1DEXT :: FunPtr (GLenum -> GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLint -> IO ())
ptr_glCopyMultiTexImage1DEXT = unsafePerformIO $ getCommand "glCopyMultiTexImage1DEXT"

-- glCopyMultiTexImage2DEXT ----------------------------------------------------

glCopyMultiTexImage2DEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type @TextureUnit@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @internalformat@ of type @TextureInternalFormat@.
  -> GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> m ()
glCopyMultiTexImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn170 ptr_glCopyMultiTexImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glCopyMultiTexImage2DEXT #-}
ptr_glCopyMultiTexImage2DEXT :: FunPtr (GLenum -> GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> IO ())
ptr_glCopyMultiTexImage2DEXT = unsafePerformIO $ getCommand "glCopyMultiTexImage2DEXT"

-- glCopyMultiTexSubImage1DEXT -------------------------------------------------

glCopyMultiTexSubImage1DEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type @TextureUnit@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> m ()
glCopyMultiTexSubImage1DEXT v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn171 ptr_glCopyMultiTexSubImage1DEXT v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glCopyMultiTexSubImage1DEXT #-}
ptr_glCopyMultiTexSubImage1DEXT :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> IO ())
ptr_glCopyMultiTexSubImage1DEXT = unsafePerformIO $ getCommand "glCopyMultiTexSubImage1DEXT"

-- glCopyMultiTexSubImage2DEXT -------------------------------------------------

glCopyMultiTexSubImage2DEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type @TextureUnit@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @yoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glCopyMultiTexSubImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn172 ptr_glCopyMultiTexSubImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glCopyMultiTexSubImage2DEXT #-}
ptr_glCopyMultiTexSubImage2DEXT :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ())
ptr_glCopyMultiTexSubImage2DEXT = unsafePerformIO $ getCommand "glCopyMultiTexSubImage2DEXT"

-- glCopyMultiTexSubImage3DEXT -------------------------------------------------

glCopyMultiTexSubImage3DEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type @TextureUnit@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @yoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @zoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glCopyMultiTexSubImage3DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO $ dyn173 ptr_glCopyMultiTexSubImage3DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10

{-# NOINLINE ptr_glCopyMultiTexSubImage3DEXT #-}
ptr_glCopyMultiTexSubImage3DEXT :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ())
ptr_glCopyMultiTexSubImage3DEXT = unsafePerformIO $ getCommand "glCopyMultiTexSubImage3DEXT"

-- glCopyNamedBufferSubData ----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glCopyBufferSubData.xhtml OpenGL 4.x>.
glCopyNamedBufferSubData
  :: MonadIO m
  => GLuint -- ^ @readBuffer@.
  -> GLuint -- ^ @writeBuffer@.
  -> GLintptr -- ^ @readOffset@.
  -> GLintptr -- ^ @writeOffset@.
  -> GLsizeiptr -- ^ @size@ of type @BufferSize@.
  -> m ()
glCopyNamedBufferSubData v1 v2 v3 v4 v5 = liftIO $ dyn174 ptr_glCopyNamedBufferSubData v1 v2 v3 v4 v5

{-# NOINLINE ptr_glCopyNamedBufferSubData #-}
ptr_glCopyNamedBufferSubData :: FunPtr (GLuint -> GLuint -> GLintptr -> GLintptr -> GLsizeiptr -> IO ())
ptr_glCopyNamedBufferSubData = unsafePerformIO $ getCommand "glCopyNamedBufferSubData"

-- glCopyPathNV ----------------------------------------------------------------

glCopyPathNV
  :: MonadIO m
  => GLuint -- ^ @resultPath@ of type @Path@.
  -> GLuint -- ^ @srcPath@ of type @Path@.
  -> m ()
glCopyPathNV v1 v2 = liftIO $ dyn3 ptr_glCopyPathNV v1 v2

{-# NOINLINE ptr_glCopyPathNV #-}
ptr_glCopyPathNV :: FunPtr (GLuint -> GLuint -> IO ())
ptr_glCopyPathNV = unsafePerformIO $ getCommand "glCopyPathNV"

-- glCopyPixels ----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glCopyPixels.xml OpenGL 2.x>.
glCopyPixels
  :: MonadIO m
  => GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLenum -- ^ @type@ of type [PixelCopyType](Graphics-GL-Groups.html#PixelCopyType).
  -> m ()
glCopyPixels v1 v2 v3 v4 v5 = liftIO $ dyn175 ptr_glCopyPixels v1 v2 v3 v4 v5

{-# NOINLINE ptr_glCopyPixels #-}
ptr_glCopyPixels :: FunPtr (GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> IO ())
ptr_glCopyPixels = unsafePerformIO $ getCommand "glCopyPixels"

-- glCopyTexImage1D ------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glCopyTexImage1D.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glCopyTexImage1D.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glCopyTexImage1D.xhtml OpenGL 4.x>.
glCopyTexImage1D
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> m ()
glCopyTexImage1D v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn176 ptr_glCopyTexImage1D v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glCopyTexImage1D #-}
ptr_glCopyTexImage1D :: FunPtr (GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLint -> IO ())
ptr_glCopyTexImage1D = unsafePerformIO $ getCommand "glCopyTexImage1D"

-- glCopyTexImage1DEXT ---------------------------------------------------------

-- | This command is an alias for 'glCopyTexImage1D'.
glCopyTexImage1DEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> m ()
glCopyTexImage1DEXT v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn176 ptr_glCopyTexImage1DEXT v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glCopyTexImage1DEXT #-}
ptr_glCopyTexImage1DEXT :: FunPtr (GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLint -> IO ())
ptr_glCopyTexImage1DEXT = unsafePerformIO $ getCommand "glCopyTexImage1DEXT"

-- glCopyTexImage2D ------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glCopyTexImage2D.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glCopyTexImage2D.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glCopyTexImage2D.xhtml OpenGL 4.x>.
glCopyTexImage2D
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> m ()
glCopyTexImage2D v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn177 ptr_glCopyTexImage2D v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glCopyTexImage2D #-}
ptr_glCopyTexImage2D :: FunPtr (GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> IO ())
ptr_glCopyTexImage2D = unsafePerformIO $ getCommand "glCopyTexImage2D"

-- glCopyTexImage2DEXT ---------------------------------------------------------

-- | This command is an alias for 'glCopyTexImage2D'.
glCopyTexImage2DEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> m ()
glCopyTexImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn177 ptr_glCopyTexImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glCopyTexImage2DEXT #-}
ptr_glCopyTexImage2DEXT :: FunPtr (GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> IO ())
ptr_glCopyTexImage2DEXT = unsafePerformIO $ getCommand "glCopyTexImage2DEXT"

-- glCopyTexSubImage1D ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glCopyTexSubImage1D.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glCopyTexSubImage1D.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glCopyTexSubImage1D.xhtml OpenGL 4.x>.
glCopyTexSubImage1D
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> m ()
glCopyTexSubImage1D v1 v2 v3 v4 v5 v6 = liftIO $ dyn178 ptr_glCopyTexSubImage1D v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glCopyTexSubImage1D #-}
ptr_glCopyTexSubImage1D :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> IO ())
ptr_glCopyTexSubImage1D = unsafePerformIO $ getCommand "glCopyTexSubImage1D"

-- glCopyTexSubImage1DEXT ------------------------------------------------------

-- | This command is an alias for 'glCopyTexSubImage1D'.
glCopyTexSubImage1DEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> m ()
glCopyTexSubImage1DEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn178 ptr_glCopyTexSubImage1DEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glCopyTexSubImage1DEXT #-}
ptr_glCopyTexSubImage1DEXT :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> IO ())
ptr_glCopyTexSubImage1DEXT = unsafePerformIO $ getCommand "glCopyTexSubImage1DEXT"

-- glCopyTexSubImage2D ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glCopyTexSubImage2D.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glCopyTexSubImage2D.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glCopyTexSubImage2D.xhtml OpenGL 4.x>.
glCopyTexSubImage2D
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @yoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glCopyTexSubImage2D v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn179 ptr_glCopyTexSubImage2D v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glCopyTexSubImage2D #-}
ptr_glCopyTexSubImage2D :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ())
ptr_glCopyTexSubImage2D = unsafePerformIO $ getCommand "glCopyTexSubImage2D"

-- glCopyTexSubImage2DEXT ------------------------------------------------------

-- | This command is an alias for 'glCopyTexSubImage2D'.
glCopyTexSubImage2DEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @yoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glCopyTexSubImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn179 ptr_glCopyTexSubImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glCopyTexSubImage2DEXT #-}
ptr_glCopyTexSubImage2DEXT :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ())
ptr_glCopyTexSubImage2DEXT = unsafePerformIO $ getCommand "glCopyTexSubImage2DEXT"

-- glCopyTexSubImage3D ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glCopyTexSubImage3D.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glCopyTexSubImage3D.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glCopyTexSubImage3D.xhtml OpenGL 4.x>.
glCopyTexSubImage3D
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @yoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @zoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glCopyTexSubImage3D v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn180 ptr_glCopyTexSubImage3D v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glCopyTexSubImage3D #-}
ptr_glCopyTexSubImage3D :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ())
ptr_glCopyTexSubImage3D = unsafePerformIO $ getCommand "glCopyTexSubImage3D"

-- glCopyTexSubImage3DEXT ------------------------------------------------------

-- | This command is an alias for 'glCopyTexSubImage3D'.
glCopyTexSubImage3DEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @yoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @zoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glCopyTexSubImage3DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn180 ptr_glCopyTexSubImage3DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glCopyTexSubImage3DEXT #-}
ptr_glCopyTexSubImage3DEXT :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ())
ptr_glCopyTexSubImage3DEXT = unsafePerformIO $ getCommand "glCopyTexSubImage3DEXT"

-- glCopyTexSubImage3DOES ------------------------------------------------------

-- | This command is an alias for 'glCopyTexSubImage3D'.
glCopyTexSubImage3DOES
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLint -- ^ @level@.
  -> GLint -- ^ @xoffset@.
  -> GLint -- ^ @yoffset@.
  -> GLint -- ^ @zoffset@.
  -> GLint -- ^ @x@.
  -> GLint -- ^ @y@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glCopyTexSubImage3DOES v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn180 ptr_glCopyTexSubImage3DOES v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glCopyTexSubImage3DOES #-}
ptr_glCopyTexSubImage3DOES :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ())
ptr_glCopyTexSubImage3DOES = unsafePerformIO $ getCommand "glCopyTexSubImage3DOES"

-- glCopyTextureImage1DEXT -----------------------------------------------------

glCopyTextureImage1DEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @internalformat@ of type @TextureInternalFormat@.
  -> GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> m ()
glCopyTextureImage1DEXT v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn181 ptr_glCopyTextureImage1DEXT v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glCopyTextureImage1DEXT #-}
ptr_glCopyTextureImage1DEXT :: FunPtr (GLuint -> GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLint -> IO ())
ptr_glCopyTextureImage1DEXT = unsafePerformIO $ getCommand "glCopyTextureImage1DEXT"

-- glCopyTextureImage2DEXT -----------------------------------------------------

glCopyTextureImage2DEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @internalformat@ of type @TextureInternalFormat@.
  -> GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> m ()
glCopyTextureImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn182 ptr_glCopyTextureImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glCopyTextureImage2DEXT #-}
ptr_glCopyTextureImage2DEXT :: FunPtr (GLuint -> GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> IO ())
ptr_glCopyTextureImage2DEXT = unsafePerformIO $ getCommand "glCopyTextureImage2DEXT"

-- glCopyTextureLevelsAPPLE ----------------------------------------------------

glCopyTextureLevelsAPPLE
  :: MonadIO m
  => GLuint -- ^ @destinationTexture@.
  -> GLuint -- ^ @sourceTexture@.
  -> GLint -- ^ @sourceBaseLevel@.
  -> GLsizei -- ^ @sourceLevelCount@.
  -> m ()
glCopyTextureLevelsAPPLE v1 v2 v3 v4 = liftIO $ dyn183 ptr_glCopyTextureLevelsAPPLE v1 v2 v3 v4

{-# NOINLINE ptr_glCopyTextureLevelsAPPLE #-}
ptr_glCopyTextureLevelsAPPLE :: FunPtr (GLuint -> GLuint -> GLint -> GLsizei -> IO ())
ptr_glCopyTextureLevelsAPPLE = unsafePerformIO $ getCommand "glCopyTextureLevelsAPPLE"

-- glCopyTextureSubImage1D -----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glCopyTexSubImage1D.xhtml OpenGL 4.x>.
glCopyTextureSubImage1D
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLint -- ^ @xoffset@.
  -> GLint -- ^ @x@.
  -> GLint -- ^ @y@.
  -> GLsizei -- ^ @width@.
  -> m ()
glCopyTextureSubImage1D v1 v2 v3 v4 v5 v6 = liftIO $ dyn184 ptr_glCopyTextureSubImage1D v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glCopyTextureSubImage1D #-}
ptr_glCopyTextureSubImage1D :: FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> IO ())
ptr_glCopyTextureSubImage1D = unsafePerformIO $ getCommand "glCopyTextureSubImage1D"

-- glCopyTextureSubImage1DEXT --------------------------------------------------

glCopyTextureSubImage1DEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> m ()
glCopyTextureSubImage1DEXT v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn185 ptr_glCopyTextureSubImage1DEXT v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glCopyTextureSubImage1DEXT #-}
ptr_glCopyTextureSubImage1DEXT :: FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> IO ())
ptr_glCopyTextureSubImage1DEXT = unsafePerformIO $ getCommand "glCopyTextureSubImage1DEXT"

-- glCopyTextureSubImage2D -----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glCopyTexSubImage2D.xhtml OpenGL 4.x>.
glCopyTextureSubImage2D
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLint -- ^ @xoffset@.
  -> GLint -- ^ @yoffset@.
  -> GLint -- ^ @x@.
  -> GLint -- ^ @y@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glCopyTextureSubImage2D v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn186 ptr_glCopyTextureSubImage2D v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glCopyTextureSubImage2D #-}
ptr_glCopyTextureSubImage2D :: FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ())
ptr_glCopyTextureSubImage2D = unsafePerformIO $ getCommand "glCopyTextureSubImage2D"

-- glCopyTextureSubImage2DEXT --------------------------------------------------

glCopyTextureSubImage2DEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @yoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glCopyTextureSubImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn187 ptr_glCopyTextureSubImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glCopyTextureSubImage2DEXT #-}
ptr_glCopyTextureSubImage2DEXT :: FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ())
ptr_glCopyTextureSubImage2DEXT = unsafePerformIO $ getCommand "glCopyTextureSubImage2DEXT"

-- glCopyTextureSubImage3D -----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glCopyTexSubImage3D.xhtml OpenGL 4.x>.
glCopyTextureSubImage3D
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLint -- ^ @xoffset@.
  -> GLint -- ^ @yoffset@.
  -> GLint -- ^ @zoffset@.
  -> GLint -- ^ @x@.
  -> GLint -- ^ @y@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glCopyTextureSubImage3D v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn188 ptr_glCopyTextureSubImage3D v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glCopyTextureSubImage3D #-}
ptr_glCopyTextureSubImage3D :: FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ())
ptr_glCopyTextureSubImage3D = unsafePerformIO $ getCommand "glCopyTextureSubImage3D"

-- glCopyTextureSubImage3DEXT --------------------------------------------------

glCopyTextureSubImage3DEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @yoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @zoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glCopyTextureSubImage3DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO $ dyn189 ptr_glCopyTextureSubImage3DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10

{-# NOINLINE ptr_glCopyTextureSubImage3DEXT #-}
ptr_glCopyTextureSubImage3DEXT :: FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ())
ptr_glCopyTextureSubImage3DEXT = unsafePerformIO $ getCommand "glCopyTextureSubImage3DEXT"

-- glCoverFillPathInstancedNV --------------------------------------------------

glCoverFillPathInstancedNV
  :: MonadIO m
  => GLsizei -- ^ @numPaths@.
  -> GLenum -- ^ @pathNameType@ of type @PathElementType@.
  -> Ptr a -- ^ @paths@ pointing to @COMPSIZE(numPaths,pathNameType,paths)@ elements of type @PathElement@.
  -> GLuint -- ^ @pathBase@ of type @Path@.
  -> GLenum -- ^ @coverMode@ of type @PathCoverMode@.
  -> GLenum -- ^ @transformType@ of type @PathTransformType@.
  -> Ptr GLfloat -- ^ @transformValues@ pointing to @COMPSIZE(numPaths,transformType)@ elements of type @GLfloat@.
  -> m ()
glCoverFillPathInstancedNV v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn190 ptr_glCoverFillPathInstancedNV v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glCoverFillPathInstancedNV #-}
ptr_glCoverFillPathInstancedNV :: FunPtr (GLsizei -> GLenum -> Ptr a -> GLuint -> GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glCoverFillPathInstancedNV = unsafePerformIO $ getCommand "glCoverFillPathInstancedNV"

-- glCoverFillPathNV -----------------------------------------------------------

glCoverFillPathNV
  :: MonadIO m
  => GLuint -- ^ @path@ of type @Path@.
  -> GLenum -- ^ @coverMode@ of type @PathCoverMode@.
  -> m ()
glCoverFillPathNV v1 v2 = liftIO $ dyn15 ptr_glCoverFillPathNV v1 v2

{-# NOINLINE ptr_glCoverFillPathNV #-}
ptr_glCoverFillPathNV :: FunPtr (GLuint -> GLenum -> IO ())
ptr_glCoverFillPathNV = unsafePerformIO $ getCommand "glCoverFillPathNV"

-- glCoverStrokePathInstancedNV ------------------------------------------------

glCoverStrokePathInstancedNV
  :: MonadIO m
  => GLsizei -- ^ @numPaths@.
  -> GLenum -- ^ @pathNameType@ of type @PathElementType@.
  -> Ptr a -- ^ @paths@ pointing to @COMPSIZE(numPaths,pathNameType,paths)@ elements of type @PathElement@.
  -> GLuint -- ^ @pathBase@ of type @Path@.
  -> GLenum -- ^ @coverMode@ of type @PathCoverMode@.
  -> GLenum -- ^ @transformType@ of type @PathTransformType@.
  -> Ptr GLfloat -- ^ @transformValues@ pointing to @COMPSIZE(numPaths,transformType)@ elements of type @GLfloat@.
  -> m ()
glCoverStrokePathInstancedNV v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn190 ptr_glCoverStrokePathInstancedNV v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glCoverStrokePathInstancedNV #-}
ptr_glCoverStrokePathInstancedNV :: FunPtr (GLsizei -> GLenum -> Ptr a -> GLuint -> GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glCoverStrokePathInstancedNV = unsafePerformIO $ getCommand "glCoverStrokePathInstancedNV"

-- glCoverStrokePathNV ---------------------------------------------------------

glCoverStrokePathNV
  :: MonadIO m
  => GLuint -- ^ @path@ of type @Path@.
  -> GLenum -- ^ @coverMode@ of type @PathCoverMode@.
  -> m ()
glCoverStrokePathNV v1 v2 = liftIO $ dyn15 ptr_glCoverStrokePathNV v1 v2

{-# NOINLINE ptr_glCoverStrokePathNV #-}
ptr_glCoverStrokePathNV :: FunPtr (GLuint -> GLenum -> IO ())
ptr_glCoverStrokePathNV = unsafePerformIO $ getCommand "glCoverStrokePathNV"

-- glCoverageMaskNV ------------------------------------------------------------

glCoverageMaskNV
  :: MonadIO m
  => GLboolean -- ^ @mask@.
  -> m ()
glCoverageMaskNV v1 = liftIO $ dyn191 ptr_glCoverageMaskNV v1

{-# NOINLINE ptr_glCoverageMaskNV #-}
ptr_glCoverageMaskNV :: FunPtr (GLboolean -> IO ())
ptr_glCoverageMaskNV = unsafePerformIO $ getCommand "glCoverageMaskNV"

-- glCoverageModulationNV ------------------------------------------------------

glCoverageModulationNV
  :: MonadIO m
  => GLenum -- ^ @components@.
  -> m ()
glCoverageModulationNV v1 = liftIO $ dyn4 ptr_glCoverageModulationNV v1

{-# NOINLINE ptr_glCoverageModulationNV #-}
ptr_glCoverageModulationNV :: FunPtr (GLenum -> IO ())
ptr_glCoverageModulationNV = unsafePerformIO $ getCommand "glCoverageModulationNV"

