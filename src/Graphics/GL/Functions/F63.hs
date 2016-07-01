--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F63
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

module Graphics.GL.Functions.F63 (
  glTextureBuffer,
  glTextureBufferEXT,
  glTextureBufferRange,
  glTextureBufferRangeEXT,
  glTextureColorMaskSGIS,
  glTextureImage1DEXT,
  glTextureImage2DEXT,
  glTextureImage2DMultisampleCoverageNV,
  glTextureImage2DMultisampleNV,
  glTextureImage3DEXT,
  glTextureImage3DMultisampleCoverageNV,
  glTextureImage3DMultisampleNV,
  glTextureLightEXT,
  glTextureMaterialEXT,
  glTextureNormalEXT,
  glTexturePageCommitmentEXT,
  glTextureParameterIiv,
  glTextureParameterIivEXT,
  glTextureParameterIuiv,
  glTextureParameterIuivEXT,
  glTextureParameterf,
  glTextureParameterfEXT,
  glTextureParameterfv,
  glTextureParameterfvEXT,
  glTextureParameteri,
  glTextureParameteriEXT,
  glTextureParameteriv,
  glTextureParameterivEXT,
  glTextureRangeAPPLE,
  glTextureRenderbufferEXT,
  glTextureStorage1D,
  glTextureStorage1DEXT,
  glTextureStorage2D,
  glTextureStorage2DEXT,
  glTextureStorage2DMultisample,
  glTextureStorage2DMultisampleEXT,
  glTextureStorage3D,
  glTextureStorage3DEXT,
  glTextureStorage3DMultisample,
  glTextureStorage3DMultisampleEXT
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

-- glTextureBuffer -------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glTexBuffer.xhtml OpenGL 4.x>.
glTextureBuffer
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLenum -- ^ @internalformat@.
  -> GLuint -- ^ @buffer@.
  -> m ()
glTextureBuffer v1 v2 v3 = liftIO $ dyn712 ptr_glTextureBuffer v1 v2 v3

{-# NOINLINE ptr_glTextureBuffer #-}
ptr_glTextureBuffer :: FunPtr (GLuint -> GLenum -> GLuint -> IO ())
ptr_glTextureBuffer = unsafePerformIO $ getCommand "glTextureBuffer"

-- glTextureBufferEXT ----------------------------------------------------------

glTextureBufferEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @internalformat@ of type @TextureInternalFormat@.
  -> GLuint -- ^ @buffer@.
  -> m ()
glTextureBufferEXT v1 v2 v3 v4 = liftIO $ dyn574 ptr_glTextureBufferEXT v1 v2 v3 v4

{-# NOINLINE ptr_glTextureBufferEXT #-}
ptr_glTextureBufferEXT :: FunPtr (GLuint -> GLenum -> GLenum -> GLuint -> IO ())
ptr_glTextureBufferEXT = unsafePerformIO $ getCommand "glTextureBufferEXT"

-- glTextureBufferRange --------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glTexBufferRange.xhtml OpenGL 4.x>.
glTextureBufferRange
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLenum -- ^ @internalformat@.
  -> GLuint -- ^ @buffer@.
  -> GLintptr -- ^ @offset@.
  -> GLsizeiptr -- ^ @size@ of type @BufferSize@.
  -> m ()
glTextureBufferRange v1 v2 v3 v4 v5 = liftIO $ dyn746 ptr_glTextureBufferRange v1 v2 v3 v4 v5

{-# NOINLINE ptr_glTextureBufferRange #-}
ptr_glTextureBufferRange :: FunPtr (GLuint -> GLenum -> GLuint -> GLintptr -> GLsizeiptr -> IO ())
ptr_glTextureBufferRange = unsafePerformIO $ getCommand "glTextureBufferRange"

-- glTextureBufferRangeEXT -----------------------------------------------------

glTextureBufferRangeEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @internalformat@ of type @TextureInternalFormat@.
  -> GLuint -- ^ @buffer@.
  -> GLintptr -- ^ @offset@ of type @BufferOffset@.
  -> GLsizeiptr -- ^ @size@ of type @BufferSize@.
  -> m ()
glTextureBufferRangeEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn747 ptr_glTextureBufferRangeEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glTextureBufferRangeEXT #-}
ptr_glTextureBufferRangeEXT :: FunPtr (GLuint -> GLenum -> GLenum -> GLuint -> GLintptr -> GLsizeiptr -> IO ())
ptr_glTextureBufferRangeEXT = unsafePerformIO $ getCommand "glTextureBufferRangeEXT"

-- glTextureColorMaskSGIS ------------------------------------------------------

glTextureColorMaskSGIS
  :: MonadIO m
  => GLboolean -- ^ @red@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLboolean -- ^ @green@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLboolean -- ^ @blue@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLboolean -- ^ @alpha@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glTextureColorMaskSGIS v1 v2 v3 v4 = liftIO $ dyn123 ptr_glTextureColorMaskSGIS v1 v2 v3 v4

{-# NOINLINE ptr_glTextureColorMaskSGIS #-}
ptr_glTextureColorMaskSGIS :: FunPtr (GLboolean -> GLboolean -> GLboolean -> GLboolean -> IO ())
ptr_glTextureColorMaskSGIS = unsafePerformIO $ getCommand "glTextureColorMaskSGIS"

-- glTextureImage1DEXT ---------------------------------------------------------

glTextureImage1DEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @internalformat@ of type @TextureComponentCount@.
  -> GLsizei -- ^ @width@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @pixels@ pointing to @COMPSIZE(format,type,width)@ elements of type @a@.
  -> m ()
glTextureImage1DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn748 ptr_glTextureImage1DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glTextureImage1DEXT #-}
ptr_glTextureImage1DEXT :: FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glTextureImage1DEXT = unsafePerformIO $ getCommand "glTextureImage1DEXT"

-- glTextureImage2DEXT ---------------------------------------------------------

glTextureImage2DEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @internalformat@ of type @TextureComponentCount@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @pixels@ pointing to @COMPSIZE(format,type,width,height)@ elements of type @a@.
  -> m ()
glTextureImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO $ dyn749 ptr_glTextureImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10

{-# NOINLINE ptr_glTextureImage2DEXT #-}
ptr_glTextureImage2DEXT :: FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glTextureImage2DEXT = unsafePerformIO $ getCommand "glTextureImage2DEXT"

-- glTextureImage2DMultisampleCoverageNV ---------------------------------------

glTextureImage2DMultisampleCoverageNV
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLenum -- ^ @target@.
  -> GLsizei -- ^ @coverageSamples@.
  -> GLsizei -- ^ @colorSamples@.
  -> GLint -- ^ @internalFormat@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLboolean -- ^ @fixedSampleLocations@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glTextureImage2DMultisampleCoverageNV v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn750 ptr_glTextureImage2DMultisampleCoverageNV v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glTextureImage2DMultisampleCoverageNV #-}
ptr_glTextureImage2DMultisampleCoverageNV :: FunPtr (GLuint -> GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> GLsizei -> GLboolean -> IO ())
ptr_glTextureImage2DMultisampleCoverageNV = unsafePerformIO $ getCommand "glTextureImage2DMultisampleCoverageNV"

-- glTextureImage2DMultisampleNV -----------------------------------------------

glTextureImage2DMultisampleNV
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLenum -- ^ @target@.
  -> GLsizei -- ^ @samples@.
  -> GLint -- ^ @internalFormat@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLboolean -- ^ @fixedSampleLocations@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glTextureImage2DMultisampleNV v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn751 ptr_glTextureImage2DMultisampleNV v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glTextureImage2DMultisampleNV #-}
ptr_glTextureImage2DMultisampleNV :: FunPtr (GLuint -> GLenum -> GLsizei -> GLint -> GLsizei -> GLsizei -> GLboolean -> IO ())
ptr_glTextureImage2DMultisampleNV = unsafePerformIO $ getCommand "glTextureImage2DMultisampleNV"

-- glTextureImage3DEXT ---------------------------------------------------------

glTextureImage3DEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @internalformat@ of type @TextureComponentCount@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @pixels@ pointing to @COMPSIZE(format,type,width,height,depth)@ elements of type @a@.
  -> m ()
glTextureImage3DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 = liftIO $ dyn752 ptr_glTextureImage3DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11

{-# NOINLINE ptr_glTextureImage3DEXT #-}
ptr_glTextureImage3DEXT :: FunPtr (GLuint -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glTextureImage3DEXT = unsafePerformIO $ getCommand "glTextureImage3DEXT"

-- glTextureImage3DMultisampleCoverageNV ---------------------------------------

glTextureImage3DMultisampleCoverageNV
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLenum -- ^ @target@.
  -> GLsizei -- ^ @coverageSamples@.
  -> GLsizei -- ^ @colorSamples@.
  -> GLint -- ^ @internalFormat@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLboolean -- ^ @fixedSampleLocations@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glTextureImage3DMultisampleCoverageNV v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn753 ptr_glTextureImage3DMultisampleCoverageNV v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glTextureImage3DMultisampleCoverageNV #-}
ptr_glTextureImage3DMultisampleCoverageNV :: FunPtr (GLuint -> GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> IO ())
ptr_glTextureImage3DMultisampleCoverageNV = unsafePerformIO $ getCommand "glTextureImage3DMultisampleCoverageNV"

-- glTextureImage3DMultisampleNV -----------------------------------------------

glTextureImage3DMultisampleNV
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLenum -- ^ @target@.
  -> GLsizei -- ^ @samples@.
  -> GLint -- ^ @internalFormat@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLboolean -- ^ @fixedSampleLocations@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glTextureImage3DMultisampleNV v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn754 ptr_glTextureImage3DMultisampleNV v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glTextureImage3DMultisampleNV #-}
ptr_glTextureImage3DMultisampleNV :: FunPtr (GLuint -> GLenum -> GLsizei -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> IO ())
ptr_glTextureImage3DMultisampleNV = unsafePerformIO $ getCommand "glTextureImage3DMultisampleNV"

-- glTextureLightEXT -----------------------------------------------------------

glTextureLightEXT
  :: MonadIO m
  => GLenum -- ^ @pname@ of type @LightTexturePNameEXT@.
  -> m ()
glTextureLightEXT v1 = liftIO $ dyn4 ptr_glTextureLightEXT v1

{-# NOINLINE ptr_glTextureLightEXT #-}
ptr_glTextureLightEXT :: FunPtr (GLenum -> IO ())
ptr_glTextureLightEXT = unsafePerformIO $ getCommand "glTextureLightEXT"

-- glTextureMaterialEXT --------------------------------------------------------

glTextureMaterialEXT
  :: MonadIO m
  => GLenum -- ^ @face@ of type [MaterialFace](Graphics-GL-Groups.html#MaterialFace).
  -> GLenum -- ^ @mode@ of type [MaterialParameter](Graphics-GL-Groups.html#MaterialParameter).
  -> m ()
glTextureMaterialEXT v1 v2 = liftIO $ dyn51 ptr_glTextureMaterialEXT v1 v2

{-# NOINLINE ptr_glTextureMaterialEXT #-}
ptr_glTextureMaterialEXT :: FunPtr (GLenum -> GLenum -> IO ())
ptr_glTextureMaterialEXT = unsafePerformIO $ getCommand "glTextureMaterialEXT"

-- glTextureNormalEXT ----------------------------------------------------------

glTextureNormalEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type @TextureNormalModeEXT@.
  -> m ()
glTextureNormalEXT v1 = liftIO $ dyn4 ptr_glTextureNormalEXT v1

{-# NOINLINE ptr_glTextureNormalEXT #-}
ptr_glTextureNormalEXT :: FunPtr (GLenum -> IO ())
ptr_glTextureNormalEXT = unsafePerformIO $ getCommand "glTextureNormalEXT"

-- glTexturePageCommitmentEXT --------------------------------------------------

glTexturePageCommitmentEXT
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLint -- ^ @xoffset@.
  -> GLint -- ^ @yoffset@.
  -> GLint -- ^ @zoffset@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLboolean -- ^ @commit@.
  -> m ()
glTexturePageCommitmentEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn755 ptr_glTexturePageCommitmentEXT v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glTexturePageCommitmentEXT #-}
ptr_glTexturePageCommitmentEXT :: FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> IO ())
ptr_glTexturePageCommitmentEXT = unsafePerformIO $ getCommand "glTexturePageCommitmentEXT"

-- glTextureParameterIiv -------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glTexParameter.xhtml OpenGL 4.x>.
glTextureParameterIiv
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @params@.
  -> m ()
glTextureParameterIiv v1 v2 v3 = liftIO $ dyn334 ptr_glTextureParameterIiv v1 v2 v3

{-# NOINLINE ptr_glTextureParameterIiv #-}
ptr_glTextureParameterIiv :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glTextureParameterIiv = unsafePerformIO $ getCommand "glTextureParameterIiv"

-- glTextureParameterIivEXT ----------------------------------------------------

glTextureParameterIivEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [TextureParameterName](Graphics-GL-Groups.html#TextureParameterName).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedInt32@.
  -> m ()
glTextureParameterIivEXT v1 v2 v3 v4 = liftIO $ dyn363 ptr_glTextureParameterIivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glTextureParameterIivEXT #-}
ptr_glTextureParameterIivEXT :: FunPtr (GLuint -> GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glTextureParameterIivEXT = unsafePerformIO $ getCommand "glTextureParameterIivEXT"

-- glTextureParameterIuiv ------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glTexParameter.xhtml OpenGL 4.x>.
glTextureParameterIuiv
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLuint -- ^ @params@.
  -> m ()
glTextureParameterIuiv v1 v2 v3 = liftIO $ dyn375 ptr_glTextureParameterIuiv v1 v2 v3

{-# NOINLINE ptr_glTextureParameterIuiv #-}
ptr_glTextureParameterIuiv :: FunPtr (GLuint -> GLenum -> Ptr GLuint -> IO ())
ptr_glTextureParameterIuiv = unsafePerformIO $ getCommand "glTextureParameterIuiv"

-- glTextureParameterIuivEXT ---------------------------------------------------

glTextureParameterIuivEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [TextureParameterName](Graphics-GL-Groups.html#TextureParameterName).
  -> Ptr GLuint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLuint@.
  -> m ()
glTextureParameterIuivEXT v1 v2 v3 v4 = liftIO $ dyn420 ptr_glTextureParameterIuivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glTextureParameterIuivEXT #-}
ptr_glTextureParameterIuivEXT :: FunPtr (GLuint -> GLenum -> GLenum -> Ptr GLuint -> IO ())
ptr_glTextureParameterIuivEXT = unsafePerformIO $ getCommand "glTextureParameterIuivEXT"

-- glTextureParameterf ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glTexParameter.xhtml OpenGL 4.x>.
glTextureParameterf
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLenum -- ^ @pname@.
  -> GLfloat -- ^ @param@.
  -> m ()
glTextureParameterf v1 v2 v3 = liftIO $ dyn487 ptr_glTextureParameterf v1 v2 v3

{-# NOINLINE ptr_glTextureParameterf #-}
ptr_glTextureParameterf :: FunPtr (GLuint -> GLenum -> GLfloat -> IO ())
ptr_glTextureParameterf = unsafePerformIO $ getCommand "glTextureParameterf"

-- glTextureParameterfEXT ------------------------------------------------------

-- | The vector equivalent of this command is 'glTextureParameterfvEXT'.
glTextureParameterfEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [TextureParameterName](Graphics-GL-Groups.html#TextureParameterName).
  -> GLfloat -- ^ @param@ of type @CheckedFloat32@.
  -> m ()
glTextureParameterfEXT v1 v2 v3 v4 = liftIO $ dyn756 ptr_glTextureParameterfEXT v1 v2 v3 v4

{-# NOINLINE ptr_glTextureParameterfEXT #-}
ptr_glTextureParameterfEXT :: FunPtr (GLuint -> GLenum -> GLenum -> GLfloat -> IO ())
ptr_glTextureParameterfEXT = unsafePerformIO $ getCommand "glTextureParameterfEXT"

-- glTextureParameterfv --------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glTexParameter.xhtml OpenGL 4.x>.
glTextureParameterfv
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLfloat -- ^ @param@.
  -> m ()
glTextureParameterfv v1 v2 v3 = liftIO $ dyn349 ptr_glTextureParameterfv v1 v2 v3

{-# NOINLINE ptr_glTextureParameterfv #-}
ptr_glTextureParameterfv :: FunPtr (GLuint -> GLenum -> Ptr GLfloat -> IO ())
ptr_glTextureParameterfv = unsafePerformIO $ getCommand "glTextureParameterfv"

-- glTextureParameterfvEXT -----------------------------------------------------

glTextureParameterfvEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [TextureParameterName](Graphics-GL-Groups.html#TextureParameterName).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedFloat32@.
  -> m ()
glTextureParameterfvEXT v1 v2 v3 v4 = liftIO $ dyn421 ptr_glTextureParameterfvEXT v1 v2 v3 v4

{-# NOINLINE ptr_glTextureParameterfvEXT #-}
ptr_glTextureParameterfvEXT :: FunPtr (GLuint -> GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glTextureParameterfvEXT = unsafePerformIO $ getCommand "glTextureParameterfvEXT"

-- glTextureParameteri ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glTexParameter.xhtml OpenGL 4.x>.
glTextureParameteri
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLenum -- ^ @pname@.
  -> GLint -- ^ @param@.
  -> m ()
glTextureParameteri v1 v2 v3 = liftIO $ dyn488 ptr_glTextureParameteri v1 v2 v3

{-# NOINLINE ptr_glTextureParameteri #-}
ptr_glTextureParameteri :: FunPtr (GLuint -> GLenum -> GLint -> IO ())
ptr_glTextureParameteri = unsafePerformIO $ getCommand "glTextureParameteri"

-- glTextureParameteriEXT ------------------------------------------------------

-- | The vector equivalent of this command is 'glTextureParameterivEXT'.
glTextureParameteriEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [TextureParameterName](Graphics-GL-Groups.html#TextureParameterName).
  -> GLint -- ^ @param@ of type @CheckedInt32@.
  -> m ()
glTextureParameteriEXT v1 v2 v3 v4 = liftIO $ dyn757 ptr_glTextureParameteriEXT v1 v2 v3 v4

{-# NOINLINE ptr_glTextureParameteriEXT #-}
ptr_glTextureParameteriEXT :: FunPtr (GLuint -> GLenum -> GLenum -> GLint -> IO ())
ptr_glTextureParameteriEXT = unsafePerformIO $ getCommand "glTextureParameteriEXT"

-- glTextureParameteriv --------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glTexParameter.xhtml OpenGL 4.x>.
glTextureParameteriv
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @param@.
  -> m ()
glTextureParameteriv v1 v2 v3 = liftIO $ dyn334 ptr_glTextureParameteriv v1 v2 v3

{-# NOINLINE ptr_glTextureParameteriv #-}
ptr_glTextureParameteriv :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glTextureParameteriv = unsafePerformIO $ getCommand "glTextureParameteriv"

-- glTextureParameterivEXT -----------------------------------------------------

glTextureParameterivEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [TextureParameterName](Graphics-GL-Groups.html#TextureParameterName).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedInt32@.
  -> m ()
glTextureParameterivEXT v1 v2 v3 v4 = liftIO $ dyn363 ptr_glTextureParameterivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glTextureParameterivEXT #-}
ptr_glTextureParameterivEXT :: FunPtr (GLuint -> GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glTextureParameterivEXT = unsafePerformIO $ getCommand "glTextureParameterivEXT"

-- glTextureRangeAPPLE ---------------------------------------------------------

glTextureRangeAPPLE
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLsizei -- ^ @length@.
  -> Ptr a -- ^ @pointer@ pointing to @length@ elements of type @a@.
  -> m ()
glTextureRangeAPPLE v1 v2 v3 = liftIO $ dyn46 ptr_glTextureRangeAPPLE v1 v2 v3

{-# NOINLINE ptr_glTextureRangeAPPLE #-}
ptr_glTextureRangeAPPLE :: FunPtr (GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glTextureRangeAPPLE = unsafePerformIO $ getCommand "glTextureRangeAPPLE"

-- glTextureRenderbufferEXT ----------------------------------------------------

glTextureRenderbufferEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLuint -- ^ @renderbuffer@.
  -> m ()
glTextureRenderbufferEXT v1 v2 v3 = liftIO $ dyn712 ptr_glTextureRenderbufferEXT v1 v2 v3

{-# NOINLINE ptr_glTextureRenderbufferEXT #-}
ptr_glTextureRenderbufferEXT :: FunPtr (GLuint -> GLenum -> GLuint -> IO ())
ptr_glTextureRenderbufferEXT = unsafePerformIO $ getCommand "glTextureRenderbufferEXT"

-- glTextureStorage1D ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glTexStorage1D.xhtml OpenGL 4.x>.
glTextureStorage1D
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLsizei -- ^ @levels@.
  -> GLenum -- ^ @internalformat@.
  -> GLsizei -- ^ @width@.
  -> m ()
glTextureStorage1D v1 v2 v3 v4 = liftIO $ dyn758 ptr_glTextureStorage1D v1 v2 v3 v4

{-# NOINLINE ptr_glTextureStorage1D #-}
ptr_glTextureStorage1D :: FunPtr (GLuint -> GLsizei -> GLenum -> GLsizei -> IO ())
ptr_glTextureStorage1D = unsafePerformIO $ getCommand "glTextureStorage1D"

-- glTextureStorage1DEXT -------------------------------------------------------

glTextureStorage1DEXT
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLenum -- ^ @target@.
  -> GLsizei -- ^ @levels@.
  -> GLenum -- ^ @internalformat@.
  -> GLsizei -- ^ @width@.
  -> m ()
glTextureStorage1DEXT v1 v2 v3 v4 v5 = liftIO $ dyn759 ptr_glTextureStorage1DEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glTextureStorage1DEXT #-}
ptr_glTextureStorage1DEXT :: FunPtr (GLuint -> GLenum -> GLsizei -> GLenum -> GLsizei -> IO ())
ptr_glTextureStorage1DEXT = unsafePerformIO $ getCommand "glTextureStorage1DEXT"

-- glTextureStorage2D ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glTexStorage2D.xhtml OpenGL 4.x>.
glTextureStorage2D
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLsizei -- ^ @levels@.
  -> GLenum -- ^ @internalformat@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glTextureStorage2D v1 v2 v3 v4 v5 = liftIO $ dyn590 ptr_glTextureStorage2D v1 v2 v3 v4 v5

{-# NOINLINE ptr_glTextureStorage2D #-}
ptr_glTextureStorage2D :: FunPtr (GLuint -> GLsizei -> GLenum -> GLsizei -> GLsizei -> IO ())
ptr_glTextureStorage2D = unsafePerformIO $ getCommand "glTextureStorage2D"

-- glTextureStorage2DEXT -------------------------------------------------------

glTextureStorage2DEXT
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLenum -- ^ @target@.
  -> GLsizei -- ^ @levels@.
  -> GLenum -- ^ @internalformat@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glTextureStorage2DEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn760 ptr_glTextureStorage2DEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glTextureStorage2DEXT #-}
ptr_glTextureStorage2DEXT :: FunPtr (GLuint -> GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> IO ())
ptr_glTextureStorage2DEXT = unsafePerformIO $ getCommand "glTextureStorage2DEXT"

-- glTextureStorage2DMultisample -----------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glTexStorage2DMultisample.xhtml OpenGL 4.x>.
glTextureStorage2DMultisample
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLsizei -- ^ @samples@.
  -> GLenum -- ^ @internalformat@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLboolean -- ^ @fixedsamplelocations@.
  -> m ()
glTextureStorage2DMultisample v1 v2 v3 v4 v5 v6 = liftIO $ dyn761 ptr_glTextureStorage2DMultisample v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glTextureStorage2DMultisample #-}
ptr_glTextureStorage2DMultisample :: FunPtr (GLuint -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLboolean -> IO ())
ptr_glTextureStorage2DMultisample = unsafePerformIO $ getCommand "glTextureStorage2DMultisample"

-- glTextureStorage2DMultisampleEXT --------------------------------------------

glTextureStorage2DMultisampleEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLsizei -- ^ @samples@.
  -> GLenum -- ^ @internalformat@ of type @TextureInternalFormat@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLboolean -- ^ @fixedsamplelocations@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glTextureStorage2DMultisampleEXT v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn762 ptr_glTextureStorage2DMultisampleEXT v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glTextureStorage2DMultisampleEXT #-}
ptr_glTextureStorage2DMultisampleEXT :: FunPtr (GLuint -> GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLboolean -> IO ())
ptr_glTextureStorage2DMultisampleEXT = unsafePerformIO $ getCommand "glTextureStorage2DMultisampleEXT"

-- glTextureStorage3D ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glTexStorage3D.xhtml OpenGL 4.x>.
glTextureStorage3D
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLsizei -- ^ @levels@.
  -> GLenum -- ^ @internalformat@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> m ()
glTextureStorage3D v1 v2 v3 v4 v5 v6 = liftIO $ dyn763 ptr_glTextureStorage3D v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glTextureStorage3D #-}
ptr_glTextureStorage3D :: FunPtr (GLuint -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLsizei -> IO ())
ptr_glTextureStorage3D = unsafePerformIO $ getCommand "glTextureStorage3D"

-- glTextureStorage3DEXT -------------------------------------------------------

glTextureStorage3DEXT
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLenum -- ^ @target@.
  -> GLsizei -- ^ @levels@.
  -> GLenum -- ^ @internalformat@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> m ()
glTextureStorage3DEXT v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn764 ptr_glTextureStorage3DEXT v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glTextureStorage3DEXT #-}
ptr_glTextureStorage3DEXT :: FunPtr (GLuint -> GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLsizei -> IO ())
ptr_glTextureStorage3DEXT = unsafePerformIO $ getCommand "glTextureStorage3DEXT"

-- glTextureStorage3DMultisample -----------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glTexStorage3DMultisample.xhtml OpenGL 4.x>.
glTextureStorage3DMultisample
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLsizei -- ^ @samples@.
  -> GLenum -- ^ @internalformat@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLboolean -- ^ @fixedsamplelocations@.
  -> m ()
glTextureStorage3DMultisample v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn765 ptr_glTextureStorage3DMultisample v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glTextureStorage3DMultisample #-}
ptr_glTextureStorage3DMultisample :: FunPtr (GLuint -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> IO ())
ptr_glTextureStorage3DMultisample = unsafePerformIO $ getCommand "glTextureStorage3DMultisample"

-- glTextureStorage3DMultisampleEXT --------------------------------------------

glTextureStorage3DMultisampleEXT
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLenum -- ^ @target@.
  -> GLsizei -- ^ @samples@.
  -> GLenum -- ^ @internalformat@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLboolean -- ^ @fixedsamplelocations@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glTextureStorage3DMultisampleEXT v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn766 ptr_glTextureStorage3DMultisampleEXT v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glTextureStorage3DMultisampleEXT #-}
ptr_glTextureStorage3DMultisampleEXT :: FunPtr (GLuint -> GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> IO ())
ptr_glTextureStorage3DMultisampleEXT = unsafePerformIO $ getCommand "glTextureStorage3DMultisampleEXT"

