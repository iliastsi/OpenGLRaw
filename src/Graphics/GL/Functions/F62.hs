--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F62
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

module Graphics.GL.Functions.F62 (
  glTexImage3DMultisampleCoverageNV,
  glTexImage3DOES,
  glTexImage4DSGIS,
  glTexPageCommitmentARB,
  glTexPageCommitmentEXT,
  glTexParameterIiv,
  glTexParameterIivEXT,
  glTexParameterIivOES,
  glTexParameterIuiv,
  glTexParameterIuivEXT,
  glTexParameterIuivOES,
  glTexParameterf,
  glTexParameterfv,
  glTexParameteri,
  glTexParameteriv,
  glTexParameterx,
  glTexParameterxOES,
  glTexParameterxv,
  glTexParameterxvOES,
  glTexRenderbufferNV,
  glTexStorage1D,
  glTexStorage1DEXT,
  glTexStorage2D,
  glTexStorage2DEXT,
  glTexStorage2DMultisample,
  glTexStorage3D,
  glTexStorage3DEXT,
  glTexStorage3DMultisample,
  glTexStorage3DMultisampleOES,
  glTexStorageSparseAMD,
  glTexSubImage1D,
  glTexSubImage1DEXT,
  glTexSubImage2D,
  glTexSubImage2DEXT,
  glTexSubImage3D,
  glTexSubImage3DEXT,
  glTexSubImage3DOES,
  glTexSubImage4DSGIS,
  glTextureBarrier,
  glTextureBarrierNV
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

-- glTexImage3DMultisampleCoverageNV -------------------------------------------

glTexImage3DMultisampleCoverageNV
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLsizei -- ^ @coverageSamples@.
  -> GLsizei -- ^ @colorSamples@.
  -> GLint -- ^ @internalFormat@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLboolean -- ^ @fixedSampleLocations@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glTexImage3DMultisampleCoverageNV v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn737 ptr_glTexImage3DMultisampleCoverageNV v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glTexImage3DMultisampleCoverageNV #-}
ptr_glTexImage3DMultisampleCoverageNV :: FunPtr (GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> IO ())
ptr_glTexImage3DMultisampleCoverageNV = unsafePerformIO $ getCommand "glTexImage3DMultisampleCoverageNV"

-- glTexImage3DOES -------------------------------------------------------------

-- | This command is an alias for 'glTexImage3D'.
glTexImage3DOES
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLint -- ^ @level@.
  -> GLenum -- ^ @internalformat@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLint -- ^ @border@.
  -> GLenum -- ^ @format@.
  -> GLenum -- ^ @type@.
  -> Ptr a -- ^ @pixels@ pointing to @COMPSIZE(format,type,width,height,depth)@ elements of type @a@.
  -> m ()
glTexImage3DOES v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO $ dyn735 ptr_glTexImage3DOES v1 v2 v3 v4 v5 v6 v7 v8 v9 v10

{-# NOINLINE ptr_glTexImage3DOES #-}
ptr_glTexImage3DOES :: FunPtr (GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glTexImage3DOES = unsafePerformIO $ getCommand "glTexImage3DOES"

-- glTexImage4DSGIS ------------------------------------------------------------

glTexImage4DSGIS
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLsizei -- ^ @size4d@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @pixels@ pointing to @COMPSIZE(format,type,width,height,depth,size4d)@ elements of type @a@.
  -> m ()
glTexImage4DSGIS v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 = liftIO $ dyn738 ptr_glTexImage4DSGIS v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11

{-# NOINLINE ptr_glTexImage4DSGIS #-}
ptr_glTexImage4DSGIS :: FunPtr (GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glTexImage4DSGIS = unsafePerformIO $ getCommand "glTexImage4DSGIS"

-- glTexPageCommitmentARB ------------------------------------------------------

glTexPageCommitmentARB
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLint -- ^ @level@.
  -> GLint -- ^ @xoffset@.
  -> GLint -- ^ @yoffset@.
  -> GLint -- ^ @zoffset@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLboolean -- ^ @commit@.
  -> m ()
glTexPageCommitmentARB v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn739 ptr_glTexPageCommitmentARB v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glTexPageCommitmentARB #-}
ptr_glTexPageCommitmentARB :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> IO ())
ptr_glTexPageCommitmentARB = unsafePerformIO $ getCommand "glTexPageCommitmentARB"

-- glTexPageCommitmentEXT ------------------------------------------------------

-- | This command is an alias for 'glTexPageCommitmentARB'.
glTexPageCommitmentEXT
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLint -- ^ @level@.
  -> GLint -- ^ @xoffset@.
  -> GLint -- ^ @yoffset@.
  -> GLint -- ^ @zoffset@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLboolean -- ^ @commit@.
  -> m ()
glTexPageCommitmentEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn739 ptr_glTexPageCommitmentEXT v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glTexPageCommitmentEXT #-}
ptr_glTexPageCommitmentEXT :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> IO ())
ptr_glTexPageCommitmentEXT = unsafePerformIO $ getCommand "glTexPageCommitmentEXT"

-- glTexParameterIiv -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glTexParameter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glTexParameter.xhtml OpenGL 4.x>.
glTexParameterIiv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [TextureParameterName](Graphics-GL-Groups.html#TextureParameterName).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glTexParameterIiv v1 v2 v3 = liftIO $ dyn133 ptr_glTexParameterIiv v1 v2 v3

{-# NOINLINE ptr_glTexParameterIiv #-}
ptr_glTexParameterIiv :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glTexParameterIiv = unsafePerformIO $ getCommand "glTexParameterIiv"

-- glTexParameterIivEXT --------------------------------------------------------

-- | This command is an alias for 'glTexParameterIiv'.
glTexParameterIivEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [TextureParameterName](Graphics-GL-Groups.html#TextureParameterName).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glTexParameterIivEXT v1 v2 v3 = liftIO $ dyn133 ptr_glTexParameterIivEXT v1 v2 v3

{-# NOINLINE ptr_glTexParameterIivEXT #-}
ptr_glTexParameterIivEXT :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glTexParameterIivEXT = unsafePerformIO $ getCommand "glTexParameterIivEXT"

-- glTexParameterIivOES --------------------------------------------------------

-- | This command is an alias for 'glTexParameterIiv'.
glTexParameterIivOES
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [TextureParameterName](Graphics-GL-Groups.html#TextureParameterName).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glTexParameterIivOES v1 v2 v3 = liftIO $ dyn133 ptr_glTexParameterIivOES v1 v2 v3

{-# NOINLINE ptr_glTexParameterIivOES #-}
ptr_glTexParameterIivOES :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glTexParameterIivOES = unsafePerformIO $ getCommand "glTexParameterIivOES"

-- glTexParameterIuiv ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glTexParameter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glTexParameter.xhtml OpenGL 4.x>.
glTexParameterIuiv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [TextureParameterName](Graphics-GL-Groups.html#TextureParameterName).
  -> Ptr GLuint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLuint@.
  -> m ()
glTexParameterIuiv v1 v2 v3 = liftIO $ dyn413 ptr_glTexParameterIuiv v1 v2 v3

{-# NOINLINE ptr_glTexParameterIuiv #-}
ptr_glTexParameterIuiv :: FunPtr (GLenum -> GLenum -> Ptr GLuint -> IO ())
ptr_glTexParameterIuiv = unsafePerformIO $ getCommand "glTexParameterIuiv"

-- glTexParameterIuivEXT -------------------------------------------------------

-- | This command is an alias for 'glTexParameterIuiv'.
glTexParameterIuivEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [TextureParameterName](Graphics-GL-Groups.html#TextureParameterName).
  -> Ptr GLuint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLuint@.
  -> m ()
glTexParameterIuivEXT v1 v2 v3 = liftIO $ dyn413 ptr_glTexParameterIuivEXT v1 v2 v3

{-# NOINLINE ptr_glTexParameterIuivEXT #-}
ptr_glTexParameterIuivEXT :: FunPtr (GLenum -> GLenum -> Ptr GLuint -> IO ())
ptr_glTexParameterIuivEXT = unsafePerformIO $ getCommand "glTexParameterIuivEXT"

-- glTexParameterIuivOES -------------------------------------------------------

-- | This command is an alias for 'glTexParameterIuiv'.
glTexParameterIuivOES
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [TextureParameterName](Graphics-GL-Groups.html#TextureParameterName).
  -> Ptr GLuint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLuint@.
  -> m ()
glTexParameterIuivOES v1 v2 v3 = liftIO $ dyn413 ptr_glTexParameterIuivOES v1 v2 v3

{-# NOINLINE ptr_glTexParameterIuivOES #-}
ptr_glTexParameterIuivOES :: FunPtr (GLenum -> GLenum -> Ptr GLuint -> IO ())
ptr_glTexParameterIuivOES = unsafePerformIO $ getCommand "glTexParameterIuivOES"

-- glTexParameterf -------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexParameter.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glTexParameter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glTexParameter.xhtml OpenGL 4.x>.
glTexParameterf
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [TextureParameterName](Graphics-GL-Groups.html#TextureParameterName).
  -> GLfloat -- ^ @param@ of type @CheckedFloat32@.
  -> m ()
glTexParameterf v1 v2 v3 = liftIO $ dyn161 ptr_glTexParameterf v1 v2 v3

{-# NOINLINE ptr_glTexParameterf #-}
ptr_glTexParameterf :: FunPtr (GLenum -> GLenum -> GLfloat -> IO ())
ptr_glTexParameterf = unsafePerformIO $ getCommand "glTexParameterf"

-- glTexParameterfv ------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexParameter.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glTexParameter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glTexParameter.xhtml OpenGL 4.x>.
glTexParameterfv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [TextureParameterName](Graphics-GL-Groups.html#TextureParameterName).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedFloat32@.
  -> m ()
glTexParameterfv v1 v2 v3 = liftIO $ dyn132 ptr_glTexParameterfv v1 v2 v3

{-# NOINLINE ptr_glTexParameterfv #-}
ptr_glTexParameterfv :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glTexParameterfv = unsafePerformIO $ getCommand "glTexParameterfv"

-- glTexParameteri -------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexParameter.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glTexParameter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glTexParameter.xhtml OpenGL 4.x>.
glTexParameteri
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [TextureParameterName](Graphics-GL-Groups.html#TextureParameterName).
  -> GLint -- ^ @param@ of type @CheckedInt32@.
  -> m ()
glTexParameteri v1 v2 v3 = liftIO $ dyn62 ptr_glTexParameteri v1 v2 v3

{-# NOINLINE ptr_glTexParameteri #-}
ptr_glTexParameteri :: FunPtr (GLenum -> GLenum -> GLint -> IO ())
ptr_glTexParameteri = unsafePerformIO $ getCommand "glTexParameteri"

-- glTexParameteriv ------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexParameter.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glTexParameter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glTexParameter.xhtml OpenGL 4.x>.
glTexParameteriv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [TextureParameterName](Graphics-GL-Groups.html#TextureParameterName).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedInt32@.
  -> m ()
glTexParameteriv v1 v2 v3 = liftIO $ dyn133 ptr_glTexParameteriv v1 v2 v3

{-# NOINLINE ptr_glTexParameteriv #-}
ptr_glTexParameteriv :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glTexParameteriv = unsafePerformIO $ getCommand "glTexParameteriv"

-- glTexParameterx -------------------------------------------------------------

glTexParameterx
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @pname@.
  -> GLfixed -- ^ @param@.
  -> m ()
glTexParameterx v1 v2 v3 = liftIO $ dyn162 ptr_glTexParameterx v1 v2 v3

{-# NOINLINE ptr_glTexParameterx #-}
ptr_glTexParameterx :: FunPtr (GLenum -> GLenum -> GLfixed -> IO ())
ptr_glTexParameterx = unsafePerformIO $ getCommand "glTexParameterx"

-- glTexParameterxOES ----------------------------------------------------------

glTexParameterxOES
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @pname@.
  -> GLfixed -- ^ @param@.
  -> m ()
glTexParameterxOES v1 v2 v3 = liftIO $ dyn162 ptr_glTexParameterxOES v1 v2 v3

{-# NOINLINE ptr_glTexParameterxOES #-}
ptr_glTexParameterxOES :: FunPtr (GLenum -> GLenum -> GLfixed -> IO ())
ptr_glTexParameterxOES = unsafePerformIO $ getCommand "glTexParameterxOES"

-- glTexParameterxv ------------------------------------------------------------

glTexParameterxv
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLfixed -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfixed@.
  -> m ()
glTexParameterxv v1 v2 v3 = liftIO $ dyn163 ptr_glTexParameterxv v1 v2 v3

{-# NOINLINE ptr_glTexParameterxv #-}
ptr_glTexParameterxv :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
ptr_glTexParameterxv = unsafePerformIO $ getCommand "glTexParameterxv"

-- glTexParameterxvOES ---------------------------------------------------------

glTexParameterxvOES
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLfixed -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfixed@.
  -> m ()
glTexParameterxvOES v1 v2 v3 = liftIO $ dyn163 ptr_glTexParameterxvOES v1 v2 v3

{-# NOINLINE ptr_glTexParameterxvOES #-}
ptr_glTexParameterxvOES :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
ptr_glTexParameterxvOES = unsafePerformIO $ getCommand "glTexParameterxvOES"

-- glTexRenderbufferNV ---------------------------------------------------------

glTexRenderbufferNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLuint -- ^ @renderbuffer@.
  -> m ()
glTexRenderbufferNV v1 v2 = liftIO $ dyn16 ptr_glTexRenderbufferNV v1 v2

{-# NOINLINE ptr_glTexRenderbufferNV #-}
ptr_glTexRenderbufferNV :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glTexRenderbufferNV = unsafePerformIO $ getCommand "glTexRenderbufferNV"

-- glTexStorage1D --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glTexStorage1D.xhtml OpenGL 4.x>.
glTexStorage1D
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLsizei -- ^ @levels@.
  -> GLenum -- ^ @internalformat@.
  -> GLsizei -- ^ @width@.
  -> m ()
glTexStorage1D v1 v2 v3 v4 = liftIO $ dyn740 ptr_glTexStorage1D v1 v2 v3 v4

{-# NOINLINE ptr_glTexStorage1D #-}
ptr_glTexStorage1D :: FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> IO ())
ptr_glTexStorage1D = unsafePerformIO $ getCommand "glTexStorage1D"

-- glTexStorage1DEXT -----------------------------------------------------------

-- | This command is an alias for 'glTexStorage1D'.
glTexStorage1DEXT
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLsizei -- ^ @levels@.
  -> GLenum -- ^ @internalformat@.
  -> GLsizei -- ^ @width@.
  -> m ()
glTexStorage1DEXT v1 v2 v3 v4 = liftIO $ dyn740 ptr_glTexStorage1DEXT v1 v2 v3 v4

{-# NOINLINE ptr_glTexStorage1DEXT #-}
ptr_glTexStorage1DEXT :: FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> IO ())
ptr_glTexStorage1DEXT = unsafePerformIO $ getCommand "glTexStorage1DEXT"

-- glTexStorage2D --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glTexStorage2D.xhtml OpenGL 4.x>.
glTexStorage2D
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLsizei -- ^ @levels@.
  -> GLenum -- ^ @internalformat@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glTexStorage2D v1 v2 v3 v4 v5 = liftIO $ dyn679 ptr_glTexStorage2D v1 v2 v3 v4 v5

{-# NOINLINE ptr_glTexStorage2D #-}
ptr_glTexStorage2D :: FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> IO ())
ptr_glTexStorage2D = unsafePerformIO $ getCommand "glTexStorage2D"

-- glTexStorage2DEXT -----------------------------------------------------------

-- | This command is an alias for 'glTexStorage2D'.
glTexStorage2DEXT
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLsizei -- ^ @levels@.
  -> GLenum -- ^ @internalformat@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glTexStorage2DEXT v1 v2 v3 v4 v5 = liftIO $ dyn679 ptr_glTexStorage2DEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glTexStorage2DEXT #-}
ptr_glTexStorage2DEXT :: FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> IO ())
ptr_glTexStorage2DEXT = unsafePerformIO $ getCommand "glTexStorage2DEXT"

-- glTexStorage2DMultisample ---------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glTexStorage2DMultisample.xhtml OpenGL 4.x>.
glTexStorage2DMultisample
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLsizei -- ^ @samples@.
  -> GLenum -- ^ @internalformat@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLboolean -- ^ @fixedsamplelocations@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glTexStorage2DMultisample v1 v2 v3 v4 v5 v6 = liftIO $ dyn732 ptr_glTexStorage2DMultisample v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glTexStorage2DMultisample #-}
ptr_glTexStorage2DMultisample :: FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLboolean -> IO ())
ptr_glTexStorage2DMultisample = unsafePerformIO $ getCommand "glTexStorage2DMultisample"

-- glTexStorage3D --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glTexStorage3D.xhtml OpenGL 4.x>.
glTexStorage3D
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLsizei -- ^ @levels@.
  -> GLenum -- ^ @internalformat@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> m ()
glTexStorage3D v1 v2 v3 v4 v5 v6 = liftIO $ dyn741 ptr_glTexStorage3D v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glTexStorage3D #-}
ptr_glTexStorage3D :: FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLsizei -> IO ())
ptr_glTexStorage3D = unsafePerformIO $ getCommand "glTexStorage3D"

-- glTexStorage3DEXT -----------------------------------------------------------

-- | This command is an alias for 'glTexStorage3D'.
glTexStorage3DEXT
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLsizei -- ^ @levels@.
  -> GLenum -- ^ @internalformat@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> m ()
glTexStorage3DEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn741 ptr_glTexStorage3DEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glTexStorage3DEXT #-}
ptr_glTexStorage3DEXT :: FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLsizei -> IO ())
ptr_glTexStorage3DEXT = unsafePerformIO $ getCommand "glTexStorage3DEXT"

-- glTexStorage3DMultisample ---------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glTexStorage3DMultisample.xhtml OpenGL 4.x>.
glTexStorage3DMultisample
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLsizei -- ^ @samples@.
  -> GLenum -- ^ @internalformat@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLboolean -- ^ @fixedsamplelocations@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glTexStorage3DMultisample v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn736 ptr_glTexStorage3DMultisample v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glTexStorage3DMultisample #-}
ptr_glTexStorage3DMultisample :: FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> IO ())
ptr_glTexStorage3DMultisample = unsafePerformIO $ getCommand "glTexStorage3DMultisample"

-- glTexStorage3DMultisampleOES ------------------------------------------------

-- | This command is an alias for 'glTexStorage3DMultisample'.
glTexStorage3DMultisampleOES
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLsizei -- ^ @samples@.
  -> GLenum -- ^ @internalformat@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLboolean -- ^ @fixedsamplelocations@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glTexStorage3DMultisampleOES v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn736 ptr_glTexStorage3DMultisampleOES v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glTexStorage3DMultisampleOES #-}
ptr_glTexStorage3DMultisampleOES :: FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> IO ())
ptr_glTexStorage3DMultisampleOES = unsafePerformIO $ getCommand "glTexStorage3DMultisampleOES"

-- glTexStorageSparseAMD -------------------------------------------------------

glTexStorageSparseAMD
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @internalFormat@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLsizei -- ^ @layers@.
  -> GLbitfield -- ^ @flags@.
  -> m ()
glTexStorageSparseAMD v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn742 ptr_glTexStorageSparseAMD v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glTexStorageSparseAMD #-}
ptr_glTexStorageSparseAMD :: FunPtr (GLenum -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLsizei -> GLbitfield -> IO ())
ptr_glTexStorageSparseAMD = unsafePerformIO $ getCommand "glTexStorageSparseAMD"

-- glTexSubImage1D -------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexSubImage1D.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glTexSubImage1D.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glTexSubImage1D.xhtml OpenGL 4.x>.
glTexSubImage1D
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLsizei -- ^ @width@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @pixels@ pointing to @COMPSIZE(format,type,width)@ elements of type @a@.
  -> m ()
glTexSubImage1D v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn743 ptr_glTexSubImage1D v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glTexSubImage1D #-}
ptr_glTexSubImage1D :: FunPtr (GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glTexSubImage1D = unsafePerformIO $ getCommand "glTexSubImage1D"

-- glTexSubImage1DEXT ----------------------------------------------------------

-- | This command is an alias for 'glTexSubImage1D'.
glTexSubImage1DEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLsizei -- ^ @width@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @pixels@ pointing to @COMPSIZE(format,type,width)@ elements of type @a@.
  -> m ()
glTexSubImage1DEXT v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn743 ptr_glTexSubImage1DEXT v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glTexSubImage1DEXT #-}
ptr_glTexSubImage1DEXT :: FunPtr (GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glTexSubImage1DEXT = unsafePerformIO $ getCommand "glTexSubImage1DEXT"

-- glTexSubImage2D -------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexSubImage2D.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glTexSubImage2D.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glTexSubImage2D.xhtml OpenGL 4.x>.
glTexSubImage2D
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @yoffset@ of type @CheckedInt32@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @pixels@ pointing to @COMPSIZE(format,type,width,height)@ elements of type @a@.
  -> m ()
glTexSubImage2D v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn744 ptr_glTexSubImage2D v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glTexSubImage2D #-}
ptr_glTexSubImage2D :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glTexSubImage2D = unsafePerformIO $ getCommand "glTexSubImage2D"

-- glTexSubImage2DEXT ----------------------------------------------------------

-- | This command is an alias for 'glTexSubImage2D'.
glTexSubImage2DEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @yoffset@ of type @CheckedInt32@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @pixels@ pointing to @COMPSIZE(format,type,width,height)@ elements of type @a@.
  -> m ()
glTexSubImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn744 ptr_glTexSubImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glTexSubImage2DEXT #-}
ptr_glTexSubImage2DEXT :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glTexSubImage2DEXT = unsafePerformIO $ getCommand "glTexSubImage2DEXT"

-- glTexSubImage3D -------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexSubImage3D.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glTexSubImage3D.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glTexSubImage3D.xhtml OpenGL 4.x>.
glTexSubImage3D
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
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @pixels@ pointing to @COMPSIZE(format,type,width,height,depth)@ elements of type @a@.
  -> m ()
glTexSubImage3D v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 = liftIO $ dyn272 ptr_glTexSubImage3D v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11

{-# NOINLINE ptr_glTexSubImage3D #-}
ptr_glTexSubImage3D :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glTexSubImage3D = unsafePerformIO $ getCommand "glTexSubImage3D"

-- glTexSubImage3DEXT ----------------------------------------------------------

-- | This command is an alias for 'glTexSubImage3D'.
glTexSubImage3DEXT
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
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @pixels@ pointing to @COMPSIZE(format,type,width,height,depth)@ elements of type @a@.
  -> m ()
glTexSubImage3DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 = liftIO $ dyn272 ptr_glTexSubImage3DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11

{-# NOINLINE ptr_glTexSubImage3DEXT #-}
ptr_glTexSubImage3DEXT :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glTexSubImage3DEXT = unsafePerformIO $ getCommand "glTexSubImage3DEXT"

-- glTexSubImage3DOES ----------------------------------------------------------

-- | This command is an alias for 'glTexSubImage3D'.
glTexSubImage3DOES
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
  -> GLenum -- ^ @type@.
  -> Ptr a -- ^ @pixels@ pointing to @COMPSIZE(format,type,width,height,depth)@ elements of type @a@.
  -> m ()
glTexSubImage3DOES v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 = liftIO $ dyn272 ptr_glTexSubImage3DOES v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11

{-# NOINLINE ptr_glTexSubImage3DOES #-}
ptr_glTexSubImage3DOES :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glTexSubImage3DOES = unsafePerformIO $ getCommand "glTexSubImage3DOES"

-- glTexSubImage4DSGIS ---------------------------------------------------------

glTexSubImage4DSGIS
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @yoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @zoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @woffset@ of type @CheckedInt32@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLsizei -- ^ @size4d@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @pixels@ pointing to @COMPSIZE(format,type,width,height,depth,size4d)@ elements of type @a@.
  -> m ()
glTexSubImage4DSGIS v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 = liftIO $ dyn745 ptr_glTexSubImage4DSGIS v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13

{-# NOINLINE ptr_glTexSubImage4DSGIS #-}
ptr_glTexSubImage4DSGIS :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glTexSubImage4DSGIS = unsafePerformIO $ getCommand "glTexSubImage4DSGIS"

-- glTextureBarrier ------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glTextureBarrier.xhtml OpenGL 4.x>.
glTextureBarrier
  :: MonadIO m
  => m ()
glTextureBarrier = liftIO $ dyn10 ptr_glTextureBarrier

{-# NOINLINE ptr_glTextureBarrier #-}
ptr_glTextureBarrier :: FunPtr (IO ())
ptr_glTextureBarrier = unsafePerformIO $ getCommand "glTextureBarrier"

-- glTextureBarrierNV ----------------------------------------------------------

glTextureBarrierNV
  :: MonadIO m
  => m ()
glTextureBarrierNV = liftIO $ dyn10 ptr_glTextureBarrierNV

{-# NOINLINE ptr_glTextureBarrierNV #-}
ptr_glTextureBarrierNV :: FunPtr (IO ())
ptr_glTextureBarrierNV = unsafePerformIO $ getCommand "glTextureBarrierNV"

