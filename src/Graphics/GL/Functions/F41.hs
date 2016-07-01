--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F41
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

module Graphics.GL.Functions.F41 (
  glMultiTexCoord2fvARB,
  glMultiTexCoord2hNV,
  glMultiTexCoord2hvNV,
  glMultiTexCoord2i,
  glMultiTexCoord2iARB,
  glMultiTexCoord2iv,
  glMultiTexCoord2ivARB,
  glMultiTexCoord2s,
  glMultiTexCoord2sARB,
  glMultiTexCoord2sv,
  glMultiTexCoord2svARB,
  glMultiTexCoord2xOES,
  glMultiTexCoord2xvOES,
  glMultiTexCoord3bOES,
  glMultiTexCoord3bvOES,
  glMultiTexCoord3d,
  glMultiTexCoord3dARB,
  glMultiTexCoord3dv,
  glMultiTexCoord3dvARB,
  glMultiTexCoord3f,
  glMultiTexCoord3fARB,
  glMultiTexCoord3fv,
  glMultiTexCoord3fvARB,
  glMultiTexCoord3hNV,
  glMultiTexCoord3hvNV,
  glMultiTexCoord3i,
  glMultiTexCoord3iARB,
  glMultiTexCoord3iv,
  glMultiTexCoord3ivARB,
  glMultiTexCoord3s,
  glMultiTexCoord3sARB,
  glMultiTexCoord3sv,
  glMultiTexCoord3svARB,
  glMultiTexCoord3xOES,
  glMultiTexCoord3xvOES,
  glMultiTexCoord4bOES,
  glMultiTexCoord4bvOES,
  glMultiTexCoord4d,
  glMultiTexCoord4dARB,
  glMultiTexCoord4dv
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

-- glMultiTexCoord2fvARB -------------------------------------------------------

-- | This command is an alias for 'glMultiTexCoord2fv'.
glMultiTexCoord2fvARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> Ptr GLfloat -- ^ @v@ pointing to @2@ elements of type @CoordF@.
  -> m ()
glMultiTexCoord2fvARB v1 v2 = liftIO $ dyn94 ptr_glMultiTexCoord2fvARB v1 v2

{-# NOINLINE ptr_glMultiTexCoord2fvARB #-}
ptr_glMultiTexCoord2fvARB :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glMultiTexCoord2fvARB = unsafePerformIO $ getCommand "glMultiTexCoord2fvARB"

-- glMultiTexCoord2hNV ---------------------------------------------------------

-- | The vector equivalent of this command is 'glMultiTexCoord2hvNV'.
glMultiTexCoord2hNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> GLhalfNV -- ^ @s@ of type @Half16NV@.
  -> GLhalfNV -- ^ @t@ of type @Half16NV@.
  -> m ()
glMultiTexCoord2hNV v1 v2 v3 = liftIO $ dyn549 ptr_glMultiTexCoord2hNV v1 v2 v3

{-# NOINLINE ptr_glMultiTexCoord2hNV #-}
ptr_glMultiTexCoord2hNV :: FunPtr (GLenum -> GLhalfNV -> GLhalfNV -> IO ())
ptr_glMultiTexCoord2hNV = unsafePerformIO $ getCommand "glMultiTexCoord2hNV"

-- glMultiTexCoord2hvNV --------------------------------------------------------

glMultiTexCoord2hvNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> Ptr GLhalfNV -- ^ @v@ pointing to @2@ elements of type @Half16NV@.
  -> m ()
glMultiTexCoord2hvNV v1 v2 = liftIO $ dyn543 ptr_glMultiTexCoord2hvNV v1 v2

{-# NOINLINE ptr_glMultiTexCoord2hvNV #-}
ptr_glMultiTexCoord2hvNV :: FunPtr (GLenum -> Ptr GLhalfNV -> IO ())
ptr_glMultiTexCoord2hvNV = unsafePerformIO $ getCommand "glMultiTexCoord2hvNV"

-- glMultiTexCoord2i -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glMultiTexCoord2iv'.
glMultiTexCoord2i
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> GLint -- ^ @s@ of type @CoordI@.
  -> GLint -- ^ @t@ of type @CoordI@.
  -> m ()
glMultiTexCoord2i v1 v2 v3 = liftIO $ dyn264 ptr_glMultiTexCoord2i v1 v2 v3

{-# NOINLINE ptr_glMultiTexCoord2i #-}
ptr_glMultiTexCoord2i :: FunPtr (GLenum -> GLint -> GLint -> IO ())
ptr_glMultiTexCoord2i = unsafePerformIO $ getCommand "glMultiTexCoord2i"

-- glMultiTexCoord2iARB --------------------------------------------------------

-- | The vector equivalent of this command is 'glMultiTexCoord2iv'. This command is an alias for 'glMultiTexCoord2i'.
glMultiTexCoord2iARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> GLint -- ^ @s@ of type @CoordI@.
  -> GLint -- ^ @t@ of type @CoordI@.
  -> m ()
glMultiTexCoord2iARB v1 v2 v3 = liftIO $ dyn264 ptr_glMultiTexCoord2iARB v1 v2 v3

{-# NOINLINE ptr_glMultiTexCoord2iARB #-}
ptr_glMultiTexCoord2iARB :: FunPtr (GLenum -> GLint -> GLint -> IO ())
ptr_glMultiTexCoord2iARB = unsafePerformIO $ getCommand "glMultiTexCoord2iARB"

-- glMultiTexCoord2iv ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>.
glMultiTexCoord2iv
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> Ptr GLint -- ^ @v@ pointing to @2@ elements of type @CoordI@.
  -> m ()
glMultiTexCoord2iv v1 v2 = liftIO $ dyn136 ptr_glMultiTexCoord2iv v1 v2

{-# NOINLINE ptr_glMultiTexCoord2iv #-}
ptr_glMultiTexCoord2iv :: FunPtr (GLenum -> Ptr GLint -> IO ())
ptr_glMultiTexCoord2iv = unsafePerformIO $ getCommand "glMultiTexCoord2iv"

-- glMultiTexCoord2ivARB -------------------------------------------------------

-- | This command is an alias for 'glMultiTexCoord2iv'.
glMultiTexCoord2ivARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> Ptr GLint -- ^ @v@ pointing to @2@ elements of type @CoordI@.
  -> m ()
glMultiTexCoord2ivARB v1 v2 = liftIO $ dyn136 ptr_glMultiTexCoord2ivARB v1 v2

{-# NOINLINE ptr_glMultiTexCoord2ivARB #-}
ptr_glMultiTexCoord2ivARB :: FunPtr (GLenum -> Ptr GLint -> IO ())
ptr_glMultiTexCoord2ivARB = unsafePerformIO $ getCommand "glMultiTexCoord2ivARB"

-- glMultiTexCoord2s -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glMultiTexCoord2sv'.
glMultiTexCoord2s
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> GLshort -- ^ @s@ of type @CoordS@.
  -> GLshort -- ^ @t@ of type @CoordS@.
  -> m ()
glMultiTexCoord2s v1 v2 v3 = liftIO $ dyn550 ptr_glMultiTexCoord2s v1 v2 v3

{-# NOINLINE ptr_glMultiTexCoord2s #-}
ptr_glMultiTexCoord2s :: FunPtr (GLenum -> GLshort -> GLshort -> IO ())
ptr_glMultiTexCoord2s = unsafePerformIO $ getCommand "glMultiTexCoord2s"

-- glMultiTexCoord2sARB --------------------------------------------------------

-- | The vector equivalent of this command is 'glMultiTexCoord2sv'. This command is an alias for 'glMultiTexCoord2s'.
glMultiTexCoord2sARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> GLshort -- ^ @s@ of type @CoordS@.
  -> GLshort -- ^ @t@ of type @CoordS@.
  -> m ()
glMultiTexCoord2sARB v1 v2 v3 = liftIO $ dyn550 ptr_glMultiTexCoord2sARB v1 v2 v3

{-# NOINLINE ptr_glMultiTexCoord2sARB #-}
ptr_glMultiTexCoord2sARB :: FunPtr (GLenum -> GLshort -> GLshort -> IO ())
ptr_glMultiTexCoord2sARB = unsafePerformIO $ getCommand "glMultiTexCoord2sARB"

-- glMultiTexCoord2sv ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>.
glMultiTexCoord2sv
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> Ptr GLshort -- ^ @v@ pointing to @2@ elements of type @CoordS@.
  -> m ()
glMultiTexCoord2sv v1 v2 = liftIO $ dyn545 ptr_glMultiTexCoord2sv v1 v2

{-# NOINLINE ptr_glMultiTexCoord2sv #-}
ptr_glMultiTexCoord2sv :: FunPtr (GLenum -> Ptr GLshort -> IO ())
ptr_glMultiTexCoord2sv = unsafePerformIO $ getCommand "glMultiTexCoord2sv"

-- glMultiTexCoord2svARB -------------------------------------------------------

-- | This command is an alias for 'glMultiTexCoord2sv'.
glMultiTexCoord2svARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> Ptr GLshort -- ^ @v@ pointing to @2@ elements of type @CoordS@.
  -> m ()
glMultiTexCoord2svARB v1 v2 = liftIO $ dyn545 ptr_glMultiTexCoord2svARB v1 v2

{-# NOINLINE ptr_glMultiTexCoord2svARB #-}
ptr_glMultiTexCoord2svARB :: FunPtr (GLenum -> Ptr GLshort -> IO ())
ptr_glMultiTexCoord2svARB = unsafePerformIO $ getCommand "glMultiTexCoord2svARB"

-- glMultiTexCoord2xOES --------------------------------------------------------

glMultiTexCoord2xOES
  :: MonadIO m
  => GLenum -- ^ @texture@.
  -> GLfixed -- ^ @s@.
  -> GLfixed -- ^ @t@.
  -> m ()
glMultiTexCoord2xOES v1 v2 v3 = liftIO $ dyn551 ptr_glMultiTexCoord2xOES v1 v2 v3

{-# NOINLINE ptr_glMultiTexCoord2xOES #-}
ptr_glMultiTexCoord2xOES :: FunPtr (GLenum -> GLfixed -> GLfixed -> IO ())
ptr_glMultiTexCoord2xOES = unsafePerformIO $ getCommand "glMultiTexCoord2xOES"

-- glMultiTexCoord2xvOES -------------------------------------------------------

glMultiTexCoord2xvOES
  :: MonadIO m
  => GLenum -- ^ @texture@.
  -> Ptr GLfixed -- ^ @coords@ pointing to @2@ elements of type @GLfixed@.
  -> m ()
glMultiTexCoord2xvOES v1 v2 = liftIO $ dyn95 ptr_glMultiTexCoord2xvOES v1 v2

{-# NOINLINE ptr_glMultiTexCoord2xvOES #-}
ptr_glMultiTexCoord2xvOES :: FunPtr (GLenum -> Ptr GLfixed -> IO ())
ptr_glMultiTexCoord2xvOES = unsafePerformIO $ getCommand "glMultiTexCoord2xvOES"

-- glMultiTexCoord3bOES --------------------------------------------------------

glMultiTexCoord3bOES
  :: MonadIO m
  => GLenum -- ^ @texture@.
  -> GLbyte -- ^ @s@.
  -> GLbyte -- ^ @t@.
  -> GLbyte -- ^ @r@.
  -> m ()
glMultiTexCoord3bOES v1 v2 v3 v4 = liftIO $ dyn552 ptr_glMultiTexCoord3bOES v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexCoord3bOES #-}
ptr_glMultiTexCoord3bOES :: FunPtr (GLenum -> GLbyte -> GLbyte -> GLbyte -> IO ())
ptr_glMultiTexCoord3bOES = unsafePerformIO $ getCommand "glMultiTexCoord3bOES"

-- glMultiTexCoord3bvOES -------------------------------------------------------

glMultiTexCoord3bvOES
  :: MonadIO m
  => GLenum -- ^ @texture@.
  -> Ptr GLbyte -- ^ @coords@ pointing to @3@ elements of type @GLbyte@.
  -> m ()
glMultiTexCoord3bvOES v1 v2 = liftIO $ dyn540 ptr_glMultiTexCoord3bvOES v1 v2

{-# NOINLINE ptr_glMultiTexCoord3bvOES #-}
ptr_glMultiTexCoord3bvOES :: FunPtr (GLenum -> Ptr GLbyte -> IO ())
ptr_glMultiTexCoord3bvOES = unsafePerformIO $ getCommand "glMultiTexCoord3bvOES"

-- glMultiTexCoord3d -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glMultiTexCoord3dv'.
glMultiTexCoord3d
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> GLdouble -- ^ @s@ of type @CoordD@.
  -> GLdouble -- ^ @t@ of type @CoordD@.
  -> GLdouble -- ^ @r@ of type @CoordD@.
  -> m ()
glMultiTexCoord3d v1 v2 v3 v4 = liftIO $ dyn522 ptr_glMultiTexCoord3d v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexCoord3d #-}
ptr_glMultiTexCoord3d :: FunPtr (GLenum -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glMultiTexCoord3d = unsafePerformIO $ getCommand "glMultiTexCoord3d"

-- glMultiTexCoord3dARB --------------------------------------------------------

-- | The vector equivalent of this command is 'glMultiTexCoord3dv'. This command is an alias for 'glMultiTexCoord3d'.
glMultiTexCoord3dARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> GLdouble -- ^ @s@ of type @CoordD@.
  -> GLdouble -- ^ @t@ of type @CoordD@.
  -> GLdouble -- ^ @r@ of type @CoordD@.
  -> m ()
glMultiTexCoord3dARB v1 v2 v3 v4 = liftIO $ dyn522 ptr_glMultiTexCoord3dARB v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexCoord3dARB #-}
ptr_glMultiTexCoord3dARB :: FunPtr (GLenum -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glMultiTexCoord3dARB = unsafePerformIO $ getCommand "glMultiTexCoord3dARB"

-- glMultiTexCoord3dv ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>.
glMultiTexCoord3dv
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> Ptr GLdouble -- ^ @v@ pointing to @3@ elements of type @CoordD@.
  -> m ()
glMultiTexCoord3dv v1 v2 = liftIO $ dyn93 ptr_glMultiTexCoord3dv v1 v2

{-# NOINLINE ptr_glMultiTexCoord3dv #-}
ptr_glMultiTexCoord3dv :: FunPtr (GLenum -> Ptr GLdouble -> IO ())
ptr_glMultiTexCoord3dv = unsafePerformIO $ getCommand "glMultiTexCoord3dv"

-- glMultiTexCoord3dvARB -------------------------------------------------------

-- | This command is an alias for 'glMultiTexCoord3dv'.
glMultiTexCoord3dvARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> Ptr GLdouble -- ^ @v@ pointing to @3@ elements of type @CoordD@.
  -> m ()
glMultiTexCoord3dvARB v1 v2 = liftIO $ dyn93 ptr_glMultiTexCoord3dvARB v1 v2

{-# NOINLINE ptr_glMultiTexCoord3dvARB #-}
ptr_glMultiTexCoord3dvARB :: FunPtr (GLenum -> Ptr GLdouble -> IO ())
ptr_glMultiTexCoord3dvARB = unsafePerformIO $ getCommand "glMultiTexCoord3dvARB"

-- glMultiTexCoord3f -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glMultiTexCoord3fv'.
glMultiTexCoord3f
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> GLfloat -- ^ @s@ of type @CoordF@.
  -> GLfloat -- ^ @t@ of type @CoordF@.
  -> GLfloat -- ^ @r@ of type @CoordF@.
  -> m ()
glMultiTexCoord3f v1 v2 v3 v4 = liftIO $ dyn523 ptr_glMultiTexCoord3f v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexCoord3f #-}
ptr_glMultiTexCoord3f :: FunPtr (GLenum -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glMultiTexCoord3f = unsafePerformIO $ getCommand "glMultiTexCoord3f"

-- glMultiTexCoord3fARB --------------------------------------------------------

-- | The vector equivalent of this command is 'glMultiTexCoord3fv'. This command is an alias for 'glMultiTexCoord3f'.
glMultiTexCoord3fARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> GLfloat -- ^ @s@ of type @CoordF@.
  -> GLfloat -- ^ @t@ of type @CoordF@.
  -> GLfloat -- ^ @r@ of type @CoordF@.
  -> m ()
glMultiTexCoord3fARB v1 v2 v3 v4 = liftIO $ dyn523 ptr_glMultiTexCoord3fARB v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexCoord3fARB #-}
ptr_glMultiTexCoord3fARB :: FunPtr (GLenum -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glMultiTexCoord3fARB = unsafePerformIO $ getCommand "glMultiTexCoord3fARB"

-- glMultiTexCoord3fv ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>.
glMultiTexCoord3fv
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @CoordF@.
  -> m ()
glMultiTexCoord3fv v1 v2 = liftIO $ dyn94 ptr_glMultiTexCoord3fv v1 v2

{-# NOINLINE ptr_glMultiTexCoord3fv #-}
ptr_glMultiTexCoord3fv :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glMultiTexCoord3fv = unsafePerformIO $ getCommand "glMultiTexCoord3fv"

-- glMultiTexCoord3fvARB -------------------------------------------------------

-- | This command is an alias for 'glMultiTexCoord3fv'.
glMultiTexCoord3fvARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @CoordF@.
  -> m ()
glMultiTexCoord3fvARB v1 v2 = liftIO $ dyn94 ptr_glMultiTexCoord3fvARB v1 v2

{-# NOINLINE ptr_glMultiTexCoord3fvARB #-}
ptr_glMultiTexCoord3fvARB :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glMultiTexCoord3fvARB = unsafePerformIO $ getCommand "glMultiTexCoord3fvARB"

-- glMultiTexCoord3hNV ---------------------------------------------------------

-- | The vector equivalent of this command is 'glMultiTexCoord3hvNV'.
glMultiTexCoord3hNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> GLhalfNV -- ^ @s@ of type @Half16NV@.
  -> GLhalfNV -- ^ @t@ of type @Half16NV@.
  -> GLhalfNV -- ^ @r@ of type @Half16NV@.
  -> m ()
glMultiTexCoord3hNV v1 v2 v3 v4 = liftIO $ dyn553 ptr_glMultiTexCoord3hNV v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexCoord3hNV #-}
ptr_glMultiTexCoord3hNV :: FunPtr (GLenum -> GLhalfNV -> GLhalfNV -> GLhalfNV -> IO ())
ptr_glMultiTexCoord3hNV = unsafePerformIO $ getCommand "glMultiTexCoord3hNV"

-- glMultiTexCoord3hvNV --------------------------------------------------------

glMultiTexCoord3hvNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> Ptr GLhalfNV -- ^ @v@ pointing to @3@ elements of type @Half16NV@.
  -> m ()
glMultiTexCoord3hvNV v1 v2 = liftIO $ dyn543 ptr_glMultiTexCoord3hvNV v1 v2

{-# NOINLINE ptr_glMultiTexCoord3hvNV #-}
ptr_glMultiTexCoord3hvNV :: FunPtr (GLenum -> Ptr GLhalfNV -> IO ())
ptr_glMultiTexCoord3hvNV = unsafePerformIO $ getCommand "glMultiTexCoord3hvNV"

-- glMultiTexCoord3i -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glMultiTexCoord3iv'.
glMultiTexCoord3i
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> GLint -- ^ @s@ of type @CoordI@.
  -> GLint -- ^ @t@ of type @CoordI@.
  -> GLint -- ^ @r@ of type @CoordI@.
  -> m ()
glMultiTexCoord3i v1 v2 v3 v4 = liftIO $ dyn554 ptr_glMultiTexCoord3i v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexCoord3i #-}
ptr_glMultiTexCoord3i :: FunPtr (GLenum -> GLint -> GLint -> GLint -> IO ())
ptr_glMultiTexCoord3i = unsafePerformIO $ getCommand "glMultiTexCoord3i"

-- glMultiTexCoord3iARB --------------------------------------------------------

-- | The vector equivalent of this command is 'glMultiTexCoord3iv'. This command is an alias for 'glMultiTexCoord3i'.
glMultiTexCoord3iARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> GLint -- ^ @s@ of type @CoordI@.
  -> GLint -- ^ @t@ of type @CoordI@.
  -> GLint -- ^ @r@ of type @CoordI@.
  -> m ()
glMultiTexCoord3iARB v1 v2 v3 v4 = liftIO $ dyn554 ptr_glMultiTexCoord3iARB v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexCoord3iARB #-}
ptr_glMultiTexCoord3iARB :: FunPtr (GLenum -> GLint -> GLint -> GLint -> IO ())
ptr_glMultiTexCoord3iARB = unsafePerformIO $ getCommand "glMultiTexCoord3iARB"

-- glMultiTexCoord3iv ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>.
glMultiTexCoord3iv
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> Ptr GLint -- ^ @v@ pointing to @3@ elements of type @CoordI@.
  -> m ()
glMultiTexCoord3iv v1 v2 = liftIO $ dyn136 ptr_glMultiTexCoord3iv v1 v2

{-# NOINLINE ptr_glMultiTexCoord3iv #-}
ptr_glMultiTexCoord3iv :: FunPtr (GLenum -> Ptr GLint -> IO ())
ptr_glMultiTexCoord3iv = unsafePerformIO $ getCommand "glMultiTexCoord3iv"

-- glMultiTexCoord3ivARB -------------------------------------------------------

-- | This command is an alias for 'glMultiTexCoord3iv'.
glMultiTexCoord3ivARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> Ptr GLint -- ^ @v@ pointing to @3@ elements of type @CoordI@.
  -> m ()
glMultiTexCoord3ivARB v1 v2 = liftIO $ dyn136 ptr_glMultiTexCoord3ivARB v1 v2

{-# NOINLINE ptr_glMultiTexCoord3ivARB #-}
ptr_glMultiTexCoord3ivARB :: FunPtr (GLenum -> Ptr GLint -> IO ())
ptr_glMultiTexCoord3ivARB = unsafePerformIO $ getCommand "glMultiTexCoord3ivARB"

-- glMultiTexCoord3s -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glMultiTexCoord3sv'.
glMultiTexCoord3s
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> GLshort -- ^ @s@ of type @CoordS@.
  -> GLshort -- ^ @t@ of type @CoordS@.
  -> GLshort -- ^ @r@ of type @CoordS@.
  -> m ()
glMultiTexCoord3s v1 v2 v3 v4 = liftIO $ dyn555 ptr_glMultiTexCoord3s v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexCoord3s #-}
ptr_glMultiTexCoord3s :: FunPtr (GLenum -> GLshort -> GLshort -> GLshort -> IO ())
ptr_glMultiTexCoord3s = unsafePerformIO $ getCommand "glMultiTexCoord3s"

-- glMultiTexCoord3sARB --------------------------------------------------------

-- | The vector equivalent of this command is 'glMultiTexCoord3sv'. This command is an alias for 'glMultiTexCoord3s'.
glMultiTexCoord3sARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> GLshort -- ^ @s@ of type @CoordS@.
  -> GLshort -- ^ @t@ of type @CoordS@.
  -> GLshort -- ^ @r@ of type @CoordS@.
  -> m ()
glMultiTexCoord3sARB v1 v2 v3 v4 = liftIO $ dyn555 ptr_glMultiTexCoord3sARB v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexCoord3sARB #-}
ptr_glMultiTexCoord3sARB :: FunPtr (GLenum -> GLshort -> GLshort -> GLshort -> IO ())
ptr_glMultiTexCoord3sARB = unsafePerformIO $ getCommand "glMultiTexCoord3sARB"

-- glMultiTexCoord3sv ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>.
glMultiTexCoord3sv
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> Ptr GLshort -- ^ @v@ pointing to @3@ elements of type @CoordS@.
  -> m ()
glMultiTexCoord3sv v1 v2 = liftIO $ dyn545 ptr_glMultiTexCoord3sv v1 v2

{-# NOINLINE ptr_glMultiTexCoord3sv #-}
ptr_glMultiTexCoord3sv :: FunPtr (GLenum -> Ptr GLshort -> IO ())
ptr_glMultiTexCoord3sv = unsafePerformIO $ getCommand "glMultiTexCoord3sv"

-- glMultiTexCoord3svARB -------------------------------------------------------

-- | This command is an alias for 'glMultiTexCoord3sv'.
glMultiTexCoord3svARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> Ptr GLshort -- ^ @v@ pointing to @3@ elements of type @CoordS@.
  -> m ()
glMultiTexCoord3svARB v1 v2 = liftIO $ dyn545 ptr_glMultiTexCoord3svARB v1 v2

{-# NOINLINE ptr_glMultiTexCoord3svARB #-}
ptr_glMultiTexCoord3svARB :: FunPtr (GLenum -> Ptr GLshort -> IO ())
ptr_glMultiTexCoord3svARB = unsafePerformIO $ getCommand "glMultiTexCoord3svARB"

-- glMultiTexCoord3xOES --------------------------------------------------------

glMultiTexCoord3xOES
  :: MonadIO m
  => GLenum -- ^ @texture@.
  -> GLfixed -- ^ @s@.
  -> GLfixed -- ^ @t@.
  -> GLfixed -- ^ @r@.
  -> m ()
glMultiTexCoord3xOES v1 v2 v3 v4 = liftIO $ dyn556 ptr_glMultiTexCoord3xOES v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexCoord3xOES #-}
ptr_glMultiTexCoord3xOES :: FunPtr (GLenum -> GLfixed -> GLfixed -> GLfixed -> IO ())
ptr_glMultiTexCoord3xOES = unsafePerformIO $ getCommand "glMultiTexCoord3xOES"

-- glMultiTexCoord3xvOES -------------------------------------------------------

glMultiTexCoord3xvOES
  :: MonadIO m
  => GLenum -- ^ @texture@.
  -> Ptr GLfixed -- ^ @coords@ pointing to @3@ elements of type @GLfixed@.
  -> m ()
glMultiTexCoord3xvOES v1 v2 = liftIO $ dyn95 ptr_glMultiTexCoord3xvOES v1 v2

{-# NOINLINE ptr_glMultiTexCoord3xvOES #-}
ptr_glMultiTexCoord3xvOES :: FunPtr (GLenum -> Ptr GLfixed -> IO ())
ptr_glMultiTexCoord3xvOES = unsafePerformIO $ getCommand "glMultiTexCoord3xvOES"

-- glMultiTexCoord4bOES --------------------------------------------------------

glMultiTexCoord4bOES
  :: MonadIO m
  => GLenum -- ^ @texture@.
  -> GLbyte -- ^ @s@.
  -> GLbyte -- ^ @t@.
  -> GLbyte -- ^ @r@.
  -> GLbyte -- ^ @q@.
  -> m ()
glMultiTexCoord4bOES v1 v2 v3 v4 v5 = liftIO $ dyn557 ptr_glMultiTexCoord4bOES v1 v2 v3 v4 v5

{-# NOINLINE ptr_glMultiTexCoord4bOES #-}
ptr_glMultiTexCoord4bOES :: FunPtr (GLenum -> GLbyte -> GLbyte -> GLbyte -> GLbyte -> IO ())
ptr_glMultiTexCoord4bOES = unsafePerformIO $ getCommand "glMultiTexCoord4bOES"

-- glMultiTexCoord4bvOES -------------------------------------------------------

glMultiTexCoord4bvOES
  :: MonadIO m
  => GLenum -- ^ @texture@.
  -> Ptr GLbyte -- ^ @coords@ pointing to @4@ elements of type @GLbyte@.
  -> m ()
glMultiTexCoord4bvOES v1 v2 = liftIO $ dyn540 ptr_glMultiTexCoord4bvOES v1 v2

{-# NOINLINE ptr_glMultiTexCoord4bvOES #-}
ptr_glMultiTexCoord4bvOES :: FunPtr (GLenum -> Ptr GLbyte -> IO ())
ptr_glMultiTexCoord4bvOES = unsafePerformIO $ getCommand "glMultiTexCoord4bvOES"

-- glMultiTexCoord4d -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glMultiTexCoord4dv'.
glMultiTexCoord4d
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> GLdouble -- ^ @s@ of type @CoordD@.
  -> GLdouble -- ^ @t@ of type @CoordD@.
  -> GLdouble -- ^ @r@ of type @CoordD@.
  -> GLdouble -- ^ @q@ of type @CoordD@.
  -> m ()
glMultiTexCoord4d v1 v2 v3 v4 v5 = liftIO $ dyn520 ptr_glMultiTexCoord4d v1 v2 v3 v4 v5

{-# NOINLINE ptr_glMultiTexCoord4d #-}
ptr_glMultiTexCoord4d :: FunPtr (GLenum -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glMultiTexCoord4d = unsafePerformIO $ getCommand "glMultiTexCoord4d"

-- glMultiTexCoord4dARB --------------------------------------------------------

-- | The vector equivalent of this command is 'glMultiTexCoord4dv'. This command is an alias for 'glMultiTexCoord4d'.
glMultiTexCoord4dARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> GLdouble -- ^ @s@ of type @CoordD@.
  -> GLdouble -- ^ @t@ of type @CoordD@.
  -> GLdouble -- ^ @r@ of type @CoordD@.
  -> GLdouble -- ^ @q@ of type @CoordD@.
  -> m ()
glMultiTexCoord4dARB v1 v2 v3 v4 v5 = liftIO $ dyn520 ptr_glMultiTexCoord4dARB v1 v2 v3 v4 v5

{-# NOINLINE ptr_glMultiTexCoord4dARB #-}
ptr_glMultiTexCoord4dARB :: FunPtr (GLenum -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glMultiTexCoord4dARB = unsafePerformIO $ getCommand "glMultiTexCoord4dARB"

-- glMultiTexCoord4dv ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>.
glMultiTexCoord4dv
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> Ptr GLdouble -- ^ @v@ pointing to @4@ elements of type @CoordD@.
  -> m ()
glMultiTexCoord4dv v1 v2 = liftIO $ dyn93 ptr_glMultiTexCoord4dv v1 v2

{-# NOINLINE ptr_glMultiTexCoord4dv #-}
ptr_glMultiTexCoord4dv :: FunPtr (GLenum -> Ptr GLdouble -> IO ())
ptr_glMultiTexCoord4dv = unsafePerformIO $ getCommand "glMultiTexCoord4dv"

