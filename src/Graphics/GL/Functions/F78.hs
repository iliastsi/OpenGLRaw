--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F78
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

module Graphics.GL.Functions.F78 (
  glWindowPos2d,
  glWindowPos2dARB,
  glWindowPos2dMESA,
  glWindowPos2dv,
  glWindowPos2dvARB,
  glWindowPos2dvMESA,
  glWindowPos2f,
  glWindowPos2fARB,
  glWindowPos2fMESA,
  glWindowPos2fv,
  glWindowPos2fvARB,
  glWindowPos2fvMESA,
  glWindowPos2i,
  glWindowPos2iARB,
  glWindowPos2iMESA,
  glWindowPos2iv,
  glWindowPos2ivARB,
  glWindowPos2ivMESA,
  glWindowPos2s,
  glWindowPos2sARB,
  glWindowPos2sMESA,
  glWindowPos2sv,
  glWindowPos2svARB,
  glWindowPos2svMESA,
  glWindowPos3d,
  glWindowPos3dARB,
  glWindowPos3dMESA,
  glWindowPos3dv,
  glWindowPos3dvARB,
  glWindowPos3dvMESA,
  glWindowPos3f,
  glWindowPos3fARB,
  glWindowPos3fMESA,
  glWindowPos3fv,
  glWindowPos3fvARB,
  glWindowPos3fvMESA,
  glWindowPos3i,
  glWindowPos3iARB,
  glWindowPos3iMESA,
  glWindowPos3iv
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

-- glWindowPos2d ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml OpenGL 2.x>. The vector equivalent of this command is 'glWindowPos2dv'.
glWindowPos2d
  :: MonadIO m
  => GLdouble -- ^ @x@ of type @CoordD@.
  -> GLdouble -- ^ @y@ of type @CoordD@.
  -> m ()
glWindowPos2d v1 v2 = liftIO $ dyn217 ptr_glWindowPos2d v1 v2

{-# NOINLINE ptr_glWindowPos2d #-}
ptr_glWindowPos2d :: FunPtr (GLdouble -> GLdouble -> IO ())
ptr_glWindowPos2d = unsafePerformIO $ getCommand "glWindowPos2d"

-- glWindowPos2dARB ------------------------------------------------------------

-- | The vector equivalent of this command is 'glWindowPos2dvARB'. This command is an alias for 'glWindowPos2d'.
glWindowPos2dARB
  :: MonadIO m
  => GLdouble -- ^ @x@ of type @CoordD@.
  -> GLdouble -- ^ @y@ of type @CoordD@.
  -> m ()
glWindowPos2dARB v1 v2 = liftIO $ dyn217 ptr_glWindowPos2dARB v1 v2

{-# NOINLINE ptr_glWindowPos2dARB #-}
ptr_glWindowPos2dARB :: FunPtr (GLdouble -> GLdouble -> IO ())
ptr_glWindowPos2dARB = unsafePerformIO $ getCommand "glWindowPos2dARB"

-- glWindowPos2dMESA -----------------------------------------------------------

-- | The vector equivalent of this command is 'glWindowPos2dvMESA'. This command is an alias for 'glWindowPos2d'.
glWindowPos2dMESA
  :: MonadIO m
  => GLdouble -- ^ @x@ of type @CoordD@.
  -> GLdouble -- ^ @y@ of type @CoordD@.
  -> m ()
glWindowPos2dMESA v1 v2 = liftIO $ dyn217 ptr_glWindowPos2dMESA v1 v2

{-# NOINLINE ptr_glWindowPos2dMESA #-}
ptr_glWindowPos2dMESA :: FunPtr (GLdouble -> GLdouble -> IO ())
ptr_glWindowPos2dMESA = unsafePerformIO $ getCommand "glWindowPos2dMESA"

-- glWindowPos2dv --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml OpenGL 2.x>.
glWindowPos2dv
  :: MonadIO m
  => Ptr GLdouble -- ^ @v@ pointing to @2@ elements of type @CoordD@.
  -> m ()
glWindowPos2dv v1 = liftIO $ dyn39 ptr_glWindowPos2dv v1

{-# NOINLINE ptr_glWindowPos2dv #-}
ptr_glWindowPos2dv :: FunPtr (Ptr GLdouble -> IO ())
ptr_glWindowPos2dv = unsafePerformIO $ getCommand "glWindowPos2dv"

-- glWindowPos2dvARB -----------------------------------------------------------

-- | This command is an alias for 'glWindowPos2dv'.
glWindowPos2dvARB
  :: MonadIO m
  => Ptr GLdouble -- ^ @v@ pointing to @2@ elements of type @CoordD@.
  -> m ()
glWindowPos2dvARB v1 = liftIO $ dyn39 ptr_glWindowPos2dvARB v1

{-# NOINLINE ptr_glWindowPos2dvARB #-}
ptr_glWindowPos2dvARB :: FunPtr (Ptr GLdouble -> IO ())
ptr_glWindowPos2dvARB = unsafePerformIO $ getCommand "glWindowPos2dvARB"

-- glWindowPos2dvMESA ----------------------------------------------------------

-- | This command is an alias for 'glWindowPos2dv'.
glWindowPos2dvMESA
  :: MonadIO m
  => Ptr GLdouble -- ^ @v@ pointing to @2@ elements of type @CoordD@.
  -> m ()
glWindowPos2dvMESA v1 = liftIO $ dyn39 ptr_glWindowPos2dvMESA v1

{-# NOINLINE ptr_glWindowPos2dvMESA #-}
ptr_glWindowPos2dvMESA :: FunPtr (Ptr GLdouble -> IO ())
ptr_glWindowPos2dvMESA = unsafePerformIO $ getCommand "glWindowPos2dvMESA"

-- glWindowPos2f ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml OpenGL 2.x>. The vector equivalent of this command is 'glWindowPos2fv'.
glWindowPos2f
  :: MonadIO m
  => GLfloat -- ^ @x@ of type @CoordF@.
  -> GLfloat -- ^ @y@ of type @CoordF@.
  -> m ()
glWindowPos2f v1 v2 = liftIO $ dyn222 ptr_glWindowPos2f v1 v2

{-# NOINLINE ptr_glWindowPos2f #-}
ptr_glWindowPos2f :: FunPtr (GLfloat -> GLfloat -> IO ())
ptr_glWindowPos2f = unsafePerformIO $ getCommand "glWindowPos2f"

-- glWindowPos2fARB ------------------------------------------------------------

-- | The vector equivalent of this command is 'glWindowPos2fvARB'. This command is an alias for 'glWindowPos2f'.
glWindowPos2fARB
  :: MonadIO m
  => GLfloat -- ^ @x@ of type @CoordF@.
  -> GLfloat -- ^ @y@ of type @CoordF@.
  -> m ()
glWindowPos2fARB v1 v2 = liftIO $ dyn222 ptr_glWindowPos2fARB v1 v2

{-# NOINLINE ptr_glWindowPos2fARB #-}
ptr_glWindowPos2fARB :: FunPtr (GLfloat -> GLfloat -> IO ())
ptr_glWindowPos2fARB = unsafePerformIO $ getCommand "glWindowPos2fARB"

-- glWindowPos2fMESA -----------------------------------------------------------

-- | The vector equivalent of this command is 'glWindowPos2fvMESA'. This command is an alias for 'glWindowPos2f'.
glWindowPos2fMESA
  :: MonadIO m
  => GLfloat -- ^ @x@ of type @CoordF@.
  -> GLfloat -- ^ @y@ of type @CoordF@.
  -> m ()
glWindowPos2fMESA v1 v2 = liftIO $ dyn222 ptr_glWindowPos2fMESA v1 v2

{-# NOINLINE ptr_glWindowPos2fMESA #-}
ptr_glWindowPos2fMESA :: FunPtr (GLfloat -> GLfloat -> IO ())
ptr_glWindowPos2fMESA = unsafePerformIO $ getCommand "glWindowPos2fMESA"

-- glWindowPos2fv --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml OpenGL 2.x>.
glWindowPos2fv
  :: MonadIO m
  => Ptr GLfloat -- ^ @v@ pointing to @2@ elements of type @CoordF@.
  -> m ()
glWindowPos2fv v1 = liftIO $ dyn41 ptr_glWindowPos2fv v1

{-# NOINLINE ptr_glWindowPos2fv #-}
ptr_glWindowPos2fv :: FunPtr (Ptr GLfloat -> IO ())
ptr_glWindowPos2fv = unsafePerformIO $ getCommand "glWindowPos2fv"

-- glWindowPos2fvARB -----------------------------------------------------------

-- | This command is an alias for 'glWindowPos2fv'.
glWindowPos2fvARB
  :: MonadIO m
  => Ptr GLfloat -- ^ @v@ pointing to @2@ elements of type @CoordF@.
  -> m ()
glWindowPos2fvARB v1 = liftIO $ dyn41 ptr_glWindowPos2fvARB v1

{-# NOINLINE ptr_glWindowPos2fvARB #-}
ptr_glWindowPos2fvARB :: FunPtr (Ptr GLfloat -> IO ())
ptr_glWindowPos2fvARB = unsafePerformIO $ getCommand "glWindowPos2fvARB"

-- glWindowPos2fvMESA ----------------------------------------------------------

-- | This command is an alias for 'glWindowPos2fv'.
glWindowPos2fvMESA
  :: MonadIO m
  => Ptr GLfloat -- ^ @v@ pointing to @2@ elements of type @CoordF@.
  -> m ()
glWindowPos2fvMESA v1 = liftIO $ dyn41 ptr_glWindowPos2fvMESA v1

{-# NOINLINE ptr_glWindowPos2fvMESA #-}
ptr_glWindowPos2fvMESA :: FunPtr (Ptr GLfloat -> IO ())
ptr_glWindowPos2fvMESA = unsafePerformIO $ getCommand "glWindowPos2fvMESA"

-- glWindowPos2i ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml OpenGL 2.x>. The vector equivalent of this command is 'glWindowPos2iv'.
glWindowPos2i
  :: MonadIO m
  => GLint -- ^ @x@ of type @CoordI@.
  -> GLint -- ^ @y@ of type @CoordI@.
  -> m ()
glWindowPos2i v1 v2 = liftIO $ dyn266 ptr_glWindowPos2i v1 v2

{-# NOINLINE ptr_glWindowPos2i #-}
ptr_glWindowPos2i :: FunPtr (GLint -> GLint -> IO ())
ptr_glWindowPos2i = unsafePerformIO $ getCommand "glWindowPos2i"

-- glWindowPos2iARB ------------------------------------------------------------

-- | The vector equivalent of this command is 'glWindowPos2ivARB'. This command is an alias for 'glWindowPos2i'.
glWindowPos2iARB
  :: MonadIO m
  => GLint -- ^ @x@ of type @CoordI@.
  -> GLint -- ^ @y@ of type @CoordI@.
  -> m ()
glWindowPos2iARB v1 v2 = liftIO $ dyn266 ptr_glWindowPos2iARB v1 v2

{-# NOINLINE ptr_glWindowPos2iARB #-}
ptr_glWindowPos2iARB :: FunPtr (GLint -> GLint -> IO ())
ptr_glWindowPos2iARB = unsafePerformIO $ getCommand "glWindowPos2iARB"

-- glWindowPos2iMESA -----------------------------------------------------------

-- | The vector equivalent of this command is 'glWindowPos2ivMESA'. This command is an alias for 'glWindowPos2i'.
glWindowPos2iMESA
  :: MonadIO m
  => GLint -- ^ @x@ of type @CoordI@.
  -> GLint -- ^ @y@ of type @CoordI@.
  -> m ()
glWindowPos2iMESA v1 v2 = liftIO $ dyn266 ptr_glWindowPos2iMESA v1 v2

{-# NOINLINE ptr_glWindowPos2iMESA #-}
ptr_glWindowPos2iMESA :: FunPtr (GLint -> GLint -> IO ())
ptr_glWindowPos2iMESA = unsafePerformIO $ getCommand "glWindowPos2iMESA"

-- glWindowPos2iv --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml OpenGL 2.x>.
glWindowPos2iv
  :: MonadIO m
  => Ptr GLint -- ^ @v@ pointing to @2@ elements of type @CoordI@.
  -> m ()
glWindowPos2iv v1 = liftIO $ dyn43 ptr_glWindowPos2iv v1

{-# NOINLINE ptr_glWindowPos2iv #-}
ptr_glWindowPos2iv :: FunPtr (Ptr GLint -> IO ())
ptr_glWindowPos2iv = unsafePerformIO $ getCommand "glWindowPos2iv"

-- glWindowPos2ivARB -----------------------------------------------------------

-- | This command is an alias for 'glWindowPos2iv'.
glWindowPos2ivARB
  :: MonadIO m
  => Ptr GLint -- ^ @v@ pointing to @2@ elements of type @CoordI@.
  -> m ()
glWindowPos2ivARB v1 = liftIO $ dyn43 ptr_glWindowPos2ivARB v1

{-# NOINLINE ptr_glWindowPos2ivARB #-}
ptr_glWindowPos2ivARB :: FunPtr (Ptr GLint -> IO ())
ptr_glWindowPos2ivARB = unsafePerformIO $ getCommand "glWindowPos2ivARB"

-- glWindowPos2ivMESA ----------------------------------------------------------

-- | This command is an alias for 'glWindowPos2iv'.
glWindowPos2ivMESA
  :: MonadIO m
  => Ptr GLint -- ^ @v@ pointing to @2@ elements of type @CoordI@.
  -> m ()
glWindowPos2ivMESA v1 = liftIO $ dyn43 ptr_glWindowPos2ivMESA v1

{-# NOINLINE ptr_glWindowPos2ivMESA #-}
ptr_glWindowPos2ivMESA :: FunPtr (Ptr GLint -> IO ())
ptr_glWindowPos2ivMESA = unsafePerformIO $ getCommand "glWindowPos2ivMESA"

-- glWindowPos2s ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml OpenGL 2.x>. The vector equivalent of this command is 'glWindowPos2sv'.
glWindowPos2s
  :: MonadIO m
  => GLshort -- ^ @x@ of type @CoordS@.
  -> GLshort -- ^ @y@ of type @CoordS@.
  -> m ()
glWindowPos2s v1 v2 = liftIO $ dyn669 ptr_glWindowPos2s v1 v2

{-# NOINLINE ptr_glWindowPos2s #-}
ptr_glWindowPos2s :: FunPtr (GLshort -> GLshort -> IO ())
ptr_glWindowPos2s = unsafePerformIO $ getCommand "glWindowPos2s"

-- glWindowPos2sARB ------------------------------------------------------------

-- | The vector equivalent of this command is 'glWindowPos2svARB'. This command is an alias for 'glWindowPos2s'.
glWindowPos2sARB
  :: MonadIO m
  => GLshort -- ^ @x@ of type @CoordS@.
  -> GLshort -- ^ @y@ of type @CoordS@.
  -> m ()
glWindowPos2sARB v1 v2 = liftIO $ dyn669 ptr_glWindowPos2sARB v1 v2

{-# NOINLINE ptr_glWindowPos2sARB #-}
ptr_glWindowPos2sARB :: FunPtr (GLshort -> GLshort -> IO ())
ptr_glWindowPos2sARB = unsafePerformIO $ getCommand "glWindowPos2sARB"

-- glWindowPos2sMESA -----------------------------------------------------------

-- | The vector equivalent of this command is 'glWindowPos2svMESA'. This command is an alias for 'glWindowPos2s'.
glWindowPos2sMESA
  :: MonadIO m
  => GLshort -- ^ @x@ of type @CoordS@.
  -> GLshort -- ^ @y@ of type @CoordS@.
  -> m ()
glWindowPos2sMESA v1 v2 = liftIO $ dyn669 ptr_glWindowPos2sMESA v1 v2

{-# NOINLINE ptr_glWindowPos2sMESA #-}
ptr_glWindowPos2sMESA :: FunPtr (GLshort -> GLshort -> IO ())
ptr_glWindowPos2sMESA = unsafePerformIO $ getCommand "glWindowPos2sMESA"

-- glWindowPos2sv --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml OpenGL 2.x>.
glWindowPos2sv
  :: MonadIO m
  => Ptr GLshort -- ^ @v@ pointing to @2@ elements of type @CoordS@.
  -> m ()
glWindowPos2sv v1 = liftIO $ dyn45 ptr_glWindowPos2sv v1

{-# NOINLINE ptr_glWindowPos2sv #-}
ptr_glWindowPos2sv :: FunPtr (Ptr GLshort -> IO ())
ptr_glWindowPos2sv = unsafePerformIO $ getCommand "glWindowPos2sv"

-- glWindowPos2svARB -----------------------------------------------------------

-- | This command is an alias for 'glWindowPos2sv'.
glWindowPos2svARB
  :: MonadIO m
  => Ptr GLshort -- ^ @v@ pointing to @2@ elements of type @CoordS@.
  -> m ()
glWindowPos2svARB v1 = liftIO $ dyn45 ptr_glWindowPos2svARB v1

{-# NOINLINE ptr_glWindowPos2svARB #-}
ptr_glWindowPos2svARB :: FunPtr (Ptr GLshort -> IO ())
ptr_glWindowPos2svARB = unsafePerformIO $ getCommand "glWindowPos2svARB"

-- glWindowPos2svMESA ----------------------------------------------------------

-- | This command is an alias for 'glWindowPos2sv'.
glWindowPos2svMESA
  :: MonadIO m
  => Ptr GLshort -- ^ @v@ pointing to @2@ elements of type @CoordS@.
  -> m ()
glWindowPos2svMESA v1 = liftIO $ dyn45 ptr_glWindowPos2svMESA v1

{-# NOINLINE ptr_glWindowPos2svMESA #-}
ptr_glWindowPos2svMESA :: FunPtr (Ptr GLshort -> IO ())
ptr_glWindowPos2svMESA = unsafePerformIO $ getCommand "glWindowPos2svMESA"

-- glWindowPos3d ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml OpenGL 2.x>. The vector equivalent of this command is 'glWindowPos3dv'.
glWindowPos3d
  :: MonadIO m
  => GLdouble -- ^ @x@ of type @CoordD@.
  -> GLdouble -- ^ @y@ of type @CoordD@.
  -> GLdouble -- ^ @z@ of type @CoordD@.
  -> m ()
glWindowPos3d v1 v2 v3 = liftIO $ dyn38 ptr_glWindowPos3d v1 v2 v3

{-# NOINLINE ptr_glWindowPos3d #-}
ptr_glWindowPos3d :: FunPtr (GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glWindowPos3d = unsafePerformIO $ getCommand "glWindowPos3d"

-- glWindowPos3dARB ------------------------------------------------------------

-- | The vector equivalent of this command is 'glWindowPos3dvARB'. This command is an alias for 'glWindowPos3d'.
glWindowPos3dARB
  :: MonadIO m
  => GLdouble -- ^ @x@ of type @CoordD@.
  -> GLdouble -- ^ @y@ of type @CoordD@.
  -> GLdouble -- ^ @z@ of type @CoordD@.
  -> m ()
glWindowPos3dARB v1 v2 v3 = liftIO $ dyn38 ptr_glWindowPos3dARB v1 v2 v3

{-# NOINLINE ptr_glWindowPos3dARB #-}
ptr_glWindowPos3dARB :: FunPtr (GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glWindowPos3dARB = unsafePerformIO $ getCommand "glWindowPos3dARB"

-- glWindowPos3dMESA -----------------------------------------------------------

-- | The vector equivalent of this command is 'glWindowPos3dvMESA'. This command is an alias for 'glWindowPos3d'.
glWindowPos3dMESA
  :: MonadIO m
  => GLdouble -- ^ @x@ of type @CoordD@.
  -> GLdouble -- ^ @y@ of type @CoordD@.
  -> GLdouble -- ^ @z@ of type @CoordD@.
  -> m ()
glWindowPos3dMESA v1 v2 v3 = liftIO $ dyn38 ptr_glWindowPos3dMESA v1 v2 v3

{-# NOINLINE ptr_glWindowPos3dMESA #-}
ptr_glWindowPos3dMESA :: FunPtr (GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glWindowPos3dMESA = unsafePerformIO $ getCommand "glWindowPos3dMESA"

-- glWindowPos3dv --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml OpenGL 2.x>.
glWindowPos3dv
  :: MonadIO m
  => Ptr GLdouble -- ^ @v@ pointing to @3@ elements of type @CoordD@.
  -> m ()
glWindowPos3dv v1 = liftIO $ dyn39 ptr_glWindowPos3dv v1

{-# NOINLINE ptr_glWindowPos3dv #-}
ptr_glWindowPos3dv :: FunPtr (Ptr GLdouble -> IO ())
ptr_glWindowPos3dv = unsafePerformIO $ getCommand "glWindowPos3dv"

-- glWindowPos3dvARB -----------------------------------------------------------

-- | This command is an alias for 'glWindowPos3dv'.
glWindowPos3dvARB
  :: MonadIO m
  => Ptr GLdouble -- ^ @v@ pointing to @3@ elements of type @CoordD@.
  -> m ()
glWindowPos3dvARB v1 = liftIO $ dyn39 ptr_glWindowPos3dvARB v1

{-# NOINLINE ptr_glWindowPos3dvARB #-}
ptr_glWindowPos3dvARB :: FunPtr (Ptr GLdouble -> IO ())
ptr_glWindowPos3dvARB = unsafePerformIO $ getCommand "glWindowPos3dvARB"

-- glWindowPos3dvMESA ----------------------------------------------------------

-- | This command is an alias for 'glWindowPos3dv'.
glWindowPos3dvMESA
  :: MonadIO m
  => Ptr GLdouble -- ^ @v@ pointing to @3@ elements of type @CoordD@.
  -> m ()
glWindowPos3dvMESA v1 = liftIO $ dyn39 ptr_glWindowPos3dvMESA v1

{-# NOINLINE ptr_glWindowPos3dvMESA #-}
ptr_glWindowPos3dvMESA :: FunPtr (Ptr GLdouble -> IO ())
ptr_glWindowPos3dvMESA = unsafePerformIO $ getCommand "glWindowPos3dvMESA"

-- glWindowPos3f ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml OpenGL 2.x>. The vector equivalent of this command is 'glWindowPos3fv'.
glWindowPos3f
  :: MonadIO m
  => GLfloat -- ^ @x@ of type @CoordF@.
  -> GLfloat -- ^ @y@ of type @CoordF@.
  -> GLfloat -- ^ @z@ of type @CoordF@.
  -> m ()
glWindowPos3f v1 v2 v3 = liftIO $ dyn40 ptr_glWindowPos3f v1 v2 v3

{-# NOINLINE ptr_glWindowPos3f #-}
ptr_glWindowPos3f :: FunPtr (GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glWindowPos3f = unsafePerformIO $ getCommand "glWindowPos3f"

-- glWindowPos3fARB ------------------------------------------------------------

-- | The vector equivalent of this command is 'glWindowPos3fvARB'. This command is an alias for 'glWindowPos3f'.
glWindowPos3fARB
  :: MonadIO m
  => GLfloat -- ^ @x@ of type @CoordF@.
  -> GLfloat -- ^ @y@ of type @CoordF@.
  -> GLfloat -- ^ @z@ of type @CoordF@.
  -> m ()
glWindowPos3fARB v1 v2 v3 = liftIO $ dyn40 ptr_glWindowPos3fARB v1 v2 v3

{-# NOINLINE ptr_glWindowPos3fARB #-}
ptr_glWindowPos3fARB :: FunPtr (GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glWindowPos3fARB = unsafePerformIO $ getCommand "glWindowPos3fARB"

-- glWindowPos3fMESA -----------------------------------------------------------

-- | The vector equivalent of this command is 'glWindowPos3fvMESA'. This command is an alias for 'glWindowPos3f'.
glWindowPos3fMESA
  :: MonadIO m
  => GLfloat -- ^ @x@ of type @CoordF@.
  -> GLfloat -- ^ @y@ of type @CoordF@.
  -> GLfloat -- ^ @z@ of type @CoordF@.
  -> m ()
glWindowPos3fMESA v1 v2 v3 = liftIO $ dyn40 ptr_glWindowPos3fMESA v1 v2 v3

{-# NOINLINE ptr_glWindowPos3fMESA #-}
ptr_glWindowPos3fMESA :: FunPtr (GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glWindowPos3fMESA = unsafePerformIO $ getCommand "glWindowPos3fMESA"

-- glWindowPos3fv --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml OpenGL 2.x>.
glWindowPos3fv
  :: MonadIO m
  => Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @CoordF@.
  -> m ()
glWindowPos3fv v1 = liftIO $ dyn41 ptr_glWindowPos3fv v1

{-# NOINLINE ptr_glWindowPos3fv #-}
ptr_glWindowPos3fv :: FunPtr (Ptr GLfloat -> IO ())
ptr_glWindowPos3fv = unsafePerformIO $ getCommand "glWindowPos3fv"

-- glWindowPos3fvARB -----------------------------------------------------------

-- | This command is an alias for 'glWindowPos3fv'.
glWindowPos3fvARB
  :: MonadIO m
  => Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @CoordF@.
  -> m ()
glWindowPos3fvARB v1 = liftIO $ dyn41 ptr_glWindowPos3fvARB v1

{-# NOINLINE ptr_glWindowPos3fvARB #-}
ptr_glWindowPos3fvARB :: FunPtr (Ptr GLfloat -> IO ())
ptr_glWindowPos3fvARB = unsafePerformIO $ getCommand "glWindowPos3fvARB"

-- glWindowPos3fvMESA ----------------------------------------------------------

-- | This command is an alias for 'glWindowPos3fv'.
glWindowPos3fvMESA
  :: MonadIO m
  => Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @CoordF@.
  -> m ()
glWindowPos3fvMESA v1 = liftIO $ dyn41 ptr_glWindowPos3fvMESA v1

{-# NOINLINE ptr_glWindowPos3fvMESA #-}
ptr_glWindowPos3fvMESA :: FunPtr (Ptr GLfloat -> IO ())
ptr_glWindowPos3fvMESA = unsafePerformIO $ getCommand "glWindowPos3fvMESA"

-- glWindowPos3i ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml OpenGL 2.x>. The vector equivalent of this command is 'glWindowPos3iv'.
glWindowPos3i
  :: MonadIO m
  => GLint -- ^ @x@ of type @CoordI@.
  -> GLint -- ^ @y@ of type @CoordI@.
  -> GLint -- ^ @z@ of type @CoordI@.
  -> m ()
glWindowPos3i v1 v2 v3 = liftIO $ dyn42 ptr_glWindowPos3i v1 v2 v3

{-# NOINLINE ptr_glWindowPos3i #-}
ptr_glWindowPos3i :: FunPtr (GLint -> GLint -> GLint -> IO ())
ptr_glWindowPos3i = unsafePerformIO $ getCommand "glWindowPos3i"

-- glWindowPos3iARB ------------------------------------------------------------

-- | The vector equivalent of this command is 'glWindowPos3ivARB'. This command is an alias for 'glWindowPos3i'.
glWindowPos3iARB
  :: MonadIO m
  => GLint -- ^ @x@ of type @CoordI@.
  -> GLint -- ^ @y@ of type @CoordI@.
  -> GLint -- ^ @z@ of type @CoordI@.
  -> m ()
glWindowPos3iARB v1 v2 v3 = liftIO $ dyn42 ptr_glWindowPos3iARB v1 v2 v3

{-# NOINLINE ptr_glWindowPos3iARB #-}
ptr_glWindowPos3iARB :: FunPtr (GLint -> GLint -> GLint -> IO ())
ptr_glWindowPos3iARB = unsafePerformIO $ getCommand "glWindowPos3iARB"

-- glWindowPos3iMESA -----------------------------------------------------------

-- | The vector equivalent of this command is 'glWindowPos3ivMESA'. This command is an alias for 'glWindowPos3i'.
glWindowPos3iMESA
  :: MonadIO m
  => GLint -- ^ @x@ of type @CoordI@.
  -> GLint -- ^ @y@ of type @CoordI@.
  -> GLint -- ^ @z@ of type @CoordI@.
  -> m ()
glWindowPos3iMESA v1 v2 v3 = liftIO $ dyn42 ptr_glWindowPos3iMESA v1 v2 v3

{-# NOINLINE ptr_glWindowPos3iMESA #-}
ptr_glWindowPos3iMESA :: FunPtr (GLint -> GLint -> GLint -> IO ())
ptr_glWindowPos3iMESA = unsafePerformIO $ getCommand "glWindowPos3iMESA"

-- glWindowPos3iv --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml OpenGL 2.x>.
glWindowPos3iv
  :: MonadIO m
  => Ptr GLint -- ^ @v@ pointing to @3@ elements of type @CoordI@.
  -> m ()
glWindowPos3iv v1 = liftIO $ dyn43 ptr_glWindowPos3iv v1

{-# NOINLINE ptr_glWindowPos3iv #-}
ptr_glWindowPos3iv :: FunPtr (Ptr GLint -> IO ())
ptr_glWindowPos3iv = unsafePerformIO $ getCommand "glWindowPos3iv"

