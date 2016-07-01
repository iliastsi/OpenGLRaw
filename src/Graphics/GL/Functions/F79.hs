--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F79
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

module Graphics.GL.Functions.F79 (
  glWindowPos3ivARB,
  glWindowPos3ivMESA,
  glWindowPos3s,
  glWindowPos3sARB,
  glWindowPos3sMESA,
  glWindowPos3sv,
  glWindowPos3svARB,
  glWindowPos3svMESA,
  glWindowPos4dMESA,
  glWindowPos4dvMESA,
  glWindowPos4fMESA,
  glWindowPos4fvMESA,
  glWindowPos4iMESA,
  glWindowPos4ivMESA,
  glWindowPos4sMESA,
  glWindowPos4svMESA,
  glWriteMaskEXT
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

-- glWindowPos3ivARB -----------------------------------------------------------

-- | This command is an alias for 'glWindowPos3iv'.
glWindowPos3ivARB
  :: MonadIO m
  => Ptr GLint -- ^ @v@ pointing to @3@ elements of type @CoordI@.
  -> m ()
glWindowPos3ivARB v1 = liftIO $ dyn43 ptr_glWindowPos3ivARB v1

{-# NOINLINE ptr_glWindowPos3ivARB #-}
ptr_glWindowPos3ivARB :: FunPtr (Ptr GLint -> IO ())
ptr_glWindowPos3ivARB = unsafePerformIO $ getCommand "glWindowPos3ivARB"

-- glWindowPos3ivMESA ----------------------------------------------------------

-- | This command is an alias for 'glWindowPos3iv'.
glWindowPos3ivMESA
  :: MonadIO m
  => Ptr GLint -- ^ @v@ pointing to @3@ elements of type @CoordI@.
  -> m ()
glWindowPos3ivMESA v1 = liftIO $ dyn43 ptr_glWindowPos3ivMESA v1

{-# NOINLINE ptr_glWindowPos3ivMESA #-}
ptr_glWindowPos3ivMESA :: FunPtr (Ptr GLint -> IO ())
ptr_glWindowPos3ivMESA = unsafePerformIO $ getCommand "glWindowPos3ivMESA"

-- glWindowPos3s ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml OpenGL 2.x>. The vector equivalent of this command is 'glWindowPos3sv'.
glWindowPos3s
  :: MonadIO m
  => GLshort -- ^ @x@ of type @CoordS@.
  -> GLshort -- ^ @y@ of type @CoordS@.
  -> GLshort -- ^ @z@ of type @CoordS@.
  -> m ()
glWindowPos3s v1 v2 v3 = liftIO $ dyn44 ptr_glWindowPos3s v1 v2 v3

{-# NOINLINE ptr_glWindowPos3s #-}
ptr_glWindowPos3s :: FunPtr (GLshort -> GLshort -> GLshort -> IO ())
ptr_glWindowPos3s = unsafePerformIO $ getCommand "glWindowPos3s"

-- glWindowPos3sARB ------------------------------------------------------------

-- | The vector equivalent of this command is 'glWindowPos3svARB'. This command is an alias for 'glWindowPos3s'.
glWindowPos3sARB
  :: MonadIO m
  => GLshort -- ^ @x@ of type @CoordS@.
  -> GLshort -- ^ @y@ of type @CoordS@.
  -> GLshort -- ^ @z@ of type @CoordS@.
  -> m ()
glWindowPos3sARB v1 v2 v3 = liftIO $ dyn44 ptr_glWindowPos3sARB v1 v2 v3

{-# NOINLINE ptr_glWindowPos3sARB #-}
ptr_glWindowPos3sARB :: FunPtr (GLshort -> GLshort -> GLshort -> IO ())
ptr_glWindowPos3sARB = unsafePerformIO $ getCommand "glWindowPos3sARB"

-- glWindowPos3sMESA -----------------------------------------------------------

-- | The vector equivalent of this command is 'glWindowPos3svMESA'. This command is an alias for 'glWindowPos3s'.
glWindowPos3sMESA
  :: MonadIO m
  => GLshort -- ^ @x@ of type @CoordS@.
  -> GLshort -- ^ @y@ of type @CoordS@.
  -> GLshort -- ^ @z@ of type @CoordS@.
  -> m ()
glWindowPos3sMESA v1 v2 v3 = liftIO $ dyn44 ptr_glWindowPos3sMESA v1 v2 v3

{-# NOINLINE ptr_glWindowPos3sMESA #-}
ptr_glWindowPos3sMESA :: FunPtr (GLshort -> GLshort -> GLshort -> IO ())
ptr_glWindowPos3sMESA = unsafePerformIO $ getCommand "glWindowPos3sMESA"

-- glWindowPos3sv --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glWindowPos.xml OpenGL 2.x>.
glWindowPos3sv
  :: MonadIO m
  => Ptr GLshort -- ^ @v@ pointing to @3@ elements of type @CoordS@.
  -> m ()
glWindowPos3sv v1 = liftIO $ dyn45 ptr_glWindowPos3sv v1

{-# NOINLINE ptr_glWindowPos3sv #-}
ptr_glWindowPos3sv :: FunPtr (Ptr GLshort -> IO ())
ptr_glWindowPos3sv = unsafePerformIO $ getCommand "glWindowPos3sv"

-- glWindowPos3svARB -----------------------------------------------------------

-- | This command is an alias for 'glWindowPos3sv'.
glWindowPos3svARB
  :: MonadIO m
  => Ptr GLshort -- ^ @v@ pointing to @3@ elements of type @CoordS@.
  -> m ()
glWindowPos3svARB v1 = liftIO $ dyn45 ptr_glWindowPos3svARB v1

{-# NOINLINE ptr_glWindowPos3svARB #-}
ptr_glWindowPos3svARB :: FunPtr (Ptr GLshort -> IO ())
ptr_glWindowPos3svARB = unsafePerformIO $ getCommand "glWindowPos3svARB"

-- glWindowPos3svMESA ----------------------------------------------------------

-- | This command is an alias for 'glWindowPos3sv'.
glWindowPos3svMESA
  :: MonadIO m
  => Ptr GLshort -- ^ @v@ pointing to @3@ elements of type @CoordS@.
  -> m ()
glWindowPos3svMESA v1 = liftIO $ dyn45 ptr_glWindowPos3svMESA v1

{-# NOINLINE ptr_glWindowPos3svMESA #-}
ptr_glWindowPos3svMESA :: FunPtr (Ptr GLshort -> IO ())
ptr_glWindowPos3svMESA = unsafePerformIO $ getCommand "glWindowPos3svMESA"

-- glWindowPos4dMESA -----------------------------------------------------------

-- | The vector equivalent of this command is 'glWindowPos4dvMESA'.
glWindowPos4dMESA
  :: MonadIO m
  => GLdouble -- ^ @x@ of type @CoordD@.
  -> GLdouble -- ^ @y@ of type @CoordD@.
  -> GLdouble -- ^ @z@ of type @CoordD@.
  -> GLdouble -- ^ @w@ of type @CoordD@.
  -> m ()
glWindowPos4dMESA v1 v2 v3 v4 = liftIO $ dyn109 ptr_glWindowPos4dMESA v1 v2 v3 v4

{-# NOINLINE ptr_glWindowPos4dMESA #-}
ptr_glWindowPos4dMESA :: FunPtr (GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glWindowPos4dMESA = unsafePerformIO $ getCommand "glWindowPos4dMESA"

-- glWindowPos4dvMESA ----------------------------------------------------------

glWindowPos4dvMESA
  :: MonadIO m
  => Ptr GLdouble -- ^ @v@ pointing to @4@ elements of type @CoordD@.
  -> m ()
glWindowPos4dvMESA v1 = liftIO $ dyn39 ptr_glWindowPos4dvMESA v1

{-# NOINLINE ptr_glWindowPos4dvMESA #-}
ptr_glWindowPos4dvMESA :: FunPtr (Ptr GLdouble -> IO ())
ptr_glWindowPos4dvMESA = unsafePerformIO $ getCommand "glWindowPos4dvMESA"

-- glWindowPos4fMESA -----------------------------------------------------------

-- | The vector equivalent of this command is 'glWindowPos4fvMESA'.
glWindowPos4fMESA
  :: MonadIO m
  => GLfloat -- ^ @x@ of type @CoordF@.
  -> GLfloat -- ^ @y@ of type @CoordF@.
  -> GLfloat -- ^ @z@ of type @CoordF@.
  -> GLfloat -- ^ @w@ of type @CoordF@.
  -> m ()
glWindowPos4fMESA v1 v2 v3 v4 = liftIO $ dyn49 ptr_glWindowPos4fMESA v1 v2 v3 v4

{-# NOINLINE ptr_glWindowPos4fMESA #-}
ptr_glWindowPos4fMESA :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glWindowPos4fMESA = unsafePerformIO $ getCommand "glWindowPos4fMESA"

-- glWindowPos4fvMESA ----------------------------------------------------------

glWindowPos4fvMESA
  :: MonadIO m
  => Ptr GLfloat -- ^ @v@ pointing to @4@ elements of type @CoordF@.
  -> m ()
glWindowPos4fvMESA v1 = liftIO $ dyn41 ptr_glWindowPos4fvMESA v1

{-# NOINLINE ptr_glWindowPos4fvMESA #-}
ptr_glWindowPos4fvMESA :: FunPtr (Ptr GLfloat -> IO ())
ptr_glWindowPos4fvMESA = unsafePerformIO $ getCommand "glWindowPos4fvMESA"

-- glWindowPos4iMESA -----------------------------------------------------------

-- | The vector equivalent of this command is 'glWindowPos4ivMESA'.
glWindowPos4iMESA
  :: MonadIO m
  => GLint -- ^ @x@ of type @CoordI@.
  -> GLint -- ^ @y@ of type @CoordI@.
  -> GLint -- ^ @z@ of type @CoordI@.
  -> GLint -- ^ @w@ of type @CoordI@.
  -> m ()
glWindowPos4iMESA v1 v2 v3 v4 = liftIO $ dyn76 ptr_glWindowPos4iMESA v1 v2 v3 v4

{-# NOINLINE ptr_glWindowPos4iMESA #-}
ptr_glWindowPos4iMESA :: FunPtr (GLint -> GLint -> GLint -> GLint -> IO ())
ptr_glWindowPos4iMESA = unsafePerformIO $ getCommand "glWindowPos4iMESA"

-- glWindowPos4ivMESA ----------------------------------------------------------

glWindowPos4ivMESA
  :: MonadIO m
  => Ptr GLint -- ^ @v@ pointing to @4@ elements of type @CoordI@.
  -> m ()
glWindowPos4ivMESA v1 = liftIO $ dyn43 ptr_glWindowPos4ivMESA v1

{-# NOINLINE ptr_glWindowPos4ivMESA #-}
ptr_glWindowPos4ivMESA :: FunPtr (Ptr GLint -> IO ())
ptr_glWindowPos4ivMESA = unsafePerformIO $ getCommand "glWindowPos4ivMESA"

-- glWindowPos4sMESA -----------------------------------------------------------

-- | The vector equivalent of this command is 'glWindowPos4svMESA'.
glWindowPos4sMESA
  :: MonadIO m
  => GLshort -- ^ @x@ of type @CoordS@.
  -> GLshort -- ^ @y@ of type @CoordS@.
  -> GLshort -- ^ @z@ of type @CoordS@.
  -> GLshort -- ^ @w@ of type @CoordS@.
  -> m ()
glWindowPos4sMESA v1 v2 v3 v4 = liftIO $ dyn113 ptr_glWindowPos4sMESA v1 v2 v3 v4

{-# NOINLINE ptr_glWindowPos4sMESA #-}
ptr_glWindowPos4sMESA :: FunPtr (GLshort -> GLshort -> GLshort -> GLshort -> IO ())
ptr_glWindowPos4sMESA = unsafePerformIO $ getCommand "glWindowPos4sMESA"

-- glWindowPos4svMESA ----------------------------------------------------------

glWindowPos4svMESA
  :: MonadIO m
  => Ptr GLshort -- ^ @v@ pointing to @4@ elements of type @CoordS@.
  -> m ()
glWindowPos4svMESA v1 = liftIO $ dyn45 ptr_glWindowPos4svMESA v1

{-# NOINLINE ptr_glWindowPos4svMESA #-}
ptr_glWindowPos4svMESA :: FunPtr (Ptr GLshort -> IO ())
ptr_glWindowPos4svMESA = unsafePerformIO $ getCommand "glWindowPos4svMESA"

-- glWriteMaskEXT --------------------------------------------------------------

glWriteMaskEXT
  :: MonadIO m
  => GLuint -- ^ @res@.
  -> GLuint -- ^ @in@.
  -> GLenum -- ^ @outX@ of type @VertexShaderWriteMaskEXT@.
  -> GLenum -- ^ @outY@ of type @VertexShaderWriteMaskEXT@.
  -> GLenum -- ^ @outZ@ of type @VertexShaderWriteMaskEXT@.
  -> GLenum -- ^ @outW@ of type @VertexShaderWriteMaskEXT@.
  -> m ()
glWriteMaskEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn720 ptr_glWriteMaskEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glWriteMaskEXT #-}
ptr_glWriteMaskEXT :: FunPtr (GLuint -> GLuint -> GLenum -> GLenum -> GLenum -> GLenum -> IO ())
ptr_glWriteMaskEXT = unsafePerformIO $ getCommand "glWriteMaskEXT"

