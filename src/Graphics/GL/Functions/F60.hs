--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F60
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

module Graphics.GL.Functions.F60 (
  glTexCoord2hvNV,
  glTexCoord2i,
  glTexCoord2iv,
  glTexCoord2s,
  glTexCoord2sv,
  glTexCoord2xOES,
  glTexCoord2xvOES,
  glTexCoord3bOES,
  glTexCoord3bvOES,
  glTexCoord3d,
  glTexCoord3dv,
  glTexCoord3f,
  glTexCoord3fv,
  glTexCoord3hNV,
  glTexCoord3hvNV,
  glTexCoord3i,
  glTexCoord3iv,
  glTexCoord3s,
  glTexCoord3sv,
  glTexCoord3xOES,
  glTexCoord3xvOES,
  glTexCoord4bOES,
  glTexCoord4bvOES,
  glTexCoord4d,
  glTexCoord4dv,
  glTexCoord4f,
  glTexCoord4fColor4fNormal3fVertex4fSUN,
  glTexCoord4fColor4fNormal3fVertex4fvSUN,
  glTexCoord4fVertex4fSUN,
  glTexCoord4fVertex4fvSUN,
  glTexCoord4fv,
  glTexCoord4hNV,
  glTexCoord4hvNV,
  glTexCoord4i,
  glTexCoord4iv,
  glTexCoord4s,
  glTexCoord4sv,
  glTexCoord4xOES,
  glTexCoord4xvOES,
  glTexCoordFormatNV
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

-- glTexCoord2hvNV -------------------------------------------------------------

glTexCoord2hvNV
  :: MonadIO m
  => Ptr GLhalfNV -- ^ @v@ pointing to @2@ elements of type @Half16NV@.
  -> m ()
glTexCoord2hvNV v1 = liftIO $ dyn99 ptr_glTexCoord2hvNV v1

{-# NOINLINE ptr_glTexCoord2hvNV #-}
ptr_glTexCoord2hvNV :: FunPtr (Ptr GLhalfNV -> IO ())
ptr_glTexCoord2hvNV = unsafePerformIO $ getCommand "glTexCoord2hvNV"

-- glTexCoord2i ----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glTexCoord2iv'.
glTexCoord2i
  :: MonadIO m
  => GLint -- ^ @s@ of type @CoordI@.
  -> GLint -- ^ @t@ of type @CoordI@.
  -> m ()
glTexCoord2i v1 v2 = liftIO $ dyn266 ptr_glTexCoord2i v1 v2

{-# NOINLINE ptr_glTexCoord2i #-}
ptr_glTexCoord2i :: FunPtr (GLint -> GLint -> IO ())
ptr_glTexCoord2i = unsafePerformIO $ getCommand "glTexCoord2i"

-- glTexCoord2iv ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml OpenGL 2.x>.
glTexCoord2iv
  :: MonadIO m
  => Ptr GLint -- ^ @v@ pointing to @2@ elements of type @CoordI@.
  -> m ()
glTexCoord2iv v1 = liftIO $ dyn43 ptr_glTexCoord2iv v1

{-# NOINLINE ptr_glTexCoord2iv #-}
ptr_glTexCoord2iv :: FunPtr (Ptr GLint -> IO ())
ptr_glTexCoord2iv = unsafePerformIO $ getCommand "glTexCoord2iv"

-- glTexCoord2s ----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glTexCoord2sv'.
glTexCoord2s
  :: MonadIO m
  => GLshort -- ^ @s@ of type @CoordS@.
  -> GLshort -- ^ @t@ of type @CoordS@.
  -> m ()
glTexCoord2s v1 v2 = liftIO $ dyn669 ptr_glTexCoord2s v1 v2

{-# NOINLINE ptr_glTexCoord2s #-}
ptr_glTexCoord2s :: FunPtr (GLshort -> GLshort -> IO ())
ptr_glTexCoord2s = unsafePerformIO $ getCommand "glTexCoord2s"

-- glTexCoord2sv ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml OpenGL 2.x>.
glTexCoord2sv
  :: MonadIO m
  => Ptr GLshort -- ^ @v@ pointing to @2@ elements of type @CoordS@.
  -> m ()
glTexCoord2sv v1 = liftIO $ dyn45 ptr_glTexCoord2sv v1

{-# NOINLINE ptr_glTexCoord2sv #-}
ptr_glTexCoord2sv :: FunPtr (Ptr GLshort -> IO ())
ptr_glTexCoord2sv = unsafePerformIO $ getCommand "glTexCoord2sv"

-- glTexCoord2xOES -------------------------------------------------------------

glTexCoord2xOES
  :: MonadIO m
  => GLfixed -- ^ @s@.
  -> GLfixed -- ^ @t@.
  -> m ()
glTexCoord2xOES v1 v2 = liftIO $ dyn224 ptr_glTexCoord2xOES v1 v2

{-# NOINLINE ptr_glTexCoord2xOES #-}
ptr_glTexCoord2xOES :: FunPtr (GLfixed -> GLfixed -> IO ())
ptr_glTexCoord2xOES = unsafePerformIO $ getCommand "glTexCoord2xOES"

-- glTexCoord2xvOES ------------------------------------------------------------

glTexCoord2xvOES
  :: MonadIO m
  => Ptr GLfixed -- ^ @coords@ pointing to @2@ elements of type @GLfixed@.
  -> m ()
glTexCoord2xvOES v1 = liftIO $ dyn107 ptr_glTexCoord2xvOES v1

{-# NOINLINE ptr_glTexCoord2xvOES #-}
ptr_glTexCoord2xvOES :: FunPtr (Ptr GLfixed -> IO ())
ptr_glTexCoord2xvOES = unsafePerformIO $ getCommand "glTexCoord2xvOES"

-- glTexCoord3bOES -------------------------------------------------------------

glTexCoord3bOES
  :: MonadIO m
  => GLbyte -- ^ @s@.
  -> GLbyte -- ^ @t@.
  -> GLbyte -- ^ @r@.
  -> m ()
glTexCoord3bOES v1 v2 v3 = liftIO $ dyn36 ptr_glTexCoord3bOES v1 v2 v3

{-# NOINLINE ptr_glTexCoord3bOES #-}
ptr_glTexCoord3bOES :: FunPtr (GLbyte -> GLbyte -> GLbyte -> IO ())
ptr_glTexCoord3bOES = unsafePerformIO $ getCommand "glTexCoord3bOES"

-- glTexCoord3bvOES ------------------------------------------------------------

glTexCoord3bvOES
  :: MonadIO m
  => Ptr GLbyte -- ^ @coords@ pointing to @3@ elements of type @GLbyte@.
  -> m ()
glTexCoord3bvOES v1 = liftIO $ dyn37 ptr_glTexCoord3bvOES v1

{-# NOINLINE ptr_glTexCoord3bvOES #-}
ptr_glTexCoord3bvOES :: FunPtr (Ptr GLbyte -> IO ())
ptr_glTexCoord3bvOES = unsafePerformIO $ getCommand "glTexCoord3bvOES"

-- glTexCoord3d ----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glTexCoord3dv'.
glTexCoord3d
  :: MonadIO m
  => GLdouble -- ^ @s@ of type @CoordD@.
  -> GLdouble -- ^ @t@ of type @CoordD@.
  -> GLdouble -- ^ @r@ of type @CoordD@.
  -> m ()
glTexCoord3d v1 v2 v3 = liftIO $ dyn38 ptr_glTexCoord3d v1 v2 v3

{-# NOINLINE ptr_glTexCoord3d #-}
ptr_glTexCoord3d :: FunPtr (GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glTexCoord3d = unsafePerformIO $ getCommand "glTexCoord3d"

-- glTexCoord3dv ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml OpenGL 2.x>.
glTexCoord3dv
  :: MonadIO m
  => Ptr GLdouble -- ^ @v@ pointing to @3@ elements of type @CoordD@.
  -> m ()
glTexCoord3dv v1 = liftIO $ dyn39 ptr_glTexCoord3dv v1

{-# NOINLINE ptr_glTexCoord3dv #-}
ptr_glTexCoord3dv :: FunPtr (Ptr GLdouble -> IO ())
ptr_glTexCoord3dv = unsafePerformIO $ getCommand "glTexCoord3dv"

-- glTexCoord3f ----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glTexCoord3fv'.
glTexCoord3f
  :: MonadIO m
  => GLfloat -- ^ @s@ of type @CoordF@.
  -> GLfloat -- ^ @t@ of type @CoordF@.
  -> GLfloat -- ^ @r@ of type @CoordF@.
  -> m ()
glTexCoord3f v1 v2 v3 = liftIO $ dyn40 ptr_glTexCoord3f v1 v2 v3

{-# NOINLINE ptr_glTexCoord3f #-}
ptr_glTexCoord3f :: FunPtr (GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glTexCoord3f = unsafePerformIO $ getCommand "glTexCoord3f"

-- glTexCoord3fv ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml OpenGL 2.x>.
glTexCoord3fv
  :: MonadIO m
  => Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @CoordF@.
  -> m ()
glTexCoord3fv v1 = liftIO $ dyn41 ptr_glTexCoord3fv v1

{-# NOINLINE ptr_glTexCoord3fv #-}
ptr_glTexCoord3fv :: FunPtr (Ptr GLfloat -> IO ())
ptr_glTexCoord3fv = unsafePerformIO $ getCommand "glTexCoord3fv"

-- glTexCoord3hNV --------------------------------------------------------------

-- | The vector equivalent of this command is 'glTexCoord3hvNV'.
glTexCoord3hNV
  :: MonadIO m
  => GLhalfNV -- ^ @s@ of type @Half16NV@.
  -> GLhalfNV -- ^ @t@ of type @Half16NV@.
  -> GLhalfNV -- ^ @r@ of type @Half16NV@.
  -> m ()
glTexCoord3hNV v1 v2 v3 = liftIO $ dyn98 ptr_glTexCoord3hNV v1 v2 v3

{-# NOINLINE ptr_glTexCoord3hNV #-}
ptr_glTexCoord3hNV :: FunPtr (GLhalfNV -> GLhalfNV -> GLhalfNV -> IO ())
ptr_glTexCoord3hNV = unsafePerformIO $ getCommand "glTexCoord3hNV"

-- glTexCoord3hvNV -------------------------------------------------------------

glTexCoord3hvNV
  :: MonadIO m
  => Ptr GLhalfNV -- ^ @v@ pointing to @3@ elements of type @Half16NV@.
  -> m ()
glTexCoord3hvNV v1 = liftIO $ dyn99 ptr_glTexCoord3hvNV v1

{-# NOINLINE ptr_glTexCoord3hvNV #-}
ptr_glTexCoord3hvNV :: FunPtr (Ptr GLhalfNV -> IO ())
ptr_glTexCoord3hvNV = unsafePerformIO $ getCommand "glTexCoord3hvNV"

-- glTexCoord3i ----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glTexCoord3iv'.
glTexCoord3i
  :: MonadIO m
  => GLint -- ^ @s@ of type @CoordI@.
  -> GLint -- ^ @t@ of type @CoordI@.
  -> GLint -- ^ @r@ of type @CoordI@.
  -> m ()
glTexCoord3i v1 v2 v3 = liftIO $ dyn42 ptr_glTexCoord3i v1 v2 v3

{-# NOINLINE ptr_glTexCoord3i #-}
ptr_glTexCoord3i :: FunPtr (GLint -> GLint -> GLint -> IO ())
ptr_glTexCoord3i = unsafePerformIO $ getCommand "glTexCoord3i"

-- glTexCoord3iv ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml OpenGL 2.x>.
glTexCoord3iv
  :: MonadIO m
  => Ptr GLint -- ^ @v@ pointing to @3@ elements of type @CoordI@.
  -> m ()
glTexCoord3iv v1 = liftIO $ dyn43 ptr_glTexCoord3iv v1

{-# NOINLINE ptr_glTexCoord3iv #-}
ptr_glTexCoord3iv :: FunPtr (Ptr GLint -> IO ())
ptr_glTexCoord3iv = unsafePerformIO $ getCommand "glTexCoord3iv"

-- glTexCoord3s ----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glTexCoord3sv'.
glTexCoord3s
  :: MonadIO m
  => GLshort -- ^ @s@ of type @CoordS@.
  -> GLshort -- ^ @t@ of type @CoordS@.
  -> GLshort -- ^ @r@ of type @CoordS@.
  -> m ()
glTexCoord3s v1 v2 v3 = liftIO $ dyn44 ptr_glTexCoord3s v1 v2 v3

{-# NOINLINE ptr_glTexCoord3s #-}
ptr_glTexCoord3s :: FunPtr (GLshort -> GLshort -> GLshort -> IO ())
ptr_glTexCoord3s = unsafePerformIO $ getCommand "glTexCoord3s"

-- glTexCoord3sv ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml OpenGL 2.x>.
glTexCoord3sv
  :: MonadIO m
  => Ptr GLshort -- ^ @v@ pointing to @3@ elements of type @CoordS@.
  -> m ()
glTexCoord3sv v1 = liftIO $ dyn45 ptr_glTexCoord3sv v1

{-# NOINLINE ptr_glTexCoord3sv #-}
ptr_glTexCoord3sv :: FunPtr (Ptr GLshort -> IO ())
ptr_glTexCoord3sv = unsafePerformIO $ getCommand "glTexCoord3sv"

-- glTexCoord3xOES -------------------------------------------------------------

glTexCoord3xOES
  :: MonadIO m
  => GLfixed -- ^ @s@.
  -> GLfixed -- ^ @t@.
  -> GLfixed -- ^ @r@.
  -> m ()
glTexCoord3xOES v1 v2 v3 = liftIO $ dyn106 ptr_glTexCoord3xOES v1 v2 v3

{-# NOINLINE ptr_glTexCoord3xOES #-}
ptr_glTexCoord3xOES :: FunPtr (GLfixed -> GLfixed -> GLfixed -> IO ())
ptr_glTexCoord3xOES = unsafePerformIO $ getCommand "glTexCoord3xOES"

-- glTexCoord3xvOES ------------------------------------------------------------

glTexCoord3xvOES
  :: MonadIO m
  => Ptr GLfixed -- ^ @coords@ pointing to @3@ elements of type @GLfixed@.
  -> m ()
glTexCoord3xvOES v1 = liftIO $ dyn107 ptr_glTexCoord3xvOES v1

{-# NOINLINE ptr_glTexCoord3xvOES #-}
ptr_glTexCoord3xvOES :: FunPtr (Ptr GLfixed -> IO ())
ptr_glTexCoord3xvOES = unsafePerformIO $ getCommand "glTexCoord3xvOES"

-- glTexCoord4bOES -------------------------------------------------------------

glTexCoord4bOES
  :: MonadIO m
  => GLbyte -- ^ @s@.
  -> GLbyte -- ^ @t@.
  -> GLbyte -- ^ @r@.
  -> GLbyte -- ^ @q@.
  -> m ()
glTexCoord4bOES v1 v2 v3 v4 = liftIO $ dyn108 ptr_glTexCoord4bOES v1 v2 v3 v4

{-# NOINLINE ptr_glTexCoord4bOES #-}
ptr_glTexCoord4bOES :: FunPtr (GLbyte -> GLbyte -> GLbyte -> GLbyte -> IO ())
ptr_glTexCoord4bOES = unsafePerformIO $ getCommand "glTexCoord4bOES"

-- glTexCoord4bvOES ------------------------------------------------------------

glTexCoord4bvOES
  :: MonadIO m
  => Ptr GLbyte -- ^ @coords@ pointing to @4@ elements of type @GLbyte@.
  -> m ()
glTexCoord4bvOES v1 = liftIO $ dyn37 ptr_glTexCoord4bvOES v1

{-# NOINLINE ptr_glTexCoord4bvOES #-}
ptr_glTexCoord4bvOES :: FunPtr (Ptr GLbyte -> IO ())
ptr_glTexCoord4bvOES = unsafePerformIO $ getCommand "glTexCoord4bvOES"

-- glTexCoord4d ----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glTexCoord4dv'.
glTexCoord4d
  :: MonadIO m
  => GLdouble -- ^ @s@ of type @CoordD@.
  -> GLdouble -- ^ @t@ of type @CoordD@.
  -> GLdouble -- ^ @r@ of type @CoordD@.
  -> GLdouble -- ^ @q@ of type @CoordD@.
  -> m ()
glTexCoord4d v1 v2 v3 v4 = liftIO $ dyn109 ptr_glTexCoord4d v1 v2 v3 v4

{-# NOINLINE ptr_glTexCoord4d #-}
ptr_glTexCoord4d :: FunPtr (GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glTexCoord4d = unsafePerformIO $ getCommand "glTexCoord4d"

-- glTexCoord4dv ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml OpenGL 2.x>.
glTexCoord4dv
  :: MonadIO m
  => Ptr GLdouble -- ^ @v@ pointing to @4@ elements of type @CoordD@.
  -> m ()
glTexCoord4dv v1 = liftIO $ dyn39 ptr_glTexCoord4dv v1

{-# NOINLINE ptr_glTexCoord4dv #-}
ptr_glTexCoord4dv :: FunPtr (Ptr GLdouble -> IO ())
ptr_glTexCoord4dv = unsafePerformIO $ getCommand "glTexCoord4dv"

-- glTexCoord4f ----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glTexCoord4fv'.
glTexCoord4f
  :: MonadIO m
  => GLfloat -- ^ @s@ of type @CoordF@.
  -> GLfloat -- ^ @t@ of type @CoordF@.
  -> GLfloat -- ^ @r@ of type @CoordF@.
  -> GLfloat -- ^ @q@ of type @CoordF@.
  -> m ()
glTexCoord4f v1 v2 v3 v4 = liftIO $ dyn49 ptr_glTexCoord4f v1 v2 v3 v4

{-# NOINLINE ptr_glTexCoord4f #-}
ptr_glTexCoord4f :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glTexCoord4f = unsafePerformIO $ getCommand "glTexCoord4f"

-- glTexCoord4fColor4fNormal3fVertex4fSUN --------------------------------------

glTexCoord4fColor4fNormal3fVertex4fSUN
  :: MonadIO m
  => GLfloat -- ^ @s@.
  -> GLfloat -- ^ @t@.
  -> GLfloat -- ^ @p@.
  -> GLfloat -- ^ @q@.
  -> GLfloat -- ^ @r@.
  -> GLfloat -- ^ @g@.
  -> GLfloat -- ^ @b@.
  -> GLfloat -- ^ @a@.
  -> GLfloat -- ^ @nx@.
  -> GLfloat -- ^ @ny@.
  -> GLfloat -- ^ @nz@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> GLfloat -- ^ @w@.
  -> m ()
glTexCoord4fColor4fNormal3fVertex4fSUN v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 = liftIO $ dyn728 ptr_glTexCoord4fColor4fNormal3fVertex4fSUN v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15

{-# NOINLINE ptr_glTexCoord4fColor4fNormal3fVertex4fSUN #-}
ptr_glTexCoord4fColor4fNormal3fVertex4fSUN :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glTexCoord4fColor4fNormal3fVertex4fSUN = unsafePerformIO $ getCommand "glTexCoord4fColor4fNormal3fVertex4fSUN"

-- glTexCoord4fColor4fNormal3fVertex4fvSUN -------------------------------------

glTexCoord4fColor4fNormal3fVertex4fvSUN
  :: MonadIO m
  => Ptr GLfloat -- ^ @tc@ pointing to @4@ elements of type @GLfloat@.
  -> Ptr GLfloat -- ^ @c@ pointing to @4@ elements of type @GLfloat@.
  -> Ptr GLfloat -- ^ @n@ pointing to @3@ elements of type @GLfloat@.
  -> Ptr GLfloat -- ^ @v@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glTexCoord4fColor4fNormal3fVertex4fvSUN v1 v2 v3 v4 = liftIO $ dyn724 ptr_glTexCoord4fColor4fNormal3fVertex4fvSUN v1 v2 v3 v4

{-# NOINLINE ptr_glTexCoord4fColor4fNormal3fVertex4fvSUN #-}
ptr_glTexCoord4fColor4fNormal3fVertex4fvSUN :: FunPtr (Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> IO ())
ptr_glTexCoord4fColor4fNormal3fVertex4fvSUN = unsafePerformIO $ getCommand "glTexCoord4fColor4fNormal3fVertex4fvSUN"

-- glTexCoord4fVertex4fSUN -----------------------------------------------------

glTexCoord4fVertex4fSUN
  :: MonadIO m
  => GLfloat -- ^ @s@.
  -> GLfloat -- ^ @t@.
  -> GLfloat -- ^ @p@.
  -> GLfloat -- ^ @q@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> GLfloat -- ^ @w@.
  -> m ()
glTexCoord4fVertex4fSUN v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn613 ptr_glTexCoord4fVertex4fSUN v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glTexCoord4fVertex4fSUN #-}
ptr_glTexCoord4fVertex4fSUN :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glTexCoord4fVertex4fSUN = unsafePerformIO $ getCommand "glTexCoord4fVertex4fSUN"

-- glTexCoord4fVertex4fvSUN ----------------------------------------------------

glTexCoord4fVertex4fvSUN
  :: MonadIO m
  => Ptr GLfloat -- ^ @tc@ pointing to @4@ elements of type @GLfloat@.
  -> Ptr GLfloat -- ^ @v@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glTexCoord4fVertex4fvSUN v1 v2 = liftIO $ dyn97 ptr_glTexCoord4fVertex4fvSUN v1 v2

{-# NOINLINE ptr_glTexCoord4fVertex4fvSUN #-}
ptr_glTexCoord4fVertex4fvSUN :: FunPtr (Ptr GLfloat -> Ptr GLfloat -> IO ())
ptr_glTexCoord4fVertex4fvSUN = unsafePerformIO $ getCommand "glTexCoord4fVertex4fvSUN"

-- glTexCoord4fv ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml OpenGL 2.x>.
glTexCoord4fv
  :: MonadIO m
  => Ptr GLfloat -- ^ @v@ pointing to @4@ elements of type @CoordF@.
  -> m ()
glTexCoord4fv v1 = liftIO $ dyn41 ptr_glTexCoord4fv v1

{-# NOINLINE ptr_glTexCoord4fv #-}
ptr_glTexCoord4fv :: FunPtr (Ptr GLfloat -> IO ())
ptr_glTexCoord4fv = unsafePerformIO $ getCommand "glTexCoord4fv"

-- glTexCoord4hNV --------------------------------------------------------------

-- | The vector equivalent of this command is 'glTexCoord4hvNV'.
glTexCoord4hNV
  :: MonadIO m
  => GLhalfNV -- ^ @s@ of type @Half16NV@.
  -> GLhalfNV -- ^ @t@ of type @Half16NV@.
  -> GLhalfNV -- ^ @r@ of type @Half16NV@.
  -> GLhalfNV -- ^ @q@ of type @Half16NV@.
  -> m ()
glTexCoord4hNV v1 v2 v3 v4 = liftIO $ dyn112 ptr_glTexCoord4hNV v1 v2 v3 v4

{-# NOINLINE ptr_glTexCoord4hNV #-}
ptr_glTexCoord4hNV :: FunPtr (GLhalfNV -> GLhalfNV -> GLhalfNV -> GLhalfNV -> IO ())
ptr_glTexCoord4hNV = unsafePerformIO $ getCommand "glTexCoord4hNV"

-- glTexCoord4hvNV -------------------------------------------------------------

glTexCoord4hvNV
  :: MonadIO m
  => Ptr GLhalfNV -- ^ @v@ pointing to @4@ elements of type @Half16NV@.
  -> m ()
glTexCoord4hvNV v1 = liftIO $ dyn99 ptr_glTexCoord4hvNV v1

{-# NOINLINE ptr_glTexCoord4hvNV #-}
ptr_glTexCoord4hvNV :: FunPtr (Ptr GLhalfNV -> IO ())
ptr_glTexCoord4hvNV = unsafePerformIO $ getCommand "glTexCoord4hvNV"

-- glTexCoord4i ----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glTexCoord4iv'.
glTexCoord4i
  :: MonadIO m
  => GLint -- ^ @s@ of type @CoordI@.
  -> GLint -- ^ @t@ of type @CoordI@.
  -> GLint -- ^ @r@ of type @CoordI@.
  -> GLint -- ^ @q@ of type @CoordI@.
  -> m ()
glTexCoord4i v1 v2 v3 v4 = liftIO $ dyn76 ptr_glTexCoord4i v1 v2 v3 v4

{-# NOINLINE ptr_glTexCoord4i #-}
ptr_glTexCoord4i :: FunPtr (GLint -> GLint -> GLint -> GLint -> IO ())
ptr_glTexCoord4i = unsafePerformIO $ getCommand "glTexCoord4i"

-- glTexCoord4iv ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml OpenGL 2.x>.
glTexCoord4iv
  :: MonadIO m
  => Ptr GLint -- ^ @v@ pointing to @4@ elements of type @CoordI@.
  -> m ()
glTexCoord4iv v1 = liftIO $ dyn43 ptr_glTexCoord4iv v1

{-# NOINLINE ptr_glTexCoord4iv #-}
ptr_glTexCoord4iv :: FunPtr (Ptr GLint -> IO ())
ptr_glTexCoord4iv = unsafePerformIO $ getCommand "glTexCoord4iv"

-- glTexCoord4s ----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glTexCoord4sv'.
glTexCoord4s
  :: MonadIO m
  => GLshort -- ^ @s@ of type @CoordS@.
  -> GLshort -- ^ @t@ of type @CoordS@.
  -> GLshort -- ^ @r@ of type @CoordS@.
  -> GLshort -- ^ @q@ of type @CoordS@.
  -> m ()
glTexCoord4s v1 v2 v3 v4 = liftIO $ dyn113 ptr_glTexCoord4s v1 v2 v3 v4

{-# NOINLINE ptr_glTexCoord4s #-}
ptr_glTexCoord4s :: FunPtr (GLshort -> GLshort -> GLshort -> GLshort -> IO ())
ptr_glTexCoord4s = unsafePerformIO $ getCommand "glTexCoord4s"

-- glTexCoord4sv ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml OpenGL 2.x>.
glTexCoord4sv
  :: MonadIO m
  => Ptr GLshort -- ^ @v@ pointing to @4@ elements of type @CoordS@.
  -> m ()
glTexCoord4sv v1 = liftIO $ dyn45 ptr_glTexCoord4sv v1

{-# NOINLINE ptr_glTexCoord4sv #-}
ptr_glTexCoord4sv :: FunPtr (Ptr GLshort -> IO ())
ptr_glTexCoord4sv = unsafePerformIO $ getCommand "glTexCoord4sv"

-- glTexCoord4xOES -------------------------------------------------------------

glTexCoord4xOES
  :: MonadIO m
  => GLfixed -- ^ @s@.
  -> GLfixed -- ^ @t@.
  -> GLfixed -- ^ @r@.
  -> GLfixed -- ^ @q@.
  -> m ()
glTexCoord4xOES v1 v2 v3 v4 = liftIO $ dyn50 ptr_glTexCoord4xOES v1 v2 v3 v4

{-# NOINLINE ptr_glTexCoord4xOES #-}
ptr_glTexCoord4xOES :: FunPtr (GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ())
ptr_glTexCoord4xOES = unsafePerformIO $ getCommand "glTexCoord4xOES"

-- glTexCoord4xvOES ------------------------------------------------------------

glTexCoord4xvOES
  :: MonadIO m
  => Ptr GLfixed -- ^ @coords@ pointing to @4@ elements of type @GLfixed@.
  -> m ()
glTexCoord4xvOES v1 = liftIO $ dyn107 ptr_glTexCoord4xvOES v1

{-# NOINLINE ptr_glTexCoord4xvOES #-}
ptr_glTexCoord4xvOES :: FunPtr (Ptr GLfixed -> IO ())
ptr_glTexCoord4xvOES = unsafePerformIO $ getCommand "glTexCoord4xvOES"

-- glTexCoordFormatNV ----------------------------------------------------------

glTexCoordFormatNV
  :: MonadIO m
  => GLint -- ^ @size@.
  -> GLenum -- ^ @type@.
  -> GLsizei -- ^ @stride@.
  -> m ()
glTexCoordFormatNV v1 v2 v3 = liftIO $ dyn119 ptr_glTexCoordFormatNV v1 v2 v3

{-# NOINLINE ptr_glTexCoordFormatNV #-}
ptr_glTexCoordFormatNV :: FunPtr (GLint -> GLenum -> GLsizei -> IO ())
ptr_glTexCoordFormatNV = unsafePerformIO $ getCommand "glTexCoordFormatNV"

