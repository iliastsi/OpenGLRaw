--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F53
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

module Graphics.GL.Functions.F53 (
  glQueryObjectParameteruiAMD,
  glRasterPos2d,
  glRasterPos2dv,
  glRasterPos2f,
  glRasterPos2fv,
  glRasterPos2i,
  glRasterPos2iv,
  glRasterPos2s,
  glRasterPos2sv,
  glRasterPos2xOES,
  glRasterPos2xvOES,
  glRasterPos3d,
  glRasterPos3dv,
  glRasterPos3f,
  glRasterPos3fv,
  glRasterPos3i,
  glRasterPos3iv,
  glRasterPos3s,
  glRasterPos3sv,
  glRasterPos3xOES,
  glRasterPos3xvOES,
  glRasterPos4d,
  glRasterPos4dv,
  glRasterPos4f,
  glRasterPos4fv,
  glRasterPos4i,
  glRasterPos4iv,
  glRasterPos4s,
  glRasterPos4sv,
  glRasterPos4xOES,
  glRasterPos4xvOES,
  glRasterSamplesEXT,
  glReadBuffer,
  glReadBufferIndexedEXT,
  glReadBufferNV,
  glReadInstrumentsSGIX,
  glReadPixels,
  glReadnPixels,
  glReadnPixelsARB,
  glReadnPixelsEXT
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

-- glQueryObjectParameteruiAMD -------------------------------------------------

glQueryObjectParameteruiAMD
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLuint -- ^ @id@.
  -> GLenum -- ^ @pname@.
  -> GLuint -- ^ @param@ of type [OcclusionQueryEventMaskAMD](Graphics-GL-Groups.html#OcclusionQueryEventMaskAMD).
  -> m ()
glQueryObjectParameteruiAMD v1 v2 v3 v4 = liftIO $ dyn668 ptr_glQueryObjectParameteruiAMD v1 v2 v3 v4

{-# NOINLINE ptr_glQueryObjectParameteruiAMD #-}
ptr_glQueryObjectParameteruiAMD :: FunPtr (GLenum -> GLuint -> GLenum -> GLuint -> IO ())
ptr_glQueryObjectParameteruiAMD = unsafePerformIO $ getCommand "glQueryObjectParameteruiAMD"

-- glRasterPos2d ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRasterPos.xml OpenGL 2.x>. The vector equivalent of this command is 'glRasterPos2dv'.
glRasterPos2d
  :: MonadIO m
  => GLdouble -- ^ @x@ of type @CoordD@.
  -> GLdouble -- ^ @y@ of type @CoordD@.
  -> m ()
glRasterPos2d v1 v2 = liftIO $ dyn217 ptr_glRasterPos2d v1 v2

{-# NOINLINE ptr_glRasterPos2d #-}
ptr_glRasterPos2d :: FunPtr (GLdouble -> GLdouble -> IO ())
ptr_glRasterPos2d = unsafePerformIO $ getCommand "glRasterPos2d"

-- glRasterPos2dv --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRasterPos.xml OpenGL 2.x>.
glRasterPos2dv
  :: MonadIO m
  => Ptr GLdouble -- ^ @v@ pointing to @2@ elements of type @CoordD@.
  -> m ()
glRasterPos2dv v1 = liftIO $ dyn39 ptr_glRasterPos2dv v1

{-# NOINLINE ptr_glRasterPos2dv #-}
ptr_glRasterPos2dv :: FunPtr (Ptr GLdouble -> IO ())
ptr_glRasterPos2dv = unsafePerformIO $ getCommand "glRasterPos2dv"

-- glRasterPos2f ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRasterPos.xml OpenGL 2.x>. The vector equivalent of this command is 'glRasterPos2fv'.
glRasterPos2f
  :: MonadIO m
  => GLfloat -- ^ @x@ of type @CoordF@.
  -> GLfloat -- ^ @y@ of type @CoordF@.
  -> m ()
glRasterPos2f v1 v2 = liftIO $ dyn222 ptr_glRasterPos2f v1 v2

{-# NOINLINE ptr_glRasterPos2f #-}
ptr_glRasterPos2f :: FunPtr (GLfloat -> GLfloat -> IO ())
ptr_glRasterPos2f = unsafePerformIO $ getCommand "glRasterPos2f"

-- glRasterPos2fv --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRasterPos.xml OpenGL 2.x>.
glRasterPos2fv
  :: MonadIO m
  => Ptr GLfloat -- ^ @v@ pointing to @2@ elements of type @CoordF@.
  -> m ()
glRasterPos2fv v1 = liftIO $ dyn41 ptr_glRasterPos2fv v1

{-# NOINLINE ptr_glRasterPos2fv #-}
ptr_glRasterPos2fv :: FunPtr (Ptr GLfloat -> IO ())
ptr_glRasterPos2fv = unsafePerformIO $ getCommand "glRasterPos2fv"

-- glRasterPos2i ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRasterPos.xml OpenGL 2.x>. The vector equivalent of this command is 'glRasterPos2iv'.
glRasterPos2i
  :: MonadIO m
  => GLint -- ^ @x@ of type @CoordI@.
  -> GLint -- ^ @y@ of type @CoordI@.
  -> m ()
glRasterPos2i v1 v2 = liftIO $ dyn266 ptr_glRasterPos2i v1 v2

{-# NOINLINE ptr_glRasterPos2i #-}
ptr_glRasterPos2i :: FunPtr (GLint -> GLint -> IO ())
ptr_glRasterPos2i = unsafePerformIO $ getCommand "glRasterPos2i"

-- glRasterPos2iv --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRasterPos.xml OpenGL 2.x>.
glRasterPos2iv
  :: MonadIO m
  => Ptr GLint -- ^ @v@ pointing to @2@ elements of type @CoordI@.
  -> m ()
glRasterPos2iv v1 = liftIO $ dyn43 ptr_glRasterPos2iv v1

{-# NOINLINE ptr_glRasterPos2iv #-}
ptr_glRasterPos2iv :: FunPtr (Ptr GLint -> IO ())
ptr_glRasterPos2iv = unsafePerformIO $ getCommand "glRasterPos2iv"

-- glRasterPos2s ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRasterPos.xml OpenGL 2.x>. The vector equivalent of this command is 'glRasterPos2sv'.
glRasterPos2s
  :: MonadIO m
  => GLshort -- ^ @x@ of type @CoordS@.
  -> GLshort -- ^ @y@ of type @CoordS@.
  -> m ()
glRasterPos2s v1 v2 = liftIO $ dyn669 ptr_glRasterPos2s v1 v2

{-# NOINLINE ptr_glRasterPos2s #-}
ptr_glRasterPos2s :: FunPtr (GLshort -> GLshort -> IO ())
ptr_glRasterPos2s = unsafePerformIO $ getCommand "glRasterPos2s"

-- glRasterPos2sv --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRasterPos.xml OpenGL 2.x>.
glRasterPos2sv
  :: MonadIO m
  => Ptr GLshort -- ^ @v@ pointing to @2@ elements of type @CoordS@.
  -> m ()
glRasterPos2sv v1 = liftIO $ dyn45 ptr_glRasterPos2sv v1

{-# NOINLINE ptr_glRasterPos2sv #-}
ptr_glRasterPos2sv :: FunPtr (Ptr GLshort -> IO ())
ptr_glRasterPos2sv = unsafePerformIO $ getCommand "glRasterPos2sv"

-- glRasterPos2xOES ------------------------------------------------------------

glRasterPos2xOES
  :: MonadIO m
  => GLfixed -- ^ @x@.
  -> GLfixed -- ^ @y@.
  -> m ()
glRasterPos2xOES v1 v2 = liftIO $ dyn224 ptr_glRasterPos2xOES v1 v2

{-# NOINLINE ptr_glRasterPos2xOES #-}
ptr_glRasterPos2xOES :: FunPtr (GLfixed -> GLfixed -> IO ())
ptr_glRasterPos2xOES = unsafePerformIO $ getCommand "glRasterPos2xOES"

-- glRasterPos2xvOES -----------------------------------------------------------

glRasterPos2xvOES
  :: MonadIO m
  => Ptr GLfixed -- ^ @coords@ pointing to @2@ elements of type @GLfixed@.
  -> m ()
glRasterPos2xvOES v1 = liftIO $ dyn107 ptr_glRasterPos2xvOES v1

{-# NOINLINE ptr_glRasterPos2xvOES #-}
ptr_glRasterPos2xvOES :: FunPtr (Ptr GLfixed -> IO ())
ptr_glRasterPos2xvOES = unsafePerformIO $ getCommand "glRasterPos2xvOES"

-- glRasterPos3d ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRasterPos.xml OpenGL 2.x>. The vector equivalent of this command is 'glRasterPos3dv'.
glRasterPos3d
  :: MonadIO m
  => GLdouble -- ^ @x@ of type @CoordD@.
  -> GLdouble -- ^ @y@ of type @CoordD@.
  -> GLdouble -- ^ @z@ of type @CoordD@.
  -> m ()
glRasterPos3d v1 v2 v3 = liftIO $ dyn38 ptr_glRasterPos3d v1 v2 v3

{-# NOINLINE ptr_glRasterPos3d #-}
ptr_glRasterPos3d :: FunPtr (GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glRasterPos3d = unsafePerformIO $ getCommand "glRasterPos3d"

-- glRasterPos3dv --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRasterPos.xml OpenGL 2.x>.
glRasterPos3dv
  :: MonadIO m
  => Ptr GLdouble -- ^ @v@ pointing to @3@ elements of type @CoordD@.
  -> m ()
glRasterPos3dv v1 = liftIO $ dyn39 ptr_glRasterPos3dv v1

{-# NOINLINE ptr_glRasterPos3dv #-}
ptr_glRasterPos3dv :: FunPtr (Ptr GLdouble -> IO ())
ptr_glRasterPos3dv = unsafePerformIO $ getCommand "glRasterPos3dv"

-- glRasterPos3f ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRasterPos.xml OpenGL 2.x>. The vector equivalent of this command is 'glRasterPos3fv'.
glRasterPos3f
  :: MonadIO m
  => GLfloat -- ^ @x@ of type @CoordF@.
  -> GLfloat -- ^ @y@ of type @CoordF@.
  -> GLfloat -- ^ @z@ of type @CoordF@.
  -> m ()
glRasterPos3f v1 v2 v3 = liftIO $ dyn40 ptr_glRasterPos3f v1 v2 v3

{-# NOINLINE ptr_glRasterPos3f #-}
ptr_glRasterPos3f :: FunPtr (GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glRasterPos3f = unsafePerformIO $ getCommand "glRasterPos3f"

-- glRasterPos3fv --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRasterPos.xml OpenGL 2.x>.
glRasterPos3fv
  :: MonadIO m
  => Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @CoordF@.
  -> m ()
glRasterPos3fv v1 = liftIO $ dyn41 ptr_glRasterPos3fv v1

{-# NOINLINE ptr_glRasterPos3fv #-}
ptr_glRasterPos3fv :: FunPtr (Ptr GLfloat -> IO ())
ptr_glRasterPos3fv = unsafePerformIO $ getCommand "glRasterPos3fv"

-- glRasterPos3i ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRasterPos.xml OpenGL 2.x>. The vector equivalent of this command is 'glRasterPos3iv'.
glRasterPos3i
  :: MonadIO m
  => GLint -- ^ @x@ of type @CoordI@.
  -> GLint -- ^ @y@ of type @CoordI@.
  -> GLint -- ^ @z@ of type @CoordI@.
  -> m ()
glRasterPos3i v1 v2 v3 = liftIO $ dyn42 ptr_glRasterPos3i v1 v2 v3

{-# NOINLINE ptr_glRasterPos3i #-}
ptr_glRasterPos3i :: FunPtr (GLint -> GLint -> GLint -> IO ())
ptr_glRasterPos3i = unsafePerformIO $ getCommand "glRasterPos3i"

-- glRasterPos3iv --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRasterPos.xml OpenGL 2.x>.
glRasterPos3iv
  :: MonadIO m
  => Ptr GLint -- ^ @v@ pointing to @3@ elements of type @CoordI@.
  -> m ()
glRasterPos3iv v1 = liftIO $ dyn43 ptr_glRasterPos3iv v1

{-# NOINLINE ptr_glRasterPos3iv #-}
ptr_glRasterPos3iv :: FunPtr (Ptr GLint -> IO ())
ptr_glRasterPos3iv = unsafePerformIO $ getCommand "glRasterPos3iv"

-- glRasterPos3s ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRasterPos.xml OpenGL 2.x>. The vector equivalent of this command is 'glRasterPos3sv'.
glRasterPos3s
  :: MonadIO m
  => GLshort -- ^ @x@ of type @CoordS@.
  -> GLshort -- ^ @y@ of type @CoordS@.
  -> GLshort -- ^ @z@ of type @CoordS@.
  -> m ()
glRasterPos3s v1 v2 v3 = liftIO $ dyn44 ptr_glRasterPos3s v1 v2 v3

{-# NOINLINE ptr_glRasterPos3s #-}
ptr_glRasterPos3s :: FunPtr (GLshort -> GLshort -> GLshort -> IO ())
ptr_glRasterPos3s = unsafePerformIO $ getCommand "glRasterPos3s"

-- glRasterPos3sv --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRasterPos.xml OpenGL 2.x>.
glRasterPos3sv
  :: MonadIO m
  => Ptr GLshort -- ^ @v@ pointing to @3@ elements of type @CoordS@.
  -> m ()
glRasterPos3sv v1 = liftIO $ dyn45 ptr_glRasterPos3sv v1

{-# NOINLINE ptr_glRasterPos3sv #-}
ptr_glRasterPos3sv :: FunPtr (Ptr GLshort -> IO ())
ptr_glRasterPos3sv = unsafePerformIO $ getCommand "glRasterPos3sv"

-- glRasterPos3xOES ------------------------------------------------------------

glRasterPos3xOES
  :: MonadIO m
  => GLfixed -- ^ @x@.
  -> GLfixed -- ^ @y@.
  -> GLfixed -- ^ @z@.
  -> m ()
glRasterPos3xOES v1 v2 v3 = liftIO $ dyn106 ptr_glRasterPos3xOES v1 v2 v3

{-# NOINLINE ptr_glRasterPos3xOES #-}
ptr_glRasterPos3xOES :: FunPtr (GLfixed -> GLfixed -> GLfixed -> IO ())
ptr_glRasterPos3xOES = unsafePerformIO $ getCommand "glRasterPos3xOES"

-- glRasterPos3xvOES -----------------------------------------------------------

glRasterPos3xvOES
  :: MonadIO m
  => Ptr GLfixed -- ^ @coords@ pointing to @3@ elements of type @GLfixed@.
  -> m ()
glRasterPos3xvOES v1 = liftIO $ dyn107 ptr_glRasterPos3xvOES v1

{-# NOINLINE ptr_glRasterPos3xvOES #-}
ptr_glRasterPos3xvOES :: FunPtr (Ptr GLfixed -> IO ())
ptr_glRasterPos3xvOES = unsafePerformIO $ getCommand "glRasterPos3xvOES"

-- glRasterPos4d ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRasterPos.xml OpenGL 2.x>. The vector equivalent of this command is 'glRasterPos4dv'.
glRasterPos4d
  :: MonadIO m
  => GLdouble -- ^ @x@ of type @CoordD@.
  -> GLdouble -- ^ @y@ of type @CoordD@.
  -> GLdouble -- ^ @z@ of type @CoordD@.
  -> GLdouble -- ^ @w@ of type @CoordD@.
  -> m ()
glRasterPos4d v1 v2 v3 v4 = liftIO $ dyn109 ptr_glRasterPos4d v1 v2 v3 v4

{-# NOINLINE ptr_glRasterPos4d #-}
ptr_glRasterPos4d :: FunPtr (GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glRasterPos4d = unsafePerformIO $ getCommand "glRasterPos4d"

-- glRasterPos4dv --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRasterPos.xml OpenGL 2.x>.
glRasterPos4dv
  :: MonadIO m
  => Ptr GLdouble -- ^ @v@ pointing to @4@ elements of type @CoordD@.
  -> m ()
glRasterPos4dv v1 = liftIO $ dyn39 ptr_glRasterPos4dv v1

{-# NOINLINE ptr_glRasterPos4dv #-}
ptr_glRasterPos4dv :: FunPtr (Ptr GLdouble -> IO ())
ptr_glRasterPos4dv = unsafePerformIO $ getCommand "glRasterPos4dv"

-- glRasterPos4f ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRasterPos.xml OpenGL 2.x>. The vector equivalent of this command is 'glRasterPos4fv'.
glRasterPos4f
  :: MonadIO m
  => GLfloat -- ^ @x@ of type @CoordF@.
  -> GLfloat -- ^ @y@ of type @CoordF@.
  -> GLfloat -- ^ @z@ of type @CoordF@.
  -> GLfloat -- ^ @w@ of type @CoordF@.
  -> m ()
glRasterPos4f v1 v2 v3 v4 = liftIO $ dyn49 ptr_glRasterPos4f v1 v2 v3 v4

{-# NOINLINE ptr_glRasterPos4f #-}
ptr_glRasterPos4f :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glRasterPos4f = unsafePerformIO $ getCommand "glRasterPos4f"

-- glRasterPos4fv --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRasterPos.xml OpenGL 2.x>.
glRasterPos4fv
  :: MonadIO m
  => Ptr GLfloat -- ^ @v@ pointing to @4@ elements of type @CoordF@.
  -> m ()
glRasterPos4fv v1 = liftIO $ dyn41 ptr_glRasterPos4fv v1

{-# NOINLINE ptr_glRasterPos4fv #-}
ptr_glRasterPos4fv :: FunPtr (Ptr GLfloat -> IO ())
ptr_glRasterPos4fv = unsafePerformIO $ getCommand "glRasterPos4fv"

-- glRasterPos4i ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRasterPos.xml OpenGL 2.x>. The vector equivalent of this command is 'glRasterPos4iv'.
glRasterPos4i
  :: MonadIO m
  => GLint -- ^ @x@ of type @CoordI@.
  -> GLint -- ^ @y@ of type @CoordI@.
  -> GLint -- ^ @z@ of type @CoordI@.
  -> GLint -- ^ @w@ of type @CoordI@.
  -> m ()
glRasterPos4i v1 v2 v3 v4 = liftIO $ dyn76 ptr_glRasterPos4i v1 v2 v3 v4

{-# NOINLINE ptr_glRasterPos4i #-}
ptr_glRasterPos4i :: FunPtr (GLint -> GLint -> GLint -> GLint -> IO ())
ptr_glRasterPos4i = unsafePerformIO $ getCommand "glRasterPos4i"

-- glRasterPos4iv --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRasterPos.xml OpenGL 2.x>.
glRasterPos4iv
  :: MonadIO m
  => Ptr GLint -- ^ @v@ pointing to @4@ elements of type @CoordI@.
  -> m ()
glRasterPos4iv v1 = liftIO $ dyn43 ptr_glRasterPos4iv v1

{-# NOINLINE ptr_glRasterPos4iv #-}
ptr_glRasterPos4iv :: FunPtr (Ptr GLint -> IO ())
ptr_glRasterPos4iv = unsafePerformIO $ getCommand "glRasterPos4iv"

-- glRasterPos4s ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRasterPos.xml OpenGL 2.x>. The vector equivalent of this command is 'glRasterPos4sv'.
glRasterPos4s
  :: MonadIO m
  => GLshort -- ^ @x@ of type @CoordS@.
  -> GLshort -- ^ @y@ of type @CoordS@.
  -> GLshort -- ^ @z@ of type @CoordS@.
  -> GLshort -- ^ @w@ of type @CoordS@.
  -> m ()
glRasterPos4s v1 v2 v3 v4 = liftIO $ dyn113 ptr_glRasterPos4s v1 v2 v3 v4

{-# NOINLINE ptr_glRasterPos4s #-}
ptr_glRasterPos4s :: FunPtr (GLshort -> GLshort -> GLshort -> GLshort -> IO ())
ptr_glRasterPos4s = unsafePerformIO $ getCommand "glRasterPos4s"

-- glRasterPos4sv --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRasterPos.xml OpenGL 2.x>.
glRasterPos4sv
  :: MonadIO m
  => Ptr GLshort -- ^ @v@ pointing to @4@ elements of type @CoordS@.
  -> m ()
glRasterPos4sv v1 = liftIO $ dyn45 ptr_glRasterPos4sv v1

{-# NOINLINE ptr_glRasterPos4sv #-}
ptr_glRasterPos4sv :: FunPtr (Ptr GLshort -> IO ())
ptr_glRasterPos4sv = unsafePerformIO $ getCommand "glRasterPos4sv"

-- glRasterPos4xOES ------------------------------------------------------------

glRasterPos4xOES
  :: MonadIO m
  => GLfixed -- ^ @x@.
  -> GLfixed -- ^ @y@.
  -> GLfixed -- ^ @z@.
  -> GLfixed -- ^ @w@.
  -> m ()
glRasterPos4xOES v1 v2 v3 v4 = liftIO $ dyn50 ptr_glRasterPos4xOES v1 v2 v3 v4

{-# NOINLINE ptr_glRasterPos4xOES #-}
ptr_glRasterPos4xOES :: FunPtr (GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ())
ptr_glRasterPos4xOES = unsafePerformIO $ getCommand "glRasterPos4xOES"

-- glRasterPos4xvOES -----------------------------------------------------------

glRasterPos4xvOES
  :: MonadIO m
  => Ptr GLfixed -- ^ @coords@ pointing to @4@ elements of type @GLfixed@.
  -> m ()
glRasterPos4xvOES v1 = liftIO $ dyn107 ptr_glRasterPos4xvOES v1

{-# NOINLINE ptr_glRasterPos4xvOES #-}
ptr_glRasterPos4xvOES :: FunPtr (Ptr GLfixed -> IO ())
ptr_glRasterPos4xvOES = unsafePerformIO $ getCommand "glRasterPos4xvOES"

-- glRasterSamplesEXT ----------------------------------------------------------

glRasterSamplesEXT
  :: MonadIO m
  => GLuint -- ^ @samples@.
  -> GLboolean -- ^ @fixedsamplelocations@.
  -> m ()
glRasterSamplesEXT v1 v2 = liftIO $ dyn670 ptr_glRasterSamplesEXT v1 v2

{-# NOINLINE ptr_glRasterSamplesEXT #-}
ptr_glRasterSamplesEXT :: FunPtr (GLuint -> GLboolean -> IO ())
ptr_glRasterSamplesEXT = unsafePerformIO $ getCommand "glRasterSamplesEXT"

-- glReadBuffer ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glReadBuffer.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glReadBuffer.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glReadBuffer.xhtml OpenGL 4.x>.
glReadBuffer
  :: MonadIO m
  => GLenum -- ^ @src@ of type [ReadBufferMode](Graphics-GL-Groups.html#ReadBufferMode).
  -> m ()
glReadBuffer v1 = liftIO $ dyn4 ptr_glReadBuffer v1

{-# NOINLINE ptr_glReadBuffer #-}
ptr_glReadBuffer :: FunPtr (GLenum -> IO ())
ptr_glReadBuffer = unsafePerformIO $ getCommand "glReadBuffer"

-- glReadBufferIndexedEXT ------------------------------------------------------

glReadBufferIndexedEXT
  :: MonadIO m
  => GLenum -- ^ @src@.
  -> GLint -- ^ @index@.
  -> m ()
glReadBufferIndexedEXT v1 v2 = liftIO $ dyn55 ptr_glReadBufferIndexedEXT v1 v2

{-# NOINLINE ptr_glReadBufferIndexedEXT #-}
ptr_glReadBufferIndexedEXT :: FunPtr (GLenum -> GLint -> IO ())
ptr_glReadBufferIndexedEXT = unsafePerformIO $ getCommand "glReadBufferIndexedEXT"

-- glReadBufferNV --------------------------------------------------------------

glReadBufferNV
  :: MonadIO m
  => GLenum -- ^ @mode@.
  -> m ()
glReadBufferNV v1 = liftIO $ dyn4 ptr_glReadBufferNV v1

{-# NOINLINE ptr_glReadBufferNV #-}
ptr_glReadBufferNV :: FunPtr (GLenum -> IO ())
ptr_glReadBufferNV = unsafePerformIO $ getCommand "glReadBufferNV"

-- glReadInstrumentsSGIX -------------------------------------------------------

glReadInstrumentsSGIX
  :: MonadIO m
  => GLint -- ^ @marker@.
  -> m ()
glReadInstrumentsSGIX v1 = liftIO $ dyn12 ptr_glReadInstrumentsSGIX v1

{-# NOINLINE ptr_glReadInstrumentsSGIX #-}
ptr_glReadInstrumentsSGIX :: FunPtr (GLint -> IO ())
ptr_glReadInstrumentsSGIX = unsafePerformIO $ getCommand "glReadInstrumentsSGIX"

-- glReadPixels ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glReadPixels.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glReadPixels.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glReadPixels.xhtml OpenGL 4.x>.
glReadPixels
  :: MonadIO m
  => GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @pixels@ pointing to @COMPSIZE(format,type,width,height)@ elements of type @a@.
  -> m ()
glReadPixels v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn671 ptr_glReadPixels v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glReadPixels #-}
ptr_glReadPixels :: FunPtr (GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glReadPixels = unsafePerformIO $ getCommand "glReadPixels"

-- glReadnPixels ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glReadPixels.xhtml OpenGL 4.x>.
glReadnPixels
  :: MonadIO m
  => GLint -- ^ @x@.
  -> GLint -- ^ @y@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLenum -- ^ @format@.
  -> GLenum -- ^ @type@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr a -- ^ @data@.
  -> m ()
glReadnPixels v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn672 ptr_glReadnPixels v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glReadnPixels #-}
ptr_glReadnPixels :: FunPtr (GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glReadnPixels = unsafePerformIO $ getCommand "glReadnPixels"

-- glReadnPixelsARB ------------------------------------------------------------

-- | This command is an alias for 'glReadnPixels'.
glReadnPixelsARB
  :: MonadIO m
  => GLint -- ^ @x@.
  -> GLint -- ^ @y@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLenum -- ^ @format@.
  -> GLenum -- ^ @type@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr a -- ^ @data@ pointing to @bufSize@ elements of type @a@.
  -> m ()
glReadnPixelsARB v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn672 ptr_glReadnPixelsARB v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glReadnPixelsARB #-}
ptr_glReadnPixelsARB :: FunPtr (GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glReadnPixelsARB = unsafePerformIO $ getCommand "glReadnPixelsARB"

-- glReadnPixelsEXT ------------------------------------------------------------

-- | This command is an alias for 'glReadnPixels'.
glReadnPixelsEXT
  :: MonadIO m
  => GLint -- ^ @x@.
  -> GLint -- ^ @y@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLenum -- ^ @format@.
  -> GLenum -- ^ @type@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr a -- ^ @data@ pointing to @bufSize@ elements of type @a@.
  -> m ()
glReadnPixelsEXT v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn672 ptr_glReadnPixelsEXT v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glReadnPixelsEXT #-}
ptr_glReadnPixelsEXT :: FunPtr (GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glReadnPixelsEXT = unsafePerformIO $ getCommand "glReadnPixelsEXT"

