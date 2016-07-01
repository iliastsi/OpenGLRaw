--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F71
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

module Graphics.GL.Functions.F71 (
  glVertexAttrib2dv,
  glVertexAttrib2dvARB,
  glVertexAttrib2dvNV,
  glVertexAttrib2f,
  glVertexAttrib2fARB,
  glVertexAttrib2fNV,
  glVertexAttrib2fv,
  glVertexAttrib2fvARB,
  glVertexAttrib2fvNV,
  glVertexAttrib2hNV,
  glVertexAttrib2hvNV,
  glVertexAttrib2s,
  glVertexAttrib2sARB,
  glVertexAttrib2sNV,
  glVertexAttrib2sv,
  glVertexAttrib2svARB,
  glVertexAttrib2svNV,
  glVertexAttrib3d,
  glVertexAttrib3dARB,
  glVertexAttrib3dNV,
  glVertexAttrib3dv,
  glVertexAttrib3dvARB,
  glVertexAttrib3dvNV,
  glVertexAttrib3f,
  glVertexAttrib3fARB,
  glVertexAttrib3fNV,
  glVertexAttrib3fv,
  glVertexAttrib3fvARB,
  glVertexAttrib3fvNV,
  glVertexAttrib3hNV,
  glVertexAttrib3hvNV,
  glVertexAttrib3s,
  glVertexAttrib3sARB,
  glVertexAttrib3sNV,
  glVertexAttrib3sv,
  glVertexAttrib3svARB,
  glVertexAttrib3svNV,
  glVertexAttrib4Nbv,
  glVertexAttrib4NbvARB,
  glVertexAttrib4Niv
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

-- glVertexAttrib2dv -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttrib2dv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLdouble -- ^ @v@ pointing to @2@ elements of type @GLdouble@.
  -> m ()
glVertexAttrib2dv v1 v2 = liftIO $ dyn828 ptr_glVertexAttrib2dv v1 v2

{-# NOINLINE ptr_glVertexAttrib2dv #-}
ptr_glVertexAttrib2dv :: FunPtr (GLuint -> Ptr GLdouble -> IO ())
ptr_glVertexAttrib2dv = unsafePerformIO $ getCommand "glVertexAttrib2dv"

-- glVertexAttrib2dvARB --------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib2dv'.
glVertexAttrib2dvARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLdouble -- ^ @v@ pointing to @2@ elements of type @GLdouble@.
  -> m ()
glVertexAttrib2dvARB v1 v2 = liftIO $ dyn828 ptr_glVertexAttrib2dvARB v1 v2

{-# NOINLINE ptr_glVertexAttrib2dvARB #-}
ptr_glVertexAttrib2dvARB :: FunPtr (GLuint -> Ptr GLdouble -> IO ())
ptr_glVertexAttrib2dvARB = unsafePerformIO $ getCommand "glVertexAttrib2dvARB"

-- glVertexAttrib2dvNV ---------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib2dv'.
glVertexAttrib2dvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLdouble -- ^ @v@ pointing to @2@ elements of type @GLdouble@.
  -> m ()
glVertexAttrib2dvNV v1 v2 = liftIO $ dyn828 ptr_glVertexAttrib2dvNV v1 v2

{-# NOINLINE ptr_glVertexAttrib2dvNV #-}
ptr_glVertexAttrib2dvNV :: FunPtr (GLuint -> Ptr GLdouble -> IO ())
ptr_glVertexAttrib2dvNV = unsafePerformIO $ getCommand "glVertexAttrib2dvNV"

-- glVertexAttrib2f ------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>. The vector equivalent of this command is 'glVertexAttrib2fv'.
glVertexAttrib2f
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> m ()
glVertexAttrib2f v1 v2 v3 = liftIO $ dyn221 ptr_glVertexAttrib2f v1 v2 v3

{-# NOINLINE ptr_glVertexAttrib2f #-}
ptr_glVertexAttrib2f :: FunPtr (GLuint -> GLfloat -> GLfloat -> IO ())
ptr_glVertexAttrib2f = unsafePerformIO $ getCommand "glVertexAttrib2f"

-- glVertexAttrib2fARB ---------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib2fvARB'. This command is an alias for 'glVertexAttrib2f'.
glVertexAttrib2fARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> m ()
glVertexAttrib2fARB v1 v2 v3 = liftIO $ dyn221 ptr_glVertexAttrib2fARB v1 v2 v3

{-# NOINLINE ptr_glVertexAttrib2fARB #-}
ptr_glVertexAttrib2fARB :: FunPtr (GLuint -> GLfloat -> GLfloat -> IO ())
ptr_glVertexAttrib2fARB = unsafePerformIO $ getCommand "glVertexAttrib2fARB"

-- glVertexAttrib2fNV ----------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib2fvNV'. This command is an alias for 'glVertexAttrib2f'.
glVertexAttrib2fNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> m ()
glVertexAttrib2fNV v1 v2 v3 = liftIO $ dyn221 ptr_glVertexAttrib2fNV v1 v2 v3

{-# NOINLINE ptr_glVertexAttrib2fNV #-}
ptr_glVertexAttrib2fNV :: FunPtr (GLuint -> GLfloat -> GLfloat -> IO ())
ptr_glVertexAttrib2fNV = unsafePerformIO $ getCommand "glVertexAttrib2fNV"

-- glVertexAttrib2fv -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttrib2fv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLfloat -- ^ @v@ pointing to @2@ elements of type @GLfloat@.
  -> m ()
glVertexAttrib2fv v1 v2 = liftIO $ dyn377 ptr_glVertexAttrib2fv v1 v2

{-# NOINLINE ptr_glVertexAttrib2fv #-}
ptr_glVertexAttrib2fv :: FunPtr (GLuint -> Ptr GLfloat -> IO ())
ptr_glVertexAttrib2fv = unsafePerformIO $ getCommand "glVertexAttrib2fv"

-- glVertexAttrib2fvARB --------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib2fv'.
glVertexAttrib2fvARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLfloat -- ^ @v@ pointing to @2@ elements of type @GLfloat@.
  -> m ()
glVertexAttrib2fvARB v1 v2 = liftIO $ dyn377 ptr_glVertexAttrib2fvARB v1 v2

{-# NOINLINE ptr_glVertexAttrib2fvARB #-}
ptr_glVertexAttrib2fvARB :: FunPtr (GLuint -> Ptr GLfloat -> IO ())
ptr_glVertexAttrib2fvARB = unsafePerformIO $ getCommand "glVertexAttrib2fvARB"

-- glVertexAttrib2fvNV ---------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib2fv'.
glVertexAttrib2fvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLfloat -- ^ @v@ pointing to @2@ elements of type @GLfloat@.
  -> m ()
glVertexAttrib2fvNV v1 v2 = liftIO $ dyn377 ptr_glVertexAttrib2fvNV v1 v2

{-# NOINLINE ptr_glVertexAttrib2fvNV #-}
ptr_glVertexAttrib2fvNV :: FunPtr (GLuint -> Ptr GLfloat -> IO ())
ptr_glVertexAttrib2fvNV = unsafePerformIO $ getCommand "glVertexAttrib2fvNV"

-- glVertexAttrib2hNV ----------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib2hvNV'.
glVertexAttrib2hNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLhalfNV -- ^ @x@ of type @Half16NV@.
  -> GLhalfNV -- ^ @y@ of type @Half16NV@.
  -> m ()
glVertexAttrib2hNV v1 v2 v3 = liftIO $ dyn846 ptr_glVertexAttrib2hNV v1 v2 v3

{-# NOINLINE ptr_glVertexAttrib2hNV #-}
ptr_glVertexAttrib2hNV :: FunPtr (GLuint -> GLhalfNV -> GLhalfNV -> IO ())
ptr_glVertexAttrib2hNV = unsafePerformIO $ getCommand "glVertexAttrib2hNV"

-- glVertexAttrib2hvNV ---------------------------------------------------------

glVertexAttrib2hvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLhalfNV -- ^ @v@ pointing to @2@ elements of type @Half16NV@.
  -> m ()
glVertexAttrib2hvNV v1 v2 = liftIO $ dyn844 ptr_glVertexAttrib2hvNV v1 v2

{-# NOINLINE ptr_glVertexAttrib2hvNV #-}
ptr_glVertexAttrib2hvNV :: FunPtr (GLuint -> Ptr GLhalfNV -> IO ())
ptr_glVertexAttrib2hvNV = unsafePerformIO $ getCommand "glVertexAttrib2hvNV"

-- glVertexAttrib2s ------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>. The vector equivalent of this command is 'glVertexAttrib2sv'.
glVertexAttrib2s
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLshort -- ^ @x@.
  -> GLshort -- ^ @y@.
  -> m ()
glVertexAttrib2s v1 v2 v3 = liftIO $ dyn847 ptr_glVertexAttrib2s v1 v2 v3

{-# NOINLINE ptr_glVertexAttrib2s #-}
ptr_glVertexAttrib2s :: FunPtr (GLuint -> GLshort -> GLshort -> IO ())
ptr_glVertexAttrib2s = unsafePerformIO $ getCommand "glVertexAttrib2s"

-- glVertexAttrib2sARB ---------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib2svARB'. This command is an alias for 'glVertexAttrib2s'.
glVertexAttrib2sARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLshort -- ^ @x@.
  -> GLshort -- ^ @y@.
  -> m ()
glVertexAttrib2sARB v1 v2 v3 = liftIO $ dyn847 ptr_glVertexAttrib2sARB v1 v2 v3

{-# NOINLINE ptr_glVertexAttrib2sARB #-}
ptr_glVertexAttrib2sARB :: FunPtr (GLuint -> GLshort -> GLshort -> IO ())
ptr_glVertexAttrib2sARB = unsafePerformIO $ getCommand "glVertexAttrib2sARB"

-- glVertexAttrib2sNV ----------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib2svNV'. This command is an alias for 'glVertexAttrib2s'.
glVertexAttrib2sNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLshort -- ^ @x@.
  -> GLshort -- ^ @y@.
  -> m ()
glVertexAttrib2sNV v1 v2 v3 = liftIO $ dyn847 ptr_glVertexAttrib2sNV v1 v2 v3

{-# NOINLINE ptr_glVertexAttrib2sNV #-}
ptr_glVertexAttrib2sNV :: FunPtr (GLuint -> GLshort -> GLshort -> IO ())
ptr_glVertexAttrib2sNV = unsafePerformIO $ getCommand "glVertexAttrib2sNV"

-- glVertexAttrib2sv -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttrib2sv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLshort -- ^ @v@ pointing to @2@ elements of type @GLshort@.
  -> m ()
glVertexAttrib2sv v1 v2 = liftIO $ dyn829 ptr_glVertexAttrib2sv v1 v2

{-# NOINLINE ptr_glVertexAttrib2sv #-}
ptr_glVertexAttrib2sv :: FunPtr (GLuint -> Ptr GLshort -> IO ())
ptr_glVertexAttrib2sv = unsafePerformIO $ getCommand "glVertexAttrib2sv"

-- glVertexAttrib2svARB --------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib2sv'.
glVertexAttrib2svARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLshort -- ^ @v@ pointing to @2@ elements of type @GLshort@.
  -> m ()
glVertexAttrib2svARB v1 v2 = liftIO $ dyn829 ptr_glVertexAttrib2svARB v1 v2

{-# NOINLINE ptr_glVertexAttrib2svARB #-}
ptr_glVertexAttrib2svARB :: FunPtr (GLuint -> Ptr GLshort -> IO ())
ptr_glVertexAttrib2svARB = unsafePerformIO $ getCommand "glVertexAttrib2svARB"

-- glVertexAttrib2svNV ---------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib2sv'.
glVertexAttrib2svNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLshort -- ^ @v@ pointing to @2@ elements of type @GLshort@.
  -> m ()
glVertexAttrib2svNV v1 v2 = liftIO $ dyn829 ptr_glVertexAttrib2svNV v1 v2

{-# NOINLINE ptr_glVertexAttrib2svNV #-}
ptr_glVertexAttrib2svNV :: FunPtr (GLuint -> Ptr GLshort -> IO ())
ptr_glVertexAttrib2svNV = unsafePerformIO $ getCommand "glVertexAttrib2svNV"

-- glVertexAttrib3d ------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>. The vector equivalent of this command is 'glVertexAttrib3dv'.
glVertexAttrib3d
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> GLdouble -- ^ @z@.
  -> m ()
glVertexAttrib3d v1 v2 v3 v4 = liftIO $ dyn848 ptr_glVertexAttrib3d v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttrib3d #-}
ptr_glVertexAttrib3d :: FunPtr (GLuint -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glVertexAttrib3d = unsafePerformIO $ getCommand "glVertexAttrib3d"

-- glVertexAttrib3dARB ---------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib3dvARB'. This command is an alias for 'glVertexAttrib3d'.
glVertexAttrib3dARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> GLdouble -- ^ @z@.
  -> m ()
glVertexAttrib3dARB v1 v2 v3 v4 = liftIO $ dyn848 ptr_glVertexAttrib3dARB v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttrib3dARB #-}
ptr_glVertexAttrib3dARB :: FunPtr (GLuint -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glVertexAttrib3dARB = unsafePerformIO $ getCommand "glVertexAttrib3dARB"

-- glVertexAttrib3dNV ----------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib3dvNV'. This command is an alias for 'glVertexAttrib3d'.
glVertexAttrib3dNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> GLdouble -- ^ @z@.
  -> m ()
glVertexAttrib3dNV v1 v2 v3 v4 = liftIO $ dyn848 ptr_glVertexAttrib3dNV v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttrib3dNV #-}
ptr_glVertexAttrib3dNV :: FunPtr (GLuint -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glVertexAttrib3dNV = unsafePerformIO $ getCommand "glVertexAttrib3dNV"

-- glVertexAttrib3dv -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttrib3dv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLdouble -- ^ @v@ pointing to @3@ elements of type @GLdouble@.
  -> m ()
glVertexAttrib3dv v1 v2 = liftIO $ dyn828 ptr_glVertexAttrib3dv v1 v2

{-# NOINLINE ptr_glVertexAttrib3dv #-}
ptr_glVertexAttrib3dv :: FunPtr (GLuint -> Ptr GLdouble -> IO ())
ptr_glVertexAttrib3dv = unsafePerformIO $ getCommand "glVertexAttrib3dv"

-- glVertexAttrib3dvARB --------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib3dv'.
glVertexAttrib3dvARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLdouble -- ^ @v@ pointing to @3@ elements of type @GLdouble@.
  -> m ()
glVertexAttrib3dvARB v1 v2 = liftIO $ dyn828 ptr_glVertexAttrib3dvARB v1 v2

{-# NOINLINE ptr_glVertexAttrib3dvARB #-}
ptr_glVertexAttrib3dvARB :: FunPtr (GLuint -> Ptr GLdouble -> IO ())
ptr_glVertexAttrib3dvARB = unsafePerformIO $ getCommand "glVertexAttrib3dvARB"

-- glVertexAttrib3dvNV ---------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib3dv'.
glVertexAttrib3dvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLdouble -- ^ @v@ pointing to @3@ elements of type @GLdouble@.
  -> m ()
glVertexAttrib3dvNV v1 v2 = liftIO $ dyn828 ptr_glVertexAttrib3dvNV v1 v2

{-# NOINLINE ptr_glVertexAttrib3dvNV #-}
ptr_glVertexAttrib3dvNV :: FunPtr (GLuint -> Ptr GLdouble -> IO ())
ptr_glVertexAttrib3dvNV = unsafePerformIO $ getCommand "glVertexAttrib3dvNV"

-- glVertexAttrib3f ------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>. The vector equivalent of this command is 'glVertexAttrib3fv'.
glVertexAttrib3f
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> m ()
glVertexAttrib3f v1 v2 v3 v4 = liftIO $ dyn692 ptr_glVertexAttrib3f v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttrib3f #-}
ptr_glVertexAttrib3f :: FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glVertexAttrib3f = unsafePerformIO $ getCommand "glVertexAttrib3f"

-- glVertexAttrib3fARB ---------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib3fvARB'. This command is an alias for 'glVertexAttrib3f'.
glVertexAttrib3fARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> m ()
glVertexAttrib3fARB v1 v2 v3 v4 = liftIO $ dyn692 ptr_glVertexAttrib3fARB v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttrib3fARB #-}
ptr_glVertexAttrib3fARB :: FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glVertexAttrib3fARB = unsafePerformIO $ getCommand "glVertexAttrib3fARB"

-- glVertexAttrib3fNV ----------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib3fvNV'. This command is an alias for 'glVertexAttrib3f'.
glVertexAttrib3fNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> m ()
glVertexAttrib3fNV v1 v2 v3 v4 = liftIO $ dyn692 ptr_glVertexAttrib3fNV v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttrib3fNV #-}
ptr_glVertexAttrib3fNV :: FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glVertexAttrib3fNV = unsafePerformIO $ getCommand "glVertexAttrib3fNV"

-- glVertexAttrib3fv -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttrib3fv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @GLfloat@.
  -> m ()
glVertexAttrib3fv v1 v2 = liftIO $ dyn377 ptr_glVertexAttrib3fv v1 v2

{-# NOINLINE ptr_glVertexAttrib3fv #-}
ptr_glVertexAttrib3fv :: FunPtr (GLuint -> Ptr GLfloat -> IO ())
ptr_glVertexAttrib3fv = unsafePerformIO $ getCommand "glVertexAttrib3fv"

-- glVertexAttrib3fvARB --------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib3fv'.
glVertexAttrib3fvARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @GLfloat@.
  -> m ()
glVertexAttrib3fvARB v1 v2 = liftIO $ dyn377 ptr_glVertexAttrib3fvARB v1 v2

{-# NOINLINE ptr_glVertexAttrib3fvARB #-}
ptr_glVertexAttrib3fvARB :: FunPtr (GLuint -> Ptr GLfloat -> IO ())
ptr_glVertexAttrib3fvARB = unsafePerformIO $ getCommand "glVertexAttrib3fvARB"

-- glVertexAttrib3fvNV ---------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib3fv'.
glVertexAttrib3fvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @GLfloat@.
  -> m ()
glVertexAttrib3fvNV v1 v2 = liftIO $ dyn377 ptr_glVertexAttrib3fvNV v1 v2

{-# NOINLINE ptr_glVertexAttrib3fvNV #-}
ptr_glVertexAttrib3fvNV :: FunPtr (GLuint -> Ptr GLfloat -> IO ())
ptr_glVertexAttrib3fvNV = unsafePerformIO $ getCommand "glVertexAttrib3fvNV"

-- glVertexAttrib3hNV ----------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib3hvNV'.
glVertexAttrib3hNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLhalfNV -- ^ @x@ of type @Half16NV@.
  -> GLhalfNV -- ^ @y@ of type @Half16NV@.
  -> GLhalfNV -- ^ @z@ of type @Half16NV@.
  -> m ()
glVertexAttrib3hNV v1 v2 v3 v4 = liftIO $ dyn849 ptr_glVertexAttrib3hNV v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttrib3hNV #-}
ptr_glVertexAttrib3hNV :: FunPtr (GLuint -> GLhalfNV -> GLhalfNV -> GLhalfNV -> IO ())
ptr_glVertexAttrib3hNV = unsafePerformIO $ getCommand "glVertexAttrib3hNV"

-- glVertexAttrib3hvNV ---------------------------------------------------------

glVertexAttrib3hvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLhalfNV -- ^ @v@ pointing to @3@ elements of type @Half16NV@.
  -> m ()
glVertexAttrib3hvNV v1 v2 = liftIO $ dyn844 ptr_glVertexAttrib3hvNV v1 v2

{-# NOINLINE ptr_glVertexAttrib3hvNV #-}
ptr_glVertexAttrib3hvNV :: FunPtr (GLuint -> Ptr GLhalfNV -> IO ())
ptr_glVertexAttrib3hvNV = unsafePerformIO $ getCommand "glVertexAttrib3hvNV"

-- glVertexAttrib3s ------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>. The vector equivalent of this command is 'glVertexAttrib3sv'.
glVertexAttrib3s
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLshort -- ^ @x@.
  -> GLshort -- ^ @y@.
  -> GLshort -- ^ @z@.
  -> m ()
glVertexAttrib3s v1 v2 v3 v4 = liftIO $ dyn850 ptr_glVertexAttrib3s v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttrib3s #-}
ptr_glVertexAttrib3s :: FunPtr (GLuint -> GLshort -> GLshort -> GLshort -> IO ())
ptr_glVertexAttrib3s = unsafePerformIO $ getCommand "glVertexAttrib3s"

-- glVertexAttrib3sARB ---------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib3svARB'. This command is an alias for 'glVertexAttrib3s'.
glVertexAttrib3sARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLshort -- ^ @x@.
  -> GLshort -- ^ @y@.
  -> GLshort -- ^ @z@.
  -> m ()
glVertexAttrib3sARB v1 v2 v3 v4 = liftIO $ dyn850 ptr_glVertexAttrib3sARB v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttrib3sARB #-}
ptr_glVertexAttrib3sARB :: FunPtr (GLuint -> GLshort -> GLshort -> GLshort -> IO ())
ptr_glVertexAttrib3sARB = unsafePerformIO $ getCommand "glVertexAttrib3sARB"

-- glVertexAttrib3sNV ----------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib3svNV'. This command is an alias for 'glVertexAttrib3s'.
glVertexAttrib3sNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLshort -- ^ @x@.
  -> GLshort -- ^ @y@.
  -> GLshort -- ^ @z@.
  -> m ()
glVertexAttrib3sNV v1 v2 v3 v4 = liftIO $ dyn850 ptr_glVertexAttrib3sNV v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttrib3sNV #-}
ptr_glVertexAttrib3sNV :: FunPtr (GLuint -> GLshort -> GLshort -> GLshort -> IO ())
ptr_glVertexAttrib3sNV = unsafePerformIO $ getCommand "glVertexAttrib3sNV"

-- glVertexAttrib3sv -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttrib3sv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLshort -- ^ @v@ pointing to @3@ elements of type @GLshort@.
  -> m ()
glVertexAttrib3sv v1 v2 = liftIO $ dyn829 ptr_glVertexAttrib3sv v1 v2

{-# NOINLINE ptr_glVertexAttrib3sv #-}
ptr_glVertexAttrib3sv :: FunPtr (GLuint -> Ptr GLshort -> IO ())
ptr_glVertexAttrib3sv = unsafePerformIO $ getCommand "glVertexAttrib3sv"

-- glVertexAttrib3svARB --------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib3sv'.
glVertexAttrib3svARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLshort -- ^ @v@ pointing to @3@ elements of type @GLshort@.
  -> m ()
glVertexAttrib3svARB v1 v2 = liftIO $ dyn829 ptr_glVertexAttrib3svARB v1 v2

{-# NOINLINE ptr_glVertexAttrib3svARB #-}
ptr_glVertexAttrib3svARB :: FunPtr (GLuint -> Ptr GLshort -> IO ())
ptr_glVertexAttrib3svARB = unsafePerformIO $ getCommand "glVertexAttrib3svARB"

-- glVertexAttrib3svNV ---------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib3sv'.
glVertexAttrib3svNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLshort -- ^ @v@ pointing to @3@ elements of type @GLshort@.
  -> m ()
glVertexAttrib3svNV v1 v2 = liftIO $ dyn829 ptr_glVertexAttrib3svNV v1 v2

{-# NOINLINE ptr_glVertexAttrib3svNV #-}
ptr_glVertexAttrib3svNV :: FunPtr (GLuint -> Ptr GLshort -> IO ())
ptr_glVertexAttrib3svNV = unsafePerformIO $ getCommand "glVertexAttrib3svNV"

-- glVertexAttrib4Nbv ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttrib4Nbv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLbyte -- ^ @v@ pointing to @4@ elements of type @GLbyte@.
  -> m ()
glVertexAttrib4Nbv v1 v2 = liftIO $ dyn827 ptr_glVertexAttrib4Nbv v1 v2

{-# NOINLINE ptr_glVertexAttrib4Nbv #-}
ptr_glVertexAttrib4Nbv :: FunPtr (GLuint -> Ptr GLbyte -> IO ())
ptr_glVertexAttrib4Nbv = unsafePerformIO $ getCommand "glVertexAttrib4Nbv"

-- glVertexAttrib4NbvARB -------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib4Nbv'.
glVertexAttrib4NbvARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLbyte -- ^ @v@ pointing to @4@ elements of type @GLbyte@.
  -> m ()
glVertexAttrib4NbvARB v1 v2 = liftIO $ dyn827 ptr_glVertexAttrib4NbvARB v1 v2

{-# NOINLINE ptr_glVertexAttrib4NbvARB #-}
ptr_glVertexAttrib4NbvARB :: FunPtr (GLuint -> Ptr GLbyte -> IO ())
ptr_glVertexAttrib4NbvARB = unsafePerformIO $ getCommand "glVertexAttrib4NbvARB"

-- glVertexAttrib4Niv ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttrib4Niv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLint -- ^ @v@ pointing to @4@ elements of type @GLint@.
  -> m ()
glVertexAttrib4Niv v1 v2 = liftIO $ dyn701 ptr_glVertexAttrib4Niv v1 v2

{-# NOINLINE ptr_glVertexAttrib4Niv #-}
ptr_glVertexAttrib4Niv :: FunPtr (GLuint -> Ptr GLint -> IO ())
ptr_glVertexAttrib4Niv = unsafePerformIO $ getCommand "glVertexAttrib4Niv"

