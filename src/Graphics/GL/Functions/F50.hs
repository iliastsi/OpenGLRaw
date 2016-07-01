--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F50
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

module Graphics.GL.Functions.F50 (
  glProgramUniform2i,
  glProgramUniform2i64ARB,
  glProgramUniform2i64NV,
  glProgramUniform2i64vARB,
  glProgramUniform2i64vNV,
  glProgramUniform2iEXT,
  glProgramUniform2iv,
  glProgramUniform2ivEXT,
  glProgramUniform2ui,
  glProgramUniform2ui64ARB,
  glProgramUniform2ui64NV,
  glProgramUniform2ui64vARB,
  glProgramUniform2ui64vNV,
  glProgramUniform2uiEXT,
  glProgramUniform2uiv,
  glProgramUniform2uivEXT,
  glProgramUniform3d,
  glProgramUniform3dEXT,
  glProgramUniform3dv,
  glProgramUniform3dvEXT,
  glProgramUniform3f,
  glProgramUniform3fEXT,
  glProgramUniform3fv,
  glProgramUniform3fvEXT,
  glProgramUniform3i,
  glProgramUniform3i64ARB,
  glProgramUniform3i64NV,
  glProgramUniform3i64vARB,
  glProgramUniform3i64vNV,
  glProgramUniform3iEXT,
  glProgramUniform3iv,
  glProgramUniform3ivEXT,
  glProgramUniform3ui,
  glProgramUniform3ui64ARB,
  glProgramUniform3ui64NV,
  glProgramUniform3ui64vARB,
  glProgramUniform3ui64vNV,
  glProgramUniform3uiEXT,
  glProgramUniform3uiv,
  glProgramUniform3uivEXT
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

-- glProgramUniform2i ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniform2i
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLint -- ^ @v0@.
  -> GLint -- ^ @v1@.
  -> m ()
glProgramUniform2i v1 v2 v3 v4 = liftIO $ dyn643 ptr_glProgramUniform2i v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform2i #-}
ptr_glProgramUniform2i :: FunPtr (GLuint -> GLint -> GLint -> GLint -> IO ())
ptr_glProgramUniform2i = unsafePerformIO $ getCommand "glProgramUniform2i"

-- glProgramUniform2i64ARB -----------------------------------------------------

glProgramUniform2i64ARB
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLint64 -- ^ @x@.
  -> GLint64 -- ^ @y@.
  -> m ()
glProgramUniform2i64ARB v1 v2 v3 v4 = liftIO $ dyn644 ptr_glProgramUniform2i64ARB v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform2i64ARB #-}
ptr_glProgramUniform2i64ARB :: FunPtr (GLuint -> GLint -> GLint64 -> GLint64 -> IO ())
ptr_glProgramUniform2i64ARB = unsafePerformIO $ getCommand "glProgramUniform2i64ARB"

-- glProgramUniform2i64NV ------------------------------------------------------

glProgramUniform2i64NV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLint64EXT -- ^ @x@.
  -> GLint64EXT -- ^ @y@.
  -> m ()
glProgramUniform2i64NV v1 v2 v3 v4 = liftIO $ dyn645 ptr_glProgramUniform2i64NV v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform2i64NV #-}
ptr_glProgramUniform2i64NV :: FunPtr (GLuint -> GLint -> GLint64EXT -> GLint64EXT -> IO ())
ptr_glProgramUniform2i64NV = unsafePerformIO $ getCommand "glProgramUniform2i64NV"

-- glProgramUniform2i64vARB ----------------------------------------------------

glProgramUniform2i64vARB
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint64 -- ^ @value@ pointing to @count*2@ elements of type @GLint64@.
  -> m ()
glProgramUniform2i64vARB v1 v2 v3 v4 = liftIO $ dyn458 ptr_glProgramUniform2i64vARB v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform2i64vARB #-}
ptr_glProgramUniform2i64vARB :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLint64 -> IO ())
ptr_glProgramUniform2i64vARB = unsafePerformIO $ getCommand "glProgramUniform2i64vARB"

-- glProgramUniform2i64vNV -----------------------------------------------------

glProgramUniform2i64vNV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint64EXT -- ^ @value@ pointing to @count*2@ elements of type @GLint64EXT@.
  -> m ()
glProgramUniform2i64vNV v1 v2 v3 v4 = liftIO $ dyn636 ptr_glProgramUniform2i64vNV v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform2i64vNV #-}
ptr_glProgramUniform2i64vNV :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLint64EXT -> IO ())
ptr_glProgramUniform2i64vNV = unsafePerformIO $ getCommand "glProgramUniform2i64vNV"

-- glProgramUniform2iEXT -------------------------------------------------------

-- | This command is an alias for 'glProgramUniform2i'.
glProgramUniform2iEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLint -- ^ @v0@.
  -> GLint -- ^ @v1@.
  -> m ()
glProgramUniform2iEXT v1 v2 v3 v4 = liftIO $ dyn643 ptr_glProgramUniform2iEXT v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform2iEXT #-}
ptr_glProgramUniform2iEXT :: FunPtr (GLuint -> GLint -> GLint -> GLint -> IO ())
ptr_glProgramUniform2iEXT = unsafePerformIO $ getCommand "glProgramUniform2iEXT"

-- glProgramUniform2iv ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniform2iv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint -- ^ @value@ pointing to @2@ elements of type @GLint@.
  -> m ()
glProgramUniform2iv v1 v2 v3 v4 = liftIO $ dyn459 ptr_glProgramUniform2iv v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform2iv #-}
ptr_glProgramUniform2iv :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLint -> IO ())
ptr_glProgramUniform2iv = unsafePerformIO $ getCommand "glProgramUniform2iv"

-- glProgramUniform2ivEXT ------------------------------------------------------

-- | This command is an alias for 'glProgramUniform2iv'.
glProgramUniform2ivEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint -- ^ @value@ pointing to @count*2@ elements of type @GLint@.
  -> m ()
glProgramUniform2ivEXT v1 v2 v3 v4 = liftIO $ dyn459 ptr_glProgramUniform2ivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform2ivEXT #-}
ptr_glProgramUniform2ivEXT :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLint -> IO ())
ptr_glProgramUniform2ivEXT = unsafePerformIO $ getCommand "glProgramUniform2ivEXT"

-- glProgramUniform2ui ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniform2ui
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLuint -- ^ @v0@.
  -> GLuint -- ^ @v1@.
  -> m ()
glProgramUniform2ui v1 v2 v3 v4 = liftIO $ dyn646 ptr_glProgramUniform2ui v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform2ui #-}
ptr_glProgramUniform2ui :: FunPtr (GLuint -> GLint -> GLuint -> GLuint -> IO ())
ptr_glProgramUniform2ui = unsafePerformIO $ getCommand "glProgramUniform2ui"

-- glProgramUniform2ui64ARB ----------------------------------------------------

glProgramUniform2ui64ARB
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLuint64 -- ^ @x@.
  -> GLuint64 -- ^ @y@.
  -> m ()
glProgramUniform2ui64ARB v1 v2 v3 v4 = liftIO $ dyn647 ptr_glProgramUniform2ui64ARB v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform2ui64ARB #-}
ptr_glProgramUniform2ui64ARB :: FunPtr (GLuint -> GLint -> GLuint64 -> GLuint64 -> IO ())
ptr_glProgramUniform2ui64ARB = unsafePerformIO $ getCommand "glProgramUniform2ui64ARB"

-- glProgramUniform2ui64NV -----------------------------------------------------

glProgramUniform2ui64NV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLuint64EXT -- ^ @x@.
  -> GLuint64EXT -- ^ @y@.
  -> m ()
glProgramUniform2ui64NV v1 v2 v3 v4 = liftIO $ dyn648 ptr_glProgramUniform2ui64NV v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform2ui64NV #-}
ptr_glProgramUniform2ui64NV :: FunPtr (GLuint -> GLint -> GLuint64EXT -> GLuint64EXT -> IO ())
ptr_glProgramUniform2ui64NV = unsafePerformIO $ getCommand "glProgramUniform2ui64NV"

-- glProgramUniform2ui64vARB ---------------------------------------------------

glProgramUniform2ui64vARB
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint64 -- ^ @value@ pointing to @count*2@ elements of type @GLuint64@.
  -> m ()
glProgramUniform2ui64vARB v1 v2 v3 v4 = liftIO $ dyn460 ptr_glProgramUniform2ui64vARB v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform2ui64vARB #-}
ptr_glProgramUniform2ui64vARB :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint64 -> IO ())
ptr_glProgramUniform2ui64vARB = unsafePerformIO $ getCommand "glProgramUniform2ui64vARB"

-- glProgramUniform2ui64vNV ----------------------------------------------------

glProgramUniform2ui64vNV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint64EXT -- ^ @value@ pointing to @count*2@ elements of type @GLuint64EXT@.
  -> m ()
glProgramUniform2ui64vNV v1 v2 v3 v4 = liftIO $ dyn640 ptr_glProgramUniform2ui64vNV v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform2ui64vNV #-}
ptr_glProgramUniform2ui64vNV :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint64EXT -> IO ())
ptr_glProgramUniform2ui64vNV = unsafePerformIO $ getCommand "glProgramUniform2ui64vNV"

-- glProgramUniform2uiEXT ------------------------------------------------------

-- | This command is an alias for 'glProgramUniform2ui'.
glProgramUniform2uiEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLuint -- ^ @v0@.
  -> GLuint -- ^ @v1@.
  -> m ()
glProgramUniform2uiEXT v1 v2 v3 v4 = liftIO $ dyn646 ptr_glProgramUniform2uiEXT v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform2uiEXT #-}
ptr_glProgramUniform2uiEXT :: FunPtr (GLuint -> GLint -> GLuint -> GLuint -> IO ())
ptr_glProgramUniform2uiEXT = unsafePerformIO $ getCommand "glProgramUniform2uiEXT"

-- glProgramUniform2uiv --------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniform2uiv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @value@ pointing to @2@ elements of type @GLuint@.
  -> m ()
glProgramUniform2uiv v1 v2 v3 v4 = liftIO $ dyn461 ptr_glProgramUniform2uiv v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform2uiv #-}
ptr_glProgramUniform2uiv :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint -> IO ())
ptr_glProgramUniform2uiv = unsafePerformIO $ getCommand "glProgramUniform2uiv"

-- glProgramUniform2uivEXT -----------------------------------------------------

-- | This command is an alias for 'glProgramUniform2uiv'.
glProgramUniform2uivEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @value@ pointing to @count*2@ elements of type @GLuint@.
  -> m ()
glProgramUniform2uivEXT v1 v2 v3 v4 = liftIO $ dyn461 ptr_glProgramUniform2uivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform2uivEXT #-}
ptr_glProgramUniform2uivEXT :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint -> IO ())
ptr_glProgramUniform2uivEXT = unsafePerformIO $ getCommand "glProgramUniform2uivEXT"

-- glProgramUniform3d ----------------------------------------------------------

glProgramUniform3d
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLdouble -- ^ @v0@.
  -> GLdouble -- ^ @v1@.
  -> GLdouble -- ^ @v2@.
  -> m ()
glProgramUniform3d v1 v2 v3 v4 v5 = liftIO $ dyn649 ptr_glProgramUniform3d v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniform3d #-}
ptr_glProgramUniform3d :: FunPtr (GLuint -> GLint -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glProgramUniform3d = unsafePerformIO $ getCommand "glProgramUniform3d"

-- glProgramUniform3dEXT -------------------------------------------------------

glProgramUniform3dEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> GLdouble -- ^ @z@.
  -> m ()
glProgramUniform3dEXT v1 v2 v3 v4 v5 = liftIO $ dyn649 ptr_glProgramUniform3dEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniform3dEXT #-}
ptr_glProgramUniform3dEXT :: FunPtr (GLuint -> GLint -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glProgramUniform3dEXT = unsafePerformIO $ getCommand "glProgramUniform3dEXT"

-- glProgramUniform3dv ---------------------------------------------------------

glProgramUniform3dv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLdouble -- ^ @value@ pointing to @3@ elements of type @GLdouble@.
  -> m ()
glProgramUniform3dv v1 v2 v3 v4 = liftIO $ dyn456 ptr_glProgramUniform3dv v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform3dv #-}
ptr_glProgramUniform3dv :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLdouble -> IO ())
ptr_glProgramUniform3dv = unsafePerformIO $ getCommand "glProgramUniform3dv"

-- glProgramUniform3dvEXT ------------------------------------------------------

glProgramUniform3dvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLdouble -- ^ @value@ pointing to @count@ elements of type @GLdouble@.
  -> m ()
glProgramUniform3dvEXT v1 v2 v3 v4 = liftIO $ dyn456 ptr_glProgramUniform3dvEXT v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform3dvEXT #-}
ptr_glProgramUniform3dvEXT :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLdouble -> IO ())
ptr_glProgramUniform3dvEXT = unsafePerformIO $ getCommand "glProgramUniform3dvEXT"

-- glProgramUniform3f ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniform3f
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLfloat -- ^ @v0@.
  -> GLfloat -- ^ @v1@.
  -> GLfloat -- ^ @v2@.
  -> m ()
glProgramUniform3f v1 v2 v3 v4 v5 = liftIO $ dyn650 ptr_glProgramUniform3f v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniform3f #-}
ptr_glProgramUniform3f :: FunPtr (GLuint -> GLint -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glProgramUniform3f = unsafePerformIO $ getCommand "glProgramUniform3f"

-- glProgramUniform3fEXT -------------------------------------------------------

-- | This command is an alias for 'glProgramUniform3f'.
glProgramUniform3fEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLfloat -- ^ @v0@.
  -> GLfloat -- ^ @v1@.
  -> GLfloat -- ^ @v2@.
  -> m ()
glProgramUniform3fEXT v1 v2 v3 v4 v5 = liftIO $ dyn650 ptr_glProgramUniform3fEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniform3fEXT #-}
ptr_glProgramUniform3fEXT :: FunPtr (GLuint -> GLint -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glProgramUniform3fEXT = unsafePerformIO $ getCommand "glProgramUniform3fEXT"

-- glProgramUniform3fv ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniform3fv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @value@ pointing to @3@ elements of type @GLfloat@.
  -> m ()
glProgramUniform3fv v1 v2 v3 v4 = liftIO $ dyn457 ptr_glProgramUniform3fv v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform3fv #-}
ptr_glProgramUniform3fv :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glProgramUniform3fv = unsafePerformIO $ getCommand "glProgramUniform3fv"

-- glProgramUniform3fvEXT ------------------------------------------------------

-- | This command is an alias for 'glProgramUniform3fv'.
glProgramUniform3fvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @value@ pointing to @count*3@ elements of type @GLfloat@.
  -> m ()
glProgramUniform3fvEXT v1 v2 v3 v4 = liftIO $ dyn457 ptr_glProgramUniform3fvEXT v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform3fvEXT #-}
ptr_glProgramUniform3fvEXT :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glProgramUniform3fvEXT = unsafePerformIO $ getCommand "glProgramUniform3fvEXT"

-- glProgramUniform3i ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniform3i
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLint -- ^ @v0@.
  -> GLint -- ^ @v1@.
  -> GLint -- ^ @v2@.
  -> m ()
glProgramUniform3i v1 v2 v3 v4 v5 = liftIO $ dyn651 ptr_glProgramUniform3i v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniform3i #-}
ptr_glProgramUniform3i :: FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> IO ())
ptr_glProgramUniform3i = unsafePerformIO $ getCommand "glProgramUniform3i"

-- glProgramUniform3i64ARB -----------------------------------------------------

glProgramUniform3i64ARB
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLint64 -- ^ @x@.
  -> GLint64 -- ^ @y@.
  -> GLint64 -- ^ @z@.
  -> m ()
glProgramUniform3i64ARB v1 v2 v3 v4 v5 = liftIO $ dyn652 ptr_glProgramUniform3i64ARB v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniform3i64ARB #-}
ptr_glProgramUniform3i64ARB :: FunPtr (GLuint -> GLint -> GLint64 -> GLint64 -> GLint64 -> IO ())
ptr_glProgramUniform3i64ARB = unsafePerformIO $ getCommand "glProgramUniform3i64ARB"

-- glProgramUniform3i64NV ------------------------------------------------------

glProgramUniform3i64NV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLint64EXT -- ^ @x@.
  -> GLint64EXT -- ^ @y@.
  -> GLint64EXT -- ^ @z@.
  -> m ()
glProgramUniform3i64NV v1 v2 v3 v4 v5 = liftIO $ dyn653 ptr_glProgramUniform3i64NV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniform3i64NV #-}
ptr_glProgramUniform3i64NV :: FunPtr (GLuint -> GLint -> GLint64EXT -> GLint64EXT -> GLint64EXT -> IO ())
ptr_glProgramUniform3i64NV = unsafePerformIO $ getCommand "glProgramUniform3i64NV"

-- glProgramUniform3i64vARB ----------------------------------------------------

glProgramUniform3i64vARB
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint64 -- ^ @value@ pointing to @count*3@ elements of type @GLint64@.
  -> m ()
glProgramUniform3i64vARB v1 v2 v3 v4 = liftIO $ dyn458 ptr_glProgramUniform3i64vARB v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform3i64vARB #-}
ptr_glProgramUniform3i64vARB :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLint64 -> IO ())
ptr_glProgramUniform3i64vARB = unsafePerformIO $ getCommand "glProgramUniform3i64vARB"

-- glProgramUniform3i64vNV -----------------------------------------------------

glProgramUniform3i64vNV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint64EXT -- ^ @value@ pointing to @count*3@ elements of type @GLint64EXT@.
  -> m ()
glProgramUniform3i64vNV v1 v2 v3 v4 = liftIO $ dyn636 ptr_glProgramUniform3i64vNV v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform3i64vNV #-}
ptr_glProgramUniform3i64vNV :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLint64EXT -> IO ())
ptr_glProgramUniform3i64vNV = unsafePerformIO $ getCommand "glProgramUniform3i64vNV"

-- glProgramUniform3iEXT -------------------------------------------------------

-- | This command is an alias for 'glProgramUniform3i'.
glProgramUniform3iEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLint -- ^ @v0@.
  -> GLint -- ^ @v1@.
  -> GLint -- ^ @v2@.
  -> m ()
glProgramUniform3iEXT v1 v2 v3 v4 v5 = liftIO $ dyn651 ptr_glProgramUniform3iEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniform3iEXT #-}
ptr_glProgramUniform3iEXT :: FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> IO ())
ptr_glProgramUniform3iEXT = unsafePerformIO $ getCommand "glProgramUniform3iEXT"

-- glProgramUniform3iv ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniform3iv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint -- ^ @value@ pointing to @3@ elements of type @GLint@.
  -> m ()
glProgramUniform3iv v1 v2 v3 v4 = liftIO $ dyn459 ptr_glProgramUniform3iv v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform3iv #-}
ptr_glProgramUniform3iv :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLint -> IO ())
ptr_glProgramUniform3iv = unsafePerformIO $ getCommand "glProgramUniform3iv"

-- glProgramUniform3ivEXT ------------------------------------------------------

-- | This command is an alias for 'glProgramUniform3iv'.
glProgramUniform3ivEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint -- ^ @value@ pointing to @count*3@ elements of type @GLint@.
  -> m ()
glProgramUniform3ivEXT v1 v2 v3 v4 = liftIO $ dyn459 ptr_glProgramUniform3ivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform3ivEXT #-}
ptr_glProgramUniform3ivEXT :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLint -> IO ())
ptr_glProgramUniform3ivEXT = unsafePerformIO $ getCommand "glProgramUniform3ivEXT"

-- glProgramUniform3ui ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniform3ui
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLuint -- ^ @v0@.
  -> GLuint -- ^ @v1@.
  -> GLuint -- ^ @v2@.
  -> m ()
glProgramUniform3ui v1 v2 v3 v4 v5 = liftIO $ dyn654 ptr_glProgramUniform3ui v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniform3ui #-}
ptr_glProgramUniform3ui :: FunPtr (GLuint -> GLint -> GLuint -> GLuint -> GLuint -> IO ())
ptr_glProgramUniform3ui = unsafePerformIO $ getCommand "glProgramUniform3ui"

-- glProgramUniform3ui64ARB ----------------------------------------------------

glProgramUniform3ui64ARB
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLuint64 -- ^ @x@.
  -> GLuint64 -- ^ @y@.
  -> GLuint64 -- ^ @z@.
  -> m ()
glProgramUniform3ui64ARB v1 v2 v3 v4 v5 = liftIO $ dyn655 ptr_glProgramUniform3ui64ARB v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniform3ui64ARB #-}
ptr_glProgramUniform3ui64ARB :: FunPtr (GLuint -> GLint -> GLuint64 -> GLuint64 -> GLuint64 -> IO ())
ptr_glProgramUniform3ui64ARB = unsafePerformIO $ getCommand "glProgramUniform3ui64ARB"

-- glProgramUniform3ui64NV -----------------------------------------------------

glProgramUniform3ui64NV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLuint64EXT -- ^ @x@.
  -> GLuint64EXT -- ^ @y@.
  -> GLuint64EXT -- ^ @z@.
  -> m ()
glProgramUniform3ui64NV v1 v2 v3 v4 v5 = liftIO $ dyn656 ptr_glProgramUniform3ui64NV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniform3ui64NV #-}
ptr_glProgramUniform3ui64NV :: FunPtr (GLuint -> GLint -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> IO ())
ptr_glProgramUniform3ui64NV = unsafePerformIO $ getCommand "glProgramUniform3ui64NV"

-- glProgramUniform3ui64vARB ---------------------------------------------------

glProgramUniform3ui64vARB
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint64 -- ^ @value@ pointing to @count*3@ elements of type @GLuint64@.
  -> m ()
glProgramUniform3ui64vARB v1 v2 v3 v4 = liftIO $ dyn460 ptr_glProgramUniform3ui64vARB v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform3ui64vARB #-}
ptr_glProgramUniform3ui64vARB :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint64 -> IO ())
ptr_glProgramUniform3ui64vARB = unsafePerformIO $ getCommand "glProgramUniform3ui64vARB"

-- glProgramUniform3ui64vNV ----------------------------------------------------

glProgramUniform3ui64vNV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint64EXT -- ^ @value@ pointing to @count*3@ elements of type @GLuint64EXT@.
  -> m ()
glProgramUniform3ui64vNV v1 v2 v3 v4 = liftIO $ dyn640 ptr_glProgramUniform3ui64vNV v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform3ui64vNV #-}
ptr_glProgramUniform3ui64vNV :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint64EXT -> IO ())
ptr_glProgramUniform3ui64vNV = unsafePerformIO $ getCommand "glProgramUniform3ui64vNV"

-- glProgramUniform3uiEXT ------------------------------------------------------

-- | This command is an alias for 'glProgramUniform3ui'.
glProgramUniform3uiEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLuint -- ^ @v0@.
  -> GLuint -- ^ @v1@.
  -> GLuint -- ^ @v2@.
  -> m ()
glProgramUniform3uiEXT v1 v2 v3 v4 v5 = liftIO $ dyn654 ptr_glProgramUniform3uiEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniform3uiEXT #-}
ptr_glProgramUniform3uiEXT :: FunPtr (GLuint -> GLint -> GLuint -> GLuint -> GLuint -> IO ())
ptr_glProgramUniform3uiEXT = unsafePerformIO $ getCommand "glProgramUniform3uiEXT"

-- glProgramUniform3uiv --------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniform3uiv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @value@ pointing to @3@ elements of type @GLuint@.
  -> m ()
glProgramUniform3uiv v1 v2 v3 v4 = liftIO $ dyn461 ptr_glProgramUniform3uiv v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform3uiv #-}
ptr_glProgramUniform3uiv :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint -> IO ())
ptr_glProgramUniform3uiv = unsafePerformIO $ getCommand "glProgramUniform3uiv"

-- glProgramUniform3uivEXT -----------------------------------------------------

-- | This command is an alias for 'glProgramUniform3uiv'.
glProgramUniform3uivEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @value@ pointing to @count*3@ elements of type @GLuint@.
  -> m ()
glProgramUniform3uivEXT v1 v2 v3 v4 = liftIO $ dyn461 ptr_glProgramUniform3uivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform3uivEXT #-}
ptr_glProgramUniform3uivEXT :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint -> IO ())
ptr_glProgramUniform3uivEXT = unsafePerformIO $ getCommand "glProgramUniform3uivEXT"

