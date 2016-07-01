--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F51
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

module Graphics.GL.Functions.F51 (
  glProgramUniform4d,
  glProgramUniform4dEXT,
  glProgramUniform4dv,
  glProgramUniform4dvEXT,
  glProgramUniform4f,
  glProgramUniform4fEXT,
  glProgramUniform4fv,
  glProgramUniform4fvEXT,
  glProgramUniform4i,
  glProgramUniform4i64ARB,
  glProgramUniform4i64NV,
  glProgramUniform4i64vARB,
  glProgramUniform4i64vNV,
  glProgramUniform4iEXT,
  glProgramUniform4iv,
  glProgramUniform4ivEXT,
  glProgramUniform4ui,
  glProgramUniform4ui64ARB,
  glProgramUniform4ui64NV,
  glProgramUniform4ui64vARB,
  glProgramUniform4ui64vNV,
  glProgramUniform4uiEXT,
  glProgramUniform4uiv,
  glProgramUniform4uivEXT,
  glProgramUniformHandleui64ARB,
  glProgramUniformHandleui64NV,
  glProgramUniformHandleui64vARB,
  glProgramUniformHandleui64vNV,
  glProgramUniformMatrix2dv,
  glProgramUniformMatrix2dvEXT,
  glProgramUniformMatrix2fv,
  glProgramUniformMatrix2fvEXT,
  glProgramUniformMatrix2x3dv,
  glProgramUniformMatrix2x3dvEXT,
  glProgramUniformMatrix2x3fv,
  glProgramUniformMatrix2x3fvEXT,
  glProgramUniformMatrix2x4dv,
  glProgramUniformMatrix2x4dvEXT,
  glProgramUniformMatrix2x4fv,
  glProgramUniformMatrix2x4fvEXT
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

-- glProgramUniform4d ----------------------------------------------------------

glProgramUniform4d
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLdouble -- ^ @v0@.
  -> GLdouble -- ^ @v1@.
  -> GLdouble -- ^ @v2@.
  -> GLdouble -- ^ @v3@.
  -> m ()
glProgramUniform4d v1 v2 v3 v4 v5 v6 = liftIO $ dyn657 ptr_glProgramUniform4d v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glProgramUniform4d #-}
ptr_glProgramUniform4d :: FunPtr (GLuint -> GLint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glProgramUniform4d = unsafePerformIO $ getCommand "glProgramUniform4d"

-- glProgramUniform4dEXT -------------------------------------------------------

glProgramUniform4dEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> GLdouble -- ^ @z@.
  -> GLdouble -- ^ @w@.
  -> m ()
glProgramUniform4dEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn657 ptr_glProgramUniform4dEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glProgramUniform4dEXT #-}
ptr_glProgramUniform4dEXT :: FunPtr (GLuint -> GLint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glProgramUniform4dEXT = unsafePerformIO $ getCommand "glProgramUniform4dEXT"

-- glProgramUniform4dv ---------------------------------------------------------

glProgramUniform4dv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLdouble -- ^ @value@ pointing to @4@ elements of type @GLdouble@.
  -> m ()
glProgramUniform4dv v1 v2 v3 v4 = liftIO $ dyn456 ptr_glProgramUniform4dv v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform4dv #-}
ptr_glProgramUniform4dv :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLdouble -> IO ())
ptr_glProgramUniform4dv = unsafePerformIO $ getCommand "glProgramUniform4dv"

-- glProgramUniform4dvEXT ------------------------------------------------------

glProgramUniform4dvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLdouble -- ^ @value@ pointing to @count@ elements of type @GLdouble@.
  -> m ()
glProgramUniform4dvEXT v1 v2 v3 v4 = liftIO $ dyn456 ptr_glProgramUniform4dvEXT v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform4dvEXT #-}
ptr_glProgramUniform4dvEXT :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLdouble -> IO ())
ptr_glProgramUniform4dvEXT = unsafePerformIO $ getCommand "glProgramUniform4dvEXT"

-- glProgramUniform4f ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniform4f
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLfloat -- ^ @v0@.
  -> GLfloat -- ^ @v1@.
  -> GLfloat -- ^ @v2@.
  -> GLfloat -- ^ @v3@.
  -> m ()
glProgramUniform4f v1 v2 v3 v4 v5 v6 = liftIO $ dyn658 ptr_glProgramUniform4f v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glProgramUniform4f #-}
ptr_glProgramUniform4f :: FunPtr (GLuint -> GLint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glProgramUniform4f = unsafePerformIO $ getCommand "glProgramUniform4f"

-- glProgramUniform4fEXT -------------------------------------------------------

-- | This command is an alias for 'glProgramUniform4f'.
glProgramUniform4fEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLfloat -- ^ @v0@.
  -> GLfloat -- ^ @v1@.
  -> GLfloat -- ^ @v2@.
  -> GLfloat -- ^ @v3@.
  -> m ()
glProgramUniform4fEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn658 ptr_glProgramUniform4fEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glProgramUniform4fEXT #-}
ptr_glProgramUniform4fEXT :: FunPtr (GLuint -> GLint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glProgramUniform4fEXT = unsafePerformIO $ getCommand "glProgramUniform4fEXT"

-- glProgramUniform4fv ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniform4fv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @value@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glProgramUniform4fv v1 v2 v3 v4 = liftIO $ dyn457 ptr_glProgramUniform4fv v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform4fv #-}
ptr_glProgramUniform4fv :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glProgramUniform4fv = unsafePerformIO $ getCommand "glProgramUniform4fv"

-- glProgramUniform4fvEXT ------------------------------------------------------

-- | This command is an alias for 'glProgramUniform4fv'.
glProgramUniform4fvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @value@ pointing to @count*4@ elements of type @GLfloat@.
  -> m ()
glProgramUniform4fvEXT v1 v2 v3 v4 = liftIO $ dyn457 ptr_glProgramUniform4fvEXT v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform4fvEXT #-}
ptr_glProgramUniform4fvEXT :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glProgramUniform4fvEXT = unsafePerformIO $ getCommand "glProgramUniform4fvEXT"

-- glProgramUniform4i ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniform4i
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLint -- ^ @v0@.
  -> GLint -- ^ @v1@.
  -> GLint -- ^ @v2@.
  -> GLint -- ^ @v3@.
  -> m ()
glProgramUniform4i v1 v2 v3 v4 v5 v6 = liftIO $ dyn659 ptr_glProgramUniform4i v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glProgramUniform4i #-}
ptr_glProgramUniform4i :: FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> GLint -> IO ())
ptr_glProgramUniform4i = unsafePerformIO $ getCommand "glProgramUniform4i"

-- glProgramUniform4i64ARB -----------------------------------------------------

glProgramUniform4i64ARB
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLint64 -- ^ @x@.
  -> GLint64 -- ^ @y@.
  -> GLint64 -- ^ @z@.
  -> GLint64 -- ^ @w@.
  -> m ()
glProgramUniform4i64ARB v1 v2 v3 v4 v5 v6 = liftIO $ dyn660 ptr_glProgramUniform4i64ARB v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glProgramUniform4i64ARB #-}
ptr_glProgramUniform4i64ARB :: FunPtr (GLuint -> GLint -> GLint64 -> GLint64 -> GLint64 -> GLint64 -> IO ())
ptr_glProgramUniform4i64ARB = unsafePerformIO $ getCommand "glProgramUniform4i64ARB"

-- glProgramUniform4i64NV ------------------------------------------------------

glProgramUniform4i64NV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLint64EXT -- ^ @x@.
  -> GLint64EXT -- ^ @y@.
  -> GLint64EXT -- ^ @z@.
  -> GLint64EXT -- ^ @w@.
  -> m ()
glProgramUniform4i64NV v1 v2 v3 v4 v5 v6 = liftIO $ dyn661 ptr_glProgramUniform4i64NV v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glProgramUniform4i64NV #-}
ptr_glProgramUniform4i64NV :: FunPtr (GLuint -> GLint -> GLint64EXT -> GLint64EXT -> GLint64EXT -> GLint64EXT -> IO ())
ptr_glProgramUniform4i64NV = unsafePerformIO $ getCommand "glProgramUniform4i64NV"

-- glProgramUniform4i64vARB ----------------------------------------------------

glProgramUniform4i64vARB
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint64 -- ^ @value@ pointing to @count*4@ elements of type @GLint64@.
  -> m ()
glProgramUniform4i64vARB v1 v2 v3 v4 = liftIO $ dyn458 ptr_glProgramUniform4i64vARB v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform4i64vARB #-}
ptr_glProgramUniform4i64vARB :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLint64 -> IO ())
ptr_glProgramUniform4i64vARB = unsafePerformIO $ getCommand "glProgramUniform4i64vARB"

-- glProgramUniform4i64vNV -----------------------------------------------------

glProgramUniform4i64vNV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint64EXT -- ^ @value@ pointing to @count*4@ elements of type @GLint64EXT@.
  -> m ()
glProgramUniform4i64vNV v1 v2 v3 v4 = liftIO $ dyn636 ptr_glProgramUniform4i64vNV v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform4i64vNV #-}
ptr_glProgramUniform4i64vNV :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLint64EXT -> IO ())
ptr_glProgramUniform4i64vNV = unsafePerformIO $ getCommand "glProgramUniform4i64vNV"

-- glProgramUniform4iEXT -------------------------------------------------------

-- | This command is an alias for 'glProgramUniform4i'.
glProgramUniform4iEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLint -- ^ @v0@.
  -> GLint -- ^ @v1@.
  -> GLint -- ^ @v2@.
  -> GLint -- ^ @v3@.
  -> m ()
glProgramUniform4iEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn659 ptr_glProgramUniform4iEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glProgramUniform4iEXT #-}
ptr_glProgramUniform4iEXT :: FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> GLint -> IO ())
ptr_glProgramUniform4iEXT = unsafePerformIO $ getCommand "glProgramUniform4iEXT"

-- glProgramUniform4iv ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniform4iv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint -- ^ @value@ pointing to @4@ elements of type @GLint@.
  -> m ()
glProgramUniform4iv v1 v2 v3 v4 = liftIO $ dyn459 ptr_glProgramUniform4iv v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform4iv #-}
ptr_glProgramUniform4iv :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLint -> IO ())
ptr_glProgramUniform4iv = unsafePerformIO $ getCommand "glProgramUniform4iv"

-- glProgramUniform4ivEXT ------------------------------------------------------

-- | This command is an alias for 'glProgramUniform4iv'.
glProgramUniform4ivEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint -- ^ @value@ pointing to @count*4@ elements of type @GLint@.
  -> m ()
glProgramUniform4ivEXT v1 v2 v3 v4 = liftIO $ dyn459 ptr_glProgramUniform4ivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform4ivEXT #-}
ptr_glProgramUniform4ivEXT :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLint -> IO ())
ptr_glProgramUniform4ivEXT = unsafePerformIO $ getCommand "glProgramUniform4ivEXT"

-- glProgramUniform4ui ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniform4ui
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLuint -- ^ @v0@.
  -> GLuint -- ^ @v1@.
  -> GLuint -- ^ @v2@.
  -> GLuint -- ^ @v3@.
  -> m ()
glProgramUniform4ui v1 v2 v3 v4 v5 v6 = liftIO $ dyn662 ptr_glProgramUniform4ui v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glProgramUniform4ui #-}
ptr_glProgramUniform4ui :: FunPtr (GLuint -> GLint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ())
ptr_glProgramUniform4ui = unsafePerformIO $ getCommand "glProgramUniform4ui"

-- glProgramUniform4ui64ARB ----------------------------------------------------

glProgramUniform4ui64ARB
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLuint64 -- ^ @x@.
  -> GLuint64 -- ^ @y@.
  -> GLuint64 -- ^ @z@.
  -> GLuint64 -- ^ @w@.
  -> m ()
glProgramUniform4ui64ARB v1 v2 v3 v4 v5 v6 = liftIO $ dyn663 ptr_glProgramUniform4ui64ARB v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glProgramUniform4ui64ARB #-}
ptr_glProgramUniform4ui64ARB :: FunPtr (GLuint -> GLint -> GLuint64 -> GLuint64 -> GLuint64 -> GLuint64 -> IO ())
ptr_glProgramUniform4ui64ARB = unsafePerformIO $ getCommand "glProgramUniform4ui64ARB"

-- glProgramUniform4ui64NV -----------------------------------------------------

glProgramUniform4ui64NV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLuint64EXT -- ^ @x@.
  -> GLuint64EXT -- ^ @y@.
  -> GLuint64EXT -- ^ @z@.
  -> GLuint64EXT -- ^ @w@.
  -> m ()
glProgramUniform4ui64NV v1 v2 v3 v4 v5 v6 = liftIO $ dyn664 ptr_glProgramUniform4ui64NV v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glProgramUniform4ui64NV #-}
ptr_glProgramUniform4ui64NV :: FunPtr (GLuint -> GLint -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> IO ())
ptr_glProgramUniform4ui64NV = unsafePerformIO $ getCommand "glProgramUniform4ui64NV"

-- glProgramUniform4ui64vARB ---------------------------------------------------

glProgramUniform4ui64vARB
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint64 -- ^ @value@ pointing to @count*4@ elements of type @GLuint64@.
  -> m ()
glProgramUniform4ui64vARB v1 v2 v3 v4 = liftIO $ dyn460 ptr_glProgramUniform4ui64vARB v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform4ui64vARB #-}
ptr_glProgramUniform4ui64vARB :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint64 -> IO ())
ptr_glProgramUniform4ui64vARB = unsafePerformIO $ getCommand "glProgramUniform4ui64vARB"

-- glProgramUniform4ui64vNV ----------------------------------------------------

glProgramUniform4ui64vNV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint64EXT -- ^ @value@ pointing to @count*4@ elements of type @GLuint64EXT@.
  -> m ()
glProgramUniform4ui64vNV v1 v2 v3 v4 = liftIO $ dyn640 ptr_glProgramUniform4ui64vNV v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform4ui64vNV #-}
ptr_glProgramUniform4ui64vNV :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint64EXT -> IO ())
ptr_glProgramUniform4ui64vNV = unsafePerformIO $ getCommand "glProgramUniform4ui64vNV"

-- glProgramUniform4uiEXT ------------------------------------------------------

-- | This command is an alias for 'glProgramUniform4ui'.
glProgramUniform4uiEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLuint -- ^ @v0@.
  -> GLuint -- ^ @v1@.
  -> GLuint -- ^ @v2@.
  -> GLuint -- ^ @v3@.
  -> m ()
glProgramUniform4uiEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn662 ptr_glProgramUniform4uiEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glProgramUniform4uiEXT #-}
ptr_glProgramUniform4uiEXT :: FunPtr (GLuint -> GLint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ())
ptr_glProgramUniform4uiEXT = unsafePerformIO $ getCommand "glProgramUniform4uiEXT"

-- glProgramUniform4uiv --------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniform4uiv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @value@ pointing to @4@ elements of type @GLuint@.
  -> m ()
glProgramUniform4uiv v1 v2 v3 v4 = liftIO $ dyn461 ptr_glProgramUniform4uiv v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform4uiv #-}
ptr_glProgramUniform4uiv :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint -> IO ())
ptr_glProgramUniform4uiv = unsafePerformIO $ getCommand "glProgramUniform4uiv"

-- glProgramUniform4uivEXT -----------------------------------------------------

-- | This command is an alias for 'glProgramUniform4uiv'.
glProgramUniform4uivEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @value@ pointing to @count*4@ elements of type @GLuint@.
  -> m ()
glProgramUniform4uivEXT v1 v2 v3 v4 = liftIO $ dyn461 ptr_glProgramUniform4uivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform4uivEXT #-}
ptr_glProgramUniform4uivEXT :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint -> IO ())
ptr_glProgramUniform4uivEXT = unsafePerformIO $ getCommand "glProgramUniform4uivEXT"

-- glProgramUniformHandleui64ARB -----------------------------------------------

glProgramUniformHandleui64ARB
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLuint64 -- ^ @value@.
  -> m ()
glProgramUniformHandleui64ARB v1 v2 v3 = liftIO $ dyn638 ptr_glProgramUniformHandleui64ARB v1 v2 v3

{-# NOINLINE ptr_glProgramUniformHandleui64ARB #-}
ptr_glProgramUniformHandleui64ARB :: FunPtr (GLuint -> GLint -> GLuint64 -> IO ())
ptr_glProgramUniformHandleui64ARB = unsafePerformIO $ getCommand "glProgramUniformHandleui64ARB"

-- glProgramUniformHandleui64NV ------------------------------------------------

glProgramUniformHandleui64NV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLuint64 -- ^ @value@.
  -> m ()
glProgramUniformHandleui64NV v1 v2 v3 = liftIO $ dyn638 ptr_glProgramUniformHandleui64NV v1 v2 v3

{-# NOINLINE ptr_glProgramUniformHandleui64NV #-}
ptr_glProgramUniformHandleui64NV :: FunPtr (GLuint -> GLint -> GLuint64 -> IO ())
ptr_glProgramUniformHandleui64NV = unsafePerformIO $ getCommand "glProgramUniformHandleui64NV"

-- glProgramUniformHandleui64vARB ----------------------------------------------

glProgramUniformHandleui64vARB
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint64 -- ^ @values@ pointing to @count@ elements of type @GLuint64@.
  -> m ()
glProgramUniformHandleui64vARB v1 v2 v3 v4 = liftIO $ dyn460 ptr_glProgramUniformHandleui64vARB v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniformHandleui64vARB #-}
ptr_glProgramUniformHandleui64vARB :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint64 -> IO ())
ptr_glProgramUniformHandleui64vARB = unsafePerformIO $ getCommand "glProgramUniformHandleui64vARB"

-- glProgramUniformHandleui64vNV -----------------------------------------------

glProgramUniformHandleui64vNV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint64 -- ^ @values@ pointing to @count@ elements of type @GLuint64@.
  -> m ()
glProgramUniformHandleui64vNV v1 v2 v3 v4 = liftIO $ dyn460 ptr_glProgramUniformHandleui64vNV v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniformHandleui64vNV #-}
ptr_glProgramUniformHandleui64vNV :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint64 -> IO ())
ptr_glProgramUniformHandleui64vNV = unsafePerformIO $ getCommand "glProgramUniformHandleui64vNV"

-- glProgramUniformMatrix2dv ---------------------------------------------------

glProgramUniformMatrix2dv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLdouble -- ^ @value@ pointing to @2@ elements of type @GLdouble@.
  -> m ()
glProgramUniformMatrix2dv v1 v2 v3 v4 v5 = liftIO $ dyn665 ptr_glProgramUniformMatrix2dv v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix2dv #-}
ptr_glProgramUniformMatrix2dv :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
ptr_glProgramUniformMatrix2dv = unsafePerformIO $ getCommand "glProgramUniformMatrix2dv"

-- glProgramUniformMatrix2dvEXT ------------------------------------------------

glProgramUniformMatrix2dvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLdouble -- ^ @value@ pointing to @count@ elements of type @GLdouble@.
  -> m ()
glProgramUniformMatrix2dvEXT v1 v2 v3 v4 v5 = liftIO $ dyn665 ptr_glProgramUniformMatrix2dvEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix2dvEXT #-}
ptr_glProgramUniformMatrix2dvEXT :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
ptr_glProgramUniformMatrix2dvEXT = unsafePerformIO $ getCommand "glProgramUniformMatrix2dvEXT"

-- glProgramUniformMatrix2fv ---------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniformMatrix2fv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @2@ elements of type @GLfloat@.
  -> m ()
glProgramUniformMatrix2fv v1 v2 v3 v4 v5 = liftIO $ dyn666 ptr_glProgramUniformMatrix2fv v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix2fv #-}
ptr_glProgramUniformMatrix2fv :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glProgramUniformMatrix2fv = unsafePerformIO $ getCommand "glProgramUniformMatrix2fv"

-- glProgramUniformMatrix2fvEXT ------------------------------------------------

-- | This command is an alias for 'glProgramUniformMatrix2fv'.
glProgramUniformMatrix2fvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*4@ elements of type @GLfloat@.
  -> m ()
glProgramUniformMatrix2fvEXT v1 v2 v3 v4 v5 = liftIO $ dyn666 ptr_glProgramUniformMatrix2fvEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix2fvEXT #-}
ptr_glProgramUniformMatrix2fvEXT :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glProgramUniformMatrix2fvEXT = unsafePerformIO $ getCommand "glProgramUniformMatrix2fvEXT"

-- glProgramUniformMatrix2x3dv -------------------------------------------------

glProgramUniformMatrix2x3dv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLdouble -- ^ @value@ pointing to @count@ elements of type @GLdouble@.
  -> m ()
glProgramUniformMatrix2x3dv v1 v2 v3 v4 v5 = liftIO $ dyn665 ptr_glProgramUniformMatrix2x3dv v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix2x3dv #-}
ptr_glProgramUniformMatrix2x3dv :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
ptr_glProgramUniformMatrix2x3dv = unsafePerformIO $ getCommand "glProgramUniformMatrix2x3dv"

-- glProgramUniformMatrix2x3dvEXT ----------------------------------------------

glProgramUniformMatrix2x3dvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLdouble -- ^ @value@ pointing to @count@ elements of type @GLdouble@.
  -> m ()
glProgramUniformMatrix2x3dvEXT v1 v2 v3 v4 v5 = liftIO $ dyn665 ptr_glProgramUniformMatrix2x3dvEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix2x3dvEXT #-}
ptr_glProgramUniformMatrix2x3dvEXT :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
ptr_glProgramUniformMatrix2x3dvEXT = unsafePerformIO $ getCommand "glProgramUniformMatrix2x3dvEXT"

-- glProgramUniformMatrix2x3fv -------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniformMatrix2x3fv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count@ elements of type @GLfloat@.
  -> m ()
glProgramUniformMatrix2x3fv v1 v2 v3 v4 v5 = liftIO $ dyn666 ptr_glProgramUniformMatrix2x3fv v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix2x3fv #-}
ptr_glProgramUniformMatrix2x3fv :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glProgramUniformMatrix2x3fv = unsafePerformIO $ getCommand "glProgramUniformMatrix2x3fv"

-- glProgramUniformMatrix2x3fvEXT ----------------------------------------------

-- | This command is an alias for 'glProgramUniformMatrix2x3fv'.
glProgramUniformMatrix2x3fvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*6@ elements of type @GLfloat@.
  -> m ()
glProgramUniformMatrix2x3fvEXT v1 v2 v3 v4 v5 = liftIO $ dyn666 ptr_glProgramUniformMatrix2x3fvEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix2x3fvEXT #-}
ptr_glProgramUniformMatrix2x3fvEXT :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glProgramUniformMatrix2x3fvEXT = unsafePerformIO $ getCommand "glProgramUniformMatrix2x3fvEXT"

-- glProgramUniformMatrix2x4dv -------------------------------------------------

glProgramUniformMatrix2x4dv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLdouble -- ^ @value@ pointing to @count@ elements of type @GLdouble@.
  -> m ()
glProgramUniformMatrix2x4dv v1 v2 v3 v4 v5 = liftIO $ dyn665 ptr_glProgramUniformMatrix2x4dv v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix2x4dv #-}
ptr_glProgramUniformMatrix2x4dv :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
ptr_glProgramUniformMatrix2x4dv = unsafePerformIO $ getCommand "glProgramUniformMatrix2x4dv"

-- glProgramUniformMatrix2x4dvEXT ----------------------------------------------

glProgramUniformMatrix2x4dvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLdouble -- ^ @value@ pointing to @count@ elements of type @GLdouble@.
  -> m ()
glProgramUniformMatrix2x4dvEXT v1 v2 v3 v4 v5 = liftIO $ dyn665 ptr_glProgramUniformMatrix2x4dvEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix2x4dvEXT #-}
ptr_glProgramUniformMatrix2x4dvEXT :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
ptr_glProgramUniformMatrix2x4dvEXT = unsafePerformIO $ getCommand "glProgramUniformMatrix2x4dvEXT"

-- glProgramUniformMatrix2x4fv -------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniformMatrix2x4fv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count@ elements of type @GLfloat@.
  -> m ()
glProgramUniformMatrix2x4fv v1 v2 v3 v4 v5 = liftIO $ dyn666 ptr_glProgramUniformMatrix2x4fv v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix2x4fv #-}
ptr_glProgramUniformMatrix2x4fv :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glProgramUniformMatrix2x4fv = unsafePerformIO $ getCommand "glProgramUniformMatrix2x4fv"

-- glProgramUniformMatrix2x4fvEXT ----------------------------------------------

-- | This command is an alias for 'glProgramUniformMatrix2x4fv'.
glProgramUniformMatrix2x4fvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*8@ elements of type @GLfloat@.
  -> m ()
glProgramUniformMatrix2x4fvEXT v1 v2 v3 v4 v5 = liftIO $ dyn666 ptr_glProgramUniformMatrix2x4fvEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix2x4fvEXT #-}
ptr_glProgramUniformMatrix2x4fvEXT :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glProgramUniformMatrix2x4fvEXT = unsafePerformIO $ getCommand "glProgramUniformMatrix2x4fvEXT"

