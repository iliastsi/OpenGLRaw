--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F66
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

module Graphics.GL.Functions.F66 (
  glUniform3ivARB,
  glUniform3ui,
  glUniform3ui64ARB,
  glUniform3ui64NV,
  glUniform3ui64vARB,
  glUniform3ui64vNV,
  glUniform3uiEXT,
  glUniform3uiv,
  glUniform3uivEXT,
  glUniform4d,
  glUniform4dv,
  glUniform4f,
  glUniform4fARB,
  glUniform4fv,
  glUniform4fvARB,
  glUniform4i,
  glUniform4i64ARB,
  glUniform4i64NV,
  glUniform4i64vARB,
  glUniform4i64vNV,
  glUniform4iARB,
  glUniform4iv,
  glUniform4ivARB,
  glUniform4ui,
  glUniform4ui64ARB,
  glUniform4ui64NV,
  glUniform4ui64vARB,
  glUniform4ui64vNV,
  glUniform4uiEXT,
  glUniform4uiv,
  glUniform4uivEXT,
  glUniformBlockBinding,
  glUniformBufferEXT,
  glUniformHandleui64ARB,
  glUniformHandleui64NV,
  glUniformHandleui64vARB,
  glUniformHandleui64vNV,
  glUniformMatrix2dv,
  glUniformMatrix2fv,
  glUniformMatrix2fvARB
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

-- glUniform3ivARB -------------------------------------------------------------

-- | This command is an alias for 'glUniform3iv'.
glUniform3ivARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint -- ^ @value@ pointing to @count*3@ elements of type @GLint@.
  -> m ()
glUniform3ivARB v1 v2 v3 = liftIO $ dyn788 ptr_glUniform3ivARB v1 v2 v3

{-# NOINLINE ptr_glUniform3ivARB #-}
ptr_glUniform3ivARB :: FunPtr (GLint -> GLsizei -> Ptr GLint -> IO ())
ptr_glUniform3ivARB = unsafePerformIO $ getCommand "glUniform3ivARB"

-- glUniform3ui ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniform3ui
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLuint -- ^ @v0@.
  -> GLuint -- ^ @v1@.
  -> GLuint -- ^ @v2@.
  -> m ()
glUniform3ui v1 v2 v3 v4 = liftIO $ dyn804 ptr_glUniform3ui v1 v2 v3 v4

{-# NOINLINE ptr_glUniform3ui #-}
ptr_glUniform3ui :: FunPtr (GLint -> GLuint -> GLuint -> GLuint -> IO ())
ptr_glUniform3ui = unsafePerformIO $ getCommand "glUniform3ui"

-- glUniform3ui64ARB -----------------------------------------------------------

glUniform3ui64ARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLuint64 -- ^ @x@.
  -> GLuint64 -- ^ @y@.
  -> GLuint64 -- ^ @z@.
  -> m ()
glUniform3ui64ARB v1 v2 v3 v4 = liftIO $ dyn805 ptr_glUniform3ui64ARB v1 v2 v3 v4

{-# NOINLINE ptr_glUniform3ui64ARB #-}
ptr_glUniform3ui64ARB :: FunPtr (GLint -> GLuint64 -> GLuint64 -> GLuint64 -> IO ())
ptr_glUniform3ui64ARB = unsafePerformIO $ getCommand "glUniform3ui64ARB"

-- glUniform3ui64NV ------------------------------------------------------------

glUniform3ui64NV
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLuint64EXT -- ^ @x@.
  -> GLuint64EXT -- ^ @y@.
  -> GLuint64EXT -- ^ @z@.
  -> m ()
glUniform3ui64NV v1 v2 v3 v4 = liftIO $ dyn806 ptr_glUniform3ui64NV v1 v2 v3 v4

{-# NOINLINE ptr_glUniform3ui64NV #-}
ptr_glUniform3ui64NV :: FunPtr (GLint -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> IO ())
ptr_glUniform3ui64NV = unsafePerformIO $ getCommand "glUniform3ui64NV"

-- glUniform3ui64vARB ----------------------------------------------------------

glUniform3ui64vARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint64 -- ^ @value@ pointing to @count*3@ elements of type @GLuint64@.
  -> m ()
glUniform3ui64vARB v1 v2 v3 = liftIO $ dyn792 ptr_glUniform3ui64vARB v1 v2 v3

{-# NOINLINE ptr_glUniform3ui64vARB #-}
ptr_glUniform3ui64vARB :: FunPtr (GLint -> GLsizei -> Ptr GLuint64 -> IO ())
ptr_glUniform3ui64vARB = unsafePerformIO $ getCommand "glUniform3ui64vARB"

-- glUniform3ui64vNV -----------------------------------------------------------

glUniform3ui64vNV
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint64EXT -- ^ @value@ pointing to @count*3@ elements of type @GLuint64EXT@.
  -> m ()
glUniform3ui64vNV v1 v2 v3 = liftIO $ dyn793 ptr_glUniform3ui64vNV v1 v2 v3

{-# NOINLINE ptr_glUniform3ui64vNV #-}
ptr_glUniform3ui64vNV :: FunPtr (GLint -> GLsizei -> Ptr GLuint64EXT -> IO ())
ptr_glUniform3ui64vNV = unsafePerformIO $ getCommand "glUniform3ui64vNV"

-- glUniform3uiEXT -------------------------------------------------------------

-- | This command is an alias for 'glUniform3ui'.
glUniform3uiEXT
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLuint -- ^ @v0@.
  -> GLuint -- ^ @v1@.
  -> GLuint -- ^ @v2@.
  -> m ()
glUniform3uiEXT v1 v2 v3 v4 = liftIO $ dyn804 ptr_glUniform3uiEXT v1 v2 v3 v4

{-# NOINLINE ptr_glUniform3uiEXT #-}
ptr_glUniform3uiEXT :: FunPtr (GLint -> GLuint -> GLuint -> GLuint -> IO ())
ptr_glUniform3uiEXT = unsafePerformIO $ getCommand "glUniform3uiEXT"

-- glUniform3uiv ---------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniform3uiv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @value@ pointing to @count*3@ elements of type @GLuint@.
  -> m ()
glUniform3uiv v1 v2 v3 = liftIO $ dyn794 ptr_glUniform3uiv v1 v2 v3

{-# NOINLINE ptr_glUniform3uiv #-}
ptr_glUniform3uiv :: FunPtr (GLint -> GLsizei -> Ptr GLuint -> IO ())
ptr_glUniform3uiv = unsafePerformIO $ getCommand "glUniform3uiv"

-- glUniform3uivEXT ------------------------------------------------------------

-- | This command is an alias for 'glUniform3uiv'.
glUniform3uivEXT
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @value@ pointing to @count*3@ elements of type @GLuint@.
  -> m ()
glUniform3uivEXT v1 v2 v3 = liftIO $ dyn794 ptr_glUniform3uivEXT v1 v2 v3

{-# NOINLINE ptr_glUniform3uivEXT #-}
ptr_glUniform3uivEXT :: FunPtr (GLint -> GLsizei -> Ptr GLuint -> IO ())
ptr_glUniform3uivEXT = unsafePerformIO $ getCommand "glUniform3uivEXT"

-- glUniform4d -----------------------------------------------------------------

glUniform4d
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> GLdouble -- ^ @z@.
  -> GLdouble -- ^ @w@.
  -> m ()
glUniform4d v1 v2 v3 v4 v5 = liftIO $ dyn807 ptr_glUniform4d v1 v2 v3 v4 v5

{-# NOINLINE ptr_glUniform4d #-}
ptr_glUniform4d :: FunPtr (GLint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glUniform4d = unsafePerformIO $ getCommand "glUniform4d"

-- glUniform4dv ----------------------------------------------------------------

glUniform4dv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLdouble -- ^ @value@ pointing to @count*4@ elements of type @GLdouble@.
  -> m ()
glUniform4dv v1 v2 v3 = liftIO $ dyn781 ptr_glUniform4dv v1 v2 v3

{-# NOINLINE ptr_glUniform4dv #-}
ptr_glUniform4dv :: FunPtr (GLint -> GLsizei -> Ptr GLdouble -> IO ())
ptr_glUniform4dv = unsafePerformIO $ getCommand "glUniform4dv"

-- glUniform4f -----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glUniform.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniform4f
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLfloat -- ^ @v0@.
  -> GLfloat -- ^ @v1@.
  -> GLfloat -- ^ @v2@.
  -> GLfloat -- ^ @v3@.
  -> m ()
glUniform4f v1 v2 v3 v4 v5 = liftIO $ dyn808 ptr_glUniform4f v1 v2 v3 v4 v5

{-# NOINLINE ptr_glUniform4f #-}
ptr_glUniform4f :: FunPtr (GLint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glUniform4f = unsafePerformIO $ getCommand "glUniform4f"

-- glUniform4fARB --------------------------------------------------------------

-- | This command is an alias for 'glUniform4f'.
glUniform4fARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLfloat -- ^ @v0@.
  -> GLfloat -- ^ @v1@.
  -> GLfloat -- ^ @v2@.
  -> GLfloat -- ^ @v3@.
  -> m ()
glUniform4fARB v1 v2 v3 v4 v5 = liftIO $ dyn808 ptr_glUniform4fARB v1 v2 v3 v4 v5

{-# NOINLINE ptr_glUniform4fARB #-}
ptr_glUniform4fARB :: FunPtr (GLint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glUniform4fARB = unsafePerformIO $ getCommand "glUniform4fARB"

-- glUniform4fv ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glUniform.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniform4fv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @value@ pointing to @count*4@ elements of type @GLfloat@.
  -> m ()
glUniform4fv v1 v2 v3 = liftIO $ dyn783 ptr_glUniform4fv v1 v2 v3

{-# NOINLINE ptr_glUniform4fv #-}
ptr_glUniform4fv :: FunPtr (GLint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glUniform4fv = unsafePerformIO $ getCommand "glUniform4fv"

-- glUniform4fvARB -------------------------------------------------------------

-- | This command is an alias for 'glUniform4fv'.
glUniform4fvARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @value@ pointing to @count*4@ elements of type @GLfloat@.
  -> m ()
glUniform4fvARB v1 v2 v3 = liftIO $ dyn783 ptr_glUniform4fvARB v1 v2 v3

{-# NOINLINE ptr_glUniform4fvARB #-}
ptr_glUniform4fvARB :: FunPtr (GLint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glUniform4fvARB = unsafePerformIO $ getCommand "glUniform4fvARB"

-- glUniform4i -----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glUniform.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniform4i
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLint -- ^ @v0@.
  -> GLint -- ^ @v1@.
  -> GLint -- ^ @v2@.
  -> GLint -- ^ @v3@.
  -> m ()
glUniform4i v1 v2 v3 v4 v5 = liftIO $ dyn253 ptr_glUniform4i v1 v2 v3 v4 v5

{-# NOINLINE ptr_glUniform4i #-}
ptr_glUniform4i :: FunPtr (GLint -> GLint -> GLint -> GLint -> GLint -> IO ())
ptr_glUniform4i = unsafePerformIO $ getCommand "glUniform4i"

-- glUniform4i64ARB ------------------------------------------------------------

glUniform4i64ARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLint64 -- ^ @x@.
  -> GLint64 -- ^ @y@.
  -> GLint64 -- ^ @z@.
  -> GLint64 -- ^ @w@.
  -> m ()
glUniform4i64ARB v1 v2 v3 v4 v5 = liftIO $ dyn809 ptr_glUniform4i64ARB v1 v2 v3 v4 v5

{-# NOINLINE ptr_glUniform4i64ARB #-}
ptr_glUniform4i64ARB :: FunPtr (GLint -> GLint64 -> GLint64 -> GLint64 -> GLint64 -> IO ())
ptr_glUniform4i64ARB = unsafePerformIO $ getCommand "glUniform4i64ARB"

-- glUniform4i64NV -------------------------------------------------------------

glUniform4i64NV
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLint64EXT -- ^ @x@.
  -> GLint64EXT -- ^ @y@.
  -> GLint64EXT -- ^ @z@.
  -> GLint64EXT -- ^ @w@.
  -> m ()
glUniform4i64NV v1 v2 v3 v4 v5 = liftIO $ dyn810 ptr_glUniform4i64NV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glUniform4i64NV #-}
ptr_glUniform4i64NV :: FunPtr (GLint -> GLint64EXT -> GLint64EXT -> GLint64EXT -> GLint64EXT -> IO ())
ptr_glUniform4i64NV = unsafePerformIO $ getCommand "glUniform4i64NV"

-- glUniform4i64vARB -----------------------------------------------------------

glUniform4i64vARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint64 -- ^ @value@ pointing to @count*4@ elements of type @GLint64@.
  -> m ()
glUniform4i64vARB v1 v2 v3 = liftIO $ dyn786 ptr_glUniform4i64vARB v1 v2 v3

{-# NOINLINE ptr_glUniform4i64vARB #-}
ptr_glUniform4i64vARB :: FunPtr (GLint -> GLsizei -> Ptr GLint64 -> IO ())
ptr_glUniform4i64vARB = unsafePerformIO $ getCommand "glUniform4i64vARB"

-- glUniform4i64vNV ------------------------------------------------------------

glUniform4i64vNV
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint64EXT -- ^ @value@ pointing to @count*4@ elements of type @GLint64EXT@.
  -> m ()
glUniform4i64vNV v1 v2 v3 = liftIO $ dyn787 ptr_glUniform4i64vNV v1 v2 v3

{-# NOINLINE ptr_glUniform4i64vNV #-}
ptr_glUniform4i64vNV :: FunPtr (GLint -> GLsizei -> Ptr GLint64EXT -> IO ())
ptr_glUniform4i64vNV = unsafePerformIO $ getCommand "glUniform4i64vNV"

-- glUniform4iARB --------------------------------------------------------------

-- | This command is an alias for 'glUniform4i'.
glUniform4iARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLint -- ^ @v0@.
  -> GLint -- ^ @v1@.
  -> GLint -- ^ @v2@.
  -> GLint -- ^ @v3@.
  -> m ()
glUniform4iARB v1 v2 v3 v4 v5 = liftIO $ dyn253 ptr_glUniform4iARB v1 v2 v3 v4 v5

{-# NOINLINE ptr_glUniform4iARB #-}
ptr_glUniform4iARB :: FunPtr (GLint -> GLint -> GLint -> GLint -> GLint -> IO ())
ptr_glUniform4iARB = unsafePerformIO $ getCommand "glUniform4iARB"

-- glUniform4iv ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glUniform.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniform4iv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint -- ^ @value@ pointing to @count*4@ elements of type @GLint@.
  -> m ()
glUniform4iv v1 v2 v3 = liftIO $ dyn788 ptr_glUniform4iv v1 v2 v3

{-# NOINLINE ptr_glUniform4iv #-}
ptr_glUniform4iv :: FunPtr (GLint -> GLsizei -> Ptr GLint -> IO ())
ptr_glUniform4iv = unsafePerformIO $ getCommand "glUniform4iv"

-- glUniform4ivARB -------------------------------------------------------------

-- | This command is an alias for 'glUniform4iv'.
glUniform4ivARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint -- ^ @value@ pointing to @count*4@ elements of type @GLint@.
  -> m ()
glUniform4ivARB v1 v2 v3 = liftIO $ dyn788 ptr_glUniform4ivARB v1 v2 v3

{-# NOINLINE ptr_glUniform4ivARB #-}
ptr_glUniform4ivARB :: FunPtr (GLint -> GLsizei -> Ptr GLint -> IO ())
ptr_glUniform4ivARB = unsafePerformIO $ getCommand "glUniform4ivARB"

-- glUniform4ui ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniform4ui
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLuint -- ^ @v0@.
  -> GLuint -- ^ @v1@.
  -> GLuint -- ^ @v2@.
  -> GLuint -- ^ @v3@.
  -> m ()
glUniform4ui v1 v2 v3 v4 v5 = liftIO $ dyn811 ptr_glUniform4ui v1 v2 v3 v4 v5

{-# NOINLINE ptr_glUniform4ui #-}
ptr_glUniform4ui :: FunPtr (GLint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ())
ptr_glUniform4ui = unsafePerformIO $ getCommand "glUniform4ui"

-- glUniform4ui64ARB -----------------------------------------------------------

glUniform4ui64ARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLuint64 -- ^ @x@.
  -> GLuint64 -- ^ @y@.
  -> GLuint64 -- ^ @z@.
  -> GLuint64 -- ^ @w@.
  -> m ()
glUniform4ui64ARB v1 v2 v3 v4 v5 = liftIO $ dyn812 ptr_glUniform4ui64ARB v1 v2 v3 v4 v5

{-# NOINLINE ptr_glUniform4ui64ARB #-}
ptr_glUniform4ui64ARB :: FunPtr (GLint -> GLuint64 -> GLuint64 -> GLuint64 -> GLuint64 -> IO ())
ptr_glUniform4ui64ARB = unsafePerformIO $ getCommand "glUniform4ui64ARB"

-- glUniform4ui64NV ------------------------------------------------------------

glUniform4ui64NV
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLuint64EXT -- ^ @x@.
  -> GLuint64EXT -- ^ @y@.
  -> GLuint64EXT -- ^ @z@.
  -> GLuint64EXT -- ^ @w@.
  -> m ()
glUniform4ui64NV v1 v2 v3 v4 v5 = liftIO $ dyn813 ptr_glUniform4ui64NV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glUniform4ui64NV #-}
ptr_glUniform4ui64NV :: FunPtr (GLint -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> IO ())
ptr_glUniform4ui64NV = unsafePerformIO $ getCommand "glUniform4ui64NV"

-- glUniform4ui64vARB ----------------------------------------------------------

glUniform4ui64vARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint64 -- ^ @value@ pointing to @count*4@ elements of type @GLuint64@.
  -> m ()
glUniform4ui64vARB v1 v2 v3 = liftIO $ dyn792 ptr_glUniform4ui64vARB v1 v2 v3

{-# NOINLINE ptr_glUniform4ui64vARB #-}
ptr_glUniform4ui64vARB :: FunPtr (GLint -> GLsizei -> Ptr GLuint64 -> IO ())
ptr_glUniform4ui64vARB = unsafePerformIO $ getCommand "glUniform4ui64vARB"

-- glUniform4ui64vNV -----------------------------------------------------------

glUniform4ui64vNV
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint64EXT -- ^ @value@ pointing to @count*4@ elements of type @GLuint64EXT@.
  -> m ()
glUniform4ui64vNV v1 v2 v3 = liftIO $ dyn793 ptr_glUniform4ui64vNV v1 v2 v3

{-# NOINLINE ptr_glUniform4ui64vNV #-}
ptr_glUniform4ui64vNV :: FunPtr (GLint -> GLsizei -> Ptr GLuint64EXT -> IO ())
ptr_glUniform4ui64vNV = unsafePerformIO $ getCommand "glUniform4ui64vNV"

-- glUniform4uiEXT -------------------------------------------------------------

-- | This command is an alias for 'glUniform4ui'.
glUniform4uiEXT
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLuint -- ^ @v0@.
  -> GLuint -- ^ @v1@.
  -> GLuint -- ^ @v2@.
  -> GLuint -- ^ @v3@.
  -> m ()
glUniform4uiEXT v1 v2 v3 v4 v5 = liftIO $ dyn811 ptr_glUniform4uiEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glUniform4uiEXT #-}
ptr_glUniform4uiEXT :: FunPtr (GLint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ())
ptr_glUniform4uiEXT = unsafePerformIO $ getCommand "glUniform4uiEXT"

-- glUniform4uiv ---------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniform4uiv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @value@ pointing to @count*4@ elements of type @GLuint@.
  -> m ()
glUniform4uiv v1 v2 v3 = liftIO $ dyn794 ptr_glUniform4uiv v1 v2 v3

{-# NOINLINE ptr_glUniform4uiv #-}
ptr_glUniform4uiv :: FunPtr (GLint -> GLsizei -> Ptr GLuint -> IO ())
ptr_glUniform4uiv = unsafePerformIO $ getCommand "glUniform4uiv"

-- glUniform4uivEXT ------------------------------------------------------------

-- | This command is an alias for 'glUniform4uiv'.
glUniform4uivEXT
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @value@ pointing to @count*4@ elements of type @GLuint@.
  -> m ()
glUniform4uivEXT v1 v2 v3 = liftIO $ dyn794 ptr_glUniform4uivEXT v1 v2 v3

{-# NOINLINE ptr_glUniform4uivEXT #-}
ptr_glUniform4uivEXT :: FunPtr (GLint -> GLsizei -> Ptr GLuint -> IO ())
ptr_glUniform4uivEXT = unsafePerformIO $ getCommand "glUniform4uivEXT"

-- glUniformBlockBinding -------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glUniformBlockBinding.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniformBlockBinding.xhtml OpenGL 4.x>.
glUniformBlockBinding
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLuint -- ^ @uniformBlockIndex@.
  -> GLuint -- ^ @uniformBlockBinding@.
  -> m ()
glUniformBlockBinding v1 v2 v3 = liftIO $ dyn102 ptr_glUniformBlockBinding v1 v2 v3

{-# NOINLINE ptr_glUniformBlockBinding #-}
ptr_glUniformBlockBinding :: FunPtr (GLuint -> GLuint -> GLuint -> IO ())
ptr_glUniformBlockBinding = unsafePerformIO $ getCommand "glUniformBlockBinding"

-- glUniformBufferEXT ----------------------------------------------------------

glUniformBufferEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLuint -- ^ @buffer@.
  -> m ()
glUniformBufferEXT v1 v2 v3 = liftIO $ dyn637 ptr_glUniformBufferEXT v1 v2 v3

{-# NOINLINE ptr_glUniformBufferEXT #-}
ptr_glUniformBufferEXT :: FunPtr (GLuint -> GLint -> GLuint -> IO ())
ptr_glUniformBufferEXT = unsafePerformIO $ getCommand "glUniformBufferEXT"

-- glUniformHandleui64ARB ------------------------------------------------------

glUniformHandleui64ARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLuint64 -- ^ @value@.
  -> m ()
glUniformHandleui64ARB v1 v2 = liftIO $ dyn790 ptr_glUniformHandleui64ARB v1 v2

{-# NOINLINE ptr_glUniformHandleui64ARB #-}
ptr_glUniformHandleui64ARB :: FunPtr (GLint -> GLuint64 -> IO ())
ptr_glUniformHandleui64ARB = unsafePerformIO $ getCommand "glUniformHandleui64ARB"

-- glUniformHandleui64NV -------------------------------------------------------

glUniformHandleui64NV
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLuint64 -- ^ @value@.
  -> m ()
glUniformHandleui64NV v1 v2 = liftIO $ dyn790 ptr_glUniformHandleui64NV v1 v2

{-# NOINLINE ptr_glUniformHandleui64NV #-}
ptr_glUniformHandleui64NV :: FunPtr (GLint -> GLuint64 -> IO ())
ptr_glUniformHandleui64NV = unsafePerformIO $ getCommand "glUniformHandleui64NV"

-- glUniformHandleui64vARB -----------------------------------------------------

glUniformHandleui64vARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint64 -- ^ @value@ pointing to @count@ elements of type @GLuint64@.
  -> m ()
glUniformHandleui64vARB v1 v2 v3 = liftIO $ dyn792 ptr_glUniformHandleui64vARB v1 v2 v3

{-# NOINLINE ptr_glUniformHandleui64vARB #-}
ptr_glUniformHandleui64vARB :: FunPtr (GLint -> GLsizei -> Ptr GLuint64 -> IO ())
ptr_glUniformHandleui64vARB = unsafePerformIO $ getCommand "glUniformHandleui64vARB"

-- glUniformHandleui64vNV ------------------------------------------------------

glUniformHandleui64vNV
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint64 -- ^ @value@ pointing to @count@ elements of type @GLuint64@.
  -> m ()
glUniformHandleui64vNV v1 v2 v3 = liftIO $ dyn792 ptr_glUniformHandleui64vNV v1 v2 v3

{-# NOINLINE ptr_glUniformHandleui64vNV #-}
ptr_glUniformHandleui64vNV :: FunPtr (GLint -> GLsizei -> Ptr GLuint64 -> IO ())
ptr_glUniformHandleui64vNV = unsafePerformIO $ getCommand "glUniformHandleui64vNV"

-- glUniformMatrix2dv ----------------------------------------------------------

glUniformMatrix2dv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLdouble -- ^ @value@ pointing to @count*4@ elements of type @GLdouble@.
  -> m ()
glUniformMatrix2dv v1 v2 v3 v4 = liftIO $ dyn814 ptr_glUniformMatrix2dv v1 v2 v3 v4

{-# NOINLINE ptr_glUniformMatrix2dv #-}
ptr_glUniformMatrix2dv :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
ptr_glUniformMatrix2dv = unsafePerformIO $ getCommand "glUniformMatrix2dv"

-- glUniformMatrix2fv ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glUniform.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniformMatrix2fv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*4@ elements of type @GLfloat@.
  -> m ()
glUniformMatrix2fv v1 v2 v3 v4 = liftIO $ dyn815 ptr_glUniformMatrix2fv v1 v2 v3 v4

{-# NOINLINE ptr_glUniformMatrix2fv #-}
ptr_glUniformMatrix2fv :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glUniformMatrix2fv = unsafePerformIO $ getCommand "glUniformMatrix2fv"

-- glUniformMatrix2fvARB -------------------------------------------------------

-- | This command is an alias for 'glUniformMatrix2fv'.
glUniformMatrix2fvARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*4@ elements of type @GLfloat@.
  -> m ()
glUniformMatrix2fvARB v1 v2 v3 v4 = liftIO $ dyn815 ptr_glUniformMatrix2fvARB v1 v2 v3 v4

{-# NOINLINE ptr_glUniformMatrix2fvARB #-}
ptr_glUniformMatrix2fvARB :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glUniformMatrix2fvARB = unsafePerformIO $ getCommand "glUniformMatrix2fvARB"

