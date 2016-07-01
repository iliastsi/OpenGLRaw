--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F65
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

module Graphics.GL.Functions.F65 (
  glUniform1ui64vARB,
  glUniform1ui64vNV,
  glUniform1uiEXT,
  glUniform1uiv,
  glUniform1uivEXT,
  glUniform2d,
  glUniform2dv,
  glUniform2f,
  glUniform2fARB,
  glUniform2fv,
  glUniform2fvARB,
  glUniform2i,
  glUniform2i64ARB,
  glUniform2i64NV,
  glUniform2i64vARB,
  glUniform2i64vNV,
  glUniform2iARB,
  glUniform2iv,
  glUniform2ivARB,
  glUniform2ui,
  glUniform2ui64ARB,
  glUniform2ui64NV,
  glUniform2ui64vARB,
  glUniform2ui64vNV,
  glUniform2uiEXT,
  glUniform2uiv,
  glUniform2uivEXT,
  glUniform3d,
  glUniform3dv,
  glUniform3f,
  glUniform3fARB,
  glUniform3fv,
  glUniform3fvARB,
  glUniform3i,
  glUniform3i64ARB,
  glUniform3i64NV,
  glUniform3i64vARB,
  glUniform3i64vNV,
  glUniform3iARB,
  glUniform3iv
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

-- glUniform1ui64vARB ----------------------------------------------------------

glUniform1ui64vARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint64 -- ^ @value@ pointing to @count*1@ elements of type @GLuint64@.
  -> m ()
glUniform1ui64vARB v1 v2 v3 = liftIO $ dyn792 ptr_glUniform1ui64vARB v1 v2 v3

{-# NOINLINE ptr_glUniform1ui64vARB #-}
ptr_glUniform1ui64vARB :: FunPtr (GLint -> GLsizei -> Ptr GLuint64 -> IO ())
ptr_glUniform1ui64vARB = unsafePerformIO $ getCommand "glUniform1ui64vARB"

-- glUniform1ui64vNV -----------------------------------------------------------

glUniform1ui64vNV
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint64EXT -- ^ @value@ pointing to @count*1@ elements of type @GLuint64EXT@.
  -> m ()
glUniform1ui64vNV v1 v2 v3 = liftIO $ dyn793 ptr_glUniform1ui64vNV v1 v2 v3

{-# NOINLINE ptr_glUniform1ui64vNV #-}
ptr_glUniform1ui64vNV :: FunPtr (GLint -> GLsizei -> Ptr GLuint64EXT -> IO ())
ptr_glUniform1ui64vNV = unsafePerformIO $ getCommand "glUniform1ui64vNV"

-- glUniform1uiEXT -------------------------------------------------------------

-- | This command is an alias for 'glUniform1ui'.
glUniform1uiEXT
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLuint -- ^ @v0@.
  -> m ()
glUniform1uiEXT v1 v2 = liftIO $ dyn789 ptr_glUniform1uiEXT v1 v2

{-# NOINLINE ptr_glUniform1uiEXT #-}
ptr_glUniform1uiEXT :: FunPtr (GLint -> GLuint -> IO ())
ptr_glUniform1uiEXT = unsafePerformIO $ getCommand "glUniform1uiEXT"

-- glUniform1uiv ---------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniform1uiv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @value@ pointing to @count*1@ elements of type @GLuint@.
  -> m ()
glUniform1uiv v1 v2 v3 = liftIO $ dyn794 ptr_glUniform1uiv v1 v2 v3

{-# NOINLINE ptr_glUniform1uiv #-}
ptr_glUniform1uiv :: FunPtr (GLint -> GLsizei -> Ptr GLuint -> IO ())
ptr_glUniform1uiv = unsafePerformIO $ getCommand "glUniform1uiv"

-- glUniform1uivEXT ------------------------------------------------------------

-- | This command is an alias for 'glUniform1uiv'.
glUniform1uivEXT
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @value@ pointing to @count*1@ elements of type @GLuint@.
  -> m ()
glUniform1uivEXT v1 v2 v3 = liftIO $ dyn794 ptr_glUniform1uivEXT v1 v2 v3

{-# NOINLINE ptr_glUniform1uivEXT #-}
ptr_glUniform1uivEXT :: FunPtr (GLint -> GLsizei -> Ptr GLuint -> IO ())
ptr_glUniform1uivEXT = unsafePerformIO $ getCommand "glUniform1uivEXT"

-- glUniform2d -----------------------------------------------------------------

glUniform2d
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> m ()
glUniform2d v1 v2 v3 = liftIO $ dyn502 ptr_glUniform2d v1 v2 v3

{-# NOINLINE ptr_glUniform2d #-}
ptr_glUniform2d :: FunPtr (GLint -> GLdouble -> GLdouble -> IO ())
ptr_glUniform2d = unsafePerformIO $ getCommand "glUniform2d"

-- glUniform2dv ----------------------------------------------------------------

glUniform2dv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLdouble -- ^ @value@ pointing to @count*2@ elements of type @GLdouble@.
  -> m ()
glUniform2dv v1 v2 v3 = liftIO $ dyn781 ptr_glUniform2dv v1 v2 v3

{-# NOINLINE ptr_glUniform2dv #-}
ptr_glUniform2dv :: FunPtr (GLint -> GLsizei -> Ptr GLdouble -> IO ())
ptr_glUniform2dv = unsafePerformIO $ getCommand "glUniform2dv"

-- glUniform2f -----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glUniform.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniform2f
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLfloat -- ^ @v0@.
  -> GLfloat -- ^ @v1@.
  -> m ()
glUniform2f v1 v2 v3 = liftIO $ dyn503 ptr_glUniform2f v1 v2 v3

{-# NOINLINE ptr_glUniform2f #-}
ptr_glUniform2f :: FunPtr (GLint -> GLfloat -> GLfloat -> IO ())
ptr_glUniform2f = unsafePerformIO $ getCommand "glUniform2f"

-- glUniform2fARB --------------------------------------------------------------

-- | This command is an alias for 'glUniform2f'.
glUniform2fARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLfloat -- ^ @v0@.
  -> GLfloat -- ^ @v1@.
  -> m ()
glUniform2fARB v1 v2 v3 = liftIO $ dyn503 ptr_glUniform2fARB v1 v2 v3

{-# NOINLINE ptr_glUniform2fARB #-}
ptr_glUniform2fARB :: FunPtr (GLint -> GLfloat -> GLfloat -> IO ())
ptr_glUniform2fARB = unsafePerformIO $ getCommand "glUniform2fARB"

-- glUniform2fv ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glUniform.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniform2fv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @value@ pointing to @count*2@ elements of type @GLfloat@.
  -> m ()
glUniform2fv v1 v2 v3 = liftIO $ dyn783 ptr_glUniform2fv v1 v2 v3

{-# NOINLINE ptr_glUniform2fv #-}
ptr_glUniform2fv :: FunPtr (GLint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glUniform2fv = unsafePerformIO $ getCommand "glUniform2fv"

-- glUniform2fvARB -------------------------------------------------------------

-- | This command is an alias for 'glUniform2fv'.
glUniform2fvARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @value@ pointing to @count*2@ elements of type @GLfloat@.
  -> m ()
glUniform2fvARB v1 v2 v3 = liftIO $ dyn783 ptr_glUniform2fvARB v1 v2 v3

{-# NOINLINE ptr_glUniform2fvARB #-}
ptr_glUniform2fvARB :: FunPtr (GLint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glUniform2fvARB = unsafePerformIO $ getCommand "glUniform2fvARB"

-- glUniform2i -----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glUniform.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniform2i
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLint -- ^ @v0@.
  -> GLint -- ^ @v1@.
  -> m ()
glUniform2i v1 v2 v3 = liftIO $ dyn42 ptr_glUniform2i v1 v2 v3

{-# NOINLINE ptr_glUniform2i #-}
ptr_glUniform2i :: FunPtr (GLint -> GLint -> GLint -> IO ())
ptr_glUniform2i = unsafePerformIO $ getCommand "glUniform2i"

-- glUniform2i64ARB ------------------------------------------------------------

glUniform2i64ARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLint64 -- ^ @x@.
  -> GLint64 -- ^ @y@.
  -> m ()
glUniform2i64ARB v1 v2 v3 = liftIO $ dyn795 ptr_glUniform2i64ARB v1 v2 v3

{-# NOINLINE ptr_glUniform2i64ARB #-}
ptr_glUniform2i64ARB :: FunPtr (GLint -> GLint64 -> GLint64 -> IO ())
ptr_glUniform2i64ARB = unsafePerformIO $ getCommand "glUniform2i64ARB"

-- glUniform2i64NV -------------------------------------------------------------

glUniform2i64NV
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLint64EXT -- ^ @x@.
  -> GLint64EXT -- ^ @y@.
  -> m ()
glUniform2i64NV v1 v2 v3 = liftIO $ dyn796 ptr_glUniform2i64NV v1 v2 v3

{-# NOINLINE ptr_glUniform2i64NV #-}
ptr_glUniform2i64NV :: FunPtr (GLint -> GLint64EXT -> GLint64EXT -> IO ())
ptr_glUniform2i64NV = unsafePerformIO $ getCommand "glUniform2i64NV"

-- glUniform2i64vARB -----------------------------------------------------------

glUniform2i64vARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint64 -- ^ @value@ pointing to @count*2@ elements of type @GLint64@.
  -> m ()
glUniform2i64vARB v1 v2 v3 = liftIO $ dyn786 ptr_glUniform2i64vARB v1 v2 v3

{-# NOINLINE ptr_glUniform2i64vARB #-}
ptr_glUniform2i64vARB :: FunPtr (GLint -> GLsizei -> Ptr GLint64 -> IO ())
ptr_glUniform2i64vARB = unsafePerformIO $ getCommand "glUniform2i64vARB"

-- glUniform2i64vNV ------------------------------------------------------------

glUniform2i64vNV
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint64EXT -- ^ @value@ pointing to @count*2@ elements of type @GLint64EXT@.
  -> m ()
glUniform2i64vNV v1 v2 v3 = liftIO $ dyn787 ptr_glUniform2i64vNV v1 v2 v3

{-# NOINLINE ptr_glUniform2i64vNV #-}
ptr_glUniform2i64vNV :: FunPtr (GLint -> GLsizei -> Ptr GLint64EXT -> IO ())
ptr_glUniform2i64vNV = unsafePerformIO $ getCommand "glUniform2i64vNV"

-- glUniform2iARB --------------------------------------------------------------

-- | This command is an alias for 'glUniform2i'.
glUniform2iARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLint -- ^ @v0@.
  -> GLint -- ^ @v1@.
  -> m ()
glUniform2iARB v1 v2 v3 = liftIO $ dyn42 ptr_glUniform2iARB v1 v2 v3

{-# NOINLINE ptr_glUniform2iARB #-}
ptr_glUniform2iARB :: FunPtr (GLint -> GLint -> GLint -> IO ())
ptr_glUniform2iARB = unsafePerformIO $ getCommand "glUniform2iARB"

-- glUniform2iv ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glUniform.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniform2iv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint -- ^ @value@ pointing to @count*2@ elements of type @GLint@.
  -> m ()
glUniform2iv v1 v2 v3 = liftIO $ dyn788 ptr_glUniform2iv v1 v2 v3

{-# NOINLINE ptr_glUniform2iv #-}
ptr_glUniform2iv :: FunPtr (GLint -> GLsizei -> Ptr GLint -> IO ())
ptr_glUniform2iv = unsafePerformIO $ getCommand "glUniform2iv"

-- glUniform2ivARB -------------------------------------------------------------

-- | This command is an alias for 'glUniform2iv'.
glUniform2ivARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint -- ^ @value@ pointing to @count*2@ elements of type @GLint@.
  -> m ()
glUniform2ivARB v1 v2 v3 = liftIO $ dyn788 ptr_glUniform2ivARB v1 v2 v3

{-# NOINLINE ptr_glUniform2ivARB #-}
ptr_glUniform2ivARB :: FunPtr (GLint -> GLsizei -> Ptr GLint -> IO ())
ptr_glUniform2ivARB = unsafePerformIO $ getCommand "glUniform2ivARB"

-- glUniform2ui ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniform2ui
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLuint -- ^ @v0@.
  -> GLuint -- ^ @v1@.
  -> m ()
glUniform2ui v1 v2 v3 = liftIO $ dyn797 ptr_glUniform2ui v1 v2 v3

{-# NOINLINE ptr_glUniform2ui #-}
ptr_glUniform2ui :: FunPtr (GLint -> GLuint -> GLuint -> IO ())
ptr_glUniform2ui = unsafePerformIO $ getCommand "glUniform2ui"

-- glUniform2ui64ARB -----------------------------------------------------------

glUniform2ui64ARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLuint64 -- ^ @x@.
  -> GLuint64 -- ^ @y@.
  -> m ()
glUniform2ui64ARB v1 v2 v3 = liftIO $ dyn798 ptr_glUniform2ui64ARB v1 v2 v3

{-# NOINLINE ptr_glUniform2ui64ARB #-}
ptr_glUniform2ui64ARB :: FunPtr (GLint -> GLuint64 -> GLuint64 -> IO ())
ptr_glUniform2ui64ARB = unsafePerformIO $ getCommand "glUniform2ui64ARB"

-- glUniform2ui64NV ------------------------------------------------------------

glUniform2ui64NV
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLuint64EXT -- ^ @x@.
  -> GLuint64EXT -- ^ @y@.
  -> m ()
glUniform2ui64NV v1 v2 v3 = liftIO $ dyn799 ptr_glUniform2ui64NV v1 v2 v3

{-# NOINLINE ptr_glUniform2ui64NV #-}
ptr_glUniform2ui64NV :: FunPtr (GLint -> GLuint64EXT -> GLuint64EXT -> IO ())
ptr_glUniform2ui64NV = unsafePerformIO $ getCommand "glUniform2ui64NV"

-- glUniform2ui64vARB ----------------------------------------------------------

glUniform2ui64vARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint64 -- ^ @value@ pointing to @count*2@ elements of type @GLuint64@.
  -> m ()
glUniform2ui64vARB v1 v2 v3 = liftIO $ dyn792 ptr_glUniform2ui64vARB v1 v2 v3

{-# NOINLINE ptr_glUniform2ui64vARB #-}
ptr_glUniform2ui64vARB :: FunPtr (GLint -> GLsizei -> Ptr GLuint64 -> IO ())
ptr_glUniform2ui64vARB = unsafePerformIO $ getCommand "glUniform2ui64vARB"

-- glUniform2ui64vNV -----------------------------------------------------------

glUniform2ui64vNV
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint64EXT -- ^ @value@ pointing to @count*2@ elements of type @GLuint64EXT@.
  -> m ()
glUniform2ui64vNV v1 v2 v3 = liftIO $ dyn793 ptr_glUniform2ui64vNV v1 v2 v3

{-# NOINLINE ptr_glUniform2ui64vNV #-}
ptr_glUniform2ui64vNV :: FunPtr (GLint -> GLsizei -> Ptr GLuint64EXT -> IO ())
ptr_glUniform2ui64vNV = unsafePerformIO $ getCommand "glUniform2ui64vNV"

-- glUniform2uiEXT -------------------------------------------------------------

-- | This command is an alias for 'glUniform2ui'.
glUniform2uiEXT
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLuint -- ^ @v0@.
  -> GLuint -- ^ @v1@.
  -> m ()
glUniform2uiEXT v1 v2 v3 = liftIO $ dyn797 ptr_glUniform2uiEXT v1 v2 v3

{-# NOINLINE ptr_glUniform2uiEXT #-}
ptr_glUniform2uiEXT :: FunPtr (GLint -> GLuint -> GLuint -> IO ())
ptr_glUniform2uiEXT = unsafePerformIO $ getCommand "glUniform2uiEXT"

-- glUniform2uiv ---------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniform2uiv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @value@ pointing to @count*2@ elements of type @GLuint@.
  -> m ()
glUniform2uiv v1 v2 v3 = liftIO $ dyn794 ptr_glUniform2uiv v1 v2 v3

{-# NOINLINE ptr_glUniform2uiv #-}
ptr_glUniform2uiv :: FunPtr (GLint -> GLsizei -> Ptr GLuint -> IO ())
ptr_glUniform2uiv = unsafePerformIO $ getCommand "glUniform2uiv"

-- glUniform2uivEXT ------------------------------------------------------------

-- | This command is an alias for 'glUniform2uiv'.
glUniform2uivEXT
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @value@ pointing to @count*2@ elements of type @GLuint@.
  -> m ()
glUniform2uivEXT v1 v2 v3 = liftIO $ dyn794 ptr_glUniform2uivEXT v1 v2 v3

{-# NOINLINE ptr_glUniform2uivEXT #-}
ptr_glUniform2uivEXT :: FunPtr (GLint -> GLsizei -> Ptr GLuint -> IO ())
ptr_glUniform2uivEXT = unsafePerformIO $ getCommand "glUniform2uivEXT"

-- glUniform3d -----------------------------------------------------------------

glUniform3d
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> GLdouble -- ^ @z@.
  -> m ()
glUniform3d v1 v2 v3 v4 = liftIO $ dyn800 ptr_glUniform3d v1 v2 v3 v4

{-# NOINLINE ptr_glUniform3d #-}
ptr_glUniform3d :: FunPtr (GLint -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glUniform3d = unsafePerformIO $ getCommand "glUniform3d"

-- glUniform3dv ----------------------------------------------------------------

glUniform3dv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLdouble -- ^ @value@ pointing to @count*3@ elements of type @GLdouble@.
  -> m ()
glUniform3dv v1 v2 v3 = liftIO $ dyn781 ptr_glUniform3dv v1 v2 v3

{-# NOINLINE ptr_glUniform3dv #-}
ptr_glUniform3dv :: FunPtr (GLint -> GLsizei -> Ptr GLdouble -> IO ())
ptr_glUniform3dv = unsafePerformIO $ getCommand "glUniform3dv"

-- glUniform3f -----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glUniform.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniform3f
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLfloat -- ^ @v0@.
  -> GLfloat -- ^ @v1@.
  -> GLfloat -- ^ @v2@.
  -> m ()
glUniform3f v1 v2 v3 v4 = liftIO $ dyn801 ptr_glUniform3f v1 v2 v3 v4

{-# NOINLINE ptr_glUniform3f #-}
ptr_glUniform3f :: FunPtr (GLint -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glUniform3f = unsafePerformIO $ getCommand "glUniform3f"

-- glUniform3fARB --------------------------------------------------------------

-- | This command is an alias for 'glUniform3f'.
glUniform3fARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLfloat -- ^ @v0@.
  -> GLfloat -- ^ @v1@.
  -> GLfloat -- ^ @v2@.
  -> m ()
glUniform3fARB v1 v2 v3 v4 = liftIO $ dyn801 ptr_glUniform3fARB v1 v2 v3 v4

{-# NOINLINE ptr_glUniform3fARB #-}
ptr_glUniform3fARB :: FunPtr (GLint -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glUniform3fARB = unsafePerformIO $ getCommand "glUniform3fARB"

-- glUniform3fv ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glUniform.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniform3fv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @value@ pointing to @count*3@ elements of type @GLfloat@.
  -> m ()
glUniform3fv v1 v2 v3 = liftIO $ dyn783 ptr_glUniform3fv v1 v2 v3

{-# NOINLINE ptr_glUniform3fv #-}
ptr_glUniform3fv :: FunPtr (GLint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glUniform3fv = unsafePerformIO $ getCommand "glUniform3fv"

-- glUniform3fvARB -------------------------------------------------------------

-- | This command is an alias for 'glUniform3fv'.
glUniform3fvARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @value@ pointing to @count*3@ elements of type @GLfloat@.
  -> m ()
glUniform3fvARB v1 v2 v3 = liftIO $ dyn783 ptr_glUniform3fvARB v1 v2 v3

{-# NOINLINE ptr_glUniform3fvARB #-}
ptr_glUniform3fvARB :: FunPtr (GLint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glUniform3fvARB = unsafePerformIO $ getCommand "glUniform3fvARB"

-- glUniform3i -----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glUniform.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniform3i
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLint -- ^ @v0@.
  -> GLint -- ^ @v1@.
  -> GLint -- ^ @v2@.
  -> m ()
glUniform3i v1 v2 v3 v4 = liftIO $ dyn76 ptr_glUniform3i v1 v2 v3 v4

{-# NOINLINE ptr_glUniform3i #-}
ptr_glUniform3i :: FunPtr (GLint -> GLint -> GLint -> GLint -> IO ())
ptr_glUniform3i = unsafePerformIO $ getCommand "glUniform3i"

-- glUniform3i64ARB ------------------------------------------------------------

glUniform3i64ARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLint64 -- ^ @x@.
  -> GLint64 -- ^ @y@.
  -> GLint64 -- ^ @z@.
  -> m ()
glUniform3i64ARB v1 v2 v3 v4 = liftIO $ dyn802 ptr_glUniform3i64ARB v1 v2 v3 v4

{-# NOINLINE ptr_glUniform3i64ARB #-}
ptr_glUniform3i64ARB :: FunPtr (GLint -> GLint64 -> GLint64 -> GLint64 -> IO ())
ptr_glUniform3i64ARB = unsafePerformIO $ getCommand "glUniform3i64ARB"

-- glUniform3i64NV -------------------------------------------------------------

glUniform3i64NV
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLint64EXT -- ^ @x@.
  -> GLint64EXT -- ^ @y@.
  -> GLint64EXT -- ^ @z@.
  -> m ()
glUniform3i64NV v1 v2 v3 v4 = liftIO $ dyn803 ptr_glUniform3i64NV v1 v2 v3 v4

{-# NOINLINE ptr_glUniform3i64NV #-}
ptr_glUniform3i64NV :: FunPtr (GLint -> GLint64EXT -> GLint64EXT -> GLint64EXT -> IO ())
ptr_glUniform3i64NV = unsafePerformIO $ getCommand "glUniform3i64NV"

-- glUniform3i64vARB -----------------------------------------------------------

glUniform3i64vARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint64 -- ^ @value@ pointing to @count*3@ elements of type @GLint64@.
  -> m ()
glUniform3i64vARB v1 v2 v3 = liftIO $ dyn786 ptr_glUniform3i64vARB v1 v2 v3

{-# NOINLINE ptr_glUniform3i64vARB #-}
ptr_glUniform3i64vARB :: FunPtr (GLint -> GLsizei -> Ptr GLint64 -> IO ())
ptr_glUniform3i64vARB = unsafePerformIO $ getCommand "glUniform3i64vARB"

-- glUniform3i64vNV ------------------------------------------------------------

glUniform3i64vNV
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint64EXT -- ^ @value@ pointing to @count*3@ elements of type @GLint64EXT@.
  -> m ()
glUniform3i64vNV v1 v2 v3 = liftIO $ dyn787 ptr_glUniform3i64vNV v1 v2 v3

{-# NOINLINE ptr_glUniform3i64vNV #-}
ptr_glUniform3i64vNV :: FunPtr (GLint -> GLsizei -> Ptr GLint64EXT -> IO ())
ptr_glUniform3i64vNV = unsafePerformIO $ getCommand "glUniform3i64vNV"

-- glUniform3iARB --------------------------------------------------------------

-- | This command is an alias for 'glUniform3i'.
glUniform3iARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLint -- ^ @v0@.
  -> GLint -- ^ @v1@.
  -> GLint -- ^ @v2@.
  -> m ()
glUniform3iARB v1 v2 v3 v4 = liftIO $ dyn76 ptr_glUniform3iARB v1 v2 v3 v4

{-# NOINLINE ptr_glUniform3iARB #-}
ptr_glUniform3iARB :: FunPtr (GLint -> GLint -> GLint -> GLint -> IO ())
ptr_glUniform3iARB = unsafePerformIO $ getCommand "glUniform3iARB"

-- glUniform3iv ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glUniform.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniform3iv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint -- ^ @value@ pointing to @count*3@ elements of type @GLint@.
  -> m ()
glUniform3iv v1 v2 v3 = liftIO $ dyn788 ptr_glUniform3iv v1 v2 v3

{-# NOINLINE ptr_glUniform3iv #-}
ptr_glUniform3iv :: FunPtr (GLint -> GLsizei -> Ptr GLint -> IO ())
ptr_glUniform3iv = unsafePerformIO $ getCommand "glUniform3iv"

