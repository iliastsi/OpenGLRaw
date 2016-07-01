--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F67
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

module Graphics.GL.Functions.F67 (
  glUniformMatrix2x3dv,
  glUniformMatrix2x3fv,
  glUniformMatrix2x3fvNV,
  glUniformMatrix2x4dv,
  glUniformMatrix2x4fv,
  glUniformMatrix2x4fvNV,
  glUniformMatrix3dv,
  glUniformMatrix3fv,
  glUniformMatrix3fvARB,
  glUniformMatrix3x2dv,
  glUniformMatrix3x2fv,
  glUniformMatrix3x2fvNV,
  glUniformMatrix3x4dv,
  glUniformMatrix3x4fv,
  glUniformMatrix3x4fvNV,
  glUniformMatrix4dv,
  glUniformMatrix4fv,
  glUniformMatrix4fvARB,
  glUniformMatrix4x2dv,
  glUniformMatrix4x2fv,
  glUniformMatrix4x2fvNV,
  glUniformMatrix4x3dv,
  glUniformMatrix4x3fv,
  glUniformMatrix4x3fvNV,
  glUniformSubroutinesuiv,
  glUniformui64NV,
  glUniformui64vNV,
  glUnlockArraysEXT,
  glUnmapBuffer,
  glUnmapBufferARB,
  glUnmapBufferOES,
  glUnmapNamedBuffer,
  glUnmapNamedBufferEXT,
  glUnmapObjectBufferATI,
  glUnmapTexture2DINTEL,
  glUpdateObjectBufferATI,
  glUseProgram,
  glUseProgramObjectARB,
  glUseProgramStages,
  glUseProgramStagesEXT
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

-- glUniformMatrix2x3dv --------------------------------------------------------

glUniformMatrix2x3dv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLdouble -- ^ @value@ pointing to @count*6@ elements of type @GLdouble@.
  -> m ()
glUniformMatrix2x3dv v1 v2 v3 v4 = liftIO $ dyn814 ptr_glUniformMatrix2x3dv v1 v2 v3 v4

{-# NOINLINE ptr_glUniformMatrix2x3dv #-}
ptr_glUniformMatrix2x3dv :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
ptr_glUniformMatrix2x3dv = unsafePerformIO $ getCommand "glUniformMatrix2x3dv"

-- glUniformMatrix2x3fv --------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glUniform.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniformMatrix2x3fv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*6@ elements of type @GLfloat@.
  -> m ()
glUniformMatrix2x3fv v1 v2 v3 v4 = liftIO $ dyn815 ptr_glUniformMatrix2x3fv v1 v2 v3 v4

{-# NOINLINE ptr_glUniformMatrix2x3fv #-}
ptr_glUniformMatrix2x3fv :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glUniformMatrix2x3fv = unsafePerformIO $ getCommand "glUniformMatrix2x3fv"

-- glUniformMatrix2x3fvNV ------------------------------------------------------

-- | This command is an alias for 'glUniformMatrix2x3fv'.
glUniformMatrix2x3fvNV
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*6@ elements of type @GLfloat@.
  -> m ()
glUniformMatrix2x3fvNV v1 v2 v3 v4 = liftIO $ dyn815 ptr_glUniformMatrix2x3fvNV v1 v2 v3 v4

{-# NOINLINE ptr_glUniformMatrix2x3fvNV #-}
ptr_glUniformMatrix2x3fvNV :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glUniformMatrix2x3fvNV = unsafePerformIO $ getCommand "glUniformMatrix2x3fvNV"

-- glUniformMatrix2x4dv --------------------------------------------------------

glUniformMatrix2x4dv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLdouble -- ^ @value@ pointing to @count*8@ elements of type @GLdouble@.
  -> m ()
glUniformMatrix2x4dv v1 v2 v3 v4 = liftIO $ dyn814 ptr_glUniformMatrix2x4dv v1 v2 v3 v4

{-# NOINLINE ptr_glUniformMatrix2x4dv #-}
ptr_glUniformMatrix2x4dv :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
ptr_glUniformMatrix2x4dv = unsafePerformIO $ getCommand "glUniformMatrix2x4dv"

-- glUniformMatrix2x4fv --------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glUniform.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniformMatrix2x4fv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*8@ elements of type @GLfloat@.
  -> m ()
glUniformMatrix2x4fv v1 v2 v3 v4 = liftIO $ dyn815 ptr_glUniformMatrix2x4fv v1 v2 v3 v4

{-# NOINLINE ptr_glUniformMatrix2x4fv #-}
ptr_glUniformMatrix2x4fv :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glUniformMatrix2x4fv = unsafePerformIO $ getCommand "glUniformMatrix2x4fv"

-- glUniformMatrix2x4fvNV ------------------------------------------------------

-- | This command is an alias for 'glUniformMatrix2x4fv'.
glUniformMatrix2x4fvNV
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*8@ elements of type @GLfloat@.
  -> m ()
glUniformMatrix2x4fvNV v1 v2 v3 v4 = liftIO $ dyn815 ptr_glUniformMatrix2x4fvNV v1 v2 v3 v4

{-# NOINLINE ptr_glUniformMatrix2x4fvNV #-}
ptr_glUniformMatrix2x4fvNV :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glUniformMatrix2x4fvNV = unsafePerformIO $ getCommand "glUniformMatrix2x4fvNV"

-- glUniformMatrix3dv ----------------------------------------------------------

glUniformMatrix3dv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLdouble -- ^ @value@ pointing to @count*9@ elements of type @GLdouble@.
  -> m ()
glUniformMatrix3dv v1 v2 v3 v4 = liftIO $ dyn814 ptr_glUniformMatrix3dv v1 v2 v3 v4

{-# NOINLINE ptr_glUniformMatrix3dv #-}
ptr_glUniformMatrix3dv :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
ptr_glUniformMatrix3dv = unsafePerformIO $ getCommand "glUniformMatrix3dv"

-- glUniformMatrix3fv ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glUniform.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniformMatrix3fv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*9@ elements of type @GLfloat@.
  -> m ()
glUniformMatrix3fv v1 v2 v3 v4 = liftIO $ dyn815 ptr_glUniformMatrix3fv v1 v2 v3 v4

{-# NOINLINE ptr_glUniformMatrix3fv #-}
ptr_glUniformMatrix3fv :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glUniformMatrix3fv = unsafePerformIO $ getCommand "glUniformMatrix3fv"

-- glUniformMatrix3fvARB -------------------------------------------------------

-- | This command is an alias for 'glUniformMatrix3fv'.
glUniformMatrix3fvARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*9@ elements of type @GLfloat@.
  -> m ()
glUniformMatrix3fvARB v1 v2 v3 v4 = liftIO $ dyn815 ptr_glUniformMatrix3fvARB v1 v2 v3 v4

{-# NOINLINE ptr_glUniformMatrix3fvARB #-}
ptr_glUniformMatrix3fvARB :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glUniformMatrix3fvARB = unsafePerformIO $ getCommand "glUniformMatrix3fvARB"

-- glUniformMatrix3x2dv --------------------------------------------------------

glUniformMatrix3x2dv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLdouble -- ^ @value@ pointing to @count*6@ elements of type @GLdouble@.
  -> m ()
glUniformMatrix3x2dv v1 v2 v3 v4 = liftIO $ dyn814 ptr_glUniformMatrix3x2dv v1 v2 v3 v4

{-# NOINLINE ptr_glUniformMatrix3x2dv #-}
ptr_glUniformMatrix3x2dv :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
ptr_glUniformMatrix3x2dv = unsafePerformIO $ getCommand "glUniformMatrix3x2dv"

-- glUniformMatrix3x2fv --------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glUniform.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniformMatrix3x2fv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*6@ elements of type @GLfloat@.
  -> m ()
glUniformMatrix3x2fv v1 v2 v3 v4 = liftIO $ dyn815 ptr_glUniformMatrix3x2fv v1 v2 v3 v4

{-# NOINLINE ptr_glUniformMatrix3x2fv #-}
ptr_glUniformMatrix3x2fv :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glUniformMatrix3x2fv = unsafePerformIO $ getCommand "glUniformMatrix3x2fv"

-- glUniformMatrix3x2fvNV ------------------------------------------------------

-- | This command is an alias for 'glUniformMatrix3x2fv'.
glUniformMatrix3x2fvNV
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*6@ elements of type @GLfloat@.
  -> m ()
glUniformMatrix3x2fvNV v1 v2 v3 v4 = liftIO $ dyn815 ptr_glUniformMatrix3x2fvNV v1 v2 v3 v4

{-# NOINLINE ptr_glUniformMatrix3x2fvNV #-}
ptr_glUniformMatrix3x2fvNV :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glUniformMatrix3x2fvNV = unsafePerformIO $ getCommand "glUniformMatrix3x2fvNV"

-- glUniformMatrix3x4dv --------------------------------------------------------

glUniformMatrix3x4dv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLdouble -- ^ @value@ pointing to @count*12@ elements of type @GLdouble@.
  -> m ()
glUniformMatrix3x4dv v1 v2 v3 v4 = liftIO $ dyn814 ptr_glUniformMatrix3x4dv v1 v2 v3 v4

{-# NOINLINE ptr_glUniformMatrix3x4dv #-}
ptr_glUniformMatrix3x4dv :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
ptr_glUniformMatrix3x4dv = unsafePerformIO $ getCommand "glUniformMatrix3x4dv"

-- glUniformMatrix3x4fv --------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glUniform.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniformMatrix3x4fv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*12@ elements of type @GLfloat@.
  -> m ()
glUniformMatrix3x4fv v1 v2 v3 v4 = liftIO $ dyn815 ptr_glUniformMatrix3x4fv v1 v2 v3 v4

{-# NOINLINE ptr_glUniformMatrix3x4fv #-}
ptr_glUniformMatrix3x4fv :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glUniformMatrix3x4fv = unsafePerformIO $ getCommand "glUniformMatrix3x4fv"

-- glUniformMatrix3x4fvNV ------------------------------------------------------

-- | This command is an alias for 'glUniformMatrix3x4fv'.
glUniformMatrix3x4fvNV
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*12@ elements of type @GLfloat@.
  -> m ()
glUniformMatrix3x4fvNV v1 v2 v3 v4 = liftIO $ dyn815 ptr_glUniformMatrix3x4fvNV v1 v2 v3 v4

{-# NOINLINE ptr_glUniformMatrix3x4fvNV #-}
ptr_glUniformMatrix3x4fvNV :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glUniformMatrix3x4fvNV = unsafePerformIO $ getCommand "glUniformMatrix3x4fvNV"

-- glUniformMatrix4dv ----------------------------------------------------------

glUniformMatrix4dv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLdouble -- ^ @value@ pointing to @count*16@ elements of type @GLdouble@.
  -> m ()
glUniformMatrix4dv v1 v2 v3 v4 = liftIO $ dyn814 ptr_glUniformMatrix4dv v1 v2 v3 v4

{-# NOINLINE ptr_glUniformMatrix4dv #-}
ptr_glUniformMatrix4dv :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
ptr_glUniformMatrix4dv = unsafePerformIO $ getCommand "glUniformMatrix4dv"

-- glUniformMatrix4fv ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glUniform.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniformMatrix4fv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*16@ elements of type @GLfloat@.
  -> m ()
glUniformMatrix4fv v1 v2 v3 v4 = liftIO $ dyn815 ptr_glUniformMatrix4fv v1 v2 v3 v4

{-# NOINLINE ptr_glUniformMatrix4fv #-}
ptr_glUniformMatrix4fv :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glUniformMatrix4fv = unsafePerformIO $ getCommand "glUniformMatrix4fv"

-- glUniformMatrix4fvARB -------------------------------------------------------

-- | This command is an alias for 'glUniformMatrix4fv'.
glUniformMatrix4fvARB
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*16@ elements of type @GLfloat@.
  -> m ()
glUniformMatrix4fvARB v1 v2 v3 v4 = liftIO $ dyn815 ptr_glUniformMatrix4fvARB v1 v2 v3 v4

{-# NOINLINE ptr_glUniformMatrix4fvARB #-}
ptr_glUniformMatrix4fvARB :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glUniformMatrix4fvARB = unsafePerformIO $ getCommand "glUniformMatrix4fvARB"

-- glUniformMatrix4x2dv --------------------------------------------------------

glUniformMatrix4x2dv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLdouble -- ^ @value@ pointing to @count*8@ elements of type @GLdouble@.
  -> m ()
glUniformMatrix4x2dv v1 v2 v3 v4 = liftIO $ dyn814 ptr_glUniformMatrix4x2dv v1 v2 v3 v4

{-# NOINLINE ptr_glUniformMatrix4x2dv #-}
ptr_glUniformMatrix4x2dv :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
ptr_glUniformMatrix4x2dv = unsafePerformIO $ getCommand "glUniformMatrix4x2dv"

-- glUniformMatrix4x2fv --------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glUniform.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniformMatrix4x2fv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*8@ elements of type @GLfloat@.
  -> m ()
glUniformMatrix4x2fv v1 v2 v3 v4 = liftIO $ dyn815 ptr_glUniformMatrix4x2fv v1 v2 v3 v4

{-# NOINLINE ptr_glUniformMatrix4x2fv #-}
ptr_glUniformMatrix4x2fv :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glUniformMatrix4x2fv = unsafePerformIO $ getCommand "glUniformMatrix4x2fv"

-- glUniformMatrix4x2fvNV ------------------------------------------------------

-- | This command is an alias for 'glUniformMatrix4x2fv'.
glUniformMatrix4x2fvNV
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*8@ elements of type @GLfloat@.
  -> m ()
glUniformMatrix4x2fvNV v1 v2 v3 v4 = liftIO $ dyn815 ptr_glUniformMatrix4x2fvNV v1 v2 v3 v4

{-# NOINLINE ptr_glUniformMatrix4x2fvNV #-}
ptr_glUniformMatrix4x2fvNV :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glUniformMatrix4x2fvNV = unsafePerformIO $ getCommand "glUniformMatrix4x2fvNV"

-- glUniformMatrix4x3dv --------------------------------------------------------

glUniformMatrix4x3dv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLdouble -- ^ @value@ pointing to @count*12@ elements of type @GLdouble@.
  -> m ()
glUniformMatrix4x3dv v1 v2 v3 v4 = liftIO $ dyn814 ptr_glUniformMatrix4x3dv v1 v2 v3 v4

{-# NOINLINE ptr_glUniformMatrix4x3dv #-}
ptr_glUniformMatrix4x3dv :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
ptr_glUniformMatrix4x3dv = unsafePerformIO $ getCommand "glUniformMatrix4x3dv"

-- glUniformMatrix4x3fv --------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glUniform.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUniform.xhtml OpenGL 4.x>.
glUniformMatrix4x3fv
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*12@ elements of type @GLfloat@.
  -> m ()
glUniformMatrix4x3fv v1 v2 v3 v4 = liftIO $ dyn815 ptr_glUniformMatrix4x3fv v1 v2 v3 v4

{-# NOINLINE ptr_glUniformMatrix4x3fv #-}
ptr_glUniformMatrix4x3fv :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glUniformMatrix4x3fv = unsafePerformIO $ getCommand "glUniformMatrix4x3fv"

-- glUniformMatrix4x3fvNV ------------------------------------------------------

-- | This command is an alias for 'glUniformMatrix4x3fv'.
glUniformMatrix4x3fvNV
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*12@ elements of type @GLfloat@.
  -> m ()
glUniformMatrix4x3fvNV v1 v2 v3 v4 = liftIO $ dyn815 ptr_glUniformMatrix4x3fvNV v1 v2 v3 v4

{-# NOINLINE ptr_glUniformMatrix4x3fvNV #-}
ptr_glUniformMatrix4x3fvNV :: FunPtr (GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glUniformMatrix4x3fvNV = unsafePerformIO $ getCommand "glUniformMatrix4x3fvNV"

-- glUniformSubroutinesuiv -----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glUniformSubroutines.xhtml OpenGL 4.x>.
glUniformSubroutinesuiv
  :: MonadIO m
  => GLenum -- ^ @shadertype@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @indices@ pointing to @count@ elements of type @GLuint@.
  -> m ()
glUniformSubroutinesuiv v1 v2 v3 = liftIO $ dyn197 ptr_glUniformSubroutinesuiv v1 v2 v3

{-# NOINLINE ptr_glUniformSubroutinesuiv #-}
ptr_glUniformSubroutinesuiv :: FunPtr (GLenum -> GLsizei -> Ptr GLuint -> IO ())
ptr_glUniformSubroutinesuiv = unsafePerformIO $ getCommand "glUniformSubroutinesuiv"

-- glUniformui64NV -------------------------------------------------------------

glUniformui64NV
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLuint64EXT -- ^ @value@.
  -> m ()
glUniformui64NV v1 v2 = liftIO $ dyn791 ptr_glUniformui64NV v1 v2

{-# NOINLINE ptr_glUniformui64NV #-}
ptr_glUniformui64NV :: FunPtr (GLint -> GLuint64EXT -> IO ())
ptr_glUniformui64NV = unsafePerformIO $ getCommand "glUniformui64NV"

-- glUniformui64vNV ------------------------------------------------------------

glUniformui64vNV
  :: MonadIO m
  => GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint64EXT -- ^ @value@ pointing to @count*1@ elements of type @GLuint64EXT@.
  -> m ()
glUniformui64vNV v1 v2 v3 = liftIO $ dyn793 ptr_glUniformui64vNV v1 v2 v3

{-# NOINLINE ptr_glUniformui64vNV #-}
ptr_glUniformui64vNV :: FunPtr (GLint -> GLsizei -> Ptr GLuint64EXT -> IO ())
ptr_glUniformui64vNV = unsafePerformIO $ getCommand "glUniformui64vNV"

-- glUnlockArraysEXT -----------------------------------------------------------

glUnlockArraysEXT
  :: MonadIO m
  => m ()
glUnlockArraysEXT = liftIO $ dyn10 ptr_glUnlockArraysEXT

{-# NOINLINE ptr_glUnlockArraysEXT #-}
ptr_glUnlockArraysEXT :: FunPtr (IO ())
ptr_glUnlockArraysEXT = unsafePerformIO $ getCommand "glUnlockArraysEXT"

-- glUnmapBuffer ---------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glMapBuffer.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glMapBuffer.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUnmapBuffer.xhtml OpenGL 4.x>.
glUnmapBuffer
  :: MonadIO m
  => GLenum -- ^ @target@ of type @BufferTargetARB@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glUnmapBuffer v1 = liftIO $ dyn476 ptr_glUnmapBuffer v1

{-# NOINLINE ptr_glUnmapBuffer #-}
ptr_glUnmapBuffer :: FunPtr (GLenum -> IO GLboolean)
ptr_glUnmapBuffer = unsafePerformIO $ getCommand "glUnmapBuffer"

-- glUnmapBufferARB ------------------------------------------------------------

-- | This command is an alias for 'glUnmapBuffer'.
glUnmapBufferARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @BufferTargetARB@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glUnmapBufferARB v1 = liftIO $ dyn476 ptr_glUnmapBufferARB v1

{-# NOINLINE ptr_glUnmapBufferARB #-}
ptr_glUnmapBufferARB :: FunPtr (GLenum -> IO GLboolean)
ptr_glUnmapBufferARB = unsafePerformIO $ getCommand "glUnmapBufferARB"

-- glUnmapBufferOES ------------------------------------------------------------

-- | This command is an alias for 'glUnmapBuffer'.
glUnmapBufferOES
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> m GLboolean
glUnmapBufferOES v1 = liftIO $ dyn476 ptr_glUnmapBufferOES v1

{-# NOINLINE ptr_glUnmapBufferOES #-}
ptr_glUnmapBufferOES :: FunPtr (GLenum -> IO GLboolean)
ptr_glUnmapBufferOES = unsafePerformIO $ getCommand "glUnmapBufferOES"

-- glUnmapNamedBuffer ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glUnmapBuffer.xhtml OpenGL 4.x>.
glUnmapNamedBuffer
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> m GLboolean
glUnmapNamedBuffer v1 = liftIO $ dyn273 ptr_glUnmapNamedBuffer v1

{-# NOINLINE ptr_glUnmapNamedBuffer #-}
ptr_glUnmapNamedBuffer :: FunPtr (GLuint -> IO GLboolean)
ptr_glUnmapNamedBuffer = unsafePerformIO $ getCommand "glUnmapNamedBuffer"

-- glUnmapNamedBufferEXT -------------------------------------------------------

glUnmapNamedBufferEXT
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glUnmapNamedBufferEXT v1 = liftIO $ dyn273 ptr_glUnmapNamedBufferEXT v1

{-# NOINLINE ptr_glUnmapNamedBufferEXT #-}
ptr_glUnmapNamedBufferEXT :: FunPtr (GLuint -> IO GLboolean)
ptr_glUnmapNamedBufferEXT = unsafePerformIO $ getCommand "glUnmapNamedBufferEXT"

-- glUnmapObjectBufferATI ------------------------------------------------------

glUnmapObjectBufferATI
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> m ()
glUnmapObjectBufferATI v1 = liftIO $ dyn2 ptr_glUnmapObjectBufferATI v1

{-# NOINLINE ptr_glUnmapObjectBufferATI #-}
ptr_glUnmapObjectBufferATI :: FunPtr (GLuint -> IO ())
ptr_glUnmapObjectBufferATI = unsafePerformIO $ getCommand "glUnmapObjectBufferATI"

-- glUnmapTexture2DINTEL -------------------------------------------------------

glUnmapTexture2DINTEL
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> m ()
glUnmapTexture2DINTEL v1 v2 = liftIO $ dyn474 ptr_glUnmapTexture2DINTEL v1 v2

{-# NOINLINE ptr_glUnmapTexture2DINTEL #-}
ptr_glUnmapTexture2DINTEL :: FunPtr (GLuint -> GLint -> IO ())
ptr_glUnmapTexture2DINTEL = unsafePerformIO $ getCommand "glUnmapTexture2DINTEL"

-- glUpdateObjectBufferATI -----------------------------------------------------

glUpdateObjectBufferATI
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLuint -- ^ @offset@.
  -> GLsizei -- ^ @size@.
  -> Ptr a -- ^ @pointer@ pointing to @size@ elements of type @a@.
  -> GLenum -- ^ @preserve@ of type @PreserveModeATI@.
  -> m ()
glUpdateObjectBufferATI v1 v2 v3 v4 v5 = liftIO $ dyn816 ptr_glUpdateObjectBufferATI v1 v2 v3 v4 v5

{-# NOINLINE ptr_glUpdateObjectBufferATI #-}
ptr_glUpdateObjectBufferATI :: FunPtr (GLuint -> GLuint -> GLsizei -> Ptr a -> GLenum -> IO ())
ptr_glUpdateObjectBufferATI = unsafePerformIO $ getCommand "glUpdateObjectBufferATI"

-- glUseProgram ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glUseProgram.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glUseProgram.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glUseProgram.xhtml OpenGL 4.x>.
glUseProgram
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> m ()
glUseProgram v1 = liftIO $ dyn2 ptr_glUseProgram v1

{-# NOINLINE ptr_glUseProgram #-}
ptr_glUseProgram :: FunPtr (GLuint -> IO ())
ptr_glUseProgram = unsafePerformIO $ getCommand "glUseProgram"

-- glUseProgramObjectARB -------------------------------------------------------

-- | This command is an alias for 'glUseProgram'.
glUseProgramObjectARB
  :: MonadIO m
  => GLhandleARB -- ^ @programObj@ of type @handleARB@.
  -> m ()
glUseProgramObjectARB v1 = liftIO $ dyn137 ptr_glUseProgramObjectARB v1

{-# NOINLINE ptr_glUseProgramObjectARB #-}
ptr_glUseProgramObjectARB :: FunPtr (GLhandleARB -> IO ())
ptr_glUseProgramObjectARB = unsafePerformIO $ getCommand "glUseProgramObjectARB"

-- glUseProgramStages ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glUseProgramStages.xhtml OpenGL 4.x>.
glUseProgramStages
  :: MonadIO m
  => GLuint -- ^ @pipeline@.
  -> GLbitfield -- ^ @stages@.
  -> GLuint -- ^ @program@.
  -> m ()
glUseProgramStages v1 v2 v3 = liftIO $ dyn817 ptr_glUseProgramStages v1 v2 v3

{-# NOINLINE ptr_glUseProgramStages #-}
ptr_glUseProgramStages :: FunPtr (GLuint -> GLbitfield -> GLuint -> IO ())
ptr_glUseProgramStages = unsafePerformIO $ getCommand "glUseProgramStages"

-- glUseProgramStagesEXT -------------------------------------------------------

glUseProgramStagesEXT
  :: MonadIO m
  => GLuint -- ^ @pipeline@.
  -> GLbitfield -- ^ @stages@.
  -> GLuint -- ^ @program@.
  -> m ()
glUseProgramStagesEXT v1 v2 v3 = liftIO $ dyn817 ptr_glUseProgramStagesEXT v1 v2 v3

{-# NOINLINE ptr_glUseProgramStagesEXT #-}
ptr_glUseProgramStagesEXT :: FunPtr (GLuint -> GLbitfield -> GLuint -> IO ())
ptr_glUseProgramStagesEXT = unsafePerformIO $ getCommand "glUseProgramStagesEXT"

