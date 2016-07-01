--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F52
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

module Graphics.GL.Functions.F52 (
  glProgramUniformMatrix3dv,
  glProgramUniformMatrix3dvEXT,
  glProgramUniformMatrix3fv,
  glProgramUniformMatrix3fvEXT,
  glProgramUniformMatrix3x2dv,
  glProgramUniformMatrix3x2dvEXT,
  glProgramUniformMatrix3x2fv,
  glProgramUniformMatrix3x2fvEXT,
  glProgramUniformMatrix3x4dv,
  glProgramUniformMatrix3x4dvEXT,
  glProgramUniformMatrix3x4fv,
  glProgramUniformMatrix3x4fvEXT,
  glProgramUniformMatrix4dv,
  glProgramUniformMatrix4dvEXT,
  glProgramUniformMatrix4fv,
  glProgramUniformMatrix4fvEXT,
  glProgramUniformMatrix4x2dv,
  glProgramUniformMatrix4x2dvEXT,
  glProgramUniformMatrix4x2fv,
  glProgramUniformMatrix4x2fvEXT,
  glProgramUniformMatrix4x3dv,
  glProgramUniformMatrix4x3dvEXT,
  glProgramUniformMatrix4x3fv,
  glProgramUniformMatrix4x3fvEXT,
  glProgramUniformui64NV,
  glProgramUniformui64vNV,
  glProgramVertexLimitNV,
  glProvokingVertex,
  glProvokingVertexEXT,
  glPushAttrib,
  glPushClientAttrib,
  glPushClientAttribDefaultEXT,
  glPushDebugGroup,
  glPushDebugGroupKHR,
  glPushGroupMarkerEXT,
  glPushMatrix,
  glPushName,
  glQueryCounter,
  glQueryCounterEXT,
  glQueryMatrixxOES
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

-- glProgramUniformMatrix3dv ---------------------------------------------------

glProgramUniformMatrix3dv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLdouble -- ^ @value@ pointing to @3@ elements of type @GLdouble@.
  -> m ()
glProgramUniformMatrix3dv v1 v2 v3 v4 v5 = liftIO $ dyn665 ptr_glProgramUniformMatrix3dv v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix3dv #-}
ptr_glProgramUniformMatrix3dv :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
ptr_glProgramUniformMatrix3dv = unsafePerformIO $ getCommand "glProgramUniformMatrix3dv"

-- glProgramUniformMatrix3dvEXT ------------------------------------------------

glProgramUniformMatrix3dvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLdouble -- ^ @value@ pointing to @count@ elements of type @GLdouble@.
  -> m ()
glProgramUniformMatrix3dvEXT v1 v2 v3 v4 v5 = liftIO $ dyn665 ptr_glProgramUniformMatrix3dvEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix3dvEXT #-}
ptr_glProgramUniformMatrix3dvEXT :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
ptr_glProgramUniformMatrix3dvEXT = unsafePerformIO $ getCommand "glProgramUniformMatrix3dvEXT"

-- glProgramUniformMatrix3fv ---------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniformMatrix3fv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @3@ elements of type @GLfloat@.
  -> m ()
glProgramUniformMatrix3fv v1 v2 v3 v4 v5 = liftIO $ dyn666 ptr_glProgramUniformMatrix3fv v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix3fv #-}
ptr_glProgramUniformMatrix3fv :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glProgramUniformMatrix3fv = unsafePerformIO $ getCommand "glProgramUniformMatrix3fv"

-- glProgramUniformMatrix3fvEXT ------------------------------------------------

-- | This command is an alias for 'glProgramUniformMatrix3fv'.
glProgramUniformMatrix3fvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*9@ elements of type @GLfloat@.
  -> m ()
glProgramUniformMatrix3fvEXT v1 v2 v3 v4 v5 = liftIO $ dyn666 ptr_glProgramUniformMatrix3fvEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix3fvEXT #-}
ptr_glProgramUniformMatrix3fvEXT :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glProgramUniformMatrix3fvEXT = unsafePerformIO $ getCommand "glProgramUniformMatrix3fvEXT"

-- glProgramUniformMatrix3x2dv -------------------------------------------------

glProgramUniformMatrix3x2dv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLdouble -- ^ @value@ pointing to @count@ elements of type @GLdouble@.
  -> m ()
glProgramUniformMatrix3x2dv v1 v2 v3 v4 v5 = liftIO $ dyn665 ptr_glProgramUniformMatrix3x2dv v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix3x2dv #-}
ptr_glProgramUniformMatrix3x2dv :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
ptr_glProgramUniformMatrix3x2dv = unsafePerformIO $ getCommand "glProgramUniformMatrix3x2dv"

-- glProgramUniformMatrix3x2dvEXT ----------------------------------------------

glProgramUniformMatrix3x2dvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLdouble -- ^ @value@ pointing to @count@ elements of type @GLdouble@.
  -> m ()
glProgramUniformMatrix3x2dvEXT v1 v2 v3 v4 v5 = liftIO $ dyn665 ptr_glProgramUniformMatrix3x2dvEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix3x2dvEXT #-}
ptr_glProgramUniformMatrix3x2dvEXT :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
ptr_glProgramUniformMatrix3x2dvEXT = unsafePerformIO $ getCommand "glProgramUniformMatrix3x2dvEXT"

-- glProgramUniformMatrix3x2fv -------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniformMatrix3x2fv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count@ elements of type @GLfloat@.
  -> m ()
glProgramUniformMatrix3x2fv v1 v2 v3 v4 v5 = liftIO $ dyn666 ptr_glProgramUniformMatrix3x2fv v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix3x2fv #-}
ptr_glProgramUniformMatrix3x2fv :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glProgramUniformMatrix3x2fv = unsafePerformIO $ getCommand "glProgramUniformMatrix3x2fv"

-- glProgramUniformMatrix3x2fvEXT ----------------------------------------------

-- | This command is an alias for 'glProgramUniformMatrix3x2fv'.
glProgramUniformMatrix3x2fvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*6@ elements of type @GLfloat@.
  -> m ()
glProgramUniformMatrix3x2fvEXT v1 v2 v3 v4 v5 = liftIO $ dyn666 ptr_glProgramUniformMatrix3x2fvEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix3x2fvEXT #-}
ptr_glProgramUniformMatrix3x2fvEXT :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glProgramUniformMatrix3x2fvEXT = unsafePerformIO $ getCommand "glProgramUniformMatrix3x2fvEXT"

-- glProgramUniformMatrix3x4dv -------------------------------------------------

glProgramUniformMatrix3x4dv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLdouble -- ^ @value@ pointing to @count@ elements of type @GLdouble@.
  -> m ()
glProgramUniformMatrix3x4dv v1 v2 v3 v4 v5 = liftIO $ dyn665 ptr_glProgramUniformMatrix3x4dv v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix3x4dv #-}
ptr_glProgramUniformMatrix3x4dv :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
ptr_glProgramUniformMatrix3x4dv = unsafePerformIO $ getCommand "glProgramUniformMatrix3x4dv"

-- glProgramUniformMatrix3x4dvEXT ----------------------------------------------

glProgramUniformMatrix3x4dvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLdouble -- ^ @value@ pointing to @count@ elements of type @GLdouble@.
  -> m ()
glProgramUniformMatrix3x4dvEXT v1 v2 v3 v4 v5 = liftIO $ dyn665 ptr_glProgramUniformMatrix3x4dvEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix3x4dvEXT #-}
ptr_glProgramUniformMatrix3x4dvEXT :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
ptr_glProgramUniformMatrix3x4dvEXT = unsafePerformIO $ getCommand "glProgramUniformMatrix3x4dvEXT"

-- glProgramUniformMatrix3x4fv -------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniformMatrix3x4fv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count@ elements of type @GLfloat@.
  -> m ()
glProgramUniformMatrix3x4fv v1 v2 v3 v4 v5 = liftIO $ dyn666 ptr_glProgramUniformMatrix3x4fv v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix3x4fv #-}
ptr_glProgramUniformMatrix3x4fv :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glProgramUniformMatrix3x4fv = unsafePerformIO $ getCommand "glProgramUniformMatrix3x4fv"

-- glProgramUniformMatrix3x4fvEXT ----------------------------------------------

-- | This command is an alias for 'glProgramUniformMatrix3x4fv'.
glProgramUniformMatrix3x4fvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*12@ elements of type @GLfloat@.
  -> m ()
glProgramUniformMatrix3x4fvEXT v1 v2 v3 v4 v5 = liftIO $ dyn666 ptr_glProgramUniformMatrix3x4fvEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix3x4fvEXT #-}
ptr_glProgramUniformMatrix3x4fvEXT :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glProgramUniformMatrix3x4fvEXT = unsafePerformIO $ getCommand "glProgramUniformMatrix3x4fvEXT"

-- glProgramUniformMatrix4dv ---------------------------------------------------

glProgramUniformMatrix4dv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLdouble -- ^ @value@ pointing to @4@ elements of type @GLdouble@.
  -> m ()
glProgramUniformMatrix4dv v1 v2 v3 v4 v5 = liftIO $ dyn665 ptr_glProgramUniformMatrix4dv v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix4dv #-}
ptr_glProgramUniformMatrix4dv :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
ptr_glProgramUniformMatrix4dv = unsafePerformIO $ getCommand "glProgramUniformMatrix4dv"

-- glProgramUniformMatrix4dvEXT ------------------------------------------------

glProgramUniformMatrix4dvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLdouble -- ^ @value@ pointing to @count@ elements of type @GLdouble@.
  -> m ()
glProgramUniformMatrix4dvEXT v1 v2 v3 v4 v5 = liftIO $ dyn665 ptr_glProgramUniformMatrix4dvEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix4dvEXT #-}
ptr_glProgramUniformMatrix4dvEXT :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
ptr_glProgramUniformMatrix4dvEXT = unsafePerformIO $ getCommand "glProgramUniformMatrix4dvEXT"

-- glProgramUniformMatrix4fv ---------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniformMatrix4fv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glProgramUniformMatrix4fv v1 v2 v3 v4 v5 = liftIO $ dyn666 ptr_glProgramUniformMatrix4fv v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix4fv #-}
ptr_glProgramUniformMatrix4fv :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glProgramUniformMatrix4fv = unsafePerformIO $ getCommand "glProgramUniformMatrix4fv"

-- glProgramUniformMatrix4fvEXT ------------------------------------------------

-- | This command is an alias for 'glProgramUniformMatrix4fv'.
glProgramUniformMatrix4fvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*16@ elements of type @GLfloat@.
  -> m ()
glProgramUniformMatrix4fvEXT v1 v2 v3 v4 v5 = liftIO $ dyn666 ptr_glProgramUniformMatrix4fvEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix4fvEXT #-}
ptr_glProgramUniformMatrix4fvEXT :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glProgramUniformMatrix4fvEXT = unsafePerformIO $ getCommand "glProgramUniformMatrix4fvEXT"

-- glProgramUniformMatrix4x2dv -------------------------------------------------

glProgramUniformMatrix4x2dv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLdouble -- ^ @value@ pointing to @count@ elements of type @GLdouble@.
  -> m ()
glProgramUniformMatrix4x2dv v1 v2 v3 v4 v5 = liftIO $ dyn665 ptr_glProgramUniformMatrix4x2dv v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix4x2dv #-}
ptr_glProgramUniformMatrix4x2dv :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
ptr_glProgramUniformMatrix4x2dv = unsafePerformIO $ getCommand "glProgramUniformMatrix4x2dv"

-- glProgramUniformMatrix4x2dvEXT ----------------------------------------------

glProgramUniformMatrix4x2dvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLdouble -- ^ @value@ pointing to @count@ elements of type @GLdouble@.
  -> m ()
glProgramUniformMatrix4x2dvEXT v1 v2 v3 v4 v5 = liftIO $ dyn665 ptr_glProgramUniformMatrix4x2dvEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix4x2dvEXT #-}
ptr_glProgramUniformMatrix4x2dvEXT :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
ptr_glProgramUniformMatrix4x2dvEXT = unsafePerformIO $ getCommand "glProgramUniformMatrix4x2dvEXT"

-- glProgramUniformMatrix4x2fv -------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniformMatrix4x2fv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count@ elements of type @GLfloat@.
  -> m ()
glProgramUniformMatrix4x2fv v1 v2 v3 v4 v5 = liftIO $ dyn666 ptr_glProgramUniformMatrix4x2fv v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix4x2fv #-}
ptr_glProgramUniformMatrix4x2fv :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glProgramUniformMatrix4x2fv = unsafePerformIO $ getCommand "glProgramUniformMatrix4x2fv"

-- glProgramUniformMatrix4x2fvEXT ----------------------------------------------

-- | This command is an alias for 'glProgramUniformMatrix4x2fv'.
glProgramUniformMatrix4x2fvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*8@ elements of type @GLfloat@.
  -> m ()
glProgramUniformMatrix4x2fvEXT v1 v2 v3 v4 v5 = liftIO $ dyn666 ptr_glProgramUniformMatrix4x2fvEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix4x2fvEXT #-}
ptr_glProgramUniformMatrix4x2fvEXT :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glProgramUniformMatrix4x2fvEXT = unsafePerformIO $ getCommand "glProgramUniformMatrix4x2fvEXT"

-- glProgramUniformMatrix4x3dv -------------------------------------------------

glProgramUniformMatrix4x3dv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLdouble -- ^ @value@ pointing to @count@ elements of type @GLdouble@.
  -> m ()
glProgramUniformMatrix4x3dv v1 v2 v3 v4 v5 = liftIO $ dyn665 ptr_glProgramUniformMatrix4x3dv v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix4x3dv #-}
ptr_glProgramUniformMatrix4x3dv :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
ptr_glProgramUniformMatrix4x3dv = unsafePerformIO $ getCommand "glProgramUniformMatrix4x3dv"

-- glProgramUniformMatrix4x3dvEXT ----------------------------------------------

glProgramUniformMatrix4x3dvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLdouble -- ^ @value@ pointing to @count@ elements of type @GLdouble@.
  -> m ()
glProgramUniformMatrix4x3dvEXT v1 v2 v3 v4 v5 = liftIO $ dyn665 ptr_glProgramUniformMatrix4x3dvEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix4x3dvEXT #-}
ptr_glProgramUniformMatrix4x3dvEXT :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLdouble -> IO ())
ptr_glProgramUniformMatrix4x3dvEXT = unsafePerformIO $ getCommand "glProgramUniformMatrix4x3dvEXT"

-- glProgramUniformMatrix4x3fv -------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniformMatrix4x3fv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count@ elements of type @GLfloat@.
  -> m ()
glProgramUniformMatrix4x3fv v1 v2 v3 v4 v5 = liftIO $ dyn666 ptr_glProgramUniformMatrix4x3fv v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix4x3fv #-}
ptr_glProgramUniformMatrix4x3fv :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glProgramUniformMatrix4x3fv = unsafePerformIO $ getCommand "glProgramUniformMatrix4x3fv"

-- glProgramUniformMatrix4x3fvEXT ----------------------------------------------

-- | This command is an alias for 'glProgramUniformMatrix4x3fv'.
glProgramUniformMatrix4x3fvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> GLboolean -- ^ @transpose@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLfloat -- ^ @value@ pointing to @count*12@ elements of type @GLfloat@.
  -> m ()
glProgramUniformMatrix4x3fvEXT v1 v2 v3 v4 v5 = liftIO $ dyn666 ptr_glProgramUniformMatrix4x3fvEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramUniformMatrix4x3fvEXT #-}
ptr_glProgramUniformMatrix4x3fvEXT :: FunPtr (GLuint -> GLint -> GLsizei -> GLboolean -> Ptr GLfloat -> IO ())
ptr_glProgramUniformMatrix4x3fvEXT = unsafePerformIO $ getCommand "glProgramUniformMatrix4x3fvEXT"

-- glProgramUniformui64NV ------------------------------------------------------

glProgramUniformui64NV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLuint64EXT -- ^ @value@.
  -> m ()
glProgramUniformui64NV v1 v2 v3 = liftIO $ dyn639 ptr_glProgramUniformui64NV v1 v2 v3

{-# NOINLINE ptr_glProgramUniformui64NV #-}
ptr_glProgramUniformui64NV :: FunPtr (GLuint -> GLint -> GLuint64EXT -> IO ())
ptr_glProgramUniformui64NV = unsafePerformIO $ getCommand "glProgramUniformui64NV"

-- glProgramUniformui64vNV -----------------------------------------------------

glProgramUniformui64vNV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint64EXT -- ^ @value@ pointing to @count@ elements of type @GLuint64EXT@.
  -> m ()
glProgramUniformui64vNV v1 v2 v3 v4 = liftIO $ dyn640 ptr_glProgramUniformui64vNV v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniformui64vNV #-}
ptr_glProgramUniformui64vNV :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint64EXT -> IO ())
ptr_glProgramUniformui64vNV = unsafePerformIO $ getCommand "glProgramUniformui64vNV"

-- glProgramVertexLimitNV ------------------------------------------------------

glProgramVertexLimitNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ProgramTarget@.
  -> GLint -- ^ @limit@.
  -> m ()
glProgramVertexLimitNV v1 v2 = liftIO $ dyn55 ptr_glProgramVertexLimitNV v1 v2

{-# NOINLINE ptr_glProgramVertexLimitNV #-}
ptr_glProgramVertexLimitNV :: FunPtr (GLenum -> GLint -> IO ())
ptr_glProgramVertexLimitNV = unsafePerformIO $ getCommand "glProgramVertexLimitNV"

-- glProvokingVertex -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glProvokingVertex.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glProvokingVertex.xhtml OpenGL 4.x>.
glProvokingVertex
  :: MonadIO m
  => GLenum -- ^ @mode@.
  -> m ()
glProvokingVertex v1 = liftIO $ dyn4 ptr_glProvokingVertex v1

{-# NOINLINE ptr_glProvokingVertex #-}
ptr_glProvokingVertex :: FunPtr (GLenum -> IO ())
ptr_glProvokingVertex = unsafePerformIO $ getCommand "glProvokingVertex"

-- glProvokingVertexEXT --------------------------------------------------------

-- | This command is an alias for 'glProvokingVertex'.
glProvokingVertexEXT
  :: MonadIO m
  => GLenum -- ^ @mode@.
  -> m ()
glProvokingVertexEXT v1 = liftIO $ dyn4 ptr_glProvokingVertexEXT v1

{-# NOINLINE ptr_glProvokingVertexEXT #-}
ptr_glProvokingVertexEXT :: FunPtr (GLenum -> IO ())
ptr_glProvokingVertexEXT = unsafePerformIO $ getCommand "glProvokingVertexEXT"

-- glPushAttrib ----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glPushAttrib.xml OpenGL 2.x>.
glPushAttrib
  :: MonadIO m
  => GLbitfield -- ^ @mask@ of type [AttribMask](Graphics-GL-Groups.html#AttribMask).
  -> m ()
glPushAttrib v1 = liftIO $ dyn69 ptr_glPushAttrib v1

{-# NOINLINE ptr_glPushAttrib #-}
ptr_glPushAttrib :: FunPtr (GLbitfield -> IO ())
ptr_glPushAttrib = unsafePerformIO $ getCommand "glPushAttrib"

-- glPushClientAttrib ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glPushClientAttrib.xml OpenGL 2.x>.
glPushClientAttrib
  :: MonadIO m
  => GLbitfield -- ^ @mask@ of type [ClientAttribMask](Graphics-GL-Groups.html#ClientAttribMask).
  -> m ()
glPushClientAttrib v1 = liftIO $ dyn69 ptr_glPushClientAttrib v1

{-# NOINLINE ptr_glPushClientAttrib #-}
ptr_glPushClientAttrib :: FunPtr (GLbitfield -> IO ())
ptr_glPushClientAttrib = unsafePerformIO $ getCommand "glPushClientAttrib"

-- glPushClientAttribDefaultEXT ------------------------------------------------

glPushClientAttribDefaultEXT
  :: MonadIO m
  => GLbitfield -- ^ @mask@ of type [ClientAttribMask](Graphics-GL-Groups.html#ClientAttribMask).
  -> m ()
glPushClientAttribDefaultEXT v1 = liftIO $ dyn69 ptr_glPushClientAttribDefaultEXT v1

{-# NOINLINE ptr_glPushClientAttribDefaultEXT #-}
ptr_glPushClientAttribDefaultEXT :: FunPtr (GLbitfield -> IO ())
ptr_glPushClientAttribDefaultEXT = unsafePerformIO $ getCommand "glPushClientAttribDefaultEXT"

-- glPushDebugGroup ------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glPushDebugGroup.xhtml OpenGL 4.x>.
glPushDebugGroup
  :: MonadIO m
  => GLenum -- ^ @source@.
  -> GLuint -- ^ @id@.
  -> GLsizei -- ^ @length@.
  -> Ptr GLchar -- ^ @message@ pointing to @COMPSIZE(message,length)@ elements of type @GLchar@.
  -> m ()
glPushDebugGroup v1 v2 v3 v4 = liftIO $ dyn484 ptr_glPushDebugGroup v1 v2 v3 v4

{-# NOINLINE ptr_glPushDebugGroup #-}
ptr_glPushDebugGroup :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLchar -> IO ())
ptr_glPushDebugGroup = unsafePerformIO $ getCommand "glPushDebugGroup"

-- glPushDebugGroupKHR ---------------------------------------------------------

-- | This command is an alias for 'glPushDebugGroup'.
glPushDebugGroupKHR
  :: MonadIO m
  => GLenum -- ^ @source@.
  -> GLuint -- ^ @id@.
  -> GLsizei -- ^ @length@.
  -> Ptr GLchar -- ^ @message@.
  -> m ()
glPushDebugGroupKHR v1 v2 v3 v4 = liftIO $ dyn484 ptr_glPushDebugGroupKHR v1 v2 v3 v4

{-# NOINLINE ptr_glPushDebugGroupKHR #-}
ptr_glPushDebugGroupKHR :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLchar -> IO ())
ptr_glPushDebugGroupKHR = unsafePerformIO $ getCommand "glPushDebugGroupKHR"

-- glPushGroupMarkerEXT --------------------------------------------------------

glPushGroupMarkerEXT
  :: MonadIO m
  => GLsizei -- ^ @length@.
  -> Ptr GLchar -- ^ @marker@.
  -> m ()
glPushGroupMarkerEXT v1 v2 = liftIO $ dyn469 ptr_glPushGroupMarkerEXT v1 v2

{-# NOINLINE ptr_glPushGroupMarkerEXT #-}
ptr_glPushGroupMarkerEXT :: FunPtr (GLsizei -> Ptr GLchar -> IO ())
ptr_glPushGroupMarkerEXT = unsafePerformIO $ getCommand "glPushGroupMarkerEXT"

-- glPushMatrix ----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glPushMatrix.xml OpenGL 2.x>.
glPushMatrix
  :: MonadIO m
  => m ()
glPushMatrix = liftIO $ dyn10 ptr_glPushMatrix

{-# NOINLINE ptr_glPushMatrix #-}
ptr_glPushMatrix :: FunPtr (IO ())
ptr_glPushMatrix = unsafePerformIO $ getCommand "glPushMatrix"

-- glPushName ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glPushName.xml OpenGL 2.x>.
glPushName
  :: MonadIO m
  => GLuint -- ^ @name@ of type @SelectName@.
  -> m ()
glPushName v1 = liftIO $ dyn2 ptr_glPushName v1

{-# NOINLINE ptr_glPushName #-}
ptr_glPushName :: FunPtr (GLuint -> IO ())
ptr_glPushName = unsafePerformIO $ getCommand "glPushName"

-- glQueryCounter --------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glQueryCounter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glQueryCounter.xhtml OpenGL 4.x>.
glQueryCounter
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @target@.
  -> m ()
glQueryCounter v1 v2 = liftIO $ dyn15 ptr_glQueryCounter v1 v2

{-# NOINLINE ptr_glQueryCounter #-}
ptr_glQueryCounter :: FunPtr (GLuint -> GLenum -> IO ())
ptr_glQueryCounter = unsafePerformIO $ getCommand "glQueryCounter"

-- glQueryCounterEXT -----------------------------------------------------------

-- | This command is an alias for 'glQueryCounter'.
glQueryCounterEXT
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @target@.
  -> m ()
glQueryCounterEXT v1 v2 = liftIO $ dyn15 ptr_glQueryCounterEXT v1 v2

{-# NOINLINE ptr_glQueryCounterEXT #-}
ptr_glQueryCounterEXT :: FunPtr (GLuint -> GLenum -> IO ())
ptr_glQueryCounterEXT = unsafePerformIO $ getCommand "glQueryCounterEXT"

-- glQueryMatrixxOES -----------------------------------------------------------

glQueryMatrixxOES
  :: MonadIO m
  => Ptr GLfixed -- ^ @mantissa@ pointing to @16@ elements of type @GLfixed@.
  -> Ptr GLint -- ^ @exponent@ pointing to @16@ elements of type @GLint@.
  -> m GLbitfield
glQueryMatrixxOES v1 v2 = liftIO $ dyn667 ptr_glQueryMatrixxOES v1 v2

{-# NOINLINE ptr_glQueryMatrixxOES #-}
ptr_glQueryMatrixxOES :: FunPtr (Ptr GLfixed -> Ptr GLint -> IO GLbitfield)
ptr_glQueryMatrixxOES = unsafePerformIO $ getCommand "glQueryMatrixxOES"

