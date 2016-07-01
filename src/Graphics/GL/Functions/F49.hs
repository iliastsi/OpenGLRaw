--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F49
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

module Graphics.GL.Functions.F49 (
  glProgramParameteri,
  glProgramParameteriARB,
  glProgramParameteriEXT,
  glProgramParameters4dvNV,
  glProgramParameters4fvNV,
  glProgramPathFragmentInputGenNV,
  glProgramStringARB,
  glProgramSubroutineParametersuivNV,
  glProgramUniform1d,
  glProgramUniform1dEXT,
  glProgramUniform1dv,
  glProgramUniform1dvEXT,
  glProgramUniform1f,
  glProgramUniform1fEXT,
  glProgramUniform1fv,
  glProgramUniform1fvEXT,
  glProgramUniform1i,
  glProgramUniform1i64ARB,
  glProgramUniform1i64NV,
  glProgramUniform1i64vARB,
  glProgramUniform1i64vNV,
  glProgramUniform1iEXT,
  glProgramUniform1iv,
  glProgramUniform1ivEXT,
  glProgramUniform1ui,
  glProgramUniform1ui64ARB,
  glProgramUniform1ui64NV,
  glProgramUniform1ui64vARB,
  glProgramUniform1ui64vNV,
  glProgramUniform1uiEXT,
  glProgramUniform1uiv,
  glProgramUniform1uivEXT,
  glProgramUniform2d,
  glProgramUniform2dEXT,
  glProgramUniform2dv,
  glProgramUniform2dvEXT,
  glProgramUniform2f,
  glProgramUniform2fEXT,
  glProgramUniform2fv,
  glProgramUniform2fvEXT
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

-- glProgramParameteri ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramParameter.xhtml OpenGL 4.x>.
glProgramParameteri
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @pname@ of type @ProgramParameterPName@.
  -> GLint -- ^ @value@.
  -> m ()
glProgramParameteri v1 v2 v3 = liftIO $ dyn488 ptr_glProgramParameteri v1 v2 v3

{-# NOINLINE ptr_glProgramParameteri #-}
ptr_glProgramParameteri :: FunPtr (GLuint -> GLenum -> GLint -> IO ())
ptr_glProgramParameteri = unsafePerformIO $ getCommand "glProgramParameteri"

-- glProgramParameteriARB ------------------------------------------------------

-- | This command is an alias for 'glProgramParameteri'.
glProgramParameteriARB
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @pname@ of type @ProgramParameterPName@.
  -> GLint -- ^ @value@.
  -> m ()
glProgramParameteriARB v1 v2 v3 = liftIO $ dyn488 ptr_glProgramParameteriARB v1 v2 v3

{-# NOINLINE ptr_glProgramParameteriARB #-}
ptr_glProgramParameteriARB :: FunPtr (GLuint -> GLenum -> GLint -> IO ())
ptr_glProgramParameteriARB = unsafePerformIO $ getCommand "glProgramParameteriARB"

-- glProgramParameteriEXT ------------------------------------------------------

-- | This command is an alias for 'glProgramParameteri'.
glProgramParameteriEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @pname@ of type @ProgramParameterPName@.
  -> GLint -- ^ @value@.
  -> m ()
glProgramParameteriEXT v1 v2 v3 = liftIO $ dyn488 ptr_glProgramParameteriEXT v1 v2 v3

{-# NOINLINE ptr_glProgramParameteriEXT #-}
ptr_glProgramParameteriEXT :: FunPtr (GLuint -> GLenum -> GLint -> IO ())
ptr_glProgramParameteriEXT = unsafePerformIO $ getCommand "glProgramParameteriEXT"

-- glProgramParameters4dvNV ----------------------------------------------------

glProgramParameters4dvNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type @VertexAttribEnumNV@.
  -> GLuint -- ^ @index@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLdouble -- ^ @v@ pointing to @count*4@ elements of type @GLdouble@.
  -> m ()
glProgramParameters4dvNV v1 v2 v3 v4 = liftIO $ dyn628 ptr_glProgramParameters4dvNV v1 v2 v3 v4

{-# NOINLINE ptr_glProgramParameters4dvNV #-}
ptr_glProgramParameters4dvNV :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLdouble -> IO ())
ptr_glProgramParameters4dvNV = unsafePerformIO $ getCommand "glProgramParameters4dvNV"

-- glProgramParameters4fvNV ----------------------------------------------------

glProgramParameters4fvNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type @VertexAttribEnumNV@.
  -> GLuint -- ^ @index@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @v@ pointing to @count*4@ elements of type @GLfloat@.
  -> m ()
glProgramParameters4fvNV v1 v2 v3 v4 = liftIO $ dyn284 ptr_glProgramParameters4fvNV v1 v2 v3 v4

{-# NOINLINE ptr_glProgramParameters4fvNV #-}
ptr_glProgramParameters4fvNV :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glProgramParameters4fvNV = unsafePerformIO $ getCommand "glProgramParameters4fvNV"

-- glProgramPathFragmentInputGenNV ---------------------------------------------

glProgramPathFragmentInputGenNV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLenum -- ^ @genMode@.
  -> GLint -- ^ @components@.
  -> Ptr GLfloat -- ^ @coeffs@.
  -> m ()
glProgramPathFragmentInputGenNV v1 v2 v3 v4 v5 = liftIO $ dyn629 ptr_glProgramPathFragmentInputGenNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramPathFragmentInputGenNV #-}
ptr_glProgramPathFragmentInputGenNV :: FunPtr (GLuint -> GLint -> GLenum -> GLint -> Ptr GLfloat -> IO ())
ptr_glProgramPathFragmentInputGenNV = unsafePerformIO $ getCommand "glProgramPathFragmentInputGenNV"

-- glProgramStringARB ----------------------------------------------------------

glProgramStringARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ProgramTargetARB@.
  -> GLenum -- ^ @format@ of type @ProgramFormatARB@.
  -> GLsizei -- ^ @len@.
  -> Ptr a -- ^ @string@ pointing to @len@ elements of type @a@.
  -> m ()
glProgramStringARB v1 v2 v3 v4 = liftIO $ dyn630 ptr_glProgramStringARB v1 v2 v3 v4

{-# NOINLINE ptr_glProgramStringARB #-}
ptr_glProgramStringARB :: FunPtr (GLenum -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glProgramStringARB = unsafePerformIO $ getCommand "glProgramStringARB"

-- glProgramSubroutineParametersuivNV ------------------------------------------

glProgramSubroutineParametersuivNV
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @params@ pointing to @count@ elements of type @GLuint@.
  -> m ()
glProgramSubroutineParametersuivNV v1 v2 v3 = liftIO $ dyn197 ptr_glProgramSubroutineParametersuivNV v1 v2 v3

{-# NOINLINE ptr_glProgramSubroutineParametersuivNV #-}
ptr_glProgramSubroutineParametersuivNV :: FunPtr (GLenum -> GLsizei -> Ptr GLuint -> IO ())
ptr_glProgramSubroutineParametersuivNV = unsafePerformIO $ getCommand "glProgramSubroutineParametersuivNV"

-- glProgramUniform1d ----------------------------------------------------------

glProgramUniform1d
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLdouble -- ^ @v0@.
  -> m ()
glProgramUniform1d v1 v2 v3 = liftIO $ dyn631 ptr_glProgramUniform1d v1 v2 v3

{-# NOINLINE ptr_glProgramUniform1d #-}
ptr_glProgramUniform1d :: FunPtr (GLuint -> GLint -> GLdouble -> IO ())
ptr_glProgramUniform1d = unsafePerformIO $ getCommand "glProgramUniform1d"

-- glProgramUniform1dEXT -------------------------------------------------------

glProgramUniform1dEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLdouble -- ^ @x@.
  -> m ()
glProgramUniform1dEXT v1 v2 v3 = liftIO $ dyn631 ptr_glProgramUniform1dEXT v1 v2 v3

{-# NOINLINE ptr_glProgramUniform1dEXT #-}
ptr_glProgramUniform1dEXT :: FunPtr (GLuint -> GLint -> GLdouble -> IO ())
ptr_glProgramUniform1dEXT = unsafePerformIO $ getCommand "glProgramUniform1dEXT"

-- glProgramUniform1dv ---------------------------------------------------------

glProgramUniform1dv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLdouble -- ^ @value@ pointing to @1@ element of type @GLdouble@.
  -> m ()
glProgramUniform1dv v1 v2 v3 v4 = liftIO $ dyn456 ptr_glProgramUniform1dv v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform1dv #-}
ptr_glProgramUniform1dv :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLdouble -> IO ())
ptr_glProgramUniform1dv = unsafePerformIO $ getCommand "glProgramUniform1dv"

-- glProgramUniform1dvEXT ------------------------------------------------------

glProgramUniform1dvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLdouble -- ^ @value@ pointing to @count@ elements of type @GLdouble@.
  -> m ()
glProgramUniform1dvEXT v1 v2 v3 v4 = liftIO $ dyn456 ptr_glProgramUniform1dvEXT v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform1dvEXT #-}
ptr_glProgramUniform1dvEXT :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLdouble -> IO ())
ptr_glProgramUniform1dvEXT = unsafePerformIO $ getCommand "glProgramUniform1dvEXT"

-- glProgramUniform1f ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniform1f
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLfloat -- ^ @v0@.
  -> m ()
glProgramUniform1f v1 v2 v3 = liftIO $ dyn632 ptr_glProgramUniform1f v1 v2 v3

{-# NOINLINE ptr_glProgramUniform1f #-}
ptr_glProgramUniform1f :: FunPtr (GLuint -> GLint -> GLfloat -> IO ())
ptr_glProgramUniform1f = unsafePerformIO $ getCommand "glProgramUniform1f"

-- glProgramUniform1fEXT -------------------------------------------------------

-- | This command is an alias for 'glProgramUniform1f'.
glProgramUniform1fEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLfloat -- ^ @v0@.
  -> m ()
glProgramUniform1fEXT v1 v2 v3 = liftIO $ dyn632 ptr_glProgramUniform1fEXT v1 v2 v3

{-# NOINLINE ptr_glProgramUniform1fEXT #-}
ptr_glProgramUniform1fEXT :: FunPtr (GLuint -> GLint -> GLfloat -> IO ())
ptr_glProgramUniform1fEXT = unsafePerformIO $ getCommand "glProgramUniform1fEXT"

-- glProgramUniform1fv ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniform1fv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @value@ pointing to @1@ element of type @GLfloat@.
  -> m ()
glProgramUniform1fv v1 v2 v3 v4 = liftIO $ dyn457 ptr_glProgramUniform1fv v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform1fv #-}
ptr_glProgramUniform1fv :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glProgramUniform1fv = unsafePerformIO $ getCommand "glProgramUniform1fv"

-- glProgramUniform1fvEXT ------------------------------------------------------

-- | This command is an alias for 'glProgramUniform1fv'.
glProgramUniform1fvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @value@ pointing to @count@ elements of type @GLfloat@.
  -> m ()
glProgramUniform1fvEXT v1 v2 v3 v4 = liftIO $ dyn457 ptr_glProgramUniform1fvEXT v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform1fvEXT #-}
ptr_glProgramUniform1fvEXT :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glProgramUniform1fvEXT = unsafePerformIO $ getCommand "glProgramUniform1fvEXT"

-- glProgramUniform1i ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniform1i
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLint -- ^ @v0@.
  -> m ()
glProgramUniform1i v1 v2 v3 = liftIO $ dyn633 ptr_glProgramUniform1i v1 v2 v3

{-# NOINLINE ptr_glProgramUniform1i #-}
ptr_glProgramUniform1i :: FunPtr (GLuint -> GLint -> GLint -> IO ())
ptr_glProgramUniform1i = unsafePerformIO $ getCommand "glProgramUniform1i"

-- glProgramUniform1i64ARB -----------------------------------------------------

glProgramUniform1i64ARB
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLint64 -- ^ @x@.
  -> m ()
glProgramUniform1i64ARB v1 v2 v3 = liftIO $ dyn634 ptr_glProgramUniform1i64ARB v1 v2 v3

{-# NOINLINE ptr_glProgramUniform1i64ARB #-}
ptr_glProgramUniform1i64ARB :: FunPtr (GLuint -> GLint -> GLint64 -> IO ())
ptr_glProgramUniform1i64ARB = unsafePerformIO $ getCommand "glProgramUniform1i64ARB"

-- glProgramUniform1i64NV ------------------------------------------------------

glProgramUniform1i64NV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLint64EXT -- ^ @x@.
  -> m ()
glProgramUniform1i64NV v1 v2 v3 = liftIO $ dyn635 ptr_glProgramUniform1i64NV v1 v2 v3

{-# NOINLINE ptr_glProgramUniform1i64NV #-}
ptr_glProgramUniform1i64NV :: FunPtr (GLuint -> GLint -> GLint64EXT -> IO ())
ptr_glProgramUniform1i64NV = unsafePerformIO $ getCommand "glProgramUniform1i64NV"

-- glProgramUniform1i64vARB ----------------------------------------------------

glProgramUniform1i64vARB
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint64 -- ^ @value@ pointing to @count@ elements of type @GLint64@.
  -> m ()
glProgramUniform1i64vARB v1 v2 v3 v4 = liftIO $ dyn458 ptr_glProgramUniform1i64vARB v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform1i64vARB #-}
ptr_glProgramUniform1i64vARB :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLint64 -> IO ())
ptr_glProgramUniform1i64vARB = unsafePerformIO $ getCommand "glProgramUniform1i64vARB"

-- glProgramUniform1i64vNV -----------------------------------------------------

glProgramUniform1i64vNV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint64EXT -- ^ @value@ pointing to @count@ elements of type @GLint64EXT@.
  -> m ()
glProgramUniform1i64vNV v1 v2 v3 v4 = liftIO $ dyn636 ptr_glProgramUniform1i64vNV v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform1i64vNV #-}
ptr_glProgramUniform1i64vNV :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLint64EXT -> IO ())
ptr_glProgramUniform1i64vNV = unsafePerformIO $ getCommand "glProgramUniform1i64vNV"

-- glProgramUniform1iEXT -------------------------------------------------------

-- | This command is an alias for 'glProgramUniform1i'.
glProgramUniform1iEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLint -- ^ @v0@.
  -> m ()
glProgramUniform1iEXT v1 v2 v3 = liftIO $ dyn633 ptr_glProgramUniform1iEXT v1 v2 v3

{-# NOINLINE ptr_glProgramUniform1iEXT #-}
ptr_glProgramUniform1iEXT :: FunPtr (GLuint -> GLint -> GLint -> IO ())
ptr_glProgramUniform1iEXT = unsafePerformIO $ getCommand "glProgramUniform1iEXT"

-- glProgramUniform1iv ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniform1iv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint -- ^ @value@ pointing to @1@ element of type @GLint@.
  -> m ()
glProgramUniform1iv v1 v2 v3 v4 = liftIO $ dyn459 ptr_glProgramUniform1iv v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform1iv #-}
ptr_glProgramUniform1iv :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLint -> IO ())
ptr_glProgramUniform1iv = unsafePerformIO $ getCommand "glProgramUniform1iv"

-- glProgramUniform1ivEXT ------------------------------------------------------

-- | This command is an alias for 'glProgramUniform1iv'.
glProgramUniform1ivEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint -- ^ @value@ pointing to @count@ elements of type @GLint@.
  -> m ()
glProgramUniform1ivEXT v1 v2 v3 v4 = liftIO $ dyn459 ptr_glProgramUniform1ivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform1ivEXT #-}
ptr_glProgramUniform1ivEXT :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLint -> IO ())
ptr_glProgramUniform1ivEXT = unsafePerformIO $ getCommand "glProgramUniform1ivEXT"

-- glProgramUniform1ui ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniform1ui
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLuint -- ^ @v0@.
  -> m ()
glProgramUniform1ui v1 v2 v3 = liftIO $ dyn637 ptr_glProgramUniform1ui v1 v2 v3

{-# NOINLINE ptr_glProgramUniform1ui #-}
ptr_glProgramUniform1ui :: FunPtr (GLuint -> GLint -> GLuint -> IO ())
ptr_glProgramUniform1ui = unsafePerformIO $ getCommand "glProgramUniform1ui"

-- glProgramUniform1ui64ARB ----------------------------------------------------

glProgramUniform1ui64ARB
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLuint64 -- ^ @x@.
  -> m ()
glProgramUniform1ui64ARB v1 v2 v3 = liftIO $ dyn638 ptr_glProgramUniform1ui64ARB v1 v2 v3

{-# NOINLINE ptr_glProgramUniform1ui64ARB #-}
ptr_glProgramUniform1ui64ARB :: FunPtr (GLuint -> GLint -> GLuint64 -> IO ())
ptr_glProgramUniform1ui64ARB = unsafePerformIO $ getCommand "glProgramUniform1ui64ARB"

-- glProgramUniform1ui64NV -----------------------------------------------------

glProgramUniform1ui64NV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLuint64EXT -- ^ @x@.
  -> m ()
glProgramUniform1ui64NV v1 v2 v3 = liftIO $ dyn639 ptr_glProgramUniform1ui64NV v1 v2 v3

{-# NOINLINE ptr_glProgramUniform1ui64NV #-}
ptr_glProgramUniform1ui64NV :: FunPtr (GLuint -> GLint -> GLuint64EXT -> IO ())
ptr_glProgramUniform1ui64NV = unsafePerformIO $ getCommand "glProgramUniform1ui64NV"

-- glProgramUniform1ui64vARB ---------------------------------------------------

glProgramUniform1ui64vARB
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint64 -- ^ @value@ pointing to @count@ elements of type @GLuint64@.
  -> m ()
glProgramUniform1ui64vARB v1 v2 v3 v4 = liftIO $ dyn460 ptr_glProgramUniform1ui64vARB v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform1ui64vARB #-}
ptr_glProgramUniform1ui64vARB :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint64 -> IO ())
ptr_glProgramUniform1ui64vARB = unsafePerformIO $ getCommand "glProgramUniform1ui64vARB"

-- glProgramUniform1ui64vNV ----------------------------------------------------

glProgramUniform1ui64vNV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint64EXT -- ^ @value@ pointing to @count@ elements of type @GLuint64EXT@.
  -> m ()
glProgramUniform1ui64vNV v1 v2 v3 v4 = liftIO $ dyn640 ptr_glProgramUniform1ui64vNV v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform1ui64vNV #-}
ptr_glProgramUniform1ui64vNV :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint64EXT -> IO ())
ptr_glProgramUniform1ui64vNV = unsafePerformIO $ getCommand "glProgramUniform1ui64vNV"

-- glProgramUniform1uiEXT ------------------------------------------------------

-- | This command is an alias for 'glProgramUniform1ui'.
glProgramUniform1uiEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLuint -- ^ @v0@.
  -> m ()
glProgramUniform1uiEXT v1 v2 v3 = liftIO $ dyn637 ptr_glProgramUniform1uiEXT v1 v2 v3

{-# NOINLINE ptr_glProgramUniform1uiEXT #-}
ptr_glProgramUniform1uiEXT :: FunPtr (GLuint -> GLint -> GLuint -> IO ())
ptr_glProgramUniform1uiEXT = unsafePerformIO $ getCommand "glProgramUniform1uiEXT"

-- glProgramUniform1uiv --------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniform1uiv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @value@ pointing to @1@ element of type @GLuint@.
  -> m ()
glProgramUniform1uiv v1 v2 v3 v4 = liftIO $ dyn461 ptr_glProgramUniform1uiv v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform1uiv #-}
ptr_glProgramUniform1uiv :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint -> IO ())
ptr_glProgramUniform1uiv = unsafePerformIO $ getCommand "glProgramUniform1uiv"

-- glProgramUniform1uivEXT -----------------------------------------------------

-- | This command is an alias for 'glProgramUniform1uiv'.
glProgramUniform1uivEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @value@ pointing to @count@ elements of type @GLuint@.
  -> m ()
glProgramUniform1uivEXT v1 v2 v3 v4 = liftIO $ dyn461 ptr_glProgramUniform1uivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform1uivEXT #-}
ptr_glProgramUniform1uivEXT :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint -> IO ())
ptr_glProgramUniform1uivEXT = unsafePerformIO $ getCommand "glProgramUniform1uivEXT"

-- glProgramUniform2d ----------------------------------------------------------

glProgramUniform2d
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLdouble -- ^ @v0@.
  -> GLdouble -- ^ @v1@.
  -> m ()
glProgramUniform2d v1 v2 v3 v4 = liftIO $ dyn641 ptr_glProgramUniform2d v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform2d #-}
ptr_glProgramUniform2d :: FunPtr (GLuint -> GLint -> GLdouble -> GLdouble -> IO ())
ptr_glProgramUniform2d = unsafePerformIO $ getCommand "glProgramUniform2d"

-- glProgramUniform2dEXT -------------------------------------------------------

glProgramUniform2dEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> m ()
glProgramUniform2dEXT v1 v2 v3 v4 = liftIO $ dyn641 ptr_glProgramUniform2dEXT v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform2dEXT #-}
ptr_glProgramUniform2dEXT :: FunPtr (GLuint -> GLint -> GLdouble -> GLdouble -> IO ())
ptr_glProgramUniform2dEXT = unsafePerformIO $ getCommand "glProgramUniform2dEXT"

-- glProgramUniform2dv ---------------------------------------------------------

glProgramUniform2dv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLdouble -- ^ @value@ pointing to @2@ elements of type @GLdouble@.
  -> m ()
glProgramUniform2dv v1 v2 v3 v4 = liftIO $ dyn456 ptr_glProgramUniform2dv v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform2dv #-}
ptr_glProgramUniform2dv :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLdouble -> IO ())
ptr_glProgramUniform2dv = unsafePerformIO $ getCommand "glProgramUniform2dv"

-- glProgramUniform2dvEXT ------------------------------------------------------

glProgramUniform2dvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLdouble -- ^ @value@ pointing to @count@ elements of type @GLdouble@.
  -> m ()
glProgramUniform2dvEXT v1 v2 v3 v4 = liftIO $ dyn456 ptr_glProgramUniform2dvEXT v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform2dvEXT #-}
ptr_glProgramUniform2dvEXT :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLdouble -> IO ())
ptr_glProgramUniform2dvEXT = unsafePerformIO $ getCommand "glProgramUniform2dvEXT"

-- glProgramUniform2f ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniform2f
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLfloat -- ^ @v0@.
  -> GLfloat -- ^ @v1@.
  -> m ()
glProgramUniform2f v1 v2 v3 v4 = liftIO $ dyn642 ptr_glProgramUniform2f v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform2f #-}
ptr_glProgramUniform2f :: FunPtr (GLuint -> GLint -> GLfloat -> GLfloat -> IO ())
ptr_glProgramUniform2f = unsafePerformIO $ getCommand "glProgramUniform2f"

-- glProgramUniform2fEXT -------------------------------------------------------

-- | This command is an alias for 'glProgramUniform2f'.
glProgramUniform2fEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLfloat -- ^ @v0@.
  -> GLfloat -- ^ @v1@.
  -> m ()
glProgramUniform2fEXT v1 v2 v3 v4 = liftIO $ dyn642 ptr_glProgramUniform2fEXT v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform2fEXT #-}
ptr_glProgramUniform2fEXT :: FunPtr (GLuint -> GLint -> GLfloat -> GLfloat -> IO ())
ptr_glProgramUniform2fEXT = unsafePerformIO $ getCommand "glProgramUniform2fEXT"

-- glProgramUniform2fv ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramUniform.xhtml OpenGL 4.x>.
glProgramUniform2fv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @value@ pointing to @2@ elements of type @GLfloat@.
  -> m ()
glProgramUniform2fv v1 v2 v3 v4 = liftIO $ dyn457 ptr_glProgramUniform2fv v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform2fv #-}
ptr_glProgramUniform2fv :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glProgramUniform2fv = unsafePerformIO $ getCommand "glProgramUniform2fv"

-- glProgramUniform2fvEXT ------------------------------------------------------

-- | This command is an alias for 'glProgramUniform2fv'.
glProgramUniform2fvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @value@ pointing to @count*2@ elements of type @GLfloat@.
  -> m ()
glProgramUniform2fvEXT v1 v2 v3 v4 = liftIO $ dyn457 ptr_glProgramUniform2fvEXT v1 v2 v3 v4

{-# NOINLINE ptr_glProgramUniform2fvEXT #-}
ptr_glProgramUniform2fvEXT :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glProgramUniform2fvEXT = unsafePerformIO $ getCommand "glProgramUniform2fvEXT"

