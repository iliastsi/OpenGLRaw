--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F48
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

module Graphics.GL.Functions.F48 (
  glPrimitiveRestartIndexNV,
  glPrimitiveRestartNV,
  glPrioritizeTextures,
  glPrioritizeTexturesEXT,
  glPrioritizeTexturesxOES,
  glProgramBinary,
  glProgramBinaryOES,
  glProgramBufferParametersIivNV,
  glProgramBufferParametersIuivNV,
  glProgramBufferParametersfvNV,
  glProgramEnvParameter4dARB,
  glProgramEnvParameter4dvARB,
  glProgramEnvParameter4fARB,
  glProgramEnvParameter4fvARB,
  glProgramEnvParameterI4iNV,
  glProgramEnvParameterI4ivNV,
  glProgramEnvParameterI4uiNV,
  glProgramEnvParameterI4uivNV,
  glProgramEnvParameters4fvEXT,
  glProgramEnvParametersI4ivNV,
  glProgramEnvParametersI4uivNV,
  glProgramLocalParameter4dARB,
  glProgramLocalParameter4dvARB,
  glProgramLocalParameter4fARB,
  glProgramLocalParameter4fvARB,
  glProgramLocalParameterI4iNV,
  glProgramLocalParameterI4ivNV,
  glProgramLocalParameterI4uiNV,
  glProgramLocalParameterI4uivNV,
  glProgramLocalParameters4fvEXT,
  glProgramLocalParametersI4ivNV,
  glProgramLocalParametersI4uivNV,
  glProgramNamedParameter4dNV,
  glProgramNamedParameter4dvNV,
  glProgramNamedParameter4fNV,
  glProgramNamedParameter4fvNV,
  glProgramParameter4dNV,
  glProgramParameter4dvNV,
  glProgramParameter4fNV,
  glProgramParameter4fvNV
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

-- glPrimitiveRestartIndexNV ---------------------------------------------------

glPrimitiveRestartIndexNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> m ()
glPrimitiveRestartIndexNV v1 = liftIO $ dyn2 ptr_glPrimitiveRestartIndexNV v1

{-# NOINLINE ptr_glPrimitiveRestartIndexNV #-}
ptr_glPrimitiveRestartIndexNV :: FunPtr (GLuint -> IO ())
ptr_glPrimitiveRestartIndexNV = unsafePerformIO $ getCommand "glPrimitiveRestartIndexNV"

-- glPrimitiveRestartNV --------------------------------------------------------

glPrimitiveRestartNV
  :: MonadIO m
  => m ()
glPrimitiveRestartNV = liftIO $ dyn10 ptr_glPrimitiveRestartNV

{-# NOINLINE ptr_glPrimitiveRestartNV #-}
ptr_glPrimitiveRestartNV :: FunPtr (IO ())
ptr_glPrimitiveRestartNV = unsafePerformIO $ getCommand "glPrimitiveRestartNV"

-- glPrioritizeTextures --------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glPrioritizeTextures.xml OpenGL 2.x>.
glPrioritizeTextures
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @textures@ pointing to @n@ elements of type @Texture@.
  -> Ptr GLfloat -- ^ @priorities@ pointing to @n@ elements of type @GLfloat@.
  -> m ()
glPrioritizeTextures v1 v2 v3 = liftIO $ dyn614 ptr_glPrioritizeTextures v1 v2 v3

{-# NOINLINE ptr_glPrioritizeTextures #-}
ptr_glPrioritizeTextures :: FunPtr (GLsizei -> Ptr GLuint -> Ptr GLfloat -> IO ())
ptr_glPrioritizeTextures = unsafePerformIO $ getCommand "glPrioritizeTextures"

-- glPrioritizeTexturesEXT -----------------------------------------------------

-- | This command is an alias for 'glPrioritizeTextures'.
glPrioritizeTexturesEXT
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @textures@ pointing to @n@ elements of type @Texture@.
  -> Ptr GLclampf -- ^ @priorities@ pointing to @n@ elements of type @ClampedFloat32@.
  -> m ()
glPrioritizeTexturesEXT v1 v2 v3 = liftIO $ dyn615 ptr_glPrioritizeTexturesEXT v1 v2 v3

{-# NOINLINE ptr_glPrioritizeTexturesEXT #-}
ptr_glPrioritizeTexturesEXT :: FunPtr (GLsizei -> Ptr GLuint -> Ptr GLclampf -> IO ())
ptr_glPrioritizeTexturesEXT = unsafePerformIO $ getCommand "glPrioritizeTexturesEXT"

-- glPrioritizeTexturesxOES ----------------------------------------------------

glPrioritizeTexturesxOES
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @textures@ pointing to @n@ elements of type @GLuint@.
  -> Ptr GLfixed -- ^ @priorities@ pointing to @n@ elements of type @ClampedFixed@.
  -> m ()
glPrioritizeTexturesxOES v1 v2 v3 = liftIO $ dyn616 ptr_glPrioritizeTexturesxOES v1 v2 v3

{-# NOINLINE ptr_glPrioritizeTexturesxOES #-}
ptr_glPrioritizeTexturesxOES :: FunPtr (GLsizei -> Ptr GLuint -> Ptr GLfixed -> IO ())
ptr_glPrioritizeTexturesxOES = unsafePerformIO $ getCommand "glPrioritizeTexturesxOES"

-- glProgramBinary -------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glProgramBinary.xhtml OpenGL 4.x>.
glProgramBinary
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @binaryFormat@.
  -> Ptr a -- ^ @binary@ pointing to @length@ elements of type @a@.
  -> GLsizei -- ^ @length@.
  -> m ()
glProgramBinary v1 v2 v3 v4 = liftIO $ dyn617 ptr_glProgramBinary v1 v2 v3 v4

{-# NOINLINE ptr_glProgramBinary #-}
ptr_glProgramBinary :: FunPtr (GLuint -> GLenum -> Ptr a -> GLsizei -> IO ())
ptr_glProgramBinary = unsafePerformIO $ getCommand "glProgramBinary"

-- glProgramBinaryOES ----------------------------------------------------------

-- | This command is an alias for 'glProgramBinary'.
glProgramBinaryOES
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @binaryFormat@.
  -> Ptr a -- ^ @binary@ pointing to @length@ elements of type @a@.
  -> GLint -- ^ @length@.
  -> m ()
glProgramBinaryOES v1 v2 v3 v4 = liftIO $ dyn618 ptr_glProgramBinaryOES v1 v2 v3 v4

{-# NOINLINE ptr_glProgramBinaryOES #-}
ptr_glProgramBinaryOES :: FunPtr (GLuint -> GLenum -> Ptr a -> GLint -> IO ())
ptr_glProgramBinaryOES = unsafePerformIO $ getCommand "glProgramBinaryOES"

-- glProgramBufferParametersIivNV ----------------------------------------------

glProgramBufferParametersIivNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ProgramTarget@.
  -> GLuint -- ^ @bindingIndex@.
  -> GLuint -- ^ @wordIndex@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint -- ^ @params@ pointing to @count@ elements of type @GLint@.
  -> m ()
glProgramBufferParametersIivNV v1 v2 v3 v4 v5 = liftIO $ dyn619 ptr_glProgramBufferParametersIivNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramBufferParametersIivNV #-}
ptr_glProgramBufferParametersIivNV :: FunPtr (GLenum -> GLuint -> GLuint -> GLsizei -> Ptr GLint -> IO ())
ptr_glProgramBufferParametersIivNV = unsafePerformIO $ getCommand "glProgramBufferParametersIivNV"

-- glProgramBufferParametersIuivNV ---------------------------------------------

glProgramBufferParametersIuivNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ProgramTarget@.
  -> GLuint -- ^ @bindingIndex@.
  -> GLuint -- ^ @wordIndex@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @params@ pointing to @count@ elements of type @GLuint@.
  -> m ()
glProgramBufferParametersIuivNV v1 v2 v3 v4 v5 = liftIO $ dyn620 ptr_glProgramBufferParametersIuivNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramBufferParametersIuivNV #-}
ptr_glProgramBufferParametersIuivNV :: FunPtr (GLenum -> GLuint -> GLuint -> GLsizei -> Ptr GLuint -> IO ())
ptr_glProgramBufferParametersIuivNV = unsafePerformIO $ getCommand "glProgramBufferParametersIuivNV"

-- glProgramBufferParametersfvNV -----------------------------------------------

glProgramBufferParametersfvNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ProgramTarget@.
  -> GLuint -- ^ @bindingIndex@.
  -> GLuint -- ^ @wordIndex@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @params@ pointing to @count@ elements of type @GLfloat@.
  -> m ()
glProgramBufferParametersfvNV v1 v2 v3 v4 v5 = liftIO $ dyn621 ptr_glProgramBufferParametersfvNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glProgramBufferParametersfvNV #-}
ptr_glProgramBufferParametersfvNV :: FunPtr (GLenum -> GLuint -> GLuint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glProgramBufferParametersfvNV = unsafePerformIO $ getCommand "glProgramBufferParametersfvNV"

-- glProgramEnvParameter4dARB --------------------------------------------------

-- | The vector equivalent of this command is 'glProgramEnvParameter4dvARB'.
glProgramEnvParameter4dARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ProgramTargetARB@.
  -> GLuint -- ^ @index@.
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> GLdouble -- ^ @z@.
  -> GLdouble -- ^ @w@.
  -> m ()
glProgramEnvParameter4dARB v1 v2 v3 v4 v5 v6 = liftIO $ dyn622 ptr_glProgramEnvParameter4dARB v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glProgramEnvParameter4dARB #-}
ptr_glProgramEnvParameter4dARB :: FunPtr (GLenum -> GLuint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glProgramEnvParameter4dARB = unsafePerformIO $ getCommand "glProgramEnvParameter4dARB"

-- glProgramEnvParameter4dvARB -------------------------------------------------

glProgramEnvParameter4dvARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ProgramTargetARB@.
  -> GLuint -- ^ @index@.
  -> Ptr GLdouble -- ^ @params@ pointing to @4@ elements of type @GLdouble@.
  -> m ()
glProgramEnvParameter4dvARB v1 v2 v3 = liftIO $ dyn330 ptr_glProgramEnvParameter4dvARB v1 v2 v3

{-# NOINLINE ptr_glProgramEnvParameter4dvARB #-}
ptr_glProgramEnvParameter4dvARB :: FunPtr (GLenum -> GLuint -> Ptr GLdouble -> IO ())
ptr_glProgramEnvParameter4dvARB = unsafePerformIO $ getCommand "glProgramEnvParameter4dvARB"

-- glProgramEnvParameter4fARB --------------------------------------------------

-- | The vector equivalent of this command is 'glProgramEnvParameter4fvARB'.
glProgramEnvParameter4fARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ProgramTargetARB@.
  -> GLuint -- ^ @index@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> GLfloat -- ^ @w@.
  -> m ()
glProgramEnvParameter4fARB v1 v2 v3 v4 v5 v6 = liftIO $ dyn623 ptr_glProgramEnvParameter4fARB v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glProgramEnvParameter4fARB #-}
ptr_glProgramEnvParameter4fARB :: FunPtr (GLenum -> GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glProgramEnvParameter4fARB = unsafePerformIO $ getCommand "glProgramEnvParameter4fARB"

-- glProgramEnvParameter4fvARB -------------------------------------------------

glProgramEnvParameter4fvARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ProgramTargetARB@.
  -> GLuint -- ^ @index@.
  -> Ptr GLfloat -- ^ @params@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glProgramEnvParameter4fvARB v1 v2 v3 = liftIO $ dyn267 ptr_glProgramEnvParameter4fvARB v1 v2 v3

{-# NOINLINE ptr_glProgramEnvParameter4fvARB #-}
ptr_glProgramEnvParameter4fvARB :: FunPtr (GLenum -> GLuint -> Ptr GLfloat -> IO ())
ptr_glProgramEnvParameter4fvARB = unsafePerformIO $ getCommand "glProgramEnvParameter4fvARB"

-- glProgramEnvParameterI4iNV --------------------------------------------------

-- | The vector equivalent of this command is 'glProgramEnvParameterI4ivNV'.
glProgramEnvParameterI4iNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ProgramTarget@.
  -> GLuint -- ^ @index@.
  -> GLint -- ^ @x@.
  -> GLint -- ^ @y@.
  -> GLint -- ^ @z@.
  -> GLint -- ^ @w@.
  -> m ()
glProgramEnvParameterI4iNV v1 v2 v3 v4 v5 v6 = liftIO $ dyn624 ptr_glProgramEnvParameterI4iNV v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glProgramEnvParameterI4iNV #-}
ptr_glProgramEnvParameterI4iNV :: FunPtr (GLenum -> GLuint -> GLint -> GLint -> GLint -> GLint -> IO ())
ptr_glProgramEnvParameterI4iNV = unsafePerformIO $ getCommand "glProgramEnvParameterI4iNV"

-- glProgramEnvParameterI4ivNV -------------------------------------------------

glProgramEnvParameterI4ivNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ProgramTarget@.
  -> GLuint -- ^ @index@.
  -> Ptr GLint -- ^ @params@ pointing to @4@ elements of type @GLint@.
  -> m ()
glProgramEnvParameterI4ivNV v1 v2 v3 = liftIO $ dyn342 ptr_glProgramEnvParameterI4ivNV v1 v2 v3

{-# NOINLINE ptr_glProgramEnvParameterI4ivNV #-}
ptr_glProgramEnvParameterI4ivNV :: FunPtr (GLenum -> GLuint -> Ptr GLint -> IO ())
ptr_glProgramEnvParameterI4ivNV = unsafePerformIO $ getCommand "glProgramEnvParameterI4ivNV"

-- glProgramEnvParameterI4uiNV -------------------------------------------------

-- | The vector equivalent of this command is 'glProgramEnvParameterI4uivNV'.
glProgramEnvParameterI4uiNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ProgramTarget@.
  -> GLuint -- ^ @index@.
  -> GLuint -- ^ @x@.
  -> GLuint -- ^ @y@.
  -> GLuint -- ^ @z@.
  -> GLuint -- ^ @w@.
  -> m ()
glProgramEnvParameterI4uiNV v1 v2 v3 v4 v5 v6 = liftIO $ dyn6 ptr_glProgramEnvParameterI4uiNV v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glProgramEnvParameterI4uiNV #-}
ptr_glProgramEnvParameterI4uiNV :: FunPtr (GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ())
ptr_glProgramEnvParameterI4uiNV = unsafePerformIO $ getCommand "glProgramEnvParameterI4uiNV"

-- glProgramEnvParameterI4uivNV ------------------------------------------------

glProgramEnvParameterI4uivNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ProgramTarget@.
  -> GLuint -- ^ @index@.
  -> Ptr GLuint -- ^ @params@ pointing to @4@ elements of type @GLuint@.
  -> m ()
glProgramEnvParameterI4uivNV v1 v2 v3 = liftIO $ dyn214 ptr_glProgramEnvParameterI4uivNV v1 v2 v3

{-# NOINLINE ptr_glProgramEnvParameterI4uivNV #-}
ptr_glProgramEnvParameterI4uivNV :: FunPtr (GLenum -> GLuint -> Ptr GLuint -> IO ())
ptr_glProgramEnvParameterI4uivNV = unsafePerformIO $ getCommand "glProgramEnvParameterI4uivNV"

-- glProgramEnvParameters4fvEXT ------------------------------------------------

glProgramEnvParameters4fvEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ProgramTargetARB@.
  -> GLuint -- ^ @index@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @params@ pointing to @count*4@ elements of type @GLfloat@.
  -> m ()
glProgramEnvParameters4fvEXT v1 v2 v3 v4 = liftIO $ dyn284 ptr_glProgramEnvParameters4fvEXT v1 v2 v3 v4

{-# NOINLINE ptr_glProgramEnvParameters4fvEXT #-}
ptr_glProgramEnvParameters4fvEXT :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glProgramEnvParameters4fvEXT = unsafePerformIO $ getCommand "glProgramEnvParameters4fvEXT"

-- glProgramEnvParametersI4ivNV ------------------------------------------------

glProgramEnvParametersI4ivNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ProgramTarget@.
  -> GLuint -- ^ @index@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint -- ^ @params@ pointing to @count*4@ elements of type @GLint@.
  -> m ()
glProgramEnvParametersI4ivNV v1 v2 v3 v4 = liftIO $ dyn625 ptr_glProgramEnvParametersI4ivNV v1 v2 v3 v4

{-# NOINLINE ptr_glProgramEnvParametersI4ivNV #-}
ptr_glProgramEnvParametersI4ivNV :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLint -> IO ())
ptr_glProgramEnvParametersI4ivNV = unsafePerformIO $ getCommand "glProgramEnvParametersI4ivNV"

-- glProgramEnvParametersI4uivNV -----------------------------------------------

glProgramEnvParametersI4uivNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ProgramTarget@.
  -> GLuint -- ^ @index@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @params@ pointing to @count*4@ elements of type @GLuint@.
  -> m ()
glProgramEnvParametersI4uivNV v1 v2 v3 v4 = liftIO $ dyn22 ptr_glProgramEnvParametersI4uivNV v1 v2 v3 v4

{-# NOINLINE ptr_glProgramEnvParametersI4uivNV #-}
ptr_glProgramEnvParametersI4uivNV :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLuint -> IO ())
ptr_glProgramEnvParametersI4uivNV = unsafePerformIO $ getCommand "glProgramEnvParametersI4uivNV"

-- glProgramLocalParameter4dARB ------------------------------------------------

-- | The vector equivalent of this command is 'glProgramLocalParameter4dvARB'.
glProgramLocalParameter4dARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ProgramTargetARB@.
  -> GLuint -- ^ @index@.
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> GLdouble -- ^ @z@.
  -> GLdouble -- ^ @w@.
  -> m ()
glProgramLocalParameter4dARB v1 v2 v3 v4 v5 v6 = liftIO $ dyn622 ptr_glProgramLocalParameter4dARB v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glProgramLocalParameter4dARB #-}
ptr_glProgramLocalParameter4dARB :: FunPtr (GLenum -> GLuint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glProgramLocalParameter4dARB = unsafePerformIO $ getCommand "glProgramLocalParameter4dARB"

-- glProgramLocalParameter4dvARB -----------------------------------------------

glProgramLocalParameter4dvARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ProgramTargetARB@.
  -> GLuint -- ^ @index@.
  -> Ptr GLdouble -- ^ @params@ pointing to @4@ elements of type @GLdouble@.
  -> m ()
glProgramLocalParameter4dvARB v1 v2 v3 = liftIO $ dyn330 ptr_glProgramLocalParameter4dvARB v1 v2 v3

{-# NOINLINE ptr_glProgramLocalParameter4dvARB #-}
ptr_glProgramLocalParameter4dvARB :: FunPtr (GLenum -> GLuint -> Ptr GLdouble -> IO ())
ptr_glProgramLocalParameter4dvARB = unsafePerformIO $ getCommand "glProgramLocalParameter4dvARB"

-- glProgramLocalParameter4fARB ------------------------------------------------

-- | The vector equivalent of this command is 'glProgramLocalParameter4fvARB'.
glProgramLocalParameter4fARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ProgramTargetARB@.
  -> GLuint -- ^ @index@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> GLfloat -- ^ @w@.
  -> m ()
glProgramLocalParameter4fARB v1 v2 v3 v4 v5 v6 = liftIO $ dyn623 ptr_glProgramLocalParameter4fARB v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glProgramLocalParameter4fARB #-}
ptr_glProgramLocalParameter4fARB :: FunPtr (GLenum -> GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glProgramLocalParameter4fARB = unsafePerformIO $ getCommand "glProgramLocalParameter4fARB"

-- glProgramLocalParameter4fvARB -----------------------------------------------

glProgramLocalParameter4fvARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ProgramTargetARB@.
  -> GLuint -- ^ @index@.
  -> Ptr GLfloat -- ^ @params@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glProgramLocalParameter4fvARB v1 v2 v3 = liftIO $ dyn267 ptr_glProgramLocalParameter4fvARB v1 v2 v3

{-# NOINLINE ptr_glProgramLocalParameter4fvARB #-}
ptr_glProgramLocalParameter4fvARB :: FunPtr (GLenum -> GLuint -> Ptr GLfloat -> IO ())
ptr_glProgramLocalParameter4fvARB = unsafePerformIO $ getCommand "glProgramLocalParameter4fvARB"

-- glProgramLocalParameterI4iNV ------------------------------------------------

-- | The vector equivalent of this command is 'glProgramLocalParameterI4ivNV'.
glProgramLocalParameterI4iNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ProgramTarget@.
  -> GLuint -- ^ @index@.
  -> GLint -- ^ @x@.
  -> GLint -- ^ @y@.
  -> GLint -- ^ @z@.
  -> GLint -- ^ @w@.
  -> m ()
glProgramLocalParameterI4iNV v1 v2 v3 v4 v5 v6 = liftIO $ dyn624 ptr_glProgramLocalParameterI4iNV v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glProgramLocalParameterI4iNV #-}
ptr_glProgramLocalParameterI4iNV :: FunPtr (GLenum -> GLuint -> GLint -> GLint -> GLint -> GLint -> IO ())
ptr_glProgramLocalParameterI4iNV = unsafePerformIO $ getCommand "glProgramLocalParameterI4iNV"

-- glProgramLocalParameterI4ivNV -----------------------------------------------

glProgramLocalParameterI4ivNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ProgramTarget@.
  -> GLuint -- ^ @index@.
  -> Ptr GLint -- ^ @params@ pointing to @4@ elements of type @GLint@.
  -> m ()
glProgramLocalParameterI4ivNV v1 v2 v3 = liftIO $ dyn342 ptr_glProgramLocalParameterI4ivNV v1 v2 v3

{-# NOINLINE ptr_glProgramLocalParameterI4ivNV #-}
ptr_glProgramLocalParameterI4ivNV :: FunPtr (GLenum -> GLuint -> Ptr GLint -> IO ())
ptr_glProgramLocalParameterI4ivNV = unsafePerformIO $ getCommand "glProgramLocalParameterI4ivNV"

-- glProgramLocalParameterI4uiNV -----------------------------------------------

-- | The vector equivalent of this command is 'glProgramLocalParameterI4uivNV'.
glProgramLocalParameterI4uiNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ProgramTarget@.
  -> GLuint -- ^ @index@.
  -> GLuint -- ^ @x@.
  -> GLuint -- ^ @y@.
  -> GLuint -- ^ @z@.
  -> GLuint -- ^ @w@.
  -> m ()
glProgramLocalParameterI4uiNV v1 v2 v3 v4 v5 v6 = liftIO $ dyn6 ptr_glProgramLocalParameterI4uiNV v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glProgramLocalParameterI4uiNV #-}
ptr_glProgramLocalParameterI4uiNV :: FunPtr (GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ())
ptr_glProgramLocalParameterI4uiNV = unsafePerformIO $ getCommand "glProgramLocalParameterI4uiNV"

-- glProgramLocalParameterI4uivNV ----------------------------------------------

glProgramLocalParameterI4uivNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ProgramTarget@.
  -> GLuint -- ^ @index@.
  -> Ptr GLuint -- ^ @params@ pointing to @4@ elements of type @GLuint@.
  -> m ()
glProgramLocalParameterI4uivNV v1 v2 v3 = liftIO $ dyn214 ptr_glProgramLocalParameterI4uivNV v1 v2 v3

{-# NOINLINE ptr_glProgramLocalParameterI4uivNV #-}
ptr_glProgramLocalParameterI4uivNV :: FunPtr (GLenum -> GLuint -> Ptr GLuint -> IO ())
ptr_glProgramLocalParameterI4uivNV = unsafePerformIO $ getCommand "glProgramLocalParameterI4uivNV"

-- glProgramLocalParameters4fvEXT ----------------------------------------------

glProgramLocalParameters4fvEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ProgramTargetARB@.
  -> GLuint -- ^ @index@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @params@ pointing to @count*4@ elements of type @GLfloat@.
  -> m ()
glProgramLocalParameters4fvEXT v1 v2 v3 v4 = liftIO $ dyn284 ptr_glProgramLocalParameters4fvEXT v1 v2 v3 v4

{-# NOINLINE ptr_glProgramLocalParameters4fvEXT #-}
ptr_glProgramLocalParameters4fvEXT :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glProgramLocalParameters4fvEXT = unsafePerformIO $ getCommand "glProgramLocalParameters4fvEXT"

-- glProgramLocalParametersI4ivNV ----------------------------------------------

glProgramLocalParametersI4ivNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ProgramTarget@.
  -> GLuint -- ^ @index@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint -- ^ @params@ pointing to @count*4@ elements of type @GLint@.
  -> m ()
glProgramLocalParametersI4ivNV v1 v2 v3 v4 = liftIO $ dyn625 ptr_glProgramLocalParametersI4ivNV v1 v2 v3 v4

{-# NOINLINE ptr_glProgramLocalParametersI4ivNV #-}
ptr_glProgramLocalParametersI4ivNV :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLint -> IO ())
ptr_glProgramLocalParametersI4ivNV = unsafePerformIO $ getCommand "glProgramLocalParametersI4ivNV"

-- glProgramLocalParametersI4uivNV ---------------------------------------------

glProgramLocalParametersI4uivNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ProgramTarget@.
  -> GLuint -- ^ @index@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @params@ pointing to @count*4@ elements of type @GLuint@.
  -> m ()
glProgramLocalParametersI4uivNV v1 v2 v3 v4 = liftIO $ dyn22 ptr_glProgramLocalParametersI4uivNV v1 v2 v3 v4

{-# NOINLINE ptr_glProgramLocalParametersI4uivNV #-}
ptr_glProgramLocalParametersI4uivNV :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLuint -> IO ())
ptr_glProgramLocalParametersI4uivNV = unsafePerformIO $ getCommand "glProgramLocalParametersI4uivNV"

-- glProgramNamedParameter4dNV -------------------------------------------------

-- | The vector equivalent of this command is 'glProgramNamedParameter4dvNV'.
glProgramNamedParameter4dNV
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLsizei -- ^ @len@.
  -> Ptr GLubyte -- ^ @name@ pointing to @1@ element of type @GLubyte@.
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> GLdouble -- ^ @z@.
  -> GLdouble -- ^ @w@.
  -> m ()
glProgramNamedParameter4dNV v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn626 ptr_glProgramNamedParameter4dNV v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glProgramNamedParameter4dNV #-}
ptr_glProgramNamedParameter4dNV :: FunPtr (GLuint -> GLsizei -> Ptr GLubyte -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glProgramNamedParameter4dNV = unsafePerformIO $ getCommand "glProgramNamedParameter4dNV"

-- glProgramNamedParameter4dvNV ------------------------------------------------

glProgramNamedParameter4dvNV
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLsizei -- ^ @len@.
  -> Ptr GLubyte -- ^ @name@ pointing to @1@ element of type @GLubyte@.
  -> Ptr GLdouble -- ^ @v@ pointing to @4@ elements of type @GLdouble@.
  -> m ()
glProgramNamedParameter4dvNV v1 v2 v3 v4 = liftIO $ dyn393 ptr_glProgramNamedParameter4dvNV v1 v2 v3 v4

{-# NOINLINE ptr_glProgramNamedParameter4dvNV #-}
ptr_glProgramNamedParameter4dvNV :: FunPtr (GLuint -> GLsizei -> Ptr GLubyte -> Ptr GLdouble -> IO ())
ptr_glProgramNamedParameter4dvNV = unsafePerformIO $ getCommand "glProgramNamedParameter4dvNV"

-- glProgramNamedParameter4fNV -------------------------------------------------

-- | The vector equivalent of this command is 'glProgramNamedParameter4fvNV'.
glProgramNamedParameter4fNV
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLsizei -- ^ @len@.
  -> Ptr GLubyte -- ^ @name@ pointing to @1@ element of type @GLubyte@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> GLfloat -- ^ @w@.
  -> m ()
glProgramNamedParameter4fNV v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn627 ptr_glProgramNamedParameter4fNV v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glProgramNamedParameter4fNV #-}
ptr_glProgramNamedParameter4fNV :: FunPtr (GLuint -> GLsizei -> Ptr GLubyte -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glProgramNamedParameter4fNV = unsafePerformIO $ getCommand "glProgramNamedParameter4fNV"

-- glProgramNamedParameter4fvNV ------------------------------------------------

glProgramNamedParameter4fvNV
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLsizei -- ^ @len@.
  -> Ptr GLubyte -- ^ @name@ pointing to @1@ element of type @GLubyte@.
  -> Ptr GLfloat -- ^ @v@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glProgramNamedParameter4fvNV v1 v2 v3 v4 = liftIO $ dyn394 ptr_glProgramNamedParameter4fvNV v1 v2 v3 v4

{-# NOINLINE ptr_glProgramNamedParameter4fvNV #-}
ptr_glProgramNamedParameter4fvNV :: FunPtr (GLuint -> GLsizei -> Ptr GLubyte -> Ptr GLfloat -> IO ())
ptr_glProgramNamedParameter4fvNV = unsafePerformIO $ getCommand "glProgramNamedParameter4fvNV"

-- glProgramParameter4dNV ------------------------------------------------------

-- | The vector equivalent of this command is 'glProgramParameter4dvNV'.
glProgramParameter4dNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type @VertexAttribEnumNV@.
  -> GLuint -- ^ @index@.
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> GLdouble -- ^ @z@.
  -> GLdouble -- ^ @w@.
  -> m ()
glProgramParameter4dNV v1 v2 v3 v4 v5 v6 = liftIO $ dyn622 ptr_glProgramParameter4dNV v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glProgramParameter4dNV #-}
ptr_glProgramParameter4dNV :: FunPtr (GLenum -> GLuint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glProgramParameter4dNV = unsafePerformIO $ getCommand "glProgramParameter4dNV"

-- glProgramParameter4dvNV -----------------------------------------------------

glProgramParameter4dvNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type @VertexAttribEnumNV@.
  -> GLuint -- ^ @index@.
  -> Ptr GLdouble -- ^ @v@ pointing to @4@ elements of type @GLdouble@.
  -> m ()
glProgramParameter4dvNV v1 v2 v3 = liftIO $ dyn330 ptr_glProgramParameter4dvNV v1 v2 v3

{-# NOINLINE ptr_glProgramParameter4dvNV #-}
ptr_glProgramParameter4dvNV :: FunPtr (GLenum -> GLuint -> Ptr GLdouble -> IO ())
ptr_glProgramParameter4dvNV = unsafePerformIO $ getCommand "glProgramParameter4dvNV"

-- glProgramParameter4fNV ------------------------------------------------------

-- | The vector equivalent of this command is 'glProgramParameter4fvNV'.
glProgramParameter4fNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type @VertexAttribEnumNV@.
  -> GLuint -- ^ @index@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> GLfloat -- ^ @w@.
  -> m ()
glProgramParameter4fNV v1 v2 v3 v4 v5 v6 = liftIO $ dyn623 ptr_glProgramParameter4fNV v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glProgramParameter4fNV #-}
ptr_glProgramParameter4fNV :: FunPtr (GLenum -> GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glProgramParameter4fNV = unsafePerformIO $ getCommand "glProgramParameter4fNV"

-- glProgramParameter4fvNV -----------------------------------------------------

glProgramParameter4fvNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type @VertexAttribEnumNV@.
  -> GLuint -- ^ @index@.
  -> Ptr GLfloat -- ^ @v@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glProgramParameter4fvNV v1 v2 v3 = liftIO $ dyn267 ptr_glProgramParameter4fvNV v1 v2 v3

{-# NOINLINE ptr_glProgramParameter4fvNV #-}
ptr_glProgramParameter4fvNV :: FunPtr (GLenum -> GLuint -> Ptr GLfloat -> IO ())
ptr_glProgramParameter4fvNV = unsafePerformIO $ getCommand "glProgramParameter4fvNV"

