--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F39
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

module Graphics.GL.Functions.F39 (
  glMatrixPushEXT,
  glMatrixRotatedEXT,
  glMatrixRotatefEXT,
  glMatrixScaledEXT,
  glMatrixScalefEXT,
  glMatrixTranslatedEXT,
  glMatrixTranslatefEXT,
  glMaxShaderCompilerThreadsARB,
  glMemoryBarrier,
  glMemoryBarrierByRegion,
  glMemoryBarrierEXT,
  glMinSampleShading,
  glMinSampleShadingARB,
  glMinSampleShadingOES,
  glMinmax,
  glMinmaxEXT,
  glMultMatrixd,
  glMultMatrixf,
  glMultMatrixx,
  glMultMatrixxOES,
  glMultTransposeMatrixd,
  glMultTransposeMatrixdARB,
  glMultTransposeMatrixf,
  glMultTransposeMatrixfARB,
  glMultTransposeMatrixxOES,
  glMultiDrawArrays,
  glMultiDrawArraysEXT,
  glMultiDrawArraysIndirect,
  glMultiDrawArraysIndirectAMD,
  glMultiDrawArraysIndirectBindlessCountNV,
  glMultiDrawArraysIndirectBindlessNV,
  glMultiDrawArraysIndirectCountARB,
  glMultiDrawArraysIndirectEXT,
  glMultiDrawElementArrayAPPLE,
  glMultiDrawElements,
  glMultiDrawElementsBaseVertex,
  glMultiDrawElementsBaseVertexEXT,
  glMultiDrawElementsBaseVertexOES,
  glMultiDrawElementsEXT,
  glMultiDrawElementsIndirect
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

-- glMatrixPushEXT -------------------------------------------------------------

glMatrixPushEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [MatrixMode](Graphics-GL-Groups.html#MatrixMode).
  -> m ()
glMatrixPushEXT v1 = liftIO $ dyn4 ptr_glMatrixPushEXT v1

{-# NOINLINE ptr_glMatrixPushEXT #-}
ptr_glMatrixPushEXT :: FunPtr (GLenum -> IO ())
ptr_glMatrixPushEXT = unsafePerformIO $ getCommand "glMatrixPushEXT"

-- glMatrixRotatedEXT ----------------------------------------------------------

glMatrixRotatedEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [MatrixMode](Graphics-GL-Groups.html#MatrixMode).
  -> GLdouble -- ^ @angle@.
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> GLdouble -- ^ @z@.
  -> m ()
glMatrixRotatedEXT v1 v2 v3 v4 v5 = liftIO $ dyn520 ptr_glMatrixRotatedEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glMatrixRotatedEXT #-}
ptr_glMatrixRotatedEXT :: FunPtr (GLenum -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glMatrixRotatedEXT = unsafePerformIO $ getCommand "glMatrixRotatedEXT"

-- glMatrixRotatefEXT ----------------------------------------------------------

glMatrixRotatefEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [MatrixMode](Graphics-GL-Groups.html#MatrixMode).
  -> GLfloat -- ^ @angle@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> m ()
glMatrixRotatefEXT v1 v2 v3 v4 v5 = liftIO $ dyn521 ptr_glMatrixRotatefEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glMatrixRotatefEXT #-}
ptr_glMatrixRotatefEXT :: FunPtr (GLenum -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glMatrixRotatefEXT = unsafePerformIO $ getCommand "glMatrixRotatefEXT"

-- glMatrixScaledEXT -----------------------------------------------------------

glMatrixScaledEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [MatrixMode](Graphics-GL-Groups.html#MatrixMode).
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> GLdouble -- ^ @z@.
  -> m ()
glMatrixScaledEXT v1 v2 v3 v4 = liftIO $ dyn522 ptr_glMatrixScaledEXT v1 v2 v3 v4

{-# NOINLINE ptr_glMatrixScaledEXT #-}
ptr_glMatrixScaledEXT :: FunPtr (GLenum -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glMatrixScaledEXT = unsafePerformIO $ getCommand "glMatrixScaledEXT"

-- glMatrixScalefEXT -----------------------------------------------------------

glMatrixScalefEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [MatrixMode](Graphics-GL-Groups.html#MatrixMode).
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> m ()
glMatrixScalefEXT v1 v2 v3 v4 = liftIO $ dyn523 ptr_glMatrixScalefEXT v1 v2 v3 v4

{-# NOINLINE ptr_glMatrixScalefEXT #-}
ptr_glMatrixScalefEXT :: FunPtr (GLenum -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glMatrixScalefEXT = unsafePerformIO $ getCommand "glMatrixScalefEXT"

-- glMatrixTranslatedEXT -------------------------------------------------------

glMatrixTranslatedEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [MatrixMode](Graphics-GL-Groups.html#MatrixMode).
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> GLdouble -- ^ @z@.
  -> m ()
glMatrixTranslatedEXT v1 v2 v3 v4 = liftIO $ dyn522 ptr_glMatrixTranslatedEXT v1 v2 v3 v4

{-# NOINLINE ptr_glMatrixTranslatedEXT #-}
ptr_glMatrixTranslatedEXT :: FunPtr (GLenum -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glMatrixTranslatedEXT = unsafePerformIO $ getCommand "glMatrixTranslatedEXT"

-- glMatrixTranslatefEXT -------------------------------------------------------

glMatrixTranslatefEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [MatrixMode](Graphics-GL-Groups.html#MatrixMode).
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> m ()
glMatrixTranslatefEXT v1 v2 v3 v4 = liftIO $ dyn523 ptr_glMatrixTranslatefEXT v1 v2 v3 v4

{-# NOINLINE ptr_glMatrixTranslatefEXT #-}
ptr_glMatrixTranslatefEXT :: FunPtr (GLenum -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glMatrixTranslatefEXT = unsafePerformIO $ getCommand "glMatrixTranslatefEXT"

-- glMaxShaderCompilerThreadsARB -----------------------------------------------

glMaxShaderCompilerThreadsARB
  :: MonadIO m
  => GLuint -- ^ @count@.
  -> m ()
glMaxShaderCompilerThreadsARB v1 = liftIO $ dyn2 ptr_glMaxShaderCompilerThreadsARB v1

{-# NOINLINE ptr_glMaxShaderCompilerThreadsARB #-}
ptr_glMaxShaderCompilerThreadsARB :: FunPtr (GLuint -> IO ())
ptr_glMaxShaderCompilerThreadsARB = unsafePerformIO $ getCommand "glMaxShaderCompilerThreadsARB"

-- glMemoryBarrier -------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glMemoryBarrier.xhtml OpenGL 4.x>.
glMemoryBarrier
  :: MonadIO m
  => GLbitfield -- ^ @barriers@.
  -> m ()
glMemoryBarrier v1 = liftIO $ dyn69 ptr_glMemoryBarrier v1

{-# NOINLINE ptr_glMemoryBarrier #-}
ptr_glMemoryBarrier :: FunPtr (GLbitfield -> IO ())
ptr_glMemoryBarrier = unsafePerformIO $ getCommand "glMemoryBarrier"

-- glMemoryBarrierByRegion -----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glMemoryBarrier.xhtml OpenGL 4.x>.
glMemoryBarrierByRegion
  :: MonadIO m
  => GLbitfield -- ^ @barriers@.
  -> m ()
glMemoryBarrierByRegion v1 = liftIO $ dyn69 ptr_glMemoryBarrierByRegion v1

{-# NOINLINE ptr_glMemoryBarrierByRegion #-}
ptr_glMemoryBarrierByRegion :: FunPtr (GLbitfield -> IO ())
ptr_glMemoryBarrierByRegion = unsafePerformIO $ getCommand "glMemoryBarrierByRegion"

-- glMemoryBarrierEXT ----------------------------------------------------------

-- | This command is an alias for 'glMemoryBarrier'.
glMemoryBarrierEXT
  :: MonadIO m
  => GLbitfield -- ^ @barriers@.
  -> m ()
glMemoryBarrierEXT v1 = liftIO $ dyn69 ptr_glMemoryBarrierEXT v1

{-# NOINLINE ptr_glMemoryBarrierEXT #-}
ptr_glMemoryBarrierEXT :: FunPtr (GLbitfield -> IO ())
ptr_glMemoryBarrierEXT = unsafePerformIO $ getCommand "glMemoryBarrierEXT"

-- glMinSampleShading ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glMinSampleShading.xhtml OpenGL 4.x>.
glMinSampleShading
  :: MonadIO m
  => GLfloat -- ^ @value@ of type @ColorF@.
  -> m ()
glMinSampleShading v1 = liftIO $ dyn79 ptr_glMinSampleShading v1

{-# NOINLINE ptr_glMinSampleShading #-}
ptr_glMinSampleShading :: FunPtr (GLfloat -> IO ())
ptr_glMinSampleShading = unsafePerformIO $ getCommand "glMinSampleShading"

-- glMinSampleShadingARB -------------------------------------------------------

-- | This command is an alias for 'glMinSampleShading'.
glMinSampleShadingARB
  :: MonadIO m
  => GLfloat -- ^ @value@ of type @ColorF@.
  -> m ()
glMinSampleShadingARB v1 = liftIO $ dyn79 ptr_glMinSampleShadingARB v1

{-# NOINLINE ptr_glMinSampleShadingARB #-}
ptr_glMinSampleShadingARB :: FunPtr (GLfloat -> IO ())
ptr_glMinSampleShadingARB = unsafePerformIO $ getCommand "glMinSampleShadingARB"

-- glMinSampleShadingOES -------------------------------------------------------

-- | This command is an alias for 'glMinSampleShading'.
glMinSampleShadingOES
  :: MonadIO m
  => GLfloat -- ^ @value@ of type @ColorF@.
  -> m ()
glMinSampleShadingOES v1 = liftIO $ dyn79 ptr_glMinSampleShadingOES v1

{-# NOINLINE ptr_glMinSampleShadingOES #-}
ptr_glMinSampleShadingOES :: FunPtr (GLfloat -> IO ())
ptr_glMinSampleShadingOES = unsafePerformIO $ getCommand "glMinSampleShadingOES"

-- glMinmax --------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMinmax.xml OpenGL 2.x>.
glMinmax
  :: MonadIO m
  => GLenum -- ^ @target@ of type @MinmaxTarget@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLboolean -- ^ @sink@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glMinmax v1 v2 v3 = liftIO $ dyn524 ptr_glMinmax v1 v2 v3

{-# NOINLINE ptr_glMinmax #-}
ptr_glMinmax :: FunPtr (GLenum -> GLenum -> GLboolean -> IO ())
ptr_glMinmax = unsafePerformIO $ getCommand "glMinmax"

-- glMinmaxEXT -----------------------------------------------------------------

-- | This command is an alias for 'glMinmax'.
glMinmaxEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [MinmaxTargetEXT](Graphics-GL-Groups.html#MinmaxTargetEXT).
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLboolean -- ^ @sink@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glMinmaxEXT v1 v2 v3 = liftIO $ dyn524 ptr_glMinmaxEXT v1 v2 v3

{-# NOINLINE ptr_glMinmaxEXT #-}
ptr_glMinmaxEXT :: FunPtr (GLenum -> GLenum -> GLboolean -> IO ())
ptr_glMinmaxEXT = unsafePerformIO $ getCommand "glMinmaxEXT"

-- glMultMatrixd ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultMatrix.xml OpenGL 2.x>.
glMultMatrixd
  :: MonadIO m
  => Ptr GLdouble -- ^ @m@ pointing to @16@ elements of type @GLdouble@.
  -> m ()
glMultMatrixd v1 = liftIO $ dyn39 ptr_glMultMatrixd v1

{-# NOINLINE ptr_glMultMatrixd #-}
ptr_glMultMatrixd :: FunPtr (Ptr GLdouble -> IO ())
ptr_glMultMatrixd = unsafePerformIO $ getCommand "glMultMatrixd"

-- glMultMatrixf ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultMatrix.xml OpenGL 2.x>.
glMultMatrixf
  :: MonadIO m
  => Ptr GLfloat -- ^ @m@ pointing to @16@ elements of type @GLfloat@.
  -> m ()
glMultMatrixf v1 = liftIO $ dyn41 ptr_glMultMatrixf v1

{-# NOINLINE ptr_glMultMatrixf #-}
ptr_glMultMatrixf :: FunPtr (Ptr GLfloat -> IO ())
ptr_glMultMatrixf = unsafePerformIO $ getCommand "glMultMatrixf"

-- glMultMatrixx ---------------------------------------------------------------

glMultMatrixx
  :: MonadIO m
  => Ptr GLfixed -- ^ @m@ pointing to @16@ elements of type @GLfixed@.
  -> m ()
glMultMatrixx v1 = liftIO $ dyn107 ptr_glMultMatrixx v1

{-# NOINLINE ptr_glMultMatrixx #-}
ptr_glMultMatrixx :: FunPtr (Ptr GLfixed -> IO ())
ptr_glMultMatrixx = unsafePerformIO $ getCommand "glMultMatrixx"

-- glMultMatrixxOES ------------------------------------------------------------

glMultMatrixxOES
  :: MonadIO m
  => Ptr GLfixed -- ^ @m@ pointing to @16@ elements of type @GLfixed@.
  -> m ()
glMultMatrixxOES v1 = liftIO $ dyn107 ptr_glMultMatrixxOES v1

{-# NOINLINE ptr_glMultMatrixxOES #-}
ptr_glMultMatrixxOES :: FunPtr (Ptr GLfixed -> IO ())
ptr_glMultMatrixxOES = unsafePerformIO $ getCommand "glMultMatrixxOES"

-- glMultTransposeMatrixd ------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultTransposeMatrix.xml OpenGL 2.x>.
glMultTransposeMatrixd
  :: MonadIO m
  => Ptr GLdouble -- ^ @m@ pointing to @16@ elements of type @GLdouble@.
  -> m ()
glMultTransposeMatrixd v1 = liftIO $ dyn39 ptr_glMultTransposeMatrixd v1

{-# NOINLINE ptr_glMultTransposeMatrixd #-}
ptr_glMultTransposeMatrixd :: FunPtr (Ptr GLdouble -> IO ())
ptr_glMultTransposeMatrixd = unsafePerformIO $ getCommand "glMultTransposeMatrixd"

-- glMultTransposeMatrixdARB ---------------------------------------------------

-- | This command is an alias for 'glMultTransposeMatrixd'.
glMultTransposeMatrixdARB
  :: MonadIO m
  => Ptr GLdouble -- ^ @m@ pointing to @16@ elements of type @GLdouble@.
  -> m ()
glMultTransposeMatrixdARB v1 = liftIO $ dyn39 ptr_glMultTransposeMatrixdARB v1

{-# NOINLINE ptr_glMultTransposeMatrixdARB #-}
ptr_glMultTransposeMatrixdARB :: FunPtr (Ptr GLdouble -> IO ())
ptr_glMultTransposeMatrixdARB = unsafePerformIO $ getCommand "glMultTransposeMatrixdARB"

-- glMultTransposeMatrixf ------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultTransposeMatrix.xml OpenGL 2.x>.
glMultTransposeMatrixf
  :: MonadIO m
  => Ptr GLfloat -- ^ @m@ pointing to @16@ elements of type @GLfloat@.
  -> m ()
glMultTransposeMatrixf v1 = liftIO $ dyn41 ptr_glMultTransposeMatrixf v1

{-# NOINLINE ptr_glMultTransposeMatrixf #-}
ptr_glMultTransposeMatrixf :: FunPtr (Ptr GLfloat -> IO ())
ptr_glMultTransposeMatrixf = unsafePerformIO $ getCommand "glMultTransposeMatrixf"

-- glMultTransposeMatrixfARB ---------------------------------------------------

-- | This command is an alias for 'glMultTransposeMatrixf'.
glMultTransposeMatrixfARB
  :: MonadIO m
  => Ptr GLfloat -- ^ @m@ pointing to @16@ elements of type @GLfloat@.
  -> m ()
glMultTransposeMatrixfARB v1 = liftIO $ dyn41 ptr_glMultTransposeMatrixfARB v1

{-# NOINLINE ptr_glMultTransposeMatrixfARB #-}
ptr_glMultTransposeMatrixfARB :: FunPtr (Ptr GLfloat -> IO ())
ptr_glMultTransposeMatrixfARB = unsafePerformIO $ getCommand "glMultTransposeMatrixfARB"

-- glMultTransposeMatrixxOES ---------------------------------------------------

glMultTransposeMatrixxOES
  :: MonadIO m
  => Ptr GLfixed -- ^ @m@ pointing to @16@ elements of type @GLfixed@.
  -> m ()
glMultTransposeMatrixxOES v1 = liftIO $ dyn107 ptr_glMultTransposeMatrixxOES v1

{-# NOINLINE ptr_glMultTransposeMatrixxOES #-}
ptr_glMultTransposeMatrixxOES :: FunPtr (Ptr GLfixed -> IO ())
ptr_glMultTransposeMatrixxOES = unsafePerformIO $ getCommand "glMultTransposeMatrixxOES"

-- glMultiDrawArrays -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiDrawArrays.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glMultiDrawArrays.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glMultiDrawArrays.xhtml OpenGL 4.x>.
glMultiDrawArrays
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> Ptr GLint -- ^ @first@ pointing to @COMPSIZE(count)@ elements of type @GLint@.
  -> Ptr GLsizei -- ^ @count@ pointing to @COMPSIZE(drawcount)@ elements of type @GLsizei@.
  -> GLsizei -- ^ @drawcount@.
  -> m ()
glMultiDrawArrays v1 v2 v3 v4 = liftIO $ dyn525 ptr_glMultiDrawArrays v1 v2 v3 v4

{-# NOINLINE ptr_glMultiDrawArrays #-}
ptr_glMultiDrawArrays :: FunPtr (GLenum -> Ptr GLint -> Ptr GLsizei -> GLsizei -> IO ())
ptr_glMultiDrawArrays = unsafePerformIO $ getCommand "glMultiDrawArrays"

-- glMultiDrawArraysEXT --------------------------------------------------------

-- | This command is an alias for 'glMultiDrawArrays'.
glMultiDrawArraysEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> Ptr GLint -- ^ @first@ pointing to @COMPSIZE(primcount)@ elements of type @GLint@.
  -> Ptr GLsizei -- ^ @count@ pointing to @COMPSIZE(primcount)@ elements of type @GLsizei@.
  -> GLsizei -- ^ @primcount@.
  -> m ()
glMultiDrawArraysEXT v1 v2 v3 v4 = liftIO $ dyn525 ptr_glMultiDrawArraysEXT v1 v2 v3 v4

{-# NOINLINE ptr_glMultiDrawArraysEXT #-}
ptr_glMultiDrawArraysEXT :: FunPtr (GLenum -> Ptr GLint -> Ptr GLsizei -> GLsizei -> IO ())
ptr_glMultiDrawArraysEXT = unsafePerformIO $ getCommand "glMultiDrawArraysEXT"

-- glMultiDrawArraysIndirect ---------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glMultiDrawArraysIndirect.xhtml OpenGL 4.x>.
glMultiDrawArraysIndirect
  :: MonadIO m
  => GLenum -- ^ @mode@.
  -> Ptr a -- ^ @indirect@ pointing to @COMPSIZE(drawcount,stride)@ elements of type @a@.
  -> GLsizei -- ^ @drawcount@.
  -> GLsizei -- ^ @stride@.
  -> m ()
glMultiDrawArraysIndirect v1 v2 v3 v4 = liftIO $ dyn526 ptr_glMultiDrawArraysIndirect v1 v2 v3 v4

{-# NOINLINE ptr_glMultiDrawArraysIndirect #-}
ptr_glMultiDrawArraysIndirect :: FunPtr (GLenum -> Ptr a -> GLsizei -> GLsizei -> IO ())
ptr_glMultiDrawArraysIndirect = unsafePerformIO $ getCommand "glMultiDrawArraysIndirect"

-- glMultiDrawArraysIndirectAMD ------------------------------------------------

-- | This command is an alias for 'glMultiDrawArraysIndirect'.
glMultiDrawArraysIndirectAMD
  :: MonadIO m
  => GLenum -- ^ @mode@.
  -> Ptr a -- ^ @indirect@.
  -> GLsizei -- ^ @primcount@.
  -> GLsizei -- ^ @stride@.
  -> m ()
glMultiDrawArraysIndirectAMD v1 v2 v3 v4 = liftIO $ dyn526 ptr_glMultiDrawArraysIndirectAMD v1 v2 v3 v4

{-# NOINLINE ptr_glMultiDrawArraysIndirectAMD #-}
ptr_glMultiDrawArraysIndirectAMD :: FunPtr (GLenum -> Ptr a -> GLsizei -> GLsizei -> IO ())
ptr_glMultiDrawArraysIndirectAMD = unsafePerformIO $ getCommand "glMultiDrawArraysIndirectAMD"

-- glMultiDrawArraysIndirectBindlessCountNV ------------------------------------

glMultiDrawArraysIndirectBindlessCountNV
  :: MonadIO m
  => GLenum -- ^ @mode@.
  -> Ptr a -- ^ @indirect@.
  -> GLsizei -- ^ @drawCount@.
  -> GLsizei -- ^ @maxDrawCount@.
  -> GLsizei -- ^ @stride@.
  -> GLint -- ^ @vertexBufferCount@.
  -> m ()
glMultiDrawArraysIndirectBindlessCountNV v1 v2 v3 v4 v5 v6 = liftIO $ dyn527 ptr_glMultiDrawArraysIndirectBindlessCountNV v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glMultiDrawArraysIndirectBindlessCountNV #-}
ptr_glMultiDrawArraysIndirectBindlessCountNV :: FunPtr (GLenum -> Ptr a -> GLsizei -> GLsizei -> GLsizei -> GLint -> IO ())
ptr_glMultiDrawArraysIndirectBindlessCountNV = unsafePerformIO $ getCommand "glMultiDrawArraysIndirectBindlessCountNV"

-- glMultiDrawArraysIndirectBindlessNV -----------------------------------------

glMultiDrawArraysIndirectBindlessNV
  :: MonadIO m
  => GLenum -- ^ @mode@.
  -> Ptr a -- ^ @indirect@.
  -> GLsizei -- ^ @drawCount@.
  -> GLsizei -- ^ @stride@.
  -> GLint -- ^ @vertexBufferCount@.
  -> m ()
glMultiDrawArraysIndirectBindlessNV v1 v2 v3 v4 v5 = liftIO $ dyn528 ptr_glMultiDrawArraysIndirectBindlessNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glMultiDrawArraysIndirectBindlessNV #-}
ptr_glMultiDrawArraysIndirectBindlessNV :: FunPtr (GLenum -> Ptr a -> GLsizei -> GLsizei -> GLint -> IO ())
ptr_glMultiDrawArraysIndirectBindlessNV = unsafePerformIO $ getCommand "glMultiDrawArraysIndirectBindlessNV"

-- glMultiDrawArraysIndirectCountARB -------------------------------------------

glMultiDrawArraysIndirectCountARB
  :: MonadIO m
  => GLenum -- ^ @mode@.
  -> GLintptr -- ^ @indirect@.
  -> GLintptr -- ^ @drawcount@.
  -> GLsizei -- ^ @maxdrawcount@.
  -> GLsizei -- ^ @stride@.
  -> m ()
glMultiDrawArraysIndirectCountARB v1 v2 v3 v4 v5 = liftIO $ dyn529 ptr_glMultiDrawArraysIndirectCountARB v1 v2 v3 v4 v5

{-# NOINLINE ptr_glMultiDrawArraysIndirectCountARB #-}
ptr_glMultiDrawArraysIndirectCountARB :: FunPtr (GLenum -> GLintptr -> GLintptr -> GLsizei -> GLsizei -> IO ())
ptr_glMultiDrawArraysIndirectCountARB = unsafePerformIO $ getCommand "glMultiDrawArraysIndirectCountARB"

-- glMultiDrawArraysIndirectEXT ------------------------------------------------

-- | This command is an alias for 'glMultiDrawArraysIndirect'.
glMultiDrawArraysIndirectEXT
  :: MonadIO m
  => GLenum -- ^ @mode@.
  -> Ptr a -- ^ @indirect@ pointing to @COMPSIZE(drawcount,stride)@ elements of type @a@.
  -> GLsizei -- ^ @drawcount@.
  -> GLsizei -- ^ @stride@.
  -> m ()
glMultiDrawArraysIndirectEXT v1 v2 v3 v4 = liftIO $ dyn526 ptr_glMultiDrawArraysIndirectEXT v1 v2 v3 v4

{-# NOINLINE ptr_glMultiDrawArraysIndirectEXT #-}
ptr_glMultiDrawArraysIndirectEXT :: FunPtr (GLenum -> Ptr a -> GLsizei -> GLsizei -> IO ())
ptr_glMultiDrawArraysIndirectEXT = unsafePerformIO $ getCommand "glMultiDrawArraysIndirectEXT"

-- glMultiDrawElementArrayAPPLE ------------------------------------------------

glMultiDrawElementArrayAPPLE
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> Ptr GLint -- ^ @first@ pointing to @primcount@ elements of type @GLint@.
  -> Ptr GLsizei -- ^ @count@ pointing to @primcount@ elements of type @GLsizei@.
  -> GLsizei -- ^ @primcount@.
  -> m ()
glMultiDrawElementArrayAPPLE v1 v2 v3 v4 = liftIO $ dyn525 ptr_glMultiDrawElementArrayAPPLE v1 v2 v3 v4

{-# NOINLINE ptr_glMultiDrawElementArrayAPPLE #-}
ptr_glMultiDrawElementArrayAPPLE :: FunPtr (GLenum -> Ptr GLint -> Ptr GLsizei -> GLsizei -> IO ())
ptr_glMultiDrawElementArrayAPPLE = unsafePerformIO $ getCommand "glMultiDrawElementArrayAPPLE"

-- glMultiDrawElements ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiDrawElements.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glMultiDrawElements.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glMultiDrawElements.xhtml OpenGL 4.x>.
glMultiDrawElements
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> Ptr GLsizei -- ^ @count@ pointing to @COMPSIZE(drawcount)@ elements of type @GLsizei@.
  -> GLenum -- ^ @type@ of type @DrawElementsType@.
  -> Ptr (Ptr a) -- ^ @indices@ pointing to @COMPSIZE(drawcount)@ elements of type @Ptr a@.
  -> GLsizei -- ^ @drawcount@.
  -> m ()
glMultiDrawElements v1 v2 v3 v4 v5 = liftIO $ dyn530 ptr_glMultiDrawElements v1 v2 v3 v4 v5

{-# NOINLINE ptr_glMultiDrawElements #-}
ptr_glMultiDrawElements :: FunPtr (GLenum -> Ptr GLsizei -> GLenum -> Ptr (Ptr a) -> GLsizei -> IO ())
ptr_glMultiDrawElements = unsafePerformIO $ getCommand "glMultiDrawElements"

-- glMultiDrawElementsBaseVertex -----------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glMultiDrawElementsBaseVertex.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glMultiDrawElementsBaseVertex.xhtml OpenGL 4.x>.
glMultiDrawElementsBaseVertex
  :: MonadIO m
  => GLenum -- ^ @mode@.
  -> Ptr GLsizei -- ^ @count@ pointing to @COMPSIZE(drawcount)@ elements of type @GLsizei@.
  -> GLenum -- ^ @type@ of type @DrawElementsType@.
  -> Ptr (Ptr a) -- ^ @indices@ pointing to @COMPSIZE(drawcount)@ elements of type @Ptr a@.
  -> GLsizei -- ^ @drawcount@.
  -> Ptr GLint -- ^ @basevertex@ pointing to @COMPSIZE(drawcount)@ elements of type @GLint@.
  -> m ()
glMultiDrawElementsBaseVertex v1 v2 v3 v4 v5 v6 = liftIO $ dyn531 ptr_glMultiDrawElementsBaseVertex v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glMultiDrawElementsBaseVertex #-}
ptr_glMultiDrawElementsBaseVertex :: FunPtr (GLenum -> Ptr GLsizei -> GLenum -> Ptr (Ptr a) -> GLsizei -> Ptr GLint -> IO ())
ptr_glMultiDrawElementsBaseVertex = unsafePerformIO $ getCommand "glMultiDrawElementsBaseVertex"

-- glMultiDrawElementsBaseVertexEXT --------------------------------------------

-- | This command is an alias for 'glMultiDrawElementsBaseVertex'.
glMultiDrawElementsBaseVertexEXT
  :: MonadIO m
  => GLenum -- ^ @mode@.
  -> Ptr GLsizei -- ^ @count@ pointing to @COMPSIZE(drawcount)@ elements of type @GLsizei@.
  -> GLenum -- ^ @type@ of type @DrawElementsType@.
  -> Ptr (Ptr a) -- ^ @indices@ pointing to @COMPSIZE(drawcount)@ elements of type @Ptr a@.
  -> GLsizei -- ^ @primcount@.
  -> Ptr GLint -- ^ @basevertex@ pointing to @COMPSIZE(drawcount)@ elements of type @GLint@.
  -> m ()
glMultiDrawElementsBaseVertexEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn531 ptr_glMultiDrawElementsBaseVertexEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glMultiDrawElementsBaseVertexEXT #-}
ptr_glMultiDrawElementsBaseVertexEXT :: FunPtr (GLenum -> Ptr GLsizei -> GLenum -> Ptr (Ptr a) -> GLsizei -> Ptr GLint -> IO ())
ptr_glMultiDrawElementsBaseVertexEXT = unsafePerformIO $ getCommand "glMultiDrawElementsBaseVertexEXT"

-- glMultiDrawElementsBaseVertexOES --------------------------------------------

-- | This command is an alias for 'glMultiDrawElementsBaseVertex'.
glMultiDrawElementsBaseVertexOES
  :: MonadIO m
  => GLenum -- ^ @mode@.
  -> Ptr GLsizei -- ^ @count@ pointing to @COMPSIZE(drawcount)@ elements of type @GLsizei@.
  -> GLenum -- ^ @type@ of type @DrawElementsType@.
  -> Ptr (Ptr a) -- ^ @indices@ pointing to @COMPSIZE(drawcount)@ elements of type @Ptr a@.
  -> GLsizei -- ^ @primcount@.
  -> Ptr GLint -- ^ @basevertex@ pointing to @COMPSIZE(drawcount)@ elements of type @GLint@.
  -> m ()
glMultiDrawElementsBaseVertexOES v1 v2 v3 v4 v5 v6 = liftIO $ dyn531 ptr_glMultiDrawElementsBaseVertexOES v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glMultiDrawElementsBaseVertexOES #-}
ptr_glMultiDrawElementsBaseVertexOES :: FunPtr (GLenum -> Ptr GLsizei -> GLenum -> Ptr (Ptr a) -> GLsizei -> Ptr GLint -> IO ())
ptr_glMultiDrawElementsBaseVertexOES = unsafePerformIO $ getCommand "glMultiDrawElementsBaseVertexOES"

-- glMultiDrawElementsEXT ------------------------------------------------------

-- | This command is an alias for 'glMultiDrawElements'.
glMultiDrawElementsEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> Ptr GLsizei -- ^ @count@ pointing to @COMPSIZE(primcount)@ elements of type @GLsizei@.
  -> GLenum -- ^ @type@ of type @DrawElementsType@.
  -> Ptr (Ptr a) -- ^ @indices@ pointing to @COMPSIZE(primcount)@ elements of type @Ptr a@.
  -> GLsizei -- ^ @primcount@.
  -> m ()
glMultiDrawElementsEXT v1 v2 v3 v4 v5 = liftIO $ dyn530 ptr_glMultiDrawElementsEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glMultiDrawElementsEXT #-}
ptr_glMultiDrawElementsEXT :: FunPtr (GLenum -> Ptr GLsizei -> GLenum -> Ptr (Ptr a) -> GLsizei -> IO ())
ptr_glMultiDrawElementsEXT = unsafePerformIO $ getCommand "glMultiDrawElementsEXT"

-- glMultiDrawElementsIndirect -------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glMultiDrawElementsIndirect.xhtml OpenGL 4.x>.
glMultiDrawElementsIndirect
  :: MonadIO m
  => GLenum -- ^ @mode@.
  -> GLenum -- ^ @type@.
  -> Ptr a -- ^ @indirect@ pointing to @COMPSIZE(drawcount,stride)@ elements of type @a@.
  -> GLsizei -- ^ @drawcount@.
  -> GLsizei -- ^ @stride@.
  -> m ()
glMultiDrawElementsIndirect v1 v2 v3 v4 v5 = liftIO $ dyn532 ptr_glMultiDrawElementsIndirect v1 v2 v3 v4 v5

{-# NOINLINE ptr_glMultiDrawElementsIndirect #-}
ptr_glMultiDrawElementsIndirect :: FunPtr (GLenum -> GLenum -> Ptr a -> GLsizei -> GLsizei -> IO ())
ptr_glMultiDrawElementsIndirect = unsafePerformIO $ getCommand "glMultiDrawElementsIndirect"

