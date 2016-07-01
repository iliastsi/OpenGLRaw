--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F17
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

module Graphics.GL.Functions.F17 (
  glEvalCoord2fv,
  glEvalCoord2xOES,
  glEvalCoord2xvOES,
  glEvalMapsNV,
  glEvalMesh1,
  glEvalMesh2,
  glEvalPoint1,
  glEvalPoint2,
  glEvaluateDepthValuesARB,
  glExecuteProgramNV,
  glExtGetBufferPointervQCOM,
  glExtGetBuffersQCOM,
  glExtGetFramebuffersQCOM,
  glExtGetProgramBinarySourceQCOM,
  glExtGetProgramsQCOM,
  glExtGetRenderbuffersQCOM,
  glExtGetShadersQCOM,
  glExtGetTexLevelParameterivQCOM,
  glExtGetTexSubImageQCOM,
  glExtGetTexturesQCOM,
  glExtIsProgramBinaryQCOM,
  glExtTexObjectStateOverrideiQCOM,
  glExtractComponentEXT,
  glFeedbackBuffer,
  glFeedbackBufferxOES,
  glFenceSync,
  glFenceSyncAPPLE,
  glFinalCombinerInputNV,
  glFinish,
  glFinishAsyncSGIX,
  glFinishFenceAPPLE,
  glFinishFenceNV,
  glFinishObjectAPPLE,
  glFinishTextureSUNX,
  glFlush,
  glFlushMappedBufferRange,
  glFlushMappedBufferRangeAPPLE,
  glFlushMappedBufferRangeEXT,
  glFlushMappedNamedBufferRange,
  glFlushMappedNamedBufferRangeEXT
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

-- glEvalCoord2fv --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glEvalCoord.xml OpenGL 2.x>.
glEvalCoord2fv
  :: MonadIO m
  => Ptr GLfloat -- ^ @u@ pointing to @2@ elements of type @CoordF@.
  -> m ()
glEvalCoord2fv v1 = liftIO $ dyn41 ptr_glEvalCoord2fv v1

{-# NOINLINE ptr_glEvalCoord2fv #-}
ptr_glEvalCoord2fv :: FunPtr (Ptr GLfloat -> IO ())
ptr_glEvalCoord2fv = unsafePerformIO $ getCommand "glEvalCoord2fv"

-- glEvalCoord2xOES ------------------------------------------------------------

glEvalCoord2xOES
  :: MonadIO m
  => GLfixed -- ^ @u@.
  -> GLfixed -- ^ @v@.
  -> m ()
glEvalCoord2xOES v1 v2 = liftIO $ dyn224 ptr_glEvalCoord2xOES v1 v2

{-# NOINLINE ptr_glEvalCoord2xOES #-}
ptr_glEvalCoord2xOES :: FunPtr (GLfixed -> GLfixed -> IO ())
ptr_glEvalCoord2xOES = unsafePerformIO $ getCommand "glEvalCoord2xOES"

-- glEvalCoord2xvOES -----------------------------------------------------------

glEvalCoord2xvOES
  :: MonadIO m
  => Ptr GLfixed -- ^ @coords@ pointing to @2@ elements of type @GLfixed@.
  -> m ()
glEvalCoord2xvOES v1 = liftIO $ dyn107 ptr_glEvalCoord2xvOES v1

{-# NOINLINE ptr_glEvalCoord2xvOES #-}
ptr_glEvalCoord2xvOES :: FunPtr (Ptr GLfixed -> IO ())
ptr_glEvalCoord2xvOES = unsafePerformIO $ getCommand "glEvalCoord2xvOES"

-- glEvalMapsNV ----------------------------------------------------------------

glEvalMapsNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type @EvalTargetNV@.
  -> GLenum -- ^ @mode@ of type @EvalMapsModeNV@.
  -> m ()
glEvalMapsNV v1 v2 = liftIO $ dyn51 ptr_glEvalMapsNV v1 v2

{-# NOINLINE ptr_glEvalMapsNV #-}
ptr_glEvalMapsNV :: FunPtr (GLenum -> GLenum -> IO ())
ptr_glEvalMapsNV = unsafePerformIO $ getCommand "glEvalMapsNV"

-- glEvalMesh1 -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glEvalMesh.xml OpenGL 2.x>.
glEvalMesh1
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [MeshMode1](Graphics-GL-Groups.html#MeshMode1).
  -> GLint -- ^ @i1@ of type @CheckedInt32@.
  -> GLint -- ^ @i2@ of type @CheckedInt32@.
  -> m ()
glEvalMesh1 v1 v2 v3 = liftIO $ dyn264 ptr_glEvalMesh1 v1 v2 v3

{-# NOINLINE ptr_glEvalMesh1 #-}
ptr_glEvalMesh1 :: FunPtr (GLenum -> GLint -> GLint -> IO ())
ptr_glEvalMesh1 = unsafePerformIO $ getCommand "glEvalMesh1"

-- glEvalMesh2 -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glEvalMesh.xml OpenGL 2.x>.
glEvalMesh2
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [MeshMode2](Graphics-GL-Groups.html#MeshMode2).
  -> GLint -- ^ @i1@ of type @CheckedInt32@.
  -> GLint -- ^ @i2@ of type @CheckedInt32@.
  -> GLint -- ^ @j1@ of type @CheckedInt32@.
  -> GLint -- ^ @j2@ of type @CheckedInt32@.
  -> m ()
glEvalMesh2 v1 v2 v3 v4 v5 = liftIO $ dyn265 ptr_glEvalMesh2 v1 v2 v3 v4 v5

{-# NOINLINE ptr_glEvalMesh2 #-}
ptr_glEvalMesh2 :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> IO ())
ptr_glEvalMesh2 = unsafePerformIO $ getCommand "glEvalMesh2"

-- glEvalPoint1 ----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glEvalPoint.xml OpenGL 2.x>.
glEvalPoint1
  :: MonadIO m
  => GLint -- ^ @i@.
  -> m ()
glEvalPoint1 v1 = liftIO $ dyn12 ptr_glEvalPoint1 v1

{-# NOINLINE ptr_glEvalPoint1 #-}
ptr_glEvalPoint1 :: FunPtr (GLint -> IO ())
ptr_glEvalPoint1 = unsafePerformIO $ getCommand "glEvalPoint1"

-- glEvalPoint2 ----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glEvalPoint.xml OpenGL 2.x>.
glEvalPoint2
  :: MonadIO m
  => GLint -- ^ @i@ of type @CheckedInt32@.
  -> GLint -- ^ @j@ of type @CheckedInt32@.
  -> m ()
glEvalPoint2 v1 v2 = liftIO $ dyn266 ptr_glEvalPoint2 v1 v2

{-# NOINLINE ptr_glEvalPoint2 #-}
ptr_glEvalPoint2 :: FunPtr (GLint -> GLint -> IO ())
ptr_glEvalPoint2 = unsafePerformIO $ getCommand "glEvalPoint2"

-- glEvaluateDepthValuesARB ----------------------------------------------------

glEvaluateDepthValuesARB
  :: MonadIO m
  => m ()
glEvaluateDepthValuesARB = liftIO $ dyn10 ptr_glEvaluateDepthValuesARB

{-# NOINLINE ptr_glEvaluateDepthValuesARB #-}
ptr_glEvaluateDepthValuesARB :: FunPtr (IO ())
ptr_glEvaluateDepthValuesARB = unsafePerformIO $ getCommand "glEvaluateDepthValuesARB"

-- glExecuteProgramNV ----------------------------------------------------------

glExecuteProgramNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type @VertexAttribEnumNV@.
  -> GLuint -- ^ @id@.
  -> Ptr GLfloat -- ^ @params@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glExecuteProgramNV v1 v2 v3 = liftIO $ dyn267 ptr_glExecuteProgramNV v1 v2 v3

{-# NOINLINE ptr_glExecuteProgramNV #-}
ptr_glExecuteProgramNV :: FunPtr (GLenum -> GLuint -> Ptr GLfloat -> IO ())
ptr_glExecuteProgramNV = unsafePerformIO $ getCommand "glExecuteProgramNV"

-- glExtGetBufferPointervQCOM --------------------------------------------------

glExtGetBufferPointervQCOM
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> Ptr (Ptr a) -- ^ @params@.
  -> m ()
glExtGetBufferPointervQCOM v1 v2 = liftIO $ dyn268 ptr_glExtGetBufferPointervQCOM v1 v2

{-# NOINLINE ptr_glExtGetBufferPointervQCOM #-}
ptr_glExtGetBufferPointervQCOM :: FunPtr (GLenum -> Ptr (Ptr a) -> IO ())
ptr_glExtGetBufferPointervQCOM = unsafePerformIO $ getCommand "glExtGetBufferPointervQCOM"

-- glExtGetBuffersQCOM ---------------------------------------------------------

glExtGetBuffersQCOM
  :: MonadIO m
  => Ptr GLuint -- ^ @buffers@ pointing to @maxBuffers@ elements of type @GLuint@.
  -> GLint -- ^ @maxBuffers@.
  -> Ptr GLint -- ^ @numBuffers@ pointing to @1@ element of type @GLint@.
  -> m ()
glExtGetBuffersQCOM v1 v2 v3 = liftIO $ dyn269 ptr_glExtGetBuffersQCOM v1 v2 v3

{-# NOINLINE ptr_glExtGetBuffersQCOM #-}
ptr_glExtGetBuffersQCOM :: FunPtr (Ptr GLuint -> GLint -> Ptr GLint -> IO ())
ptr_glExtGetBuffersQCOM = unsafePerformIO $ getCommand "glExtGetBuffersQCOM"

-- glExtGetFramebuffersQCOM ----------------------------------------------------

glExtGetFramebuffersQCOM
  :: MonadIO m
  => Ptr GLuint -- ^ @framebuffers@ pointing to @maxFramebuffers@ elements of type @GLuint@.
  -> GLint -- ^ @maxFramebuffers@.
  -> Ptr GLint -- ^ @numFramebuffers@ pointing to @1@ element of type @GLint@.
  -> m ()
glExtGetFramebuffersQCOM v1 v2 v3 = liftIO $ dyn269 ptr_glExtGetFramebuffersQCOM v1 v2 v3

{-# NOINLINE ptr_glExtGetFramebuffersQCOM #-}
ptr_glExtGetFramebuffersQCOM :: FunPtr (Ptr GLuint -> GLint -> Ptr GLint -> IO ())
ptr_glExtGetFramebuffersQCOM = unsafePerformIO $ getCommand "glExtGetFramebuffersQCOM"

-- glExtGetProgramBinarySourceQCOM ---------------------------------------------

glExtGetProgramBinarySourceQCOM
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @shadertype@.
  -> Ptr GLchar -- ^ @source@.
  -> Ptr GLint -- ^ @length@.
  -> m ()
glExtGetProgramBinarySourceQCOM v1 v2 v3 v4 = liftIO $ dyn270 ptr_glExtGetProgramBinarySourceQCOM v1 v2 v3 v4

{-# NOINLINE ptr_glExtGetProgramBinarySourceQCOM #-}
ptr_glExtGetProgramBinarySourceQCOM :: FunPtr (GLuint -> GLenum -> Ptr GLchar -> Ptr GLint -> IO ())
ptr_glExtGetProgramBinarySourceQCOM = unsafePerformIO $ getCommand "glExtGetProgramBinarySourceQCOM"

-- glExtGetProgramsQCOM --------------------------------------------------------

glExtGetProgramsQCOM
  :: MonadIO m
  => Ptr GLuint -- ^ @programs@ pointing to @maxPrograms@ elements of type @GLuint@.
  -> GLint -- ^ @maxPrograms@.
  -> Ptr GLint -- ^ @numPrograms@ pointing to @1@ element of type @GLint@.
  -> m ()
glExtGetProgramsQCOM v1 v2 v3 = liftIO $ dyn269 ptr_glExtGetProgramsQCOM v1 v2 v3

{-# NOINLINE ptr_glExtGetProgramsQCOM #-}
ptr_glExtGetProgramsQCOM :: FunPtr (Ptr GLuint -> GLint -> Ptr GLint -> IO ())
ptr_glExtGetProgramsQCOM = unsafePerformIO $ getCommand "glExtGetProgramsQCOM"

-- glExtGetRenderbuffersQCOM ---------------------------------------------------

glExtGetRenderbuffersQCOM
  :: MonadIO m
  => Ptr GLuint -- ^ @renderbuffers@ pointing to @maxRenderbuffers@ elements of type @GLuint@.
  -> GLint -- ^ @maxRenderbuffers@.
  -> Ptr GLint -- ^ @numRenderbuffers@ pointing to @1@ element of type @GLint@.
  -> m ()
glExtGetRenderbuffersQCOM v1 v2 v3 = liftIO $ dyn269 ptr_glExtGetRenderbuffersQCOM v1 v2 v3

{-# NOINLINE ptr_glExtGetRenderbuffersQCOM #-}
ptr_glExtGetRenderbuffersQCOM :: FunPtr (Ptr GLuint -> GLint -> Ptr GLint -> IO ())
ptr_glExtGetRenderbuffersQCOM = unsafePerformIO $ getCommand "glExtGetRenderbuffersQCOM"

-- glExtGetShadersQCOM ---------------------------------------------------------

glExtGetShadersQCOM
  :: MonadIO m
  => Ptr GLuint -- ^ @shaders@ pointing to @maxShaders@ elements of type @GLuint@.
  -> GLint -- ^ @maxShaders@.
  -> Ptr GLint -- ^ @numShaders@ pointing to @1@ element of type @GLint@.
  -> m ()
glExtGetShadersQCOM v1 v2 v3 = liftIO $ dyn269 ptr_glExtGetShadersQCOM v1 v2 v3

{-# NOINLINE ptr_glExtGetShadersQCOM #-}
ptr_glExtGetShadersQCOM :: FunPtr (Ptr GLuint -> GLint -> Ptr GLint -> IO ())
ptr_glExtGetShadersQCOM = unsafePerformIO $ getCommand "glExtGetShadersQCOM"

-- glExtGetTexLevelParameterivQCOM ---------------------------------------------

glExtGetTexLevelParameterivQCOM
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLenum -- ^ @face@.
  -> GLint -- ^ @level@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @params@.
  -> m ()
glExtGetTexLevelParameterivQCOM v1 v2 v3 v4 v5 = liftIO $ dyn271 ptr_glExtGetTexLevelParameterivQCOM v1 v2 v3 v4 v5

{-# NOINLINE ptr_glExtGetTexLevelParameterivQCOM #-}
ptr_glExtGetTexLevelParameterivQCOM :: FunPtr (GLuint -> GLenum -> GLint -> GLenum -> Ptr GLint -> IO ())
ptr_glExtGetTexLevelParameterivQCOM = unsafePerformIO $ getCommand "glExtGetTexLevelParameterivQCOM"

-- glExtGetTexSubImageQCOM -----------------------------------------------------

glExtGetTexSubImageQCOM
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLint -- ^ @level@.
  -> GLint -- ^ @xoffset@.
  -> GLint -- ^ @yoffset@.
  -> GLint -- ^ @zoffset@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLenum -- ^ @format@.
  -> GLenum -- ^ @type@.
  -> Ptr a -- ^ @texels@.
  -> m ()
glExtGetTexSubImageQCOM v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 = liftIO $ dyn272 ptr_glExtGetTexSubImageQCOM v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11

{-# NOINLINE ptr_glExtGetTexSubImageQCOM #-}
ptr_glExtGetTexSubImageQCOM :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glExtGetTexSubImageQCOM = unsafePerformIO $ getCommand "glExtGetTexSubImageQCOM"

-- glExtGetTexturesQCOM --------------------------------------------------------

glExtGetTexturesQCOM
  :: MonadIO m
  => Ptr GLuint -- ^ @textures@.
  -> GLint -- ^ @maxTextures@.
  -> Ptr GLint -- ^ @numTextures@.
  -> m ()
glExtGetTexturesQCOM v1 v2 v3 = liftIO $ dyn269 ptr_glExtGetTexturesQCOM v1 v2 v3

{-# NOINLINE ptr_glExtGetTexturesQCOM #-}
ptr_glExtGetTexturesQCOM :: FunPtr (Ptr GLuint -> GLint -> Ptr GLint -> IO ())
ptr_glExtGetTexturesQCOM = unsafePerformIO $ getCommand "glExtGetTexturesQCOM"

-- glExtIsProgramBinaryQCOM ----------------------------------------------------

glExtIsProgramBinaryQCOM
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> m GLboolean
glExtIsProgramBinaryQCOM v1 = liftIO $ dyn273 ptr_glExtIsProgramBinaryQCOM v1

{-# NOINLINE ptr_glExtIsProgramBinaryQCOM #-}
ptr_glExtIsProgramBinaryQCOM :: FunPtr (GLuint -> IO GLboolean)
ptr_glExtIsProgramBinaryQCOM = unsafePerformIO $ getCommand "glExtIsProgramBinaryQCOM"

-- glExtTexObjectStateOverrideiQCOM --------------------------------------------

glExtTexObjectStateOverrideiQCOM
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @pname@.
  -> GLint -- ^ @param@.
  -> m ()
glExtTexObjectStateOverrideiQCOM v1 v2 v3 = liftIO $ dyn62 ptr_glExtTexObjectStateOverrideiQCOM v1 v2 v3

{-# NOINLINE ptr_glExtTexObjectStateOverrideiQCOM #-}
ptr_glExtTexObjectStateOverrideiQCOM :: FunPtr (GLenum -> GLenum -> GLint -> IO ())
ptr_glExtTexObjectStateOverrideiQCOM = unsafePerformIO $ getCommand "glExtTexObjectStateOverrideiQCOM"

-- glExtractComponentEXT -------------------------------------------------------

glExtractComponentEXT
  :: MonadIO m
  => GLuint -- ^ @res@.
  -> GLuint -- ^ @src@.
  -> GLuint -- ^ @num@.
  -> m ()
glExtractComponentEXT v1 v2 v3 = liftIO $ dyn102 ptr_glExtractComponentEXT v1 v2 v3

{-# NOINLINE ptr_glExtractComponentEXT #-}
ptr_glExtractComponentEXT :: FunPtr (GLuint -> GLuint -> GLuint -> IO ())
ptr_glExtractComponentEXT = unsafePerformIO $ getCommand "glExtractComponentEXT"

-- glFeedbackBuffer ------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glFeedbackBuffer.xml OpenGL 2.x>.
glFeedbackBuffer
  :: MonadIO m
  => GLsizei -- ^ @size@.
  -> GLenum -- ^ @type@ of type [FeedbackType](Graphics-GL-Groups.html#FeedbackType).
  -> Ptr GLfloat -- ^ @buffer@ pointing to @size@ elements of type @FeedbackElement@.
  -> m ()
glFeedbackBuffer v1 v2 v3 = liftIO $ dyn274 ptr_glFeedbackBuffer v1 v2 v3

{-# NOINLINE ptr_glFeedbackBuffer #-}
ptr_glFeedbackBuffer :: FunPtr (GLsizei -> GLenum -> Ptr GLfloat -> IO ())
ptr_glFeedbackBuffer = unsafePerformIO $ getCommand "glFeedbackBuffer"

-- glFeedbackBufferxOES --------------------------------------------------------

glFeedbackBufferxOES
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> GLenum -- ^ @type@.
  -> Ptr GLfixed -- ^ @buffer@ pointing to @n@ elements of type @GLfixed@.
  -> m ()
glFeedbackBufferxOES v1 v2 v3 = liftIO $ dyn275 ptr_glFeedbackBufferxOES v1 v2 v3

{-# NOINLINE ptr_glFeedbackBufferxOES #-}
ptr_glFeedbackBufferxOES :: FunPtr (GLsizei -> GLenum -> Ptr GLfixed -> IO ())
ptr_glFeedbackBufferxOES = unsafePerformIO $ getCommand "glFeedbackBufferxOES"

-- glFenceSync -----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glFenceSync.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glFenceSync.xhtml OpenGL 4.x>.
glFenceSync
  :: MonadIO m
  => GLenum -- ^ @condition@.
  -> GLbitfield -- ^ @flags@.
  -> m GLsync -- ^ of type @sync@.
glFenceSync v1 v2 = liftIO $ dyn276 ptr_glFenceSync v1 v2

{-# NOINLINE ptr_glFenceSync #-}
ptr_glFenceSync :: FunPtr (GLenum -> GLbitfield -> IO GLsync)
ptr_glFenceSync = unsafePerformIO $ getCommand "glFenceSync"

-- glFenceSyncAPPLE ------------------------------------------------------------

-- | This command is an alias for 'glFenceSync'.
glFenceSyncAPPLE
  :: MonadIO m
  => GLenum -- ^ @condition@.
  -> GLbitfield -- ^ @flags@.
  -> m GLsync
glFenceSyncAPPLE v1 v2 = liftIO $ dyn276 ptr_glFenceSyncAPPLE v1 v2

{-# NOINLINE ptr_glFenceSyncAPPLE #-}
ptr_glFenceSyncAPPLE :: FunPtr (GLenum -> GLbitfield -> IO GLsync)
ptr_glFenceSyncAPPLE = unsafePerformIO $ getCommand "glFenceSyncAPPLE"

-- glFinalCombinerInputNV ------------------------------------------------------

glFinalCombinerInputNV
  :: MonadIO m
  => GLenum -- ^ @variable@ of type @CombinerVariableNV@.
  -> GLenum -- ^ @input@ of type @CombinerRegisterNV@.
  -> GLenum -- ^ @mapping@ of type @CombinerMappingNV@.
  -> GLenum -- ^ @componentUsage@ of type @CombinerComponentUsageNV@.
  -> m ()
glFinalCombinerInputNV v1 v2 v3 v4 = liftIO $ dyn53 ptr_glFinalCombinerInputNV v1 v2 v3 v4

{-# NOINLINE ptr_glFinalCombinerInputNV #-}
ptr_glFinalCombinerInputNV :: FunPtr (GLenum -> GLenum -> GLenum -> GLenum -> IO ())
ptr_glFinalCombinerInputNV = unsafePerformIO $ getCommand "glFinalCombinerInputNV"

-- glFinish --------------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glFinish.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glFinish.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glFinish.xhtml OpenGL 4.x>.
glFinish
  :: MonadIO m
  => m ()
glFinish = liftIO $ dyn10 ptr_glFinish

{-# NOINLINE ptr_glFinish #-}
ptr_glFinish :: FunPtr (IO ())
ptr_glFinish = unsafePerformIO $ getCommand "glFinish"

-- glFinishAsyncSGIX -----------------------------------------------------------

glFinishAsyncSGIX
  :: MonadIO m
  => Ptr GLuint -- ^ @markerp@ pointing to @1@ element of type @GLuint@.
  -> m GLint
glFinishAsyncSGIX v1 = liftIO $ dyn277 ptr_glFinishAsyncSGIX v1

{-# NOINLINE ptr_glFinishAsyncSGIX #-}
ptr_glFinishAsyncSGIX :: FunPtr (Ptr GLuint -> IO GLint)
ptr_glFinishAsyncSGIX = unsafePerformIO $ getCommand "glFinishAsyncSGIX"

-- glFinishFenceAPPLE ----------------------------------------------------------

glFinishFenceAPPLE
  :: MonadIO m
  => GLuint -- ^ @fence@ of type @FenceNV@.
  -> m ()
glFinishFenceAPPLE v1 = liftIO $ dyn2 ptr_glFinishFenceAPPLE v1

{-# NOINLINE ptr_glFinishFenceAPPLE #-}
ptr_glFinishFenceAPPLE :: FunPtr (GLuint -> IO ())
ptr_glFinishFenceAPPLE = unsafePerformIO $ getCommand "glFinishFenceAPPLE"

-- glFinishFenceNV -------------------------------------------------------------

glFinishFenceNV
  :: MonadIO m
  => GLuint -- ^ @fence@ of type @FenceNV@.
  -> m ()
glFinishFenceNV v1 = liftIO $ dyn2 ptr_glFinishFenceNV v1

{-# NOINLINE ptr_glFinishFenceNV #-}
ptr_glFinishFenceNV :: FunPtr (GLuint -> IO ())
ptr_glFinishFenceNV = unsafePerformIO $ getCommand "glFinishFenceNV"

-- glFinishObjectAPPLE ---------------------------------------------------------

glFinishObjectAPPLE
  :: MonadIO m
  => GLenum -- ^ @object@ of type @ObjectTypeAPPLE@.
  -> GLint -- ^ @name@.
  -> m ()
glFinishObjectAPPLE v1 v2 = liftIO $ dyn55 ptr_glFinishObjectAPPLE v1 v2

{-# NOINLINE ptr_glFinishObjectAPPLE #-}
ptr_glFinishObjectAPPLE :: FunPtr (GLenum -> GLint -> IO ())
ptr_glFinishObjectAPPLE = unsafePerformIO $ getCommand "glFinishObjectAPPLE"

-- glFinishTextureSUNX ---------------------------------------------------------

glFinishTextureSUNX
  :: MonadIO m
  => m ()
glFinishTextureSUNX = liftIO $ dyn10 ptr_glFinishTextureSUNX

{-# NOINLINE ptr_glFinishTextureSUNX #-}
ptr_glFinishTextureSUNX :: FunPtr (IO ())
ptr_glFinishTextureSUNX = unsafePerformIO $ getCommand "glFinishTextureSUNX"

-- glFlush ---------------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glFlush.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glFlush.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glFlush.xhtml OpenGL 4.x>.
glFlush
  :: MonadIO m
  => m ()
glFlush = liftIO $ dyn10 ptr_glFlush

{-# NOINLINE ptr_glFlush #-}
ptr_glFlush :: FunPtr (IO ())
ptr_glFlush = unsafePerformIO $ getCommand "glFlush"

-- glFlushMappedBufferRange ----------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glFlushMappedBufferRange.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glFlushMappedBufferRange.xhtml OpenGL 4.x>.
glFlushMappedBufferRange
  :: MonadIO m
  => GLenum -- ^ @target@ of type @BufferTargetARB@.
  -> GLintptr -- ^ @offset@ of type @BufferOffset@.
  -> GLsizeiptr -- ^ @length@ of type @BufferSize@.
  -> m ()
glFlushMappedBufferRange v1 v2 v3 = liftIO $ dyn278 ptr_glFlushMappedBufferRange v1 v2 v3

{-# NOINLINE ptr_glFlushMappedBufferRange #-}
ptr_glFlushMappedBufferRange :: FunPtr (GLenum -> GLintptr -> GLsizeiptr -> IO ())
ptr_glFlushMappedBufferRange = unsafePerformIO $ getCommand "glFlushMappedBufferRange"

-- glFlushMappedBufferRangeAPPLE -----------------------------------------------

-- | This command is an alias for 'glFlushMappedBufferRange'.
glFlushMappedBufferRangeAPPLE
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLintptr -- ^ @offset@ of type @BufferOffset@.
  -> GLsizeiptr -- ^ @size@ of type @BufferSize@.
  -> m ()
glFlushMappedBufferRangeAPPLE v1 v2 v3 = liftIO $ dyn278 ptr_glFlushMappedBufferRangeAPPLE v1 v2 v3

{-# NOINLINE ptr_glFlushMappedBufferRangeAPPLE #-}
ptr_glFlushMappedBufferRangeAPPLE :: FunPtr (GLenum -> GLintptr -> GLsizeiptr -> IO ())
ptr_glFlushMappedBufferRangeAPPLE = unsafePerformIO $ getCommand "glFlushMappedBufferRangeAPPLE"

-- glFlushMappedBufferRangeEXT -------------------------------------------------

-- | This command is an alias for 'glFlushMappedBufferRange'.
glFlushMappedBufferRangeEXT
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLintptr -- ^ @offset@.
  -> GLsizeiptr -- ^ @length@.
  -> m ()
glFlushMappedBufferRangeEXT v1 v2 v3 = liftIO $ dyn278 ptr_glFlushMappedBufferRangeEXT v1 v2 v3

{-# NOINLINE ptr_glFlushMappedBufferRangeEXT #-}
ptr_glFlushMappedBufferRangeEXT :: FunPtr (GLenum -> GLintptr -> GLsizeiptr -> IO ())
ptr_glFlushMappedBufferRangeEXT = unsafePerformIO $ getCommand "glFlushMappedBufferRangeEXT"

-- glFlushMappedNamedBufferRange -----------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glFlushMappedBufferRange.xhtml OpenGL 4.x>.
glFlushMappedNamedBufferRange
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLintptr -- ^ @offset@.
  -> GLsizeiptr -- ^ @length@ of type @BufferSize@.
  -> m ()
glFlushMappedNamedBufferRange v1 v2 v3 = liftIO $ dyn279 ptr_glFlushMappedNamedBufferRange v1 v2 v3

{-# NOINLINE ptr_glFlushMappedNamedBufferRange #-}
ptr_glFlushMappedNamedBufferRange :: FunPtr (GLuint -> GLintptr -> GLsizeiptr -> IO ())
ptr_glFlushMappedNamedBufferRange = unsafePerformIO $ getCommand "glFlushMappedNamedBufferRange"

-- glFlushMappedNamedBufferRangeEXT --------------------------------------------

glFlushMappedNamedBufferRangeEXT
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLintptr -- ^ @offset@.
  -> GLsizeiptr -- ^ @length@.
  -> m ()
glFlushMappedNamedBufferRangeEXT v1 v2 v3 = liftIO $ dyn279 ptr_glFlushMappedNamedBufferRangeEXT v1 v2 v3

{-# NOINLINE ptr_glFlushMappedNamedBufferRangeEXT #-}
ptr_glFlushMappedNamedBufferRangeEXT :: FunPtr (GLuint -> GLintptr -> GLsizeiptr -> IO ())
ptr_glFlushMappedNamedBufferRangeEXT = unsafePerformIO $ getCommand "glFlushMappedNamedBufferRangeEXT"

