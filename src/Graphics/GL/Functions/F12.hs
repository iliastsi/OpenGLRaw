--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F12
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

module Graphics.GL.Functions.F12 (
  glDeformationMap3dSGIX,
  glDeformationMap3fSGIX,
  glDeleteAsyncMarkersSGIX,
  glDeleteBuffers,
  glDeleteBuffersARB,
  glDeleteCommandListsNV,
  glDeleteFencesAPPLE,
  glDeleteFencesNV,
  glDeleteFragmentShaderATI,
  glDeleteFramebuffers,
  glDeleteFramebuffersEXT,
  glDeleteFramebuffersOES,
  glDeleteLists,
  glDeleteNamedStringARB,
  glDeleteNamesAMD,
  glDeleteObjectARB,
  glDeleteOcclusionQueriesNV,
  glDeletePathsNV,
  glDeletePerfMonitorsAMD,
  glDeletePerfQueryINTEL,
  glDeleteProgram,
  glDeleteProgramPipelines,
  glDeleteProgramPipelinesEXT,
  glDeleteProgramsARB,
  glDeleteProgramsNV,
  glDeleteQueries,
  glDeleteQueriesARB,
  glDeleteQueriesEXT,
  glDeleteRenderbuffers,
  glDeleteRenderbuffersEXT,
  glDeleteRenderbuffersOES,
  glDeleteSamplers,
  glDeleteShader,
  glDeleteStatesNV,
  glDeleteSync,
  glDeleteSyncAPPLE,
  glDeleteTextures,
  glDeleteTexturesEXT,
  glDeleteTransformFeedbacks,
  glDeleteTransformFeedbacksNV
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

-- glDeformationMap3dSGIX ------------------------------------------------------

glDeformationMap3dSGIX
  :: MonadIO m
  => GLenum -- ^ @target@ of type [FfdTargetSGIX](Graphics-GL-Groups.html#FfdTargetSGIX).
  -> GLdouble -- ^ @u1@ of type @CoordD@.
  -> GLdouble -- ^ @u2@ of type @CoordD@.
  -> GLint -- ^ @ustride@.
  -> GLint -- ^ @uorder@ of type @CheckedInt32@.
  -> GLdouble -- ^ @v1@ of type @CoordD@.
  -> GLdouble -- ^ @v2@ of type @CoordD@.
  -> GLint -- ^ @vstride@.
  -> GLint -- ^ @vorder@ of type @CheckedInt32@.
  -> GLdouble -- ^ @w1@ of type @CoordD@.
  -> GLdouble -- ^ @w2@ of type @CoordD@.
  -> GLint -- ^ @wstride@.
  -> GLint -- ^ @worder@ of type @CheckedInt32@.
  -> Ptr GLdouble -- ^ @points@ pointing to @COMPSIZE(target,ustride,uorder,vstride,vorder,wstride,worder)@ elements of type @CoordD@.
  -> m ()
glDeformationMap3dSGIX v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 = liftIO $ dyn210 ptr_glDeformationMap3dSGIX v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14

{-# NOINLINE ptr_glDeformationMap3dSGIX #-}
ptr_glDeformationMap3dSGIX :: FunPtr (GLenum -> GLdouble -> GLdouble -> GLint -> GLint -> GLdouble -> GLdouble -> GLint -> GLint -> GLdouble -> GLdouble -> GLint -> GLint -> Ptr GLdouble -> IO ())
ptr_glDeformationMap3dSGIX = unsafePerformIO $ getCommand "glDeformationMap3dSGIX"

-- glDeformationMap3fSGIX ------------------------------------------------------

glDeformationMap3fSGIX
  :: MonadIO m
  => GLenum -- ^ @target@ of type [FfdTargetSGIX](Graphics-GL-Groups.html#FfdTargetSGIX).
  -> GLfloat -- ^ @u1@ of type @CoordF@.
  -> GLfloat -- ^ @u2@ of type @CoordF@.
  -> GLint -- ^ @ustride@.
  -> GLint -- ^ @uorder@ of type @CheckedInt32@.
  -> GLfloat -- ^ @v1@ of type @CoordF@.
  -> GLfloat -- ^ @v2@ of type @CoordF@.
  -> GLint -- ^ @vstride@.
  -> GLint -- ^ @vorder@ of type @CheckedInt32@.
  -> GLfloat -- ^ @w1@ of type @CoordF@.
  -> GLfloat -- ^ @w2@ of type @CoordF@.
  -> GLint -- ^ @wstride@.
  -> GLint -- ^ @worder@ of type @CheckedInt32@.
  -> Ptr GLfloat -- ^ @points@ pointing to @COMPSIZE(target,ustride,uorder,vstride,vorder,wstride,worder)@ elements of type @CoordF@.
  -> m ()
glDeformationMap3fSGIX v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 = liftIO $ dyn211 ptr_glDeformationMap3fSGIX v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14

{-# NOINLINE ptr_glDeformationMap3fSGIX #-}
ptr_glDeformationMap3fSGIX :: FunPtr (GLenum -> GLfloat -> GLfloat -> GLint -> GLint -> GLfloat -> GLfloat -> GLint -> GLint -> GLfloat -> GLfloat -> GLint -> GLint -> Ptr GLfloat -> IO ())
ptr_glDeformationMap3fSGIX = unsafePerformIO $ getCommand "glDeformationMap3fSGIX"

-- glDeleteAsyncMarkersSGIX ----------------------------------------------------

glDeleteAsyncMarkersSGIX
  :: MonadIO m
  => GLuint -- ^ @marker@.
  -> GLsizei -- ^ @range@.
  -> m ()
glDeleteAsyncMarkersSGIX v1 v2 = liftIO $ dyn212 ptr_glDeleteAsyncMarkersSGIX v1 v2

{-# NOINLINE ptr_glDeleteAsyncMarkersSGIX #-}
ptr_glDeleteAsyncMarkersSGIX :: FunPtr (GLuint -> GLsizei -> IO ())
ptr_glDeleteAsyncMarkersSGIX = unsafePerformIO $ getCommand "glDeleteAsyncMarkersSGIX"

-- glDeleteBuffers -------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glDeleteBuffers.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glDeleteBuffers.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glDeleteBuffers.xhtml OpenGL 4.x>.
glDeleteBuffers
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @buffers@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glDeleteBuffers v1 v2 = liftIO $ dyn193 ptr_glDeleteBuffers v1 v2

{-# NOINLINE ptr_glDeleteBuffers #-}
ptr_glDeleteBuffers :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteBuffers = unsafePerformIO $ getCommand "glDeleteBuffers"

-- glDeleteBuffersARB ----------------------------------------------------------

-- | This command is an alias for 'glDeleteBuffers'.
glDeleteBuffersARB
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @buffers@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glDeleteBuffersARB v1 v2 = liftIO $ dyn193 ptr_glDeleteBuffersARB v1 v2

{-# NOINLINE ptr_glDeleteBuffersARB #-}
ptr_glDeleteBuffersARB :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteBuffersARB = unsafePerformIO $ getCommand "glDeleteBuffersARB"

-- glDeleteCommandListsNV ------------------------------------------------------

glDeleteCommandListsNV
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @lists@.
  -> m ()
glDeleteCommandListsNV v1 v2 = liftIO $ dyn193 ptr_glDeleteCommandListsNV v1 v2

{-# NOINLINE ptr_glDeleteCommandListsNV #-}
ptr_glDeleteCommandListsNV :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteCommandListsNV = unsafePerformIO $ getCommand "glDeleteCommandListsNV"

-- glDeleteFencesAPPLE ---------------------------------------------------------

glDeleteFencesAPPLE
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @fences@ pointing to @n@ elements of type @FenceNV@.
  -> m ()
glDeleteFencesAPPLE v1 v2 = liftIO $ dyn193 ptr_glDeleteFencesAPPLE v1 v2

{-# NOINLINE ptr_glDeleteFencesAPPLE #-}
ptr_glDeleteFencesAPPLE :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteFencesAPPLE = unsafePerformIO $ getCommand "glDeleteFencesAPPLE"

-- glDeleteFencesNV ------------------------------------------------------------

glDeleteFencesNV
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @fences@ pointing to @n@ elements of type @FenceNV@.
  -> m ()
glDeleteFencesNV v1 v2 = liftIO $ dyn193 ptr_glDeleteFencesNV v1 v2

{-# NOINLINE ptr_glDeleteFencesNV #-}
ptr_glDeleteFencesNV :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteFencesNV = unsafePerformIO $ getCommand "glDeleteFencesNV"

-- glDeleteFragmentShaderATI ---------------------------------------------------

glDeleteFragmentShaderATI
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> m ()
glDeleteFragmentShaderATI v1 = liftIO $ dyn2 ptr_glDeleteFragmentShaderATI v1

{-# NOINLINE ptr_glDeleteFragmentShaderATI #-}
ptr_glDeleteFragmentShaderATI :: FunPtr (GLuint -> IO ())
ptr_glDeleteFragmentShaderATI = unsafePerformIO $ getCommand "glDeleteFragmentShaderATI"

-- glDeleteFramebuffers --------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glDeleteFramebuffers.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glDeleteFramebuffers.xhtml OpenGL 4.x>.
glDeleteFramebuffers
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @framebuffers@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glDeleteFramebuffers v1 v2 = liftIO $ dyn193 ptr_glDeleteFramebuffers v1 v2

{-# NOINLINE ptr_glDeleteFramebuffers #-}
ptr_glDeleteFramebuffers :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteFramebuffers = unsafePerformIO $ getCommand "glDeleteFramebuffers"

-- glDeleteFramebuffersEXT -----------------------------------------------------

-- | This command is an alias for 'glDeleteFramebuffers'.
glDeleteFramebuffersEXT
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @framebuffers@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glDeleteFramebuffersEXT v1 v2 = liftIO $ dyn193 ptr_glDeleteFramebuffersEXT v1 v2

{-# NOINLINE ptr_glDeleteFramebuffersEXT #-}
ptr_glDeleteFramebuffersEXT :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteFramebuffersEXT = unsafePerformIO $ getCommand "glDeleteFramebuffersEXT"

-- glDeleteFramebuffersOES -----------------------------------------------------

glDeleteFramebuffersOES
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @framebuffers@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glDeleteFramebuffersOES v1 v2 = liftIO $ dyn193 ptr_glDeleteFramebuffersOES v1 v2

{-# NOINLINE ptr_glDeleteFramebuffersOES #-}
ptr_glDeleteFramebuffersOES :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteFramebuffersOES = unsafePerformIO $ getCommand "glDeleteFramebuffersOES"

-- glDeleteLists ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glDeleteLists.xml OpenGL 2.x>.
glDeleteLists
  :: MonadIO m
  => GLuint -- ^ @list@ of type @List@.
  -> GLsizei -- ^ @range@.
  -> m ()
glDeleteLists v1 v2 = liftIO $ dyn212 ptr_glDeleteLists v1 v2

{-# NOINLINE ptr_glDeleteLists #-}
ptr_glDeleteLists :: FunPtr (GLuint -> GLsizei -> IO ())
ptr_glDeleteLists = unsafePerformIO $ getCommand "glDeleteLists"

-- glDeleteNamedStringARB ------------------------------------------------------

glDeleteNamedStringARB
  :: MonadIO m
  => GLint -- ^ @namelen@.
  -> Ptr GLchar -- ^ @name@ pointing to @namelen@ elements of type @GLchar@.
  -> m ()
glDeleteNamedStringARB v1 v2 = liftIO $ dyn213 ptr_glDeleteNamedStringARB v1 v2

{-# NOINLINE ptr_glDeleteNamedStringARB #-}
ptr_glDeleteNamedStringARB :: FunPtr (GLint -> Ptr GLchar -> IO ())
ptr_glDeleteNamedStringARB = unsafePerformIO $ getCommand "glDeleteNamedStringARB"

-- glDeleteNamesAMD ------------------------------------------------------------

glDeleteNamesAMD
  :: MonadIO m
  => GLenum -- ^ @identifier@.
  -> GLuint -- ^ @num@.
  -> Ptr GLuint -- ^ @names@ pointing to @num@ elements of type @GLuint@.
  -> m ()
glDeleteNamesAMD v1 v2 v3 = liftIO $ dyn214 ptr_glDeleteNamesAMD v1 v2 v3

{-# NOINLINE ptr_glDeleteNamesAMD #-}
ptr_glDeleteNamesAMD :: FunPtr (GLenum -> GLuint -> Ptr GLuint -> IO ())
ptr_glDeleteNamesAMD = unsafePerformIO $ getCommand "glDeleteNamesAMD"

-- glDeleteObjectARB -----------------------------------------------------------

glDeleteObjectARB
  :: MonadIO m
  => GLhandleARB -- ^ @obj@ of type @handleARB@.
  -> m ()
glDeleteObjectARB v1 = liftIO $ dyn137 ptr_glDeleteObjectARB v1

{-# NOINLINE ptr_glDeleteObjectARB #-}
ptr_glDeleteObjectARB :: FunPtr (GLhandleARB -> IO ())
ptr_glDeleteObjectARB = unsafePerformIO $ getCommand "glDeleteObjectARB"

-- glDeleteOcclusionQueriesNV --------------------------------------------------

glDeleteOcclusionQueriesNV
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @ids@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glDeleteOcclusionQueriesNV v1 v2 = liftIO $ dyn193 ptr_glDeleteOcclusionQueriesNV v1 v2

{-# NOINLINE ptr_glDeleteOcclusionQueriesNV #-}
ptr_glDeleteOcclusionQueriesNV :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteOcclusionQueriesNV = unsafePerformIO $ getCommand "glDeleteOcclusionQueriesNV"

-- glDeletePathsNV -------------------------------------------------------------

glDeletePathsNV
  :: MonadIO m
  => GLuint -- ^ @path@ of type @Path@.
  -> GLsizei -- ^ @range@.
  -> m ()
glDeletePathsNV v1 v2 = liftIO $ dyn212 ptr_glDeletePathsNV v1 v2

{-# NOINLINE ptr_glDeletePathsNV #-}
ptr_glDeletePathsNV :: FunPtr (GLuint -> GLsizei -> IO ())
ptr_glDeletePathsNV = unsafePerformIO $ getCommand "glDeletePathsNV"

-- glDeletePerfMonitorsAMD -----------------------------------------------------

glDeletePerfMonitorsAMD
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @monitors@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glDeletePerfMonitorsAMD v1 v2 = liftIO $ dyn193 ptr_glDeletePerfMonitorsAMD v1 v2

{-# NOINLINE ptr_glDeletePerfMonitorsAMD #-}
ptr_glDeletePerfMonitorsAMD :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeletePerfMonitorsAMD = unsafePerformIO $ getCommand "glDeletePerfMonitorsAMD"

-- glDeletePerfQueryINTEL ------------------------------------------------------

glDeletePerfQueryINTEL
  :: MonadIO m
  => GLuint -- ^ @queryHandle@.
  -> m ()
glDeletePerfQueryINTEL v1 = liftIO $ dyn2 ptr_glDeletePerfQueryINTEL v1

{-# NOINLINE ptr_glDeletePerfQueryINTEL #-}
ptr_glDeletePerfQueryINTEL :: FunPtr (GLuint -> IO ())
ptr_glDeletePerfQueryINTEL = unsafePerformIO $ getCommand "glDeletePerfQueryINTEL"

-- glDeleteProgram -------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glDeleteProgram.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glDeleteProgram.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glDeleteProgram.xhtml OpenGL 4.x>.
glDeleteProgram
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> m ()
glDeleteProgram v1 = liftIO $ dyn2 ptr_glDeleteProgram v1

{-# NOINLINE ptr_glDeleteProgram #-}
ptr_glDeleteProgram :: FunPtr (GLuint -> IO ())
ptr_glDeleteProgram = unsafePerformIO $ getCommand "glDeleteProgram"

-- glDeleteProgramPipelines ----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glDeleteProgramPipelines.xhtml OpenGL 4.x>.
glDeleteProgramPipelines
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @pipelines@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glDeleteProgramPipelines v1 v2 = liftIO $ dyn193 ptr_glDeleteProgramPipelines v1 v2

{-# NOINLINE ptr_glDeleteProgramPipelines #-}
ptr_glDeleteProgramPipelines :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteProgramPipelines = unsafePerformIO $ getCommand "glDeleteProgramPipelines"

-- glDeleteProgramPipelinesEXT -------------------------------------------------

glDeleteProgramPipelinesEXT
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @pipelines@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glDeleteProgramPipelinesEXT v1 v2 = liftIO $ dyn193 ptr_glDeleteProgramPipelinesEXT v1 v2

{-# NOINLINE ptr_glDeleteProgramPipelinesEXT #-}
ptr_glDeleteProgramPipelinesEXT :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteProgramPipelinesEXT = unsafePerformIO $ getCommand "glDeleteProgramPipelinesEXT"

-- glDeleteProgramsARB ---------------------------------------------------------

glDeleteProgramsARB
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @programs@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glDeleteProgramsARB v1 v2 = liftIO $ dyn193 ptr_glDeleteProgramsARB v1 v2

{-# NOINLINE ptr_glDeleteProgramsARB #-}
ptr_glDeleteProgramsARB :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteProgramsARB = unsafePerformIO $ getCommand "glDeleteProgramsARB"

-- glDeleteProgramsNV ----------------------------------------------------------

-- | This command is an alias for 'glDeleteProgramsARB'.
glDeleteProgramsNV
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @programs@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glDeleteProgramsNV v1 v2 = liftIO $ dyn193 ptr_glDeleteProgramsNV v1 v2

{-# NOINLINE ptr_glDeleteProgramsNV #-}
ptr_glDeleteProgramsNV :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteProgramsNV = unsafePerformIO $ getCommand "glDeleteProgramsNV"

-- glDeleteQueries -------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glDeleteQueries.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glDeleteQueries.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glDeleteQueries.xhtml OpenGL 4.x>.
glDeleteQueries
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @ids@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glDeleteQueries v1 v2 = liftIO $ dyn193 ptr_glDeleteQueries v1 v2

{-# NOINLINE ptr_glDeleteQueries #-}
ptr_glDeleteQueries :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteQueries = unsafePerformIO $ getCommand "glDeleteQueries"

-- glDeleteQueriesARB ----------------------------------------------------------

-- | This command is an alias for 'glDeleteQueries'.
glDeleteQueriesARB
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @ids@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glDeleteQueriesARB v1 v2 = liftIO $ dyn193 ptr_glDeleteQueriesARB v1 v2

{-# NOINLINE ptr_glDeleteQueriesARB #-}
ptr_glDeleteQueriesARB :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteQueriesARB = unsafePerformIO $ getCommand "glDeleteQueriesARB"

-- glDeleteQueriesEXT ----------------------------------------------------------

glDeleteQueriesEXT
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @ids@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glDeleteQueriesEXT v1 v2 = liftIO $ dyn193 ptr_glDeleteQueriesEXT v1 v2

{-# NOINLINE ptr_glDeleteQueriesEXT #-}
ptr_glDeleteQueriesEXT :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteQueriesEXT = unsafePerformIO $ getCommand "glDeleteQueriesEXT"

-- glDeleteRenderbuffers -------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glDeleteRenderbuffers.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glDeleteRenderbuffers.xhtml OpenGL 4.x>.
glDeleteRenderbuffers
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @renderbuffers@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glDeleteRenderbuffers v1 v2 = liftIO $ dyn193 ptr_glDeleteRenderbuffers v1 v2

{-# NOINLINE ptr_glDeleteRenderbuffers #-}
ptr_glDeleteRenderbuffers :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteRenderbuffers = unsafePerformIO $ getCommand "glDeleteRenderbuffers"

-- glDeleteRenderbuffersEXT ----------------------------------------------------

-- | This command is an alias for 'glDeleteRenderbuffers'.
glDeleteRenderbuffersEXT
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @renderbuffers@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glDeleteRenderbuffersEXT v1 v2 = liftIO $ dyn193 ptr_glDeleteRenderbuffersEXT v1 v2

{-# NOINLINE ptr_glDeleteRenderbuffersEXT #-}
ptr_glDeleteRenderbuffersEXT :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteRenderbuffersEXT = unsafePerformIO $ getCommand "glDeleteRenderbuffersEXT"

-- glDeleteRenderbuffersOES ----------------------------------------------------

glDeleteRenderbuffersOES
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @renderbuffers@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glDeleteRenderbuffersOES v1 v2 = liftIO $ dyn193 ptr_glDeleteRenderbuffersOES v1 v2

{-# NOINLINE ptr_glDeleteRenderbuffersOES #-}
ptr_glDeleteRenderbuffersOES :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteRenderbuffersOES = unsafePerformIO $ getCommand "glDeleteRenderbuffersOES"

-- glDeleteSamplers ------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glDeleteSamplers.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glDeleteSamplers.xhtml OpenGL 4.x>.
glDeleteSamplers
  :: MonadIO m
  => GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @samplers@ pointing to @count@ elements of type @GLuint@.
  -> m ()
glDeleteSamplers v1 v2 = liftIO $ dyn193 ptr_glDeleteSamplers v1 v2

{-# NOINLINE ptr_glDeleteSamplers #-}
ptr_glDeleteSamplers :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteSamplers = unsafePerformIO $ getCommand "glDeleteSamplers"

-- glDeleteShader --------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glDeleteShader.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glDeleteShader.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glDeleteShader.xhtml OpenGL 4.x>.
glDeleteShader
  :: MonadIO m
  => GLuint -- ^ @shader@.
  -> m ()
glDeleteShader v1 = liftIO $ dyn2 ptr_glDeleteShader v1

{-# NOINLINE ptr_glDeleteShader #-}
ptr_glDeleteShader :: FunPtr (GLuint -> IO ())
ptr_glDeleteShader = unsafePerformIO $ getCommand "glDeleteShader"

-- glDeleteStatesNV ------------------------------------------------------------

glDeleteStatesNV
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @states@.
  -> m ()
glDeleteStatesNV v1 v2 = liftIO $ dyn193 ptr_glDeleteStatesNV v1 v2

{-# NOINLINE ptr_glDeleteStatesNV #-}
ptr_glDeleteStatesNV :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteStatesNV = unsafePerformIO $ getCommand "glDeleteStatesNV"

-- glDeleteSync ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glDeleteSync.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glDeleteSync.xhtml OpenGL 4.x>.
glDeleteSync
  :: MonadIO m
  => GLsync -- ^ @sync@ of type @sync@.
  -> m ()
glDeleteSync v1 = liftIO $ dyn215 ptr_glDeleteSync v1

{-# NOINLINE ptr_glDeleteSync #-}
ptr_glDeleteSync :: FunPtr (GLsync -> IO ())
ptr_glDeleteSync = unsafePerformIO $ getCommand "glDeleteSync"

-- glDeleteSyncAPPLE -----------------------------------------------------------

-- | This command is an alias for 'glDeleteSync'.
glDeleteSyncAPPLE
  :: MonadIO m
  => GLsync -- ^ @sync@.
  -> m ()
glDeleteSyncAPPLE v1 = liftIO $ dyn215 ptr_glDeleteSyncAPPLE v1

{-# NOINLINE ptr_glDeleteSyncAPPLE #-}
ptr_glDeleteSyncAPPLE :: FunPtr (GLsync -> IO ())
ptr_glDeleteSyncAPPLE = unsafePerformIO $ getCommand "glDeleteSyncAPPLE"

-- glDeleteTextures ------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glDeleteTextures.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glDeleteTextures.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glDeleteTextures.xhtml OpenGL 4.x>.
glDeleteTextures
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @textures@ pointing to @n@ elements of type @Texture@.
  -> m ()
glDeleteTextures v1 v2 = liftIO $ dyn193 ptr_glDeleteTextures v1 v2

{-# NOINLINE ptr_glDeleteTextures #-}
ptr_glDeleteTextures :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteTextures = unsafePerformIO $ getCommand "glDeleteTextures"

-- glDeleteTexturesEXT ---------------------------------------------------------

glDeleteTexturesEXT
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @textures@ pointing to @n@ elements of type @Texture@.
  -> m ()
glDeleteTexturesEXT v1 v2 = liftIO $ dyn193 ptr_glDeleteTexturesEXT v1 v2

{-# NOINLINE ptr_glDeleteTexturesEXT #-}
ptr_glDeleteTexturesEXT :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteTexturesEXT = unsafePerformIO $ getCommand "glDeleteTexturesEXT"

-- glDeleteTransformFeedbacks --------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glDeleteTransformFeedbacks.xhtml OpenGL 4.x>.
glDeleteTransformFeedbacks
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @ids@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glDeleteTransformFeedbacks v1 v2 = liftIO $ dyn193 ptr_glDeleteTransformFeedbacks v1 v2

{-# NOINLINE ptr_glDeleteTransformFeedbacks #-}
ptr_glDeleteTransformFeedbacks :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteTransformFeedbacks = unsafePerformIO $ getCommand "glDeleteTransformFeedbacks"

-- glDeleteTransformFeedbacksNV ------------------------------------------------

-- | This command is an alias for 'glDeleteTransformFeedbacks'.
glDeleteTransformFeedbacksNV
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @ids@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glDeleteTransformFeedbacksNV v1 v2 = liftIO $ dyn193 ptr_glDeleteTransformFeedbacksNV v1 v2

{-# NOINLINE ptr_glDeleteTransformFeedbacksNV #-}
ptr_glDeleteTransformFeedbacksNV :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteTransformFeedbacksNV = unsafePerformIO $ getCommand "glDeleteTransformFeedbacksNV"

