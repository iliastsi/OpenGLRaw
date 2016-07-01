--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F20
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

module Graphics.GL.Functions.F20 (
  glFrustumf,
  glFrustumfOES,
  glFrustumx,
  glFrustumxOES,
  glGenAsyncMarkersSGIX,
  glGenBuffers,
  glGenBuffersARB,
  glGenFencesAPPLE,
  glGenFencesNV,
  glGenFragmentShadersATI,
  glGenFramebuffers,
  glGenFramebuffersEXT,
  glGenFramebuffersOES,
  glGenLists,
  glGenNamesAMD,
  glGenOcclusionQueriesNV,
  glGenPathsNV,
  glGenPerfMonitorsAMD,
  glGenProgramPipelines,
  glGenProgramPipelinesEXT,
  glGenProgramsARB,
  glGenProgramsNV,
  glGenQueries,
  glGenQueriesARB,
  glGenQueriesEXT,
  glGenRenderbuffers,
  glGenRenderbuffersEXT,
  glGenRenderbuffersOES,
  glGenSamplers,
  glGenSymbolsEXT,
  glGenTextures,
  glGenTexturesEXT,
  glGenTransformFeedbacks,
  glGenTransformFeedbacksNV,
  glGenVertexArrays,
  glGenVertexArraysAPPLE,
  glGenVertexArraysOES,
  glGenVertexShadersEXT,
  glGenerateMipmap,
  glGenerateMipmapEXT
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

-- glFrustumf ------------------------------------------------------------------

glFrustumf
  :: MonadIO m
  => GLfloat -- ^ @l@.
  -> GLfloat -- ^ @r@.
  -> GLfloat -- ^ @b@.
  -> GLfloat -- ^ @t@.
  -> GLfloat -- ^ @n@.
  -> GLfloat -- ^ @f@.
  -> m ()
glFrustumf v1 v2 v3 v4 v5 v6 = liftIO $ dyn96 ptr_glFrustumf v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glFrustumf #-}
ptr_glFrustumf :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glFrustumf = unsafePerformIO $ getCommand "glFrustumf"

-- glFrustumfOES ---------------------------------------------------------------

glFrustumfOES
  :: MonadIO m
  => GLfloat -- ^ @l@.
  -> GLfloat -- ^ @r@.
  -> GLfloat -- ^ @b@.
  -> GLfloat -- ^ @t@.
  -> GLfloat -- ^ @n@.
  -> GLfloat -- ^ @f@.
  -> m ()
glFrustumfOES v1 v2 v3 v4 v5 v6 = liftIO $ dyn96 ptr_glFrustumfOES v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glFrustumfOES #-}
ptr_glFrustumfOES :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glFrustumfOES = unsafePerformIO $ getCommand "glFrustumfOES"

-- glFrustumx ------------------------------------------------------------------

glFrustumx
  :: MonadIO m
  => GLfixed -- ^ @l@.
  -> GLfixed -- ^ @r@.
  -> GLfixed -- ^ @b@.
  -> GLfixed -- ^ @t@.
  -> GLfixed -- ^ @n@.
  -> GLfixed -- ^ @f@.
  -> m ()
glFrustumx v1 v2 v3 v4 v5 v6 = liftIO $ dyn296 ptr_glFrustumx v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glFrustumx #-}
ptr_glFrustumx :: FunPtr (GLfixed -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ())
ptr_glFrustumx = unsafePerformIO $ getCommand "glFrustumx"

-- glFrustumxOES ---------------------------------------------------------------

glFrustumxOES
  :: MonadIO m
  => GLfixed -- ^ @l@.
  -> GLfixed -- ^ @r@.
  -> GLfixed -- ^ @b@.
  -> GLfixed -- ^ @t@.
  -> GLfixed -- ^ @n@.
  -> GLfixed -- ^ @f@.
  -> m ()
glFrustumxOES v1 v2 v3 v4 v5 v6 = liftIO $ dyn296 ptr_glFrustumxOES v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glFrustumxOES #-}
ptr_glFrustumxOES :: FunPtr (GLfixed -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ())
ptr_glFrustumxOES = unsafePerformIO $ getCommand "glFrustumxOES"

-- glGenAsyncMarkersSGIX -------------------------------------------------------

glGenAsyncMarkersSGIX
  :: MonadIO m
  => GLsizei -- ^ @range@.
  -> m GLuint
glGenAsyncMarkersSGIX v1 = liftIO $ dyn297 ptr_glGenAsyncMarkersSGIX v1

{-# NOINLINE ptr_glGenAsyncMarkersSGIX #-}
ptr_glGenAsyncMarkersSGIX :: FunPtr (GLsizei -> IO GLuint)
ptr_glGenAsyncMarkersSGIX = unsafePerformIO $ getCommand "glGenAsyncMarkersSGIX"

-- glGenBuffers ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGenBuffers.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGenBuffers.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGenBuffers.xhtml OpenGL 4.x>.
glGenBuffers
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @buffers@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glGenBuffers v1 v2 = liftIO $ dyn193 ptr_glGenBuffers v1 v2

{-# NOINLINE ptr_glGenBuffers #-}
ptr_glGenBuffers :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glGenBuffers = unsafePerformIO $ getCommand "glGenBuffers"

-- glGenBuffersARB -------------------------------------------------------------

-- | This command is an alias for 'glGenBuffers'.
glGenBuffersARB
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @buffers@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glGenBuffersARB v1 v2 = liftIO $ dyn193 ptr_glGenBuffersARB v1 v2

{-# NOINLINE ptr_glGenBuffersARB #-}
ptr_glGenBuffersARB :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glGenBuffersARB = unsafePerformIO $ getCommand "glGenBuffersARB"

-- glGenFencesAPPLE ------------------------------------------------------------

glGenFencesAPPLE
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @fences@ pointing to @n@ elements of type @FenceNV@.
  -> m ()
glGenFencesAPPLE v1 v2 = liftIO $ dyn193 ptr_glGenFencesAPPLE v1 v2

{-# NOINLINE ptr_glGenFencesAPPLE #-}
ptr_glGenFencesAPPLE :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glGenFencesAPPLE = unsafePerformIO $ getCommand "glGenFencesAPPLE"

-- glGenFencesNV ---------------------------------------------------------------

glGenFencesNV
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @fences@ pointing to @n@ elements of type @FenceNV@.
  -> m ()
glGenFencesNV v1 v2 = liftIO $ dyn193 ptr_glGenFencesNV v1 v2

{-# NOINLINE ptr_glGenFencesNV #-}
ptr_glGenFencesNV :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glGenFencesNV = unsafePerformIO $ getCommand "glGenFencesNV"

-- glGenFragmentShadersATI -----------------------------------------------------

glGenFragmentShadersATI
  :: MonadIO m
  => GLuint -- ^ @range@.
  -> m GLuint
glGenFragmentShadersATI v1 = liftIO $ dyn298 ptr_glGenFragmentShadersATI v1

{-# NOINLINE ptr_glGenFragmentShadersATI #-}
ptr_glGenFragmentShadersATI :: FunPtr (GLuint -> IO GLuint)
ptr_glGenFragmentShadersATI = unsafePerformIO $ getCommand "glGenFragmentShadersATI"

-- glGenFramebuffers -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGenFramebuffers.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGenFramebuffers.xhtml OpenGL 4.x>.
glGenFramebuffers
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @framebuffers@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glGenFramebuffers v1 v2 = liftIO $ dyn193 ptr_glGenFramebuffers v1 v2

{-# NOINLINE ptr_glGenFramebuffers #-}
ptr_glGenFramebuffers :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glGenFramebuffers = unsafePerformIO $ getCommand "glGenFramebuffers"

-- glGenFramebuffersEXT --------------------------------------------------------

-- | This command is an alias for 'glGenFramebuffers'.
glGenFramebuffersEXT
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @framebuffers@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glGenFramebuffersEXT v1 v2 = liftIO $ dyn193 ptr_glGenFramebuffersEXT v1 v2

{-# NOINLINE ptr_glGenFramebuffersEXT #-}
ptr_glGenFramebuffersEXT :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glGenFramebuffersEXT = unsafePerformIO $ getCommand "glGenFramebuffersEXT"

-- glGenFramebuffersOES --------------------------------------------------------

glGenFramebuffersOES
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @framebuffers@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glGenFramebuffersOES v1 v2 = liftIO $ dyn193 ptr_glGenFramebuffersOES v1 v2

{-# NOINLINE ptr_glGenFramebuffersOES #-}
ptr_glGenFramebuffersOES :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glGenFramebuffersOES = unsafePerformIO $ getCommand "glGenFramebuffersOES"

-- glGenLists ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glGenLists.xml OpenGL 2.x>.
glGenLists
  :: MonadIO m
  => GLsizei -- ^ @range@.
  -> m GLuint -- ^ of type @List@.
glGenLists v1 = liftIO $ dyn297 ptr_glGenLists v1

{-# NOINLINE ptr_glGenLists #-}
ptr_glGenLists :: FunPtr (GLsizei -> IO GLuint)
ptr_glGenLists = unsafePerformIO $ getCommand "glGenLists"

-- glGenNamesAMD ---------------------------------------------------------------

glGenNamesAMD
  :: MonadIO m
  => GLenum -- ^ @identifier@.
  -> GLuint -- ^ @num@.
  -> Ptr GLuint -- ^ @names@ pointing to @num@ elements of type @GLuint@.
  -> m ()
glGenNamesAMD v1 v2 v3 = liftIO $ dyn214 ptr_glGenNamesAMD v1 v2 v3

{-# NOINLINE ptr_glGenNamesAMD #-}
ptr_glGenNamesAMD :: FunPtr (GLenum -> GLuint -> Ptr GLuint -> IO ())
ptr_glGenNamesAMD = unsafePerformIO $ getCommand "glGenNamesAMD"

-- glGenOcclusionQueriesNV -----------------------------------------------------

glGenOcclusionQueriesNV
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @ids@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glGenOcclusionQueriesNV v1 v2 = liftIO $ dyn193 ptr_glGenOcclusionQueriesNV v1 v2

{-# NOINLINE ptr_glGenOcclusionQueriesNV #-}
ptr_glGenOcclusionQueriesNV :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glGenOcclusionQueriesNV = unsafePerformIO $ getCommand "glGenOcclusionQueriesNV"

-- glGenPathsNV ----------------------------------------------------------------

glGenPathsNV
  :: MonadIO m
  => GLsizei -- ^ @range@.
  -> m GLuint -- ^ of type @Path@.
glGenPathsNV v1 = liftIO $ dyn297 ptr_glGenPathsNV v1

{-# NOINLINE ptr_glGenPathsNV #-}
ptr_glGenPathsNV :: FunPtr (GLsizei -> IO GLuint)
ptr_glGenPathsNV = unsafePerformIO $ getCommand "glGenPathsNV"

-- glGenPerfMonitorsAMD --------------------------------------------------------

glGenPerfMonitorsAMD
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @monitors@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glGenPerfMonitorsAMD v1 v2 = liftIO $ dyn193 ptr_glGenPerfMonitorsAMD v1 v2

{-# NOINLINE ptr_glGenPerfMonitorsAMD #-}
ptr_glGenPerfMonitorsAMD :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glGenPerfMonitorsAMD = unsafePerformIO $ getCommand "glGenPerfMonitorsAMD"

-- glGenProgramPipelines -------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGenProgramPipelines.xhtml OpenGL 4.x>.
glGenProgramPipelines
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @pipelines@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glGenProgramPipelines v1 v2 = liftIO $ dyn193 ptr_glGenProgramPipelines v1 v2

{-# NOINLINE ptr_glGenProgramPipelines #-}
ptr_glGenProgramPipelines :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glGenProgramPipelines = unsafePerformIO $ getCommand "glGenProgramPipelines"

-- glGenProgramPipelinesEXT ----------------------------------------------------

glGenProgramPipelinesEXT
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @pipelines@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glGenProgramPipelinesEXT v1 v2 = liftIO $ dyn193 ptr_glGenProgramPipelinesEXT v1 v2

{-# NOINLINE ptr_glGenProgramPipelinesEXT #-}
ptr_glGenProgramPipelinesEXT :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glGenProgramPipelinesEXT = unsafePerformIO $ getCommand "glGenProgramPipelinesEXT"

-- glGenProgramsARB ------------------------------------------------------------

glGenProgramsARB
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @programs@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glGenProgramsARB v1 v2 = liftIO $ dyn193 ptr_glGenProgramsARB v1 v2

{-# NOINLINE ptr_glGenProgramsARB #-}
ptr_glGenProgramsARB :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glGenProgramsARB = unsafePerformIO $ getCommand "glGenProgramsARB"

-- glGenProgramsNV -------------------------------------------------------------

-- | This command is an alias for 'glGenProgramsARB'.
glGenProgramsNV
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @programs@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glGenProgramsNV v1 v2 = liftIO $ dyn193 ptr_glGenProgramsNV v1 v2

{-# NOINLINE ptr_glGenProgramsNV #-}
ptr_glGenProgramsNV :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glGenProgramsNV = unsafePerformIO $ getCommand "glGenProgramsNV"

-- glGenQueries ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGenQueries.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGenQueries.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGenQueries.xhtml OpenGL 4.x>.
glGenQueries
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @ids@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glGenQueries v1 v2 = liftIO $ dyn193 ptr_glGenQueries v1 v2

{-# NOINLINE ptr_glGenQueries #-}
ptr_glGenQueries :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glGenQueries = unsafePerformIO $ getCommand "glGenQueries"

-- glGenQueriesARB -------------------------------------------------------------

-- | This command is an alias for 'glGenQueries'.
glGenQueriesARB
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @ids@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glGenQueriesARB v1 v2 = liftIO $ dyn193 ptr_glGenQueriesARB v1 v2

{-# NOINLINE ptr_glGenQueriesARB #-}
ptr_glGenQueriesARB :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glGenQueriesARB = unsafePerformIO $ getCommand "glGenQueriesARB"

-- glGenQueriesEXT -------------------------------------------------------------

glGenQueriesEXT
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @ids@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glGenQueriesEXT v1 v2 = liftIO $ dyn193 ptr_glGenQueriesEXT v1 v2

{-# NOINLINE ptr_glGenQueriesEXT #-}
ptr_glGenQueriesEXT :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glGenQueriesEXT = unsafePerformIO $ getCommand "glGenQueriesEXT"

-- glGenRenderbuffers ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGenRenderbuffers.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGenRenderbuffers.xhtml OpenGL 4.x>.
glGenRenderbuffers
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @renderbuffers@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glGenRenderbuffers v1 v2 = liftIO $ dyn193 ptr_glGenRenderbuffers v1 v2

{-# NOINLINE ptr_glGenRenderbuffers #-}
ptr_glGenRenderbuffers :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glGenRenderbuffers = unsafePerformIO $ getCommand "glGenRenderbuffers"

-- glGenRenderbuffersEXT -------------------------------------------------------

-- | This command is an alias for 'glGenRenderbuffers'.
glGenRenderbuffersEXT
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @renderbuffers@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glGenRenderbuffersEXT v1 v2 = liftIO $ dyn193 ptr_glGenRenderbuffersEXT v1 v2

{-# NOINLINE ptr_glGenRenderbuffersEXT #-}
ptr_glGenRenderbuffersEXT :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glGenRenderbuffersEXT = unsafePerformIO $ getCommand "glGenRenderbuffersEXT"

-- glGenRenderbuffersOES -------------------------------------------------------

glGenRenderbuffersOES
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @renderbuffers@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glGenRenderbuffersOES v1 v2 = liftIO $ dyn193 ptr_glGenRenderbuffersOES v1 v2

{-# NOINLINE ptr_glGenRenderbuffersOES #-}
ptr_glGenRenderbuffersOES :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glGenRenderbuffersOES = unsafePerformIO $ getCommand "glGenRenderbuffersOES"

-- glGenSamplers ---------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGenSamplers.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGenSamplers.xhtml OpenGL 4.x>.
glGenSamplers
  :: MonadIO m
  => GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @samplers@ pointing to @count@ elements of type @GLuint@.
  -> m ()
glGenSamplers v1 v2 = liftIO $ dyn193 ptr_glGenSamplers v1 v2

{-# NOINLINE ptr_glGenSamplers #-}
ptr_glGenSamplers :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glGenSamplers = unsafePerformIO $ getCommand "glGenSamplers"

-- glGenSymbolsEXT -------------------------------------------------------------

glGenSymbolsEXT
  :: MonadIO m
  => GLenum -- ^ @datatype@ of type @DataTypeEXT@.
  -> GLenum -- ^ @storagetype@ of type @VertexShaderStorageTypeEXT@.
  -> GLenum -- ^ @range@ of type @ParameterRangeEXT@.
  -> GLuint -- ^ @components@.
  -> m GLuint
glGenSymbolsEXT v1 v2 v3 v4 = liftIO $ dyn299 ptr_glGenSymbolsEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGenSymbolsEXT #-}
ptr_glGenSymbolsEXT :: FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> IO GLuint)
ptr_glGenSymbolsEXT = unsafePerformIO $ getCommand "glGenSymbolsEXT"

-- glGenTextures ---------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGenTextures.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGenTextures.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGenTextures.xhtml OpenGL 4.x>.
glGenTextures
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @textures@ pointing to @n@ elements of type @Texture@.
  -> m ()
glGenTextures v1 v2 = liftIO $ dyn193 ptr_glGenTextures v1 v2

{-# NOINLINE ptr_glGenTextures #-}
ptr_glGenTextures :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glGenTextures = unsafePerformIO $ getCommand "glGenTextures"

-- glGenTexturesEXT ------------------------------------------------------------

glGenTexturesEXT
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @textures@ pointing to @n@ elements of type @Texture@.
  -> m ()
glGenTexturesEXT v1 v2 = liftIO $ dyn193 ptr_glGenTexturesEXT v1 v2

{-# NOINLINE ptr_glGenTexturesEXT #-}
ptr_glGenTexturesEXT :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glGenTexturesEXT = unsafePerformIO $ getCommand "glGenTexturesEXT"

-- glGenTransformFeedbacks -----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGenTransformFeedbacks.xhtml OpenGL 4.x>.
glGenTransformFeedbacks
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @ids@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glGenTransformFeedbacks v1 v2 = liftIO $ dyn193 ptr_glGenTransformFeedbacks v1 v2

{-# NOINLINE ptr_glGenTransformFeedbacks #-}
ptr_glGenTransformFeedbacks :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glGenTransformFeedbacks = unsafePerformIO $ getCommand "glGenTransformFeedbacks"

-- glGenTransformFeedbacksNV ---------------------------------------------------

-- | This command is an alias for 'glGenTransformFeedbacks'.
glGenTransformFeedbacksNV
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @ids@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glGenTransformFeedbacksNV v1 v2 = liftIO $ dyn193 ptr_glGenTransformFeedbacksNV v1 v2

{-# NOINLINE ptr_glGenTransformFeedbacksNV #-}
ptr_glGenTransformFeedbacksNV :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glGenTransformFeedbacksNV = unsafePerformIO $ getCommand "glGenTransformFeedbacksNV"

-- glGenVertexArrays -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGenVertexArrays.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGenVertexArrays.xhtml OpenGL 4.x>.
glGenVertexArrays
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @arrays@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glGenVertexArrays v1 v2 = liftIO $ dyn193 ptr_glGenVertexArrays v1 v2

{-# NOINLINE ptr_glGenVertexArrays #-}
ptr_glGenVertexArrays :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glGenVertexArrays = unsafePerformIO $ getCommand "glGenVertexArrays"

-- glGenVertexArraysAPPLE ------------------------------------------------------

-- | This command is an alias for 'glGenVertexArrays'.
glGenVertexArraysAPPLE
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @arrays@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glGenVertexArraysAPPLE v1 v2 = liftIO $ dyn193 ptr_glGenVertexArraysAPPLE v1 v2

{-# NOINLINE ptr_glGenVertexArraysAPPLE #-}
ptr_glGenVertexArraysAPPLE :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glGenVertexArraysAPPLE = unsafePerformIO $ getCommand "glGenVertexArraysAPPLE"

-- glGenVertexArraysOES --------------------------------------------------------

-- | This command is an alias for 'glGenVertexArrays'.
glGenVertexArraysOES
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @arrays@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glGenVertexArraysOES v1 v2 = liftIO $ dyn193 ptr_glGenVertexArraysOES v1 v2

{-# NOINLINE ptr_glGenVertexArraysOES #-}
ptr_glGenVertexArraysOES :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glGenVertexArraysOES = unsafePerformIO $ getCommand "glGenVertexArraysOES"

-- glGenVertexShadersEXT -------------------------------------------------------

glGenVertexShadersEXT
  :: MonadIO m
  => GLuint -- ^ @range@.
  -> m GLuint
glGenVertexShadersEXT v1 = liftIO $ dyn298 ptr_glGenVertexShadersEXT v1

{-# NOINLINE ptr_glGenVertexShadersEXT #-}
ptr_glGenVertexShadersEXT :: FunPtr (GLuint -> IO GLuint)
ptr_glGenVertexShadersEXT = unsafePerformIO $ getCommand "glGenVertexShadersEXT"

-- glGenerateMipmap ------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGenerateMipmap.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGenerateMipmap.xhtml OpenGL 4.x>.
glGenerateMipmap
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> m ()
glGenerateMipmap v1 = liftIO $ dyn4 ptr_glGenerateMipmap v1

{-# NOINLINE ptr_glGenerateMipmap #-}
ptr_glGenerateMipmap :: FunPtr (GLenum -> IO ())
ptr_glGenerateMipmap = unsafePerformIO $ getCommand "glGenerateMipmap"

-- glGenerateMipmapEXT ---------------------------------------------------------

-- | This command is an alias for 'glGenerateMipmap'.
glGenerateMipmapEXT
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> m ()
glGenerateMipmapEXT v1 = liftIO $ dyn4 ptr_glGenerateMipmapEXT v1

{-# NOINLINE ptr_glGenerateMipmapEXT #-}
ptr_glGenerateMipmapEXT :: FunPtr (GLenum -> IO ())
ptr_glGenerateMipmapEXT = unsafePerformIO $ getCommand "glGenerateMipmapEXT"

