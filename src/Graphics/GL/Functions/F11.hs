--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F11
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

module Graphics.GL.Functions.F11 (
  glCoverageModulationTableNV,
  glCoverageOperationNV,
  glCreateBuffers,
  glCreateCommandListsNV,
  glCreateFramebuffers,
  glCreatePerfQueryINTEL,
  glCreateProgram,
  glCreateProgramObjectARB,
  glCreateProgramPipelines,
  glCreateQueries,
  glCreateRenderbuffers,
  glCreateSamplers,
  glCreateShader,
  glCreateShaderObjectARB,
  glCreateShaderProgramEXT,
  glCreateShaderProgramv,
  glCreateShaderProgramvEXT,
  glCreateStatesNV,
  glCreateSyncFromCLeventARB,
  glCreateTextures,
  glCreateTransformFeedbacks,
  glCreateVertexArrays,
  glCullFace,
  glCullParameterdvEXT,
  glCullParameterfvEXT,
  glCurrentPaletteMatrixARB,
  glCurrentPaletteMatrixOES,
  glDebugMessageCallback,
  glDebugMessageCallbackAMD,
  glDebugMessageCallbackARB,
  glDebugMessageCallbackKHR,
  glDebugMessageControl,
  glDebugMessageControlARB,
  glDebugMessageControlKHR,
  glDebugMessageEnableAMD,
  glDebugMessageInsert,
  glDebugMessageInsertAMD,
  glDebugMessageInsertARB,
  glDebugMessageInsertKHR,
  glDeformSGIX
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

-- glCoverageModulationTableNV -------------------------------------------------

glCoverageModulationTableNV
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLfloat -- ^ @v@.
  -> m ()
glCoverageModulationTableNV v1 v2 = liftIO $ dyn192 ptr_glCoverageModulationTableNV v1 v2

{-# NOINLINE ptr_glCoverageModulationTableNV #-}
ptr_glCoverageModulationTableNV :: FunPtr (GLsizei -> Ptr GLfloat -> IO ())
ptr_glCoverageModulationTableNV = unsafePerformIO $ getCommand "glCoverageModulationTableNV"

-- glCoverageOperationNV -------------------------------------------------------

glCoverageOperationNV
  :: MonadIO m
  => GLenum -- ^ @operation@.
  -> m ()
glCoverageOperationNV v1 = liftIO $ dyn4 ptr_glCoverageOperationNV v1

{-# NOINLINE ptr_glCoverageOperationNV #-}
ptr_glCoverageOperationNV :: FunPtr (GLenum -> IO ())
ptr_glCoverageOperationNV = unsafePerformIO $ getCommand "glCoverageOperationNV"

-- glCreateBuffers -------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glCreateBuffers.xhtml OpenGL 4.x>.
glCreateBuffers
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @buffers@.
  -> m ()
glCreateBuffers v1 v2 = liftIO $ dyn193 ptr_glCreateBuffers v1 v2

{-# NOINLINE ptr_glCreateBuffers #-}
ptr_glCreateBuffers :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glCreateBuffers = unsafePerformIO $ getCommand "glCreateBuffers"

-- glCreateCommandListsNV ------------------------------------------------------

glCreateCommandListsNV
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @lists@.
  -> m ()
glCreateCommandListsNV v1 v2 = liftIO $ dyn193 ptr_glCreateCommandListsNV v1 v2

{-# NOINLINE ptr_glCreateCommandListsNV #-}
ptr_glCreateCommandListsNV :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glCreateCommandListsNV = unsafePerformIO $ getCommand "glCreateCommandListsNV"

-- glCreateFramebuffers --------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glCreateFramebuffers.xhtml OpenGL 4.x>.
glCreateFramebuffers
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @framebuffers@.
  -> m ()
glCreateFramebuffers v1 v2 = liftIO $ dyn193 ptr_glCreateFramebuffers v1 v2

{-# NOINLINE ptr_glCreateFramebuffers #-}
ptr_glCreateFramebuffers :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glCreateFramebuffers = unsafePerformIO $ getCommand "glCreateFramebuffers"

-- glCreatePerfQueryINTEL ------------------------------------------------------

glCreatePerfQueryINTEL
  :: MonadIO m
  => GLuint -- ^ @queryId@.
  -> Ptr GLuint -- ^ @queryHandle@.
  -> m ()
glCreatePerfQueryINTEL v1 v2 = liftIO $ dyn194 ptr_glCreatePerfQueryINTEL v1 v2

{-# NOINLINE ptr_glCreatePerfQueryINTEL #-}
ptr_glCreatePerfQueryINTEL :: FunPtr (GLuint -> Ptr GLuint -> IO ())
ptr_glCreatePerfQueryINTEL = unsafePerformIO $ getCommand "glCreatePerfQueryINTEL"

-- glCreateProgram -------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glCreateProgram.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glCreateProgram.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glCreateProgram.xhtml OpenGL 4.x>.
glCreateProgram
  :: MonadIO m
  => m GLuint
glCreateProgram = liftIO $ dyn195 ptr_glCreateProgram

{-# NOINLINE ptr_glCreateProgram #-}
ptr_glCreateProgram :: FunPtr (IO GLuint)
ptr_glCreateProgram = unsafePerformIO $ getCommand "glCreateProgram"

-- glCreateProgramObjectARB ----------------------------------------------------

-- | This command is an alias for 'glCreateProgram'.
glCreateProgramObjectARB
  :: MonadIO m
  => m GLhandleARB -- ^ of type @handleARB@.
glCreateProgramObjectARB = liftIO $ dyn196 ptr_glCreateProgramObjectARB

{-# NOINLINE ptr_glCreateProgramObjectARB #-}
ptr_glCreateProgramObjectARB :: FunPtr (IO GLhandleARB)
ptr_glCreateProgramObjectARB = unsafePerformIO $ getCommand "glCreateProgramObjectARB"

-- glCreateProgramPipelines ----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glCreateProgramPipelines.xhtml OpenGL 4.x>.
glCreateProgramPipelines
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @pipelines@.
  -> m ()
glCreateProgramPipelines v1 v2 = liftIO $ dyn193 ptr_glCreateProgramPipelines v1 v2

{-# NOINLINE ptr_glCreateProgramPipelines #-}
ptr_glCreateProgramPipelines :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glCreateProgramPipelines = unsafePerformIO $ getCommand "glCreateProgramPipelines"

-- glCreateQueries -------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glCreateQueries.xhtml OpenGL 4.x>.
glCreateQueries
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @ids@.
  -> m ()
glCreateQueries v1 v2 v3 = liftIO $ dyn197 ptr_glCreateQueries v1 v2 v3

{-# NOINLINE ptr_glCreateQueries #-}
ptr_glCreateQueries :: FunPtr (GLenum -> GLsizei -> Ptr GLuint -> IO ())
ptr_glCreateQueries = unsafePerformIO $ getCommand "glCreateQueries"

-- glCreateRenderbuffers -------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glCreateRenderbuffers.xhtml OpenGL 4.x>.
glCreateRenderbuffers
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @renderbuffers@.
  -> m ()
glCreateRenderbuffers v1 v2 = liftIO $ dyn193 ptr_glCreateRenderbuffers v1 v2

{-# NOINLINE ptr_glCreateRenderbuffers #-}
ptr_glCreateRenderbuffers :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glCreateRenderbuffers = unsafePerformIO $ getCommand "glCreateRenderbuffers"

-- glCreateSamplers ------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glCreateSamplers.xhtml OpenGL 4.x>.
glCreateSamplers
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @samplers@.
  -> m ()
glCreateSamplers v1 v2 = liftIO $ dyn193 ptr_glCreateSamplers v1 v2

{-# NOINLINE ptr_glCreateSamplers #-}
ptr_glCreateSamplers :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glCreateSamplers = unsafePerformIO $ getCommand "glCreateSamplers"

-- glCreateShader --------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glCreateShader.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glCreateShader.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glCreateShader.xhtml OpenGL 4.x>.
glCreateShader
  :: MonadIO m
  => GLenum -- ^ @type@.
  -> m GLuint
glCreateShader v1 = liftIO $ dyn30 ptr_glCreateShader v1

{-# NOINLINE ptr_glCreateShader #-}
ptr_glCreateShader :: FunPtr (GLenum -> IO GLuint)
ptr_glCreateShader = unsafePerformIO $ getCommand "glCreateShader"

-- glCreateShaderObjectARB -----------------------------------------------------

-- | This command is an alias for 'glCreateShader'.
glCreateShaderObjectARB
  :: MonadIO m
  => GLenum -- ^ @shaderType@.
  -> m GLhandleARB -- ^ of type @handleARB@.
glCreateShaderObjectARB v1 = liftIO $ dyn198 ptr_glCreateShaderObjectARB v1

{-# NOINLINE ptr_glCreateShaderObjectARB #-}
ptr_glCreateShaderObjectARB :: FunPtr (GLenum -> IO GLhandleARB)
ptr_glCreateShaderObjectARB = unsafePerformIO $ getCommand "glCreateShaderObjectARB"

-- glCreateShaderProgramEXT ----------------------------------------------------

glCreateShaderProgramEXT
  :: MonadIO m
  => GLenum -- ^ @type@.
  -> Ptr GLchar -- ^ @string@.
  -> m GLuint
glCreateShaderProgramEXT v1 v2 = liftIO $ dyn199 ptr_glCreateShaderProgramEXT v1 v2

{-# NOINLINE ptr_glCreateShaderProgramEXT #-}
ptr_glCreateShaderProgramEXT :: FunPtr (GLenum -> Ptr GLchar -> IO GLuint)
ptr_glCreateShaderProgramEXT = unsafePerformIO $ getCommand "glCreateShaderProgramEXT"

-- glCreateShaderProgramv ------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glCreateShaderProgram.xhtml OpenGL 4.x>.
glCreateShaderProgramv
  :: MonadIO m
  => GLenum -- ^ @type@.
  -> GLsizei -- ^ @count@.
  -> Ptr (Ptr GLchar) -- ^ @strings@ pointing to @count@ elements of type @Ptr GLchar@.
  -> m GLuint
glCreateShaderProgramv v1 v2 v3 = liftIO $ dyn200 ptr_glCreateShaderProgramv v1 v2 v3

{-# NOINLINE ptr_glCreateShaderProgramv #-}
ptr_glCreateShaderProgramv :: FunPtr (GLenum -> GLsizei -> Ptr (Ptr GLchar) -> IO GLuint)
ptr_glCreateShaderProgramv = unsafePerformIO $ getCommand "glCreateShaderProgramv"

-- glCreateShaderProgramvEXT ---------------------------------------------------

glCreateShaderProgramvEXT
  :: MonadIO m
  => GLenum -- ^ @type@.
  -> GLsizei -- ^ @count@.
  -> Ptr (Ptr GLchar) -- ^ @strings@ pointing to @count@ elements of type @Ptr GLchar@.
  -> m GLuint
glCreateShaderProgramvEXT v1 v2 v3 = liftIO $ dyn200 ptr_glCreateShaderProgramvEXT v1 v2 v3

{-# NOINLINE ptr_glCreateShaderProgramvEXT #-}
ptr_glCreateShaderProgramvEXT :: FunPtr (GLenum -> GLsizei -> Ptr (Ptr GLchar) -> IO GLuint)
ptr_glCreateShaderProgramvEXT = unsafePerformIO $ getCommand "glCreateShaderProgramvEXT"

-- glCreateStatesNV ------------------------------------------------------------

glCreateStatesNV
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @states@.
  -> m ()
glCreateStatesNV v1 v2 = liftIO $ dyn193 ptr_glCreateStatesNV v1 v2

{-# NOINLINE ptr_glCreateStatesNV #-}
ptr_glCreateStatesNV :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glCreateStatesNV = unsafePerformIO $ getCommand "glCreateStatesNV"

-- glCreateSyncFromCLeventARB --------------------------------------------------

glCreateSyncFromCLeventARB
  :: MonadIO m
  => Ptr a -- ^ @context@ pointing to elements of type @cl_context@.
  -> Ptr b -- ^ @event@ pointing to elements of type @cl_event@.
  -> GLbitfield -- ^ @flags@.
  -> m GLsync -- ^ of type @sync@.
glCreateSyncFromCLeventARB v1 v2 v3 = liftIO $ dyn201 ptr_glCreateSyncFromCLeventARB v1 v2 v3

{-# NOINLINE ptr_glCreateSyncFromCLeventARB #-}
ptr_glCreateSyncFromCLeventARB :: FunPtr (Ptr a -> Ptr b -> GLbitfield -> IO GLsync)
ptr_glCreateSyncFromCLeventARB = unsafePerformIO $ getCommand "glCreateSyncFromCLeventARB"

-- glCreateTextures ------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glCreateTextures.xhtml OpenGL 4.x>.
glCreateTextures
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @textures@.
  -> m ()
glCreateTextures v1 v2 v3 = liftIO $ dyn197 ptr_glCreateTextures v1 v2 v3

{-# NOINLINE ptr_glCreateTextures #-}
ptr_glCreateTextures :: FunPtr (GLenum -> GLsizei -> Ptr GLuint -> IO ())
ptr_glCreateTextures = unsafePerformIO $ getCommand "glCreateTextures"

-- glCreateTransformFeedbacks --------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glCreateTransformFeedbacks.xhtml OpenGL 4.x>.
glCreateTransformFeedbacks
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @ids@.
  -> m ()
glCreateTransformFeedbacks v1 v2 = liftIO $ dyn193 ptr_glCreateTransformFeedbacks v1 v2

{-# NOINLINE ptr_glCreateTransformFeedbacks #-}
ptr_glCreateTransformFeedbacks :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glCreateTransformFeedbacks = unsafePerformIO $ getCommand "glCreateTransformFeedbacks"

-- glCreateVertexArrays --------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glCreateVertexArrays.xhtml OpenGL 4.x>.
glCreateVertexArrays
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @arrays@.
  -> m ()
glCreateVertexArrays v1 v2 = liftIO $ dyn193 ptr_glCreateVertexArrays v1 v2

{-# NOINLINE ptr_glCreateVertexArrays #-}
ptr_glCreateVertexArrays :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glCreateVertexArrays = unsafePerformIO $ getCommand "glCreateVertexArrays"

-- glCullFace ------------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glCullFace.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glCullFace.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glCullFace.xhtml OpenGL 4.x>.
glCullFace
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [CullFaceMode](Graphics-GL-Groups.html#CullFaceMode).
  -> m ()
glCullFace v1 = liftIO $ dyn4 ptr_glCullFace v1

{-# NOINLINE ptr_glCullFace #-}
ptr_glCullFace :: FunPtr (GLenum -> IO ())
ptr_glCullFace = unsafePerformIO $ getCommand "glCullFace"

-- glCullParameterdvEXT --------------------------------------------------------

glCullParameterdvEXT
  :: MonadIO m
  => GLenum -- ^ @pname@ of type @CullParameterEXT@.
  -> Ptr GLdouble -- ^ @params@ pointing to @4@ elements of type @GLdouble@.
  -> m ()
glCullParameterdvEXT v1 v2 = liftIO $ dyn93 ptr_glCullParameterdvEXT v1 v2

{-# NOINLINE ptr_glCullParameterdvEXT #-}
ptr_glCullParameterdvEXT :: FunPtr (GLenum -> Ptr GLdouble -> IO ())
ptr_glCullParameterdvEXT = unsafePerformIO $ getCommand "glCullParameterdvEXT"

-- glCullParameterfvEXT --------------------------------------------------------

glCullParameterfvEXT
  :: MonadIO m
  => GLenum -- ^ @pname@ of type @CullParameterEXT@.
  -> Ptr GLfloat -- ^ @params@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glCullParameterfvEXT v1 v2 = liftIO $ dyn94 ptr_glCullParameterfvEXT v1 v2

{-# NOINLINE ptr_glCullParameterfvEXT #-}
ptr_glCullParameterfvEXT :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glCullParameterfvEXT = unsafePerformIO $ getCommand "glCullParameterfvEXT"

-- glCurrentPaletteMatrixARB ---------------------------------------------------

glCurrentPaletteMatrixARB
  :: MonadIO m
  => GLint -- ^ @index@.
  -> m ()
glCurrentPaletteMatrixARB v1 = liftIO $ dyn12 ptr_glCurrentPaletteMatrixARB v1

{-# NOINLINE ptr_glCurrentPaletteMatrixARB #-}
ptr_glCurrentPaletteMatrixARB :: FunPtr (GLint -> IO ())
ptr_glCurrentPaletteMatrixARB = unsafePerformIO $ getCommand "glCurrentPaletteMatrixARB"

-- glCurrentPaletteMatrixOES ---------------------------------------------------

glCurrentPaletteMatrixOES
  :: MonadIO m
  => GLuint -- ^ @matrixpaletteindex@.
  -> m ()
glCurrentPaletteMatrixOES v1 = liftIO $ dyn2 ptr_glCurrentPaletteMatrixOES v1

{-# NOINLINE ptr_glCurrentPaletteMatrixOES #-}
ptr_glCurrentPaletteMatrixOES :: FunPtr (GLuint -> IO ())
ptr_glCurrentPaletteMatrixOES = unsafePerformIO $ getCommand "glCurrentPaletteMatrixOES"

-- glDebugMessageCallback ------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glDebugMessageCallback.xhtml OpenGL 4.x>.
glDebugMessageCallback
  :: MonadIO m
  => GLDEBUGPROC -- ^ @callback@.
  -> Ptr a -- ^ @userParam@.
  -> m ()
glDebugMessageCallback v1 v2 = liftIO $ dyn202 ptr_glDebugMessageCallback v1 v2

{-# NOINLINE ptr_glDebugMessageCallback #-}
ptr_glDebugMessageCallback :: FunPtr (GLDEBUGPROC -> Ptr a -> IO ())
ptr_glDebugMessageCallback = unsafePerformIO $ getCommand "glDebugMessageCallback"

-- glDebugMessageCallbackAMD ---------------------------------------------------

glDebugMessageCallbackAMD
  :: MonadIO m
  => GLDEBUGPROCAMD -- ^ @callback@.
  -> Ptr a -- ^ @userParam@.
  -> m ()
glDebugMessageCallbackAMD v1 v2 = liftIO $ dyn203 ptr_glDebugMessageCallbackAMD v1 v2

{-# NOINLINE ptr_glDebugMessageCallbackAMD #-}
ptr_glDebugMessageCallbackAMD :: FunPtr (GLDEBUGPROCAMD -> Ptr a -> IO ())
ptr_glDebugMessageCallbackAMD = unsafePerformIO $ getCommand "glDebugMessageCallbackAMD"

-- glDebugMessageCallbackARB ---------------------------------------------------

-- | This command is an alias for 'glDebugMessageCallback'.
glDebugMessageCallbackARB
  :: MonadIO m
  => GLDEBUGPROCARB -- ^ @callback@.
  -> Ptr a -- ^ @userParam@ pointing to @COMPSIZE(callback)@ elements of type @a@.
  -> m ()
glDebugMessageCallbackARB v1 v2 = liftIO $ dyn204 ptr_glDebugMessageCallbackARB v1 v2

{-# NOINLINE ptr_glDebugMessageCallbackARB #-}
ptr_glDebugMessageCallbackARB :: FunPtr (GLDEBUGPROCARB -> Ptr a -> IO ())
ptr_glDebugMessageCallbackARB = unsafePerformIO $ getCommand "glDebugMessageCallbackARB"

-- glDebugMessageCallbackKHR ---------------------------------------------------

-- | This command is an alias for 'glDebugMessageCallback'.
glDebugMessageCallbackKHR
  :: MonadIO m
  => GLDEBUGPROCKHR -- ^ @callback@.
  -> Ptr a -- ^ @userParam@.
  -> m ()
glDebugMessageCallbackKHR v1 v2 = liftIO $ dyn205 ptr_glDebugMessageCallbackKHR v1 v2

{-# NOINLINE ptr_glDebugMessageCallbackKHR #-}
ptr_glDebugMessageCallbackKHR :: FunPtr (GLDEBUGPROCKHR -> Ptr a -> IO ())
ptr_glDebugMessageCallbackKHR = unsafePerformIO $ getCommand "glDebugMessageCallbackKHR"

-- glDebugMessageControl -------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glDebugMessageControl.xhtml OpenGL 4.x>.
glDebugMessageControl
  :: MonadIO m
  => GLenum -- ^ @source@.
  -> GLenum -- ^ @type@.
  -> GLenum -- ^ @severity@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @ids@ pointing to @count@ elements of type @GLuint@.
  -> GLboolean -- ^ @enabled@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glDebugMessageControl v1 v2 v3 v4 v5 v6 = liftIO $ dyn206 ptr_glDebugMessageControl v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glDebugMessageControl #-}
ptr_glDebugMessageControl :: FunPtr (GLenum -> GLenum -> GLenum -> GLsizei -> Ptr GLuint -> GLboolean -> IO ())
ptr_glDebugMessageControl = unsafePerformIO $ getCommand "glDebugMessageControl"

-- glDebugMessageControlARB ----------------------------------------------------

-- | This command is an alias for 'glDebugMessageControl'.
glDebugMessageControlARB
  :: MonadIO m
  => GLenum -- ^ @source@.
  -> GLenum -- ^ @type@.
  -> GLenum -- ^ @severity@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @ids@ pointing to @count@ elements of type @GLuint@.
  -> GLboolean -- ^ @enabled@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glDebugMessageControlARB v1 v2 v3 v4 v5 v6 = liftIO $ dyn206 ptr_glDebugMessageControlARB v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glDebugMessageControlARB #-}
ptr_glDebugMessageControlARB :: FunPtr (GLenum -> GLenum -> GLenum -> GLsizei -> Ptr GLuint -> GLboolean -> IO ())
ptr_glDebugMessageControlARB = unsafePerformIO $ getCommand "glDebugMessageControlARB"

-- glDebugMessageControlKHR ----------------------------------------------------

-- | This command is an alias for 'glDebugMessageControl'.
glDebugMessageControlKHR
  :: MonadIO m
  => GLenum -- ^ @source@.
  -> GLenum -- ^ @type@.
  -> GLenum -- ^ @severity@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @ids@.
  -> GLboolean -- ^ @enabled@.
  -> m ()
glDebugMessageControlKHR v1 v2 v3 v4 v5 v6 = liftIO $ dyn206 ptr_glDebugMessageControlKHR v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glDebugMessageControlKHR #-}
ptr_glDebugMessageControlKHR :: FunPtr (GLenum -> GLenum -> GLenum -> GLsizei -> Ptr GLuint -> GLboolean -> IO ())
ptr_glDebugMessageControlKHR = unsafePerformIO $ getCommand "glDebugMessageControlKHR"

-- glDebugMessageEnableAMD -----------------------------------------------------

glDebugMessageEnableAMD
  :: MonadIO m
  => GLenum -- ^ @category@.
  -> GLenum -- ^ @severity@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @ids@ pointing to @count@ elements of type @GLuint@.
  -> GLboolean -- ^ @enabled@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glDebugMessageEnableAMD v1 v2 v3 v4 v5 = liftIO $ dyn207 ptr_glDebugMessageEnableAMD v1 v2 v3 v4 v5

{-# NOINLINE ptr_glDebugMessageEnableAMD #-}
ptr_glDebugMessageEnableAMD :: FunPtr (GLenum -> GLenum -> GLsizei -> Ptr GLuint -> GLboolean -> IO ())
ptr_glDebugMessageEnableAMD = unsafePerformIO $ getCommand "glDebugMessageEnableAMD"

-- glDebugMessageInsert --------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glDebugMessageInsert.xhtml OpenGL 4.x>.
glDebugMessageInsert
  :: MonadIO m
  => GLenum -- ^ @source@.
  -> GLenum -- ^ @type@.
  -> GLuint -- ^ @id@.
  -> GLenum -- ^ @severity@.
  -> GLsizei -- ^ @length@.
  -> Ptr GLchar -- ^ @buf@ pointing to @COMPSIZE(buf,length)@ elements of type @GLchar@.
  -> m ()
glDebugMessageInsert v1 v2 v3 v4 v5 v6 = liftIO $ dyn208 ptr_glDebugMessageInsert v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glDebugMessageInsert #-}
ptr_glDebugMessageInsert :: FunPtr (GLenum -> GLenum -> GLuint -> GLenum -> GLsizei -> Ptr GLchar -> IO ())
ptr_glDebugMessageInsert = unsafePerformIO $ getCommand "glDebugMessageInsert"

-- glDebugMessageInsertAMD -----------------------------------------------------

glDebugMessageInsertAMD
  :: MonadIO m
  => GLenum -- ^ @category@.
  -> GLenum -- ^ @severity@.
  -> GLuint -- ^ @id@.
  -> GLsizei -- ^ @length@.
  -> Ptr GLchar -- ^ @buf@ pointing to @length@ elements of type @GLchar@.
  -> m ()
glDebugMessageInsertAMD v1 v2 v3 v4 v5 = liftIO $ dyn209 ptr_glDebugMessageInsertAMD v1 v2 v3 v4 v5

{-# NOINLINE ptr_glDebugMessageInsertAMD #-}
ptr_glDebugMessageInsertAMD :: FunPtr (GLenum -> GLenum -> GLuint -> GLsizei -> Ptr GLchar -> IO ())
ptr_glDebugMessageInsertAMD = unsafePerformIO $ getCommand "glDebugMessageInsertAMD"

-- glDebugMessageInsertARB -----------------------------------------------------

-- | This command is an alias for 'glDebugMessageInsert'.
glDebugMessageInsertARB
  :: MonadIO m
  => GLenum -- ^ @source@.
  -> GLenum -- ^ @type@.
  -> GLuint -- ^ @id@.
  -> GLenum -- ^ @severity@.
  -> GLsizei -- ^ @length@.
  -> Ptr GLchar -- ^ @buf@ pointing to @length@ elements of type @GLchar@.
  -> m ()
glDebugMessageInsertARB v1 v2 v3 v4 v5 v6 = liftIO $ dyn208 ptr_glDebugMessageInsertARB v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glDebugMessageInsertARB #-}
ptr_glDebugMessageInsertARB :: FunPtr (GLenum -> GLenum -> GLuint -> GLenum -> GLsizei -> Ptr GLchar -> IO ())
ptr_glDebugMessageInsertARB = unsafePerformIO $ getCommand "glDebugMessageInsertARB"

-- glDebugMessageInsertKHR -----------------------------------------------------

-- | This command is an alias for 'glDebugMessageInsert'.
glDebugMessageInsertKHR
  :: MonadIO m
  => GLenum -- ^ @source@.
  -> GLenum -- ^ @type@.
  -> GLuint -- ^ @id@.
  -> GLenum -- ^ @severity@.
  -> GLsizei -- ^ @length@.
  -> Ptr GLchar -- ^ @buf@.
  -> m ()
glDebugMessageInsertKHR v1 v2 v3 v4 v5 v6 = liftIO $ dyn208 ptr_glDebugMessageInsertKHR v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glDebugMessageInsertKHR #-}
ptr_glDebugMessageInsertKHR :: FunPtr (GLenum -> GLenum -> GLuint -> GLenum -> GLsizei -> Ptr GLchar -> IO ())
ptr_glDebugMessageInsertKHR = unsafePerformIO $ getCommand "glDebugMessageInsertKHR"

-- glDeformSGIX ----------------------------------------------------------------

glDeformSGIX
  :: MonadIO m
  => GLbitfield -- ^ @mask@ of type [FfdMaskSGIX](Graphics-GL-Groups.html#FfdMaskSGIX).
  -> m ()
glDeformSGIX v1 = liftIO $ dyn69 ptr_glDeformSGIX v1

{-# NOINLINE ptr_glDeformSGIX #-}
ptr_glDeformSGIX :: FunPtr (GLbitfield -> IO ())
ptr_glDeformSGIX = unsafePerformIO $ getCommand "glDeformSGIX"

