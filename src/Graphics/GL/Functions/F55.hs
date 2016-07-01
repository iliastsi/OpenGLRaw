--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F55
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

module Graphics.GL.Functions.F55 (
  glReplacementCodeuiTexCoord2fVertex3fSUN,
  glReplacementCodeuiTexCoord2fVertex3fvSUN,
  glReplacementCodeuiVertex3fSUN,
  glReplacementCodeuiVertex3fvSUN,
  glReplacementCodeuivSUN,
  glReplacementCodeusSUN,
  glReplacementCodeusvSUN,
  glRequestResidentProgramsNV,
  glResetHistogram,
  glResetHistogramEXT,
  glResetMinmax,
  glResetMinmaxEXT,
  glResizeBuffersMESA,
  glResolveDepthValuesNV,
  glResolveMultisampleFramebufferAPPLE,
  glResumeTransformFeedback,
  glResumeTransformFeedbackNV,
  glRotated,
  glRotatef,
  glRotatex,
  glRotatexOES,
  glSampleCoverage,
  glSampleCoverageARB,
  glSampleCoveragex,
  glSampleCoveragexOES,
  glSampleMapATI,
  glSampleMaskEXT,
  glSampleMaskIndexedNV,
  glSampleMaskSGIS,
  glSampleMaski,
  glSamplePatternEXT,
  glSamplePatternSGIS,
  glSamplerParameterIiv,
  glSamplerParameterIivEXT,
  glSamplerParameterIivOES,
  glSamplerParameterIuiv,
  glSamplerParameterIuivEXT,
  glSamplerParameterIuivOES,
  glSamplerParameterf,
  glSamplerParameterfv
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

-- glReplacementCodeuiTexCoord2fVertex3fSUN ------------------------------------

glReplacementCodeuiTexCoord2fVertex3fSUN
  :: MonadIO m
  => GLuint -- ^ @rc@ of type @ReplacementCodeSUN@.
  -> GLfloat -- ^ @s@.
  -> GLfloat -- ^ @t@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> m ()
glReplacementCodeuiTexCoord2fVertex3fSUN v1 v2 v3 v4 v5 v6 = liftIO $ dyn691 ptr_glReplacementCodeuiTexCoord2fVertex3fSUN v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glReplacementCodeuiTexCoord2fVertex3fSUN #-}
ptr_glReplacementCodeuiTexCoord2fVertex3fSUN :: FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glReplacementCodeuiTexCoord2fVertex3fSUN = unsafePerformIO $ getCommand "glReplacementCodeuiTexCoord2fVertex3fSUN"

-- glReplacementCodeuiTexCoord2fVertex3fvSUN -----------------------------------

glReplacementCodeuiTexCoord2fVertex3fvSUN
  :: MonadIO m
  => Ptr GLuint -- ^ @rc@ pointing to @1@ element of type @ReplacementCodeSUN@.
  -> Ptr GLfloat -- ^ @tc@ pointing to @2@ elements of type @GLfloat@.
  -> Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @GLfloat@.
  -> m ()
glReplacementCodeuiTexCoord2fVertex3fvSUN v1 v2 v3 = liftIO $ dyn683 ptr_glReplacementCodeuiTexCoord2fVertex3fvSUN v1 v2 v3

{-# NOINLINE ptr_glReplacementCodeuiTexCoord2fVertex3fvSUN #-}
ptr_glReplacementCodeuiTexCoord2fVertex3fvSUN :: FunPtr (Ptr GLuint -> Ptr GLfloat -> Ptr GLfloat -> IO ())
ptr_glReplacementCodeuiTexCoord2fVertex3fvSUN = unsafePerformIO $ getCommand "glReplacementCodeuiTexCoord2fVertex3fvSUN"

-- glReplacementCodeuiVertex3fSUN ----------------------------------------------

glReplacementCodeuiVertex3fSUN
  :: MonadIO m
  => GLuint -- ^ @rc@ of type @ReplacementCodeSUN@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> m ()
glReplacementCodeuiVertex3fSUN v1 v2 v3 v4 = liftIO $ dyn692 ptr_glReplacementCodeuiVertex3fSUN v1 v2 v3 v4

{-# NOINLINE ptr_glReplacementCodeuiVertex3fSUN #-}
ptr_glReplacementCodeuiVertex3fSUN :: FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glReplacementCodeuiVertex3fSUN = unsafePerformIO $ getCommand "glReplacementCodeuiVertex3fSUN"

-- glReplacementCodeuiVertex3fvSUN ---------------------------------------------

glReplacementCodeuiVertex3fvSUN
  :: MonadIO m
  => Ptr GLuint -- ^ @rc@ pointing to @1@ element of type @ReplacementCodeSUN@.
  -> Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @GLfloat@.
  -> m ()
glReplacementCodeuiVertex3fvSUN v1 v2 = liftIO $ dyn693 ptr_glReplacementCodeuiVertex3fvSUN v1 v2

{-# NOINLINE ptr_glReplacementCodeuiVertex3fvSUN #-}
ptr_glReplacementCodeuiVertex3fvSUN :: FunPtr (Ptr GLuint -> Ptr GLfloat -> IO ())
ptr_glReplacementCodeuiVertex3fvSUN = unsafePerformIO $ getCommand "glReplacementCodeuiVertex3fvSUN"

-- glReplacementCodeuivSUN -----------------------------------------------------

glReplacementCodeuivSUN
  :: MonadIO m
  => Ptr GLuint -- ^ @code@ pointing to @COMPSIZE()@ elements of type @GLuint@.
  -> m ()
glReplacementCodeuivSUN v1 = liftIO $ dyn103 ptr_glReplacementCodeuivSUN v1

{-# NOINLINE ptr_glReplacementCodeuivSUN #-}
ptr_glReplacementCodeuivSUN :: FunPtr (Ptr GLuint -> IO ())
ptr_glReplacementCodeuivSUN = unsafePerformIO $ getCommand "glReplacementCodeuivSUN"

-- glReplacementCodeusSUN ------------------------------------------------------

glReplacementCodeusSUN
  :: MonadIO m
  => GLushort -- ^ @code@.
  -> m ()
glReplacementCodeusSUN v1 = liftIO $ dyn465 ptr_glReplacementCodeusSUN v1

{-# NOINLINE ptr_glReplacementCodeusSUN #-}
ptr_glReplacementCodeusSUN :: FunPtr (GLushort -> IO ())
ptr_glReplacementCodeusSUN = unsafePerformIO $ getCommand "glReplacementCodeusSUN"

-- glReplacementCodeusvSUN -----------------------------------------------------

glReplacementCodeusvSUN
  :: MonadIO m
  => Ptr GLushort -- ^ @code@ pointing to @COMPSIZE()@ elements of type @GLushort@.
  -> m ()
glReplacementCodeusvSUN v1 = liftIO $ dyn105 ptr_glReplacementCodeusvSUN v1

{-# NOINLINE ptr_glReplacementCodeusvSUN #-}
ptr_glReplacementCodeusvSUN :: FunPtr (Ptr GLushort -> IO ())
ptr_glReplacementCodeusvSUN = unsafePerformIO $ getCommand "glReplacementCodeusvSUN"

-- glRequestResidentProgramsNV -------------------------------------------------

glRequestResidentProgramsNV
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @programs@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glRequestResidentProgramsNV v1 v2 = liftIO $ dyn193 ptr_glRequestResidentProgramsNV v1 v2

{-# NOINLINE ptr_glRequestResidentProgramsNV #-}
ptr_glRequestResidentProgramsNV :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glRequestResidentProgramsNV = unsafePerformIO $ getCommand "glRequestResidentProgramsNV"

-- glResetHistogram ------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glResetHistogram.xml OpenGL 2.x>.
glResetHistogram
  :: MonadIO m
  => GLenum -- ^ @target@ of type @HistogramTarget@.
  -> m ()
glResetHistogram v1 = liftIO $ dyn4 ptr_glResetHistogram v1

{-# NOINLINE ptr_glResetHistogram #-}
ptr_glResetHistogram :: FunPtr (GLenum -> IO ())
ptr_glResetHistogram = unsafePerformIO $ getCommand "glResetHistogram"

-- glResetHistogramEXT ---------------------------------------------------------

-- | This command is an alias for 'glResetHistogram'.
glResetHistogramEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [HistogramTargetEXT](Graphics-GL-Groups.html#HistogramTargetEXT).
  -> m ()
glResetHistogramEXT v1 = liftIO $ dyn4 ptr_glResetHistogramEXT v1

{-# NOINLINE ptr_glResetHistogramEXT #-}
ptr_glResetHistogramEXT :: FunPtr (GLenum -> IO ())
ptr_glResetHistogramEXT = unsafePerformIO $ getCommand "glResetHistogramEXT"

-- glResetMinmax ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glResetMinmax.xml OpenGL 2.x>.
glResetMinmax
  :: MonadIO m
  => GLenum -- ^ @target@ of type @MinmaxTarget@.
  -> m ()
glResetMinmax v1 = liftIO $ dyn4 ptr_glResetMinmax v1

{-# NOINLINE ptr_glResetMinmax #-}
ptr_glResetMinmax :: FunPtr (GLenum -> IO ())
ptr_glResetMinmax = unsafePerformIO $ getCommand "glResetMinmax"

-- glResetMinmaxEXT ------------------------------------------------------------

-- | This command is an alias for 'glResetMinmax'.
glResetMinmaxEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [MinmaxTargetEXT](Graphics-GL-Groups.html#MinmaxTargetEXT).
  -> m ()
glResetMinmaxEXT v1 = liftIO $ dyn4 ptr_glResetMinmaxEXT v1

{-# NOINLINE ptr_glResetMinmaxEXT #-}
ptr_glResetMinmaxEXT :: FunPtr (GLenum -> IO ())
ptr_glResetMinmaxEXT = unsafePerformIO $ getCommand "glResetMinmaxEXT"

-- glResizeBuffersMESA ---------------------------------------------------------

glResizeBuffersMESA
  :: MonadIO m
  => m ()
glResizeBuffersMESA = liftIO $ dyn10 ptr_glResizeBuffersMESA

{-# NOINLINE ptr_glResizeBuffersMESA #-}
ptr_glResizeBuffersMESA :: FunPtr (IO ())
ptr_glResizeBuffersMESA = unsafePerformIO $ getCommand "glResizeBuffersMESA"

-- glResolveDepthValuesNV ------------------------------------------------------

glResolveDepthValuesNV
  :: MonadIO m
  => m ()
glResolveDepthValuesNV = liftIO $ dyn10 ptr_glResolveDepthValuesNV

{-# NOINLINE ptr_glResolveDepthValuesNV #-}
ptr_glResolveDepthValuesNV :: FunPtr (IO ())
ptr_glResolveDepthValuesNV = unsafePerformIO $ getCommand "glResolveDepthValuesNV"

-- glResolveMultisampleFramebufferAPPLE ----------------------------------------

glResolveMultisampleFramebufferAPPLE
  :: MonadIO m
  => m ()
glResolveMultisampleFramebufferAPPLE = liftIO $ dyn10 ptr_glResolveMultisampleFramebufferAPPLE

{-# NOINLINE ptr_glResolveMultisampleFramebufferAPPLE #-}
ptr_glResolveMultisampleFramebufferAPPLE :: FunPtr (IO ())
ptr_glResolveMultisampleFramebufferAPPLE = unsafePerformIO $ getCommand "glResolveMultisampleFramebufferAPPLE"

-- glResumeTransformFeedback ---------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glResumeTransformFeedback.xhtml OpenGL 4.x>.
glResumeTransformFeedback
  :: MonadIO m
  => m ()
glResumeTransformFeedback = liftIO $ dyn10 ptr_glResumeTransformFeedback

{-# NOINLINE ptr_glResumeTransformFeedback #-}
ptr_glResumeTransformFeedback :: FunPtr (IO ())
ptr_glResumeTransformFeedback = unsafePerformIO $ getCommand "glResumeTransformFeedback"

-- glResumeTransformFeedbackNV -------------------------------------------------

-- | This command is an alias for 'glResumeTransformFeedback'.
glResumeTransformFeedbackNV
  :: MonadIO m
  => m ()
glResumeTransformFeedbackNV = liftIO $ dyn10 ptr_glResumeTransformFeedbackNV

{-# NOINLINE ptr_glResumeTransformFeedbackNV #-}
ptr_glResumeTransformFeedbackNV :: FunPtr (IO ())
ptr_glResumeTransformFeedbackNV = unsafePerformIO $ getCommand "glResumeTransformFeedbackNV"

-- glRotated -------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRotate.xml OpenGL 2.x>.
glRotated
  :: MonadIO m
  => GLdouble -- ^ @angle@.
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> GLdouble -- ^ @z@.
  -> m ()
glRotated v1 v2 v3 v4 = liftIO $ dyn109 ptr_glRotated v1 v2 v3 v4

{-# NOINLINE ptr_glRotated #-}
ptr_glRotated :: FunPtr (GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glRotated = unsafePerformIO $ getCommand "glRotated"

-- glRotatef -------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRotate.xml OpenGL 2.x>.
glRotatef
  :: MonadIO m
  => GLfloat -- ^ @angle@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> m ()
glRotatef v1 v2 v3 v4 = liftIO $ dyn49 ptr_glRotatef v1 v2 v3 v4

{-# NOINLINE ptr_glRotatef #-}
ptr_glRotatef :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glRotatef = unsafePerformIO $ getCommand "glRotatef"

-- glRotatex -------------------------------------------------------------------

glRotatex
  :: MonadIO m
  => GLfixed -- ^ @angle@.
  -> GLfixed -- ^ @x@.
  -> GLfixed -- ^ @y@.
  -> GLfixed -- ^ @z@.
  -> m ()
glRotatex v1 v2 v3 v4 = liftIO $ dyn50 ptr_glRotatex v1 v2 v3 v4

{-# NOINLINE ptr_glRotatex #-}
ptr_glRotatex :: FunPtr (GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ())
ptr_glRotatex = unsafePerformIO $ getCommand "glRotatex"

-- glRotatexOES ----------------------------------------------------------------

glRotatexOES
  :: MonadIO m
  => GLfixed -- ^ @angle@.
  -> GLfixed -- ^ @x@.
  -> GLfixed -- ^ @y@.
  -> GLfixed -- ^ @z@.
  -> m ()
glRotatexOES v1 v2 v3 v4 = liftIO $ dyn50 ptr_glRotatexOES v1 v2 v3 v4

{-# NOINLINE ptr_glRotatexOES #-}
ptr_glRotatexOES :: FunPtr (GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ())
ptr_glRotatexOES = unsafePerformIO $ getCommand "glRotatexOES"

-- glSampleCoverage ------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glSampleCoverage.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glSampleCoverage.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glSampleCoverage.xhtml OpenGL 4.x>.
glSampleCoverage
  :: MonadIO m
  => GLfloat -- ^ @value@.
  -> GLboolean -- ^ @invert@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glSampleCoverage v1 v2 = liftIO $ dyn694 ptr_glSampleCoverage v1 v2

{-# NOINLINE ptr_glSampleCoverage #-}
ptr_glSampleCoverage :: FunPtr (GLfloat -> GLboolean -> IO ())
ptr_glSampleCoverage = unsafePerformIO $ getCommand "glSampleCoverage"

-- glSampleCoverageARB ---------------------------------------------------------

-- | This command is an alias for 'glSampleCoverage'.
glSampleCoverageARB
  :: MonadIO m
  => GLfloat -- ^ @value@.
  -> GLboolean -- ^ @invert@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glSampleCoverageARB v1 v2 = liftIO $ dyn694 ptr_glSampleCoverageARB v1 v2

{-# NOINLINE ptr_glSampleCoverageARB #-}
ptr_glSampleCoverageARB :: FunPtr (GLfloat -> GLboolean -> IO ())
ptr_glSampleCoverageARB = unsafePerformIO $ getCommand "glSampleCoverageARB"

-- glSampleCoveragex -----------------------------------------------------------

glSampleCoveragex
  :: MonadIO m
  => GLclampx -- ^ @value@.
  -> GLboolean -- ^ @invert@.
  -> m ()
glSampleCoveragex v1 v2 = liftIO $ dyn695 ptr_glSampleCoveragex v1 v2

{-# NOINLINE ptr_glSampleCoveragex #-}
ptr_glSampleCoveragex :: FunPtr (GLclampx -> GLboolean -> IO ())
ptr_glSampleCoveragex = unsafePerformIO $ getCommand "glSampleCoveragex"

-- glSampleCoveragexOES --------------------------------------------------------

glSampleCoveragexOES
  :: MonadIO m
  => GLclampx -- ^ @value@.
  -> GLboolean -- ^ @invert@.
  -> m ()
glSampleCoveragexOES v1 v2 = liftIO $ dyn695 ptr_glSampleCoveragexOES v1 v2

{-# NOINLINE ptr_glSampleCoveragexOES #-}
ptr_glSampleCoveragexOES :: FunPtr (GLclampx -> GLboolean -> IO ())
ptr_glSampleCoveragexOES = unsafePerformIO $ getCommand "glSampleCoveragexOES"

-- glSampleMapATI --------------------------------------------------------------

glSampleMapATI
  :: MonadIO m
  => GLuint -- ^ @dst@.
  -> GLuint -- ^ @interp@.
  -> GLenum -- ^ @swizzle@ of type @SwizzleOpATI@.
  -> m ()
glSampleMapATI v1 v2 v3 = liftIO $ dyn596 ptr_glSampleMapATI v1 v2 v3

{-# NOINLINE ptr_glSampleMapATI #-}
ptr_glSampleMapATI :: FunPtr (GLuint -> GLuint -> GLenum -> IO ())
ptr_glSampleMapATI = unsafePerformIO $ getCommand "glSampleMapATI"

-- glSampleMaskEXT -------------------------------------------------------------

glSampleMaskEXT
  :: MonadIO m
  => GLclampf -- ^ @value@ of type @ClampedFloat32@.
  -> GLboolean -- ^ @invert@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glSampleMaskEXT v1 v2 = liftIO $ dyn696 ptr_glSampleMaskEXT v1 v2

{-# NOINLINE ptr_glSampleMaskEXT #-}
ptr_glSampleMaskEXT :: FunPtr (GLclampf -> GLboolean -> IO ())
ptr_glSampleMaskEXT = unsafePerformIO $ getCommand "glSampleMaskEXT"

-- glSampleMaskIndexedNV -------------------------------------------------------

glSampleMaskIndexedNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLbitfield -- ^ @mask@ of type @SampleMaskNV@.
  -> m ()
glSampleMaskIndexedNV v1 v2 = liftIO $ dyn697 ptr_glSampleMaskIndexedNV v1 v2

{-# NOINLINE ptr_glSampleMaskIndexedNV #-}
ptr_glSampleMaskIndexedNV :: FunPtr (GLuint -> GLbitfield -> IO ())
ptr_glSampleMaskIndexedNV = unsafePerformIO $ getCommand "glSampleMaskIndexedNV"

-- glSampleMaskSGIS ------------------------------------------------------------

-- | This command is an alias for 'glSampleMaskEXT'.
glSampleMaskSGIS
  :: MonadIO m
  => GLclampf -- ^ @value@ of type @ClampedFloat32@.
  -> GLboolean -- ^ @invert@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glSampleMaskSGIS v1 v2 = liftIO $ dyn696 ptr_glSampleMaskSGIS v1 v2

{-# NOINLINE ptr_glSampleMaskSGIS #-}
ptr_glSampleMaskSGIS :: FunPtr (GLclampf -> GLboolean -> IO ())
ptr_glSampleMaskSGIS = unsafePerformIO $ getCommand "glSampleMaskSGIS"

-- glSampleMaski ---------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glSampleMaski.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glSampleMaski.xhtml OpenGL 4.x>.
glSampleMaski
  :: MonadIO m
  => GLuint -- ^ @maskNumber@.
  -> GLbitfield -- ^ @mask@.
  -> m ()
glSampleMaski v1 v2 = liftIO $ dyn697 ptr_glSampleMaski v1 v2

{-# NOINLINE ptr_glSampleMaski #-}
ptr_glSampleMaski :: FunPtr (GLuint -> GLbitfield -> IO ())
ptr_glSampleMaski = unsafePerformIO $ getCommand "glSampleMaski"

-- glSamplePatternEXT ----------------------------------------------------------

glSamplePatternEXT
  :: MonadIO m
  => GLenum -- ^ @pattern@ of type @SamplePatternEXT@.
  -> m ()
glSamplePatternEXT v1 = liftIO $ dyn4 ptr_glSamplePatternEXT v1

{-# NOINLINE ptr_glSamplePatternEXT #-}
ptr_glSamplePatternEXT :: FunPtr (GLenum -> IO ())
ptr_glSamplePatternEXT = unsafePerformIO $ getCommand "glSamplePatternEXT"

-- glSamplePatternSGIS ---------------------------------------------------------

-- | This command is an alias for 'glSamplePatternEXT'.
glSamplePatternSGIS
  :: MonadIO m
  => GLenum -- ^ @pattern@ of type [SamplePatternSGIS](Graphics-GL-Groups.html#SamplePatternSGIS).
  -> m ()
glSamplePatternSGIS v1 = liftIO $ dyn4 ptr_glSamplePatternSGIS v1

{-# NOINLINE ptr_glSamplePatternSGIS #-}
ptr_glSamplePatternSGIS :: FunPtr (GLenum -> IO ())
ptr_glSamplePatternSGIS = unsafePerformIO $ getCommand "glSamplePatternSGIS"

-- glSamplerParameterIiv -------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glSamplerParameter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glSamplerParameter.xhtml OpenGL 4.x>.
glSamplerParameterIiv
  :: MonadIO m
  => GLuint -- ^ @sampler@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @param@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glSamplerParameterIiv v1 v2 v3 = liftIO $ dyn334 ptr_glSamplerParameterIiv v1 v2 v3

{-# NOINLINE ptr_glSamplerParameterIiv #-}
ptr_glSamplerParameterIiv :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glSamplerParameterIiv = unsafePerformIO $ getCommand "glSamplerParameterIiv"

-- glSamplerParameterIivEXT ----------------------------------------------------

-- | This command is an alias for 'glSamplerParameterIiv'.
glSamplerParameterIivEXT
  :: MonadIO m
  => GLuint -- ^ @sampler@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @param@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glSamplerParameterIivEXT v1 v2 v3 = liftIO $ dyn334 ptr_glSamplerParameterIivEXT v1 v2 v3

{-# NOINLINE ptr_glSamplerParameterIivEXT #-}
ptr_glSamplerParameterIivEXT :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glSamplerParameterIivEXT = unsafePerformIO $ getCommand "glSamplerParameterIivEXT"

-- glSamplerParameterIivOES ----------------------------------------------------

-- | This command is an alias for 'glSamplerParameterIiv'.
glSamplerParameterIivOES
  :: MonadIO m
  => GLuint -- ^ @sampler@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @param@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glSamplerParameterIivOES v1 v2 v3 = liftIO $ dyn334 ptr_glSamplerParameterIivOES v1 v2 v3

{-# NOINLINE ptr_glSamplerParameterIivOES #-}
ptr_glSamplerParameterIivOES :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glSamplerParameterIivOES = unsafePerformIO $ getCommand "glSamplerParameterIivOES"

-- glSamplerParameterIuiv ------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glSamplerParameter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glSamplerParameter.xhtml OpenGL 4.x>.
glSamplerParameterIuiv
  :: MonadIO m
  => GLuint -- ^ @sampler@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLuint -- ^ @param@ pointing to @COMPSIZE(pname)@ elements of type @GLuint@.
  -> m ()
glSamplerParameterIuiv v1 v2 v3 = liftIO $ dyn375 ptr_glSamplerParameterIuiv v1 v2 v3

{-# NOINLINE ptr_glSamplerParameterIuiv #-}
ptr_glSamplerParameterIuiv :: FunPtr (GLuint -> GLenum -> Ptr GLuint -> IO ())
ptr_glSamplerParameterIuiv = unsafePerformIO $ getCommand "glSamplerParameterIuiv"

-- glSamplerParameterIuivEXT ---------------------------------------------------

-- | This command is an alias for 'glSamplerParameterIuiv'.
glSamplerParameterIuivEXT
  :: MonadIO m
  => GLuint -- ^ @sampler@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLuint -- ^ @param@ pointing to @COMPSIZE(pname)@ elements of type @GLuint@.
  -> m ()
glSamplerParameterIuivEXT v1 v2 v3 = liftIO $ dyn375 ptr_glSamplerParameterIuivEXT v1 v2 v3

{-# NOINLINE ptr_glSamplerParameterIuivEXT #-}
ptr_glSamplerParameterIuivEXT :: FunPtr (GLuint -> GLenum -> Ptr GLuint -> IO ())
ptr_glSamplerParameterIuivEXT = unsafePerformIO $ getCommand "glSamplerParameterIuivEXT"

-- glSamplerParameterIuivOES ---------------------------------------------------

-- | This command is an alias for 'glSamplerParameterIuiv'.
glSamplerParameterIuivOES
  :: MonadIO m
  => GLuint -- ^ @sampler@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLuint -- ^ @param@ pointing to @COMPSIZE(pname)@ elements of type @GLuint@.
  -> m ()
glSamplerParameterIuivOES v1 v2 v3 = liftIO $ dyn375 ptr_glSamplerParameterIuivOES v1 v2 v3

{-# NOINLINE ptr_glSamplerParameterIuivOES #-}
ptr_glSamplerParameterIuivOES :: FunPtr (GLuint -> GLenum -> Ptr GLuint -> IO ())
ptr_glSamplerParameterIuivOES = unsafePerformIO $ getCommand "glSamplerParameterIuivOES"

-- glSamplerParameterf ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glSamplerParameter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glSamplerParameter.xhtml OpenGL 4.x>.
glSamplerParameterf
  :: MonadIO m
  => GLuint -- ^ @sampler@.
  -> GLenum -- ^ @pname@.
  -> GLfloat -- ^ @param@.
  -> m ()
glSamplerParameterf v1 v2 v3 = liftIO $ dyn487 ptr_glSamplerParameterf v1 v2 v3

{-# NOINLINE ptr_glSamplerParameterf #-}
ptr_glSamplerParameterf :: FunPtr (GLuint -> GLenum -> GLfloat -> IO ())
ptr_glSamplerParameterf = unsafePerformIO $ getCommand "glSamplerParameterf"

-- glSamplerParameterfv --------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glSamplerParameter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glSamplerParameter.xhtml OpenGL 4.x>.
glSamplerParameterfv
  :: MonadIO m
  => GLuint -- ^ @sampler@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLfloat -- ^ @param@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glSamplerParameterfv v1 v2 v3 = liftIO $ dyn349 ptr_glSamplerParameterfv v1 v2 v3

{-# NOINLINE ptr_glSamplerParameterfv #-}
ptr_glSamplerParameterfv :: FunPtr (GLuint -> GLenum -> Ptr GLfloat -> IO ())
ptr_glSamplerParameterfv = unsafePerformIO $ getCommand "glSamplerParameterfv"

