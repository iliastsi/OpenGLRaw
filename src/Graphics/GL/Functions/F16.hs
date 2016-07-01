--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F16
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

module Graphics.GL.Functions.F16 (
  glEnableIndexedEXT,
  glEnableVariantClientStateEXT,
  glEnableVertexArrayAttrib,
  glEnableVertexArrayAttribEXT,
  glEnableVertexArrayEXT,
  glEnableVertexAttribAPPLE,
  glEnableVertexAttribArray,
  glEnableVertexAttribArrayARB,
  glEnablei,
  glEnableiEXT,
  glEnableiNV,
  glEnableiOES,
  glEnd,
  glEndConditionalRender,
  glEndConditionalRenderNV,
  glEndConditionalRenderNVX,
  glEndFragmentShaderATI,
  glEndList,
  glEndOcclusionQueryNV,
  glEndPerfMonitorAMD,
  glEndPerfQueryINTEL,
  glEndQuery,
  glEndQueryARB,
  glEndQueryEXT,
  glEndQueryIndexed,
  glEndTilingQCOM,
  glEndTransformFeedback,
  glEndTransformFeedbackEXT,
  glEndTransformFeedbackNV,
  glEndVertexShaderEXT,
  glEndVideoCaptureNV,
  glEvalCoord1d,
  glEvalCoord1dv,
  glEvalCoord1f,
  glEvalCoord1fv,
  glEvalCoord1xOES,
  glEvalCoord1xvOES,
  glEvalCoord2d,
  glEvalCoord2dv,
  glEvalCoord2f
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

-- glEnableIndexedEXT ----------------------------------------------------------

-- | This command is an alias for 'glEnablei'.
glEnableIndexedEXT
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLuint -- ^ @index@.
  -> m ()
glEnableIndexedEXT v1 v2 = liftIO $ dyn16 ptr_glEnableIndexedEXT v1 v2

{-# NOINLINE ptr_glEnableIndexedEXT #-}
ptr_glEnableIndexedEXT :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glEnableIndexedEXT = unsafePerformIO $ getCommand "glEnableIndexedEXT"

-- glEnableVariantClientStateEXT -----------------------------------------------

glEnableVariantClientStateEXT
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> m ()
glEnableVariantClientStateEXT v1 = liftIO $ dyn2 ptr_glEnableVariantClientStateEXT v1

{-# NOINLINE ptr_glEnableVariantClientStateEXT #-}
ptr_glEnableVariantClientStateEXT :: FunPtr (GLuint -> IO ())
ptr_glEnableVariantClientStateEXT = unsafePerformIO $ getCommand "glEnableVariantClientStateEXT"

-- glEnableVertexArrayAttrib ---------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glEnableVertexAttribArray.xhtml OpenGL 4.x>.
glEnableVertexArrayAttrib
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @index@.
  -> m ()
glEnableVertexArrayAttrib v1 v2 = liftIO $ dyn3 ptr_glEnableVertexArrayAttrib v1 v2

{-# NOINLINE ptr_glEnableVertexArrayAttrib #-}
ptr_glEnableVertexArrayAttrib :: FunPtr (GLuint -> GLuint -> IO ())
ptr_glEnableVertexArrayAttrib = unsafePerformIO $ getCommand "glEnableVertexArrayAttrib"

-- glEnableVertexArrayAttribEXT ------------------------------------------------

glEnableVertexArrayAttribEXT
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @index@.
  -> m ()
glEnableVertexArrayAttribEXT v1 v2 = liftIO $ dyn3 ptr_glEnableVertexArrayAttribEXT v1 v2

{-# NOINLINE ptr_glEnableVertexArrayAttribEXT #-}
ptr_glEnableVertexArrayAttribEXT :: FunPtr (GLuint -> GLuint -> IO ())
ptr_glEnableVertexArrayAttribEXT = unsafePerformIO $ getCommand "glEnableVertexArrayAttribEXT"

-- glEnableVertexArrayEXT ------------------------------------------------------

glEnableVertexArrayEXT
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLenum -- ^ @array@ of type [EnableCap](Graphics-GL-Groups.html#EnableCap).
  -> m ()
glEnableVertexArrayEXT v1 v2 = liftIO $ dyn15 ptr_glEnableVertexArrayEXT v1 v2

{-# NOINLINE ptr_glEnableVertexArrayEXT #-}
ptr_glEnableVertexArrayEXT :: FunPtr (GLuint -> GLenum -> IO ())
ptr_glEnableVertexArrayEXT = unsafePerformIO $ getCommand "glEnableVertexArrayEXT"

-- glEnableVertexAttribAPPLE ---------------------------------------------------

glEnableVertexAttribAPPLE
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@.
  -> m ()
glEnableVertexAttribAPPLE v1 v2 = liftIO $ dyn15 ptr_glEnableVertexAttribAPPLE v1 v2

{-# NOINLINE ptr_glEnableVertexAttribAPPLE #-}
ptr_glEnableVertexAttribAPPLE :: FunPtr (GLuint -> GLenum -> IO ())
ptr_glEnableVertexAttribAPPLE = unsafePerformIO $ getCommand "glEnableVertexAttribAPPLE"

-- glEnableVertexAttribArray ---------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glEnableVertexAttribArray.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glEnableVertexAttribArray.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glEnableVertexAttribArray.xhtml OpenGL 4.x>.
glEnableVertexAttribArray
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> m ()
glEnableVertexAttribArray v1 = liftIO $ dyn2 ptr_glEnableVertexAttribArray v1

{-# NOINLINE ptr_glEnableVertexAttribArray #-}
ptr_glEnableVertexAttribArray :: FunPtr (GLuint -> IO ())
ptr_glEnableVertexAttribArray = unsafePerformIO $ getCommand "glEnableVertexAttribArray"

-- glEnableVertexAttribArrayARB ------------------------------------------------

-- | This command is an alias for 'glEnableVertexAttribArray'.
glEnableVertexAttribArrayARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> m ()
glEnableVertexAttribArrayARB v1 = liftIO $ dyn2 ptr_glEnableVertexAttribArrayARB v1

{-# NOINLINE ptr_glEnableVertexAttribArrayARB #-}
ptr_glEnableVertexAttribArrayARB :: FunPtr (GLuint -> IO ())
ptr_glEnableVertexAttribArrayARB = unsafePerformIO $ getCommand "glEnableVertexAttribArrayARB"

-- glEnablei -------------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glEnable.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glEnable.xhtml OpenGL 4.x>.
glEnablei
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLuint -- ^ @index@.
  -> m ()
glEnablei v1 v2 = liftIO $ dyn16 ptr_glEnablei v1 v2

{-# NOINLINE ptr_glEnablei #-}
ptr_glEnablei :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glEnablei = unsafePerformIO $ getCommand "glEnablei"

-- glEnableiEXT ----------------------------------------------------------------

-- | This command is an alias for 'glEnablei'.
glEnableiEXT
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLuint -- ^ @index@.
  -> m ()
glEnableiEXT v1 v2 = liftIO $ dyn16 ptr_glEnableiEXT v1 v2

{-# NOINLINE ptr_glEnableiEXT #-}
ptr_glEnableiEXT :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glEnableiEXT = unsafePerformIO $ getCommand "glEnableiEXT"

-- glEnableiNV -----------------------------------------------------------------

-- | This command is an alias for 'glEnablei'.
glEnableiNV
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLuint -- ^ @index@.
  -> m ()
glEnableiNV v1 v2 = liftIO $ dyn16 ptr_glEnableiNV v1 v2

{-# NOINLINE ptr_glEnableiNV #-}
ptr_glEnableiNV :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glEnableiNV = unsafePerformIO $ getCommand "glEnableiNV"

-- glEnableiOES ----------------------------------------------------------------

-- | This command is an alias for 'glEnablei'.
glEnableiOES
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLuint -- ^ @index@.
  -> m ()
glEnableiOES v1 v2 = liftIO $ dyn16 ptr_glEnableiOES v1 v2

{-# NOINLINE ptr_glEnableiOES #-}
ptr_glEnableiOES :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glEnableiOES = unsafePerformIO $ getCommand "glEnableiOES"

-- glEnd -----------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glBegin.xml OpenGL 2.x>.
glEnd
  :: MonadIO m
  => m ()
glEnd = liftIO $ dyn10 ptr_glEnd

{-# NOINLINE ptr_glEnd #-}
ptr_glEnd :: FunPtr (IO ())
ptr_glEnd = unsafePerformIO $ getCommand "glEnd"

-- glEndConditionalRender ------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glBeginConditionalRender.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glBeginConditionalRender.xhtml OpenGL 4.x>.
glEndConditionalRender
  :: MonadIO m
  => m ()
glEndConditionalRender = liftIO $ dyn10 ptr_glEndConditionalRender

{-# NOINLINE ptr_glEndConditionalRender #-}
ptr_glEndConditionalRender :: FunPtr (IO ())
ptr_glEndConditionalRender = unsafePerformIO $ getCommand "glEndConditionalRender"

-- glEndConditionalRenderNV ----------------------------------------------------

-- | This command is an alias for 'glEndConditionalRender'.
glEndConditionalRenderNV
  :: MonadIO m
  => m ()
glEndConditionalRenderNV = liftIO $ dyn10 ptr_glEndConditionalRenderNV

{-# NOINLINE ptr_glEndConditionalRenderNV #-}
ptr_glEndConditionalRenderNV :: FunPtr (IO ())
ptr_glEndConditionalRenderNV = unsafePerformIO $ getCommand "glEndConditionalRenderNV"

-- glEndConditionalRenderNVX ---------------------------------------------------

-- | This command is an alias for 'glEndConditionalRender'.
glEndConditionalRenderNVX
  :: MonadIO m
  => m ()
glEndConditionalRenderNVX = liftIO $ dyn10 ptr_glEndConditionalRenderNVX

{-# NOINLINE ptr_glEndConditionalRenderNVX #-}
ptr_glEndConditionalRenderNVX :: FunPtr (IO ())
ptr_glEndConditionalRenderNVX = unsafePerformIO $ getCommand "glEndConditionalRenderNVX"

-- glEndFragmentShaderATI ------------------------------------------------------

glEndFragmentShaderATI
  :: MonadIO m
  => m ()
glEndFragmentShaderATI = liftIO $ dyn10 ptr_glEndFragmentShaderATI

{-# NOINLINE ptr_glEndFragmentShaderATI #-}
ptr_glEndFragmentShaderATI :: FunPtr (IO ())
ptr_glEndFragmentShaderATI = unsafePerformIO $ getCommand "glEndFragmentShaderATI"

-- glEndList -------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glNewList.xml OpenGL 2.x>.
glEndList
  :: MonadIO m
  => m ()
glEndList = liftIO $ dyn10 ptr_glEndList

{-# NOINLINE ptr_glEndList #-}
ptr_glEndList :: FunPtr (IO ())
ptr_glEndList = unsafePerformIO $ getCommand "glEndList"

-- glEndOcclusionQueryNV -------------------------------------------------------

glEndOcclusionQueryNV
  :: MonadIO m
  => m ()
glEndOcclusionQueryNV = liftIO $ dyn10 ptr_glEndOcclusionQueryNV

{-# NOINLINE ptr_glEndOcclusionQueryNV #-}
ptr_glEndOcclusionQueryNV :: FunPtr (IO ())
ptr_glEndOcclusionQueryNV = unsafePerformIO $ getCommand "glEndOcclusionQueryNV"

-- glEndPerfMonitorAMD ---------------------------------------------------------

glEndPerfMonitorAMD
  :: MonadIO m
  => GLuint -- ^ @monitor@.
  -> m ()
glEndPerfMonitorAMD v1 = liftIO $ dyn2 ptr_glEndPerfMonitorAMD v1

{-# NOINLINE ptr_glEndPerfMonitorAMD #-}
ptr_glEndPerfMonitorAMD :: FunPtr (GLuint -> IO ())
ptr_glEndPerfMonitorAMD = unsafePerformIO $ getCommand "glEndPerfMonitorAMD"

-- glEndPerfQueryINTEL ---------------------------------------------------------

glEndPerfQueryINTEL
  :: MonadIO m
  => GLuint -- ^ @queryHandle@.
  -> m ()
glEndPerfQueryINTEL v1 = liftIO $ dyn2 ptr_glEndPerfQueryINTEL v1

{-# NOINLINE ptr_glEndPerfQueryINTEL #-}
ptr_glEndPerfQueryINTEL :: FunPtr (GLuint -> IO ())
ptr_glEndPerfQueryINTEL = unsafePerformIO $ getCommand "glEndPerfQueryINTEL"

-- glEndQuery ------------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glBeginQuery.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glBeginQuery.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glBeginQuery.xhtml OpenGL 4.x>.
glEndQuery
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> m ()
glEndQuery v1 = liftIO $ dyn4 ptr_glEndQuery v1

{-# NOINLINE ptr_glEndQuery #-}
ptr_glEndQuery :: FunPtr (GLenum -> IO ())
ptr_glEndQuery = unsafePerformIO $ getCommand "glEndQuery"

-- glEndQueryARB ---------------------------------------------------------------

-- | This command is an alias for 'glEndQuery'.
glEndQueryARB
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> m ()
glEndQueryARB v1 = liftIO $ dyn4 ptr_glEndQueryARB v1

{-# NOINLINE ptr_glEndQueryARB #-}
ptr_glEndQueryARB :: FunPtr (GLenum -> IO ())
ptr_glEndQueryARB = unsafePerformIO $ getCommand "glEndQueryARB"

-- glEndQueryEXT ---------------------------------------------------------------

glEndQueryEXT
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> m ()
glEndQueryEXT v1 = liftIO $ dyn4 ptr_glEndQueryEXT v1

{-# NOINLINE ptr_glEndQueryEXT #-}
ptr_glEndQueryEXT :: FunPtr (GLenum -> IO ())
ptr_glEndQueryEXT = unsafePerformIO $ getCommand "glEndQueryEXT"

-- glEndQueryIndexed -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glBeginQueryIndexed.xhtml OpenGL 4.x>.
glEndQueryIndexed
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLuint -- ^ @index@.
  -> m ()
glEndQueryIndexed v1 v2 = liftIO $ dyn16 ptr_glEndQueryIndexed v1 v2

{-# NOINLINE ptr_glEndQueryIndexed #-}
ptr_glEndQueryIndexed :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glEndQueryIndexed = unsafePerformIO $ getCommand "glEndQueryIndexed"

-- glEndTilingQCOM -------------------------------------------------------------

glEndTilingQCOM
  :: MonadIO m
  => GLbitfield -- ^ @preserveMask@.
  -> m ()
glEndTilingQCOM v1 = liftIO $ dyn69 ptr_glEndTilingQCOM v1

{-# NOINLINE ptr_glEndTilingQCOM #-}
ptr_glEndTilingQCOM :: FunPtr (GLbitfield -> IO ())
ptr_glEndTilingQCOM = unsafePerformIO $ getCommand "glEndTilingQCOM"

-- glEndTransformFeedback ------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glBeginTransformFeedback.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glBeginTransformFeedback.xhtml OpenGL 4.x>.
glEndTransformFeedback
  :: MonadIO m
  => m ()
glEndTransformFeedback = liftIO $ dyn10 ptr_glEndTransformFeedback

{-# NOINLINE ptr_glEndTransformFeedback #-}
ptr_glEndTransformFeedback :: FunPtr (IO ())
ptr_glEndTransformFeedback = unsafePerformIO $ getCommand "glEndTransformFeedback"

-- glEndTransformFeedbackEXT ---------------------------------------------------

-- | This command is an alias for 'glEndTransformFeedback'.
glEndTransformFeedbackEXT
  :: MonadIO m
  => m ()
glEndTransformFeedbackEXT = liftIO $ dyn10 ptr_glEndTransformFeedbackEXT

{-# NOINLINE ptr_glEndTransformFeedbackEXT #-}
ptr_glEndTransformFeedbackEXT :: FunPtr (IO ())
ptr_glEndTransformFeedbackEXT = unsafePerformIO $ getCommand "glEndTransformFeedbackEXT"

-- glEndTransformFeedbackNV ----------------------------------------------------

-- | This command is an alias for 'glEndTransformFeedback'.
glEndTransformFeedbackNV
  :: MonadIO m
  => m ()
glEndTransformFeedbackNV = liftIO $ dyn10 ptr_glEndTransformFeedbackNV

{-# NOINLINE ptr_glEndTransformFeedbackNV #-}
ptr_glEndTransformFeedbackNV :: FunPtr (IO ())
ptr_glEndTransformFeedbackNV = unsafePerformIO $ getCommand "glEndTransformFeedbackNV"

-- glEndVertexShaderEXT --------------------------------------------------------

glEndVertexShaderEXT
  :: MonadIO m
  => m ()
glEndVertexShaderEXT = liftIO $ dyn10 ptr_glEndVertexShaderEXT

{-# NOINLINE ptr_glEndVertexShaderEXT #-}
ptr_glEndVertexShaderEXT :: FunPtr (IO ())
ptr_glEndVertexShaderEXT = unsafePerformIO $ getCommand "glEndVertexShaderEXT"

-- glEndVideoCaptureNV ---------------------------------------------------------

glEndVideoCaptureNV
  :: MonadIO m
  => GLuint -- ^ @video_capture_slot@.
  -> m ()
glEndVideoCaptureNV v1 = liftIO $ dyn2 ptr_glEndVideoCaptureNV v1

{-# NOINLINE ptr_glEndVideoCaptureNV #-}
ptr_glEndVideoCaptureNV :: FunPtr (GLuint -> IO ())
ptr_glEndVideoCaptureNV = unsafePerformIO $ getCommand "glEndVideoCaptureNV"

-- glEvalCoord1d ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glEvalCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glEvalCoord1dv'.
glEvalCoord1d
  :: MonadIO m
  => GLdouble -- ^ @u@ of type @CoordD@.
  -> m ()
glEvalCoord1d v1 = liftIO $ dyn78 ptr_glEvalCoord1d v1

{-# NOINLINE ptr_glEvalCoord1d #-}
ptr_glEvalCoord1d :: FunPtr (GLdouble -> IO ())
ptr_glEvalCoord1d = unsafePerformIO $ getCommand "glEvalCoord1d"

-- glEvalCoord1dv --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glEvalCoord.xml OpenGL 2.x>.
glEvalCoord1dv
  :: MonadIO m
  => Ptr GLdouble -- ^ @u@ pointing to @1@ element of type @CoordD@.
  -> m ()
glEvalCoord1dv v1 = liftIO $ dyn39 ptr_glEvalCoord1dv v1

{-# NOINLINE ptr_glEvalCoord1dv #-}
ptr_glEvalCoord1dv :: FunPtr (Ptr GLdouble -> IO ())
ptr_glEvalCoord1dv = unsafePerformIO $ getCommand "glEvalCoord1dv"

-- glEvalCoord1f ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glEvalCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glEvalCoord1fv'.
glEvalCoord1f
  :: MonadIO m
  => GLfloat -- ^ @u@ of type @CoordF@.
  -> m ()
glEvalCoord1f v1 = liftIO $ dyn79 ptr_glEvalCoord1f v1

{-# NOINLINE ptr_glEvalCoord1f #-}
ptr_glEvalCoord1f :: FunPtr (GLfloat -> IO ())
ptr_glEvalCoord1f = unsafePerformIO $ getCommand "glEvalCoord1f"

-- glEvalCoord1fv --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glEvalCoord.xml OpenGL 2.x>.
glEvalCoord1fv
  :: MonadIO m
  => Ptr GLfloat -- ^ @u@ pointing to @1@ element of type @CoordF@.
  -> m ()
glEvalCoord1fv v1 = liftIO $ dyn41 ptr_glEvalCoord1fv v1

{-# NOINLINE ptr_glEvalCoord1fv #-}
ptr_glEvalCoord1fv :: FunPtr (Ptr GLfloat -> IO ())
ptr_glEvalCoord1fv = unsafePerformIO $ getCommand "glEvalCoord1fv"

-- glEvalCoord1xOES ------------------------------------------------------------

glEvalCoord1xOES
  :: MonadIO m
  => GLfixed -- ^ @u@.
  -> m ()
glEvalCoord1xOES v1 = liftIO $ dyn81 ptr_glEvalCoord1xOES v1

{-# NOINLINE ptr_glEvalCoord1xOES #-}
ptr_glEvalCoord1xOES :: FunPtr (GLfixed -> IO ())
ptr_glEvalCoord1xOES = unsafePerformIO $ getCommand "glEvalCoord1xOES"

-- glEvalCoord1xvOES -----------------------------------------------------------

glEvalCoord1xvOES
  :: MonadIO m
  => Ptr GLfixed -- ^ @coords@ pointing to @1@ element of type @GLfixed@.
  -> m ()
glEvalCoord1xvOES v1 = liftIO $ dyn107 ptr_glEvalCoord1xvOES v1

{-# NOINLINE ptr_glEvalCoord1xvOES #-}
ptr_glEvalCoord1xvOES :: FunPtr (Ptr GLfixed -> IO ())
ptr_glEvalCoord1xvOES = unsafePerformIO $ getCommand "glEvalCoord1xvOES"

-- glEvalCoord2d ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glEvalCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glEvalCoord2dv'.
glEvalCoord2d
  :: MonadIO m
  => GLdouble -- ^ @u@ of type @CoordD@.
  -> GLdouble -- ^ @v@ of type @CoordD@.
  -> m ()
glEvalCoord2d v1 v2 = liftIO $ dyn217 ptr_glEvalCoord2d v1 v2

{-# NOINLINE ptr_glEvalCoord2d #-}
ptr_glEvalCoord2d :: FunPtr (GLdouble -> GLdouble -> IO ())
ptr_glEvalCoord2d = unsafePerformIO $ getCommand "glEvalCoord2d"

-- glEvalCoord2dv --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glEvalCoord.xml OpenGL 2.x>.
glEvalCoord2dv
  :: MonadIO m
  => Ptr GLdouble -- ^ @u@ pointing to @2@ elements of type @CoordD@.
  -> m ()
glEvalCoord2dv v1 = liftIO $ dyn39 ptr_glEvalCoord2dv v1

{-# NOINLINE ptr_glEvalCoord2dv #-}
ptr_glEvalCoord2dv :: FunPtr (Ptr GLdouble -> IO ())
ptr_glEvalCoord2dv = unsafePerformIO $ getCommand "glEvalCoord2dv"

-- glEvalCoord2f ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glEvalCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glEvalCoord2fv'.
glEvalCoord2f
  :: MonadIO m
  => GLfloat -- ^ @u@ of type @CoordF@.
  -> GLfloat -- ^ @v@ of type @CoordF@.
  -> m ()
glEvalCoord2f v1 v2 = liftIO $ dyn222 ptr_glEvalCoord2f v1 v2

{-# NOINLINE ptr_glEvalCoord2f #-}
ptr_glEvalCoord2f :: FunPtr (GLfloat -> GLfloat -> IO ())
ptr_glEvalCoord2f = unsafePerformIO $ getCommand "glEvalCoord2f"

