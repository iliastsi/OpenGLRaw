--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F47
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

module Graphics.GL.Functions.F47 (
  glPointParameterfv,
  glPointParameterfvARB,
  glPointParameterfvEXT,
  glPointParameterfvSGIS,
  glPointParameteri,
  glPointParameteriNV,
  glPointParameteriv,
  glPointParameterivNV,
  glPointParameterx,
  glPointParameterxOES,
  glPointParameterxv,
  glPointParameterxvOES,
  glPointSize,
  glPointSizePointerOES,
  glPointSizex,
  glPointSizexOES,
  glPollAsyncSGIX,
  glPollInstrumentsSGIX,
  glPolygonMode,
  glPolygonModeNV,
  glPolygonOffset,
  glPolygonOffsetClampEXT,
  glPolygonOffsetEXT,
  glPolygonOffsetx,
  glPolygonOffsetxOES,
  glPolygonStipple,
  glPopAttrib,
  glPopClientAttrib,
  glPopDebugGroup,
  glPopDebugGroupKHR,
  glPopGroupMarkerEXT,
  glPopMatrix,
  glPopName,
  glPresentFrameDualFillNV,
  glPresentFrameKeyedNV,
  glPrimitiveBoundingBox,
  glPrimitiveBoundingBoxARB,
  glPrimitiveBoundingBoxEXT,
  glPrimitiveBoundingBoxOES,
  glPrimitiveRestartIndex
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

-- glPointParameterfv ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glPointParameter.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glPointParameter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glPointParameter.xhtml OpenGL 4.x>.
glPointParameterfv
  :: MonadIO m
  => GLenum -- ^ @pname@ of type @PointParameterNameARB@.
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedFloat32@.
  -> m ()
glPointParameterfv v1 v2 = liftIO $ dyn94 ptr_glPointParameterfv v1 v2

{-# NOINLINE ptr_glPointParameterfv #-}
ptr_glPointParameterfv :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glPointParameterfv = unsafePerformIO $ getCommand "glPointParameterfv"

-- glPointParameterfvARB -------------------------------------------------------

-- | This command is an alias for 'glPointParameterfv'.
glPointParameterfvARB
  :: MonadIO m
  => GLenum -- ^ @pname@ of type @PointParameterNameARB@.
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedFloat32@.
  -> m ()
glPointParameterfvARB v1 v2 = liftIO $ dyn94 ptr_glPointParameterfvARB v1 v2

{-# NOINLINE ptr_glPointParameterfvARB #-}
ptr_glPointParameterfvARB :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glPointParameterfvARB = unsafePerformIO $ getCommand "glPointParameterfvARB"

-- glPointParameterfvEXT -------------------------------------------------------

-- | This command is an alias for 'glPointParameterfv'.
glPointParameterfvEXT
  :: MonadIO m
  => GLenum -- ^ @pname@ of type @PointParameterNameARB@.
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedFloat32@.
  -> m ()
glPointParameterfvEXT v1 v2 = liftIO $ dyn94 ptr_glPointParameterfvEXT v1 v2

{-# NOINLINE ptr_glPointParameterfvEXT #-}
ptr_glPointParameterfvEXT :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glPointParameterfvEXT = unsafePerformIO $ getCommand "glPointParameterfvEXT"

-- glPointParameterfvSGIS ------------------------------------------------------

-- | This command is an alias for 'glPointParameterfv'.
glPointParameterfvSGIS
  :: MonadIO m
  => GLenum -- ^ @pname@ of type @PointParameterNameARB@.
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedFloat32@.
  -> m ()
glPointParameterfvSGIS v1 v2 = liftIO $ dyn94 ptr_glPointParameterfvSGIS v1 v2

{-# NOINLINE ptr_glPointParameterfvSGIS #-}
ptr_glPointParameterfvSGIS :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glPointParameterfvSGIS = unsafePerformIO $ getCommand "glPointParameterfvSGIS"

-- glPointParameteri -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glPointParameter.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glPointParameter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glPointParameter.xhtml OpenGL 4.x>.
glPointParameteri
  :: MonadIO m
  => GLenum -- ^ @pname@ of type @PointParameterNameARB@.
  -> GLint -- ^ @param@.
  -> m ()
glPointParameteri v1 v2 = liftIO $ dyn55 ptr_glPointParameteri v1 v2

{-# NOINLINE ptr_glPointParameteri #-}
ptr_glPointParameteri :: FunPtr (GLenum -> GLint -> IO ())
ptr_glPointParameteri = unsafePerformIO $ getCommand "glPointParameteri"

-- glPointParameteriNV ---------------------------------------------------------

-- | This command is an alias for 'glPointParameteri'.
glPointParameteriNV
  :: MonadIO m
  => GLenum -- ^ @pname@ of type @PointParameterNameARB@.
  -> GLint -- ^ @param@.
  -> m ()
glPointParameteriNV v1 v2 = liftIO $ dyn55 ptr_glPointParameteriNV v1 v2

{-# NOINLINE ptr_glPointParameteriNV #-}
ptr_glPointParameteriNV :: FunPtr (GLenum -> GLint -> IO ())
ptr_glPointParameteriNV = unsafePerformIO $ getCommand "glPointParameteriNV"

-- glPointParameteriv ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glPointParameter.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glPointParameter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glPointParameter.xhtml OpenGL 4.x>.
glPointParameteriv
  :: MonadIO m
  => GLenum -- ^ @pname@ of type @PointParameterNameARB@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glPointParameteriv v1 v2 = liftIO $ dyn136 ptr_glPointParameteriv v1 v2

{-# NOINLINE ptr_glPointParameteriv #-}
ptr_glPointParameteriv :: FunPtr (GLenum -> Ptr GLint -> IO ())
ptr_glPointParameteriv = unsafePerformIO $ getCommand "glPointParameteriv"

-- glPointParameterivNV --------------------------------------------------------

-- | This command is an alias for 'glPointParameteriv'.
glPointParameterivNV
  :: MonadIO m
  => GLenum -- ^ @pname@ of type @PointParameterNameARB@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glPointParameterivNV v1 v2 = liftIO $ dyn136 ptr_glPointParameterivNV v1 v2

{-# NOINLINE ptr_glPointParameterivNV #-}
ptr_glPointParameterivNV :: FunPtr (GLenum -> Ptr GLint -> IO ())
ptr_glPointParameterivNV = unsafePerformIO $ getCommand "glPointParameterivNV"

-- glPointParameterx -----------------------------------------------------------

glPointParameterx
  :: MonadIO m
  => GLenum -- ^ @pname@.
  -> GLfixed -- ^ @param@.
  -> m ()
glPointParameterx v1 v2 = liftIO $ dyn1 ptr_glPointParameterx v1 v2

{-# NOINLINE ptr_glPointParameterx #-}
ptr_glPointParameterx :: FunPtr (GLenum -> GLfixed -> IO ())
ptr_glPointParameterx = unsafePerformIO $ getCommand "glPointParameterx"

-- glPointParameterxOES --------------------------------------------------------

glPointParameterxOES
  :: MonadIO m
  => GLenum -- ^ @pname@.
  -> GLfixed -- ^ @param@.
  -> m ()
glPointParameterxOES v1 v2 = liftIO $ dyn1 ptr_glPointParameterxOES v1 v2

{-# NOINLINE ptr_glPointParameterxOES #-}
ptr_glPointParameterxOES :: FunPtr (GLenum -> GLfixed -> IO ())
ptr_glPointParameterxOES = unsafePerformIO $ getCommand "glPointParameterxOES"

-- glPointParameterxv ----------------------------------------------------------

glPointParameterxv
  :: MonadIO m
  => GLenum -- ^ @pname@.
  -> Ptr GLfixed -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfixed@.
  -> m ()
glPointParameterxv v1 v2 = liftIO $ dyn95 ptr_glPointParameterxv v1 v2

{-# NOINLINE ptr_glPointParameterxv #-}
ptr_glPointParameterxv :: FunPtr (GLenum -> Ptr GLfixed -> IO ())
ptr_glPointParameterxv = unsafePerformIO $ getCommand "glPointParameterxv"

-- glPointParameterxvOES -------------------------------------------------------

glPointParameterxvOES
  :: MonadIO m
  => GLenum -- ^ @pname@.
  -> Ptr GLfixed -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfixed@.
  -> m ()
glPointParameterxvOES v1 v2 = liftIO $ dyn95 ptr_glPointParameterxvOES v1 v2

{-# NOINLINE ptr_glPointParameterxvOES #-}
ptr_glPointParameterxvOES :: FunPtr (GLenum -> Ptr GLfixed -> IO ())
ptr_glPointParameterxvOES = unsafePerformIO $ getCommand "glPointParameterxvOES"

-- glPointSize -----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glPointSize.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glPointSize.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glPointSize.xhtml OpenGL 4.x>.
glPointSize
  :: MonadIO m
  => GLfloat -- ^ @size@ of type @CheckedFloat32@.
  -> m ()
glPointSize v1 = liftIO $ dyn79 ptr_glPointSize v1

{-# NOINLINE ptr_glPointSize #-}
ptr_glPointSize :: FunPtr (GLfloat -> IO ())
ptr_glPointSize = unsafePerformIO $ getCommand "glPointSize"

-- glPointSizePointerOES -------------------------------------------------------

glPointSizePointerOES
  :: MonadIO m
  => GLenum -- ^ @type@.
  -> GLsizei -- ^ @stride@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(type,stride)@ elements of type @a@.
  -> m ()
glPointSizePointerOES v1 v2 v3 = liftIO $ dyn46 ptr_glPointSizePointerOES v1 v2 v3

{-# NOINLINE ptr_glPointSizePointerOES #-}
ptr_glPointSizePointerOES :: FunPtr (GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glPointSizePointerOES = unsafePerformIO $ getCommand "glPointSizePointerOES"

-- glPointSizex ----------------------------------------------------------------

glPointSizex
  :: MonadIO m
  => GLfixed -- ^ @size@.
  -> m ()
glPointSizex v1 = liftIO $ dyn81 ptr_glPointSizex v1

{-# NOINLINE ptr_glPointSizex #-}
ptr_glPointSizex :: FunPtr (GLfixed -> IO ())
ptr_glPointSizex = unsafePerformIO $ getCommand "glPointSizex"

-- glPointSizexOES -------------------------------------------------------------

glPointSizexOES
  :: MonadIO m
  => GLfixed -- ^ @size@.
  -> m ()
glPointSizexOES v1 = liftIO $ dyn81 ptr_glPointSizexOES v1

{-# NOINLINE ptr_glPointSizexOES #-}
ptr_glPointSizexOES :: FunPtr (GLfixed -> IO ())
ptr_glPointSizexOES = unsafePerformIO $ getCommand "glPointSizexOES"

-- glPollAsyncSGIX -------------------------------------------------------------

glPollAsyncSGIX
  :: MonadIO m
  => Ptr GLuint -- ^ @markerp@ pointing to @1@ element of type @GLuint@.
  -> m GLint
glPollAsyncSGIX v1 = liftIO $ dyn277 ptr_glPollAsyncSGIX v1

{-# NOINLINE ptr_glPollAsyncSGIX #-}
ptr_glPollAsyncSGIX :: FunPtr (Ptr GLuint -> IO GLint)
ptr_glPollAsyncSGIX = unsafePerformIO $ getCommand "glPollAsyncSGIX"

-- glPollInstrumentsSGIX -------------------------------------------------------

glPollInstrumentsSGIX
  :: MonadIO m
  => Ptr GLint -- ^ @marker_p@ pointing to @1@ element of type @GLint@.
  -> m GLint
glPollInstrumentsSGIX v1 = liftIO $ dyn610 ptr_glPollInstrumentsSGIX v1

{-# NOINLINE ptr_glPollInstrumentsSGIX #-}
ptr_glPollInstrumentsSGIX :: FunPtr (Ptr GLint -> IO GLint)
ptr_glPollInstrumentsSGIX = unsafePerformIO $ getCommand "glPollInstrumentsSGIX"

-- glPolygonMode ---------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glPolygonMode.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glPolygonMode.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glPolygonMode.xhtml OpenGL 4.x>.
glPolygonMode
  :: MonadIO m
  => GLenum -- ^ @face@ of type [MaterialFace](Graphics-GL-Groups.html#MaterialFace).
  -> GLenum -- ^ @mode@ of type [PolygonMode](Graphics-GL-Groups.html#PolygonMode).
  -> m ()
glPolygonMode v1 v2 = liftIO $ dyn51 ptr_glPolygonMode v1 v2

{-# NOINLINE ptr_glPolygonMode #-}
ptr_glPolygonMode :: FunPtr (GLenum -> GLenum -> IO ())
ptr_glPolygonMode = unsafePerformIO $ getCommand "glPolygonMode"

-- glPolygonModeNV -------------------------------------------------------------

-- | This command is an alias for 'glPolygonMode'.
glPolygonModeNV
  :: MonadIO m
  => GLenum -- ^ @face@ of type [MaterialFace](Graphics-GL-Groups.html#MaterialFace).
  -> GLenum -- ^ @mode@ of type [PolygonMode](Graphics-GL-Groups.html#PolygonMode).
  -> m ()
glPolygonModeNV v1 v2 = liftIO $ dyn51 ptr_glPolygonModeNV v1 v2

{-# NOINLINE ptr_glPolygonModeNV #-}
ptr_glPolygonModeNV :: FunPtr (GLenum -> GLenum -> IO ())
ptr_glPolygonModeNV = unsafePerformIO $ getCommand "glPolygonModeNV"

-- glPolygonOffset -------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glPolygonOffset.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glPolygonOffset.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glPolygonOffset.xhtml OpenGL 4.x>.
glPolygonOffset
  :: MonadIO m
  => GLfloat -- ^ @factor@.
  -> GLfloat -- ^ @units@.
  -> m ()
glPolygonOffset v1 v2 = liftIO $ dyn222 ptr_glPolygonOffset v1 v2

{-# NOINLINE ptr_glPolygonOffset #-}
ptr_glPolygonOffset :: FunPtr (GLfloat -> GLfloat -> IO ())
ptr_glPolygonOffset = unsafePerformIO $ getCommand "glPolygonOffset"

-- glPolygonOffsetClampEXT -----------------------------------------------------

glPolygonOffsetClampEXT
  :: MonadIO m
  => GLfloat -- ^ @factor@.
  -> GLfloat -- ^ @units@.
  -> GLfloat -- ^ @clamp@.
  -> m ()
glPolygonOffsetClampEXT v1 v2 v3 = liftIO $ dyn40 ptr_glPolygonOffsetClampEXT v1 v2 v3

{-# NOINLINE ptr_glPolygonOffsetClampEXT #-}
ptr_glPolygonOffsetClampEXT :: FunPtr (GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glPolygonOffsetClampEXT = unsafePerformIO $ getCommand "glPolygonOffsetClampEXT"

-- glPolygonOffsetEXT ----------------------------------------------------------

glPolygonOffsetEXT
  :: MonadIO m
  => GLfloat -- ^ @factor@.
  -> GLfloat -- ^ @bias@.
  -> m ()
glPolygonOffsetEXT v1 v2 = liftIO $ dyn222 ptr_glPolygonOffsetEXT v1 v2

{-# NOINLINE ptr_glPolygonOffsetEXT #-}
ptr_glPolygonOffsetEXT :: FunPtr (GLfloat -> GLfloat -> IO ())
ptr_glPolygonOffsetEXT = unsafePerformIO $ getCommand "glPolygonOffsetEXT"

-- glPolygonOffsetx ------------------------------------------------------------

glPolygonOffsetx
  :: MonadIO m
  => GLfixed -- ^ @factor@.
  -> GLfixed -- ^ @units@.
  -> m ()
glPolygonOffsetx v1 v2 = liftIO $ dyn224 ptr_glPolygonOffsetx v1 v2

{-# NOINLINE ptr_glPolygonOffsetx #-}
ptr_glPolygonOffsetx :: FunPtr (GLfixed -> GLfixed -> IO ())
ptr_glPolygonOffsetx = unsafePerformIO $ getCommand "glPolygonOffsetx"

-- glPolygonOffsetxOES ---------------------------------------------------------

glPolygonOffsetxOES
  :: MonadIO m
  => GLfixed -- ^ @factor@.
  -> GLfixed -- ^ @units@.
  -> m ()
glPolygonOffsetxOES v1 v2 = liftIO $ dyn224 ptr_glPolygonOffsetxOES v1 v2

{-# NOINLINE ptr_glPolygonOffsetxOES #-}
ptr_glPolygonOffsetxOES :: FunPtr (GLfixed -> GLfixed -> IO ())
ptr_glPolygonOffsetxOES = unsafePerformIO $ getCommand "glPolygonOffsetxOES"

-- glPolygonStipple ------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glPolygonStipple.xml OpenGL 2.x>.
glPolygonStipple
  :: MonadIO m
  => Ptr GLubyte -- ^ @mask@ pointing to @COMPSIZE()@ elements of type @GLubyte@.
  -> m ()
glPolygonStipple v1 = liftIO $ dyn101 ptr_glPolygonStipple v1

{-# NOINLINE ptr_glPolygonStipple #-}
ptr_glPolygonStipple :: FunPtr (Ptr GLubyte -> IO ())
ptr_glPolygonStipple = unsafePerformIO $ getCommand "glPolygonStipple"

-- glPopAttrib -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glPushAttrib.xml OpenGL 2.x>.
glPopAttrib
  :: MonadIO m
  => m ()
glPopAttrib = liftIO $ dyn10 ptr_glPopAttrib

{-# NOINLINE ptr_glPopAttrib #-}
ptr_glPopAttrib :: FunPtr (IO ())
ptr_glPopAttrib = unsafePerformIO $ getCommand "glPopAttrib"

-- glPopClientAttrib -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glPushClientAttrib.xml OpenGL 2.x>.
glPopClientAttrib
  :: MonadIO m
  => m ()
glPopClientAttrib = liftIO $ dyn10 ptr_glPopClientAttrib

{-# NOINLINE ptr_glPopClientAttrib #-}
ptr_glPopClientAttrib :: FunPtr (IO ())
ptr_glPopClientAttrib = unsafePerformIO $ getCommand "glPopClientAttrib"

-- glPopDebugGroup -------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glPopDebugGroup.xhtml OpenGL 4.x>.
glPopDebugGroup
  :: MonadIO m
  => m ()
glPopDebugGroup = liftIO $ dyn10 ptr_glPopDebugGroup

{-# NOINLINE ptr_glPopDebugGroup #-}
ptr_glPopDebugGroup :: FunPtr (IO ())
ptr_glPopDebugGroup = unsafePerformIO $ getCommand "glPopDebugGroup"

-- glPopDebugGroupKHR ----------------------------------------------------------

-- | This command is an alias for 'glPopDebugGroup'.
glPopDebugGroupKHR
  :: MonadIO m
  => m ()
glPopDebugGroupKHR = liftIO $ dyn10 ptr_glPopDebugGroupKHR

{-# NOINLINE ptr_glPopDebugGroupKHR #-}
ptr_glPopDebugGroupKHR :: FunPtr (IO ())
ptr_glPopDebugGroupKHR = unsafePerformIO $ getCommand "glPopDebugGroupKHR"

-- glPopGroupMarkerEXT ---------------------------------------------------------

glPopGroupMarkerEXT
  :: MonadIO m
  => m ()
glPopGroupMarkerEXT = liftIO $ dyn10 ptr_glPopGroupMarkerEXT

{-# NOINLINE ptr_glPopGroupMarkerEXT #-}
ptr_glPopGroupMarkerEXT :: FunPtr (IO ())
ptr_glPopGroupMarkerEXT = unsafePerformIO $ getCommand "glPopGroupMarkerEXT"

-- glPopMatrix -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glPushMatrix.xml OpenGL 2.x>.
glPopMatrix
  :: MonadIO m
  => m ()
glPopMatrix = liftIO $ dyn10 ptr_glPopMatrix

{-# NOINLINE ptr_glPopMatrix #-}
ptr_glPopMatrix :: FunPtr (IO ())
ptr_glPopMatrix = unsafePerformIO $ getCommand "glPopMatrix"

-- glPopName -------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glPushName.xml OpenGL 2.x>.
glPopName
  :: MonadIO m
  => m ()
glPopName = liftIO $ dyn10 ptr_glPopName

{-# NOINLINE ptr_glPopName #-}
ptr_glPopName :: FunPtr (IO ())
ptr_glPopName = unsafePerformIO $ getCommand "glPopName"

-- glPresentFrameDualFillNV ----------------------------------------------------

glPresentFrameDualFillNV
  :: MonadIO m
  => GLuint -- ^ @video_slot@.
  -> GLuint64EXT -- ^ @minPresentTime@.
  -> GLuint -- ^ @beginPresentTimeId@.
  -> GLuint -- ^ @presentDurationId@.
  -> GLenum -- ^ @type@.
  -> GLenum -- ^ @target0@.
  -> GLuint -- ^ @fill0@.
  -> GLenum -- ^ @target1@.
  -> GLuint -- ^ @fill1@.
  -> GLenum -- ^ @target2@.
  -> GLuint -- ^ @fill2@.
  -> GLenum -- ^ @target3@.
  -> GLuint -- ^ @fill3@.
  -> m ()
glPresentFrameDualFillNV v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 = liftIO $ dyn611 ptr_glPresentFrameDualFillNV v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13

{-# NOINLINE ptr_glPresentFrameDualFillNV #-}
ptr_glPresentFrameDualFillNV :: FunPtr (GLuint -> GLuint64EXT -> GLuint -> GLuint -> GLenum -> GLenum -> GLuint -> GLenum -> GLuint -> GLenum -> GLuint -> GLenum -> GLuint -> IO ())
ptr_glPresentFrameDualFillNV = unsafePerformIO $ getCommand "glPresentFrameDualFillNV"

-- glPresentFrameKeyedNV -------------------------------------------------------

glPresentFrameKeyedNV
  :: MonadIO m
  => GLuint -- ^ @video_slot@.
  -> GLuint64EXT -- ^ @minPresentTime@.
  -> GLuint -- ^ @beginPresentTimeId@.
  -> GLuint -- ^ @presentDurationId@.
  -> GLenum -- ^ @type@.
  -> GLenum -- ^ @target0@.
  -> GLuint -- ^ @fill0@.
  -> GLuint -- ^ @key0@.
  -> GLenum -- ^ @target1@.
  -> GLuint -- ^ @fill1@.
  -> GLuint -- ^ @key1@.
  -> m ()
glPresentFrameKeyedNV v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 = liftIO $ dyn612 ptr_glPresentFrameKeyedNV v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11

{-# NOINLINE ptr_glPresentFrameKeyedNV #-}
ptr_glPresentFrameKeyedNV :: FunPtr (GLuint -> GLuint64EXT -> GLuint -> GLuint -> GLenum -> GLenum -> GLuint -> GLuint -> GLenum -> GLuint -> GLuint -> IO ())
ptr_glPresentFrameKeyedNV = unsafePerformIO $ getCommand "glPresentFrameKeyedNV"

-- glPrimitiveBoundingBox ------------------------------------------------------

glPrimitiveBoundingBox
  :: MonadIO m
  => GLfloat -- ^ @minX@.
  -> GLfloat -- ^ @minY@.
  -> GLfloat -- ^ @minZ@.
  -> GLfloat -- ^ @minW@.
  -> GLfloat -- ^ @maxX@.
  -> GLfloat -- ^ @maxY@.
  -> GLfloat -- ^ @maxZ@.
  -> GLfloat -- ^ @maxW@.
  -> m ()
glPrimitiveBoundingBox v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn613 ptr_glPrimitiveBoundingBox v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glPrimitiveBoundingBox #-}
ptr_glPrimitiveBoundingBox :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glPrimitiveBoundingBox = unsafePerformIO $ getCommand "glPrimitiveBoundingBox"

-- glPrimitiveBoundingBoxARB ---------------------------------------------------

-- | This command is an alias for 'glPrimitiveBoundingBox'.
glPrimitiveBoundingBoxARB
  :: MonadIO m
  => GLfloat -- ^ @minX@.
  -> GLfloat -- ^ @minY@.
  -> GLfloat -- ^ @minZ@.
  -> GLfloat -- ^ @minW@.
  -> GLfloat -- ^ @maxX@.
  -> GLfloat -- ^ @maxY@.
  -> GLfloat -- ^ @maxZ@.
  -> GLfloat -- ^ @maxW@.
  -> m ()
glPrimitiveBoundingBoxARB v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn613 ptr_glPrimitiveBoundingBoxARB v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glPrimitiveBoundingBoxARB #-}
ptr_glPrimitiveBoundingBoxARB :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glPrimitiveBoundingBoxARB = unsafePerformIO $ getCommand "glPrimitiveBoundingBoxARB"

-- glPrimitiveBoundingBoxEXT ---------------------------------------------------

-- | This command is an alias for 'glPrimitiveBoundingBox'.
glPrimitiveBoundingBoxEXT
  :: MonadIO m
  => GLfloat -- ^ @minX@.
  -> GLfloat -- ^ @minY@.
  -> GLfloat -- ^ @minZ@.
  -> GLfloat -- ^ @minW@.
  -> GLfloat -- ^ @maxX@.
  -> GLfloat -- ^ @maxY@.
  -> GLfloat -- ^ @maxZ@.
  -> GLfloat -- ^ @maxW@.
  -> m ()
glPrimitiveBoundingBoxEXT v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn613 ptr_glPrimitiveBoundingBoxEXT v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glPrimitiveBoundingBoxEXT #-}
ptr_glPrimitiveBoundingBoxEXT :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glPrimitiveBoundingBoxEXT = unsafePerformIO $ getCommand "glPrimitiveBoundingBoxEXT"

-- glPrimitiveBoundingBoxOES ---------------------------------------------------

-- | This command is an alias for 'glPrimitiveBoundingBox'.
glPrimitiveBoundingBoxOES
  :: MonadIO m
  => GLfloat -- ^ @minX@.
  -> GLfloat -- ^ @minY@.
  -> GLfloat -- ^ @minZ@.
  -> GLfloat -- ^ @minW@.
  -> GLfloat -- ^ @maxX@.
  -> GLfloat -- ^ @maxY@.
  -> GLfloat -- ^ @maxZ@.
  -> GLfloat -- ^ @maxW@.
  -> m ()
glPrimitiveBoundingBoxOES v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn613 ptr_glPrimitiveBoundingBoxOES v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glPrimitiveBoundingBoxOES #-}
ptr_glPrimitiveBoundingBoxOES :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glPrimitiveBoundingBoxOES = unsafePerformIO $ getCommand "glPrimitiveBoundingBoxOES"

-- glPrimitiveRestartIndex -----------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glPrimitiveRestartIndex.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glPrimitiveRestartIndex.xhtml OpenGL 4.x>.
glPrimitiveRestartIndex
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> m ()
glPrimitiveRestartIndex v1 = liftIO $ dyn2 ptr_glPrimitiveRestartIndex v1

{-# NOINLINE ptr_glPrimitiveRestartIndex #-}
ptr_glPrimitiveRestartIndex :: FunPtr (GLuint -> IO ())
ptr_glPrimitiveRestartIndex = unsafePerformIO $ getCommand "glPrimitiveRestartIndex"

