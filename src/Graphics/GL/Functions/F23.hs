--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F23
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

module Graphics.GL.Functions.F23 (
  glGetError,
  glGetFenceivNV,
  glGetFinalCombinerInputParameterfvNV,
  glGetFinalCombinerInputParameterivNV,
  glGetFirstPerfQueryIdINTEL,
  glGetFixedv,
  glGetFixedvOES,
  glGetFloatIndexedvEXT,
  glGetFloati_v,
  glGetFloati_vEXT,
  glGetFloati_vNV,
  glGetFloatv,
  glGetFogFuncSGIS,
  glGetFragDataIndex,
  glGetFragDataIndexEXT,
  glGetFragDataLocation,
  glGetFragDataLocationEXT,
  glGetFragmentLightfvSGIX,
  glGetFragmentLightivSGIX,
  glGetFragmentMaterialfvSGIX,
  glGetFragmentMaterialivSGIX,
  glGetFramebufferAttachmentParameteriv,
  glGetFramebufferAttachmentParameterivEXT,
  glGetFramebufferAttachmentParameterivOES,
  glGetFramebufferParameteriv,
  glGetFramebufferParameterivEXT,
  glGetFramebufferPixelLocalStorageSizeEXT,
  glGetGraphicsResetStatus,
  glGetGraphicsResetStatusARB,
  glGetGraphicsResetStatusEXT,
  glGetGraphicsResetStatusKHR,
  glGetHandleARB,
  glGetHistogram,
  glGetHistogramEXT,
  glGetHistogramParameterfv,
  glGetHistogramParameterfvEXT,
  glGetHistogramParameteriv,
  glGetHistogramParameterivEXT,
  glGetHistogramParameterxvOES,
  glGetImageHandleARB
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

-- glGetError ------------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetError.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetError.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetError.xhtml OpenGL 4.x>.
glGetError
  :: MonadIO m
  => m GLenum -- ^ of type [ErrorCode](Graphics-GL-Groups.html#ErrorCode).
glGetError = liftIO $ dyn333 ptr_glGetError

{-# NOINLINE ptr_glGetError #-}
ptr_glGetError :: FunPtr (IO GLenum)
ptr_glGetError = unsafePerformIO $ getCommand "glGetError"

-- glGetFenceivNV --------------------------------------------------------------

glGetFenceivNV
  :: MonadIO m
  => GLuint -- ^ @fence@ of type @FenceNV@.
  -> GLenum -- ^ @pname@ of type @FenceParameterNameNV@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetFenceivNV v1 v2 v3 = liftIO $ dyn334 ptr_glGetFenceivNV v1 v2 v3

{-# NOINLINE ptr_glGetFenceivNV #-}
ptr_glGetFenceivNV :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetFenceivNV = unsafePerformIO $ getCommand "glGetFenceivNV"

-- glGetFinalCombinerInputParameterfvNV ----------------------------------------

glGetFinalCombinerInputParameterfvNV
  :: MonadIO m
  => GLenum -- ^ @variable@ of type @CombinerVariableNV@.
  -> GLenum -- ^ @pname@ of type @CombinerParameterNV@.
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetFinalCombinerInputParameterfvNV v1 v2 v3 = liftIO $ dyn132 ptr_glGetFinalCombinerInputParameterfvNV v1 v2 v3

{-# NOINLINE ptr_glGetFinalCombinerInputParameterfvNV #-}
ptr_glGetFinalCombinerInputParameterfvNV :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetFinalCombinerInputParameterfvNV = unsafePerformIO $ getCommand "glGetFinalCombinerInputParameterfvNV"

-- glGetFinalCombinerInputParameterivNV ----------------------------------------

glGetFinalCombinerInputParameterivNV
  :: MonadIO m
  => GLenum -- ^ @variable@ of type @CombinerVariableNV@.
  -> GLenum -- ^ @pname@ of type @CombinerParameterNV@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetFinalCombinerInputParameterivNV v1 v2 v3 = liftIO $ dyn133 ptr_glGetFinalCombinerInputParameterivNV v1 v2 v3

{-# NOINLINE ptr_glGetFinalCombinerInputParameterivNV #-}
ptr_glGetFinalCombinerInputParameterivNV :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetFinalCombinerInputParameterivNV = unsafePerformIO $ getCommand "glGetFinalCombinerInputParameterivNV"

-- glGetFirstPerfQueryIdINTEL --------------------------------------------------

glGetFirstPerfQueryIdINTEL
  :: MonadIO m
  => Ptr GLuint -- ^ @queryId@.
  -> m ()
glGetFirstPerfQueryIdINTEL v1 = liftIO $ dyn103 ptr_glGetFirstPerfQueryIdINTEL v1

{-# NOINLINE ptr_glGetFirstPerfQueryIdINTEL #-}
ptr_glGetFirstPerfQueryIdINTEL :: FunPtr (Ptr GLuint -> IO ())
ptr_glGetFirstPerfQueryIdINTEL = unsafePerformIO $ getCommand "glGetFirstPerfQueryIdINTEL"

-- glGetFixedv -----------------------------------------------------------------

glGetFixedv
  :: MonadIO m
  => GLenum -- ^ @pname@.
  -> Ptr GLfixed -- ^ @params@.
  -> m ()
glGetFixedv v1 v2 = liftIO $ dyn95 ptr_glGetFixedv v1 v2

{-# NOINLINE ptr_glGetFixedv #-}
ptr_glGetFixedv :: FunPtr (GLenum -> Ptr GLfixed -> IO ())
ptr_glGetFixedv = unsafePerformIO $ getCommand "glGetFixedv"

-- glGetFixedvOES --------------------------------------------------------------

glGetFixedvOES
  :: MonadIO m
  => GLenum -- ^ @pname@.
  -> Ptr GLfixed -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfixed@.
  -> m ()
glGetFixedvOES v1 v2 = liftIO $ dyn95 ptr_glGetFixedvOES v1 v2

{-# NOINLINE ptr_glGetFixedvOES #-}
ptr_glGetFixedvOES :: FunPtr (GLenum -> Ptr GLfixed -> IO ())
ptr_glGetFixedvOES = unsafePerformIO $ getCommand "glGetFixedvOES"

-- glGetFloatIndexedvEXT -------------------------------------------------------

-- | This command is an alias for 'glGetFloati_v'.
glGetFloatIndexedvEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TypeEnum@.
  -> GLuint -- ^ @index@.
  -> Ptr GLfloat -- ^ @data@ pointing to @COMPSIZE(target)@ elements of type @GLfloat@.
  -> m ()
glGetFloatIndexedvEXT v1 v2 v3 = liftIO $ dyn267 ptr_glGetFloatIndexedvEXT v1 v2 v3

{-# NOINLINE ptr_glGetFloatIndexedvEXT #-}
ptr_glGetFloatIndexedvEXT :: FunPtr (GLenum -> GLuint -> Ptr GLfloat -> IO ())
ptr_glGetFloatIndexedvEXT = unsafePerformIO $ getCommand "glGetFloatIndexedvEXT"

-- glGetFloati_v ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGet.xhtml OpenGL 4.x>.
glGetFloati_v
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TypeEnum@.
  -> GLuint -- ^ @index@.
  -> Ptr GLfloat -- ^ @data@ pointing to @COMPSIZE(target)@ elements of type @GLfloat@.
  -> m ()
glGetFloati_v v1 v2 v3 = liftIO $ dyn267 ptr_glGetFloati_v v1 v2 v3

{-# NOINLINE ptr_glGetFloati_v #-}
ptr_glGetFloati_v :: FunPtr (GLenum -> GLuint -> Ptr GLfloat -> IO ())
ptr_glGetFloati_v = unsafePerformIO $ getCommand "glGetFloati_v"

-- glGetFloati_vEXT ------------------------------------------------------------

-- | This command is an alias for 'glGetFloati_v'.
glGetFloati_vEXT
  :: MonadIO m
  => GLenum -- ^ @pname@ of type @TypeEnum@.
  -> GLuint -- ^ @index@.
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetFloati_vEXT v1 v2 v3 = liftIO $ dyn267 ptr_glGetFloati_vEXT v1 v2 v3

{-# NOINLINE ptr_glGetFloati_vEXT #-}
ptr_glGetFloati_vEXT :: FunPtr (GLenum -> GLuint -> Ptr GLfloat -> IO ())
ptr_glGetFloati_vEXT = unsafePerformIO $ getCommand "glGetFloati_vEXT"

-- glGetFloati_vNV -------------------------------------------------------------

-- | This command is an alias for 'glGetFloati_v'.
glGetFloati_vNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TypeEnum@.
  -> GLuint -- ^ @index@.
  -> Ptr GLfloat -- ^ @data@ pointing to @COMPSIZE(target)@ elements of type @GLfloat@.
  -> m ()
glGetFloati_vNV v1 v2 v3 = liftIO $ dyn267 ptr_glGetFloati_vNV v1 v2 v3

{-# NOINLINE ptr_glGetFloati_vNV #-}
ptr_glGetFloati_vNV :: FunPtr (GLenum -> GLuint -> Ptr GLfloat -> IO ())
ptr_glGetFloati_vNV = unsafePerformIO $ getCommand "glGetFloati_vNV"

-- glGetFloatv -----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGet.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGet.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGet.xhtml OpenGL 4.x>.
glGetFloatv
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [GetPName](Graphics-GL-Groups.html#GetPName).
  -> Ptr GLfloat -- ^ @data@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetFloatv v1 v2 = liftIO $ dyn94 ptr_glGetFloatv v1 v2

{-# NOINLINE ptr_glGetFloatv #-}
ptr_glGetFloatv :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glGetFloatv = unsafePerformIO $ getCommand "glGetFloatv"

-- glGetFogFuncSGIS ------------------------------------------------------------

glGetFogFuncSGIS
  :: MonadIO m
  => Ptr GLfloat -- ^ @points@ pointing to @COMPSIZE()@ elements of type @GLfloat@.
  -> m ()
glGetFogFuncSGIS v1 = liftIO $ dyn41 ptr_glGetFogFuncSGIS v1

{-# NOINLINE ptr_glGetFogFuncSGIS #-}
ptr_glGetFogFuncSGIS :: FunPtr (Ptr GLfloat -> IO ())
ptr_glGetFogFuncSGIS = unsafePerformIO $ getCommand "glGetFogFuncSGIS"

-- glGetFragDataIndex ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGetFragDataIndex.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetFragDataIndex.xhtml OpenGL 4.x>.
glGetFragDataIndex
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> Ptr GLchar -- ^ @name@.
  -> m GLint
glGetFragDataIndex v1 v2 = liftIO $ dyn310 ptr_glGetFragDataIndex v1 v2

{-# NOINLINE ptr_glGetFragDataIndex #-}
ptr_glGetFragDataIndex :: FunPtr (GLuint -> Ptr GLchar -> IO GLint)
ptr_glGetFragDataIndex = unsafePerformIO $ getCommand "glGetFragDataIndex"

-- glGetFragDataIndexEXT -------------------------------------------------------

-- | This command is an alias for 'glGetFragDataIndex'.
glGetFragDataIndexEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> Ptr GLchar -- ^ @name@.
  -> m GLint
glGetFragDataIndexEXT v1 v2 = liftIO $ dyn310 ptr_glGetFragDataIndexEXT v1 v2

{-# NOINLINE ptr_glGetFragDataIndexEXT #-}
ptr_glGetFragDataIndexEXT :: FunPtr (GLuint -> Ptr GLchar -> IO GLint)
ptr_glGetFragDataIndexEXT = unsafePerformIO $ getCommand "glGetFragDataIndexEXT"

-- glGetFragDataLocation -------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGetFragDataLocation.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetFragDataLocation.xhtml OpenGL 4.x>.
glGetFragDataLocation
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> Ptr GLchar -- ^ @name@ pointing to @COMPSIZE(name)@ elements of type @GLchar@.
  -> m GLint
glGetFragDataLocation v1 v2 = liftIO $ dyn310 ptr_glGetFragDataLocation v1 v2

{-# NOINLINE ptr_glGetFragDataLocation #-}
ptr_glGetFragDataLocation :: FunPtr (GLuint -> Ptr GLchar -> IO GLint)
ptr_glGetFragDataLocation = unsafePerformIO $ getCommand "glGetFragDataLocation"

-- glGetFragDataLocationEXT ----------------------------------------------------

-- | This command is an alias for 'glGetFragDataLocation'.
glGetFragDataLocationEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> Ptr GLchar -- ^ @name@ pointing to @COMPSIZE(name)@ elements of type @GLchar@.
  -> m GLint
glGetFragDataLocationEXT v1 v2 = liftIO $ dyn310 ptr_glGetFragDataLocationEXT v1 v2

{-# NOINLINE ptr_glGetFragDataLocationEXT #-}
ptr_glGetFragDataLocationEXT :: FunPtr (GLuint -> Ptr GLchar -> IO GLint)
ptr_glGetFragDataLocationEXT = unsafePerformIO $ getCommand "glGetFragDataLocationEXT"

-- glGetFragmentLightfvSGIX ----------------------------------------------------

glGetFragmentLightfvSGIX
  :: MonadIO m
  => GLenum -- ^ @light@ of type @FragmentLightNameSGIX@.
  -> GLenum -- ^ @pname@ of type @FragmentLightParameterSGIX@.
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetFragmentLightfvSGIX v1 v2 v3 = liftIO $ dyn132 ptr_glGetFragmentLightfvSGIX v1 v2 v3

{-# NOINLINE ptr_glGetFragmentLightfvSGIX #-}
ptr_glGetFragmentLightfvSGIX :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetFragmentLightfvSGIX = unsafePerformIO $ getCommand "glGetFragmentLightfvSGIX"

-- glGetFragmentLightivSGIX ----------------------------------------------------

glGetFragmentLightivSGIX
  :: MonadIO m
  => GLenum -- ^ @light@ of type @FragmentLightNameSGIX@.
  -> GLenum -- ^ @pname@ of type @FragmentLightParameterSGIX@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetFragmentLightivSGIX v1 v2 v3 = liftIO $ dyn133 ptr_glGetFragmentLightivSGIX v1 v2 v3

{-# NOINLINE ptr_glGetFragmentLightivSGIX #-}
ptr_glGetFragmentLightivSGIX :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetFragmentLightivSGIX = unsafePerformIO $ getCommand "glGetFragmentLightivSGIX"

-- glGetFragmentMaterialfvSGIX -------------------------------------------------

glGetFragmentMaterialfvSGIX
  :: MonadIO m
  => GLenum -- ^ @face@ of type [MaterialFace](Graphics-GL-Groups.html#MaterialFace).
  -> GLenum -- ^ @pname@ of type [MaterialParameter](Graphics-GL-Groups.html#MaterialParameter).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetFragmentMaterialfvSGIX v1 v2 v3 = liftIO $ dyn132 ptr_glGetFragmentMaterialfvSGIX v1 v2 v3

{-# NOINLINE ptr_glGetFragmentMaterialfvSGIX #-}
ptr_glGetFragmentMaterialfvSGIX :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetFragmentMaterialfvSGIX = unsafePerformIO $ getCommand "glGetFragmentMaterialfvSGIX"

-- glGetFragmentMaterialivSGIX -------------------------------------------------

glGetFragmentMaterialivSGIX
  :: MonadIO m
  => GLenum -- ^ @face@ of type [MaterialFace](Graphics-GL-Groups.html#MaterialFace).
  -> GLenum -- ^ @pname@ of type [MaterialParameter](Graphics-GL-Groups.html#MaterialParameter).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetFragmentMaterialivSGIX v1 v2 v3 = liftIO $ dyn133 ptr_glGetFragmentMaterialivSGIX v1 v2 v3

{-# NOINLINE ptr_glGetFragmentMaterialivSGIX #-}
ptr_glGetFragmentMaterialivSGIX :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetFragmentMaterialivSGIX = unsafePerformIO $ getCommand "glGetFragmentMaterialivSGIX"

-- glGetFramebufferAttachmentParameteriv ---------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGetFramebufferAttachmentParameter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetFramebufferAttachmentParameter.xhtml OpenGL 4.x>.
glGetFramebufferAttachmentParameteriv
  :: MonadIO m
  => GLenum -- ^ @target@ of type @FramebufferTarget@.
  -> GLenum -- ^ @attachment@ of type @FramebufferAttachment@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetFramebufferAttachmentParameteriv v1 v2 v3 v4 = liftIO $ dyn321 ptr_glGetFramebufferAttachmentParameteriv v1 v2 v3 v4

{-# NOINLINE ptr_glGetFramebufferAttachmentParameteriv #-}
ptr_glGetFramebufferAttachmentParameteriv :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetFramebufferAttachmentParameteriv = unsafePerformIO $ getCommand "glGetFramebufferAttachmentParameteriv"

-- glGetFramebufferAttachmentParameterivEXT ------------------------------------

-- | This command is an alias for 'glGetFramebufferAttachmentParameteriv'.
glGetFramebufferAttachmentParameterivEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type @FramebufferTarget@.
  -> GLenum -- ^ @attachment@ of type @FramebufferAttachment@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetFramebufferAttachmentParameterivEXT v1 v2 v3 v4 = liftIO $ dyn321 ptr_glGetFramebufferAttachmentParameterivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetFramebufferAttachmentParameterivEXT #-}
ptr_glGetFramebufferAttachmentParameterivEXT :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetFramebufferAttachmentParameterivEXT = unsafePerformIO $ getCommand "glGetFramebufferAttachmentParameterivEXT"

-- glGetFramebufferAttachmentParameterivOES ------------------------------------

glGetFramebufferAttachmentParameterivOES
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @attachment@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetFramebufferAttachmentParameterivOES v1 v2 v3 v4 = liftIO $ dyn321 ptr_glGetFramebufferAttachmentParameterivOES v1 v2 v3 v4

{-# NOINLINE ptr_glGetFramebufferAttachmentParameterivOES #-}
ptr_glGetFramebufferAttachmentParameterivOES :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetFramebufferAttachmentParameterivOES = unsafePerformIO $ getCommand "glGetFramebufferAttachmentParameterivOES"

-- glGetFramebufferParameteriv -------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetFramebufferParameter.xhtml OpenGL 4.x>.
glGetFramebufferParameteriv
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetFramebufferParameteriv v1 v2 v3 = liftIO $ dyn133 ptr_glGetFramebufferParameteriv v1 v2 v3

{-# NOINLINE ptr_glGetFramebufferParameteriv #-}
ptr_glGetFramebufferParameteriv :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetFramebufferParameteriv = unsafePerformIO $ getCommand "glGetFramebufferParameteriv"

-- glGetFramebufferParameterivEXT ----------------------------------------------

glGetFramebufferParameterivEXT
  :: MonadIO m
  => GLuint -- ^ @framebuffer@ of type @Framebuffer@.
  -> GLenum -- ^ @pname@ of type @GetFramebufferParameter@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetFramebufferParameterivEXT v1 v2 v3 = liftIO $ dyn334 ptr_glGetFramebufferParameterivEXT v1 v2 v3

{-# NOINLINE ptr_glGetFramebufferParameterivEXT #-}
ptr_glGetFramebufferParameterivEXT :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetFramebufferParameterivEXT = unsafePerformIO $ getCommand "glGetFramebufferParameterivEXT"

-- glGetFramebufferPixelLocalStorageSizeEXT ------------------------------------

glGetFramebufferPixelLocalStorageSizeEXT
  :: MonadIO m
  => GLuint -- ^ @target@.
  -> m GLsizei
glGetFramebufferPixelLocalStorageSizeEXT v1 = liftIO $ dyn335 ptr_glGetFramebufferPixelLocalStorageSizeEXT v1

{-# NOINLINE ptr_glGetFramebufferPixelLocalStorageSizeEXT #-}
ptr_glGetFramebufferPixelLocalStorageSizeEXT :: FunPtr (GLuint -> IO GLsizei)
ptr_glGetFramebufferPixelLocalStorageSizeEXT = unsafePerformIO $ getCommand "glGetFramebufferPixelLocalStorageSizeEXT"

-- glGetGraphicsResetStatus ----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetGraphicsResetStatus.xhtml OpenGL 4.x>.
glGetGraphicsResetStatus
  :: MonadIO m
  => m GLenum
glGetGraphicsResetStatus = liftIO $ dyn333 ptr_glGetGraphicsResetStatus

{-# NOINLINE ptr_glGetGraphicsResetStatus #-}
ptr_glGetGraphicsResetStatus :: FunPtr (IO GLenum)
ptr_glGetGraphicsResetStatus = unsafePerformIO $ getCommand "glGetGraphicsResetStatus"

-- glGetGraphicsResetStatusARB -------------------------------------------------

glGetGraphicsResetStatusARB
  :: MonadIO m
  => m GLenum
glGetGraphicsResetStatusARB = liftIO $ dyn333 ptr_glGetGraphicsResetStatusARB

{-# NOINLINE ptr_glGetGraphicsResetStatusARB #-}
ptr_glGetGraphicsResetStatusARB :: FunPtr (IO GLenum)
ptr_glGetGraphicsResetStatusARB = unsafePerformIO $ getCommand "glGetGraphicsResetStatusARB"

-- glGetGraphicsResetStatusEXT -------------------------------------------------

glGetGraphicsResetStatusEXT
  :: MonadIO m
  => m GLenum
glGetGraphicsResetStatusEXT = liftIO $ dyn333 ptr_glGetGraphicsResetStatusEXT

{-# NOINLINE ptr_glGetGraphicsResetStatusEXT #-}
ptr_glGetGraphicsResetStatusEXT :: FunPtr (IO GLenum)
ptr_glGetGraphicsResetStatusEXT = unsafePerformIO $ getCommand "glGetGraphicsResetStatusEXT"

-- glGetGraphicsResetStatusKHR -------------------------------------------------

-- | This command is an alias for 'glGetGraphicsResetStatus'.
glGetGraphicsResetStatusKHR
  :: MonadIO m
  => m GLenum
glGetGraphicsResetStatusKHR = liftIO $ dyn333 ptr_glGetGraphicsResetStatusKHR

{-# NOINLINE ptr_glGetGraphicsResetStatusKHR #-}
ptr_glGetGraphicsResetStatusKHR :: FunPtr (IO GLenum)
ptr_glGetGraphicsResetStatusKHR = unsafePerformIO $ getCommand "glGetGraphicsResetStatusKHR"

-- glGetHandleARB --------------------------------------------------------------

glGetHandleARB
  :: MonadIO m
  => GLenum -- ^ @pname@.
  -> m GLhandleARB -- ^ of type @handleARB@.
glGetHandleARB v1 = liftIO $ dyn198 ptr_glGetHandleARB v1

{-# NOINLINE ptr_glGetHandleARB #-}
ptr_glGetHandleARB :: FunPtr (GLenum -> IO GLhandleARB)
ptr_glGetHandleARB = unsafePerformIO $ getCommand "glGetHandleARB"

-- glGetHistogram --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetHistogram.xml OpenGL 2.x>.
glGetHistogram
  :: MonadIO m
  => GLenum -- ^ @target@ of type @HistogramTarget@.
  -> GLboolean -- ^ @reset@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @values@ pointing to @COMPSIZE(target,format,type)@ elements of type @a@.
  -> m ()
glGetHistogram v1 v2 v3 v4 v5 = liftIO $ dyn336 ptr_glGetHistogram v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetHistogram #-}
ptr_glGetHistogram :: FunPtr (GLenum -> GLboolean -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glGetHistogram = unsafePerformIO $ getCommand "glGetHistogram"

-- glGetHistogramEXT -----------------------------------------------------------

glGetHistogramEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [HistogramTargetEXT](Graphics-GL-Groups.html#HistogramTargetEXT).
  -> GLboolean -- ^ @reset@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @values@ pointing to @COMPSIZE(target,format,type)@ elements of type @a@.
  -> m ()
glGetHistogramEXT v1 v2 v3 v4 v5 = liftIO $ dyn336 ptr_glGetHistogramEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetHistogramEXT #-}
ptr_glGetHistogramEXT :: FunPtr (GLenum -> GLboolean -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glGetHistogramEXT = unsafePerformIO $ getCommand "glGetHistogramEXT"

-- glGetHistogramParameterfv ---------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetHistogramParameter.xml OpenGL 2.x>.
glGetHistogramParameterfv
  :: MonadIO m
  => GLenum -- ^ @target@ of type @HistogramTarget@.
  -> GLenum -- ^ @pname@ of type @GetHistogramParameterPName@.
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetHistogramParameterfv v1 v2 v3 = liftIO $ dyn132 ptr_glGetHistogramParameterfv v1 v2 v3

{-# NOINLINE ptr_glGetHistogramParameterfv #-}
ptr_glGetHistogramParameterfv :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetHistogramParameterfv = unsafePerformIO $ getCommand "glGetHistogramParameterfv"

-- glGetHistogramParameterfvEXT ------------------------------------------------

glGetHistogramParameterfvEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [HistogramTargetEXT](Graphics-GL-Groups.html#HistogramTargetEXT).
  -> GLenum -- ^ @pname@ of type [GetHistogramParameterPNameEXT](Graphics-GL-Groups.html#GetHistogramParameterPNameEXT).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetHistogramParameterfvEXT v1 v2 v3 = liftIO $ dyn132 ptr_glGetHistogramParameterfvEXT v1 v2 v3

{-# NOINLINE ptr_glGetHistogramParameterfvEXT #-}
ptr_glGetHistogramParameterfvEXT :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetHistogramParameterfvEXT = unsafePerformIO $ getCommand "glGetHistogramParameterfvEXT"

-- glGetHistogramParameteriv ---------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetHistogramParameter.xml OpenGL 2.x>.
glGetHistogramParameteriv
  :: MonadIO m
  => GLenum -- ^ @target@ of type @HistogramTarget@.
  -> GLenum -- ^ @pname@ of type @GetHistogramParameterPName@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetHistogramParameteriv v1 v2 v3 = liftIO $ dyn133 ptr_glGetHistogramParameteriv v1 v2 v3

{-# NOINLINE ptr_glGetHistogramParameteriv #-}
ptr_glGetHistogramParameteriv :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetHistogramParameteriv = unsafePerformIO $ getCommand "glGetHistogramParameteriv"

-- glGetHistogramParameterivEXT ------------------------------------------------

glGetHistogramParameterivEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [HistogramTargetEXT](Graphics-GL-Groups.html#HistogramTargetEXT).
  -> GLenum -- ^ @pname@ of type [GetHistogramParameterPNameEXT](Graphics-GL-Groups.html#GetHistogramParameterPNameEXT).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetHistogramParameterivEXT v1 v2 v3 = liftIO $ dyn133 ptr_glGetHistogramParameterivEXT v1 v2 v3

{-# NOINLINE ptr_glGetHistogramParameterivEXT #-}
ptr_glGetHistogramParameterivEXT :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetHistogramParameterivEXT = unsafePerformIO $ getCommand "glGetHistogramParameterivEXT"

-- glGetHistogramParameterxvOES ------------------------------------------------

glGetHistogramParameterxvOES
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLfixed -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfixed@.
  -> m ()
glGetHistogramParameterxvOES v1 v2 v3 = liftIO $ dyn163 ptr_glGetHistogramParameterxvOES v1 v2 v3

{-# NOINLINE ptr_glGetHistogramParameterxvOES #-}
ptr_glGetHistogramParameterxvOES :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
ptr_glGetHistogramParameterxvOES = unsafePerformIO $ getCommand "glGetHistogramParameterxvOES"

-- glGetImageHandleARB ---------------------------------------------------------

glGetImageHandleARB
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLboolean -- ^ @layered@.
  -> GLint -- ^ @layer@.
  -> GLenum -- ^ @format@.
  -> m GLuint64
glGetImageHandleARB v1 v2 v3 v4 v5 = liftIO $ dyn337 ptr_glGetImageHandleARB v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetImageHandleARB #-}
ptr_glGetImageHandleARB :: FunPtr (GLuint -> GLint -> GLboolean -> GLint -> GLenum -> IO GLuint64)
ptr_glGetImageHandleARB = unsafePerformIO $ getCommand "glGetImageHandleARB"

