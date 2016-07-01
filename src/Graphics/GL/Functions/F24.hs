--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F24
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

module Graphics.GL.Functions.F24 (
  glGetImageHandleNV,
  glGetImageTransformParameterfvHP,
  glGetImageTransformParameterivHP,
  glGetInfoLogARB,
  glGetInstrumentsSGIX,
  glGetInteger64i_v,
  glGetInteger64v,
  glGetInteger64vAPPLE,
  glGetIntegerIndexedvEXT,
  glGetIntegeri_v,
  glGetIntegeri_vEXT,
  glGetIntegerui64i_vNV,
  glGetIntegerui64vNV,
  glGetIntegerv,
  glGetInternalformatSampleivNV,
  glGetInternalformati64v,
  glGetInternalformativ,
  glGetInvariantBooleanvEXT,
  glGetInvariantFloatvEXT,
  glGetInvariantIntegervEXT,
  glGetLightfv,
  glGetLightiv,
  glGetLightxOES,
  glGetLightxv,
  glGetLightxvOES,
  glGetListParameterfvSGIX,
  glGetListParameterivSGIX,
  glGetLocalConstantBooleanvEXT,
  glGetLocalConstantFloatvEXT,
  glGetLocalConstantIntegervEXT,
  glGetMapAttribParameterfvNV,
  glGetMapAttribParameterivNV,
  glGetMapControlPointsNV,
  glGetMapParameterfvNV,
  glGetMapParameterivNV,
  glGetMapdv,
  glGetMapfv,
  glGetMapiv,
  glGetMapxvOES,
  glGetMaterialfv
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

-- glGetImageHandleNV ----------------------------------------------------------

glGetImageHandleNV
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLboolean -- ^ @layered@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLint -- ^ @layer@.
  -> GLenum -- ^ @format@.
  -> m GLuint64
glGetImageHandleNV v1 v2 v3 v4 v5 = liftIO $ dyn337 ptr_glGetImageHandleNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetImageHandleNV #-}
ptr_glGetImageHandleNV :: FunPtr (GLuint -> GLint -> GLboolean -> GLint -> GLenum -> IO GLuint64)
ptr_glGetImageHandleNV = unsafePerformIO $ getCommand "glGetImageHandleNV"

-- glGetImageTransformParameterfvHP --------------------------------------------

glGetImageTransformParameterfvHP
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ImageTransformTargetHP@.
  -> GLenum -- ^ @pname@ of type @ImageTransformPNameHP@.
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetImageTransformParameterfvHP v1 v2 v3 = liftIO $ dyn132 ptr_glGetImageTransformParameterfvHP v1 v2 v3

{-# NOINLINE ptr_glGetImageTransformParameterfvHP #-}
ptr_glGetImageTransformParameterfvHP :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetImageTransformParameterfvHP = unsafePerformIO $ getCommand "glGetImageTransformParameterfvHP"

-- glGetImageTransformParameterivHP --------------------------------------------

glGetImageTransformParameterivHP
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ImageTransformTargetHP@.
  -> GLenum -- ^ @pname@ of type @ImageTransformPNameHP@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetImageTransformParameterivHP v1 v2 v3 = liftIO $ dyn133 ptr_glGetImageTransformParameterivHP v1 v2 v3

{-# NOINLINE ptr_glGetImageTransformParameterivHP #-}
ptr_glGetImageTransformParameterivHP :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetImageTransformParameterivHP = unsafePerformIO $ getCommand "glGetImageTransformParameterivHP"

-- glGetInfoLogARB -------------------------------------------------------------

glGetInfoLogARB
  :: MonadIO m
  => GLhandleARB -- ^ @obj@ of type @handleARB@.
  -> GLsizei -- ^ @maxLength@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLcharARB -- ^ @infoLog@ pointing to @maxLength@ elements of type @GLcharARB@.
  -> m ()
glGetInfoLogARB v1 v2 v3 v4 = liftIO $ dyn338 ptr_glGetInfoLogARB v1 v2 v3 v4

{-# NOINLINE ptr_glGetInfoLogARB #-}
ptr_glGetInfoLogARB :: FunPtr (GLhandleARB -> GLsizei -> Ptr GLsizei -> Ptr GLcharARB -> IO ())
ptr_glGetInfoLogARB = unsafePerformIO $ getCommand "glGetInfoLogARB"

-- glGetInstrumentsSGIX --------------------------------------------------------

glGetInstrumentsSGIX
  :: MonadIO m
  => m GLint
glGetInstrumentsSGIX = liftIO $ dyn339 ptr_glGetInstrumentsSGIX

{-# NOINLINE ptr_glGetInstrumentsSGIX #-}
ptr_glGetInstrumentsSGIX :: FunPtr (IO GLint)
ptr_glGetInstrumentsSGIX = unsafePerformIO $ getCommand "glGetInstrumentsSGIX"

-- glGetInteger64i_v -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGet.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGet.xhtml OpenGL 4.x>.
glGetInteger64i_v
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLuint -- ^ @index@.
  -> Ptr GLint64 -- ^ @data@ pointing to @COMPSIZE(target)@ elements of type @GLint64@.
  -> m ()
glGetInteger64i_v v1 v2 v3 = liftIO $ dyn340 ptr_glGetInteger64i_v v1 v2 v3

{-# NOINLINE ptr_glGetInteger64i_v #-}
ptr_glGetInteger64i_v :: FunPtr (GLenum -> GLuint -> Ptr GLint64 -> IO ())
ptr_glGetInteger64i_v = unsafePerformIO $ getCommand "glGetInteger64i_v"

-- glGetInteger64v -------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGet.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGet.xhtml OpenGL 4.x>.
glGetInteger64v
  :: MonadIO m
  => GLenum -- ^ @pname@.
  -> Ptr GLint64 -- ^ @data@ pointing to @COMPSIZE(pname)@ elements of type @GLint64@.
  -> m ()
glGetInteger64v v1 v2 = liftIO $ dyn341 ptr_glGetInteger64v v1 v2

{-# NOINLINE ptr_glGetInteger64v #-}
ptr_glGetInteger64v :: FunPtr (GLenum -> Ptr GLint64 -> IO ())
ptr_glGetInteger64v = unsafePerformIO $ getCommand "glGetInteger64v"

-- glGetInteger64vAPPLE --------------------------------------------------------

-- | This command is an alias for 'glGetInteger64v'.
glGetInteger64vAPPLE
  :: MonadIO m
  => GLenum -- ^ @pname@.
  -> Ptr GLint64 -- ^ @params@.
  -> m ()
glGetInteger64vAPPLE v1 v2 = liftIO $ dyn341 ptr_glGetInteger64vAPPLE v1 v2

{-# NOINLINE ptr_glGetInteger64vAPPLE #-}
ptr_glGetInteger64vAPPLE :: FunPtr (GLenum -> Ptr GLint64 -> IO ())
ptr_glGetInteger64vAPPLE = unsafePerformIO $ getCommand "glGetInteger64vAPPLE"

-- glGetIntegerIndexedvEXT -----------------------------------------------------

-- | This command is an alias for 'glGetIntegeri_v'.
glGetIntegerIndexedvEXT
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLuint -- ^ @index@.
  -> Ptr GLint -- ^ @data@ pointing to @COMPSIZE(target)@ elements of type @GLint@.
  -> m ()
glGetIntegerIndexedvEXT v1 v2 v3 = liftIO $ dyn342 ptr_glGetIntegerIndexedvEXT v1 v2 v3

{-# NOINLINE ptr_glGetIntegerIndexedvEXT #-}
ptr_glGetIntegerIndexedvEXT :: FunPtr (GLenum -> GLuint -> Ptr GLint -> IO ())
ptr_glGetIntegerIndexedvEXT = unsafePerformIO $ getCommand "glGetIntegerIndexedvEXT"

-- glGetIntegeri_v -------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGet.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGet.xhtml OpenGL 4.x>.
glGetIntegeri_v
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLuint -- ^ @index@.
  -> Ptr GLint -- ^ @data@ pointing to @COMPSIZE(target)@ elements of type @GLint@.
  -> m ()
glGetIntegeri_v v1 v2 v3 = liftIO $ dyn342 ptr_glGetIntegeri_v v1 v2 v3

{-# NOINLINE ptr_glGetIntegeri_v #-}
ptr_glGetIntegeri_v :: FunPtr (GLenum -> GLuint -> Ptr GLint -> IO ())
ptr_glGetIntegeri_v = unsafePerformIO $ getCommand "glGetIntegeri_v"

-- glGetIntegeri_vEXT ----------------------------------------------------------

glGetIntegeri_vEXT
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLuint -- ^ @index@.
  -> Ptr GLint -- ^ @data@.
  -> m ()
glGetIntegeri_vEXT v1 v2 v3 = liftIO $ dyn342 ptr_glGetIntegeri_vEXT v1 v2 v3

{-# NOINLINE ptr_glGetIntegeri_vEXT #-}
ptr_glGetIntegeri_vEXT :: FunPtr (GLenum -> GLuint -> Ptr GLint -> IO ())
ptr_glGetIntegeri_vEXT = unsafePerformIO $ getCommand "glGetIntegeri_vEXT"

-- glGetIntegerui64i_vNV -------------------------------------------------------

glGetIntegerui64i_vNV
  :: MonadIO m
  => GLenum -- ^ @value@.
  -> GLuint -- ^ @index@.
  -> Ptr GLuint64EXT -- ^ @result@ pointing to @COMPSIZE(value)@ elements of type @GLuint64EXT@.
  -> m ()
glGetIntegerui64i_vNV v1 v2 v3 = liftIO $ dyn343 ptr_glGetIntegerui64i_vNV v1 v2 v3

{-# NOINLINE ptr_glGetIntegerui64i_vNV #-}
ptr_glGetIntegerui64i_vNV :: FunPtr (GLenum -> GLuint -> Ptr GLuint64EXT -> IO ())
ptr_glGetIntegerui64i_vNV = unsafePerformIO $ getCommand "glGetIntegerui64i_vNV"

-- glGetIntegerui64vNV ---------------------------------------------------------

glGetIntegerui64vNV
  :: MonadIO m
  => GLenum -- ^ @value@.
  -> Ptr GLuint64EXT -- ^ @result@ pointing to @COMPSIZE(value)@ elements of type @GLuint64EXT@.
  -> m ()
glGetIntegerui64vNV v1 v2 = liftIO $ dyn344 ptr_glGetIntegerui64vNV v1 v2

{-# NOINLINE ptr_glGetIntegerui64vNV #-}
ptr_glGetIntegerui64vNV :: FunPtr (GLenum -> Ptr GLuint64EXT -> IO ())
ptr_glGetIntegerui64vNV = unsafePerformIO $ getCommand "glGetIntegerui64vNV"

-- glGetIntegerv ---------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGet.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGet.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGet.xhtml OpenGL 4.x>.
glGetIntegerv
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [GetPName](Graphics-GL-Groups.html#GetPName).
  -> Ptr GLint -- ^ @data@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetIntegerv v1 v2 = liftIO $ dyn136 ptr_glGetIntegerv v1 v2

{-# NOINLINE ptr_glGetIntegerv #-}
ptr_glGetIntegerv :: FunPtr (GLenum -> Ptr GLint -> IO ())
ptr_glGetIntegerv = unsafePerformIO $ getCommand "glGetIntegerv"

-- glGetInternalformatSampleivNV -----------------------------------------------

glGetInternalformatSampleivNV
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @internalformat@.
  -> GLsizei -- ^ @samples@.
  -> GLenum -- ^ @pname@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLint -- ^ @params@ pointing to @bufSize@ elements of type @GLint@.
  -> m ()
glGetInternalformatSampleivNV v1 v2 v3 v4 v5 v6 = liftIO $ dyn345 ptr_glGetInternalformatSampleivNV v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glGetInternalformatSampleivNV #-}
ptr_glGetInternalformatSampleivNV :: FunPtr (GLenum -> GLenum -> GLsizei -> GLenum -> GLsizei -> Ptr GLint -> IO ())
ptr_glGetInternalformatSampleivNV = unsafePerformIO $ getCommand "glGetInternalformatSampleivNV"

-- glGetInternalformati64v -----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetInternalformat.xhtml OpenGL 4.x>.
glGetInternalformati64v
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @internalformat@.
  -> GLenum -- ^ @pname@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLint64 -- ^ @params@ pointing to @bufSize@ elements of type @GLint64@.
  -> m ()
glGetInternalformati64v v1 v2 v3 v4 v5 = liftIO $ dyn346 ptr_glGetInternalformati64v v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetInternalformati64v #-}
ptr_glGetInternalformati64v :: FunPtr (GLenum -> GLenum -> GLenum -> GLsizei -> Ptr GLint64 -> IO ())
ptr_glGetInternalformati64v = unsafePerformIO $ getCommand "glGetInternalformati64v"

-- glGetInternalformativ -------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetInternalformat.xhtml OpenGL 4.x>.
glGetInternalformativ
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @internalformat@.
  -> GLenum -- ^ @pname@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLint -- ^ @params@ pointing to @bufSize@ elements of type @GLint@.
  -> m ()
glGetInternalformativ v1 v2 v3 v4 v5 = liftIO $ dyn347 ptr_glGetInternalformativ v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetInternalformativ #-}
ptr_glGetInternalformativ :: FunPtr (GLenum -> GLenum -> GLenum -> GLsizei -> Ptr GLint -> IO ())
ptr_glGetInternalformativ = unsafePerformIO $ getCommand "glGetInternalformativ"

-- glGetInvariantBooleanvEXT ---------------------------------------------------

glGetInvariantBooleanvEXT
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @value@ of type @GetVariantValueEXT@.
  -> Ptr GLboolean -- ^ @data@ pointing to @COMPSIZE(id)@ elements of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glGetInvariantBooleanvEXT v1 v2 v3 = liftIO $ dyn348 ptr_glGetInvariantBooleanvEXT v1 v2 v3

{-# NOINLINE ptr_glGetInvariantBooleanvEXT #-}
ptr_glGetInvariantBooleanvEXT :: FunPtr (GLuint -> GLenum -> Ptr GLboolean -> IO ())
ptr_glGetInvariantBooleanvEXT = unsafePerformIO $ getCommand "glGetInvariantBooleanvEXT"

-- glGetInvariantFloatvEXT -----------------------------------------------------

glGetInvariantFloatvEXT
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @value@ of type @GetVariantValueEXT@.
  -> Ptr GLfloat -- ^ @data@ pointing to @COMPSIZE(id)@ elements of type @GLfloat@.
  -> m ()
glGetInvariantFloatvEXT v1 v2 v3 = liftIO $ dyn349 ptr_glGetInvariantFloatvEXT v1 v2 v3

{-# NOINLINE ptr_glGetInvariantFloatvEXT #-}
ptr_glGetInvariantFloatvEXT :: FunPtr (GLuint -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetInvariantFloatvEXT = unsafePerformIO $ getCommand "glGetInvariantFloatvEXT"

-- glGetInvariantIntegervEXT ---------------------------------------------------

glGetInvariantIntegervEXT
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @value@ of type @GetVariantValueEXT@.
  -> Ptr GLint -- ^ @data@ pointing to @COMPSIZE(id)@ elements of type @GLint@.
  -> m ()
glGetInvariantIntegervEXT v1 v2 v3 = liftIO $ dyn334 ptr_glGetInvariantIntegervEXT v1 v2 v3

{-# NOINLINE ptr_glGetInvariantIntegervEXT #-}
ptr_glGetInvariantIntegervEXT :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetInvariantIntegervEXT = unsafePerformIO $ getCommand "glGetInvariantIntegervEXT"

-- glGetLightfv ----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetLight.xml OpenGL 2.x>.
glGetLightfv
  :: MonadIO m
  => GLenum -- ^ @light@ of type [LightName](Graphics-GL-Groups.html#LightName).
  -> GLenum -- ^ @pname@ of type [LightParameter](Graphics-GL-Groups.html#LightParameter).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetLightfv v1 v2 v3 = liftIO $ dyn132 ptr_glGetLightfv v1 v2 v3

{-# NOINLINE ptr_glGetLightfv #-}
ptr_glGetLightfv :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetLightfv = unsafePerformIO $ getCommand "glGetLightfv"

-- glGetLightiv ----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetLight.xml OpenGL 2.x>.
glGetLightiv
  :: MonadIO m
  => GLenum -- ^ @light@ of type [LightName](Graphics-GL-Groups.html#LightName).
  -> GLenum -- ^ @pname@ of type [LightParameter](Graphics-GL-Groups.html#LightParameter).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetLightiv v1 v2 v3 = liftIO $ dyn133 ptr_glGetLightiv v1 v2 v3

{-# NOINLINE ptr_glGetLightiv #-}
ptr_glGetLightiv :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetLightiv = unsafePerformIO $ getCommand "glGetLightiv"

-- glGetLightxOES --------------------------------------------------------------

glGetLightxOES
  :: MonadIO m
  => GLenum -- ^ @light@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLfixed -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfixed@.
  -> m ()
glGetLightxOES v1 v2 v3 = liftIO $ dyn163 ptr_glGetLightxOES v1 v2 v3

{-# NOINLINE ptr_glGetLightxOES #-}
ptr_glGetLightxOES :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
ptr_glGetLightxOES = unsafePerformIO $ getCommand "glGetLightxOES"

-- glGetLightxv ----------------------------------------------------------------

glGetLightxv
  :: MonadIO m
  => GLenum -- ^ @light@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLfixed -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfixed@.
  -> m ()
glGetLightxv v1 v2 v3 = liftIO $ dyn163 ptr_glGetLightxv v1 v2 v3

{-# NOINLINE ptr_glGetLightxv #-}
ptr_glGetLightxv :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
ptr_glGetLightxv = unsafePerformIO $ getCommand "glGetLightxv"

-- glGetLightxvOES -------------------------------------------------------------

glGetLightxvOES
  :: MonadIO m
  => GLenum -- ^ @light@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLfixed -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfixed@.
  -> m ()
glGetLightxvOES v1 v2 v3 = liftIO $ dyn163 ptr_glGetLightxvOES v1 v2 v3

{-# NOINLINE ptr_glGetLightxvOES #-}
ptr_glGetLightxvOES :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
ptr_glGetLightxvOES = unsafePerformIO $ getCommand "glGetLightxvOES"

-- glGetListParameterfvSGIX ----------------------------------------------------

glGetListParameterfvSGIX
  :: MonadIO m
  => GLuint -- ^ @list@ of type @List@.
  -> GLenum -- ^ @pname@ of type [ListParameterName](Graphics-GL-Groups.html#ListParameterName).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedFloat32@.
  -> m ()
glGetListParameterfvSGIX v1 v2 v3 = liftIO $ dyn349 ptr_glGetListParameterfvSGIX v1 v2 v3

{-# NOINLINE ptr_glGetListParameterfvSGIX #-}
ptr_glGetListParameterfvSGIX :: FunPtr (GLuint -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetListParameterfvSGIX = unsafePerformIO $ getCommand "glGetListParameterfvSGIX"

-- glGetListParameterivSGIX ----------------------------------------------------

glGetListParameterivSGIX
  :: MonadIO m
  => GLuint -- ^ @list@ of type @List@.
  -> GLenum -- ^ @pname@ of type [ListParameterName](Graphics-GL-Groups.html#ListParameterName).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedInt32@.
  -> m ()
glGetListParameterivSGIX v1 v2 v3 = liftIO $ dyn334 ptr_glGetListParameterivSGIX v1 v2 v3

{-# NOINLINE ptr_glGetListParameterivSGIX #-}
ptr_glGetListParameterivSGIX :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetListParameterivSGIX = unsafePerformIO $ getCommand "glGetListParameterivSGIX"

-- glGetLocalConstantBooleanvEXT -----------------------------------------------

glGetLocalConstantBooleanvEXT
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @value@ of type @GetVariantValueEXT@.
  -> Ptr GLboolean -- ^ @data@ pointing to @COMPSIZE(id)@ elements of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glGetLocalConstantBooleanvEXT v1 v2 v3 = liftIO $ dyn348 ptr_glGetLocalConstantBooleanvEXT v1 v2 v3

{-# NOINLINE ptr_glGetLocalConstantBooleanvEXT #-}
ptr_glGetLocalConstantBooleanvEXT :: FunPtr (GLuint -> GLenum -> Ptr GLboolean -> IO ())
ptr_glGetLocalConstantBooleanvEXT = unsafePerformIO $ getCommand "glGetLocalConstantBooleanvEXT"

-- glGetLocalConstantFloatvEXT -------------------------------------------------

glGetLocalConstantFloatvEXT
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @value@ of type @GetVariantValueEXT@.
  -> Ptr GLfloat -- ^ @data@ pointing to @COMPSIZE(id)@ elements of type @GLfloat@.
  -> m ()
glGetLocalConstantFloatvEXT v1 v2 v3 = liftIO $ dyn349 ptr_glGetLocalConstantFloatvEXT v1 v2 v3

{-# NOINLINE ptr_glGetLocalConstantFloatvEXT #-}
ptr_glGetLocalConstantFloatvEXT :: FunPtr (GLuint -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetLocalConstantFloatvEXT = unsafePerformIO $ getCommand "glGetLocalConstantFloatvEXT"

-- glGetLocalConstantIntegervEXT -----------------------------------------------

glGetLocalConstantIntegervEXT
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @value@ of type @GetVariantValueEXT@.
  -> Ptr GLint -- ^ @data@ pointing to @COMPSIZE(id)@ elements of type @GLint@.
  -> m ()
glGetLocalConstantIntegervEXT v1 v2 v3 = liftIO $ dyn334 ptr_glGetLocalConstantIntegervEXT v1 v2 v3

{-# NOINLINE ptr_glGetLocalConstantIntegervEXT #-}
ptr_glGetLocalConstantIntegervEXT :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetLocalConstantIntegervEXT = unsafePerformIO $ getCommand "glGetLocalConstantIntegervEXT"

-- glGetMapAttribParameterfvNV -------------------------------------------------

glGetMapAttribParameterfvNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type @EvalTargetNV@.
  -> GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@ of type @MapAttribParameterNV@.
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetMapAttribParameterfvNV v1 v2 v3 v4 = liftIO $ dyn350 ptr_glGetMapAttribParameterfvNV v1 v2 v3 v4

{-# NOINLINE ptr_glGetMapAttribParameterfvNV #-}
ptr_glGetMapAttribParameterfvNV :: FunPtr (GLenum -> GLuint -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetMapAttribParameterfvNV = unsafePerformIO $ getCommand "glGetMapAttribParameterfvNV"

-- glGetMapAttribParameterivNV -------------------------------------------------

glGetMapAttribParameterivNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type @EvalTargetNV@.
  -> GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@ of type @MapAttribParameterNV@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetMapAttribParameterivNV v1 v2 v3 v4 = liftIO $ dyn351 ptr_glGetMapAttribParameterivNV v1 v2 v3 v4

{-# NOINLINE ptr_glGetMapAttribParameterivNV #-}
ptr_glGetMapAttribParameterivNV :: FunPtr (GLenum -> GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetMapAttribParameterivNV = unsafePerformIO $ getCommand "glGetMapAttribParameterivNV"

-- glGetMapControlPointsNV -----------------------------------------------------

glGetMapControlPointsNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type @EvalTargetNV@.
  -> GLuint -- ^ @index@.
  -> GLenum -- ^ @type@ of type @MapTypeNV@.
  -> GLsizei -- ^ @ustride@.
  -> GLsizei -- ^ @vstride@.
  -> GLboolean -- ^ @packed@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr a -- ^ @points@ pointing to @COMPSIZE(target)@ elements of type @a@.
  -> m ()
glGetMapControlPointsNV v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn352 ptr_glGetMapControlPointsNV v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glGetMapControlPointsNV #-}
ptr_glGetMapControlPointsNV :: FunPtr (GLenum -> GLuint -> GLenum -> GLsizei -> GLsizei -> GLboolean -> Ptr a -> IO ())
ptr_glGetMapControlPointsNV = unsafePerformIO $ getCommand "glGetMapControlPointsNV"

-- glGetMapParameterfvNV -------------------------------------------------------

glGetMapParameterfvNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type @EvalTargetNV@.
  -> GLenum -- ^ @pname@ of type @MapParameterNV@.
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(target,pname)@ elements of type @GLfloat@.
  -> m ()
glGetMapParameterfvNV v1 v2 v3 = liftIO $ dyn132 ptr_glGetMapParameterfvNV v1 v2 v3

{-# NOINLINE ptr_glGetMapParameterfvNV #-}
ptr_glGetMapParameterfvNV :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetMapParameterfvNV = unsafePerformIO $ getCommand "glGetMapParameterfvNV"

-- glGetMapParameterivNV -------------------------------------------------------

glGetMapParameterivNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type @EvalTargetNV@.
  -> GLenum -- ^ @pname@ of type @MapParameterNV@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(target,pname)@ elements of type @GLint@.
  -> m ()
glGetMapParameterivNV v1 v2 v3 = liftIO $ dyn133 ptr_glGetMapParameterivNV v1 v2 v3

{-# NOINLINE ptr_glGetMapParameterivNV #-}
ptr_glGetMapParameterivNV :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetMapParameterivNV = unsafePerformIO $ getCommand "glGetMapParameterivNV"

-- glGetMapdv ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetMap.xml OpenGL 2.x>.
glGetMapdv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [MapTarget](Graphics-GL-Groups.html#MapTarget).
  -> GLenum -- ^ @query@ of type [GetMapQuery](Graphics-GL-Groups.html#GetMapQuery).
  -> Ptr GLdouble -- ^ @v@ pointing to @COMPSIZE(target,query)@ elements of type @GLdouble@.
  -> m ()
glGetMapdv v1 v2 v3 = liftIO $ dyn353 ptr_glGetMapdv v1 v2 v3

{-# NOINLINE ptr_glGetMapdv #-}
ptr_glGetMapdv :: FunPtr (GLenum -> GLenum -> Ptr GLdouble -> IO ())
ptr_glGetMapdv = unsafePerformIO $ getCommand "glGetMapdv"

-- glGetMapfv ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetMap.xml OpenGL 2.x>.
glGetMapfv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [MapTarget](Graphics-GL-Groups.html#MapTarget).
  -> GLenum -- ^ @query@ of type [GetMapQuery](Graphics-GL-Groups.html#GetMapQuery).
  -> Ptr GLfloat -- ^ @v@ pointing to @COMPSIZE(target,query)@ elements of type @GLfloat@.
  -> m ()
glGetMapfv v1 v2 v3 = liftIO $ dyn132 ptr_glGetMapfv v1 v2 v3

{-# NOINLINE ptr_glGetMapfv #-}
ptr_glGetMapfv :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetMapfv = unsafePerformIO $ getCommand "glGetMapfv"

-- glGetMapiv ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetMap.xml OpenGL 2.x>.
glGetMapiv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [MapTarget](Graphics-GL-Groups.html#MapTarget).
  -> GLenum -- ^ @query@ of type [GetMapQuery](Graphics-GL-Groups.html#GetMapQuery).
  -> Ptr GLint -- ^ @v@ pointing to @COMPSIZE(target,query)@ elements of type @GLint@.
  -> m ()
glGetMapiv v1 v2 v3 = liftIO $ dyn133 ptr_glGetMapiv v1 v2 v3

{-# NOINLINE ptr_glGetMapiv #-}
ptr_glGetMapiv :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetMapiv = unsafePerformIO $ getCommand "glGetMapiv"

-- glGetMapxvOES ---------------------------------------------------------------

glGetMapxvOES
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @query@.
  -> Ptr GLfixed -- ^ @v@ pointing to @COMPSIZE(query)@ elements of type @GLfixed@.
  -> m ()
glGetMapxvOES v1 v2 v3 = liftIO $ dyn163 ptr_glGetMapxvOES v1 v2 v3

{-# NOINLINE ptr_glGetMapxvOES #-}
ptr_glGetMapxvOES :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
ptr_glGetMapxvOES = unsafePerformIO $ getCommand "glGetMapxvOES"

-- glGetMaterialfv -------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetMaterial.xml OpenGL 2.x>.
glGetMaterialfv
  :: MonadIO m
  => GLenum -- ^ @face@ of type [MaterialFace](Graphics-GL-Groups.html#MaterialFace).
  -> GLenum -- ^ @pname@ of type [MaterialParameter](Graphics-GL-Groups.html#MaterialParameter).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetMaterialfv v1 v2 v3 = liftIO $ dyn132 ptr_glGetMaterialfv v1 v2 v3

{-# NOINLINE ptr_glGetMaterialfv #-}
ptr_glGetMaterialfv :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetMaterialfv = unsafePerformIO $ getCommand "glGetMaterialfv"

