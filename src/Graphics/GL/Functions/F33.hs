--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F33
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

module Graphics.GL.Functions.F33 (
  glGetnUniformdv,
  glGetnUniformdvARB,
  glGetnUniformfv,
  glGetnUniformfvARB,
  glGetnUniformfvEXT,
  glGetnUniformfvKHR,
  glGetnUniformi64vARB,
  glGetnUniformiv,
  glGetnUniformivARB,
  glGetnUniformivEXT,
  glGetnUniformivKHR,
  glGetnUniformui64vARB,
  glGetnUniformuiv,
  glGetnUniformuivARB,
  glGetnUniformuivKHR,
  glGlobalAlphaFactorbSUN,
  glGlobalAlphaFactordSUN,
  glGlobalAlphaFactorfSUN,
  glGlobalAlphaFactoriSUN,
  glGlobalAlphaFactorsSUN,
  glGlobalAlphaFactorubSUN,
  glGlobalAlphaFactoruiSUN,
  glGlobalAlphaFactorusSUN,
  glHint,
  glHintPGI,
  glHistogram,
  glHistogramEXT,
  glIglooInterfaceSGIX,
  glImageTransformParameterfHP,
  glImageTransformParameterfvHP,
  glImageTransformParameteriHP,
  glImageTransformParameterivHP,
  glImportSyncEXT,
  glIndexFormatNV,
  glIndexFuncEXT,
  glIndexMask,
  glIndexMaterialEXT,
  glIndexPointer,
  glIndexPointerEXT,
  glIndexPointerListIBM
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

-- glGetnUniformdv -------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetUniform.xhtml OpenGL 4.x>.
glGetnUniformdv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLdouble -- ^ @params@.
  -> m ()
glGetnUniformdv v1 v2 v3 v4 = liftIO $ dyn456 ptr_glGetnUniformdv v1 v2 v3 v4

{-# NOINLINE ptr_glGetnUniformdv #-}
ptr_glGetnUniformdv :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLdouble -> IO ())
ptr_glGetnUniformdv = unsafePerformIO $ getCommand "glGetnUniformdv"

-- glGetnUniformdvARB ----------------------------------------------------------

glGetnUniformdvARB
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLdouble -- ^ @params@ pointing to @bufSize@ elements of type @GLdouble@.
  -> m ()
glGetnUniformdvARB v1 v2 v3 v4 = liftIO $ dyn456 ptr_glGetnUniformdvARB v1 v2 v3 v4

{-# NOINLINE ptr_glGetnUniformdvARB #-}
ptr_glGetnUniformdvARB :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLdouble -> IO ())
ptr_glGetnUniformdvARB = unsafePerformIO $ getCommand "glGetnUniformdvARB"

-- glGetnUniformfv -------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetUniform.xhtml OpenGL 4.x>.
glGetnUniformfv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLfloat -- ^ @params@.
  -> m ()
glGetnUniformfv v1 v2 v3 v4 = liftIO $ dyn457 ptr_glGetnUniformfv v1 v2 v3 v4

{-# NOINLINE ptr_glGetnUniformfv #-}
ptr_glGetnUniformfv :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glGetnUniformfv = unsafePerformIO $ getCommand "glGetnUniformfv"

-- glGetnUniformfvARB ----------------------------------------------------------

glGetnUniformfvARB
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLfloat -- ^ @params@ pointing to @bufSize@ elements of type @GLfloat@.
  -> m ()
glGetnUniformfvARB v1 v2 v3 v4 = liftIO $ dyn457 ptr_glGetnUniformfvARB v1 v2 v3 v4

{-# NOINLINE ptr_glGetnUniformfvARB #-}
ptr_glGetnUniformfvARB :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glGetnUniformfvARB = unsafePerformIO $ getCommand "glGetnUniformfvARB"

-- glGetnUniformfvEXT ----------------------------------------------------------

glGetnUniformfvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLfloat -- ^ @params@ pointing to @bufSize@ elements of type @GLfloat@.
  -> m ()
glGetnUniformfvEXT v1 v2 v3 v4 = liftIO $ dyn457 ptr_glGetnUniformfvEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetnUniformfvEXT #-}
ptr_glGetnUniformfvEXT :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glGetnUniformfvEXT = unsafePerformIO $ getCommand "glGetnUniformfvEXT"

-- glGetnUniformfvKHR ----------------------------------------------------------

-- | This command is an alias for 'glGetnUniformfv'.
glGetnUniformfvKHR
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLfloat -- ^ @params@.
  -> m ()
glGetnUniformfvKHR v1 v2 v3 v4 = liftIO $ dyn457 ptr_glGetnUniformfvKHR v1 v2 v3 v4

{-# NOINLINE ptr_glGetnUniformfvKHR #-}
ptr_glGetnUniformfvKHR :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glGetnUniformfvKHR = unsafePerformIO $ getCommand "glGetnUniformfvKHR"

-- glGetnUniformi64vARB --------------------------------------------------------

glGetnUniformi64vARB
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLint64 -- ^ @params@.
  -> m ()
glGetnUniformi64vARB v1 v2 v3 v4 = liftIO $ dyn458 ptr_glGetnUniformi64vARB v1 v2 v3 v4

{-# NOINLINE ptr_glGetnUniformi64vARB #-}
ptr_glGetnUniformi64vARB :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLint64 -> IO ())
ptr_glGetnUniformi64vARB = unsafePerformIO $ getCommand "glGetnUniformi64vARB"

-- glGetnUniformiv -------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetUniform.xhtml OpenGL 4.x>.
glGetnUniformiv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLint -- ^ @params@.
  -> m ()
glGetnUniformiv v1 v2 v3 v4 = liftIO $ dyn459 ptr_glGetnUniformiv v1 v2 v3 v4

{-# NOINLINE ptr_glGetnUniformiv #-}
ptr_glGetnUniformiv :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLint -> IO ())
ptr_glGetnUniformiv = unsafePerformIO $ getCommand "glGetnUniformiv"

-- glGetnUniformivARB ----------------------------------------------------------

glGetnUniformivARB
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLint -- ^ @params@ pointing to @bufSize@ elements of type @GLint@.
  -> m ()
glGetnUniformivARB v1 v2 v3 v4 = liftIO $ dyn459 ptr_glGetnUniformivARB v1 v2 v3 v4

{-# NOINLINE ptr_glGetnUniformivARB #-}
ptr_glGetnUniformivARB :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLint -> IO ())
ptr_glGetnUniformivARB = unsafePerformIO $ getCommand "glGetnUniformivARB"

-- glGetnUniformivEXT ----------------------------------------------------------

glGetnUniformivEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLint -- ^ @params@ pointing to @bufSize@ elements of type @GLint@.
  -> m ()
glGetnUniformivEXT v1 v2 v3 v4 = liftIO $ dyn459 ptr_glGetnUniformivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetnUniformivEXT #-}
ptr_glGetnUniformivEXT :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLint -> IO ())
ptr_glGetnUniformivEXT = unsafePerformIO $ getCommand "glGetnUniformivEXT"

-- glGetnUniformivKHR ----------------------------------------------------------

-- | This command is an alias for 'glGetnUniformiv'.
glGetnUniformivKHR
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLint -- ^ @params@.
  -> m ()
glGetnUniformivKHR v1 v2 v3 v4 = liftIO $ dyn459 ptr_glGetnUniformivKHR v1 v2 v3 v4

{-# NOINLINE ptr_glGetnUniformivKHR #-}
ptr_glGetnUniformivKHR :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLint -> IO ())
ptr_glGetnUniformivKHR = unsafePerformIO $ getCommand "glGetnUniformivKHR"

-- glGetnUniformui64vARB -------------------------------------------------------

glGetnUniformui64vARB
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLuint64 -- ^ @params@.
  -> m ()
glGetnUniformui64vARB v1 v2 v3 v4 = liftIO $ dyn460 ptr_glGetnUniformui64vARB v1 v2 v3 v4

{-# NOINLINE ptr_glGetnUniformui64vARB #-}
ptr_glGetnUniformui64vARB :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint64 -> IO ())
ptr_glGetnUniformui64vARB = unsafePerformIO $ getCommand "glGetnUniformui64vARB"

-- glGetnUniformuiv ------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetUniform.xhtml OpenGL 4.x>.
glGetnUniformuiv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLuint -- ^ @params@.
  -> m ()
glGetnUniformuiv v1 v2 v3 v4 = liftIO $ dyn461 ptr_glGetnUniformuiv v1 v2 v3 v4

{-# NOINLINE ptr_glGetnUniformuiv #-}
ptr_glGetnUniformuiv :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint -> IO ())
ptr_glGetnUniformuiv = unsafePerformIO $ getCommand "glGetnUniformuiv"

-- glGetnUniformuivARB ---------------------------------------------------------

glGetnUniformuivARB
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLuint -- ^ @params@ pointing to @bufSize@ elements of type @GLuint@.
  -> m ()
glGetnUniformuivARB v1 v2 v3 v4 = liftIO $ dyn461 ptr_glGetnUniformuivARB v1 v2 v3 v4

{-# NOINLINE ptr_glGetnUniformuivARB #-}
ptr_glGetnUniformuivARB :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint -> IO ())
ptr_glGetnUniformuivARB = unsafePerformIO $ getCommand "glGetnUniformuivARB"

-- glGetnUniformuivKHR ---------------------------------------------------------

-- | This command is an alias for 'glGetnUniformuiv'.
glGetnUniformuivKHR
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLuint -- ^ @params@.
  -> m ()
glGetnUniformuivKHR v1 v2 v3 v4 = liftIO $ dyn461 ptr_glGetnUniformuivKHR v1 v2 v3 v4

{-# NOINLINE ptr_glGetnUniformuivKHR #-}
ptr_glGetnUniformuivKHR :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr GLuint -> IO ())
ptr_glGetnUniformuivKHR = unsafePerformIO $ getCommand "glGetnUniformuivKHR"

-- glGlobalAlphaFactorbSUN -----------------------------------------------------

glGlobalAlphaFactorbSUN
  :: MonadIO m
  => GLbyte -- ^ @factor@.
  -> m ()
glGlobalAlphaFactorbSUN v1 = liftIO $ dyn462 ptr_glGlobalAlphaFactorbSUN v1

{-# NOINLINE ptr_glGlobalAlphaFactorbSUN #-}
ptr_glGlobalAlphaFactorbSUN :: FunPtr (GLbyte -> IO ())
ptr_glGlobalAlphaFactorbSUN = unsafePerformIO $ getCommand "glGlobalAlphaFactorbSUN"

-- glGlobalAlphaFactordSUN -----------------------------------------------------

glGlobalAlphaFactordSUN
  :: MonadIO m
  => GLdouble -- ^ @factor@.
  -> m ()
glGlobalAlphaFactordSUN v1 = liftIO $ dyn78 ptr_glGlobalAlphaFactordSUN v1

{-# NOINLINE ptr_glGlobalAlphaFactordSUN #-}
ptr_glGlobalAlphaFactordSUN :: FunPtr (GLdouble -> IO ())
ptr_glGlobalAlphaFactordSUN = unsafePerformIO $ getCommand "glGlobalAlphaFactordSUN"

-- glGlobalAlphaFactorfSUN -----------------------------------------------------

glGlobalAlphaFactorfSUN
  :: MonadIO m
  => GLfloat -- ^ @factor@.
  -> m ()
glGlobalAlphaFactorfSUN v1 = liftIO $ dyn79 ptr_glGlobalAlphaFactorfSUN v1

{-# NOINLINE ptr_glGlobalAlphaFactorfSUN #-}
ptr_glGlobalAlphaFactorfSUN :: FunPtr (GLfloat -> IO ())
ptr_glGlobalAlphaFactorfSUN = unsafePerformIO $ getCommand "glGlobalAlphaFactorfSUN"

-- glGlobalAlphaFactoriSUN -----------------------------------------------------

glGlobalAlphaFactoriSUN
  :: MonadIO m
  => GLint -- ^ @factor@.
  -> m ()
glGlobalAlphaFactoriSUN v1 = liftIO $ dyn12 ptr_glGlobalAlphaFactoriSUN v1

{-# NOINLINE ptr_glGlobalAlphaFactoriSUN #-}
ptr_glGlobalAlphaFactoriSUN :: FunPtr (GLint -> IO ())
ptr_glGlobalAlphaFactoriSUN = unsafePerformIO $ getCommand "glGlobalAlphaFactoriSUN"

-- glGlobalAlphaFactorsSUN -----------------------------------------------------

glGlobalAlphaFactorsSUN
  :: MonadIO m
  => GLshort -- ^ @factor@.
  -> m ()
glGlobalAlphaFactorsSUN v1 = liftIO $ dyn463 ptr_glGlobalAlphaFactorsSUN v1

{-# NOINLINE ptr_glGlobalAlphaFactorsSUN #-}
ptr_glGlobalAlphaFactorsSUN :: FunPtr (GLshort -> IO ())
ptr_glGlobalAlphaFactorsSUN = unsafePerformIO $ getCommand "glGlobalAlphaFactorsSUN"

-- glGlobalAlphaFactorubSUN ----------------------------------------------------

glGlobalAlphaFactorubSUN
  :: MonadIO m
  => GLubyte -- ^ @factor@.
  -> m ()
glGlobalAlphaFactorubSUN v1 = liftIO $ dyn464 ptr_glGlobalAlphaFactorubSUN v1

{-# NOINLINE ptr_glGlobalAlphaFactorubSUN #-}
ptr_glGlobalAlphaFactorubSUN :: FunPtr (GLubyte -> IO ())
ptr_glGlobalAlphaFactorubSUN = unsafePerformIO $ getCommand "glGlobalAlphaFactorubSUN"

-- glGlobalAlphaFactoruiSUN ----------------------------------------------------

glGlobalAlphaFactoruiSUN
  :: MonadIO m
  => GLuint -- ^ @factor@.
  -> m ()
glGlobalAlphaFactoruiSUN v1 = liftIO $ dyn2 ptr_glGlobalAlphaFactoruiSUN v1

{-# NOINLINE ptr_glGlobalAlphaFactoruiSUN #-}
ptr_glGlobalAlphaFactoruiSUN :: FunPtr (GLuint -> IO ())
ptr_glGlobalAlphaFactoruiSUN = unsafePerformIO $ getCommand "glGlobalAlphaFactoruiSUN"

-- glGlobalAlphaFactorusSUN ----------------------------------------------------

glGlobalAlphaFactorusSUN
  :: MonadIO m
  => GLushort -- ^ @factor@.
  -> m ()
glGlobalAlphaFactorusSUN v1 = liftIO $ dyn465 ptr_glGlobalAlphaFactorusSUN v1

{-# NOINLINE ptr_glGlobalAlphaFactorusSUN #-}
ptr_glGlobalAlphaFactorusSUN :: FunPtr (GLushort -> IO ())
ptr_glGlobalAlphaFactorusSUN = unsafePerformIO $ getCommand "glGlobalAlphaFactorusSUN"

-- glHint ----------------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glHint.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glHint.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glHint.xhtml OpenGL 4.x>.
glHint
  :: MonadIO m
  => GLenum -- ^ @target@ of type [HintTarget](Graphics-GL-Groups.html#HintTarget).
  -> GLenum -- ^ @mode@ of type [HintMode](Graphics-GL-Groups.html#HintMode).
  -> m ()
glHint v1 v2 = liftIO $ dyn51 ptr_glHint v1 v2

{-# NOINLINE ptr_glHint #-}
ptr_glHint :: FunPtr (GLenum -> GLenum -> IO ())
ptr_glHint = unsafePerformIO $ getCommand "glHint"

-- glHintPGI -------------------------------------------------------------------

glHintPGI
  :: MonadIO m
  => GLenum -- ^ @target@ of type @HintTargetPGI@.
  -> GLint -- ^ @mode@.
  -> m ()
glHintPGI v1 v2 = liftIO $ dyn55 ptr_glHintPGI v1 v2

{-# NOINLINE ptr_glHintPGI #-}
ptr_glHintPGI :: FunPtr (GLenum -> GLint -> IO ())
ptr_glHintPGI = unsafePerformIO $ getCommand "glHintPGI"

-- glHistogram -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glHistogram.xml OpenGL 2.x>.
glHistogram
  :: MonadIO m
  => GLenum -- ^ @target@ of type @HistogramTarget@.
  -> GLsizei -- ^ @width@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLboolean -- ^ @sink@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glHistogram v1 v2 v3 v4 = liftIO $ dyn466 ptr_glHistogram v1 v2 v3 v4

{-# NOINLINE ptr_glHistogram #-}
ptr_glHistogram :: FunPtr (GLenum -> GLsizei -> GLenum -> GLboolean -> IO ())
ptr_glHistogram = unsafePerformIO $ getCommand "glHistogram"

-- glHistogramEXT --------------------------------------------------------------

-- | This command is an alias for 'glHistogram'.
glHistogramEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [HistogramTargetEXT](Graphics-GL-Groups.html#HistogramTargetEXT).
  -> GLsizei -- ^ @width@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLboolean -- ^ @sink@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glHistogramEXT v1 v2 v3 v4 = liftIO $ dyn466 ptr_glHistogramEXT v1 v2 v3 v4

{-# NOINLINE ptr_glHistogramEXT #-}
ptr_glHistogramEXT :: FunPtr (GLenum -> GLsizei -> GLenum -> GLboolean -> IO ())
ptr_glHistogramEXT = unsafePerformIO $ getCommand "glHistogramEXT"

-- glIglooInterfaceSGIX --------------------------------------------------------

glIglooInterfaceSGIX
  :: MonadIO m
  => GLenum -- ^ @pname@ of type @IglooFunctionSelectSGIX@.
  -> Ptr a -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @IglooParameterSGIX@.
  -> m ()
glIglooInterfaceSGIX v1 v2 = liftIO $ dyn230 ptr_glIglooInterfaceSGIX v1 v2

{-# NOINLINE ptr_glIglooInterfaceSGIX #-}
ptr_glIglooInterfaceSGIX :: FunPtr (GLenum -> Ptr a -> IO ())
ptr_glIglooInterfaceSGIX = unsafePerformIO $ getCommand "glIglooInterfaceSGIX"

-- glImageTransformParameterfHP ------------------------------------------------

glImageTransformParameterfHP
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ImageTransformTargetHP@.
  -> GLenum -- ^ @pname@ of type @ImageTransformPNameHP@.
  -> GLfloat -- ^ @param@.
  -> m ()
glImageTransformParameterfHP v1 v2 v3 = liftIO $ dyn161 ptr_glImageTransformParameterfHP v1 v2 v3

{-# NOINLINE ptr_glImageTransformParameterfHP #-}
ptr_glImageTransformParameterfHP :: FunPtr (GLenum -> GLenum -> GLfloat -> IO ())
ptr_glImageTransformParameterfHP = unsafePerformIO $ getCommand "glImageTransformParameterfHP"

-- glImageTransformParameterfvHP -----------------------------------------------

glImageTransformParameterfvHP
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ImageTransformTargetHP@.
  -> GLenum -- ^ @pname@ of type @ImageTransformPNameHP@.
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glImageTransformParameterfvHP v1 v2 v3 = liftIO $ dyn132 ptr_glImageTransformParameterfvHP v1 v2 v3

{-# NOINLINE ptr_glImageTransformParameterfvHP #-}
ptr_glImageTransformParameterfvHP :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glImageTransformParameterfvHP = unsafePerformIO $ getCommand "glImageTransformParameterfvHP"

-- glImageTransformParameteriHP ------------------------------------------------

glImageTransformParameteriHP
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ImageTransformTargetHP@.
  -> GLenum -- ^ @pname@ of type @ImageTransformPNameHP@.
  -> GLint -- ^ @param@.
  -> m ()
glImageTransformParameteriHP v1 v2 v3 = liftIO $ dyn62 ptr_glImageTransformParameteriHP v1 v2 v3

{-# NOINLINE ptr_glImageTransformParameteriHP #-}
ptr_glImageTransformParameteriHP :: FunPtr (GLenum -> GLenum -> GLint -> IO ())
ptr_glImageTransformParameteriHP = unsafePerformIO $ getCommand "glImageTransformParameteriHP"

-- glImageTransformParameterivHP -----------------------------------------------

glImageTransformParameterivHP
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ImageTransformTargetHP@.
  -> GLenum -- ^ @pname@ of type @ImageTransformPNameHP@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glImageTransformParameterivHP v1 v2 v3 = liftIO $ dyn133 ptr_glImageTransformParameterivHP v1 v2 v3

{-# NOINLINE ptr_glImageTransformParameterivHP #-}
ptr_glImageTransformParameterivHP :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glImageTransformParameterivHP = unsafePerformIO $ getCommand "glImageTransformParameterivHP"

-- glImportSyncEXT -------------------------------------------------------------

glImportSyncEXT
  :: MonadIO m
  => GLenum -- ^ @external_sync_type@.
  -> GLintptr -- ^ @external_sync@.
  -> GLbitfield -- ^ @flags@.
  -> m GLsync -- ^ of type @sync@.
glImportSyncEXT v1 v2 v3 = liftIO $ dyn467 ptr_glImportSyncEXT v1 v2 v3

{-# NOINLINE ptr_glImportSyncEXT #-}
ptr_glImportSyncEXT :: FunPtr (GLenum -> GLintptr -> GLbitfield -> IO GLsync)
ptr_glImportSyncEXT = unsafePerformIO $ getCommand "glImportSyncEXT"

-- glIndexFormatNV -------------------------------------------------------------

glIndexFormatNV
  :: MonadIO m
  => GLenum -- ^ @type@.
  -> GLsizei -- ^ @stride@.
  -> m ()
glIndexFormatNV v1 v2 = liftIO $ dyn239 ptr_glIndexFormatNV v1 v2

{-# NOINLINE ptr_glIndexFormatNV #-}
ptr_glIndexFormatNV :: FunPtr (GLenum -> GLsizei -> IO ())
ptr_glIndexFormatNV = unsafePerformIO $ getCommand "glIndexFormatNV"

-- glIndexFuncEXT --------------------------------------------------------------

glIndexFuncEXT
  :: MonadIO m
  => GLenum -- ^ @func@ of type @IndexFunctionEXT@.
  -> GLclampf -- ^ @ref@ of type @ClampedFloat32@.
  -> m ()
glIndexFuncEXT v1 v2 = liftIO $ dyn9 ptr_glIndexFuncEXT v1 v2

{-# NOINLINE ptr_glIndexFuncEXT #-}
ptr_glIndexFuncEXT :: FunPtr (GLenum -> GLclampf -> IO ())
ptr_glIndexFuncEXT = unsafePerformIO $ getCommand "glIndexFuncEXT"

-- glIndexMask -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glIndexMask.xml OpenGL 2.x>.
glIndexMask
  :: MonadIO m
  => GLuint -- ^ @mask@ of type @MaskedColorIndexValueI@.
  -> m ()
glIndexMask v1 = liftIO $ dyn2 ptr_glIndexMask v1

{-# NOINLINE ptr_glIndexMask #-}
ptr_glIndexMask :: FunPtr (GLuint -> IO ())
ptr_glIndexMask = unsafePerformIO $ getCommand "glIndexMask"

-- glIndexMaterialEXT ----------------------------------------------------------

glIndexMaterialEXT
  :: MonadIO m
  => GLenum -- ^ @face@ of type [MaterialFace](Graphics-GL-Groups.html#MaterialFace).
  -> GLenum -- ^ @mode@ of type @IndexMaterialParameterEXT@.
  -> m ()
glIndexMaterialEXT v1 v2 = liftIO $ dyn51 ptr_glIndexMaterialEXT v1 v2

{-# NOINLINE ptr_glIndexMaterialEXT #-}
ptr_glIndexMaterialEXT :: FunPtr (GLenum -> GLenum -> IO ())
ptr_glIndexMaterialEXT = unsafePerformIO $ getCommand "glIndexMaterialEXT"

-- glIndexPointer --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glIndexPointer.xml OpenGL 2.x>.
glIndexPointer
  :: MonadIO m
  => GLenum -- ^ @type@ of type [IndexPointerType](Graphics-GL-Groups.html#IndexPointerType).
  -> GLsizei -- ^ @stride@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(type,stride)@ elements of type @a@.
  -> m ()
glIndexPointer v1 v2 v3 = liftIO $ dyn46 ptr_glIndexPointer v1 v2 v3

{-# NOINLINE ptr_glIndexPointer #-}
ptr_glIndexPointer :: FunPtr (GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glIndexPointer = unsafePerformIO $ getCommand "glIndexPointer"

-- glIndexPointerEXT -----------------------------------------------------------

glIndexPointerEXT
  :: MonadIO m
  => GLenum -- ^ @type@ of type [IndexPointerType](Graphics-GL-Groups.html#IndexPointerType).
  -> GLsizei -- ^ @stride@.
  -> GLsizei -- ^ @count@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(type,stride,count)@ elements of type @a@.
  -> m ()
glIndexPointerEXT v1 v2 v3 v4 = liftIO $ dyn468 ptr_glIndexPointerEXT v1 v2 v3 v4

{-# NOINLINE ptr_glIndexPointerEXT #-}
ptr_glIndexPointerEXT :: FunPtr (GLenum -> GLsizei -> GLsizei -> Ptr a -> IO ())
ptr_glIndexPointerEXT = unsafePerformIO $ getCommand "glIndexPointerEXT"

-- glIndexPointerListIBM -------------------------------------------------------

glIndexPointerListIBM
  :: MonadIO m
  => GLenum -- ^ @type@ of type [IndexPointerType](Graphics-GL-Groups.html#IndexPointerType).
  -> GLint -- ^ @stride@.
  -> Ptr (Ptr a) -- ^ @pointer@ pointing to @COMPSIZE(type,stride)@ elements of type @Ptr a@.
  -> GLint -- ^ @ptrstride@.
  -> m ()
glIndexPointerListIBM v1 v2 v3 v4 = liftIO $ dyn280 ptr_glIndexPointerListIBM v1 v2 v3 v4

{-# NOINLINE ptr_glIndexPointerListIBM #-}
ptr_glIndexPointerListIBM :: FunPtr (GLenum -> GLint -> Ptr (Ptr a) -> GLint -> IO ())
ptr_glIndexPointerListIBM = unsafePerformIO $ getCommand "glIndexPointerListIBM"

