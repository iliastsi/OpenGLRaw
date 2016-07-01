--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F27
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

module Graphics.GL.Functions.F27 (
  glGetPerfQueryIdByNameINTEL,
  glGetPerfQueryInfoINTEL,
  glGetPixelMapfv,
  glGetPixelMapuiv,
  glGetPixelMapusv,
  glGetPixelMapxv,
  glGetPixelTexGenParameterfvSGIS,
  glGetPixelTexGenParameterivSGIS,
  glGetPixelTransformParameterfvEXT,
  glGetPixelTransformParameterivEXT,
  glGetPointerIndexedvEXT,
  glGetPointeri_vEXT,
  glGetPointerv,
  glGetPointervEXT,
  glGetPointervKHR,
  glGetPolygonStipple,
  glGetProgramBinary,
  glGetProgramBinaryOES,
  glGetProgramEnvParameterIivNV,
  glGetProgramEnvParameterIuivNV,
  glGetProgramEnvParameterdvARB,
  glGetProgramEnvParameterfvARB,
  glGetProgramInfoLog,
  glGetProgramInterfaceiv,
  glGetProgramLocalParameterIivNV,
  glGetProgramLocalParameterIuivNV,
  glGetProgramLocalParameterdvARB,
  glGetProgramLocalParameterfvARB,
  glGetProgramNamedParameterdvNV,
  glGetProgramNamedParameterfvNV,
  glGetProgramParameterdvNV,
  glGetProgramParameterfvNV,
  glGetProgramPipelineInfoLog,
  glGetProgramPipelineInfoLogEXT,
  glGetProgramPipelineiv,
  glGetProgramPipelineivEXT,
  glGetProgramResourceIndex,
  glGetProgramResourceLocation,
  glGetProgramResourceLocationIndex,
  glGetProgramResourceLocationIndexEXT
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

-- glGetPerfQueryIdByNameINTEL -------------------------------------------------

glGetPerfQueryIdByNameINTEL
  :: MonadIO m
  => Ptr GLchar -- ^ @queryName@.
  -> Ptr GLuint -- ^ @queryId@.
  -> m ()
glGetPerfQueryIdByNameINTEL v1 v2 = liftIO $ dyn387 ptr_glGetPerfQueryIdByNameINTEL v1 v2

{-# NOINLINE ptr_glGetPerfQueryIdByNameINTEL #-}
ptr_glGetPerfQueryIdByNameINTEL :: FunPtr (Ptr GLchar -> Ptr GLuint -> IO ())
ptr_glGetPerfQueryIdByNameINTEL = unsafePerformIO $ getCommand "glGetPerfQueryIdByNameINTEL"

-- glGetPerfQueryInfoINTEL -----------------------------------------------------

glGetPerfQueryInfoINTEL
  :: MonadIO m
  => GLuint -- ^ @queryId@.
  -> GLuint -- ^ @queryNameLength@.
  -> Ptr GLchar -- ^ @queryName@.
  -> Ptr GLuint -- ^ @dataSize@.
  -> Ptr GLuint -- ^ @noCounters@.
  -> Ptr GLuint -- ^ @noInstances@.
  -> Ptr GLuint -- ^ @capsMask@.
  -> m ()
glGetPerfQueryInfoINTEL v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn388 ptr_glGetPerfQueryInfoINTEL v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glGetPerfQueryInfoINTEL #-}
ptr_glGetPerfQueryInfoINTEL :: FunPtr (GLuint -> GLuint -> Ptr GLchar -> Ptr GLuint -> Ptr GLuint -> Ptr GLuint -> Ptr GLuint -> IO ())
ptr_glGetPerfQueryInfoINTEL = unsafePerformIO $ getCommand "glGetPerfQueryInfoINTEL"

-- glGetPixelMapfv -------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetPixelMap.xml OpenGL 2.x>.
glGetPixelMapfv
  :: MonadIO m
  => GLenum -- ^ @map@ of type [PixelMap](Graphics-GL-Groups.html#PixelMap).
  -> Ptr GLfloat -- ^ @values@ pointing to @COMPSIZE(map)@ elements of type @GLfloat@.
  -> m ()
glGetPixelMapfv v1 v2 = liftIO $ dyn94 ptr_glGetPixelMapfv v1 v2

{-# NOINLINE ptr_glGetPixelMapfv #-}
ptr_glGetPixelMapfv :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glGetPixelMapfv = unsafePerformIO $ getCommand "glGetPixelMapfv"

-- glGetPixelMapuiv ------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetPixelMap.xml OpenGL 2.x>.
glGetPixelMapuiv
  :: MonadIO m
  => GLenum -- ^ @map@ of type [PixelMap](Graphics-GL-Groups.html#PixelMap).
  -> Ptr GLuint -- ^ @values@ pointing to @COMPSIZE(map)@ elements of type @GLuint@.
  -> m ()
glGetPixelMapuiv v1 v2 = liftIO $ dyn125 ptr_glGetPixelMapuiv v1 v2

{-# NOINLINE ptr_glGetPixelMapuiv #-}
ptr_glGetPixelMapuiv :: FunPtr (GLenum -> Ptr GLuint -> IO ())
ptr_glGetPixelMapuiv = unsafePerformIO $ getCommand "glGetPixelMapuiv"

-- glGetPixelMapusv ------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetPixelMap.xml OpenGL 2.x>.
glGetPixelMapusv
  :: MonadIO m
  => GLenum -- ^ @map@ of type [PixelMap](Graphics-GL-Groups.html#PixelMap).
  -> Ptr GLushort -- ^ @values@ pointing to @COMPSIZE(map)@ elements of type @GLushort@.
  -> m ()
glGetPixelMapusv v1 v2 = liftIO $ dyn389 ptr_glGetPixelMapusv v1 v2

{-# NOINLINE ptr_glGetPixelMapusv #-}
ptr_glGetPixelMapusv :: FunPtr (GLenum -> Ptr GLushort -> IO ())
ptr_glGetPixelMapusv = unsafePerformIO $ getCommand "glGetPixelMapusv"

-- glGetPixelMapxv -------------------------------------------------------------

glGetPixelMapxv
  :: MonadIO m
  => GLenum -- ^ @map@.
  -> GLint -- ^ @size@.
  -> Ptr GLfixed -- ^ @values@ pointing to @size@ elements of type @GLfixed@.
  -> m ()
glGetPixelMapxv v1 v2 v3 = liftIO $ dyn390 ptr_glGetPixelMapxv v1 v2 v3

{-# NOINLINE ptr_glGetPixelMapxv #-}
ptr_glGetPixelMapxv :: FunPtr (GLenum -> GLint -> Ptr GLfixed -> IO ())
ptr_glGetPixelMapxv = unsafePerformIO $ getCommand "glGetPixelMapxv"

-- glGetPixelTexGenParameterfvSGIS ---------------------------------------------

glGetPixelTexGenParameterfvSGIS
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [PixelTexGenParameterNameSGIS](Graphics-GL-Groups.html#PixelTexGenParameterNameSGIS).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedFloat32@.
  -> m ()
glGetPixelTexGenParameterfvSGIS v1 v2 = liftIO $ dyn94 ptr_glGetPixelTexGenParameterfvSGIS v1 v2

{-# NOINLINE ptr_glGetPixelTexGenParameterfvSGIS #-}
ptr_glGetPixelTexGenParameterfvSGIS :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glGetPixelTexGenParameterfvSGIS = unsafePerformIO $ getCommand "glGetPixelTexGenParameterfvSGIS"

-- glGetPixelTexGenParameterivSGIS ---------------------------------------------

glGetPixelTexGenParameterivSGIS
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [PixelTexGenParameterNameSGIS](Graphics-GL-Groups.html#PixelTexGenParameterNameSGIS).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedInt32@.
  -> m ()
glGetPixelTexGenParameterivSGIS v1 v2 = liftIO $ dyn136 ptr_glGetPixelTexGenParameterivSGIS v1 v2

{-# NOINLINE ptr_glGetPixelTexGenParameterivSGIS #-}
ptr_glGetPixelTexGenParameterivSGIS :: FunPtr (GLenum -> Ptr GLint -> IO ())
ptr_glGetPixelTexGenParameterivSGIS = unsafePerformIO $ getCommand "glGetPixelTexGenParameterivSGIS"

-- glGetPixelTransformParameterfvEXT -------------------------------------------

glGetPixelTransformParameterfvEXT
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetPixelTransformParameterfvEXT v1 v2 v3 = liftIO $ dyn132 ptr_glGetPixelTransformParameterfvEXT v1 v2 v3

{-# NOINLINE ptr_glGetPixelTransformParameterfvEXT #-}
ptr_glGetPixelTransformParameterfvEXT :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetPixelTransformParameterfvEXT = unsafePerformIO $ getCommand "glGetPixelTransformParameterfvEXT"

-- glGetPixelTransformParameterivEXT -------------------------------------------

glGetPixelTransformParameterivEXT
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetPixelTransformParameterivEXT v1 v2 v3 = liftIO $ dyn133 ptr_glGetPixelTransformParameterivEXT v1 v2 v3

{-# NOINLINE ptr_glGetPixelTransformParameterivEXT #-}
ptr_glGetPixelTransformParameterivEXT :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetPixelTransformParameterivEXT = unsafePerformIO $ getCommand "glGetPixelTransformParameterivEXT"

-- glGetPointerIndexedvEXT -----------------------------------------------------

glGetPointerIndexedvEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TypeEnum@.
  -> GLuint -- ^ @index@.
  -> Ptr (Ptr a) -- ^ @data@ pointing to @1@ element of type @Ptr a@.
  -> m ()
glGetPointerIndexedvEXT v1 v2 v3 = liftIO $ dyn391 ptr_glGetPointerIndexedvEXT v1 v2 v3

{-# NOINLINE ptr_glGetPointerIndexedvEXT #-}
ptr_glGetPointerIndexedvEXT :: FunPtr (GLenum -> GLuint -> Ptr (Ptr a) -> IO ())
ptr_glGetPointerIndexedvEXT = unsafePerformIO $ getCommand "glGetPointerIndexedvEXT"

-- glGetPointeri_vEXT ----------------------------------------------------------

glGetPointeri_vEXT
  :: MonadIO m
  => GLenum -- ^ @pname@ of type @TypeEnum@.
  -> GLuint -- ^ @index@.
  -> Ptr (Ptr a) -- ^ @params@ pointing to @1@ element of type @Ptr a@.
  -> m ()
glGetPointeri_vEXT v1 v2 v3 = liftIO $ dyn391 ptr_glGetPointeri_vEXT v1 v2 v3

{-# NOINLINE ptr_glGetPointeri_vEXT #-}
ptr_glGetPointeri_vEXT :: FunPtr (GLenum -> GLuint -> Ptr (Ptr a) -> IO ())
ptr_glGetPointeri_vEXT = unsafePerformIO $ getCommand "glGetPointeri_vEXT"

-- glGetPointerv ---------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetPointerv.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetPointerv.xhtml OpenGL 4.x>.
glGetPointerv
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [GetPointervPName](Graphics-GL-Groups.html#GetPointervPName).
  -> Ptr (Ptr a) -- ^ @params@ pointing to @1@ element of type @Ptr a@.
  -> m ()
glGetPointerv v1 v2 = liftIO $ dyn268 ptr_glGetPointerv v1 v2

{-# NOINLINE ptr_glGetPointerv #-}
ptr_glGetPointerv :: FunPtr (GLenum -> Ptr (Ptr a) -> IO ())
ptr_glGetPointerv = unsafePerformIO $ getCommand "glGetPointerv"

-- glGetPointervEXT ------------------------------------------------------------

-- | This command is an alias for 'glGetPointerv'.
glGetPointervEXT
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [GetPointervPName](Graphics-GL-Groups.html#GetPointervPName).
  -> Ptr (Ptr a) -- ^ @params@ pointing to @1@ element of type @Ptr a@.
  -> m ()
glGetPointervEXT v1 v2 = liftIO $ dyn268 ptr_glGetPointervEXT v1 v2

{-# NOINLINE ptr_glGetPointervEXT #-}
ptr_glGetPointervEXT :: FunPtr (GLenum -> Ptr (Ptr a) -> IO ())
ptr_glGetPointervEXT = unsafePerformIO $ getCommand "glGetPointervEXT"

-- glGetPointervKHR ------------------------------------------------------------

-- | This command is an alias for 'glGetPointerv'.
glGetPointervKHR
  :: MonadIO m
  => GLenum -- ^ @pname@.
  -> Ptr (Ptr a) -- ^ @params@.
  -> m ()
glGetPointervKHR v1 v2 = liftIO $ dyn268 ptr_glGetPointervKHR v1 v2

{-# NOINLINE ptr_glGetPointervKHR #-}
ptr_glGetPointervKHR :: FunPtr (GLenum -> Ptr (Ptr a) -> IO ())
ptr_glGetPointervKHR = unsafePerformIO $ getCommand "glGetPointervKHR"

-- glGetPolygonStipple ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetPolygonStipple.xml OpenGL 2.x>.
glGetPolygonStipple
  :: MonadIO m
  => Ptr GLubyte -- ^ @mask@ pointing to @COMPSIZE()@ elements of type @GLubyte@.
  -> m ()
glGetPolygonStipple v1 = liftIO $ dyn101 ptr_glGetPolygonStipple v1

{-# NOINLINE ptr_glGetPolygonStipple #-}
ptr_glGetPolygonStipple :: FunPtr (Ptr GLubyte -> IO ())
ptr_glGetPolygonStipple = unsafePerformIO $ getCommand "glGetPolygonStipple"

-- glGetProgramBinary ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetProgramBinary.xhtml OpenGL 4.x>.
glGetProgramBinary
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLenum -- ^ @binaryFormat@ pointing to @1@ element of type @GLenum@.
  -> Ptr a -- ^ @binary@ pointing to @bufSize@ elements of type @a@.
  -> m ()
glGetProgramBinary v1 v2 v3 v4 v5 = liftIO $ dyn392 ptr_glGetProgramBinary v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetProgramBinary #-}
ptr_glGetProgramBinary :: FunPtr (GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLenum -> Ptr a -> IO ())
ptr_glGetProgramBinary = unsafePerformIO $ getCommand "glGetProgramBinary"

-- glGetProgramBinaryOES -------------------------------------------------------

-- | This command is an alias for 'glGetProgramBinary'.
glGetProgramBinaryOES
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLenum -- ^ @binaryFormat@ pointing to @1@ element of type @GLenum@.
  -> Ptr a -- ^ @binary@ pointing to @bufSize@ elements of type @a@.
  -> m ()
glGetProgramBinaryOES v1 v2 v3 v4 v5 = liftIO $ dyn392 ptr_glGetProgramBinaryOES v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetProgramBinaryOES #-}
ptr_glGetProgramBinaryOES :: FunPtr (GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLenum -> Ptr a -> IO ())
ptr_glGetProgramBinaryOES = unsafePerformIO $ getCommand "glGetProgramBinaryOES"

-- glGetProgramEnvParameterIivNV -----------------------------------------------

glGetProgramEnvParameterIivNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ProgramTarget@.
  -> GLuint -- ^ @index@.
  -> Ptr GLint -- ^ @params@ pointing to @4@ elements of type @GLint@.
  -> m ()
glGetProgramEnvParameterIivNV v1 v2 v3 = liftIO $ dyn342 ptr_glGetProgramEnvParameterIivNV v1 v2 v3

{-# NOINLINE ptr_glGetProgramEnvParameterIivNV #-}
ptr_glGetProgramEnvParameterIivNV :: FunPtr (GLenum -> GLuint -> Ptr GLint -> IO ())
ptr_glGetProgramEnvParameterIivNV = unsafePerformIO $ getCommand "glGetProgramEnvParameterIivNV"

-- glGetProgramEnvParameterIuivNV ----------------------------------------------

glGetProgramEnvParameterIuivNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ProgramTarget@.
  -> GLuint -- ^ @index@.
  -> Ptr GLuint -- ^ @params@ pointing to @4@ elements of type @GLuint@.
  -> m ()
glGetProgramEnvParameterIuivNV v1 v2 v3 = liftIO $ dyn214 ptr_glGetProgramEnvParameterIuivNV v1 v2 v3

{-# NOINLINE ptr_glGetProgramEnvParameterIuivNV #-}
ptr_glGetProgramEnvParameterIuivNV :: FunPtr (GLenum -> GLuint -> Ptr GLuint -> IO ())
ptr_glGetProgramEnvParameterIuivNV = unsafePerformIO $ getCommand "glGetProgramEnvParameterIuivNV"

-- glGetProgramEnvParameterdvARB -----------------------------------------------

glGetProgramEnvParameterdvARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ProgramTargetARB@.
  -> GLuint -- ^ @index@.
  -> Ptr GLdouble -- ^ @params@ pointing to @4@ elements of type @GLdouble@.
  -> m ()
glGetProgramEnvParameterdvARB v1 v2 v3 = liftIO $ dyn330 ptr_glGetProgramEnvParameterdvARB v1 v2 v3

{-# NOINLINE ptr_glGetProgramEnvParameterdvARB #-}
ptr_glGetProgramEnvParameterdvARB :: FunPtr (GLenum -> GLuint -> Ptr GLdouble -> IO ())
ptr_glGetProgramEnvParameterdvARB = unsafePerformIO $ getCommand "glGetProgramEnvParameterdvARB"

-- glGetProgramEnvParameterfvARB -----------------------------------------------

glGetProgramEnvParameterfvARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ProgramTargetARB@.
  -> GLuint -- ^ @index@.
  -> Ptr GLfloat -- ^ @params@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glGetProgramEnvParameterfvARB v1 v2 v3 = liftIO $ dyn267 ptr_glGetProgramEnvParameterfvARB v1 v2 v3

{-# NOINLINE ptr_glGetProgramEnvParameterfvARB #-}
ptr_glGetProgramEnvParameterfvARB :: FunPtr (GLenum -> GLuint -> Ptr GLfloat -> IO ())
ptr_glGetProgramEnvParameterfvARB = unsafePerformIO $ getCommand "glGetProgramEnvParameterfvARB"

-- glGetProgramInfoLog ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetProgramInfoLog.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetProgramInfoLog.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetProgramInfoLog.xhtml OpenGL 4.x>.
glGetProgramInfoLog
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLchar -- ^ @infoLog@ pointing to @bufSize@ elements of type @GLchar@.
  -> m ()
glGetProgramInfoLog v1 v2 v3 v4 = liftIO $ dyn331 ptr_glGetProgramInfoLog v1 v2 v3 v4

{-# NOINLINE ptr_glGetProgramInfoLog #-}
ptr_glGetProgramInfoLog :: FunPtr (GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
ptr_glGetProgramInfoLog = unsafePerformIO $ getCommand "glGetProgramInfoLog"

-- glGetProgramInterfaceiv -----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetProgramInterface.xhtml OpenGL 4.x>.
glGetProgramInterfaceiv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @programInterface@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetProgramInterfaceiv v1 v2 v3 v4 = liftIO $ dyn363 ptr_glGetProgramInterfaceiv v1 v2 v3 v4

{-# NOINLINE ptr_glGetProgramInterfaceiv #-}
ptr_glGetProgramInterfaceiv :: FunPtr (GLuint -> GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetProgramInterfaceiv = unsafePerformIO $ getCommand "glGetProgramInterfaceiv"

-- glGetProgramLocalParameterIivNV ---------------------------------------------

glGetProgramLocalParameterIivNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ProgramTarget@.
  -> GLuint -- ^ @index@.
  -> Ptr GLint -- ^ @params@ pointing to @4@ elements of type @GLint@.
  -> m ()
glGetProgramLocalParameterIivNV v1 v2 v3 = liftIO $ dyn342 ptr_glGetProgramLocalParameterIivNV v1 v2 v3

{-# NOINLINE ptr_glGetProgramLocalParameterIivNV #-}
ptr_glGetProgramLocalParameterIivNV :: FunPtr (GLenum -> GLuint -> Ptr GLint -> IO ())
ptr_glGetProgramLocalParameterIivNV = unsafePerformIO $ getCommand "glGetProgramLocalParameterIivNV"

-- glGetProgramLocalParameterIuivNV --------------------------------------------

glGetProgramLocalParameterIuivNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ProgramTarget@.
  -> GLuint -- ^ @index@.
  -> Ptr GLuint -- ^ @params@ pointing to @4@ elements of type @GLuint@.
  -> m ()
glGetProgramLocalParameterIuivNV v1 v2 v3 = liftIO $ dyn214 ptr_glGetProgramLocalParameterIuivNV v1 v2 v3

{-# NOINLINE ptr_glGetProgramLocalParameterIuivNV #-}
ptr_glGetProgramLocalParameterIuivNV :: FunPtr (GLenum -> GLuint -> Ptr GLuint -> IO ())
ptr_glGetProgramLocalParameterIuivNV = unsafePerformIO $ getCommand "glGetProgramLocalParameterIuivNV"

-- glGetProgramLocalParameterdvARB ---------------------------------------------

glGetProgramLocalParameterdvARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ProgramTargetARB@.
  -> GLuint -- ^ @index@.
  -> Ptr GLdouble -- ^ @params@ pointing to @4@ elements of type @GLdouble@.
  -> m ()
glGetProgramLocalParameterdvARB v1 v2 v3 = liftIO $ dyn330 ptr_glGetProgramLocalParameterdvARB v1 v2 v3

{-# NOINLINE ptr_glGetProgramLocalParameterdvARB #-}
ptr_glGetProgramLocalParameterdvARB :: FunPtr (GLenum -> GLuint -> Ptr GLdouble -> IO ())
ptr_glGetProgramLocalParameterdvARB = unsafePerformIO $ getCommand "glGetProgramLocalParameterdvARB"

-- glGetProgramLocalParameterfvARB ---------------------------------------------

glGetProgramLocalParameterfvARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ProgramTargetARB@.
  -> GLuint -- ^ @index@.
  -> Ptr GLfloat -- ^ @params@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glGetProgramLocalParameterfvARB v1 v2 v3 = liftIO $ dyn267 ptr_glGetProgramLocalParameterfvARB v1 v2 v3

{-# NOINLINE ptr_glGetProgramLocalParameterfvARB #-}
ptr_glGetProgramLocalParameterfvARB :: FunPtr (GLenum -> GLuint -> Ptr GLfloat -> IO ())
ptr_glGetProgramLocalParameterfvARB = unsafePerformIO $ getCommand "glGetProgramLocalParameterfvARB"

-- glGetProgramNamedParameterdvNV ----------------------------------------------

glGetProgramNamedParameterdvNV
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLsizei -- ^ @len@.
  -> Ptr GLubyte -- ^ @name@ pointing to @1@ element of type @GLubyte@.
  -> Ptr GLdouble -- ^ @params@ pointing to @4@ elements of type @GLdouble@.
  -> m ()
glGetProgramNamedParameterdvNV v1 v2 v3 v4 = liftIO $ dyn393 ptr_glGetProgramNamedParameterdvNV v1 v2 v3 v4

{-# NOINLINE ptr_glGetProgramNamedParameterdvNV #-}
ptr_glGetProgramNamedParameterdvNV :: FunPtr (GLuint -> GLsizei -> Ptr GLubyte -> Ptr GLdouble -> IO ())
ptr_glGetProgramNamedParameterdvNV = unsafePerformIO $ getCommand "glGetProgramNamedParameterdvNV"

-- glGetProgramNamedParameterfvNV ----------------------------------------------

glGetProgramNamedParameterfvNV
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLsizei -- ^ @len@.
  -> Ptr GLubyte -- ^ @name@ pointing to @1@ element of type @GLubyte@.
  -> Ptr GLfloat -- ^ @params@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glGetProgramNamedParameterfvNV v1 v2 v3 v4 = liftIO $ dyn394 ptr_glGetProgramNamedParameterfvNV v1 v2 v3 v4

{-# NOINLINE ptr_glGetProgramNamedParameterfvNV #-}
ptr_glGetProgramNamedParameterfvNV :: FunPtr (GLuint -> GLsizei -> Ptr GLubyte -> Ptr GLfloat -> IO ())
ptr_glGetProgramNamedParameterfvNV = unsafePerformIO $ getCommand "glGetProgramNamedParameterfvNV"

-- glGetProgramParameterdvNV ---------------------------------------------------

glGetProgramParameterdvNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type @VertexAttribEnumNV@.
  -> GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@ of type @VertexAttribEnumNV@.
  -> Ptr GLdouble -- ^ @params@ pointing to @4@ elements of type @GLdouble@.
  -> m ()
glGetProgramParameterdvNV v1 v2 v3 v4 = liftIO $ dyn395 ptr_glGetProgramParameterdvNV v1 v2 v3 v4

{-# NOINLINE ptr_glGetProgramParameterdvNV #-}
ptr_glGetProgramParameterdvNV :: FunPtr (GLenum -> GLuint -> GLenum -> Ptr GLdouble -> IO ())
ptr_glGetProgramParameterdvNV = unsafePerformIO $ getCommand "glGetProgramParameterdvNV"

-- glGetProgramParameterfvNV ---------------------------------------------------

glGetProgramParameterfvNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type @VertexAttribEnumNV@.
  -> GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@ of type @VertexAttribEnumNV@.
  -> Ptr GLfloat -- ^ @params@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glGetProgramParameterfvNV v1 v2 v3 v4 = liftIO $ dyn350 ptr_glGetProgramParameterfvNV v1 v2 v3 v4

{-# NOINLINE ptr_glGetProgramParameterfvNV #-}
ptr_glGetProgramParameterfvNV :: FunPtr (GLenum -> GLuint -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetProgramParameterfvNV = unsafePerformIO $ getCommand "glGetProgramParameterfvNV"

-- glGetProgramPipelineInfoLog -------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetProgramPipelineInfoLog.xhtml OpenGL 4.x>.
glGetProgramPipelineInfoLog
  :: MonadIO m
  => GLuint -- ^ @pipeline@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLchar -- ^ @infoLog@ pointing to @bufSize@ elements of type @GLchar@.
  -> m ()
glGetProgramPipelineInfoLog v1 v2 v3 v4 = liftIO $ dyn331 ptr_glGetProgramPipelineInfoLog v1 v2 v3 v4

{-# NOINLINE ptr_glGetProgramPipelineInfoLog #-}
ptr_glGetProgramPipelineInfoLog :: FunPtr (GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
ptr_glGetProgramPipelineInfoLog = unsafePerformIO $ getCommand "glGetProgramPipelineInfoLog"

-- glGetProgramPipelineInfoLogEXT ----------------------------------------------

glGetProgramPipelineInfoLogEXT
  :: MonadIO m
  => GLuint -- ^ @pipeline@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLchar -- ^ @infoLog@ pointing to @bufSize@ elements of type @GLchar@.
  -> m ()
glGetProgramPipelineInfoLogEXT v1 v2 v3 v4 = liftIO $ dyn331 ptr_glGetProgramPipelineInfoLogEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetProgramPipelineInfoLogEXT #-}
ptr_glGetProgramPipelineInfoLogEXT :: FunPtr (GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
ptr_glGetProgramPipelineInfoLogEXT = unsafePerformIO $ getCommand "glGetProgramPipelineInfoLogEXT"

-- glGetProgramPipelineiv ------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetProgramPipeline.xhtml OpenGL 4.x>.
glGetProgramPipelineiv
  :: MonadIO m
  => GLuint -- ^ @pipeline@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetProgramPipelineiv v1 v2 v3 = liftIO $ dyn334 ptr_glGetProgramPipelineiv v1 v2 v3

{-# NOINLINE ptr_glGetProgramPipelineiv #-}
ptr_glGetProgramPipelineiv :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetProgramPipelineiv = unsafePerformIO $ getCommand "glGetProgramPipelineiv"

-- glGetProgramPipelineivEXT ---------------------------------------------------

glGetProgramPipelineivEXT
  :: MonadIO m
  => GLuint -- ^ @pipeline@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @params@.
  -> m ()
glGetProgramPipelineivEXT v1 v2 v3 = liftIO $ dyn334 ptr_glGetProgramPipelineivEXT v1 v2 v3

{-# NOINLINE ptr_glGetProgramPipelineivEXT #-}
ptr_glGetProgramPipelineivEXT :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetProgramPipelineivEXT = unsafePerformIO $ getCommand "glGetProgramPipelineivEXT"

-- glGetProgramResourceIndex ---------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetProgramResourceIndex.xhtml OpenGL 4.x>.
glGetProgramResourceIndex
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @programInterface@.
  -> Ptr GLchar -- ^ @name@ pointing to @COMPSIZE(name)@ elements of type @GLchar@.
  -> m GLuint
glGetProgramResourceIndex v1 v2 v3 = liftIO $ dyn396 ptr_glGetProgramResourceIndex v1 v2 v3

{-# NOINLINE ptr_glGetProgramResourceIndex #-}
ptr_glGetProgramResourceIndex :: FunPtr (GLuint -> GLenum -> Ptr GLchar -> IO GLuint)
ptr_glGetProgramResourceIndex = unsafePerformIO $ getCommand "glGetProgramResourceIndex"

-- glGetProgramResourceLocation ------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetProgramResourceLocation.xhtml OpenGL 4.x>.
glGetProgramResourceLocation
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @programInterface@.
  -> Ptr GLchar -- ^ @name@ pointing to @COMPSIZE(name)@ elements of type @GLchar@.
  -> m GLint
glGetProgramResourceLocation v1 v2 v3 = liftIO $ dyn397 ptr_glGetProgramResourceLocation v1 v2 v3

{-# NOINLINE ptr_glGetProgramResourceLocation #-}
ptr_glGetProgramResourceLocation :: FunPtr (GLuint -> GLenum -> Ptr GLchar -> IO GLint)
ptr_glGetProgramResourceLocation = unsafePerformIO $ getCommand "glGetProgramResourceLocation"

-- glGetProgramResourceLocationIndex -------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetProgramResourceLocationIndex.xhtml OpenGL 4.x>.
glGetProgramResourceLocationIndex
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @programInterface@.
  -> Ptr GLchar -- ^ @name@ pointing to @COMPSIZE(name)@ elements of type @GLchar@.
  -> m GLint
glGetProgramResourceLocationIndex v1 v2 v3 = liftIO $ dyn397 ptr_glGetProgramResourceLocationIndex v1 v2 v3

{-# NOINLINE ptr_glGetProgramResourceLocationIndex #-}
ptr_glGetProgramResourceLocationIndex :: FunPtr (GLuint -> GLenum -> Ptr GLchar -> IO GLint)
ptr_glGetProgramResourceLocationIndex = unsafePerformIO $ getCommand "glGetProgramResourceLocationIndex"

-- glGetProgramResourceLocationIndexEXT ----------------------------------------

glGetProgramResourceLocationIndexEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @programInterface@.
  -> Ptr GLchar -- ^ @name@ pointing to @COMPSIZE(name)@ elements of type @GLchar@.
  -> m GLint
glGetProgramResourceLocationIndexEXT v1 v2 v3 = liftIO $ dyn397 ptr_glGetProgramResourceLocationIndexEXT v1 v2 v3

{-# NOINLINE ptr_glGetProgramResourceLocationIndexEXT #-}
ptr_glGetProgramResourceLocationIndexEXT :: FunPtr (GLuint -> GLenum -> Ptr GLchar -> IO GLint)
ptr_glGetProgramResourceLocationIndexEXT = unsafePerformIO $ getCommand "glGetProgramResourceLocationIndexEXT"

