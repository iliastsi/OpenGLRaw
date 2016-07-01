--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F29
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

module Graphics.GL.Functions.F29 (
  glGetSeparableFilterEXT,
  glGetShaderInfoLog,
  glGetShaderPrecisionFormat,
  glGetShaderSource,
  glGetShaderSourceARB,
  glGetShaderiv,
  glGetSharpenTexFuncSGIS,
  glGetStageIndexNV,
  glGetString,
  glGetStringi,
  glGetSubroutineIndex,
  glGetSubroutineUniformLocation,
  glGetSynciv,
  glGetSyncivAPPLE,
  glGetTexBumpParameterfvATI,
  glGetTexBumpParameterivATI,
  glGetTexEnvfv,
  glGetTexEnviv,
  glGetTexEnvxv,
  glGetTexEnvxvOES,
  glGetTexFilterFuncSGIS,
  glGetTexGendv,
  glGetTexGenfv,
  glGetTexGenfvOES,
  glGetTexGeniv,
  glGetTexGenivOES,
  glGetTexGenxvOES,
  glGetTexImage,
  glGetTexLevelParameterfv,
  glGetTexLevelParameteriv,
  glGetTexLevelParameterxvOES,
  glGetTexParameterIiv,
  glGetTexParameterIivEXT,
  glGetTexParameterIivOES,
  glGetTexParameterIuiv,
  glGetTexParameterIuivEXT,
  glGetTexParameterIuivOES,
  glGetTexParameterPointervAPPLE,
  glGetTexParameterfv,
  glGetTexParameteriv
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

-- glGetSeparableFilterEXT -----------------------------------------------------

glGetSeparableFilterEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [SeparableTargetEXT](Graphics-GL-Groups.html#SeparableTargetEXT).
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @row@ pointing to @COMPSIZE(target,format,type)@ elements of type @a@.
  -> Ptr b -- ^ @column@ pointing to @COMPSIZE(target,format,type)@ elements of type @b@.
  -> Ptr c -- ^ @span@ pointing to @COMPSIZE(target,format,type)@ elements of type @c@.
  -> m ()
glGetSeparableFilterEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn403 ptr_glGetSeparableFilterEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glGetSeparableFilterEXT #-}
ptr_glGetSeparableFilterEXT :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr a -> Ptr b -> Ptr c -> IO ())
ptr_glGetSeparableFilterEXT = unsafePerformIO $ getCommand "glGetSeparableFilterEXT"

-- glGetShaderInfoLog ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetShaderInfoLog.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetShaderInfoLog.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetShaderInfoLog.xhtml OpenGL 4.x>.
glGetShaderInfoLog
  :: MonadIO m
  => GLuint -- ^ @shader@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLchar -- ^ @infoLog@ pointing to @bufSize@ elements of type @GLchar@.
  -> m ()
glGetShaderInfoLog v1 v2 v3 v4 = liftIO $ dyn331 ptr_glGetShaderInfoLog v1 v2 v3 v4

{-# NOINLINE ptr_glGetShaderInfoLog #-}
ptr_glGetShaderInfoLog :: FunPtr (GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
ptr_glGetShaderInfoLog = unsafePerformIO $ getCommand "glGetShaderInfoLog"

-- glGetShaderPrecisionFormat --------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetShaderPrecisionFormat.xhtml OpenGL 4.x>.
glGetShaderPrecisionFormat
  :: MonadIO m
  => GLenum -- ^ @shadertype@.
  -> GLenum -- ^ @precisiontype@.
  -> Ptr GLint -- ^ @range@ pointing to @2@ elements of type @GLint@.
  -> Ptr GLint -- ^ @precision@ pointing to @2@ elements of type @GLint@.
  -> m ()
glGetShaderPrecisionFormat v1 v2 v3 v4 = liftIO $ dyn404 ptr_glGetShaderPrecisionFormat v1 v2 v3 v4

{-# NOINLINE ptr_glGetShaderPrecisionFormat #-}
ptr_glGetShaderPrecisionFormat :: FunPtr (GLenum -> GLenum -> Ptr GLint -> Ptr GLint -> IO ())
ptr_glGetShaderPrecisionFormat = unsafePerformIO $ getCommand "glGetShaderPrecisionFormat"

-- glGetShaderSource -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetShaderSource.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetShaderSource.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetShaderSource.xhtml OpenGL 4.x>.
glGetShaderSource
  :: MonadIO m
  => GLuint -- ^ @shader@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLchar -- ^ @source@ pointing to @bufSize@ elements of type @GLchar@.
  -> m ()
glGetShaderSource v1 v2 v3 v4 = liftIO $ dyn331 ptr_glGetShaderSource v1 v2 v3 v4

{-# NOINLINE ptr_glGetShaderSource #-}
ptr_glGetShaderSource :: FunPtr (GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
ptr_glGetShaderSource = unsafePerformIO $ getCommand "glGetShaderSource"

-- glGetShaderSourceARB --------------------------------------------------------

-- | This command is an alias for 'glGetShaderSource'.
glGetShaderSourceARB
  :: MonadIO m
  => GLhandleARB -- ^ @obj@ of type @handleARB@.
  -> GLsizei -- ^ @maxLength@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLcharARB -- ^ @source@ pointing to @maxLength@ elements of type @GLcharARB@.
  -> m ()
glGetShaderSourceARB v1 v2 v3 v4 = liftIO $ dyn338 ptr_glGetShaderSourceARB v1 v2 v3 v4

{-# NOINLINE ptr_glGetShaderSourceARB #-}
ptr_glGetShaderSourceARB :: FunPtr (GLhandleARB -> GLsizei -> Ptr GLsizei -> Ptr GLcharARB -> IO ())
ptr_glGetShaderSourceARB = unsafePerformIO $ getCommand "glGetShaderSourceARB"

-- glGetShaderiv ---------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetShader.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetShader.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetShader.xhtml OpenGL 4.x>.
glGetShaderiv
  :: MonadIO m
  => GLuint -- ^ @shader@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetShaderiv v1 v2 v3 = liftIO $ dyn334 ptr_glGetShaderiv v1 v2 v3

{-# NOINLINE ptr_glGetShaderiv #-}
ptr_glGetShaderiv :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetShaderiv = unsafePerformIO $ getCommand "glGetShaderiv"

-- glGetSharpenTexFuncSGIS -----------------------------------------------------

glGetSharpenTexFuncSGIS
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> Ptr GLfloat -- ^ @points@ pointing to @COMPSIZE(target)@ elements of type @GLfloat@.
  -> m ()
glGetSharpenTexFuncSGIS v1 v2 = liftIO $ dyn94 ptr_glGetSharpenTexFuncSGIS v1 v2

{-# NOINLINE ptr_glGetSharpenTexFuncSGIS #-}
ptr_glGetSharpenTexFuncSGIS :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glGetSharpenTexFuncSGIS = unsafePerformIO $ getCommand "glGetSharpenTexFuncSGIS"

-- glGetStageIndexNV -----------------------------------------------------------

glGetStageIndexNV
  :: MonadIO m
  => GLenum -- ^ @shadertype@.
  -> m GLushort
glGetStageIndexNV v1 = liftIO $ dyn405 ptr_glGetStageIndexNV v1

{-# NOINLINE ptr_glGetStageIndexNV #-}
ptr_glGetStageIndexNV :: FunPtr (GLenum -> IO GLushort)
ptr_glGetStageIndexNV = unsafePerformIO $ getCommand "glGetStageIndexNV"

-- glGetString -----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetString.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetString.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetString.xhtml OpenGL 4.x>.
glGetString
  :: MonadIO m
  => GLenum -- ^ @name@ of type [StringName](Graphics-GL-Groups.html#StringName).
  -> m (Ptr GLubyte) -- ^ pointing to elements of type @String@.
glGetString v1 = liftIO $ dyn406 ptr_glGetString v1

{-# NOINLINE ptr_glGetString #-}
ptr_glGetString :: FunPtr (GLenum -> IO (Ptr GLubyte))
ptr_glGetString = unsafePerformIO $ getCommand "glGetString"

-- glGetStringi ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGetString.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetString.xhtml OpenGL 4.x>.
glGetStringi
  :: MonadIO m
  => GLenum -- ^ @name@.
  -> GLuint -- ^ @index@.
  -> m (Ptr GLubyte) -- ^ pointing to elements of type @String@.
glGetStringi v1 v2 = liftIO $ dyn407 ptr_glGetStringi v1 v2

{-# NOINLINE ptr_glGetStringi #-}
ptr_glGetStringi :: FunPtr (GLenum -> GLuint -> IO (Ptr GLubyte))
ptr_glGetStringi = unsafePerformIO $ getCommand "glGetStringi"

-- glGetSubroutineIndex --------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetSubroutineIndex.xhtml OpenGL 4.x>.
glGetSubroutineIndex
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @shadertype@.
  -> Ptr GLchar -- ^ @name@.
  -> m GLuint
glGetSubroutineIndex v1 v2 v3 = liftIO $ dyn396 ptr_glGetSubroutineIndex v1 v2 v3

{-# NOINLINE ptr_glGetSubroutineIndex #-}
ptr_glGetSubroutineIndex :: FunPtr (GLuint -> GLenum -> Ptr GLchar -> IO GLuint)
ptr_glGetSubroutineIndex = unsafePerformIO $ getCommand "glGetSubroutineIndex"

-- glGetSubroutineUniformLocation ----------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetSubroutineUniformLocation.xhtml OpenGL 4.x>.
glGetSubroutineUniformLocation
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @shadertype@.
  -> Ptr GLchar -- ^ @name@.
  -> m GLint
glGetSubroutineUniformLocation v1 v2 v3 = liftIO $ dyn397 ptr_glGetSubroutineUniformLocation v1 v2 v3

{-# NOINLINE ptr_glGetSubroutineUniformLocation #-}
ptr_glGetSubroutineUniformLocation :: FunPtr (GLuint -> GLenum -> Ptr GLchar -> IO GLint)
ptr_glGetSubroutineUniformLocation = unsafePerformIO $ getCommand "glGetSubroutineUniformLocation"

-- glGetSynciv -----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGetSync.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetSync.xhtml OpenGL 4.x>.
glGetSynciv
  :: MonadIO m
  => GLsync -- ^ @sync@ of type @sync@.
  -> GLenum -- ^ @pname@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLint -- ^ @values@ pointing to @bufSize@ elements of type @GLint@.
  -> m ()
glGetSynciv v1 v2 v3 v4 v5 = liftIO $ dyn408 ptr_glGetSynciv v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetSynciv #-}
ptr_glGetSynciv :: FunPtr (GLsync -> GLenum -> GLsizei -> Ptr GLsizei -> Ptr GLint -> IO ())
ptr_glGetSynciv = unsafePerformIO $ getCommand "glGetSynciv"

-- glGetSyncivAPPLE ------------------------------------------------------------

-- | This command is an alias for 'glGetSynciv'.
glGetSyncivAPPLE
  :: MonadIO m
  => GLsync -- ^ @sync@.
  -> GLenum -- ^ @pname@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLsizei -- ^ @length@.
  -> Ptr GLint -- ^ @values@ pointing to @bufSize@ elements of type @GLint@.
  -> m ()
glGetSyncivAPPLE v1 v2 v3 v4 v5 = liftIO $ dyn408 ptr_glGetSyncivAPPLE v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetSyncivAPPLE #-}
ptr_glGetSyncivAPPLE :: FunPtr (GLsync -> GLenum -> GLsizei -> Ptr GLsizei -> Ptr GLint -> IO ())
ptr_glGetSyncivAPPLE = unsafePerformIO $ getCommand "glGetSyncivAPPLE"

-- glGetTexBumpParameterfvATI --------------------------------------------------

glGetTexBumpParameterfvATI
  :: MonadIO m
  => GLenum -- ^ @pname@ of type @GetTexBumpParameterATI@.
  -> Ptr GLfloat -- ^ @param@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetTexBumpParameterfvATI v1 v2 = liftIO $ dyn94 ptr_glGetTexBumpParameterfvATI v1 v2

{-# NOINLINE ptr_glGetTexBumpParameterfvATI #-}
ptr_glGetTexBumpParameterfvATI :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glGetTexBumpParameterfvATI = unsafePerformIO $ getCommand "glGetTexBumpParameterfvATI"

-- glGetTexBumpParameterivATI --------------------------------------------------

glGetTexBumpParameterivATI
  :: MonadIO m
  => GLenum -- ^ @pname@ of type @GetTexBumpParameterATI@.
  -> Ptr GLint -- ^ @param@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetTexBumpParameterivATI v1 v2 = liftIO $ dyn136 ptr_glGetTexBumpParameterivATI v1 v2

{-# NOINLINE ptr_glGetTexBumpParameterivATI #-}
ptr_glGetTexBumpParameterivATI :: FunPtr (GLenum -> Ptr GLint -> IO ())
ptr_glGetTexBumpParameterivATI = unsafePerformIO $ getCommand "glGetTexBumpParameterivATI"

-- glGetTexEnvfv ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetTexEnv.xml OpenGL 2.x>.
glGetTexEnvfv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureEnvTarget](Graphics-GL-Groups.html#TextureEnvTarget).
  -> GLenum -- ^ @pname@ of type [TextureEnvParameter](Graphics-GL-Groups.html#TextureEnvParameter).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetTexEnvfv v1 v2 v3 = liftIO $ dyn132 ptr_glGetTexEnvfv v1 v2 v3

{-# NOINLINE ptr_glGetTexEnvfv #-}
ptr_glGetTexEnvfv :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetTexEnvfv = unsafePerformIO $ getCommand "glGetTexEnvfv"

-- glGetTexEnviv ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetTexEnv.xml OpenGL 2.x>.
glGetTexEnviv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureEnvTarget](Graphics-GL-Groups.html#TextureEnvTarget).
  -> GLenum -- ^ @pname@ of type [TextureEnvParameter](Graphics-GL-Groups.html#TextureEnvParameter).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetTexEnviv v1 v2 v3 = liftIO $ dyn133 ptr_glGetTexEnviv v1 v2 v3

{-# NOINLINE ptr_glGetTexEnviv #-}
ptr_glGetTexEnviv :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetTexEnviv = unsafePerformIO $ getCommand "glGetTexEnviv"

-- glGetTexEnvxv ---------------------------------------------------------------

glGetTexEnvxv
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLfixed -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfixed@.
  -> m ()
glGetTexEnvxv v1 v2 v3 = liftIO $ dyn163 ptr_glGetTexEnvxv v1 v2 v3

{-# NOINLINE ptr_glGetTexEnvxv #-}
ptr_glGetTexEnvxv :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
ptr_glGetTexEnvxv = unsafePerformIO $ getCommand "glGetTexEnvxv"

-- glGetTexEnvxvOES ------------------------------------------------------------

glGetTexEnvxvOES
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLfixed -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfixed@.
  -> m ()
glGetTexEnvxvOES v1 v2 v3 = liftIO $ dyn163 ptr_glGetTexEnvxvOES v1 v2 v3

{-# NOINLINE ptr_glGetTexEnvxvOES #-}
ptr_glGetTexEnvxvOES :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
ptr_glGetTexEnvxvOES = unsafePerformIO $ getCommand "glGetTexEnvxvOES"

-- glGetTexFilterFuncSGIS ------------------------------------------------------

glGetTexFilterFuncSGIS
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @filter@ of type @TextureFilterSGIS@.
  -> Ptr GLfloat -- ^ @weights@ pointing to @COMPSIZE(target,filter)@ elements of type @GLfloat@.
  -> m ()
glGetTexFilterFuncSGIS v1 v2 v3 = liftIO $ dyn132 ptr_glGetTexFilterFuncSGIS v1 v2 v3

{-# NOINLINE ptr_glGetTexFilterFuncSGIS #-}
ptr_glGetTexFilterFuncSGIS :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetTexFilterFuncSGIS = unsafePerformIO $ getCommand "glGetTexFilterFuncSGIS"

-- glGetTexGendv ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetTexGen.xml OpenGL 2.x>.
glGetTexGendv
  :: MonadIO m
  => GLenum -- ^ @coord@ of type [TextureCoordName](Graphics-GL-Groups.html#TextureCoordName).
  -> GLenum -- ^ @pname@ of type [TextureGenParameter](Graphics-GL-Groups.html#TextureGenParameter).
  -> Ptr GLdouble -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLdouble@.
  -> m ()
glGetTexGendv v1 v2 v3 = liftIO $ dyn353 ptr_glGetTexGendv v1 v2 v3

{-# NOINLINE ptr_glGetTexGendv #-}
ptr_glGetTexGendv :: FunPtr (GLenum -> GLenum -> Ptr GLdouble -> IO ())
ptr_glGetTexGendv = unsafePerformIO $ getCommand "glGetTexGendv"

-- glGetTexGenfv ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetTexGen.xml OpenGL 2.x>.
glGetTexGenfv
  :: MonadIO m
  => GLenum -- ^ @coord@ of type [TextureCoordName](Graphics-GL-Groups.html#TextureCoordName).
  -> GLenum -- ^ @pname@ of type [TextureGenParameter](Graphics-GL-Groups.html#TextureGenParameter).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetTexGenfv v1 v2 v3 = liftIO $ dyn132 ptr_glGetTexGenfv v1 v2 v3

{-# NOINLINE ptr_glGetTexGenfv #-}
ptr_glGetTexGenfv :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetTexGenfv = unsafePerformIO $ getCommand "glGetTexGenfv"

-- glGetTexGenfvOES ------------------------------------------------------------

glGetTexGenfvOES
  :: MonadIO m
  => GLenum -- ^ @coord@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetTexGenfvOES v1 v2 v3 = liftIO $ dyn132 ptr_glGetTexGenfvOES v1 v2 v3

{-# NOINLINE ptr_glGetTexGenfvOES #-}
ptr_glGetTexGenfvOES :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetTexGenfvOES = unsafePerformIO $ getCommand "glGetTexGenfvOES"

-- glGetTexGeniv ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetTexGen.xml OpenGL 2.x>.
glGetTexGeniv
  :: MonadIO m
  => GLenum -- ^ @coord@ of type [TextureCoordName](Graphics-GL-Groups.html#TextureCoordName).
  -> GLenum -- ^ @pname@ of type [TextureGenParameter](Graphics-GL-Groups.html#TextureGenParameter).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetTexGeniv v1 v2 v3 = liftIO $ dyn133 ptr_glGetTexGeniv v1 v2 v3

{-# NOINLINE ptr_glGetTexGeniv #-}
ptr_glGetTexGeniv :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetTexGeniv = unsafePerformIO $ getCommand "glGetTexGeniv"

-- glGetTexGenivOES ------------------------------------------------------------

glGetTexGenivOES
  :: MonadIO m
  => GLenum -- ^ @coord@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetTexGenivOES v1 v2 v3 = liftIO $ dyn133 ptr_glGetTexGenivOES v1 v2 v3

{-# NOINLINE ptr_glGetTexGenivOES #-}
ptr_glGetTexGenivOES :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetTexGenivOES = unsafePerformIO $ getCommand "glGetTexGenivOES"

-- glGetTexGenxvOES ------------------------------------------------------------

glGetTexGenxvOES
  :: MonadIO m
  => GLenum -- ^ @coord@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLfixed -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfixed@.
  -> m ()
glGetTexGenxvOES v1 v2 v3 = liftIO $ dyn163 ptr_glGetTexGenxvOES v1 v2 v3

{-# NOINLINE ptr_glGetTexGenxvOES #-}
ptr_glGetTexGenxvOES :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
ptr_glGetTexGenxvOES = unsafePerformIO $ getCommand "glGetTexGenxvOES"

-- glGetTexImage ---------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetTexImage.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetTexImage.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetTexImage.xhtml OpenGL 4.x>.
glGetTexImage
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @pixels@ pointing to @COMPSIZE(target,level,format,type)@ elements of type @a@.
  -> m ()
glGetTexImage v1 v2 v3 v4 v5 = liftIO $ dyn409 ptr_glGetTexImage v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetTexImage #-}
ptr_glGetTexImage :: FunPtr (GLenum -> GLint -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glGetTexImage = unsafePerformIO $ getCommand "glGetTexImage"

-- glGetTexLevelParameterfv ----------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetTexLevelParameter.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetTexLevelParameter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetTexLevelParameter.xhtml OpenGL 4.x>.
glGetTexLevelParameterfv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetTexLevelParameterfv v1 v2 v3 v4 = liftIO $ dyn410 ptr_glGetTexLevelParameterfv v1 v2 v3 v4

{-# NOINLINE ptr_glGetTexLevelParameterfv #-}
ptr_glGetTexLevelParameterfv :: FunPtr (GLenum -> GLint -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetTexLevelParameterfv = unsafePerformIO $ getCommand "glGetTexLevelParameterfv"

-- glGetTexLevelParameteriv ----------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetTexLevelParameter.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetTexLevelParameter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetTexLevelParameter.xhtml OpenGL 4.x>.
glGetTexLevelParameteriv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetTexLevelParameteriv v1 v2 v3 v4 = liftIO $ dyn411 ptr_glGetTexLevelParameteriv v1 v2 v3 v4

{-# NOINLINE ptr_glGetTexLevelParameteriv #-}
ptr_glGetTexLevelParameteriv :: FunPtr (GLenum -> GLint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetTexLevelParameteriv = unsafePerformIO $ getCommand "glGetTexLevelParameteriv"

-- glGetTexLevelParameterxvOES -------------------------------------------------

glGetTexLevelParameterxvOES
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLint -- ^ @level@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLfixed -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfixed@.
  -> m ()
glGetTexLevelParameterxvOES v1 v2 v3 v4 = liftIO $ dyn412 ptr_glGetTexLevelParameterxvOES v1 v2 v3 v4

{-# NOINLINE ptr_glGetTexLevelParameterxvOES #-}
ptr_glGetTexLevelParameterxvOES :: FunPtr (GLenum -> GLint -> GLenum -> Ptr GLfixed -> IO ())
ptr_glGetTexLevelParameterxvOES = unsafePerformIO $ getCommand "glGetTexLevelParameterxvOES"

-- glGetTexParameterIiv --------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGetTexParameter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetTexParameter.xhtml OpenGL 4.x>.
glGetTexParameterIiv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetTexParameterIiv v1 v2 v3 = liftIO $ dyn133 ptr_glGetTexParameterIiv v1 v2 v3

{-# NOINLINE ptr_glGetTexParameterIiv #-}
ptr_glGetTexParameterIiv :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetTexParameterIiv = unsafePerformIO $ getCommand "glGetTexParameterIiv"

-- glGetTexParameterIivEXT -----------------------------------------------------

-- | This command is an alias for 'glGetTexParameterIiv'.
glGetTexParameterIivEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetTexParameterIivEXT v1 v2 v3 = liftIO $ dyn133 ptr_glGetTexParameterIivEXT v1 v2 v3

{-# NOINLINE ptr_glGetTexParameterIivEXT #-}
ptr_glGetTexParameterIivEXT :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetTexParameterIivEXT = unsafePerformIO $ getCommand "glGetTexParameterIivEXT"

-- glGetTexParameterIivOES -----------------------------------------------------

-- | This command is an alias for 'glGetTexParameterIiv'.
glGetTexParameterIivOES
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetTexParameterIivOES v1 v2 v3 = liftIO $ dyn133 ptr_glGetTexParameterIivOES v1 v2 v3

{-# NOINLINE ptr_glGetTexParameterIivOES #-}
ptr_glGetTexParameterIivOES :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetTexParameterIivOES = unsafePerformIO $ getCommand "glGetTexParameterIivOES"

-- glGetTexParameterIuiv -------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGetTexParameter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetTexParameter.xhtml OpenGL 4.x>.
glGetTexParameterIuiv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> Ptr GLuint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLuint@.
  -> m ()
glGetTexParameterIuiv v1 v2 v3 = liftIO $ dyn413 ptr_glGetTexParameterIuiv v1 v2 v3

{-# NOINLINE ptr_glGetTexParameterIuiv #-}
ptr_glGetTexParameterIuiv :: FunPtr (GLenum -> GLenum -> Ptr GLuint -> IO ())
ptr_glGetTexParameterIuiv = unsafePerformIO $ getCommand "glGetTexParameterIuiv"

-- glGetTexParameterIuivEXT ----------------------------------------------------

-- | This command is an alias for 'glGetTexParameterIuiv'.
glGetTexParameterIuivEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> Ptr GLuint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLuint@.
  -> m ()
glGetTexParameterIuivEXT v1 v2 v3 = liftIO $ dyn413 ptr_glGetTexParameterIuivEXT v1 v2 v3

{-# NOINLINE ptr_glGetTexParameterIuivEXT #-}
ptr_glGetTexParameterIuivEXT :: FunPtr (GLenum -> GLenum -> Ptr GLuint -> IO ())
ptr_glGetTexParameterIuivEXT = unsafePerformIO $ getCommand "glGetTexParameterIuivEXT"

-- glGetTexParameterIuivOES ----------------------------------------------------

-- | This command is an alias for 'glGetTexParameterIuiv'.
glGetTexParameterIuivOES
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> Ptr GLuint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLuint@.
  -> m ()
glGetTexParameterIuivOES v1 v2 v3 = liftIO $ dyn413 ptr_glGetTexParameterIuivOES v1 v2 v3

{-# NOINLINE ptr_glGetTexParameterIuivOES #-}
ptr_glGetTexParameterIuivOES :: FunPtr (GLenum -> GLenum -> Ptr GLuint -> IO ())
ptr_glGetTexParameterIuivOES = unsafePerformIO $ getCommand "glGetTexParameterIuivOES"

-- glGetTexParameterPointervAPPLE ----------------------------------------------

glGetTexParameterPointervAPPLE
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @pname@.
  -> Ptr (Ptr a) -- ^ @params@ pointing to @1@ element of type @Ptr a@.
  -> m ()
glGetTexParameterPointervAPPLE v1 v2 v3 = liftIO $ dyn316 ptr_glGetTexParameterPointervAPPLE v1 v2 v3

{-# NOINLINE ptr_glGetTexParameterPointervAPPLE #-}
ptr_glGetTexParameterPointervAPPLE :: FunPtr (GLenum -> GLenum -> Ptr (Ptr a) -> IO ())
ptr_glGetTexParameterPointervAPPLE = unsafePerformIO $ getCommand "glGetTexParameterPointervAPPLE"

-- glGetTexParameterfv ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetTexParameter.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetTexParameter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetTexParameter.xhtml OpenGL 4.x>.
glGetTexParameterfv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetTexParameterfv v1 v2 v3 = liftIO $ dyn132 ptr_glGetTexParameterfv v1 v2 v3

{-# NOINLINE ptr_glGetTexParameterfv #-}
ptr_glGetTexParameterfv :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetTexParameterfv = unsafePerformIO $ getCommand "glGetTexParameterfv"

-- glGetTexParameteriv ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetTexParameter.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetTexParameter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetTexParameter.xhtml OpenGL 4.x>.
glGetTexParameteriv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetTexParameteriv v1 v2 v3 = liftIO $ dyn133 ptr_glGetTexParameteriv v1 v2 v3

{-# NOINLINE ptr_glGetTexParameteriv #-}
ptr_glGetTexParameteriv :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetTexParameteriv = unsafePerformIO $ getCommand "glGetTexParameteriv"

