--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F21
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

module Graphics.GL.Functions.F21 (
  glGenerateMipmapOES,
  glGenerateMultiTexMipmapEXT,
  glGenerateTextureMipmap,
  glGenerateTextureMipmapEXT,
  glGetActiveAtomicCounterBufferiv,
  glGetActiveAttrib,
  glGetActiveAttribARB,
  glGetActiveSubroutineName,
  glGetActiveSubroutineUniformName,
  glGetActiveSubroutineUniformiv,
  glGetActiveUniform,
  glGetActiveUniformARB,
  glGetActiveUniformBlockName,
  glGetActiveUniformBlockiv,
  glGetActiveUniformName,
  glGetActiveUniformsiv,
  glGetActiveVaryingNV,
  glGetArrayObjectfvATI,
  glGetArrayObjectivATI,
  glGetAttachedObjectsARB,
  glGetAttachedShaders,
  glGetAttribLocation,
  glGetAttribLocationARB,
  glGetBooleanIndexedvEXT,
  glGetBooleani_v,
  glGetBooleanv,
  glGetBufferParameteri64v,
  glGetBufferParameteriv,
  glGetBufferParameterivARB,
  glGetBufferParameterui64vNV,
  glGetBufferPointerv,
  glGetBufferPointervARB,
  glGetBufferPointervOES,
  glGetBufferSubData,
  glGetBufferSubDataARB,
  glGetClipPlane,
  glGetClipPlanef,
  glGetClipPlanefOES,
  glGetClipPlanex,
  glGetClipPlanexOES
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

-- glGenerateMipmapOES ---------------------------------------------------------

glGenerateMipmapOES
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> m ()
glGenerateMipmapOES v1 = liftIO $ dyn4 ptr_glGenerateMipmapOES v1

{-# NOINLINE ptr_glGenerateMipmapOES #-}
ptr_glGenerateMipmapOES :: FunPtr (GLenum -> IO ())
ptr_glGenerateMipmapOES = unsafePerformIO $ getCommand "glGenerateMipmapOES"

-- glGenerateMultiTexMipmapEXT -------------------------------------------------

glGenerateMultiTexMipmapEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type @TextureUnit@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> m ()
glGenerateMultiTexMipmapEXT v1 v2 = liftIO $ dyn51 ptr_glGenerateMultiTexMipmapEXT v1 v2

{-# NOINLINE ptr_glGenerateMultiTexMipmapEXT #-}
ptr_glGenerateMultiTexMipmapEXT :: FunPtr (GLenum -> GLenum -> IO ())
ptr_glGenerateMultiTexMipmapEXT = unsafePerformIO $ getCommand "glGenerateMultiTexMipmapEXT"

-- glGenerateTextureMipmap -----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGenerateMipmap.xhtml OpenGL 4.x>.
glGenerateTextureMipmap
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> m ()
glGenerateTextureMipmap v1 = liftIO $ dyn2 ptr_glGenerateTextureMipmap v1

{-# NOINLINE ptr_glGenerateTextureMipmap #-}
ptr_glGenerateTextureMipmap :: FunPtr (GLuint -> IO ())
ptr_glGenerateTextureMipmap = unsafePerformIO $ getCommand "glGenerateTextureMipmap"

-- glGenerateTextureMipmapEXT --------------------------------------------------

glGenerateTextureMipmapEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> m ()
glGenerateTextureMipmapEXT v1 v2 = liftIO $ dyn15 ptr_glGenerateTextureMipmapEXT v1 v2

{-# NOINLINE ptr_glGenerateTextureMipmapEXT #-}
ptr_glGenerateTextureMipmapEXT :: FunPtr (GLuint -> GLenum -> IO ())
ptr_glGenerateTextureMipmapEXT = unsafePerformIO $ getCommand "glGenerateTextureMipmapEXT"

-- glGetActiveAtomicCounterBufferiv --------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetActiveAtomicCounterBufferiv.xhtml OpenGL 4.x>.
glGetActiveAtomicCounterBufferiv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLuint -- ^ @bufferIndex@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetActiveAtomicCounterBufferiv v1 v2 v3 v4 = liftIO $ dyn300 ptr_glGetActiveAtomicCounterBufferiv v1 v2 v3 v4

{-# NOINLINE ptr_glGetActiveAtomicCounterBufferiv #-}
ptr_glGetActiveAtomicCounterBufferiv :: FunPtr (GLuint -> GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetActiveAtomicCounterBufferiv = unsafePerformIO $ getCommand "glGetActiveAtomicCounterBufferiv"

-- glGetActiveAttrib -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetActiveAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetActiveAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetActiveAttrib.xhtml OpenGL 4.x>.
glGetActiveAttrib
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLuint -- ^ @index@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLint -- ^ @size@ pointing to @1@ element of type @GLint@.
  -> Ptr GLenum -- ^ @type@ pointing to @1@ element of type @GLenum@.
  -> Ptr GLchar -- ^ @name@ pointing to @bufSize@ elements of type @GLchar@.
  -> m ()
glGetActiveAttrib v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn301 ptr_glGetActiveAttrib v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glGetActiveAttrib #-}
ptr_glGetActiveAttrib :: FunPtr (GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLint -> Ptr GLenum -> Ptr GLchar -> IO ())
ptr_glGetActiveAttrib = unsafePerformIO $ getCommand "glGetActiveAttrib"

-- glGetActiveAttribARB --------------------------------------------------------

-- | This command is an alias for 'glGetActiveAttrib'.
glGetActiveAttribARB
  :: MonadIO m
  => GLhandleARB -- ^ @programObj@ of type @handleARB@.
  -> GLuint -- ^ @index@.
  -> GLsizei -- ^ @maxLength@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLint -- ^ @size@ pointing to @1@ element of type @GLint@.
  -> Ptr GLenum -- ^ @type@ pointing to @1@ element of type @GLenum@.
  -> Ptr GLcharARB -- ^ @name@ pointing to @maxLength@ elements of type @GLcharARB@.
  -> m ()
glGetActiveAttribARB v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn302 ptr_glGetActiveAttribARB v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glGetActiveAttribARB #-}
ptr_glGetActiveAttribARB :: FunPtr (GLhandleARB -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLint -> Ptr GLenum -> Ptr GLcharARB -> IO ())
ptr_glGetActiveAttribARB = unsafePerformIO $ getCommand "glGetActiveAttribARB"

-- glGetActiveSubroutineName ---------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetActiveSubroutineName.xhtml OpenGL 4.x>.
glGetActiveSubroutineName
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @shadertype@.
  -> GLuint -- ^ @index@.
  -> GLsizei -- ^ @bufsize@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLchar -- ^ @name@ pointing to @bufsize@ elements of type @GLchar@.
  -> m ()
glGetActiveSubroutineName v1 v2 v3 v4 v5 v6 = liftIO $ dyn303 ptr_glGetActiveSubroutineName v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glGetActiveSubroutineName #-}
ptr_glGetActiveSubroutineName :: FunPtr (GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
ptr_glGetActiveSubroutineName = unsafePerformIO $ getCommand "glGetActiveSubroutineName"

-- glGetActiveSubroutineUniformName --------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetActiveSubroutineUniformName.xhtml OpenGL 4.x>.
glGetActiveSubroutineUniformName
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @shadertype@.
  -> GLuint -- ^ @index@.
  -> GLsizei -- ^ @bufsize@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLchar -- ^ @name@ pointing to @bufsize@ elements of type @GLchar@.
  -> m ()
glGetActiveSubroutineUniformName v1 v2 v3 v4 v5 v6 = liftIO $ dyn303 ptr_glGetActiveSubroutineUniformName v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glGetActiveSubroutineUniformName #-}
ptr_glGetActiveSubroutineUniformName :: FunPtr (GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
ptr_glGetActiveSubroutineUniformName = unsafePerformIO $ getCommand "glGetActiveSubroutineUniformName"

-- glGetActiveSubroutineUniformiv ----------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetActiveSubroutineUniform.xhtml OpenGL 4.x>.
glGetActiveSubroutineUniformiv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @shadertype@.
  -> GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @values@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetActiveSubroutineUniformiv v1 v2 v3 v4 v5 = liftIO $ dyn304 ptr_glGetActiveSubroutineUniformiv v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetActiveSubroutineUniformiv #-}
ptr_glGetActiveSubroutineUniformiv :: FunPtr (GLuint -> GLenum -> GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetActiveSubroutineUniformiv = unsafePerformIO $ getCommand "glGetActiveSubroutineUniformiv"

-- glGetActiveUniform ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetActiveUniform.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetActiveUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetActiveUniform.xhtml OpenGL 4.x>.
glGetActiveUniform
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLuint -- ^ @index@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLint -- ^ @size@ pointing to @1@ element of type @GLint@.
  -> Ptr GLenum -- ^ @type@ pointing to @1@ element of type @GLenum@.
  -> Ptr GLchar -- ^ @name@ pointing to @bufSize@ elements of type @GLchar@.
  -> m ()
glGetActiveUniform v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn301 ptr_glGetActiveUniform v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glGetActiveUniform #-}
ptr_glGetActiveUniform :: FunPtr (GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLint -> Ptr GLenum -> Ptr GLchar -> IO ())
ptr_glGetActiveUniform = unsafePerformIO $ getCommand "glGetActiveUniform"

-- glGetActiveUniformARB -------------------------------------------------------

-- | This command is an alias for 'glGetActiveUniform'.
glGetActiveUniformARB
  :: MonadIO m
  => GLhandleARB -- ^ @programObj@ of type @handleARB@.
  -> GLuint -- ^ @index@.
  -> GLsizei -- ^ @maxLength@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLint -- ^ @size@ pointing to @1@ element of type @GLint@.
  -> Ptr GLenum -- ^ @type@ pointing to @1@ element of type @GLenum@.
  -> Ptr GLcharARB -- ^ @name@ pointing to @maxLength@ elements of type @GLcharARB@.
  -> m ()
glGetActiveUniformARB v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn302 ptr_glGetActiveUniformARB v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glGetActiveUniformARB #-}
ptr_glGetActiveUniformARB :: FunPtr (GLhandleARB -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLint -> Ptr GLenum -> Ptr GLcharARB -> IO ())
ptr_glGetActiveUniformARB = unsafePerformIO $ getCommand "glGetActiveUniformARB"

-- glGetActiveUniformBlockName -------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGetActiveUniformBlockName.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetActiveUniformBlockName.xhtml OpenGL 4.x>.
glGetActiveUniformBlockName
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLuint -- ^ @uniformBlockIndex@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLchar -- ^ @uniformBlockName@ pointing to @bufSize@ elements of type @GLchar@.
  -> m ()
glGetActiveUniformBlockName v1 v2 v3 v4 v5 = liftIO $ dyn305 ptr_glGetActiveUniformBlockName v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetActiveUniformBlockName #-}
ptr_glGetActiveUniformBlockName :: FunPtr (GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
ptr_glGetActiveUniformBlockName = unsafePerformIO $ getCommand "glGetActiveUniformBlockName"

-- glGetActiveUniformBlockiv ---------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGetActiveUniformBlock.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetActiveUniformBlock.xhtml OpenGL 4.x>.
glGetActiveUniformBlockiv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLuint -- ^ @uniformBlockIndex@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(program,uniformBlockIndex,pname)@ elements of type @GLint@.
  -> m ()
glGetActiveUniformBlockiv v1 v2 v3 v4 = liftIO $ dyn300 ptr_glGetActiveUniformBlockiv v1 v2 v3 v4

{-# NOINLINE ptr_glGetActiveUniformBlockiv #-}
ptr_glGetActiveUniformBlockiv :: FunPtr (GLuint -> GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetActiveUniformBlockiv = unsafePerformIO $ getCommand "glGetActiveUniformBlockiv"

-- glGetActiveUniformName ------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGetActiveUniformName.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetActiveUniformName.xhtml OpenGL 4.x>.
glGetActiveUniformName
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLuint -- ^ @uniformIndex@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLchar -- ^ @uniformName@ pointing to @bufSize@ elements of type @GLchar@.
  -> m ()
glGetActiveUniformName v1 v2 v3 v4 v5 = liftIO $ dyn305 ptr_glGetActiveUniformName v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetActiveUniformName #-}
ptr_glGetActiveUniformName :: FunPtr (GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
ptr_glGetActiveUniformName = unsafePerformIO $ getCommand "glGetActiveUniformName"

-- glGetActiveUniformsiv -------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGetActiveUniformsiv.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetActiveUniformsiv.xhtml OpenGL 4.x>.
glGetActiveUniformsiv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLsizei -- ^ @uniformCount@.
  -> Ptr GLuint -- ^ @uniformIndices@ pointing to @uniformCount@ elements of type @GLuint@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(uniformCount,pname)@ elements of type @GLint@.
  -> m ()
glGetActiveUniformsiv v1 v2 v3 v4 v5 = liftIO $ dyn306 ptr_glGetActiveUniformsiv v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetActiveUniformsiv #-}
ptr_glGetActiveUniformsiv :: FunPtr (GLuint -> GLsizei -> Ptr GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetActiveUniformsiv = unsafePerformIO $ getCommand "glGetActiveUniformsiv"

-- glGetActiveVaryingNV --------------------------------------------------------

glGetActiveVaryingNV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLuint -- ^ @index@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLsizei -- ^ @size@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLenum -- ^ @type@ pointing to @1@ element of type @GLenum@.
  -> Ptr GLchar -- ^ @name@ pointing to @COMPSIZE(program,index,bufSize)@ elements of type @GLchar@.
  -> m ()
glGetActiveVaryingNV v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn307 ptr_glGetActiveVaryingNV v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glGetActiveVaryingNV #-}
ptr_glGetActiveVaryingNV :: FunPtr (GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLsizei -> Ptr GLenum -> Ptr GLchar -> IO ())
ptr_glGetActiveVaryingNV = unsafePerformIO $ getCommand "glGetActiveVaryingNV"

-- glGetArrayObjectfvATI -------------------------------------------------------

glGetArrayObjectfvATI
  :: MonadIO m
  => GLenum -- ^ @array@ of type [EnableCap](Graphics-GL-Groups.html#EnableCap).
  -> GLenum -- ^ @pname@ of type @ArrayObjectPNameATI@.
  -> Ptr GLfloat -- ^ @params@ pointing to @1@ element of type @GLfloat@.
  -> m ()
glGetArrayObjectfvATI v1 v2 v3 = liftIO $ dyn132 ptr_glGetArrayObjectfvATI v1 v2 v3

{-# NOINLINE ptr_glGetArrayObjectfvATI #-}
ptr_glGetArrayObjectfvATI :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetArrayObjectfvATI = unsafePerformIO $ getCommand "glGetArrayObjectfvATI"

-- glGetArrayObjectivATI -------------------------------------------------------

glGetArrayObjectivATI
  :: MonadIO m
  => GLenum -- ^ @array@ of type [EnableCap](Graphics-GL-Groups.html#EnableCap).
  -> GLenum -- ^ @pname@ of type @ArrayObjectPNameATI@.
  -> Ptr GLint -- ^ @params@ pointing to @1@ element of type @GLint@.
  -> m ()
glGetArrayObjectivATI v1 v2 v3 = liftIO $ dyn133 ptr_glGetArrayObjectivATI v1 v2 v3

{-# NOINLINE ptr_glGetArrayObjectivATI #-}
ptr_glGetArrayObjectivATI :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetArrayObjectivATI = unsafePerformIO $ getCommand "glGetArrayObjectivATI"

-- glGetAttachedObjectsARB -----------------------------------------------------

glGetAttachedObjectsARB
  :: MonadIO m
  => GLhandleARB -- ^ @containerObj@ of type @handleARB@.
  -> GLsizei -- ^ @maxCount@.
  -> Ptr GLsizei -- ^ @count@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLhandleARB -- ^ @obj@ pointing to @maxCount@ elements of type @handleARB@.
  -> m ()
glGetAttachedObjectsARB v1 v2 v3 v4 = liftIO $ dyn308 ptr_glGetAttachedObjectsARB v1 v2 v3 v4

{-# NOINLINE ptr_glGetAttachedObjectsARB #-}
ptr_glGetAttachedObjectsARB :: FunPtr (GLhandleARB -> GLsizei -> Ptr GLsizei -> Ptr GLhandleARB -> IO ())
ptr_glGetAttachedObjectsARB = unsafePerformIO $ getCommand "glGetAttachedObjectsARB"

-- glGetAttachedShaders --------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetAttachedShaders.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetAttachedShaders.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetAttachedShaders.xhtml OpenGL 4.x>.
glGetAttachedShaders
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLsizei -- ^ @maxCount@.
  -> Ptr GLsizei -- ^ @count@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLuint -- ^ @shaders@ pointing to @maxCount@ elements of type @GLuint@.
  -> m ()
glGetAttachedShaders v1 v2 v3 v4 = liftIO $ dyn309 ptr_glGetAttachedShaders v1 v2 v3 v4

{-# NOINLINE ptr_glGetAttachedShaders #-}
ptr_glGetAttachedShaders :: FunPtr (GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLuint -> IO ())
ptr_glGetAttachedShaders = unsafePerformIO $ getCommand "glGetAttachedShaders"

-- glGetAttribLocation ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetAttribLocation.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetAttribLocation.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetAttribLocation.xhtml OpenGL 4.x>.
glGetAttribLocation
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> Ptr GLchar -- ^ @name@.
  -> m GLint
glGetAttribLocation v1 v2 = liftIO $ dyn310 ptr_glGetAttribLocation v1 v2

{-# NOINLINE ptr_glGetAttribLocation #-}
ptr_glGetAttribLocation :: FunPtr (GLuint -> Ptr GLchar -> IO GLint)
ptr_glGetAttribLocation = unsafePerformIO $ getCommand "glGetAttribLocation"

-- glGetAttribLocationARB ------------------------------------------------------

-- | This command is an alias for 'glGetAttribLocation'.
glGetAttribLocationARB
  :: MonadIO m
  => GLhandleARB -- ^ @programObj@ of type @handleARB@.
  -> Ptr GLcharARB -- ^ @name@.
  -> m GLint
glGetAttribLocationARB v1 v2 = liftIO $ dyn311 ptr_glGetAttribLocationARB v1 v2

{-# NOINLINE ptr_glGetAttribLocationARB #-}
ptr_glGetAttribLocationARB :: FunPtr (GLhandleARB -> Ptr GLcharARB -> IO GLint)
ptr_glGetAttribLocationARB = unsafePerformIO $ getCommand "glGetAttribLocationARB"

-- glGetBooleanIndexedvEXT -----------------------------------------------------

-- | This command is an alias for 'glGetBooleani_v'.
glGetBooleanIndexedvEXT
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLuint -- ^ @index@.
  -> Ptr GLboolean -- ^ @data@ pointing to @COMPSIZE(target)@ elements of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glGetBooleanIndexedvEXT v1 v2 v3 = liftIO $ dyn312 ptr_glGetBooleanIndexedvEXT v1 v2 v3

{-# NOINLINE ptr_glGetBooleanIndexedvEXT #-}
ptr_glGetBooleanIndexedvEXT :: FunPtr (GLenum -> GLuint -> Ptr GLboolean -> IO ())
ptr_glGetBooleanIndexedvEXT = unsafePerformIO $ getCommand "glGetBooleanIndexedvEXT"

-- glGetBooleani_v -------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGet.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGet.xhtml OpenGL 4.x>.
glGetBooleani_v
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLuint -- ^ @index@.
  -> Ptr GLboolean -- ^ @data@ pointing to @COMPSIZE(target)@ elements of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glGetBooleani_v v1 v2 v3 = liftIO $ dyn312 ptr_glGetBooleani_v v1 v2 v3

{-# NOINLINE ptr_glGetBooleani_v #-}
ptr_glGetBooleani_v :: FunPtr (GLenum -> GLuint -> Ptr GLboolean -> IO ())
ptr_glGetBooleani_v = unsafePerformIO $ getCommand "glGetBooleani_v"

-- glGetBooleanv ---------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGet.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGet.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGet.xhtml OpenGL 4.x>.
glGetBooleanv
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [GetPName](Graphics-GL-Groups.html#GetPName).
  -> Ptr GLboolean -- ^ @data@ pointing to @COMPSIZE(pname)@ elements of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glGetBooleanv v1 v2 = liftIO $ dyn313 ptr_glGetBooleanv v1 v2

{-# NOINLINE ptr_glGetBooleanv #-}
ptr_glGetBooleanv :: FunPtr (GLenum -> Ptr GLboolean -> IO ())
ptr_glGetBooleanv = unsafePerformIO $ getCommand "glGetBooleanv"

-- glGetBufferParameteri64v ----------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGetBufferParameter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetBufferParameter.xhtml OpenGL 4.x>.
glGetBufferParameteri64v
  :: MonadIO m
  => GLenum -- ^ @target@ of type @BufferTargetARB@.
  -> GLenum -- ^ @pname@ of type @BufferPNameARB@.
  -> Ptr GLint64 -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint64@.
  -> m ()
glGetBufferParameteri64v v1 v2 v3 = liftIO $ dyn314 ptr_glGetBufferParameteri64v v1 v2 v3

{-# NOINLINE ptr_glGetBufferParameteri64v #-}
ptr_glGetBufferParameteri64v :: FunPtr (GLenum -> GLenum -> Ptr GLint64 -> IO ())
ptr_glGetBufferParameteri64v = unsafePerformIO $ getCommand "glGetBufferParameteri64v"

-- glGetBufferParameteriv ------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetBufferParameteriv.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetBufferParameter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetBufferParameter.xhtml OpenGL 4.x>.
glGetBufferParameteriv
  :: MonadIO m
  => GLenum -- ^ @target@ of type @BufferTargetARB@.
  -> GLenum -- ^ @pname@ of type @BufferPNameARB@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetBufferParameteriv v1 v2 v3 = liftIO $ dyn133 ptr_glGetBufferParameteriv v1 v2 v3

{-# NOINLINE ptr_glGetBufferParameteriv #-}
ptr_glGetBufferParameteriv :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetBufferParameteriv = unsafePerformIO $ getCommand "glGetBufferParameteriv"

-- glGetBufferParameterivARB ---------------------------------------------------

-- | This command is an alias for 'glGetBufferParameteriv'.
glGetBufferParameterivARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @BufferTargetARB@.
  -> GLenum -- ^ @pname@ of type @BufferPNameARB@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetBufferParameterivARB v1 v2 v3 = liftIO $ dyn133 ptr_glGetBufferParameterivARB v1 v2 v3

{-# NOINLINE ptr_glGetBufferParameterivARB #-}
ptr_glGetBufferParameterivARB :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetBufferParameterivARB = unsafePerformIO $ getCommand "glGetBufferParameterivARB"

-- glGetBufferParameterui64vNV -------------------------------------------------

glGetBufferParameterui64vNV
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLuint64EXT -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLuint64EXT@.
  -> m ()
glGetBufferParameterui64vNV v1 v2 v3 = liftIO $ dyn315 ptr_glGetBufferParameterui64vNV v1 v2 v3

{-# NOINLINE ptr_glGetBufferParameterui64vNV #-}
ptr_glGetBufferParameterui64vNV :: FunPtr (GLenum -> GLenum -> Ptr GLuint64EXT -> IO ())
ptr_glGetBufferParameterui64vNV = unsafePerformIO $ getCommand "glGetBufferParameterui64vNV"

-- glGetBufferPointerv ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetBufferPointerv.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetBufferPointerv.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetBufferPointerv.xhtml OpenGL 4.x>.
glGetBufferPointerv
  :: MonadIO m
  => GLenum -- ^ @target@ of type @BufferTargetARB@.
  -> GLenum -- ^ @pname@ of type @BufferPointerNameARB@.
  -> Ptr (Ptr a) -- ^ @params@ pointing to @1@ element of type @Ptr a@.
  -> m ()
glGetBufferPointerv v1 v2 v3 = liftIO $ dyn316 ptr_glGetBufferPointerv v1 v2 v3

{-# NOINLINE ptr_glGetBufferPointerv #-}
ptr_glGetBufferPointerv :: FunPtr (GLenum -> GLenum -> Ptr (Ptr a) -> IO ())
ptr_glGetBufferPointerv = unsafePerformIO $ getCommand "glGetBufferPointerv"

-- glGetBufferPointervARB ------------------------------------------------------

-- | This command is an alias for 'glGetBufferPointerv'.
glGetBufferPointervARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @BufferTargetARB@.
  -> GLenum -- ^ @pname@ of type @BufferPointerNameARB@.
  -> Ptr (Ptr a) -- ^ @params@ pointing to @1@ element of type @Ptr a@.
  -> m ()
glGetBufferPointervARB v1 v2 v3 = liftIO $ dyn316 ptr_glGetBufferPointervARB v1 v2 v3

{-# NOINLINE ptr_glGetBufferPointervARB #-}
ptr_glGetBufferPointervARB :: FunPtr (GLenum -> GLenum -> Ptr (Ptr a) -> IO ())
ptr_glGetBufferPointervARB = unsafePerformIO $ getCommand "glGetBufferPointervARB"

-- glGetBufferPointervOES ------------------------------------------------------

-- | This command is an alias for 'glGetBufferPointerv'.
glGetBufferPointervOES
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @pname@.
  -> Ptr (Ptr a) -- ^ @params@.
  -> m ()
glGetBufferPointervOES v1 v2 v3 = liftIO $ dyn316 ptr_glGetBufferPointervOES v1 v2 v3

{-# NOINLINE ptr_glGetBufferPointervOES #-}
ptr_glGetBufferPointervOES :: FunPtr (GLenum -> GLenum -> Ptr (Ptr a) -> IO ())
ptr_glGetBufferPointervOES = unsafePerformIO $ getCommand "glGetBufferPointervOES"

-- glGetBufferSubData ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetBufferSubData.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetBufferSubData.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetBufferSubData.xhtml OpenGL 4.x>.
glGetBufferSubData
  :: MonadIO m
  => GLenum -- ^ @target@ of type @BufferTargetARB@.
  -> GLintptr -- ^ @offset@ of type @BufferOffset@.
  -> GLsizeiptr -- ^ @size@ of type @BufferSize@.
  -> Ptr a -- ^ @data@ pointing to @size@ elements of type @a@.
  -> m ()
glGetBufferSubData v1 v2 v3 v4 = liftIO $ dyn64 ptr_glGetBufferSubData v1 v2 v3 v4

{-# NOINLINE ptr_glGetBufferSubData #-}
ptr_glGetBufferSubData :: FunPtr (GLenum -> GLintptr -> GLsizeiptr -> Ptr a -> IO ())
ptr_glGetBufferSubData = unsafePerformIO $ getCommand "glGetBufferSubData"

-- glGetBufferSubDataARB -------------------------------------------------------

-- | This command is an alias for 'glGetBufferSubData'.
glGetBufferSubDataARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @BufferTargetARB@.
  -> GLintptrARB -- ^ @offset@ of type @BufferOffsetARB@.
  -> GLsizeiptrARB -- ^ @size@ of type @BufferSizeARB@.
  -> Ptr a -- ^ @data@ pointing to @size@ elements of type @a@.
  -> m ()
glGetBufferSubDataARB v1 v2 v3 v4 = liftIO $ dyn65 ptr_glGetBufferSubDataARB v1 v2 v3 v4

{-# NOINLINE ptr_glGetBufferSubDataARB #-}
ptr_glGetBufferSubDataARB :: FunPtr (GLenum -> GLintptrARB -> GLsizeiptrARB -> Ptr a -> IO ())
ptr_glGetBufferSubDataARB = unsafePerformIO $ getCommand "glGetBufferSubDataARB"

-- glGetClipPlane --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetClipPlane.xml OpenGL 2.x>.
glGetClipPlane
  :: MonadIO m
  => GLenum -- ^ @plane@ of type [ClipPlaneName](Graphics-GL-Groups.html#ClipPlaneName).
  -> Ptr GLdouble -- ^ @equation@ pointing to @4@ elements of type @GLdouble@.
  -> m ()
glGetClipPlane v1 v2 = liftIO $ dyn93 ptr_glGetClipPlane v1 v2

{-# NOINLINE ptr_glGetClipPlane #-}
ptr_glGetClipPlane :: FunPtr (GLenum -> Ptr GLdouble -> IO ())
ptr_glGetClipPlane = unsafePerformIO $ getCommand "glGetClipPlane"

-- glGetClipPlanef -------------------------------------------------------------

glGetClipPlanef
  :: MonadIO m
  => GLenum -- ^ @plane@.
  -> Ptr GLfloat -- ^ @equation@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glGetClipPlanef v1 v2 = liftIO $ dyn94 ptr_glGetClipPlanef v1 v2

{-# NOINLINE ptr_glGetClipPlanef #-}
ptr_glGetClipPlanef :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glGetClipPlanef = unsafePerformIO $ getCommand "glGetClipPlanef"

-- glGetClipPlanefOES ----------------------------------------------------------

glGetClipPlanefOES
  :: MonadIO m
  => GLenum -- ^ @plane@.
  -> Ptr GLfloat -- ^ @equation@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glGetClipPlanefOES v1 v2 = liftIO $ dyn94 ptr_glGetClipPlanefOES v1 v2

{-# NOINLINE ptr_glGetClipPlanefOES #-}
ptr_glGetClipPlanefOES :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glGetClipPlanefOES = unsafePerformIO $ getCommand "glGetClipPlanefOES"

-- glGetClipPlanex -------------------------------------------------------------

glGetClipPlanex
  :: MonadIO m
  => GLenum -- ^ @plane@.
  -> Ptr GLfixed -- ^ @equation@ pointing to @4@ elements of type @GLfixed@.
  -> m ()
glGetClipPlanex v1 v2 = liftIO $ dyn95 ptr_glGetClipPlanex v1 v2

{-# NOINLINE ptr_glGetClipPlanex #-}
ptr_glGetClipPlanex :: FunPtr (GLenum -> Ptr GLfixed -> IO ())
ptr_glGetClipPlanex = unsafePerformIO $ getCommand "glGetClipPlanex"

-- glGetClipPlanexOES ----------------------------------------------------------

glGetClipPlanexOES
  :: MonadIO m
  => GLenum -- ^ @plane@.
  -> Ptr GLfixed -- ^ @equation@ pointing to @4@ elements of type @GLfixed@.
  -> m ()
glGetClipPlanexOES v1 v2 = liftIO $ dyn95 ptr_glGetClipPlanexOES v1 v2

{-# NOINLINE ptr_glGetClipPlanexOES #-}
ptr_glGetClipPlanexOES :: FunPtr (GLenum -> Ptr GLfixed -> IO ())
ptr_glGetClipPlanexOES = unsafePerformIO $ getCommand "glGetClipPlanexOES"

