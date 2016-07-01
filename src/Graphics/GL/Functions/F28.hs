--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F28
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

module Graphics.GL.Functions.F28 (
  glGetProgramResourceName,
  glGetProgramResourcefvNV,
  glGetProgramResourceiv,
  glGetProgramStageiv,
  glGetProgramStringARB,
  glGetProgramStringNV,
  glGetProgramSubroutineParameteruivNV,
  glGetProgramiv,
  glGetProgramivARB,
  glGetProgramivNV,
  glGetQueryBufferObjecti64v,
  glGetQueryBufferObjectiv,
  glGetQueryBufferObjectui64v,
  glGetQueryBufferObjectuiv,
  glGetQueryIndexediv,
  glGetQueryObjecti64v,
  glGetQueryObjecti64vEXT,
  glGetQueryObjectiv,
  glGetQueryObjectivARB,
  glGetQueryObjectivEXT,
  glGetQueryObjectui64v,
  glGetQueryObjectui64vEXT,
  glGetQueryObjectuiv,
  glGetQueryObjectuivARB,
  glGetQueryObjectuivEXT,
  glGetQueryiv,
  glGetQueryivARB,
  glGetQueryivEXT,
  glGetRenderbufferParameteriv,
  glGetRenderbufferParameterivEXT,
  glGetRenderbufferParameterivOES,
  glGetSamplerParameterIiv,
  glGetSamplerParameterIivEXT,
  glGetSamplerParameterIivOES,
  glGetSamplerParameterIuiv,
  glGetSamplerParameterIuivEXT,
  glGetSamplerParameterIuivOES,
  glGetSamplerParameterfv,
  glGetSamplerParameteriv,
  glGetSeparableFilter
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

-- glGetProgramResourceName ----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetProgramResourceName.xhtml OpenGL 4.x>.
glGetProgramResourceName
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @programInterface@.
  -> GLuint -- ^ @index@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLchar -- ^ @name@ pointing to @bufSize@ elements of type @GLchar@.
  -> m ()
glGetProgramResourceName v1 v2 v3 v4 v5 v6 = liftIO $ dyn303 ptr_glGetProgramResourceName v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glGetProgramResourceName #-}
ptr_glGetProgramResourceName :: FunPtr (GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
ptr_glGetProgramResourceName = unsafePerformIO $ getCommand "glGetProgramResourceName"

-- glGetProgramResourcefvNV ----------------------------------------------------

glGetProgramResourcefvNV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @programInterface@.
  -> GLuint -- ^ @index@.
  -> GLsizei -- ^ @propCount@.
  -> Ptr GLenum -- ^ @props@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLsizei -- ^ @length@.
  -> Ptr GLfloat -- ^ @params@.
  -> m ()
glGetProgramResourcefvNV v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn398 ptr_glGetProgramResourcefvNV v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glGetProgramResourcefvNV #-}
ptr_glGetProgramResourcefvNV :: FunPtr (GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLenum -> GLsizei -> Ptr GLsizei -> Ptr GLfloat -> IO ())
ptr_glGetProgramResourcefvNV = unsafePerformIO $ getCommand "glGetProgramResourcefvNV"

-- glGetProgramResourceiv ------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetProgramResource.xhtml OpenGL 4.x>.
glGetProgramResourceiv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @programInterface@.
  -> GLuint -- ^ @index@.
  -> GLsizei -- ^ @propCount@.
  -> Ptr GLenum -- ^ @props@ pointing to @propCount@ elements of type @GLenum@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLint -- ^ @params@ pointing to @bufSize@ elements of type @GLint@.
  -> m ()
glGetProgramResourceiv v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn399 ptr_glGetProgramResourceiv v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glGetProgramResourceiv #-}
ptr_glGetProgramResourceiv :: FunPtr (GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLenum -> GLsizei -> Ptr GLsizei -> Ptr GLint -> IO ())
ptr_glGetProgramResourceiv = unsafePerformIO $ getCommand "glGetProgramResourceiv"

-- glGetProgramStageiv ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetProgramStage.xhtml OpenGL 4.x>.
glGetProgramStageiv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @shadertype@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @values@ pointing to @1@ element of type @GLint@.
  -> m ()
glGetProgramStageiv v1 v2 v3 v4 = liftIO $ dyn363 ptr_glGetProgramStageiv v1 v2 v3 v4

{-# NOINLINE ptr_glGetProgramStageiv #-}
ptr_glGetProgramStageiv :: FunPtr (GLuint -> GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetProgramStageiv = unsafePerformIO $ getCommand "glGetProgramStageiv"

-- glGetProgramStringARB -------------------------------------------------------

glGetProgramStringARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ProgramTargetARB@.
  -> GLenum -- ^ @pname@ of type @ProgramStringPropertyARB@.
  -> Ptr a -- ^ @string@ pointing to @COMPSIZE(target,pname)@ elements of type @a@.
  -> m ()
glGetProgramStringARB v1 v2 v3 = liftIO $ dyn242 ptr_glGetProgramStringARB v1 v2 v3

{-# NOINLINE ptr_glGetProgramStringARB #-}
ptr_glGetProgramStringARB :: FunPtr (GLenum -> GLenum -> Ptr a -> IO ())
ptr_glGetProgramStringARB = unsafePerformIO $ getCommand "glGetProgramStringARB"

-- glGetProgramStringNV --------------------------------------------------------

glGetProgramStringNV
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @pname@ of type @VertexAttribEnumNV@.
  -> Ptr GLubyte -- ^ @program@ pointing to @COMPSIZE(id,pname)@ elements of type @ProgramCharacterNV@.
  -> m ()
glGetProgramStringNV v1 v2 v3 = liftIO $ dyn400 ptr_glGetProgramStringNV v1 v2 v3

{-# NOINLINE ptr_glGetProgramStringNV #-}
ptr_glGetProgramStringNV :: FunPtr (GLuint -> GLenum -> Ptr GLubyte -> IO ())
ptr_glGetProgramStringNV = unsafePerformIO $ getCommand "glGetProgramStringNV"

-- glGetProgramSubroutineParameteruivNV ----------------------------------------

glGetProgramSubroutineParameteruivNV
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLuint -- ^ @index@.
  -> Ptr GLuint -- ^ @param@ pointing to @COMPSIZE(target)@ elements of type @GLuint@.
  -> m ()
glGetProgramSubroutineParameteruivNV v1 v2 v3 = liftIO $ dyn214 ptr_glGetProgramSubroutineParameteruivNV v1 v2 v3

{-# NOINLINE ptr_glGetProgramSubroutineParameteruivNV #-}
ptr_glGetProgramSubroutineParameteruivNV :: FunPtr (GLenum -> GLuint -> Ptr GLuint -> IO ())
ptr_glGetProgramSubroutineParameteruivNV = unsafePerformIO $ getCommand "glGetProgramSubroutineParameteruivNV"

-- glGetProgramiv --------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetProgram.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetProgram.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetProgram.xhtml OpenGL 4.x>.
glGetProgramiv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetProgramiv v1 v2 v3 = liftIO $ dyn334 ptr_glGetProgramiv v1 v2 v3

{-# NOINLINE ptr_glGetProgramiv #-}
ptr_glGetProgramiv :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetProgramiv = unsafePerformIO $ getCommand "glGetProgramiv"

-- glGetProgramivARB -----------------------------------------------------------

glGetProgramivARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ProgramTargetARB@.
  -> GLenum -- ^ @pname@ of type @ProgramPropertyARB@.
  -> Ptr GLint -- ^ @params@ pointing to @1@ element of type @GLint@.
  -> m ()
glGetProgramivARB v1 v2 v3 = liftIO $ dyn133 ptr_glGetProgramivARB v1 v2 v3

{-# NOINLINE ptr_glGetProgramivARB #-}
ptr_glGetProgramivARB :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetProgramivARB = unsafePerformIO $ getCommand "glGetProgramivARB"

-- glGetProgramivNV ------------------------------------------------------------

glGetProgramivNV
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @pname@ of type @VertexAttribEnumNV@.
  -> Ptr GLint -- ^ @params@ pointing to @4@ elements of type @GLint@.
  -> m ()
glGetProgramivNV v1 v2 v3 = liftIO $ dyn334 ptr_glGetProgramivNV v1 v2 v3

{-# NOINLINE ptr_glGetProgramivNV #-}
ptr_glGetProgramivNV :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetProgramivNV = unsafePerformIO $ getCommand "glGetProgramivNV"

-- glGetQueryBufferObjecti64v --------------------------------------------------

glGetQueryBufferObjecti64v
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLuint -- ^ @buffer@.
  -> GLenum -- ^ @pname@.
  -> GLintptr -- ^ @offset@.
  -> m ()
glGetQueryBufferObjecti64v v1 v2 v3 v4 = liftIO $ dyn401 ptr_glGetQueryBufferObjecti64v v1 v2 v3 v4

{-# NOINLINE ptr_glGetQueryBufferObjecti64v #-}
ptr_glGetQueryBufferObjecti64v :: FunPtr (GLuint -> GLuint -> GLenum -> GLintptr -> IO ())
ptr_glGetQueryBufferObjecti64v = unsafePerformIO $ getCommand "glGetQueryBufferObjecti64v"

-- glGetQueryBufferObjectiv ----------------------------------------------------

glGetQueryBufferObjectiv
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLuint -- ^ @buffer@.
  -> GLenum -- ^ @pname@.
  -> GLintptr -- ^ @offset@.
  -> m ()
glGetQueryBufferObjectiv v1 v2 v3 v4 = liftIO $ dyn401 ptr_glGetQueryBufferObjectiv v1 v2 v3 v4

{-# NOINLINE ptr_glGetQueryBufferObjectiv #-}
ptr_glGetQueryBufferObjectiv :: FunPtr (GLuint -> GLuint -> GLenum -> GLintptr -> IO ())
ptr_glGetQueryBufferObjectiv = unsafePerformIO $ getCommand "glGetQueryBufferObjectiv"

-- glGetQueryBufferObjectui64v -------------------------------------------------

glGetQueryBufferObjectui64v
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLuint -- ^ @buffer@.
  -> GLenum -- ^ @pname@.
  -> GLintptr -- ^ @offset@.
  -> m ()
glGetQueryBufferObjectui64v v1 v2 v3 v4 = liftIO $ dyn401 ptr_glGetQueryBufferObjectui64v v1 v2 v3 v4

{-# NOINLINE ptr_glGetQueryBufferObjectui64v #-}
ptr_glGetQueryBufferObjectui64v :: FunPtr (GLuint -> GLuint -> GLenum -> GLintptr -> IO ())
ptr_glGetQueryBufferObjectui64v = unsafePerformIO $ getCommand "glGetQueryBufferObjectui64v"

-- glGetQueryBufferObjectuiv ---------------------------------------------------

glGetQueryBufferObjectuiv
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLuint -- ^ @buffer@.
  -> GLenum -- ^ @pname@.
  -> GLintptr -- ^ @offset@.
  -> m ()
glGetQueryBufferObjectuiv v1 v2 v3 v4 = liftIO $ dyn401 ptr_glGetQueryBufferObjectuiv v1 v2 v3 v4

{-# NOINLINE ptr_glGetQueryBufferObjectuiv #-}
ptr_glGetQueryBufferObjectuiv :: FunPtr (GLuint -> GLuint -> GLenum -> GLintptr -> IO ())
ptr_glGetQueryBufferObjectuiv = unsafePerformIO $ getCommand "glGetQueryBufferObjectuiv"

-- glGetQueryIndexediv ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetQueryIndexed.xhtml OpenGL 4.x>.
glGetQueryIndexediv
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetQueryIndexediv v1 v2 v3 v4 = liftIO $ dyn351 ptr_glGetQueryIndexediv v1 v2 v3 v4

{-# NOINLINE ptr_glGetQueryIndexediv #-}
ptr_glGetQueryIndexediv :: FunPtr (GLenum -> GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetQueryIndexediv = unsafePerformIO $ getCommand "glGetQueryIndexediv"

-- glGetQueryObjecti64v --------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGetQueryObject.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetQueryObject.xhtml OpenGL 4.x>.
glGetQueryObjecti64v
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint64 -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint64@.
  -> m ()
glGetQueryObjecti64v v1 v2 v3 = liftIO $ dyn359 ptr_glGetQueryObjecti64v v1 v2 v3

{-# NOINLINE ptr_glGetQueryObjecti64v #-}
ptr_glGetQueryObjecti64v :: FunPtr (GLuint -> GLenum -> Ptr GLint64 -> IO ())
ptr_glGetQueryObjecti64v = unsafePerformIO $ getCommand "glGetQueryObjecti64v"

-- glGetQueryObjecti64vEXT -----------------------------------------------------

-- | This command is an alias for 'glGetQueryObjecti64v'.
glGetQueryObjecti64vEXT
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint64 -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint64@.
  -> m ()
glGetQueryObjecti64vEXT v1 v2 v3 = liftIO $ dyn359 ptr_glGetQueryObjecti64vEXT v1 v2 v3

{-# NOINLINE ptr_glGetQueryObjecti64vEXT #-}
ptr_glGetQueryObjecti64vEXT :: FunPtr (GLuint -> GLenum -> Ptr GLint64 -> IO ())
ptr_glGetQueryObjecti64vEXT = unsafePerformIO $ getCommand "glGetQueryObjecti64vEXT"

-- glGetQueryObjectiv ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetQueryObject.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetQueryObject.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetQueryObject.xhtml OpenGL 4.x>.
glGetQueryObjectiv
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetQueryObjectiv v1 v2 v3 = liftIO $ dyn334 ptr_glGetQueryObjectiv v1 v2 v3

{-# NOINLINE ptr_glGetQueryObjectiv #-}
ptr_glGetQueryObjectiv :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetQueryObjectiv = unsafePerformIO $ getCommand "glGetQueryObjectiv"

-- glGetQueryObjectivARB -------------------------------------------------------

-- | This command is an alias for 'glGetQueryObjectiv'.
glGetQueryObjectivARB
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetQueryObjectivARB v1 v2 v3 = liftIO $ dyn334 ptr_glGetQueryObjectivARB v1 v2 v3

{-# NOINLINE ptr_glGetQueryObjectivARB #-}
ptr_glGetQueryObjectivARB :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetQueryObjectivARB = unsafePerformIO $ getCommand "glGetQueryObjectivARB"

-- glGetQueryObjectivEXT -------------------------------------------------------

-- | This command is an alias for 'glGetQueryObjectiv'.
glGetQueryObjectivEXT
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @params@.
  -> m ()
glGetQueryObjectivEXT v1 v2 v3 = liftIO $ dyn334 ptr_glGetQueryObjectivEXT v1 v2 v3

{-# NOINLINE ptr_glGetQueryObjectivEXT #-}
ptr_glGetQueryObjectivEXT :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetQueryObjectivEXT = unsafePerformIO $ getCommand "glGetQueryObjectivEXT"

-- glGetQueryObjectui64v -------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGetQueryObject.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetQueryObject.xhtml OpenGL 4.x>.
glGetQueryObjectui64v
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLuint64 -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLuint64@.
  -> m ()
glGetQueryObjectui64v v1 v2 v3 = liftIO $ dyn402 ptr_glGetQueryObjectui64v v1 v2 v3

{-# NOINLINE ptr_glGetQueryObjectui64v #-}
ptr_glGetQueryObjectui64v :: FunPtr (GLuint -> GLenum -> Ptr GLuint64 -> IO ())
ptr_glGetQueryObjectui64v = unsafePerformIO $ getCommand "glGetQueryObjectui64v"

-- glGetQueryObjectui64vEXT ----------------------------------------------------

-- | This command is an alias for 'glGetQueryObjectui64v'.
glGetQueryObjectui64vEXT
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLuint64 -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLuint64@.
  -> m ()
glGetQueryObjectui64vEXT v1 v2 v3 = liftIO $ dyn402 ptr_glGetQueryObjectui64vEXT v1 v2 v3

{-# NOINLINE ptr_glGetQueryObjectui64vEXT #-}
ptr_glGetQueryObjectui64vEXT :: FunPtr (GLuint -> GLenum -> Ptr GLuint64 -> IO ())
ptr_glGetQueryObjectui64vEXT = unsafePerformIO $ getCommand "glGetQueryObjectui64vEXT"

-- glGetQueryObjectuiv ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetQueryObject.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetQueryObject.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetQueryObject.xhtml OpenGL 4.x>.
glGetQueryObjectuiv
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLuint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLuint@.
  -> m ()
glGetQueryObjectuiv v1 v2 v3 = liftIO $ dyn375 ptr_glGetQueryObjectuiv v1 v2 v3

{-# NOINLINE ptr_glGetQueryObjectuiv #-}
ptr_glGetQueryObjectuiv :: FunPtr (GLuint -> GLenum -> Ptr GLuint -> IO ())
ptr_glGetQueryObjectuiv = unsafePerformIO $ getCommand "glGetQueryObjectuiv"

-- glGetQueryObjectuivARB ------------------------------------------------------

-- | This command is an alias for 'glGetQueryObjectuiv'.
glGetQueryObjectuivARB
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLuint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLuint@.
  -> m ()
glGetQueryObjectuivARB v1 v2 v3 = liftIO $ dyn375 ptr_glGetQueryObjectuivARB v1 v2 v3

{-# NOINLINE ptr_glGetQueryObjectuivARB #-}
ptr_glGetQueryObjectuivARB :: FunPtr (GLuint -> GLenum -> Ptr GLuint -> IO ())
ptr_glGetQueryObjectuivARB = unsafePerformIO $ getCommand "glGetQueryObjectuivARB"

-- glGetQueryObjectuivEXT ------------------------------------------------------

glGetQueryObjectuivEXT
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLuint -- ^ @params@.
  -> m ()
glGetQueryObjectuivEXT v1 v2 v3 = liftIO $ dyn375 ptr_glGetQueryObjectuivEXT v1 v2 v3

{-# NOINLINE ptr_glGetQueryObjectuivEXT #-}
ptr_glGetQueryObjectuivEXT :: FunPtr (GLuint -> GLenum -> Ptr GLuint -> IO ())
ptr_glGetQueryObjectuivEXT = unsafePerformIO $ getCommand "glGetQueryObjectuivEXT"

-- glGetQueryiv ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetQueryiv.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetQueryiv.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetQueryiv.xhtml OpenGL 4.x>.
glGetQueryiv
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetQueryiv v1 v2 v3 = liftIO $ dyn133 ptr_glGetQueryiv v1 v2 v3

{-# NOINLINE ptr_glGetQueryiv #-}
ptr_glGetQueryiv :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetQueryiv = unsafePerformIO $ getCommand "glGetQueryiv"

-- glGetQueryivARB -------------------------------------------------------------

-- | This command is an alias for 'glGetQueryiv'.
glGetQueryivARB
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetQueryivARB v1 v2 v3 = liftIO $ dyn133 ptr_glGetQueryivARB v1 v2 v3

{-# NOINLINE ptr_glGetQueryivARB #-}
ptr_glGetQueryivARB :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetQueryivARB = unsafePerformIO $ getCommand "glGetQueryivARB"

-- glGetQueryivEXT -------------------------------------------------------------

glGetQueryivEXT
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @params@.
  -> m ()
glGetQueryivEXT v1 v2 v3 = liftIO $ dyn133 ptr_glGetQueryivEXT v1 v2 v3

{-# NOINLINE ptr_glGetQueryivEXT #-}
ptr_glGetQueryivEXT :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetQueryivEXT = unsafePerformIO $ getCommand "glGetQueryivEXT"

-- glGetRenderbufferParameteriv ------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGetRenderbufferParameter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetRenderbufferParameter.xhtml OpenGL 4.x>.
glGetRenderbufferParameteriv
  :: MonadIO m
  => GLenum -- ^ @target@ of type @RenderbufferTarget@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetRenderbufferParameteriv v1 v2 v3 = liftIO $ dyn133 ptr_glGetRenderbufferParameteriv v1 v2 v3

{-# NOINLINE ptr_glGetRenderbufferParameteriv #-}
ptr_glGetRenderbufferParameteriv :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetRenderbufferParameteriv = unsafePerformIO $ getCommand "glGetRenderbufferParameteriv"

-- glGetRenderbufferParameterivEXT ---------------------------------------------

-- | This command is an alias for 'glGetRenderbufferParameteriv'.
glGetRenderbufferParameterivEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type @RenderbufferTarget@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetRenderbufferParameterivEXT v1 v2 v3 = liftIO $ dyn133 ptr_glGetRenderbufferParameterivEXT v1 v2 v3

{-# NOINLINE ptr_glGetRenderbufferParameterivEXT #-}
ptr_glGetRenderbufferParameterivEXT :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetRenderbufferParameterivEXT = unsafePerformIO $ getCommand "glGetRenderbufferParameterivEXT"

-- glGetRenderbufferParameterivOES ---------------------------------------------

glGetRenderbufferParameterivOES
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetRenderbufferParameterivOES v1 v2 v3 = liftIO $ dyn133 ptr_glGetRenderbufferParameterivOES v1 v2 v3

{-# NOINLINE ptr_glGetRenderbufferParameterivOES #-}
ptr_glGetRenderbufferParameterivOES :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetRenderbufferParameterivOES = unsafePerformIO $ getCommand "glGetRenderbufferParameterivOES"

-- glGetSamplerParameterIiv ----------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGetSamplerParameter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetSamplerParameter.xhtml OpenGL 4.x>.
glGetSamplerParameterIiv
  :: MonadIO m
  => GLuint -- ^ @sampler@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetSamplerParameterIiv v1 v2 v3 = liftIO $ dyn334 ptr_glGetSamplerParameterIiv v1 v2 v3

{-# NOINLINE ptr_glGetSamplerParameterIiv #-}
ptr_glGetSamplerParameterIiv :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetSamplerParameterIiv = unsafePerformIO $ getCommand "glGetSamplerParameterIiv"

-- glGetSamplerParameterIivEXT -------------------------------------------------

-- | This command is an alias for 'glGetSamplerParameterIiv'.
glGetSamplerParameterIivEXT
  :: MonadIO m
  => GLuint -- ^ @sampler@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetSamplerParameterIivEXT v1 v2 v3 = liftIO $ dyn334 ptr_glGetSamplerParameterIivEXT v1 v2 v3

{-# NOINLINE ptr_glGetSamplerParameterIivEXT #-}
ptr_glGetSamplerParameterIivEXT :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetSamplerParameterIivEXT = unsafePerformIO $ getCommand "glGetSamplerParameterIivEXT"

-- glGetSamplerParameterIivOES -------------------------------------------------

-- | This command is an alias for 'glGetSamplerParameterIiv'.
glGetSamplerParameterIivOES
  :: MonadIO m
  => GLuint -- ^ @sampler@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetSamplerParameterIivOES v1 v2 v3 = liftIO $ dyn334 ptr_glGetSamplerParameterIivOES v1 v2 v3

{-# NOINLINE ptr_glGetSamplerParameterIivOES #-}
ptr_glGetSamplerParameterIivOES :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetSamplerParameterIivOES = unsafePerformIO $ getCommand "glGetSamplerParameterIivOES"

-- glGetSamplerParameterIuiv ---------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGetSamplerParameter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetSamplerParameter.xhtml OpenGL 4.x>.
glGetSamplerParameterIuiv
  :: MonadIO m
  => GLuint -- ^ @sampler@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLuint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLuint@.
  -> m ()
glGetSamplerParameterIuiv v1 v2 v3 = liftIO $ dyn375 ptr_glGetSamplerParameterIuiv v1 v2 v3

{-# NOINLINE ptr_glGetSamplerParameterIuiv #-}
ptr_glGetSamplerParameterIuiv :: FunPtr (GLuint -> GLenum -> Ptr GLuint -> IO ())
ptr_glGetSamplerParameterIuiv = unsafePerformIO $ getCommand "glGetSamplerParameterIuiv"

-- glGetSamplerParameterIuivEXT ------------------------------------------------

-- | This command is an alias for 'glGetSamplerParameterIuiv'.
glGetSamplerParameterIuivEXT
  :: MonadIO m
  => GLuint -- ^ @sampler@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLuint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLuint@.
  -> m ()
glGetSamplerParameterIuivEXT v1 v2 v3 = liftIO $ dyn375 ptr_glGetSamplerParameterIuivEXT v1 v2 v3

{-# NOINLINE ptr_glGetSamplerParameterIuivEXT #-}
ptr_glGetSamplerParameterIuivEXT :: FunPtr (GLuint -> GLenum -> Ptr GLuint -> IO ())
ptr_glGetSamplerParameterIuivEXT = unsafePerformIO $ getCommand "glGetSamplerParameterIuivEXT"

-- glGetSamplerParameterIuivOES ------------------------------------------------

-- | This command is an alias for 'glGetSamplerParameterIuiv'.
glGetSamplerParameterIuivOES
  :: MonadIO m
  => GLuint -- ^ @sampler@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLuint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLuint@.
  -> m ()
glGetSamplerParameterIuivOES v1 v2 v3 = liftIO $ dyn375 ptr_glGetSamplerParameterIuivOES v1 v2 v3

{-# NOINLINE ptr_glGetSamplerParameterIuivOES #-}
ptr_glGetSamplerParameterIuivOES :: FunPtr (GLuint -> GLenum -> Ptr GLuint -> IO ())
ptr_glGetSamplerParameterIuivOES = unsafePerformIO $ getCommand "glGetSamplerParameterIuivOES"

-- glGetSamplerParameterfv -----------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGetSamplerParameter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetSamplerParameter.xhtml OpenGL 4.x>.
glGetSamplerParameterfv
  :: MonadIO m
  => GLuint -- ^ @sampler@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetSamplerParameterfv v1 v2 v3 = liftIO $ dyn349 ptr_glGetSamplerParameterfv v1 v2 v3

{-# NOINLINE ptr_glGetSamplerParameterfv #-}
ptr_glGetSamplerParameterfv :: FunPtr (GLuint -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetSamplerParameterfv = unsafePerformIO $ getCommand "glGetSamplerParameterfv"

-- glGetSamplerParameteriv -----------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGetSamplerParameter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetSamplerParameter.xhtml OpenGL 4.x>.
glGetSamplerParameteriv
  :: MonadIO m
  => GLuint -- ^ @sampler@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetSamplerParameteriv v1 v2 v3 = liftIO $ dyn334 ptr_glGetSamplerParameteriv v1 v2 v3

{-# NOINLINE ptr_glGetSamplerParameteriv #-}
ptr_glGetSamplerParameteriv :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetSamplerParameteriv = unsafePerformIO $ getCommand "glGetSamplerParameteriv"

-- glGetSeparableFilter --------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetSeparableFilter.xml OpenGL 2.x>.
glGetSeparableFilter
  :: MonadIO m
  => GLenum -- ^ @target@ of type @SeparableTarget@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @row@ pointing to @COMPSIZE(target,format,type)@ elements of type @a@.
  -> Ptr b -- ^ @column@ pointing to @COMPSIZE(target,format,type)@ elements of type @b@.
  -> Ptr c -- ^ @span@ pointing to @COMPSIZE(target,format,type)@ elements of type @c@.
  -> m ()
glGetSeparableFilter v1 v2 v3 v4 v5 v6 = liftIO $ dyn403 ptr_glGetSeparableFilter v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glGetSeparableFilter #-}
ptr_glGetSeparableFilter :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr a -> Ptr b -> Ptr c -> IO ())
ptr_glGetSeparableFilter = unsafePerformIO $ getCommand "glGetSeparableFilter"

