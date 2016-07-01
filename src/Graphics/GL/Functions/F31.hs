--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F31
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

module Graphics.GL.Functions.F31 (
  glGetUniformi64vNV,
  glGetUniformiv,
  glGetUniformivARB,
  glGetUniformui64vARB,
  glGetUniformui64vNV,
  glGetUniformuiv,
  glGetUniformuivEXT,
  glGetVariantArrayObjectfvATI,
  glGetVariantArrayObjectivATI,
  glGetVariantBooleanvEXT,
  glGetVariantFloatvEXT,
  glGetVariantIntegervEXT,
  glGetVariantPointervEXT,
  glGetVaryingLocationNV,
  glGetVertexArrayIndexed64iv,
  glGetVertexArrayIndexediv,
  glGetVertexArrayIntegeri_vEXT,
  glGetVertexArrayIntegervEXT,
  glGetVertexArrayPointeri_vEXT,
  glGetVertexArrayPointervEXT,
  glGetVertexArrayiv,
  glGetVertexAttribArrayObjectfvATI,
  glGetVertexAttribArrayObjectivATI,
  glGetVertexAttribIiv,
  glGetVertexAttribIivEXT,
  glGetVertexAttribIuiv,
  glGetVertexAttribIuivEXT,
  glGetVertexAttribLdv,
  glGetVertexAttribLdvEXT,
  glGetVertexAttribLi64vNV,
  glGetVertexAttribLui64vARB,
  glGetVertexAttribLui64vNV,
  glGetVertexAttribPointerv,
  glGetVertexAttribPointervARB,
  glGetVertexAttribPointervNV,
  glGetVertexAttribdv,
  glGetVertexAttribdvARB,
  glGetVertexAttribdvNV,
  glGetVertexAttribfv,
  glGetVertexAttribfvARB
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

-- glGetUniformi64vNV ----------------------------------------------------------

glGetUniformi64vNV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> Ptr GLint64EXT -- ^ @params@ pointing to @COMPSIZE(program,location)@ elements of type @GLint64EXT@.
  -> m ()
glGetUniformi64vNV v1 v2 v3 = liftIO $ dyn434 ptr_glGetUniformi64vNV v1 v2 v3

{-# NOINLINE ptr_glGetUniformi64vNV #-}
ptr_glGetUniformi64vNV :: FunPtr (GLuint -> GLint -> Ptr GLint64EXT -> IO ())
ptr_glGetUniformi64vNV = unsafePerformIO $ getCommand "glGetUniformi64vNV"

-- glGetUniformiv --------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetUniform.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetUniform.xhtml OpenGL 4.x>.
glGetUniformiv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(program,location)@ elements of type @GLint@.
  -> m ()
glGetUniformiv v1 v2 v3 = liftIO $ dyn435 ptr_glGetUniformiv v1 v2 v3

{-# NOINLINE ptr_glGetUniformiv #-}
ptr_glGetUniformiv :: FunPtr (GLuint -> GLint -> Ptr GLint -> IO ())
ptr_glGetUniformiv = unsafePerformIO $ getCommand "glGetUniformiv"

-- glGetUniformivARB -----------------------------------------------------------

-- | This command is an alias for 'glGetUniformiv'.
glGetUniformivARB
  :: MonadIO m
  => GLhandleARB -- ^ @programObj@ of type @handleARB@.
  -> GLint -- ^ @location@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(programObj,location)@ elements of type @GLint@.
  -> m ()
glGetUniformivARB v1 v2 v3 = liftIO $ dyn436 ptr_glGetUniformivARB v1 v2 v3

{-# NOINLINE ptr_glGetUniformivARB #-}
ptr_glGetUniformivARB :: FunPtr (GLhandleARB -> GLint -> Ptr GLint -> IO ())
ptr_glGetUniformivARB = unsafePerformIO $ getCommand "glGetUniformivARB"

-- glGetUniformui64vARB --------------------------------------------------------

glGetUniformui64vARB
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> Ptr GLuint64 -- ^ @params@ pointing to @COMPSIZE(program,location)@ elements of type @GLuint64@.
  -> m ()
glGetUniformui64vARB v1 v2 v3 = liftIO $ dyn437 ptr_glGetUniformui64vARB v1 v2 v3

{-# NOINLINE ptr_glGetUniformui64vARB #-}
ptr_glGetUniformui64vARB :: FunPtr (GLuint -> GLint -> Ptr GLuint64 -> IO ())
ptr_glGetUniformui64vARB = unsafePerformIO $ getCommand "glGetUniformui64vARB"

-- glGetUniformui64vNV ---------------------------------------------------------

glGetUniformui64vNV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> Ptr GLuint64EXT -- ^ @params@ pointing to @COMPSIZE(program,location)@ elements of type @GLuint64EXT@.
  -> m ()
glGetUniformui64vNV v1 v2 v3 = liftIO $ dyn438 ptr_glGetUniformui64vNV v1 v2 v3

{-# NOINLINE ptr_glGetUniformui64vNV #-}
ptr_glGetUniformui64vNV :: FunPtr (GLuint -> GLint -> Ptr GLuint64EXT -> IO ())
ptr_glGetUniformui64vNV = unsafePerformIO $ getCommand "glGetUniformui64vNV"

-- glGetUniformuiv -------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGetUniform.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetUniform.xhtml OpenGL 4.x>.
glGetUniformuiv
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> Ptr GLuint -- ^ @params@ pointing to @COMPSIZE(program,location)@ elements of type @GLuint@.
  -> m ()
glGetUniformuiv v1 v2 v3 = liftIO $ dyn439 ptr_glGetUniformuiv v1 v2 v3

{-# NOINLINE ptr_glGetUniformuiv #-}
ptr_glGetUniformuiv :: FunPtr (GLuint -> GLint -> Ptr GLuint -> IO ())
ptr_glGetUniformuiv = unsafePerformIO $ getCommand "glGetUniformuiv"

-- glGetUniformuivEXT ----------------------------------------------------------

-- | This command is an alias for 'glGetUniformuiv'.
glGetUniformuivEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLint -- ^ @location@.
  -> Ptr GLuint -- ^ @params@ pointing to @COMPSIZE(program,location)@ elements of type @GLuint@.
  -> m ()
glGetUniformuivEXT v1 v2 v3 = liftIO $ dyn439 ptr_glGetUniformuivEXT v1 v2 v3

{-# NOINLINE ptr_glGetUniformuivEXT #-}
ptr_glGetUniformuivEXT :: FunPtr (GLuint -> GLint -> Ptr GLuint -> IO ())
ptr_glGetUniformuivEXT = unsafePerformIO $ getCommand "glGetUniformuivEXT"

-- glGetVariantArrayObjectfvATI ------------------------------------------------

glGetVariantArrayObjectfvATI
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @pname@ of type @ArrayObjectPNameATI@.
  -> Ptr GLfloat -- ^ @params@ pointing to @1@ element of type @GLfloat@.
  -> m ()
glGetVariantArrayObjectfvATI v1 v2 v3 = liftIO $ dyn349 ptr_glGetVariantArrayObjectfvATI v1 v2 v3

{-# NOINLINE ptr_glGetVariantArrayObjectfvATI #-}
ptr_glGetVariantArrayObjectfvATI :: FunPtr (GLuint -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetVariantArrayObjectfvATI = unsafePerformIO $ getCommand "glGetVariantArrayObjectfvATI"

-- glGetVariantArrayObjectivATI ------------------------------------------------

glGetVariantArrayObjectivATI
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @pname@ of type @ArrayObjectPNameATI@.
  -> Ptr GLint -- ^ @params@ pointing to @1@ element of type @GLint@.
  -> m ()
glGetVariantArrayObjectivATI v1 v2 v3 = liftIO $ dyn334 ptr_glGetVariantArrayObjectivATI v1 v2 v3

{-# NOINLINE ptr_glGetVariantArrayObjectivATI #-}
ptr_glGetVariantArrayObjectivATI :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetVariantArrayObjectivATI = unsafePerformIO $ getCommand "glGetVariantArrayObjectivATI"

-- glGetVariantBooleanvEXT -----------------------------------------------------

glGetVariantBooleanvEXT
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @value@ of type @GetVariantValueEXT@.
  -> Ptr GLboolean -- ^ @data@ pointing to @COMPSIZE(id)@ elements of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glGetVariantBooleanvEXT v1 v2 v3 = liftIO $ dyn348 ptr_glGetVariantBooleanvEXT v1 v2 v3

{-# NOINLINE ptr_glGetVariantBooleanvEXT #-}
ptr_glGetVariantBooleanvEXT :: FunPtr (GLuint -> GLenum -> Ptr GLboolean -> IO ())
ptr_glGetVariantBooleanvEXT = unsafePerformIO $ getCommand "glGetVariantBooleanvEXT"

-- glGetVariantFloatvEXT -------------------------------------------------------

glGetVariantFloatvEXT
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @value@ of type @GetVariantValueEXT@.
  -> Ptr GLfloat -- ^ @data@ pointing to @COMPSIZE(id)@ elements of type @GLfloat@.
  -> m ()
glGetVariantFloatvEXT v1 v2 v3 = liftIO $ dyn349 ptr_glGetVariantFloatvEXT v1 v2 v3

{-# NOINLINE ptr_glGetVariantFloatvEXT #-}
ptr_glGetVariantFloatvEXT :: FunPtr (GLuint -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetVariantFloatvEXT = unsafePerformIO $ getCommand "glGetVariantFloatvEXT"

-- glGetVariantIntegervEXT -----------------------------------------------------

glGetVariantIntegervEXT
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @value@ of type @GetVariantValueEXT@.
  -> Ptr GLint -- ^ @data@ pointing to @COMPSIZE(id)@ elements of type @GLint@.
  -> m ()
glGetVariantIntegervEXT v1 v2 v3 = liftIO $ dyn334 ptr_glGetVariantIntegervEXT v1 v2 v3

{-# NOINLINE ptr_glGetVariantIntegervEXT #-}
ptr_glGetVariantIntegervEXT :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetVariantIntegervEXT = unsafePerformIO $ getCommand "glGetVariantIntegervEXT"

-- glGetVariantPointervEXT -----------------------------------------------------

glGetVariantPointervEXT
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @value@ of type @GetVariantValueEXT@.
  -> Ptr (Ptr a) -- ^ @data@ pointing to @COMPSIZE(id)@ elements of type @Ptr a@.
  -> m ()
glGetVariantPointervEXT v1 v2 v3 = liftIO $ dyn361 ptr_glGetVariantPointervEXT v1 v2 v3

{-# NOINLINE ptr_glGetVariantPointervEXT #-}
ptr_glGetVariantPointervEXT :: FunPtr (GLuint -> GLenum -> Ptr (Ptr a) -> IO ())
ptr_glGetVariantPointervEXT = unsafePerformIO $ getCommand "glGetVariantPointervEXT"

-- glGetVaryingLocationNV ------------------------------------------------------

glGetVaryingLocationNV
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> Ptr GLchar -- ^ @name@ pointing to @COMPSIZE(name)@ elements of type @GLchar@.
  -> m GLint
glGetVaryingLocationNV v1 v2 = liftIO $ dyn310 ptr_glGetVaryingLocationNV v1 v2

{-# NOINLINE ptr_glGetVaryingLocationNV #-}
ptr_glGetVaryingLocationNV :: FunPtr (GLuint -> Ptr GLchar -> IO GLint)
ptr_glGetVaryingLocationNV = unsafePerformIO $ getCommand "glGetVaryingLocationNV"

-- glGetVertexArrayIndexed64iv -------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetVertexArrayIndexed.xhtml OpenGL 4.x>.
glGetVertexArrayIndexed64iv
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint64 -- ^ @param@.
  -> m ()
glGetVertexArrayIndexed64iv v1 v2 v3 v4 = liftIO $ dyn440 ptr_glGetVertexArrayIndexed64iv v1 v2 v3 v4

{-# NOINLINE ptr_glGetVertexArrayIndexed64iv #-}
ptr_glGetVertexArrayIndexed64iv :: FunPtr (GLuint -> GLuint -> GLenum -> Ptr GLint64 -> IO ())
ptr_glGetVertexArrayIndexed64iv = unsafePerformIO $ getCommand "glGetVertexArrayIndexed64iv"

-- glGetVertexArrayIndexediv ---------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetVertexArrayIndexed.xhtml OpenGL 4.x>.
glGetVertexArrayIndexediv
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @param@.
  -> m ()
glGetVertexArrayIndexediv v1 v2 v3 v4 = liftIO $ dyn300 ptr_glGetVertexArrayIndexediv v1 v2 v3 v4

{-# NOINLINE ptr_glGetVertexArrayIndexediv #-}
ptr_glGetVertexArrayIndexediv :: FunPtr (GLuint -> GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetVertexArrayIndexediv = unsafePerformIO $ getCommand "glGetVertexArrayIndexediv"

-- glGetVertexArrayIntegeri_vEXT -----------------------------------------------

glGetVertexArrayIntegeri_vEXT
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @param@.
  -> m ()
glGetVertexArrayIntegeri_vEXT v1 v2 v3 v4 = liftIO $ dyn300 ptr_glGetVertexArrayIntegeri_vEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetVertexArrayIntegeri_vEXT #-}
ptr_glGetVertexArrayIntegeri_vEXT :: FunPtr (GLuint -> GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetVertexArrayIntegeri_vEXT = unsafePerformIO $ getCommand "glGetVertexArrayIntegeri_vEXT"

-- glGetVertexArrayIntegervEXT -------------------------------------------------

glGetVertexArrayIntegervEXT
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @param@.
  -> m ()
glGetVertexArrayIntegervEXT v1 v2 v3 = liftIO $ dyn334 ptr_glGetVertexArrayIntegervEXT v1 v2 v3

{-# NOINLINE ptr_glGetVertexArrayIntegervEXT #-}
ptr_glGetVertexArrayIntegervEXT :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetVertexArrayIntegervEXT = unsafePerformIO $ getCommand "glGetVertexArrayIntegervEXT"

-- glGetVertexArrayPointeri_vEXT -----------------------------------------------

glGetVertexArrayPointeri_vEXT
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@.
  -> Ptr (Ptr a) -- ^ @param@.
  -> m ()
glGetVertexArrayPointeri_vEXT v1 v2 v3 v4 = liftIO $ dyn441 ptr_glGetVertexArrayPointeri_vEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetVertexArrayPointeri_vEXT #-}
ptr_glGetVertexArrayPointeri_vEXT :: FunPtr (GLuint -> GLuint -> GLenum -> Ptr (Ptr a) -> IO ())
ptr_glGetVertexArrayPointeri_vEXT = unsafePerformIO $ getCommand "glGetVertexArrayPointeri_vEXT"

-- glGetVertexArrayPointervEXT -------------------------------------------------

glGetVertexArrayPointervEXT
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLenum -- ^ @pname@.
  -> Ptr (Ptr a) -- ^ @param@ pointing to @1@ element of type @Ptr a@.
  -> m ()
glGetVertexArrayPointervEXT v1 v2 v3 = liftIO $ dyn361 ptr_glGetVertexArrayPointervEXT v1 v2 v3

{-# NOINLINE ptr_glGetVertexArrayPointervEXT #-}
ptr_glGetVertexArrayPointervEXT :: FunPtr (GLuint -> GLenum -> Ptr (Ptr a) -> IO ())
ptr_glGetVertexArrayPointervEXT = unsafePerformIO $ getCommand "glGetVertexArrayPointervEXT"

-- glGetVertexArrayiv ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetVertexArrayiv.xhtml OpenGL 4.x>.
glGetVertexArrayiv
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @param@.
  -> m ()
glGetVertexArrayiv v1 v2 v3 = liftIO $ dyn334 ptr_glGetVertexArrayiv v1 v2 v3

{-# NOINLINE ptr_glGetVertexArrayiv #-}
ptr_glGetVertexArrayiv :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetVertexArrayiv = unsafePerformIO $ getCommand "glGetVertexArrayiv"

-- glGetVertexAttribArrayObjectfvATI -------------------------------------------

glGetVertexAttribArrayObjectfvATI
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@ of type @ArrayObjectPNameATI@.
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetVertexAttribArrayObjectfvATI v1 v2 v3 = liftIO $ dyn349 ptr_glGetVertexAttribArrayObjectfvATI v1 v2 v3

{-# NOINLINE ptr_glGetVertexAttribArrayObjectfvATI #-}
ptr_glGetVertexAttribArrayObjectfvATI :: FunPtr (GLuint -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetVertexAttribArrayObjectfvATI = unsafePerformIO $ getCommand "glGetVertexAttribArrayObjectfvATI"

-- glGetVertexAttribArrayObjectivATI -------------------------------------------

glGetVertexAttribArrayObjectivATI
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@ of type @ArrayObjectPNameATI@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetVertexAttribArrayObjectivATI v1 v2 v3 = liftIO $ dyn334 ptr_glGetVertexAttribArrayObjectivATI v1 v2 v3

{-# NOINLINE ptr_glGetVertexAttribArrayObjectivATI #-}
ptr_glGetVertexAttribArrayObjectivATI :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetVertexAttribArrayObjectivATI = unsafePerformIO $ getCommand "glGetVertexAttribArrayObjectivATI"

-- glGetVertexAttribIiv --------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGetVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetVertexAttrib.xhtml OpenGL 4.x>.
glGetVertexAttribIiv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@ of type @VertexAttribEnum@.
  -> Ptr GLint -- ^ @params@ pointing to @1@ element of type @GLint@.
  -> m ()
glGetVertexAttribIiv v1 v2 v3 = liftIO $ dyn334 ptr_glGetVertexAttribIiv v1 v2 v3

{-# NOINLINE ptr_glGetVertexAttribIiv #-}
ptr_glGetVertexAttribIiv :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetVertexAttribIiv = unsafePerformIO $ getCommand "glGetVertexAttribIiv"

-- glGetVertexAttribIivEXT -----------------------------------------------------

-- | This command is an alias for 'glGetVertexAttribIiv'.
glGetVertexAttribIivEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@ of type @VertexAttribEnum@.
  -> Ptr GLint -- ^ @params@ pointing to @1@ element of type @GLint@.
  -> m ()
glGetVertexAttribIivEXT v1 v2 v3 = liftIO $ dyn334 ptr_glGetVertexAttribIivEXT v1 v2 v3

{-# NOINLINE ptr_glGetVertexAttribIivEXT #-}
ptr_glGetVertexAttribIivEXT :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetVertexAttribIivEXT = unsafePerformIO $ getCommand "glGetVertexAttribIivEXT"

-- glGetVertexAttribIuiv -------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGetVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetVertexAttrib.xhtml OpenGL 4.x>.
glGetVertexAttribIuiv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@ of type @VertexAttribEnum@.
  -> Ptr GLuint -- ^ @params@ pointing to @1@ element of type @GLuint@.
  -> m ()
glGetVertexAttribIuiv v1 v2 v3 = liftIO $ dyn375 ptr_glGetVertexAttribIuiv v1 v2 v3

{-# NOINLINE ptr_glGetVertexAttribIuiv #-}
ptr_glGetVertexAttribIuiv :: FunPtr (GLuint -> GLenum -> Ptr GLuint -> IO ())
ptr_glGetVertexAttribIuiv = unsafePerformIO $ getCommand "glGetVertexAttribIuiv"

-- glGetVertexAttribIuivEXT ----------------------------------------------------

-- | This command is an alias for 'glGetVertexAttribIuiv'.
glGetVertexAttribIuivEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@ of type @VertexAttribEnum@.
  -> Ptr GLuint -- ^ @params@ pointing to @1@ element of type @GLuint@.
  -> m ()
glGetVertexAttribIuivEXT v1 v2 v3 = liftIO $ dyn375 ptr_glGetVertexAttribIuivEXT v1 v2 v3

{-# NOINLINE ptr_glGetVertexAttribIuivEXT #-}
ptr_glGetVertexAttribIuivEXT :: FunPtr (GLuint -> GLenum -> Ptr GLuint -> IO ())
ptr_glGetVertexAttribIuivEXT = unsafePerformIO $ getCommand "glGetVertexAttribIuivEXT"

-- glGetVertexAttribLdv --------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetVertexAttrib.xhtml OpenGL 4.x>.
glGetVertexAttribLdv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLdouble -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLdouble@.
  -> m ()
glGetVertexAttribLdv v1 v2 v3 = liftIO $ dyn442 ptr_glGetVertexAttribLdv v1 v2 v3

{-# NOINLINE ptr_glGetVertexAttribLdv #-}
ptr_glGetVertexAttribLdv :: FunPtr (GLuint -> GLenum -> Ptr GLdouble -> IO ())
ptr_glGetVertexAttribLdv = unsafePerformIO $ getCommand "glGetVertexAttribLdv"

-- glGetVertexAttribLdvEXT -----------------------------------------------------

-- | This command is an alias for 'glGetVertexAttribLdv'.
glGetVertexAttribLdvEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLdouble -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLdouble@.
  -> m ()
glGetVertexAttribLdvEXT v1 v2 v3 = liftIO $ dyn442 ptr_glGetVertexAttribLdvEXT v1 v2 v3

{-# NOINLINE ptr_glGetVertexAttribLdvEXT #-}
ptr_glGetVertexAttribLdvEXT :: FunPtr (GLuint -> GLenum -> Ptr GLdouble -> IO ())
ptr_glGetVertexAttribLdvEXT = unsafePerformIO $ getCommand "glGetVertexAttribLdvEXT"

-- glGetVertexAttribLi64vNV ----------------------------------------------------

glGetVertexAttribLi64vNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint64EXT -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint64EXT@.
  -> m ()
glGetVertexAttribLi64vNV v1 v2 v3 = liftIO $ dyn443 ptr_glGetVertexAttribLi64vNV v1 v2 v3

{-# NOINLINE ptr_glGetVertexAttribLi64vNV #-}
ptr_glGetVertexAttribLi64vNV :: FunPtr (GLuint -> GLenum -> Ptr GLint64EXT -> IO ())
ptr_glGetVertexAttribLi64vNV = unsafePerformIO $ getCommand "glGetVertexAttribLi64vNV"

-- glGetVertexAttribLui64vARB --------------------------------------------------

glGetVertexAttribLui64vARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLuint64EXT -- ^ @params@.
  -> m ()
glGetVertexAttribLui64vARB v1 v2 v3 = liftIO $ dyn360 ptr_glGetVertexAttribLui64vARB v1 v2 v3

{-# NOINLINE ptr_glGetVertexAttribLui64vARB #-}
ptr_glGetVertexAttribLui64vARB :: FunPtr (GLuint -> GLenum -> Ptr GLuint64EXT -> IO ())
ptr_glGetVertexAttribLui64vARB = unsafePerformIO $ getCommand "glGetVertexAttribLui64vARB"

-- glGetVertexAttribLui64vNV ---------------------------------------------------

glGetVertexAttribLui64vNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLuint64EXT -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLuint64EXT@.
  -> m ()
glGetVertexAttribLui64vNV v1 v2 v3 = liftIO $ dyn360 ptr_glGetVertexAttribLui64vNV v1 v2 v3

{-# NOINLINE ptr_glGetVertexAttribLui64vNV #-}
ptr_glGetVertexAttribLui64vNV :: FunPtr (GLuint -> GLenum -> Ptr GLuint64EXT -> IO ())
ptr_glGetVertexAttribLui64vNV = unsafePerformIO $ getCommand "glGetVertexAttribLui64vNV"

-- glGetVertexAttribPointerv ---------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetVertexAttribPointerv.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetVertexAttribPointerv.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetVertexAttribPointerv.xhtml OpenGL 4.x>.
glGetVertexAttribPointerv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@ of type @VertexAttribPointerPropertyARB@.
  -> Ptr (Ptr a) -- ^ @pointer@ pointing to @1@ element of type @Ptr a@.
  -> m ()
glGetVertexAttribPointerv v1 v2 v3 = liftIO $ dyn361 ptr_glGetVertexAttribPointerv v1 v2 v3

{-# NOINLINE ptr_glGetVertexAttribPointerv #-}
ptr_glGetVertexAttribPointerv :: FunPtr (GLuint -> GLenum -> Ptr (Ptr a) -> IO ())
ptr_glGetVertexAttribPointerv = unsafePerformIO $ getCommand "glGetVertexAttribPointerv"

-- glGetVertexAttribPointervARB ------------------------------------------------

-- | This command is an alias for 'glGetVertexAttribPointerv'.
glGetVertexAttribPointervARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@ of type @VertexAttribPointerPropertyARB@.
  -> Ptr (Ptr a) -- ^ @pointer@ pointing to @1@ element of type @Ptr a@.
  -> m ()
glGetVertexAttribPointervARB v1 v2 v3 = liftIO $ dyn361 ptr_glGetVertexAttribPointervARB v1 v2 v3

{-# NOINLINE ptr_glGetVertexAttribPointervARB #-}
ptr_glGetVertexAttribPointervARB :: FunPtr (GLuint -> GLenum -> Ptr (Ptr a) -> IO ())
ptr_glGetVertexAttribPointervARB = unsafePerformIO $ getCommand "glGetVertexAttribPointervARB"

-- glGetVertexAttribPointervNV -------------------------------------------------

-- | This command is an alias for 'glGetVertexAttribPointerv'.
glGetVertexAttribPointervNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@ of type @VertexAttribEnumNV@.
  -> Ptr (Ptr a) -- ^ @pointer@ pointing to @1@ element of type @Ptr a@.
  -> m ()
glGetVertexAttribPointervNV v1 v2 v3 = liftIO $ dyn361 ptr_glGetVertexAttribPointervNV v1 v2 v3

{-# NOINLINE ptr_glGetVertexAttribPointervNV #-}
ptr_glGetVertexAttribPointervNV :: FunPtr (GLuint -> GLenum -> Ptr (Ptr a) -> IO ())
ptr_glGetVertexAttribPointervNV = unsafePerformIO $ getCommand "glGetVertexAttribPointervNV"

-- glGetVertexAttribdv ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetVertexAttrib.xhtml OpenGL 4.x>.
glGetVertexAttribdv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@ of type @VertexAttribPropertyARB@.
  -> Ptr GLdouble -- ^ @params@ pointing to @4@ elements of type @GLdouble@.
  -> m ()
glGetVertexAttribdv v1 v2 v3 = liftIO $ dyn442 ptr_glGetVertexAttribdv v1 v2 v3

{-# NOINLINE ptr_glGetVertexAttribdv #-}
ptr_glGetVertexAttribdv :: FunPtr (GLuint -> GLenum -> Ptr GLdouble -> IO ())
ptr_glGetVertexAttribdv = unsafePerformIO $ getCommand "glGetVertexAttribdv"

-- glGetVertexAttribdvARB ------------------------------------------------------

-- | This command is an alias for 'glGetVertexAttribdv'.
glGetVertexAttribdvARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@ of type @VertexAttribPropertyARB@.
  -> Ptr GLdouble -- ^ @params@ pointing to @4@ elements of type @GLdouble@.
  -> m ()
glGetVertexAttribdvARB v1 v2 v3 = liftIO $ dyn442 ptr_glGetVertexAttribdvARB v1 v2 v3

{-# NOINLINE ptr_glGetVertexAttribdvARB #-}
ptr_glGetVertexAttribdvARB :: FunPtr (GLuint -> GLenum -> Ptr GLdouble -> IO ())
ptr_glGetVertexAttribdvARB = unsafePerformIO $ getCommand "glGetVertexAttribdvARB"

-- glGetVertexAttribdvNV -------------------------------------------------------

-- | This command is an alias for 'glGetVertexAttribdv'.
glGetVertexAttribdvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@ of type @VertexAttribEnumNV@.
  -> Ptr GLdouble -- ^ @params@ pointing to @1@ element of type @GLdouble@.
  -> m ()
glGetVertexAttribdvNV v1 v2 v3 = liftIO $ dyn442 ptr_glGetVertexAttribdvNV v1 v2 v3

{-# NOINLINE ptr_glGetVertexAttribdvNV #-}
ptr_glGetVertexAttribdvNV :: FunPtr (GLuint -> GLenum -> Ptr GLdouble -> IO ())
ptr_glGetVertexAttribdvNV = unsafePerformIO $ getCommand "glGetVertexAttribdvNV"

-- glGetVertexAttribfv ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetVertexAttrib.xhtml OpenGL 4.x>.
glGetVertexAttribfv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@ of type @VertexAttribPropertyARB@.
  -> Ptr GLfloat -- ^ @params@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glGetVertexAttribfv v1 v2 v3 = liftIO $ dyn349 ptr_glGetVertexAttribfv v1 v2 v3

{-# NOINLINE ptr_glGetVertexAttribfv #-}
ptr_glGetVertexAttribfv :: FunPtr (GLuint -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetVertexAttribfv = unsafePerformIO $ getCommand "glGetVertexAttribfv"

-- glGetVertexAttribfvARB ------------------------------------------------------

-- | This command is an alias for 'glGetVertexAttribfv'.
glGetVertexAttribfvARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@ of type @VertexAttribPropertyARB@.
  -> Ptr GLfloat -- ^ @params@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glGetVertexAttribfvARB v1 v2 v3 = liftIO $ dyn349 ptr_glGetVertexAttribfvARB v1 v2 v3

{-# NOINLINE ptr_glGetVertexAttribfvARB #-}
ptr_glGetVertexAttribfvARB :: FunPtr (GLuint -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetVertexAttribfvARB = unsafePerformIO $ getCommand "glGetVertexAttribfvARB"

