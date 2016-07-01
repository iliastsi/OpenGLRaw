--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F25
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

module Graphics.GL.Functions.F25 (
  glGetMaterialiv,
  glGetMaterialxOES,
  glGetMaterialxv,
  glGetMaterialxvOES,
  glGetMinmax,
  glGetMinmaxEXT,
  glGetMinmaxParameterfv,
  glGetMinmaxParameterfvEXT,
  glGetMinmaxParameteriv,
  glGetMinmaxParameterivEXT,
  glGetMultiTexEnvfvEXT,
  glGetMultiTexEnvivEXT,
  glGetMultiTexGendvEXT,
  glGetMultiTexGenfvEXT,
  glGetMultiTexGenivEXT,
  glGetMultiTexImageEXT,
  glGetMultiTexLevelParameterfvEXT,
  glGetMultiTexLevelParameterivEXT,
  glGetMultiTexParameterIivEXT,
  glGetMultiTexParameterIuivEXT,
  glGetMultiTexParameterfvEXT,
  glGetMultiTexParameterivEXT,
  glGetMultisamplefv,
  glGetMultisamplefvNV,
  glGetNamedBufferParameteri64v,
  glGetNamedBufferParameteriv,
  glGetNamedBufferParameterivEXT,
  glGetNamedBufferParameterui64vNV,
  glGetNamedBufferPointerv,
  glGetNamedBufferPointervEXT,
  glGetNamedBufferSubData,
  glGetNamedBufferSubDataEXT,
  glGetNamedFramebufferAttachmentParameteriv,
  glGetNamedFramebufferAttachmentParameterivEXT,
  glGetNamedFramebufferParameteriv,
  glGetNamedFramebufferParameterivEXT,
  glGetNamedProgramLocalParameterIivEXT,
  glGetNamedProgramLocalParameterIuivEXT,
  glGetNamedProgramLocalParameterdvEXT,
  glGetNamedProgramLocalParameterfvEXT
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

-- glGetMaterialiv -------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetMaterial.xml OpenGL 2.x>.
glGetMaterialiv
  :: MonadIO m
  => GLenum -- ^ @face@ of type [MaterialFace](Graphics-GL-Groups.html#MaterialFace).
  -> GLenum -- ^ @pname@ of type [MaterialParameter](Graphics-GL-Groups.html#MaterialParameter).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetMaterialiv v1 v2 v3 = liftIO $ dyn133 ptr_glGetMaterialiv v1 v2 v3

{-# NOINLINE ptr_glGetMaterialiv #-}
ptr_glGetMaterialiv :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetMaterialiv = unsafePerformIO $ getCommand "glGetMaterialiv"

-- glGetMaterialxOES -----------------------------------------------------------

glGetMaterialxOES
  :: MonadIO m
  => GLenum -- ^ @face@.
  -> GLenum -- ^ @pname@.
  -> GLfixed -- ^ @param@.
  -> m ()
glGetMaterialxOES v1 v2 v3 = liftIO $ dyn162 ptr_glGetMaterialxOES v1 v2 v3

{-# NOINLINE ptr_glGetMaterialxOES #-}
ptr_glGetMaterialxOES :: FunPtr (GLenum -> GLenum -> GLfixed -> IO ())
ptr_glGetMaterialxOES = unsafePerformIO $ getCommand "glGetMaterialxOES"

-- glGetMaterialxv -------------------------------------------------------------

glGetMaterialxv
  :: MonadIO m
  => GLenum -- ^ @face@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLfixed -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfixed@.
  -> m ()
glGetMaterialxv v1 v2 v3 = liftIO $ dyn163 ptr_glGetMaterialxv v1 v2 v3

{-# NOINLINE ptr_glGetMaterialxv #-}
ptr_glGetMaterialxv :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
ptr_glGetMaterialxv = unsafePerformIO $ getCommand "glGetMaterialxv"

-- glGetMaterialxvOES ----------------------------------------------------------

glGetMaterialxvOES
  :: MonadIO m
  => GLenum -- ^ @face@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLfixed -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfixed@.
  -> m ()
glGetMaterialxvOES v1 v2 v3 = liftIO $ dyn163 ptr_glGetMaterialxvOES v1 v2 v3

{-# NOINLINE ptr_glGetMaterialxvOES #-}
ptr_glGetMaterialxvOES :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
ptr_glGetMaterialxvOES = unsafePerformIO $ getCommand "glGetMaterialxvOES"

-- glGetMinmax -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetMinmax.xml OpenGL 2.x>.
glGetMinmax
  :: MonadIO m
  => GLenum -- ^ @target@ of type @MinmaxTarget@.
  -> GLboolean -- ^ @reset@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @values@ pointing to @COMPSIZE(target,format,type)@ elements of type @a@.
  -> m ()
glGetMinmax v1 v2 v3 v4 v5 = liftIO $ dyn336 ptr_glGetMinmax v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetMinmax #-}
ptr_glGetMinmax :: FunPtr (GLenum -> GLboolean -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glGetMinmax = unsafePerformIO $ getCommand "glGetMinmax"

-- glGetMinmaxEXT --------------------------------------------------------------

glGetMinmaxEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [MinmaxTargetEXT](Graphics-GL-Groups.html#MinmaxTargetEXT).
  -> GLboolean -- ^ @reset@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @values@ pointing to @COMPSIZE(target,format,type)@ elements of type @a@.
  -> m ()
glGetMinmaxEXT v1 v2 v3 v4 v5 = liftIO $ dyn336 ptr_glGetMinmaxEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetMinmaxEXT #-}
ptr_glGetMinmaxEXT :: FunPtr (GLenum -> GLboolean -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glGetMinmaxEXT = unsafePerformIO $ getCommand "glGetMinmaxEXT"

-- glGetMinmaxParameterfv ------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetMinmaxParameter.xml OpenGL 2.x>.
glGetMinmaxParameterfv
  :: MonadIO m
  => GLenum -- ^ @target@ of type @MinmaxTarget@.
  -> GLenum -- ^ @pname@ of type @GetMinmaxParameterPName@.
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetMinmaxParameterfv v1 v2 v3 = liftIO $ dyn132 ptr_glGetMinmaxParameterfv v1 v2 v3

{-# NOINLINE ptr_glGetMinmaxParameterfv #-}
ptr_glGetMinmaxParameterfv :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetMinmaxParameterfv = unsafePerformIO $ getCommand "glGetMinmaxParameterfv"

-- glGetMinmaxParameterfvEXT ---------------------------------------------------

glGetMinmaxParameterfvEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [MinmaxTargetEXT](Graphics-GL-Groups.html#MinmaxTargetEXT).
  -> GLenum -- ^ @pname@ of type [GetMinmaxParameterPNameEXT](Graphics-GL-Groups.html#GetMinmaxParameterPNameEXT).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetMinmaxParameterfvEXT v1 v2 v3 = liftIO $ dyn132 ptr_glGetMinmaxParameterfvEXT v1 v2 v3

{-# NOINLINE ptr_glGetMinmaxParameterfvEXT #-}
ptr_glGetMinmaxParameterfvEXT :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetMinmaxParameterfvEXT = unsafePerformIO $ getCommand "glGetMinmaxParameterfvEXT"

-- glGetMinmaxParameteriv ------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetMinmaxParameter.xml OpenGL 2.x>.
glGetMinmaxParameteriv
  :: MonadIO m
  => GLenum -- ^ @target@ of type @MinmaxTarget@.
  -> GLenum -- ^ @pname@ of type @GetMinmaxParameterPName@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetMinmaxParameteriv v1 v2 v3 = liftIO $ dyn133 ptr_glGetMinmaxParameteriv v1 v2 v3

{-# NOINLINE ptr_glGetMinmaxParameteriv #-}
ptr_glGetMinmaxParameteriv :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetMinmaxParameteriv = unsafePerformIO $ getCommand "glGetMinmaxParameteriv"

-- glGetMinmaxParameterivEXT ---------------------------------------------------

glGetMinmaxParameterivEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [MinmaxTargetEXT](Graphics-GL-Groups.html#MinmaxTargetEXT).
  -> GLenum -- ^ @pname@ of type [GetMinmaxParameterPNameEXT](Graphics-GL-Groups.html#GetMinmaxParameterPNameEXT).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetMinmaxParameterivEXT v1 v2 v3 = liftIO $ dyn133 ptr_glGetMinmaxParameterivEXT v1 v2 v3

{-# NOINLINE ptr_glGetMinmaxParameterivEXT #-}
ptr_glGetMinmaxParameterivEXT :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetMinmaxParameterivEXT = unsafePerformIO $ getCommand "glGetMinmaxParameterivEXT"

-- glGetMultiTexEnvfvEXT -------------------------------------------------------

glGetMultiTexEnvfvEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type @TextureUnit@.
  -> GLenum -- ^ @target@ of type [TextureEnvTarget](Graphics-GL-Groups.html#TextureEnvTarget).
  -> GLenum -- ^ @pname@ of type [TextureEnvParameter](Graphics-GL-Groups.html#TextureEnvParameter).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetMultiTexEnvfvEXT v1 v2 v3 v4 = liftIO $ dyn320 ptr_glGetMultiTexEnvfvEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetMultiTexEnvfvEXT #-}
ptr_glGetMultiTexEnvfvEXT :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetMultiTexEnvfvEXT = unsafePerformIO $ getCommand "glGetMultiTexEnvfvEXT"

-- glGetMultiTexEnvivEXT -------------------------------------------------------

glGetMultiTexEnvivEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type @TextureUnit@.
  -> GLenum -- ^ @target@ of type [TextureEnvTarget](Graphics-GL-Groups.html#TextureEnvTarget).
  -> GLenum -- ^ @pname@ of type [TextureEnvParameter](Graphics-GL-Groups.html#TextureEnvParameter).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetMultiTexEnvivEXT v1 v2 v3 v4 = liftIO $ dyn321 ptr_glGetMultiTexEnvivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetMultiTexEnvivEXT #-}
ptr_glGetMultiTexEnvivEXT :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetMultiTexEnvivEXT = unsafePerformIO $ getCommand "glGetMultiTexEnvivEXT"

-- glGetMultiTexGendvEXT -------------------------------------------------------

glGetMultiTexGendvEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type @TextureUnit@.
  -> GLenum -- ^ @coord@ of type [TextureCoordName](Graphics-GL-Groups.html#TextureCoordName).
  -> GLenum -- ^ @pname@ of type [TextureGenParameter](Graphics-GL-Groups.html#TextureGenParameter).
  -> Ptr GLdouble -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLdouble@.
  -> m ()
glGetMultiTexGendvEXT v1 v2 v3 v4 = liftIO $ dyn354 ptr_glGetMultiTexGendvEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetMultiTexGendvEXT #-}
ptr_glGetMultiTexGendvEXT :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLdouble -> IO ())
ptr_glGetMultiTexGendvEXT = unsafePerformIO $ getCommand "glGetMultiTexGendvEXT"

-- glGetMultiTexGenfvEXT -------------------------------------------------------

glGetMultiTexGenfvEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type @TextureUnit@.
  -> GLenum -- ^ @coord@ of type [TextureCoordName](Graphics-GL-Groups.html#TextureCoordName).
  -> GLenum -- ^ @pname@ of type [TextureGenParameter](Graphics-GL-Groups.html#TextureGenParameter).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetMultiTexGenfvEXT v1 v2 v3 v4 = liftIO $ dyn320 ptr_glGetMultiTexGenfvEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetMultiTexGenfvEXT #-}
ptr_glGetMultiTexGenfvEXT :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetMultiTexGenfvEXT = unsafePerformIO $ getCommand "glGetMultiTexGenfvEXT"

-- glGetMultiTexGenivEXT -------------------------------------------------------

glGetMultiTexGenivEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type @TextureUnit@.
  -> GLenum -- ^ @coord@ of type [TextureCoordName](Graphics-GL-Groups.html#TextureCoordName).
  -> GLenum -- ^ @pname@ of type [TextureGenParameter](Graphics-GL-Groups.html#TextureGenParameter).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetMultiTexGenivEXT v1 v2 v3 v4 = liftIO $ dyn321 ptr_glGetMultiTexGenivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetMultiTexGenivEXT #-}
ptr_glGetMultiTexGenivEXT :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetMultiTexGenivEXT = unsafePerformIO $ getCommand "glGetMultiTexGenivEXT"

-- glGetMultiTexImageEXT -------------------------------------------------------

glGetMultiTexImageEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type @TextureUnit@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @pixels@ pointing to @COMPSIZE(target,level,format,type)@ elements of type @a@.
  -> m ()
glGetMultiTexImageEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn355 ptr_glGetMultiTexImageEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glGetMultiTexImageEXT #-}
ptr_glGetMultiTexImageEXT :: FunPtr (GLenum -> GLenum -> GLint -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glGetMultiTexImageEXT = unsafePerformIO $ getCommand "glGetMultiTexImageEXT"

-- glGetMultiTexLevelParameterfvEXT --------------------------------------------

glGetMultiTexLevelParameterfvEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type @TextureUnit@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetMultiTexLevelParameterfvEXT v1 v2 v3 v4 v5 = liftIO $ dyn356 ptr_glGetMultiTexLevelParameterfvEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetMultiTexLevelParameterfvEXT #-}
ptr_glGetMultiTexLevelParameterfvEXT :: FunPtr (GLenum -> GLenum -> GLint -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetMultiTexLevelParameterfvEXT = unsafePerformIO $ getCommand "glGetMultiTexLevelParameterfvEXT"

-- glGetMultiTexLevelParameterivEXT --------------------------------------------

glGetMultiTexLevelParameterivEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type @TextureUnit@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetMultiTexLevelParameterivEXT v1 v2 v3 v4 v5 = liftIO $ dyn357 ptr_glGetMultiTexLevelParameterivEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetMultiTexLevelParameterivEXT #-}
ptr_glGetMultiTexLevelParameterivEXT :: FunPtr (GLenum -> GLenum -> GLint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetMultiTexLevelParameterivEXT = unsafePerformIO $ getCommand "glGetMultiTexLevelParameterivEXT"

-- glGetMultiTexParameterIivEXT ------------------------------------------------

glGetMultiTexParameterIivEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type @TextureUnit@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetMultiTexParameterIivEXT v1 v2 v3 v4 = liftIO $ dyn321 ptr_glGetMultiTexParameterIivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetMultiTexParameterIivEXT #-}
ptr_glGetMultiTexParameterIivEXT :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetMultiTexParameterIivEXT = unsafePerformIO $ getCommand "glGetMultiTexParameterIivEXT"

-- glGetMultiTexParameterIuivEXT -----------------------------------------------

glGetMultiTexParameterIuivEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type @TextureUnit@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> Ptr GLuint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLuint@.
  -> m ()
glGetMultiTexParameterIuivEXT v1 v2 v3 v4 = liftIO $ dyn358 ptr_glGetMultiTexParameterIuivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetMultiTexParameterIuivEXT #-}
ptr_glGetMultiTexParameterIuivEXT :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLuint -> IO ())
ptr_glGetMultiTexParameterIuivEXT = unsafePerformIO $ getCommand "glGetMultiTexParameterIuivEXT"

-- glGetMultiTexParameterfvEXT -------------------------------------------------

glGetMultiTexParameterfvEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type @TextureUnit@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetMultiTexParameterfvEXT v1 v2 v3 v4 = liftIO $ dyn320 ptr_glGetMultiTexParameterfvEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetMultiTexParameterfvEXT #-}
ptr_glGetMultiTexParameterfvEXT :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetMultiTexParameterfvEXT = unsafePerformIO $ getCommand "glGetMultiTexParameterfvEXT"

-- glGetMultiTexParameterivEXT -------------------------------------------------

glGetMultiTexParameterivEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type @TextureUnit@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @pname@ of type [GetTextureParameter](Graphics-GL-Groups.html#GetTextureParameter).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetMultiTexParameterivEXT v1 v2 v3 v4 = liftIO $ dyn321 ptr_glGetMultiTexParameterivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetMultiTexParameterivEXT #-}
ptr_glGetMultiTexParameterivEXT :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetMultiTexParameterivEXT = unsafePerformIO $ getCommand "glGetMultiTexParameterivEXT"

-- glGetMultisamplefv ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glGetMultisample.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetMultisample.xhtml OpenGL 4.x>.
glGetMultisamplefv
  :: MonadIO m
  => GLenum -- ^ @pname@.
  -> GLuint -- ^ @index@.
  -> Ptr GLfloat -- ^ @val@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetMultisamplefv v1 v2 v3 = liftIO $ dyn267 ptr_glGetMultisamplefv v1 v2 v3

{-# NOINLINE ptr_glGetMultisamplefv #-}
ptr_glGetMultisamplefv :: FunPtr (GLenum -> GLuint -> Ptr GLfloat -> IO ())
ptr_glGetMultisamplefv = unsafePerformIO $ getCommand "glGetMultisamplefv"

-- glGetMultisamplefvNV --------------------------------------------------------

-- | This command is an alias for 'glGetMultisamplefv'.
glGetMultisamplefvNV
  :: MonadIO m
  => GLenum -- ^ @pname@ of type @GetMultisamplePNameNV@.
  -> GLuint -- ^ @index@.
  -> Ptr GLfloat -- ^ @val@ pointing to @2@ elements of type @GLfloat@.
  -> m ()
glGetMultisamplefvNV v1 v2 v3 = liftIO $ dyn267 ptr_glGetMultisamplefvNV v1 v2 v3

{-# NOINLINE ptr_glGetMultisamplefvNV #-}
ptr_glGetMultisamplefvNV :: FunPtr (GLenum -> GLuint -> Ptr GLfloat -> IO ())
ptr_glGetMultisamplefvNV = unsafePerformIO $ getCommand "glGetMultisamplefvNV"

-- glGetNamedBufferParameteri64v -----------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetBufferParameter.xhtml OpenGL 4.x>.
glGetNamedBufferParameteri64v
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint64 -- ^ @params@.
  -> m ()
glGetNamedBufferParameteri64v v1 v2 v3 = liftIO $ dyn359 ptr_glGetNamedBufferParameteri64v v1 v2 v3

{-# NOINLINE ptr_glGetNamedBufferParameteri64v #-}
ptr_glGetNamedBufferParameteri64v :: FunPtr (GLuint -> GLenum -> Ptr GLint64 -> IO ())
ptr_glGetNamedBufferParameteri64v = unsafePerformIO $ getCommand "glGetNamedBufferParameteri64v"

-- glGetNamedBufferParameteriv -------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetBufferParameter.xhtml OpenGL 4.x>.
glGetNamedBufferParameteriv
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @params@.
  -> m ()
glGetNamedBufferParameteriv v1 v2 v3 = liftIO $ dyn334 ptr_glGetNamedBufferParameteriv v1 v2 v3

{-# NOINLINE ptr_glGetNamedBufferParameteriv #-}
ptr_glGetNamedBufferParameteriv :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetNamedBufferParameteriv = unsafePerformIO $ getCommand "glGetNamedBufferParameteriv"

-- glGetNamedBufferParameterivEXT ----------------------------------------------

glGetNamedBufferParameterivEXT
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLenum -- ^ @pname@ of type @VertexBufferObjectParameter@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetNamedBufferParameterivEXT v1 v2 v3 = liftIO $ dyn334 ptr_glGetNamedBufferParameterivEXT v1 v2 v3

{-# NOINLINE ptr_glGetNamedBufferParameterivEXT #-}
ptr_glGetNamedBufferParameterivEXT :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetNamedBufferParameterivEXT = unsafePerformIO $ getCommand "glGetNamedBufferParameterivEXT"

-- glGetNamedBufferParameterui64vNV --------------------------------------------

glGetNamedBufferParameterui64vNV
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLenum -- ^ @pname@ of type @VertexBufferObjectParameter@.
  -> Ptr GLuint64EXT -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLuint64EXT@.
  -> m ()
glGetNamedBufferParameterui64vNV v1 v2 v3 = liftIO $ dyn360 ptr_glGetNamedBufferParameterui64vNV v1 v2 v3

{-# NOINLINE ptr_glGetNamedBufferParameterui64vNV #-}
ptr_glGetNamedBufferParameterui64vNV :: FunPtr (GLuint -> GLenum -> Ptr GLuint64EXT -> IO ())
ptr_glGetNamedBufferParameterui64vNV = unsafePerformIO $ getCommand "glGetNamedBufferParameterui64vNV"

-- glGetNamedBufferPointerv ----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetBufferPointerv.xhtml OpenGL 4.x>.
glGetNamedBufferPointerv
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLenum -- ^ @pname@.
  -> Ptr (Ptr a) -- ^ @params@.
  -> m ()
glGetNamedBufferPointerv v1 v2 v3 = liftIO $ dyn361 ptr_glGetNamedBufferPointerv v1 v2 v3

{-# NOINLINE ptr_glGetNamedBufferPointerv #-}
ptr_glGetNamedBufferPointerv :: FunPtr (GLuint -> GLenum -> Ptr (Ptr a) -> IO ())
ptr_glGetNamedBufferPointerv = unsafePerformIO $ getCommand "glGetNamedBufferPointerv"

-- glGetNamedBufferPointervEXT -------------------------------------------------

glGetNamedBufferPointervEXT
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLenum -- ^ @pname@ of type @VertexBufferObjectParameter@.
  -> Ptr (Ptr a) -- ^ @params@ pointing to @1@ element of type @Ptr a@.
  -> m ()
glGetNamedBufferPointervEXT v1 v2 v3 = liftIO $ dyn361 ptr_glGetNamedBufferPointervEXT v1 v2 v3

{-# NOINLINE ptr_glGetNamedBufferPointervEXT #-}
ptr_glGetNamedBufferPointervEXT :: FunPtr (GLuint -> GLenum -> Ptr (Ptr a) -> IO ())
ptr_glGetNamedBufferPointervEXT = unsafePerformIO $ getCommand "glGetNamedBufferPointervEXT"

-- glGetNamedBufferSubData -----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetBufferSubData.xhtml OpenGL 4.x>.
glGetNamedBufferSubData
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLintptr -- ^ @offset@.
  -> GLsizeiptr -- ^ @size@ of type @BufferSize@.
  -> Ptr a -- ^ @data@.
  -> m ()
glGetNamedBufferSubData v1 v2 v3 v4 = liftIO $ dyn362 ptr_glGetNamedBufferSubData v1 v2 v3 v4

{-# NOINLINE ptr_glGetNamedBufferSubData #-}
ptr_glGetNamedBufferSubData :: FunPtr (GLuint -> GLintptr -> GLsizeiptr -> Ptr a -> IO ())
ptr_glGetNamedBufferSubData = unsafePerformIO $ getCommand "glGetNamedBufferSubData"

-- glGetNamedBufferSubDataEXT --------------------------------------------------

glGetNamedBufferSubDataEXT
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLintptr -- ^ @offset@.
  -> GLsizeiptr -- ^ @size@.
  -> Ptr a -- ^ @data@ pointing to @COMPSIZE(size)@ elements of type @a@.
  -> m ()
glGetNamedBufferSubDataEXT v1 v2 v3 v4 = liftIO $ dyn362 ptr_glGetNamedBufferSubDataEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetNamedBufferSubDataEXT #-}
ptr_glGetNamedBufferSubDataEXT :: FunPtr (GLuint -> GLintptr -> GLsizeiptr -> Ptr a -> IO ())
ptr_glGetNamedBufferSubDataEXT = unsafePerformIO $ getCommand "glGetNamedBufferSubDataEXT"

-- glGetNamedFramebufferAttachmentParameteriv ----------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetFramebufferAttachmentParameter.xhtml OpenGL 4.x>.
glGetNamedFramebufferAttachmentParameteriv
  :: MonadIO m
  => GLuint -- ^ @framebuffer@.
  -> GLenum -- ^ @attachment@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @params@.
  -> m ()
glGetNamedFramebufferAttachmentParameteriv v1 v2 v3 v4 = liftIO $ dyn363 ptr_glGetNamedFramebufferAttachmentParameteriv v1 v2 v3 v4

{-# NOINLINE ptr_glGetNamedFramebufferAttachmentParameteriv #-}
ptr_glGetNamedFramebufferAttachmentParameteriv :: FunPtr (GLuint -> GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetNamedFramebufferAttachmentParameteriv = unsafePerformIO $ getCommand "glGetNamedFramebufferAttachmentParameteriv"

-- glGetNamedFramebufferAttachmentParameterivEXT -------------------------------

glGetNamedFramebufferAttachmentParameterivEXT
  :: MonadIO m
  => GLuint -- ^ @framebuffer@ of type @Framebuffer@.
  -> GLenum -- ^ @attachment@ of type @FramebufferAttachment@.
  -> GLenum -- ^ @pname@ of type @FramebufferAttachmentParameterName@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetNamedFramebufferAttachmentParameterivEXT v1 v2 v3 v4 = liftIO $ dyn363 ptr_glGetNamedFramebufferAttachmentParameterivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetNamedFramebufferAttachmentParameterivEXT #-}
ptr_glGetNamedFramebufferAttachmentParameterivEXT :: FunPtr (GLuint -> GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetNamedFramebufferAttachmentParameterivEXT = unsafePerformIO $ getCommand "glGetNamedFramebufferAttachmentParameterivEXT"

-- glGetNamedFramebufferParameteriv --------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetFramebufferParameter.xhtml OpenGL 4.x>.
glGetNamedFramebufferParameteriv
  :: MonadIO m
  => GLuint -- ^ @framebuffer@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @param@.
  -> m ()
glGetNamedFramebufferParameteriv v1 v2 v3 = liftIO $ dyn334 ptr_glGetNamedFramebufferParameteriv v1 v2 v3

{-# NOINLINE ptr_glGetNamedFramebufferParameteriv #-}
ptr_glGetNamedFramebufferParameteriv :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetNamedFramebufferParameteriv = unsafePerformIO $ getCommand "glGetNamedFramebufferParameteriv"

-- glGetNamedFramebufferParameterivEXT -----------------------------------------

glGetNamedFramebufferParameterivEXT
  :: MonadIO m
  => GLuint -- ^ @framebuffer@ of type @Framebuffer@.
  -> GLenum -- ^ @pname@ of type @GetFramebufferParameter@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetNamedFramebufferParameterivEXT v1 v2 v3 = liftIO $ dyn334 ptr_glGetNamedFramebufferParameterivEXT v1 v2 v3

{-# NOINLINE ptr_glGetNamedFramebufferParameterivEXT #-}
ptr_glGetNamedFramebufferParameterivEXT :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetNamedFramebufferParameterivEXT = unsafePerformIO $ getCommand "glGetNamedFramebufferParameterivEXT"

-- glGetNamedProgramLocalParameterIivEXT ---------------------------------------

glGetNamedProgramLocalParameterIivEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @target@ of type @ProgramTarget@.
  -> GLuint -- ^ @index@.
  -> Ptr GLint -- ^ @params@ pointing to @4@ elements of type @GLint@.
  -> m ()
glGetNamedProgramLocalParameterIivEXT v1 v2 v3 v4 = liftIO $ dyn364 ptr_glGetNamedProgramLocalParameterIivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetNamedProgramLocalParameterIivEXT #-}
ptr_glGetNamedProgramLocalParameterIivEXT :: FunPtr (GLuint -> GLenum -> GLuint -> Ptr GLint -> IO ())
ptr_glGetNamedProgramLocalParameterIivEXT = unsafePerformIO $ getCommand "glGetNamedProgramLocalParameterIivEXT"

-- glGetNamedProgramLocalParameterIuivEXT --------------------------------------

glGetNamedProgramLocalParameterIuivEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @target@ of type @ProgramTarget@.
  -> GLuint -- ^ @index@.
  -> Ptr GLuint -- ^ @params@ pointing to @4@ elements of type @GLuint@.
  -> m ()
glGetNamedProgramLocalParameterIuivEXT v1 v2 v3 v4 = liftIO $ dyn365 ptr_glGetNamedProgramLocalParameterIuivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetNamedProgramLocalParameterIuivEXT #-}
ptr_glGetNamedProgramLocalParameterIuivEXT :: FunPtr (GLuint -> GLenum -> GLuint -> Ptr GLuint -> IO ())
ptr_glGetNamedProgramLocalParameterIuivEXT = unsafePerformIO $ getCommand "glGetNamedProgramLocalParameterIuivEXT"

-- glGetNamedProgramLocalParameterdvEXT ----------------------------------------

glGetNamedProgramLocalParameterdvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @target@ of type @ProgramTarget@.
  -> GLuint -- ^ @index@.
  -> Ptr GLdouble -- ^ @params@ pointing to @4@ elements of type @GLdouble@.
  -> m ()
glGetNamedProgramLocalParameterdvEXT v1 v2 v3 v4 = liftIO $ dyn366 ptr_glGetNamedProgramLocalParameterdvEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetNamedProgramLocalParameterdvEXT #-}
ptr_glGetNamedProgramLocalParameterdvEXT :: FunPtr (GLuint -> GLenum -> GLuint -> Ptr GLdouble -> IO ())
ptr_glGetNamedProgramLocalParameterdvEXT = unsafePerformIO $ getCommand "glGetNamedProgramLocalParameterdvEXT"

-- glGetNamedProgramLocalParameterfvEXT ----------------------------------------

glGetNamedProgramLocalParameterfvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @target@ of type @ProgramTarget@.
  -> GLuint -- ^ @index@.
  -> Ptr GLfloat -- ^ @params@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glGetNamedProgramLocalParameterfvEXT v1 v2 v3 v4 = liftIO $ dyn367 ptr_glGetNamedProgramLocalParameterfvEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetNamedProgramLocalParameterfvEXT #-}
ptr_glGetNamedProgramLocalParameterfvEXT :: FunPtr (GLuint -> GLenum -> GLuint -> Ptr GLfloat -> IO ())
ptr_glGetNamedProgramLocalParameterfvEXT = unsafePerformIO $ getCommand "glGetNamedProgramLocalParameterfvEXT"

