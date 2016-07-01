--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F42
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

module Graphics.GL.Functions.F42 (
  glMultiTexCoord4dvARB,
  glMultiTexCoord4f,
  glMultiTexCoord4fARB,
  glMultiTexCoord4fv,
  glMultiTexCoord4fvARB,
  glMultiTexCoord4hNV,
  glMultiTexCoord4hvNV,
  glMultiTexCoord4i,
  glMultiTexCoord4iARB,
  glMultiTexCoord4iv,
  glMultiTexCoord4ivARB,
  glMultiTexCoord4s,
  glMultiTexCoord4sARB,
  glMultiTexCoord4sv,
  glMultiTexCoord4svARB,
  glMultiTexCoord4x,
  glMultiTexCoord4xOES,
  glMultiTexCoord4xvOES,
  glMultiTexCoordP1ui,
  glMultiTexCoordP1uiv,
  glMultiTexCoordP2ui,
  glMultiTexCoordP2uiv,
  glMultiTexCoordP3ui,
  glMultiTexCoordP3uiv,
  glMultiTexCoordP4ui,
  glMultiTexCoordP4uiv,
  glMultiTexCoordPointerEXT,
  glMultiTexEnvfEXT,
  glMultiTexEnvfvEXT,
  glMultiTexEnviEXT,
  glMultiTexEnvivEXT,
  glMultiTexGendEXT,
  glMultiTexGendvEXT,
  glMultiTexGenfEXT,
  glMultiTexGenfvEXT,
  glMultiTexGeniEXT,
  glMultiTexGenivEXT,
  glMultiTexImage1DEXT,
  glMultiTexImage2DEXT,
  glMultiTexImage3DEXT
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

-- glMultiTexCoord4dvARB -------------------------------------------------------

-- | This command is an alias for 'glMultiTexCoord4dv'.
glMultiTexCoord4dvARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> Ptr GLdouble -- ^ @v@ pointing to @4@ elements of type @CoordD@.
  -> m ()
glMultiTexCoord4dvARB v1 v2 = liftIO $ dyn93 ptr_glMultiTexCoord4dvARB v1 v2

{-# NOINLINE ptr_glMultiTexCoord4dvARB #-}
ptr_glMultiTexCoord4dvARB :: FunPtr (GLenum -> Ptr GLdouble -> IO ())
ptr_glMultiTexCoord4dvARB = unsafePerformIO $ getCommand "glMultiTexCoord4dvARB"

-- glMultiTexCoord4f -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glMultiTexCoord4fv'.
glMultiTexCoord4f
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> GLfloat -- ^ @s@ of type @CoordF@.
  -> GLfloat -- ^ @t@ of type @CoordF@.
  -> GLfloat -- ^ @r@ of type @CoordF@.
  -> GLfloat -- ^ @q@ of type @CoordF@.
  -> m ()
glMultiTexCoord4f v1 v2 v3 v4 v5 = liftIO $ dyn521 ptr_glMultiTexCoord4f v1 v2 v3 v4 v5

{-# NOINLINE ptr_glMultiTexCoord4f #-}
ptr_glMultiTexCoord4f :: FunPtr (GLenum -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glMultiTexCoord4f = unsafePerformIO $ getCommand "glMultiTexCoord4f"

-- glMultiTexCoord4fARB --------------------------------------------------------

-- | The vector equivalent of this command is 'glMultiTexCoord4fv'. This command is an alias for 'glMultiTexCoord4f'.
glMultiTexCoord4fARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> GLfloat -- ^ @s@ of type @CoordF@.
  -> GLfloat -- ^ @t@ of type @CoordF@.
  -> GLfloat -- ^ @r@ of type @CoordF@.
  -> GLfloat -- ^ @q@ of type @CoordF@.
  -> m ()
glMultiTexCoord4fARB v1 v2 v3 v4 v5 = liftIO $ dyn521 ptr_glMultiTexCoord4fARB v1 v2 v3 v4 v5

{-# NOINLINE ptr_glMultiTexCoord4fARB #-}
ptr_glMultiTexCoord4fARB :: FunPtr (GLenum -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glMultiTexCoord4fARB = unsafePerformIO $ getCommand "glMultiTexCoord4fARB"

-- glMultiTexCoord4fv ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>.
glMultiTexCoord4fv
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> Ptr GLfloat -- ^ @v@ pointing to @4@ elements of type @CoordF@.
  -> m ()
glMultiTexCoord4fv v1 v2 = liftIO $ dyn94 ptr_glMultiTexCoord4fv v1 v2

{-# NOINLINE ptr_glMultiTexCoord4fv #-}
ptr_glMultiTexCoord4fv :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glMultiTexCoord4fv = unsafePerformIO $ getCommand "glMultiTexCoord4fv"

-- glMultiTexCoord4fvARB -------------------------------------------------------

-- | This command is an alias for 'glMultiTexCoord4fv'.
glMultiTexCoord4fvARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> Ptr GLfloat -- ^ @v@ pointing to @4@ elements of type @CoordF@.
  -> m ()
glMultiTexCoord4fvARB v1 v2 = liftIO $ dyn94 ptr_glMultiTexCoord4fvARB v1 v2

{-# NOINLINE ptr_glMultiTexCoord4fvARB #-}
ptr_glMultiTexCoord4fvARB :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glMultiTexCoord4fvARB = unsafePerformIO $ getCommand "glMultiTexCoord4fvARB"

-- glMultiTexCoord4hNV ---------------------------------------------------------

-- | The vector equivalent of this command is 'glMultiTexCoord4hvNV'.
glMultiTexCoord4hNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> GLhalfNV -- ^ @s@ of type @Half16NV@.
  -> GLhalfNV -- ^ @t@ of type @Half16NV@.
  -> GLhalfNV -- ^ @r@ of type @Half16NV@.
  -> GLhalfNV -- ^ @q@ of type @Half16NV@.
  -> m ()
glMultiTexCoord4hNV v1 v2 v3 v4 v5 = liftIO $ dyn558 ptr_glMultiTexCoord4hNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glMultiTexCoord4hNV #-}
ptr_glMultiTexCoord4hNV :: FunPtr (GLenum -> GLhalfNV -> GLhalfNV -> GLhalfNV -> GLhalfNV -> IO ())
ptr_glMultiTexCoord4hNV = unsafePerformIO $ getCommand "glMultiTexCoord4hNV"

-- glMultiTexCoord4hvNV --------------------------------------------------------

glMultiTexCoord4hvNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> Ptr GLhalfNV -- ^ @v@ pointing to @4@ elements of type @Half16NV@.
  -> m ()
glMultiTexCoord4hvNV v1 v2 = liftIO $ dyn543 ptr_glMultiTexCoord4hvNV v1 v2

{-# NOINLINE ptr_glMultiTexCoord4hvNV #-}
ptr_glMultiTexCoord4hvNV :: FunPtr (GLenum -> Ptr GLhalfNV -> IO ())
ptr_glMultiTexCoord4hvNV = unsafePerformIO $ getCommand "glMultiTexCoord4hvNV"

-- glMultiTexCoord4i -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glMultiTexCoord4iv'.
glMultiTexCoord4i
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> GLint -- ^ @s@ of type @CoordI@.
  -> GLint -- ^ @t@ of type @CoordI@.
  -> GLint -- ^ @r@ of type @CoordI@.
  -> GLint -- ^ @q@ of type @CoordI@.
  -> m ()
glMultiTexCoord4i v1 v2 v3 v4 v5 = liftIO $ dyn265 ptr_glMultiTexCoord4i v1 v2 v3 v4 v5

{-# NOINLINE ptr_glMultiTexCoord4i #-}
ptr_glMultiTexCoord4i :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> IO ())
ptr_glMultiTexCoord4i = unsafePerformIO $ getCommand "glMultiTexCoord4i"

-- glMultiTexCoord4iARB --------------------------------------------------------

-- | The vector equivalent of this command is 'glMultiTexCoord4iv'. This command is an alias for 'glMultiTexCoord4i'.
glMultiTexCoord4iARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> GLint -- ^ @s@ of type @CoordI@.
  -> GLint -- ^ @t@ of type @CoordI@.
  -> GLint -- ^ @r@ of type @CoordI@.
  -> GLint -- ^ @q@ of type @CoordI@.
  -> m ()
glMultiTexCoord4iARB v1 v2 v3 v4 v5 = liftIO $ dyn265 ptr_glMultiTexCoord4iARB v1 v2 v3 v4 v5

{-# NOINLINE ptr_glMultiTexCoord4iARB #-}
ptr_glMultiTexCoord4iARB :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> IO ())
ptr_glMultiTexCoord4iARB = unsafePerformIO $ getCommand "glMultiTexCoord4iARB"

-- glMultiTexCoord4iv ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>.
glMultiTexCoord4iv
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> Ptr GLint -- ^ @v@ pointing to @4@ elements of type @CoordI@.
  -> m ()
glMultiTexCoord4iv v1 v2 = liftIO $ dyn136 ptr_glMultiTexCoord4iv v1 v2

{-# NOINLINE ptr_glMultiTexCoord4iv #-}
ptr_glMultiTexCoord4iv :: FunPtr (GLenum -> Ptr GLint -> IO ())
ptr_glMultiTexCoord4iv = unsafePerformIO $ getCommand "glMultiTexCoord4iv"

-- glMultiTexCoord4ivARB -------------------------------------------------------

-- | This command is an alias for 'glMultiTexCoord4iv'.
glMultiTexCoord4ivARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> Ptr GLint -- ^ @v@ pointing to @4@ elements of type @CoordI@.
  -> m ()
glMultiTexCoord4ivARB v1 v2 = liftIO $ dyn136 ptr_glMultiTexCoord4ivARB v1 v2

{-# NOINLINE ptr_glMultiTexCoord4ivARB #-}
ptr_glMultiTexCoord4ivARB :: FunPtr (GLenum -> Ptr GLint -> IO ())
ptr_glMultiTexCoord4ivARB = unsafePerformIO $ getCommand "glMultiTexCoord4ivARB"

-- glMultiTexCoord4s -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glMultiTexCoord4sv'.
glMultiTexCoord4s
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> GLshort -- ^ @s@ of type @CoordS@.
  -> GLshort -- ^ @t@ of type @CoordS@.
  -> GLshort -- ^ @r@ of type @CoordS@.
  -> GLshort -- ^ @q@ of type @CoordS@.
  -> m ()
glMultiTexCoord4s v1 v2 v3 v4 v5 = liftIO $ dyn559 ptr_glMultiTexCoord4s v1 v2 v3 v4 v5

{-# NOINLINE ptr_glMultiTexCoord4s #-}
ptr_glMultiTexCoord4s :: FunPtr (GLenum -> GLshort -> GLshort -> GLshort -> GLshort -> IO ())
ptr_glMultiTexCoord4s = unsafePerformIO $ getCommand "glMultiTexCoord4s"

-- glMultiTexCoord4sARB --------------------------------------------------------

-- | The vector equivalent of this command is 'glMultiTexCoord4sv'. This command is an alias for 'glMultiTexCoord4s'.
glMultiTexCoord4sARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> GLshort -- ^ @s@ of type @CoordS@.
  -> GLshort -- ^ @t@ of type @CoordS@.
  -> GLshort -- ^ @r@ of type @CoordS@.
  -> GLshort -- ^ @q@ of type @CoordS@.
  -> m ()
glMultiTexCoord4sARB v1 v2 v3 v4 v5 = liftIO $ dyn559 ptr_glMultiTexCoord4sARB v1 v2 v3 v4 v5

{-# NOINLINE ptr_glMultiTexCoord4sARB #-}
ptr_glMultiTexCoord4sARB :: FunPtr (GLenum -> GLshort -> GLshort -> GLshort -> GLshort -> IO ())
ptr_glMultiTexCoord4sARB = unsafePerformIO $ getCommand "glMultiTexCoord4sARB"

-- glMultiTexCoord4sv ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>.
glMultiTexCoord4sv
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> Ptr GLshort -- ^ @v@ pointing to @4@ elements of type @CoordS@.
  -> m ()
glMultiTexCoord4sv v1 v2 = liftIO $ dyn545 ptr_glMultiTexCoord4sv v1 v2

{-# NOINLINE ptr_glMultiTexCoord4sv #-}
ptr_glMultiTexCoord4sv :: FunPtr (GLenum -> Ptr GLshort -> IO ())
ptr_glMultiTexCoord4sv = unsafePerformIO $ getCommand "glMultiTexCoord4sv"

-- glMultiTexCoord4svARB -------------------------------------------------------

-- | This command is an alias for 'glMultiTexCoord4sv'.
glMultiTexCoord4svARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> Ptr GLshort -- ^ @v@ pointing to @4@ elements of type @CoordS@.
  -> m ()
glMultiTexCoord4svARB v1 v2 = liftIO $ dyn545 ptr_glMultiTexCoord4svARB v1 v2

{-# NOINLINE ptr_glMultiTexCoord4svARB #-}
ptr_glMultiTexCoord4svARB :: FunPtr (GLenum -> Ptr GLshort -> IO ())
ptr_glMultiTexCoord4svARB = unsafePerformIO $ getCommand "glMultiTexCoord4svARB"

-- glMultiTexCoord4x -----------------------------------------------------------

glMultiTexCoord4x
  :: MonadIO m
  => GLenum -- ^ @texture@.
  -> GLfixed -- ^ @s@.
  -> GLfixed -- ^ @t@.
  -> GLfixed -- ^ @r@.
  -> GLfixed -- ^ @q@.
  -> m ()
glMultiTexCoord4x v1 v2 v3 v4 v5 = liftIO $ dyn560 ptr_glMultiTexCoord4x v1 v2 v3 v4 v5

{-# NOINLINE ptr_glMultiTexCoord4x #-}
ptr_glMultiTexCoord4x :: FunPtr (GLenum -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ())
ptr_glMultiTexCoord4x = unsafePerformIO $ getCommand "glMultiTexCoord4x"

-- glMultiTexCoord4xOES --------------------------------------------------------

glMultiTexCoord4xOES
  :: MonadIO m
  => GLenum -- ^ @texture@.
  -> GLfixed -- ^ @s@.
  -> GLfixed -- ^ @t@.
  -> GLfixed -- ^ @r@.
  -> GLfixed -- ^ @q@.
  -> m ()
glMultiTexCoord4xOES v1 v2 v3 v4 v5 = liftIO $ dyn560 ptr_glMultiTexCoord4xOES v1 v2 v3 v4 v5

{-# NOINLINE ptr_glMultiTexCoord4xOES #-}
ptr_glMultiTexCoord4xOES :: FunPtr (GLenum -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ())
ptr_glMultiTexCoord4xOES = unsafePerformIO $ getCommand "glMultiTexCoord4xOES"

-- glMultiTexCoord4xvOES -------------------------------------------------------

glMultiTexCoord4xvOES
  :: MonadIO m
  => GLenum -- ^ @texture@.
  -> Ptr GLfixed -- ^ @coords@ pointing to @4@ elements of type @GLfixed@.
  -> m ()
glMultiTexCoord4xvOES v1 v2 = liftIO $ dyn95 ptr_glMultiTexCoord4xvOES v1 v2

{-# NOINLINE ptr_glMultiTexCoord4xvOES #-}
ptr_glMultiTexCoord4xvOES :: FunPtr (GLenum -> Ptr GLfixed -> IO ())
ptr_glMultiTexCoord4xvOES = unsafePerformIO $ getCommand "glMultiTexCoord4xvOES"

-- glMultiTexCoordP1ui ---------------------------------------------------------

glMultiTexCoordP1ui
  :: MonadIO m
  => GLenum -- ^ @texture@.
  -> GLenum -- ^ @type@.
  -> GLuint -- ^ @coords@.
  -> m ()
glMultiTexCoordP1ui v1 v2 v3 = liftIO $ dyn29 ptr_glMultiTexCoordP1ui v1 v2 v3

{-# NOINLINE ptr_glMultiTexCoordP1ui #-}
ptr_glMultiTexCoordP1ui :: FunPtr (GLenum -> GLenum -> GLuint -> IO ())
ptr_glMultiTexCoordP1ui = unsafePerformIO $ getCommand "glMultiTexCoordP1ui"

-- glMultiTexCoordP1uiv --------------------------------------------------------

glMultiTexCoordP1uiv
  :: MonadIO m
  => GLenum -- ^ @texture@.
  -> GLenum -- ^ @type@.
  -> Ptr GLuint -- ^ @coords@ pointing to @1@ element of type @GLuint@.
  -> m ()
glMultiTexCoordP1uiv v1 v2 v3 = liftIO $ dyn413 ptr_glMultiTexCoordP1uiv v1 v2 v3

{-# NOINLINE ptr_glMultiTexCoordP1uiv #-}
ptr_glMultiTexCoordP1uiv :: FunPtr (GLenum -> GLenum -> Ptr GLuint -> IO ())
ptr_glMultiTexCoordP1uiv = unsafePerformIO $ getCommand "glMultiTexCoordP1uiv"

-- glMultiTexCoordP2ui ---------------------------------------------------------

glMultiTexCoordP2ui
  :: MonadIO m
  => GLenum -- ^ @texture@.
  -> GLenum -- ^ @type@.
  -> GLuint -- ^ @coords@.
  -> m ()
glMultiTexCoordP2ui v1 v2 v3 = liftIO $ dyn29 ptr_glMultiTexCoordP2ui v1 v2 v3

{-# NOINLINE ptr_glMultiTexCoordP2ui #-}
ptr_glMultiTexCoordP2ui :: FunPtr (GLenum -> GLenum -> GLuint -> IO ())
ptr_glMultiTexCoordP2ui = unsafePerformIO $ getCommand "glMultiTexCoordP2ui"

-- glMultiTexCoordP2uiv --------------------------------------------------------

glMultiTexCoordP2uiv
  :: MonadIO m
  => GLenum -- ^ @texture@.
  -> GLenum -- ^ @type@.
  -> Ptr GLuint -- ^ @coords@ pointing to @1@ element of type @GLuint@.
  -> m ()
glMultiTexCoordP2uiv v1 v2 v3 = liftIO $ dyn413 ptr_glMultiTexCoordP2uiv v1 v2 v3

{-# NOINLINE ptr_glMultiTexCoordP2uiv #-}
ptr_glMultiTexCoordP2uiv :: FunPtr (GLenum -> GLenum -> Ptr GLuint -> IO ())
ptr_glMultiTexCoordP2uiv = unsafePerformIO $ getCommand "glMultiTexCoordP2uiv"

-- glMultiTexCoordP3ui ---------------------------------------------------------

glMultiTexCoordP3ui
  :: MonadIO m
  => GLenum -- ^ @texture@.
  -> GLenum -- ^ @type@.
  -> GLuint -- ^ @coords@.
  -> m ()
glMultiTexCoordP3ui v1 v2 v3 = liftIO $ dyn29 ptr_glMultiTexCoordP3ui v1 v2 v3

{-# NOINLINE ptr_glMultiTexCoordP3ui #-}
ptr_glMultiTexCoordP3ui :: FunPtr (GLenum -> GLenum -> GLuint -> IO ())
ptr_glMultiTexCoordP3ui = unsafePerformIO $ getCommand "glMultiTexCoordP3ui"

-- glMultiTexCoordP3uiv --------------------------------------------------------

glMultiTexCoordP3uiv
  :: MonadIO m
  => GLenum -- ^ @texture@.
  -> GLenum -- ^ @type@.
  -> Ptr GLuint -- ^ @coords@ pointing to @1@ element of type @GLuint@.
  -> m ()
glMultiTexCoordP3uiv v1 v2 v3 = liftIO $ dyn413 ptr_glMultiTexCoordP3uiv v1 v2 v3

{-# NOINLINE ptr_glMultiTexCoordP3uiv #-}
ptr_glMultiTexCoordP3uiv :: FunPtr (GLenum -> GLenum -> Ptr GLuint -> IO ())
ptr_glMultiTexCoordP3uiv = unsafePerformIO $ getCommand "glMultiTexCoordP3uiv"

-- glMultiTexCoordP4ui ---------------------------------------------------------

glMultiTexCoordP4ui
  :: MonadIO m
  => GLenum -- ^ @texture@.
  -> GLenum -- ^ @type@.
  -> GLuint -- ^ @coords@.
  -> m ()
glMultiTexCoordP4ui v1 v2 v3 = liftIO $ dyn29 ptr_glMultiTexCoordP4ui v1 v2 v3

{-# NOINLINE ptr_glMultiTexCoordP4ui #-}
ptr_glMultiTexCoordP4ui :: FunPtr (GLenum -> GLenum -> GLuint -> IO ())
ptr_glMultiTexCoordP4ui = unsafePerformIO $ getCommand "glMultiTexCoordP4ui"

-- glMultiTexCoordP4uiv --------------------------------------------------------

glMultiTexCoordP4uiv
  :: MonadIO m
  => GLenum -- ^ @texture@.
  -> GLenum -- ^ @type@.
  -> Ptr GLuint -- ^ @coords@ pointing to @1@ element of type @GLuint@.
  -> m ()
glMultiTexCoordP4uiv v1 v2 v3 = liftIO $ dyn413 ptr_glMultiTexCoordP4uiv v1 v2 v3

{-# NOINLINE ptr_glMultiTexCoordP4uiv #-}
ptr_glMultiTexCoordP4uiv :: FunPtr (GLenum -> GLenum -> Ptr GLuint -> IO ())
ptr_glMultiTexCoordP4uiv = unsafePerformIO $ getCommand "glMultiTexCoordP4uiv"

-- glMultiTexCoordPointerEXT ---------------------------------------------------

glMultiTexCoordPointerEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type @TextureUnit@.
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [TexCoordPointerType](Graphics-GL-Groups.html#TexCoordPointerType).
  -> GLsizei -- ^ @stride@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(size,type,stride)@ elements of type @a@.
  -> m ()
glMultiTexCoordPointerEXT v1 v2 v3 v4 v5 = liftIO $ dyn561 ptr_glMultiTexCoordPointerEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glMultiTexCoordPointerEXT #-}
ptr_glMultiTexCoordPointerEXT :: FunPtr (GLenum -> GLint -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glMultiTexCoordPointerEXT = unsafePerformIO $ getCommand "glMultiTexCoordPointerEXT"

-- glMultiTexEnvfEXT -----------------------------------------------------------

-- | The vector equivalent of this command is 'glMultiTexEnvfvEXT'.
glMultiTexEnvfEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type @TextureUnit@.
  -> GLenum -- ^ @target@ of type [TextureEnvTarget](Graphics-GL-Groups.html#TextureEnvTarget).
  -> GLenum -- ^ @pname@ of type [TextureEnvParameter](Graphics-GL-Groups.html#TextureEnvParameter).
  -> GLfloat -- ^ @param@ of type @CheckedFloat32@.
  -> m ()
glMultiTexEnvfEXT v1 v2 v3 v4 = liftIO $ dyn562 ptr_glMultiTexEnvfEXT v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexEnvfEXT #-}
ptr_glMultiTexEnvfEXT :: FunPtr (GLenum -> GLenum -> GLenum -> GLfloat -> IO ())
ptr_glMultiTexEnvfEXT = unsafePerformIO $ getCommand "glMultiTexEnvfEXT"

-- glMultiTexEnvfvEXT ----------------------------------------------------------

glMultiTexEnvfvEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type @TextureUnit@.
  -> GLenum -- ^ @target@ of type [TextureEnvTarget](Graphics-GL-Groups.html#TextureEnvTarget).
  -> GLenum -- ^ @pname@ of type [TextureEnvParameter](Graphics-GL-Groups.html#TextureEnvParameter).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedFloat32@.
  -> m ()
glMultiTexEnvfvEXT v1 v2 v3 v4 = liftIO $ dyn320 ptr_glMultiTexEnvfvEXT v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexEnvfvEXT #-}
ptr_glMultiTexEnvfvEXT :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glMultiTexEnvfvEXT = unsafePerformIO $ getCommand "glMultiTexEnvfvEXT"

-- glMultiTexEnviEXT -----------------------------------------------------------

-- | The vector equivalent of this command is 'glMultiTexEnvivEXT'.
glMultiTexEnviEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type @TextureUnit@.
  -> GLenum -- ^ @target@ of type [TextureEnvTarget](Graphics-GL-Groups.html#TextureEnvTarget).
  -> GLenum -- ^ @pname@ of type [TextureEnvParameter](Graphics-GL-Groups.html#TextureEnvParameter).
  -> GLint -- ^ @param@ of type @CheckedInt32@.
  -> m ()
glMultiTexEnviEXT v1 v2 v3 v4 = liftIO $ dyn563 ptr_glMultiTexEnviEXT v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexEnviEXT #-}
ptr_glMultiTexEnviEXT :: FunPtr (GLenum -> GLenum -> GLenum -> GLint -> IO ())
ptr_glMultiTexEnviEXT = unsafePerformIO $ getCommand "glMultiTexEnviEXT"

-- glMultiTexEnvivEXT ----------------------------------------------------------

glMultiTexEnvivEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type @TextureUnit@.
  -> GLenum -- ^ @target@ of type [TextureEnvTarget](Graphics-GL-Groups.html#TextureEnvTarget).
  -> GLenum -- ^ @pname@ of type [TextureEnvParameter](Graphics-GL-Groups.html#TextureEnvParameter).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedInt32@.
  -> m ()
glMultiTexEnvivEXT v1 v2 v3 v4 = liftIO $ dyn321 ptr_glMultiTexEnvivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexEnvivEXT #-}
ptr_glMultiTexEnvivEXT :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glMultiTexEnvivEXT = unsafePerformIO $ getCommand "glMultiTexEnvivEXT"

-- glMultiTexGendEXT -----------------------------------------------------------

-- | The vector equivalent of this command is 'glMultiTexGendvEXT'.
glMultiTexGendEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type @TextureUnit@.
  -> GLenum -- ^ @coord@ of type [TextureCoordName](Graphics-GL-Groups.html#TextureCoordName).
  -> GLenum -- ^ @pname@ of type [TextureGenParameter](Graphics-GL-Groups.html#TextureGenParameter).
  -> GLdouble -- ^ @param@.
  -> m ()
glMultiTexGendEXT v1 v2 v3 v4 = liftIO $ dyn564 ptr_glMultiTexGendEXT v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexGendEXT #-}
ptr_glMultiTexGendEXT :: FunPtr (GLenum -> GLenum -> GLenum -> GLdouble -> IO ())
ptr_glMultiTexGendEXT = unsafePerformIO $ getCommand "glMultiTexGendEXT"

-- glMultiTexGendvEXT ----------------------------------------------------------

glMultiTexGendvEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type @TextureUnit@.
  -> GLenum -- ^ @coord@ of type [TextureCoordName](Graphics-GL-Groups.html#TextureCoordName).
  -> GLenum -- ^ @pname@ of type [TextureGenParameter](Graphics-GL-Groups.html#TextureGenParameter).
  -> Ptr GLdouble -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLdouble@.
  -> m ()
glMultiTexGendvEXT v1 v2 v3 v4 = liftIO $ dyn354 ptr_glMultiTexGendvEXT v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexGendvEXT #-}
ptr_glMultiTexGendvEXT :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLdouble -> IO ())
ptr_glMultiTexGendvEXT = unsafePerformIO $ getCommand "glMultiTexGendvEXT"

-- glMultiTexGenfEXT -----------------------------------------------------------

-- | The vector equivalent of this command is 'glMultiTexGenfvEXT'.
glMultiTexGenfEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type @TextureUnit@.
  -> GLenum -- ^ @coord@ of type [TextureCoordName](Graphics-GL-Groups.html#TextureCoordName).
  -> GLenum -- ^ @pname@ of type [TextureGenParameter](Graphics-GL-Groups.html#TextureGenParameter).
  -> GLfloat -- ^ @param@ of type @CheckedFloat32@.
  -> m ()
glMultiTexGenfEXT v1 v2 v3 v4 = liftIO $ dyn562 ptr_glMultiTexGenfEXT v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexGenfEXT #-}
ptr_glMultiTexGenfEXT :: FunPtr (GLenum -> GLenum -> GLenum -> GLfloat -> IO ())
ptr_glMultiTexGenfEXT = unsafePerformIO $ getCommand "glMultiTexGenfEXT"

-- glMultiTexGenfvEXT ----------------------------------------------------------

glMultiTexGenfvEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type @TextureUnit@.
  -> GLenum -- ^ @coord@ of type [TextureCoordName](Graphics-GL-Groups.html#TextureCoordName).
  -> GLenum -- ^ @pname@ of type [TextureGenParameter](Graphics-GL-Groups.html#TextureGenParameter).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedFloat32@.
  -> m ()
glMultiTexGenfvEXT v1 v2 v3 v4 = liftIO $ dyn320 ptr_glMultiTexGenfvEXT v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexGenfvEXT #-}
ptr_glMultiTexGenfvEXT :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glMultiTexGenfvEXT = unsafePerformIO $ getCommand "glMultiTexGenfvEXT"

-- glMultiTexGeniEXT -----------------------------------------------------------

-- | The vector equivalent of this command is 'glMultiTexGenivEXT'.
glMultiTexGeniEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type @TextureUnit@.
  -> GLenum -- ^ @coord@ of type [TextureCoordName](Graphics-GL-Groups.html#TextureCoordName).
  -> GLenum -- ^ @pname@ of type [TextureGenParameter](Graphics-GL-Groups.html#TextureGenParameter).
  -> GLint -- ^ @param@ of type @CheckedInt32@.
  -> m ()
glMultiTexGeniEXT v1 v2 v3 v4 = liftIO $ dyn563 ptr_glMultiTexGeniEXT v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexGeniEXT #-}
ptr_glMultiTexGeniEXT :: FunPtr (GLenum -> GLenum -> GLenum -> GLint -> IO ())
ptr_glMultiTexGeniEXT = unsafePerformIO $ getCommand "glMultiTexGeniEXT"

-- glMultiTexGenivEXT ----------------------------------------------------------

glMultiTexGenivEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type @TextureUnit@.
  -> GLenum -- ^ @coord@ of type [TextureCoordName](Graphics-GL-Groups.html#TextureCoordName).
  -> GLenum -- ^ @pname@ of type [TextureGenParameter](Graphics-GL-Groups.html#TextureGenParameter).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedInt32@.
  -> m ()
glMultiTexGenivEXT v1 v2 v3 v4 = liftIO $ dyn321 ptr_glMultiTexGenivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexGenivEXT #-}
ptr_glMultiTexGenivEXT :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glMultiTexGenivEXT = unsafePerformIO $ getCommand "glMultiTexGenivEXT"

-- glMultiTexImage1DEXT --------------------------------------------------------

glMultiTexImage1DEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type @TextureUnit@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @internalformat@ of type @TextureComponentCount@.
  -> GLsizei -- ^ @width@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @pixels@ pointing to @COMPSIZE(format,type,width)@ elements of type @a@.
  -> m ()
glMultiTexImage1DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn565 ptr_glMultiTexImage1DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glMultiTexImage1DEXT #-}
ptr_glMultiTexImage1DEXT :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glMultiTexImage1DEXT = unsafePerformIO $ getCommand "glMultiTexImage1DEXT"

-- glMultiTexImage2DEXT --------------------------------------------------------

glMultiTexImage2DEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type @TextureUnit@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @internalformat@ of type @TextureComponentCount@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @pixels@ pointing to @COMPSIZE(format,type,width,height)@ elements of type @a@.
  -> m ()
glMultiTexImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO $ dyn566 ptr_glMultiTexImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10

{-# NOINLINE ptr_glMultiTexImage2DEXT #-}
ptr_glMultiTexImage2DEXT :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glMultiTexImage2DEXT = unsafePerformIO $ getCommand "glMultiTexImage2DEXT"

-- glMultiTexImage3DEXT --------------------------------------------------------

glMultiTexImage3DEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type @TextureUnit@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @internalformat@ of type @TextureComponentCount@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @pixels@ pointing to @COMPSIZE(format,type,width,height,depth)@ elements of type @a@.
  -> m ()
glMultiTexImage3DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 = liftIO $ dyn567 ptr_glMultiTexImage3DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11

{-# NOINLINE ptr_glMultiTexImage3DEXT #-}
ptr_glMultiTexImage3DEXT :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glMultiTexImage3DEXT = unsafePerformIO $ getCommand "glMultiTexImage3DEXT"

