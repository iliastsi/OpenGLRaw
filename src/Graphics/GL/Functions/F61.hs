--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F61
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

module Graphics.GL.Functions.F61 (
  glTexCoordP1ui,
  glTexCoordP1uiv,
  glTexCoordP2ui,
  glTexCoordP2uiv,
  glTexCoordP3ui,
  glTexCoordP3uiv,
  glTexCoordP4ui,
  glTexCoordP4uiv,
  glTexCoordPointer,
  glTexCoordPointerEXT,
  glTexCoordPointerListIBM,
  glTexCoordPointervINTEL,
  glTexEnvf,
  glTexEnvfv,
  glTexEnvi,
  glTexEnviv,
  glTexEnvx,
  glTexEnvxOES,
  glTexEnvxv,
  glTexEnvxvOES,
  glTexFilterFuncSGIS,
  glTexGend,
  glTexGendv,
  glTexGenf,
  glTexGenfOES,
  glTexGenfv,
  glTexGenfvOES,
  glTexGeni,
  glTexGeniOES,
  glTexGeniv,
  glTexGenivOES,
  glTexGenxOES,
  glTexGenxvOES,
  glTexImage1D,
  glTexImage2D,
  glTexImage2DMultisample,
  glTexImage2DMultisampleCoverageNV,
  glTexImage3D,
  glTexImage3DEXT,
  glTexImage3DMultisample
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

-- glTexCoordP1ui --------------------------------------------------------------

glTexCoordP1ui
  :: MonadIO m
  => GLenum -- ^ @type@.
  -> GLuint -- ^ @coords@.
  -> m ()
glTexCoordP1ui v1 v2 = liftIO $ dyn16 ptr_glTexCoordP1ui v1 v2

{-# NOINLINE ptr_glTexCoordP1ui #-}
ptr_glTexCoordP1ui :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glTexCoordP1ui = unsafePerformIO $ getCommand "glTexCoordP1ui"

-- glTexCoordP1uiv -------------------------------------------------------------

glTexCoordP1uiv
  :: MonadIO m
  => GLenum -- ^ @type@.
  -> Ptr GLuint -- ^ @coords@ pointing to @1@ element of type @GLuint@.
  -> m ()
glTexCoordP1uiv v1 v2 = liftIO $ dyn125 ptr_glTexCoordP1uiv v1 v2

{-# NOINLINE ptr_glTexCoordP1uiv #-}
ptr_glTexCoordP1uiv :: FunPtr (GLenum -> Ptr GLuint -> IO ())
ptr_glTexCoordP1uiv = unsafePerformIO $ getCommand "glTexCoordP1uiv"

-- glTexCoordP2ui --------------------------------------------------------------

glTexCoordP2ui
  :: MonadIO m
  => GLenum -- ^ @type@.
  -> GLuint -- ^ @coords@.
  -> m ()
glTexCoordP2ui v1 v2 = liftIO $ dyn16 ptr_glTexCoordP2ui v1 v2

{-# NOINLINE ptr_glTexCoordP2ui #-}
ptr_glTexCoordP2ui :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glTexCoordP2ui = unsafePerformIO $ getCommand "glTexCoordP2ui"

-- glTexCoordP2uiv -------------------------------------------------------------

glTexCoordP2uiv
  :: MonadIO m
  => GLenum -- ^ @type@.
  -> Ptr GLuint -- ^ @coords@ pointing to @1@ element of type @GLuint@.
  -> m ()
glTexCoordP2uiv v1 v2 = liftIO $ dyn125 ptr_glTexCoordP2uiv v1 v2

{-# NOINLINE ptr_glTexCoordP2uiv #-}
ptr_glTexCoordP2uiv :: FunPtr (GLenum -> Ptr GLuint -> IO ())
ptr_glTexCoordP2uiv = unsafePerformIO $ getCommand "glTexCoordP2uiv"

-- glTexCoordP3ui --------------------------------------------------------------

glTexCoordP3ui
  :: MonadIO m
  => GLenum -- ^ @type@.
  -> GLuint -- ^ @coords@.
  -> m ()
glTexCoordP3ui v1 v2 = liftIO $ dyn16 ptr_glTexCoordP3ui v1 v2

{-# NOINLINE ptr_glTexCoordP3ui #-}
ptr_glTexCoordP3ui :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glTexCoordP3ui = unsafePerformIO $ getCommand "glTexCoordP3ui"

-- glTexCoordP3uiv -------------------------------------------------------------

glTexCoordP3uiv
  :: MonadIO m
  => GLenum -- ^ @type@.
  -> Ptr GLuint -- ^ @coords@ pointing to @1@ element of type @GLuint@.
  -> m ()
glTexCoordP3uiv v1 v2 = liftIO $ dyn125 ptr_glTexCoordP3uiv v1 v2

{-# NOINLINE ptr_glTexCoordP3uiv #-}
ptr_glTexCoordP3uiv :: FunPtr (GLenum -> Ptr GLuint -> IO ())
ptr_glTexCoordP3uiv = unsafePerformIO $ getCommand "glTexCoordP3uiv"

-- glTexCoordP4ui --------------------------------------------------------------

glTexCoordP4ui
  :: MonadIO m
  => GLenum -- ^ @type@.
  -> GLuint -- ^ @coords@.
  -> m ()
glTexCoordP4ui v1 v2 = liftIO $ dyn16 ptr_glTexCoordP4ui v1 v2

{-# NOINLINE ptr_glTexCoordP4ui #-}
ptr_glTexCoordP4ui :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glTexCoordP4ui = unsafePerformIO $ getCommand "glTexCoordP4ui"

-- glTexCoordP4uiv -------------------------------------------------------------

glTexCoordP4uiv
  :: MonadIO m
  => GLenum -- ^ @type@.
  -> Ptr GLuint -- ^ @coords@ pointing to @1@ element of type @GLuint@.
  -> m ()
glTexCoordP4uiv v1 v2 = liftIO $ dyn125 ptr_glTexCoordP4uiv v1 v2

{-# NOINLINE ptr_glTexCoordP4uiv #-}
ptr_glTexCoordP4uiv :: FunPtr (GLenum -> Ptr GLuint -> IO ())
ptr_glTexCoordP4uiv = unsafePerformIO $ getCommand "glTexCoordP4uiv"

-- glTexCoordPointer -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexCoordPointer.xml OpenGL 2.x>.
glTexCoordPointer
  :: MonadIO m
  => GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [TexCoordPointerType](Graphics-GL-Groups.html#TexCoordPointerType).
  -> GLsizei -- ^ @stride@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(size,type,stride)@ elements of type @a@.
  -> m ()
glTexCoordPointer v1 v2 v3 v4 = liftIO $ dyn126 ptr_glTexCoordPointer v1 v2 v3 v4

{-# NOINLINE ptr_glTexCoordPointer #-}
ptr_glTexCoordPointer :: FunPtr (GLint -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glTexCoordPointer = unsafePerformIO $ getCommand "glTexCoordPointer"

-- glTexCoordPointerEXT --------------------------------------------------------

glTexCoordPointerEXT
  :: MonadIO m
  => GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [TexCoordPointerType](Graphics-GL-Groups.html#TexCoordPointerType).
  -> GLsizei -- ^ @stride@.
  -> GLsizei -- ^ @count@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(size,type,stride,count)@ elements of type @a@.
  -> m ()
glTexCoordPointerEXT v1 v2 v3 v4 v5 = liftIO $ dyn127 ptr_glTexCoordPointerEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glTexCoordPointerEXT #-}
ptr_glTexCoordPointerEXT :: FunPtr (GLint -> GLenum -> GLsizei -> GLsizei -> Ptr a -> IO ())
ptr_glTexCoordPointerEXT = unsafePerformIO $ getCommand "glTexCoordPointerEXT"

-- glTexCoordPointerListIBM ----------------------------------------------------

glTexCoordPointerListIBM
  :: MonadIO m
  => GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [TexCoordPointerType](Graphics-GL-Groups.html#TexCoordPointerType).
  -> GLint -- ^ @stride@.
  -> Ptr (Ptr a) -- ^ @pointer@ pointing to @COMPSIZE(size,type,stride)@ elements of type @Ptr a@.
  -> GLint -- ^ @ptrstride@.
  -> m ()
glTexCoordPointerListIBM v1 v2 v3 v4 v5 = liftIO $ dyn128 ptr_glTexCoordPointerListIBM v1 v2 v3 v4 v5

{-# NOINLINE ptr_glTexCoordPointerListIBM #-}
ptr_glTexCoordPointerListIBM :: FunPtr (GLint -> GLenum -> GLint -> Ptr (Ptr a) -> GLint -> IO ())
ptr_glTexCoordPointerListIBM = unsafePerformIO $ getCommand "glTexCoordPointerListIBM"

-- glTexCoordPointervINTEL -----------------------------------------------------

glTexCoordPointervINTEL
  :: MonadIO m
  => GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [VertexPointerType](Graphics-GL-Groups.html#VertexPointerType).
  -> Ptr (Ptr a) -- ^ @pointer@ pointing to @4@ elements of type @Ptr a@.
  -> m ()
glTexCoordPointervINTEL v1 v2 v3 = liftIO $ dyn129 ptr_glTexCoordPointervINTEL v1 v2 v3

{-# NOINLINE ptr_glTexCoordPointervINTEL #-}
ptr_glTexCoordPointervINTEL :: FunPtr (GLint -> GLenum -> Ptr (Ptr a) -> IO ())
ptr_glTexCoordPointervINTEL = unsafePerformIO $ getCommand "glTexCoordPointervINTEL"

-- glTexEnvf -------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexEnv.xml OpenGL 2.x>.
glTexEnvf
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureEnvTarget](Graphics-GL-Groups.html#TextureEnvTarget).
  -> GLenum -- ^ @pname@ of type [TextureEnvParameter](Graphics-GL-Groups.html#TextureEnvParameter).
  -> GLfloat -- ^ @param@ of type @CheckedFloat32@.
  -> m ()
glTexEnvf v1 v2 v3 = liftIO $ dyn161 ptr_glTexEnvf v1 v2 v3

{-# NOINLINE ptr_glTexEnvf #-}
ptr_glTexEnvf :: FunPtr (GLenum -> GLenum -> GLfloat -> IO ())
ptr_glTexEnvf = unsafePerformIO $ getCommand "glTexEnvf"

-- glTexEnvfv ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexEnv.xml OpenGL 2.x>.
glTexEnvfv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureEnvTarget](Graphics-GL-Groups.html#TextureEnvTarget).
  -> GLenum -- ^ @pname@ of type [TextureEnvParameter](Graphics-GL-Groups.html#TextureEnvParameter).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedFloat32@.
  -> m ()
glTexEnvfv v1 v2 v3 = liftIO $ dyn132 ptr_glTexEnvfv v1 v2 v3

{-# NOINLINE ptr_glTexEnvfv #-}
ptr_glTexEnvfv :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glTexEnvfv = unsafePerformIO $ getCommand "glTexEnvfv"

-- glTexEnvi -------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexEnv.xml OpenGL 2.x>.
glTexEnvi
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureEnvTarget](Graphics-GL-Groups.html#TextureEnvTarget).
  -> GLenum -- ^ @pname@ of type [TextureEnvParameter](Graphics-GL-Groups.html#TextureEnvParameter).
  -> GLint -- ^ @param@ of type @CheckedInt32@.
  -> m ()
glTexEnvi v1 v2 v3 = liftIO $ dyn62 ptr_glTexEnvi v1 v2 v3

{-# NOINLINE ptr_glTexEnvi #-}
ptr_glTexEnvi :: FunPtr (GLenum -> GLenum -> GLint -> IO ())
ptr_glTexEnvi = unsafePerformIO $ getCommand "glTexEnvi"

-- glTexEnviv ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexEnv.xml OpenGL 2.x>.
glTexEnviv
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureEnvTarget](Graphics-GL-Groups.html#TextureEnvTarget).
  -> GLenum -- ^ @pname@ of type [TextureEnvParameter](Graphics-GL-Groups.html#TextureEnvParameter).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedInt32@.
  -> m ()
glTexEnviv v1 v2 v3 = liftIO $ dyn133 ptr_glTexEnviv v1 v2 v3

{-# NOINLINE ptr_glTexEnviv #-}
ptr_glTexEnviv :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glTexEnviv = unsafePerformIO $ getCommand "glTexEnviv"

-- glTexEnvx -------------------------------------------------------------------

glTexEnvx
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @pname@.
  -> GLfixed -- ^ @param@.
  -> m ()
glTexEnvx v1 v2 v3 = liftIO $ dyn162 ptr_glTexEnvx v1 v2 v3

{-# NOINLINE ptr_glTexEnvx #-}
ptr_glTexEnvx :: FunPtr (GLenum -> GLenum -> GLfixed -> IO ())
ptr_glTexEnvx = unsafePerformIO $ getCommand "glTexEnvx"

-- glTexEnvxOES ----------------------------------------------------------------

glTexEnvxOES
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @pname@.
  -> GLfixed -- ^ @param@.
  -> m ()
glTexEnvxOES v1 v2 v3 = liftIO $ dyn162 ptr_glTexEnvxOES v1 v2 v3

{-# NOINLINE ptr_glTexEnvxOES #-}
ptr_glTexEnvxOES :: FunPtr (GLenum -> GLenum -> GLfixed -> IO ())
ptr_glTexEnvxOES = unsafePerformIO $ getCommand "glTexEnvxOES"

-- glTexEnvxv ------------------------------------------------------------------

glTexEnvxv
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLfixed -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfixed@.
  -> m ()
glTexEnvxv v1 v2 v3 = liftIO $ dyn163 ptr_glTexEnvxv v1 v2 v3

{-# NOINLINE ptr_glTexEnvxv #-}
ptr_glTexEnvxv :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
ptr_glTexEnvxv = unsafePerformIO $ getCommand "glTexEnvxv"

-- glTexEnvxvOES ---------------------------------------------------------------

glTexEnvxvOES
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLfixed -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfixed@.
  -> m ()
glTexEnvxvOES v1 v2 v3 = liftIO $ dyn163 ptr_glTexEnvxvOES v1 v2 v3

{-# NOINLINE ptr_glTexEnvxvOES #-}
ptr_glTexEnvxvOES :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
ptr_glTexEnvxvOES = unsafePerformIO $ getCommand "glTexEnvxvOES"

-- glTexFilterFuncSGIS ---------------------------------------------------------

glTexFilterFuncSGIS
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @filter@ of type @TextureFilterSGIS@.
  -> GLsizei -- ^ @n@.
  -> Ptr GLfloat -- ^ @weights@ pointing to @n@ elements of type @GLfloat@.
  -> m ()
glTexFilterFuncSGIS v1 v2 v3 v4 = liftIO $ dyn450 ptr_glTexFilterFuncSGIS v1 v2 v3 v4

{-# NOINLINE ptr_glTexFilterFuncSGIS #-}
ptr_glTexFilterFuncSGIS :: FunPtr (GLenum -> GLenum -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glTexFilterFuncSGIS = unsafePerformIO $ getCommand "glTexFilterFuncSGIS"

-- glTexGend -------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexGen.xml OpenGL 2.x>.
glTexGend
  :: MonadIO m
  => GLenum -- ^ @coord@ of type [TextureCoordName](Graphics-GL-Groups.html#TextureCoordName).
  -> GLenum -- ^ @pname@ of type [TextureGenParameter](Graphics-GL-Groups.html#TextureGenParameter).
  -> GLdouble -- ^ @param@.
  -> m ()
glTexGend v1 v2 v3 = liftIO $ dyn729 ptr_glTexGend v1 v2 v3

{-# NOINLINE ptr_glTexGend #-}
ptr_glTexGend :: FunPtr (GLenum -> GLenum -> GLdouble -> IO ())
ptr_glTexGend = unsafePerformIO $ getCommand "glTexGend"

-- glTexGendv ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexGen.xml OpenGL 2.x>.
glTexGendv
  :: MonadIO m
  => GLenum -- ^ @coord@ of type [TextureCoordName](Graphics-GL-Groups.html#TextureCoordName).
  -> GLenum -- ^ @pname@ of type [TextureGenParameter](Graphics-GL-Groups.html#TextureGenParameter).
  -> Ptr GLdouble -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLdouble@.
  -> m ()
glTexGendv v1 v2 v3 = liftIO $ dyn353 ptr_glTexGendv v1 v2 v3

{-# NOINLINE ptr_glTexGendv #-}
ptr_glTexGendv :: FunPtr (GLenum -> GLenum -> Ptr GLdouble -> IO ())
ptr_glTexGendv = unsafePerformIO $ getCommand "glTexGendv"

-- glTexGenf -------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexGen.xml OpenGL 2.x>.
glTexGenf
  :: MonadIO m
  => GLenum -- ^ @coord@ of type [TextureCoordName](Graphics-GL-Groups.html#TextureCoordName).
  -> GLenum -- ^ @pname@ of type [TextureGenParameter](Graphics-GL-Groups.html#TextureGenParameter).
  -> GLfloat -- ^ @param@ of type @CheckedFloat32@.
  -> m ()
glTexGenf v1 v2 v3 = liftIO $ dyn161 ptr_glTexGenf v1 v2 v3

{-# NOINLINE ptr_glTexGenf #-}
ptr_glTexGenf :: FunPtr (GLenum -> GLenum -> GLfloat -> IO ())
ptr_glTexGenf = unsafePerformIO $ getCommand "glTexGenf"

-- glTexGenfOES ----------------------------------------------------------------

glTexGenfOES
  :: MonadIO m
  => GLenum -- ^ @coord@.
  -> GLenum -- ^ @pname@.
  -> GLfloat -- ^ @param@.
  -> m ()
glTexGenfOES v1 v2 v3 = liftIO $ dyn161 ptr_glTexGenfOES v1 v2 v3

{-# NOINLINE ptr_glTexGenfOES #-}
ptr_glTexGenfOES :: FunPtr (GLenum -> GLenum -> GLfloat -> IO ())
ptr_glTexGenfOES = unsafePerformIO $ getCommand "glTexGenfOES"

-- glTexGenfv ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexGen.xml OpenGL 2.x>.
glTexGenfv
  :: MonadIO m
  => GLenum -- ^ @coord@ of type [TextureCoordName](Graphics-GL-Groups.html#TextureCoordName).
  -> GLenum -- ^ @pname@ of type [TextureGenParameter](Graphics-GL-Groups.html#TextureGenParameter).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedFloat32@.
  -> m ()
glTexGenfv v1 v2 v3 = liftIO $ dyn132 ptr_glTexGenfv v1 v2 v3

{-# NOINLINE ptr_glTexGenfv #-}
ptr_glTexGenfv :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glTexGenfv = unsafePerformIO $ getCommand "glTexGenfv"

-- glTexGenfvOES ---------------------------------------------------------------

glTexGenfvOES
  :: MonadIO m
  => GLenum -- ^ @coord@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glTexGenfvOES v1 v2 v3 = liftIO $ dyn132 ptr_glTexGenfvOES v1 v2 v3

{-# NOINLINE ptr_glTexGenfvOES #-}
ptr_glTexGenfvOES :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glTexGenfvOES = unsafePerformIO $ getCommand "glTexGenfvOES"

-- glTexGeni -------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexGen.xml OpenGL 2.x>.
glTexGeni
  :: MonadIO m
  => GLenum -- ^ @coord@ of type [TextureCoordName](Graphics-GL-Groups.html#TextureCoordName).
  -> GLenum -- ^ @pname@ of type [TextureGenParameter](Graphics-GL-Groups.html#TextureGenParameter).
  -> GLint -- ^ @param@ of type @CheckedInt32@.
  -> m ()
glTexGeni v1 v2 v3 = liftIO $ dyn62 ptr_glTexGeni v1 v2 v3

{-# NOINLINE ptr_glTexGeni #-}
ptr_glTexGeni :: FunPtr (GLenum -> GLenum -> GLint -> IO ())
ptr_glTexGeni = unsafePerformIO $ getCommand "glTexGeni"

-- glTexGeniOES ----------------------------------------------------------------

glTexGeniOES
  :: MonadIO m
  => GLenum -- ^ @coord@.
  -> GLenum -- ^ @pname@.
  -> GLint -- ^ @param@.
  -> m ()
glTexGeniOES v1 v2 v3 = liftIO $ dyn62 ptr_glTexGeniOES v1 v2 v3

{-# NOINLINE ptr_glTexGeniOES #-}
ptr_glTexGeniOES :: FunPtr (GLenum -> GLenum -> GLint -> IO ())
ptr_glTexGeniOES = unsafePerformIO $ getCommand "glTexGeniOES"

-- glTexGeniv ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexGen.xml OpenGL 2.x>.
glTexGeniv
  :: MonadIO m
  => GLenum -- ^ @coord@ of type [TextureCoordName](Graphics-GL-Groups.html#TextureCoordName).
  -> GLenum -- ^ @pname@ of type [TextureGenParameter](Graphics-GL-Groups.html#TextureGenParameter).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedInt32@.
  -> m ()
glTexGeniv v1 v2 v3 = liftIO $ dyn133 ptr_glTexGeniv v1 v2 v3

{-# NOINLINE ptr_glTexGeniv #-}
ptr_glTexGeniv :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glTexGeniv = unsafePerformIO $ getCommand "glTexGeniv"

-- glTexGenivOES ---------------------------------------------------------------

glTexGenivOES
  :: MonadIO m
  => GLenum -- ^ @coord@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glTexGenivOES v1 v2 v3 = liftIO $ dyn133 ptr_glTexGenivOES v1 v2 v3

{-# NOINLINE ptr_glTexGenivOES #-}
ptr_glTexGenivOES :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glTexGenivOES = unsafePerformIO $ getCommand "glTexGenivOES"

-- glTexGenxOES ----------------------------------------------------------------

glTexGenxOES
  :: MonadIO m
  => GLenum -- ^ @coord@.
  -> GLenum -- ^ @pname@.
  -> GLfixed -- ^ @param@.
  -> m ()
glTexGenxOES v1 v2 v3 = liftIO $ dyn162 ptr_glTexGenxOES v1 v2 v3

{-# NOINLINE ptr_glTexGenxOES #-}
ptr_glTexGenxOES :: FunPtr (GLenum -> GLenum -> GLfixed -> IO ())
ptr_glTexGenxOES = unsafePerformIO $ getCommand "glTexGenxOES"

-- glTexGenxvOES ---------------------------------------------------------------

glTexGenxvOES
  :: MonadIO m
  => GLenum -- ^ @coord@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLfixed -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfixed@.
  -> m ()
glTexGenxvOES v1 v2 v3 = liftIO $ dyn163 ptr_glTexGenxvOES v1 v2 v3

{-# NOINLINE ptr_glTexGenxvOES #-}
ptr_glTexGenxvOES :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
ptr_glTexGenxvOES = unsafePerformIO $ getCommand "glTexGenxvOES"

-- glTexImage1D ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexImage1D.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glTexImage1D.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glTexImage1D.xhtml OpenGL 4.x>.
glTexImage1D
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @internalformat@ of type @TextureComponentCount@.
  -> GLsizei -- ^ @width@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @pixels@ pointing to @COMPSIZE(format,type,width)@ elements of type @a@.
  -> m ()
glTexImage1D v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn730 ptr_glTexImage1D v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glTexImage1D #-}
ptr_glTexImage1D :: FunPtr (GLenum -> GLint -> GLint -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glTexImage1D = unsafePerformIO $ getCommand "glTexImage1D"

-- glTexImage2D ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexImage2D.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glTexImage2D.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glTexImage2D.xhtml OpenGL 4.x>.
glTexImage2D
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @internalformat@ of type @TextureComponentCount@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @pixels@ pointing to @COMPSIZE(format,type,width,height)@ elements of type @a@.
  -> m ()
glTexImage2D v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn731 ptr_glTexImage2D v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glTexImage2D #-}
ptr_glTexImage2D :: FunPtr (GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glTexImage2D = unsafePerformIO $ getCommand "glTexImage2D"

-- glTexImage2DMultisample -----------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glTexImage2DMultisample.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glTexImage2DMultisample.xhtml OpenGL 4.x>.
glTexImage2DMultisample
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLsizei -- ^ @samples@.
  -> GLenum -- ^ @internalformat@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLboolean -- ^ @fixedsamplelocations@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glTexImage2DMultisample v1 v2 v3 v4 v5 v6 = liftIO $ dyn732 ptr_glTexImage2DMultisample v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glTexImage2DMultisample #-}
ptr_glTexImage2DMultisample :: FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLboolean -> IO ())
ptr_glTexImage2DMultisample = unsafePerformIO $ getCommand "glTexImage2DMultisample"

-- glTexImage2DMultisampleCoverageNV -------------------------------------------

glTexImage2DMultisampleCoverageNV
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLsizei -- ^ @coverageSamples@.
  -> GLsizei -- ^ @colorSamples@.
  -> GLint -- ^ @internalFormat@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLboolean -- ^ @fixedSampleLocations@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glTexImage2DMultisampleCoverageNV v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn733 ptr_glTexImage2DMultisampleCoverageNV v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glTexImage2DMultisampleCoverageNV #-}
ptr_glTexImage2DMultisampleCoverageNV :: FunPtr (GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> GLsizei -> GLboolean -> IO ())
ptr_glTexImage2DMultisampleCoverageNV = unsafePerformIO $ getCommand "glTexImage2DMultisampleCoverageNV"

-- glTexImage3D ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexImage3D.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glTexImage3D.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glTexImage3D.xhtml OpenGL 4.x>.
glTexImage3D
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
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
glTexImage3D v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO $ dyn734 ptr_glTexImage3D v1 v2 v3 v4 v5 v6 v7 v8 v9 v10

{-# NOINLINE ptr_glTexImage3D #-}
ptr_glTexImage3D :: FunPtr (GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glTexImage3D = unsafePerformIO $ getCommand "glTexImage3D"

-- glTexImage3DEXT -------------------------------------------------------------

-- | This command is an alias for 'glTexImage3D'.
glTexImage3DEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @pixels@ pointing to @COMPSIZE(format,type,width,height,depth)@ elements of type @a@.
  -> m ()
glTexImage3DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO $ dyn735 ptr_glTexImage3DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10

{-# NOINLINE ptr_glTexImage3DEXT #-}
ptr_glTexImage3DEXT :: FunPtr (GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glTexImage3DEXT = unsafePerformIO $ getCommand "glTexImage3DEXT"

-- glTexImage3DMultisample -----------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glTexImage3DMultisample.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glTexImage3DMultisample.xhtml OpenGL 4.x>.
glTexImage3DMultisample
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLsizei -- ^ @samples@.
  -> GLenum -- ^ @internalformat@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLboolean -- ^ @fixedsamplelocations@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glTexImage3DMultisample v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn736 ptr_glTexImage3DMultisample v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glTexImage3DMultisample #-}
ptr_glTexImage3DMultisample :: FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLboolean -> IO ())
ptr_glTexImage3DMultisample = unsafePerformIO $ getCommand "glTexImage3DMultisample"

