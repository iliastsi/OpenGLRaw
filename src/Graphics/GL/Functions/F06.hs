--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F06
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

module Graphics.GL.Functions.F06 (
  glClearNamedFramebufferiv,
  glClearNamedFramebufferuiv,
  glClearPixelLocalStorageuiEXT,
  glClearStencil,
  glClearTexImage,
  glClearTexSubImage,
  glClientActiveTexture,
  glClientActiveTextureARB,
  glClientActiveVertexStreamATI,
  glClientAttribDefaultEXT,
  glClientWaitSync,
  glClientWaitSyncAPPLE,
  glClipControl,
  glClipPlane,
  glClipPlanef,
  glClipPlanefIMG,
  glClipPlanefOES,
  glClipPlanex,
  glClipPlanexIMG,
  glClipPlanexOES,
  glColor3b,
  glColor3bv,
  glColor3d,
  glColor3dv,
  glColor3f,
  glColor3fVertex3fSUN,
  glColor3fVertex3fvSUN,
  glColor3fv,
  glColor3hNV,
  glColor3hvNV,
  glColor3i,
  glColor3iv,
  glColor3s,
  glColor3sv,
  glColor3ub,
  glColor3ubv,
  glColor3ui,
  glColor3uiv,
  glColor3us,
  glColor3usv
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

-- glClearNamedFramebufferiv ---------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glClearBuffer.xhtml OpenGL 4.x>.
glClearNamedFramebufferiv
  :: MonadIO m
  => GLuint -- ^ @framebuffer@.
  -> GLenum -- ^ @buffer@.
  -> GLint -- ^ @drawbuffer@.
  -> Ptr GLint -- ^ @value@.
  -> m ()
glClearNamedFramebufferiv v1 v2 v3 v4 = liftIO $ dyn87 ptr_glClearNamedFramebufferiv v1 v2 v3 v4

{-# NOINLINE ptr_glClearNamedFramebufferiv #-}
ptr_glClearNamedFramebufferiv :: FunPtr (GLuint -> GLenum -> GLint -> Ptr GLint -> IO ())
ptr_glClearNamedFramebufferiv = unsafePerformIO $ getCommand "glClearNamedFramebufferiv"

-- glClearNamedFramebufferuiv --------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glClearBuffer.xhtml OpenGL 4.x>.
glClearNamedFramebufferuiv
  :: MonadIO m
  => GLuint -- ^ @framebuffer@.
  -> GLenum -- ^ @buffer@.
  -> GLint -- ^ @drawbuffer@.
  -> Ptr GLuint -- ^ @value@.
  -> m ()
glClearNamedFramebufferuiv v1 v2 v3 v4 = liftIO $ dyn88 ptr_glClearNamedFramebufferuiv v1 v2 v3 v4

{-# NOINLINE ptr_glClearNamedFramebufferuiv #-}
ptr_glClearNamedFramebufferuiv :: FunPtr (GLuint -> GLenum -> GLint -> Ptr GLuint -> IO ())
ptr_glClearNamedFramebufferuiv = unsafePerformIO $ getCommand "glClearNamedFramebufferuiv"

-- glClearPixelLocalStorageuiEXT -----------------------------------------------

glClearPixelLocalStorageuiEXT
  :: MonadIO m
  => GLsizei -- ^ @offset@.
  -> GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @values@.
  -> m ()
glClearPixelLocalStorageuiEXT v1 v2 v3 = liftIO $ dyn89 ptr_glClearPixelLocalStorageuiEXT v1 v2 v3

{-# NOINLINE ptr_glClearPixelLocalStorageuiEXT #-}
ptr_glClearPixelLocalStorageuiEXT :: FunPtr (GLsizei -> GLsizei -> Ptr GLuint -> IO ())
ptr_glClearPixelLocalStorageuiEXT = unsafePerformIO $ getCommand "glClearPixelLocalStorageuiEXT"

-- glClearStencil --------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glClearStencil.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glClearStencil.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glClearStencil.xhtml OpenGL 4.x>.
glClearStencil
  :: MonadIO m
  => GLint -- ^ @s@ of type @StencilValue@.
  -> m ()
glClearStencil v1 = liftIO $ dyn12 ptr_glClearStencil v1

{-# NOINLINE ptr_glClearStencil #-}
ptr_glClearStencil :: FunPtr (GLint -> IO ())
ptr_glClearStencil = unsafePerformIO $ getCommand "glClearStencil"

-- glClearTexImage -------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glClearTexImage.xhtml OpenGL 4.x>.
glClearTexImage
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLenum -- ^ @format@.
  -> GLenum -- ^ @type@.
  -> Ptr a -- ^ @data@ pointing to @COMPSIZE(format,type)@ elements of type @a@.
  -> m ()
glClearTexImage v1 v2 v3 v4 v5 = liftIO $ dyn90 ptr_glClearTexImage v1 v2 v3 v4 v5

{-# NOINLINE ptr_glClearTexImage #-}
ptr_glClearTexImage :: FunPtr (GLuint -> GLint -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glClearTexImage = unsafePerformIO $ getCommand "glClearTexImage"

-- glClearTexSubImage ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glClearTexSubImage.xhtml OpenGL 4.x>.
glClearTexSubImage
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLint -- ^ @xoffset@.
  -> GLint -- ^ @yoffset@.
  -> GLint -- ^ @zoffset@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLenum -- ^ @format@.
  -> GLenum -- ^ @type@.
  -> Ptr a -- ^ @data@ pointing to @COMPSIZE(format,type)@ elements of type @a@.
  -> m ()
glClearTexSubImage v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 = liftIO $ dyn91 ptr_glClearTexSubImage v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11

{-# NOINLINE ptr_glClearTexSubImage #-}
ptr_glClearTexSubImage :: FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glClearTexSubImage = unsafePerformIO $ getCommand "glClearTexSubImage"

-- glClientActiveTexture -------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glClientActiveTexture.xml OpenGL 2.x>.
glClientActiveTexture
  :: MonadIO m
  => GLenum -- ^ @texture@ of type @TextureUnit@.
  -> m ()
glClientActiveTexture v1 = liftIO $ dyn4 ptr_glClientActiveTexture v1

{-# NOINLINE ptr_glClientActiveTexture #-}
ptr_glClientActiveTexture :: FunPtr (GLenum -> IO ())
ptr_glClientActiveTexture = unsafePerformIO $ getCommand "glClientActiveTexture"

-- glClientActiveTextureARB ----------------------------------------------------

-- | This command is an alias for 'glClientActiveTexture'.
glClientActiveTextureARB
  :: MonadIO m
  => GLenum -- ^ @texture@ of type @TextureUnit@.
  -> m ()
glClientActiveTextureARB v1 = liftIO $ dyn4 ptr_glClientActiveTextureARB v1

{-# NOINLINE ptr_glClientActiveTextureARB #-}
ptr_glClientActiveTextureARB :: FunPtr (GLenum -> IO ())
ptr_glClientActiveTextureARB = unsafePerformIO $ getCommand "glClientActiveTextureARB"

-- glClientActiveVertexStreamATI -----------------------------------------------

glClientActiveVertexStreamATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type @VertexStreamATI@.
  -> m ()
glClientActiveVertexStreamATI v1 = liftIO $ dyn4 ptr_glClientActiveVertexStreamATI v1

{-# NOINLINE ptr_glClientActiveVertexStreamATI #-}
ptr_glClientActiveVertexStreamATI :: FunPtr (GLenum -> IO ())
ptr_glClientActiveVertexStreamATI = unsafePerformIO $ getCommand "glClientActiveVertexStreamATI"

-- glClientAttribDefaultEXT ----------------------------------------------------

glClientAttribDefaultEXT
  :: MonadIO m
  => GLbitfield -- ^ @mask@ of type [ClientAttribMask](Graphics-GL-Groups.html#ClientAttribMask).
  -> m ()
glClientAttribDefaultEXT v1 = liftIO $ dyn69 ptr_glClientAttribDefaultEXT v1

{-# NOINLINE ptr_glClientAttribDefaultEXT #-}
ptr_glClientAttribDefaultEXT :: FunPtr (GLbitfield -> IO ())
ptr_glClientAttribDefaultEXT = unsafePerformIO $ getCommand "glClientAttribDefaultEXT"

-- glClientWaitSync ------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glClientWaitSync.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glClientWaitSync.xhtml OpenGL 4.x>.
glClientWaitSync
  :: MonadIO m
  => GLsync -- ^ @sync@ of type @sync@.
  -> GLbitfield -- ^ @flags@.
  -> GLuint64 -- ^ @timeout@.
  -> m GLenum
glClientWaitSync v1 v2 v3 = liftIO $ dyn92 ptr_glClientWaitSync v1 v2 v3

{-# NOINLINE ptr_glClientWaitSync #-}
ptr_glClientWaitSync :: FunPtr (GLsync -> GLbitfield -> GLuint64 -> IO GLenum)
ptr_glClientWaitSync = unsafePerformIO $ getCommand "glClientWaitSync"

-- glClientWaitSyncAPPLE -------------------------------------------------------

-- | This command is an alias for 'glClientWaitSync'.
glClientWaitSyncAPPLE
  :: MonadIO m
  => GLsync -- ^ @sync@.
  -> GLbitfield -- ^ @flags@.
  -> GLuint64 -- ^ @timeout@.
  -> m GLenum
glClientWaitSyncAPPLE v1 v2 v3 = liftIO $ dyn92 ptr_glClientWaitSyncAPPLE v1 v2 v3

{-# NOINLINE ptr_glClientWaitSyncAPPLE #-}
ptr_glClientWaitSyncAPPLE :: FunPtr (GLsync -> GLbitfield -> GLuint64 -> IO GLenum)
ptr_glClientWaitSyncAPPLE = unsafePerformIO $ getCommand "glClientWaitSyncAPPLE"

-- glClipControl ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glClipControl.xhtml OpenGL 4.x>.
glClipControl
  :: MonadIO m
  => GLenum -- ^ @origin@.
  -> GLenum -- ^ @depth@.
  -> m ()
glClipControl v1 v2 = liftIO $ dyn51 ptr_glClipControl v1 v2

{-# NOINLINE ptr_glClipControl #-}
ptr_glClipControl :: FunPtr (GLenum -> GLenum -> IO ())
ptr_glClipControl = unsafePerformIO $ getCommand "glClipControl"

-- glClipPlane -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glClipPlane.xml OpenGL 2.x>.
glClipPlane
  :: MonadIO m
  => GLenum -- ^ @plane@ of type [ClipPlaneName](Graphics-GL-Groups.html#ClipPlaneName).
  -> Ptr GLdouble -- ^ @equation@ pointing to @4@ elements of type @GLdouble@.
  -> m ()
glClipPlane v1 v2 = liftIO $ dyn93 ptr_glClipPlane v1 v2

{-# NOINLINE ptr_glClipPlane #-}
ptr_glClipPlane :: FunPtr (GLenum -> Ptr GLdouble -> IO ())
ptr_glClipPlane = unsafePerformIO $ getCommand "glClipPlane"

-- glClipPlanef ----------------------------------------------------------------

glClipPlanef
  :: MonadIO m
  => GLenum -- ^ @p@.
  -> Ptr GLfloat -- ^ @eqn@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glClipPlanef v1 v2 = liftIO $ dyn94 ptr_glClipPlanef v1 v2

{-# NOINLINE ptr_glClipPlanef #-}
ptr_glClipPlanef :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glClipPlanef = unsafePerformIO $ getCommand "glClipPlanef"

-- glClipPlanefIMG -------------------------------------------------------------

glClipPlanefIMG
  :: MonadIO m
  => GLenum -- ^ @p@.
  -> Ptr GLfloat -- ^ @eqn@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glClipPlanefIMG v1 v2 = liftIO $ dyn94 ptr_glClipPlanefIMG v1 v2

{-# NOINLINE ptr_glClipPlanefIMG #-}
ptr_glClipPlanefIMG :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glClipPlanefIMG = unsafePerformIO $ getCommand "glClipPlanefIMG"

-- glClipPlanefOES -------------------------------------------------------------

glClipPlanefOES
  :: MonadIO m
  => GLenum -- ^ @plane@.
  -> Ptr GLfloat -- ^ @equation@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glClipPlanefOES v1 v2 = liftIO $ dyn94 ptr_glClipPlanefOES v1 v2

{-# NOINLINE ptr_glClipPlanefOES #-}
ptr_glClipPlanefOES :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glClipPlanefOES = unsafePerformIO $ getCommand "glClipPlanefOES"

-- glClipPlanex ----------------------------------------------------------------

glClipPlanex
  :: MonadIO m
  => GLenum -- ^ @plane@.
  -> Ptr GLfixed -- ^ @equation@ pointing to @4@ elements of type @GLfixed@.
  -> m ()
glClipPlanex v1 v2 = liftIO $ dyn95 ptr_glClipPlanex v1 v2

{-# NOINLINE ptr_glClipPlanex #-}
ptr_glClipPlanex :: FunPtr (GLenum -> Ptr GLfixed -> IO ())
ptr_glClipPlanex = unsafePerformIO $ getCommand "glClipPlanex"

-- glClipPlanexIMG -------------------------------------------------------------

glClipPlanexIMG
  :: MonadIO m
  => GLenum -- ^ @p@.
  -> Ptr GLfixed -- ^ @eqn@ pointing to @4@ elements of type @GLfixed@.
  -> m ()
glClipPlanexIMG v1 v2 = liftIO $ dyn95 ptr_glClipPlanexIMG v1 v2

{-# NOINLINE ptr_glClipPlanexIMG #-}
ptr_glClipPlanexIMG :: FunPtr (GLenum -> Ptr GLfixed -> IO ())
ptr_glClipPlanexIMG = unsafePerformIO $ getCommand "glClipPlanexIMG"

-- glClipPlanexOES -------------------------------------------------------------

glClipPlanexOES
  :: MonadIO m
  => GLenum -- ^ @plane@.
  -> Ptr GLfixed -- ^ @equation@ pointing to @4@ elements of type @GLfixed@.
  -> m ()
glClipPlanexOES v1 v2 = liftIO $ dyn95 ptr_glClipPlanexOES v1 v2

{-# NOINLINE ptr_glClipPlanexOES #-}
ptr_glClipPlanexOES :: FunPtr (GLenum -> Ptr GLfixed -> IO ())
ptr_glClipPlanexOES = unsafePerformIO $ getCommand "glClipPlanexOES"

-- glColor3b -------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>. The vector equivalent of this command is 'glColor3bv'.
glColor3b
  :: MonadIO m
  => GLbyte -- ^ @red@ of type @ColorB@.
  -> GLbyte -- ^ @green@ of type @ColorB@.
  -> GLbyte -- ^ @blue@ of type @ColorB@.
  -> m ()
glColor3b v1 v2 v3 = liftIO $ dyn36 ptr_glColor3b v1 v2 v3

{-# NOINLINE ptr_glColor3b #-}
ptr_glColor3b :: FunPtr (GLbyte -> GLbyte -> GLbyte -> IO ())
ptr_glColor3b = unsafePerformIO $ getCommand "glColor3b"

-- glColor3bv ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>.
glColor3bv
  :: MonadIO m
  => Ptr GLbyte -- ^ @v@ pointing to @3@ elements of type @ColorB@.
  -> m ()
glColor3bv v1 = liftIO $ dyn37 ptr_glColor3bv v1

{-# NOINLINE ptr_glColor3bv #-}
ptr_glColor3bv :: FunPtr (Ptr GLbyte -> IO ())
ptr_glColor3bv = unsafePerformIO $ getCommand "glColor3bv"

-- glColor3d -------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>. The vector equivalent of this command is 'glColor3dv'.
glColor3d
  :: MonadIO m
  => GLdouble -- ^ @red@ of type @ColorD@.
  -> GLdouble -- ^ @green@ of type @ColorD@.
  -> GLdouble -- ^ @blue@ of type @ColorD@.
  -> m ()
glColor3d v1 v2 v3 = liftIO $ dyn38 ptr_glColor3d v1 v2 v3

{-# NOINLINE ptr_glColor3d #-}
ptr_glColor3d :: FunPtr (GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glColor3d = unsafePerformIO $ getCommand "glColor3d"

-- glColor3dv ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>.
glColor3dv
  :: MonadIO m
  => Ptr GLdouble -- ^ @v@ pointing to @3@ elements of type @ColorD@.
  -> m ()
glColor3dv v1 = liftIO $ dyn39 ptr_glColor3dv v1

{-# NOINLINE ptr_glColor3dv #-}
ptr_glColor3dv :: FunPtr (Ptr GLdouble -> IO ())
ptr_glColor3dv = unsafePerformIO $ getCommand "glColor3dv"

-- glColor3f -------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>. The vector equivalent of this command is 'glColor3fv'.
glColor3f
  :: MonadIO m
  => GLfloat -- ^ @red@ of type @ColorF@.
  -> GLfloat -- ^ @green@ of type @ColorF@.
  -> GLfloat -- ^ @blue@ of type @ColorF@.
  -> m ()
glColor3f v1 v2 v3 = liftIO $ dyn40 ptr_glColor3f v1 v2 v3

{-# NOINLINE ptr_glColor3f #-}
ptr_glColor3f :: FunPtr (GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glColor3f = unsafePerformIO $ getCommand "glColor3f"

-- glColor3fVertex3fSUN --------------------------------------------------------

glColor3fVertex3fSUN
  :: MonadIO m
  => GLfloat -- ^ @r@.
  -> GLfloat -- ^ @g@.
  -> GLfloat -- ^ @b@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> m ()
glColor3fVertex3fSUN v1 v2 v3 v4 v5 v6 = liftIO $ dyn96 ptr_glColor3fVertex3fSUN v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glColor3fVertex3fSUN #-}
ptr_glColor3fVertex3fSUN :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glColor3fVertex3fSUN = unsafePerformIO $ getCommand "glColor3fVertex3fSUN"

-- glColor3fVertex3fvSUN -------------------------------------------------------

glColor3fVertex3fvSUN
  :: MonadIO m
  => Ptr GLfloat -- ^ @c@ pointing to @3@ elements of type @GLfloat@.
  -> Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @GLfloat@.
  -> m ()
glColor3fVertex3fvSUN v1 v2 = liftIO $ dyn97 ptr_glColor3fVertex3fvSUN v1 v2

{-# NOINLINE ptr_glColor3fVertex3fvSUN #-}
ptr_glColor3fVertex3fvSUN :: FunPtr (Ptr GLfloat -> Ptr GLfloat -> IO ())
ptr_glColor3fVertex3fvSUN = unsafePerformIO $ getCommand "glColor3fVertex3fvSUN"

-- glColor3fv ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>.
glColor3fv
  :: MonadIO m
  => Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @ColorF@.
  -> m ()
glColor3fv v1 = liftIO $ dyn41 ptr_glColor3fv v1

{-# NOINLINE ptr_glColor3fv #-}
ptr_glColor3fv :: FunPtr (Ptr GLfloat -> IO ())
ptr_glColor3fv = unsafePerformIO $ getCommand "glColor3fv"

-- glColor3hNV -----------------------------------------------------------------

-- | The vector equivalent of this command is 'glColor3hvNV'.
glColor3hNV
  :: MonadIO m
  => GLhalfNV -- ^ @red@ of type @Half16NV@.
  -> GLhalfNV -- ^ @green@ of type @Half16NV@.
  -> GLhalfNV -- ^ @blue@ of type @Half16NV@.
  -> m ()
glColor3hNV v1 v2 v3 = liftIO $ dyn98 ptr_glColor3hNV v1 v2 v3

{-# NOINLINE ptr_glColor3hNV #-}
ptr_glColor3hNV :: FunPtr (GLhalfNV -> GLhalfNV -> GLhalfNV -> IO ())
ptr_glColor3hNV = unsafePerformIO $ getCommand "glColor3hNV"

-- glColor3hvNV ----------------------------------------------------------------

glColor3hvNV
  :: MonadIO m
  => Ptr GLhalfNV -- ^ @v@ pointing to @3@ elements of type @Half16NV@.
  -> m ()
glColor3hvNV v1 = liftIO $ dyn99 ptr_glColor3hvNV v1

{-# NOINLINE ptr_glColor3hvNV #-}
ptr_glColor3hvNV :: FunPtr (Ptr GLhalfNV -> IO ())
ptr_glColor3hvNV = unsafePerformIO $ getCommand "glColor3hvNV"

-- glColor3i -------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>. The vector equivalent of this command is 'glColor3iv'.
glColor3i
  :: MonadIO m
  => GLint -- ^ @red@ of type @ColorI@.
  -> GLint -- ^ @green@ of type @ColorI@.
  -> GLint -- ^ @blue@ of type @ColorI@.
  -> m ()
glColor3i v1 v2 v3 = liftIO $ dyn42 ptr_glColor3i v1 v2 v3

{-# NOINLINE ptr_glColor3i #-}
ptr_glColor3i :: FunPtr (GLint -> GLint -> GLint -> IO ())
ptr_glColor3i = unsafePerformIO $ getCommand "glColor3i"

-- glColor3iv ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>.
glColor3iv
  :: MonadIO m
  => Ptr GLint -- ^ @v@ pointing to @3@ elements of type @ColorI@.
  -> m ()
glColor3iv v1 = liftIO $ dyn43 ptr_glColor3iv v1

{-# NOINLINE ptr_glColor3iv #-}
ptr_glColor3iv :: FunPtr (Ptr GLint -> IO ())
ptr_glColor3iv = unsafePerformIO $ getCommand "glColor3iv"

-- glColor3s -------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>. The vector equivalent of this command is 'glColor3sv'.
glColor3s
  :: MonadIO m
  => GLshort -- ^ @red@ of type @ColorS@.
  -> GLshort -- ^ @green@ of type @ColorS@.
  -> GLshort -- ^ @blue@ of type @ColorS@.
  -> m ()
glColor3s v1 v2 v3 = liftIO $ dyn44 ptr_glColor3s v1 v2 v3

{-# NOINLINE ptr_glColor3s #-}
ptr_glColor3s :: FunPtr (GLshort -> GLshort -> GLshort -> IO ())
ptr_glColor3s = unsafePerformIO $ getCommand "glColor3s"

-- glColor3sv ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>.
glColor3sv
  :: MonadIO m
  => Ptr GLshort -- ^ @v@ pointing to @3@ elements of type @ColorS@.
  -> m ()
glColor3sv v1 = liftIO $ dyn45 ptr_glColor3sv v1

{-# NOINLINE ptr_glColor3sv #-}
ptr_glColor3sv :: FunPtr (Ptr GLshort -> IO ())
ptr_glColor3sv = unsafePerformIO $ getCommand "glColor3sv"

-- glColor3ub ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>. The vector equivalent of this command is 'glColor3ubv'.
glColor3ub
  :: MonadIO m
  => GLubyte -- ^ @red@ of type @ColorUB@.
  -> GLubyte -- ^ @green@ of type @ColorUB@.
  -> GLubyte -- ^ @blue@ of type @ColorUB@.
  -> m ()
glColor3ub v1 v2 v3 = liftIO $ dyn100 ptr_glColor3ub v1 v2 v3

{-# NOINLINE ptr_glColor3ub #-}
ptr_glColor3ub :: FunPtr (GLubyte -> GLubyte -> GLubyte -> IO ())
ptr_glColor3ub = unsafePerformIO $ getCommand "glColor3ub"

-- glColor3ubv -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>.
glColor3ubv
  :: MonadIO m
  => Ptr GLubyte -- ^ @v@ pointing to @3@ elements of type @ColorUB@.
  -> m ()
glColor3ubv v1 = liftIO $ dyn101 ptr_glColor3ubv v1

{-# NOINLINE ptr_glColor3ubv #-}
ptr_glColor3ubv :: FunPtr (Ptr GLubyte -> IO ())
ptr_glColor3ubv = unsafePerformIO $ getCommand "glColor3ubv"

-- glColor3ui ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>. The vector equivalent of this command is 'glColor3uiv'.
glColor3ui
  :: MonadIO m
  => GLuint -- ^ @red@ of type @ColorUI@.
  -> GLuint -- ^ @green@ of type @ColorUI@.
  -> GLuint -- ^ @blue@ of type @ColorUI@.
  -> m ()
glColor3ui v1 v2 v3 = liftIO $ dyn102 ptr_glColor3ui v1 v2 v3

{-# NOINLINE ptr_glColor3ui #-}
ptr_glColor3ui :: FunPtr (GLuint -> GLuint -> GLuint -> IO ())
ptr_glColor3ui = unsafePerformIO $ getCommand "glColor3ui"

-- glColor3uiv -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>.
glColor3uiv
  :: MonadIO m
  => Ptr GLuint -- ^ @v@ pointing to @3@ elements of type @ColorUI@.
  -> m ()
glColor3uiv v1 = liftIO $ dyn103 ptr_glColor3uiv v1

{-# NOINLINE ptr_glColor3uiv #-}
ptr_glColor3uiv :: FunPtr (Ptr GLuint -> IO ())
ptr_glColor3uiv = unsafePerformIO $ getCommand "glColor3uiv"

-- glColor3us ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>. The vector equivalent of this command is 'glColor3usv'.
glColor3us
  :: MonadIO m
  => GLushort -- ^ @red@ of type @ColorUS@.
  -> GLushort -- ^ @green@ of type @ColorUS@.
  -> GLushort -- ^ @blue@ of type @ColorUS@.
  -> m ()
glColor3us v1 v2 v3 = liftIO $ dyn104 ptr_glColor3us v1 v2 v3

{-# NOINLINE ptr_glColor3us #-}
ptr_glColor3us :: FunPtr (GLushort -> GLushort -> GLushort -> IO ())
ptr_glColor3us = unsafePerformIO $ getCommand "glColor3us"

-- glColor3usv -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>.
glColor3usv
  :: MonadIO m
  => Ptr GLushort -- ^ @v@ pointing to @3@ elements of type @ColorUS@.
  -> m ()
glColor3usv v1 = liftIO $ dyn105 ptr_glColor3usv v1

{-# NOINLINE ptr_glColor3usv #-}
ptr_glColor3usv :: FunPtr (Ptr GLushort -> IO ())
ptr_glColor3usv = unsafePerformIO $ getCommand "glColor3usv"

