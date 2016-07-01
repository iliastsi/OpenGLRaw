--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F07
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

module Graphics.GL.Functions.F07 (
  glColor3xOES,
  glColor3xvOES,
  glColor4b,
  glColor4bv,
  glColor4d,
  glColor4dv,
  glColor4f,
  glColor4fNormal3fVertex3fSUN,
  glColor4fNormal3fVertex3fvSUN,
  glColor4fv,
  glColor4hNV,
  glColor4hvNV,
  glColor4i,
  glColor4iv,
  glColor4s,
  glColor4sv,
  glColor4ub,
  glColor4ubVertex2fSUN,
  glColor4ubVertex2fvSUN,
  glColor4ubVertex3fSUN,
  glColor4ubVertex3fvSUN,
  glColor4ubv,
  glColor4ui,
  glColor4uiv,
  glColor4us,
  glColor4usv,
  glColor4x,
  glColor4xOES,
  glColor4xvOES,
  glColorFormatNV,
  glColorFragmentOp1ATI,
  glColorFragmentOp2ATI,
  glColorFragmentOp3ATI,
  glColorMask,
  glColorMaskIndexedEXT,
  glColorMaski,
  glColorMaskiEXT,
  glColorMaskiOES,
  glColorMaterial,
  glColorP3ui
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

-- glColor3xOES ----------------------------------------------------------------

glColor3xOES
  :: MonadIO m
  => GLfixed -- ^ @red@.
  -> GLfixed -- ^ @green@.
  -> GLfixed -- ^ @blue@.
  -> m ()
glColor3xOES v1 v2 v3 = liftIO $ dyn106 ptr_glColor3xOES v1 v2 v3

{-# NOINLINE ptr_glColor3xOES #-}
ptr_glColor3xOES :: FunPtr (GLfixed -> GLfixed -> GLfixed -> IO ())
ptr_glColor3xOES = unsafePerformIO $ getCommand "glColor3xOES"

-- glColor3xvOES ---------------------------------------------------------------

glColor3xvOES
  :: MonadIO m
  => Ptr GLfixed -- ^ @components@ pointing to @3@ elements of type @GLfixed@.
  -> m ()
glColor3xvOES v1 = liftIO $ dyn107 ptr_glColor3xvOES v1

{-# NOINLINE ptr_glColor3xvOES #-}
ptr_glColor3xvOES :: FunPtr (Ptr GLfixed -> IO ())
ptr_glColor3xvOES = unsafePerformIO $ getCommand "glColor3xvOES"

-- glColor4b -------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>. The vector equivalent of this command is 'glColor4bv'.
glColor4b
  :: MonadIO m
  => GLbyte -- ^ @red@ of type @ColorB@.
  -> GLbyte -- ^ @green@ of type @ColorB@.
  -> GLbyte -- ^ @blue@ of type @ColorB@.
  -> GLbyte -- ^ @alpha@ of type @ColorB@.
  -> m ()
glColor4b v1 v2 v3 v4 = liftIO $ dyn108 ptr_glColor4b v1 v2 v3 v4

{-# NOINLINE ptr_glColor4b #-}
ptr_glColor4b :: FunPtr (GLbyte -> GLbyte -> GLbyte -> GLbyte -> IO ())
ptr_glColor4b = unsafePerformIO $ getCommand "glColor4b"

-- glColor4bv ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>.
glColor4bv
  :: MonadIO m
  => Ptr GLbyte -- ^ @v@ pointing to @4@ elements of type @ColorB@.
  -> m ()
glColor4bv v1 = liftIO $ dyn37 ptr_glColor4bv v1

{-# NOINLINE ptr_glColor4bv #-}
ptr_glColor4bv :: FunPtr (Ptr GLbyte -> IO ())
ptr_glColor4bv = unsafePerformIO $ getCommand "glColor4bv"

-- glColor4d -------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>. The vector equivalent of this command is 'glColor4dv'.
glColor4d
  :: MonadIO m
  => GLdouble -- ^ @red@ of type @ColorD@.
  -> GLdouble -- ^ @green@ of type @ColorD@.
  -> GLdouble -- ^ @blue@ of type @ColorD@.
  -> GLdouble -- ^ @alpha@ of type @ColorD@.
  -> m ()
glColor4d v1 v2 v3 v4 = liftIO $ dyn109 ptr_glColor4d v1 v2 v3 v4

{-# NOINLINE ptr_glColor4d #-}
ptr_glColor4d :: FunPtr (GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glColor4d = unsafePerformIO $ getCommand "glColor4d"

-- glColor4dv ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>.
glColor4dv
  :: MonadIO m
  => Ptr GLdouble -- ^ @v@ pointing to @4@ elements of type @ColorD@.
  -> m ()
glColor4dv v1 = liftIO $ dyn39 ptr_glColor4dv v1

{-# NOINLINE ptr_glColor4dv #-}
ptr_glColor4dv :: FunPtr (Ptr GLdouble -> IO ())
ptr_glColor4dv = unsafePerformIO $ getCommand "glColor4dv"

-- glColor4f -------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>. The vector equivalent of this command is 'glColor4fv'.
glColor4f
  :: MonadIO m
  => GLfloat -- ^ @red@ of type @ColorF@.
  -> GLfloat -- ^ @green@ of type @ColorF@.
  -> GLfloat -- ^ @blue@ of type @ColorF@.
  -> GLfloat -- ^ @alpha@ of type @ColorF@.
  -> m ()
glColor4f v1 v2 v3 v4 = liftIO $ dyn49 ptr_glColor4f v1 v2 v3 v4

{-# NOINLINE ptr_glColor4f #-}
ptr_glColor4f :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glColor4f = unsafePerformIO $ getCommand "glColor4f"

-- glColor4fNormal3fVertex3fSUN ------------------------------------------------

glColor4fNormal3fVertex3fSUN
  :: MonadIO m
  => GLfloat -- ^ @r@.
  -> GLfloat -- ^ @g@.
  -> GLfloat -- ^ @b@.
  -> GLfloat -- ^ @a@.
  -> GLfloat -- ^ @nx@.
  -> GLfloat -- ^ @ny@.
  -> GLfloat -- ^ @nz@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> m ()
glColor4fNormal3fVertex3fSUN v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO $ dyn110 ptr_glColor4fNormal3fVertex3fSUN v1 v2 v3 v4 v5 v6 v7 v8 v9 v10

{-# NOINLINE ptr_glColor4fNormal3fVertex3fSUN #-}
ptr_glColor4fNormal3fVertex3fSUN :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glColor4fNormal3fVertex3fSUN = unsafePerformIO $ getCommand "glColor4fNormal3fVertex3fSUN"

-- glColor4fNormal3fVertex3fvSUN -----------------------------------------------

glColor4fNormal3fVertex3fvSUN
  :: MonadIO m
  => Ptr GLfloat -- ^ @c@ pointing to @4@ elements of type @GLfloat@.
  -> Ptr GLfloat -- ^ @n@ pointing to @3@ elements of type @GLfloat@.
  -> Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @GLfloat@.
  -> m ()
glColor4fNormal3fVertex3fvSUN v1 v2 v3 = liftIO $ dyn111 ptr_glColor4fNormal3fVertex3fvSUN v1 v2 v3

{-# NOINLINE ptr_glColor4fNormal3fVertex3fvSUN #-}
ptr_glColor4fNormal3fVertex3fvSUN :: FunPtr (Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> IO ())
ptr_glColor4fNormal3fVertex3fvSUN = unsafePerformIO $ getCommand "glColor4fNormal3fVertex3fvSUN"

-- glColor4fv ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>.
glColor4fv
  :: MonadIO m
  => Ptr GLfloat -- ^ @v@ pointing to @4@ elements of type @ColorF@.
  -> m ()
glColor4fv v1 = liftIO $ dyn41 ptr_glColor4fv v1

{-# NOINLINE ptr_glColor4fv #-}
ptr_glColor4fv :: FunPtr (Ptr GLfloat -> IO ())
ptr_glColor4fv = unsafePerformIO $ getCommand "glColor4fv"

-- glColor4hNV -----------------------------------------------------------------

-- | The vector equivalent of this command is 'glColor4hvNV'.
glColor4hNV
  :: MonadIO m
  => GLhalfNV -- ^ @red@ of type @Half16NV@.
  -> GLhalfNV -- ^ @green@ of type @Half16NV@.
  -> GLhalfNV -- ^ @blue@ of type @Half16NV@.
  -> GLhalfNV -- ^ @alpha@ of type @Half16NV@.
  -> m ()
glColor4hNV v1 v2 v3 v4 = liftIO $ dyn112 ptr_glColor4hNV v1 v2 v3 v4

{-# NOINLINE ptr_glColor4hNV #-}
ptr_glColor4hNV :: FunPtr (GLhalfNV -> GLhalfNV -> GLhalfNV -> GLhalfNV -> IO ())
ptr_glColor4hNV = unsafePerformIO $ getCommand "glColor4hNV"

-- glColor4hvNV ----------------------------------------------------------------

glColor4hvNV
  :: MonadIO m
  => Ptr GLhalfNV -- ^ @v@ pointing to @4@ elements of type @Half16NV@.
  -> m ()
glColor4hvNV v1 = liftIO $ dyn99 ptr_glColor4hvNV v1

{-# NOINLINE ptr_glColor4hvNV #-}
ptr_glColor4hvNV :: FunPtr (Ptr GLhalfNV -> IO ())
ptr_glColor4hvNV = unsafePerformIO $ getCommand "glColor4hvNV"

-- glColor4i -------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>. The vector equivalent of this command is 'glColor4iv'.
glColor4i
  :: MonadIO m
  => GLint -- ^ @red@ of type @ColorI@.
  -> GLint -- ^ @green@ of type @ColorI@.
  -> GLint -- ^ @blue@ of type @ColorI@.
  -> GLint -- ^ @alpha@ of type @ColorI@.
  -> m ()
glColor4i v1 v2 v3 v4 = liftIO $ dyn76 ptr_glColor4i v1 v2 v3 v4

{-# NOINLINE ptr_glColor4i #-}
ptr_glColor4i :: FunPtr (GLint -> GLint -> GLint -> GLint -> IO ())
ptr_glColor4i = unsafePerformIO $ getCommand "glColor4i"

-- glColor4iv ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>.
glColor4iv
  :: MonadIO m
  => Ptr GLint -- ^ @v@ pointing to @4@ elements of type @ColorI@.
  -> m ()
glColor4iv v1 = liftIO $ dyn43 ptr_glColor4iv v1

{-# NOINLINE ptr_glColor4iv #-}
ptr_glColor4iv :: FunPtr (Ptr GLint -> IO ())
ptr_glColor4iv = unsafePerformIO $ getCommand "glColor4iv"

-- glColor4s -------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>. The vector equivalent of this command is 'glColor4sv'.
glColor4s
  :: MonadIO m
  => GLshort -- ^ @red@ of type @ColorS@.
  -> GLshort -- ^ @green@ of type @ColorS@.
  -> GLshort -- ^ @blue@ of type @ColorS@.
  -> GLshort -- ^ @alpha@ of type @ColorS@.
  -> m ()
glColor4s v1 v2 v3 v4 = liftIO $ dyn113 ptr_glColor4s v1 v2 v3 v4

{-# NOINLINE ptr_glColor4s #-}
ptr_glColor4s :: FunPtr (GLshort -> GLshort -> GLshort -> GLshort -> IO ())
ptr_glColor4s = unsafePerformIO $ getCommand "glColor4s"

-- glColor4sv ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>.
glColor4sv
  :: MonadIO m
  => Ptr GLshort -- ^ @v@ pointing to @4@ elements of type @ColorS@.
  -> m ()
glColor4sv v1 = liftIO $ dyn45 ptr_glColor4sv v1

{-# NOINLINE ptr_glColor4sv #-}
ptr_glColor4sv :: FunPtr (Ptr GLshort -> IO ())
ptr_glColor4sv = unsafePerformIO $ getCommand "glColor4sv"

-- glColor4ub ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>. The vector equivalent of this command is 'glColor4ubv'.
glColor4ub
  :: MonadIO m
  => GLubyte -- ^ @red@ of type @ColorUB@.
  -> GLubyte -- ^ @green@ of type @ColorUB@.
  -> GLubyte -- ^ @blue@ of type @ColorUB@.
  -> GLubyte -- ^ @alpha@ of type @ColorUB@.
  -> m ()
glColor4ub v1 v2 v3 v4 = liftIO $ dyn114 ptr_glColor4ub v1 v2 v3 v4

{-# NOINLINE ptr_glColor4ub #-}
ptr_glColor4ub :: FunPtr (GLubyte -> GLubyte -> GLubyte -> GLubyte -> IO ())
ptr_glColor4ub = unsafePerformIO $ getCommand "glColor4ub"

-- glColor4ubVertex2fSUN -------------------------------------------------------

glColor4ubVertex2fSUN
  :: MonadIO m
  => GLubyte -- ^ @r@.
  -> GLubyte -- ^ @g@.
  -> GLubyte -- ^ @b@.
  -> GLubyte -- ^ @a@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> m ()
glColor4ubVertex2fSUN v1 v2 v3 v4 v5 v6 = liftIO $ dyn115 ptr_glColor4ubVertex2fSUN v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glColor4ubVertex2fSUN #-}
ptr_glColor4ubVertex2fSUN :: FunPtr (GLubyte -> GLubyte -> GLubyte -> GLubyte -> GLfloat -> GLfloat -> IO ())
ptr_glColor4ubVertex2fSUN = unsafePerformIO $ getCommand "glColor4ubVertex2fSUN"

-- glColor4ubVertex2fvSUN ------------------------------------------------------

glColor4ubVertex2fvSUN
  :: MonadIO m
  => Ptr GLubyte -- ^ @c@ pointing to @4@ elements of type @GLubyte@.
  -> Ptr GLfloat -- ^ @v@ pointing to @2@ elements of type @GLfloat@.
  -> m ()
glColor4ubVertex2fvSUN v1 v2 = liftIO $ dyn116 ptr_glColor4ubVertex2fvSUN v1 v2

{-# NOINLINE ptr_glColor4ubVertex2fvSUN #-}
ptr_glColor4ubVertex2fvSUN :: FunPtr (Ptr GLubyte -> Ptr GLfloat -> IO ())
ptr_glColor4ubVertex2fvSUN = unsafePerformIO $ getCommand "glColor4ubVertex2fvSUN"

-- glColor4ubVertex3fSUN -------------------------------------------------------

glColor4ubVertex3fSUN
  :: MonadIO m
  => GLubyte -- ^ @r@.
  -> GLubyte -- ^ @g@.
  -> GLubyte -- ^ @b@.
  -> GLubyte -- ^ @a@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> m ()
glColor4ubVertex3fSUN v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn117 ptr_glColor4ubVertex3fSUN v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glColor4ubVertex3fSUN #-}
ptr_glColor4ubVertex3fSUN :: FunPtr (GLubyte -> GLubyte -> GLubyte -> GLubyte -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glColor4ubVertex3fSUN = unsafePerformIO $ getCommand "glColor4ubVertex3fSUN"

-- glColor4ubVertex3fvSUN ------------------------------------------------------

glColor4ubVertex3fvSUN
  :: MonadIO m
  => Ptr GLubyte -- ^ @c@ pointing to @4@ elements of type @GLubyte@.
  -> Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @GLfloat@.
  -> m ()
glColor4ubVertex3fvSUN v1 v2 = liftIO $ dyn116 ptr_glColor4ubVertex3fvSUN v1 v2

{-# NOINLINE ptr_glColor4ubVertex3fvSUN #-}
ptr_glColor4ubVertex3fvSUN :: FunPtr (Ptr GLubyte -> Ptr GLfloat -> IO ())
ptr_glColor4ubVertex3fvSUN = unsafePerformIO $ getCommand "glColor4ubVertex3fvSUN"

-- glColor4ubv -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>.
glColor4ubv
  :: MonadIO m
  => Ptr GLubyte -- ^ @v@ pointing to @4@ elements of type @ColorUB@.
  -> m ()
glColor4ubv v1 = liftIO $ dyn101 ptr_glColor4ubv v1

{-# NOINLINE ptr_glColor4ubv #-}
ptr_glColor4ubv :: FunPtr (Ptr GLubyte -> IO ())
ptr_glColor4ubv = unsafePerformIO $ getCommand "glColor4ubv"

-- glColor4ui ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>. The vector equivalent of this command is 'glColor4uiv'.
glColor4ui
  :: MonadIO m
  => GLuint -- ^ @red@ of type @ColorUI@.
  -> GLuint -- ^ @green@ of type @ColorUI@.
  -> GLuint -- ^ @blue@ of type @ColorUI@.
  -> GLuint -- ^ @alpha@ of type @ColorUI@.
  -> m ()
glColor4ui v1 v2 v3 v4 = liftIO $ dyn77 ptr_glColor4ui v1 v2 v3 v4

{-# NOINLINE ptr_glColor4ui #-}
ptr_glColor4ui :: FunPtr (GLuint -> GLuint -> GLuint -> GLuint -> IO ())
ptr_glColor4ui = unsafePerformIO $ getCommand "glColor4ui"

-- glColor4uiv -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>.
glColor4uiv
  :: MonadIO m
  => Ptr GLuint -- ^ @v@ pointing to @4@ elements of type @ColorUI@.
  -> m ()
glColor4uiv v1 = liftIO $ dyn103 ptr_glColor4uiv v1

{-# NOINLINE ptr_glColor4uiv #-}
ptr_glColor4uiv :: FunPtr (Ptr GLuint -> IO ())
ptr_glColor4uiv = unsafePerformIO $ getCommand "glColor4uiv"

-- glColor4us ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>. The vector equivalent of this command is 'glColor4usv'.
glColor4us
  :: MonadIO m
  => GLushort -- ^ @red@ of type @ColorUS@.
  -> GLushort -- ^ @green@ of type @ColorUS@.
  -> GLushort -- ^ @blue@ of type @ColorUS@.
  -> GLushort -- ^ @alpha@ of type @ColorUS@.
  -> m ()
glColor4us v1 v2 v3 v4 = liftIO $ dyn118 ptr_glColor4us v1 v2 v3 v4

{-# NOINLINE ptr_glColor4us #-}
ptr_glColor4us :: FunPtr (GLushort -> GLushort -> GLushort -> GLushort -> IO ())
ptr_glColor4us = unsafePerformIO $ getCommand "glColor4us"

-- glColor4usv -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml OpenGL 2.x>.
glColor4usv
  :: MonadIO m
  => Ptr GLushort -- ^ @v@ pointing to @4@ elements of type @ColorUS@.
  -> m ()
glColor4usv v1 = liftIO $ dyn105 ptr_glColor4usv v1

{-# NOINLINE ptr_glColor4usv #-}
ptr_glColor4usv :: FunPtr (Ptr GLushort -> IO ())
ptr_glColor4usv = unsafePerformIO $ getCommand "glColor4usv"

-- glColor4x -------------------------------------------------------------------

glColor4x
  :: MonadIO m
  => GLfixed -- ^ @red@.
  -> GLfixed -- ^ @green@.
  -> GLfixed -- ^ @blue@.
  -> GLfixed -- ^ @alpha@.
  -> m ()
glColor4x v1 v2 v3 v4 = liftIO $ dyn50 ptr_glColor4x v1 v2 v3 v4

{-# NOINLINE ptr_glColor4x #-}
ptr_glColor4x :: FunPtr (GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ())
ptr_glColor4x = unsafePerformIO $ getCommand "glColor4x"

-- glColor4xOES ----------------------------------------------------------------

glColor4xOES
  :: MonadIO m
  => GLfixed -- ^ @red@.
  -> GLfixed -- ^ @green@.
  -> GLfixed -- ^ @blue@.
  -> GLfixed -- ^ @alpha@.
  -> m ()
glColor4xOES v1 v2 v3 v4 = liftIO $ dyn50 ptr_glColor4xOES v1 v2 v3 v4

{-# NOINLINE ptr_glColor4xOES #-}
ptr_glColor4xOES :: FunPtr (GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ())
ptr_glColor4xOES = unsafePerformIO $ getCommand "glColor4xOES"

-- glColor4xvOES ---------------------------------------------------------------

glColor4xvOES
  :: MonadIO m
  => Ptr GLfixed -- ^ @components@ pointing to @4@ elements of type @GLfixed@.
  -> m ()
glColor4xvOES v1 = liftIO $ dyn107 ptr_glColor4xvOES v1

{-# NOINLINE ptr_glColor4xvOES #-}
ptr_glColor4xvOES :: FunPtr (Ptr GLfixed -> IO ())
ptr_glColor4xvOES = unsafePerformIO $ getCommand "glColor4xvOES"

-- glColorFormatNV -------------------------------------------------------------

glColorFormatNV
  :: MonadIO m
  => GLint -- ^ @size@.
  -> GLenum -- ^ @type@.
  -> GLsizei -- ^ @stride@.
  -> m ()
glColorFormatNV v1 v2 v3 = liftIO $ dyn119 ptr_glColorFormatNV v1 v2 v3

{-# NOINLINE ptr_glColorFormatNV #-}
ptr_glColorFormatNV :: FunPtr (GLint -> GLenum -> GLsizei -> IO ())
ptr_glColorFormatNV = unsafePerformIO $ getCommand "glColorFormatNV"

-- glColorFragmentOp1ATI -------------------------------------------------------

glColorFragmentOp1ATI
  :: MonadIO m
  => GLenum -- ^ @op@ of type @FragmentOpATI@.
  -> GLuint -- ^ @dst@.
  -> GLuint -- ^ @dstMask@.
  -> GLuint -- ^ @dstMod@.
  -> GLuint -- ^ @arg1@.
  -> GLuint -- ^ @arg1Rep@.
  -> GLuint -- ^ @arg1Mod@.
  -> m ()
glColorFragmentOp1ATI v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn120 ptr_glColorFragmentOp1ATI v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glColorFragmentOp1ATI #-}
ptr_glColorFragmentOp1ATI :: FunPtr (GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ())
ptr_glColorFragmentOp1ATI = unsafePerformIO $ getCommand "glColorFragmentOp1ATI"

-- glColorFragmentOp2ATI -------------------------------------------------------

glColorFragmentOp2ATI
  :: MonadIO m
  => GLenum -- ^ @op@ of type @FragmentOpATI@.
  -> GLuint -- ^ @dst@.
  -> GLuint -- ^ @dstMask@.
  -> GLuint -- ^ @dstMod@.
  -> GLuint -- ^ @arg1@.
  -> GLuint -- ^ @arg1Rep@.
  -> GLuint -- ^ @arg1Mod@.
  -> GLuint -- ^ @arg2@.
  -> GLuint -- ^ @arg2Rep@.
  -> GLuint -- ^ @arg2Mod@.
  -> m ()
glColorFragmentOp2ATI v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO $ dyn121 ptr_glColorFragmentOp2ATI v1 v2 v3 v4 v5 v6 v7 v8 v9 v10

{-# NOINLINE ptr_glColorFragmentOp2ATI #-}
ptr_glColorFragmentOp2ATI :: FunPtr (GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ())
ptr_glColorFragmentOp2ATI = unsafePerformIO $ getCommand "glColorFragmentOp2ATI"

-- glColorFragmentOp3ATI -------------------------------------------------------

glColorFragmentOp3ATI
  :: MonadIO m
  => GLenum -- ^ @op@ of type @FragmentOpATI@.
  -> GLuint -- ^ @dst@.
  -> GLuint -- ^ @dstMask@.
  -> GLuint -- ^ @dstMod@.
  -> GLuint -- ^ @arg1@.
  -> GLuint -- ^ @arg1Rep@.
  -> GLuint -- ^ @arg1Mod@.
  -> GLuint -- ^ @arg2@.
  -> GLuint -- ^ @arg2Rep@.
  -> GLuint -- ^ @arg2Mod@.
  -> GLuint -- ^ @arg3@.
  -> GLuint -- ^ @arg3Rep@.
  -> GLuint -- ^ @arg3Mod@.
  -> m ()
glColorFragmentOp3ATI v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 = liftIO $ dyn122 ptr_glColorFragmentOp3ATI v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13

{-# NOINLINE ptr_glColorFragmentOp3ATI #-}
ptr_glColorFragmentOp3ATI :: FunPtr (GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ())
ptr_glColorFragmentOp3ATI = unsafePerformIO $ getCommand "glColorFragmentOp3ATI"

-- glColorMask -----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glColorMask.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glColorMask.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glColorMask.xhtml OpenGL 4.x>.
glColorMask
  :: MonadIO m
  => GLboolean -- ^ @red@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLboolean -- ^ @green@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLboolean -- ^ @blue@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLboolean -- ^ @alpha@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glColorMask v1 v2 v3 v4 = liftIO $ dyn123 ptr_glColorMask v1 v2 v3 v4

{-# NOINLINE ptr_glColorMask #-}
ptr_glColorMask :: FunPtr (GLboolean -> GLboolean -> GLboolean -> GLboolean -> IO ())
ptr_glColorMask = unsafePerformIO $ getCommand "glColorMask"

-- glColorMaskIndexedEXT -------------------------------------------------------

-- | This command is an alias for 'glColorMaski'.
glColorMaskIndexedEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLboolean -- ^ @r@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLboolean -- ^ @g@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLboolean -- ^ @b@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLboolean -- ^ @a@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glColorMaskIndexedEXT v1 v2 v3 v4 v5 = liftIO $ dyn124 ptr_glColorMaskIndexedEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glColorMaskIndexedEXT #-}
ptr_glColorMaskIndexedEXT :: FunPtr (GLuint -> GLboolean -> GLboolean -> GLboolean -> GLboolean -> IO ())
ptr_glColorMaskIndexedEXT = unsafePerformIO $ getCommand "glColorMaskIndexedEXT"

-- glColorMaski ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glColorMask.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glColorMask.xhtml OpenGL 4.x>.
glColorMaski
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLboolean -- ^ @r@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLboolean -- ^ @g@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLboolean -- ^ @b@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLboolean -- ^ @a@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glColorMaski v1 v2 v3 v4 v5 = liftIO $ dyn124 ptr_glColorMaski v1 v2 v3 v4 v5

{-# NOINLINE ptr_glColorMaski #-}
ptr_glColorMaski :: FunPtr (GLuint -> GLboolean -> GLboolean -> GLboolean -> GLboolean -> IO ())
ptr_glColorMaski = unsafePerformIO $ getCommand "glColorMaski"

-- glColorMaskiEXT -------------------------------------------------------------

-- | This command is an alias for 'glColorMaski'.
glColorMaskiEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLboolean -- ^ @r@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLboolean -- ^ @g@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLboolean -- ^ @b@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLboolean -- ^ @a@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glColorMaskiEXT v1 v2 v3 v4 v5 = liftIO $ dyn124 ptr_glColorMaskiEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glColorMaskiEXT #-}
ptr_glColorMaskiEXT :: FunPtr (GLuint -> GLboolean -> GLboolean -> GLboolean -> GLboolean -> IO ())
ptr_glColorMaskiEXT = unsafePerformIO $ getCommand "glColorMaskiEXT"

-- glColorMaskiOES -------------------------------------------------------------

-- | This command is an alias for 'glColorMaski'.
glColorMaskiOES
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLboolean -- ^ @r@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLboolean -- ^ @g@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLboolean -- ^ @b@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLboolean -- ^ @a@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glColorMaskiOES v1 v2 v3 v4 v5 = liftIO $ dyn124 ptr_glColorMaskiOES v1 v2 v3 v4 v5

{-# NOINLINE ptr_glColorMaskiOES #-}
ptr_glColorMaskiOES :: FunPtr (GLuint -> GLboolean -> GLboolean -> GLboolean -> GLboolean -> IO ())
ptr_glColorMaskiOES = unsafePerformIO $ getCommand "glColorMaskiOES"

-- glColorMaterial -------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColorMaterial.xml OpenGL 2.x>.
glColorMaterial
  :: MonadIO m
  => GLenum -- ^ @face@ of type [MaterialFace](Graphics-GL-Groups.html#MaterialFace).
  -> GLenum -- ^ @mode@ of type [ColorMaterialParameter](Graphics-GL-Groups.html#ColorMaterialParameter).
  -> m ()
glColorMaterial v1 v2 = liftIO $ dyn51 ptr_glColorMaterial v1 v2

{-# NOINLINE ptr_glColorMaterial #-}
ptr_glColorMaterial :: FunPtr (GLenum -> GLenum -> IO ())
ptr_glColorMaterial = unsafePerformIO $ getCommand "glColorMaterial"

-- glColorP3ui -----------------------------------------------------------------

glColorP3ui
  :: MonadIO m
  => GLenum -- ^ @type@.
  -> GLuint -- ^ @color@.
  -> m ()
glColorP3ui v1 v2 = liftIO $ dyn16 ptr_glColorP3ui v1 v2

{-# NOINLINE ptr_glColorP3ui #-}
ptr_glColorP3ui :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glColorP3ui = unsafePerformIO $ getCommand "glColorP3ui"

