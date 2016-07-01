--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F56
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

module Graphics.GL.Functions.F56 (
  glSamplerParameteri,
  glSamplerParameteriv,
  glScaled,
  glScalef,
  glScalex,
  glScalexOES,
  glScissor,
  glScissorArrayv,
  glScissorArrayvNV,
  glScissorIndexed,
  glScissorIndexedNV,
  glScissorIndexedv,
  glScissorIndexedvNV,
  glSecondaryColor3b,
  glSecondaryColor3bEXT,
  glSecondaryColor3bv,
  glSecondaryColor3bvEXT,
  glSecondaryColor3d,
  glSecondaryColor3dEXT,
  glSecondaryColor3dv,
  glSecondaryColor3dvEXT,
  glSecondaryColor3f,
  glSecondaryColor3fEXT,
  glSecondaryColor3fv,
  glSecondaryColor3fvEXT,
  glSecondaryColor3hNV,
  glSecondaryColor3hvNV,
  glSecondaryColor3i,
  glSecondaryColor3iEXT,
  glSecondaryColor3iv,
  glSecondaryColor3ivEXT,
  glSecondaryColor3s,
  glSecondaryColor3sEXT,
  glSecondaryColor3sv,
  glSecondaryColor3svEXT,
  glSecondaryColor3ub,
  glSecondaryColor3ubEXT,
  glSecondaryColor3ubv,
  glSecondaryColor3ubvEXT,
  glSecondaryColor3ui
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

-- glSamplerParameteri ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glSamplerParameter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glSamplerParameter.xhtml OpenGL 4.x>.
glSamplerParameteri
  :: MonadIO m
  => GLuint -- ^ @sampler@.
  -> GLenum -- ^ @pname@.
  -> GLint -- ^ @param@.
  -> m ()
glSamplerParameteri v1 v2 v3 = liftIO $ dyn488 ptr_glSamplerParameteri v1 v2 v3

{-# NOINLINE ptr_glSamplerParameteri #-}
ptr_glSamplerParameteri :: FunPtr (GLuint -> GLenum -> GLint -> IO ())
ptr_glSamplerParameteri = unsafePerformIO $ getCommand "glSamplerParameteri"

-- glSamplerParameteriv --------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glSamplerParameter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glSamplerParameter.xhtml OpenGL 4.x>.
glSamplerParameteriv
  :: MonadIO m
  => GLuint -- ^ @sampler@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @param@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glSamplerParameteriv v1 v2 v3 = liftIO $ dyn334 ptr_glSamplerParameteriv v1 v2 v3

{-# NOINLINE ptr_glSamplerParameteriv #-}
ptr_glSamplerParameteriv :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glSamplerParameteriv = unsafePerformIO $ getCommand "glSamplerParameteriv"

-- glScaled --------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glScale.xml OpenGL 2.x>.
glScaled
  :: MonadIO m
  => GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> GLdouble -- ^ @z@.
  -> m ()
glScaled v1 v2 v3 = liftIO $ dyn38 ptr_glScaled v1 v2 v3

{-# NOINLINE ptr_glScaled #-}
ptr_glScaled :: FunPtr (GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glScaled = unsafePerformIO $ getCommand "glScaled"

-- glScalef --------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glScale.xml OpenGL 2.x>.
glScalef
  :: MonadIO m
  => GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> m ()
glScalef v1 v2 v3 = liftIO $ dyn40 ptr_glScalef v1 v2 v3

{-# NOINLINE ptr_glScalef #-}
ptr_glScalef :: FunPtr (GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glScalef = unsafePerformIO $ getCommand "glScalef"

-- glScalex --------------------------------------------------------------------

glScalex
  :: MonadIO m
  => GLfixed -- ^ @x@.
  -> GLfixed -- ^ @y@.
  -> GLfixed -- ^ @z@.
  -> m ()
glScalex v1 v2 v3 = liftIO $ dyn106 ptr_glScalex v1 v2 v3

{-# NOINLINE ptr_glScalex #-}
ptr_glScalex :: FunPtr (GLfixed -> GLfixed -> GLfixed -> IO ())
ptr_glScalex = unsafePerformIO $ getCommand "glScalex"

-- glScalexOES -----------------------------------------------------------------

glScalexOES
  :: MonadIO m
  => GLfixed -- ^ @x@.
  -> GLfixed -- ^ @y@.
  -> GLfixed -- ^ @z@.
  -> m ()
glScalexOES v1 v2 v3 = liftIO $ dyn106 ptr_glScalexOES v1 v2 v3

{-# NOINLINE ptr_glScalexOES #-}
ptr_glScalexOES :: FunPtr (GLfixed -> GLfixed -> GLfixed -> IO ())
ptr_glScalexOES = unsafePerformIO $ getCommand "glScalexOES"

-- glScissor -------------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glScissor.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glScissor.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glScissor.xhtml OpenGL 4.x>.
glScissor
  :: MonadIO m
  => GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glScissor v1 v2 v3 v4 = liftIO $ dyn698 ptr_glScissor v1 v2 v3 v4

{-# NOINLINE ptr_glScissor #-}
ptr_glScissor :: FunPtr (GLint -> GLint -> GLsizei -> GLsizei -> IO ())
ptr_glScissor = unsafePerformIO $ getCommand "glScissor"

-- glScissorArrayv -------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glScissorArray.xhtml OpenGL 4.x>.
glScissorArrayv
  :: MonadIO m
  => GLuint -- ^ @first@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint -- ^ @v@ pointing to @COMPSIZE(count)@ elements of type @GLint@.
  -> m ()
glScissorArrayv v1 v2 v3 = liftIO $ dyn699 ptr_glScissorArrayv v1 v2 v3

{-# NOINLINE ptr_glScissorArrayv #-}
ptr_glScissorArrayv :: FunPtr (GLuint -> GLsizei -> Ptr GLint -> IO ())
ptr_glScissorArrayv = unsafePerformIO $ getCommand "glScissorArrayv"

-- glScissorArrayvNV -----------------------------------------------------------

-- | This command is an alias for 'glScissorArrayv'.
glScissorArrayvNV
  :: MonadIO m
  => GLuint -- ^ @first@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint -- ^ @v@ pointing to @COMPSIZE(count)@ elements of type @GLint@.
  -> m ()
glScissorArrayvNV v1 v2 v3 = liftIO $ dyn699 ptr_glScissorArrayvNV v1 v2 v3

{-# NOINLINE ptr_glScissorArrayvNV #-}
ptr_glScissorArrayvNV :: FunPtr (GLuint -> GLsizei -> Ptr GLint -> IO ())
ptr_glScissorArrayvNV = unsafePerformIO $ getCommand "glScissorArrayvNV"

-- glScissorIndexed ------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glScissorIndexed.xhtml OpenGL 4.x>.
glScissorIndexed
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLint -- ^ @left@.
  -> GLint -- ^ @bottom@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glScissorIndexed v1 v2 v3 v4 v5 = liftIO $ dyn700 ptr_glScissorIndexed v1 v2 v3 v4 v5

{-# NOINLINE ptr_glScissorIndexed #-}
ptr_glScissorIndexed :: FunPtr (GLuint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ())
ptr_glScissorIndexed = unsafePerformIO $ getCommand "glScissorIndexed"

-- glScissorIndexedNV ----------------------------------------------------------

-- | This command is an alias for 'glScissorIndexed'.
glScissorIndexedNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLint -- ^ @left@.
  -> GLint -- ^ @bottom@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glScissorIndexedNV v1 v2 v3 v4 v5 = liftIO $ dyn700 ptr_glScissorIndexedNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glScissorIndexedNV #-}
ptr_glScissorIndexedNV :: FunPtr (GLuint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ())
ptr_glScissorIndexedNV = unsafePerformIO $ getCommand "glScissorIndexedNV"

-- glScissorIndexedv -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glScissorIndexed.xhtml OpenGL 4.x>.
glScissorIndexedv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLint -- ^ @v@ pointing to @4@ elements of type @GLint@.
  -> m ()
glScissorIndexedv v1 v2 = liftIO $ dyn701 ptr_glScissorIndexedv v1 v2

{-# NOINLINE ptr_glScissorIndexedv #-}
ptr_glScissorIndexedv :: FunPtr (GLuint -> Ptr GLint -> IO ())
ptr_glScissorIndexedv = unsafePerformIO $ getCommand "glScissorIndexedv"

-- glScissorIndexedvNV ---------------------------------------------------------

-- | This command is an alias for 'glScissorIndexedv'.
glScissorIndexedvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLint -- ^ @v@ pointing to @4@ elements of type @GLint@.
  -> m ()
glScissorIndexedvNV v1 v2 = liftIO $ dyn701 ptr_glScissorIndexedvNV v1 v2

{-# NOINLINE ptr_glScissorIndexedvNV #-}
ptr_glScissorIndexedvNV :: FunPtr (GLuint -> Ptr GLint -> IO ())
ptr_glScissorIndexedvNV = unsafePerformIO $ getCommand "glScissorIndexedvNV"

-- glSecondaryColor3b ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glSecondaryColor.xml OpenGL 2.x>. The vector equivalent of this command is 'glSecondaryColor3bv'.
glSecondaryColor3b
  :: MonadIO m
  => GLbyte -- ^ @red@ of type @ColorB@.
  -> GLbyte -- ^ @green@ of type @ColorB@.
  -> GLbyte -- ^ @blue@ of type @ColorB@.
  -> m ()
glSecondaryColor3b v1 v2 v3 = liftIO $ dyn36 ptr_glSecondaryColor3b v1 v2 v3

{-# NOINLINE ptr_glSecondaryColor3b #-}
ptr_glSecondaryColor3b :: FunPtr (GLbyte -> GLbyte -> GLbyte -> IO ())
ptr_glSecondaryColor3b = unsafePerformIO $ getCommand "glSecondaryColor3b"

-- glSecondaryColor3bEXT -------------------------------------------------------

-- | The vector equivalent of this command is 'glSecondaryColor3bvEXT'. This command is an alias for 'glSecondaryColor3b'.
glSecondaryColor3bEXT
  :: MonadIO m
  => GLbyte -- ^ @red@ of type @ColorB@.
  -> GLbyte -- ^ @green@ of type @ColorB@.
  -> GLbyte -- ^ @blue@ of type @ColorB@.
  -> m ()
glSecondaryColor3bEXT v1 v2 v3 = liftIO $ dyn36 ptr_glSecondaryColor3bEXT v1 v2 v3

{-# NOINLINE ptr_glSecondaryColor3bEXT #-}
ptr_glSecondaryColor3bEXT :: FunPtr (GLbyte -> GLbyte -> GLbyte -> IO ())
ptr_glSecondaryColor3bEXT = unsafePerformIO $ getCommand "glSecondaryColor3bEXT"

-- glSecondaryColor3bv ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glSecondaryColor.xml OpenGL 2.x>.
glSecondaryColor3bv
  :: MonadIO m
  => Ptr GLbyte -- ^ @v@ pointing to @3@ elements of type @ColorB@.
  -> m ()
glSecondaryColor3bv v1 = liftIO $ dyn37 ptr_glSecondaryColor3bv v1

{-# NOINLINE ptr_glSecondaryColor3bv #-}
ptr_glSecondaryColor3bv :: FunPtr (Ptr GLbyte -> IO ())
ptr_glSecondaryColor3bv = unsafePerformIO $ getCommand "glSecondaryColor3bv"

-- glSecondaryColor3bvEXT ------------------------------------------------------

-- | This command is an alias for 'glSecondaryColor3bv'.
glSecondaryColor3bvEXT
  :: MonadIO m
  => Ptr GLbyte -- ^ @v@ pointing to @3@ elements of type @ColorB@.
  -> m ()
glSecondaryColor3bvEXT v1 = liftIO $ dyn37 ptr_glSecondaryColor3bvEXT v1

{-# NOINLINE ptr_glSecondaryColor3bvEXT #-}
ptr_glSecondaryColor3bvEXT :: FunPtr (Ptr GLbyte -> IO ())
ptr_glSecondaryColor3bvEXT = unsafePerformIO $ getCommand "glSecondaryColor3bvEXT"

-- glSecondaryColor3d ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glSecondaryColor.xml OpenGL 2.x>. The vector equivalent of this command is 'glSecondaryColor3dv'.
glSecondaryColor3d
  :: MonadIO m
  => GLdouble -- ^ @red@ of type @ColorD@.
  -> GLdouble -- ^ @green@ of type @ColorD@.
  -> GLdouble -- ^ @blue@ of type @ColorD@.
  -> m ()
glSecondaryColor3d v1 v2 v3 = liftIO $ dyn38 ptr_glSecondaryColor3d v1 v2 v3

{-# NOINLINE ptr_glSecondaryColor3d #-}
ptr_glSecondaryColor3d :: FunPtr (GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glSecondaryColor3d = unsafePerformIO $ getCommand "glSecondaryColor3d"

-- glSecondaryColor3dEXT -------------------------------------------------------

-- | The vector equivalent of this command is 'glSecondaryColor3dvEXT'. This command is an alias for 'glSecondaryColor3d'.
glSecondaryColor3dEXT
  :: MonadIO m
  => GLdouble -- ^ @red@ of type @ColorD@.
  -> GLdouble -- ^ @green@ of type @ColorD@.
  -> GLdouble -- ^ @blue@ of type @ColorD@.
  -> m ()
glSecondaryColor3dEXT v1 v2 v3 = liftIO $ dyn38 ptr_glSecondaryColor3dEXT v1 v2 v3

{-# NOINLINE ptr_glSecondaryColor3dEXT #-}
ptr_glSecondaryColor3dEXT :: FunPtr (GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glSecondaryColor3dEXT = unsafePerformIO $ getCommand "glSecondaryColor3dEXT"

-- glSecondaryColor3dv ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glSecondaryColor.xml OpenGL 2.x>.
glSecondaryColor3dv
  :: MonadIO m
  => Ptr GLdouble -- ^ @v@ pointing to @3@ elements of type @ColorD@.
  -> m ()
glSecondaryColor3dv v1 = liftIO $ dyn39 ptr_glSecondaryColor3dv v1

{-# NOINLINE ptr_glSecondaryColor3dv #-}
ptr_glSecondaryColor3dv :: FunPtr (Ptr GLdouble -> IO ())
ptr_glSecondaryColor3dv = unsafePerformIO $ getCommand "glSecondaryColor3dv"

-- glSecondaryColor3dvEXT ------------------------------------------------------

-- | This command is an alias for 'glSecondaryColor3dv'.
glSecondaryColor3dvEXT
  :: MonadIO m
  => Ptr GLdouble -- ^ @v@ pointing to @3@ elements of type @ColorD@.
  -> m ()
glSecondaryColor3dvEXT v1 = liftIO $ dyn39 ptr_glSecondaryColor3dvEXT v1

{-# NOINLINE ptr_glSecondaryColor3dvEXT #-}
ptr_glSecondaryColor3dvEXT :: FunPtr (Ptr GLdouble -> IO ())
ptr_glSecondaryColor3dvEXT = unsafePerformIO $ getCommand "glSecondaryColor3dvEXT"

-- glSecondaryColor3f ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glSecondaryColor.xml OpenGL 2.x>. The vector equivalent of this command is 'glSecondaryColor3fv'.
glSecondaryColor3f
  :: MonadIO m
  => GLfloat -- ^ @red@ of type @ColorF@.
  -> GLfloat -- ^ @green@ of type @ColorF@.
  -> GLfloat -- ^ @blue@ of type @ColorF@.
  -> m ()
glSecondaryColor3f v1 v2 v3 = liftIO $ dyn40 ptr_glSecondaryColor3f v1 v2 v3

{-# NOINLINE ptr_glSecondaryColor3f #-}
ptr_glSecondaryColor3f :: FunPtr (GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glSecondaryColor3f = unsafePerformIO $ getCommand "glSecondaryColor3f"

-- glSecondaryColor3fEXT -------------------------------------------------------

-- | The vector equivalent of this command is 'glSecondaryColor3fvEXT'. This command is an alias for 'glSecondaryColor3f'.
glSecondaryColor3fEXT
  :: MonadIO m
  => GLfloat -- ^ @red@ of type @ColorF@.
  -> GLfloat -- ^ @green@ of type @ColorF@.
  -> GLfloat -- ^ @blue@ of type @ColorF@.
  -> m ()
glSecondaryColor3fEXT v1 v2 v3 = liftIO $ dyn40 ptr_glSecondaryColor3fEXT v1 v2 v3

{-# NOINLINE ptr_glSecondaryColor3fEXT #-}
ptr_glSecondaryColor3fEXT :: FunPtr (GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glSecondaryColor3fEXT = unsafePerformIO $ getCommand "glSecondaryColor3fEXT"

-- glSecondaryColor3fv ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glSecondaryColor.xml OpenGL 2.x>.
glSecondaryColor3fv
  :: MonadIO m
  => Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @ColorF@.
  -> m ()
glSecondaryColor3fv v1 = liftIO $ dyn41 ptr_glSecondaryColor3fv v1

{-# NOINLINE ptr_glSecondaryColor3fv #-}
ptr_glSecondaryColor3fv :: FunPtr (Ptr GLfloat -> IO ())
ptr_glSecondaryColor3fv = unsafePerformIO $ getCommand "glSecondaryColor3fv"

-- glSecondaryColor3fvEXT ------------------------------------------------------

-- | This command is an alias for 'glSecondaryColor3fv'.
glSecondaryColor3fvEXT
  :: MonadIO m
  => Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @ColorF@.
  -> m ()
glSecondaryColor3fvEXT v1 = liftIO $ dyn41 ptr_glSecondaryColor3fvEXT v1

{-# NOINLINE ptr_glSecondaryColor3fvEXT #-}
ptr_glSecondaryColor3fvEXT :: FunPtr (Ptr GLfloat -> IO ())
ptr_glSecondaryColor3fvEXT = unsafePerformIO $ getCommand "glSecondaryColor3fvEXT"

-- glSecondaryColor3hNV --------------------------------------------------------

-- | The vector equivalent of this command is 'glSecondaryColor3hvNV'.
glSecondaryColor3hNV
  :: MonadIO m
  => GLhalfNV -- ^ @red@ of type @Half16NV@.
  -> GLhalfNV -- ^ @green@ of type @Half16NV@.
  -> GLhalfNV -- ^ @blue@ of type @Half16NV@.
  -> m ()
glSecondaryColor3hNV v1 v2 v3 = liftIO $ dyn98 ptr_glSecondaryColor3hNV v1 v2 v3

{-# NOINLINE ptr_glSecondaryColor3hNV #-}
ptr_glSecondaryColor3hNV :: FunPtr (GLhalfNV -> GLhalfNV -> GLhalfNV -> IO ())
ptr_glSecondaryColor3hNV = unsafePerformIO $ getCommand "glSecondaryColor3hNV"

-- glSecondaryColor3hvNV -------------------------------------------------------

glSecondaryColor3hvNV
  :: MonadIO m
  => Ptr GLhalfNV -- ^ @v@ pointing to @3@ elements of type @Half16NV@.
  -> m ()
glSecondaryColor3hvNV v1 = liftIO $ dyn99 ptr_glSecondaryColor3hvNV v1

{-# NOINLINE ptr_glSecondaryColor3hvNV #-}
ptr_glSecondaryColor3hvNV :: FunPtr (Ptr GLhalfNV -> IO ())
ptr_glSecondaryColor3hvNV = unsafePerformIO $ getCommand "glSecondaryColor3hvNV"

-- glSecondaryColor3i ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glSecondaryColor.xml OpenGL 2.x>. The vector equivalent of this command is 'glSecondaryColor3iv'.
glSecondaryColor3i
  :: MonadIO m
  => GLint -- ^ @red@ of type @ColorI@.
  -> GLint -- ^ @green@ of type @ColorI@.
  -> GLint -- ^ @blue@ of type @ColorI@.
  -> m ()
glSecondaryColor3i v1 v2 v3 = liftIO $ dyn42 ptr_glSecondaryColor3i v1 v2 v3

{-# NOINLINE ptr_glSecondaryColor3i #-}
ptr_glSecondaryColor3i :: FunPtr (GLint -> GLint -> GLint -> IO ())
ptr_glSecondaryColor3i = unsafePerformIO $ getCommand "glSecondaryColor3i"

-- glSecondaryColor3iEXT -------------------------------------------------------

-- | The vector equivalent of this command is 'glSecondaryColor3ivEXT'. This command is an alias for 'glSecondaryColor3i'.
glSecondaryColor3iEXT
  :: MonadIO m
  => GLint -- ^ @red@ of type @ColorI@.
  -> GLint -- ^ @green@ of type @ColorI@.
  -> GLint -- ^ @blue@ of type @ColorI@.
  -> m ()
glSecondaryColor3iEXT v1 v2 v3 = liftIO $ dyn42 ptr_glSecondaryColor3iEXT v1 v2 v3

{-# NOINLINE ptr_glSecondaryColor3iEXT #-}
ptr_glSecondaryColor3iEXT :: FunPtr (GLint -> GLint -> GLint -> IO ())
ptr_glSecondaryColor3iEXT = unsafePerformIO $ getCommand "glSecondaryColor3iEXT"

-- glSecondaryColor3iv ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glSecondaryColor.xml OpenGL 2.x>.
glSecondaryColor3iv
  :: MonadIO m
  => Ptr GLint -- ^ @v@ pointing to @3@ elements of type @ColorI@.
  -> m ()
glSecondaryColor3iv v1 = liftIO $ dyn43 ptr_glSecondaryColor3iv v1

{-# NOINLINE ptr_glSecondaryColor3iv #-}
ptr_glSecondaryColor3iv :: FunPtr (Ptr GLint -> IO ())
ptr_glSecondaryColor3iv = unsafePerformIO $ getCommand "glSecondaryColor3iv"

-- glSecondaryColor3ivEXT ------------------------------------------------------

-- | This command is an alias for 'glSecondaryColor3iv'.
glSecondaryColor3ivEXT
  :: MonadIO m
  => Ptr GLint -- ^ @v@ pointing to @3@ elements of type @ColorI@.
  -> m ()
glSecondaryColor3ivEXT v1 = liftIO $ dyn43 ptr_glSecondaryColor3ivEXT v1

{-# NOINLINE ptr_glSecondaryColor3ivEXT #-}
ptr_glSecondaryColor3ivEXT :: FunPtr (Ptr GLint -> IO ())
ptr_glSecondaryColor3ivEXT = unsafePerformIO $ getCommand "glSecondaryColor3ivEXT"

-- glSecondaryColor3s ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glSecondaryColor.xml OpenGL 2.x>. The vector equivalent of this command is 'glSecondaryColor3sv'.
glSecondaryColor3s
  :: MonadIO m
  => GLshort -- ^ @red@ of type @ColorS@.
  -> GLshort -- ^ @green@ of type @ColorS@.
  -> GLshort -- ^ @blue@ of type @ColorS@.
  -> m ()
glSecondaryColor3s v1 v2 v3 = liftIO $ dyn44 ptr_glSecondaryColor3s v1 v2 v3

{-# NOINLINE ptr_glSecondaryColor3s #-}
ptr_glSecondaryColor3s :: FunPtr (GLshort -> GLshort -> GLshort -> IO ())
ptr_glSecondaryColor3s = unsafePerformIO $ getCommand "glSecondaryColor3s"

-- glSecondaryColor3sEXT -------------------------------------------------------

-- | The vector equivalent of this command is 'glSecondaryColor3svEXT'. This command is an alias for 'glSecondaryColor3s'.
glSecondaryColor3sEXT
  :: MonadIO m
  => GLshort -- ^ @red@ of type @ColorS@.
  -> GLshort -- ^ @green@ of type @ColorS@.
  -> GLshort -- ^ @blue@ of type @ColorS@.
  -> m ()
glSecondaryColor3sEXT v1 v2 v3 = liftIO $ dyn44 ptr_glSecondaryColor3sEXT v1 v2 v3

{-# NOINLINE ptr_glSecondaryColor3sEXT #-}
ptr_glSecondaryColor3sEXT :: FunPtr (GLshort -> GLshort -> GLshort -> IO ())
ptr_glSecondaryColor3sEXT = unsafePerformIO $ getCommand "glSecondaryColor3sEXT"

-- glSecondaryColor3sv ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glSecondaryColor.xml OpenGL 2.x>.
glSecondaryColor3sv
  :: MonadIO m
  => Ptr GLshort -- ^ @v@ pointing to @3@ elements of type @ColorS@.
  -> m ()
glSecondaryColor3sv v1 = liftIO $ dyn45 ptr_glSecondaryColor3sv v1

{-# NOINLINE ptr_glSecondaryColor3sv #-}
ptr_glSecondaryColor3sv :: FunPtr (Ptr GLshort -> IO ())
ptr_glSecondaryColor3sv = unsafePerformIO $ getCommand "glSecondaryColor3sv"

-- glSecondaryColor3svEXT ------------------------------------------------------

-- | This command is an alias for 'glSecondaryColor3sv'.
glSecondaryColor3svEXT
  :: MonadIO m
  => Ptr GLshort -- ^ @v@ pointing to @3@ elements of type @ColorS@.
  -> m ()
glSecondaryColor3svEXT v1 = liftIO $ dyn45 ptr_glSecondaryColor3svEXT v1

{-# NOINLINE ptr_glSecondaryColor3svEXT #-}
ptr_glSecondaryColor3svEXT :: FunPtr (Ptr GLshort -> IO ())
ptr_glSecondaryColor3svEXT = unsafePerformIO $ getCommand "glSecondaryColor3svEXT"

-- glSecondaryColor3ub ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glSecondaryColor.xml OpenGL 2.x>. The vector equivalent of this command is 'glSecondaryColor3ubv'.
glSecondaryColor3ub
  :: MonadIO m
  => GLubyte -- ^ @red@ of type @ColorUB@.
  -> GLubyte -- ^ @green@ of type @ColorUB@.
  -> GLubyte -- ^ @blue@ of type @ColorUB@.
  -> m ()
glSecondaryColor3ub v1 v2 v3 = liftIO $ dyn100 ptr_glSecondaryColor3ub v1 v2 v3

{-# NOINLINE ptr_glSecondaryColor3ub #-}
ptr_glSecondaryColor3ub :: FunPtr (GLubyte -> GLubyte -> GLubyte -> IO ())
ptr_glSecondaryColor3ub = unsafePerformIO $ getCommand "glSecondaryColor3ub"

-- glSecondaryColor3ubEXT ------------------------------------------------------

-- | The vector equivalent of this command is 'glSecondaryColor3ubvEXT'. This command is an alias for 'glSecondaryColor3ub'.
glSecondaryColor3ubEXT
  :: MonadIO m
  => GLubyte -- ^ @red@ of type @ColorUB@.
  -> GLubyte -- ^ @green@ of type @ColorUB@.
  -> GLubyte -- ^ @blue@ of type @ColorUB@.
  -> m ()
glSecondaryColor3ubEXT v1 v2 v3 = liftIO $ dyn100 ptr_glSecondaryColor3ubEXT v1 v2 v3

{-# NOINLINE ptr_glSecondaryColor3ubEXT #-}
ptr_glSecondaryColor3ubEXT :: FunPtr (GLubyte -> GLubyte -> GLubyte -> IO ())
ptr_glSecondaryColor3ubEXT = unsafePerformIO $ getCommand "glSecondaryColor3ubEXT"

-- glSecondaryColor3ubv --------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glSecondaryColor.xml OpenGL 2.x>.
glSecondaryColor3ubv
  :: MonadIO m
  => Ptr GLubyte -- ^ @v@ pointing to @3@ elements of type @ColorUB@.
  -> m ()
glSecondaryColor3ubv v1 = liftIO $ dyn101 ptr_glSecondaryColor3ubv v1

{-# NOINLINE ptr_glSecondaryColor3ubv #-}
ptr_glSecondaryColor3ubv :: FunPtr (Ptr GLubyte -> IO ())
ptr_glSecondaryColor3ubv = unsafePerformIO $ getCommand "glSecondaryColor3ubv"

-- glSecondaryColor3ubvEXT -----------------------------------------------------

-- | This command is an alias for 'glSecondaryColor3ubv'.
glSecondaryColor3ubvEXT
  :: MonadIO m
  => Ptr GLubyte -- ^ @v@ pointing to @3@ elements of type @ColorUB@.
  -> m ()
glSecondaryColor3ubvEXT v1 = liftIO $ dyn101 ptr_glSecondaryColor3ubvEXT v1

{-# NOINLINE ptr_glSecondaryColor3ubvEXT #-}
ptr_glSecondaryColor3ubvEXT :: FunPtr (Ptr GLubyte -> IO ())
ptr_glSecondaryColor3ubvEXT = unsafePerformIO $ getCommand "glSecondaryColor3ubvEXT"

-- glSecondaryColor3ui ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glSecondaryColor.xml OpenGL 2.x>. The vector equivalent of this command is 'glSecondaryColor3uiv'.
glSecondaryColor3ui
  :: MonadIO m
  => GLuint -- ^ @red@ of type @ColorUI@.
  -> GLuint -- ^ @green@ of type @ColorUI@.
  -> GLuint -- ^ @blue@ of type @ColorUI@.
  -> m ()
glSecondaryColor3ui v1 v2 v3 = liftIO $ dyn102 ptr_glSecondaryColor3ui v1 v2 v3

{-# NOINLINE ptr_glSecondaryColor3ui #-}
ptr_glSecondaryColor3ui :: FunPtr (GLuint -> GLuint -> GLuint -> IO ())
ptr_glSecondaryColor3ui = unsafePerformIO $ getCommand "glSecondaryColor3ui"

