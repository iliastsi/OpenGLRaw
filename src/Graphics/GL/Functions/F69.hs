--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F69
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

module Graphics.GL.Functions.F69 (
  glVertex3bvOES,
  glVertex3d,
  glVertex3dv,
  glVertex3f,
  glVertex3fv,
  glVertex3hNV,
  glVertex3hvNV,
  glVertex3i,
  glVertex3iv,
  glVertex3s,
  glVertex3sv,
  glVertex3xOES,
  glVertex3xvOES,
  glVertex4bOES,
  glVertex4bvOES,
  glVertex4d,
  glVertex4dv,
  glVertex4f,
  glVertex4fv,
  glVertex4hNV,
  glVertex4hvNV,
  glVertex4i,
  glVertex4iv,
  glVertex4s,
  glVertex4sv,
  glVertex4xOES,
  glVertex4xvOES,
  glVertexArrayAttribBinding,
  glVertexArrayAttribFormat,
  glVertexArrayAttribIFormat,
  glVertexArrayAttribLFormat,
  glVertexArrayBindVertexBufferEXT,
  glVertexArrayBindingDivisor,
  glVertexArrayColorOffsetEXT,
  glVertexArrayEdgeFlagOffsetEXT,
  glVertexArrayElementBuffer,
  glVertexArrayFogCoordOffsetEXT,
  glVertexArrayIndexOffsetEXT,
  glVertexArrayMultiTexCoordOffsetEXT,
  glVertexArrayNormalOffsetEXT
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

-- glVertex3bvOES --------------------------------------------------------------

glVertex3bvOES
  :: MonadIO m
  => Ptr GLbyte -- ^ @coords@ pointing to @3@ elements of type @GLbyte@.
  -> m ()
glVertex3bvOES v1 = liftIO $ dyn37 ptr_glVertex3bvOES v1

{-# NOINLINE ptr_glVertex3bvOES #-}
ptr_glVertex3bvOES :: FunPtr (Ptr GLbyte -> IO ())
ptr_glVertex3bvOES = unsafePerformIO $ getCommand "glVertex3bvOES"

-- glVertex3d ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml OpenGL 2.x>. The vector equivalent of this command is 'glVertex3dv'.
glVertex3d
  :: MonadIO m
  => GLdouble -- ^ @x@ of type @CoordD@.
  -> GLdouble -- ^ @y@ of type @CoordD@.
  -> GLdouble -- ^ @z@ of type @CoordD@.
  -> m ()
glVertex3d v1 v2 v3 = liftIO $ dyn38 ptr_glVertex3d v1 v2 v3

{-# NOINLINE ptr_glVertex3d #-}
ptr_glVertex3d :: FunPtr (GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glVertex3d = unsafePerformIO $ getCommand "glVertex3d"

-- glVertex3dv -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml OpenGL 2.x>.
glVertex3dv
  :: MonadIO m
  => Ptr GLdouble -- ^ @v@ pointing to @3@ elements of type @CoordD@.
  -> m ()
glVertex3dv v1 = liftIO $ dyn39 ptr_glVertex3dv v1

{-# NOINLINE ptr_glVertex3dv #-}
ptr_glVertex3dv :: FunPtr (Ptr GLdouble -> IO ())
ptr_glVertex3dv = unsafePerformIO $ getCommand "glVertex3dv"

-- glVertex3f ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml OpenGL 2.x>. The vector equivalent of this command is 'glVertex3fv'.
glVertex3f
  :: MonadIO m
  => GLfloat -- ^ @x@ of type @CoordF@.
  -> GLfloat -- ^ @y@ of type @CoordF@.
  -> GLfloat -- ^ @z@ of type @CoordF@.
  -> m ()
glVertex3f v1 v2 v3 = liftIO $ dyn40 ptr_glVertex3f v1 v2 v3

{-# NOINLINE ptr_glVertex3f #-}
ptr_glVertex3f :: FunPtr (GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glVertex3f = unsafePerformIO $ getCommand "glVertex3f"

-- glVertex3fv -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml OpenGL 2.x>.
glVertex3fv
  :: MonadIO m
  => Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @CoordF@.
  -> m ()
glVertex3fv v1 = liftIO $ dyn41 ptr_glVertex3fv v1

{-# NOINLINE ptr_glVertex3fv #-}
ptr_glVertex3fv :: FunPtr (Ptr GLfloat -> IO ())
ptr_glVertex3fv = unsafePerformIO $ getCommand "glVertex3fv"

-- glVertex3hNV ----------------------------------------------------------------

-- | The vector equivalent of this command is 'glVertex3hvNV'.
glVertex3hNV
  :: MonadIO m
  => GLhalfNV -- ^ @x@ of type @Half16NV@.
  -> GLhalfNV -- ^ @y@ of type @Half16NV@.
  -> GLhalfNV -- ^ @z@ of type @Half16NV@.
  -> m ()
glVertex3hNV v1 v2 v3 = liftIO $ dyn98 ptr_glVertex3hNV v1 v2 v3

{-# NOINLINE ptr_glVertex3hNV #-}
ptr_glVertex3hNV :: FunPtr (GLhalfNV -> GLhalfNV -> GLhalfNV -> IO ())
ptr_glVertex3hNV = unsafePerformIO $ getCommand "glVertex3hNV"

-- glVertex3hvNV ---------------------------------------------------------------

glVertex3hvNV
  :: MonadIO m
  => Ptr GLhalfNV -- ^ @v@ pointing to @3@ elements of type @Half16NV@.
  -> m ()
glVertex3hvNV v1 = liftIO $ dyn99 ptr_glVertex3hvNV v1

{-# NOINLINE ptr_glVertex3hvNV #-}
ptr_glVertex3hvNV :: FunPtr (Ptr GLhalfNV -> IO ())
ptr_glVertex3hvNV = unsafePerformIO $ getCommand "glVertex3hvNV"

-- glVertex3i ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml OpenGL 2.x>. The vector equivalent of this command is 'glVertex3iv'.
glVertex3i
  :: MonadIO m
  => GLint -- ^ @x@ of type @CoordI@.
  -> GLint -- ^ @y@ of type @CoordI@.
  -> GLint -- ^ @z@ of type @CoordI@.
  -> m ()
glVertex3i v1 v2 v3 = liftIO $ dyn42 ptr_glVertex3i v1 v2 v3

{-# NOINLINE ptr_glVertex3i #-}
ptr_glVertex3i :: FunPtr (GLint -> GLint -> GLint -> IO ())
ptr_glVertex3i = unsafePerformIO $ getCommand "glVertex3i"

-- glVertex3iv -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml OpenGL 2.x>.
glVertex3iv
  :: MonadIO m
  => Ptr GLint -- ^ @v@ pointing to @3@ elements of type @CoordI@.
  -> m ()
glVertex3iv v1 = liftIO $ dyn43 ptr_glVertex3iv v1

{-# NOINLINE ptr_glVertex3iv #-}
ptr_glVertex3iv :: FunPtr (Ptr GLint -> IO ())
ptr_glVertex3iv = unsafePerformIO $ getCommand "glVertex3iv"

-- glVertex3s ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml OpenGL 2.x>. The vector equivalent of this command is 'glVertex3sv'.
glVertex3s
  :: MonadIO m
  => GLshort -- ^ @x@ of type @CoordS@.
  -> GLshort -- ^ @y@ of type @CoordS@.
  -> GLshort -- ^ @z@ of type @CoordS@.
  -> m ()
glVertex3s v1 v2 v3 = liftIO $ dyn44 ptr_glVertex3s v1 v2 v3

{-# NOINLINE ptr_glVertex3s #-}
ptr_glVertex3s :: FunPtr (GLshort -> GLshort -> GLshort -> IO ())
ptr_glVertex3s = unsafePerformIO $ getCommand "glVertex3s"

-- glVertex3sv -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml OpenGL 2.x>.
glVertex3sv
  :: MonadIO m
  => Ptr GLshort -- ^ @v@ pointing to @3@ elements of type @CoordS@.
  -> m ()
glVertex3sv v1 = liftIO $ dyn45 ptr_glVertex3sv v1

{-# NOINLINE ptr_glVertex3sv #-}
ptr_glVertex3sv :: FunPtr (Ptr GLshort -> IO ())
ptr_glVertex3sv = unsafePerformIO $ getCommand "glVertex3sv"

-- glVertex3xOES ---------------------------------------------------------------

glVertex3xOES
  :: MonadIO m
  => GLfixed -- ^ @x@.
  -> GLfixed -- ^ @y@.
  -> m ()
glVertex3xOES v1 v2 = liftIO $ dyn224 ptr_glVertex3xOES v1 v2

{-# NOINLINE ptr_glVertex3xOES #-}
ptr_glVertex3xOES :: FunPtr (GLfixed -> GLfixed -> IO ())
ptr_glVertex3xOES = unsafePerformIO $ getCommand "glVertex3xOES"

-- glVertex3xvOES --------------------------------------------------------------

glVertex3xvOES
  :: MonadIO m
  => Ptr GLfixed -- ^ @coords@ pointing to @3@ elements of type @GLfixed@.
  -> m ()
glVertex3xvOES v1 = liftIO $ dyn107 ptr_glVertex3xvOES v1

{-# NOINLINE ptr_glVertex3xvOES #-}
ptr_glVertex3xvOES :: FunPtr (Ptr GLfixed -> IO ())
ptr_glVertex3xvOES = unsafePerformIO $ getCommand "glVertex3xvOES"

-- glVertex4bOES ---------------------------------------------------------------

glVertex4bOES
  :: MonadIO m
  => GLbyte -- ^ @x@.
  -> GLbyte -- ^ @y@.
  -> GLbyte -- ^ @z@.
  -> GLbyte -- ^ @w@.
  -> m ()
glVertex4bOES v1 v2 v3 v4 = liftIO $ dyn108 ptr_glVertex4bOES v1 v2 v3 v4

{-# NOINLINE ptr_glVertex4bOES #-}
ptr_glVertex4bOES :: FunPtr (GLbyte -> GLbyte -> GLbyte -> GLbyte -> IO ())
ptr_glVertex4bOES = unsafePerformIO $ getCommand "glVertex4bOES"

-- glVertex4bvOES --------------------------------------------------------------

glVertex4bvOES
  :: MonadIO m
  => Ptr GLbyte -- ^ @coords@ pointing to @4@ elements of type @GLbyte@.
  -> m ()
glVertex4bvOES v1 = liftIO $ dyn37 ptr_glVertex4bvOES v1

{-# NOINLINE ptr_glVertex4bvOES #-}
ptr_glVertex4bvOES :: FunPtr (Ptr GLbyte -> IO ())
ptr_glVertex4bvOES = unsafePerformIO $ getCommand "glVertex4bvOES"

-- glVertex4d ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml OpenGL 2.x>. The vector equivalent of this command is 'glVertex4dv'.
glVertex4d
  :: MonadIO m
  => GLdouble -- ^ @x@ of type @CoordD@.
  -> GLdouble -- ^ @y@ of type @CoordD@.
  -> GLdouble -- ^ @z@ of type @CoordD@.
  -> GLdouble -- ^ @w@ of type @CoordD@.
  -> m ()
glVertex4d v1 v2 v3 v4 = liftIO $ dyn109 ptr_glVertex4d v1 v2 v3 v4

{-# NOINLINE ptr_glVertex4d #-}
ptr_glVertex4d :: FunPtr (GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glVertex4d = unsafePerformIO $ getCommand "glVertex4d"

-- glVertex4dv -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml OpenGL 2.x>.
glVertex4dv
  :: MonadIO m
  => Ptr GLdouble -- ^ @v@ pointing to @4@ elements of type @CoordD@.
  -> m ()
glVertex4dv v1 = liftIO $ dyn39 ptr_glVertex4dv v1

{-# NOINLINE ptr_glVertex4dv #-}
ptr_glVertex4dv :: FunPtr (Ptr GLdouble -> IO ())
ptr_glVertex4dv = unsafePerformIO $ getCommand "glVertex4dv"

-- glVertex4f ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml OpenGL 2.x>. The vector equivalent of this command is 'glVertex4fv'.
glVertex4f
  :: MonadIO m
  => GLfloat -- ^ @x@ of type @CoordF@.
  -> GLfloat -- ^ @y@ of type @CoordF@.
  -> GLfloat -- ^ @z@ of type @CoordF@.
  -> GLfloat -- ^ @w@ of type @CoordF@.
  -> m ()
glVertex4f v1 v2 v3 v4 = liftIO $ dyn49 ptr_glVertex4f v1 v2 v3 v4

{-# NOINLINE ptr_glVertex4f #-}
ptr_glVertex4f :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glVertex4f = unsafePerformIO $ getCommand "glVertex4f"

-- glVertex4fv -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml OpenGL 2.x>.
glVertex4fv
  :: MonadIO m
  => Ptr GLfloat -- ^ @v@ pointing to @4@ elements of type @CoordF@.
  -> m ()
glVertex4fv v1 = liftIO $ dyn41 ptr_glVertex4fv v1

{-# NOINLINE ptr_glVertex4fv #-}
ptr_glVertex4fv :: FunPtr (Ptr GLfloat -> IO ())
ptr_glVertex4fv = unsafePerformIO $ getCommand "glVertex4fv"

-- glVertex4hNV ----------------------------------------------------------------

-- | The vector equivalent of this command is 'glVertex4hvNV'.
glVertex4hNV
  :: MonadIO m
  => GLhalfNV -- ^ @x@ of type @Half16NV@.
  -> GLhalfNV -- ^ @y@ of type @Half16NV@.
  -> GLhalfNV -- ^ @z@ of type @Half16NV@.
  -> GLhalfNV -- ^ @w@ of type @Half16NV@.
  -> m ()
glVertex4hNV v1 v2 v3 v4 = liftIO $ dyn112 ptr_glVertex4hNV v1 v2 v3 v4

{-# NOINLINE ptr_glVertex4hNV #-}
ptr_glVertex4hNV :: FunPtr (GLhalfNV -> GLhalfNV -> GLhalfNV -> GLhalfNV -> IO ())
ptr_glVertex4hNV = unsafePerformIO $ getCommand "glVertex4hNV"

-- glVertex4hvNV ---------------------------------------------------------------

glVertex4hvNV
  :: MonadIO m
  => Ptr GLhalfNV -- ^ @v@ pointing to @4@ elements of type @Half16NV@.
  -> m ()
glVertex4hvNV v1 = liftIO $ dyn99 ptr_glVertex4hvNV v1

{-# NOINLINE ptr_glVertex4hvNV #-}
ptr_glVertex4hvNV :: FunPtr (Ptr GLhalfNV -> IO ())
ptr_glVertex4hvNV = unsafePerformIO $ getCommand "glVertex4hvNV"

-- glVertex4i ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml OpenGL 2.x>. The vector equivalent of this command is 'glVertex4iv'.
glVertex4i
  :: MonadIO m
  => GLint -- ^ @x@ of type @CoordI@.
  -> GLint -- ^ @y@ of type @CoordI@.
  -> GLint -- ^ @z@ of type @CoordI@.
  -> GLint -- ^ @w@ of type @CoordI@.
  -> m ()
glVertex4i v1 v2 v3 v4 = liftIO $ dyn76 ptr_glVertex4i v1 v2 v3 v4

{-# NOINLINE ptr_glVertex4i #-}
ptr_glVertex4i :: FunPtr (GLint -> GLint -> GLint -> GLint -> IO ())
ptr_glVertex4i = unsafePerformIO $ getCommand "glVertex4i"

-- glVertex4iv -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml OpenGL 2.x>.
glVertex4iv
  :: MonadIO m
  => Ptr GLint -- ^ @v@ pointing to @4@ elements of type @CoordI@.
  -> m ()
glVertex4iv v1 = liftIO $ dyn43 ptr_glVertex4iv v1

{-# NOINLINE ptr_glVertex4iv #-}
ptr_glVertex4iv :: FunPtr (Ptr GLint -> IO ())
ptr_glVertex4iv = unsafePerformIO $ getCommand "glVertex4iv"

-- glVertex4s ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml OpenGL 2.x>. The vector equivalent of this command is 'glVertex4sv'.
glVertex4s
  :: MonadIO m
  => GLshort -- ^ @x@ of type @CoordS@.
  -> GLshort -- ^ @y@ of type @CoordS@.
  -> GLshort -- ^ @z@ of type @CoordS@.
  -> GLshort -- ^ @w@ of type @CoordS@.
  -> m ()
glVertex4s v1 v2 v3 v4 = liftIO $ dyn113 ptr_glVertex4s v1 v2 v3 v4

{-# NOINLINE ptr_glVertex4s #-}
ptr_glVertex4s :: FunPtr (GLshort -> GLshort -> GLshort -> GLshort -> IO ())
ptr_glVertex4s = unsafePerformIO $ getCommand "glVertex4s"

-- glVertex4sv -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml OpenGL 2.x>.
glVertex4sv
  :: MonadIO m
  => Ptr GLshort -- ^ @v@ pointing to @4@ elements of type @CoordS@.
  -> m ()
glVertex4sv v1 = liftIO $ dyn45 ptr_glVertex4sv v1

{-# NOINLINE ptr_glVertex4sv #-}
ptr_glVertex4sv :: FunPtr (Ptr GLshort -> IO ())
ptr_glVertex4sv = unsafePerformIO $ getCommand "glVertex4sv"

-- glVertex4xOES ---------------------------------------------------------------

glVertex4xOES
  :: MonadIO m
  => GLfixed -- ^ @x@.
  -> GLfixed -- ^ @y@.
  -> GLfixed -- ^ @z@.
  -> m ()
glVertex4xOES v1 v2 v3 = liftIO $ dyn106 ptr_glVertex4xOES v1 v2 v3

{-# NOINLINE ptr_glVertex4xOES #-}
ptr_glVertex4xOES :: FunPtr (GLfixed -> GLfixed -> GLfixed -> IO ())
ptr_glVertex4xOES = unsafePerformIO $ getCommand "glVertex4xOES"

-- glVertex4xvOES --------------------------------------------------------------

glVertex4xvOES
  :: MonadIO m
  => Ptr GLfixed -- ^ @coords@ pointing to @4@ elements of type @GLfixed@.
  -> m ()
glVertex4xvOES v1 = liftIO $ dyn107 ptr_glVertex4xvOES v1

{-# NOINLINE ptr_glVertex4xvOES #-}
ptr_glVertex4xvOES :: FunPtr (Ptr GLfixed -> IO ())
ptr_glVertex4xvOES = unsafePerformIO $ getCommand "glVertex4xvOES"

-- glVertexArrayAttribBinding --------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glVertexAttribBinding.xhtml OpenGL 4.x>.
glVertexArrayAttribBinding
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @attribindex@.
  -> GLuint -- ^ @bindingindex@.
  -> m ()
glVertexArrayAttribBinding v1 v2 v3 = liftIO $ dyn102 ptr_glVertexArrayAttribBinding v1 v2 v3

{-# NOINLINE ptr_glVertexArrayAttribBinding #-}
ptr_glVertexArrayAttribBinding :: FunPtr (GLuint -> GLuint -> GLuint -> IO ())
ptr_glVertexArrayAttribBinding = unsafePerformIO $ getCommand "glVertexArrayAttribBinding"

-- glVertexArrayAttribFormat ---------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glVertexAttribFormat.xhtml OpenGL 4.x>.
glVertexArrayAttribFormat
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @attribindex@.
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@.
  -> GLboolean -- ^ @normalized@.
  -> GLuint -- ^ @relativeoffset@.
  -> m ()
glVertexArrayAttribFormat v1 v2 v3 v4 v5 v6 = liftIO $ dyn831 ptr_glVertexArrayAttribFormat v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glVertexArrayAttribFormat #-}
ptr_glVertexArrayAttribFormat :: FunPtr (GLuint -> GLuint -> GLint -> GLenum -> GLboolean -> GLuint -> IO ())
ptr_glVertexArrayAttribFormat = unsafePerformIO $ getCommand "glVertexArrayAttribFormat"

-- glVertexArrayAttribIFormat --------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glVertexAttribFormat.xhtml OpenGL 4.x>.
glVertexArrayAttribIFormat
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @attribindex@.
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@.
  -> GLuint -- ^ @relativeoffset@.
  -> m ()
glVertexArrayAttribIFormat v1 v2 v3 v4 v5 = liftIO $ dyn832 ptr_glVertexArrayAttribIFormat v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexArrayAttribIFormat #-}
ptr_glVertexArrayAttribIFormat :: FunPtr (GLuint -> GLuint -> GLint -> GLenum -> GLuint -> IO ())
ptr_glVertexArrayAttribIFormat = unsafePerformIO $ getCommand "glVertexArrayAttribIFormat"

-- glVertexArrayAttribLFormat --------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glVertexAttribFormat.xhtml OpenGL 4.x>.
glVertexArrayAttribLFormat
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @attribindex@.
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@.
  -> GLuint -- ^ @relativeoffset@.
  -> m ()
glVertexArrayAttribLFormat v1 v2 v3 v4 v5 = liftIO $ dyn832 ptr_glVertexArrayAttribLFormat v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexArrayAttribLFormat #-}
ptr_glVertexArrayAttribLFormat :: FunPtr (GLuint -> GLuint -> GLint -> GLenum -> GLuint -> IO ())
ptr_glVertexArrayAttribLFormat = unsafePerformIO $ getCommand "glVertexArrayAttribLFormat"

-- glVertexArrayBindVertexBufferEXT --------------------------------------------

glVertexArrayBindVertexBufferEXT
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @bindingindex@.
  -> GLuint -- ^ @buffer@.
  -> GLintptr -- ^ @offset@ of type @BufferOffset@.
  -> GLsizei -- ^ @stride@.
  -> m ()
glVertexArrayBindVertexBufferEXT v1 v2 v3 v4 v5 = liftIO $ dyn833 ptr_glVertexArrayBindVertexBufferEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexArrayBindVertexBufferEXT #-}
ptr_glVertexArrayBindVertexBufferEXT :: FunPtr (GLuint -> GLuint -> GLuint -> GLintptr -> GLsizei -> IO ())
ptr_glVertexArrayBindVertexBufferEXT = unsafePerformIO $ getCommand "glVertexArrayBindVertexBufferEXT"

-- glVertexArrayBindingDivisor -------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glVertexBindingDivisor.xhtml OpenGL 4.x>.
glVertexArrayBindingDivisor
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @bindingindex@.
  -> GLuint -- ^ @divisor@.
  -> m ()
glVertexArrayBindingDivisor v1 v2 v3 = liftIO $ dyn102 ptr_glVertexArrayBindingDivisor v1 v2 v3

{-# NOINLINE ptr_glVertexArrayBindingDivisor #-}
ptr_glVertexArrayBindingDivisor :: FunPtr (GLuint -> GLuint -> GLuint -> IO ())
ptr_glVertexArrayBindingDivisor = unsafePerformIO $ getCommand "glVertexArrayBindingDivisor"

-- glVertexArrayColorOffsetEXT -------------------------------------------------

glVertexArrayColorOffsetEXT
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @buffer@.
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [ColorPointerType](Graphics-GL-Groups.html#ColorPointerType).
  -> GLsizei -- ^ @stride@.
  -> GLintptr -- ^ @offset@.
  -> m ()
glVertexArrayColorOffsetEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn834 ptr_glVertexArrayColorOffsetEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glVertexArrayColorOffsetEXT #-}
ptr_glVertexArrayColorOffsetEXT :: FunPtr (GLuint -> GLuint -> GLint -> GLenum -> GLsizei -> GLintptr -> IO ())
ptr_glVertexArrayColorOffsetEXT = unsafePerformIO $ getCommand "glVertexArrayColorOffsetEXT"

-- glVertexArrayEdgeFlagOffsetEXT ----------------------------------------------

glVertexArrayEdgeFlagOffsetEXT
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @buffer@.
  -> GLsizei -- ^ @stride@.
  -> GLintptr -- ^ @offset@.
  -> m ()
glVertexArrayEdgeFlagOffsetEXT v1 v2 v3 v4 = liftIO $ dyn835 ptr_glVertexArrayEdgeFlagOffsetEXT v1 v2 v3 v4

{-# NOINLINE ptr_glVertexArrayEdgeFlagOffsetEXT #-}
ptr_glVertexArrayEdgeFlagOffsetEXT :: FunPtr (GLuint -> GLuint -> GLsizei -> GLintptr -> IO ())
ptr_glVertexArrayEdgeFlagOffsetEXT = unsafePerformIO $ getCommand "glVertexArrayEdgeFlagOffsetEXT"

-- glVertexArrayElementBuffer --------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glVertexArrayElementBuffer.xhtml OpenGL 4.x>.
glVertexArrayElementBuffer
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @buffer@.
  -> m ()
glVertexArrayElementBuffer v1 v2 = liftIO $ dyn3 ptr_glVertexArrayElementBuffer v1 v2

{-# NOINLINE ptr_glVertexArrayElementBuffer #-}
ptr_glVertexArrayElementBuffer :: FunPtr (GLuint -> GLuint -> IO ())
ptr_glVertexArrayElementBuffer = unsafePerformIO $ getCommand "glVertexArrayElementBuffer"

-- glVertexArrayFogCoordOffsetEXT ----------------------------------------------

glVertexArrayFogCoordOffsetEXT
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @buffer@.
  -> GLenum -- ^ @type@ of type [FogCoordinatePointerType](Graphics-GL-Groups.html#FogCoordinatePointerType).
  -> GLsizei -- ^ @stride@.
  -> GLintptr -- ^ @offset@.
  -> m ()
glVertexArrayFogCoordOffsetEXT v1 v2 v3 v4 v5 = liftIO $ dyn836 ptr_glVertexArrayFogCoordOffsetEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexArrayFogCoordOffsetEXT #-}
ptr_glVertexArrayFogCoordOffsetEXT :: FunPtr (GLuint -> GLuint -> GLenum -> GLsizei -> GLintptr -> IO ())
ptr_glVertexArrayFogCoordOffsetEXT = unsafePerformIO $ getCommand "glVertexArrayFogCoordOffsetEXT"

-- glVertexArrayIndexOffsetEXT -------------------------------------------------

glVertexArrayIndexOffsetEXT
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @buffer@.
  -> GLenum -- ^ @type@ of type [IndexPointerType](Graphics-GL-Groups.html#IndexPointerType).
  -> GLsizei -- ^ @stride@.
  -> GLintptr -- ^ @offset@.
  -> m ()
glVertexArrayIndexOffsetEXT v1 v2 v3 v4 v5 = liftIO $ dyn836 ptr_glVertexArrayIndexOffsetEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexArrayIndexOffsetEXT #-}
ptr_glVertexArrayIndexOffsetEXT :: FunPtr (GLuint -> GLuint -> GLenum -> GLsizei -> GLintptr -> IO ())
ptr_glVertexArrayIndexOffsetEXT = unsafePerformIO $ getCommand "glVertexArrayIndexOffsetEXT"

-- glVertexArrayMultiTexCoordOffsetEXT -----------------------------------------

glVertexArrayMultiTexCoordOffsetEXT
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @buffer@.
  -> GLenum -- ^ @texunit@.
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [TexCoordPointerType](Graphics-GL-Groups.html#TexCoordPointerType).
  -> GLsizei -- ^ @stride@.
  -> GLintptr -- ^ @offset@.
  -> m ()
glVertexArrayMultiTexCoordOffsetEXT v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn837 ptr_glVertexArrayMultiTexCoordOffsetEXT v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glVertexArrayMultiTexCoordOffsetEXT #-}
ptr_glVertexArrayMultiTexCoordOffsetEXT :: FunPtr (GLuint -> GLuint -> GLenum -> GLint -> GLenum -> GLsizei -> GLintptr -> IO ())
ptr_glVertexArrayMultiTexCoordOffsetEXT = unsafePerformIO $ getCommand "glVertexArrayMultiTexCoordOffsetEXT"

-- glVertexArrayNormalOffsetEXT ------------------------------------------------

glVertexArrayNormalOffsetEXT
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @buffer@.
  -> GLenum -- ^ @type@ of type [NormalPointerType](Graphics-GL-Groups.html#NormalPointerType).
  -> GLsizei -- ^ @stride@.
  -> GLintptr -- ^ @offset@.
  -> m ()
glVertexArrayNormalOffsetEXT v1 v2 v3 v4 v5 = liftIO $ dyn836 ptr_glVertexArrayNormalOffsetEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexArrayNormalOffsetEXT #-}
ptr_glVertexArrayNormalOffsetEXT :: FunPtr (GLuint -> GLuint -> GLenum -> GLsizei -> GLintptr -> IO ())
ptr_glVertexArrayNormalOffsetEXT = unsafePerformIO $ getCommand "glVertexArrayNormalOffsetEXT"

