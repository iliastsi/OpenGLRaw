--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F38
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

module Graphics.GL.Functions.F38 (
  glMapObjectBufferATI,
  glMapParameterfvNV,
  glMapParameterivNV,
  glMapTexture2DINTEL,
  glMapVertexAttrib1dAPPLE,
  glMapVertexAttrib1fAPPLE,
  glMapVertexAttrib2dAPPLE,
  glMapVertexAttrib2fAPPLE,
  glMaterialf,
  glMaterialfv,
  glMateriali,
  glMaterialiv,
  glMaterialx,
  glMaterialxOES,
  glMaterialxv,
  glMaterialxvOES,
  glMatrixFrustumEXT,
  glMatrixIndexPointerARB,
  glMatrixIndexPointerOES,
  glMatrixIndexubvARB,
  glMatrixIndexuivARB,
  glMatrixIndexusvARB,
  glMatrixLoad3x2fNV,
  glMatrixLoad3x3fNV,
  glMatrixLoadIdentityEXT,
  glMatrixLoadTranspose3x3fNV,
  glMatrixLoadTransposedEXT,
  glMatrixLoadTransposefEXT,
  glMatrixLoaddEXT,
  glMatrixLoadfEXT,
  glMatrixMode,
  glMatrixMult3x2fNV,
  glMatrixMult3x3fNV,
  glMatrixMultTranspose3x3fNV,
  glMatrixMultTransposedEXT,
  glMatrixMultTransposefEXT,
  glMatrixMultdEXT,
  glMatrixMultfEXT,
  glMatrixOrthoEXT,
  glMatrixPopEXT
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

-- glMapObjectBufferATI --------------------------------------------------------

glMapObjectBufferATI
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> m (Ptr a)
glMapObjectBufferATI v1 = liftIO $ dyn510 ptr_glMapObjectBufferATI v1

{-# NOINLINE ptr_glMapObjectBufferATI #-}
ptr_glMapObjectBufferATI :: FunPtr (GLuint -> IO (Ptr a))
ptr_glMapObjectBufferATI = unsafePerformIO $ getCommand "glMapObjectBufferATI"

-- glMapParameterfvNV ----------------------------------------------------------

glMapParameterfvNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type @EvalTargetNV@.
  -> GLenum -- ^ @pname@ of type @MapParameterNV@.
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(target,pname)@ elements of type @CheckedFloat32@.
  -> m ()
glMapParameterfvNV v1 v2 v3 = liftIO $ dyn132 ptr_glMapParameterfvNV v1 v2 v3

{-# NOINLINE ptr_glMapParameterfvNV #-}
ptr_glMapParameterfvNV :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glMapParameterfvNV = unsafePerformIO $ getCommand "glMapParameterfvNV"

-- glMapParameterivNV ----------------------------------------------------------

glMapParameterivNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type @EvalTargetNV@.
  -> GLenum -- ^ @pname@ of type @MapParameterNV@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(target,pname)@ elements of type @CheckedInt32@.
  -> m ()
glMapParameterivNV v1 v2 v3 = liftIO $ dyn133 ptr_glMapParameterivNV v1 v2 v3

{-# NOINLINE ptr_glMapParameterivNV #-}
ptr_glMapParameterivNV :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glMapParameterivNV = unsafePerformIO $ getCommand "glMapParameterivNV"

-- glMapTexture2DINTEL ---------------------------------------------------------

glMapTexture2DINTEL
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLbitfield -- ^ @access@.
  -> Ptr GLint -- ^ @stride@ pointing to @1@ element of type @GLint@.
  -> Ptr GLenum -- ^ @layout@ pointing to @1@ element of type @GLenum@.
  -> m (Ptr a)
glMapTexture2DINTEL v1 v2 v3 v4 v5 = liftIO $ dyn511 ptr_glMapTexture2DINTEL v1 v2 v3 v4 v5

{-# NOINLINE ptr_glMapTexture2DINTEL #-}
ptr_glMapTexture2DINTEL :: FunPtr (GLuint -> GLint -> GLbitfield -> Ptr GLint -> Ptr GLenum -> IO (Ptr a))
ptr_glMapTexture2DINTEL = unsafePerformIO $ getCommand "glMapTexture2DINTEL"

-- glMapVertexAttrib1dAPPLE ----------------------------------------------------

glMapVertexAttrib1dAPPLE
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLuint -- ^ @size@.
  -> GLdouble -- ^ @u1@ of type @CoordD@.
  -> GLdouble -- ^ @u2@ of type @CoordD@.
  -> GLint -- ^ @stride@.
  -> GLint -- ^ @order@ of type @CheckedInt32@.
  -> Ptr GLdouble -- ^ @points@ pointing to @COMPSIZE(size,stride,order)@ elements of type @CoordD@.
  -> m ()
glMapVertexAttrib1dAPPLE v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn512 ptr_glMapVertexAttrib1dAPPLE v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glMapVertexAttrib1dAPPLE #-}
ptr_glMapVertexAttrib1dAPPLE :: FunPtr (GLuint -> GLuint -> GLdouble -> GLdouble -> GLint -> GLint -> Ptr GLdouble -> IO ())
ptr_glMapVertexAttrib1dAPPLE = unsafePerformIO $ getCommand "glMapVertexAttrib1dAPPLE"

-- glMapVertexAttrib1fAPPLE ----------------------------------------------------

glMapVertexAttrib1fAPPLE
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLuint -- ^ @size@.
  -> GLfloat -- ^ @u1@ of type @CoordF@.
  -> GLfloat -- ^ @u2@ of type @CoordF@.
  -> GLint -- ^ @stride@.
  -> GLint -- ^ @order@ of type @CheckedInt32@.
  -> Ptr GLfloat -- ^ @points@ pointing to @COMPSIZE(size,stride,order)@ elements of type @CoordF@.
  -> m ()
glMapVertexAttrib1fAPPLE v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn513 ptr_glMapVertexAttrib1fAPPLE v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glMapVertexAttrib1fAPPLE #-}
ptr_glMapVertexAttrib1fAPPLE :: FunPtr (GLuint -> GLuint -> GLfloat -> GLfloat -> GLint -> GLint -> Ptr GLfloat -> IO ())
ptr_glMapVertexAttrib1fAPPLE = unsafePerformIO $ getCommand "glMapVertexAttrib1fAPPLE"

-- glMapVertexAttrib2dAPPLE ----------------------------------------------------

glMapVertexAttrib2dAPPLE
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLuint -- ^ @size@.
  -> GLdouble -- ^ @u1@ of type @CoordD@.
  -> GLdouble -- ^ @u2@ of type @CoordD@.
  -> GLint -- ^ @ustride@.
  -> GLint -- ^ @uorder@ of type @CheckedInt32@.
  -> GLdouble -- ^ @v1@ of type @CoordD@.
  -> GLdouble -- ^ @v2@ of type @CoordD@.
  -> GLint -- ^ @vstride@.
  -> GLint -- ^ @vorder@ of type @CheckedInt32@.
  -> Ptr GLdouble -- ^ @points@ pointing to @COMPSIZE(size,ustride,uorder,vstride,vorder)@ elements of type @CoordD@.
  -> m ()
glMapVertexAttrib2dAPPLE v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 = liftIO $ dyn514 ptr_glMapVertexAttrib2dAPPLE v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11

{-# NOINLINE ptr_glMapVertexAttrib2dAPPLE #-}
ptr_glMapVertexAttrib2dAPPLE :: FunPtr (GLuint -> GLuint -> GLdouble -> GLdouble -> GLint -> GLint -> GLdouble -> GLdouble -> GLint -> GLint -> Ptr GLdouble -> IO ())
ptr_glMapVertexAttrib2dAPPLE = unsafePerformIO $ getCommand "glMapVertexAttrib2dAPPLE"

-- glMapVertexAttrib2fAPPLE ----------------------------------------------------

glMapVertexAttrib2fAPPLE
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLuint -- ^ @size@.
  -> GLfloat -- ^ @u1@ of type @CoordF@.
  -> GLfloat -- ^ @u2@ of type @CoordF@.
  -> GLint -- ^ @ustride@.
  -> GLint -- ^ @uorder@ of type @CheckedInt32@.
  -> GLfloat -- ^ @v1@ of type @CoordF@.
  -> GLfloat -- ^ @v2@ of type @CoordF@.
  -> GLint -- ^ @vstride@.
  -> GLint -- ^ @vorder@ of type @CheckedInt32@.
  -> Ptr GLfloat -- ^ @points@ pointing to @COMPSIZE(size,ustride,uorder,vstride,vorder)@ elements of type @CoordF@.
  -> m ()
glMapVertexAttrib2fAPPLE v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 = liftIO $ dyn515 ptr_glMapVertexAttrib2fAPPLE v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11

{-# NOINLINE ptr_glMapVertexAttrib2fAPPLE #-}
ptr_glMapVertexAttrib2fAPPLE :: FunPtr (GLuint -> GLuint -> GLfloat -> GLfloat -> GLint -> GLint -> GLfloat -> GLfloat -> GLint -> GLint -> Ptr GLfloat -> IO ())
ptr_glMapVertexAttrib2fAPPLE = unsafePerformIO $ getCommand "glMapVertexAttrib2fAPPLE"

-- glMaterialf -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMaterial.xml OpenGL 2.x>.
glMaterialf
  :: MonadIO m
  => GLenum -- ^ @face@ of type [MaterialFace](Graphics-GL-Groups.html#MaterialFace).
  -> GLenum -- ^ @pname@ of type [MaterialParameter](Graphics-GL-Groups.html#MaterialParameter).
  -> GLfloat -- ^ @param@ of type @CheckedFloat32@.
  -> m ()
glMaterialf v1 v2 v3 = liftIO $ dyn161 ptr_glMaterialf v1 v2 v3

{-# NOINLINE ptr_glMaterialf #-}
ptr_glMaterialf :: FunPtr (GLenum -> GLenum -> GLfloat -> IO ())
ptr_glMaterialf = unsafePerformIO $ getCommand "glMaterialf"

-- glMaterialfv ----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMaterial.xml OpenGL 2.x>.
glMaterialfv
  :: MonadIO m
  => GLenum -- ^ @face@ of type [MaterialFace](Graphics-GL-Groups.html#MaterialFace).
  -> GLenum -- ^ @pname@ of type [MaterialParameter](Graphics-GL-Groups.html#MaterialParameter).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedFloat32@.
  -> m ()
glMaterialfv v1 v2 v3 = liftIO $ dyn132 ptr_glMaterialfv v1 v2 v3

{-# NOINLINE ptr_glMaterialfv #-}
ptr_glMaterialfv :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glMaterialfv = unsafePerformIO $ getCommand "glMaterialfv"

-- glMateriali -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMaterial.xml OpenGL 2.x>.
glMateriali
  :: MonadIO m
  => GLenum -- ^ @face@ of type [MaterialFace](Graphics-GL-Groups.html#MaterialFace).
  -> GLenum -- ^ @pname@ of type [MaterialParameter](Graphics-GL-Groups.html#MaterialParameter).
  -> GLint -- ^ @param@ of type @CheckedInt32@.
  -> m ()
glMateriali v1 v2 v3 = liftIO $ dyn62 ptr_glMateriali v1 v2 v3

{-# NOINLINE ptr_glMateriali #-}
ptr_glMateriali :: FunPtr (GLenum -> GLenum -> GLint -> IO ())
ptr_glMateriali = unsafePerformIO $ getCommand "glMateriali"

-- glMaterialiv ----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMaterial.xml OpenGL 2.x>.
glMaterialiv
  :: MonadIO m
  => GLenum -- ^ @face@ of type [MaterialFace](Graphics-GL-Groups.html#MaterialFace).
  -> GLenum -- ^ @pname@ of type [MaterialParameter](Graphics-GL-Groups.html#MaterialParameter).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedInt32@.
  -> m ()
glMaterialiv v1 v2 v3 = liftIO $ dyn133 ptr_glMaterialiv v1 v2 v3

{-# NOINLINE ptr_glMaterialiv #-}
ptr_glMaterialiv :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glMaterialiv = unsafePerformIO $ getCommand "glMaterialiv"

-- glMaterialx -----------------------------------------------------------------

glMaterialx
  :: MonadIO m
  => GLenum -- ^ @face@.
  -> GLenum -- ^ @pname@.
  -> GLfixed -- ^ @param@.
  -> m ()
glMaterialx v1 v2 v3 = liftIO $ dyn162 ptr_glMaterialx v1 v2 v3

{-# NOINLINE ptr_glMaterialx #-}
ptr_glMaterialx :: FunPtr (GLenum -> GLenum -> GLfixed -> IO ())
ptr_glMaterialx = unsafePerformIO $ getCommand "glMaterialx"

-- glMaterialxOES --------------------------------------------------------------

glMaterialxOES
  :: MonadIO m
  => GLenum -- ^ @face@.
  -> GLenum -- ^ @pname@.
  -> GLfixed -- ^ @param@.
  -> m ()
glMaterialxOES v1 v2 v3 = liftIO $ dyn162 ptr_glMaterialxOES v1 v2 v3

{-# NOINLINE ptr_glMaterialxOES #-}
ptr_glMaterialxOES :: FunPtr (GLenum -> GLenum -> GLfixed -> IO ())
ptr_glMaterialxOES = unsafePerformIO $ getCommand "glMaterialxOES"

-- glMaterialxv ----------------------------------------------------------------

glMaterialxv
  :: MonadIO m
  => GLenum -- ^ @face@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLfixed -- ^ @param@ pointing to @COMPSIZE(pname)@ elements of type @GLfixed@.
  -> m ()
glMaterialxv v1 v2 v3 = liftIO $ dyn163 ptr_glMaterialxv v1 v2 v3

{-# NOINLINE ptr_glMaterialxv #-}
ptr_glMaterialxv :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
ptr_glMaterialxv = unsafePerformIO $ getCommand "glMaterialxv"

-- glMaterialxvOES -------------------------------------------------------------

glMaterialxvOES
  :: MonadIO m
  => GLenum -- ^ @face@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLfixed -- ^ @param@ pointing to @COMPSIZE(pname)@ elements of type @GLfixed@.
  -> m ()
glMaterialxvOES v1 v2 v3 = liftIO $ dyn163 ptr_glMaterialxvOES v1 v2 v3

{-# NOINLINE ptr_glMaterialxvOES #-}
ptr_glMaterialxvOES :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
ptr_glMaterialxvOES = unsafePerformIO $ getCommand "glMaterialxvOES"

-- glMatrixFrustumEXT ----------------------------------------------------------

glMatrixFrustumEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [MatrixMode](Graphics-GL-Groups.html#MatrixMode).
  -> GLdouble -- ^ @left@.
  -> GLdouble -- ^ @right@.
  -> GLdouble -- ^ @bottom@.
  -> GLdouble -- ^ @top@.
  -> GLdouble -- ^ @zNear@.
  -> GLdouble -- ^ @zFar@.
  -> m ()
glMatrixFrustumEXT v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn516 ptr_glMatrixFrustumEXT v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glMatrixFrustumEXT #-}
ptr_glMatrixFrustumEXT :: FunPtr (GLenum -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glMatrixFrustumEXT = unsafePerformIO $ getCommand "glMatrixFrustumEXT"

-- glMatrixIndexPointerARB -----------------------------------------------------

glMatrixIndexPointerARB
  :: MonadIO m
  => GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type @MatrixIndexPointerTypeARB@.
  -> GLsizei -- ^ @stride@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(size,type,stride)@ elements of type @a@.
  -> m ()
glMatrixIndexPointerARB v1 v2 v3 v4 = liftIO $ dyn126 ptr_glMatrixIndexPointerARB v1 v2 v3 v4

{-# NOINLINE ptr_glMatrixIndexPointerARB #-}
ptr_glMatrixIndexPointerARB :: FunPtr (GLint -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glMatrixIndexPointerARB = unsafePerformIO $ getCommand "glMatrixIndexPointerARB"

-- glMatrixIndexPointerOES -----------------------------------------------------

glMatrixIndexPointerOES
  :: MonadIO m
  => GLint -- ^ @size@.
  -> GLenum -- ^ @type@.
  -> GLsizei -- ^ @stride@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(size,type,stride)@ elements of type @a@.
  -> m ()
glMatrixIndexPointerOES v1 v2 v3 v4 = liftIO $ dyn126 ptr_glMatrixIndexPointerOES v1 v2 v3 v4

{-# NOINLINE ptr_glMatrixIndexPointerOES #-}
ptr_glMatrixIndexPointerOES :: FunPtr (GLint -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glMatrixIndexPointerOES = unsafePerformIO $ getCommand "glMatrixIndexPointerOES"

-- glMatrixIndexubvARB ---------------------------------------------------------

glMatrixIndexubvARB
  :: MonadIO m
  => GLint -- ^ @size@.
  -> Ptr GLubyte -- ^ @indices@ pointing to @size@ elements of type @GLubyte@.
  -> m ()
glMatrixIndexubvARB v1 v2 = liftIO $ dyn517 ptr_glMatrixIndexubvARB v1 v2

{-# NOINLINE ptr_glMatrixIndexubvARB #-}
ptr_glMatrixIndexubvARB :: FunPtr (GLint -> Ptr GLubyte -> IO ())
ptr_glMatrixIndexubvARB = unsafePerformIO $ getCommand "glMatrixIndexubvARB"

-- glMatrixIndexuivARB ---------------------------------------------------------

glMatrixIndexuivARB
  :: MonadIO m
  => GLint -- ^ @size@.
  -> Ptr GLuint -- ^ @indices@ pointing to @size@ elements of type @GLuint@.
  -> m ()
glMatrixIndexuivARB v1 v2 = liftIO $ dyn518 ptr_glMatrixIndexuivARB v1 v2

{-# NOINLINE ptr_glMatrixIndexuivARB #-}
ptr_glMatrixIndexuivARB :: FunPtr (GLint -> Ptr GLuint -> IO ())
ptr_glMatrixIndexuivARB = unsafePerformIO $ getCommand "glMatrixIndexuivARB"

-- glMatrixIndexusvARB ---------------------------------------------------------

glMatrixIndexusvARB
  :: MonadIO m
  => GLint -- ^ @size@.
  -> Ptr GLushort -- ^ @indices@ pointing to @size@ elements of type @GLushort@.
  -> m ()
glMatrixIndexusvARB v1 v2 = liftIO $ dyn519 ptr_glMatrixIndexusvARB v1 v2

{-# NOINLINE ptr_glMatrixIndexusvARB #-}
ptr_glMatrixIndexusvARB :: FunPtr (GLint -> Ptr GLushort -> IO ())
ptr_glMatrixIndexusvARB = unsafePerformIO $ getCommand "glMatrixIndexusvARB"

-- glMatrixLoad3x2fNV ----------------------------------------------------------

glMatrixLoad3x2fNV
  :: MonadIO m
  => GLenum -- ^ @matrixMode@.
  -> Ptr GLfloat -- ^ @m@.
  -> m ()
glMatrixLoad3x2fNV v1 v2 = liftIO $ dyn94 ptr_glMatrixLoad3x2fNV v1 v2

{-# NOINLINE ptr_glMatrixLoad3x2fNV #-}
ptr_glMatrixLoad3x2fNV :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glMatrixLoad3x2fNV = unsafePerformIO $ getCommand "glMatrixLoad3x2fNV"

-- glMatrixLoad3x3fNV ----------------------------------------------------------

glMatrixLoad3x3fNV
  :: MonadIO m
  => GLenum -- ^ @matrixMode@.
  -> Ptr GLfloat -- ^ @m@.
  -> m ()
glMatrixLoad3x3fNV v1 v2 = liftIO $ dyn94 ptr_glMatrixLoad3x3fNV v1 v2

{-# NOINLINE ptr_glMatrixLoad3x3fNV #-}
ptr_glMatrixLoad3x3fNV :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glMatrixLoad3x3fNV = unsafePerformIO $ getCommand "glMatrixLoad3x3fNV"

-- glMatrixLoadIdentityEXT -----------------------------------------------------

glMatrixLoadIdentityEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [MatrixMode](Graphics-GL-Groups.html#MatrixMode).
  -> m ()
glMatrixLoadIdentityEXT v1 = liftIO $ dyn4 ptr_glMatrixLoadIdentityEXT v1

{-# NOINLINE ptr_glMatrixLoadIdentityEXT #-}
ptr_glMatrixLoadIdentityEXT :: FunPtr (GLenum -> IO ())
ptr_glMatrixLoadIdentityEXT = unsafePerformIO $ getCommand "glMatrixLoadIdentityEXT"

-- glMatrixLoadTranspose3x3fNV -------------------------------------------------

glMatrixLoadTranspose3x3fNV
  :: MonadIO m
  => GLenum -- ^ @matrixMode@.
  -> Ptr GLfloat -- ^ @m@.
  -> m ()
glMatrixLoadTranspose3x3fNV v1 v2 = liftIO $ dyn94 ptr_glMatrixLoadTranspose3x3fNV v1 v2

{-# NOINLINE ptr_glMatrixLoadTranspose3x3fNV #-}
ptr_glMatrixLoadTranspose3x3fNV :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glMatrixLoadTranspose3x3fNV = unsafePerformIO $ getCommand "glMatrixLoadTranspose3x3fNV"

-- glMatrixLoadTransposedEXT ---------------------------------------------------

glMatrixLoadTransposedEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [MatrixMode](Graphics-GL-Groups.html#MatrixMode).
  -> Ptr GLdouble -- ^ @m@ pointing to @16@ elements of type @GLdouble@.
  -> m ()
glMatrixLoadTransposedEXT v1 v2 = liftIO $ dyn93 ptr_glMatrixLoadTransposedEXT v1 v2

{-# NOINLINE ptr_glMatrixLoadTransposedEXT #-}
ptr_glMatrixLoadTransposedEXT :: FunPtr (GLenum -> Ptr GLdouble -> IO ())
ptr_glMatrixLoadTransposedEXT = unsafePerformIO $ getCommand "glMatrixLoadTransposedEXT"

-- glMatrixLoadTransposefEXT ---------------------------------------------------

glMatrixLoadTransposefEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [MatrixMode](Graphics-GL-Groups.html#MatrixMode).
  -> Ptr GLfloat -- ^ @m@ pointing to @16@ elements of type @GLfloat@.
  -> m ()
glMatrixLoadTransposefEXT v1 v2 = liftIO $ dyn94 ptr_glMatrixLoadTransposefEXT v1 v2

{-# NOINLINE ptr_glMatrixLoadTransposefEXT #-}
ptr_glMatrixLoadTransposefEXT :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glMatrixLoadTransposefEXT = unsafePerformIO $ getCommand "glMatrixLoadTransposefEXT"

-- glMatrixLoaddEXT ------------------------------------------------------------

glMatrixLoaddEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [MatrixMode](Graphics-GL-Groups.html#MatrixMode).
  -> Ptr GLdouble -- ^ @m@ pointing to @16@ elements of type @GLdouble@.
  -> m ()
glMatrixLoaddEXT v1 v2 = liftIO $ dyn93 ptr_glMatrixLoaddEXT v1 v2

{-# NOINLINE ptr_glMatrixLoaddEXT #-}
ptr_glMatrixLoaddEXT :: FunPtr (GLenum -> Ptr GLdouble -> IO ())
ptr_glMatrixLoaddEXT = unsafePerformIO $ getCommand "glMatrixLoaddEXT"

-- glMatrixLoadfEXT ------------------------------------------------------------

glMatrixLoadfEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [MatrixMode](Graphics-GL-Groups.html#MatrixMode).
  -> Ptr GLfloat -- ^ @m@ pointing to @16@ elements of type @GLfloat@.
  -> m ()
glMatrixLoadfEXT v1 v2 = liftIO $ dyn94 ptr_glMatrixLoadfEXT v1 v2

{-# NOINLINE ptr_glMatrixLoadfEXT #-}
ptr_glMatrixLoadfEXT :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glMatrixLoadfEXT = unsafePerformIO $ getCommand "glMatrixLoadfEXT"

-- glMatrixMode ----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMatrixMode.xml OpenGL 2.x>.
glMatrixMode
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [MatrixMode](Graphics-GL-Groups.html#MatrixMode).
  -> m ()
glMatrixMode v1 = liftIO $ dyn4 ptr_glMatrixMode v1

{-# NOINLINE ptr_glMatrixMode #-}
ptr_glMatrixMode :: FunPtr (GLenum -> IO ())
ptr_glMatrixMode = unsafePerformIO $ getCommand "glMatrixMode"

-- glMatrixMult3x2fNV ----------------------------------------------------------

glMatrixMult3x2fNV
  :: MonadIO m
  => GLenum -- ^ @matrixMode@.
  -> Ptr GLfloat -- ^ @m@.
  -> m ()
glMatrixMult3x2fNV v1 v2 = liftIO $ dyn94 ptr_glMatrixMult3x2fNV v1 v2

{-# NOINLINE ptr_glMatrixMult3x2fNV #-}
ptr_glMatrixMult3x2fNV :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glMatrixMult3x2fNV = unsafePerformIO $ getCommand "glMatrixMult3x2fNV"

-- glMatrixMult3x3fNV ----------------------------------------------------------

glMatrixMult3x3fNV
  :: MonadIO m
  => GLenum -- ^ @matrixMode@.
  -> Ptr GLfloat -- ^ @m@.
  -> m ()
glMatrixMult3x3fNV v1 v2 = liftIO $ dyn94 ptr_glMatrixMult3x3fNV v1 v2

{-# NOINLINE ptr_glMatrixMult3x3fNV #-}
ptr_glMatrixMult3x3fNV :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glMatrixMult3x3fNV = unsafePerformIO $ getCommand "glMatrixMult3x3fNV"

-- glMatrixMultTranspose3x3fNV -------------------------------------------------

glMatrixMultTranspose3x3fNV
  :: MonadIO m
  => GLenum -- ^ @matrixMode@.
  -> Ptr GLfloat -- ^ @m@.
  -> m ()
glMatrixMultTranspose3x3fNV v1 v2 = liftIO $ dyn94 ptr_glMatrixMultTranspose3x3fNV v1 v2

{-# NOINLINE ptr_glMatrixMultTranspose3x3fNV #-}
ptr_glMatrixMultTranspose3x3fNV :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glMatrixMultTranspose3x3fNV = unsafePerformIO $ getCommand "glMatrixMultTranspose3x3fNV"

-- glMatrixMultTransposedEXT ---------------------------------------------------

glMatrixMultTransposedEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [MatrixMode](Graphics-GL-Groups.html#MatrixMode).
  -> Ptr GLdouble -- ^ @m@ pointing to @16@ elements of type @GLdouble@.
  -> m ()
glMatrixMultTransposedEXT v1 v2 = liftIO $ dyn93 ptr_glMatrixMultTransposedEXT v1 v2

{-# NOINLINE ptr_glMatrixMultTransposedEXT #-}
ptr_glMatrixMultTransposedEXT :: FunPtr (GLenum -> Ptr GLdouble -> IO ())
ptr_glMatrixMultTransposedEXT = unsafePerformIO $ getCommand "glMatrixMultTransposedEXT"

-- glMatrixMultTransposefEXT ---------------------------------------------------

glMatrixMultTransposefEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [MatrixMode](Graphics-GL-Groups.html#MatrixMode).
  -> Ptr GLfloat -- ^ @m@ pointing to @16@ elements of type @GLfloat@.
  -> m ()
glMatrixMultTransposefEXT v1 v2 = liftIO $ dyn94 ptr_glMatrixMultTransposefEXT v1 v2

{-# NOINLINE ptr_glMatrixMultTransposefEXT #-}
ptr_glMatrixMultTransposefEXT :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glMatrixMultTransposefEXT = unsafePerformIO $ getCommand "glMatrixMultTransposefEXT"

-- glMatrixMultdEXT ------------------------------------------------------------

glMatrixMultdEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [MatrixMode](Graphics-GL-Groups.html#MatrixMode).
  -> Ptr GLdouble -- ^ @m@ pointing to @16@ elements of type @GLdouble@.
  -> m ()
glMatrixMultdEXT v1 v2 = liftIO $ dyn93 ptr_glMatrixMultdEXT v1 v2

{-# NOINLINE ptr_glMatrixMultdEXT #-}
ptr_glMatrixMultdEXT :: FunPtr (GLenum -> Ptr GLdouble -> IO ())
ptr_glMatrixMultdEXT = unsafePerformIO $ getCommand "glMatrixMultdEXT"

-- glMatrixMultfEXT ------------------------------------------------------------

glMatrixMultfEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [MatrixMode](Graphics-GL-Groups.html#MatrixMode).
  -> Ptr GLfloat -- ^ @m@ pointing to @16@ elements of type @GLfloat@.
  -> m ()
glMatrixMultfEXT v1 v2 = liftIO $ dyn94 ptr_glMatrixMultfEXT v1 v2

{-# NOINLINE ptr_glMatrixMultfEXT #-}
ptr_glMatrixMultfEXT :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glMatrixMultfEXT = unsafePerformIO $ getCommand "glMatrixMultfEXT"

-- glMatrixOrthoEXT ------------------------------------------------------------

glMatrixOrthoEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [MatrixMode](Graphics-GL-Groups.html#MatrixMode).
  -> GLdouble -- ^ @left@.
  -> GLdouble -- ^ @right@.
  -> GLdouble -- ^ @bottom@.
  -> GLdouble -- ^ @top@.
  -> GLdouble -- ^ @zNear@.
  -> GLdouble -- ^ @zFar@.
  -> m ()
glMatrixOrthoEXT v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn516 ptr_glMatrixOrthoEXT v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glMatrixOrthoEXT #-}
ptr_glMatrixOrthoEXT :: FunPtr (GLenum -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glMatrixOrthoEXT = unsafePerformIO $ getCommand "glMatrixOrthoEXT"

-- glMatrixPopEXT --------------------------------------------------------------

glMatrixPopEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [MatrixMode](Graphics-GL-Groups.html#MatrixMode).
  -> m ()
glMatrixPopEXT v1 = liftIO $ dyn4 ptr_glMatrixPopEXT v1

{-# NOINLINE ptr_glMatrixPopEXT #-}
ptr_glMatrixPopEXT :: FunPtr (GLenum -> IO ())
ptr_glMatrixPopEXT = unsafePerformIO $ getCommand "glMatrixPopEXT"

