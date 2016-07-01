--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F37
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

module Graphics.GL.Functions.F37 (
  glLoadTransposeMatrixdARB,
  glLoadTransposeMatrixf,
  glLoadTransposeMatrixfARB,
  glLoadTransposeMatrixxOES,
  glLockArraysEXT,
  glLogicOp,
  glMakeBufferNonResidentNV,
  glMakeBufferResidentNV,
  glMakeImageHandleNonResidentARB,
  glMakeImageHandleNonResidentNV,
  glMakeImageHandleResidentARB,
  glMakeImageHandleResidentNV,
  glMakeNamedBufferNonResidentNV,
  glMakeNamedBufferResidentNV,
  glMakeTextureHandleNonResidentARB,
  glMakeTextureHandleNonResidentNV,
  glMakeTextureHandleResidentARB,
  glMakeTextureHandleResidentNV,
  glMap1d,
  glMap1f,
  glMap1xOES,
  glMap2d,
  glMap2f,
  glMap2xOES,
  glMapBuffer,
  glMapBufferARB,
  glMapBufferOES,
  glMapBufferRange,
  glMapBufferRangeEXT,
  glMapControlPointsNV,
  glMapGrid1d,
  glMapGrid1f,
  glMapGrid1xOES,
  glMapGrid2d,
  glMapGrid2f,
  glMapGrid2xOES,
  glMapNamedBuffer,
  glMapNamedBufferEXT,
  glMapNamedBufferRange,
  glMapNamedBufferRangeEXT
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

-- glLoadTransposeMatrixdARB ---------------------------------------------------

-- | This command is an alias for 'glLoadTransposeMatrixd'.
glLoadTransposeMatrixdARB
  :: MonadIO m
  => Ptr GLdouble -- ^ @m@ pointing to @16@ elements of type @GLdouble@.
  -> m ()
glLoadTransposeMatrixdARB v1 = liftIO $ dyn39 ptr_glLoadTransposeMatrixdARB v1

{-# NOINLINE ptr_glLoadTransposeMatrixdARB #-}
ptr_glLoadTransposeMatrixdARB :: FunPtr (Ptr GLdouble -> IO ())
ptr_glLoadTransposeMatrixdARB = unsafePerformIO $ getCommand "glLoadTransposeMatrixdARB"

-- glLoadTransposeMatrixf ------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glLoadTransposeMatrix.xml OpenGL 2.x>.
glLoadTransposeMatrixf
  :: MonadIO m
  => Ptr GLfloat -- ^ @m@ pointing to @16@ elements of type @GLfloat@.
  -> m ()
glLoadTransposeMatrixf v1 = liftIO $ dyn41 ptr_glLoadTransposeMatrixf v1

{-# NOINLINE ptr_glLoadTransposeMatrixf #-}
ptr_glLoadTransposeMatrixf :: FunPtr (Ptr GLfloat -> IO ())
ptr_glLoadTransposeMatrixf = unsafePerformIO $ getCommand "glLoadTransposeMatrixf"

-- glLoadTransposeMatrixfARB ---------------------------------------------------

-- | This command is an alias for 'glLoadTransposeMatrixf'.
glLoadTransposeMatrixfARB
  :: MonadIO m
  => Ptr GLfloat -- ^ @m@ pointing to @16@ elements of type @GLfloat@.
  -> m ()
glLoadTransposeMatrixfARB v1 = liftIO $ dyn41 ptr_glLoadTransposeMatrixfARB v1

{-# NOINLINE ptr_glLoadTransposeMatrixfARB #-}
ptr_glLoadTransposeMatrixfARB :: FunPtr (Ptr GLfloat -> IO ())
ptr_glLoadTransposeMatrixfARB = unsafePerformIO $ getCommand "glLoadTransposeMatrixfARB"

-- glLoadTransposeMatrixxOES ---------------------------------------------------

glLoadTransposeMatrixxOES
  :: MonadIO m
  => Ptr GLfixed -- ^ @m@ pointing to @16@ elements of type @GLfixed@.
  -> m ()
glLoadTransposeMatrixxOES v1 = liftIO $ dyn107 ptr_glLoadTransposeMatrixxOES v1

{-# NOINLINE ptr_glLoadTransposeMatrixxOES #-}
ptr_glLoadTransposeMatrixxOES :: FunPtr (Ptr GLfixed -> IO ())
ptr_glLoadTransposeMatrixxOES = unsafePerformIO $ getCommand "glLoadTransposeMatrixxOES"

-- glLockArraysEXT -------------------------------------------------------------

glLockArraysEXT
  :: MonadIO m
  => GLint -- ^ @first@.
  -> GLsizei -- ^ @count@.
  -> m ()
glLockArraysEXT v1 v2 = liftIO $ dyn490 ptr_glLockArraysEXT v1 v2

{-# NOINLINE ptr_glLockArraysEXT #-}
ptr_glLockArraysEXT :: FunPtr (GLint -> GLsizei -> IO ())
ptr_glLockArraysEXT = unsafePerformIO $ getCommand "glLockArraysEXT"

-- glLogicOp -------------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glLogicOp.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glLogicOp.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glLogicOp.xhtml OpenGL 4.x>.
glLogicOp
  :: MonadIO m
  => GLenum -- ^ @opcode@ of type [LogicOp](Graphics-GL-Groups.html#LogicOp).
  -> m ()
glLogicOp v1 = liftIO $ dyn4 ptr_glLogicOp v1

{-# NOINLINE ptr_glLogicOp #-}
ptr_glLogicOp :: FunPtr (GLenum -> IO ())
ptr_glLogicOp = unsafePerformIO $ getCommand "glLogicOp"

-- glMakeBufferNonResidentNV ---------------------------------------------------

glMakeBufferNonResidentNV
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> m ()
glMakeBufferNonResidentNV v1 = liftIO $ dyn4 ptr_glMakeBufferNonResidentNV v1

{-# NOINLINE ptr_glMakeBufferNonResidentNV #-}
ptr_glMakeBufferNonResidentNV :: FunPtr (GLenum -> IO ())
ptr_glMakeBufferNonResidentNV = unsafePerformIO $ getCommand "glMakeBufferNonResidentNV"

-- glMakeBufferResidentNV ------------------------------------------------------

glMakeBufferResidentNV
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @access@.
  -> m ()
glMakeBufferResidentNV v1 v2 = liftIO $ dyn51 ptr_glMakeBufferResidentNV v1 v2

{-# NOINLINE ptr_glMakeBufferResidentNV #-}
ptr_glMakeBufferResidentNV :: FunPtr (GLenum -> GLenum -> IO ())
ptr_glMakeBufferResidentNV = unsafePerformIO $ getCommand "glMakeBufferResidentNV"

-- glMakeImageHandleNonResidentARB ---------------------------------------------

glMakeImageHandleNonResidentARB
  :: MonadIO m
  => GLuint64 -- ^ @handle@.
  -> m ()
glMakeImageHandleNonResidentARB v1 = liftIO $ dyn491 ptr_glMakeImageHandleNonResidentARB v1

{-# NOINLINE ptr_glMakeImageHandleNonResidentARB #-}
ptr_glMakeImageHandleNonResidentARB :: FunPtr (GLuint64 -> IO ())
ptr_glMakeImageHandleNonResidentARB = unsafePerformIO $ getCommand "glMakeImageHandleNonResidentARB"

-- glMakeImageHandleNonResidentNV ----------------------------------------------

glMakeImageHandleNonResidentNV
  :: MonadIO m
  => GLuint64 -- ^ @handle@.
  -> m ()
glMakeImageHandleNonResidentNV v1 = liftIO $ dyn491 ptr_glMakeImageHandleNonResidentNV v1

{-# NOINLINE ptr_glMakeImageHandleNonResidentNV #-}
ptr_glMakeImageHandleNonResidentNV :: FunPtr (GLuint64 -> IO ())
ptr_glMakeImageHandleNonResidentNV = unsafePerformIO $ getCommand "glMakeImageHandleNonResidentNV"

-- glMakeImageHandleResidentARB ------------------------------------------------

glMakeImageHandleResidentARB
  :: MonadIO m
  => GLuint64 -- ^ @handle@.
  -> GLenum -- ^ @access@.
  -> m ()
glMakeImageHandleResidentARB v1 v2 = liftIO $ dyn492 ptr_glMakeImageHandleResidentARB v1 v2

{-# NOINLINE ptr_glMakeImageHandleResidentARB #-}
ptr_glMakeImageHandleResidentARB :: FunPtr (GLuint64 -> GLenum -> IO ())
ptr_glMakeImageHandleResidentARB = unsafePerformIO $ getCommand "glMakeImageHandleResidentARB"

-- glMakeImageHandleResidentNV -------------------------------------------------

glMakeImageHandleResidentNV
  :: MonadIO m
  => GLuint64 -- ^ @handle@.
  -> GLenum -- ^ @access@.
  -> m ()
glMakeImageHandleResidentNV v1 v2 = liftIO $ dyn492 ptr_glMakeImageHandleResidentNV v1 v2

{-# NOINLINE ptr_glMakeImageHandleResidentNV #-}
ptr_glMakeImageHandleResidentNV :: FunPtr (GLuint64 -> GLenum -> IO ())
ptr_glMakeImageHandleResidentNV = unsafePerformIO $ getCommand "glMakeImageHandleResidentNV"

-- glMakeNamedBufferNonResidentNV ----------------------------------------------

glMakeNamedBufferNonResidentNV
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> m ()
glMakeNamedBufferNonResidentNV v1 = liftIO $ dyn2 ptr_glMakeNamedBufferNonResidentNV v1

{-# NOINLINE ptr_glMakeNamedBufferNonResidentNV #-}
ptr_glMakeNamedBufferNonResidentNV :: FunPtr (GLuint -> IO ())
ptr_glMakeNamedBufferNonResidentNV = unsafePerformIO $ getCommand "glMakeNamedBufferNonResidentNV"

-- glMakeNamedBufferResidentNV -------------------------------------------------

glMakeNamedBufferResidentNV
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLenum -- ^ @access@.
  -> m ()
glMakeNamedBufferResidentNV v1 v2 = liftIO $ dyn15 ptr_glMakeNamedBufferResidentNV v1 v2

{-# NOINLINE ptr_glMakeNamedBufferResidentNV #-}
ptr_glMakeNamedBufferResidentNV :: FunPtr (GLuint -> GLenum -> IO ())
ptr_glMakeNamedBufferResidentNV = unsafePerformIO $ getCommand "glMakeNamedBufferResidentNV"

-- glMakeTextureHandleNonResidentARB -------------------------------------------

glMakeTextureHandleNonResidentARB
  :: MonadIO m
  => GLuint64 -- ^ @handle@.
  -> m ()
glMakeTextureHandleNonResidentARB v1 = liftIO $ dyn491 ptr_glMakeTextureHandleNonResidentARB v1

{-# NOINLINE ptr_glMakeTextureHandleNonResidentARB #-}
ptr_glMakeTextureHandleNonResidentARB :: FunPtr (GLuint64 -> IO ())
ptr_glMakeTextureHandleNonResidentARB = unsafePerformIO $ getCommand "glMakeTextureHandleNonResidentARB"

-- glMakeTextureHandleNonResidentNV --------------------------------------------

glMakeTextureHandleNonResidentNV
  :: MonadIO m
  => GLuint64 -- ^ @handle@.
  -> m ()
glMakeTextureHandleNonResidentNV v1 = liftIO $ dyn491 ptr_glMakeTextureHandleNonResidentNV v1

{-# NOINLINE ptr_glMakeTextureHandleNonResidentNV #-}
ptr_glMakeTextureHandleNonResidentNV :: FunPtr (GLuint64 -> IO ())
ptr_glMakeTextureHandleNonResidentNV = unsafePerformIO $ getCommand "glMakeTextureHandleNonResidentNV"

-- glMakeTextureHandleResidentARB ----------------------------------------------

glMakeTextureHandleResidentARB
  :: MonadIO m
  => GLuint64 -- ^ @handle@.
  -> m ()
glMakeTextureHandleResidentARB v1 = liftIO $ dyn491 ptr_glMakeTextureHandleResidentARB v1

{-# NOINLINE ptr_glMakeTextureHandleResidentARB #-}
ptr_glMakeTextureHandleResidentARB :: FunPtr (GLuint64 -> IO ())
ptr_glMakeTextureHandleResidentARB = unsafePerformIO $ getCommand "glMakeTextureHandleResidentARB"

-- glMakeTextureHandleResidentNV -----------------------------------------------

glMakeTextureHandleResidentNV
  :: MonadIO m
  => GLuint64 -- ^ @handle@.
  -> m ()
glMakeTextureHandleResidentNV v1 = liftIO $ dyn491 ptr_glMakeTextureHandleResidentNV v1

{-# NOINLINE ptr_glMakeTextureHandleResidentNV #-}
ptr_glMakeTextureHandleResidentNV :: FunPtr (GLuint64 -> IO ())
ptr_glMakeTextureHandleResidentNV = unsafePerformIO $ getCommand "glMakeTextureHandleResidentNV"

-- glMap1d ---------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMap1.xml OpenGL 2.x>.
glMap1d
  :: MonadIO m
  => GLenum -- ^ @target@ of type [MapTarget](Graphics-GL-Groups.html#MapTarget).
  -> GLdouble -- ^ @u1@ of type @CoordD@.
  -> GLdouble -- ^ @u2@ of type @CoordD@.
  -> GLint -- ^ @stride@.
  -> GLint -- ^ @order@ of type @CheckedInt32@.
  -> Ptr GLdouble -- ^ @points@ pointing to @COMPSIZE(target,stride,order)@ elements of type @CoordD@.
  -> m ()
glMap1d v1 v2 v3 v4 v5 v6 = liftIO $ dyn493 ptr_glMap1d v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glMap1d #-}
ptr_glMap1d :: FunPtr (GLenum -> GLdouble -> GLdouble -> GLint -> GLint -> Ptr GLdouble -> IO ())
ptr_glMap1d = unsafePerformIO $ getCommand "glMap1d"

-- glMap1f ---------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMap1.xml OpenGL 2.x>.
glMap1f
  :: MonadIO m
  => GLenum -- ^ @target@ of type [MapTarget](Graphics-GL-Groups.html#MapTarget).
  -> GLfloat -- ^ @u1@ of type @CoordF@.
  -> GLfloat -- ^ @u2@ of type @CoordF@.
  -> GLint -- ^ @stride@.
  -> GLint -- ^ @order@ of type @CheckedInt32@.
  -> Ptr GLfloat -- ^ @points@ pointing to @COMPSIZE(target,stride,order)@ elements of type @CoordF@.
  -> m ()
glMap1f v1 v2 v3 v4 v5 v6 = liftIO $ dyn494 ptr_glMap1f v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glMap1f #-}
ptr_glMap1f :: FunPtr (GLenum -> GLfloat -> GLfloat -> GLint -> GLint -> Ptr GLfloat -> IO ())
ptr_glMap1f = unsafePerformIO $ getCommand "glMap1f"

-- glMap1xOES ------------------------------------------------------------------

glMap1xOES
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLfixed -- ^ @u1@.
  -> GLfixed -- ^ @u2@.
  -> GLint -- ^ @stride@.
  -> GLint -- ^ @order@.
  -> GLfixed -- ^ @points@.
  -> m ()
glMap1xOES v1 v2 v3 v4 v5 v6 = liftIO $ dyn495 ptr_glMap1xOES v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glMap1xOES #-}
ptr_glMap1xOES :: FunPtr (GLenum -> GLfixed -> GLfixed -> GLint -> GLint -> GLfixed -> IO ())
ptr_glMap1xOES = unsafePerformIO $ getCommand "glMap1xOES"

-- glMap2d ---------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMap2.xml OpenGL 2.x>.
glMap2d
  :: MonadIO m
  => GLenum -- ^ @target@ of type [MapTarget](Graphics-GL-Groups.html#MapTarget).
  -> GLdouble -- ^ @u1@ of type @CoordD@.
  -> GLdouble -- ^ @u2@ of type @CoordD@.
  -> GLint -- ^ @ustride@.
  -> GLint -- ^ @uorder@ of type @CheckedInt32@.
  -> GLdouble -- ^ @v1@ of type @CoordD@.
  -> GLdouble -- ^ @v2@ of type @CoordD@.
  -> GLint -- ^ @vstride@.
  -> GLint -- ^ @vorder@ of type @CheckedInt32@.
  -> Ptr GLdouble -- ^ @points@ pointing to @COMPSIZE(target,ustride,uorder,vstride,vorder)@ elements of type @CoordD@.
  -> m ()
glMap2d v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO $ dyn496 ptr_glMap2d v1 v2 v3 v4 v5 v6 v7 v8 v9 v10

{-# NOINLINE ptr_glMap2d #-}
ptr_glMap2d :: FunPtr (GLenum -> GLdouble -> GLdouble -> GLint -> GLint -> GLdouble -> GLdouble -> GLint -> GLint -> Ptr GLdouble -> IO ())
ptr_glMap2d = unsafePerformIO $ getCommand "glMap2d"

-- glMap2f ---------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMap2.xml OpenGL 2.x>.
glMap2f
  :: MonadIO m
  => GLenum -- ^ @target@ of type [MapTarget](Graphics-GL-Groups.html#MapTarget).
  -> GLfloat -- ^ @u1@ of type @CoordF@.
  -> GLfloat -- ^ @u2@ of type @CoordF@.
  -> GLint -- ^ @ustride@.
  -> GLint -- ^ @uorder@ of type @CheckedInt32@.
  -> GLfloat -- ^ @v1@ of type @CoordF@.
  -> GLfloat -- ^ @v2@ of type @CoordF@.
  -> GLint -- ^ @vstride@.
  -> GLint -- ^ @vorder@ of type @CheckedInt32@.
  -> Ptr GLfloat -- ^ @points@ pointing to @COMPSIZE(target,ustride,uorder,vstride,vorder)@ elements of type @CoordF@.
  -> m ()
glMap2f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO $ dyn497 ptr_glMap2f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10

{-# NOINLINE ptr_glMap2f #-}
ptr_glMap2f :: FunPtr (GLenum -> GLfloat -> GLfloat -> GLint -> GLint -> GLfloat -> GLfloat -> GLint -> GLint -> Ptr GLfloat -> IO ())
ptr_glMap2f = unsafePerformIO $ getCommand "glMap2f"

-- glMap2xOES ------------------------------------------------------------------

glMap2xOES
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLfixed -- ^ @u1@.
  -> GLfixed -- ^ @u2@.
  -> GLint -- ^ @ustride@.
  -> GLint -- ^ @uorder@.
  -> GLfixed -- ^ @v1@.
  -> GLfixed -- ^ @v2@.
  -> GLint -- ^ @vstride@.
  -> GLint -- ^ @vorder@.
  -> GLfixed -- ^ @points@.
  -> m ()
glMap2xOES v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO $ dyn498 ptr_glMap2xOES v1 v2 v3 v4 v5 v6 v7 v8 v9 v10

{-# NOINLINE ptr_glMap2xOES #-}
ptr_glMap2xOES :: FunPtr (GLenum -> GLfixed -> GLfixed -> GLint -> GLint -> GLfixed -> GLfixed -> GLint -> GLint -> GLfixed -> IO ())
ptr_glMap2xOES = unsafePerformIO $ getCommand "glMap2xOES"

-- glMapBuffer -----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glMapBuffer.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glMapBuffer.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glMapBuffer.xhtml OpenGL 4.x>.
glMapBuffer
  :: MonadIO m
  => GLenum -- ^ @target@ of type @BufferTargetARB@.
  -> GLenum -- ^ @access@ of type @BufferAccessARB@.
  -> m (Ptr a)
glMapBuffer v1 v2 = liftIO $ dyn499 ptr_glMapBuffer v1 v2

{-# NOINLINE ptr_glMapBuffer #-}
ptr_glMapBuffer :: FunPtr (GLenum -> GLenum -> IO (Ptr a))
ptr_glMapBuffer = unsafePerformIO $ getCommand "glMapBuffer"

-- glMapBufferARB --------------------------------------------------------------

-- | This command is an alias for 'glMapBuffer'.
glMapBufferARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @BufferTargetARB@.
  -> GLenum -- ^ @access@ of type @BufferAccessARB@.
  -> m (Ptr a)
glMapBufferARB v1 v2 = liftIO $ dyn499 ptr_glMapBufferARB v1 v2

{-# NOINLINE ptr_glMapBufferARB #-}
ptr_glMapBufferARB :: FunPtr (GLenum -> GLenum -> IO (Ptr a))
ptr_glMapBufferARB = unsafePerformIO $ getCommand "glMapBufferARB"

-- glMapBufferOES --------------------------------------------------------------

-- | This command is an alias for 'glMapBuffer'.
glMapBufferOES
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @access@.
  -> m (Ptr a)
glMapBufferOES v1 v2 = liftIO $ dyn499 ptr_glMapBufferOES v1 v2

{-# NOINLINE ptr_glMapBufferOES #-}
ptr_glMapBufferOES :: FunPtr (GLenum -> GLenum -> IO (Ptr a))
ptr_glMapBufferOES = unsafePerformIO $ getCommand "glMapBufferOES"

-- glMapBufferRange ------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glMapBufferRange.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glMapBufferRange.xhtml OpenGL 4.x>.
glMapBufferRange
  :: MonadIO m
  => GLenum -- ^ @target@ of type @BufferTargetARB@.
  -> GLintptr -- ^ @offset@ of type @BufferOffset@.
  -> GLsizeiptr -- ^ @length@ of type @BufferSize@.
  -> GLbitfield -- ^ @access@ of type @BufferAccessMask@.
  -> m (Ptr a)
glMapBufferRange v1 v2 v3 v4 = liftIO $ dyn500 ptr_glMapBufferRange v1 v2 v3 v4

{-# NOINLINE ptr_glMapBufferRange #-}
ptr_glMapBufferRange :: FunPtr (GLenum -> GLintptr -> GLsizeiptr -> GLbitfield -> IO (Ptr a))
ptr_glMapBufferRange = unsafePerformIO $ getCommand "glMapBufferRange"

-- glMapBufferRangeEXT ---------------------------------------------------------

-- | This command is an alias for 'glMapBufferRange'.
glMapBufferRangeEXT
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLintptr -- ^ @offset@.
  -> GLsizeiptr -- ^ @length@.
  -> GLbitfield -- ^ @access@.
  -> m (Ptr a)
glMapBufferRangeEXT v1 v2 v3 v4 = liftIO $ dyn500 ptr_glMapBufferRangeEXT v1 v2 v3 v4

{-# NOINLINE ptr_glMapBufferRangeEXT #-}
ptr_glMapBufferRangeEXT :: FunPtr (GLenum -> GLintptr -> GLsizeiptr -> GLbitfield -> IO (Ptr a))
ptr_glMapBufferRangeEXT = unsafePerformIO $ getCommand "glMapBufferRangeEXT"

-- glMapControlPointsNV --------------------------------------------------------

glMapControlPointsNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type @EvalTargetNV@.
  -> GLuint -- ^ @index@.
  -> GLenum -- ^ @type@ of type @MapTypeNV@.
  -> GLsizei -- ^ @ustride@.
  -> GLsizei -- ^ @vstride@.
  -> GLint -- ^ @uorder@ of type @CheckedInt32@.
  -> GLint -- ^ @vorder@ of type @CheckedInt32@.
  -> GLboolean -- ^ @packed@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr a -- ^ @points@ pointing to @COMPSIZE(target,uorder,vorder)@ elements of type @a@.
  -> m ()
glMapControlPointsNV v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn501 ptr_glMapControlPointsNV v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glMapControlPointsNV #-}
ptr_glMapControlPointsNV :: FunPtr (GLenum -> GLuint -> GLenum -> GLsizei -> GLsizei -> GLint -> GLint -> GLboolean -> Ptr a -> IO ())
ptr_glMapControlPointsNV = unsafePerformIO $ getCommand "glMapControlPointsNV"

-- glMapGrid1d -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMapGrid.xml OpenGL 2.x>.
glMapGrid1d
  :: MonadIO m
  => GLint -- ^ @un@.
  -> GLdouble -- ^ @u1@ of type @CoordD@.
  -> GLdouble -- ^ @u2@ of type @CoordD@.
  -> m ()
glMapGrid1d v1 v2 v3 = liftIO $ dyn502 ptr_glMapGrid1d v1 v2 v3

{-# NOINLINE ptr_glMapGrid1d #-}
ptr_glMapGrid1d :: FunPtr (GLint -> GLdouble -> GLdouble -> IO ())
ptr_glMapGrid1d = unsafePerformIO $ getCommand "glMapGrid1d"

-- glMapGrid1f -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMapGrid.xml OpenGL 2.x>.
glMapGrid1f
  :: MonadIO m
  => GLint -- ^ @un@.
  -> GLfloat -- ^ @u1@ of type @CoordF@.
  -> GLfloat -- ^ @u2@ of type @CoordF@.
  -> m ()
glMapGrid1f v1 v2 v3 = liftIO $ dyn503 ptr_glMapGrid1f v1 v2 v3

{-# NOINLINE ptr_glMapGrid1f #-}
ptr_glMapGrid1f :: FunPtr (GLint -> GLfloat -> GLfloat -> IO ())
ptr_glMapGrid1f = unsafePerformIO $ getCommand "glMapGrid1f"

-- glMapGrid1xOES --------------------------------------------------------------

glMapGrid1xOES
  :: MonadIO m
  => GLint -- ^ @n@.
  -> GLfixed -- ^ @u1@.
  -> GLfixed -- ^ @u2@.
  -> m ()
glMapGrid1xOES v1 v2 v3 = liftIO $ dyn504 ptr_glMapGrid1xOES v1 v2 v3

{-# NOINLINE ptr_glMapGrid1xOES #-}
ptr_glMapGrid1xOES :: FunPtr (GLint -> GLfixed -> GLfixed -> IO ())
ptr_glMapGrid1xOES = unsafePerformIO $ getCommand "glMapGrid1xOES"

-- glMapGrid2d -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMapGrid.xml OpenGL 2.x>.
glMapGrid2d
  :: MonadIO m
  => GLint -- ^ @un@.
  -> GLdouble -- ^ @u1@ of type @CoordD@.
  -> GLdouble -- ^ @u2@ of type @CoordD@.
  -> GLint -- ^ @vn@.
  -> GLdouble -- ^ @v1@ of type @CoordD@.
  -> GLdouble -- ^ @v2@ of type @CoordD@.
  -> m ()
glMapGrid2d v1 v2 v3 v4 v5 v6 = liftIO $ dyn505 ptr_glMapGrid2d v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glMapGrid2d #-}
ptr_glMapGrid2d :: FunPtr (GLint -> GLdouble -> GLdouble -> GLint -> GLdouble -> GLdouble -> IO ())
ptr_glMapGrid2d = unsafePerformIO $ getCommand "glMapGrid2d"

-- glMapGrid2f -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMapGrid.xml OpenGL 2.x>.
glMapGrid2f
  :: MonadIO m
  => GLint -- ^ @un@.
  -> GLfloat -- ^ @u1@ of type @CoordF@.
  -> GLfloat -- ^ @u2@ of type @CoordF@.
  -> GLint -- ^ @vn@.
  -> GLfloat -- ^ @v1@ of type @CoordF@.
  -> GLfloat -- ^ @v2@ of type @CoordF@.
  -> m ()
glMapGrid2f v1 v2 v3 v4 v5 v6 = liftIO $ dyn506 ptr_glMapGrid2f v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glMapGrid2f #-}
ptr_glMapGrid2f :: FunPtr (GLint -> GLfloat -> GLfloat -> GLint -> GLfloat -> GLfloat -> IO ())
ptr_glMapGrid2f = unsafePerformIO $ getCommand "glMapGrid2f"

-- glMapGrid2xOES --------------------------------------------------------------

glMapGrid2xOES
  :: MonadIO m
  => GLint -- ^ @n@.
  -> GLfixed -- ^ @u1@.
  -> GLfixed -- ^ @u2@.
  -> GLfixed -- ^ @v1@.
  -> GLfixed -- ^ @v2@.
  -> m ()
glMapGrid2xOES v1 v2 v3 v4 v5 = liftIO $ dyn507 ptr_glMapGrid2xOES v1 v2 v3 v4 v5

{-# NOINLINE ptr_glMapGrid2xOES #-}
ptr_glMapGrid2xOES :: FunPtr (GLint -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ())
ptr_glMapGrid2xOES = unsafePerformIO $ getCommand "glMapGrid2xOES"

-- glMapNamedBuffer ------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glMapBuffer.xhtml OpenGL 4.x>.
glMapNamedBuffer
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLenum -- ^ @access@.
  -> m (Ptr a)
glMapNamedBuffer v1 v2 = liftIO $ dyn508 ptr_glMapNamedBuffer v1 v2

{-# NOINLINE ptr_glMapNamedBuffer #-}
ptr_glMapNamedBuffer :: FunPtr (GLuint -> GLenum -> IO (Ptr a))
ptr_glMapNamedBuffer = unsafePerformIO $ getCommand "glMapNamedBuffer"

-- glMapNamedBufferEXT ---------------------------------------------------------

glMapNamedBufferEXT
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLenum -- ^ @access@ of type @VertexBufferObjectAccess@.
  -> m (Ptr a)
glMapNamedBufferEXT v1 v2 = liftIO $ dyn508 ptr_glMapNamedBufferEXT v1 v2

{-# NOINLINE ptr_glMapNamedBufferEXT #-}
ptr_glMapNamedBufferEXT :: FunPtr (GLuint -> GLenum -> IO (Ptr a))
ptr_glMapNamedBufferEXT = unsafePerformIO $ getCommand "glMapNamedBufferEXT"

-- glMapNamedBufferRange -------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glMapBufferRange.xhtml OpenGL 4.x>.
glMapNamedBufferRange
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLintptr -- ^ @offset@.
  -> GLsizeiptr -- ^ @length@ of type @BufferSize@.
  -> GLbitfield -- ^ @access@.
  -> m (Ptr a)
glMapNamedBufferRange v1 v2 v3 v4 = liftIO $ dyn509 ptr_glMapNamedBufferRange v1 v2 v3 v4

{-# NOINLINE ptr_glMapNamedBufferRange #-}
ptr_glMapNamedBufferRange :: FunPtr (GLuint -> GLintptr -> GLsizeiptr -> GLbitfield -> IO (Ptr a))
ptr_glMapNamedBufferRange = unsafePerformIO $ getCommand "glMapNamedBufferRange"

-- glMapNamedBufferRangeEXT ----------------------------------------------------

glMapNamedBufferRangeEXT
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLintptr -- ^ @offset@.
  -> GLsizeiptr -- ^ @length@.
  -> GLbitfield -- ^ @access@ of type @BufferAccessMask@.
  -> m (Ptr a)
glMapNamedBufferRangeEXT v1 v2 v3 v4 = liftIO $ dyn509 ptr_glMapNamedBufferRangeEXT v1 v2 v3 v4

{-# NOINLINE ptr_glMapNamedBufferRangeEXT #-}
ptr_glMapNamedBufferRangeEXT :: FunPtr (GLuint -> GLintptr -> GLsizeiptr -> GLbitfield -> IO (Ptr a))
ptr_glMapNamedBufferRangeEXT = unsafePerformIO $ getCommand "glMapNamedBufferRangeEXT"

