--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F74
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

module Graphics.GL.Functions.F74 (
  glVertexAttribI4iv,
  glVertexAttribI4ivEXT,
  glVertexAttribI4sv,
  glVertexAttribI4svEXT,
  glVertexAttribI4ubv,
  glVertexAttribI4ubvEXT,
  glVertexAttribI4ui,
  glVertexAttribI4uiEXT,
  glVertexAttribI4uiv,
  glVertexAttribI4uivEXT,
  glVertexAttribI4usv,
  glVertexAttribI4usvEXT,
  glVertexAttribIFormat,
  glVertexAttribIFormatNV,
  glVertexAttribIPointer,
  glVertexAttribIPointerEXT,
  glVertexAttribL1d,
  glVertexAttribL1dEXT,
  glVertexAttribL1dv,
  glVertexAttribL1dvEXT,
  glVertexAttribL1i64NV,
  glVertexAttribL1i64vNV,
  glVertexAttribL1ui64ARB,
  glVertexAttribL1ui64NV,
  glVertexAttribL1ui64vARB,
  glVertexAttribL1ui64vNV,
  glVertexAttribL2d,
  glVertexAttribL2dEXT,
  glVertexAttribL2dv,
  glVertexAttribL2dvEXT,
  glVertexAttribL2i64NV,
  glVertexAttribL2i64vNV,
  glVertexAttribL2ui64NV,
  glVertexAttribL2ui64vNV,
  glVertexAttribL3d,
  glVertexAttribL3dEXT,
  glVertexAttribL3dv,
  glVertexAttribL3dvEXT,
  glVertexAttribL3i64NV,
  glVertexAttribL3i64vNV
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

-- glVertexAttribI4iv ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttribI4iv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLint -- ^ @v@ pointing to @4@ elements of type @GLint@.
  -> m ()
glVertexAttribI4iv v1 v2 = liftIO $ dyn701 ptr_glVertexAttribI4iv v1 v2

{-# NOINLINE ptr_glVertexAttribI4iv #-}
ptr_glVertexAttribI4iv :: FunPtr (GLuint -> Ptr GLint -> IO ())
ptr_glVertexAttribI4iv = unsafePerformIO $ getCommand "glVertexAttribI4iv"

-- glVertexAttribI4ivEXT -------------------------------------------------------

-- | This command is an alias for 'glVertexAttribI4iv'.
glVertexAttribI4ivEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLint -- ^ @v@ pointing to @4@ elements of type @GLint@.
  -> m ()
glVertexAttribI4ivEXT v1 v2 = liftIO $ dyn701 ptr_glVertexAttribI4ivEXT v1 v2

{-# NOINLINE ptr_glVertexAttribI4ivEXT #-}
ptr_glVertexAttribI4ivEXT :: FunPtr (GLuint -> Ptr GLint -> IO ())
ptr_glVertexAttribI4ivEXT = unsafePerformIO $ getCommand "glVertexAttribI4ivEXT"

-- glVertexAttribI4sv ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttribI4sv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLshort -- ^ @v@ pointing to @4@ elements of type @GLshort@.
  -> m ()
glVertexAttribI4sv v1 v2 = liftIO $ dyn829 ptr_glVertexAttribI4sv v1 v2

{-# NOINLINE ptr_glVertexAttribI4sv #-}
ptr_glVertexAttribI4sv :: FunPtr (GLuint -> Ptr GLshort -> IO ())
ptr_glVertexAttribI4sv = unsafePerformIO $ getCommand "glVertexAttribI4sv"

-- glVertexAttribI4svEXT -------------------------------------------------------

-- | This command is an alias for 'glVertexAttribI4sv'.
glVertexAttribI4svEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLshort -- ^ @v@ pointing to @4@ elements of type @GLshort@.
  -> m ()
glVertexAttribI4svEXT v1 v2 = liftIO $ dyn829 ptr_glVertexAttribI4svEXT v1 v2

{-# NOINLINE ptr_glVertexAttribI4svEXT #-}
ptr_glVertexAttribI4svEXT :: FunPtr (GLuint -> Ptr GLshort -> IO ())
ptr_glVertexAttribI4svEXT = unsafePerformIO $ getCommand "glVertexAttribI4svEXT"

-- glVertexAttribI4ubv ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttribI4ubv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLubyte -- ^ @v@ pointing to @4@ elements of type @GLubyte@.
  -> m ()
glVertexAttribI4ubv v1 v2 = liftIO $ dyn376 ptr_glVertexAttribI4ubv v1 v2

{-# NOINLINE ptr_glVertexAttribI4ubv #-}
ptr_glVertexAttribI4ubv :: FunPtr (GLuint -> Ptr GLubyte -> IO ())
ptr_glVertexAttribI4ubv = unsafePerformIO $ getCommand "glVertexAttribI4ubv"

-- glVertexAttribI4ubvEXT ------------------------------------------------------

-- | This command is an alias for 'glVertexAttribI4ubv'.
glVertexAttribI4ubvEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLubyte -- ^ @v@ pointing to @4@ elements of type @GLubyte@.
  -> m ()
glVertexAttribI4ubvEXT v1 v2 = liftIO $ dyn376 ptr_glVertexAttribI4ubvEXT v1 v2

{-# NOINLINE ptr_glVertexAttribI4ubvEXT #-}
ptr_glVertexAttribI4ubvEXT :: FunPtr (GLuint -> Ptr GLubyte -> IO ())
ptr_glVertexAttribI4ubvEXT = unsafePerformIO $ getCommand "glVertexAttribI4ubvEXT"

-- glVertexAttribI4ui ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>. The vector equivalent of this command is 'glVertexAttribI4uiv'.
glVertexAttribI4ui
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLuint -- ^ @x@.
  -> GLuint -- ^ @y@.
  -> GLuint -- ^ @z@.
  -> GLuint -- ^ @w@.
  -> m ()
glVertexAttribI4ui v1 v2 v3 v4 v5 = liftIO $ dyn859 ptr_glVertexAttribI4ui v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttribI4ui #-}
ptr_glVertexAttribI4ui :: FunPtr (GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ())
ptr_glVertexAttribI4ui = unsafePerformIO $ getCommand "glVertexAttribI4ui"

-- glVertexAttribI4uiEXT -------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttribI4uivEXT'. This command is an alias for 'glVertexAttribI4ui'.
glVertexAttribI4uiEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLuint -- ^ @x@.
  -> GLuint -- ^ @y@.
  -> GLuint -- ^ @z@.
  -> GLuint -- ^ @w@.
  -> m ()
glVertexAttribI4uiEXT v1 v2 v3 v4 v5 = liftIO $ dyn859 ptr_glVertexAttribI4uiEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttribI4uiEXT #-}
ptr_glVertexAttribI4uiEXT :: FunPtr (GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ())
ptr_glVertexAttribI4uiEXT = unsafePerformIO $ getCommand "glVertexAttribI4uiEXT"

-- glVertexAttribI4uiv ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttribI4uiv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLuint -- ^ @v@ pointing to @4@ elements of type @GLuint@.
  -> m ()
glVertexAttribI4uiv v1 v2 = liftIO $ dyn194 ptr_glVertexAttribI4uiv v1 v2

{-# NOINLINE ptr_glVertexAttribI4uiv #-}
ptr_glVertexAttribI4uiv :: FunPtr (GLuint -> Ptr GLuint -> IO ())
ptr_glVertexAttribI4uiv = unsafePerformIO $ getCommand "glVertexAttribI4uiv"

-- glVertexAttribI4uivEXT ------------------------------------------------------

-- | This command is an alias for 'glVertexAttribI4uiv'.
glVertexAttribI4uivEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLuint -- ^ @v@ pointing to @4@ elements of type @GLuint@.
  -> m ()
glVertexAttribI4uivEXT v1 v2 = liftIO $ dyn194 ptr_glVertexAttribI4uivEXT v1 v2

{-# NOINLINE ptr_glVertexAttribI4uivEXT #-}
ptr_glVertexAttribI4uivEXT :: FunPtr (GLuint -> Ptr GLuint -> IO ())
ptr_glVertexAttribI4uivEXT = unsafePerformIO $ getCommand "glVertexAttribI4uivEXT"

-- glVertexAttribI4usv ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttribI4usv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLushort -- ^ @v@ pointing to @4@ elements of type @GLushort@.
  -> m ()
glVertexAttribI4usv v1 v2 = liftIO $ dyn830 ptr_glVertexAttribI4usv v1 v2

{-# NOINLINE ptr_glVertexAttribI4usv #-}
ptr_glVertexAttribI4usv :: FunPtr (GLuint -> Ptr GLushort -> IO ())
ptr_glVertexAttribI4usv = unsafePerformIO $ getCommand "glVertexAttribI4usv"

-- glVertexAttribI4usvEXT ------------------------------------------------------

-- | This command is an alias for 'glVertexAttribI4usv'.
glVertexAttribI4usvEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLushort -- ^ @v@ pointing to @4@ elements of type @GLushort@.
  -> m ()
glVertexAttribI4usvEXT v1 v2 = liftIO $ dyn830 ptr_glVertexAttribI4usvEXT v1 v2

{-# NOINLINE ptr_glVertexAttribI4usvEXT #-}
ptr_glVertexAttribI4usvEXT :: FunPtr (GLuint -> Ptr GLushort -> IO ())
ptr_glVertexAttribI4usvEXT = unsafePerformIO $ getCommand "glVertexAttribI4usvEXT"

-- glVertexAttribIFormat -------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glVertexAttribFormat.xhtml OpenGL 4.x>.
glVertexAttribIFormat
  :: MonadIO m
  => GLuint -- ^ @attribindex@.
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@.
  -> GLuint -- ^ @relativeoffset@.
  -> m ()
glVertexAttribIFormat v1 v2 v3 v4 = liftIO $ dyn860 ptr_glVertexAttribIFormat v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttribIFormat #-}
ptr_glVertexAttribIFormat :: FunPtr (GLuint -> GLint -> GLenum -> GLuint -> IO ())
ptr_glVertexAttribIFormat = unsafePerformIO $ getCommand "glVertexAttribIFormat"

-- glVertexAttribIFormatNV -----------------------------------------------------

glVertexAttribIFormatNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@.
  -> GLsizei -- ^ @stride@.
  -> m ()
glVertexAttribIFormatNV v1 v2 v3 v4 = liftIO $ dyn861 ptr_glVertexAttribIFormatNV v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttribIFormatNV #-}
ptr_glVertexAttribIFormatNV :: FunPtr (GLuint -> GLint -> GLenum -> GLsizei -> IO ())
ptr_glVertexAttribIFormatNV = unsafePerformIO $ getCommand "glVertexAttribIFormatNV"

-- glVertexAttribIPointer ------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttribPointer.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttribPointer.xhtml OpenGL 4.x>.
glVertexAttribIPointer
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type @VertexAttribEnum@.
  -> GLsizei -- ^ @stride@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(size,type,stride)@ elements of type @a@.
  -> m ()
glVertexAttribIPointer v1 v2 v3 v4 v5 = liftIO $ dyn862 ptr_glVertexAttribIPointer v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttribIPointer #-}
ptr_glVertexAttribIPointer :: FunPtr (GLuint -> GLint -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glVertexAttribIPointer = unsafePerformIO $ getCommand "glVertexAttribIPointer"

-- glVertexAttribIPointerEXT ---------------------------------------------------

-- | This command is an alias for 'glVertexAttribIPointer'.
glVertexAttribIPointerEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type @VertexAttribEnum@.
  -> GLsizei -- ^ @stride@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(size,type,stride)@ elements of type @a@.
  -> m ()
glVertexAttribIPointerEXT v1 v2 v3 v4 v5 = liftIO $ dyn862 ptr_glVertexAttribIPointerEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttribIPointerEXT #-}
ptr_glVertexAttribIPointerEXT :: FunPtr (GLuint -> GLint -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glVertexAttribIPointerEXT = unsafePerformIO $ getCommand "glVertexAttribIPointerEXT"

-- glVertexAttribL1d -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttribL1d
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLdouble -- ^ @x@.
  -> m ()
glVertexAttribL1d v1 v2 = liftIO $ dyn841 ptr_glVertexAttribL1d v1 v2

{-# NOINLINE ptr_glVertexAttribL1d #-}
ptr_glVertexAttribL1d :: FunPtr (GLuint -> GLdouble -> IO ())
ptr_glVertexAttribL1d = unsafePerformIO $ getCommand "glVertexAttribL1d"

-- glVertexAttribL1dEXT --------------------------------------------------------

-- | This command is an alias for 'glVertexAttribL1d'.
glVertexAttribL1dEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLdouble -- ^ @x@.
  -> m ()
glVertexAttribL1dEXT v1 v2 = liftIO $ dyn841 ptr_glVertexAttribL1dEXT v1 v2

{-# NOINLINE ptr_glVertexAttribL1dEXT #-}
ptr_glVertexAttribL1dEXT :: FunPtr (GLuint -> GLdouble -> IO ())
ptr_glVertexAttribL1dEXT = unsafePerformIO $ getCommand "glVertexAttribL1dEXT"

-- glVertexAttribL1dv ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttribL1dv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLdouble -- ^ @v@ pointing to @1@ element of type @GLdouble@.
  -> m ()
glVertexAttribL1dv v1 v2 = liftIO $ dyn828 ptr_glVertexAttribL1dv v1 v2

{-# NOINLINE ptr_glVertexAttribL1dv #-}
ptr_glVertexAttribL1dv :: FunPtr (GLuint -> Ptr GLdouble -> IO ())
ptr_glVertexAttribL1dv = unsafePerformIO $ getCommand "glVertexAttribL1dv"

-- glVertexAttribL1dvEXT -------------------------------------------------------

-- | This command is an alias for 'glVertexAttribL1dv'.
glVertexAttribL1dvEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLdouble -- ^ @v@ pointing to @1@ element of type @GLdouble@.
  -> m ()
glVertexAttribL1dvEXT v1 v2 = liftIO $ dyn828 ptr_glVertexAttribL1dvEXT v1 v2

{-# NOINLINE ptr_glVertexAttribL1dvEXT #-}
ptr_glVertexAttribL1dvEXT :: FunPtr (GLuint -> Ptr GLdouble -> IO ())
ptr_glVertexAttribL1dvEXT = unsafePerformIO $ getCommand "glVertexAttribL1dvEXT"

-- glVertexAttribL1i64NV -------------------------------------------------------

glVertexAttribL1i64NV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLint64EXT -- ^ @x@.
  -> m ()
glVertexAttribL1i64NV v1 v2 = liftIO $ dyn863 ptr_glVertexAttribL1i64NV v1 v2

{-# NOINLINE ptr_glVertexAttribL1i64NV #-}
ptr_glVertexAttribL1i64NV :: FunPtr (GLuint -> GLint64EXT -> IO ())
ptr_glVertexAttribL1i64NV = unsafePerformIO $ getCommand "glVertexAttribL1i64NV"

-- glVertexAttribL1i64vNV ------------------------------------------------------

glVertexAttribL1i64vNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLint64EXT -- ^ @v@ pointing to @1@ element of type @GLint64EXT@.
  -> m ()
glVertexAttribL1i64vNV v1 v2 = liftIO $ dyn864 ptr_glVertexAttribL1i64vNV v1 v2

{-# NOINLINE ptr_glVertexAttribL1i64vNV #-}
ptr_glVertexAttribL1i64vNV :: FunPtr (GLuint -> Ptr GLint64EXT -> IO ())
ptr_glVertexAttribL1i64vNV = unsafePerformIO $ getCommand "glVertexAttribL1i64vNV"

-- glVertexAttribL1ui64ARB -----------------------------------------------------

glVertexAttribL1ui64ARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLuint64EXT -- ^ @x@.
  -> m ()
glVertexAttribL1ui64ARB v1 v2 = liftIO $ dyn865 ptr_glVertexAttribL1ui64ARB v1 v2

{-# NOINLINE ptr_glVertexAttribL1ui64ARB #-}
ptr_glVertexAttribL1ui64ARB :: FunPtr (GLuint -> GLuint64EXT -> IO ())
ptr_glVertexAttribL1ui64ARB = unsafePerformIO $ getCommand "glVertexAttribL1ui64ARB"

-- glVertexAttribL1ui64NV ------------------------------------------------------

glVertexAttribL1ui64NV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLuint64EXT -- ^ @x@.
  -> m ()
glVertexAttribL1ui64NV v1 v2 = liftIO $ dyn865 ptr_glVertexAttribL1ui64NV v1 v2

{-# NOINLINE ptr_glVertexAttribL1ui64NV #-}
ptr_glVertexAttribL1ui64NV :: FunPtr (GLuint -> GLuint64EXT -> IO ())
ptr_glVertexAttribL1ui64NV = unsafePerformIO $ getCommand "glVertexAttribL1ui64NV"

-- glVertexAttribL1ui64vARB ----------------------------------------------------

glVertexAttribL1ui64vARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLuint64EXT -- ^ @v@.
  -> m ()
glVertexAttribL1ui64vARB v1 v2 = liftIO $ dyn866 ptr_glVertexAttribL1ui64vARB v1 v2

{-# NOINLINE ptr_glVertexAttribL1ui64vARB #-}
ptr_glVertexAttribL1ui64vARB :: FunPtr (GLuint -> Ptr GLuint64EXT -> IO ())
ptr_glVertexAttribL1ui64vARB = unsafePerformIO $ getCommand "glVertexAttribL1ui64vARB"

-- glVertexAttribL1ui64vNV -----------------------------------------------------

glVertexAttribL1ui64vNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLuint64EXT -- ^ @v@ pointing to @1@ element of type @GLuint64EXT@.
  -> m ()
glVertexAttribL1ui64vNV v1 v2 = liftIO $ dyn866 ptr_glVertexAttribL1ui64vNV v1 v2

{-# NOINLINE ptr_glVertexAttribL1ui64vNV #-}
ptr_glVertexAttribL1ui64vNV :: FunPtr (GLuint -> Ptr GLuint64EXT -> IO ())
ptr_glVertexAttribL1ui64vNV = unsafePerformIO $ getCommand "glVertexAttribL1ui64vNV"

-- glVertexAttribL2d -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttribL2d
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> m ()
glVertexAttribL2d v1 v2 v3 = liftIO $ dyn220 ptr_glVertexAttribL2d v1 v2 v3

{-# NOINLINE ptr_glVertexAttribL2d #-}
ptr_glVertexAttribL2d :: FunPtr (GLuint -> GLdouble -> GLdouble -> IO ())
ptr_glVertexAttribL2d = unsafePerformIO $ getCommand "glVertexAttribL2d"

-- glVertexAttribL2dEXT --------------------------------------------------------

-- | This command is an alias for 'glVertexAttribL2d'.
glVertexAttribL2dEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> m ()
glVertexAttribL2dEXT v1 v2 v3 = liftIO $ dyn220 ptr_glVertexAttribL2dEXT v1 v2 v3

{-# NOINLINE ptr_glVertexAttribL2dEXT #-}
ptr_glVertexAttribL2dEXT :: FunPtr (GLuint -> GLdouble -> GLdouble -> IO ())
ptr_glVertexAttribL2dEXT = unsafePerformIO $ getCommand "glVertexAttribL2dEXT"

-- glVertexAttribL2dv ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttribL2dv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLdouble -- ^ @v@ pointing to @2@ elements of type @GLdouble@.
  -> m ()
glVertexAttribL2dv v1 v2 = liftIO $ dyn828 ptr_glVertexAttribL2dv v1 v2

{-# NOINLINE ptr_glVertexAttribL2dv #-}
ptr_glVertexAttribL2dv :: FunPtr (GLuint -> Ptr GLdouble -> IO ())
ptr_glVertexAttribL2dv = unsafePerformIO $ getCommand "glVertexAttribL2dv"

-- glVertexAttribL2dvEXT -------------------------------------------------------

-- | This command is an alias for 'glVertexAttribL2dv'.
glVertexAttribL2dvEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLdouble -- ^ @v@ pointing to @2@ elements of type @GLdouble@.
  -> m ()
glVertexAttribL2dvEXT v1 v2 = liftIO $ dyn828 ptr_glVertexAttribL2dvEXT v1 v2

{-# NOINLINE ptr_glVertexAttribL2dvEXT #-}
ptr_glVertexAttribL2dvEXT :: FunPtr (GLuint -> Ptr GLdouble -> IO ())
ptr_glVertexAttribL2dvEXT = unsafePerformIO $ getCommand "glVertexAttribL2dvEXT"

-- glVertexAttribL2i64NV -------------------------------------------------------

glVertexAttribL2i64NV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLint64EXT -- ^ @x@.
  -> GLint64EXT -- ^ @y@.
  -> m ()
glVertexAttribL2i64NV v1 v2 v3 = liftIO $ dyn867 ptr_glVertexAttribL2i64NV v1 v2 v3

{-# NOINLINE ptr_glVertexAttribL2i64NV #-}
ptr_glVertexAttribL2i64NV :: FunPtr (GLuint -> GLint64EXT -> GLint64EXT -> IO ())
ptr_glVertexAttribL2i64NV = unsafePerformIO $ getCommand "glVertexAttribL2i64NV"

-- glVertexAttribL2i64vNV ------------------------------------------------------

glVertexAttribL2i64vNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLint64EXT -- ^ @v@ pointing to @2@ elements of type @GLint64EXT@.
  -> m ()
glVertexAttribL2i64vNV v1 v2 = liftIO $ dyn864 ptr_glVertexAttribL2i64vNV v1 v2

{-# NOINLINE ptr_glVertexAttribL2i64vNV #-}
ptr_glVertexAttribL2i64vNV :: FunPtr (GLuint -> Ptr GLint64EXT -> IO ())
ptr_glVertexAttribL2i64vNV = unsafePerformIO $ getCommand "glVertexAttribL2i64vNV"

-- glVertexAttribL2ui64NV ------------------------------------------------------

glVertexAttribL2ui64NV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLuint64EXT -- ^ @x@.
  -> GLuint64EXT -- ^ @y@.
  -> m ()
glVertexAttribL2ui64NV v1 v2 v3 = liftIO $ dyn868 ptr_glVertexAttribL2ui64NV v1 v2 v3

{-# NOINLINE ptr_glVertexAttribL2ui64NV #-}
ptr_glVertexAttribL2ui64NV :: FunPtr (GLuint -> GLuint64EXT -> GLuint64EXT -> IO ())
ptr_glVertexAttribL2ui64NV = unsafePerformIO $ getCommand "glVertexAttribL2ui64NV"

-- glVertexAttribL2ui64vNV -----------------------------------------------------

glVertexAttribL2ui64vNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLuint64EXT -- ^ @v@ pointing to @2@ elements of type @GLuint64EXT@.
  -> m ()
glVertexAttribL2ui64vNV v1 v2 = liftIO $ dyn866 ptr_glVertexAttribL2ui64vNV v1 v2

{-# NOINLINE ptr_glVertexAttribL2ui64vNV #-}
ptr_glVertexAttribL2ui64vNV :: FunPtr (GLuint -> Ptr GLuint64EXT -> IO ())
ptr_glVertexAttribL2ui64vNV = unsafePerformIO $ getCommand "glVertexAttribL2ui64vNV"

-- glVertexAttribL3d -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttribL3d
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> GLdouble -- ^ @z@.
  -> m ()
glVertexAttribL3d v1 v2 v3 v4 = liftIO $ dyn848 ptr_glVertexAttribL3d v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttribL3d #-}
ptr_glVertexAttribL3d :: FunPtr (GLuint -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glVertexAttribL3d = unsafePerformIO $ getCommand "glVertexAttribL3d"

-- glVertexAttribL3dEXT --------------------------------------------------------

-- | This command is an alias for 'glVertexAttribL3d'.
glVertexAttribL3dEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> GLdouble -- ^ @z@.
  -> m ()
glVertexAttribL3dEXT v1 v2 v3 v4 = liftIO $ dyn848 ptr_glVertexAttribL3dEXT v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttribL3dEXT #-}
ptr_glVertexAttribL3dEXT :: FunPtr (GLuint -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glVertexAttribL3dEXT = unsafePerformIO $ getCommand "glVertexAttribL3dEXT"

-- glVertexAttribL3dv ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttribL3dv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLdouble -- ^ @v@ pointing to @3@ elements of type @GLdouble@.
  -> m ()
glVertexAttribL3dv v1 v2 = liftIO $ dyn828 ptr_glVertexAttribL3dv v1 v2

{-# NOINLINE ptr_glVertexAttribL3dv #-}
ptr_glVertexAttribL3dv :: FunPtr (GLuint -> Ptr GLdouble -> IO ())
ptr_glVertexAttribL3dv = unsafePerformIO $ getCommand "glVertexAttribL3dv"

-- glVertexAttribL3dvEXT -------------------------------------------------------

-- | This command is an alias for 'glVertexAttribL3dv'.
glVertexAttribL3dvEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLdouble -- ^ @v@ pointing to @3@ elements of type @GLdouble@.
  -> m ()
glVertexAttribL3dvEXT v1 v2 = liftIO $ dyn828 ptr_glVertexAttribL3dvEXT v1 v2

{-# NOINLINE ptr_glVertexAttribL3dvEXT #-}
ptr_glVertexAttribL3dvEXT :: FunPtr (GLuint -> Ptr GLdouble -> IO ())
ptr_glVertexAttribL3dvEXT = unsafePerformIO $ getCommand "glVertexAttribL3dvEXT"

-- glVertexAttribL3i64NV -------------------------------------------------------

glVertexAttribL3i64NV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLint64EXT -- ^ @x@.
  -> GLint64EXT -- ^ @y@.
  -> GLint64EXT -- ^ @z@.
  -> m ()
glVertexAttribL3i64NV v1 v2 v3 v4 = liftIO $ dyn869 ptr_glVertexAttribL3i64NV v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttribL3i64NV #-}
ptr_glVertexAttribL3i64NV :: FunPtr (GLuint -> GLint64EXT -> GLint64EXT -> GLint64EXT -> IO ())
ptr_glVertexAttribL3i64NV = unsafePerformIO $ getCommand "glVertexAttribL3i64NV"

-- glVertexAttribL3i64vNV ------------------------------------------------------

glVertexAttribL3i64vNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLint64EXT -- ^ @v@ pointing to @3@ elements of type @GLint64EXT@.
  -> m ()
glVertexAttribL3i64vNV v1 v2 = liftIO $ dyn864 ptr_glVertexAttribL3i64vNV v1 v2

{-# NOINLINE ptr_glVertexAttribL3i64vNV #-}
ptr_glVertexAttribL3i64vNV :: FunPtr (GLuint -> Ptr GLint64EXT -> IO ())
ptr_glVertexAttribL3i64vNV = unsafePerformIO $ getCommand "glVertexAttribL3i64vNV"

