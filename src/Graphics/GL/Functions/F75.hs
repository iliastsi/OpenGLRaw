--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F75
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

module Graphics.GL.Functions.F75 (
  glVertexAttribL3ui64NV,
  glVertexAttribL3ui64vNV,
  glVertexAttribL4d,
  glVertexAttribL4dEXT,
  glVertexAttribL4dv,
  glVertexAttribL4dvEXT,
  glVertexAttribL4i64NV,
  glVertexAttribL4i64vNV,
  glVertexAttribL4ui64NV,
  glVertexAttribL4ui64vNV,
  glVertexAttribLFormat,
  glVertexAttribLFormatNV,
  glVertexAttribLPointer,
  glVertexAttribLPointerEXT,
  glVertexAttribP1ui,
  glVertexAttribP1uiv,
  glVertexAttribP2ui,
  glVertexAttribP2uiv,
  glVertexAttribP3ui,
  glVertexAttribP3uiv,
  glVertexAttribP4ui,
  glVertexAttribP4uiv,
  glVertexAttribParameteriAMD,
  glVertexAttribPointer,
  glVertexAttribPointerARB,
  glVertexAttribPointerNV,
  glVertexAttribs1dvNV,
  glVertexAttribs1fvNV,
  glVertexAttribs1hvNV,
  glVertexAttribs1svNV,
  glVertexAttribs2dvNV,
  glVertexAttribs2fvNV,
  glVertexAttribs2hvNV,
  glVertexAttribs2svNV,
  glVertexAttribs3dvNV,
  glVertexAttribs3fvNV,
  glVertexAttribs3hvNV,
  glVertexAttribs3svNV,
  glVertexAttribs4dvNV,
  glVertexAttribs4fvNV
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

-- glVertexAttribL3ui64NV ------------------------------------------------------

glVertexAttribL3ui64NV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLuint64EXT -- ^ @x@.
  -> GLuint64EXT -- ^ @y@.
  -> GLuint64EXT -- ^ @z@.
  -> m ()
glVertexAttribL3ui64NV v1 v2 v3 v4 = liftIO $ dyn870 ptr_glVertexAttribL3ui64NV v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttribL3ui64NV #-}
ptr_glVertexAttribL3ui64NV :: FunPtr (GLuint -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> IO ())
ptr_glVertexAttribL3ui64NV = unsafePerformIO $ getCommand "glVertexAttribL3ui64NV"

-- glVertexAttribL3ui64vNV -----------------------------------------------------

glVertexAttribL3ui64vNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLuint64EXT -- ^ @v@ pointing to @3@ elements of type @GLuint64EXT@.
  -> m ()
glVertexAttribL3ui64vNV v1 v2 = liftIO $ dyn866 ptr_glVertexAttribL3ui64vNV v1 v2

{-# NOINLINE ptr_glVertexAttribL3ui64vNV #-}
ptr_glVertexAttribL3ui64vNV :: FunPtr (GLuint -> Ptr GLuint64EXT -> IO ())
ptr_glVertexAttribL3ui64vNV = unsafePerformIO $ getCommand "glVertexAttribL3ui64vNV"

-- glVertexAttribL4d -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttribL4d
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> GLdouble -- ^ @z@.
  -> GLdouble -- ^ @w@.
  -> m ()
glVertexAttribL4d v1 v2 v3 v4 v5 = liftIO $ dyn852 ptr_glVertexAttribL4d v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttribL4d #-}
ptr_glVertexAttribL4d :: FunPtr (GLuint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glVertexAttribL4d = unsafePerformIO $ getCommand "glVertexAttribL4d"

-- glVertexAttribL4dEXT --------------------------------------------------------

-- | This command is an alias for 'glVertexAttribL4d'.
glVertexAttribL4dEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> GLdouble -- ^ @z@.
  -> GLdouble -- ^ @w@.
  -> m ()
glVertexAttribL4dEXT v1 v2 v3 v4 v5 = liftIO $ dyn852 ptr_glVertexAttribL4dEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttribL4dEXT #-}
ptr_glVertexAttribL4dEXT :: FunPtr (GLuint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glVertexAttribL4dEXT = unsafePerformIO $ getCommand "glVertexAttribL4dEXT"

-- glVertexAttribL4dv ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttribL4dv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLdouble -- ^ @v@ pointing to @4@ elements of type @GLdouble@.
  -> m ()
glVertexAttribL4dv v1 v2 = liftIO $ dyn828 ptr_glVertexAttribL4dv v1 v2

{-# NOINLINE ptr_glVertexAttribL4dv #-}
ptr_glVertexAttribL4dv :: FunPtr (GLuint -> Ptr GLdouble -> IO ())
ptr_glVertexAttribL4dv = unsafePerformIO $ getCommand "glVertexAttribL4dv"

-- glVertexAttribL4dvEXT -------------------------------------------------------

-- | This command is an alias for 'glVertexAttribL4dv'.
glVertexAttribL4dvEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLdouble -- ^ @v@ pointing to @4@ elements of type @GLdouble@.
  -> m ()
glVertexAttribL4dvEXT v1 v2 = liftIO $ dyn828 ptr_glVertexAttribL4dvEXT v1 v2

{-# NOINLINE ptr_glVertexAttribL4dvEXT #-}
ptr_glVertexAttribL4dvEXT :: FunPtr (GLuint -> Ptr GLdouble -> IO ())
ptr_glVertexAttribL4dvEXT = unsafePerformIO $ getCommand "glVertexAttribL4dvEXT"

-- glVertexAttribL4i64NV -------------------------------------------------------

glVertexAttribL4i64NV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLint64EXT -- ^ @x@.
  -> GLint64EXT -- ^ @y@.
  -> GLint64EXT -- ^ @z@.
  -> GLint64EXT -- ^ @w@.
  -> m ()
glVertexAttribL4i64NV v1 v2 v3 v4 v5 = liftIO $ dyn871 ptr_glVertexAttribL4i64NV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttribL4i64NV #-}
ptr_glVertexAttribL4i64NV :: FunPtr (GLuint -> GLint64EXT -> GLint64EXT -> GLint64EXT -> GLint64EXT -> IO ())
ptr_glVertexAttribL4i64NV = unsafePerformIO $ getCommand "glVertexAttribL4i64NV"

-- glVertexAttribL4i64vNV ------------------------------------------------------

glVertexAttribL4i64vNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLint64EXT -- ^ @v@ pointing to @4@ elements of type @GLint64EXT@.
  -> m ()
glVertexAttribL4i64vNV v1 v2 = liftIO $ dyn864 ptr_glVertexAttribL4i64vNV v1 v2

{-# NOINLINE ptr_glVertexAttribL4i64vNV #-}
ptr_glVertexAttribL4i64vNV :: FunPtr (GLuint -> Ptr GLint64EXT -> IO ())
ptr_glVertexAttribL4i64vNV = unsafePerformIO $ getCommand "glVertexAttribL4i64vNV"

-- glVertexAttribL4ui64NV ------------------------------------------------------

glVertexAttribL4ui64NV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLuint64EXT -- ^ @x@.
  -> GLuint64EXT -- ^ @y@.
  -> GLuint64EXT -- ^ @z@.
  -> GLuint64EXT -- ^ @w@.
  -> m ()
glVertexAttribL4ui64NV v1 v2 v3 v4 v5 = liftIO $ dyn872 ptr_glVertexAttribL4ui64NV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttribL4ui64NV #-}
ptr_glVertexAttribL4ui64NV :: FunPtr (GLuint -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> GLuint64EXT -> IO ())
ptr_glVertexAttribL4ui64NV = unsafePerformIO $ getCommand "glVertexAttribL4ui64NV"

-- glVertexAttribL4ui64vNV -----------------------------------------------------

glVertexAttribL4ui64vNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLuint64EXT -- ^ @v@ pointing to @4@ elements of type @GLuint64EXT@.
  -> m ()
glVertexAttribL4ui64vNV v1 v2 = liftIO $ dyn866 ptr_glVertexAttribL4ui64vNV v1 v2

{-# NOINLINE ptr_glVertexAttribL4ui64vNV #-}
ptr_glVertexAttribL4ui64vNV :: FunPtr (GLuint -> Ptr GLuint64EXT -> IO ())
ptr_glVertexAttribL4ui64vNV = unsafePerformIO $ getCommand "glVertexAttribL4ui64vNV"

-- glVertexAttribLFormat -------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glVertexAttribFormat.xhtml OpenGL 4.x>.
glVertexAttribLFormat
  :: MonadIO m
  => GLuint -- ^ @attribindex@.
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@.
  -> GLuint -- ^ @relativeoffset@.
  -> m ()
glVertexAttribLFormat v1 v2 v3 v4 = liftIO $ dyn860 ptr_glVertexAttribLFormat v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttribLFormat #-}
ptr_glVertexAttribLFormat :: FunPtr (GLuint -> GLint -> GLenum -> GLuint -> IO ())
ptr_glVertexAttribLFormat = unsafePerformIO $ getCommand "glVertexAttribLFormat"

-- glVertexAttribLFormatNV -----------------------------------------------------

glVertexAttribLFormatNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@.
  -> GLsizei -- ^ @stride@.
  -> m ()
glVertexAttribLFormatNV v1 v2 v3 v4 = liftIO $ dyn861 ptr_glVertexAttribLFormatNV v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttribLFormatNV #-}
ptr_glVertexAttribLFormatNV :: FunPtr (GLuint -> GLint -> GLenum -> GLsizei -> IO ())
ptr_glVertexAttribLFormatNV = unsafePerformIO $ getCommand "glVertexAttribLFormatNV"

-- glVertexAttribLPointer ------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glVertexAttribPointer.xhtml OpenGL 4.x>.
glVertexAttribLPointer
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@.
  -> GLsizei -- ^ @stride@.
  -> Ptr a -- ^ @pointer@ pointing to @size@ elements of type @a@.
  -> m ()
glVertexAttribLPointer v1 v2 v3 v4 v5 = liftIO $ dyn862 ptr_glVertexAttribLPointer v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttribLPointer #-}
ptr_glVertexAttribLPointer :: FunPtr (GLuint -> GLint -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glVertexAttribLPointer = unsafePerformIO $ getCommand "glVertexAttribLPointer"

-- glVertexAttribLPointerEXT ---------------------------------------------------

-- | This command is an alias for 'glVertexAttribLPointer'.
glVertexAttribLPointerEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@.
  -> GLsizei -- ^ @stride@.
  -> Ptr a -- ^ @pointer@ pointing to @size@ elements of type @a@.
  -> m ()
glVertexAttribLPointerEXT v1 v2 v3 v4 v5 = liftIO $ dyn862 ptr_glVertexAttribLPointerEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttribLPointerEXT #-}
ptr_glVertexAttribLPointerEXT :: FunPtr (GLuint -> GLint -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glVertexAttribLPointerEXT = unsafePerformIO $ getCommand "glVertexAttribLPointerEXT"

-- glVertexAttribP1ui ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttribP1ui
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @type@.
  -> GLboolean -- ^ @normalized@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLuint -- ^ @value@.
  -> m ()
glVertexAttribP1ui v1 v2 v3 v4 = liftIO $ dyn873 ptr_glVertexAttribP1ui v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttribP1ui #-}
ptr_glVertexAttribP1ui :: FunPtr (GLuint -> GLenum -> GLboolean -> GLuint -> IO ())
ptr_glVertexAttribP1ui = unsafePerformIO $ getCommand "glVertexAttribP1ui"

-- glVertexAttribP1uiv ---------------------------------------------------------

glVertexAttribP1uiv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @type@.
  -> GLboolean -- ^ @normalized@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLuint -- ^ @value@ pointing to @1@ element of type @GLuint@.
  -> m ()
glVertexAttribP1uiv v1 v2 v3 v4 = liftIO $ dyn874 ptr_glVertexAttribP1uiv v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttribP1uiv #-}
ptr_glVertexAttribP1uiv :: FunPtr (GLuint -> GLenum -> GLboolean -> Ptr GLuint -> IO ())
ptr_glVertexAttribP1uiv = unsafePerformIO $ getCommand "glVertexAttribP1uiv"

-- glVertexAttribP2ui ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttribP2ui
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @type@.
  -> GLboolean -- ^ @normalized@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLuint -- ^ @value@.
  -> m ()
glVertexAttribP2ui v1 v2 v3 v4 = liftIO $ dyn873 ptr_glVertexAttribP2ui v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttribP2ui #-}
ptr_glVertexAttribP2ui :: FunPtr (GLuint -> GLenum -> GLboolean -> GLuint -> IO ())
ptr_glVertexAttribP2ui = unsafePerformIO $ getCommand "glVertexAttribP2ui"

-- glVertexAttribP2uiv ---------------------------------------------------------

glVertexAttribP2uiv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @type@.
  -> GLboolean -- ^ @normalized@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLuint -- ^ @value@ pointing to @1@ element of type @GLuint@.
  -> m ()
glVertexAttribP2uiv v1 v2 v3 v4 = liftIO $ dyn874 ptr_glVertexAttribP2uiv v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttribP2uiv #-}
ptr_glVertexAttribP2uiv :: FunPtr (GLuint -> GLenum -> GLboolean -> Ptr GLuint -> IO ())
ptr_glVertexAttribP2uiv = unsafePerformIO $ getCommand "glVertexAttribP2uiv"

-- glVertexAttribP3ui ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttribP3ui
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @type@.
  -> GLboolean -- ^ @normalized@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLuint -- ^ @value@.
  -> m ()
glVertexAttribP3ui v1 v2 v3 v4 = liftIO $ dyn873 ptr_glVertexAttribP3ui v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttribP3ui #-}
ptr_glVertexAttribP3ui :: FunPtr (GLuint -> GLenum -> GLboolean -> GLuint -> IO ())
ptr_glVertexAttribP3ui = unsafePerformIO $ getCommand "glVertexAttribP3ui"

-- glVertexAttribP3uiv ---------------------------------------------------------

glVertexAttribP3uiv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @type@.
  -> GLboolean -- ^ @normalized@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLuint -- ^ @value@ pointing to @1@ element of type @GLuint@.
  -> m ()
glVertexAttribP3uiv v1 v2 v3 v4 = liftIO $ dyn874 ptr_glVertexAttribP3uiv v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttribP3uiv #-}
ptr_glVertexAttribP3uiv :: FunPtr (GLuint -> GLenum -> GLboolean -> Ptr GLuint -> IO ())
ptr_glVertexAttribP3uiv = unsafePerformIO $ getCommand "glVertexAttribP3uiv"

-- glVertexAttribP4ui ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttribP4ui
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @type@.
  -> GLboolean -- ^ @normalized@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLuint -- ^ @value@.
  -> m ()
glVertexAttribP4ui v1 v2 v3 v4 = liftIO $ dyn873 ptr_glVertexAttribP4ui v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttribP4ui #-}
ptr_glVertexAttribP4ui :: FunPtr (GLuint -> GLenum -> GLboolean -> GLuint -> IO ())
ptr_glVertexAttribP4ui = unsafePerformIO $ getCommand "glVertexAttribP4ui"

-- glVertexAttribP4uiv ---------------------------------------------------------

glVertexAttribP4uiv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @type@.
  -> GLboolean -- ^ @normalized@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> Ptr GLuint -- ^ @value@ pointing to @1@ element of type @GLuint@.
  -> m ()
glVertexAttribP4uiv v1 v2 v3 v4 = liftIO $ dyn874 ptr_glVertexAttribP4uiv v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttribP4uiv #-}
ptr_glVertexAttribP4uiv :: FunPtr (GLuint -> GLenum -> GLboolean -> Ptr GLuint -> IO ())
ptr_glVertexAttribP4uiv = unsafePerformIO $ getCommand "glVertexAttribP4uiv"

-- glVertexAttribParameteriAMD -------------------------------------------------

glVertexAttribParameteriAMD
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@.
  -> GLint -- ^ @param@.
  -> m ()
glVertexAttribParameteriAMD v1 v2 v3 = liftIO $ dyn488 ptr_glVertexAttribParameteriAMD v1 v2 v3

{-# NOINLINE ptr_glVertexAttribParameteriAMD #-}
ptr_glVertexAttribParameteriAMD :: FunPtr (GLuint -> GLenum -> GLint -> IO ())
ptr_glVertexAttribParameteriAMD = unsafePerformIO $ getCommand "glVertexAttribParameteriAMD"

-- glVertexAttribPointer -------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttribPointer.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttribPointer.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttribPointer.xhtml OpenGL 4.x>.
glVertexAttribPointer
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type @VertexAttribPointerType@.
  -> GLboolean -- ^ @normalized@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLsizei -- ^ @stride@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(size,type,stride)@ elements of type @a@.
  -> m ()
glVertexAttribPointer v1 v2 v3 v4 v5 v6 = liftIO $ dyn875 ptr_glVertexAttribPointer v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glVertexAttribPointer #-}
ptr_glVertexAttribPointer :: FunPtr (GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> Ptr a -> IO ())
ptr_glVertexAttribPointer = unsafePerformIO $ getCommand "glVertexAttribPointer"

-- glVertexAttribPointerARB ----------------------------------------------------

-- | This command is an alias for 'glVertexAttribPointer'.
glVertexAttribPointerARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type @VertexAttribPointerType@.
  -> GLboolean -- ^ @normalized@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLsizei -- ^ @stride@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(size,type,stride)@ elements of type @a@.
  -> m ()
glVertexAttribPointerARB v1 v2 v3 v4 v5 v6 = liftIO $ dyn875 ptr_glVertexAttribPointerARB v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glVertexAttribPointerARB #-}
ptr_glVertexAttribPointerARB :: FunPtr (GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> Ptr a -> IO ())
ptr_glVertexAttribPointerARB = unsafePerformIO $ getCommand "glVertexAttribPointerARB"

-- glVertexAttribPointerNV -----------------------------------------------------

glVertexAttribPointerNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLint -- ^ @fsize@.
  -> GLenum -- ^ @type@ of type @VertexAttribEnumNV@.
  -> GLsizei -- ^ @stride@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(fsize,type,stride)@ elements of type @a@.
  -> m ()
glVertexAttribPointerNV v1 v2 v3 v4 v5 = liftIO $ dyn862 ptr_glVertexAttribPointerNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttribPointerNV #-}
ptr_glVertexAttribPointerNV :: FunPtr (GLuint -> GLint -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glVertexAttribPointerNV = unsafePerformIO $ getCommand "glVertexAttribPointerNV"

-- glVertexAttribs1dvNV --------------------------------------------------------

glVertexAttribs1dvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLdouble -- ^ @v@ pointing to @count@ elements of type @GLdouble@.
  -> m ()
glVertexAttribs1dvNV v1 v2 v3 = liftIO $ dyn219 ptr_glVertexAttribs1dvNV v1 v2 v3

{-# NOINLINE ptr_glVertexAttribs1dvNV #-}
ptr_glVertexAttribs1dvNV :: FunPtr (GLuint -> GLsizei -> Ptr GLdouble -> IO ())
ptr_glVertexAttribs1dvNV = unsafePerformIO $ getCommand "glVertexAttribs1dvNV"

-- glVertexAttribs1fvNV --------------------------------------------------------

glVertexAttribs1fvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @v@ pointing to @count@ elements of type @GLfloat@.
  -> m ()
glVertexAttribs1fvNV v1 v2 v3 = liftIO $ dyn218 ptr_glVertexAttribs1fvNV v1 v2 v3

{-# NOINLINE ptr_glVertexAttribs1fvNV #-}
ptr_glVertexAttribs1fvNV :: FunPtr (GLuint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glVertexAttribs1fvNV = unsafePerformIO $ getCommand "glVertexAttribs1fvNV"

-- glVertexAttribs1hvNV --------------------------------------------------------

glVertexAttribs1hvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLsizei -- ^ @n@.
  -> Ptr GLhalfNV -- ^ @v@ pointing to @n@ elements of type @Half16NV@.
  -> m ()
glVertexAttribs1hvNV v1 v2 v3 = liftIO $ dyn876 ptr_glVertexAttribs1hvNV v1 v2 v3

{-# NOINLINE ptr_glVertexAttribs1hvNV #-}
ptr_glVertexAttribs1hvNV :: FunPtr (GLuint -> GLsizei -> Ptr GLhalfNV -> IO ())
ptr_glVertexAttribs1hvNV = unsafePerformIO $ getCommand "glVertexAttribs1hvNV"

-- glVertexAttribs1svNV --------------------------------------------------------

glVertexAttribs1svNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLshort -- ^ @v@ pointing to @count@ elements of type @GLshort@.
  -> m ()
glVertexAttribs1svNV v1 v2 v3 = liftIO $ dyn877 ptr_glVertexAttribs1svNV v1 v2 v3

{-# NOINLINE ptr_glVertexAttribs1svNV #-}
ptr_glVertexAttribs1svNV :: FunPtr (GLuint -> GLsizei -> Ptr GLshort -> IO ())
ptr_glVertexAttribs1svNV = unsafePerformIO $ getCommand "glVertexAttribs1svNV"

-- glVertexAttribs2dvNV --------------------------------------------------------

glVertexAttribs2dvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLdouble -- ^ @v@ pointing to @count*2@ elements of type @GLdouble@.
  -> m ()
glVertexAttribs2dvNV v1 v2 v3 = liftIO $ dyn219 ptr_glVertexAttribs2dvNV v1 v2 v3

{-# NOINLINE ptr_glVertexAttribs2dvNV #-}
ptr_glVertexAttribs2dvNV :: FunPtr (GLuint -> GLsizei -> Ptr GLdouble -> IO ())
ptr_glVertexAttribs2dvNV = unsafePerformIO $ getCommand "glVertexAttribs2dvNV"

-- glVertexAttribs2fvNV --------------------------------------------------------

glVertexAttribs2fvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @v@ pointing to @count*2@ elements of type @GLfloat@.
  -> m ()
glVertexAttribs2fvNV v1 v2 v3 = liftIO $ dyn218 ptr_glVertexAttribs2fvNV v1 v2 v3

{-# NOINLINE ptr_glVertexAttribs2fvNV #-}
ptr_glVertexAttribs2fvNV :: FunPtr (GLuint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glVertexAttribs2fvNV = unsafePerformIO $ getCommand "glVertexAttribs2fvNV"

-- glVertexAttribs2hvNV --------------------------------------------------------

glVertexAttribs2hvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLsizei -- ^ @n@.
  -> Ptr GLhalfNV -- ^ @v@ pointing to @n@ elements of type @Half16NV@.
  -> m ()
glVertexAttribs2hvNV v1 v2 v3 = liftIO $ dyn876 ptr_glVertexAttribs2hvNV v1 v2 v3

{-# NOINLINE ptr_glVertexAttribs2hvNV #-}
ptr_glVertexAttribs2hvNV :: FunPtr (GLuint -> GLsizei -> Ptr GLhalfNV -> IO ())
ptr_glVertexAttribs2hvNV = unsafePerformIO $ getCommand "glVertexAttribs2hvNV"

-- glVertexAttribs2svNV --------------------------------------------------------

glVertexAttribs2svNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLshort -- ^ @v@ pointing to @count*2@ elements of type @GLshort@.
  -> m ()
glVertexAttribs2svNV v1 v2 v3 = liftIO $ dyn877 ptr_glVertexAttribs2svNV v1 v2 v3

{-# NOINLINE ptr_glVertexAttribs2svNV #-}
ptr_glVertexAttribs2svNV :: FunPtr (GLuint -> GLsizei -> Ptr GLshort -> IO ())
ptr_glVertexAttribs2svNV = unsafePerformIO $ getCommand "glVertexAttribs2svNV"

-- glVertexAttribs3dvNV --------------------------------------------------------

glVertexAttribs3dvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLdouble -- ^ @v@ pointing to @count*3@ elements of type @GLdouble@.
  -> m ()
glVertexAttribs3dvNV v1 v2 v3 = liftIO $ dyn219 ptr_glVertexAttribs3dvNV v1 v2 v3

{-# NOINLINE ptr_glVertexAttribs3dvNV #-}
ptr_glVertexAttribs3dvNV :: FunPtr (GLuint -> GLsizei -> Ptr GLdouble -> IO ())
ptr_glVertexAttribs3dvNV = unsafePerformIO $ getCommand "glVertexAttribs3dvNV"

-- glVertexAttribs3fvNV --------------------------------------------------------

glVertexAttribs3fvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @v@ pointing to @count*3@ elements of type @GLfloat@.
  -> m ()
glVertexAttribs3fvNV v1 v2 v3 = liftIO $ dyn218 ptr_glVertexAttribs3fvNV v1 v2 v3

{-# NOINLINE ptr_glVertexAttribs3fvNV #-}
ptr_glVertexAttribs3fvNV :: FunPtr (GLuint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glVertexAttribs3fvNV = unsafePerformIO $ getCommand "glVertexAttribs3fvNV"

-- glVertexAttribs3hvNV --------------------------------------------------------

glVertexAttribs3hvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLsizei -- ^ @n@.
  -> Ptr GLhalfNV -- ^ @v@ pointing to @n@ elements of type @Half16NV@.
  -> m ()
glVertexAttribs3hvNV v1 v2 v3 = liftIO $ dyn876 ptr_glVertexAttribs3hvNV v1 v2 v3

{-# NOINLINE ptr_glVertexAttribs3hvNV #-}
ptr_glVertexAttribs3hvNV :: FunPtr (GLuint -> GLsizei -> Ptr GLhalfNV -> IO ())
ptr_glVertexAttribs3hvNV = unsafePerformIO $ getCommand "glVertexAttribs3hvNV"

-- glVertexAttribs3svNV --------------------------------------------------------

glVertexAttribs3svNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLshort -- ^ @v@ pointing to @count*3@ elements of type @GLshort@.
  -> m ()
glVertexAttribs3svNV v1 v2 v3 = liftIO $ dyn877 ptr_glVertexAttribs3svNV v1 v2 v3

{-# NOINLINE ptr_glVertexAttribs3svNV #-}
ptr_glVertexAttribs3svNV :: FunPtr (GLuint -> GLsizei -> Ptr GLshort -> IO ())
ptr_glVertexAttribs3svNV = unsafePerformIO $ getCommand "glVertexAttribs3svNV"

-- glVertexAttribs4dvNV --------------------------------------------------------

glVertexAttribs4dvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLdouble -- ^ @v@ pointing to @count*4@ elements of type @GLdouble@.
  -> m ()
glVertexAttribs4dvNV v1 v2 v3 = liftIO $ dyn219 ptr_glVertexAttribs4dvNV v1 v2 v3

{-# NOINLINE ptr_glVertexAttribs4dvNV #-}
ptr_glVertexAttribs4dvNV :: FunPtr (GLuint -> GLsizei -> Ptr GLdouble -> IO ())
ptr_glVertexAttribs4dvNV = unsafePerformIO $ getCommand "glVertexAttribs4dvNV"

-- glVertexAttribs4fvNV --------------------------------------------------------

glVertexAttribs4fvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @v@ pointing to @count*4@ elements of type @GLfloat@.
  -> m ()
glVertexAttribs4fvNV v1 v2 v3 = liftIO $ dyn218 ptr_glVertexAttribs4fvNV v1 v2 v3

{-# NOINLINE ptr_glVertexAttribs4fvNV #-}
ptr_glVertexAttribs4fvNV :: FunPtr (GLuint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glVertexAttribs4fvNV = unsafePerformIO $ getCommand "glVertexAttribs4fvNV"

