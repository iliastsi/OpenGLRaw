--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F73
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

module Graphics.GL.Functions.F73 (
  glVertexAttrib4uivARB,
  glVertexAttrib4usv,
  glVertexAttrib4usvARB,
  glVertexAttribArrayObjectATI,
  glVertexAttribBinding,
  glVertexAttribDivisor,
  glVertexAttribDivisorANGLE,
  glVertexAttribDivisorARB,
  glVertexAttribDivisorEXT,
  glVertexAttribDivisorNV,
  glVertexAttribFormat,
  glVertexAttribFormatNV,
  glVertexAttribI1i,
  glVertexAttribI1iEXT,
  glVertexAttribI1iv,
  glVertexAttribI1ivEXT,
  glVertexAttribI1ui,
  glVertexAttribI1uiEXT,
  glVertexAttribI1uiv,
  glVertexAttribI1uivEXT,
  glVertexAttribI2i,
  glVertexAttribI2iEXT,
  glVertexAttribI2iv,
  glVertexAttribI2ivEXT,
  glVertexAttribI2ui,
  glVertexAttribI2uiEXT,
  glVertexAttribI2uiv,
  glVertexAttribI2uivEXT,
  glVertexAttribI3i,
  glVertexAttribI3iEXT,
  glVertexAttribI3iv,
  glVertexAttribI3ivEXT,
  glVertexAttribI3ui,
  glVertexAttribI3uiEXT,
  glVertexAttribI3uiv,
  glVertexAttribI3uivEXT,
  glVertexAttribI4bv,
  glVertexAttribI4bvEXT,
  glVertexAttribI4i,
  glVertexAttribI4iEXT
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

-- glVertexAttrib4uivARB -------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib4uiv'.
glVertexAttrib4uivARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLuint -- ^ @v@ pointing to @4@ elements of type @GLuint@.
  -> m ()
glVertexAttrib4uivARB v1 v2 = liftIO $ dyn194 ptr_glVertexAttrib4uivARB v1 v2

{-# NOINLINE ptr_glVertexAttrib4uivARB #-}
ptr_glVertexAttrib4uivARB :: FunPtr (GLuint -> Ptr GLuint -> IO ())
ptr_glVertexAttrib4uivARB = unsafePerformIO $ getCommand "glVertexAttrib4uivARB"

-- glVertexAttrib4usv ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttrib4usv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLushort -- ^ @v@ pointing to @4@ elements of type @GLushort@.
  -> m ()
glVertexAttrib4usv v1 v2 = liftIO $ dyn830 ptr_glVertexAttrib4usv v1 v2

{-# NOINLINE ptr_glVertexAttrib4usv #-}
ptr_glVertexAttrib4usv :: FunPtr (GLuint -> Ptr GLushort -> IO ())
ptr_glVertexAttrib4usv = unsafePerformIO $ getCommand "glVertexAttrib4usv"

-- glVertexAttrib4usvARB -------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib4usv'.
glVertexAttrib4usvARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLushort -- ^ @v@ pointing to @4@ elements of type @GLushort@.
  -> m ()
glVertexAttrib4usvARB v1 v2 = liftIO $ dyn830 ptr_glVertexAttrib4usvARB v1 v2

{-# NOINLINE ptr_glVertexAttrib4usvARB #-}
ptr_glVertexAttrib4usvARB :: FunPtr (GLuint -> Ptr GLushort -> IO ())
ptr_glVertexAttrib4usvARB = unsafePerformIO $ getCommand "glVertexAttrib4usvARB"

-- glVertexAttribArrayObjectATI ------------------------------------------------

glVertexAttribArrayObjectATI
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type @VertexAttribPointerType@.
  -> GLboolean -- ^ @normalized@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLsizei -- ^ @stride@.
  -> GLuint -- ^ @buffer@.
  -> GLuint -- ^ @offset@.
  -> m ()
glVertexAttribArrayObjectATI v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn856 ptr_glVertexAttribArrayObjectATI v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glVertexAttribArrayObjectATI #-}
ptr_glVertexAttribArrayObjectATI :: FunPtr (GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> GLuint -> GLuint -> IO ())
ptr_glVertexAttribArrayObjectATI = unsafePerformIO $ getCommand "glVertexAttribArrayObjectATI"

-- glVertexAttribBinding -------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glVertexAttribBinding.xhtml OpenGL 4.x>.
glVertexAttribBinding
  :: MonadIO m
  => GLuint -- ^ @attribindex@.
  -> GLuint -- ^ @bindingindex@.
  -> m ()
glVertexAttribBinding v1 v2 = liftIO $ dyn3 ptr_glVertexAttribBinding v1 v2

{-# NOINLINE ptr_glVertexAttribBinding #-}
ptr_glVertexAttribBinding :: FunPtr (GLuint -> GLuint -> IO ())
ptr_glVertexAttribBinding = unsafePerformIO $ getCommand "glVertexAttribBinding"

-- glVertexAttribDivisor -------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttribDivisor.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttribDivisor.xhtml OpenGL 4.x>.
glVertexAttribDivisor
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLuint -- ^ @divisor@.
  -> m ()
glVertexAttribDivisor v1 v2 = liftIO $ dyn3 ptr_glVertexAttribDivisor v1 v2

{-# NOINLINE ptr_glVertexAttribDivisor #-}
ptr_glVertexAttribDivisor :: FunPtr (GLuint -> GLuint -> IO ())
ptr_glVertexAttribDivisor = unsafePerformIO $ getCommand "glVertexAttribDivisor"

-- glVertexAttribDivisorANGLE --------------------------------------------------

-- | This command is an alias for 'glVertexAttribDivisor'.
glVertexAttribDivisorANGLE
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLuint -- ^ @divisor@.
  -> m ()
glVertexAttribDivisorANGLE v1 v2 = liftIO $ dyn3 ptr_glVertexAttribDivisorANGLE v1 v2

{-# NOINLINE ptr_glVertexAttribDivisorANGLE #-}
ptr_glVertexAttribDivisorANGLE :: FunPtr (GLuint -> GLuint -> IO ())
ptr_glVertexAttribDivisorANGLE = unsafePerformIO $ getCommand "glVertexAttribDivisorANGLE"

-- glVertexAttribDivisorARB ----------------------------------------------------

-- | This command is an alias for 'glVertexAttribDivisor'.
glVertexAttribDivisorARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLuint -- ^ @divisor@.
  -> m ()
glVertexAttribDivisorARB v1 v2 = liftIO $ dyn3 ptr_glVertexAttribDivisorARB v1 v2

{-# NOINLINE ptr_glVertexAttribDivisorARB #-}
ptr_glVertexAttribDivisorARB :: FunPtr (GLuint -> GLuint -> IO ())
ptr_glVertexAttribDivisorARB = unsafePerformIO $ getCommand "glVertexAttribDivisorARB"

-- glVertexAttribDivisorEXT ----------------------------------------------------

-- | This command is an alias for 'glVertexAttribDivisor'.
glVertexAttribDivisorEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLuint -- ^ @divisor@.
  -> m ()
glVertexAttribDivisorEXT v1 v2 = liftIO $ dyn3 ptr_glVertexAttribDivisorEXT v1 v2

{-# NOINLINE ptr_glVertexAttribDivisorEXT #-}
ptr_glVertexAttribDivisorEXT :: FunPtr (GLuint -> GLuint -> IO ())
ptr_glVertexAttribDivisorEXT = unsafePerformIO $ getCommand "glVertexAttribDivisorEXT"

-- glVertexAttribDivisorNV -----------------------------------------------------

-- | This command is an alias for 'glVertexAttribDivisor'.
glVertexAttribDivisorNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLuint -- ^ @divisor@.
  -> m ()
glVertexAttribDivisorNV v1 v2 = liftIO $ dyn3 ptr_glVertexAttribDivisorNV v1 v2

{-# NOINLINE ptr_glVertexAttribDivisorNV #-}
ptr_glVertexAttribDivisorNV :: FunPtr (GLuint -> GLuint -> IO ())
ptr_glVertexAttribDivisorNV = unsafePerformIO $ getCommand "glVertexAttribDivisorNV"

-- glVertexAttribFormat --------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glVertexAttribFormat.xhtml OpenGL 4.x>.
glVertexAttribFormat
  :: MonadIO m
  => GLuint -- ^ @attribindex@.
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@.
  -> GLboolean -- ^ @normalized@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLuint -- ^ @relativeoffset@.
  -> m ()
glVertexAttribFormat v1 v2 v3 v4 v5 = liftIO $ dyn857 ptr_glVertexAttribFormat v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttribFormat #-}
ptr_glVertexAttribFormat :: FunPtr (GLuint -> GLint -> GLenum -> GLboolean -> GLuint -> IO ())
ptr_glVertexAttribFormat = unsafePerformIO $ getCommand "glVertexAttribFormat"

-- glVertexAttribFormatNV ------------------------------------------------------

glVertexAttribFormatNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLint -- ^ @size@.
  -> GLenum -- ^ @type@.
  -> GLboolean -- ^ @normalized@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLsizei -- ^ @stride@.
  -> m ()
glVertexAttribFormatNV v1 v2 v3 v4 v5 = liftIO $ dyn858 ptr_glVertexAttribFormatNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttribFormatNV #-}
ptr_glVertexAttribFormatNV :: FunPtr (GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> IO ())
ptr_glVertexAttribFormatNV = unsafePerformIO $ getCommand "glVertexAttribFormatNV"

-- glVertexAttribI1i -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>. The vector equivalent of this command is 'glVertexAttribI1iv'.
glVertexAttribI1i
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLint -- ^ @x@.
  -> m ()
glVertexAttribI1i v1 v2 = liftIO $ dyn474 ptr_glVertexAttribI1i v1 v2

{-# NOINLINE ptr_glVertexAttribI1i #-}
ptr_glVertexAttribI1i :: FunPtr (GLuint -> GLint -> IO ())
ptr_glVertexAttribI1i = unsafePerformIO $ getCommand "glVertexAttribI1i"

-- glVertexAttribI1iEXT --------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttribI1ivEXT'. This command is an alias for 'glVertexAttribI1i'.
glVertexAttribI1iEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLint -- ^ @x@.
  -> m ()
glVertexAttribI1iEXT v1 v2 = liftIO $ dyn474 ptr_glVertexAttribI1iEXT v1 v2

{-# NOINLINE ptr_glVertexAttribI1iEXT #-}
ptr_glVertexAttribI1iEXT :: FunPtr (GLuint -> GLint -> IO ())
ptr_glVertexAttribI1iEXT = unsafePerformIO $ getCommand "glVertexAttribI1iEXT"

-- glVertexAttribI1iv ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttribI1iv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLint -- ^ @v@ pointing to @1@ element of type @GLint@.
  -> m ()
glVertexAttribI1iv v1 v2 = liftIO $ dyn701 ptr_glVertexAttribI1iv v1 v2

{-# NOINLINE ptr_glVertexAttribI1iv #-}
ptr_glVertexAttribI1iv :: FunPtr (GLuint -> Ptr GLint -> IO ())
ptr_glVertexAttribI1iv = unsafePerformIO $ getCommand "glVertexAttribI1iv"

-- glVertexAttribI1ivEXT -------------------------------------------------------

-- | This command is an alias for 'glVertexAttribI1iv'.
glVertexAttribI1ivEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLint -- ^ @v@ pointing to @1@ element of type @GLint@.
  -> m ()
glVertexAttribI1ivEXT v1 v2 = liftIO $ dyn701 ptr_glVertexAttribI1ivEXT v1 v2

{-# NOINLINE ptr_glVertexAttribI1ivEXT #-}
ptr_glVertexAttribI1ivEXT :: FunPtr (GLuint -> Ptr GLint -> IO ())
ptr_glVertexAttribI1ivEXT = unsafePerformIO $ getCommand "glVertexAttribI1ivEXT"

-- glVertexAttribI1ui ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>. The vector equivalent of this command is 'glVertexAttribI1uiv'.
glVertexAttribI1ui
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLuint -- ^ @x@.
  -> m ()
glVertexAttribI1ui v1 v2 = liftIO $ dyn3 ptr_glVertexAttribI1ui v1 v2

{-# NOINLINE ptr_glVertexAttribI1ui #-}
ptr_glVertexAttribI1ui :: FunPtr (GLuint -> GLuint -> IO ())
ptr_glVertexAttribI1ui = unsafePerformIO $ getCommand "glVertexAttribI1ui"

-- glVertexAttribI1uiEXT -------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttribI1uivEXT'. This command is an alias for 'glVertexAttribI1ui'.
glVertexAttribI1uiEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLuint -- ^ @x@.
  -> m ()
glVertexAttribI1uiEXT v1 v2 = liftIO $ dyn3 ptr_glVertexAttribI1uiEXT v1 v2

{-# NOINLINE ptr_glVertexAttribI1uiEXT #-}
ptr_glVertexAttribI1uiEXT :: FunPtr (GLuint -> GLuint -> IO ())
ptr_glVertexAttribI1uiEXT = unsafePerformIO $ getCommand "glVertexAttribI1uiEXT"

-- glVertexAttribI1uiv ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttribI1uiv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLuint -- ^ @v@ pointing to @1@ element of type @GLuint@.
  -> m ()
glVertexAttribI1uiv v1 v2 = liftIO $ dyn194 ptr_glVertexAttribI1uiv v1 v2

{-# NOINLINE ptr_glVertexAttribI1uiv #-}
ptr_glVertexAttribI1uiv :: FunPtr (GLuint -> Ptr GLuint -> IO ())
ptr_glVertexAttribI1uiv = unsafePerformIO $ getCommand "glVertexAttribI1uiv"

-- glVertexAttribI1uivEXT ------------------------------------------------------

-- | This command is an alias for 'glVertexAttribI1uiv'.
glVertexAttribI1uivEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLuint -- ^ @v@ pointing to @1@ element of type @GLuint@.
  -> m ()
glVertexAttribI1uivEXT v1 v2 = liftIO $ dyn194 ptr_glVertexAttribI1uivEXT v1 v2

{-# NOINLINE ptr_glVertexAttribI1uivEXT #-}
ptr_glVertexAttribI1uivEXT :: FunPtr (GLuint -> Ptr GLuint -> IO ())
ptr_glVertexAttribI1uivEXT = unsafePerformIO $ getCommand "glVertexAttribI1uivEXT"

-- glVertexAttribI2i -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>. The vector equivalent of this command is 'glVertexAttribI2iv'.
glVertexAttribI2i
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLint -- ^ @x@.
  -> GLint -- ^ @y@.
  -> m ()
glVertexAttribI2i v1 v2 v3 = liftIO $ dyn633 ptr_glVertexAttribI2i v1 v2 v3

{-# NOINLINE ptr_glVertexAttribI2i #-}
ptr_glVertexAttribI2i :: FunPtr (GLuint -> GLint -> GLint -> IO ())
ptr_glVertexAttribI2i = unsafePerformIO $ getCommand "glVertexAttribI2i"

-- glVertexAttribI2iEXT --------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttribI2ivEXT'. This command is an alias for 'glVertexAttribI2i'.
glVertexAttribI2iEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLint -- ^ @x@.
  -> GLint -- ^ @y@.
  -> m ()
glVertexAttribI2iEXT v1 v2 v3 = liftIO $ dyn633 ptr_glVertexAttribI2iEXT v1 v2 v3

{-# NOINLINE ptr_glVertexAttribI2iEXT #-}
ptr_glVertexAttribI2iEXT :: FunPtr (GLuint -> GLint -> GLint -> IO ())
ptr_glVertexAttribI2iEXT = unsafePerformIO $ getCommand "glVertexAttribI2iEXT"

-- glVertexAttribI2iv ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttribI2iv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLint -- ^ @v@ pointing to @2@ elements of type @GLint@.
  -> m ()
glVertexAttribI2iv v1 v2 = liftIO $ dyn701 ptr_glVertexAttribI2iv v1 v2

{-# NOINLINE ptr_glVertexAttribI2iv #-}
ptr_glVertexAttribI2iv :: FunPtr (GLuint -> Ptr GLint -> IO ())
ptr_glVertexAttribI2iv = unsafePerformIO $ getCommand "glVertexAttribI2iv"

-- glVertexAttribI2ivEXT -------------------------------------------------------

-- | This command is an alias for 'glVertexAttribI2iv'.
glVertexAttribI2ivEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLint -- ^ @v@ pointing to @2@ elements of type @GLint@.
  -> m ()
glVertexAttribI2ivEXT v1 v2 = liftIO $ dyn701 ptr_glVertexAttribI2ivEXT v1 v2

{-# NOINLINE ptr_glVertexAttribI2ivEXT #-}
ptr_glVertexAttribI2ivEXT :: FunPtr (GLuint -> Ptr GLint -> IO ())
ptr_glVertexAttribI2ivEXT = unsafePerformIO $ getCommand "glVertexAttribI2ivEXT"

-- glVertexAttribI2ui ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>. The vector equivalent of this command is 'glVertexAttribI2uiv'.
glVertexAttribI2ui
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLuint -- ^ @x@.
  -> GLuint -- ^ @y@.
  -> m ()
glVertexAttribI2ui v1 v2 v3 = liftIO $ dyn102 ptr_glVertexAttribI2ui v1 v2 v3

{-# NOINLINE ptr_glVertexAttribI2ui #-}
ptr_glVertexAttribI2ui :: FunPtr (GLuint -> GLuint -> GLuint -> IO ())
ptr_glVertexAttribI2ui = unsafePerformIO $ getCommand "glVertexAttribI2ui"

-- glVertexAttribI2uiEXT -------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttribI2uivEXT'. This command is an alias for 'glVertexAttribI2ui'.
glVertexAttribI2uiEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLuint -- ^ @x@.
  -> GLuint -- ^ @y@.
  -> m ()
glVertexAttribI2uiEXT v1 v2 v3 = liftIO $ dyn102 ptr_glVertexAttribI2uiEXT v1 v2 v3

{-# NOINLINE ptr_glVertexAttribI2uiEXT #-}
ptr_glVertexAttribI2uiEXT :: FunPtr (GLuint -> GLuint -> GLuint -> IO ())
ptr_glVertexAttribI2uiEXT = unsafePerformIO $ getCommand "glVertexAttribI2uiEXT"

-- glVertexAttribI2uiv ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttribI2uiv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLuint -- ^ @v@ pointing to @2@ elements of type @GLuint@.
  -> m ()
glVertexAttribI2uiv v1 v2 = liftIO $ dyn194 ptr_glVertexAttribI2uiv v1 v2

{-# NOINLINE ptr_glVertexAttribI2uiv #-}
ptr_glVertexAttribI2uiv :: FunPtr (GLuint -> Ptr GLuint -> IO ())
ptr_glVertexAttribI2uiv = unsafePerformIO $ getCommand "glVertexAttribI2uiv"

-- glVertexAttribI2uivEXT ------------------------------------------------------

-- | This command is an alias for 'glVertexAttribI2uiv'.
glVertexAttribI2uivEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLuint -- ^ @v@ pointing to @2@ elements of type @GLuint@.
  -> m ()
glVertexAttribI2uivEXT v1 v2 = liftIO $ dyn194 ptr_glVertexAttribI2uivEXT v1 v2

{-# NOINLINE ptr_glVertexAttribI2uivEXT #-}
ptr_glVertexAttribI2uivEXT :: FunPtr (GLuint -> Ptr GLuint -> IO ())
ptr_glVertexAttribI2uivEXT = unsafePerformIO $ getCommand "glVertexAttribI2uivEXT"

-- glVertexAttribI3i -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>. The vector equivalent of this command is 'glVertexAttribI3iv'.
glVertexAttribI3i
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLint -- ^ @x@.
  -> GLint -- ^ @y@.
  -> GLint -- ^ @z@.
  -> m ()
glVertexAttribI3i v1 v2 v3 v4 = liftIO $ dyn643 ptr_glVertexAttribI3i v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttribI3i #-}
ptr_glVertexAttribI3i :: FunPtr (GLuint -> GLint -> GLint -> GLint -> IO ())
ptr_glVertexAttribI3i = unsafePerformIO $ getCommand "glVertexAttribI3i"

-- glVertexAttribI3iEXT --------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttribI3ivEXT'. This command is an alias for 'glVertexAttribI3i'.
glVertexAttribI3iEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLint -- ^ @x@.
  -> GLint -- ^ @y@.
  -> GLint -- ^ @z@.
  -> m ()
glVertexAttribI3iEXT v1 v2 v3 v4 = liftIO $ dyn643 ptr_glVertexAttribI3iEXT v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttribI3iEXT #-}
ptr_glVertexAttribI3iEXT :: FunPtr (GLuint -> GLint -> GLint -> GLint -> IO ())
ptr_glVertexAttribI3iEXT = unsafePerformIO $ getCommand "glVertexAttribI3iEXT"

-- glVertexAttribI3iv ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttribI3iv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLint -- ^ @v@ pointing to @3@ elements of type @GLint@.
  -> m ()
glVertexAttribI3iv v1 v2 = liftIO $ dyn701 ptr_glVertexAttribI3iv v1 v2

{-# NOINLINE ptr_glVertexAttribI3iv #-}
ptr_glVertexAttribI3iv :: FunPtr (GLuint -> Ptr GLint -> IO ())
ptr_glVertexAttribI3iv = unsafePerformIO $ getCommand "glVertexAttribI3iv"

-- glVertexAttribI3ivEXT -------------------------------------------------------

-- | This command is an alias for 'glVertexAttribI3iv'.
glVertexAttribI3ivEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLint -- ^ @v@ pointing to @3@ elements of type @GLint@.
  -> m ()
glVertexAttribI3ivEXT v1 v2 = liftIO $ dyn701 ptr_glVertexAttribI3ivEXT v1 v2

{-# NOINLINE ptr_glVertexAttribI3ivEXT #-}
ptr_glVertexAttribI3ivEXT :: FunPtr (GLuint -> Ptr GLint -> IO ())
ptr_glVertexAttribI3ivEXT = unsafePerformIO $ getCommand "glVertexAttribI3ivEXT"

-- glVertexAttribI3ui ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>. The vector equivalent of this command is 'glVertexAttribI3uiv'.
glVertexAttribI3ui
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLuint -- ^ @x@.
  -> GLuint -- ^ @y@.
  -> GLuint -- ^ @z@.
  -> m ()
glVertexAttribI3ui v1 v2 v3 v4 = liftIO $ dyn77 ptr_glVertexAttribI3ui v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttribI3ui #-}
ptr_glVertexAttribI3ui :: FunPtr (GLuint -> GLuint -> GLuint -> GLuint -> IO ())
ptr_glVertexAttribI3ui = unsafePerformIO $ getCommand "glVertexAttribI3ui"

-- glVertexAttribI3uiEXT -------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttribI3uivEXT'. This command is an alias for 'glVertexAttribI3ui'.
glVertexAttribI3uiEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLuint -- ^ @x@.
  -> GLuint -- ^ @y@.
  -> GLuint -- ^ @z@.
  -> m ()
glVertexAttribI3uiEXT v1 v2 v3 v4 = liftIO $ dyn77 ptr_glVertexAttribI3uiEXT v1 v2 v3 v4

{-# NOINLINE ptr_glVertexAttribI3uiEXT #-}
ptr_glVertexAttribI3uiEXT :: FunPtr (GLuint -> GLuint -> GLuint -> GLuint -> IO ())
ptr_glVertexAttribI3uiEXT = unsafePerformIO $ getCommand "glVertexAttribI3uiEXT"

-- glVertexAttribI3uiv ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttribI3uiv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLuint -- ^ @v@ pointing to @3@ elements of type @GLuint@.
  -> m ()
glVertexAttribI3uiv v1 v2 = liftIO $ dyn194 ptr_glVertexAttribI3uiv v1 v2

{-# NOINLINE ptr_glVertexAttribI3uiv #-}
ptr_glVertexAttribI3uiv :: FunPtr (GLuint -> Ptr GLuint -> IO ())
ptr_glVertexAttribI3uiv = unsafePerformIO $ getCommand "glVertexAttribI3uiv"

-- glVertexAttribI3uivEXT ------------------------------------------------------

-- | This command is an alias for 'glVertexAttribI3uiv'.
glVertexAttribI3uivEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLuint -- ^ @v@ pointing to @3@ elements of type @GLuint@.
  -> m ()
glVertexAttribI3uivEXT v1 v2 = liftIO $ dyn194 ptr_glVertexAttribI3uivEXT v1 v2

{-# NOINLINE ptr_glVertexAttribI3uivEXT #-}
ptr_glVertexAttribI3uivEXT :: FunPtr (GLuint -> Ptr GLuint -> IO ())
ptr_glVertexAttribI3uivEXT = unsafePerformIO $ getCommand "glVertexAttribI3uivEXT"

-- glVertexAttribI4bv ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttribI4bv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLbyte -- ^ @v@ pointing to @4@ elements of type @GLbyte@.
  -> m ()
glVertexAttribI4bv v1 v2 = liftIO $ dyn827 ptr_glVertexAttribI4bv v1 v2

{-# NOINLINE ptr_glVertexAttribI4bv #-}
ptr_glVertexAttribI4bv :: FunPtr (GLuint -> Ptr GLbyte -> IO ())
ptr_glVertexAttribI4bv = unsafePerformIO $ getCommand "glVertexAttribI4bv"

-- glVertexAttribI4bvEXT -------------------------------------------------------

-- | This command is an alias for 'glVertexAttribI4bv'.
glVertexAttribI4bvEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLbyte -- ^ @v@ pointing to @4@ elements of type @GLbyte@.
  -> m ()
glVertexAttribI4bvEXT v1 v2 = liftIO $ dyn827 ptr_glVertexAttribI4bvEXT v1 v2

{-# NOINLINE ptr_glVertexAttribI4bvEXT #-}
ptr_glVertexAttribI4bvEXT :: FunPtr (GLuint -> Ptr GLbyte -> IO ())
ptr_glVertexAttribI4bvEXT = unsafePerformIO $ getCommand "glVertexAttribI4bvEXT"

-- glVertexAttribI4i -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>. The vector equivalent of this command is 'glVertexAttribI4iv'.
glVertexAttribI4i
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLint -- ^ @x@.
  -> GLint -- ^ @y@.
  -> GLint -- ^ @z@.
  -> GLint -- ^ @w@.
  -> m ()
glVertexAttribI4i v1 v2 v3 v4 v5 = liftIO $ dyn651 ptr_glVertexAttribI4i v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttribI4i #-}
ptr_glVertexAttribI4i :: FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> IO ())
ptr_glVertexAttribI4i = unsafePerformIO $ getCommand "glVertexAttribI4i"

-- glVertexAttribI4iEXT --------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttribI4ivEXT'. This command is an alias for 'glVertexAttribI4i'.
glVertexAttribI4iEXT
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLint -- ^ @x@.
  -> GLint -- ^ @y@.
  -> GLint -- ^ @z@.
  -> GLint -- ^ @w@.
  -> m ()
glVertexAttribI4iEXT v1 v2 v3 v4 v5 = liftIO $ dyn651 ptr_glVertexAttribI4iEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttribI4iEXT #-}
ptr_glVertexAttribI4iEXT :: FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> IO ())
ptr_glVertexAttribI4iEXT = unsafePerformIO $ getCommand "glVertexAttribI4iEXT"

