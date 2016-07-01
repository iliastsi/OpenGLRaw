--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F34
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

module Graphics.GL.Functions.F34 (
  glIndexd,
  glIndexdv,
  glIndexf,
  glIndexfv,
  glIndexi,
  glIndexiv,
  glIndexs,
  glIndexsv,
  glIndexub,
  glIndexubv,
  glIndexxOES,
  glIndexxvOES,
  glInitNames,
  glInsertComponentEXT,
  glInsertEventMarkerEXT,
  glInstrumentsBufferSGIX,
  glInterleavedArrays,
  glInterpolatePathsNV,
  glInvalidateBufferData,
  glInvalidateBufferSubData,
  glInvalidateFramebuffer,
  glInvalidateNamedFramebufferData,
  glInvalidateNamedFramebufferSubData,
  glInvalidateSubFramebuffer,
  glInvalidateTexImage,
  glInvalidateTexSubImage,
  glIsAsyncMarkerSGIX,
  glIsBuffer,
  glIsBufferARB,
  glIsBufferResidentNV,
  glIsCommandListNV,
  glIsEnabled,
  glIsEnabledIndexedEXT,
  glIsEnabledi,
  glIsEnablediEXT,
  glIsEnablediNV,
  glIsEnablediOES,
  glIsFenceAPPLE,
  glIsFenceNV,
  glIsFramebuffer
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

-- glIndexd --------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glIndex.xml OpenGL 2.x>. The vector equivalent of this command is 'glIndexdv'.
glIndexd
  :: MonadIO m
  => GLdouble -- ^ @c@ of type @ColorIndexValueD@.
  -> m ()
glIndexd v1 = liftIO $ dyn78 ptr_glIndexd v1

{-# NOINLINE ptr_glIndexd #-}
ptr_glIndexd :: FunPtr (GLdouble -> IO ())
ptr_glIndexd = unsafePerformIO $ getCommand "glIndexd"

-- glIndexdv -------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glIndex.xml OpenGL 2.x>.
glIndexdv
  :: MonadIO m
  => Ptr GLdouble -- ^ @c@ pointing to @1@ element of type @ColorIndexValueD@.
  -> m ()
glIndexdv v1 = liftIO $ dyn39 ptr_glIndexdv v1

{-# NOINLINE ptr_glIndexdv #-}
ptr_glIndexdv :: FunPtr (Ptr GLdouble -> IO ())
ptr_glIndexdv = unsafePerformIO $ getCommand "glIndexdv"

-- glIndexf --------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glIndex.xml OpenGL 2.x>. The vector equivalent of this command is 'glIndexfv'.
glIndexf
  :: MonadIO m
  => GLfloat -- ^ @c@ of type @ColorIndexValueF@.
  -> m ()
glIndexf v1 = liftIO $ dyn79 ptr_glIndexf v1

{-# NOINLINE ptr_glIndexf #-}
ptr_glIndexf :: FunPtr (GLfloat -> IO ())
ptr_glIndexf = unsafePerformIO $ getCommand "glIndexf"

-- glIndexfv -------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glIndex.xml OpenGL 2.x>.
glIndexfv
  :: MonadIO m
  => Ptr GLfloat -- ^ @c@ pointing to @1@ element of type @ColorIndexValueF@.
  -> m ()
glIndexfv v1 = liftIO $ dyn41 ptr_glIndexfv v1

{-# NOINLINE ptr_glIndexfv #-}
ptr_glIndexfv :: FunPtr (Ptr GLfloat -> IO ())
ptr_glIndexfv = unsafePerformIO $ getCommand "glIndexfv"

-- glIndexi --------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glIndex.xml OpenGL 2.x>. The vector equivalent of this command is 'glIndexiv'.
glIndexi
  :: MonadIO m
  => GLint -- ^ @c@ of type @ColorIndexValueI@.
  -> m ()
glIndexi v1 = liftIO $ dyn12 ptr_glIndexi v1

{-# NOINLINE ptr_glIndexi #-}
ptr_glIndexi :: FunPtr (GLint -> IO ())
ptr_glIndexi = unsafePerformIO $ getCommand "glIndexi"

-- glIndexiv -------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glIndex.xml OpenGL 2.x>.
glIndexiv
  :: MonadIO m
  => Ptr GLint -- ^ @c@ pointing to @1@ element of type @ColorIndexValueI@.
  -> m ()
glIndexiv v1 = liftIO $ dyn43 ptr_glIndexiv v1

{-# NOINLINE ptr_glIndexiv #-}
ptr_glIndexiv :: FunPtr (Ptr GLint -> IO ())
ptr_glIndexiv = unsafePerformIO $ getCommand "glIndexiv"

-- glIndexs --------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glIndex.xml OpenGL 2.x>. The vector equivalent of this command is 'glIndexsv'.
glIndexs
  :: MonadIO m
  => GLshort -- ^ @c@ of type @ColorIndexValueS@.
  -> m ()
glIndexs v1 = liftIO $ dyn463 ptr_glIndexs v1

{-# NOINLINE ptr_glIndexs #-}
ptr_glIndexs :: FunPtr (GLshort -> IO ())
ptr_glIndexs = unsafePerformIO $ getCommand "glIndexs"

-- glIndexsv -------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glIndex.xml OpenGL 2.x>.
glIndexsv
  :: MonadIO m
  => Ptr GLshort -- ^ @c@ pointing to @1@ element of type @ColorIndexValueS@.
  -> m ()
glIndexsv v1 = liftIO $ dyn45 ptr_glIndexsv v1

{-# NOINLINE ptr_glIndexsv #-}
ptr_glIndexsv :: FunPtr (Ptr GLshort -> IO ())
ptr_glIndexsv = unsafePerformIO $ getCommand "glIndexsv"

-- glIndexub -------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glIndex.xml OpenGL 2.x>. The vector equivalent of this command is 'glIndexubv'.
glIndexub
  :: MonadIO m
  => GLubyte -- ^ @c@ of type @ColorIndexValueUB@.
  -> m ()
glIndexub v1 = liftIO $ dyn464 ptr_glIndexub v1

{-# NOINLINE ptr_glIndexub #-}
ptr_glIndexub :: FunPtr (GLubyte -> IO ())
ptr_glIndexub = unsafePerformIO $ getCommand "glIndexub"

-- glIndexubv ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glIndex.xml OpenGL 2.x>.
glIndexubv
  :: MonadIO m
  => Ptr GLubyte -- ^ @c@ pointing to @1@ element of type @ColorIndexValueUB@.
  -> m ()
glIndexubv v1 = liftIO $ dyn101 ptr_glIndexubv v1

{-# NOINLINE ptr_glIndexubv #-}
ptr_glIndexubv :: FunPtr (Ptr GLubyte -> IO ())
ptr_glIndexubv = unsafePerformIO $ getCommand "glIndexubv"

-- glIndexxOES -----------------------------------------------------------------

glIndexxOES
  :: MonadIO m
  => GLfixed -- ^ @component@.
  -> m ()
glIndexxOES v1 = liftIO $ dyn81 ptr_glIndexxOES v1

{-# NOINLINE ptr_glIndexxOES #-}
ptr_glIndexxOES :: FunPtr (GLfixed -> IO ())
ptr_glIndexxOES = unsafePerformIO $ getCommand "glIndexxOES"

-- glIndexxvOES ----------------------------------------------------------------

glIndexxvOES
  :: MonadIO m
  => Ptr GLfixed -- ^ @component@ pointing to @1@ element of type @GLfixed@.
  -> m ()
glIndexxvOES v1 = liftIO $ dyn107 ptr_glIndexxvOES v1

{-# NOINLINE ptr_glIndexxvOES #-}
ptr_glIndexxvOES :: FunPtr (Ptr GLfixed -> IO ())
ptr_glIndexxvOES = unsafePerformIO $ getCommand "glIndexxvOES"

-- glInitNames -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glInitNames.xml OpenGL 2.x>.
glInitNames
  :: MonadIO m
  => m ()
glInitNames = liftIO $ dyn10 ptr_glInitNames

{-# NOINLINE ptr_glInitNames #-}
ptr_glInitNames :: FunPtr (IO ())
ptr_glInitNames = unsafePerformIO $ getCommand "glInitNames"

-- glInsertComponentEXT --------------------------------------------------------

glInsertComponentEXT
  :: MonadIO m
  => GLuint -- ^ @res@.
  -> GLuint -- ^ @src@.
  -> GLuint -- ^ @num@.
  -> m ()
glInsertComponentEXT v1 v2 v3 = liftIO $ dyn102 ptr_glInsertComponentEXT v1 v2 v3

{-# NOINLINE ptr_glInsertComponentEXT #-}
ptr_glInsertComponentEXT :: FunPtr (GLuint -> GLuint -> GLuint -> IO ())
ptr_glInsertComponentEXT = unsafePerformIO $ getCommand "glInsertComponentEXT"

-- glInsertEventMarkerEXT ------------------------------------------------------

glInsertEventMarkerEXT
  :: MonadIO m
  => GLsizei -- ^ @length@.
  -> Ptr GLchar -- ^ @marker@.
  -> m ()
glInsertEventMarkerEXT v1 v2 = liftIO $ dyn469 ptr_glInsertEventMarkerEXT v1 v2

{-# NOINLINE ptr_glInsertEventMarkerEXT #-}
ptr_glInsertEventMarkerEXT :: FunPtr (GLsizei -> Ptr GLchar -> IO ())
ptr_glInsertEventMarkerEXT = unsafePerformIO $ getCommand "glInsertEventMarkerEXT"

-- glInstrumentsBufferSGIX -----------------------------------------------------

glInstrumentsBufferSGIX
  :: MonadIO m
  => GLsizei -- ^ @size@.
  -> Ptr GLint -- ^ @buffer@ pointing to @size@ elements of type @GLint@.
  -> m ()
glInstrumentsBufferSGIX v1 v2 = liftIO $ dyn470 ptr_glInstrumentsBufferSGIX v1 v2

{-# NOINLINE ptr_glInstrumentsBufferSGIX #-}
ptr_glInstrumentsBufferSGIX :: FunPtr (GLsizei -> Ptr GLint -> IO ())
ptr_glInstrumentsBufferSGIX = unsafePerformIO $ getCommand "glInstrumentsBufferSGIX"

-- glInterleavedArrays ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glInterleavedArrays.xml OpenGL 2.x>.
glInterleavedArrays
  :: MonadIO m
  => GLenum -- ^ @format@ of type [InterleavedArrayFormat](Graphics-GL-Groups.html#InterleavedArrayFormat).
  -> GLsizei -- ^ @stride@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(format,stride)@ elements of type @a@.
  -> m ()
glInterleavedArrays v1 v2 v3 = liftIO $ dyn46 ptr_glInterleavedArrays v1 v2 v3

{-# NOINLINE ptr_glInterleavedArrays #-}
ptr_glInterleavedArrays :: FunPtr (GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glInterleavedArrays = unsafePerformIO $ getCommand "glInterleavedArrays"

-- glInterpolatePathsNV --------------------------------------------------------

glInterpolatePathsNV
  :: MonadIO m
  => GLuint -- ^ @resultPath@ of type @Path@.
  -> GLuint -- ^ @pathA@ of type @Path@.
  -> GLuint -- ^ @pathB@ of type @Path@.
  -> GLfloat -- ^ @weight@.
  -> m ()
glInterpolatePathsNV v1 v2 v3 v4 = liftIO $ dyn471 ptr_glInterpolatePathsNV v1 v2 v3 v4

{-# NOINLINE ptr_glInterpolatePathsNV #-}
ptr_glInterpolatePathsNV :: FunPtr (GLuint -> GLuint -> GLuint -> GLfloat -> IO ())
ptr_glInterpolatePathsNV = unsafePerformIO $ getCommand "glInterpolatePathsNV"

-- glInvalidateBufferData ------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glInvalidateBufferData.xhtml OpenGL 4.x>.
glInvalidateBufferData
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> m ()
glInvalidateBufferData v1 = liftIO $ dyn2 ptr_glInvalidateBufferData v1

{-# NOINLINE ptr_glInvalidateBufferData #-}
ptr_glInvalidateBufferData :: FunPtr (GLuint -> IO ())
ptr_glInvalidateBufferData = unsafePerformIO $ getCommand "glInvalidateBufferData"

-- glInvalidateBufferSubData ---------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glInvalidateBufferSubData.xhtml OpenGL 4.x>.
glInvalidateBufferSubData
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLintptr -- ^ @offset@ of type @BufferOffset@.
  -> GLsizeiptr -- ^ @length@ of type @BufferSize@.
  -> m ()
glInvalidateBufferSubData v1 v2 v3 = liftIO $ dyn279 ptr_glInvalidateBufferSubData v1 v2 v3

{-# NOINLINE ptr_glInvalidateBufferSubData #-}
ptr_glInvalidateBufferSubData :: FunPtr (GLuint -> GLintptr -> GLsizeiptr -> IO ())
ptr_glInvalidateBufferSubData = unsafePerformIO $ getCommand "glInvalidateBufferSubData"

-- glInvalidateFramebuffer -----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glInvalidateFramebuffer.xhtml OpenGL 4.x>.
glInvalidateFramebuffer
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLsizei -- ^ @numAttachments@.
  -> Ptr GLenum -- ^ @attachments@ pointing to @numAttachments@ elements of type @GLenum@.
  -> m ()
glInvalidateFramebuffer v1 v2 v3 = liftIO $ dyn226 ptr_glInvalidateFramebuffer v1 v2 v3

{-# NOINLINE ptr_glInvalidateFramebuffer #-}
ptr_glInvalidateFramebuffer :: FunPtr (GLenum -> GLsizei -> Ptr GLenum -> IO ())
ptr_glInvalidateFramebuffer = unsafePerformIO $ getCommand "glInvalidateFramebuffer"

-- glInvalidateNamedFramebufferData --------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glInvalidateFramebuffer.xhtml OpenGL 4.x>.
glInvalidateNamedFramebufferData
  :: MonadIO m
  => GLuint -- ^ @framebuffer@.
  -> GLsizei -- ^ @numAttachments@.
  -> Ptr GLenum -- ^ @attachments@.
  -> m ()
glInvalidateNamedFramebufferData v1 v2 v3 = liftIO $ dyn282 ptr_glInvalidateNamedFramebufferData v1 v2 v3

{-# NOINLINE ptr_glInvalidateNamedFramebufferData #-}
ptr_glInvalidateNamedFramebufferData :: FunPtr (GLuint -> GLsizei -> Ptr GLenum -> IO ())
ptr_glInvalidateNamedFramebufferData = unsafePerformIO $ getCommand "glInvalidateNamedFramebufferData"

-- glInvalidateNamedFramebufferSubData -----------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glInvalidateSubFramebuffer.xhtml OpenGL 4.x>.
glInvalidateNamedFramebufferSubData
  :: MonadIO m
  => GLuint -- ^ @framebuffer@.
  -> GLsizei -- ^ @numAttachments@.
  -> Ptr GLenum -- ^ @attachments@.
  -> GLint -- ^ @x@.
  -> GLint -- ^ @y@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glInvalidateNamedFramebufferSubData v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn472 ptr_glInvalidateNamedFramebufferSubData v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glInvalidateNamedFramebufferSubData #-}
ptr_glInvalidateNamedFramebufferSubData :: FunPtr (GLuint -> GLsizei -> Ptr GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> IO ())
ptr_glInvalidateNamedFramebufferSubData = unsafePerformIO $ getCommand "glInvalidateNamedFramebufferSubData"

-- glInvalidateSubFramebuffer --------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glInvalidateSubFramebuffer.xhtml OpenGL 4.x>.
glInvalidateSubFramebuffer
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLsizei -- ^ @numAttachments@.
  -> Ptr GLenum -- ^ @attachments@ pointing to @numAttachments@ elements of type @GLenum@.
  -> GLint -- ^ @x@.
  -> GLint -- ^ @y@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glInvalidateSubFramebuffer v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn473 ptr_glInvalidateSubFramebuffer v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glInvalidateSubFramebuffer #-}
ptr_glInvalidateSubFramebuffer :: FunPtr (GLenum -> GLsizei -> Ptr GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> IO ())
ptr_glInvalidateSubFramebuffer = unsafePerformIO $ getCommand "glInvalidateSubFramebuffer"

-- glInvalidateTexImage --------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glInvalidateTexImage.xhtml OpenGL 4.x>.
glInvalidateTexImage
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> m ()
glInvalidateTexImage v1 v2 = liftIO $ dyn474 ptr_glInvalidateTexImage v1 v2

{-# NOINLINE ptr_glInvalidateTexImage #-}
ptr_glInvalidateTexImage :: FunPtr (GLuint -> GLint -> IO ())
ptr_glInvalidateTexImage = unsafePerformIO $ getCommand "glInvalidateTexImage"

-- glInvalidateTexSubImage -----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glInvalidateTexSubImage.xhtml OpenGL 4.x>.
glInvalidateTexSubImage
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLint -- ^ @xoffset@.
  -> GLint -- ^ @yoffset@.
  -> GLint -- ^ @zoffset@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> m ()
glInvalidateTexSubImage v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn475 ptr_glInvalidateTexSubImage v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glInvalidateTexSubImage #-}
ptr_glInvalidateTexSubImage :: FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> IO ())
ptr_glInvalidateTexSubImage = unsafePerformIO $ getCommand "glInvalidateTexSubImage"

-- glIsAsyncMarkerSGIX ---------------------------------------------------------

glIsAsyncMarkerSGIX
  :: MonadIO m
  => GLuint -- ^ @marker@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsAsyncMarkerSGIX v1 = liftIO $ dyn273 ptr_glIsAsyncMarkerSGIX v1

{-# NOINLINE ptr_glIsAsyncMarkerSGIX #-}
ptr_glIsAsyncMarkerSGIX :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsAsyncMarkerSGIX = unsafePerformIO $ getCommand "glIsAsyncMarkerSGIX"

-- glIsBuffer ------------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glIsBuffer.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glIsBuffer.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glIsBuffer.xhtml OpenGL 4.x>.
glIsBuffer
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsBuffer v1 = liftIO $ dyn273 ptr_glIsBuffer v1

{-# NOINLINE ptr_glIsBuffer #-}
ptr_glIsBuffer :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsBuffer = unsafePerformIO $ getCommand "glIsBuffer"

-- glIsBufferARB ---------------------------------------------------------------

-- | This command is an alias for 'glIsBuffer'.
glIsBufferARB
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsBufferARB v1 = liftIO $ dyn273 ptr_glIsBufferARB v1

{-# NOINLINE ptr_glIsBufferARB #-}
ptr_glIsBufferARB :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsBufferARB = unsafePerformIO $ getCommand "glIsBufferARB"

-- glIsBufferResidentNV --------------------------------------------------------

glIsBufferResidentNV
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsBufferResidentNV v1 = liftIO $ dyn476 ptr_glIsBufferResidentNV v1

{-# NOINLINE ptr_glIsBufferResidentNV #-}
ptr_glIsBufferResidentNV :: FunPtr (GLenum -> IO GLboolean)
ptr_glIsBufferResidentNV = unsafePerformIO $ getCommand "glIsBufferResidentNV"

-- glIsCommandListNV -----------------------------------------------------------

glIsCommandListNV
  :: MonadIO m
  => GLuint -- ^ @list@.
  -> m GLboolean
glIsCommandListNV v1 = liftIO $ dyn273 ptr_glIsCommandListNV v1

{-# NOINLINE ptr_glIsCommandListNV #-}
ptr_glIsCommandListNV :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsCommandListNV = unsafePerformIO $ getCommand "glIsCommandListNV"

-- glIsEnabled -----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glIsEnabled.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glIsEnabled.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glIsEnabled.xhtml OpenGL 4.x>.
glIsEnabled
  :: MonadIO m
  => GLenum -- ^ @cap@ of type [EnableCap](Graphics-GL-Groups.html#EnableCap).
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsEnabled v1 = liftIO $ dyn476 ptr_glIsEnabled v1

{-# NOINLINE ptr_glIsEnabled #-}
ptr_glIsEnabled :: FunPtr (GLenum -> IO GLboolean)
ptr_glIsEnabled = unsafePerformIO $ getCommand "glIsEnabled"

-- glIsEnabledIndexedEXT -------------------------------------------------------

-- | This command is an alias for 'glIsEnabledi'.
glIsEnabledIndexedEXT
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLuint -- ^ @index@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsEnabledIndexedEXT v1 v2 = liftIO $ dyn477 ptr_glIsEnabledIndexedEXT v1 v2

{-# NOINLINE ptr_glIsEnabledIndexedEXT #-}
ptr_glIsEnabledIndexedEXT :: FunPtr (GLenum -> GLuint -> IO GLboolean)
ptr_glIsEnabledIndexedEXT = unsafePerformIO $ getCommand "glIsEnabledIndexedEXT"

-- glIsEnabledi ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glIsEnabled.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glIsEnabled.xhtml OpenGL 4.x>.
glIsEnabledi
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLuint -- ^ @index@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsEnabledi v1 v2 = liftIO $ dyn477 ptr_glIsEnabledi v1 v2

{-# NOINLINE ptr_glIsEnabledi #-}
ptr_glIsEnabledi :: FunPtr (GLenum -> GLuint -> IO GLboolean)
ptr_glIsEnabledi = unsafePerformIO $ getCommand "glIsEnabledi"

-- glIsEnablediEXT -------------------------------------------------------------

-- | This command is an alias for 'glIsEnabledi'.
glIsEnablediEXT
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLuint -- ^ @index@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsEnablediEXT v1 v2 = liftIO $ dyn477 ptr_glIsEnablediEXT v1 v2

{-# NOINLINE ptr_glIsEnablediEXT #-}
ptr_glIsEnablediEXT :: FunPtr (GLenum -> GLuint -> IO GLboolean)
ptr_glIsEnablediEXT = unsafePerformIO $ getCommand "glIsEnablediEXT"

-- glIsEnablediNV --------------------------------------------------------------

-- | This command is an alias for 'glIsEnabledi'.
glIsEnablediNV
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLuint -- ^ @index@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsEnablediNV v1 v2 = liftIO $ dyn477 ptr_glIsEnablediNV v1 v2

{-# NOINLINE ptr_glIsEnablediNV #-}
ptr_glIsEnablediNV :: FunPtr (GLenum -> GLuint -> IO GLboolean)
ptr_glIsEnablediNV = unsafePerformIO $ getCommand "glIsEnablediNV"

-- glIsEnablediOES -------------------------------------------------------------

-- | This command is an alias for 'glIsEnabledi'.
glIsEnablediOES
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLuint -- ^ @index@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsEnablediOES v1 v2 = liftIO $ dyn477 ptr_glIsEnablediOES v1 v2

{-# NOINLINE ptr_glIsEnablediOES #-}
ptr_glIsEnablediOES :: FunPtr (GLenum -> GLuint -> IO GLboolean)
ptr_glIsEnablediOES = unsafePerformIO $ getCommand "glIsEnablediOES"

-- glIsFenceAPPLE --------------------------------------------------------------

glIsFenceAPPLE
  :: MonadIO m
  => GLuint -- ^ @fence@ of type @FenceNV@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsFenceAPPLE v1 = liftIO $ dyn273 ptr_glIsFenceAPPLE v1

{-# NOINLINE ptr_glIsFenceAPPLE #-}
ptr_glIsFenceAPPLE :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsFenceAPPLE = unsafePerformIO $ getCommand "glIsFenceAPPLE"

-- glIsFenceNV -----------------------------------------------------------------

glIsFenceNV
  :: MonadIO m
  => GLuint -- ^ @fence@ of type @FenceNV@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsFenceNV v1 = liftIO $ dyn273 ptr_glIsFenceNV v1

{-# NOINLINE ptr_glIsFenceNV #-}
ptr_glIsFenceNV :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsFenceNV = unsafePerformIO $ getCommand "glIsFenceNV"

-- glIsFramebuffer -------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glIsFramebuffer.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glIsFramebuffer.xhtml OpenGL 4.x>.
glIsFramebuffer
  :: MonadIO m
  => GLuint -- ^ @framebuffer@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsFramebuffer v1 = liftIO $ dyn273 ptr_glIsFramebuffer v1

{-# NOINLINE ptr_glIsFramebuffer #-}
ptr_glIsFramebuffer :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsFramebuffer = unsafePerformIO $ getCommand "glIsFramebuffer"

