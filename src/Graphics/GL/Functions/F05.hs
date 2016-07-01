--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F05
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

module Graphics.GL.Functions.F05 (
  glBufferStorageEXT,
  glBufferSubData,
  glBufferSubDataARB,
  glCallCommandListNV,
  glCallList,
  glCallLists,
  glCheckFramebufferStatus,
  glCheckFramebufferStatusEXT,
  glCheckFramebufferStatusOES,
  glCheckNamedFramebufferStatus,
  glCheckNamedFramebufferStatusEXT,
  glClampColor,
  glClampColorARB,
  glClear,
  glClearAccum,
  glClearAccumxOES,
  glClearBufferData,
  glClearBufferSubData,
  glClearBufferfi,
  glClearBufferfv,
  glClearBufferiv,
  glClearBufferuiv,
  glClearColor,
  glClearColorIiEXT,
  glClearColorIuiEXT,
  glClearColorx,
  glClearColorxOES,
  glClearDepth,
  glClearDepthdNV,
  glClearDepthf,
  glClearDepthfOES,
  glClearDepthx,
  glClearDepthxOES,
  glClearIndex,
  glClearNamedBufferData,
  glClearNamedBufferDataEXT,
  glClearNamedBufferSubData,
  glClearNamedBufferSubDataEXT,
  glClearNamedFramebufferfi,
  glClearNamedFramebufferfv
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

-- glBufferStorageEXT ----------------------------------------------------------

-- | This command is an alias for 'glBufferStorage'.
glBufferStorageEXT
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLsizeiptr -- ^ @size@.
  -> Ptr a -- ^ @data@ pointing to @size@ elements of type @a@.
  -> GLbitfield -- ^ @flags@.
  -> m ()
glBufferStorageEXT v1 v2 v3 v4 = liftIO $ dyn63 ptr_glBufferStorageEXT v1 v2 v3 v4

{-# NOINLINE ptr_glBufferStorageEXT #-}
ptr_glBufferStorageEXT :: FunPtr (GLenum -> GLsizeiptr -> Ptr a -> GLbitfield -> IO ())
ptr_glBufferStorageEXT = unsafePerformIO $ getCommand "glBufferStorageEXT"

-- glBufferSubData -------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glBufferSubData.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glBufferSubData.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glBufferSubData.xhtml OpenGL 4.x>.
glBufferSubData
  :: MonadIO m
  => GLenum -- ^ @target@ of type @BufferTargetARB@.
  -> GLintptr -- ^ @offset@ of type @BufferOffset@.
  -> GLsizeiptr -- ^ @size@ of type @BufferSize@.
  -> Ptr a -- ^ @data@ pointing to @size@ elements of type @a@.
  -> m ()
glBufferSubData v1 v2 v3 v4 = liftIO $ dyn64 ptr_glBufferSubData v1 v2 v3 v4

{-# NOINLINE ptr_glBufferSubData #-}
ptr_glBufferSubData :: FunPtr (GLenum -> GLintptr -> GLsizeiptr -> Ptr a -> IO ())
ptr_glBufferSubData = unsafePerformIO $ getCommand "glBufferSubData"

-- glBufferSubDataARB ----------------------------------------------------------

-- | This command is an alias for 'glBufferSubData'.
glBufferSubDataARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @BufferTargetARB@.
  -> GLintptrARB -- ^ @offset@ of type @BufferOffsetARB@.
  -> GLsizeiptrARB -- ^ @size@ of type @BufferSizeARB@.
  -> Ptr a -- ^ @data@ pointing to @size@ elements of type @a@.
  -> m ()
glBufferSubDataARB v1 v2 v3 v4 = liftIO $ dyn65 ptr_glBufferSubDataARB v1 v2 v3 v4

{-# NOINLINE ptr_glBufferSubDataARB #-}
ptr_glBufferSubDataARB :: FunPtr (GLenum -> GLintptrARB -> GLsizeiptrARB -> Ptr a -> IO ())
ptr_glBufferSubDataARB = unsafePerformIO $ getCommand "glBufferSubDataARB"

-- glCallCommandListNV ---------------------------------------------------------

glCallCommandListNV
  :: MonadIO m
  => GLuint -- ^ @list@.
  -> m ()
glCallCommandListNV v1 = liftIO $ dyn2 ptr_glCallCommandListNV v1

{-# NOINLINE ptr_glCallCommandListNV #-}
ptr_glCallCommandListNV :: FunPtr (GLuint -> IO ())
ptr_glCallCommandListNV = unsafePerformIO $ getCommand "glCallCommandListNV"

-- glCallList ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glCallList.xml OpenGL 2.x>.
glCallList
  :: MonadIO m
  => GLuint -- ^ @list@ of type @List@.
  -> m ()
glCallList v1 = liftIO $ dyn2 ptr_glCallList v1

{-# NOINLINE ptr_glCallList #-}
ptr_glCallList :: FunPtr (GLuint -> IO ())
ptr_glCallList = unsafePerformIO $ getCommand "glCallList"

-- glCallLists -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glCallLists.xml OpenGL 2.x>.
glCallLists
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> GLenum -- ^ @type@ of type [ListNameType](Graphics-GL-Groups.html#ListNameType).
  -> Ptr a -- ^ @lists@ pointing to @COMPSIZE(n,type)@ elements of type @a@.
  -> m ()
glCallLists v1 v2 v3 = liftIO $ dyn66 ptr_glCallLists v1 v2 v3

{-# NOINLINE ptr_glCallLists #-}
ptr_glCallLists :: FunPtr (GLsizei -> GLenum -> Ptr a -> IO ())
ptr_glCallLists = unsafePerformIO $ getCommand "glCallLists"

-- glCheckFramebufferStatus ----------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glCheckFramebufferStatus.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glCheckFramebufferStatus.xhtml OpenGL 4.x>.
glCheckFramebufferStatus
  :: MonadIO m
  => GLenum -- ^ @target@ of type @FramebufferTarget@.
  -> m GLenum
glCheckFramebufferStatus v1 = liftIO $ dyn67 ptr_glCheckFramebufferStatus v1

{-# NOINLINE ptr_glCheckFramebufferStatus #-}
ptr_glCheckFramebufferStatus :: FunPtr (GLenum -> IO GLenum)
ptr_glCheckFramebufferStatus = unsafePerformIO $ getCommand "glCheckFramebufferStatus"

-- glCheckFramebufferStatusEXT -------------------------------------------------

-- | This command is an alias for 'glCheckFramebufferStatus'.
glCheckFramebufferStatusEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type @FramebufferTarget@.
  -> m GLenum
glCheckFramebufferStatusEXT v1 = liftIO $ dyn67 ptr_glCheckFramebufferStatusEXT v1

{-# NOINLINE ptr_glCheckFramebufferStatusEXT #-}
ptr_glCheckFramebufferStatusEXT :: FunPtr (GLenum -> IO GLenum)
ptr_glCheckFramebufferStatusEXT = unsafePerformIO $ getCommand "glCheckFramebufferStatusEXT"

-- glCheckFramebufferStatusOES -------------------------------------------------

glCheckFramebufferStatusOES
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> m GLenum
glCheckFramebufferStatusOES v1 = liftIO $ dyn67 ptr_glCheckFramebufferStatusOES v1

{-# NOINLINE ptr_glCheckFramebufferStatusOES #-}
ptr_glCheckFramebufferStatusOES :: FunPtr (GLenum -> IO GLenum)
ptr_glCheckFramebufferStatusOES = unsafePerformIO $ getCommand "glCheckFramebufferStatusOES"

-- glCheckNamedFramebufferStatus -----------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glCheckFramebufferStatus.xhtml OpenGL 4.x>.
glCheckNamedFramebufferStatus
  :: MonadIO m
  => GLuint -- ^ @framebuffer@.
  -> GLenum -- ^ @target@.
  -> m GLenum
glCheckNamedFramebufferStatus v1 v2 = liftIO $ dyn68 ptr_glCheckNamedFramebufferStatus v1 v2

{-# NOINLINE ptr_glCheckNamedFramebufferStatus #-}
ptr_glCheckNamedFramebufferStatus :: FunPtr (GLuint -> GLenum -> IO GLenum)
ptr_glCheckNamedFramebufferStatus = unsafePerformIO $ getCommand "glCheckNamedFramebufferStatus"

-- glCheckNamedFramebufferStatusEXT --------------------------------------------

glCheckNamedFramebufferStatusEXT
  :: MonadIO m
  => GLuint -- ^ @framebuffer@ of type @Framebuffer@.
  -> GLenum -- ^ @target@ of type @FramebufferTarget@.
  -> m GLenum -- ^ of type @FramebufferStatus@.
glCheckNamedFramebufferStatusEXT v1 v2 = liftIO $ dyn68 ptr_glCheckNamedFramebufferStatusEXT v1 v2

{-# NOINLINE ptr_glCheckNamedFramebufferStatusEXT #-}
ptr_glCheckNamedFramebufferStatusEXT :: FunPtr (GLuint -> GLenum -> IO GLenum)
ptr_glCheckNamedFramebufferStatusEXT = unsafePerformIO $ getCommand "glCheckNamedFramebufferStatusEXT"

-- glClampColor ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glClampColor.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glClampColor.xhtml OpenGL 4.x>.
glClampColor
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ClampColorTargetARB@.
  -> GLenum -- ^ @clamp@ of type @ClampColorModeARB@.
  -> m ()
glClampColor v1 v2 = liftIO $ dyn51 ptr_glClampColor v1 v2

{-# NOINLINE ptr_glClampColor #-}
ptr_glClampColor :: FunPtr (GLenum -> GLenum -> IO ())
ptr_glClampColor = unsafePerformIO $ getCommand "glClampColor"

-- glClampColorARB -------------------------------------------------------------

-- | This command is an alias for 'glClampColor'.
glClampColorARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ClampColorTargetARB@.
  -> GLenum -- ^ @clamp@ of type @ClampColorModeARB@.
  -> m ()
glClampColorARB v1 v2 = liftIO $ dyn51 ptr_glClampColorARB v1 v2

{-# NOINLINE ptr_glClampColorARB #-}
ptr_glClampColorARB :: FunPtr (GLenum -> GLenum -> IO ())
ptr_glClampColorARB = unsafePerformIO $ getCommand "glClampColorARB"

-- glClear ---------------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glClear.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glClear.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glClear.xhtml OpenGL 4.x>.
glClear
  :: MonadIO m
  => GLbitfield -- ^ @mask@ of type [ClearBufferMask](Graphics-GL-Groups.html#ClearBufferMask).
  -> m ()
glClear v1 = liftIO $ dyn69 ptr_glClear v1

{-# NOINLINE ptr_glClear #-}
ptr_glClear :: FunPtr (GLbitfield -> IO ())
ptr_glClear = unsafePerformIO $ getCommand "glClear"

-- glClearAccum ----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glClearAccum.xml OpenGL 2.x>.
glClearAccum
  :: MonadIO m
  => GLfloat -- ^ @red@.
  -> GLfloat -- ^ @green@.
  -> GLfloat -- ^ @blue@.
  -> GLfloat -- ^ @alpha@.
  -> m ()
glClearAccum v1 v2 v3 v4 = liftIO $ dyn49 ptr_glClearAccum v1 v2 v3 v4

{-# NOINLINE ptr_glClearAccum #-}
ptr_glClearAccum :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glClearAccum = unsafePerformIO $ getCommand "glClearAccum"

-- glClearAccumxOES ------------------------------------------------------------

glClearAccumxOES
  :: MonadIO m
  => GLfixed -- ^ @red@ of type @ClampedFixed@.
  -> GLfixed -- ^ @green@ of type @ClampedFixed@.
  -> GLfixed -- ^ @blue@ of type @ClampedFixed@.
  -> GLfixed -- ^ @alpha@ of type @ClampedFixed@.
  -> m ()
glClearAccumxOES v1 v2 v3 v4 = liftIO $ dyn50 ptr_glClearAccumxOES v1 v2 v3 v4

{-# NOINLINE ptr_glClearAccumxOES #-}
ptr_glClearAccumxOES :: FunPtr (GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ())
ptr_glClearAccumxOES = unsafePerformIO $ getCommand "glClearAccumxOES"

-- glClearBufferData -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glClearBufferData.xhtml OpenGL 4.x>.
glClearBufferData
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @internalformat@.
  -> GLenum -- ^ @format@.
  -> GLenum -- ^ @type@.
  -> Ptr a -- ^ @data@ pointing to @COMPSIZE(format,type)@ elements of type @a@.
  -> m ()
glClearBufferData v1 v2 v3 v4 v5 = liftIO $ dyn70 ptr_glClearBufferData v1 v2 v3 v4 v5

{-# NOINLINE ptr_glClearBufferData #-}
ptr_glClearBufferData :: FunPtr (GLenum -> GLenum -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glClearBufferData = unsafePerformIO $ getCommand "glClearBufferData"

-- glClearBufferSubData --------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glClearBufferSubData.xhtml OpenGL 4.x>.
glClearBufferSubData
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @internalformat@.
  -> GLintptr -- ^ @offset@ of type @BufferOffset@.
  -> GLsizeiptr -- ^ @size@ of type @BufferSize@.
  -> GLenum -- ^ @format@.
  -> GLenum -- ^ @type@.
  -> Ptr a -- ^ @data@ pointing to @COMPSIZE(format,type)@ elements of type @a@.
  -> m ()
glClearBufferSubData v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn71 ptr_glClearBufferSubData v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glClearBufferSubData #-}
ptr_glClearBufferSubData :: FunPtr (GLenum -> GLenum -> GLintptr -> GLsizeiptr -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glClearBufferSubData = unsafePerformIO $ getCommand "glClearBufferSubData"

-- glClearBufferfi -------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glClearBuffer.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glClearBuffer.xhtml OpenGL 4.x>.
glClearBufferfi
  :: MonadIO m
  => GLenum -- ^ @buffer@.
  -> GLint -- ^ @drawbuffer@ of type @DrawBufferName@.
  -> GLfloat -- ^ @depth@.
  -> GLint -- ^ @stencil@.
  -> m ()
glClearBufferfi v1 v2 v3 v4 = liftIO $ dyn72 ptr_glClearBufferfi v1 v2 v3 v4

{-# NOINLINE ptr_glClearBufferfi #-}
ptr_glClearBufferfi :: FunPtr (GLenum -> GLint -> GLfloat -> GLint -> IO ())
ptr_glClearBufferfi = unsafePerformIO $ getCommand "glClearBufferfi"

-- glClearBufferfv -------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glClearBuffer.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glClearBuffer.xhtml OpenGL 4.x>.
glClearBufferfv
  :: MonadIO m
  => GLenum -- ^ @buffer@.
  -> GLint -- ^ @drawbuffer@ of type @DrawBufferName@.
  -> Ptr GLfloat -- ^ @value@ pointing to @COMPSIZE(buffer)@ elements of type @GLfloat@.
  -> m ()
glClearBufferfv v1 v2 v3 = liftIO $ dyn73 ptr_glClearBufferfv v1 v2 v3

{-# NOINLINE ptr_glClearBufferfv #-}
ptr_glClearBufferfv :: FunPtr (GLenum -> GLint -> Ptr GLfloat -> IO ())
ptr_glClearBufferfv = unsafePerformIO $ getCommand "glClearBufferfv"

-- glClearBufferiv -------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glClearBuffer.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glClearBuffer.xhtml OpenGL 4.x>.
glClearBufferiv
  :: MonadIO m
  => GLenum -- ^ @buffer@.
  -> GLint -- ^ @drawbuffer@ of type @DrawBufferName@.
  -> Ptr GLint -- ^ @value@ pointing to @COMPSIZE(buffer)@ elements of type @GLint@.
  -> m ()
glClearBufferiv v1 v2 v3 = liftIO $ dyn74 ptr_glClearBufferiv v1 v2 v3

{-# NOINLINE ptr_glClearBufferiv #-}
ptr_glClearBufferiv :: FunPtr (GLenum -> GLint -> Ptr GLint -> IO ())
ptr_glClearBufferiv = unsafePerformIO $ getCommand "glClearBufferiv"

-- glClearBufferuiv ------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glClearBuffer.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glClearBuffer.xhtml OpenGL 4.x>.
glClearBufferuiv
  :: MonadIO m
  => GLenum -- ^ @buffer@.
  -> GLint -- ^ @drawbuffer@ of type @DrawBufferName@.
  -> Ptr GLuint -- ^ @value@ pointing to @COMPSIZE(buffer)@ elements of type @GLuint@.
  -> m ()
glClearBufferuiv v1 v2 v3 = liftIO $ dyn75 ptr_glClearBufferuiv v1 v2 v3

{-# NOINLINE ptr_glClearBufferuiv #-}
ptr_glClearBufferuiv :: FunPtr (GLenum -> GLint -> Ptr GLuint -> IO ())
ptr_glClearBufferuiv = unsafePerformIO $ getCommand "glClearBufferuiv"

-- glClearColor ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glClearColor.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glClearColor.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glClearColor.xhtml OpenGL 4.x>.
glClearColor
  :: MonadIO m
  => GLfloat -- ^ @red@ of type @ColorF@.
  -> GLfloat -- ^ @green@ of type @ColorF@.
  -> GLfloat -- ^ @blue@ of type @ColorF@.
  -> GLfloat -- ^ @alpha@ of type @ColorF@.
  -> m ()
glClearColor v1 v2 v3 v4 = liftIO $ dyn49 ptr_glClearColor v1 v2 v3 v4

{-# NOINLINE ptr_glClearColor #-}
ptr_glClearColor :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glClearColor = unsafePerformIO $ getCommand "glClearColor"

-- glClearColorIiEXT -----------------------------------------------------------

glClearColorIiEXT
  :: MonadIO m
  => GLint -- ^ @red@.
  -> GLint -- ^ @green@.
  -> GLint -- ^ @blue@.
  -> GLint -- ^ @alpha@.
  -> m ()
glClearColorIiEXT v1 v2 v3 v4 = liftIO $ dyn76 ptr_glClearColorIiEXT v1 v2 v3 v4

{-# NOINLINE ptr_glClearColorIiEXT #-}
ptr_glClearColorIiEXT :: FunPtr (GLint -> GLint -> GLint -> GLint -> IO ())
ptr_glClearColorIiEXT = unsafePerformIO $ getCommand "glClearColorIiEXT"

-- glClearColorIuiEXT ----------------------------------------------------------

glClearColorIuiEXT
  :: MonadIO m
  => GLuint -- ^ @red@.
  -> GLuint -- ^ @green@.
  -> GLuint -- ^ @blue@.
  -> GLuint -- ^ @alpha@.
  -> m ()
glClearColorIuiEXT v1 v2 v3 v4 = liftIO $ dyn77 ptr_glClearColorIuiEXT v1 v2 v3 v4

{-# NOINLINE ptr_glClearColorIuiEXT #-}
ptr_glClearColorIuiEXT :: FunPtr (GLuint -> GLuint -> GLuint -> GLuint -> IO ())
ptr_glClearColorIuiEXT = unsafePerformIO $ getCommand "glClearColorIuiEXT"

-- glClearColorx ---------------------------------------------------------------

glClearColorx
  :: MonadIO m
  => GLfixed -- ^ @red@.
  -> GLfixed -- ^ @green@.
  -> GLfixed -- ^ @blue@.
  -> GLfixed -- ^ @alpha@.
  -> m ()
glClearColorx v1 v2 v3 v4 = liftIO $ dyn50 ptr_glClearColorx v1 v2 v3 v4

{-# NOINLINE ptr_glClearColorx #-}
ptr_glClearColorx :: FunPtr (GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ())
ptr_glClearColorx = unsafePerformIO $ getCommand "glClearColorx"

-- glClearColorxOES ------------------------------------------------------------

glClearColorxOES
  :: MonadIO m
  => GLfixed -- ^ @red@ of type @ClampedFixed@.
  -> GLfixed -- ^ @green@ of type @ClampedFixed@.
  -> GLfixed -- ^ @blue@ of type @ClampedFixed@.
  -> GLfixed -- ^ @alpha@ of type @ClampedFixed@.
  -> m ()
glClearColorxOES v1 v2 v3 v4 = liftIO $ dyn50 ptr_glClearColorxOES v1 v2 v3 v4

{-# NOINLINE ptr_glClearColorxOES #-}
ptr_glClearColorxOES :: FunPtr (GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ())
ptr_glClearColorxOES = unsafePerformIO $ getCommand "glClearColorxOES"

-- glClearDepth ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glClearDepth.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glClearDepth.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glClearDepth.xhtml OpenGL 4.x>.
glClearDepth
  :: MonadIO m
  => GLdouble -- ^ @depth@.
  -> m ()
glClearDepth v1 = liftIO $ dyn78 ptr_glClearDepth v1

{-# NOINLINE ptr_glClearDepth #-}
ptr_glClearDepth :: FunPtr (GLdouble -> IO ())
ptr_glClearDepth = unsafePerformIO $ getCommand "glClearDepth"

-- glClearDepthdNV -------------------------------------------------------------

glClearDepthdNV
  :: MonadIO m
  => GLdouble -- ^ @depth@.
  -> m ()
glClearDepthdNV v1 = liftIO $ dyn78 ptr_glClearDepthdNV v1

{-# NOINLINE ptr_glClearDepthdNV #-}
ptr_glClearDepthdNV :: FunPtr (GLdouble -> IO ())
ptr_glClearDepthdNV = unsafePerformIO $ getCommand "glClearDepthdNV"

-- glClearDepthf ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glClearDepth.xhtml OpenGL 4.x>.
glClearDepthf
  :: MonadIO m
  => GLfloat -- ^ @d@.
  -> m ()
glClearDepthf v1 = liftIO $ dyn79 ptr_glClearDepthf v1

{-# NOINLINE ptr_glClearDepthf #-}
ptr_glClearDepthf :: FunPtr (GLfloat -> IO ())
ptr_glClearDepthf = unsafePerformIO $ getCommand "glClearDepthf"

-- glClearDepthfOES ------------------------------------------------------------

-- | This command is an alias for 'glClearDepthf'.
glClearDepthfOES
  :: MonadIO m
  => GLclampf -- ^ @depth@ of type @ClampedFloat32@.
  -> m ()
glClearDepthfOES v1 = liftIO $ dyn80 ptr_glClearDepthfOES v1

{-# NOINLINE ptr_glClearDepthfOES #-}
ptr_glClearDepthfOES :: FunPtr (GLclampf -> IO ())
ptr_glClearDepthfOES = unsafePerformIO $ getCommand "glClearDepthfOES"

-- glClearDepthx ---------------------------------------------------------------

glClearDepthx
  :: MonadIO m
  => GLfixed -- ^ @depth@.
  -> m ()
glClearDepthx v1 = liftIO $ dyn81 ptr_glClearDepthx v1

{-# NOINLINE ptr_glClearDepthx #-}
ptr_glClearDepthx :: FunPtr (GLfixed -> IO ())
ptr_glClearDepthx = unsafePerformIO $ getCommand "glClearDepthx"

-- glClearDepthxOES ------------------------------------------------------------

glClearDepthxOES
  :: MonadIO m
  => GLfixed -- ^ @depth@ of type @ClampedFixed@.
  -> m ()
glClearDepthxOES v1 = liftIO $ dyn81 ptr_glClearDepthxOES v1

{-# NOINLINE ptr_glClearDepthxOES #-}
ptr_glClearDepthxOES :: FunPtr (GLfixed -> IO ())
ptr_glClearDepthxOES = unsafePerformIO $ getCommand "glClearDepthxOES"

-- glClearIndex ----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glClearIndex.xml OpenGL 2.x>.
glClearIndex
  :: MonadIO m
  => GLfloat -- ^ @c@ of type @MaskedColorIndexValueF@.
  -> m ()
glClearIndex v1 = liftIO $ dyn79 ptr_glClearIndex v1

{-# NOINLINE ptr_glClearIndex #-}
ptr_glClearIndex :: FunPtr (GLfloat -> IO ())
ptr_glClearIndex = unsafePerformIO $ getCommand "glClearIndex"

-- glClearNamedBufferData ------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glClearBufferData.xhtml OpenGL 4.x>.
glClearNamedBufferData
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLenum -- ^ @internalformat@.
  -> GLenum -- ^ @format@.
  -> GLenum -- ^ @type@.
  -> Ptr a -- ^ @data@.
  -> m ()
glClearNamedBufferData v1 v2 v3 v4 v5 = liftIO $ dyn82 ptr_glClearNamedBufferData v1 v2 v3 v4 v5

{-# NOINLINE ptr_glClearNamedBufferData #-}
ptr_glClearNamedBufferData :: FunPtr (GLuint -> GLenum -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glClearNamedBufferData = unsafePerformIO $ getCommand "glClearNamedBufferData"

-- glClearNamedBufferDataEXT ---------------------------------------------------

glClearNamedBufferDataEXT
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLenum -- ^ @internalformat@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @data@ pointing to @COMPSIZE(format,type)@ elements of type @a@.
  -> m ()
glClearNamedBufferDataEXT v1 v2 v3 v4 v5 = liftIO $ dyn82 ptr_glClearNamedBufferDataEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glClearNamedBufferDataEXT #-}
ptr_glClearNamedBufferDataEXT :: FunPtr (GLuint -> GLenum -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glClearNamedBufferDataEXT = unsafePerformIO $ getCommand "glClearNamedBufferDataEXT"

-- glClearNamedBufferSubData ---------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glClearBufferSubData.xhtml OpenGL 4.x>.
glClearNamedBufferSubData
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLenum -- ^ @internalformat@.
  -> GLintptr -- ^ @offset@.
  -> GLsizeiptr -- ^ @size@ of type @BufferSize@.
  -> GLenum -- ^ @format@.
  -> GLenum -- ^ @type@.
  -> Ptr a -- ^ @data@.
  -> m ()
glClearNamedBufferSubData v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn83 ptr_glClearNamedBufferSubData v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glClearNamedBufferSubData #-}
ptr_glClearNamedBufferSubData :: FunPtr (GLuint -> GLenum -> GLintptr -> GLsizeiptr -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glClearNamedBufferSubData = unsafePerformIO $ getCommand "glClearNamedBufferSubData"

-- glClearNamedBufferSubDataEXT ------------------------------------------------

glClearNamedBufferSubDataEXT
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLenum -- ^ @internalformat@.
  -> GLsizeiptr -- ^ @offset@ of type @BufferSize@.
  -> GLsizeiptr -- ^ @size@ of type @BufferSize@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @data@ pointing to @COMPSIZE(format,type)@ elements of type @a@.
  -> m ()
glClearNamedBufferSubDataEXT v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn84 ptr_glClearNamedBufferSubDataEXT v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glClearNamedBufferSubDataEXT #-}
ptr_glClearNamedBufferSubDataEXT :: FunPtr (GLuint -> GLenum -> GLsizeiptr -> GLsizeiptr -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glClearNamedBufferSubDataEXT = unsafePerformIO $ getCommand "glClearNamedBufferSubDataEXT"

-- glClearNamedFramebufferfi ---------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glClearBuffer.xhtml OpenGL 4.x>.
glClearNamedFramebufferfi
  :: MonadIO m
  => GLuint -- ^ @framebuffer@.
  -> GLenum -- ^ @buffer@.
  -> GLint -- ^ @drawbuffer@.
  -> GLfloat -- ^ @depth@.
  -> GLint -- ^ @stencil@.
  -> m ()
glClearNamedFramebufferfi v1 v2 v3 v4 v5 = liftIO $ dyn85 ptr_glClearNamedFramebufferfi v1 v2 v3 v4 v5

{-# NOINLINE ptr_glClearNamedFramebufferfi #-}
ptr_glClearNamedFramebufferfi :: FunPtr (GLuint -> GLenum -> GLint -> GLfloat -> GLint -> IO ())
ptr_glClearNamedFramebufferfi = unsafePerformIO $ getCommand "glClearNamedFramebufferfi"

-- glClearNamedFramebufferfv ---------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glClearBuffer.xhtml OpenGL 4.x>.
glClearNamedFramebufferfv
  :: MonadIO m
  => GLuint -- ^ @framebuffer@.
  -> GLenum -- ^ @buffer@.
  -> GLint -- ^ @drawbuffer@.
  -> Ptr GLfloat -- ^ @value@.
  -> m ()
glClearNamedFramebufferfv v1 v2 v3 v4 = liftIO $ dyn86 ptr_glClearNamedFramebufferfv v1 v2 v3 v4

{-# NOINLINE ptr_glClearNamedFramebufferfv #-}
ptr_glClearNamedFramebufferfv :: FunPtr (GLuint -> GLenum -> GLint -> Ptr GLfloat -> IO ())
ptr_glClearNamedFramebufferfv = unsafePerformIO $ getCommand "glClearNamedFramebufferfv"

