--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F14
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

module Graphics.GL.Functions.F14 (
  glDispatchComputeGroupSizeARB,
  glDispatchComputeIndirect,
  glDrawArrays,
  glDrawArraysEXT,
  glDrawArraysIndirect,
  glDrawArraysInstanced,
  glDrawArraysInstancedANGLE,
  glDrawArraysInstancedARB,
  glDrawArraysInstancedBaseInstance,
  glDrawArraysInstancedBaseInstanceEXT,
  glDrawArraysInstancedEXT,
  glDrawArraysInstancedNV,
  glDrawBuffer,
  glDrawBuffers,
  glDrawBuffersARB,
  glDrawBuffersATI,
  glDrawBuffersEXT,
  glDrawBuffersIndexedEXT,
  glDrawBuffersNV,
  glDrawCommandsAddressNV,
  glDrawCommandsNV,
  glDrawCommandsStatesAddressNV,
  glDrawCommandsStatesNV,
  glDrawElementArrayAPPLE,
  glDrawElementArrayATI,
  glDrawElements,
  glDrawElementsBaseVertex,
  glDrawElementsBaseVertexEXT,
  glDrawElementsBaseVertexOES,
  glDrawElementsIndirect,
  glDrawElementsInstanced,
  glDrawElementsInstancedANGLE,
  glDrawElementsInstancedARB,
  glDrawElementsInstancedBaseInstance,
  glDrawElementsInstancedBaseInstanceEXT,
  glDrawElementsInstancedBaseVertex,
  glDrawElementsInstancedBaseVertexBaseInstance,
  glDrawElementsInstancedBaseVertexBaseInstanceEXT,
  glDrawElementsInstancedBaseVertexEXT,
  glDrawElementsInstancedBaseVertexOES
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

-- glDispatchComputeGroupSizeARB -----------------------------------------------

glDispatchComputeGroupSizeARB
  :: MonadIO m
  => GLuint -- ^ @num_groups_x@.
  -> GLuint -- ^ @num_groups_y@.
  -> GLuint -- ^ @num_groups_z@.
  -> GLuint -- ^ @group_size_x@.
  -> GLuint -- ^ @group_size_y@.
  -> GLuint -- ^ @group_size_z@.
  -> m ()
glDispatchComputeGroupSizeARB v1 v2 v3 v4 v5 v6 = liftIO $ dyn227 ptr_glDispatchComputeGroupSizeARB v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glDispatchComputeGroupSizeARB #-}
ptr_glDispatchComputeGroupSizeARB :: FunPtr (GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ())
ptr_glDispatchComputeGroupSizeARB = unsafePerformIO $ getCommand "glDispatchComputeGroupSizeARB"

-- glDispatchComputeIndirect ---------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glDispatchComputeIndirect.xhtml OpenGL 4.x>.
glDispatchComputeIndirect
  :: MonadIO m
  => GLintptr -- ^ @indirect@ of type @BufferOffset@.
  -> m ()
glDispatchComputeIndirect v1 = liftIO $ dyn228 ptr_glDispatchComputeIndirect v1

{-# NOINLINE ptr_glDispatchComputeIndirect #-}
ptr_glDispatchComputeIndirect :: FunPtr (GLintptr -> IO ())
ptr_glDispatchComputeIndirect = unsafePerformIO $ getCommand "glDispatchComputeIndirect"

-- glDrawArrays ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glDrawArrays.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glDrawArrays.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glDrawArrays.xhtml OpenGL 4.x>.
glDrawArrays
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLint -- ^ @first@.
  -> GLsizei -- ^ @count@.
  -> m ()
glDrawArrays v1 v2 v3 = liftIO $ dyn229 ptr_glDrawArrays v1 v2 v3

{-# NOINLINE ptr_glDrawArrays #-}
ptr_glDrawArrays :: FunPtr (GLenum -> GLint -> GLsizei -> IO ())
ptr_glDrawArrays = unsafePerformIO $ getCommand "glDrawArrays"

-- glDrawArraysEXT -------------------------------------------------------------

-- | This command is an alias for 'glDrawArrays'.
glDrawArraysEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLint -- ^ @first@.
  -> GLsizei -- ^ @count@.
  -> m ()
glDrawArraysEXT v1 v2 v3 = liftIO $ dyn229 ptr_glDrawArraysEXT v1 v2 v3

{-# NOINLINE ptr_glDrawArraysEXT #-}
ptr_glDrawArraysEXT :: FunPtr (GLenum -> GLint -> GLsizei -> IO ())
ptr_glDrawArraysEXT = unsafePerformIO $ getCommand "glDrawArraysEXT"

-- glDrawArraysIndirect --------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glDrawArraysIndirect.xhtml OpenGL 4.x>.
glDrawArraysIndirect
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> Ptr a -- ^ @indirect@.
  -> m ()
glDrawArraysIndirect v1 v2 = liftIO $ dyn230 ptr_glDrawArraysIndirect v1 v2

{-# NOINLINE ptr_glDrawArraysIndirect #-}
ptr_glDrawArraysIndirect :: FunPtr (GLenum -> Ptr a -> IO ())
ptr_glDrawArraysIndirect = unsafePerformIO $ getCommand "glDrawArraysIndirect"

-- glDrawArraysInstanced -------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glDrawArraysInstanced.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glDrawArraysInstanced.xhtml OpenGL 4.x>.
glDrawArraysInstanced
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLint -- ^ @first@.
  -> GLsizei -- ^ @count@.
  -> GLsizei -- ^ @instancecount@.
  -> m ()
glDrawArraysInstanced v1 v2 v3 v4 = liftIO $ dyn231 ptr_glDrawArraysInstanced v1 v2 v3 v4

{-# NOINLINE ptr_glDrawArraysInstanced #-}
ptr_glDrawArraysInstanced :: FunPtr (GLenum -> GLint -> GLsizei -> GLsizei -> IO ())
ptr_glDrawArraysInstanced = unsafePerformIO $ getCommand "glDrawArraysInstanced"

-- glDrawArraysInstancedANGLE --------------------------------------------------

-- | This command is an alias for 'glDrawArraysInstanced'.
glDrawArraysInstancedANGLE
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLint -- ^ @first@.
  -> GLsizei -- ^ @count@.
  -> GLsizei -- ^ @primcount@.
  -> m ()
glDrawArraysInstancedANGLE v1 v2 v3 v4 = liftIO $ dyn231 ptr_glDrawArraysInstancedANGLE v1 v2 v3 v4

{-# NOINLINE ptr_glDrawArraysInstancedANGLE #-}
ptr_glDrawArraysInstancedANGLE :: FunPtr (GLenum -> GLint -> GLsizei -> GLsizei -> IO ())
ptr_glDrawArraysInstancedANGLE = unsafePerformIO $ getCommand "glDrawArraysInstancedANGLE"

-- glDrawArraysInstancedARB ----------------------------------------------------

-- | This command is an alias for 'glDrawArraysInstanced'.
glDrawArraysInstancedARB
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLint -- ^ @first@.
  -> GLsizei -- ^ @count@.
  -> GLsizei -- ^ @primcount@.
  -> m ()
glDrawArraysInstancedARB v1 v2 v3 v4 = liftIO $ dyn231 ptr_glDrawArraysInstancedARB v1 v2 v3 v4

{-# NOINLINE ptr_glDrawArraysInstancedARB #-}
ptr_glDrawArraysInstancedARB :: FunPtr (GLenum -> GLint -> GLsizei -> GLsizei -> IO ())
ptr_glDrawArraysInstancedARB = unsafePerformIO $ getCommand "glDrawArraysInstancedARB"

-- glDrawArraysInstancedBaseInstance -------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glDrawArraysInstancedBaseInstance.xhtml OpenGL 4.x>.
glDrawArraysInstancedBaseInstance
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLint -- ^ @first@.
  -> GLsizei -- ^ @count@.
  -> GLsizei -- ^ @instancecount@.
  -> GLuint -- ^ @baseinstance@.
  -> m ()
glDrawArraysInstancedBaseInstance v1 v2 v3 v4 v5 = liftIO $ dyn232 ptr_glDrawArraysInstancedBaseInstance v1 v2 v3 v4 v5

{-# NOINLINE ptr_glDrawArraysInstancedBaseInstance #-}
ptr_glDrawArraysInstancedBaseInstance :: FunPtr (GLenum -> GLint -> GLsizei -> GLsizei -> GLuint -> IO ())
ptr_glDrawArraysInstancedBaseInstance = unsafePerformIO $ getCommand "glDrawArraysInstancedBaseInstance"

-- glDrawArraysInstancedBaseInstanceEXT ----------------------------------------

-- | This command is an alias for 'glDrawArraysInstancedBaseInstance'.
glDrawArraysInstancedBaseInstanceEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLint -- ^ @first@.
  -> GLsizei -- ^ @count@.
  -> GLsizei -- ^ @instancecount@.
  -> GLuint -- ^ @baseinstance@.
  -> m ()
glDrawArraysInstancedBaseInstanceEXT v1 v2 v3 v4 v5 = liftIO $ dyn232 ptr_glDrawArraysInstancedBaseInstanceEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glDrawArraysInstancedBaseInstanceEXT #-}
ptr_glDrawArraysInstancedBaseInstanceEXT :: FunPtr (GLenum -> GLint -> GLsizei -> GLsizei -> GLuint -> IO ())
ptr_glDrawArraysInstancedBaseInstanceEXT = unsafePerformIO $ getCommand "glDrawArraysInstancedBaseInstanceEXT"

-- glDrawArraysInstancedEXT ----------------------------------------------------

-- | This command is an alias for 'glDrawArraysInstanced'.
glDrawArraysInstancedEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLint -- ^ @start@.
  -> GLsizei -- ^ @count@.
  -> GLsizei -- ^ @primcount@.
  -> m ()
glDrawArraysInstancedEXT v1 v2 v3 v4 = liftIO $ dyn231 ptr_glDrawArraysInstancedEXT v1 v2 v3 v4

{-# NOINLINE ptr_glDrawArraysInstancedEXT #-}
ptr_glDrawArraysInstancedEXT :: FunPtr (GLenum -> GLint -> GLsizei -> GLsizei -> IO ())
ptr_glDrawArraysInstancedEXT = unsafePerformIO $ getCommand "glDrawArraysInstancedEXT"

-- glDrawArraysInstancedNV -----------------------------------------------------

-- | This command is an alias for 'glDrawArraysInstanced'.
glDrawArraysInstancedNV
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLint -- ^ @first@.
  -> GLsizei -- ^ @count@.
  -> GLsizei -- ^ @primcount@.
  -> m ()
glDrawArraysInstancedNV v1 v2 v3 v4 = liftIO $ dyn231 ptr_glDrawArraysInstancedNV v1 v2 v3 v4

{-# NOINLINE ptr_glDrawArraysInstancedNV #-}
ptr_glDrawArraysInstancedNV :: FunPtr (GLenum -> GLint -> GLsizei -> GLsizei -> IO ())
ptr_glDrawArraysInstancedNV = unsafePerformIO $ getCommand "glDrawArraysInstancedNV"

-- glDrawBuffer ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glDrawBuffer.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glDrawBuffer.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glDrawBuffer.xhtml OpenGL 4.x>.
glDrawBuffer
  :: MonadIO m
  => GLenum -- ^ @buf@ of type [DrawBufferMode](Graphics-GL-Groups.html#DrawBufferMode).
  -> m ()
glDrawBuffer v1 = liftIO $ dyn4 ptr_glDrawBuffer v1

{-# NOINLINE ptr_glDrawBuffer #-}
ptr_glDrawBuffer :: FunPtr (GLenum -> IO ())
ptr_glDrawBuffer = unsafePerformIO $ getCommand "glDrawBuffer"

-- glDrawBuffers ---------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glDrawBuffers.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glDrawBuffers.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glDrawBuffers.xhtml OpenGL 4.x>.
glDrawBuffers
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLenum -- ^ @bufs@ pointing to @n@ elements of type @DrawBufferModeATI@.
  -> m ()
glDrawBuffers v1 v2 = liftIO $ dyn233 ptr_glDrawBuffers v1 v2

{-# NOINLINE ptr_glDrawBuffers #-}
ptr_glDrawBuffers :: FunPtr (GLsizei -> Ptr GLenum -> IO ())
ptr_glDrawBuffers = unsafePerformIO $ getCommand "glDrawBuffers"

-- glDrawBuffersARB ------------------------------------------------------------

-- | This command is an alias for 'glDrawBuffers'.
glDrawBuffersARB
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLenum -- ^ @bufs@ pointing to @n@ elements of type @DrawBufferModeATI@.
  -> m ()
glDrawBuffersARB v1 v2 = liftIO $ dyn233 ptr_glDrawBuffersARB v1 v2

{-# NOINLINE ptr_glDrawBuffersARB #-}
ptr_glDrawBuffersARB :: FunPtr (GLsizei -> Ptr GLenum -> IO ())
ptr_glDrawBuffersARB = unsafePerformIO $ getCommand "glDrawBuffersARB"

-- glDrawBuffersATI ------------------------------------------------------------

-- | This command is an alias for 'glDrawBuffers'.
glDrawBuffersATI
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLenum -- ^ @bufs@ pointing to @n@ elements of type @DrawBufferModeATI@.
  -> m ()
glDrawBuffersATI v1 v2 = liftIO $ dyn233 ptr_glDrawBuffersATI v1 v2

{-# NOINLINE ptr_glDrawBuffersATI #-}
ptr_glDrawBuffersATI :: FunPtr (GLsizei -> Ptr GLenum -> IO ())
ptr_glDrawBuffersATI = unsafePerformIO $ getCommand "glDrawBuffersATI"

-- glDrawBuffersEXT ------------------------------------------------------------

-- | This command is an alias for 'glDrawBuffers'.
glDrawBuffersEXT
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLenum -- ^ @bufs@.
  -> m ()
glDrawBuffersEXT v1 v2 = liftIO $ dyn233 ptr_glDrawBuffersEXT v1 v2

{-# NOINLINE ptr_glDrawBuffersEXT #-}
ptr_glDrawBuffersEXT :: FunPtr (GLsizei -> Ptr GLenum -> IO ())
ptr_glDrawBuffersEXT = unsafePerformIO $ getCommand "glDrawBuffersEXT"

-- glDrawBuffersIndexedEXT -----------------------------------------------------

glDrawBuffersIndexedEXT
  :: MonadIO m
  => GLint -- ^ @n@.
  -> Ptr GLenum -- ^ @location@ pointing to @n@ elements of type @GLenum@.
  -> Ptr GLint -- ^ @indices@ pointing to @n@ elements of type @GLint@.
  -> m ()
glDrawBuffersIndexedEXT v1 v2 v3 = liftIO $ dyn234 ptr_glDrawBuffersIndexedEXT v1 v2 v3

{-# NOINLINE ptr_glDrawBuffersIndexedEXT #-}
ptr_glDrawBuffersIndexedEXT :: FunPtr (GLint -> Ptr GLenum -> Ptr GLint -> IO ())
ptr_glDrawBuffersIndexedEXT = unsafePerformIO $ getCommand "glDrawBuffersIndexedEXT"

-- glDrawBuffersNV -------------------------------------------------------------

glDrawBuffersNV
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLenum -- ^ @bufs@ pointing to @n@ elements of type @GLenum@.
  -> m ()
glDrawBuffersNV v1 v2 = liftIO $ dyn233 ptr_glDrawBuffersNV v1 v2

{-# NOINLINE ptr_glDrawBuffersNV #-}
ptr_glDrawBuffersNV :: FunPtr (GLsizei -> Ptr GLenum -> IO ())
ptr_glDrawBuffersNV = unsafePerformIO $ getCommand "glDrawBuffersNV"

-- glDrawCommandsAddressNV -----------------------------------------------------

glDrawCommandsAddressNV
  :: MonadIO m
  => GLenum -- ^ @primitiveMode@.
  -> Ptr GLuint64 -- ^ @indirects@.
  -> Ptr GLsizei -- ^ @sizes@.
  -> GLuint -- ^ @count@.
  -> m ()
glDrawCommandsAddressNV v1 v2 v3 v4 = liftIO $ dyn235 ptr_glDrawCommandsAddressNV v1 v2 v3 v4

{-# NOINLINE ptr_glDrawCommandsAddressNV #-}
ptr_glDrawCommandsAddressNV :: FunPtr (GLenum -> Ptr GLuint64 -> Ptr GLsizei -> GLuint -> IO ())
ptr_glDrawCommandsAddressNV = unsafePerformIO $ getCommand "glDrawCommandsAddressNV"

-- glDrawCommandsNV ------------------------------------------------------------

glDrawCommandsNV
  :: MonadIO m
  => GLenum -- ^ @primitiveMode@.
  -> GLuint -- ^ @buffer@.
  -> Ptr GLintptr -- ^ @indirects@.
  -> Ptr GLsizei -- ^ @sizes@.
  -> GLuint -- ^ @count@.
  -> m ()
glDrawCommandsNV v1 v2 v3 v4 v5 = liftIO $ dyn236 ptr_glDrawCommandsNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glDrawCommandsNV #-}
ptr_glDrawCommandsNV :: FunPtr (GLenum -> GLuint -> Ptr GLintptr -> Ptr GLsizei -> GLuint -> IO ())
ptr_glDrawCommandsNV = unsafePerformIO $ getCommand "glDrawCommandsNV"

-- glDrawCommandsStatesAddressNV -----------------------------------------------

glDrawCommandsStatesAddressNV
  :: MonadIO m
  => Ptr GLuint64 -- ^ @indirects@.
  -> Ptr GLsizei -- ^ @sizes@.
  -> Ptr GLuint -- ^ @states@.
  -> Ptr GLuint -- ^ @fbos@.
  -> GLuint -- ^ @count@.
  -> m ()
glDrawCommandsStatesAddressNV v1 v2 v3 v4 v5 = liftIO $ dyn237 ptr_glDrawCommandsStatesAddressNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glDrawCommandsStatesAddressNV #-}
ptr_glDrawCommandsStatesAddressNV :: FunPtr (Ptr GLuint64 -> Ptr GLsizei -> Ptr GLuint -> Ptr GLuint -> GLuint -> IO ())
ptr_glDrawCommandsStatesAddressNV = unsafePerformIO $ getCommand "glDrawCommandsStatesAddressNV"

-- glDrawCommandsStatesNV ------------------------------------------------------

glDrawCommandsStatesNV
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> Ptr GLintptr -- ^ @indirects@.
  -> Ptr GLsizei -- ^ @sizes@.
  -> Ptr GLuint -- ^ @states@.
  -> Ptr GLuint -- ^ @fbos@.
  -> GLuint -- ^ @count@.
  -> m ()
glDrawCommandsStatesNV v1 v2 v3 v4 v5 v6 = liftIO $ dyn238 ptr_glDrawCommandsStatesNV v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glDrawCommandsStatesNV #-}
ptr_glDrawCommandsStatesNV :: FunPtr (GLuint -> Ptr GLintptr -> Ptr GLsizei -> Ptr GLuint -> Ptr GLuint -> GLuint -> IO ())
ptr_glDrawCommandsStatesNV = unsafePerformIO $ getCommand "glDrawCommandsStatesNV"

-- glDrawElementArrayAPPLE -----------------------------------------------------

glDrawElementArrayAPPLE
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLint -- ^ @first@.
  -> GLsizei -- ^ @count@.
  -> m ()
glDrawElementArrayAPPLE v1 v2 v3 = liftIO $ dyn229 ptr_glDrawElementArrayAPPLE v1 v2 v3

{-# NOINLINE ptr_glDrawElementArrayAPPLE #-}
ptr_glDrawElementArrayAPPLE :: FunPtr (GLenum -> GLint -> GLsizei -> IO ())
ptr_glDrawElementArrayAPPLE = unsafePerformIO $ getCommand "glDrawElementArrayAPPLE"

-- glDrawElementArrayATI -------------------------------------------------------

glDrawElementArrayATI
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLsizei -- ^ @count@.
  -> m ()
glDrawElementArrayATI v1 v2 = liftIO $ dyn239 ptr_glDrawElementArrayATI v1 v2

{-# NOINLINE ptr_glDrawElementArrayATI #-}
ptr_glDrawElementArrayATI :: FunPtr (GLenum -> GLsizei -> IO ())
ptr_glDrawElementArrayATI = unsafePerformIO $ getCommand "glDrawElementArrayATI"

-- glDrawElements --------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glDrawElements.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glDrawElements.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glDrawElements.xhtml OpenGL 4.x>.
glDrawElements
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLsizei -- ^ @count@.
  -> GLenum -- ^ @type@ of type @DrawElementsType@.
  -> Ptr a -- ^ @indices@ pointing to @COMPSIZE(count,type)@ elements of type @a@.
  -> m ()
glDrawElements v1 v2 v3 v4 = liftIO $ dyn240 ptr_glDrawElements v1 v2 v3 v4

{-# NOINLINE ptr_glDrawElements #-}
ptr_glDrawElements :: FunPtr (GLenum -> GLsizei -> GLenum -> Ptr a -> IO ())
ptr_glDrawElements = unsafePerformIO $ getCommand "glDrawElements"

-- glDrawElementsBaseVertex ----------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glDrawElementsBaseVertex.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glDrawElementsBaseVertex.xhtml OpenGL 4.x>.
glDrawElementsBaseVertex
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLsizei -- ^ @count@.
  -> GLenum -- ^ @type@ of type @DrawElementsType@.
  -> Ptr a -- ^ @indices@ pointing to @COMPSIZE(count,type)@ elements of type @a@.
  -> GLint -- ^ @basevertex@.
  -> m ()
glDrawElementsBaseVertex v1 v2 v3 v4 v5 = liftIO $ dyn241 ptr_glDrawElementsBaseVertex v1 v2 v3 v4 v5

{-# NOINLINE ptr_glDrawElementsBaseVertex #-}
ptr_glDrawElementsBaseVertex :: FunPtr (GLenum -> GLsizei -> GLenum -> Ptr a -> GLint -> IO ())
ptr_glDrawElementsBaseVertex = unsafePerformIO $ getCommand "glDrawElementsBaseVertex"

-- glDrawElementsBaseVertexEXT -------------------------------------------------

-- | This command is an alias for 'glDrawElementsBaseVertex'.
glDrawElementsBaseVertexEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLsizei -- ^ @count@.
  -> GLenum -- ^ @type@ of type @DrawElementsType@.
  -> Ptr a -- ^ @indices@ pointing to @COMPSIZE(count,type)@ elements of type @a@.
  -> GLint -- ^ @basevertex@.
  -> m ()
glDrawElementsBaseVertexEXT v1 v2 v3 v4 v5 = liftIO $ dyn241 ptr_glDrawElementsBaseVertexEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glDrawElementsBaseVertexEXT #-}
ptr_glDrawElementsBaseVertexEXT :: FunPtr (GLenum -> GLsizei -> GLenum -> Ptr a -> GLint -> IO ())
ptr_glDrawElementsBaseVertexEXT = unsafePerformIO $ getCommand "glDrawElementsBaseVertexEXT"

-- glDrawElementsBaseVertexOES -------------------------------------------------

-- | This command is an alias for 'glDrawElementsBaseVertex'.
glDrawElementsBaseVertexOES
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLsizei -- ^ @count@.
  -> GLenum -- ^ @type@ of type @DrawElementsType@.
  -> Ptr a -- ^ @indices@ pointing to @COMPSIZE(count,type)@ elements of type @a@.
  -> GLint -- ^ @basevertex@.
  -> m ()
glDrawElementsBaseVertexOES v1 v2 v3 v4 v5 = liftIO $ dyn241 ptr_glDrawElementsBaseVertexOES v1 v2 v3 v4 v5

{-# NOINLINE ptr_glDrawElementsBaseVertexOES #-}
ptr_glDrawElementsBaseVertexOES :: FunPtr (GLenum -> GLsizei -> GLenum -> Ptr a -> GLint -> IO ())
ptr_glDrawElementsBaseVertexOES = unsafePerformIO $ getCommand "glDrawElementsBaseVertexOES"

-- glDrawElementsIndirect ------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glDrawElementsIndirect.xhtml OpenGL 4.x>.
glDrawElementsIndirect
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLenum -- ^ @type@.
  -> Ptr a -- ^ @indirect@.
  -> m ()
glDrawElementsIndirect v1 v2 v3 = liftIO $ dyn242 ptr_glDrawElementsIndirect v1 v2 v3

{-# NOINLINE ptr_glDrawElementsIndirect #-}
ptr_glDrawElementsIndirect :: FunPtr (GLenum -> GLenum -> Ptr a -> IO ())
ptr_glDrawElementsIndirect = unsafePerformIO $ getCommand "glDrawElementsIndirect"

-- glDrawElementsInstanced -----------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glDrawElementsInstanced.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glDrawElementsInstanced.xhtml OpenGL 4.x>.
glDrawElementsInstanced
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLsizei -- ^ @count@.
  -> GLenum -- ^ @type@ of type @DrawElementsType@.
  -> Ptr a -- ^ @indices@ pointing to @COMPSIZE(count,type)@ elements of type @a@.
  -> GLsizei -- ^ @instancecount@.
  -> m ()
glDrawElementsInstanced v1 v2 v3 v4 v5 = liftIO $ dyn243 ptr_glDrawElementsInstanced v1 v2 v3 v4 v5

{-# NOINLINE ptr_glDrawElementsInstanced #-}
ptr_glDrawElementsInstanced :: FunPtr (GLenum -> GLsizei -> GLenum -> Ptr a -> GLsizei -> IO ())
ptr_glDrawElementsInstanced = unsafePerformIO $ getCommand "glDrawElementsInstanced"

-- glDrawElementsInstancedANGLE ------------------------------------------------

-- | This command is an alias for 'glDrawElementsInstanced'.
glDrawElementsInstancedANGLE
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLsizei -- ^ @count@.
  -> GLenum -- ^ @type@.
  -> Ptr a -- ^ @indices@ pointing to @COMPSIZE(count,type)@ elements of type @a@.
  -> GLsizei -- ^ @primcount@.
  -> m ()
glDrawElementsInstancedANGLE v1 v2 v3 v4 v5 = liftIO $ dyn243 ptr_glDrawElementsInstancedANGLE v1 v2 v3 v4 v5

{-# NOINLINE ptr_glDrawElementsInstancedANGLE #-}
ptr_glDrawElementsInstancedANGLE :: FunPtr (GLenum -> GLsizei -> GLenum -> Ptr a -> GLsizei -> IO ())
ptr_glDrawElementsInstancedANGLE = unsafePerformIO $ getCommand "glDrawElementsInstancedANGLE"

-- glDrawElementsInstancedARB --------------------------------------------------

-- | This command is an alias for 'glDrawElementsInstanced'.
glDrawElementsInstancedARB
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLsizei -- ^ @count@.
  -> GLenum -- ^ @type@ of type @DrawElementsType@.
  -> Ptr a -- ^ @indices@ pointing to @COMPSIZE(count,type)@ elements of type @a@.
  -> GLsizei -- ^ @primcount@.
  -> m ()
glDrawElementsInstancedARB v1 v2 v3 v4 v5 = liftIO $ dyn243 ptr_glDrawElementsInstancedARB v1 v2 v3 v4 v5

{-# NOINLINE ptr_glDrawElementsInstancedARB #-}
ptr_glDrawElementsInstancedARB :: FunPtr (GLenum -> GLsizei -> GLenum -> Ptr a -> GLsizei -> IO ())
ptr_glDrawElementsInstancedARB = unsafePerformIO $ getCommand "glDrawElementsInstancedARB"

-- glDrawElementsInstancedBaseInstance -----------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glDrawElementsInstancedBaseInstance.xhtml OpenGL 4.x>.
glDrawElementsInstancedBaseInstance
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLsizei -- ^ @count@.
  -> GLenum -- ^ @type@.
  -> Ptr a -- ^ @indices@ pointing to @count@ elements of type @a@.
  -> GLsizei -- ^ @instancecount@.
  -> GLuint -- ^ @baseinstance@.
  -> m ()
glDrawElementsInstancedBaseInstance v1 v2 v3 v4 v5 v6 = liftIO $ dyn244 ptr_glDrawElementsInstancedBaseInstance v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glDrawElementsInstancedBaseInstance #-}
ptr_glDrawElementsInstancedBaseInstance :: FunPtr (GLenum -> GLsizei -> GLenum -> Ptr a -> GLsizei -> GLuint -> IO ())
ptr_glDrawElementsInstancedBaseInstance = unsafePerformIO $ getCommand "glDrawElementsInstancedBaseInstance"

-- glDrawElementsInstancedBaseInstanceEXT --------------------------------------

-- | This command is an alias for 'glDrawElementsInstancedBaseInstance'.
glDrawElementsInstancedBaseInstanceEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLsizei -- ^ @count@.
  -> GLenum -- ^ @type@.
  -> Ptr a -- ^ @indices@ pointing to @count@ elements of type @a@.
  -> GLsizei -- ^ @instancecount@.
  -> GLuint -- ^ @baseinstance@.
  -> m ()
glDrawElementsInstancedBaseInstanceEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn244 ptr_glDrawElementsInstancedBaseInstanceEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glDrawElementsInstancedBaseInstanceEXT #-}
ptr_glDrawElementsInstancedBaseInstanceEXT :: FunPtr (GLenum -> GLsizei -> GLenum -> Ptr a -> GLsizei -> GLuint -> IO ())
ptr_glDrawElementsInstancedBaseInstanceEXT = unsafePerformIO $ getCommand "glDrawElementsInstancedBaseInstanceEXT"

-- glDrawElementsInstancedBaseVertex -------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glDrawElementsInstancedBaseVertex.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glDrawElementsInstancedBaseVertex.xhtml OpenGL 4.x>.
glDrawElementsInstancedBaseVertex
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLsizei -- ^ @count@.
  -> GLenum -- ^ @type@ of type @DrawElementsType@.
  -> Ptr a -- ^ @indices@ pointing to @COMPSIZE(count,type)@ elements of type @a@.
  -> GLsizei -- ^ @instancecount@.
  -> GLint -- ^ @basevertex@.
  -> m ()
glDrawElementsInstancedBaseVertex v1 v2 v3 v4 v5 v6 = liftIO $ dyn245 ptr_glDrawElementsInstancedBaseVertex v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glDrawElementsInstancedBaseVertex #-}
ptr_glDrawElementsInstancedBaseVertex :: FunPtr (GLenum -> GLsizei -> GLenum -> Ptr a -> GLsizei -> GLint -> IO ())
ptr_glDrawElementsInstancedBaseVertex = unsafePerformIO $ getCommand "glDrawElementsInstancedBaseVertex"

-- glDrawElementsInstancedBaseVertexBaseInstance -------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glDrawElementsInstancedBaseVertexBaseInstance.xhtml OpenGL 4.x>.
glDrawElementsInstancedBaseVertexBaseInstance
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLsizei -- ^ @count@.
  -> GLenum -- ^ @type@.
  -> Ptr a -- ^ @indices@ pointing to @count@ elements of type @a@.
  -> GLsizei -- ^ @instancecount@.
  -> GLint -- ^ @basevertex@.
  -> GLuint -- ^ @baseinstance@.
  -> m ()
glDrawElementsInstancedBaseVertexBaseInstance v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn246 ptr_glDrawElementsInstancedBaseVertexBaseInstance v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glDrawElementsInstancedBaseVertexBaseInstance #-}
ptr_glDrawElementsInstancedBaseVertexBaseInstance :: FunPtr (GLenum -> GLsizei -> GLenum -> Ptr a -> GLsizei -> GLint -> GLuint -> IO ())
ptr_glDrawElementsInstancedBaseVertexBaseInstance = unsafePerformIO $ getCommand "glDrawElementsInstancedBaseVertexBaseInstance"

-- glDrawElementsInstancedBaseVertexBaseInstanceEXT ----------------------------

-- | This command is an alias for 'glDrawElementsInstancedBaseVertexBaseInstance'.
glDrawElementsInstancedBaseVertexBaseInstanceEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLsizei -- ^ @count@.
  -> GLenum -- ^ @type@.
  -> Ptr a -- ^ @indices@ pointing to @count@ elements of type @a@.
  -> GLsizei -- ^ @instancecount@.
  -> GLint -- ^ @basevertex@.
  -> GLuint -- ^ @baseinstance@.
  -> m ()
glDrawElementsInstancedBaseVertexBaseInstanceEXT v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn246 ptr_glDrawElementsInstancedBaseVertexBaseInstanceEXT v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glDrawElementsInstancedBaseVertexBaseInstanceEXT #-}
ptr_glDrawElementsInstancedBaseVertexBaseInstanceEXT :: FunPtr (GLenum -> GLsizei -> GLenum -> Ptr a -> GLsizei -> GLint -> GLuint -> IO ())
ptr_glDrawElementsInstancedBaseVertexBaseInstanceEXT = unsafePerformIO $ getCommand "glDrawElementsInstancedBaseVertexBaseInstanceEXT"

-- glDrawElementsInstancedBaseVertexEXT ----------------------------------------

-- | This command is an alias for 'glDrawElementsInstancedBaseVertex'.
glDrawElementsInstancedBaseVertexEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLsizei -- ^ @count@.
  -> GLenum -- ^ @type@ of type @DrawElementsType@.
  -> Ptr a -- ^ @indices@ pointing to @COMPSIZE(count,type)@ elements of type @a@.
  -> GLsizei -- ^ @instancecount@.
  -> GLint -- ^ @basevertex@.
  -> m ()
glDrawElementsInstancedBaseVertexEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn245 ptr_glDrawElementsInstancedBaseVertexEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glDrawElementsInstancedBaseVertexEXT #-}
ptr_glDrawElementsInstancedBaseVertexEXT :: FunPtr (GLenum -> GLsizei -> GLenum -> Ptr a -> GLsizei -> GLint -> IO ())
ptr_glDrawElementsInstancedBaseVertexEXT = unsafePerformIO $ getCommand "glDrawElementsInstancedBaseVertexEXT"

-- glDrawElementsInstancedBaseVertexOES ----------------------------------------

-- | This command is an alias for 'glDrawElementsInstancedBaseVertex'.
glDrawElementsInstancedBaseVertexOES
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLsizei -- ^ @count@.
  -> GLenum -- ^ @type@ of type @DrawElementsType@.
  -> Ptr a -- ^ @indices@ pointing to @COMPSIZE(count,type)@ elements of type @a@.
  -> GLsizei -- ^ @instancecount@.
  -> GLint -- ^ @basevertex@.
  -> m ()
glDrawElementsInstancedBaseVertexOES v1 v2 v3 v4 v5 v6 = liftIO $ dyn245 ptr_glDrawElementsInstancedBaseVertexOES v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glDrawElementsInstancedBaseVertexOES #-}
ptr_glDrawElementsInstancedBaseVertexOES :: FunPtr (GLenum -> GLsizei -> GLenum -> Ptr a -> GLsizei -> GLint -> IO ())
ptr_glDrawElementsInstancedBaseVertexOES = unsafePerformIO $ getCommand "glDrawElementsInstancedBaseVertexOES"

