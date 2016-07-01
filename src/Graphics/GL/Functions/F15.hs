--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F15
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

module Graphics.GL.Functions.F15 (
  glDrawElementsInstancedEXT,
  glDrawElementsInstancedNV,
  glDrawMeshArraysSUN,
  glDrawPixels,
  glDrawRangeElementArrayAPPLE,
  glDrawRangeElementArrayATI,
  glDrawRangeElements,
  glDrawRangeElementsBaseVertex,
  glDrawRangeElementsBaseVertexEXT,
  glDrawRangeElementsBaseVertexOES,
  glDrawRangeElementsEXT,
  glDrawTexfOES,
  glDrawTexfvOES,
  glDrawTexiOES,
  glDrawTexivOES,
  glDrawTexsOES,
  glDrawTexsvOES,
  glDrawTextureNV,
  glDrawTexxOES,
  glDrawTexxvOES,
  glDrawTransformFeedback,
  glDrawTransformFeedbackInstanced,
  glDrawTransformFeedbackNV,
  glDrawTransformFeedbackStream,
  glDrawTransformFeedbackStreamInstanced,
  glEGLImageTargetRenderbufferStorageOES,
  glEGLImageTargetTexture2DOES,
  glEdgeFlag,
  glEdgeFlagFormatNV,
  glEdgeFlagPointer,
  glEdgeFlagPointerEXT,
  glEdgeFlagPointerListIBM,
  glEdgeFlagv,
  glElementPointerAPPLE,
  glElementPointerATI,
  glEnable,
  glEnableClientState,
  glEnableClientStateIndexedEXT,
  glEnableClientStateiEXT,
  glEnableDriverControlQCOM
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

-- glDrawElementsInstancedEXT --------------------------------------------------

-- | This command is an alias for 'glDrawElementsInstanced'.
glDrawElementsInstancedEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLsizei -- ^ @count@.
  -> GLenum -- ^ @type@ of type @DrawElementsType@.
  -> Ptr a -- ^ @indices@ pointing to @COMPSIZE(count,type)@ elements of type @a@.
  -> GLsizei -- ^ @primcount@.
  -> m ()
glDrawElementsInstancedEXT v1 v2 v3 v4 v5 = liftIO $ dyn243 ptr_glDrawElementsInstancedEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glDrawElementsInstancedEXT #-}
ptr_glDrawElementsInstancedEXT :: FunPtr (GLenum -> GLsizei -> GLenum -> Ptr a -> GLsizei -> IO ())
ptr_glDrawElementsInstancedEXT = unsafePerformIO $ getCommand "glDrawElementsInstancedEXT"

-- glDrawElementsInstancedNV ---------------------------------------------------

-- | This command is an alias for 'glDrawElementsInstanced'.
glDrawElementsInstancedNV
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLsizei -- ^ @count@.
  -> GLenum -- ^ @type@.
  -> Ptr a -- ^ @indices@ pointing to @COMPSIZE(count,type)@ elements of type @a@.
  -> GLsizei -- ^ @primcount@.
  -> m ()
glDrawElementsInstancedNV v1 v2 v3 v4 v5 = liftIO $ dyn243 ptr_glDrawElementsInstancedNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glDrawElementsInstancedNV #-}
ptr_glDrawElementsInstancedNV :: FunPtr (GLenum -> GLsizei -> GLenum -> Ptr a -> GLsizei -> IO ())
ptr_glDrawElementsInstancedNV = unsafePerformIO $ getCommand "glDrawElementsInstancedNV"

-- glDrawMeshArraysSUN ---------------------------------------------------------

glDrawMeshArraysSUN
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLint -- ^ @first@.
  -> GLsizei -- ^ @count@.
  -> GLsizei -- ^ @width@.
  -> m ()
glDrawMeshArraysSUN v1 v2 v3 v4 = liftIO $ dyn231 ptr_glDrawMeshArraysSUN v1 v2 v3 v4

{-# NOINLINE ptr_glDrawMeshArraysSUN #-}
ptr_glDrawMeshArraysSUN :: FunPtr (GLenum -> GLint -> GLsizei -> GLsizei -> IO ())
ptr_glDrawMeshArraysSUN = unsafePerformIO $ getCommand "glDrawMeshArraysSUN"

-- glDrawPixels ----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glDrawPixels.xml OpenGL 2.x>.
glDrawPixels
  :: MonadIO m
  => GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @pixels@ pointing to @COMPSIZE(format,type,width,height)@ elements of type @a@.
  -> m ()
glDrawPixels v1 v2 v3 v4 v5 = liftIO $ dyn247 ptr_glDrawPixels v1 v2 v3 v4 v5

{-# NOINLINE ptr_glDrawPixels #-}
ptr_glDrawPixels :: FunPtr (GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glDrawPixels = unsafePerformIO $ getCommand "glDrawPixels"

-- glDrawRangeElementArrayAPPLE ------------------------------------------------

glDrawRangeElementArrayAPPLE
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLuint -- ^ @start@.
  -> GLuint -- ^ @end@.
  -> GLint -- ^ @first@.
  -> GLsizei -- ^ @count@.
  -> m ()
glDrawRangeElementArrayAPPLE v1 v2 v3 v4 v5 = liftIO $ dyn248 ptr_glDrawRangeElementArrayAPPLE v1 v2 v3 v4 v5

{-# NOINLINE ptr_glDrawRangeElementArrayAPPLE #-}
ptr_glDrawRangeElementArrayAPPLE :: FunPtr (GLenum -> GLuint -> GLuint -> GLint -> GLsizei -> IO ())
ptr_glDrawRangeElementArrayAPPLE = unsafePerformIO $ getCommand "glDrawRangeElementArrayAPPLE"

-- glDrawRangeElementArrayATI --------------------------------------------------

glDrawRangeElementArrayATI
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLuint -- ^ @start@.
  -> GLuint -- ^ @end@.
  -> GLsizei -- ^ @count@.
  -> m ()
glDrawRangeElementArrayATI v1 v2 v3 v4 = liftIO $ dyn249 ptr_glDrawRangeElementArrayATI v1 v2 v3 v4

{-# NOINLINE ptr_glDrawRangeElementArrayATI #-}
ptr_glDrawRangeElementArrayATI :: FunPtr (GLenum -> GLuint -> GLuint -> GLsizei -> IO ())
ptr_glDrawRangeElementArrayATI = unsafePerformIO $ getCommand "glDrawRangeElementArrayATI"

-- glDrawRangeElements ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glDrawRangeElements.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glDrawRangeElements.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glDrawRangeElements.xhtml OpenGL 4.x>.
glDrawRangeElements
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLuint -- ^ @start@.
  -> GLuint -- ^ @end@.
  -> GLsizei -- ^ @count@.
  -> GLenum -- ^ @type@ of type @DrawElementsType@.
  -> Ptr a -- ^ @indices@ pointing to @COMPSIZE(count,type)@ elements of type @a@.
  -> m ()
glDrawRangeElements v1 v2 v3 v4 v5 v6 = liftIO $ dyn250 ptr_glDrawRangeElements v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glDrawRangeElements #-}
ptr_glDrawRangeElements :: FunPtr (GLenum -> GLuint -> GLuint -> GLsizei -> GLenum -> Ptr a -> IO ())
ptr_glDrawRangeElements = unsafePerformIO $ getCommand "glDrawRangeElements"

-- glDrawRangeElementsBaseVertex -----------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glDrawRangeElementsBaseVertex.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glDrawRangeElementsBaseVertex.xhtml OpenGL 4.x>.
glDrawRangeElementsBaseVertex
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLuint -- ^ @start@.
  -> GLuint -- ^ @end@.
  -> GLsizei -- ^ @count@.
  -> GLenum -- ^ @type@ of type @DrawElementsType@.
  -> Ptr a -- ^ @indices@ pointing to @COMPSIZE(count,type)@ elements of type @a@.
  -> GLint -- ^ @basevertex@.
  -> m ()
glDrawRangeElementsBaseVertex v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn251 ptr_glDrawRangeElementsBaseVertex v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glDrawRangeElementsBaseVertex #-}
ptr_glDrawRangeElementsBaseVertex :: FunPtr (GLenum -> GLuint -> GLuint -> GLsizei -> GLenum -> Ptr a -> GLint -> IO ())
ptr_glDrawRangeElementsBaseVertex = unsafePerformIO $ getCommand "glDrawRangeElementsBaseVertex"

-- glDrawRangeElementsBaseVertexEXT --------------------------------------------

-- | This command is an alias for 'glDrawRangeElementsBaseVertex'.
glDrawRangeElementsBaseVertexEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLuint -- ^ @start@.
  -> GLuint -- ^ @end@.
  -> GLsizei -- ^ @count@.
  -> GLenum -- ^ @type@ of type @DrawElementsType@.
  -> Ptr a -- ^ @indices@ pointing to @COMPSIZE(count,type)@ elements of type @a@.
  -> GLint -- ^ @basevertex@.
  -> m ()
glDrawRangeElementsBaseVertexEXT v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn251 ptr_glDrawRangeElementsBaseVertexEXT v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glDrawRangeElementsBaseVertexEXT #-}
ptr_glDrawRangeElementsBaseVertexEXT :: FunPtr (GLenum -> GLuint -> GLuint -> GLsizei -> GLenum -> Ptr a -> GLint -> IO ())
ptr_glDrawRangeElementsBaseVertexEXT = unsafePerformIO $ getCommand "glDrawRangeElementsBaseVertexEXT"

-- glDrawRangeElementsBaseVertexOES --------------------------------------------

-- | This command is an alias for 'glDrawRangeElementsBaseVertex'.
glDrawRangeElementsBaseVertexOES
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLuint -- ^ @start@.
  -> GLuint -- ^ @end@.
  -> GLsizei -- ^ @count@.
  -> GLenum -- ^ @type@ of type @DrawElementsType@.
  -> Ptr a -- ^ @indices@ pointing to @COMPSIZE(count,type)@ elements of type @a@.
  -> GLint -- ^ @basevertex@.
  -> m ()
glDrawRangeElementsBaseVertexOES v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn251 ptr_glDrawRangeElementsBaseVertexOES v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glDrawRangeElementsBaseVertexOES #-}
ptr_glDrawRangeElementsBaseVertexOES :: FunPtr (GLenum -> GLuint -> GLuint -> GLsizei -> GLenum -> Ptr a -> GLint -> IO ())
ptr_glDrawRangeElementsBaseVertexOES = unsafePerformIO $ getCommand "glDrawRangeElementsBaseVertexOES"

-- glDrawRangeElementsEXT ------------------------------------------------------

-- | This command is an alias for 'glDrawRangeElements'.
glDrawRangeElementsEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLuint -- ^ @start@.
  -> GLuint -- ^ @end@.
  -> GLsizei -- ^ @count@.
  -> GLenum -- ^ @type@ of type @DrawElementsType@.
  -> Ptr a -- ^ @indices@ pointing to @COMPSIZE(count,type)@ elements of type @a@.
  -> m ()
glDrawRangeElementsEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn250 ptr_glDrawRangeElementsEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glDrawRangeElementsEXT #-}
ptr_glDrawRangeElementsEXT :: FunPtr (GLenum -> GLuint -> GLuint -> GLsizei -> GLenum -> Ptr a -> IO ())
ptr_glDrawRangeElementsEXT = unsafePerformIO $ getCommand "glDrawRangeElementsEXT"

-- glDrawTexfOES ---------------------------------------------------------------

glDrawTexfOES
  :: MonadIO m
  => GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> GLfloat -- ^ @width@.
  -> GLfloat -- ^ @height@.
  -> m ()
glDrawTexfOES v1 v2 v3 v4 v5 = liftIO $ dyn252 ptr_glDrawTexfOES v1 v2 v3 v4 v5

{-# NOINLINE ptr_glDrawTexfOES #-}
ptr_glDrawTexfOES :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glDrawTexfOES = unsafePerformIO $ getCommand "glDrawTexfOES"

-- glDrawTexfvOES --------------------------------------------------------------

glDrawTexfvOES
  :: MonadIO m
  => Ptr GLfloat -- ^ @coords@.
  -> m ()
glDrawTexfvOES v1 = liftIO $ dyn41 ptr_glDrawTexfvOES v1

{-# NOINLINE ptr_glDrawTexfvOES #-}
ptr_glDrawTexfvOES :: FunPtr (Ptr GLfloat -> IO ())
ptr_glDrawTexfvOES = unsafePerformIO $ getCommand "glDrawTexfvOES"

-- glDrawTexiOES ---------------------------------------------------------------

glDrawTexiOES
  :: MonadIO m
  => GLint -- ^ @x@.
  -> GLint -- ^ @y@.
  -> GLint -- ^ @z@.
  -> GLint -- ^ @width@.
  -> GLint -- ^ @height@.
  -> m ()
glDrawTexiOES v1 v2 v3 v4 v5 = liftIO $ dyn253 ptr_glDrawTexiOES v1 v2 v3 v4 v5

{-# NOINLINE ptr_glDrawTexiOES #-}
ptr_glDrawTexiOES :: FunPtr (GLint -> GLint -> GLint -> GLint -> GLint -> IO ())
ptr_glDrawTexiOES = unsafePerformIO $ getCommand "glDrawTexiOES"

-- glDrawTexivOES --------------------------------------------------------------

glDrawTexivOES
  :: MonadIO m
  => Ptr GLint -- ^ @coords@.
  -> m ()
glDrawTexivOES v1 = liftIO $ dyn43 ptr_glDrawTexivOES v1

{-# NOINLINE ptr_glDrawTexivOES #-}
ptr_glDrawTexivOES :: FunPtr (Ptr GLint -> IO ())
ptr_glDrawTexivOES = unsafePerformIO $ getCommand "glDrawTexivOES"

-- glDrawTexsOES ---------------------------------------------------------------

glDrawTexsOES
  :: MonadIO m
  => GLshort -- ^ @x@.
  -> GLshort -- ^ @y@.
  -> GLshort -- ^ @z@.
  -> GLshort -- ^ @width@.
  -> GLshort -- ^ @height@.
  -> m ()
glDrawTexsOES v1 v2 v3 v4 v5 = liftIO $ dyn254 ptr_glDrawTexsOES v1 v2 v3 v4 v5

{-# NOINLINE ptr_glDrawTexsOES #-}
ptr_glDrawTexsOES :: FunPtr (GLshort -> GLshort -> GLshort -> GLshort -> GLshort -> IO ())
ptr_glDrawTexsOES = unsafePerformIO $ getCommand "glDrawTexsOES"

-- glDrawTexsvOES --------------------------------------------------------------

glDrawTexsvOES
  :: MonadIO m
  => Ptr GLshort -- ^ @coords@.
  -> m ()
glDrawTexsvOES v1 = liftIO $ dyn45 ptr_glDrawTexsvOES v1

{-# NOINLINE ptr_glDrawTexsvOES #-}
ptr_glDrawTexsvOES :: FunPtr (Ptr GLshort -> IO ())
ptr_glDrawTexsvOES = unsafePerformIO $ getCommand "glDrawTexsvOES"

-- glDrawTextureNV -------------------------------------------------------------

glDrawTextureNV
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLuint -- ^ @sampler@.
  -> GLfloat -- ^ @x0@.
  -> GLfloat -- ^ @y0@.
  -> GLfloat -- ^ @x1@.
  -> GLfloat -- ^ @y1@.
  -> GLfloat -- ^ @z@.
  -> GLfloat -- ^ @s0@.
  -> GLfloat -- ^ @t0@.
  -> GLfloat -- ^ @s1@.
  -> GLfloat -- ^ @t1@.
  -> m ()
glDrawTextureNV v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 = liftIO $ dyn255 ptr_glDrawTextureNV v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11

{-# NOINLINE ptr_glDrawTextureNV #-}
ptr_glDrawTextureNV :: FunPtr (GLuint -> GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glDrawTextureNV = unsafePerformIO $ getCommand "glDrawTextureNV"

-- glDrawTexxOES ---------------------------------------------------------------

glDrawTexxOES
  :: MonadIO m
  => GLfixed -- ^ @x@.
  -> GLfixed -- ^ @y@.
  -> GLfixed -- ^ @z@.
  -> GLfixed -- ^ @width@.
  -> GLfixed -- ^ @height@.
  -> m ()
glDrawTexxOES v1 v2 v3 v4 v5 = liftIO $ dyn256 ptr_glDrawTexxOES v1 v2 v3 v4 v5

{-# NOINLINE ptr_glDrawTexxOES #-}
ptr_glDrawTexxOES :: FunPtr (GLfixed -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ())
ptr_glDrawTexxOES = unsafePerformIO $ getCommand "glDrawTexxOES"

-- glDrawTexxvOES --------------------------------------------------------------

glDrawTexxvOES
  :: MonadIO m
  => Ptr GLfixed -- ^ @coords@.
  -> m ()
glDrawTexxvOES v1 = liftIO $ dyn107 ptr_glDrawTexxvOES v1

{-# NOINLINE ptr_glDrawTexxvOES #-}
ptr_glDrawTexxvOES :: FunPtr (Ptr GLfixed -> IO ())
ptr_glDrawTexxvOES = unsafePerformIO $ getCommand "glDrawTexxvOES"

-- glDrawTransformFeedback -----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glDrawTransformFeedback.xhtml OpenGL 4.x>.
glDrawTransformFeedback
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLuint -- ^ @id@.
  -> m ()
glDrawTransformFeedback v1 v2 = liftIO $ dyn16 ptr_glDrawTransformFeedback v1 v2

{-# NOINLINE ptr_glDrawTransformFeedback #-}
ptr_glDrawTransformFeedback :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glDrawTransformFeedback = unsafePerformIO $ getCommand "glDrawTransformFeedback"

-- glDrawTransformFeedbackInstanced --------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glDrawTransformFeedbackInstanced.xhtml OpenGL 4.x>.
glDrawTransformFeedbackInstanced
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLuint -- ^ @id@.
  -> GLsizei -- ^ @instancecount@.
  -> m ()
glDrawTransformFeedbackInstanced v1 v2 v3 = liftIO $ dyn257 ptr_glDrawTransformFeedbackInstanced v1 v2 v3

{-# NOINLINE ptr_glDrawTransformFeedbackInstanced #-}
ptr_glDrawTransformFeedbackInstanced :: FunPtr (GLenum -> GLuint -> GLsizei -> IO ())
ptr_glDrawTransformFeedbackInstanced = unsafePerformIO $ getCommand "glDrawTransformFeedbackInstanced"

-- glDrawTransformFeedbackNV ---------------------------------------------------

-- | This command is an alias for 'glDrawTransformFeedback'.
glDrawTransformFeedbackNV
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLuint -- ^ @id@.
  -> m ()
glDrawTransformFeedbackNV v1 v2 = liftIO $ dyn16 ptr_glDrawTransformFeedbackNV v1 v2

{-# NOINLINE ptr_glDrawTransformFeedbackNV #-}
ptr_glDrawTransformFeedbackNV :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glDrawTransformFeedbackNV = unsafePerformIO $ getCommand "glDrawTransformFeedbackNV"

-- glDrawTransformFeedbackStream -----------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glDrawTransformFeedbackStream.xhtml OpenGL 4.x>.
glDrawTransformFeedbackStream
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLuint -- ^ @id@.
  -> GLuint -- ^ @stream@.
  -> m ()
glDrawTransformFeedbackStream v1 v2 v3 = liftIO $ dyn17 ptr_glDrawTransformFeedbackStream v1 v2 v3

{-# NOINLINE ptr_glDrawTransformFeedbackStream #-}
ptr_glDrawTransformFeedbackStream :: FunPtr (GLenum -> GLuint -> GLuint -> IO ())
ptr_glDrawTransformFeedbackStream = unsafePerformIO $ getCommand "glDrawTransformFeedbackStream"

-- glDrawTransformFeedbackStreamInstanced --------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glDrawTransformFeedbackStreamInstanced.xhtml OpenGL 4.x>.
glDrawTransformFeedbackStreamInstanced
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLuint -- ^ @id@.
  -> GLuint -- ^ @stream@.
  -> GLsizei -- ^ @instancecount@.
  -> m ()
glDrawTransformFeedbackStreamInstanced v1 v2 v3 v4 = liftIO $ dyn249 ptr_glDrawTransformFeedbackStreamInstanced v1 v2 v3 v4

{-# NOINLINE ptr_glDrawTransformFeedbackStreamInstanced #-}
ptr_glDrawTransformFeedbackStreamInstanced :: FunPtr (GLenum -> GLuint -> GLuint -> GLsizei -> IO ())
ptr_glDrawTransformFeedbackStreamInstanced = unsafePerformIO $ getCommand "glDrawTransformFeedbackStreamInstanced"

-- glEGLImageTargetRenderbufferStorageOES --------------------------------------

glEGLImageTargetRenderbufferStorageOES
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLeglImageOES -- ^ @image@.
  -> m ()
glEGLImageTargetRenderbufferStorageOES v1 v2 = liftIO $ dyn258 ptr_glEGLImageTargetRenderbufferStorageOES v1 v2

{-# NOINLINE ptr_glEGLImageTargetRenderbufferStorageOES #-}
ptr_glEGLImageTargetRenderbufferStorageOES :: FunPtr (GLenum -> GLeglImageOES -> IO ())
ptr_glEGLImageTargetRenderbufferStorageOES = unsafePerformIO $ getCommand "glEGLImageTargetRenderbufferStorageOES"

-- glEGLImageTargetTexture2DOES ------------------------------------------------

glEGLImageTargetTexture2DOES
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLeglImageOES -- ^ @image@.
  -> m ()
glEGLImageTargetTexture2DOES v1 v2 = liftIO $ dyn258 ptr_glEGLImageTargetTexture2DOES v1 v2

{-# NOINLINE ptr_glEGLImageTargetTexture2DOES #-}
ptr_glEGLImageTargetTexture2DOES :: FunPtr (GLenum -> GLeglImageOES -> IO ())
ptr_glEGLImageTargetTexture2DOES = unsafePerformIO $ getCommand "glEGLImageTargetTexture2DOES"

-- glEdgeFlag ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glEdgeFlag.xml OpenGL 2.x>. The vector equivalent of this command is 'glEdgeFlagv'.
glEdgeFlag
  :: MonadIO m
  => GLboolean -- ^ @flag@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glEdgeFlag v1 = liftIO $ dyn191 ptr_glEdgeFlag v1

{-# NOINLINE ptr_glEdgeFlag #-}
ptr_glEdgeFlag :: FunPtr (GLboolean -> IO ())
ptr_glEdgeFlag = unsafePerformIO $ getCommand "glEdgeFlag"

-- glEdgeFlagFormatNV ----------------------------------------------------------

glEdgeFlagFormatNV
  :: MonadIO m
  => GLsizei -- ^ @stride@.
  -> m ()
glEdgeFlagFormatNV v1 = liftIO $ dyn259 ptr_glEdgeFlagFormatNV v1

{-# NOINLINE ptr_glEdgeFlagFormatNV #-}
ptr_glEdgeFlagFormatNV :: FunPtr (GLsizei -> IO ())
ptr_glEdgeFlagFormatNV = unsafePerformIO $ getCommand "glEdgeFlagFormatNV"

-- glEdgeFlagPointer -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glEdgeFlagPointer.xml OpenGL 2.x>.
glEdgeFlagPointer
  :: MonadIO m
  => GLsizei -- ^ @stride@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(stride)@ elements of type @a@.
  -> m ()
glEdgeFlagPointer v1 v2 = liftIO $ dyn260 ptr_glEdgeFlagPointer v1 v2

{-# NOINLINE ptr_glEdgeFlagPointer #-}
ptr_glEdgeFlagPointer :: FunPtr (GLsizei -> Ptr a -> IO ())
ptr_glEdgeFlagPointer = unsafePerformIO $ getCommand "glEdgeFlagPointer"

-- glEdgeFlagPointerEXT --------------------------------------------------------

glEdgeFlagPointerEXT
  :: MonadIO m
  => GLsizei -- ^ @stride@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLboolean -- ^ @pointer@ pointing to @COMPSIZE(stride,count)@ elements of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glEdgeFlagPointerEXT v1 v2 v3 = liftIO $ dyn261 ptr_glEdgeFlagPointerEXT v1 v2 v3

{-# NOINLINE ptr_glEdgeFlagPointerEXT #-}
ptr_glEdgeFlagPointerEXT :: FunPtr (GLsizei -> GLsizei -> Ptr GLboolean -> IO ())
ptr_glEdgeFlagPointerEXT = unsafePerformIO $ getCommand "glEdgeFlagPointerEXT"

-- glEdgeFlagPointerListIBM ----------------------------------------------------

glEdgeFlagPointerListIBM
  :: MonadIO m
  => GLint -- ^ @stride@.
  -> Ptr (Ptr GLboolean) -- ^ @pointer@ pointing to @COMPSIZE(stride)@ elements of type @Ptr BooleanPointer@.
  -> GLint -- ^ @ptrstride@.
  -> m ()
glEdgeFlagPointerListIBM v1 v2 v3 = liftIO $ dyn262 ptr_glEdgeFlagPointerListIBM v1 v2 v3

{-# NOINLINE ptr_glEdgeFlagPointerListIBM #-}
ptr_glEdgeFlagPointerListIBM :: FunPtr (GLint -> Ptr (Ptr GLboolean) -> GLint -> IO ())
ptr_glEdgeFlagPointerListIBM = unsafePerformIO $ getCommand "glEdgeFlagPointerListIBM"

-- glEdgeFlagv -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glEdgeFlag.xml OpenGL 2.x>.
glEdgeFlagv
  :: MonadIO m
  => Ptr GLboolean -- ^ @flag@ pointing to @1@ element of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glEdgeFlagv v1 = liftIO $ dyn263 ptr_glEdgeFlagv v1

{-# NOINLINE ptr_glEdgeFlagv #-}
ptr_glEdgeFlagv :: FunPtr (Ptr GLboolean -> IO ())
ptr_glEdgeFlagv = unsafePerformIO $ getCommand "glEdgeFlagv"

-- glElementPointerAPPLE -------------------------------------------------------

glElementPointerAPPLE
  :: MonadIO m
  => GLenum -- ^ @type@ of type @ElementPointerTypeATI@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(type)@ elements of type @a@.
  -> m ()
glElementPointerAPPLE v1 v2 = liftIO $ dyn230 ptr_glElementPointerAPPLE v1 v2

{-# NOINLINE ptr_glElementPointerAPPLE #-}
ptr_glElementPointerAPPLE :: FunPtr (GLenum -> Ptr a -> IO ())
ptr_glElementPointerAPPLE = unsafePerformIO $ getCommand "glElementPointerAPPLE"

-- glElementPointerATI ---------------------------------------------------------

glElementPointerATI
  :: MonadIO m
  => GLenum -- ^ @type@ of type @ElementPointerTypeATI@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(type)@ elements of type @a@.
  -> m ()
glElementPointerATI v1 v2 = liftIO $ dyn230 ptr_glElementPointerATI v1 v2

{-# NOINLINE ptr_glElementPointerATI #-}
ptr_glElementPointerATI :: FunPtr (GLenum -> Ptr a -> IO ())
ptr_glElementPointerATI = unsafePerformIO $ getCommand "glElementPointerATI"

-- glEnable --------------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glEnable.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glEnable.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glEnable.xhtml OpenGL 4.x>.
glEnable
  :: MonadIO m
  => GLenum -- ^ @cap@ of type [EnableCap](Graphics-GL-Groups.html#EnableCap).
  -> m ()
glEnable v1 = liftIO $ dyn4 ptr_glEnable v1

{-# NOINLINE ptr_glEnable #-}
ptr_glEnable :: FunPtr (GLenum -> IO ())
ptr_glEnable = unsafePerformIO $ getCommand "glEnable"

-- glEnableClientState ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glEnableClientState.xml OpenGL 2.x>.
glEnableClientState
  :: MonadIO m
  => GLenum -- ^ @array@ of type [EnableCap](Graphics-GL-Groups.html#EnableCap).
  -> m ()
glEnableClientState v1 = liftIO $ dyn4 ptr_glEnableClientState v1

{-# NOINLINE ptr_glEnableClientState #-}
ptr_glEnableClientState :: FunPtr (GLenum -> IO ())
ptr_glEnableClientState = unsafePerformIO $ getCommand "glEnableClientState"

-- glEnableClientStateIndexedEXT -----------------------------------------------

glEnableClientStateIndexedEXT
  :: MonadIO m
  => GLenum -- ^ @array@ of type [EnableCap](Graphics-GL-Groups.html#EnableCap).
  -> GLuint -- ^ @index@.
  -> m ()
glEnableClientStateIndexedEXT v1 v2 = liftIO $ dyn16 ptr_glEnableClientStateIndexedEXT v1 v2

{-# NOINLINE ptr_glEnableClientStateIndexedEXT #-}
ptr_glEnableClientStateIndexedEXT :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glEnableClientStateIndexedEXT = unsafePerformIO $ getCommand "glEnableClientStateIndexedEXT"

-- glEnableClientStateiEXT -----------------------------------------------------

glEnableClientStateiEXT
  :: MonadIO m
  => GLenum -- ^ @array@ of type [EnableCap](Graphics-GL-Groups.html#EnableCap).
  -> GLuint -- ^ @index@.
  -> m ()
glEnableClientStateiEXT v1 v2 = liftIO $ dyn16 ptr_glEnableClientStateiEXT v1 v2

{-# NOINLINE ptr_glEnableClientStateiEXT #-}
ptr_glEnableClientStateiEXT :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glEnableClientStateiEXT = unsafePerformIO $ getCommand "glEnableClientStateiEXT"

-- glEnableDriverControlQCOM ---------------------------------------------------

glEnableDriverControlQCOM
  :: MonadIO m
  => GLuint -- ^ @driverControl@.
  -> m ()
glEnableDriverControlQCOM v1 = liftIO $ dyn2 ptr_glEnableDriverControlQCOM v1

{-# NOINLINE ptr_glEnableDriverControlQCOM #-}
ptr_glEnableDriverControlQCOM :: FunPtr (GLuint -> IO ())
ptr_glEnableDriverControlQCOM = unsafePerformIO $ getCommand "glEnableDriverControlQCOM"

