--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F77
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

module Graphics.GL.Functions.F77 (
  glVertexStream3sATI,
  glVertexStream3svATI,
  glVertexStream4dATI,
  glVertexStream4dvATI,
  glVertexStream4fATI,
  glVertexStream4fvATI,
  glVertexStream4iATI,
  glVertexStream4ivATI,
  glVertexStream4sATI,
  glVertexStream4svATI,
  glVertexWeightPointerEXT,
  glVertexWeightfEXT,
  glVertexWeightfvEXT,
  glVertexWeighthNV,
  glVertexWeighthvNV,
  glVideoCaptureNV,
  glVideoCaptureStreamParameterdvNV,
  glVideoCaptureStreamParameterfvNV,
  glVideoCaptureStreamParameterivNV,
  glViewport,
  glViewportArrayv,
  glViewportArrayvNV,
  glViewportIndexedf,
  glViewportIndexedfNV,
  glViewportIndexedfv,
  glViewportIndexedfvNV,
  glViewportSwizzleNV,
  glWaitSync,
  glWaitSyncAPPLE,
  glWeightPathsNV,
  glWeightPointerARB,
  glWeightPointerOES,
  glWeightbvARB,
  glWeightdvARB,
  glWeightfvARB,
  glWeightivARB,
  glWeightsvARB,
  glWeightubvARB,
  glWeightuivARB,
  glWeightusvARB
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

-- glVertexStream3sATI ---------------------------------------------------------

glVertexStream3sATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type @VertexStreamATI@.
  -> GLshort -- ^ @x@.
  -> GLshort -- ^ @y@.
  -> GLshort -- ^ @z@.
  -> m ()
glVertexStream3sATI v1 v2 v3 v4 = liftIO $ dyn555 ptr_glVertexStream3sATI v1 v2 v3 v4

{-# NOINLINE ptr_glVertexStream3sATI #-}
ptr_glVertexStream3sATI :: FunPtr (GLenum -> GLshort -> GLshort -> GLshort -> IO ())
ptr_glVertexStream3sATI = unsafePerformIO $ getCommand "glVertexStream3sATI"

-- glVertexStream3svATI --------------------------------------------------------

glVertexStream3svATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type @VertexStreamATI@.
  -> Ptr GLshort -- ^ @coords@ pointing to @3@ elements of type @GLshort@.
  -> m ()
glVertexStream3svATI v1 v2 = liftIO $ dyn545 ptr_glVertexStream3svATI v1 v2

{-# NOINLINE ptr_glVertexStream3svATI #-}
ptr_glVertexStream3svATI :: FunPtr (GLenum -> Ptr GLshort -> IO ())
ptr_glVertexStream3svATI = unsafePerformIO $ getCommand "glVertexStream3svATI"

-- glVertexStream4dATI ---------------------------------------------------------

glVertexStream4dATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type @VertexStreamATI@.
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> GLdouble -- ^ @z@.
  -> GLdouble -- ^ @w@.
  -> m ()
glVertexStream4dATI v1 v2 v3 v4 v5 = liftIO $ dyn520 ptr_glVertexStream4dATI v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexStream4dATI #-}
ptr_glVertexStream4dATI :: FunPtr (GLenum -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glVertexStream4dATI = unsafePerformIO $ getCommand "glVertexStream4dATI"

-- glVertexStream4dvATI --------------------------------------------------------

glVertexStream4dvATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type @VertexStreamATI@.
  -> Ptr GLdouble -- ^ @coords@ pointing to @4@ elements of type @GLdouble@.
  -> m ()
glVertexStream4dvATI v1 v2 = liftIO $ dyn93 ptr_glVertexStream4dvATI v1 v2

{-# NOINLINE ptr_glVertexStream4dvATI #-}
ptr_glVertexStream4dvATI :: FunPtr (GLenum -> Ptr GLdouble -> IO ())
ptr_glVertexStream4dvATI = unsafePerformIO $ getCommand "glVertexStream4dvATI"

-- glVertexStream4fATI ---------------------------------------------------------

glVertexStream4fATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type @VertexStreamATI@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> GLfloat -- ^ @w@.
  -> m ()
glVertexStream4fATI v1 v2 v3 v4 v5 = liftIO $ dyn521 ptr_glVertexStream4fATI v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexStream4fATI #-}
ptr_glVertexStream4fATI :: FunPtr (GLenum -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glVertexStream4fATI = unsafePerformIO $ getCommand "glVertexStream4fATI"

-- glVertexStream4fvATI --------------------------------------------------------

glVertexStream4fvATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type @VertexStreamATI@.
  -> Ptr GLfloat -- ^ @coords@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glVertexStream4fvATI v1 v2 = liftIO $ dyn94 ptr_glVertexStream4fvATI v1 v2

{-# NOINLINE ptr_glVertexStream4fvATI #-}
ptr_glVertexStream4fvATI :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glVertexStream4fvATI = unsafePerformIO $ getCommand "glVertexStream4fvATI"

-- glVertexStream4iATI ---------------------------------------------------------

glVertexStream4iATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type @VertexStreamATI@.
  -> GLint -- ^ @x@.
  -> GLint -- ^ @y@.
  -> GLint -- ^ @z@.
  -> GLint -- ^ @w@.
  -> m ()
glVertexStream4iATI v1 v2 v3 v4 v5 = liftIO $ dyn265 ptr_glVertexStream4iATI v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexStream4iATI #-}
ptr_glVertexStream4iATI :: FunPtr (GLenum -> GLint -> GLint -> GLint -> GLint -> IO ())
ptr_glVertexStream4iATI = unsafePerformIO $ getCommand "glVertexStream4iATI"

-- glVertexStream4ivATI --------------------------------------------------------

glVertexStream4ivATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type @VertexStreamATI@.
  -> Ptr GLint -- ^ @coords@ pointing to @4@ elements of type @GLint@.
  -> m ()
glVertexStream4ivATI v1 v2 = liftIO $ dyn136 ptr_glVertexStream4ivATI v1 v2

{-# NOINLINE ptr_glVertexStream4ivATI #-}
ptr_glVertexStream4ivATI :: FunPtr (GLenum -> Ptr GLint -> IO ())
ptr_glVertexStream4ivATI = unsafePerformIO $ getCommand "glVertexStream4ivATI"

-- glVertexStream4sATI ---------------------------------------------------------

glVertexStream4sATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type @VertexStreamATI@.
  -> GLshort -- ^ @x@.
  -> GLshort -- ^ @y@.
  -> GLshort -- ^ @z@.
  -> GLshort -- ^ @w@.
  -> m ()
glVertexStream4sATI v1 v2 v3 v4 v5 = liftIO $ dyn559 ptr_glVertexStream4sATI v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexStream4sATI #-}
ptr_glVertexStream4sATI :: FunPtr (GLenum -> GLshort -> GLshort -> GLshort -> GLshort -> IO ())
ptr_glVertexStream4sATI = unsafePerformIO $ getCommand "glVertexStream4sATI"

-- glVertexStream4svATI --------------------------------------------------------

glVertexStream4svATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type @VertexStreamATI@.
  -> Ptr GLshort -- ^ @coords@ pointing to @4@ elements of type @GLshort@.
  -> m ()
glVertexStream4svATI v1 v2 = liftIO $ dyn545 ptr_glVertexStream4svATI v1 v2

{-# NOINLINE ptr_glVertexStream4svATI #-}
ptr_glVertexStream4svATI :: FunPtr (GLenum -> Ptr GLshort -> IO ())
ptr_glVertexStream4svATI = unsafePerformIO $ getCommand "glVertexStream4svATI"

-- glVertexWeightPointerEXT ----------------------------------------------------

glVertexWeightPointerEXT
  :: MonadIO m
  => GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type @VertexWeightPointerTypeEXT@.
  -> GLsizei -- ^ @stride@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(type,stride)@ elements of type @a@.
  -> m ()
glVertexWeightPointerEXT v1 v2 v3 v4 = liftIO $ dyn126 ptr_glVertexWeightPointerEXT v1 v2 v3 v4

{-# NOINLINE ptr_glVertexWeightPointerEXT #-}
ptr_glVertexWeightPointerEXT :: FunPtr (GLint -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glVertexWeightPointerEXT = unsafePerformIO $ getCommand "glVertexWeightPointerEXT"

-- glVertexWeightfEXT ----------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexWeightfvEXT'.
glVertexWeightfEXT
  :: MonadIO m
  => GLfloat -- ^ @weight@.
  -> m ()
glVertexWeightfEXT v1 = liftIO $ dyn79 ptr_glVertexWeightfEXT v1

{-# NOINLINE ptr_glVertexWeightfEXT #-}
ptr_glVertexWeightfEXT :: FunPtr (GLfloat -> IO ())
ptr_glVertexWeightfEXT = unsafePerformIO $ getCommand "glVertexWeightfEXT"

-- glVertexWeightfvEXT ---------------------------------------------------------

glVertexWeightfvEXT
  :: MonadIO m
  => Ptr GLfloat -- ^ @weight@ pointing to @1@ element of type @GLfloat@.
  -> m ()
glVertexWeightfvEXT v1 = liftIO $ dyn41 ptr_glVertexWeightfvEXT v1

{-# NOINLINE ptr_glVertexWeightfvEXT #-}
ptr_glVertexWeightfvEXT :: FunPtr (Ptr GLfloat -> IO ())
ptr_glVertexWeightfvEXT = unsafePerformIO $ getCommand "glVertexWeightfvEXT"

-- glVertexWeighthNV -----------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexWeighthvNV'.
glVertexWeighthNV
  :: MonadIO m
  => GLhalfNV -- ^ @weight@ of type @Half16NV@.
  -> m ()
glVertexWeighthNV v1 = liftIO $ dyn281 ptr_glVertexWeighthNV v1

{-# NOINLINE ptr_glVertexWeighthNV #-}
ptr_glVertexWeighthNV :: FunPtr (GLhalfNV -> IO ())
ptr_glVertexWeighthNV = unsafePerformIO $ getCommand "glVertexWeighthNV"

-- glVertexWeighthvNV ----------------------------------------------------------

glVertexWeighthvNV
  :: MonadIO m
  => Ptr GLhalfNV -- ^ @weight@ pointing to @1@ element of type @Half16NV@.
  -> m ()
glVertexWeighthvNV v1 = liftIO $ dyn99 ptr_glVertexWeighthvNV v1

{-# NOINLINE ptr_glVertexWeighthvNV #-}
ptr_glVertexWeighthvNV :: FunPtr (Ptr GLhalfNV -> IO ())
ptr_glVertexWeighthvNV = unsafePerformIO $ getCommand "glVertexWeighthvNV"

-- glVideoCaptureNV ------------------------------------------------------------

glVideoCaptureNV
  :: MonadIO m
  => GLuint -- ^ @video_capture_slot@.
  -> Ptr GLuint -- ^ @sequence_num@.
  -> Ptr GLuint64EXT -- ^ @capture_time@.
  -> m GLenum
glVideoCaptureNV v1 v2 v3 = liftIO $ dyn879 ptr_glVideoCaptureNV v1 v2 v3

{-# NOINLINE ptr_glVideoCaptureNV #-}
ptr_glVideoCaptureNV :: FunPtr (GLuint -> Ptr GLuint -> Ptr GLuint64EXT -> IO GLenum)
ptr_glVideoCaptureNV = unsafePerformIO $ getCommand "glVideoCaptureNV"

-- glVideoCaptureStreamParameterdvNV -------------------------------------------

glVideoCaptureStreamParameterdvNV
  :: MonadIO m
  => GLuint -- ^ @video_capture_slot@.
  -> GLuint -- ^ @stream@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLdouble -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLdouble@.
  -> m ()
glVideoCaptureStreamParameterdvNV v1 v2 v3 v4 = liftIO $ dyn444 ptr_glVideoCaptureStreamParameterdvNV v1 v2 v3 v4

{-# NOINLINE ptr_glVideoCaptureStreamParameterdvNV #-}
ptr_glVideoCaptureStreamParameterdvNV :: FunPtr (GLuint -> GLuint -> GLenum -> Ptr GLdouble -> IO ())
ptr_glVideoCaptureStreamParameterdvNV = unsafePerformIO $ getCommand "glVideoCaptureStreamParameterdvNV"

-- glVideoCaptureStreamParameterfvNV -------------------------------------------

glVideoCaptureStreamParameterfvNV
  :: MonadIO m
  => GLuint -- ^ @video_capture_slot@.
  -> GLuint -- ^ @stream@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glVideoCaptureStreamParameterfvNV v1 v2 v3 v4 = liftIO $ dyn445 ptr_glVideoCaptureStreamParameterfvNV v1 v2 v3 v4

{-# NOINLINE ptr_glVideoCaptureStreamParameterfvNV #-}
ptr_glVideoCaptureStreamParameterfvNV :: FunPtr (GLuint -> GLuint -> GLenum -> Ptr GLfloat -> IO ())
ptr_glVideoCaptureStreamParameterfvNV = unsafePerformIO $ getCommand "glVideoCaptureStreamParameterfvNV"

-- glVideoCaptureStreamParameterivNV -------------------------------------------

glVideoCaptureStreamParameterivNV
  :: MonadIO m
  => GLuint -- ^ @video_capture_slot@.
  -> GLuint -- ^ @stream@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glVideoCaptureStreamParameterivNV v1 v2 v3 v4 = liftIO $ dyn300 ptr_glVideoCaptureStreamParameterivNV v1 v2 v3 v4

{-# NOINLINE ptr_glVideoCaptureStreamParameterivNV #-}
ptr_glVideoCaptureStreamParameterivNV :: FunPtr (GLuint -> GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glVideoCaptureStreamParameterivNV = unsafePerformIO $ getCommand "glVideoCaptureStreamParameterivNV"

-- glViewport ------------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glViewport.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glViewport.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glViewport.xhtml OpenGL 4.x>.
glViewport
  :: MonadIO m
  => GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glViewport v1 v2 v3 v4 = liftIO $ dyn698 ptr_glViewport v1 v2 v3 v4

{-# NOINLINE ptr_glViewport #-}
ptr_glViewport :: FunPtr (GLint -> GLint -> GLsizei -> GLsizei -> IO ())
ptr_glViewport = unsafePerformIO $ getCommand "glViewport"

-- glViewportArrayv ------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glViewportArray.xhtml OpenGL 4.x>.
glViewportArrayv
  :: MonadIO m
  => GLuint -- ^ @first@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @v@ pointing to @COMPSIZE(count)@ elements of type @GLfloat@.
  -> m ()
glViewportArrayv v1 v2 v3 = liftIO $ dyn218 ptr_glViewportArrayv v1 v2 v3

{-# NOINLINE ptr_glViewportArrayv #-}
ptr_glViewportArrayv :: FunPtr (GLuint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glViewportArrayv = unsafePerformIO $ getCommand "glViewportArrayv"

-- glViewportArrayvNV ----------------------------------------------------------

-- | This command is an alias for 'glViewportArrayv'.
glViewportArrayvNV
  :: MonadIO m
  => GLuint -- ^ @first@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @v@ pointing to @COMPSIZE(count)@ elements of type @GLfloat@.
  -> m ()
glViewportArrayvNV v1 v2 v3 = liftIO $ dyn218 ptr_glViewportArrayvNV v1 v2 v3

{-# NOINLINE ptr_glViewportArrayvNV #-}
ptr_glViewportArrayvNV :: FunPtr (GLuint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glViewportArrayvNV = unsafePerformIO $ getCommand "glViewportArrayvNV"

-- glViewportIndexedf ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glViewportIndexed.xhtml OpenGL 4.x>.
glViewportIndexedf
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @w@.
  -> GLfloat -- ^ @h@.
  -> m ()
glViewportIndexedf v1 v2 v3 v4 v5 = liftIO $ dyn853 ptr_glViewportIndexedf v1 v2 v3 v4 v5

{-# NOINLINE ptr_glViewportIndexedf #-}
ptr_glViewportIndexedf :: FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glViewportIndexedf = unsafePerformIO $ getCommand "glViewportIndexedf"

-- glViewportIndexedfNV --------------------------------------------------------

-- | This command is an alias for 'glViewportIndexedf'.
glViewportIndexedfNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @w@.
  -> GLfloat -- ^ @h@.
  -> m ()
glViewportIndexedfNV v1 v2 v3 v4 v5 = liftIO $ dyn853 ptr_glViewportIndexedfNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glViewportIndexedfNV #-}
ptr_glViewportIndexedfNV :: FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glViewportIndexedfNV = unsafePerformIO $ getCommand "glViewportIndexedfNV"

-- glViewportIndexedfv ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glViewportIndexed.xhtml OpenGL 4.x>.
glViewportIndexedfv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLfloat -- ^ @v@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glViewportIndexedfv v1 v2 = liftIO $ dyn377 ptr_glViewportIndexedfv v1 v2

{-# NOINLINE ptr_glViewportIndexedfv #-}
ptr_glViewportIndexedfv :: FunPtr (GLuint -> Ptr GLfloat -> IO ())
ptr_glViewportIndexedfv = unsafePerformIO $ getCommand "glViewportIndexedfv"

-- glViewportIndexedfvNV -------------------------------------------------------

-- | This command is an alias for 'glViewportIndexedfv'.
glViewportIndexedfvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLfloat -- ^ @v@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glViewportIndexedfvNV v1 v2 = liftIO $ dyn377 ptr_glViewportIndexedfvNV v1 v2

{-# NOINLINE ptr_glViewportIndexedfvNV #-}
ptr_glViewportIndexedfvNV :: FunPtr (GLuint -> Ptr GLfloat -> IO ())
ptr_glViewportIndexedfvNV = unsafePerformIO $ getCommand "glViewportIndexedfvNV"

-- glViewportSwizzleNV ---------------------------------------------------------

glViewportSwizzleNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @swizzlex@.
  -> GLenum -- ^ @swizzley@.
  -> GLenum -- ^ @swizzlez@.
  -> GLenum -- ^ @swizzlew@.
  -> m ()
glViewportSwizzleNV v1 v2 v3 v4 v5 = liftIO $ dyn54 ptr_glViewportSwizzleNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glViewportSwizzleNV #-}
ptr_glViewportSwizzleNV :: FunPtr (GLuint -> GLenum -> GLenum -> GLenum -> GLenum -> IO ())
ptr_glViewportSwizzleNV = unsafePerformIO $ getCommand "glViewportSwizzleNV"

-- glWaitSync ------------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glWaitSync.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glWaitSync.xhtml OpenGL 4.x>.
glWaitSync
  :: MonadIO m
  => GLsync -- ^ @sync@ of type @sync@.
  -> GLbitfield -- ^ @flags@.
  -> GLuint64 -- ^ @timeout@.
  -> m ()
glWaitSync v1 v2 v3 = liftIO $ dyn880 ptr_glWaitSync v1 v2 v3

{-# NOINLINE ptr_glWaitSync #-}
ptr_glWaitSync :: FunPtr (GLsync -> GLbitfield -> GLuint64 -> IO ())
ptr_glWaitSync = unsafePerformIO $ getCommand "glWaitSync"

-- glWaitSyncAPPLE -------------------------------------------------------------

-- | This command is an alias for 'glWaitSync'.
glWaitSyncAPPLE
  :: MonadIO m
  => GLsync -- ^ @sync@.
  -> GLbitfield -- ^ @flags@.
  -> GLuint64 -- ^ @timeout@.
  -> m ()
glWaitSyncAPPLE v1 v2 v3 = liftIO $ dyn880 ptr_glWaitSyncAPPLE v1 v2 v3

{-# NOINLINE ptr_glWaitSyncAPPLE #-}
ptr_glWaitSyncAPPLE :: FunPtr (GLsync -> GLbitfield -> GLuint64 -> IO ())
ptr_glWaitSyncAPPLE = unsafePerformIO $ getCommand "glWaitSyncAPPLE"

-- glWeightPathsNV -------------------------------------------------------------

glWeightPathsNV
  :: MonadIO m
  => GLuint -- ^ @resultPath@ of type @Path@.
  -> GLsizei -- ^ @numPaths@.
  -> Ptr GLuint -- ^ @paths@ pointing to @numPaths@ elements of type @Path@.
  -> Ptr GLfloat -- ^ @weights@ pointing to @numPaths@ elements of type @GLfloat@.
  -> m ()
glWeightPathsNV v1 v2 v3 v4 = liftIO $ dyn881 ptr_glWeightPathsNV v1 v2 v3 v4

{-# NOINLINE ptr_glWeightPathsNV #-}
ptr_glWeightPathsNV :: FunPtr (GLuint -> GLsizei -> Ptr GLuint -> Ptr GLfloat -> IO ())
ptr_glWeightPathsNV = unsafePerformIO $ getCommand "glWeightPathsNV"

-- glWeightPointerARB ----------------------------------------------------------

glWeightPointerARB
  :: MonadIO m
  => GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type @WeightPointerTypeARB@.
  -> GLsizei -- ^ @stride@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(type,stride)@ elements of type @a@.
  -> m ()
glWeightPointerARB v1 v2 v3 v4 = liftIO $ dyn126 ptr_glWeightPointerARB v1 v2 v3 v4

{-# NOINLINE ptr_glWeightPointerARB #-}
ptr_glWeightPointerARB :: FunPtr (GLint -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glWeightPointerARB = unsafePerformIO $ getCommand "glWeightPointerARB"

-- glWeightPointerOES ----------------------------------------------------------

glWeightPointerOES
  :: MonadIO m
  => GLint -- ^ @size@.
  -> GLenum -- ^ @type@.
  -> GLsizei -- ^ @stride@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(type,stride)@ elements of type @a@.
  -> m ()
glWeightPointerOES v1 v2 v3 v4 = liftIO $ dyn126 ptr_glWeightPointerOES v1 v2 v3 v4

{-# NOINLINE ptr_glWeightPointerOES #-}
ptr_glWeightPointerOES :: FunPtr (GLint -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glWeightPointerOES = unsafePerformIO $ getCommand "glWeightPointerOES"

-- glWeightbvARB ---------------------------------------------------------------

glWeightbvARB
  :: MonadIO m
  => GLint -- ^ @size@.
  -> Ptr GLbyte -- ^ @weights@ pointing to @size@ elements of type @GLbyte@.
  -> m ()
glWeightbvARB v1 v2 = liftIO $ dyn882 ptr_glWeightbvARB v1 v2

{-# NOINLINE ptr_glWeightbvARB #-}
ptr_glWeightbvARB :: FunPtr (GLint -> Ptr GLbyte -> IO ())
ptr_glWeightbvARB = unsafePerformIO $ getCommand "glWeightbvARB"

-- glWeightdvARB ---------------------------------------------------------------

glWeightdvARB
  :: MonadIO m
  => GLint -- ^ @size@.
  -> Ptr GLdouble -- ^ @weights@ pointing to @size@ elements of type @GLdouble@.
  -> m ()
glWeightdvARB v1 v2 = liftIO $ dyn883 ptr_glWeightdvARB v1 v2

{-# NOINLINE ptr_glWeightdvARB #-}
ptr_glWeightdvARB :: FunPtr (GLint -> Ptr GLdouble -> IO ())
ptr_glWeightdvARB = unsafePerformIO $ getCommand "glWeightdvARB"

-- glWeightfvARB ---------------------------------------------------------------

glWeightfvARB
  :: MonadIO m
  => GLint -- ^ @size@.
  -> Ptr GLfloat -- ^ @weights@ pointing to @size@ elements of type @GLfloat@.
  -> m ()
glWeightfvARB v1 v2 = liftIO $ dyn884 ptr_glWeightfvARB v1 v2

{-# NOINLINE ptr_glWeightfvARB #-}
ptr_glWeightfvARB :: FunPtr (GLint -> Ptr GLfloat -> IO ())
ptr_glWeightfvARB = unsafePerformIO $ getCommand "glWeightfvARB"

-- glWeightivARB ---------------------------------------------------------------

glWeightivARB
  :: MonadIO m
  => GLint -- ^ @size@.
  -> Ptr GLint -- ^ @weights@ pointing to @size@ elements of type @GLint@.
  -> m ()
glWeightivARB v1 v2 = liftIO $ dyn885 ptr_glWeightivARB v1 v2

{-# NOINLINE ptr_glWeightivARB #-}
ptr_glWeightivARB :: FunPtr (GLint -> Ptr GLint -> IO ())
ptr_glWeightivARB = unsafePerformIO $ getCommand "glWeightivARB"

-- glWeightsvARB ---------------------------------------------------------------

glWeightsvARB
  :: MonadIO m
  => GLint -- ^ @size@.
  -> Ptr GLshort -- ^ @weights@ pointing to @size@ elements of type @GLshort@.
  -> m ()
glWeightsvARB v1 v2 = liftIO $ dyn886 ptr_glWeightsvARB v1 v2

{-# NOINLINE ptr_glWeightsvARB #-}
ptr_glWeightsvARB :: FunPtr (GLint -> Ptr GLshort -> IO ())
ptr_glWeightsvARB = unsafePerformIO $ getCommand "glWeightsvARB"

-- glWeightubvARB --------------------------------------------------------------

glWeightubvARB
  :: MonadIO m
  => GLint -- ^ @size@.
  -> Ptr GLubyte -- ^ @weights@ pointing to @size@ elements of type @GLubyte@.
  -> m ()
glWeightubvARB v1 v2 = liftIO $ dyn517 ptr_glWeightubvARB v1 v2

{-# NOINLINE ptr_glWeightubvARB #-}
ptr_glWeightubvARB :: FunPtr (GLint -> Ptr GLubyte -> IO ())
ptr_glWeightubvARB = unsafePerformIO $ getCommand "glWeightubvARB"

-- glWeightuivARB --------------------------------------------------------------

glWeightuivARB
  :: MonadIO m
  => GLint -- ^ @size@.
  -> Ptr GLuint -- ^ @weights@ pointing to @size@ elements of type @GLuint@.
  -> m ()
glWeightuivARB v1 v2 = liftIO $ dyn518 ptr_glWeightuivARB v1 v2

{-# NOINLINE ptr_glWeightuivARB #-}
ptr_glWeightuivARB :: FunPtr (GLint -> Ptr GLuint -> IO ())
ptr_glWeightuivARB = unsafePerformIO $ getCommand "glWeightuivARB"

-- glWeightusvARB --------------------------------------------------------------

glWeightusvARB
  :: MonadIO m
  => GLint -- ^ @size@.
  -> Ptr GLushort -- ^ @weights@ pointing to @size@ elements of type @GLushort@.
  -> m ()
glWeightusvARB v1 v2 = liftIO $ dyn519 ptr_glWeightusvARB v1 v2

{-# NOINLINE ptr_glWeightusvARB #-}
ptr_glWeightusvARB :: FunPtr (GLint -> Ptr GLushort -> IO ())
ptr_glWeightusvARB = unsafePerformIO $ getCommand "glWeightusvARB"

