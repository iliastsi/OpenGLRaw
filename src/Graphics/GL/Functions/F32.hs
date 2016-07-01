--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F32
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

module Graphics.GL.Functions.F32 (
  glGetVertexAttribfvNV,
  glGetVertexAttribiv,
  glGetVertexAttribivARB,
  glGetVertexAttribivNV,
  glGetVideoCaptureStreamdvNV,
  glGetVideoCaptureStreamfvNV,
  glGetVideoCaptureStreamivNV,
  glGetVideoCaptureivNV,
  glGetVideoi64vNV,
  glGetVideoivNV,
  glGetVideoui64vNV,
  glGetVideouivNV,
  glGetnColorTable,
  glGetnColorTableARB,
  glGetnCompressedTexImage,
  glGetnCompressedTexImageARB,
  glGetnConvolutionFilter,
  glGetnConvolutionFilterARB,
  glGetnHistogram,
  glGetnHistogramARB,
  glGetnMapdv,
  glGetnMapdvARB,
  glGetnMapfv,
  glGetnMapfvARB,
  glGetnMapiv,
  glGetnMapivARB,
  glGetnMinmax,
  glGetnMinmaxARB,
  glGetnPixelMapfv,
  glGetnPixelMapfvARB,
  glGetnPixelMapuiv,
  glGetnPixelMapuivARB,
  glGetnPixelMapusv,
  glGetnPixelMapusvARB,
  glGetnPolygonStipple,
  glGetnPolygonStippleARB,
  glGetnSeparableFilter,
  glGetnSeparableFilterARB,
  glGetnTexImage,
  glGetnTexImageARB
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

-- glGetVertexAttribfvNV -------------------------------------------------------

-- | This command is an alias for 'glGetVertexAttribfv'.
glGetVertexAttribfvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@ of type @VertexAttribEnumNV@.
  -> Ptr GLfloat -- ^ @params@ pointing to @1@ element of type @GLfloat@.
  -> m ()
glGetVertexAttribfvNV v1 v2 v3 = liftIO $ dyn349 ptr_glGetVertexAttribfvNV v1 v2 v3

{-# NOINLINE ptr_glGetVertexAttribfvNV #-}
ptr_glGetVertexAttribfvNV :: FunPtr (GLuint -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetVertexAttribfvNV = unsafePerformIO $ getCommand "glGetVertexAttribfvNV"

-- glGetVertexAttribiv ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetVertexAttrib.xhtml OpenGL 4.x>.
glGetVertexAttribiv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@ of type @VertexAttribPropertyARB@.
  -> Ptr GLint -- ^ @params@ pointing to @4@ elements of type @GLint@.
  -> m ()
glGetVertexAttribiv v1 v2 v3 = liftIO $ dyn334 ptr_glGetVertexAttribiv v1 v2 v3

{-# NOINLINE ptr_glGetVertexAttribiv #-}
ptr_glGetVertexAttribiv :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetVertexAttribiv = unsafePerformIO $ getCommand "glGetVertexAttribiv"

-- glGetVertexAttribivARB ------------------------------------------------------

-- | This command is an alias for 'glGetVertexAttribiv'.
glGetVertexAttribivARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@ of type @VertexAttribPropertyARB@.
  -> Ptr GLint -- ^ @params@ pointing to @4@ elements of type @GLint@.
  -> m ()
glGetVertexAttribivARB v1 v2 v3 = liftIO $ dyn334 ptr_glGetVertexAttribivARB v1 v2 v3

{-# NOINLINE ptr_glGetVertexAttribivARB #-}
ptr_glGetVertexAttribivARB :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetVertexAttribivARB = unsafePerformIO $ getCommand "glGetVertexAttribivARB"

-- glGetVertexAttribivNV -------------------------------------------------------

-- | This command is an alias for 'glGetVertexAttribiv'.
glGetVertexAttribivNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@ of type @VertexAttribEnumNV@.
  -> Ptr GLint -- ^ @params@ pointing to @1@ element of type @GLint@.
  -> m ()
glGetVertexAttribivNV v1 v2 v3 = liftIO $ dyn334 ptr_glGetVertexAttribivNV v1 v2 v3

{-# NOINLINE ptr_glGetVertexAttribivNV #-}
ptr_glGetVertexAttribivNV :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetVertexAttribivNV = unsafePerformIO $ getCommand "glGetVertexAttribivNV"

-- glGetVideoCaptureStreamdvNV -------------------------------------------------

glGetVideoCaptureStreamdvNV
  :: MonadIO m
  => GLuint -- ^ @video_capture_slot@.
  -> GLuint -- ^ @stream@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLdouble -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLdouble@.
  -> m ()
glGetVideoCaptureStreamdvNV v1 v2 v3 v4 = liftIO $ dyn444 ptr_glGetVideoCaptureStreamdvNV v1 v2 v3 v4

{-# NOINLINE ptr_glGetVideoCaptureStreamdvNV #-}
ptr_glGetVideoCaptureStreamdvNV :: FunPtr (GLuint -> GLuint -> GLenum -> Ptr GLdouble -> IO ())
ptr_glGetVideoCaptureStreamdvNV = unsafePerformIO $ getCommand "glGetVideoCaptureStreamdvNV"

-- glGetVideoCaptureStreamfvNV -------------------------------------------------

glGetVideoCaptureStreamfvNV
  :: MonadIO m
  => GLuint -- ^ @video_capture_slot@.
  -> GLuint -- ^ @stream@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetVideoCaptureStreamfvNV v1 v2 v3 v4 = liftIO $ dyn445 ptr_glGetVideoCaptureStreamfvNV v1 v2 v3 v4

{-# NOINLINE ptr_glGetVideoCaptureStreamfvNV #-}
ptr_glGetVideoCaptureStreamfvNV :: FunPtr (GLuint -> GLuint -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetVideoCaptureStreamfvNV = unsafePerformIO $ getCommand "glGetVideoCaptureStreamfvNV"

-- glGetVideoCaptureStreamivNV -------------------------------------------------

glGetVideoCaptureStreamivNV
  :: MonadIO m
  => GLuint -- ^ @video_capture_slot@.
  -> GLuint -- ^ @stream@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetVideoCaptureStreamivNV v1 v2 v3 v4 = liftIO $ dyn300 ptr_glGetVideoCaptureStreamivNV v1 v2 v3 v4

{-# NOINLINE ptr_glGetVideoCaptureStreamivNV #-}
ptr_glGetVideoCaptureStreamivNV :: FunPtr (GLuint -> GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetVideoCaptureStreamivNV = unsafePerformIO $ getCommand "glGetVideoCaptureStreamivNV"

-- glGetVideoCaptureivNV -------------------------------------------------------

glGetVideoCaptureivNV
  :: MonadIO m
  => GLuint -- ^ @video_capture_slot@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetVideoCaptureivNV v1 v2 v3 = liftIO $ dyn334 ptr_glGetVideoCaptureivNV v1 v2 v3

{-# NOINLINE ptr_glGetVideoCaptureivNV #-}
ptr_glGetVideoCaptureivNV :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetVideoCaptureivNV = unsafePerformIO $ getCommand "glGetVideoCaptureivNV"

-- glGetVideoi64vNV ------------------------------------------------------------

glGetVideoi64vNV
  :: MonadIO m
  => GLuint -- ^ @video_slot@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint64EXT -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint64EXT@.
  -> m ()
glGetVideoi64vNV v1 v2 v3 = liftIO $ dyn443 ptr_glGetVideoi64vNV v1 v2 v3

{-# NOINLINE ptr_glGetVideoi64vNV #-}
ptr_glGetVideoi64vNV :: FunPtr (GLuint -> GLenum -> Ptr GLint64EXT -> IO ())
ptr_glGetVideoi64vNV = unsafePerformIO $ getCommand "glGetVideoi64vNV"

-- glGetVideoivNV --------------------------------------------------------------

glGetVideoivNV
  :: MonadIO m
  => GLuint -- ^ @video_slot@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetVideoivNV v1 v2 v3 = liftIO $ dyn334 ptr_glGetVideoivNV v1 v2 v3

{-# NOINLINE ptr_glGetVideoivNV #-}
ptr_glGetVideoivNV :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetVideoivNV = unsafePerformIO $ getCommand "glGetVideoivNV"

-- glGetVideoui64vNV -----------------------------------------------------------

glGetVideoui64vNV
  :: MonadIO m
  => GLuint -- ^ @video_slot@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLuint64EXT -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLuint64EXT@.
  -> m ()
glGetVideoui64vNV v1 v2 v3 = liftIO $ dyn360 ptr_glGetVideoui64vNV v1 v2 v3

{-# NOINLINE ptr_glGetVideoui64vNV #-}
ptr_glGetVideoui64vNV :: FunPtr (GLuint -> GLenum -> Ptr GLuint64EXT -> IO ())
ptr_glGetVideoui64vNV = unsafePerformIO $ getCommand "glGetVideoui64vNV"

-- glGetVideouivNV -------------------------------------------------------------

glGetVideouivNV
  :: MonadIO m
  => GLuint -- ^ @video_slot@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLuint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLuint@.
  -> m ()
glGetVideouivNV v1 v2 v3 = liftIO $ dyn375 ptr_glGetVideouivNV v1 v2 v3

{-# NOINLINE ptr_glGetVideouivNV #-}
ptr_glGetVideouivNV :: FunPtr (GLuint -> GLenum -> Ptr GLuint -> IO ())
ptr_glGetVideouivNV = unsafePerformIO $ getCommand "glGetVideouivNV"

-- glGetnColorTable ------------------------------------------------------------

glGetnColorTable
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @format@.
  -> GLenum -- ^ @type@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr a -- ^ @table@.
  -> m ()
glGetnColorTable v1 v2 v3 v4 v5 = liftIO $ dyn446 ptr_glGetnColorTable v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetnColorTable #-}
ptr_glGetnColorTable :: FunPtr (GLenum -> GLenum -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glGetnColorTable = unsafePerformIO $ getCommand "glGetnColorTable"

-- glGetnColorTableARB ---------------------------------------------------------

glGetnColorTableARB
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @format@.
  -> GLenum -- ^ @type@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr a -- ^ @table@ pointing to @bufSize@ elements of type @a@.
  -> m ()
glGetnColorTableARB v1 v2 v3 v4 v5 = liftIO $ dyn446 ptr_glGetnColorTableARB v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetnColorTableARB #-}
ptr_glGetnColorTableARB :: FunPtr (GLenum -> GLenum -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glGetnColorTableARB = unsafePerformIO $ getCommand "glGetnColorTableARB"

-- glGetnCompressedTexImage ----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetCompressedTexImage.xhtml OpenGL 4.x>.
glGetnCompressedTexImage
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLint -- ^ @lod@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr a -- ^ @pixels@.
  -> m ()
glGetnCompressedTexImage v1 v2 v3 v4 = liftIO $ dyn447 ptr_glGetnCompressedTexImage v1 v2 v3 v4

{-# NOINLINE ptr_glGetnCompressedTexImage #-}
ptr_glGetnCompressedTexImage :: FunPtr (GLenum -> GLint -> GLsizei -> Ptr a -> IO ())
ptr_glGetnCompressedTexImage = unsafePerformIO $ getCommand "glGetnCompressedTexImage"

-- glGetnCompressedTexImageARB -------------------------------------------------

glGetnCompressedTexImageARB
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLint -- ^ @lod@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr a -- ^ @img@ pointing to @bufSize@ elements of type @a@.
  -> m ()
glGetnCompressedTexImageARB v1 v2 v3 v4 = liftIO $ dyn447 ptr_glGetnCompressedTexImageARB v1 v2 v3 v4

{-# NOINLINE ptr_glGetnCompressedTexImageARB #-}
ptr_glGetnCompressedTexImageARB :: FunPtr (GLenum -> GLint -> GLsizei -> Ptr a -> IO ())
ptr_glGetnCompressedTexImageARB = unsafePerformIO $ getCommand "glGetnCompressedTexImageARB"

-- glGetnConvolutionFilter -----------------------------------------------------

glGetnConvolutionFilter
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @format@.
  -> GLenum -- ^ @type@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr a -- ^ @image@.
  -> m ()
glGetnConvolutionFilter v1 v2 v3 v4 v5 = liftIO $ dyn446 ptr_glGetnConvolutionFilter v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetnConvolutionFilter #-}
ptr_glGetnConvolutionFilter :: FunPtr (GLenum -> GLenum -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glGetnConvolutionFilter = unsafePerformIO $ getCommand "glGetnConvolutionFilter"

-- glGetnConvolutionFilterARB --------------------------------------------------

glGetnConvolutionFilterARB
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @format@.
  -> GLenum -- ^ @type@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr a -- ^ @image@ pointing to @bufSize@ elements of type @a@.
  -> m ()
glGetnConvolutionFilterARB v1 v2 v3 v4 v5 = liftIO $ dyn446 ptr_glGetnConvolutionFilterARB v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetnConvolutionFilterARB #-}
ptr_glGetnConvolutionFilterARB :: FunPtr (GLenum -> GLenum -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glGetnConvolutionFilterARB = unsafePerformIO $ getCommand "glGetnConvolutionFilterARB"

-- glGetnHistogram -------------------------------------------------------------

glGetnHistogram
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLboolean -- ^ @reset@.
  -> GLenum -- ^ @format@.
  -> GLenum -- ^ @type@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr a -- ^ @values@.
  -> m ()
glGetnHistogram v1 v2 v3 v4 v5 v6 = liftIO $ dyn448 ptr_glGetnHistogram v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glGetnHistogram #-}
ptr_glGetnHistogram :: FunPtr (GLenum -> GLboolean -> GLenum -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glGetnHistogram = unsafePerformIO $ getCommand "glGetnHistogram"

-- glGetnHistogramARB ----------------------------------------------------------

glGetnHistogramARB
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLboolean -- ^ @reset@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLenum -- ^ @format@.
  -> GLenum -- ^ @type@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr a -- ^ @values@ pointing to @bufSize@ elements of type @a@.
  -> m ()
glGetnHistogramARB v1 v2 v3 v4 v5 v6 = liftIO $ dyn448 ptr_glGetnHistogramARB v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glGetnHistogramARB #-}
ptr_glGetnHistogramARB :: FunPtr (GLenum -> GLboolean -> GLenum -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glGetnHistogramARB = unsafePerformIO $ getCommand "glGetnHistogramARB"

-- glGetnMapdv -----------------------------------------------------------------

glGetnMapdv
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @query@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLdouble -- ^ @v@.
  -> m ()
glGetnMapdv v1 v2 v3 v4 = liftIO $ dyn449 ptr_glGetnMapdv v1 v2 v3 v4

{-# NOINLINE ptr_glGetnMapdv #-}
ptr_glGetnMapdv :: FunPtr (GLenum -> GLenum -> GLsizei -> Ptr GLdouble -> IO ())
ptr_glGetnMapdv = unsafePerformIO $ getCommand "glGetnMapdv"

-- glGetnMapdvARB --------------------------------------------------------------

glGetnMapdvARB
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @query@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLdouble -- ^ @v@ pointing to @bufSize@ elements of type @GLdouble@.
  -> m ()
glGetnMapdvARB v1 v2 v3 v4 = liftIO $ dyn449 ptr_glGetnMapdvARB v1 v2 v3 v4

{-# NOINLINE ptr_glGetnMapdvARB #-}
ptr_glGetnMapdvARB :: FunPtr (GLenum -> GLenum -> GLsizei -> Ptr GLdouble -> IO ())
ptr_glGetnMapdvARB = unsafePerformIO $ getCommand "glGetnMapdvARB"

-- glGetnMapfv -----------------------------------------------------------------

glGetnMapfv
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @query@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLfloat -- ^ @v@.
  -> m ()
glGetnMapfv v1 v2 v3 v4 = liftIO $ dyn450 ptr_glGetnMapfv v1 v2 v3 v4

{-# NOINLINE ptr_glGetnMapfv #-}
ptr_glGetnMapfv :: FunPtr (GLenum -> GLenum -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glGetnMapfv = unsafePerformIO $ getCommand "glGetnMapfv"

-- glGetnMapfvARB --------------------------------------------------------------

glGetnMapfvARB
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @query@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLfloat -- ^ @v@ pointing to @bufSize@ elements of type @GLfloat@.
  -> m ()
glGetnMapfvARB v1 v2 v3 v4 = liftIO $ dyn450 ptr_glGetnMapfvARB v1 v2 v3 v4

{-# NOINLINE ptr_glGetnMapfvARB #-}
ptr_glGetnMapfvARB :: FunPtr (GLenum -> GLenum -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glGetnMapfvARB = unsafePerformIO $ getCommand "glGetnMapfvARB"

-- glGetnMapiv -----------------------------------------------------------------

glGetnMapiv
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @query@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLint -- ^ @v@.
  -> m ()
glGetnMapiv v1 v2 v3 v4 = liftIO $ dyn451 ptr_glGetnMapiv v1 v2 v3 v4

{-# NOINLINE ptr_glGetnMapiv #-}
ptr_glGetnMapiv :: FunPtr (GLenum -> GLenum -> GLsizei -> Ptr GLint -> IO ())
ptr_glGetnMapiv = unsafePerformIO $ getCommand "glGetnMapiv"

-- glGetnMapivARB --------------------------------------------------------------

glGetnMapivARB
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @query@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLint -- ^ @v@ pointing to @bufSize@ elements of type @GLint@.
  -> m ()
glGetnMapivARB v1 v2 v3 v4 = liftIO $ dyn451 ptr_glGetnMapivARB v1 v2 v3 v4

{-# NOINLINE ptr_glGetnMapivARB #-}
ptr_glGetnMapivARB :: FunPtr (GLenum -> GLenum -> GLsizei -> Ptr GLint -> IO ())
ptr_glGetnMapivARB = unsafePerformIO $ getCommand "glGetnMapivARB"

-- glGetnMinmax ----------------------------------------------------------------

glGetnMinmax
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLboolean -- ^ @reset@.
  -> GLenum -- ^ @format@.
  -> GLenum -- ^ @type@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr a -- ^ @values@.
  -> m ()
glGetnMinmax v1 v2 v3 v4 v5 v6 = liftIO $ dyn448 ptr_glGetnMinmax v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glGetnMinmax #-}
ptr_glGetnMinmax :: FunPtr (GLenum -> GLboolean -> GLenum -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glGetnMinmax = unsafePerformIO $ getCommand "glGetnMinmax"

-- glGetnMinmaxARB -------------------------------------------------------------

glGetnMinmaxARB
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLboolean -- ^ @reset@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLenum -- ^ @format@.
  -> GLenum -- ^ @type@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr a -- ^ @values@ pointing to @bufSize@ elements of type @a@.
  -> m ()
glGetnMinmaxARB v1 v2 v3 v4 v5 v6 = liftIO $ dyn448 ptr_glGetnMinmaxARB v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glGetnMinmaxARB #-}
ptr_glGetnMinmaxARB :: FunPtr (GLenum -> GLboolean -> GLenum -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glGetnMinmaxARB = unsafePerformIO $ getCommand "glGetnMinmaxARB"

-- glGetnPixelMapfv ------------------------------------------------------------

glGetnPixelMapfv
  :: MonadIO m
  => GLenum -- ^ @map@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLfloat -- ^ @values@.
  -> m ()
glGetnPixelMapfv v1 v2 v3 = liftIO $ dyn225 ptr_glGetnPixelMapfv v1 v2 v3

{-# NOINLINE ptr_glGetnPixelMapfv #-}
ptr_glGetnPixelMapfv :: FunPtr (GLenum -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glGetnPixelMapfv = unsafePerformIO $ getCommand "glGetnPixelMapfv"

-- glGetnPixelMapfvARB ---------------------------------------------------------

glGetnPixelMapfvARB
  :: MonadIO m
  => GLenum -- ^ @map@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLfloat -- ^ @values@ pointing to @bufSize@ elements of type @GLfloat@.
  -> m ()
glGetnPixelMapfvARB v1 v2 v3 = liftIO $ dyn225 ptr_glGetnPixelMapfvARB v1 v2 v3

{-# NOINLINE ptr_glGetnPixelMapfvARB #-}
ptr_glGetnPixelMapfvARB :: FunPtr (GLenum -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glGetnPixelMapfvARB = unsafePerformIO $ getCommand "glGetnPixelMapfvARB"

-- glGetnPixelMapuiv -----------------------------------------------------------

glGetnPixelMapuiv
  :: MonadIO m
  => GLenum -- ^ @map@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLuint -- ^ @values@.
  -> m ()
glGetnPixelMapuiv v1 v2 v3 = liftIO $ dyn197 ptr_glGetnPixelMapuiv v1 v2 v3

{-# NOINLINE ptr_glGetnPixelMapuiv #-}
ptr_glGetnPixelMapuiv :: FunPtr (GLenum -> GLsizei -> Ptr GLuint -> IO ())
ptr_glGetnPixelMapuiv = unsafePerformIO $ getCommand "glGetnPixelMapuiv"

-- glGetnPixelMapuivARB --------------------------------------------------------

glGetnPixelMapuivARB
  :: MonadIO m
  => GLenum -- ^ @map@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLuint -- ^ @values@ pointing to @bufSize@ elements of type @GLuint@.
  -> m ()
glGetnPixelMapuivARB v1 v2 v3 = liftIO $ dyn197 ptr_glGetnPixelMapuivARB v1 v2 v3

{-# NOINLINE ptr_glGetnPixelMapuivARB #-}
ptr_glGetnPixelMapuivARB :: FunPtr (GLenum -> GLsizei -> Ptr GLuint -> IO ())
ptr_glGetnPixelMapuivARB = unsafePerformIO $ getCommand "glGetnPixelMapuivARB"

-- glGetnPixelMapusv -----------------------------------------------------------

glGetnPixelMapusv
  :: MonadIO m
  => GLenum -- ^ @map@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLushort -- ^ @values@.
  -> m ()
glGetnPixelMapusv v1 v2 v3 = liftIO $ dyn452 ptr_glGetnPixelMapusv v1 v2 v3

{-# NOINLINE ptr_glGetnPixelMapusv #-}
ptr_glGetnPixelMapusv :: FunPtr (GLenum -> GLsizei -> Ptr GLushort -> IO ())
ptr_glGetnPixelMapusv = unsafePerformIO $ getCommand "glGetnPixelMapusv"

-- glGetnPixelMapusvARB --------------------------------------------------------

glGetnPixelMapusvARB
  :: MonadIO m
  => GLenum -- ^ @map@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLushort -- ^ @values@ pointing to @bufSize@ elements of type @GLushort@.
  -> m ()
glGetnPixelMapusvARB v1 v2 v3 = liftIO $ dyn452 ptr_glGetnPixelMapusvARB v1 v2 v3

{-# NOINLINE ptr_glGetnPixelMapusvARB #-}
ptr_glGetnPixelMapusvARB :: FunPtr (GLenum -> GLsizei -> Ptr GLushort -> IO ())
ptr_glGetnPixelMapusvARB = unsafePerformIO $ getCommand "glGetnPixelMapusvARB"

-- glGetnPolygonStipple --------------------------------------------------------

glGetnPolygonStipple
  :: MonadIO m
  => GLsizei -- ^ @bufSize@.
  -> Ptr GLubyte -- ^ @pattern@.
  -> m ()
glGetnPolygonStipple v1 v2 = liftIO $ dyn453 ptr_glGetnPolygonStipple v1 v2

{-# NOINLINE ptr_glGetnPolygonStipple #-}
ptr_glGetnPolygonStipple :: FunPtr (GLsizei -> Ptr GLubyte -> IO ())
ptr_glGetnPolygonStipple = unsafePerformIO $ getCommand "glGetnPolygonStipple"

-- glGetnPolygonStippleARB -----------------------------------------------------

glGetnPolygonStippleARB
  :: MonadIO m
  => GLsizei -- ^ @bufSize@.
  -> Ptr GLubyte -- ^ @pattern@ pointing to @bufSize@ elements of type @GLubyte@.
  -> m ()
glGetnPolygonStippleARB v1 v2 = liftIO $ dyn453 ptr_glGetnPolygonStippleARB v1 v2

{-# NOINLINE ptr_glGetnPolygonStippleARB #-}
ptr_glGetnPolygonStippleARB :: FunPtr (GLsizei -> Ptr GLubyte -> IO ())
ptr_glGetnPolygonStippleARB = unsafePerformIO $ getCommand "glGetnPolygonStippleARB"

-- glGetnSeparableFilter -------------------------------------------------------

glGetnSeparableFilter
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @format@.
  -> GLenum -- ^ @type@.
  -> GLsizei -- ^ @rowBufSize@.
  -> Ptr a -- ^ @row@.
  -> GLsizei -- ^ @columnBufSize@.
  -> Ptr b -- ^ @column@.
  -> Ptr c -- ^ @span@.
  -> m ()
glGetnSeparableFilter v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn454 ptr_glGetnSeparableFilter v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glGetnSeparableFilter #-}
ptr_glGetnSeparableFilter :: FunPtr (GLenum -> GLenum -> GLenum -> GLsizei -> Ptr a -> GLsizei -> Ptr b -> Ptr c -> IO ())
ptr_glGetnSeparableFilter = unsafePerformIO $ getCommand "glGetnSeparableFilter"

-- glGetnSeparableFilterARB ----------------------------------------------------

glGetnSeparableFilterARB
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @format@.
  -> GLenum -- ^ @type@.
  -> GLsizei -- ^ @rowBufSize@.
  -> Ptr a -- ^ @row@ pointing to @rowBufSize@ elements of type @a@.
  -> GLsizei -- ^ @columnBufSize@.
  -> Ptr b -- ^ @column@ pointing to @columnBufSize@ elements of type @b@.
  -> Ptr c -- ^ @span@ pointing to @0@ elements of type @c@.
  -> m ()
glGetnSeparableFilterARB v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn454 ptr_glGetnSeparableFilterARB v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glGetnSeparableFilterARB #-}
ptr_glGetnSeparableFilterARB :: FunPtr (GLenum -> GLenum -> GLenum -> GLsizei -> Ptr a -> GLsizei -> Ptr b -> Ptr c -> IO ())
ptr_glGetnSeparableFilterARB = unsafePerformIO $ getCommand "glGetnSeparableFilterARB"

-- glGetnTexImage --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetTexImage.xhtml OpenGL 4.x>.
glGetnTexImage
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLint -- ^ @level@.
  -> GLenum -- ^ @format@.
  -> GLenum -- ^ @type@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr a -- ^ @pixels@.
  -> m ()
glGetnTexImage v1 v2 v3 v4 v5 v6 = liftIO $ dyn455 ptr_glGetnTexImage v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glGetnTexImage #-}
ptr_glGetnTexImage :: FunPtr (GLenum -> GLint -> GLenum -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glGetnTexImage = unsafePerformIO $ getCommand "glGetnTexImage"

-- glGetnTexImageARB -----------------------------------------------------------

glGetnTexImageARB
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLint -- ^ @level@.
  -> GLenum -- ^ @format@.
  -> GLenum -- ^ @type@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr a -- ^ @img@ pointing to @bufSize@ elements of type @a@.
  -> m ()
glGetnTexImageARB v1 v2 v3 v4 v5 v6 = liftIO $ dyn455 ptr_glGetnTexImageARB v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glGetnTexImageARB #-}
ptr_glGetnTexImageARB :: FunPtr (GLenum -> GLint -> GLenum -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glGetnTexImageARB = unsafePerformIO $ getCommand "glGetnTexImageARB"

