--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F19
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

module Graphics.GL.Functions.F19 (
  glFragmentMaterialiSGIX,
  glFragmentMaterialivSGIX,
  glFrameTerminatorGREMEDY,
  glFrameZoomSGIX,
  glFramebufferDrawBufferEXT,
  glFramebufferDrawBuffersEXT,
  glFramebufferParameteri,
  glFramebufferPixelLocalStorageSizeEXT,
  glFramebufferReadBufferEXT,
  glFramebufferRenderbuffer,
  glFramebufferRenderbufferEXT,
  glFramebufferRenderbufferOES,
  glFramebufferSampleLocationsfvARB,
  glFramebufferSampleLocationsfvNV,
  glFramebufferTexture,
  glFramebufferTexture1D,
  glFramebufferTexture1DEXT,
  glFramebufferTexture2D,
  glFramebufferTexture2DDownsampleIMG,
  glFramebufferTexture2DEXT,
  glFramebufferTexture2DMultisampleEXT,
  glFramebufferTexture2DMultisampleIMG,
  glFramebufferTexture2DOES,
  glFramebufferTexture3D,
  glFramebufferTexture3DEXT,
  glFramebufferTexture3DOES,
  glFramebufferTextureARB,
  glFramebufferTextureEXT,
  glFramebufferTextureFaceARB,
  glFramebufferTextureFaceEXT,
  glFramebufferTextureLayer,
  glFramebufferTextureLayerARB,
  glFramebufferTextureLayerDownsampleIMG,
  glFramebufferTextureLayerEXT,
  glFramebufferTextureMultisampleMultiviewOVR,
  glFramebufferTextureMultiviewOVR,
  glFramebufferTextureOES,
  glFreeObjectBufferATI,
  glFrontFace,
  glFrustum
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

-- glFragmentMaterialiSGIX -----------------------------------------------------

glFragmentMaterialiSGIX
  :: MonadIO m
  => GLenum -- ^ @face@ of type [MaterialFace](Graphics-GL-Groups.html#MaterialFace).
  -> GLenum -- ^ @pname@ of type [MaterialParameter](Graphics-GL-Groups.html#MaterialParameter).
  -> GLint -- ^ @param@ of type @CheckedInt32@.
  -> m ()
glFragmentMaterialiSGIX v1 v2 v3 = liftIO $ dyn62 ptr_glFragmentMaterialiSGIX v1 v2 v3

{-# NOINLINE ptr_glFragmentMaterialiSGIX #-}
ptr_glFragmentMaterialiSGIX :: FunPtr (GLenum -> GLenum -> GLint -> IO ())
ptr_glFragmentMaterialiSGIX = unsafePerformIO $ getCommand "glFragmentMaterialiSGIX"

-- glFragmentMaterialivSGIX ----------------------------------------------------

glFragmentMaterialivSGIX
  :: MonadIO m
  => GLenum -- ^ @face@ of type [MaterialFace](Graphics-GL-Groups.html#MaterialFace).
  -> GLenum -- ^ @pname@ of type [MaterialParameter](Graphics-GL-Groups.html#MaterialParameter).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedInt32@.
  -> m ()
glFragmentMaterialivSGIX v1 v2 v3 = liftIO $ dyn133 ptr_glFragmentMaterialivSGIX v1 v2 v3

{-# NOINLINE ptr_glFragmentMaterialivSGIX #-}
ptr_glFragmentMaterialivSGIX :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glFragmentMaterialivSGIX = unsafePerformIO $ getCommand "glFragmentMaterialivSGIX"

-- glFrameTerminatorGREMEDY ----------------------------------------------------

glFrameTerminatorGREMEDY
  :: MonadIO m
  => m ()
glFrameTerminatorGREMEDY = liftIO $ dyn10 ptr_glFrameTerminatorGREMEDY

{-# NOINLINE ptr_glFrameTerminatorGREMEDY #-}
ptr_glFrameTerminatorGREMEDY :: FunPtr (IO ())
ptr_glFrameTerminatorGREMEDY = unsafePerformIO $ getCommand "glFrameTerminatorGREMEDY"

-- glFrameZoomSGIX -------------------------------------------------------------

glFrameZoomSGIX
  :: MonadIO m
  => GLint -- ^ @factor@ of type @CheckedInt32@.
  -> m ()
glFrameZoomSGIX v1 = liftIO $ dyn12 ptr_glFrameZoomSGIX v1

{-# NOINLINE ptr_glFrameZoomSGIX #-}
ptr_glFrameZoomSGIX :: FunPtr (GLint -> IO ())
ptr_glFrameZoomSGIX = unsafePerformIO $ getCommand "glFrameZoomSGIX"

-- glFramebufferDrawBufferEXT --------------------------------------------------

glFramebufferDrawBufferEXT
  :: MonadIO m
  => GLuint -- ^ @framebuffer@ of type @Framebuffer@.
  -> GLenum -- ^ @mode@ of type [DrawBufferMode](Graphics-GL-Groups.html#DrawBufferMode).
  -> m ()
glFramebufferDrawBufferEXT v1 v2 = liftIO $ dyn15 ptr_glFramebufferDrawBufferEXT v1 v2

{-# NOINLINE ptr_glFramebufferDrawBufferEXT #-}
ptr_glFramebufferDrawBufferEXT :: FunPtr (GLuint -> GLenum -> IO ())
ptr_glFramebufferDrawBufferEXT = unsafePerformIO $ getCommand "glFramebufferDrawBufferEXT"

-- glFramebufferDrawBuffersEXT -------------------------------------------------

glFramebufferDrawBuffersEXT
  :: MonadIO m
  => GLuint -- ^ @framebuffer@ of type @Framebuffer@.
  -> GLsizei -- ^ @n@.
  -> Ptr GLenum -- ^ @bufs@ pointing to @n@ elements of type [DrawBufferMode](Graphics-GL-Groups.html#DrawBufferMode).
  -> m ()
glFramebufferDrawBuffersEXT v1 v2 v3 = liftIO $ dyn282 ptr_glFramebufferDrawBuffersEXT v1 v2 v3

{-# NOINLINE ptr_glFramebufferDrawBuffersEXT #-}
ptr_glFramebufferDrawBuffersEXT :: FunPtr (GLuint -> GLsizei -> Ptr GLenum -> IO ())
ptr_glFramebufferDrawBuffersEXT = unsafePerformIO $ getCommand "glFramebufferDrawBuffersEXT"

-- glFramebufferParameteri -----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glFramebufferParameteri.xhtml OpenGL 4.x>.
glFramebufferParameteri
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @pname@.
  -> GLint -- ^ @param@.
  -> m ()
glFramebufferParameteri v1 v2 v3 = liftIO $ dyn62 ptr_glFramebufferParameteri v1 v2 v3

{-# NOINLINE ptr_glFramebufferParameteri #-}
ptr_glFramebufferParameteri :: FunPtr (GLenum -> GLenum -> GLint -> IO ())
ptr_glFramebufferParameteri = unsafePerformIO $ getCommand "glFramebufferParameteri"

-- glFramebufferPixelLocalStorageSizeEXT ---------------------------------------

glFramebufferPixelLocalStorageSizeEXT
  :: MonadIO m
  => GLuint -- ^ @target@.
  -> GLsizei -- ^ @size@.
  -> m ()
glFramebufferPixelLocalStorageSizeEXT v1 v2 = liftIO $ dyn212 ptr_glFramebufferPixelLocalStorageSizeEXT v1 v2

{-# NOINLINE ptr_glFramebufferPixelLocalStorageSizeEXT #-}
ptr_glFramebufferPixelLocalStorageSizeEXT :: FunPtr (GLuint -> GLsizei -> IO ())
ptr_glFramebufferPixelLocalStorageSizeEXT = unsafePerformIO $ getCommand "glFramebufferPixelLocalStorageSizeEXT"

-- glFramebufferReadBufferEXT --------------------------------------------------

glFramebufferReadBufferEXT
  :: MonadIO m
  => GLuint -- ^ @framebuffer@ of type @Framebuffer@.
  -> GLenum -- ^ @mode@ of type [ReadBufferMode](Graphics-GL-Groups.html#ReadBufferMode).
  -> m ()
glFramebufferReadBufferEXT v1 v2 = liftIO $ dyn15 ptr_glFramebufferReadBufferEXT v1 v2

{-# NOINLINE ptr_glFramebufferReadBufferEXT #-}
ptr_glFramebufferReadBufferEXT :: FunPtr (GLuint -> GLenum -> IO ())
ptr_glFramebufferReadBufferEXT = unsafePerformIO $ getCommand "glFramebufferReadBufferEXT"

-- glFramebufferRenderbuffer ---------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glFramebufferRenderbuffer.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glFramebufferRenderbuffer.xhtml OpenGL 4.x>.
glFramebufferRenderbuffer
  :: MonadIO m
  => GLenum -- ^ @target@ of type @FramebufferTarget@.
  -> GLenum -- ^ @attachment@ of type @FramebufferAttachment@.
  -> GLenum -- ^ @renderbuffertarget@ of type @RenderbufferTarget@.
  -> GLuint -- ^ @renderbuffer@.
  -> m ()
glFramebufferRenderbuffer v1 v2 v3 v4 = liftIO $ dyn283 ptr_glFramebufferRenderbuffer v1 v2 v3 v4

{-# NOINLINE ptr_glFramebufferRenderbuffer #-}
ptr_glFramebufferRenderbuffer :: FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> IO ())
ptr_glFramebufferRenderbuffer = unsafePerformIO $ getCommand "glFramebufferRenderbuffer"

-- glFramebufferRenderbufferEXT ------------------------------------------------

-- | This command is an alias for 'glFramebufferRenderbuffer'.
glFramebufferRenderbufferEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type @FramebufferTarget@.
  -> GLenum -- ^ @attachment@ of type @FramebufferAttachment@.
  -> GLenum -- ^ @renderbuffertarget@ of type @RenderbufferTarget@.
  -> GLuint -- ^ @renderbuffer@.
  -> m ()
glFramebufferRenderbufferEXT v1 v2 v3 v4 = liftIO $ dyn283 ptr_glFramebufferRenderbufferEXT v1 v2 v3 v4

{-# NOINLINE ptr_glFramebufferRenderbufferEXT #-}
ptr_glFramebufferRenderbufferEXT :: FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> IO ())
ptr_glFramebufferRenderbufferEXT = unsafePerformIO $ getCommand "glFramebufferRenderbufferEXT"

-- glFramebufferRenderbufferOES ------------------------------------------------

glFramebufferRenderbufferOES
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @attachment@.
  -> GLenum -- ^ @renderbuffertarget@.
  -> GLuint -- ^ @renderbuffer@.
  -> m ()
glFramebufferRenderbufferOES v1 v2 v3 v4 = liftIO $ dyn283 ptr_glFramebufferRenderbufferOES v1 v2 v3 v4

{-# NOINLINE ptr_glFramebufferRenderbufferOES #-}
ptr_glFramebufferRenderbufferOES :: FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> IO ())
ptr_glFramebufferRenderbufferOES = unsafePerformIO $ getCommand "glFramebufferRenderbufferOES"

-- glFramebufferSampleLocationsfvARB -------------------------------------------

glFramebufferSampleLocationsfvARB
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLuint -- ^ @start@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @v@.
  -> m ()
glFramebufferSampleLocationsfvARB v1 v2 v3 v4 = liftIO $ dyn284 ptr_glFramebufferSampleLocationsfvARB v1 v2 v3 v4

{-# NOINLINE ptr_glFramebufferSampleLocationsfvARB #-}
ptr_glFramebufferSampleLocationsfvARB :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glFramebufferSampleLocationsfvARB = unsafePerformIO $ getCommand "glFramebufferSampleLocationsfvARB"

-- glFramebufferSampleLocationsfvNV --------------------------------------------

glFramebufferSampleLocationsfvNV
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLuint -- ^ @start@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @v@.
  -> m ()
glFramebufferSampleLocationsfvNV v1 v2 v3 v4 = liftIO $ dyn284 ptr_glFramebufferSampleLocationsfvNV v1 v2 v3 v4

{-# NOINLINE ptr_glFramebufferSampleLocationsfvNV #-}
ptr_glFramebufferSampleLocationsfvNV :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glFramebufferSampleLocationsfvNV = unsafePerformIO $ getCommand "glFramebufferSampleLocationsfvNV"

-- glFramebufferTexture --------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glFramebufferTexture.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glFramebufferTexture.xhtml OpenGL 4.x>.
glFramebufferTexture
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @attachment@.
  -> GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> m ()
glFramebufferTexture v1 v2 v3 v4 = liftIO $ dyn285 ptr_glFramebufferTexture v1 v2 v3 v4

{-# NOINLINE ptr_glFramebufferTexture #-}
ptr_glFramebufferTexture :: FunPtr (GLenum -> GLenum -> GLuint -> GLint -> IO ())
ptr_glFramebufferTexture = unsafePerformIO $ getCommand "glFramebufferTexture"

-- glFramebufferTexture1D ------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glFramebufferTexture.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glFramebufferTexture.xhtml OpenGL 4.x>.
glFramebufferTexture1D
  :: MonadIO m
  => GLenum -- ^ @target@ of type @FramebufferTarget@.
  -> GLenum -- ^ @attachment@ of type @FramebufferAttachment@.
  -> GLenum -- ^ @textarget@.
  -> GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> m ()
glFramebufferTexture1D v1 v2 v3 v4 v5 = liftIO $ dyn286 ptr_glFramebufferTexture1D v1 v2 v3 v4 v5

{-# NOINLINE ptr_glFramebufferTexture1D #-}
ptr_glFramebufferTexture1D :: FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> GLint -> IO ())
ptr_glFramebufferTexture1D = unsafePerformIO $ getCommand "glFramebufferTexture1D"

-- glFramebufferTexture1DEXT ---------------------------------------------------

-- | This command is an alias for 'glFramebufferTexture1D'.
glFramebufferTexture1DEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type @FramebufferTarget@.
  -> GLenum -- ^ @attachment@ of type @FramebufferAttachment@.
  -> GLenum -- ^ @textarget@.
  -> GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> m ()
glFramebufferTexture1DEXT v1 v2 v3 v4 v5 = liftIO $ dyn286 ptr_glFramebufferTexture1DEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glFramebufferTexture1DEXT #-}
ptr_glFramebufferTexture1DEXT :: FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> GLint -> IO ())
ptr_glFramebufferTexture1DEXT = unsafePerformIO $ getCommand "glFramebufferTexture1DEXT"

-- glFramebufferTexture2D ------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glFramebufferTexture.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glFramebufferTexture.xhtml OpenGL 4.x>.
glFramebufferTexture2D
  :: MonadIO m
  => GLenum -- ^ @target@ of type @FramebufferTarget@.
  -> GLenum -- ^ @attachment@ of type @FramebufferAttachment@.
  -> GLenum -- ^ @textarget@.
  -> GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> m ()
glFramebufferTexture2D v1 v2 v3 v4 v5 = liftIO $ dyn286 ptr_glFramebufferTexture2D v1 v2 v3 v4 v5

{-# NOINLINE ptr_glFramebufferTexture2D #-}
ptr_glFramebufferTexture2D :: FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> GLint -> IO ())
ptr_glFramebufferTexture2D = unsafePerformIO $ getCommand "glFramebufferTexture2D"

-- glFramebufferTexture2DDownsampleIMG -----------------------------------------

glFramebufferTexture2DDownsampleIMG
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @attachment@.
  -> GLenum -- ^ @textarget@.
  -> GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLint -- ^ @xscale@.
  -> GLint -- ^ @yscale@.
  -> m ()
glFramebufferTexture2DDownsampleIMG v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn287 ptr_glFramebufferTexture2DDownsampleIMG v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glFramebufferTexture2DDownsampleIMG #-}
ptr_glFramebufferTexture2DDownsampleIMG :: FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> GLint -> GLint -> GLint -> IO ())
ptr_glFramebufferTexture2DDownsampleIMG = unsafePerformIO $ getCommand "glFramebufferTexture2DDownsampleIMG"

-- glFramebufferTexture2DEXT ---------------------------------------------------

-- | This command is an alias for 'glFramebufferTexture2D'.
glFramebufferTexture2DEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type @FramebufferTarget@.
  -> GLenum -- ^ @attachment@ of type @FramebufferAttachment@.
  -> GLenum -- ^ @textarget@.
  -> GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> m ()
glFramebufferTexture2DEXT v1 v2 v3 v4 v5 = liftIO $ dyn286 ptr_glFramebufferTexture2DEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glFramebufferTexture2DEXT #-}
ptr_glFramebufferTexture2DEXT :: FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> GLint -> IO ())
ptr_glFramebufferTexture2DEXT = unsafePerformIO $ getCommand "glFramebufferTexture2DEXT"

-- glFramebufferTexture2DMultisampleEXT ----------------------------------------

glFramebufferTexture2DMultisampleEXT
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @attachment@.
  -> GLenum -- ^ @textarget@.
  -> GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLsizei -- ^ @samples@.
  -> m ()
glFramebufferTexture2DMultisampleEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn288 ptr_glFramebufferTexture2DMultisampleEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glFramebufferTexture2DMultisampleEXT #-}
ptr_glFramebufferTexture2DMultisampleEXT :: FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> GLint -> GLsizei -> IO ())
ptr_glFramebufferTexture2DMultisampleEXT = unsafePerformIO $ getCommand "glFramebufferTexture2DMultisampleEXT"

-- glFramebufferTexture2DMultisampleIMG ----------------------------------------

glFramebufferTexture2DMultisampleIMG
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @attachment@.
  -> GLenum -- ^ @textarget@.
  -> GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLsizei -- ^ @samples@.
  -> m ()
glFramebufferTexture2DMultisampleIMG v1 v2 v3 v4 v5 v6 = liftIO $ dyn288 ptr_glFramebufferTexture2DMultisampleIMG v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glFramebufferTexture2DMultisampleIMG #-}
ptr_glFramebufferTexture2DMultisampleIMG :: FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> GLint -> GLsizei -> IO ())
ptr_glFramebufferTexture2DMultisampleIMG = unsafePerformIO $ getCommand "glFramebufferTexture2DMultisampleIMG"

-- glFramebufferTexture2DOES ---------------------------------------------------

glFramebufferTexture2DOES
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @attachment@.
  -> GLenum -- ^ @textarget@.
  -> GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> m ()
glFramebufferTexture2DOES v1 v2 v3 v4 v5 = liftIO $ dyn286 ptr_glFramebufferTexture2DOES v1 v2 v3 v4 v5

{-# NOINLINE ptr_glFramebufferTexture2DOES #-}
ptr_glFramebufferTexture2DOES :: FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> GLint -> IO ())
ptr_glFramebufferTexture2DOES = unsafePerformIO $ getCommand "glFramebufferTexture2DOES"

-- glFramebufferTexture3D ------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glFramebufferTexture.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glFramebufferTexture.xhtml OpenGL 4.x>.
glFramebufferTexture3D
  :: MonadIO m
  => GLenum -- ^ @target@ of type @FramebufferTarget@.
  -> GLenum -- ^ @attachment@ of type @FramebufferAttachment@.
  -> GLenum -- ^ @textarget@.
  -> GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLint -- ^ @zoffset@.
  -> m ()
glFramebufferTexture3D v1 v2 v3 v4 v5 v6 = liftIO $ dyn289 ptr_glFramebufferTexture3D v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glFramebufferTexture3D #-}
ptr_glFramebufferTexture3D :: FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> GLint -> GLint -> IO ())
ptr_glFramebufferTexture3D = unsafePerformIO $ getCommand "glFramebufferTexture3D"

-- glFramebufferTexture3DEXT ---------------------------------------------------

-- | This command is an alias for 'glFramebufferTexture3D'.
glFramebufferTexture3DEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type @FramebufferTarget@.
  -> GLenum -- ^ @attachment@ of type @FramebufferAttachment@.
  -> GLenum -- ^ @textarget@.
  -> GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLint -- ^ @zoffset@.
  -> m ()
glFramebufferTexture3DEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn289 ptr_glFramebufferTexture3DEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glFramebufferTexture3DEXT #-}
ptr_glFramebufferTexture3DEXT :: FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> GLint -> GLint -> IO ())
ptr_glFramebufferTexture3DEXT = unsafePerformIO $ getCommand "glFramebufferTexture3DEXT"

-- glFramebufferTexture3DOES ---------------------------------------------------

-- | This command is an alias for 'glFramebufferTexture3D'.
glFramebufferTexture3DOES
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @attachment@.
  -> GLenum -- ^ @textarget@.
  -> GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLint -- ^ @zoffset@.
  -> m ()
glFramebufferTexture3DOES v1 v2 v3 v4 v5 v6 = liftIO $ dyn289 ptr_glFramebufferTexture3DOES v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glFramebufferTexture3DOES #-}
ptr_glFramebufferTexture3DOES :: FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> GLint -> GLint -> IO ())
ptr_glFramebufferTexture3DOES = unsafePerformIO $ getCommand "glFramebufferTexture3DOES"

-- glFramebufferTextureARB -----------------------------------------------------

-- | This command is an alias for 'glFramebufferTexture'.
glFramebufferTextureARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @FramebufferTarget@.
  -> GLenum -- ^ @attachment@ of type @FramebufferAttachment@.
  -> GLuint -- ^ @texture@ of type @Texture@.
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> m ()
glFramebufferTextureARB v1 v2 v3 v4 = liftIO $ dyn285 ptr_glFramebufferTextureARB v1 v2 v3 v4

{-# NOINLINE ptr_glFramebufferTextureARB #-}
ptr_glFramebufferTextureARB :: FunPtr (GLenum -> GLenum -> GLuint -> GLint -> IO ())
ptr_glFramebufferTextureARB = unsafePerformIO $ getCommand "glFramebufferTextureARB"

-- glFramebufferTextureEXT -----------------------------------------------------

-- | This command is an alias for 'glFramebufferTexture'.
glFramebufferTextureEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type @FramebufferTarget@.
  -> GLenum -- ^ @attachment@ of type @FramebufferAttachment@.
  -> GLuint -- ^ @texture@ of type @Texture@.
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> m ()
glFramebufferTextureEXT v1 v2 v3 v4 = liftIO $ dyn285 ptr_glFramebufferTextureEXT v1 v2 v3 v4

{-# NOINLINE ptr_glFramebufferTextureEXT #-}
ptr_glFramebufferTextureEXT :: FunPtr (GLenum -> GLenum -> GLuint -> GLint -> IO ())
ptr_glFramebufferTextureEXT = unsafePerformIO $ getCommand "glFramebufferTextureEXT"

-- glFramebufferTextureFaceARB -------------------------------------------------

glFramebufferTextureFaceARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @FramebufferTarget@.
  -> GLenum -- ^ @attachment@ of type @FramebufferAttachment@.
  -> GLuint -- ^ @texture@ of type @Texture@.
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @face@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> m ()
glFramebufferTextureFaceARB v1 v2 v3 v4 v5 = liftIO $ dyn290 ptr_glFramebufferTextureFaceARB v1 v2 v3 v4 v5

{-# NOINLINE ptr_glFramebufferTextureFaceARB #-}
ptr_glFramebufferTextureFaceARB :: FunPtr (GLenum -> GLenum -> GLuint -> GLint -> GLenum -> IO ())
ptr_glFramebufferTextureFaceARB = unsafePerformIO $ getCommand "glFramebufferTextureFaceARB"

-- glFramebufferTextureFaceEXT -------------------------------------------------

-- | This command is an alias for 'glFramebufferTextureFaceARB'.
glFramebufferTextureFaceEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type @FramebufferTarget@.
  -> GLenum -- ^ @attachment@ of type @FramebufferAttachment@.
  -> GLuint -- ^ @texture@ of type @Texture@.
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @face@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> m ()
glFramebufferTextureFaceEXT v1 v2 v3 v4 v5 = liftIO $ dyn290 ptr_glFramebufferTextureFaceEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glFramebufferTextureFaceEXT #-}
ptr_glFramebufferTextureFaceEXT :: FunPtr (GLenum -> GLenum -> GLuint -> GLint -> GLenum -> IO ())
ptr_glFramebufferTextureFaceEXT = unsafePerformIO $ getCommand "glFramebufferTextureFaceEXT"

-- glFramebufferTextureLayer ---------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glFramebufferTextureLayer.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glFramebufferTextureLayer.xhtml OpenGL 4.x>.
glFramebufferTextureLayer
  :: MonadIO m
  => GLenum -- ^ @target@ of type @FramebufferTarget@.
  -> GLenum -- ^ @attachment@ of type @FramebufferAttachment@.
  -> GLuint -- ^ @texture@ of type @Texture@.
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @layer@ of type @CheckedInt32@.
  -> m ()
glFramebufferTextureLayer v1 v2 v3 v4 v5 = liftIO $ dyn291 ptr_glFramebufferTextureLayer v1 v2 v3 v4 v5

{-# NOINLINE ptr_glFramebufferTextureLayer #-}
ptr_glFramebufferTextureLayer :: FunPtr (GLenum -> GLenum -> GLuint -> GLint -> GLint -> IO ())
ptr_glFramebufferTextureLayer = unsafePerformIO $ getCommand "glFramebufferTextureLayer"

-- glFramebufferTextureLayerARB ------------------------------------------------

-- | This command is an alias for 'glFramebufferTextureLayer'.
glFramebufferTextureLayerARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @FramebufferTarget@.
  -> GLenum -- ^ @attachment@ of type @FramebufferAttachment@.
  -> GLuint -- ^ @texture@ of type @Texture@.
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @layer@ of type @CheckedInt32@.
  -> m ()
glFramebufferTextureLayerARB v1 v2 v3 v4 v5 = liftIO $ dyn291 ptr_glFramebufferTextureLayerARB v1 v2 v3 v4 v5

{-# NOINLINE ptr_glFramebufferTextureLayerARB #-}
ptr_glFramebufferTextureLayerARB :: FunPtr (GLenum -> GLenum -> GLuint -> GLint -> GLint -> IO ())
ptr_glFramebufferTextureLayerARB = unsafePerformIO $ getCommand "glFramebufferTextureLayerARB"

-- glFramebufferTextureLayerDownsampleIMG --------------------------------------

glFramebufferTextureLayerDownsampleIMG
  :: MonadIO m
  => GLenum -- ^ @target@ of type @FramebufferTarget@.
  -> GLenum -- ^ @attachment@ of type @FramebufferAttachment@.
  -> GLuint -- ^ @texture@ of type @Texture@.
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @layer@ of type @CheckedInt32@.
  -> GLint -- ^ @xscale@.
  -> GLint -- ^ @yscale@.
  -> m ()
glFramebufferTextureLayerDownsampleIMG v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn292 ptr_glFramebufferTextureLayerDownsampleIMG v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glFramebufferTextureLayerDownsampleIMG #-}
ptr_glFramebufferTextureLayerDownsampleIMG :: FunPtr (GLenum -> GLenum -> GLuint -> GLint -> GLint -> GLint -> GLint -> IO ())
ptr_glFramebufferTextureLayerDownsampleIMG = unsafePerformIO $ getCommand "glFramebufferTextureLayerDownsampleIMG"

-- glFramebufferTextureLayerEXT ------------------------------------------------

-- | This command is an alias for 'glFramebufferTextureLayer'.
glFramebufferTextureLayerEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type @FramebufferTarget@.
  -> GLenum -- ^ @attachment@ of type @FramebufferAttachment@.
  -> GLuint -- ^ @texture@ of type @Texture@.
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @layer@ of type @CheckedInt32@.
  -> m ()
glFramebufferTextureLayerEXT v1 v2 v3 v4 v5 = liftIO $ dyn291 ptr_glFramebufferTextureLayerEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glFramebufferTextureLayerEXT #-}
ptr_glFramebufferTextureLayerEXT :: FunPtr (GLenum -> GLenum -> GLuint -> GLint -> GLint -> IO ())
ptr_glFramebufferTextureLayerEXT = unsafePerformIO $ getCommand "glFramebufferTextureLayerEXT"

-- glFramebufferTextureMultisampleMultiviewOVR ---------------------------------

glFramebufferTextureMultisampleMultiviewOVR
  :: MonadIO m
  => GLenum -- ^ @target@ of type @FramebufferTarget@.
  -> GLenum -- ^ @attachment@ of type @FramebufferAttachment@.
  -> GLuint -- ^ @texture@ of type @Texture@.
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLsizei -- ^ @samples@.
  -> GLint -- ^ @baseViewIndex@.
  -> GLsizei -- ^ @numViews@.
  -> m ()
glFramebufferTextureMultisampleMultiviewOVR v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn293 ptr_glFramebufferTextureMultisampleMultiviewOVR v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glFramebufferTextureMultisampleMultiviewOVR #-}
ptr_glFramebufferTextureMultisampleMultiviewOVR :: FunPtr (GLenum -> GLenum -> GLuint -> GLint -> GLsizei -> GLint -> GLsizei -> IO ())
ptr_glFramebufferTextureMultisampleMultiviewOVR = unsafePerformIO $ getCommand "glFramebufferTextureMultisampleMultiviewOVR"

-- glFramebufferTextureMultiviewOVR --------------------------------------------

glFramebufferTextureMultiviewOVR
  :: MonadIO m
  => GLenum -- ^ @target@ of type @FramebufferTarget@.
  -> GLenum -- ^ @attachment@ of type @FramebufferAttachment@.
  -> GLuint -- ^ @texture@ of type @Texture@.
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @baseViewIndex@.
  -> GLsizei -- ^ @numViews@.
  -> m ()
glFramebufferTextureMultiviewOVR v1 v2 v3 v4 v5 v6 = liftIO $ dyn294 ptr_glFramebufferTextureMultiviewOVR v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glFramebufferTextureMultiviewOVR #-}
ptr_glFramebufferTextureMultiviewOVR :: FunPtr (GLenum -> GLenum -> GLuint -> GLint -> GLint -> GLsizei -> IO ())
ptr_glFramebufferTextureMultiviewOVR = unsafePerformIO $ getCommand "glFramebufferTextureMultiviewOVR"

-- glFramebufferTextureOES -----------------------------------------------------

-- | This command is an alias for 'glFramebufferTexture'.
glFramebufferTextureOES
  :: MonadIO m
  => GLenum -- ^ @target@ of type @FramebufferTarget@.
  -> GLenum -- ^ @attachment@ of type @FramebufferAttachment@.
  -> GLuint -- ^ @texture@ of type @Texture@.
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> m ()
glFramebufferTextureOES v1 v2 v3 v4 = liftIO $ dyn285 ptr_glFramebufferTextureOES v1 v2 v3 v4

{-# NOINLINE ptr_glFramebufferTextureOES #-}
ptr_glFramebufferTextureOES :: FunPtr (GLenum -> GLenum -> GLuint -> GLint -> IO ())
ptr_glFramebufferTextureOES = unsafePerformIO $ getCommand "glFramebufferTextureOES"

-- glFreeObjectBufferATI -------------------------------------------------------

glFreeObjectBufferATI
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> m ()
glFreeObjectBufferATI v1 = liftIO $ dyn2 ptr_glFreeObjectBufferATI v1

{-# NOINLINE ptr_glFreeObjectBufferATI #-}
ptr_glFreeObjectBufferATI :: FunPtr (GLuint -> IO ())
ptr_glFreeObjectBufferATI = unsafePerformIO $ getCommand "glFreeObjectBufferATI"

-- glFrontFace -----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glFrontFace.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glFrontFace.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glFrontFace.xhtml OpenGL 4.x>.
glFrontFace
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [FrontFaceDirection](Graphics-GL-Groups.html#FrontFaceDirection).
  -> m ()
glFrontFace v1 = liftIO $ dyn4 ptr_glFrontFace v1

{-# NOINLINE ptr_glFrontFace #-}
ptr_glFrontFace :: FunPtr (GLenum -> IO ())
ptr_glFrontFace = unsafePerformIO $ getCommand "glFrontFace"

-- glFrustum -------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glFrustum.xml OpenGL 2.x>.
glFrustum
  :: MonadIO m
  => GLdouble -- ^ @left@.
  -> GLdouble -- ^ @right@.
  -> GLdouble -- ^ @bottom@.
  -> GLdouble -- ^ @top@.
  -> GLdouble -- ^ @zNear@.
  -> GLdouble -- ^ @zFar@.
  -> m ()
glFrustum v1 v2 v3 v4 v5 v6 = liftIO $ dyn295 ptr_glFrustum v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glFrustum #-}
ptr_glFrustum :: FunPtr (GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glFrustum = unsafePerformIO $ getCommand "glFrustum"

