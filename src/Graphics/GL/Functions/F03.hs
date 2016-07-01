--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F03
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

module Graphics.GL.Functions.F03 (
  glBindSampler,
  glBindSamplers,
  glBindTexGenParameterEXT,
  glBindTexture,
  glBindTextureEXT,
  glBindTextureUnit,
  glBindTextureUnitParameterEXT,
  glBindTextures,
  glBindTransformFeedback,
  glBindTransformFeedbackNV,
  glBindVertexArray,
  glBindVertexArrayAPPLE,
  glBindVertexArrayOES,
  glBindVertexBuffer,
  glBindVertexBuffers,
  glBindVertexShaderEXT,
  glBindVideoCaptureStreamBufferNV,
  glBindVideoCaptureStreamTextureNV,
  glBinormal3bEXT,
  glBinormal3bvEXT,
  glBinormal3dEXT,
  glBinormal3dvEXT,
  glBinormal3fEXT,
  glBinormal3fvEXT,
  glBinormal3iEXT,
  glBinormal3ivEXT,
  glBinormal3sEXT,
  glBinormal3svEXT,
  glBinormalPointerEXT,
  glBitmap,
  glBitmapxOES,
  glBlendBarrier,
  glBlendBarrierKHR,
  glBlendBarrierNV,
  glBlendColor,
  glBlendColorEXT,
  glBlendColorxOES,
  glBlendEquation,
  glBlendEquationEXT,
  glBlendEquationIndexedAMD
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

-- glBindSampler ---------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glBindSampler.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glBindSampler.xhtml OpenGL 4.x>.
glBindSampler
  :: MonadIO m
  => GLuint -- ^ @unit@.
  -> GLuint -- ^ @sampler@.
  -> m ()
glBindSampler v1 v2 = liftIO $ dyn3 ptr_glBindSampler v1 v2

{-# NOINLINE ptr_glBindSampler #-}
ptr_glBindSampler :: FunPtr (GLuint -> GLuint -> IO ())
ptr_glBindSampler = unsafePerformIO $ getCommand "glBindSampler"

-- glBindSamplers --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glBindSamplers.xhtml OpenGL 4.x>.
glBindSamplers
  :: MonadIO m
  => GLuint -- ^ @first@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @samplers@ pointing to @count@ elements of type @GLuint@.
  -> m ()
glBindSamplers v1 v2 v3 = liftIO $ dyn27 ptr_glBindSamplers v1 v2 v3

{-# NOINLINE ptr_glBindSamplers #-}
ptr_glBindSamplers :: FunPtr (GLuint -> GLsizei -> Ptr GLuint -> IO ())
ptr_glBindSamplers = unsafePerformIO $ getCommand "glBindSamplers"

-- glBindTexGenParameterEXT ----------------------------------------------------

glBindTexGenParameterEXT
  :: MonadIO m
  => GLenum -- ^ @unit@ of type @TextureUnit@.
  -> GLenum -- ^ @coord@ of type [TextureCoordName](Graphics-GL-Groups.html#TextureCoordName).
  -> GLenum -- ^ @value@ of type [TextureGenParameter](Graphics-GL-Groups.html#TextureGenParameter).
  -> m GLuint
glBindTexGenParameterEXT v1 v2 v3 = liftIO $ dyn31 ptr_glBindTexGenParameterEXT v1 v2 v3

{-# NOINLINE ptr_glBindTexGenParameterEXT #-}
ptr_glBindTexGenParameterEXT :: FunPtr (GLenum -> GLenum -> GLenum -> IO GLuint)
ptr_glBindTexGenParameterEXT = unsafePerformIO $ getCommand "glBindTexGenParameterEXT"

-- glBindTexture ---------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glBindTexture.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glBindTexture.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glBindTexture.xhtml OpenGL 4.x>.
glBindTexture
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLuint -- ^ @texture@ of type @Texture@.
  -> m ()
glBindTexture v1 v2 = liftIO $ dyn16 ptr_glBindTexture v1 v2

{-# NOINLINE ptr_glBindTexture #-}
ptr_glBindTexture :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glBindTexture = unsafePerformIO $ getCommand "glBindTexture"

-- glBindTextureEXT ------------------------------------------------------------

-- | This command is an alias for 'glBindTexture'.
glBindTextureEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLuint -- ^ @texture@ of type @Texture@.
  -> m ()
glBindTextureEXT v1 v2 = liftIO $ dyn16 ptr_glBindTextureEXT v1 v2

{-# NOINLINE ptr_glBindTextureEXT #-}
ptr_glBindTextureEXT :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glBindTextureEXT = unsafePerformIO $ getCommand "glBindTextureEXT"

-- glBindTextureUnit -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glBindTextureUnit.xhtml OpenGL 4.x>.
glBindTextureUnit
  :: MonadIO m
  => GLuint -- ^ @unit@.
  -> GLuint -- ^ @texture@.
  -> m ()
glBindTextureUnit v1 v2 = liftIO $ dyn3 ptr_glBindTextureUnit v1 v2

{-# NOINLINE ptr_glBindTextureUnit #-}
ptr_glBindTextureUnit :: FunPtr (GLuint -> GLuint -> IO ())
ptr_glBindTextureUnit = unsafePerformIO $ getCommand "glBindTextureUnit"

-- glBindTextureUnitParameterEXT -----------------------------------------------

glBindTextureUnitParameterEXT
  :: MonadIO m
  => GLenum -- ^ @unit@ of type @TextureUnit@.
  -> GLenum -- ^ @value@ of type @VertexShaderTextureUnitParameter@.
  -> m GLuint
glBindTextureUnitParameterEXT v1 v2 = liftIO $ dyn28 ptr_glBindTextureUnitParameterEXT v1 v2

{-# NOINLINE ptr_glBindTextureUnitParameterEXT #-}
ptr_glBindTextureUnitParameterEXT :: FunPtr (GLenum -> GLenum -> IO GLuint)
ptr_glBindTextureUnitParameterEXT = unsafePerformIO $ getCommand "glBindTextureUnitParameterEXT"

-- glBindTextures --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glBindTextures.xhtml OpenGL 4.x>.
glBindTextures
  :: MonadIO m
  => GLuint -- ^ @first@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @textures@ pointing to @count@ elements of type @GLuint@.
  -> m ()
glBindTextures v1 v2 v3 = liftIO $ dyn27 ptr_glBindTextures v1 v2 v3

{-# NOINLINE ptr_glBindTextures #-}
ptr_glBindTextures :: FunPtr (GLuint -> GLsizei -> Ptr GLuint -> IO ())
ptr_glBindTextures = unsafePerformIO $ getCommand "glBindTextures"

-- glBindTransformFeedback -----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glBindTransformFeedback.xhtml OpenGL 4.x>.
glBindTransformFeedback
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLuint -- ^ @id@.
  -> m ()
glBindTransformFeedback v1 v2 = liftIO $ dyn16 ptr_glBindTransformFeedback v1 v2

{-# NOINLINE ptr_glBindTransformFeedback #-}
ptr_glBindTransformFeedback :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glBindTransformFeedback = unsafePerformIO $ getCommand "glBindTransformFeedback"

-- glBindTransformFeedbackNV ---------------------------------------------------

glBindTransformFeedbackNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type @BufferTargetARB@.
  -> GLuint -- ^ @id@.
  -> m ()
glBindTransformFeedbackNV v1 v2 = liftIO $ dyn16 ptr_glBindTransformFeedbackNV v1 v2

{-# NOINLINE ptr_glBindTransformFeedbackNV #-}
ptr_glBindTransformFeedbackNV :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glBindTransformFeedbackNV = unsafePerformIO $ getCommand "glBindTransformFeedbackNV"

-- glBindVertexArray -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glBindVertexArray.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glBindVertexArray.xhtml OpenGL 4.x>.
glBindVertexArray
  :: MonadIO m
  => GLuint -- ^ @array@.
  -> m ()
glBindVertexArray v1 = liftIO $ dyn2 ptr_glBindVertexArray v1

{-# NOINLINE ptr_glBindVertexArray #-}
ptr_glBindVertexArray :: FunPtr (GLuint -> IO ())
ptr_glBindVertexArray = unsafePerformIO $ getCommand "glBindVertexArray"

-- glBindVertexArrayAPPLE ------------------------------------------------------

glBindVertexArrayAPPLE
  :: MonadIO m
  => GLuint -- ^ @array@.
  -> m ()
glBindVertexArrayAPPLE v1 = liftIO $ dyn2 ptr_glBindVertexArrayAPPLE v1

{-# NOINLINE ptr_glBindVertexArrayAPPLE #-}
ptr_glBindVertexArrayAPPLE :: FunPtr (GLuint -> IO ())
ptr_glBindVertexArrayAPPLE = unsafePerformIO $ getCommand "glBindVertexArrayAPPLE"

-- glBindVertexArrayOES --------------------------------------------------------

-- | This command is an alias for 'glBindVertexArray'.
glBindVertexArrayOES
  :: MonadIO m
  => GLuint -- ^ @array@.
  -> m ()
glBindVertexArrayOES v1 = liftIO $ dyn2 ptr_glBindVertexArrayOES v1

{-# NOINLINE ptr_glBindVertexArrayOES #-}
ptr_glBindVertexArrayOES :: FunPtr (GLuint -> IO ())
ptr_glBindVertexArrayOES = unsafePerformIO $ getCommand "glBindVertexArrayOES"

-- glBindVertexBuffer ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glBindVertexBuffer.xhtml OpenGL 4.x>.
glBindVertexBuffer
  :: MonadIO m
  => GLuint -- ^ @bindingindex@.
  -> GLuint -- ^ @buffer@.
  -> GLintptr -- ^ @offset@ of type @BufferOffset@.
  -> GLsizei -- ^ @stride@.
  -> m ()
glBindVertexBuffer v1 v2 v3 v4 = liftIO $ dyn32 ptr_glBindVertexBuffer v1 v2 v3 v4

{-# NOINLINE ptr_glBindVertexBuffer #-}
ptr_glBindVertexBuffer :: FunPtr (GLuint -> GLuint -> GLintptr -> GLsizei -> IO ())
ptr_glBindVertexBuffer = unsafePerformIO $ getCommand "glBindVertexBuffer"

-- glBindVertexBuffers ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glBindVertexBuffers.xhtml OpenGL 4.x>.
glBindVertexBuffers
  :: MonadIO m
  => GLuint -- ^ @first@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @buffers@ pointing to @count@ elements of type @GLuint@.
  -> Ptr GLintptr -- ^ @offsets@ pointing to @count@ elements of type @GLintptr@.
  -> Ptr GLsizei -- ^ @strides@ pointing to @count@ elements of type @GLsizei@.
  -> m ()
glBindVertexBuffers v1 v2 v3 v4 v5 = liftIO $ dyn33 ptr_glBindVertexBuffers v1 v2 v3 v4 v5

{-# NOINLINE ptr_glBindVertexBuffers #-}
ptr_glBindVertexBuffers :: FunPtr (GLuint -> GLsizei -> Ptr GLuint -> Ptr GLintptr -> Ptr GLsizei -> IO ())
ptr_glBindVertexBuffers = unsafePerformIO $ getCommand "glBindVertexBuffers"

-- glBindVertexShaderEXT -------------------------------------------------------

glBindVertexShaderEXT
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> m ()
glBindVertexShaderEXT v1 = liftIO $ dyn2 ptr_glBindVertexShaderEXT v1

{-# NOINLINE ptr_glBindVertexShaderEXT #-}
ptr_glBindVertexShaderEXT :: FunPtr (GLuint -> IO ())
ptr_glBindVertexShaderEXT = unsafePerformIO $ getCommand "glBindVertexShaderEXT"

-- glBindVideoCaptureStreamBufferNV --------------------------------------------

glBindVideoCaptureStreamBufferNV
  :: MonadIO m
  => GLuint -- ^ @video_capture_slot@.
  -> GLuint -- ^ @stream@.
  -> GLenum -- ^ @frame_region@.
  -> GLintptrARB -- ^ @offset@ of type @BufferOffsetARB@.
  -> m ()
glBindVideoCaptureStreamBufferNV v1 v2 v3 v4 = liftIO $ dyn34 ptr_glBindVideoCaptureStreamBufferNV v1 v2 v3 v4

{-# NOINLINE ptr_glBindVideoCaptureStreamBufferNV #-}
ptr_glBindVideoCaptureStreamBufferNV :: FunPtr (GLuint -> GLuint -> GLenum -> GLintptrARB -> IO ())
ptr_glBindVideoCaptureStreamBufferNV = unsafePerformIO $ getCommand "glBindVideoCaptureStreamBufferNV"

-- glBindVideoCaptureStreamTextureNV -------------------------------------------

glBindVideoCaptureStreamTextureNV
  :: MonadIO m
  => GLuint -- ^ @video_capture_slot@.
  -> GLuint -- ^ @stream@.
  -> GLenum -- ^ @frame_region@.
  -> GLenum -- ^ @target@.
  -> GLuint -- ^ @texture@.
  -> m ()
glBindVideoCaptureStreamTextureNV v1 v2 v3 v4 v5 = liftIO $ dyn35 ptr_glBindVideoCaptureStreamTextureNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glBindVideoCaptureStreamTextureNV #-}
ptr_glBindVideoCaptureStreamTextureNV :: FunPtr (GLuint -> GLuint -> GLenum -> GLenum -> GLuint -> IO ())
ptr_glBindVideoCaptureStreamTextureNV = unsafePerformIO $ getCommand "glBindVideoCaptureStreamTextureNV"

-- glBinormal3bEXT -------------------------------------------------------------

-- | The vector equivalent of this command is 'glBinormal3bvEXT'.
glBinormal3bEXT
  :: MonadIO m
  => GLbyte -- ^ @bx@.
  -> GLbyte -- ^ @by@.
  -> GLbyte -- ^ @bz@.
  -> m ()
glBinormal3bEXT v1 v2 v3 = liftIO $ dyn36 ptr_glBinormal3bEXT v1 v2 v3

{-# NOINLINE ptr_glBinormal3bEXT #-}
ptr_glBinormal3bEXT :: FunPtr (GLbyte -> GLbyte -> GLbyte -> IO ())
ptr_glBinormal3bEXT = unsafePerformIO $ getCommand "glBinormal3bEXT"

-- glBinormal3bvEXT ------------------------------------------------------------

glBinormal3bvEXT
  :: MonadIO m
  => Ptr GLbyte -- ^ @v@ pointing to @3@ elements of type @GLbyte@.
  -> m ()
glBinormal3bvEXT v1 = liftIO $ dyn37 ptr_glBinormal3bvEXT v1

{-# NOINLINE ptr_glBinormal3bvEXT #-}
ptr_glBinormal3bvEXT :: FunPtr (Ptr GLbyte -> IO ())
ptr_glBinormal3bvEXT = unsafePerformIO $ getCommand "glBinormal3bvEXT"

-- glBinormal3dEXT -------------------------------------------------------------

-- | The vector equivalent of this command is 'glBinormal3dvEXT'.
glBinormal3dEXT
  :: MonadIO m
  => GLdouble -- ^ @bx@ of type @CoordD@.
  -> GLdouble -- ^ @by@ of type @CoordD@.
  -> GLdouble -- ^ @bz@ of type @CoordD@.
  -> m ()
glBinormal3dEXT v1 v2 v3 = liftIO $ dyn38 ptr_glBinormal3dEXT v1 v2 v3

{-# NOINLINE ptr_glBinormal3dEXT #-}
ptr_glBinormal3dEXT :: FunPtr (GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glBinormal3dEXT = unsafePerformIO $ getCommand "glBinormal3dEXT"

-- glBinormal3dvEXT ------------------------------------------------------------

glBinormal3dvEXT
  :: MonadIO m
  => Ptr GLdouble -- ^ @v@ pointing to @3@ elements of type @CoordD@.
  -> m ()
glBinormal3dvEXT v1 = liftIO $ dyn39 ptr_glBinormal3dvEXT v1

{-# NOINLINE ptr_glBinormal3dvEXT #-}
ptr_glBinormal3dvEXT :: FunPtr (Ptr GLdouble -> IO ())
ptr_glBinormal3dvEXT = unsafePerformIO $ getCommand "glBinormal3dvEXT"

-- glBinormal3fEXT -------------------------------------------------------------

-- | The vector equivalent of this command is 'glBinormal3fvEXT'.
glBinormal3fEXT
  :: MonadIO m
  => GLfloat -- ^ @bx@ of type @CoordF@.
  -> GLfloat -- ^ @by@ of type @CoordF@.
  -> GLfloat -- ^ @bz@ of type @CoordF@.
  -> m ()
glBinormal3fEXT v1 v2 v3 = liftIO $ dyn40 ptr_glBinormal3fEXT v1 v2 v3

{-# NOINLINE ptr_glBinormal3fEXT #-}
ptr_glBinormal3fEXT :: FunPtr (GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glBinormal3fEXT = unsafePerformIO $ getCommand "glBinormal3fEXT"

-- glBinormal3fvEXT ------------------------------------------------------------

glBinormal3fvEXT
  :: MonadIO m
  => Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @CoordF@.
  -> m ()
glBinormal3fvEXT v1 = liftIO $ dyn41 ptr_glBinormal3fvEXT v1

{-# NOINLINE ptr_glBinormal3fvEXT #-}
ptr_glBinormal3fvEXT :: FunPtr (Ptr GLfloat -> IO ())
ptr_glBinormal3fvEXT = unsafePerformIO $ getCommand "glBinormal3fvEXT"

-- glBinormal3iEXT -------------------------------------------------------------

-- | The vector equivalent of this command is 'glBinormal3ivEXT'.
glBinormal3iEXT
  :: MonadIO m
  => GLint -- ^ @bx@.
  -> GLint -- ^ @by@.
  -> GLint -- ^ @bz@.
  -> m ()
glBinormal3iEXT v1 v2 v3 = liftIO $ dyn42 ptr_glBinormal3iEXT v1 v2 v3

{-# NOINLINE ptr_glBinormal3iEXT #-}
ptr_glBinormal3iEXT :: FunPtr (GLint -> GLint -> GLint -> IO ())
ptr_glBinormal3iEXT = unsafePerformIO $ getCommand "glBinormal3iEXT"

-- glBinormal3ivEXT ------------------------------------------------------------

glBinormal3ivEXT
  :: MonadIO m
  => Ptr GLint -- ^ @v@ pointing to @3@ elements of type @GLint@.
  -> m ()
glBinormal3ivEXT v1 = liftIO $ dyn43 ptr_glBinormal3ivEXT v1

{-# NOINLINE ptr_glBinormal3ivEXT #-}
ptr_glBinormal3ivEXT :: FunPtr (Ptr GLint -> IO ())
ptr_glBinormal3ivEXT = unsafePerformIO $ getCommand "glBinormal3ivEXT"

-- glBinormal3sEXT -------------------------------------------------------------

-- | The vector equivalent of this command is 'glBinormal3svEXT'.
glBinormal3sEXT
  :: MonadIO m
  => GLshort -- ^ @bx@.
  -> GLshort -- ^ @by@.
  -> GLshort -- ^ @bz@.
  -> m ()
glBinormal3sEXT v1 v2 v3 = liftIO $ dyn44 ptr_glBinormal3sEXT v1 v2 v3

{-# NOINLINE ptr_glBinormal3sEXT #-}
ptr_glBinormal3sEXT :: FunPtr (GLshort -> GLshort -> GLshort -> IO ())
ptr_glBinormal3sEXT = unsafePerformIO $ getCommand "glBinormal3sEXT"

-- glBinormal3svEXT ------------------------------------------------------------

glBinormal3svEXT
  :: MonadIO m
  => Ptr GLshort -- ^ @v@ pointing to @3@ elements of type @GLshort@.
  -> m ()
glBinormal3svEXT v1 = liftIO $ dyn45 ptr_glBinormal3svEXT v1

{-# NOINLINE ptr_glBinormal3svEXT #-}
ptr_glBinormal3svEXT :: FunPtr (Ptr GLshort -> IO ())
ptr_glBinormal3svEXT = unsafePerformIO $ getCommand "glBinormal3svEXT"

-- glBinormalPointerEXT --------------------------------------------------------

glBinormalPointerEXT
  :: MonadIO m
  => GLenum -- ^ @type@ of type @BinormalPointerTypeEXT@.
  -> GLsizei -- ^ @stride@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(type,stride)@ elements of type @a@.
  -> m ()
glBinormalPointerEXT v1 v2 v3 = liftIO $ dyn46 ptr_glBinormalPointerEXT v1 v2 v3

{-# NOINLINE ptr_glBinormalPointerEXT #-}
ptr_glBinormalPointerEXT :: FunPtr (GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glBinormalPointerEXT = unsafePerformIO $ getCommand "glBinormalPointerEXT"

-- glBitmap --------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glBitmap.xml OpenGL 2.x>.
glBitmap
  :: MonadIO m
  => GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLfloat -- ^ @xorig@ of type @CoordF@.
  -> GLfloat -- ^ @yorig@ of type @CoordF@.
  -> GLfloat -- ^ @xmove@ of type @CoordF@.
  -> GLfloat -- ^ @ymove@ of type @CoordF@.
  -> Ptr GLubyte -- ^ @bitmap@ pointing to @COMPSIZE(width,height)@ elements of type @GLubyte@.
  -> m ()
glBitmap v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn47 ptr_glBitmap v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glBitmap #-}
ptr_glBitmap :: FunPtr (GLsizei -> GLsizei -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> Ptr GLubyte -> IO ())
ptr_glBitmap = unsafePerformIO $ getCommand "glBitmap"

-- glBitmapxOES ----------------------------------------------------------------

glBitmapxOES
  :: MonadIO m
  => GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLfixed -- ^ @xorig@.
  -> GLfixed -- ^ @yorig@.
  -> GLfixed -- ^ @xmove@.
  -> GLfixed -- ^ @ymove@.
  -> Ptr GLubyte -- ^ @bitmap@ pointing to @COMPSIZE(width,height)@ elements of type @GLubyte@.
  -> m ()
glBitmapxOES v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn48 ptr_glBitmapxOES v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glBitmapxOES #-}
ptr_glBitmapxOES :: FunPtr (GLsizei -> GLsizei -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> Ptr GLubyte -> IO ())
ptr_glBitmapxOES = unsafePerformIO $ getCommand "glBitmapxOES"

-- glBlendBarrier --------------------------------------------------------------

glBlendBarrier
  :: MonadIO m
  => m ()
glBlendBarrier = liftIO $ dyn10 ptr_glBlendBarrier

{-# NOINLINE ptr_glBlendBarrier #-}
ptr_glBlendBarrier :: FunPtr (IO ())
ptr_glBlendBarrier = unsafePerformIO $ getCommand "glBlendBarrier"

-- glBlendBarrierKHR -----------------------------------------------------------

-- | This command is an alias for 'glBlendBarrier'.
glBlendBarrierKHR
  :: MonadIO m
  => m ()
glBlendBarrierKHR = liftIO $ dyn10 ptr_glBlendBarrierKHR

{-# NOINLINE ptr_glBlendBarrierKHR #-}
ptr_glBlendBarrierKHR :: FunPtr (IO ())
ptr_glBlendBarrierKHR = unsafePerformIO $ getCommand "glBlendBarrierKHR"

-- glBlendBarrierNV ------------------------------------------------------------

-- | This command is an alias for 'glBlendBarrier'.
glBlendBarrierNV
  :: MonadIO m
  => m ()
glBlendBarrierNV = liftIO $ dyn10 ptr_glBlendBarrierNV

{-# NOINLINE ptr_glBlendBarrierNV #-}
ptr_glBlendBarrierNV :: FunPtr (IO ())
ptr_glBlendBarrierNV = unsafePerformIO $ getCommand "glBlendBarrierNV"

-- glBlendColor ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glBlendColor.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glBlendColor.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glBlendColor.xhtml OpenGL 4.x>.
glBlendColor
  :: MonadIO m
  => GLfloat -- ^ @red@ of type @ColorF@.
  -> GLfloat -- ^ @green@ of type @ColorF@.
  -> GLfloat -- ^ @blue@ of type @ColorF@.
  -> GLfloat -- ^ @alpha@ of type @ColorF@.
  -> m ()
glBlendColor v1 v2 v3 v4 = liftIO $ dyn49 ptr_glBlendColor v1 v2 v3 v4

{-# NOINLINE ptr_glBlendColor #-}
ptr_glBlendColor :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glBlendColor = unsafePerformIO $ getCommand "glBlendColor"

-- glBlendColorEXT -------------------------------------------------------------

-- | This command is an alias for 'glBlendColor'.
glBlendColorEXT
  :: MonadIO m
  => GLfloat -- ^ @red@ of type @ColorF@.
  -> GLfloat -- ^ @green@ of type @ColorF@.
  -> GLfloat -- ^ @blue@ of type @ColorF@.
  -> GLfloat -- ^ @alpha@ of type @ColorF@.
  -> m ()
glBlendColorEXT v1 v2 v3 v4 = liftIO $ dyn49 ptr_glBlendColorEXT v1 v2 v3 v4

{-# NOINLINE ptr_glBlendColorEXT #-}
ptr_glBlendColorEXT :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glBlendColorEXT = unsafePerformIO $ getCommand "glBlendColorEXT"

-- glBlendColorxOES ------------------------------------------------------------

glBlendColorxOES
  :: MonadIO m
  => GLfixed -- ^ @red@ of type @ClampedFixed@.
  -> GLfixed -- ^ @green@ of type @ClampedFixed@.
  -> GLfixed -- ^ @blue@ of type @ClampedFixed@.
  -> GLfixed -- ^ @alpha@ of type @ClampedFixed@.
  -> m ()
glBlendColorxOES v1 v2 v3 v4 = liftIO $ dyn50 ptr_glBlendColorxOES v1 v2 v3 v4

{-# NOINLINE ptr_glBlendColorxOES #-}
ptr_glBlendColorxOES :: FunPtr (GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ())
ptr_glBlendColorxOES = unsafePerformIO $ getCommand "glBlendColorxOES"

-- glBlendEquation -------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glBlendEquation.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glBlendEquation.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glBlendEquation.xhtml OpenGL 4.x>.
glBlendEquation
  :: MonadIO m
  => GLenum -- ^ @mode@ of type @BlendEquationMode@.
  -> m ()
glBlendEquation v1 = liftIO $ dyn4 ptr_glBlendEquation v1

{-# NOINLINE ptr_glBlendEquation #-}
ptr_glBlendEquation :: FunPtr (GLenum -> IO ())
ptr_glBlendEquation = unsafePerformIO $ getCommand "glBlendEquation"

-- glBlendEquationEXT ----------------------------------------------------------

-- | This command is an alias for 'glBlendEquation'.
glBlendEquationEXT
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [BlendEquationModeEXT](Graphics-GL-Groups.html#BlendEquationModeEXT).
  -> m ()
glBlendEquationEXT v1 = liftIO $ dyn4 ptr_glBlendEquationEXT v1

{-# NOINLINE ptr_glBlendEquationEXT #-}
ptr_glBlendEquationEXT :: FunPtr (GLenum -> IO ())
ptr_glBlendEquationEXT = unsafePerformIO $ getCommand "glBlendEquationEXT"

-- glBlendEquationIndexedAMD ---------------------------------------------------

-- | This command is an alias for 'glBlendEquationi'.
glBlendEquationIndexedAMD
  :: MonadIO m
  => GLuint -- ^ @buf@.
  -> GLenum -- ^ @mode@.
  -> m ()
glBlendEquationIndexedAMD v1 v2 = liftIO $ dyn15 ptr_glBlendEquationIndexedAMD v1 v2

{-# NOINLINE ptr_glBlendEquationIndexedAMD #-}
ptr_glBlendEquationIndexedAMD :: FunPtr (GLuint -> GLenum -> IO ())
ptr_glBlendEquationIndexedAMD = unsafePerformIO $ getCommand "glBlendEquationIndexedAMD"

