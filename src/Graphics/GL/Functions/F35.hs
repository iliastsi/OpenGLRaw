--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F35
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

module Graphics.GL.Functions.F35 (
  glIsFramebufferEXT,
  glIsFramebufferOES,
  glIsImageHandleResidentARB,
  glIsImageHandleResidentNV,
  glIsList,
  glIsNameAMD,
  glIsNamedBufferResidentNV,
  glIsNamedStringARB,
  glIsObjectBufferATI,
  glIsOcclusionQueryNV,
  glIsPathNV,
  glIsPointInFillPathNV,
  glIsPointInStrokePathNV,
  glIsProgram,
  glIsProgramARB,
  glIsProgramNV,
  glIsProgramPipeline,
  glIsProgramPipelineEXT,
  glIsQuery,
  glIsQueryARB,
  glIsQueryEXT,
  glIsRenderbuffer,
  glIsRenderbufferEXT,
  glIsRenderbufferOES,
  glIsSampler,
  glIsShader,
  glIsStateNV,
  glIsSync,
  glIsSyncAPPLE,
  glIsTexture,
  glIsTextureEXT,
  glIsTextureHandleResidentARB,
  glIsTextureHandleResidentNV,
  glIsTransformFeedback,
  glIsTransformFeedbackNV,
  glIsVariantEnabledEXT,
  glIsVertexArray,
  glIsVertexArrayAPPLE,
  glIsVertexArrayOES,
  glIsVertexAttribEnabledAPPLE
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

-- glIsFramebufferEXT ----------------------------------------------------------

-- | This command is an alias for 'glIsFramebuffer'.
glIsFramebufferEXT
  :: MonadIO m
  => GLuint -- ^ @framebuffer@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsFramebufferEXT v1 = liftIO $ dyn273 ptr_glIsFramebufferEXT v1

{-# NOINLINE ptr_glIsFramebufferEXT #-}
ptr_glIsFramebufferEXT :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsFramebufferEXT = unsafePerformIO $ getCommand "glIsFramebufferEXT"

-- glIsFramebufferOES ----------------------------------------------------------

glIsFramebufferOES
  :: MonadIO m
  => GLuint -- ^ @framebuffer@.
  -> m GLboolean
glIsFramebufferOES v1 = liftIO $ dyn273 ptr_glIsFramebufferOES v1

{-# NOINLINE ptr_glIsFramebufferOES #-}
ptr_glIsFramebufferOES :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsFramebufferOES = unsafePerformIO $ getCommand "glIsFramebufferOES"

-- glIsImageHandleResidentARB --------------------------------------------------

glIsImageHandleResidentARB
  :: MonadIO m
  => GLuint64 -- ^ @handle@.
  -> m GLboolean
glIsImageHandleResidentARB v1 = liftIO $ dyn478 ptr_glIsImageHandleResidentARB v1

{-# NOINLINE ptr_glIsImageHandleResidentARB #-}
ptr_glIsImageHandleResidentARB :: FunPtr (GLuint64 -> IO GLboolean)
ptr_glIsImageHandleResidentARB = unsafePerformIO $ getCommand "glIsImageHandleResidentARB"

-- glIsImageHandleResidentNV ---------------------------------------------------

glIsImageHandleResidentNV
  :: MonadIO m
  => GLuint64 -- ^ @handle@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsImageHandleResidentNV v1 = liftIO $ dyn478 ptr_glIsImageHandleResidentNV v1

{-# NOINLINE ptr_glIsImageHandleResidentNV #-}
ptr_glIsImageHandleResidentNV :: FunPtr (GLuint64 -> IO GLboolean)
ptr_glIsImageHandleResidentNV = unsafePerformIO $ getCommand "glIsImageHandleResidentNV"

-- glIsList --------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glIsList.xml OpenGL 2.x>.
glIsList
  :: MonadIO m
  => GLuint -- ^ @list@ of type @List@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsList v1 = liftIO $ dyn273 ptr_glIsList v1

{-# NOINLINE ptr_glIsList #-}
ptr_glIsList :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsList = unsafePerformIO $ getCommand "glIsList"

-- glIsNameAMD -----------------------------------------------------------------

glIsNameAMD
  :: MonadIO m
  => GLenum -- ^ @identifier@.
  -> GLuint -- ^ @name@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsNameAMD v1 v2 = liftIO $ dyn477 ptr_glIsNameAMD v1 v2

{-# NOINLINE ptr_glIsNameAMD #-}
ptr_glIsNameAMD :: FunPtr (GLenum -> GLuint -> IO GLboolean)
ptr_glIsNameAMD = unsafePerformIO $ getCommand "glIsNameAMD"

-- glIsNamedBufferResidentNV ---------------------------------------------------

glIsNamedBufferResidentNV
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsNamedBufferResidentNV v1 = liftIO $ dyn273 ptr_glIsNamedBufferResidentNV v1

{-# NOINLINE ptr_glIsNamedBufferResidentNV #-}
ptr_glIsNamedBufferResidentNV :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsNamedBufferResidentNV = unsafePerformIO $ getCommand "glIsNamedBufferResidentNV"

-- glIsNamedStringARB ----------------------------------------------------------

glIsNamedStringARB
  :: MonadIO m
  => GLint -- ^ @namelen@.
  -> Ptr GLchar -- ^ @name@ pointing to @namelen@ elements of type @GLchar@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsNamedStringARB v1 v2 = liftIO $ dyn479 ptr_glIsNamedStringARB v1 v2

{-# NOINLINE ptr_glIsNamedStringARB #-}
ptr_glIsNamedStringARB :: FunPtr (GLint -> Ptr GLchar -> IO GLboolean)
ptr_glIsNamedStringARB = unsafePerformIO $ getCommand "glIsNamedStringARB"

-- glIsObjectBufferATI ---------------------------------------------------------

glIsObjectBufferATI
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsObjectBufferATI v1 = liftIO $ dyn273 ptr_glIsObjectBufferATI v1

{-# NOINLINE ptr_glIsObjectBufferATI #-}
ptr_glIsObjectBufferATI :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsObjectBufferATI = unsafePerformIO $ getCommand "glIsObjectBufferATI"

-- glIsOcclusionQueryNV --------------------------------------------------------

glIsOcclusionQueryNV
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsOcclusionQueryNV v1 = liftIO $ dyn273 ptr_glIsOcclusionQueryNV v1

{-# NOINLINE ptr_glIsOcclusionQueryNV #-}
ptr_glIsOcclusionQueryNV :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsOcclusionQueryNV = unsafePerformIO $ getCommand "glIsOcclusionQueryNV"

-- glIsPathNV ------------------------------------------------------------------

glIsPathNV
  :: MonadIO m
  => GLuint -- ^ @path@ of type @Path@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsPathNV v1 = liftIO $ dyn273 ptr_glIsPathNV v1

{-# NOINLINE ptr_glIsPathNV #-}
ptr_glIsPathNV :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsPathNV = unsafePerformIO $ getCommand "glIsPathNV"

-- glIsPointInFillPathNV -------------------------------------------------------

glIsPointInFillPathNV
  :: MonadIO m
  => GLuint -- ^ @path@ of type @Path@.
  -> GLuint -- ^ @mask@ of type @MaskedStencilValue@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsPointInFillPathNV v1 v2 v3 v4 = liftIO $ dyn480 ptr_glIsPointInFillPathNV v1 v2 v3 v4

{-# NOINLINE ptr_glIsPointInFillPathNV #-}
ptr_glIsPointInFillPathNV :: FunPtr (GLuint -> GLuint -> GLfloat -> GLfloat -> IO GLboolean)
ptr_glIsPointInFillPathNV = unsafePerformIO $ getCommand "glIsPointInFillPathNV"

-- glIsPointInStrokePathNV -----------------------------------------------------

glIsPointInStrokePathNV
  :: MonadIO m
  => GLuint -- ^ @path@ of type @Path@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsPointInStrokePathNV v1 v2 v3 = liftIO $ dyn481 ptr_glIsPointInStrokePathNV v1 v2 v3

{-# NOINLINE ptr_glIsPointInStrokePathNV #-}
ptr_glIsPointInStrokePathNV :: FunPtr (GLuint -> GLfloat -> GLfloat -> IO GLboolean)
ptr_glIsPointInStrokePathNV = unsafePerformIO $ getCommand "glIsPointInStrokePathNV"

-- glIsProgram -----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glIsProgram.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glIsProgram.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glIsProgram.xhtml OpenGL 4.x>.
glIsProgram
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsProgram v1 = liftIO $ dyn273 ptr_glIsProgram v1

{-# NOINLINE ptr_glIsProgram #-}
ptr_glIsProgram :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsProgram = unsafePerformIO $ getCommand "glIsProgram"

-- glIsProgramARB --------------------------------------------------------------

glIsProgramARB
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsProgramARB v1 = liftIO $ dyn273 ptr_glIsProgramARB v1

{-# NOINLINE ptr_glIsProgramARB #-}
ptr_glIsProgramARB :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsProgramARB = unsafePerformIO $ getCommand "glIsProgramARB"

-- glIsProgramNV ---------------------------------------------------------------

-- | This command is an alias for 'glIsProgramARB'.
glIsProgramNV
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsProgramNV v1 = liftIO $ dyn273 ptr_glIsProgramNV v1

{-# NOINLINE ptr_glIsProgramNV #-}
ptr_glIsProgramNV :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsProgramNV = unsafePerformIO $ getCommand "glIsProgramNV"

-- glIsProgramPipeline ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glIsProgramPipeline.xhtml OpenGL 4.x>.
glIsProgramPipeline
  :: MonadIO m
  => GLuint -- ^ @pipeline@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsProgramPipeline v1 = liftIO $ dyn273 ptr_glIsProgramPipeline v1

{-# NOINLINE ptr_glIsProgramPipeline #-}
ptr_glIsProgramPipeline :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsProgramPipeline = unsafePerformIO $ getCommand "glIsProgramPipeline"

-- glIsProgramPipelineEXT ------------------------------------------------------

glIsProgramPipelineEXT
  :: MonadIO m
  => GLuint -- ^ @pipeline@.
  -> m GLboolean
glIsProgramPipelineEXT v1 = liftIO $ dyn273 ptr_glIsProgramPipelineEXT v1

{-# NOINLINE ptr_glIsProgramPipelineEXT #-}
ptr_glIsProgramPipelineEXT :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsProgramPipelineEXT = unsafePerformIO $ getCommand "glIsProgramPipelineEXT"

-- glIsQuery -------------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glIsQuery.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glIsQuery.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glIsQuery.xhtml OpenGL 4.x>.
glIsQuery
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsQuery v1 = liftIO $ dyn273 ptr_glIsQuery v1

{-# NOINLINE ptr_glIsQuery #-}
ptr_glIsQuery :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsQuery = unsafePerformIO $ getCommand "glIsQuery"

-- glIsQueryARB ----------------------------------------------------------------

-- | This command is an alias for 'glIsQuery'.
glIsQueryARB
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsQueryARB v1 = liftIO $ dyn273 ptr_glIsQueryARB v1

{-# NOINLINE ptr_glIsQueryARB #-}
ptr_glIsQueryARB :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsQueryARB = unsafePerformIO $ getCommand "glIsQueryARB"

-- glIsQueryEXT ----------------------------------------------------------------

glIsQueryEXT
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> m GLboolean
glIsQueryEXT v1 = liftIO $ dyn273 ptr_glIsQueryEXT v1

{-# NOINLINE ptr_glIsQueryEXT #-}
ptr_glIsQueryEXT :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsQueryEXT = unsafePerformIO $ getCommand "glIsQueryEXT"

-- glIsRenderbuffer ------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glIsRenderbuffer.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glIsRenderbuffer.xhtml OpenGL 4.x>.
glIsRenderbuffer
  :: MonadIO m
  => GLuint -- ^ @renderbuffer@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsRenderbuffer v1 = liftIO $ dyn273 ptr_glIsRenderbuffer v1

{-# NOINLINE ptr_glIsRenderbuffer #-}
ptr_glIsRenderbuffer :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsRenderbuffer = unsafePerformIO $ getCommand "glIsRenderbuffer"

-- glIsRenderbufferEXT ---------------------------------------------------------

-- | This command is an alias for 'glIsRenderbuffer'.
glIsRenderbufferEXT
  :: MonadIO m
  => GLuint -- ^ @renderbuffer@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsRenderbufferEXT v1 = liftIO $ dyn273 ptr_glIsRenderbufferEXT v1

{-# NOINLINE ptr_glIsRenderbufferEXT #-}
ptr_glIsRenderbufferEXT :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsRenderbufferEXT = unsafePerformIO $ getCommand "glIsRenderbufferEXT"

-- glIsRenderbufferOES ---------------------------------------------------------

glIsRenderbufferOES
  :: MonadIO m
  => GLuint -- ^ @renderbuffer@.
  -> m GLboolean
glIsRenderbufferOES v1 = liftIO $ dyn273 ptr_glIsRenderbufferOES v1

{-# NOINLINE ptr_glIsRenderbufferOES #-}
ptr_glIsRenderbufferOES :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsRenderbufferOES = unsafePerformIO $ getCommand "glIsRenderbufferOES"

-- glIsSampler -----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glIsSampler.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glIsSampler.xhtml OpenGL 4.x>.
glIsSampler
  :: MonadIO m
  => GLuint -- ^ @sampler@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsSampler v1 = liftIO $ dyn273 ptr_glIsSampler v1

{-# NOINLINE ptr_glIsSampler #-}
ptr_glIsSampler :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsSampler = unsafePerformIO $ getCommand "glIsSampler"

-- glIsShader ------------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glIsShader.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glIsShader.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glIsShader.xhtml OpenGL 4.x>.
glIsShader
  :: MonadIO m
  => GLuint -- ^ @shader@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsShader v1 = liftIO $ dyn273 ptr_glIsShader v1

{-# NOINLINE ptr_glIsShader #-}
ptr_glIsShader :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsShader = unsafePerformIO $ getCommand "glIsShader"

-- glIsStateNV -----------------------------------------------------------------

glIsStateNV
  :: MonadIO m
  => GLuint -- ^ @state@.
  -> m GLboolean
glIsStateNV v1 = liftIO $ dyn273 ptr_glIsStateNV v1

{-# NOINLINE ptr_glIsStateNV #-}
ptr_glIsStateNV :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsStateNV = unsafePerformIO $ getCommand "glIsStateNV"

-- glIsSync --------------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glIsSync.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glIsSync.xhtml OpenGL 4.x>.
glIsSync
  :: MonadIO m
  => GLsync -- ^ @sync@ of type @sync@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsSync v1 = liftIO $ dyn482 ptr_glIsSync v1

{-# NOINLINE ptr_glIsSync #-}
ptr_glIsSync :: FunPtr (GLsync -> IO GLboolean)
ptr_glIsSync = unsafePerformIO $ getCommand "glIsSync"

-- glIsSyncAPPLE ---------------------------------------------------------------

-- | This command is an alias for 'glIsSync'.
glIsSyncAPPLE
  :: MonadIO m
  => GLsync -- ^ @sync@.
  -> m GLboolean
glIsSyncAPPLE v1 = liftIO $ dyn482 ptr_glIsSyncAPPLE v1

{-# NOINLINE ptr_glIsSyncAPPLE #-}
ptr_glIsSyncAPPLE :: FunPtr (GLsync -> IO GLboolean)
ptr_glIsSyncAPPLE = unsafePerformIO $ getCommand "glIsSyncAPPLE"

-- glIsTexture -----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glIsTexture.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glIsTexture.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glIsTexture.xhtml OpenGL 4.x>.
glIsTexture
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsTexture v1 = liftIO $ dyn273 ptr_glIsTexture v1

{-# NOINLINE ptr_glIsTexture #-}
ptr_glIsTexture :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsTexture = unsafePerformIO $ getCommand "glIsTexture"

-- glIsTextureEXT --------------------------------------------------------------

glIsTextureEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsTextureEXT v1 = liftIO $ dyn273 ptr_glIsTextureEXT v1

{-# NOINLINE ptr_glIsTextureEXT #-}
ptr_glIsTextureEXT :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsTextureEXT = unsafePerformIO $ getCommand "glIsTextureEXT"

-- glIsTextureHandleResidentARB ------------------------------------------------

glIsTextureHandleResidentARB
  :: MonadIO m
  => GLuint64 -- ^ @handle@.
  -> m GLboolean
glIsTextureHandleResidentARB v1 = liftIO $ dyn478 ptr_glIsTextureHandleResidentARB v1

{-# NOINLINE ptr_glIsTextureHandleResidentARB #-}
ptr_glIsTextureHandleResidentARB :: FunPtr (GLuint64 -> IO GLboolean)
ptr_glIsTextureHandleResidentARB = unsafePerformIO $ getCommand "glIsTextureHandleResidentARB"

-- glIsTextureHandleResidentNV -------------------------------------------------

glIsTextureHandleResidentNV
  :: MonadIO m
  => GLuint64 -- ^ @handle@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsTextureHandleResidentNV v1 = liftIO $ dyn478 ptr_glIsTextureHandleResidentNV v1

{-# NOINLINE ptr_glIsTextureHandleResidentNV #-}
ptr_glIsTextureHandleResidentNV :: FunPtr (GLuint64 -> IO GLboolean)
ptr_glIsTextureHandleResidentNV = unsafePerformIO $ getCommand "glIsTextureHandleResidentNV"

-- glIsTransformFeedback -------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glIsTransformFeedback.xhtml OpenGL 4.x>.
glIsTransformFeedback
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsTransformFeedback v1 = liftIO $ dyn273 ptr_glIsTransformFeedback v1

{-# NOINLINE ptr_glIsTransformFeedback #-}
ptr_glIsTransformFeedback :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsTransformFeedback = unsafePerformIO $ getCommand "glIsTransformFeedback"

-- glIsTransformFeedbackNV -----------------------------------------------------

-- | This command is an alias for 'glIsTransformFeedback'.
glIsTransformFeedbackNV
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsTransformFeedbackNV v1 = liftIO $ dyn273 ptr_glIsTransformFeedbackNV v1

{-# NOINLINE ptr_glIsTransformFeedbackNV #-}
ptr_glIsTransformFeedbackNV :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsTransformFeedbackNV = unsafePerformIO $ getCommand "glIsTransformFeedbackNV"

-- glIsVariantEnabledEXT -------------------------------------------------------

glIsVariantEnabledEXT
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @cap@ of type @VariantCapEXT@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsVariantEnabledEXT v1 v2 = liftIO $ dyn483 ptr_glIsVariantEnabledEXT v1 v2

{-# NOINLINE ptr_glIsVariantEnabledEXT #-}
ptr_glIsVariantEnabledEXT :: FunPtr (GLuint -> GLenum -> IO GLboolean)
ptr_glIsVariantEnabledEXT = unsafePerformIO $ getCommand "glIsVariantEnabledEXT"

-- glIsVertexArray -------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glIsVertexArray.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glIsVertexArray.xhtml OpenGL 4.x>.
glIsVertexArray
  :: MonadIO m
  => GLuint -- ^ @array@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsVertexArray v1 = liftIO $ dyn273 ptr_glIsVertexArray v1

{-# NOINLINE ptr_glIsVertexArray #-}
ptr_glIsVertexArray :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsVertexArray = unsafePerformIO $ getCommand "glIsVertexArray"

-- glIsVertexArrayAPPLE --------------------------------------------------------

-- | This command is an alias for 'glIsVertexArray'.
glIsVertexArrayAPPLE
  :: MonadIO m
  => GLuint -- ^ @array@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsVertexArrayAPPLE v1 = liftIO $ dyn273 ptr_glIsVertexArrayAPPLE v1

{-# NOINLINE ptr_glIsVertexArrayAPPLE #-}
ptr_glIsVertexArrayAPPLE :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsVertexArrayAPPLE = unsafePerformIO $ getCommand "glIsVertexArrayAPPLE"

-- glIsVertexArrayOES ----------------------------------------------------------

-- | This command is an alias for 'glIsVertexArray'.
glIsVertexArrayOES
  :: MonadIO m
  => GLuint -- ^ @array@.
  -> m GLboolean
glIsVertexArrayOES v1 = liftIO $ dyn273 ptr_glIsVertexArrayOES v1

{-# NOINLINE ptr_glIsVertexArrayOES #-}
ptr_glIsVertexArrayOES :: FunPtr (GLuint -> IO GLboolean)
ptr_glIsVertexArrayOES = unsafePerformIO $ getCommand "glIsVertexArrayOES"

-- glIsVertexAttribEnabledAPPLE ------------------------------------------------

glIsVertexAttribEnabledAPPLE
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glIsVertexAttribEnabledAPPLE v1 v2 = liftIO $ dyn483 ptr_glIsVertexAttribEnabledAPPLE v1 v2

{-# NOINLINE ptr_glIsVertexAttribEnabledAPPLE #-}
ptr_glIsVertexAttribEnabledAPPLE :: FunPtr (GLuint -> GLenum -> IO GLboolean)
ptr_glIsVertexAttribEnabledAPPLE = unsafePerformIO $ getCommand "glIsVertexAttribEnabledAPPLE"

