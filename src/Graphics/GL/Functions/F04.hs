--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F04
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

module Graphics.GL.Functions.F04 (
  glBlendEquationOES,
  glBlendEquationSeparate,
  glBlendEquationSeparateEXT,
  glBlendEquationSeparateIndexedAMD,
  glBlendEquationSeparateOES,
  glBlendEquationSeparatei,
  glBlendEquationSeparateiARB,
  glBlendEquationSeparateiEXT,
  glBlendEquationSeparateiOES,
  glBlendEquationi,
  glBlendEquationiARB,
  glBlendEquationiEXT,
  glBlendEquationiOES,
  glBlendFunc,
  glBlendFuncIndexedAMD,
  glBlendFuncSeparate,
  glBlendFuncSeparateEXT,
  glBlendFuncSeparateINGR,
  glBlendFuncSeparateIndexedAMD,
  glBlendFuncSeparateOES,
  glBlendFuncSeparatei,
  glBlendFuncSeparateiARB,
  glBlendFuncSeparateiEXT,
  glBlendFuncSeparateiOES,
  glBlendFunci,
  glBlendFunciARB,
  glBlendFunciEXT,
  glBlendFunciOES,
  glBlendParameteriNV,
  glBlitFramebuffer,
  glBlitFramebufferANGLE,
  glBlitFramebufferEXT,
  glBlitFramebufferNV,
  glBlitNamedFramebuffer,
  glBufferAddressRangeNV,
  glBufferData,
  glBufferDataARB,
  glBufferPageCommitmentARB,
  glBufferParameteriAPPLE,
  glBufferStorage
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

-- glBlendEquationOES ----------------------------------------------------------

glBlendEquationOES
  :: MonadIO m
  => GLenum -- ^ @mode@.
  -> m ()
glBlendEquationOES v1 = liftIO $ dyn4 ptr_glBlendEquationOES v1

{-# NOINLINE ptr_glBlendEquationOES #-}
ptr_glBlendEquationOES :: FunPtr (GLenum -> IO ())
ptr_glBlendEquationOES = unsafePerformIO $ getCommand "glBlendEquationOES"

-- glBlendEquationSeparate -----------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glBlendEquationSeparate.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glBlendEquationSeparate.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glBlendEquationSeparate.xhtml OpenGL 4.x>.
glBlendEquationSeparate
  :: MonadIO m
  => GLenum -- ^ @modeRGB@ of type [BlendEquationModeEXT](Graphics-GL-Groups.html#BlendEquationModeEXT).
  -> GLenum -- ^ @modeAlpha@ of type [BlendEquationModeEXT](Graphics-GL-Groups.html#BlendEquationModeEXT).
  -> m ()
glBlendEquationSeparate v1 v2 = liftIO $ dyn51 ptr_glBlendEquationSeparate v1 v2

{-# NOINLINE ptr_glBlendEquationSeparate #-}
ptr_glBlendEquationSeparate :: FunPtr (GLenum -> GLenum -> IO ())
ptr_glBlendEquationSeparate = unsafePerformIO $ getCommand "glBlendEquationSeparate"

-- glBlendEquationSeparateEXT --------------------------------------------------

-- | This command is an alias for 'glBlendEquationSeparate'.
glBlendEquationSeparateEXT
  :: MonadIO m
  => GLenum -- ^ @modeRGB@ of type [BlendEquationModeEXT](Graphics-GL-Groups.html#BlendEquationModeEXT).
  -> GLenum -- ^ @modeAlpha@ of type [BlendEquationModeEXT](Graphics-GL-Groups.html#BlendEquationModeEXT).
  -> m ()
glBlendEquationSeparateEXT v1 v2 = liftIO $ dyn51 ptr_glBlendEquationSeparateEXT v1 v2

{-# NOINLINE ptr_glBlendEquationSeparateEXT #-}
ptr_glBlendEquationSeparateEXT :: FunPtr (GLenum -> GLenum -> IO ())
ptr_glBlendEquationSeparateEXT = unsafePerformIO $ getCommand "glBlendEquationSeparateEXT"

-- glBlendEquationSeparateIndexedAMD -------------------------------------------

-- | This command is an alias for 'glBlendEquationSeparatei'.
glBlendEquationSeparateIndexedAMD
  :: MonadIO m
  => GLuint -- ^ @buf@.
  -> GLenum -- ^ @modeRGB@.
  -> GLenum -- ^ @modeAlpha@.
  -> m ()
glBlendEquationSeparateIndexedAMD v1 v2 v3 = liftIO $ dyn52 ptr_glBlendEquationSeparateIndexedAMD v1 v2 v3

{-# NOINLINE ptr_glBlendEquationSeparateIndexedAMD #-}
ptr_glBlendEquationSeparateIndexedAMD :: FunPtr (GLuint -> GLenum -> GLenum -> IO ())
ptr_glBlendEquationSeparateIndexedAMD = unsafePerformIO $ getCommand "glBlendEquationSeparateIndexedAMD"

-- glBlendEquationSeparateOES --------------------------------------------------

glBlendEquationSeparateOES
  :: MonadIO m
  => GLenum -- ^ @modeRGB@.
  -> GLenum -- ^ @modeAlpha@.
  -> m ()
glBlendEquationSeparateOES v1 v2 = liftIO $ dyn51 ptr_glBlendEquationSeparateOES v1 v2

{-# NOINLINE ptr_glBlendEquationSeparateOES #-}
ptr_glBlendEquationSeparateOES :: FunPtr (GLenum -> GLenum -> IO ())
ptr_glBlendEquationSeparateOES = unsafePerformIO $ getCommand "glBlendEquationSeparateOES"

-- glBlendEquationSeparatei ----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glBlendEquationSeparate.xhtml OpenGL 4.x>.
glBlendEquationSeparatei
  :: MonadIO m
  => GLuint -- ^ @buf@.
  -> GLenum -- ^ @modeRGB@.
  -> GLenum -- ^ @modeAlpha@.
  -> m ()
glBlendEquationSeparatei v1 v2 v3 = liftIO $ dyn52 ptr_glBlendEquationSeparatei v1 v2 v3

{-# NOINLINE ptr_glBlendEquationSeparatei #-}
ptr_glBlendEquationSeparatei :: FunPtr (GLuint -> GLenum -> GLenum -> IO ())
ptr_glBlendEquationSeparatei = unsafePerformIO $ getCommand "glBlendEquationSeparatei"

-- glBlendEquationSeparateiARB -------------------------------------------------

-- | This command is an alias for 'glBlendEquationSeparatei'.
glBlendEquationSeparateiARB
  :: MonadIO m
  => GLuint -- ^ @buf@.
  -> GLenum -- ^ @modeRGB@.
  -> GLenum -- ^ @modeAlpha@.
  -> m ()
glBlendEquationSeparateiARB v1 v2 v3 = liftIO $ dyn52 ptr_glBlendEquationSeparateiARB v1 v2 v3

{-# NOINLINE ptr_glBlendEquationSeparateiARB #-}
ptr_glBlendEquationSeparateiARB :: FunPtr (GLuint -> GLenum -> GLenum -> IO ())
ptr_glBlendEquationSeparateiARB = unsafePerformIO $ getCommand "glBlendEquationSeparateiARB"

-- glBlendEquationSeparateiEXT -------------------------------------------------

-- | This command is an alias for 'glBlendEquationSeparatei'.
glBlendEquationSeparateiEXT
  :: MonadIO m
  => GLuint -- ^ @buf@.
  -> GLenum -- ^ @modeRGB@.
  -> GLenum -- ^ @modeAlpha@.
  -> m ()
glBlendEquationSeparateiEXT v1 v2 v3 = liftIO $ dyn52 ptr_glBlendEquationSeparateiEXT v1 v2 v3

{-# NOINLINE ptr_glBlendEquationSeparateiEXT #-}
ptr_glBlendEquationSeparateiEXT :: FunPtr (GLuint -> GLenum -> GLenum -> IO ())
ptr_glBlendEquationSeparateiEXT = unsafePerformIO $ getCommand "glBlendEquationSeparateiEXT"

-- glBlendEquationSeparateiOES -------------------------------------------------

-- | This command is an alias for 'glBlendEquationSeparatei'.
glBlendEquationSeparateiOES
  :: MonadIO m
  => GLuint -- ^ @buf@.
  -> GLenum -- ^ @modeRGB@.
  -> GLenum -- ^ @modeAlpha@.
  -> m ()
glBlendEquationSeparateiOES v1 v2 v3 = liftIO $ dyn52 ptr_glBlendEquationSeparateiOES v1 v2 v3

{-# NOINLINE ptr_glBlendEquationSeparateiOES #-}
ptr_glBlendEquationSeparateiOES :: FunPtr (GLuint -> GLenum -> GLenum -> IO ())
ptr_glBlendEquationSeparateiOES = unsafePerformIO $ getCommand "glBlendEquationSeparateiOES"

-- glBlendEquationi ------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glBlendEquation.xhtml OpenGL 4.x>.
glBlendEquationi
  :: MonadIO m
  => GLuint -- ^ @buf@.
  -> GLenum -- ^ @mode@.
  -> m ()
glBlendEquationi v1 v2 = liftIO $ dyn15 ptr_glBlendEquationi v1 v2

{-# NOINLINE ptr_glBlendEquationi #-}
ptr_glBlendEquationi :: FunPtr (GLuint -> GLenum -> IO ())
ptr_glBlendEquationi = unsafePerformIO $ getCommand "glBlendEquationi"

-- glBlendEquationiARB ---------------------------------------------------------

-- | This command is an alias for 'glBlendEquationi'.
glBlendEquationiARB
  :: MonadIO m
  => GLuint -- ^ @buf@.
  -> GLenum -- ^ @mode@.
  -> m ()
glBlendEquationiARB v1 v2 = liftIO $ dyn15 ptr_glBlendEquationiARB v1 v2

{-# NOINLINE ptr_glBlendEquationiARB #-}
ptr_glBlendEquationiARB :: FunPtr (GLuint -> GLenum -> IO ())
ptr_glBlendEquationiARB = unsafePerformIO $ getCommand "glBlendEquationiARB"

-- glBlendEquationiEXT ---------------------------------------------------------

-- | This command is an alias for 'glBlendEquationi'.
glBlendEquationiEXT
  :: MonadIO m
  => GLuint -- ^ @buf@.
  -> GLenum -- ^ @mode@.
  -> m ()
glBlendEquationiEXT v1 v2 = liftIO $ dyn15 ptr_glBlendEquationiEXT v1 v2

{-# NOINLINE ptr_glBlendEquationiEXT #-}
ptr_glBlendEquationiEXT :: FunPtr (GLuint -> GLenum -> IO ())
ptr_glBlendEquationiEXT = unsafePerformIO $ getCommand "glBlendEquationiEXT"

-- glBlendEquationiOES ---------------------------------------------------------

-- | This command is an alias for 'glBlendEquationi'.
glBlendEquationiOES
  :: MonadIO m
  => GLuint -- ^ @buf@.
  -> GLenum -- ^ @mode@.
  -> m ()
glBlendEquationiOES v1 v2 = liftIO $ dyn15 ptr_glBlendEquationiOES v1 v2

{-# NOINLINE ptr_glBlendEquationiOES #-}
ptr_glBlendEquationiOES :: FunPtr (GLuint -> GLenum -> IO ())
ptr_glBlendEquationiOES = unsafePerformIO $ getCommand "glBlendEquationiOES"

-- glBlendFunc -----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glBlendFunc.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glBlendFunc.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glBlendFunc.xhtml OpenGL 4.x>.
glBlendFunc
  :: MonadIO m
  => GLenum -- ^ @sfactor@ of type [BlendingFactorSrc](Graphics-GL-Groups.html#BlendingFactorSrc).
  -> GLenum -- ^ @dfactor@ of type [BlendingFactorDest](Graphics-GL-Groups.html#BlendingFactorDest).
  -> m ()
glBlendFunc v1 v2 = liftIO $ dyn51 ptr_glBlendFunc v1 v2

{-# NOINLINE ptr_glBlendFunc #-}
ptr_glBlendFunc :: FunPtr (GLenum -> GLenum -> IO ())
ptr_glBlendFunc = unsafePerformIO $ getCommand "glBlendFunc"

-- glBlendFuncIndexedAMD -------------------------------------------------------

-- | This command is an alias for 'glBlendFunci'.
glBlendFuncIndexedAMD
  :: MonadIO m
  => GLuint -- ^ @buf@.
  -> GLenum -- ^ @src@.
  -> GLenum -- ^ @dst@.
  -> m ()
glBlendFuncIndexedAMD v1 v2 v3 = liftIO $ dyn52 ptr_glBlendFuncIndexedAMD v1 v2 v3

{-# NOINLINE ptr_glBlendFuncIndexedAMD #-}
ptr_glBlendFuncIndexedAMD :: FunPtr (GLuint -> GLenum -> GLenum -> IO ())
ptr_glBlendFuncIndexedAMD = unsafePerformIO $ getCommand "glBlendFuncIndexedAMD"

-- glBlendFuncSeparate ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glBlendFuncSeparate.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glBlendFuncSeparate.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glBlendFuncSeparate.xhtml OpenGL 4.x>.
glBlendFuncSeparate
  :: MonadIO m
  => GLenum -- ^ @sfactorRGB@ of type @BlendFuncSeparateParameterEXT@.
  -> GLenum -- ^ @dfactorRGB@ of type @BlendFuncSeparateParameterEXT@.
  -> GLenum -- ^ @sfactorAlpha@ of type @BlendFuncSeparateParameterEXT@.
  -> GLenum -- ^ @dfactorAlpha@ of type @BlendFuncSeparateParameterEXT@.
  -> m ()
glBlendFuncSeparate v1 v2 v3 v4 = liftIO $ dyn53 ptr_glBlendFuncSeparate v1 v2 v3 v4

{-# NOINLINE ptr_glBlendFuncSeparate #-}
ptr_glBlendFuncSeparate :: FunPtr (GLenum -> GLenum -> GLenum -> GLenum -> IO ())
ptr_glBlendFuncSeparate = unsafePerformIO $ getCommand "glBlendFuncSeparate"

-- glBlendFuncSeparateEXT ------------------------------------------------------

-- | This command is an alias for 'glBlendFuncSeparate'.
glBlendFuncSeparateEXT
  :: MonadIO m
  => GLenum -- ^ @sfactorRGB@ of type @BlendFuncSeparateParameterEXT@.
  -> GLenum -- ^ @dfactorRGB@ of type @BlendFuncSeparateParameterEXT@.
  -> GLenum -- ^ @sfactorAlpha@ of type @BlendFuncSeparateParameterEXT@.
  -> GLenum -- ^ @dfactorAlpha@ of type @BlendFuncSeparateParameterEXT@.
  -> m ()
glBlendFuncSeparateEXT v1 v2 v3 v4 = liftIO $ dyn53 ptr_glBlendFuncSeparateEXT v1 v2 v3 v4

{-# NOINLINE ptr_glBlendFuncSeparateEXT #-}
ptr_glBlendFuncSeparateEXT :: FunPtr (GLenum -> GLenum -> GLenum -> GLenum -> IO ())
ptr_glBlendFuncSeparateEXT = unsafePerformIO $ getCommand "glBlendFuncSeparateEXT"

-- glBlendFuncSeparateINGR -----------------------------------------------------

-- | This command is an alias for 'glBlendFuncSeparate'.
glBlendFuncSeparateINGR
  :: MonadIO m
  => GLenum -- ^ @sfactorRGB@ of type @BlendFuncSeparateParameterEXT@.
  -> GLenum -- ^ @dfactorRGB@ of type @BlendFuncSeparateParameterEXT@.
  -> GLenum -- ^ @sfactorAlpha@ of type @BlendFuncSeparateParameterEXT@.
  -> GLenum -- ^ @dfactorAlpha@ of type @BlendFuncSeparateParameterEXT@.
  -> m ()
glBlendFuncSeparateINGR v1 v2 v3 v4 = liftIO $ dyn53 ptr_glBlendFuncSeparateINGR v1 v2 v3 v4

{-# NOINLINE ptr_glBlendFuncSeparateINGR #-}
ptr_glBlendFuncSeparateINGR :: FunPtr (GLenum -> GLenum -> GLenum -> GLenum -> IO ())
ptr_glBlendFuncSeparateINGR = unsafePerformIO $ getCommand "glBlendFuncSeparateINGR"

-- glBlendFuncSeparateIndexedAMD -----------------------------------------------

-- | This command is an alias for 'glBlendFuncSeparatei'.
glBlendFuncSeparateIndexedAMD
  :: MonadIO m
  => GLuint -- ^ @buf@.
  -> GLenum -- ^ @srcRGB@.
  -> GLenum -- ^ @dstRGB@.
  -> GLenum -- ^ @srcAlpha@.
  -> GLenum -- ^ @dstAlpha@.
  -> m ()
glBlendFuncSeparateIndexedAMD v1 v2 v3 v4 v5 = liftIO $ dyn54 ptr_glBlendFuncSeparateIndexedAMD v1 v2 v3 v4 v5

{-# NOINLINE ptr_glBlendFuncSeparateIndexedAMD #-}
ptr_glBlendFuncSeparateIndexedAMD :: FunPtr (GLuint -> GLenum -> GLenum -> GLenum -> GLenum -> IO ())
ptr_glBlendFuncSeparateIndexedAMD = unsafePerformIO $ getCommand "glBlendFuncSeparateIndexedAMD"

-- glBlendFuncSeparateOES ------------------------------------------------------

glBlendFuncSeparateOES
  :: MonadIO m
  => GLenum -- ^ @srcRGB@.
  -> GLenum -- ^ @dstRGB@.
  -> GLenum -- ^ @srcAlpha@.
  -> GLenum -- ^ @dstAlpha@.
  -> m ()
glBlendFuncSeparateOES v1 v2 v3 v4 = liftIO $ dyn53 ptr_glBlendFuncSeparateOES v1 v2 v3 v4

{-# NOINLINE ptr_glBlendFuncSeparateOES #-}
ptr_glBlendFuncSeparateOES :: FunPtr (GLenum -> GLenum -> GLenum -> GLenum -> IO ())
ptr_glBlendFuncSeparateOES = unsafePerformIO $ getCommand "glBlendFuncSeparateOES"

-- glBlendFuncSeparatei --------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glBlendFuncSeparate.xhtml OpenGL 4.x>.
glBlendFuncSeparatei
  :: MonadIO m
  => GLuint -- ^ @buf@.
  -> GLenum -- ^ @srcRGB@.
  -> GLenum -- ^ @dstRGB@.
  -> GLenum -- ^ @srcAlpha@.
  -> GLenum -- ^ @dstAlpha@.
  -> m ()
glBlendFuncSeparatei v1 v2 v3 v4 v5 = liftIO $ dyn54 ptr_glBlendFuncSeparatei v1 v2 v3 v4 v5

{-# NOINLINE ptr_glBlendFuncSeparatei #-}
ptr_glBlendFuncSeparatei :: FunPtr (GLuint -> GLenum -> GLenum -> GLenum -> GLenum -> IO ())
ptr_glBlendFuncSeparatei = unsafePerformIO $ getCommand "glBlendFuncSeparatei"

-- glBlendFuncSeparateiARB -----------------------------------------------------

-- | This command is an alias for 'glBlendFuncSeparatei'.
glBlendFuncSeparateiARB
  :: MonadIO m
  => GLuint -- ^ @buf@.
  -> GLenum -- ^ @srcRGB@.
  -> GLenum -- ^ @dstRGB@.
  -> GLenum -- ^ @srcAlpha@.
  -> GLenum -- ^ @dstAlpha@.
  -> m ()
glBlendFuncSeparateiARB v1 v2 v3 v4 v5 = liftIO $ dyn54 ptr_glBlendFuncSeparateiARB v1 v2 v3 v4 v5

{-# NOINLINE ptr_glBlendFuncSeparateiARB #-}
ptr_glBlendFuncSeparateiARB :: FunPtr (GLuint -> GLenum -> GLenum -> GLenum -> GLenum -> IO ())
ptr_glBlendFuncSeparateiARB = unsafePerformIO $ getCommand "glBlendFuncSeparateiARB"

-- glBlendFuncSeparateiEXT -----------------------------------------------------

-- | This command is an alias for 'glBlendFuncSeparatei'.
glBlendFuncSeparateiEXT
  :: MonadIO m
  => GLuint -- ^ @buf@.
  -> GLenum -- ^ @srcRGB@.
  -> GLenum -- ^ @dstRGB@.
  -> GLenum -- ^ @srcAlpha@.
  -> GLenum -- ^ @dstAlpha@.
  -> m ()
glBlendFuncSeparateiEXT v1 v2 v3 v4 v5 = liftIO $ dyn54 ptr_glBlendFuncSeparateiEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glBlendFuncSeparateiEXT #-}
ptr_glBlendFuncSeparateiEXT :: FunPtr (GLuint -> GLenum -> GLenum -> GLenum -> GLenum -> IO ())
ptr_glBlendFuncSeparateiEXT = unsafePerformIO $ getCommand "glBlendFuncSeparateiEXT"

-- glBlendFuncSeparateiOES -----------------------------------------------------

-- | This command is an alias for 'glBlendFuncSeparatei'.
glBlendFuncSeparateiOES
  :: MonadIO m
  => GLuint -- ^ @buf@.
  -> GLenum -- ^ @srcRGB@.
  -> GLenum -- ^ @dstRGB@.
  -> GLenum -- ^ @srcAlpha@.
  -> GLenum -- ^ @dstAlpha@.
  -> m ()
glBlendFuncSeparateiOES v1 v2 v3 v4 v5 = liftIO $ dyn54 ptr_glBlendFuncSeparateiOES v1 v2 v3 v4 v5

{-# NOINLINE ptr_glBlendFuncSeparateiOES #-}
ptr_glBlendFuncSeparateiOES :: FunPtr (GLuint -> GLenum -> GLenum -> GLenum -> GLenum -> IO ())
ptr_glBlendFuncSeparateiOES = unsafePerformIO $ getCommand "glBlendFuncSeparateiOES"

-- glBlendFunci ----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glBlendFunc.xhtml OpenGL 4.x>.
glBlendFunci
  :: MonadIO m
  => GLuint -- ^ @buf@.
  -> GLenum -- ^ @src@.
  -> GLenum -- ^ @dst@.
  -> m ()
glBlendFunci v1 v2 v3 = liftIO $ dyn52 ptr_glBlendFunci v1 v2 v3

{-# NOINLINE ptr_glBlendFunci #-}
ptr_glBlendFunci :: FunPtr (GLuint -> GLenum -> GLenum -> IO ())
ptr_glBlendFunci = unsafePerformIO $ getCommand "glBlendFunci"

-- glBlendFunciARB -------------------------------------------------------------

-- | This command is an alias for 'glBlendFunci'.
glBlendFunciARB
  :: MonadIO m
  => GLuint -- ^ @buf@.
  -> GLenum -- ^ @src@.
  -> GLenum -- ^ @dst@.
  -> m ()
glBlendFunciARB v1 v2 v3 = liftIO $ dyn52 ptr_glBlendFunciARB v1 v2 v3

{-# NOINLINE ptr_glBlendFunciARB #-}
ptr_glBlendFunciARB :: FunPtr (GLuint -> GLenum -> GLenum -> IO ())
ptr_glBlendFunciARB = unsafePerformIO $ getCommand "glBlendFunciARB"

-- glBlendFunciEXT -------------------------------------------------------------

-- | This command is an alias for 'glBlendFunci'.
glBlendFunciEXT
  :: MonadIO m
  => GLuint -- ^ @buf@.
  -> GLenum -- ^ @src@.
  -> GLenum -- ^ @dst@.
  -> m ()
glBlendFunciEXT v1 v2 v3 = liftIO $ dyn52 ptr_glBlendFunciEXT v1 v2 v3

{-# NOINLINE ptr_glBlendFunciEXT #-}
ptr_glBlendFunciEXT :: FunPtr (GLuint -> GLenum -> GLenum -> IO ())
ptr_glBlendFunciEXT = unsafePerformIO $ getCommand "glBlendFunciEXT"

-- glBlendFunciOES -------------------------------------------------------------

-- | This command is an alias for 'glBlendFunci'.
glBlendFunciOES
  :: MonadIO m
  => GLuint -- ^ @buf@.
  -> GLenum -- ^ @src@.
  -> GLenum -- ^ @dst@.
  -> m ()
glBlendFunciOES v1 v2 v3 = liftIO $ dyn52 ptr_glBlendFunciOES v1 v2 v3

{-# NOINLINE ptr_glBlendFunciOES #-}
ptr_glBlendFunciOES :: FunPtr (GLuint -> GLenum -> GLenum -> IO ())
ptr_glBlendFunciOES = unsafePerformIO $ getCommand "glBlendFunciOES"

-- glBlendParameteriNV ---------------------------------------------------------

glBlendParameteriNV
  :: MonadIO m
  => GLenum -- ^ @pname@.
  -> GLint -- ^ @value@.
  -> m ()
glBlendParameteriNV v1 v2 = liftIO $ dyn55 ptr_glBlendParameteriNV v1 v2

{-# NOINLINE ptr_glBlendParameteriNV #-}
ptr_glBlendParameteriNV :: FunPtr (GLenum -> GLint -> IO ())
ptr_glBlendParameteriNV = unsafePerformIO $ getCommand "glBlendParameteriNV"

-- glBlitFramebuffer -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glBlitFramebuffer.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glBlitFramebuffer.xhtml OpenGL 4.x>.
glBlitFramebuffer
  :: MonadIO m
  => GLint -- ^ @srcX0@.
  -> GLint -- ^ @srcY0@.
  -> GLint -- ^ @srcX1@.
  -> GLint -- ^ @srcY1@.
  -> GLint -- ^ @dstX0@.
  -> GLint -- ^ @dstY0@.
  -> GLint -- ^ @dstX1@.
  -> GLint -- ^ @dstY1@.
  -> GLbitfield -- ^ @mask@ of type [ClearBufferMask](Graphics-GL-Groups.html#ClearBufferMask).
  -> GLenum -- ^ @filter@.
  -> m ()
glBlitFramebuffer v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO $ dyn56 ptr_glBlitFramebuffer v1 v2 v3 v4 v5 v6 v7 v8 v9 v10

{-# NOINLINE ptr_glBlitFramebuffer #-}
ptr_glBlitFramebuffer :: FunPtr (GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLbitfield -> GLenum -> IO ())
ptr_glBlitFramebuffer = unsafePerformIO $ getCommand "glBlitFramebuffer"

-- glBlitFramebufferANGLE ------------------------------------------------------

glBlitFramebufferANGLE
  :: MonadIO m
  => GLint -- ^ @srcX0@.
  -> GLint -- ^ @srcY0@.
  -> GLint -- ^ @srcX1@.
  -> GLint -- ^ @srcY1@.
  -> GLint -- ^ @dstX0@.
  -> GLint -- ^ @dstY0@.
  -> GLint -- ^ @dstX1@.
  -> GLint -- ^ @dstY1@.
  -> GLbitfield -- ^ @mask@.
  -> GLenum -- ^ @filter@.
  -> m ()
glBlitFramebufferANGLE v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO $ dyn56 ptr_glBlitFramebufferANGLE v1 v2 v3 v4 v5 v6 v7 v8 v9 v10

{-# NOINLINE ptr_glBlitFramebufferANGLE #-}
ptr_glBlitFramebufferANGLE :: FunPtr (GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLbitfield -> GLenum -> IO ())
ptr_glBlitFramebufferANGLE = unsafePerformIO $ getCommand "glBlitFramebufferANGLE"

-- glBlitFramebufferEXT --------------------------------------------------------

-- | This command is an alias for 'glBlitFramebuffer'.
glBlitFramebufferEXT
  :: MonadIO m
  => GLint -- ^ @srcX0@.
  -> GLint -- ^ @srcY0@.
  -> GLint -- ^ @srcX1@.
  -> GLint -- ^ @srcY1@.
  -> GLint -- ^ @dstX0@.
  -> GLint -- ^ @dstY0@.
  -> GLint -- ^ @dstX1@.
  -> GLint -- ^ @dstY1@.
  -> GLbitfield -- ^ @mask@ of type [ClearBufferMask](Graphics-GL-Groups.html#ClearBufferMask).
  -> GLenum -- ^ @filter@.
  -> m ()
glBlitFramebufferEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO $ dyn56 ptr_glBlitFramebufferEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10

{-# NOINLINE ptr_glBlitFramebufferEXT #-}
ptr_glBlitFramebufferEXT :: FunPtr (GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLbitfield -> GLenum -> IO ())
ptr_glBlitFramebufferEXT = unsafePerformIO $ getCommand "glBlitFramebufferEXT"

-- glBlitFramebufferNV ---------------------------------------------------------

-- | This command is an alias for 'glBlitFramebuffer'.
glBlitFramebufferNV
  :: MonadIO m
  => GLint -- ^ @srcX0@.
  -> GLint -- ^ @srcY0@.
  -> GLint -- ^ @srcX1@.
  -> GLint -- ^ @srcY1@.
  -> GLint -- ^ @dstX0@.
  -> GLint -- ^ @dstY0@.
  -> GLint -- ^ @dstX1@.
  -> GLint -- ^ @dstY1@.
  -> GLbitfield -- ^ @mask@.
  -> GLenum -- ^ @filter@.
  -> m ()
glBlitFramebufferNV v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO $ dyn56 ptr_glBlitFramebufferNV v1 v2 v3 v4 v5 v6 v7 v8 v9 v10

{-# NOINLINE ptr_glBlitFramebufferNV #-}
ptr_glBlitFramebufferNV :: FunPtr (GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLbitfield -> GLenum -> IO ())
ptr_glBlitFramebufferNV = unsafePerformIO $ getCommand "glBlitFramebufferNV"

-- glBlitNamedFramebuffer ------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glBlitFramebuffer.xhtml OpenGL 4.x>.
glBlitNamedFramebuffer
  :: MonadIO m
  => GLuint -- ^ @readFramebuffer@.
  -> GLuint -- ^ @drawFramebuffer@.
  -> GLint -- ^ @srcX0@.
  -> GLint -- ^ @srcY0@.
  -> GLint -- ^ @srcX1@.
  -> GLint -- ^ @srcY1@.
  -> GLint -- ^ @dstX0@.
  -> GLint -- ^ @dstY0@.
  -> GLint -- ^ @dstX1@.
  -> GLint -- ^ @dstY1@.
  -> GLbitfield -- ^ @mask@.
  -> GLenum -- ^ @filter@.
  -> m ()
glBlitNamedFramebuffer v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 = liftIO $ dyn57 ptr_glBlitNamedFramebuffer v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12

{-# NOINLINE ptr_glBlitNamedFramebuffer #-}
ptr_glBlitNamedFramebuffer :: FunPtr (GLuint -> GLuint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLbitfield -> GLenum -> IO ())
ptr_glBlitNamedFramebuffer = unsafePerformIO $ getCommand "glBlitNamedFramebuffer"

-- glBufferAddressRangeNV ------------------------------------------------------

glBufferAddressRangeNV
  :: MonadIO m
  => GLenum -- ^ @pname@.
  -> GLuint -- ^ @index@.
  -> GLuint64EXT -- ^ @address@.
  -> GLsizeiptr -- ^ @length@ of type @BufferSize@.
  -> m ()
glBufferAddressRangeNV v1 v2 v3 v4 = liftIO $ dyn58 ptr_glBufferAddressRangeNV v1 v2 v3 v4

{-# NOINLINE ptr_glBufferAddressRangeNV #-}
ptr_glBufferAddressRangeNV :: FunPtr (GLenum -> GLuint -> GLuint64EXT -> GLsizeiptr -> IO ())
ptr_glBufferAddressRangeNV = unsafePerformIO $ getCommand "glBufferAddressRangeNV"

-- glBufferData ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glBufferData.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glBufferData.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glBufferData.xhtml OpenGL 4.x>.
glBufferData
  :: MonadIO m
  => GLenum -- ^ @target@ of type @BufferTargetARB@.
  -> GLsizeiptr -- ^ @size@ of type @BufferSize@.
  -> Ptr a -- ^ @data@ pointing to @size@ elements of type @a@.
  -> GLenum -- ^ @usage@ of type @BufferUsageARB@.
  -> m ()
glBufferData v1 v2 v3 v4 = liftIO $ dyn59 ptr_glBufferData v1 v2 v3 v4

{-# NOINLINE ptr_glBufferData #-}
ptr_glBufferData :: FunPtr (GLenum -> GLsizeiptr -> Ptr a -> GLenum -> IO ())
ptr_glBufferData = unsafePerformIO $ getCommand "glBufferData"

-- glBufferDataARB -------------------------------------------------------------

-- | This command is an alias for 'glBufferData'.
glBufferDataARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @BufferTargetARB@.
  -> GLsizeiptrARB -- ^ @size@ of type @BufferSizeARB@.
  -> Ptr a -- ^ @data@ pointing to @size@ elements of type @a@.
  -> GLenum -- ^ @usage@ of type @BufferUsageARB@.
  -> m ()
glBufferDataARB v1 v2 v3 v4 = liftIO $ dyn60 ptr_glBufferDataARB v1 v2 v3 v4

{-# NOINLINE ptr_glBufferDataARB #-}
ptr_glBufferDataARB :: FunPtr (GLenum -> GLsizeiptrARB -> Ptr a -> GLenum -> IO ())
ptr_glBufferDataARB = unsafePerformIO $ getCommand "glBufferDataARB"

-- glBufferPageCommitmentARB ---------------------------------------------------

glBufferPageCommitmentARB
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLintptr -- ^ @offset@.
  -> GLsizeiptr -- ^ @size@.
  -> GLboolean -- ^ @commit@.
  -> m ()
glBufferPageCommitmentARB v1 v2 v3 v4 = liftIO $ dyn61 ptr_glBufferPageCommitmentARB v1 v2 v3 v4

{-# NOINLINE ptr_glBufferPageCommitmentARB #-}
ptr_glBufferPageCommitmentARB :: FunPtr (GLenum -> GLintptr -> GLsizeiptr -> GLboolean -> IO ())
ptr_glBufferPageCommitmentARB = unsafePerformIO $ getCommand "glBufferPageCommitmentARB"

-- glBufferParameteriAPPLE -----------------------------------------------------

glBufferParameteriAPPLE
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @pname@.
  -> GLint -- ^ @param@.
  -> m ()
glBufferParameteriAPPLE v1 v2 v3 = liftIO $ dyn62 ptr_glBufferParameteriAPPLE v1 v2 v3

{-# NOINLINE ptr_glBufferParameteriAPPLE #-}
ptr_glBufferParameteriAPPLE :: FunPtr (GLenum -> GLenum -> GLint -> IO ())
ptr_glBufferParameteriAPPLE = unsafePerformIO $ getCommand "glBufferParameteriAPPLE"

-- glBufferStorage -------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glBufferStorage.xhtml OpenGL 4.x>.
glBufferStorage
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLsizeiptr -- ^ @size@.
  -> Ptr a -- ^ @data@ pointing to @size@ elements of type @a@.
  -> GLbitfield -- ^ @flags@.
  -> m ()
glBufferStorage v1 v2 v3 v4 = liftIO $ dyn63 ptr_glBufferStorage v1 v2 v3 v4

{-# NOINLINE ptr_glBufferStorage #-}
ptr_glBufferStorage :: FunPtr (GLenum -> GLsizeiptr -> Ptr a -> GLbitfield -> IO ())
ptr_glBufferStorage = unsafePerformIO $ getCommand "glBufferStorage"

