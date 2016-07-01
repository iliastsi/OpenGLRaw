--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F40
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

module Graphics.GL.Functions.F40 (
  glMultiDrawElementsIndirectAMD,
  glMultiDrawElementsIndirectBindlessCountNV,
  glMultiDrawElementsIndirectBindlessNV,
  glMultiDrawElementsIndirectCountARB,
  glMultiDrawElementsIndirectEXT,
  glMultiDrawRangeElementArrayAPPLE,
  glMultiModeDrawArraysIBM,
  glMultiModeDrawElementsIBM,
  glMultiTexBufferEXT,
  glMultiTexCoord1bOES,
  glMultiTexCoord1bvOES,
  glMultiTexCoord1d,
  glMultiTexCoord1dARB,
  glMultiTexCoord1dv,
  glMultiTexCoord1dvARB,
  glMultiTexCoord1f,
  glMultiTexCoord1fARB,
  glMultiTexCoord1fv,
  glMultiTexCoord1fvARB,
  glMultiTexCoord1hNV,
  glMultiTexCoord1hvNV,
  glMultiTexCoord1i,
  glMultiTexCoord1iARB,
  glMultiTexCoord1iv,
  glMultiTexCoord1ivARB,
  glMultiTexCoord1s,
  glMultiTexCoord1sARB,
  glMultiTexCoord1sv,
  glMultiTexCoord1svARB,
  glMultiTexCoord1xOES,
  glMultiTexCoord1xvOES,
  glMultiTexCoord2bOES,
  glMultiTexCoord2bvOES,
  glMultiTexCoord2d,
  glMultiTexCoord2dARB,
  glMultiTexCoord2dv,
  glMultiTexCoord2dvARB,
  glMultiTexCoord2f,
  glMultiTexCoord2fARB,
  glMultiTexCoord2fv
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

-- glMultiDrawElementsIndirectAMD ----------------------------------------------

-- | This command is an alias for 'glMultiDrawElementsIndirect'.
glMultiDrawElementsIndirectAMD
  :: MonadIO m
  => GLenum -- ^ @mode@.
  -> GLenum -- ^ @type@.
  -> Ptr a -- ^ @indirect@.
  -> GLsizei -- ^ @primcount@.
  -> GLsizei -- ^ @stride@.
  -> m ()
glMultiDrawElementsIndirectAMD v1 v2 v3 v4 v5 = liftIO $ dyn532 ptr_glMultiDrawElementsIndirectAMD v1 v2 v3 v4 v5

{-# NOINLINE ptr_glMultiDrawElementsIndirectAMD #-}
ptr_glMultiDrawElementsIndirectAMD :: FunPtr (GLenum -> GLenum -> Ptr a -> GLsizei -> GLsizei -> IO ())
ptr_glMultiDrawElementsIndirectAMD = unsafePerformIO $ getCommand "glMultiDrawElementsIndirectAMD"

-- glMultiDrawElementsIndirectBindlessCountNV ----------------------------------

glMultiDrawElementsIndirectBindlessCountNV
  :: MonadIO m
  => GLenum -- ^ @mode@.
  -> GLenum -- ^ @type@.
  -> Ptr a -- ^ @indirect@.
  -> GLsizei -- ^ @drawCount@.
  -> GLsizei -- ^ @maxDrawCount@.
  -> GLsizei -- ^ @stride@.
  -> GLint -- ^ @vertexBufferCount@.
  -> m ()
glMultiDrawElementsIndirectBindlessCountNV v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn533 ptr_glMultiDrawElementsIndirectBindlessCountNV v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glMultiDrawElementsIndirectBindlessCountNV #-}
ptr_glMultiDrawElementsIndirectBindlessCountNV :: FunPtr (GLenum -> GLenum -> Ptr a -> GLsizei -> GLsizei -> GLsizei -> GLint -> IO ())
ptr_glMultiDrawElementsIndirectBindlessCountNV = unsafePerformIO $ getCommand "glMultiDrawElementsIndirectBindlessCountNV"

-- glMultiDrawElementsIndirectBindlessNV ---------------------------------------

glMultiDrawElementsIndirectBindlessNV
  :: MonadIO m
  => GLenum -- ^ @mode@.
  -> GLenum -- ^ @type@.
  -> Ptr a -- ^ @indirect@.
  -> GLsizei -- ^ @drawCount@.
  -> GLsizei -- ^ @stride@.
  -> GLint -- ^ @vertexBufferCount@.
  -> m ()
glMultiDrawElementsIndirectBindlessNV v1 v2 v3 v4 v5 v6 = liftIO $ dyn534 ptr_glMultiDrawElementsIndirectBindlessNV v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glMultiDrawElementsIndirectBindlessNV #-}
ptr_glMultiDrawElementsIndirectBindlessNV :: FunPtr (GLenum -> GLenum -> Ptr a -> GLsizei -> GLsizei -> GLint -> IO ())
ptr_glMultiDrawElementsIndirectBindlessNV = unsafePerformIO $ getCommand "glMultiDrawElementsIndirectBindlessNV"

-- glMultiDrawElementsIndirectCountARB -----------------------------------------

glMultiDrawElementsIndirectCountARB
  :: MonadIO m
  => GLenum -- ^ @mode@.
  -> GLenum -- ^ @type@.
  -> GLintptr -- ^ @indirect@.
  -> GLintptr -- ^ @drawcount@.
  -> GLsizei -- ^ @maxdrawcount@.
  -> GLsizei -- ^ @stride@.
  -> m ()
glMultiDrawElementsIndirectCountARB v1 v2 v3 v4 v5 v6 = liftIO $ dyn535 ptr_glMultiDrawElementsIndirectCountARB v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glMultiDrawElementsIndirectCountARB #-}
ptr_glMultiDrawElementsIndirectCountARB :: FunPtr (GLenum -> GLenum -> GLintptr -> GLintptr -> GLsizei -> GLsizei -> IO ())
ptr_glMultiDrawElementsIndirectCountARB = unsafePerformIO $ getCommand "glMultiDrawElementsIndirectCountARB"

-- glMultiDrawElementsIndirectEXT ----------------------------------------------

-- | This command is an alias for 'glMultiDrawElementsIndirect'.
glMultiDrawElementsIndirectEXT
  :: MonadIO m
  => GLenum -- ^ @mode@.
  -> GLenum -- ^ @type@.
  -> Ptr a -- ^ @indirect@ pointing to @COMPSIZE(drawcount,stride)@ elements of type @a@.
  -> GLsizei -- ^ @drawcount@.
  -> GLsizei -- ^ @stride@.
  -> m ()
glMultiDrawElementsIndirectEXT v1 v2 v3 v4 v5 = liftIO $ dyn532 ptr_glMultiDrawElementsIndirectEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glMultiDrawElementsIndirectEXT #-}
ptr_glMultiDrawElementsIndirectEXT :: FunPtr (GLenum -> GLenum -> Ptr a -> GLsizei -> GLsizei -> IO ())
ptr_glMultiDrawElementsIndirectEXT = unsafePerformIO $ getCommand "glMultiDrawElementsIndirectEXT"

-- glMultiDrawRangeElementArrayAPPLE -------------------------------------------

glMultiDrawRangeElementArrayAPPLE
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> GLuint -- ^ @start@.
  -> GLuint -- ^ @end@.
  -> Ptr GLint -- ^ @first@ pointing to @primcount@ elements of type @GLint@.
  -> Ptr GLsizei -- ^ @count@ pointing to @primcount@ elements of type @GLsizei@.
  -> GLsizei -- ^ @primcount@.
  -> m ()
glMultiDrawRangeElementArrayAPPLE v1 v2 v3 v4 v5 v6 = liftIO $ dyn536 ptr_glMultiDrawRangeElementArrayAPPLE v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glMultiDrawRangeElementArrayAPPLE #-}
ptr_glMultiDrawRangeElementArrayAPPLE :: FunPtr (GLenum -> GLuint -> GLuint -> Ptr GLint -> Ptr GLsizei -> GLsizei -> IO ())
ptr_glMultiDrawRangeElementArrayAPPLE = unsafePerformIO $ getCommand "glMultiDrawRangeElementArrayAPPLE"

-- glMultiModeDrawArraysIBM ----------------------------------------------------

glMultiModeDrawArraysIBM
  :: MonadIO m
  => Ptr GLenum -- ^ @mode@ pointing to @COMPSIZE(primcount)@ elements of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> Ptr GLint -- ^ @first@ pointing to @COMPSIZE(primcount)@ elements of type @GLint@.
  -> Ptr GLsizei -- ^ @count@ pointing to @COMPSIZE(primcount)@ elements of type @GLsizei@.
  -> GLsizei -- ^ @primcount@.
  -> GLint -- ^ @modestride@.
  -> m ()
glMultiModeDrawArraysIBM v1 v2 v3 v4 v5 = liftIO $ dyn537 ptr_glMultiModeDrawArraysIBM v1 v2 v3 v4 v5

{-# NOINLINE ptr_glMultiModeDrawArraysIBM #-}
ptr_glMultiModeDrawArraysIBM :: FunPtr (Ptr GLenum -> Ptr GLint -> Ptr GLsizei -> GLsizei -> GLint -> IO ())
ptr_glMultiModeDrawArraysIBM = unsafePerformIO $ getCommand "glMultiModeDrawArraysIBM"

-- glMultiModeDrawElementsIBM --------------------------------------------------

glMultiModeDrawElementsIBM
  :: MonadIO m
  => Ptr GLenum -- ^ @mode@ pointing to @COMPSIZE(primcount)@ elements of type [PrimitiveType](Graphics-GL-Groups.html#PrimitiveType).
  -> Ptr GLsizei -- ^ @count@ pointing to @COMPSIZE(primcount)@ elements of type @GLsizei@.
  -> GLenum -- ^ @type@ of type @DrawElementsType@.
  -> Ptr (Ptr a) -- ^ @indices@ pointing to @COMPSIZE(primcount)@ elements of type @Ptr a@.
  -> GLsizei -- ^ @primcount@.
  -> GLint -- ^ @modestride@.
  -> m ()
glMultiModeDrawElementsIBM v1 v2 v3 v4 v5 v6 = liftIO $ dyn538 ptr_glMultiModeDrawElementsIBM v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glMultiModeDrawElementsIBM #-}
ptr_glMultiModeDrawElementsIBM :: FunPtr (Ptr GLenum -> Ptr GLsizei -> GLenum -> Ptr (Ptr a) -> GLsizei -> GLint -> IO ())
ptr_glMultiModeDrawElementsIBM = unsafePerformIO $ getCommand "glMultiModeDrawElementsIBM"

-- glMultiTexBufferEXT ---------------------------------------------------------

glMultiTexBufferEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type @TextureUnit@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @internalformat@ of type @TypeEnum@.
  -> GLuint -- ^ @buffer@.
  -> m ()
glMultiTexBufferEXT v1 v2 v3 v4 = liftIO $ dyn283 ptr_glMultiTexBufferEXT v1 v2 v3 v4

{-# NOINLINE ptr_glMultiTexBufferEXT #-}
ptr_glMultiTexBufferEXT :: FunPtr (GLenum -> GLenum -> GLenum -> GLuint -> IO ())
ptr_glMultiTexBufferEXT = unsafePerformIO $ getCommand "glMultiTexBufferEXT"

-- glMultiTexCoord1bOES --------------------------------------------------------

glMultiTexCoord1bOES
  :: MonadIO m
  => GLenum -- ^ @texture@.
  -> GLbyte -- ^ @s@.
  -> m ()
glMultiTexCoord1bOES v1 v2 = liftIO $ dyn539 ptr_glMultiTexCoord1bOES v1 v2

{-# NOINLINE ptr_glMultiTexCoord1bOES #-}
ptr_glMultiTexCoord1bOES :: FunPtr (GLenum -> GLbyte -> IO ())
ptr_glMultiTexCoord1bOES = unsafePerformIO $ getCommand "glMultiTexCoord1bOES"

-- glMultiTexCoord1bvOES -------------------------------------------------------

glMultiTexCoord1bvOES
  :: MonadIO m
  => GLenum -- ^ @texture@.
  -> Ptr GLbyte -- ^ @coords@ pointing to @1@ element of type @GLbyte@.
  -> m ()
glMultiTexCoord1bvOES v1 v2 = liftIO $ dyn540 ptr_glMultiTexCoord1bvOES v1 v2

{-# NOINLINE ptr_glMultiTexCoord1bvOES #-}
ptr_glMultiTexCoord1bvOES :: FunPtr (GLenum -> Ptr GLbyte -> IO ())
ptr_glMultiTexCoord1bvOES = unsafePerformIO $ getCommand "glMultiTexCoord1bvOES"

-- glMultiTexCoord1d -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glMultiTexCoord1dv'.
glMultiTexCoord1d
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> GLdouble -- ^ @s@ of type @CoordD@.
  -> m ()
glMultiTexCoord1d v1 v2 = liftIO $ dyn541 ptr_glMultiTexCoord1d v1 v2

{-# NOINLINE ptr_glMultiTexCoord1d #-}
ptr_glMultiTexCoord1d :: FunPtr (GLenum -> GLdouble -> IO ())
ptr_glMultiTexCoord1d = unsafePerformIO $ getCommand "glMultiTexCoord1d"

-- glMultiTexCoord1dARB --------------------------------------------------------

-- | The vector equivalent of this command is 'glMultiTexCoord1dv'. This command is an alias for 'glMultiTexCoord1d'.
glMultiTexCoord1dARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> GLdouble -- ^ @s@ of type @CoordD@.
  -> m ()
glMultiTexCoord1dARB v1 v2 = liftIO $ dyn541 ptr_glMultiTexCoord1dARB v1 v2

{-# NOINLINE ptr_glMultiTexCoord1dARB #-}
ptr_glMultiTexCoord1dARB :: FunPtr (GLenum -> GLdouble -> IO ())
ptr_glMultiTexCoord1dARB = unsafePerformIO $ getCommand "glMultiTexCoord1dARB"

-- glMultiTexCoord1dv ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>.
glMultiTexCoord1dv
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> Ptr GLdouble -- ^ @v@ pointing to @1@ element of type @CoordD@.
  -> m ()
glMultiTexCoord1dv v1 v2 = liftIO $ dyn93 ptr_glMultiTexCoord1dv v1 v2

{-# NOINLINE ptr_glMultiTexCoord1dv #-}
ptr_glMultiTexCoord1dv :: FunPtr (GLenum -> Ptr GLdouble -> IO ())
ptr_glMultiTexCoord1dv = unsafePerformIO $ getCommand "glMultiTexCoord1dv"

-- glMultiTexCoord1dvARB -------------------------------------------------------

-- | This command is an alias for 'glMultiTexCoord1dv'.
glMultiTexCoord1dvARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> Ptr GLdouble -- ^ @v@ pointing to @1@ element of type @CoordD@.
  -> m ()
glMultiTexCoord1dvARB v1 v2 = liftIO $ dyn93 ptr_glMultiTexCoord1dvARB v1 v2

{-# NOINLINE ptr_glMultiTexCoord1dvARB #-}
ptr_glMultiTexCoord1dvARB :: FunPtr (GLenum -> Ptr GLdouble -> IO ())
ptr_glMultiTexCoord1dvARB = unsafePerformIO $ getCommand "glMultiTexCoord1dvARB"

-- glMultiTexCoord1f -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glMultiTexCoord1fv'.
glMultiTexCoord1f
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> GLfloat -- ^ @s@ of type @CoordF@.
  -> m ()
glMultiTexCoord1f v1 v2 = liftIO $ dyn0 ptr_glMultiTexCoord1f v1 v2

{-# NOINLINE ptr_glMultiTexCoord1f #-}
ptr_glMultiTexCoord1f :: FunPtr (GLenum -> GLfloat -> IO ())
ptr_glMultiTexCoord1f = unsafePerformIO $ getCommand "glMultiTexCoord1f"

-- glMultiTexCoord1fARB --------------------------------------------------------

-- | The vector equivalent of this command is 'glMultiTexCoord1fv'. This command is an alias for 'glMultiTexCoord1f'.
glMultiTexCoord1fARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> GLfloat -- ^ @s@ of type @CoordF@.
  -> m ()
glMultiTexCoord1fARB v1 v2 = liftIO $ dyn0 ptr_glMultiTexCoord1fARB v1 v2

{-# NOINLINE ptr_glMultiTexCoord1fARB #-}
ptr_glMultiTexCoord1fARB :: FunPtr (GLenum -> GLfloat -> IO ())
ptr_glMultiTexCoord1fARB = unsafePerformIO $ getCommand "glMultiTexCoord1fARB"

-- glMultiTexCoord1fv ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>.
glMultiTexCoord1fv
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> Ptr GLfloat -- ^ @v@ pointing to @1@ element of type @CoordF@.
  -> m ()
glMultiTexCoord1fv v1 v2 = liftIO $ dyn94 ptr_glMultiTexCoord1fv v1 v2

{-# NOINLINE ptr_glMultiTexCoord1fv #-}
ptr_glMultiTexCoord1fv :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glMultiTexCoord1fv = unsafePerformIO $ getCommand "glMultiTexCoord1fv"

-- glMultiTexCoord1fvARB -------------------------------------------------------

-- | This command is an alias for 'glMultiTexCoord1fv'.
glMultiTexCoord1fvARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> Ptr GLfloat -- ^ @v@ pointing to @1@ element of type @CoordF@.
  -> m ()
glMultiTexCoord1fvARB v1 v2 = liftIO $ dyn94 ptr_glMultiTexCoord1fvARB v1 v2

{-# NOINLINE ptr_glMultiTexCoord1fvARB #-}
ptr_glMultiTexCoord1fvARB :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glMultiTexCoord1fvARB = unsafePerformIO $ getCommand "glMultiTexCoord1fvARB"

-- glMultiTexCoord1hNV ---------------------------------------------------------

-- | The vector equivalent of this command is 'glMultiTexCoord1hvNV'.
glMultiTexCoord1hNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> GLhalfNV -- ^ @s@ of type @Half16NV@.
  -> m ()
glMultiTexCoord1hNV v1 v2 = liftIO $ dyn542 ptr_glMultiTexCoord1hNV v1 v2

{-# NOINLINE ptr_glMultiTexCoord1hNV #-}
ptr_glMultiTexCoord1hNV :: FunPtr (GLenum -> GLhalfNV -> IO ())
ptr_glMultiTexCoord1hNV = unsafePerformIO $ getCommand "glMultiTexCoord1hNV"

-- glMultiTexCoord1hvNV --------------------------------------------------------

glMultiTexCoord1hvNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> Ptr GLhalfNV -- ^ @v@ pointing to @1@ element of type @Half16NV@.
  -> m ()
glMultiTexCoord1hvNV v1 v2 = liftIO $ dyn543 ptr_glMultiTexCoord1hvNV v1 v2

{-# NOINLINE ptr_glMultiTexCoord1hvNV #-}
ptr_glMultiTexCoord1hvNV :: FunPtr (GLenum -> Ptr GLhalfNV -> IO ())
ptr_glMultiTexCoord1hvNV = unsafePerformIO $ getCommand "glMultiTexCoord1hvNV"

-- glMultiTexCoord1i -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glMultiTexCoord1iv'.
glMultiTexCoord1i
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> GLint -- ^ @s@ of type @CoordI@.
  -> m ()
glMultiTexCoord1i v1 v2 = liftIO $ dyn55 ptr_glMultiTexCoord1i v1 v2

{-# NOINLINE ptr_glMultiTexCoord1i #-}
ptr_glMultiTexCoord1i :: FunPtr (GLenum -> GLint -> IO ())
ptr_glMultiTexCoord1i = unsafePerformIO $ getCommand "glMultiTexCoord1i"

-- glMultiTexCoord1iARB --------------------------------------------------------

-- | The vector equivalent of this command is 'glMultiTexCoord1iv'. This command is an alias for 'glMultiTexCoord1i'.
glMultiTexCoord1iARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> GLint -- ^ @s@ of type @CoordI@.
  -> m ()
glMultiTexCoord1iARB v1 v2 = liftIO $ dyn55 ptr_glMultiTexCoord1iARB v1 v2

{-# NOINLINE ptr_glMultiTexCoord1iARB #-}
ptr_glMultiTexCoord1iARB :: FunPtr (GLenum -> GLint -> IO ())
ptr_glMultiTexCoord1iARB = unsafePerformIO $ getCommand "glMultiTexCoord1iARB"

-- glMultiTexCoord1iv ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>.
glMultiTexCoord1iv
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> Ptr GLint -- ^ @v@ pointing to @1@ element of type @CoordI@.
  -> m ()
glMultiTexCoord1iv v1 v2 = liftIO $ dyn136 ptr_glMultiTexCoord1iv v1 v2

{-# NOINLINE ptr_glMultiTexCoord1iv #-}
ptr_glMultiTexCoord1iv :: FunPtr (GLenum -> Ptr GLint -> IO ())
ptr_glMultiTexCoord1iv = unsafePerformIO $ getCommand "glMultiTexCoord1iv"

-- glMultiTexCoord1ivARB -------------------------------------------------------

-- | This command is an alias for 'glMultiTexCoord1iv'.
glMultiTexCoord1ivARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> Ptr GLint -- ^ @v@ pointing to @1@ element of type @CoordI@.
  -> m ()
glMultiTexCoord1ivARB v1 v2 = liftIO $ dyn136 ptr_glMultiTexCoord1ivARB v1 v2

{-# NOINLINE ptr_glMultiTexCoord1ivARB #-}
ptr_glMultiTexCoord1ivARB :: FunPtr (GLenum -> Ptr GLint -> IO ())
ptr_glMultiTexCoord1ivARB = unsafePerformIO $ getCommand "glMultiTexCoord1ivARB"

-- glMultiTexCoord1s -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glMultiTexCoord1sv'.
glMultiTexCoord1s
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> GLshort -- ^ @s@ of type @CoordS@.
  -> m ()
glMultiTexCoord1s v1 v2 = liftIO $ dyn544 ptr_glMultiTexCoord1s v1 v2

{-# NOINLINE ptr_glMultiTexCoord1s #-}
ptr_glMultiTexCoord1s :: FunPtr (GLenum -> GLshort -> IO ())
ptr_glMultiTexCoord1s = unsafePerformIO $ getCommand "glMultiTexCoord1s"

-- glMultiTexCoord1sARB --------------------------------------------------------

-- | The vector equivalent of this command is 'glMultiTexCoord1sv'. This command is an alias for 'glMultiTexCoord1s'.
glMultiTexCoord1sARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> GLshort -- ^ @s@ of type @CoordS@.
  -> m ()
glMultiTexCoord1sARB v1 v2 = liftIO $ dyn544 ptr_glMultiTexCoord1sARB v1 v2

{-# NOINLINE ptr_glMultiTexCoord1sARB #-}
ptr_glMultiTexCoord1sARB :: FunPtr (GLenum -> GLshort -> IO ())
ptr_glMultiTexCoord1sARB = unsafePerformIO $ getCommand "glMultiTexCoord1sARB"

-- glMultiTexCoord1sv ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>.
glMultiTexCoord1sv
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> Ptr GLshort -- ^ @v@ pointing to @1@ element of type @CoordS@.
  -> m ()
glMultiTexCoord1sv v1 v2 = liftIO $ dyn545 ptr_glMultiTexCoord1sv v1 v2

{-# NOINLINE ptr_glMultiTexCoord1sv #-}
ptr_glMultiTexCoord1sv :: FunPtr (GLenum -> Ptr GLshort -> IO ())
ptr_glMultiTexCoord1sv = unsafePerformIO $ getCommand "glMultiTexCoord1sv"

-- glMultiTexCoord1svARB -------------------------------------------------------

-- | This command is an alias for 'glMultiTexCoord1sv'.
glMultiTexCoord1svARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> Ptr GLshort -- ^ @v@ pointing to @1@ element of type @CoordS@.
  -> m ()
glMultiTexCoord1svARB v1 v2 = liftIO $ dyn545 ptr_glMultiTexCoord1svARB v1 v2

{-# NOINLINE ptr_glMultiTexCoord1svARB #-}
ptr_glMultiTexCoord1svARB :: FunPtr (GLenum -> Ptr GLshort -> IO ())
ptr_glMultiTexCoord1svARB = unsafePerformIO $ getCommand "glMultiTexCoord1svARB"

-- glMultiTexCoord1xOES --------------------------------------------------------

glMultiTexCoord1xOES
  :: MonadIO m
  => GLenum -- ^ @texture@.
  -> GLfixed -- ^ @s@.
  -> m ()
glMultiTexCoord1xOES v1 v2 = liftIO $ dyn1 ptr_glMultiTexCoord1xOES v1 v2

{-# NOINLINE ptr_glMultiTexCoord1xOES #-}
ptr_glMultiTexCoord1xOES :: FunPtr (GLenum -> GLfixed -> IO ())
ptr_glMultiTexCoord1xOES = unsafePerformIO $ getCommand "glMultiTexCoord1xOES"

-- glMultiTexCoord1xvOES -------------------------------------------------------

glMultiTexCoord1xvOES
  :: MonadIO m
  => GLenum -- ^ @texture@.
  -> Ptr GLfixed -- ^ @coords@ pointing to @1@ element of type @GLfixed@.
  -> m ()
glMultiTexCoord1xvOES v1 v2 = liftIO $ dyn95 ptr_glMultiTexCoord1xvOES v1 v2

{-# NOINLINE ptr_glMultiTexCoord1xvOES #-}
ptr_glMultiTexCoord1xvOES :: FunPtr (GLenum -> Ptr GLfixed -> IO ())
ptr_glMultiTexCoord1xvOES = unsafePerformIO $ getCommand "glMultiTexCoord1xvOES"

-- glMultiTexCoord2bOES --------------------------------------------------------

glMultiTexCoord2bOES
  :: MonadIO m
  => GLenum -- ^ @texture@.
  -> GLbyte -- ^ @s@.
  -> GLbyte -- ^ @t@.
  -> m ()
glMultiTexCoord2bOES v1 v2 v3 = liftIO $ dyn546 ptr_glMultiTexCoord2bOES v1 v2 v3

{-# NOINLINE ptr_glMultiTexCoord2bOES #-}
ptr_glMultiTexCoord2bOES :: FunPtr (GLenum -> GLbyte -> GLbyte -> IO ())
ptr_glMultiTexCoord2bOES = unsafePerformIO $ getCommand "glMultiTexCoord2bOES"

-- glMultiTexCoord2bvOES -------------------------------------------------------

glMultiTexCoord2bvOES
  :: MonadIO m
  => GLenum -- ^ @texture@.
  -> Ptr GLbyte -- ^ @coords@ pointing to @2@ elements of type @GLbyte@.
  -> m ()
glMultiTexCoord2bvOES v1 v2 = liftIO $ dyn540 ptr_glMultiTexCoord2bvOES v1 v2

{-# NOINLINE ptr_glMultiTexCoord2bvOES #-}
ptr_glMultiTexCoord2bvOES :: FunPtr (GLenum -> Ptr GLbyte -> IO ())
ptr_glMultiTexCoord2bvOES = unsafePerformIO $ getCommand "glMultiTexCoord2bvOES"

-- glMultiTexCoord2d -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glMultiTexCoord2dv'.
glMultiTexCoord2d
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> GLdouble -- ^ @s@ of type @CoordD@.
  -> GLdouble -- ^ @t@ of type @CoordD@.
  -> m ()
glMultiTexCoord2d v1 v2 v3 = liftIO $ dyn547 ptr_glMultiTexCoord2d v1 v2 v3

{-# NOINLINE ptr_glMultiTexCoord2d #-}
ptr_glMultiTexCoord2d :: FunPtr (GLenum -> GLdouble -> GLdouble -> IO ())
ptr_glMultiTexCoord2d = unsafePerformIO $ getCommand "glMultiTexCoord2d"

-- glMultiTexCoord2dARB --------------------------------------------------------

-- | The vector equivalent of this command is 'glMultiTexCoord2dv'. This command is an alias for 'glMultiTexCoord2d'.
glMultiTexCoord2dARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> GLdouble -- ^ @s@ of type @CoordD@.
  -> GLdouble -- ^ @t@ of type @CoordD@.
  -> m ()
glMultiTexCoord2dARB v1 v2 v3 = liftIO $ dyn547 ptr_glMultiTexCoord2dARB v1 v2 v3

{-# NOINLINE ptr_glMultiTexCoord2dARB #-}
ptr_glMultiTexCoord2dARB :: FunPtr (GLenum -> GLdouble -> GLdouble -> IO ())
ptr_glMultiTexCoord2dARB = unsafePerformIO $ getCommand "glMultiTexCoord2dARB"

-- glMultiTexCoord2dv ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>.
glMultiTexCoord2dv
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> Ptr GLdouble -- ^ @v@ pointing to @2@ elements of type @CoordD@.
  -> m ()
glMultiTexCoord2dv v1 v2 = liftIO $ dyn93 ptr_glMultiTexCoord2dv v1 v2

{-# NOINLINE ptr_glMultiTexCoord2dv #-}
ptr_glMultiTexCoord2dv :: FunPtr (GLenum -> Ptr GLdouble -> IO ())
ptr_glMultiTexCoord2dv = unsafePerformIO $ getCommand "glMultiTexCoord2dv"

-- glMultiTexCoord2dvARB -------------------------------------------------------

-- | This command is an alias for 'glMultiTexCoord2dv'.
glMultiTexCoord2dvARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> Ptr GLdouble -- ^ @v@ pointing to @2@ elements of type @CoordD@.
  -> m ()
glMultiTexCoord2dvARB v1 v2 = liftIO $ dyn93 ptr_glMultiTexCoord2dvARB v1 v2

{-# NOINLINE ptr_glMultiTexCoord2dvARB #-}
ptr_glMultiTexCoord2dvARB :: FunPtr (GLenum -> Ptr GLdouble -> IO ())
ptr_glMultiTexCoord2dvARB = unsafePerformIO $ getCommand "glMultiTexCoord2dvARB"

-- glMultiTexCoord2f -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glMultiTexCoord2fv'.
glMultiTexCoord2f
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> GLfloat -- ^ @s@ of type @CoordF@.
  -> GLfloat -- ^ @t@ of type @CoordF@.
  -> m ()
glMultiTexCoord2f v1 v2 v3 = liftIO $ dyn548 ptr_glMultiTexCoord2f v1 v2 v3

{-# NOINLINE ptr_glMultiTexCoord2f #-}
ptr_glMultiTexCoord2f :: FunPtr (GLenum -> GLfloat -> GLfloat -> IO ())
ptr_glMultiTexCoord2f = unsafePerformIO $ getCommand "glMultiTexCoord2f"

-- glMultiTexCoord2fARB --------------------------------------------------------

-- | The vector equivalent of this command is 'glMultiTexCoord2fv'. This command is an alias for 'glMultiTexCoord2f'.
glMultiTexCoord2fARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> GLfloat -- ^ @s@ of type @CoordF@.
  -> GLfloat -- ^ @t@ of type @CoordF@.
  -> m ()
glMultiTexCoord2fARB v1 v2 v3 = liftIO $ dyn548 ptr_glMultiTexCoord2fARB v1 v2 v3

{-# NOINLINE ptr_glMultiTexCoord2fARB #-}
ptr_glMultiTexCoord2fARB :: FunPtr (GLenum -> GLfloat -> GLfloat -> IO ())
ptr_glMultiTexCoord2fARB = unsafePerformIO $ getCommand "glMultiTexCoord2fARB"

-- glMultiTexCoord2fv ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glMultiTexCoord.xml OpenGL 2.x>.
glMultiTexCoord2fv
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TextureUnit@.
  -> Ptr GLfloat -- ^ @v@ pointing to @2@ elements of type @CoordF@.
  -> m ()
glMultiTexCoord2fv v1 v2 = liftIO $ dyn94 ptr_glMultiTexCoord2fv v1 v2

{-# NOINLINE ptr_glMultiTexCoord2fv #-}
ptr_glMultiTexCoord2fv :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glMultiTexCoord2fv = unsafePerformIO $ getCommand "glMultiTexCoord2fv"

