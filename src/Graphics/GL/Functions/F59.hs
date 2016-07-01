--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F59
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

module Graphics.GL.Functions.F59 (
  glTexBuffer,
  glTexBufferARB,
  glTexBufferEXT,
  glTexBufferOES,
  glTexBufferRange,
  glTexBufferRangeEXT,
  glTexBufferRangeOES,
  glTexBumpParameterfvATI,
  glTexBumpParameterivATI,
  glTexCoord1bOES,
  glTexCoord1bvOES,
  glTexCoord1d,
  glTexCoord1dv,
  glTexCoord1f,
  glTexCoord1fv,
  glTexCoord1hNV,
  glTexCoord1hvNV,
  glTexCoord1i,
  glTexCoord1iv,
  glTexCoord1s,
  glTexCoord1sv,
  glTexCoord1xOES,
  glTexCoord1xvOES,
  glTexCoord2bOES,
  glTexCoord2bvOES,
  glTexCoord2d,
  glTexCoord2dv,
  glTexCoord2f,
  glTexCoord2fColor3fVertex3fSUN,
  glTexCoord2fColor3fVertex3fvSUN,
  glTexCoord2fColor4fNormal3fVertex3fSUN,
  glTexCoord2fColor4fNormal3fVertex3fvSUN,
  glTexCoord2fColor4ubVertex3fSUN,
  glTexCoord2fColor4ubVertex3fvSUN,
  glTexCoord2fNormal3fVertex3fSUN,
  glTexCoord2fNormal3fVertex3fvSUN,
  glTexCoord2fVertex3fSUN,
  glTexCoord2fVertex3fvSUN,
  glTexCoord2fv,
  glTexCoord2hNV
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

-- glTexBuffer -----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glTexBuffer.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glTexBuffer.xhtml OpenGL 4.x>.
glTexBuffer
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @internalformat@.
  -> GLuint -- ^ @buffer@.
  -> m ()
glTexBuffer v1 v2 v3 = liftIO $ dyn29 ptr_glTexBuffer v1 v2 v3

{-# NOINLINE ptr_glTexBuffer #-}
ptr_glTexBuffer :: FunPtr (GLenum -> GLenum -> GLuint -> IO ())
ptr_glTexBuffer = unsafePerformIO $ getCommand "glTexBuffer"

-- glTexBufferARB --------------------------------------------------------------

-- | This command is an alias for 'glTexBuffer'.
glTexBufferARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @internalformat@.
  -> GLuint -- ^ @buffer@.
  -> m ()
glTexBufferARB v1 v2 v3 = liftIO $ dyn29 ptr_glTexBufferARB v1 v2 v3

{-# NOINLINE ptr_glTexBufferARB #-}
ptr_glTexBufferARB :: FunPtr (GLenum -> GLenum -> GLuint -> IO ())
ptr_glTexBufferARB = unsafePerformIO $ getCommand "glTexBufferARB"

-- glTexBufferEXT --------------------------------------------------------------

-- | This command is an alias for 'glTexBuffer'.
glTexBufferEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @internalformat@.
  -> GLuint -- ^ @buffer@.
  -> m ()
glTexBufferEXT v1 v2 v3 = liftIO $ dyn29 ptr_glTexBufferEXT v1 v2 v3

{-# NOINLINE ptr_glTexBufferEXT #-}
ptr_glTexBufferEXT :: FunPtr (GLenum -> GLenum -> GLuint -> IO ())
ptr_glTexBufferEXT = unsafePerformIO $ getCommand "glTexBufferEXT"

-- glTexBufferOES --------------------------------------------------------------

-- | This command is an alias for 'glTexBuffer'.
glTexBufferOES
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLenum -- ^ @internalformat@.
  -> GLuint -- ^ @buffer@.
  -> m ()
glTexBufferOES v1 v2 v3 = liftIO $ dyn29 ptr_glTexBufferOES v1 v2 v3

{-# NOINLINE ptr_glTexBufferOES #-}
ptr_glTexBufferOES :: FunPtr (GLenum -> GLenum -> GLuint -> IO ())
ptr_glTexBufferOES = unsafePerformIO $ getCommand "glTexBufferOES"

-- glTexBufferRange ------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glTexBufferRange.xhtml OpenGL 4.x>.
glTexBufferRange
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @internalformat@.
  -> GLuint -- ^ @buffer@.
  -> GLintptr -- ^ @offset@ of type @BufferOffset@.
  -> GLsizeiptr -- ^ @size@ of type @BufferSize@.
  -> m ()
glTexBufferRange v1 v2 v3 v4 v5 = liftIO $ dyn721 ptr_glTexBufferRange v1 v2 v3 v4 v5

{-# NOINLINE ptr_glTexBufferRange #-}
ptr_glTexBufferRange :: FunPtr (GLenum -> GLenum -> GLuint -> GLintptr -> GLsizeiptr -> IO ())
ptr_glTexBufferRange = unsafePerformIO $ getCommand "glTexBufferRange"

-- glTexBufferRangeEXT ---------------------------------------------------------

-- | This command is an alias for 'glTexBufferRange'.
glTexBufferRangeEXT
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @internalformat@.
  -> GLuint -- ^ @buffer@.
  -> GLintptr -- ^ @offset@ of type @BufferOffset@.
  -> GLsizeiptr -- ^ @size@ of type @BufferSize@.
  -> m ()
glTexBufferRangeEXT v1 v2 v3 v4 v5 = liftIO $ dyn721 ptr_glTexBufferRangeEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glTexBufferRangeEXT #-}
ptr_glTexBufferRangeEXT :: FunPtr (GLenum -> GLenum -> GLuint -> GLintptr -> GLsizeiptr -> IO ())
ptr_glTexBufferRangeEXT = unsafePerformIO $ getCommand "glTexBufferRangeEXT"

-- glTexBufferRangeOES ---------------------------------------------------------

-- | This command is an alias for 'glTexBufferRange'.
glTexBufferRangeOES
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @internalformat@.
  -> GLuint -- ^ @buffer@.
  -> GLintptr -- ^ @offset@ of type @BufferOffset@.
  -> GLsizeiptr -- ^ @size@ of type @BufferSize@.
  -> m ()
glTexBufferRangeOES v1 v2 v3 v4 v5 = liftIO $ dyn721 ptr_glTexBufferRangeOES v1 v2 v3 v4 v5

{-# NOINLINE ptr_glTexBufferRangeOES #-}
ptr_glTexBufferRangeOES :: FunPtr (GLenum -> GLenum -> GLuint -> GLintptr -> GLsizeiptr -> IO ())
ptr_glTexBufferRangeOES = unsafePerformIO $ getCommand "glTexBufferRangeOES"

-- glTexBumpParameterfvATI -----------------------------------------------------

glTexBumpParameterfvATI
  :: MonadIO m
  => GLenum -- ^ @pname@ of type @TexBumpParameterATI@.
  -> Ptr GLfloat -- ^ @param@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glTexBumpParameterfvATI v1 v2 = liftIO $ dyn94 ptr_glTexBumpParameterfvATI v1 v2

{-# NOINLINE ptr_glTexBumpParameterfvATI #-}
ptr_glTexBumpParameterfvATI :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glTexBumpParameterfvATI = unsafePerformIO $ getCommand "glTexBumpParameterfvATI"

-- glTexBumpParameterivATI -----------------------------------------------------

glTexBumpParameterivATI
  :: MonadIO m
  => GLenum -- ^ @pname@ of type @TexBumpParameterATI@.
  -> Ptr GLint -- ^ @param@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glTexBumpParameterivATI v1 v2 = liftIO $ dyn136 ptr_glTexBumpParameterivATI v1 v2

{-# NOINLINE ptr_glTexBumpParameterivATI #-}
ptr_glTexBumpParameterivATI :: FunPtr (GLenum -> Ptr GLint -> IO ())
ptr_glTexBumpParameterivATI = unsafePerformIO $ getCommand "glTexBumpParameterivATI"

-- glTexCoord1bOES -------------------------------------------------------------

glTexCoord1bOES
  :: MonadIO m
  => GLbyte -- ^ @s@.
  -> m ()
glTexCoord1bOES v1 = liftIO $ dyn462 ptr_glTexCoord1bOES v1

{-# NOINLINE ptr_glTexCoord1bOES #-}
ptr_glTexCoord1bOES :: FunPtr (GLbyte -> IO ())
ptr_glTexCoord1bOES = unsafePerformIO $ getCommand "glTexCoord1bOES"

-- glTexCoord1bvOES ------------------------------------------------------------

glTexCoord1bvOES
  :: MonadIO m
  => Ptr GLbyte -- ^ @coords@ pointing to @1@ element of type @GLbyte@.
  -> m ()
glTexCoord1bvOES v1 = liftIO $ dyn37 ptr_glTexCoord1bvOES v1

{-# NOINLINE ptr_glTexCoord1bvOES #-}
ptr_glTexCoord1bvOES :: FunPtr (Ptr GLbyte -> IO ())
ptr_glTexCoord1bvOES = unsafePerformIO $ getCommand "glTexCoord1bvOES"

-- glTexCoord1d ----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glTexCoord1dv'.
glTexCoord1d
  :: MonadIO m
  => GLdouble -- ^ @s@ of type @CoordD@.
  -> m ()
glTexCoord1d v1 = liftIO $ dyn78 ptr_glTexCoord1d v1

{-# NOINLINE ptr_glTexCoord1d #-}
ptr_glTexCoord1d :: FunPtr (GLdouble -> IO ())
ptr_glTexCoord1d = unsafePerformIO $ getCommand "glTexCoord1d"

-- glTexCoord1dv ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml OpenGL 2.x>.
glTexCoord1dv
  :: MonadIO m
  => Ptr GLdouble -- ^ @v@ pointing to @1@ element of type @CoordD@.
  -> m ()
glTexCoord1dv v1 = liftIO $ dyn39 ptr_glTexCoord1dv v1

{-# NOINLINE ptr_glTexCoord1dv #-}
ptr_glTexCoord1dv :: FunPtr (Ptr GLdouble -> IO ())
ptr_glTexCoord1dv = unsafePerformIO $ getCommand "glTexCoord1dv"

-- glTexCoord1f ----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glTexCoord1fv'.
glTexCoord1f
  :: MonadIO m
  => GLfloat -- ^ @s@ of type @CoordF@.
  -> m ()
glTexCoord1f v1 = liftIO $ dyn79 ptr_glTexCoord1f v1

{-# NOINLINE ptr_glTexCoord1f #-}
ptr_glTexCoord1f :: FunPtr (GLfloat -> IO ())
ptr_glTexCoord1f = unsafePerformIO $ getCommand "glTexCoord1f"

-- glTexCoord1fv ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml OpenGL 2.x>.
glTexCoord1fv
  :: MonadIO m
  => Ptr GLfloat -- ^ @v@ pointing to @1@ element of type @CoordF@.
  -> m ()
glTexCoord1fv v1 = liftIO $ dyn41 ptr_glTexCoord1fv v1

{-# NOINLINE ptr_glTexCoord1fv #-}
ptr_glTexCoord1fv :: FunPtr (Ptr GLfloat -> IO ())
ptr_glTexCoord1fv = unsafePerformIO $ getCommand "glTexCoord1fv"

-- glTexCoord1hNV --------------------------------------------------------------

-- | The vector equivalent of this command is 'glTexCoord1hvNV'.
glTexCoord1hNV
  :: MonadIO m
  => GLhalfNV -- ^ @s@ of type @Half16NV@.
  -> m ()
glTexCoord1hNV v1 = liftIO $ dyn281 ptr_glTexCoord1hNV v1

{-# NOINLINE ptr_glTexCoord1hNV #-}
ptr_glTexCoord1hNV :: FunPtr (GLhalfNV -> IO ())
ptr_glTexCoord1hNV = unsafePerformIO $ getCommand "glTexCoord1hNV"

-- glTexCoord1hvNV -------------------------------------------------------------

glTexCoord1hvNV
  :: MonadIO m
  => Ptr GLhalfNV -- ^ @v@ pointing to @1@ element of type @Half16NV@.
  -> m ()
glTexCoord1hvNV v1 = liftIO $ dyn99 ptr_glTexCoord1hvNV v1

{-# NOINLINE ptr_glTexCoord1hvNV #-}
ptr_glTexCoord1hvNV :: FunPtr (Ptr GLhalfNV -> IO ())
ptr_glTexCoord1hvNV = unsafePerformIO $ getCommand "glTexCoord1hvNV"

-- glTexCoord1i ----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glTexCoord1iv'.
glTexCoord1i
  :: MonadIO m
  => GLint -- ^ @s@ of type @CoordI@.
  -> m ()
glTexCoord1i v1 = liftIO $ dyn12 ptr_glTexCoord1i v1

{-# NOINLINE ptr_glTexCoord1i #-}
ptr_glTexCoord1i :: FunPtr (GLint -> IO ())
ptr_glTexCoord1i = unsafePerformIO $ getCommand "glTexCoord1i"

-- glTexCoord1iv ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml OpenGL 2.x>.
glTexCoord1iv
  :: MonadIO m
  => Ptr GLint -- ^ @v@ pointing to @1@ element of type @CoordI@.
  -> m ()
glTexCoord1iv v1 = liftIO $ dyn43 ptr_glTexCoord1iv v1

{-# NOINLINE ptr_glTexCoord1iv #-}
ptr_glTexCoord1iv :: FunPtr (Ptr GLint -> IO ())
ptr_glTexCoord1iv = unsafePerformIO $ getCommand "glTexCoord1iv"

-- glTexCoord1s ----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glTexCoord1sv'.
glTexCoord1s
  :: MonadIO m
  => GLshort -- ^ @s@ of type @CoordS@.
  -> m ()
glTexCoord1s v1 = liftIO $ dyn463 ptr_glTexCoord1s v1

{-# NOINLINE ptr_glTexCoord1s #-}
ptr_glTexCoord1s :: FunPtr (GLshort -> IO ())
ptr_glTexCoord1s = unsafePerformIO $ getCommand "glTexCoord1s"

-- glTexCoord1sv ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml OpenGL 2.x>.
glTexCoord1sv
  :: MonadIO m
  => Ptr GLshort -- ^ @v@ pointing to @1@ element of type @CoordS@.
  -> m ()
glTexCoord1sv v1 = liftIO $ dyn45 ptr_glTexCoord1sv v1

{-# NOINLINE ptr_glTexCoord1sv #-}
ptr_glTexCoord1sv :: FunPtr (Ptr GLshort -> IO ())
ptr_glTexCoord1sv = unsafePerformIO $ getCommand "glTexCoord1sv"

-- glTexCoord1xOES -------------------------------------------------------------

glTexCoord1xOES
  :: MonadIO m
  => GLfixed -- ^ @s@.
  -> m ()
glTexCoord1xOES v1 = liftIO $ dyn81 ptr_glTexCoord1xOES v1

{-# NOINLINE ptr_glTexCoord1xOES #-}
ptr_glTexCoord1xOES :: FunPtr (GLfixed -> IO ())
ptr_glTexCoord1xOES = unsafePerformIO $ getCommand "glTexCoord1xOES"

-- glTexCoord1xvOES ------------------------------------------------------------

glTexCoord1xvOES
  :: MonadIO m
  => Ptr GLfixed -- ^ @coords@ pointing to @1@ element of type @GLfixed@.
  -> m ()
glTexCoord1xvOES v1 = liftIO $ dyn107 ptr_glTexCoord1xvOES v1

{-# NOINLINE ptr_glTexCoord1xvOES #-}
ptr_glTexCoord1xvOES :: FunPtr (Ptr GLfixed -> IO ())
ptr_glTexCoord1xvOES = unsafePerformIO $ getCommand "glTexCoord1xvOES"

-- glTexCoord2bOES -------------------------------------------------------------

glTexCoord2bOES
  :: MonadIO m
  => GLbyte -- ^ @s@.
  -> GLbyte -- ^ @t@.
  -> m ()
glTexCoord2bOES v1 v2 = liftIO $ dyn722 ptr_glTexCoord2bOES v1 v2

{-# NOINLINE ptr_glTexCoord2bOES #-}
ptr_glTexCoord2bOES :: FunPtr (GLbyte -> GLbyte -> IO ())
ptr_glTexCoord2bOES = unsafePerformIO $ getCommand "glTexCoord2bOES"

-- glTexCoord2bvOES ------------------------------------------------------------

glTexCoord2bvOES
  :: MonadIO m
  => Ptr GLbyte -- ^ @coords@ pointing to @2@ elements of type @GLbyte@.
  -> m ()
glTexCoord2bvOES v1 = liftIO $ dyn37 ptr_glTexCoord2bvOES v1

{-# NOINLINE ptr_glTexCoord2bvOES #-}
ptr_glTexCoord2bvOES :: FunPtr (Ptr GLbyte -> IO ())
ptr_glTexCoord2bvOES = unsafePerformIO $ getCommand "glTexCoord2bvOES"

-- glTexCoord2d ----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glTexCoord2dv'.
glTexCoord2d
  :: MonadIO m
  => GLdouble -- ^ @s@ of type @CoordD@.
  -> GLdouble -- ^ @t@ of type @CoordD@.
  -> m ()
glTexCoord2d v1 v2 = liftIO $ dyn217 ptr_glTexCoord2d v1 v2

{-# NOINLINE ptr_glTexCoord2d #-}
ptr_glTexCoord2d :: FunPtr (GLdouble -> GLdouble -> IO ())
ptr_glTexCoord2d = unsafePerformIO $ getCommand "glTexCoord2d"

-- glTexCoord2dv ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml OpenGL 2.x>.
glTexCoord2dv
  :: MonadIO m
  => Ptr GLdouble -- ^ @v@ pointing to @2@ elements of type @CoordD@.
  -> m ()
glTexCoord2dv v1 = liftIO $ dyn39 ptr_glTexCoord2dv v1

{-# NOINLINE ptr_glTexCoord2dv #-}
ptr_glTexCoord2dv :: FunPtr (Ptr GLdouble -> IO ())
ptr_glTexCoord2dv = unsafePerformIO $ getCommand "glTexCoord2dv"

-- glTexCoord2f ----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glTexCoord2fv'.
glTexCoord2f
  :: MonadIO m
  => GLfloat -- ^ @s@ of type @CoordF@.
  -> GLfloat -- ^ @t@ of type @CoordF@.
  -> m ()
glTexCoord2f v1 v2 = liftIO $ dyn222 ptr_glTexCoord2f v1 v2

{-# NOINLINE ptr_glTexCoord2f #-}
ptr_glTexCoord2f :: FunPtr (GLfloat -> GLfloat -> IO ())
ptr_glTexCoord2f = unsafePerformIO $ getCommand "glTexCoord2f"

-- glTexCoord2fColor3fVertex3fSUN ----------------------------------------------

glTexCoord2fColor3fVertex3fSUN
  :: MonadIO m
  => GLfloat -- ^ @s@.
  -> GLfloat -- ^ @t@.
  -> GLfloat -- ^ @r@.
  -> GLfloat -- ^ @g@.
  -> GLfloat -- ^ @b@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> m ()
glTexCoord2fColor3fVertex3fSUN v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn613 ptr_glTexCoord2fColor3fVertex3fSUN v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glTexCoord2fColor3fVertex3fSUN #-}
ptr_glTexCoord2fColor3fVertex3fSUN :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glTexCoord2fColor3fVertex3fSUN = unsafePerformIO $ getCommand "glTexCoord2fColor3fVertex3fSUN"

-- glTexCoord2fColor3fVertex3fvSUN ---------------------------------------------

glTexCoord2fColor3fVertex3fvSUN
  :: MonadIO m
  => Ptr GLfloat -- ^ @tc@ pointing to @2@ elements of type @GLfloat@.
  -> Ptr GLfloat -- ^ @c@ pointing to @3@ elements of type @GLfloat@.
  -> Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @GLfloat@.
  -> m ()
glTexCoord2fColor3fVertex3fvSUN v1 v2 v3 = liftIO $ dyn111 ptr_glTexCoord2fColor3fVertex3fvSUN v1 v2 v3

{-# NOINLINE ptr_glTexCoord2fColor3fVertex3fvSUN #-}
ptr_glTexCoord2fColor3fVertex3fvSUN :: FunPtr (Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> IO ())
ptr_glTexCoord2fColor3fVertex3fvSUN = unsafePerformIO $ getCommand "glTexCoord2fColor3fVertex3fvSUN"

-- glTexCoord2fColor4fNormal3fVertex3fSUN --------------------------------------

glTexCoord2fColor4fNormal3fVertex3fSUN
  :: MonadIO m
  => GLfloat -- ^ @s@.
  -> GLfloat -- ^ @t@.
  -> GLfloat -- ^ @r@.
  -> GLfloat -- ^ @g@.
  -> GLfloat -- ^ @b@.
  -> GLfloat -- ^ @a@.
  -> GLfloat -- ^ @nx@.
  -> GLfloat -- ^ @ny@.
  -> GLfloat -- ^ @nz@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> m ()
glTexCoord2fColor4fNormal3fVertex3fSUN v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 = liftIO $ dyn723 ptr_glTexCoord2fColor4fNormal3fVertex3fSUN v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12

{-# NOINLINE ptr_glTexCoord2fColor4fNormal3fVertex3fSUN #-}
ptr_glTexCoord2fColor4fNormal3fVertex3fSUN :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glTexCoord2fColor4fNormal3fVertex3fSUN = unsafePerformIO $ getCommand "glTexCoord2fColor4fNormal3fVertex3fSUN"

-- glTexCoord2fColor4fNormal3fVertex3fvSUN -------------------------------------

glTexCoord2fColor4fNormal3fVertex3fvSUN
  :: MonadIO m
  => Ptr GLfloat -- ^ @tc@ pointing to @2@ elements of type @GLfloat@.
  -> Ptr GLfloat -- ^ @c@ pointing to @4@ elements of type @GLfloat@.
  -> Ptr GLfloat -- ^ @n@ pointing to @3@ elements of type @GLfloat@.
  -> Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @GLfloat@.
  -> m ()
glTexCoord2fColor4fNormal3fVertex3fvSUN v1 v2 v3 v4 = liftIO $ dyn724 ptr_glTexCoord2fColor4fNormal3fVertex3fvSUN v1 v2 v3 v4

{-# NOINLINE ptr_glTexCoord2fColor4fNormal3fVertex3fvSUN #-}
ptr_glTexCoord2fColor4fNormal3fVertex3fvSUN :: FunPtr (Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> IO ())
ptr_glTexCoord2fColor4fNormal3fVertex3fvSUN = unsafePerformIO $ getCommand "glTexCoord2fColor4fNormal3fVertex3fvSUN"

-- glTexCoord2fColor4ubVertex3fSUN ---------------------------------------------

glTexCoord2fColor4ubVertex3fSUN
  :: MonadIO m
  => GLfloat -- ^ @s@.
  -> GLfloat -- ^ @t@.
  -> GLubyte -- ^ @r@.
  -> GLubyte -- ^ @g@.
  -> GLubyte -- ^ @b@.
  -> GLubyte -- ^ @a@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> m ()
glTexCoord2fColor4ubVertex3fSUN v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn725 ptr_glTexCoord2fColor4ubVertex3fSUN v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glTexCoord2fColor4ubVertex3fSUN #-}
ptr_glTexCoord2fColor4ubVertex3fSUN :: FunPtr (GLfloat -> GLfloat -> GLubyte -> GLubyte -> GLubyte -> GLubyte -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glTexCoord2fColor4ubVertex3fSUN = unsafePerformIO $ getCommand "glTexCoord2fColor4ubVertex3fSUN"

-- glTexCoord2fColor4ubVertex3fvSUN --------------------------------------------

glTexCoord2fColor4ubVertex3fvSUN
  :: MonadIO m
  => Ptr GLfloat -- ^ @tc@ pointing to @2@ elements of type @GLfloat@.
  -> Ptr GLubyte -- ^ @c@ pointing to @4@ elements of type @GLubyte@.
  -> Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @GLfloat@.
  -> m ()
glTexCoord2fColor4ubVertex3fvSUN v1 v2 v3 = liftIO $ dyn726 ptr_glTexCoord2fColor4ubVertex3fvSUN v1 v2 v3

{-# NOINLINE ptr_glTexCoord2fColor4ubVertex3fvSUN #-}
ptr_glTexCoord2fColor4ubVertex3fvSUN :: FunPtr (Ptr GLfloat -> Ptr GLubyte -> Ptr GLfloat -> IO ())
ptr_glTexCoord2fColor4ubVertex3fvSUN = unsafePerformIO $ getCommand "glTexCoord2fColor4ubVertex3fvSUN"

-- glTexCoord2fNormal3fVertex3fSUN ---------------------------------------------

glTexCoord2fNormal3fVertex3fSUN
  :: MonadIO m
  => GLfloat -- ^ @s@.
  -> GLfloat -- ^ @t@.
  -> GLfloat -- ^ @nx@.
  -> GLfloat -- ^ @ny@.
  -> GLfloat -- ^ @nz@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> m ()
glTexCoord2fNormal3fVertex3fSUN v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn613 ptr_glTexCoord2fNormal3fVertex3fSUN v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glTexCoord2fNormal3fVertex3fSUN #-}
ptr_glTexCoord2fNormal3fVertex3fSUN :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glTexCoord2fNormal3fVertex3fSUN = unsafePerformIO $ getCommand "glTexCoord2fNormal3fVertex3fSUN"

-- glTexCoord2fNormal3fVertex3fvSUN --------------------------------------------

glTexCoord2fNormal3fVertex3fvSUN
  :: MonadIO m
  => Ptr GLfloat -- ^ @tc@ pointing to @2@ elements of type @GLfloat@.
  -> Ptr GLfloat -- ^ @n@ pointing to @3@ elements of type @GLfloat@.
  -> Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @GLfloat@.
  -> m ()
glTexCoord2fNormal3fVertex3fvSUN v1 v2 v3 = liftIO $ dyn111 ptr_glTexCoord2fNormal3fVertex3fvSUN v1 v2 v3

{-# NOINLINE ptr_glTexCoord2fNormal3fVertex3fvSUN #-}
ptr_glTexCoord2fNormal3fVertex3fvSUN :: FunPtr (Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> IO ())
ptr_glTexCoord2fNormal3fVertex3fvSUN = unsafePerformIO $ getCommand "glTexCoord2fNormal3fVertex3fvSUN"

-- glTexCoord2fVertex3fSUN -----------------------------------------------------

glTexCoord2fVertex3fSUN
  :: MonadIO m
  => GLfloat -- ^ @s@.
  -> GLfloat -- ^ @t@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> m ()
glTexCoord2fVertex3fSUN v1 v2 v3 v4 v5 = liftIO $ dyn252 ptr_glTexCoord2fVertex3fSUN v1 v2 v3 v4 v5

{-# NOINLINE ptr_glTexCoord2fVertex3fSUN #-}
ptr_glTexCoord2fVertex3fSUN :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glTexCoord2fVertex3fSUN = unsafePerformIO $ getCommand "glTexCoord2fVertex3fSUN"

-- glTexCoord2fVertex3fvSUN ----------------------------------------------------

glTexCoord2fVertex3fvSUN
  :: MonadIO m
  => Ptr GLfloat -- ^ @tc@ pointing to @2@ elements of type @GLfloat@.
  -> Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @GLfloat@.
  -> m ()
glTexCoord2fVertex3fvSUN v1 v2 = liftIO $ dyn97 ptr_glTexCoord2fVertex3fvSUN v1 v2

{-# NOINLINE ptr_glTexCoord2fVertex3fvSUN #-}
ptr_glTexCoord2fVertex3fvSUN :: FunPtr (Ptr GLfloat -> Ptr GLfloat -> IO ())
ptr_glTexCoord2fVertex3fvSUN = unsafePerformIO $ getCommand "glTexCoord2fVertex3fvSUN"

-- glTexCoord2fv ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glTexCoord.xml OpenGL 2.x>.
glTexCoord2fv
  :: MonadIO m
  => Ptr GLfloat -- ^ @v@ pointing to @2@ elements of type @CoordF@.
  -> m ()
glTexCoord2fv v1 = liftIO $ dyn41 ptr_glTexCoord2fv v1

{-# NOINLINE ptr_glTexCoord2fv #-}
ptr_glTexCoord2fv :: FunPtr (Ptr GLfloat -> IO ())
ptr_glTexCoord2fv = unsafePerformIO $ getCommand "glTexCoord2fv"

-- glTexCoord2hNV --------------------------------------------------------------

-- | The vector equivalent of this command is 'glTexCoord2hvNV'.
glTexCoord2hNV
  :: MonadIO m
  => GLhalfNV -- ^ @s@ of type @Half16NV@.
  -> GLhalfNV -- ^ @t@ of type @Half16NV@.
  -> m ()
glTexCoord2hNV v1 v2 = liftIO $ dyn727 ptr_glTexCoord2hNV v1 v2

{-# NOINLINE ptr_glTexCoord2hNV #-}
ptr_glTexCoord2hNV :: FunPtr (GLhalfNV -> GLhalfNV -> IO ())
ptr_glTexCoord2hNV = unsafePerformIO $ getCommand "glTexCoord2hNV"

