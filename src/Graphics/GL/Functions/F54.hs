--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F54
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

module Graphics.GL.Functions.F54 (
  glReadnPixelsKHR,
  glRectd,
  glRectdv,
  glRectf,
  glRectfv,
  glRecti,
  glRectiv,
  glRects,
  glRectsv,
  glRectxOES,
  glRectxvOES,
  glReferencePlaneSGIX,
  glReleaseShaderCompiler,
  glRenderMode,
  glRenderbufferStorage,
  glRenderbufferStorageEXT,
  glRenderbufferStorageMultisample,
  glRenderbufferStorageMultisampleANGLE,
  glRenderbufferStorageMultisampleAPPLE,
  glRenderbufferStorageMultisampleCoverageNV,
  glRenderbufferStorageMultisampleEXT,
  glRenderbufferStorageMultisampleIMG,
  glRenderbufferStorageMultisampleNV,
  glRenderbufferStorageOES,
  glReplacementCodePointerSUN,
  glReplacementCodeubSUN,
  glReplacementCodeubvSUN,
  glReplacementCodeuiColor3fVertex3fSUN,
  glReplacementCodeuiColor3fVertex3fvSUN,
  glReplacementCodeuiColor4fNormal3fVertex3fSUN,
  glReplacementCodeuiColor4fNormal3fVertex3fvSUN,
  glReplacementCodeuiColor4ubVertex3fSUN,
  glReplacementCodeuiColor4ubVertex3fvSUN,
  glReplacementCodeuiNormal3fVertex3fSUN,
  glReplacementCodeuiNormal3fVertex3fvSUN,
  glReplacementCodeuiSUN,
  glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN,
  glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN,
  glReplacementCodeuiTexCoord2fNormal3fVertex3fSUN,
  glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN
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

-- glReadnPixelsKHR ------------------------------------------------------------

-- | This command is an alias for 'glReadnPixels'.
glReadnPixelsKHR
  :: MonadIO m
  => GLint -- ^ @x@ of type @WinCoord@.
  -> GLint -- ^ @y@ of type @WinCoord@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> GLsizei -- ^ @bufSize@.
  -> Ptr a -- ^ @data@ pointing to @bufSize@ elements of type @a@.
  -> m ()
glReadnPixelsKHR v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn672 ptr_glReadnPixelsKHR v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glReadnPixelsKHR #-}
ptr_glReadnPixelsKHR :: FunPtr (GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glReadnPixelsKHR = unsafePerformIO $ getCommand "glReadnPixelsKHR"

-- glRectd ---------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRect.xml OpenGL 2.x>. The vector equivalent of this command is 'glRectdv'.
glRectd
  :: MonadIO m
  => GLdouble -- ^ @x1@ of type @CoordD@.
  -> GLdouble -- ^ @y1@ of type @CoordD@.
  -> GLdouble -- ^ @x2@ of type @CoordD@.
  -> GLdouble -- ^ @y2@ of type @CoordD@.
  -> m ()
glRectd v1 v2 v3 v4 = liftIO $ dyn109 ptr_glRectd v1 v2 v3 v4

{-# NOINLINE ptr_glRectd #-}
ptr_glRectd :: FunPtr (GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glRectd = unsafePerformIO $ getCommand "glRectd"

-- glRectdv --------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRect.xml OpenGL 2.x>.
glRectdv
  :: MonadIO m
  => Ptr GLdouble -- ^ @v1@ pointing to @2@ elements of type @CoordD@.
  -> Ptr GLdouble -- ^ @v2@ pointing to @2@ elements of type @CoordD@.
  -> m ()
glRectdv v1 v2 = liftIO $ dyn673 ptr_glRectdv v1 v2

{-# NOINLINE ptr_glRectdv #-}
ptr_glRectdv :: FunPtr (Ptr GLdouble -> Ptr GLdouble -> IO ())
ptr_glRectdv = unsafePerformIO $ getCommand "glRectdv"

-- glRectf ---------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRect.xml OpenGL 2.x>. The vector equivalent of this command is 'glRectfv'.
glRectf
  :: MonadIO m
  => GLfloat -- ^ @x1@ of type @CoordF@.
  -> GLfloat -- ^ @y1@ of type @CoordF@.
  -> GLfloat -- ^ @x2@ of type @CoordF@.
  -> GLfloat -- ^ @y2@ of type @CoordF@.
  -> m ()
glRectf v1 v2 v3 v4 = liftIO $ dyn49 ptr_glRectf v1 v2 v3 v4

{-# NOINLINE ptr_glRectf #-}
ptr_glRectf :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glRectf = unsafePerformIO $ getCommand "glRectf"

-- glRectfv --------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRect.xml OpenGL 2.x>.
glRectfv
  :: MonadIO m
  => Ptr GLfloat -- ^ @v1@ pointing to @2@ elements of type @CoordF@.
  -> Ptr GLfloat -- ^ @v2@ pointing to @2@ elements of type @CoordF@.
  -> m ()
glRectfv v1 v2 = liftIO $ dyn97 ptr_glRectfv v1 v2

{-# NOINLINE ptr_glRectfv #-}
ptr_glRectfv :: FunPtr (Ptr GLfloat -> Ptr GLfloat -> IO ())
ptr_glRectfv = unsafePerformIO $ getCommand "glRectfv"

-- glRecti ---------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRect.xml OpenGL 2.x>. The vector equivalent of this command is 'glRectiv'.
glRecti
  :: MonadIO m
  => GLint -- ^ @x1@ of type @CoordI@.
  -> GLint -- ^ @y1@ of type @CoordI@.
  -> GLint -- ^ @x2@ of type @CoordI@.
  -> GLint -- ^ @y2@ of type @CoordI@.
  -> m ()
glRecti v1 v2 v3 v4 = liftIO $ dyn76 ptr_glRecti v1 v2 v3 v4

{-# NOINLINE ptr_glRecti #-}
ptr_glRecti :: FunPtr (GLint -> GLint -> GLint -> GLint -> IO ())
ptr_glRecti = unsafePerformIO $ getCommand "glRecti"

-- glRectiv --------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRect.xml OpenGL 2.x>.
glRectiv
  :: MonadIO m
  => Ptr GLint -- ^ @v1@ pointing to @2@ elements of type @CoordI@.
  -> Ptr GLint -- ^ @v2@ pointing to @2@ elements of type @CoordI@.
  -> m ()
glRectiv v1 v2 = liftIO $ dyn674 ptr_glRectiv v1 v2

{-# NOINLINE ptr_glRectiv #-}
ptr_glRectiv :: FunPtr (Ptr GLint -> Ptr GLint -> IO ())
ptr_glRectiv = unsafePerformIO $ getCommand "glRectiv"

-- glRects ---------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRect.xml OpenGL 2.x>. The vector equivalent of this command is 'glRectsv'.
glRects
  :: MonadIO m
  => GLshort -- ^ @x1@ of type @CoordS@.
  -> GLshort -- ^ @y1@ of type @CoordS@.
  -> GLshort -- ^ @x2@ of type @CoordS@.
  -> GLshort -- ^ @y2@ of type @CoordS@.
  -> m ()
glRects v1 v2 v3 v4 = liftIO $ dyn113 ptr_glRects v1 v2 v3 v4

{-# NOINLINE ptr_glRects #-}
ptr_glRects :: FunPtr (GLshort -> GLshort -> GLshort -> GLshort -> IO ())
ptr_glRects = unsafePerformIO $ getCommand "glRects"

-- glRectsv --------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRect.xml OpenGL 2.x>.
glRectsv
  :: MonadIO m
  => Ptr GLshort -- ^ @v1@ pointing to @2@ elements of type @CoordS@.
  -> Ptr GLshort -- ^ @v2@ pointing to @2@ elements of type @CoordS@.
  -> m ()
glRectsv v1 v2 = liftIO $ dyn675 ptr_glRectsv v1 v2

{-# NOINLINE ptr_glRectsv #-}
ptr_glRectsv :: FunPtr (Ptr GLshort -> Ptr GLshort -> IO ())
ptr_glRectsv = unsafePerformIO $ getCommand "glRectsv"

-- glRectxOES ------------------------------------------------------------------

glRectxOES
  :: MonadIO m
  => GLfixed -- ^ @x1@.
  -> GLfixed -- ^ @y1@.
  -> GLfixed -- ^ @x2@.
  -> GLfixed -- ^ @y2@.
  -> m ()
glRectxOES v1 v2 v3 v4 = liftIO $ dyn50 ptr_glRectxOES v1 v2 v3 v4

{-# NOINLINE ptr_glRectxOES #-}
ptr_glRectxOES :: FunPtr (GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ())
ptr_glRectxOES = unsafePerformIO $ getCommand "glRectxOES"

-- glRectxvOES -----------------------------------------------------------------

glRectxvOES
  :: MonadIO m
  => Ptr GLfixed -- ^ @v1@ pointing to @2@ elements of type @GLfixed@.
  -> Ptr GLfixed -- ^ @v2@ pointing to @2@ elements of type @GLfixed@.
  -> m ()
glRectxvOES v1 v2 = liftIO $ dyn676 ptr_glRectxvOES v1 v2

{-# NOINLINE ptr_glRectxvOES #-}
ptr_glRectxvOES :: FunPtr (Ptr GLfixed -> Ptr GLfixed -> IO ())
ptr_glRectxvOES = unsafePerformIO $ getCommand "glRectxvOES"

-- glReferencePlaneSGIX --------------------------------------------------------

glReferencePlaneSGIX
  :: MonadIO m
  => Ptr GLdouble -- ^ @equation@ pointing to @4@ elements of type @GLdouble@.
  -> m ()
glReferencePlaneSGIX v1 = liftIO $ dyn39 ptr_glReferencePlaneSGIX v1

{-# NOINLINE ptr_glReferencePlaneSGIX #-}
ptr_glReferencePlaneSGIX :: FunPtr (Ptr GLdouble -> IO ())
ptr_glReferencePlaneSGIX = unsafePerformIO $ getCommand "glReferencePlaneSGIX"

-- glReleaseShaderCompiler -----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glReleaseShaderCompiler.xhtml OpenGL 4.x>.
glReleaseShaderCompiler
  :: MonadIO m
  => m ()
glReleaseShaderCompiler = liftIO $ dyn10 ptr_glReleaseShaderCompiler

{-# NOINLINE ptr_glReleaseShaderCompiler #-}
ptr_glReleaseShaderCompiler :: FunPtr (IO ())
ptr_glReleaseShaderCompiler = unsafePerformIO $ getCommand "glReleaseShaderCompiler"

-- glRenderMode ----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glRenderMode.xml OpenGL 2.x>.
glRenderMode
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [RenderingMode](Graphics-GL-Groups.html#RenderingMode).
  -> m GLint
glRenderMode v1 = liftIO $ dyn677 ptr_glRenderMode v1

{-# NOINLINE ptr_glRenderMode #-}
ptr_glRenderMode :: FunPtr (GLenum -> IO GLint)
ptr_glRenderMode = unsafePerformIO $ getCommand "glRenderMode"

-- glRenderbufferStorage -------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glRenderbufferStorage.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glRenderbufferStorage.xhtml OpenGL 4.x>.
glRenderbufferStorage
  :: MonadIO m
  => GLenum -- ^ @target@ of type @RenderbufferTarget@.
  -> GLenum -- ^ @internalformat@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glRenderbufferStorage v1 v2 v3 v4 = liftIO $ dyn678 ptr_glRenderbufferStorage v1 v2 v3 v4

{-# NOINLINE ptr_glRenderbufferStorage #-}
ptr_glRenderbufferStorage :: FunPtr (GLenum -> GLenum -> GLsizei -> GLsizei -> IO ())
ptr_glRenderbufferStorage = unsafePerformIO $ getCommand "glRenderbufferStorage"

-- glRenderbufferStorageEXT ----------------------------------------------------

-- | This command is an alias for 'glRenderbufferStorage'.
glRenderbufferStorageEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type @RenderbufferTarget@.
  -> GLenum -- ^ @internalformat@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glRenderbufferStorageEXT v1 v2 v3 v4 = liftIO $ dyn678 ptr_glRenderbufferStorageEXT v1 v2 v3 v4

{-# NOINLINE ptr_glRenderbufferStorageEXT #-}
ptr_glRenderbufferStorageEXT :: FunPtr (GLenum -> GLenum -> GLsizei -> GLsizei -> IO ())
ptr_glRenderbufferStorageEXT = unsafePerformIO $ getCommand "glRenderbufferStorageEXT"

-- glRenderbufferStorageMultisample --------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glRenderbufferStorageMultisample.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glRenderbufferStorageMultisample.xhtml OpenGL 4.x>.
glRenderbufferStorageMultisample
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLsizei -- ^ @samples@.
  -> GLenum -- ^ @internalformat@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glRenderbufferStorageMultisample v1 v2 v3 v4 v5 = liftIO $ dyn679 ptr_glRenderbufferStorageMultisample v1 v2 v3 v4 v5

{-# NOINLINE ptr_glRenderbufferStorageMultisample #-}
ptr_glRenderbufferStorageMultisample :: FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> IO ())
ptr_glRenderbufferStorageMultisample = unsafePerformIO $ getCommand "glRenderbufferStorageMultisample"

-- glRenderbufferStorageMultisampleANGLE ---------------------------------------

glRenderbufferStorageMultisampleANGLE
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLsizei -- ^ @samples@.
  -> GLenum -- ^ @internalformat@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glRenderbufferStorageMultisampleANGLE v1 v2 v3 v4 v5 = liftIO $ dyn679 ptr_glRenderbufferStorageMultisampleANGLE v1 v2 v3 v4 v5

{-# NOINLINE ptr_glRenderbufferStorageMultisampleANGLE #-}
ptr_glRenderbufferStorageMultisampleANGLE :: FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> IO ())
ptr_glRenderbufferStorageMultisampleANGLE = unsafePerformIO $ getCommand "glRenderbufferStorageMultisampleANGLE"

-- glRenderbufferStorageMultisampleAPPLE ---------------------------------------

glRenderbufferStorageMultisampleAPPLE
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLsizei -- ^ @samples@.
  -> GLenum -- ^ @internalformat@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glRenderbufferStorageMultisampleAPPLE v1 v2 v3 v4 v5 = liftIO $ dyn679 ptr_glRenderbufferStorageMultisampleAPPLE v1 v2 v3 v4 v5

{-# NOINLINE ptr_glRenderbufferStorageMultisampleAPPLE #-}
ptr_glRenderbufferStorageMultisampleAPPLE :: FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> IO ())
ptr_glRenderbufferStorageMultisampleAPPLE = unsafePerformIO $ getCommand "glRenderbufferStorageMultisampleAPPLE"

-- glRenderbufferStorageMultisampleCoverageNV ----------------------------------

glRenderbufferStorageMultisampleCoverageNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type @RenderbufferTarget@.
  -> GLsizei -- ^ @coverageSamples@.
  -> GLsizei -- ^ @colorSamples@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glRenderbufferStorageMultisampleCoverageNV v1 v2 v3 v4 v5 v6 = liftIO $ dyn680 ptr_glRenderbufferStorageMultisampleCoverageNV v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glRenderbufferStorageMultisampleCoverageNV #-}
ptr_glRenderbufferStorageMultisampleCoverageNV :: FunPtr (GLenum -> GLsizei -> GLsizei -> GLenum -> GLsizei -> GLsizei -> IO ())
ptr_glRenderbufferStorageMultisampleCoverageNV = unsafePerformIO $ getCommand "glRenderbufferStorageMultisampleCoverageNV"

-- glRenderbufferStorageMultisampleEXT -----------------------------------------

-- | This command is an alias for 'glRenderbufferStorageMultisample'.
glRenderbufferStorageMultisampleEXT
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLsizei -- ^ @samples@.
  -> GLenum -- ^ @internalformat@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glRenderbufferStorageMultisampleEXT v1 v2 v3 v4 v5 = liftIO $ dyn679 ptr_glRenderbufferStorageMultisampleEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glRenderbufferStorageMultisampleEXT #-}
ptr_glRenderbufferStorageMultisampleEXT :: FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> IO ())
ptr_glRenderbufferStorageMultisampleEXT = unsafePerformIO $ getCommand "glRenderbufferStorageMultisampleEXT"

-- glRenderbufferStorageMultisampleIMG -----------------------------------------

glRenderbufferStorageMultisampleIMG
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLsizei -- ^ @samples@.
  -> GLenum -- ^ @internalformat@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glRenderbufferStorageMultisampleIMG v1 v2 v3 v4 v5 = liftIO $ dyn679 ptr_glRenderbufferStorageMultisampleIMG v1 v2 v3 v4 v5

{-# NOINLINE ptr_glRenderbufferStorageMultisampleIMG #-}
ptr_glRenderbufferStorageMultisampleIMG :: FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> IO ())
ptr_glRenderbufferStorageMultisampleIMG = unsafePerformIO $ getCommand "glRenderbufferStorageMultisampleIMG"

-- glRenderbufferStorageMultisampleNV ------------------------------------------

-- | This command is an alias for 'glRenderbufferStorageMultisample'.
glRenderbufferStorageMultisampleNV
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLsizei -- ^ @samples@.
  -> GLenum -- ^ @internalformat@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glRenderbufferStorageMultisampleNV v1 v2 v3 v4 v5 = liftIO $ dyn679 ptr_glRenderbufferStorageMultisampleNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glRenderbufferStorageMultisampleNV #-}
ptr_glRenderbufferStorageMultisampleNV :: FunPtr (GLenum -> GLsizei -> GLenum -> GLsizei -> GLsizei -> IO ())
ptr_glRenderbufferStorageMultisampleNV = unsafePerformIO $ getCommand "glRenderbufferStorageMultisampleNV"

-- glRenderbufferStorageOES ----------------------------------------------------

glRenderbufferStorageOES
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @internalformat@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glRenderbufferStorageOES v1 v2 v3 v4 = liftIO $ dyn678 ptr_glRenderbufferStorageOES v1 v2 v3 v4

{-# NOINLINE ptr_glRenderbufferStorageOES #-}
ptr_glRenderbufferStorageOES :: FunPtr (GLenum -> GLenum -> GLsizei -> GLsizei -> IO ())
ptr_glRenderbufferStorageOES = unsafePerformIO $ getCommand "glRenderbufferStorageOES"

-- glReplacementCodePointerSUN -------------------------------------------------

glReplacementCodePointerSUN
  :: MonadIO m
  => GLenum -- ^ @type@ of type @ReplacementCodeTypeSUN@.
  -> GLsizei -- ^ @stride@.
  -> Ptr (Ptr a) -- ^ @pointer@ pointing to @COMPSIZE(type,stride)@ elements of type @Ptr a@.
  -> m ()
glReplacementCodePointerSUN v1 v2 v3 = liftIO $ dyn681 ptr_glReplacementCodePointerSUN v1 v2 v3

{-# NOINLINE ptr_glReplacementCodePointerSUN #-}
ptr_glReplacementCodePointerSUN :: FunPtr (GLenum -> GLsizei -> Ptr (Ptr a) -> IO ())
ptr_glReplacementCodePointerSUN = unsafePerformIO $ getCommand "glReplacementCodePointerSUN"

-- glReplacementCodeubSUN ------------------------------------------------------

glReplacementCodeubSUN
  :: MonadIO m
  => GLubyte -- ^ @code@.
  -> m ()
glReplacementCodeubSUN v1 = liftIO $ dyn464 ptr_glReplacementCodeubSUN v1

{-# NOINLINE ptr_glReplacementCodeubSUN #-}
ptr_glReplacementCodeubSUN :: FunPtr (GLubyte -> IO ())
ptr_glReplacementCodeubSUN = unsafePerformIO $ getCommand "glReplacementCodeubSUN"

-- glReplacementCodeubvSUN -----------------------------------------------------

glReplacementCodeubvSUN
  :: MonadIO m
  => Ptr GLubyte -- ^ @code@ pointing to @COMPSIZE()@ elements of type @GLubyte@.
  -> m ()
glReplacementCodeubvSUN v1 = liftIO $ dyn101 ptr_glReplacementCodeubvSUN v1

{-# NOINLINE ptr_glReplacementCodeubvSUN #-}
ptr_glReplacementCodeubvSUN :: FunPtr (Ptr GLubyte -> IO ())
ptr_glReplacementCodeubvSUN = unsafePerformIO $ getCommand "glReplacementCodeubvSUN"

-- glReplacementCodeuiColor3fVertex3fSUN ---------------------------------------

glReplacementCodeuiColor3fVertex3fSUN
  :: MonadIO m
  => GLuint -- ^ @rc@ of type @ReplacementCodeSUN@.
  -> GLfloat -- ^ @r@.
  -> GLfloat -- ^ @g@.
  -> GLfloat -- ^ @b@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> m ()
glReplacementCodeuiColor3fVertex3fSUN v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn682 ptr_glReplacementCodeuiColor3fVertex3fSUN v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glReplacementCodeuiColor3fVertex3fSUN #-}
ptr_glReplacementCodeuiColor3fVertex3fSUN :: FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glReplacementCodeuiColor3fVertex3fSUN = unsafePerformIO $ getCommand "glReplacementCodeuiColor3fVertex3fSUN"

-- glReplacementCodeuiColor3fVertex3fvSUN --------------------------------------

glReplacementCodeuiColor3fVertex3fvSUN
  :: MonadIO m
  => Ptr GLuint -- ^ @rc@ pointing to @1@ element of type @ReplacementCodeSUN@.
  -> Ptr GLfloat -- ^ @c@ pointing to @3@ elements of type @GLfloat@.
  -> Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @GLfloat@.
  -> m ()
glReplacementCodeuiColor3fVertex3fvSUN v1 v2 v3 = liftIO $ dyn683 ptr_glReplacementCodeuiColor3fVertex3fvSUN v1 v2 v3

{-# NOINLINE ptr_glReplacementCodeuiColor3fVertex3fvSUN #-}
ptr_glReplacementCodeuiColor3fVertex3fvSUN :: FunPtr (Ptr GLuint -> Ptr GLfloat -> Ptr GLfloat -> IO ())
ptr_glReplacementCodeuiColor3fVertex3fvSUN = unsafePerformIO $ getCommand "glReplacementCodeuiColor3fVertex3fvSUN"

-- glReplacementCodeuiColor4fNormal3fVertex3fSUN -------------------------------

glReplacementCodeuiColor4fNormal3fVertex3fSUN
  :: MonadIO m
  => GLuint -- ^ @rc@ of type @ReplacementCodeSUN@.
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
glReplacementCodeuiColor4fNormal3fVertex3fSUN v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 = liftIO $ dyn684 ptr_glReplacementCodeuiColor4fNormal3fVertex3fSUN v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11

{-# NOINLINE ptr_glReplacementCodeuiColor4fNormal3fVertex3fSUN #-}
ptr_glReplacementCodeuiColor4fNormal3fVertex3fSUN :: FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glReplacementCodeuiColor4fNormal3fVertex3fSUN = unsafePerformIO $ getCommand "glReplacementCodeuiColor4fNormal3fVertex3fSUN"

-- glReplacementCodeuiColor4fNormal3fVertex3fvSUN ------------------------------

glReplacementCodeuiColor4fNormal3fVertex3fvSUN
  :: MonadIO m
  => Ptr GLuint -- ^ @rc@ pointing to @1@ element of type @ReplacementCodeSUN@.
  -> Ptr GLfloat -- ^ @c@ pointing to @4@ elements of type @GLfloat@.
  -> Ptr GLfloat -- ^ @n@ pointing to @3@ elements of type @GLfloat@.
  -> Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @GLfloat@.
  -> m ()
glReplacementCodeuiColor4fNormal3fVertex3fvSUN v1 v2 v3 v4 = liftIO $ dyn685 ptr_glReplacementCodeuiColor4fNormal3fVertex3fvSUN v1 v2 v3 v4

{-# NOINLINE ptr_glReplacementCodeuiColor4fNormal3fVertex3fvSUN #-}
ptr_glReplacementCodeuiColor4fNormal3fVertex3fvSUN :: FunPtr (Ptr GLuint -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> IO ())
ptr_glReplacementCodeuiColor4fNormal3fVertex3fvSUN = unsafePerformIO $ getCommand "glReplacementCodeuiColor4fNormal3fVertex3fvSUN"

-- glReplacementCodeuiColor4ubVertex3fSUN --------------------------------------

glReplacementCodeuiColor4ubVertex3fSUN
  :: MonadIO m
  => GLuint -- ^ @rc@ of type @ReplacementCodeSUN@.
  -> GLubyte -- ^ @r@.
  -> GLubyte -- ^ @g@.
  -> GLubyte -- ^ @b@.
  -> GLubyte -- ^ @a@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> m ()
glReplacementCodeuiColor4ubVertex3fSUN v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn686 ptr_glReplacementCodeuiColor4ubVertex3fSUN v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glReplacementCodeuiColor4ubVertex3fSUN #-}
ptr_glReplacementCodeuiColor4ubVertex3fSUN :: FunPtr (GLuint -> GLubyte -> GLubyte -> GLubyte -> GLubyte -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glReplacementCodeuiColor4ubVertex3fSUN = unsafePerformIO $ getCommand "glReplacementCodeuiColor4ubVertex3fSUN"

-- glReplacementCodeuiColor4ubVertex3fvSUN -------------------------------------

glReplacementCodeuiColor4ubVertex3fvSUN
  :: MonadIO m
  => Ptr GLuint -- ^ @rc@ pointing to @1@ element of type @ReplacementCodeSUN@.
  -> Ptr GLubyte -- ^ @c@ pointing to @4@ elements of type @GLubyte@.
  -> Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @GLfloat@.
  -> m ()
glReplacementCodeuiColor4ubVertex3fvSUN v1 v2 v3 = liftIO $ dyn687 ptr_glReplacementCodeuiColor4ubVertex3fvSUN v1 v2 v3

{-# NOINLINE ptr_glReplacementCodeuiColor4ubVertex3fvSUN #-}
ptr_glReplacementCodeuiColor4ubVertex3fvSUN :: FunPtr (Ptr GLuint -> Ptr GLubyte -> Ptr GLfloat -> IO ())
ptr_glReplacementCodeuiColor4ubVertex3fvSUN = unsafePerformIO $ getCommand "glReplacementCodeuiColor4ubVertex3fvSUN"

-- glReplacementCodeuiNormal3fVertex3fSUN --------------------------------------

glReplacementCodeuiNormal3fVertex3fSUN
  :: MonadIO m
  => GLuint -- ^ @rc@ of type @ReplacementCodeSUN@.
  -> GLfloat -- ^ @nx@.
  -> GLfloat -- ^ @ny@.
  -> GLfloat -- ^ @nz@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> m ()
glReplacementCodeuiNormal3fVertex3fSUN v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn682 ptr_glReplacementCodeuiNormal3fVertex3fSUN v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glReplacementCodeuiNormal3fVertex3fSUN #-}
ptr_glReplacementCodeuiNormal3fVertex3fSUN :: FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glReplacementCodeuiNormal3fVertex3fSUN = unsafePerformIO $ getCommand "glReplacementCodeuiNormal3fVertex3fSUN"

-- glReplacementCodeuiNormal3fVertex3fvSUN -------------------------------------

glReplacementCodeuiNormal3fVertex3fvSUN
  :: MonadIO m
  => Ptr GLuint -- ^ @rc@ pointing to @1@ element of type @ReplacementCodeSUN@.
  -> Ptr GLfloat -- ^ @n@ pointing to @3@ elements of type @GLfloat@.
  -> Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @GLfloat@.
  -> m ()
glReplacementCodeuiNormal3fVertex3fvSUN v1 v2 v3 = liftIO $ dyn683 ptr_glReplacementCodeuiNormal3fVertex3fvSUN v1 v2 v3

{-# NOINLINE ptr_glReplacementCodeuiNormal3fVertex3fvSUN #-}
ptr_glReplacementCodeuiNormal3fVertex3fvSUN :: FunPtr (Ptr GLuint -> Ptr GLfloat -> Ptr GLfloat -> IO ())
ptr_glReplacementCodeuiNormal3fVertex3fvSUN = unsafePerformIO $ getCommand "glReplacementCodeuiNormal3fVertex3fvSUN"

-- glReplacementCodeuiSUN ------------------------------------------------------

glReplacementCodeuiSUN
  :: MonadIO m
  => GLuint -- ^ @code@.
  -> m ()
glReplacementCodeuiSUN v1 = liftIO $ dyn2 ptr_glReplacementCodeuiSUN v1

{-# NOINLINE ptr_glReplacementCodeuiSUN #-}
ptr_glReplacementCodeuiSUN :: FunPtr (GLuint -> IO ())
ptr_glReplacementCodeuiSUN = unsafePerformIO $ getCommand "glReplacementCodeuiSUN"

-- glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN ---------------------

glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN
  :: MonadIO m
  => GLuint -- ^ @rc@ of type @ReplacementCodeSUN@.
  -> GLfloat -- ^ @s@.
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
glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 = liftIO $ dyn688 ptr_glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13

{-# NOINLINE ptr_glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN #-}
ptr_glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN :: FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN = unsafePerformIO $ getCommand "glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fSUN"

-- glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN --------------------

glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN
  :: MonadIO m
  => Ptr GLuint -- ^ @rc@ pointing to @1@ element of type @ReplacementCodeSUN@.
  -> Ptr GLfloat -- ^ @tc@ pointing to @2@ elements of type @GLfloat@.
  -> Ptr GLfloat -- ^ @c@ pointing to @4@ elements of type @GLfloat@.
  -> Ptr GLfloat -- ^ @n@ pointing to @3@ elements of type @GLfloat@.
  -> Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @GLfloat@.
  -> m ()
glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN v1 v2 v3 v4 v5 = liftIO $ dyn689 ptr_glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN v1 v2 v3 v4 v5

{-# NOINLINE ptr_glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN #-}
ptr_glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN :: FunPtr (Ptr GLuint -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> IO ())
ptr_glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN = unsafePerformIO $ getCommand "glReplacementCodeuiTexCoord2fColor4fNormal3fVertex3fvSUN"

-- glReplacementCodeuiTexCoord2fNormal3fVertex3fSUN ----------------------------

glReplacementCodeuiTexCoord2fNormal3fVertex3fSUN
  :: MonadIO m
  => GLuint -- ^ @rc@ of type @ReplacementCodeSUN@.
  -> GLfloat -- ^ @s@.
  -> GLfloat -- ^ @t@.
  -> GLfloat -- ^ @nx@.
  -> GLfloat -- ^ @ny@.
  -> GLfloat -- ^ @nz@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> m ()
glReplacementCodeuiTexCoord2fNormal3fVertex3fSUN v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn690 ptr_glReplacementCodeuiTexCoord2fNormal3fVertex3fSUN v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glReplacementCodeuiTexCoord2fNormal3fVertex3fSUN #-}
ptr_glReplacementCodeuiTexCoord2fNormal3fVertex3fSUN :: FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glReplacementCodeuiTexCoord2fNormal3fVertex3fSUN = unsafePerformIO $ getCommand "glReplacementCodeuiTexCoord2fNormal3fVertex3fSUN"

-- glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN ---------------------------

glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN
  :: MonadIO m
  => Ptr GLuint -- ^ @rc@ pointing to @1@ element of type @ReplacementCodeSUN@.
  -> Ptr GLfloat -- ^ @tc@ pointing to @2@ elements of type @GLfloat@.
  -> Ptr GLfloat -- ^ @n@ pointing to @3@ elements of type @GLfloat@.
  -> Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @GLfloat@.
  -> m ()
glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN v1 v2 v3 v4 = liftIO $ dyn685 ptr_glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN v1 v2 v3 v4

{-# NOINLINE ptr_glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN #-}
ptr_glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN :: FunPtr (Ptr GLuint -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> IO ())
ptr_glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN = unsafePerformIO $ getCommand "glReplacementCodeuiTexCoord2fNormal3fVertex3fvSUN"

