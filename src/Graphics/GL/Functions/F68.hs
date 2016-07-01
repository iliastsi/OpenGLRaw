--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F68
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

module Graphics.GL.Functions.F68 (
  glUseShaderProgramEXT,
  glVDPAUFiniNV,
  glVDPAUGetSurfaceivNV,
  glVDPAUInitNV,
  glVDPAUIsSurfaceNV,
  glVDPAUMapSurfacesNV,
  glVDPAURegisterOutputSurfaceNV,
  glVDPAURegisterVideoSurfaceNV,
  glVDPAUSurfaceAccessNV,
  glVDPAUUnmapSurfacesNV,
  glVDPAUUnregisterSurfaceNV,
  glValidateProgram,
  glValidateProgramARB,
  glValidateProgramPipeline,
  glValidateProgramPipelineEXT,
  glVariantArrayObjectATI,
  glVariantPointerEXT,
  glVariantbvEXT,
  glVariantdvEXT,
  glVariantfvEXT,
  glVariantivEXT,
  glVariantsvEXT,
  glVariantubvEXT,
  glVariantuivEXT,
  glVariantusvEXT,
  glVertex2bOES,
  glVertex2bvOES,
  glVertex2d,
  glVertex2dv,
  glVertex2f,
  glVertex2fv,
  glVertex2hNV,
  glVertex2hvNV,
  glVertex2i,
  glVertex2iv,
  glVertex2s,
  glVertex2sv,
  glVertex2xOES,
  glVertex2xvOES,
  glVertex3bOES
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

-- glUseShaderProgramEXT -------------------------------------------------------

glUseShaderProgramEXT
  :: MonadIO m
  => GLenum -- ^ @type@.
  -> GLuint -- ^ @program@.
  -> m ()
glUseShaderProgramEXT v1 v2 = liftIO $ dyn16 ptr_glUseShaderProgramEXT v1 v2

{-# NOINLINE ptr_glUseShaderProgramEXT #-}
ptr_glUseShaderProgramEXT :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glUseShaderProgramEXT = unsafePerformIO $ getCommand "glUseShaderProgramEXT"

-- glVDPAUFiniNV ---------------------------------------------------------------

glVDPAUFiniNV
  :: MonadIO m
  => m ()
glVDPAUFiniNV = liftIO $ dyn10 ptr_glVDPAUFiniNV

{-# NOINLINE ptr_glVDPAUFiniNV #-}
ptr_glVDPAUFiniNV :: FunPtr (IO ())
ptr_glVDPAUFiniNV = unsafePerformIO $ getCommand "glVDPAUFiniNV"

-- glVDPAUGetSurfaceivNV -------------------------------------------------------

glVDPAUGetSurfaceivNV
  :: MonadIO m
  => GLvdpauSurfaceNV -- ^ @surface@ of type @vdpauSurfaceNV@.
  -> GLenum -- ^ @pname@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLsizei -- ^ @length@.
  -> Ptr GLint -- ^ @values@ pointing to @bufSize@ elements of type @GLint@.
  -> m ()
glVDPAUGetSurfaceivNV v1 v2 v3 v4 v5 = liftIO $ dyn818 ptr_glVDPAUGetSurfaceivNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVDPAUGetSurfaceivNV #-}
ptr_glVDPAUGetSurfaceivNV :: FunPtr (GLvdpauSurfaceNV -> GLenum -> GLsizei -> Ptr GLsizei -> Ptr GLint -> IO ())
ptr_glVDPAUGetSurfaceivNV = unsafePerformIO $ getCommand "glVDPAUGetSurfaceivNV"

-- glVDPAUInitNV ---------------------------------------------------------------

glVDPAUInitNV
  :: MonadIO m
  => Ptr a -- ^ @vdpDevice@.
  -> Ptr b -- ^ @getProcAddress@.
  -> m ()
glVDPAUInitNV v1 v2 = liftIO $ dyn819 ptr_glVDPAUInitNV v1 v2

{-# NOINLINE ptr_glVDPAUInitNV #-}
ptr_glVDPAUInitNV :: FunPtr (Ptr a -> Ptr b -> IO ())
ptr_glVDPAUInitNV = unsafePerformIO $ getCommand "glVDPAUInitNV"

-- glVDPAUIsSurfaceNV ----------------------------------------------------------

glVDPAUIsSurfaceNV
  :: MonadIO m
  => GLvdpauSurfaceNV -- ^ @surface@ of type @vdpauSurfaceNV@.
  -> m GLboolean
glVDPAUIsSurfaceNV v1 = liftIO $ dyn820 ptr_glVDPAUIsSurfaceNV v1

{-# NOINLINE ptr_glVDPAUIsSurfaceNV #-}
ptr_glVDPAUIsSurfaceNV :: FunPtr (GLvdpauSurfaceNV -> IO GLboolean)
ptr_glVDPAUIsSurfaceNV = unsafePerformIO $ getCommand "glVDPAUIsSurfaceNV"

-- glVDPAUMapSurfacesNV --------------------------------------------------------

glVDPAUMapSurfacesNV
  :: MonadIO m
  => GLsizei -- ^ @numSurfaces@.
  -> Ptr GLvdpauSurfaceNV -- ^ @surfaces@ pointing to @numSurfaces@ elements of type @vdpauSurfaceNV@.
  -> m ()
glVDPAUMapSurfacesNV v1 v2 = liftIO $ dyn821 ptr_glVDPAUMapSurfacesNV v1 v2

{-# NOINLINE ptr_glVDPAUMapSurfacesNV #-}
ptr_glVDPAUMapSurfacesNV :: FunPtr (GLsizei -> Ptr GLvdpauSurfaceNV -> IO ())
ptr_glVDPAUMapSurfacesNV = unsafePerformIO $ getCommand "glVDPAUMapSurfacesNV"

-- glVDPAURegisterOutputSurfaceNV ----------------------------------------------

glVDPAURegisterOutputSurfaceNV
  :: MonadIO m
  => Ptr a -- ^ @vdpSurface@.
  -> GLenum -- ^ @target@.
  -> GLsizei -- ^ @numTextureNames@.
  -> Ptr GLuint -- ^ @textureNames@ pointing to @numTextureNames@ elements of type @GLuint@.
  -> m GLvdpauSurfaceNV -- ^ of type @vdpauSurfaceNV@.
glVDPAURegisterOutputSurfaceNV v1 v2 v3 v4 = liftIO $ dyn822 ptr_glVDPAURegisterOutputSurfaceNV v1 v2 v3 v4

{-# NOINLINE ptr_glVDPAURegisterOutputSurfaceNV #-}
ptr_glVDPAURegisterOutputSurfaceNV :: FunPtr (Ptr a -> GLenum -> GLsizei -> Ptr GLuint -> IO GLvdpauSurfaceNV)
ptr_glVDPAURegisterOutputSurfaceNV = unsafePerformIO $ getCommand "glVDPAURegisterOutputSurfaceNV"

-- glVDPAURegisterVideoSurfaceNV -----------------------------------------------

glVDPAURegisterVideoSurfaceNV
  :: MonadIO m
  => Ptr a -- ^ @vdpSurface@.
  -> GLenum -- ^ @target@.
  -> GLsizei -- ^ @numTextureNames@.
  -> Ptr GLuint -- ^ @textureNames@ pointing to @numTextureNames@ elements of type @GLuint@.
  -> m GLvdpauSurfaceNV -- ^ of type @vdpauSurfaceNV@.
glVDPAURegisterVideoSurfaceNV v1 v2 v3 v4 = liftIO $ dyn822 ptr_glVDPAURegisterVideoSurfaceNV v1 v2 v3 v4

{-# NOINLINE ptr_glVDPAURegisterVideoSurfaceNV #-}
ptr_glVDPAURegisterVideoSurfaceNV :: FunPtr (Ptr a -> GLenum -> GLsizei -> Ptr GLuint -> IO GLvdpauSurfaceNV)
ptr_glVDPAURegisterVideoSurfaceNV = unsafePerformIO $ getCommand "glVDPAURegisterVideoSurfaceNV"

-- glVDPAUSurfaceAccessNV ------------------------------------------------------

glVDPAUSurfaceAccessNV
  :: MonadIO m
  => GLvdpauSurfaceNV -- ^ @surface@ of type @vdpauSurfaceNV@.
  -> GLenum -- ^ @access@.
  -> m ()
glVDPAUSurfaceAccessNV v1 v2 = liftIO $ dyn823 ptr_glVDPAUSurfaceAccessNV v1 v2

{-# NOINLINE ptr_glVDPAUSurfaceAccessNV #-}
ptr_glVDPAUSurfaceAccessNV :: FunPtr (GLvdpauSurfaceNV -> GLenum -> IO ())
ptr_glVDPAUSurfaceAccessNV = unsafePerformIO $ getCommand "glVDPAUSurfaceAccessNV"

-- glVDPAUUnmapSurfacesNV ------------------------------------------------------

glVDPAUUnmapSurfacesNV
  :: MonadIO m
  => GLsizei -- ^ @numSurface@.
  -> Ptr GLvdpauSurfaceNV -- ^ @surfaces@ pointing to @numSurface@ elements of type @vdpauSurfaceNV@.
  -> m ()
glVDPAUUnmapSurfacesNV v1 v2 = liftIO $ dyn821 ptr_glVDPAUUnmapSurfacesNV v1 v2

{-# NOINLINE ptr_glVDPAUUnmapSurfacesNV #-}
ptr_glVDPAUUnmapSurfacesNV :: FunPtr (GLsizei -> Ptr GLvdpauSurfaceNV -> IO ())
ptr_glVDPAUUnmapSurfacesNV = unsafePerformIO $ getCommand "glVDPAUUnmapSurfacesNV"

-- glVDPAUUnregisterSurfaceNV --------------------------------------------------

glVDPAUUnregisterSurfaceNV
  :: MonadIO m
  => GLvdpauSurfaceNV -- ^ @surface@ of type @vdpauSurfaceNV@.
  -> m ()
glVDPAUUnregisterSurfaceNV v1 = liftIO $ dyn824 ptr_glVDPAUUnregisterSurfaceNV v1

{-# NOINLINE ptr_glVDPAUUnregisterSurfaceNV #-}
ptr_glVDPAUUnregisterSurfaceNV :: FunPtr (GLvdpauSurfaceNV -> IO ())
ptr_glVDPAUUnregisterSurfaceNV = unsafePerformIO $ getCommand "glVDPAUUnregisterSurfaceNV"

-- glValidateProgram -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glValidateProgram.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glValidateProgram.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glValidateProgram.xhtml OpenGL 4.x>.
glValidateProgram
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> m ()
glValidateProgram v1 = liftIO $ dyn2 ptr_glValidateProgram v1

{-# NOINLINE ptr_glValidateProgram #-}
ptr_glValidateProgram :: FunPtr (GLuint -> IO ())
ptr_glValidateProgram = unsafePerformIO $ getCommand "glValidateProgram"

-- glValidateProgramARB --------------------------------------------------------

-- | This command is an alias for 'glValidateProgram'.
glValidateProgramARB
  :: MonadIO m
  => GLhandleARB -- ^ @programObj@ of type @handleARB@.
  -> m ()
glValidateProgramARB v1 = liftIO $ dyn137 ptr_glValidateProgramARB v1

{-# NOINLINE ptr_glValidateProgramARB #-}
ptr_glValidateProgramARB :: FunPtr (GLhandleARB -> IO ())
ptr_glValidateProgramARB = unsafePerformIO $ getCommand "glValidateProgramARB"

-- glValidateProgramPipeline ---------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glValidateProgramPipeline.xhtml OpenGL 4.x>.
glValidateProgramPipeline
  :: MonadIO m
  => GLuint -- ^ @pipeline@.
  -> m ()
glValidateProgramPipeline v1 = liftIO $ dyn2 ptr_glValidateProgramPipeline v1

{-# NOINLINE ptr_glValidateProgramPipeline #-}
ptr_glValidateProgramPipeline :: FunPtr (GLuint -> IO ())
ptr_glValidateProgramPipeline = unsafePerformIO $ getCommand "glValidateProgramPipeline"

-- glValidateProgramPipelineEXT ------------------------------------------------

glValidateProgramPipelineEXT
  :: MonadIO m
  => GLuint -- ^ @pipeline@.
  -> m ()
glValidateProgramPipelineEXT v1 = liftIO $ dyn2 ptr_glValidateProgramPipelineEXT v1

{-# NOINLINE ptr_glValidateProgramPipelineEXT #-}
ptr_glValidateProgramPipelineEXT :: FunPtr (GLuint -> IO ())
ptr_glValidateProgramPipelineEXT = unsafePerformIO $ getCommand "glValidateProgramPipelineEXT"

-- glVariantArrayObjectATI -----------------------------------------------------

glVariantArrayObjectATI
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @type@ of type @ScalarType@.
  -> GLsizei -- ^ @stride@.
  -> GLuint -- ^ @buffer@.
  -> GLuint -- ^ @offset@.
  -> m ()
glVariantArrayObjectATI v1 v2 v3 v4 v5 = liftIO $ dyn825 ptr_glVariantArrayObjectATI v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVariantArrayObjectATI #-}
ptr_glVariantArrayObjectATI :: FunPtr (GLuint -> GLenum -> GLsizei -> GLuint -> GLuint -> IO ())
ptr_glVariantArrayObjectATI = unsafePerformIO $ getCommand "glVariantArrayObjectATI"

-- glVariantPointerEXT ---------------------------------------------------------

glVariantPointerEXT
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @type@ of type @ScalarType@.
  -> GLuint -- ^ @stride@.
  -> Ptr a -- ^ @addr@ pointing to @COMPSIZE(id,type,stride)@ elements of type @a@.
  -> m ()
glVariantPointerEXT v1 v2 v3 v4 = liftIO $ dyn826 ptr_glVariantPointerEXT v1 v2 v3 v4

{-# NOINLINE ptr_glVariantPointerEXT #-}
ptr_glVariantPointerEXT :: FunPtr (GLuint -> GLenum -> GLuint -> Ptr a -> IO ())
ptr_glVariantPointerEXT = unsafePerformIO $ getCommand "glVariantPointerEXT"

-- glVariantbvEXT --------------------------------------------------------------

glVariantbvEXT
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> Ptr GLbyte -- ^ @addr@ pointing to @COMPSIZE(id)@ elements of type @GLbyte@.
  -> m ()
glVariantbvEXT v1 v2 = liftIO $ dyn827 ptr_glVariantbvEXT v1 v2

{-# NOINLINE ptr_glVariantbvEXT #-}
ptr_glVariantbvEXT :: FunPtr (GLuint -> Ptr GLbyte -> IO ())
ptr_glVariantbvEXT = unsafePerformIO $ getCommand "glVariantbvEXT"

-- glVariantdvEXT --------------------------------------------------------------

glVariantdvEXT
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> Ptr GLdouble -- ^ @addr@ pointing to @COMPSIZE(id)@ elements of type @GLdouble@.
  -> m ()
glVariantdvEXT v1 v2 = liftIO $ dyn828 ptr_glVariantdvEXT v1 v2

{-# NOINLINE ptr_glVariantdvEXT #-}
ptr_glVariantdvEXT :: FunPtr (GLuint -> Ptr GLdouble -> IO ())
ptr_glVariantdvEXT = unsafePerformIO $ getCommand "glVariantdvEXT"

-- glVariantfvEXT --------------------------------------------------------------

glVariantfvEXT
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> Ptr GLfloat -- ^ @addr@ pointing to @COMPSIZE(id)@ elements of type @GLfloat@.
  -> m ()
glVariantfvEXT v1 v2 = liftIO $ dyn377 ptr_glVariantfvEXT v1 v2

{-# NOINLINE ptr_glVariantfvEXT #-}
ptr_glVariantfvEXT :: FunPtr (GLuint -> Ptr GLfloat -> IO ())
ptr_glVariantfvEXT = unsafePerformIO $ getCommand "glVariantfvEXT"

-- glVariantivEXT --------------------------------------------------------------

glVariantivEXT
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> Ptr GLint -- ^ @addr@ pointing to @COMPSIZE(id)@ elements of type @GLint@.
  -> m ()
glVariantivEXT v1 v2 = liftIO $ dyn701 ptr_glVariantivEXT v1 v2

{-# NOINLINE ptr_glVariantivEXT #-}
ptr_glVariantivEXT :: FunPtr (GLuint -> Ptr GLint -> IO ())
ptr_glVariantivEXT = unsafePerformIO $ getCommand "glVariantivEXT"

-- glVariantsvEXT --------------------------------------------------------------

glVariantsvEXT
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> Ptr GLshort -- ^ @addr@ pointing to @COMPSIZE(id)@ elements of type @GLshort@.
  -> m ()
glVariantsvEXT v1 v2 = liftIO $ dyn829 ptr_glVariantsvEXT v1 v2

{-# NOINLINE ptr_glVariantsvEXT #-}
ptr_glVariantsvEXT :: FunPtr (GLuint -> Ptr GLshort -> IO ())
ptr_glVariantsvEXT = unsafePerformIO $ getCommand "glVariantsvEXT"

-- glVariantubvEXT -------------------------------------------------------------

glVariantubvEXT
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> Ptr GLubyte -- ^ @addr@ pointing to @COMPSIZE(id)@ elements of type @GLubyte@.
  -> m ()
glVariantubvEXT v1 v2 = liftIO $ dyn376 ptr_glVariantubvEXT v1 v2

{-# NOINLINE ptr_glVariantubvEXT #-}
ptr_glVariantubvEXT :: FunPtr (GLuint -> Ptr GLubyte -> IO ())
ptr_glVariantubvEXT = unsafePerformIO $ getCommand "glVariantubvEXT"

-- glVariantuivEXT -------------------------------------------------------------

glVariantuivEXT
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> Ptr GLuint -- ^ @addr@ pointing to @COMPSIZE(id)@ elements of type @GLuint@.
  -> m ()
glVariantuivEXT v1 v2 = liftIO $ dyn194 ptr_glVariantuivEXT v1 v2

{-# NOINLINE ptr_glVariantuivEXT #-}
ptr_glVariantuivEXT :: FunPtr (GLuint -> Ptr GLuint -> IO ())
ptr_glVariantuivEXT = unsafePerformIO $ getCommand "glVariantuivEXT"

-- glVariantusvEXT -------------------------------------------------------------

glVariantusvEXT
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> Ptr GLushort -- ^ @addr@ pointing to @COMPSIZE(id)@ elements of type @GLushort@.
  -> m ()
glVariantusvEXT v1 v2 = liftIO $ dyn830 ptr_glVariantusvEXT v1 v2

{-# NOINLINE ptr_glVariantusvEXT #-}
ptr_glVariantusvEXT :: FunPtr (GLuint -> Ptr GLushort -> IO ())
ptr_glVariantusvEXT = unsafePerformIO $ getCommand "glVariantusvEXT"

-- glVertex2bOES ---------------------------------------------------------------

glVertex2bOES
  :: MonadIO m
  => GLbyte -- ^ @x@.
  -> GLbyte -- ^ @y@.
  -> m ()
glVertex2bOES v1 v2 = liftIO $ dyn722 ptr_glVertex2bOES v1 v2

{-# NOINLINE ptr_glVertex2bOES #-}
ptr_glVertex2bOES :: FunPtr (GLbyte -> GLbyte -> IO ())
ptr_glVertex2bOES = unsafePerformIO $ getCommand "glVertex2bOES"

-- glVertex2bvOES --------------------------------------------------------------

glVertex2bvOES
  :: MonadIO m
  => Ptr GLbyte -- ^ @coords@ pointing to @2@ elements of type @GLbyte@.
  -> m ()
glVertex2bvOES v1 = liftIO $ dyn37 ptr_glVertex2bvOES v1

{-# NOINLINE ptr_glVertex2bvOES #-}
ptr_glVertex2bvOES :: FunPtr (Ptr GLbyte -> IO ())
ptr_glVertex2bvOES = unsafePerformIO $ getCommand "glVertex2bvOES"

-- glVertex2d ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml OpenGL 2.x>. The vector equivalent of this command is 'glVertex2dv'.
glVertex2d
  :: MonadIO m
  => GLdouble -- ^ @x@ of type @CoordD@.
  -> GLdouble -- ^ @y@ of type @CoordD@.
  -> m ()
glVertex2d v1 v2 = liftIO $ dyn217 ptr_glVertex2d v1 v2

{-# NOINLINE ptr_glVertex2d #-}
ptr_glVertex2d :: FunPtr (GLdouble -> GLdouble -> IO ())
ptr_glVertex2d = unsafePerformIO $ getCommand "glVertex2d"

-- glVertex2dv -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml OpenGL 2.x>.
glVertex2dv
  :: MonadIO m
  => Ptr GLdouble -- ^ @v@ pointing to @2@ elements of type @CoordD@.
  -> m ()
glVertex2dv v1 = liftIO $ dyn39 ptr_glVertex2dv v1

{-# NOINLINE ptr_glVertex2dv #-}
ptr_glVertex2dv :: FunPtr (Ptr GLdouble -> IO ())
ptr_glVertex2dv = unsafePerformIO $ getCommand "glVertex2dv"

-- glVertex2f ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml OpenGL 2.x>. The vector equivalent of this command is 'glVertex2fv'.
glVertex2f
  :: MonadIO m
  => GLfloat -- ^ @x@ of type @CoordF@.
  -> GLfloat -- ^ @y@ of type @CoordF@.
  -> m ()
glVertex2f v1 v2 = liftIO $ dyn222 ptr_glVertex2f v1 v2

{-# NOINLINE ptr_glVertex2f #-}
ptr_glVertex2f :: FunPtr (GLfloat -> GLfloat -> IO ())
ptr_glVertex2f = unsafePerformIO $ getCommand "glVertex2f"

-- glVertex2fv -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml OpenGL 2.x>.
glVertex2fv
  :: MonadIO m
  => Ptr GLfloat -- ^ @v@ pointing to @2@ elements of type @CoordF@.
  -> m ()
glVertex2fv v1 = liftIO $ dyn41 ptr_glVertex2fv v1

{-# NOINLINE ptr_glVertex2fv #-}
ptr_glVertex2fv :: FunPtr (Ptr GLfloat -> IO ())
ptr_glVertex2fv = unsafePerformIO $ getCommand "glVertex2fv"

-- glVertex2hNV ----------------------------------------------------------------

-- | The vector equivalent of this command is 'glVertex2hvNV'.
glVertex2hNV
  :: MonadIO m
  => GLhalfNV -- ^ @x@ of type @Half16NV@.
  -> GLhalfNV -- ^ @y@ of type @Half16NV@.
  -> m ()
glVertex2hNV v1 v2 = liftIO $ dyn727 ptr_glVertex2hNV v1 v2

{-# NOINLINE ptr_glVertex2hNV #-}
ptr_glVertex2hNV :: FunPtr (GLhalfNV -> GLhalfNV -> IO ())
ptr_glVertex2hNV = unsafePerformIO $ getCommand "glVertex2hNV"

-- glVertex2hvNV ---------------------------------------------------------------

glVertex2hvNV
  :: MonadIO m
  => Ptr GLhalfNV -- ^ @v@ pointing to @2@ elements of type @Half16NV@.
  -> m ()
glVertex2hvNV v1 = liftIO $ dyn99 ptr_glVertex2hvNV v1

{-# NOINLINE ptr_glVertex2hvNV #-}
ptr_glVertex2hvNV :: FunPtr (Ptr GLhalfNV -> IO ())
ptr_glVertex2hvNV = unsafePerformIO $ getCommand "glVertex2hvNV"

-- glVertex2i ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml OpenGL 2.x>. The vector equivalent of this command is 'glVertex2iv'.
glVertex2i
  :: MonadIO m
  => GLint -- ^ @x@ of type @CoordI@.
  -> GLint -- ^ @y@ of type @CoordI@.
  -> m ()
glVertex2i v1 v2 = liftIO $ dyn266 ptr_glVertex2i v1 v2

{-# NOINLINE ptr_glVertex2i #-}
ptr_glVertex2i :: FunPtr (GLint -> GLint -> IO ())
ptr_glVertex2i = unsafePerformIO $ getCommand "glVertex2i"

-- glVertex2iv -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml OpenGL 2.x>.
glVertex2iv
  :: MonadIO m
  => Ptr GLint -- ^ @v@ pointing to @2@ elements of type @CoordI@.
  -> m ()
glVertex2iv v1 = liftIO $ dyn43 ptr_glVertex2iv v1

{-# NOINLINE ptr_glVertex2iv #-}
ptr_glVertex2iv :: FunPtr (Ptr GLint -> IO ())
ptr_glVertex2iv = unsafePerformIO $ getCommand "glVertex2iv"

-- glVertex2s ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml OpenGL 2.x>. The vector equivalent of this command is 'glVertex2sv'.
glVertex2s
  :: MonadIO m
  => GLshort -- ^ @x@ of type @CoordS@.
  -> GLshort -- ^ @y@ of type @CoordS@.
  -> m ()
glVertex2s v1 v2 = liftIO $ dyn669 ptr_glVertex2s v1 v2

{-# NOINLINE ptr_glVertex2s #-}
ptr_glVertex2s :: FunPtr (GLshort -> GLshort -> IO ())
ptr_glVertex2s = unsafePerformIO $ getCommand "glVertex2s"

-- glVertex2sv -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertex.xml OpenGL 2.x>.
glVertex2sv
  :: MonadIO m
  => Ptr GLshort -- ^ @v@ pointing to @2@ elements of type @CoordS@.
  -> m ()
glVertex2sv v1 = liftIO $ dyn45 ptr_glVertex2sv v1

{-# NOINLINE ptr_glVertex2sv #-}
ptr_glVertex2sv :: FunPtr (Ptr GLshort -> IO ())
ptr_glVertex2sv = unsafePerformIO $ getCommand "glVertex2sv"

-- glVertex2xOES ---------------------------------------------------------------

glVertex2xOES
  :: MonadIO m
  => GLfixed -- ^ @x@.
  -> m ()
glVertex2xOES v1 = liftIO $ dyn81 ptr_glVertex2xOES v1

{-# NOINLINE ptr_glVertex2xOES #-}
ptr_glVertex2xOES :: FunPtr (GLfixed -> IO ())
ptr_glVertex2xOES = unsafePerformIO $ getCommand "glVertex2xOES"

-- glVertex2xvOES --------------------------------------------------------------

glVertex2xvOES
  :: MonadIO m
  => Ptr GLfixed -- ^ @coords@ pointing to @2@ elements of type @GLfixed@.
  -> m ()
glVertex2xvOES v1 = liftIO $ dyn107 ptr_glVertex2xvOES v1

{-# NOINLINE ptr_glVertex2xvOES #-}
ptr_glVertex2xvOES :: FunPtr (Ptr GLfixed -> IO ())
ptr_glVertex2xvOES = unsafePerformIO $ getCommand "glVertex2xvOES"

-- glVertex3bOES ---------------------------------------------------------------

glVertex3bOES
  :: MonadIO m
  => GLbyte -- ^ @x@.
  -> GLbyte -- ^ @y@.
  -> GLbyte -- ^ @z@.
  -> m ()
glVertex3bOES v1 v2 v3 = liftIO $ dyn36 ptr_glVertex3bOES v1 v2 v3

{-# NOINLINE ptr_glVertex3bOES #-}
ptr_glVertex3bOES :: FunPtr (GLbyte -> GLbyte -> GLbyte -> IO ())
ptr_glVertex3bOES = unsafePerformIO $ getCommand "glVertex3bOES"

