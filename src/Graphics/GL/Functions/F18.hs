--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F18
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

module Graphics.GL.Functions.F18 (
  glFlushPixelDataRangeNV,
  glFlushRasterSGIX,
  glFlushStaticDataIBM,
  glFlushVertexArrayRangeAPPLE,
  glFlushVertexArrayRangeNV,
  glFogCoordFormatNV,
  glFogCoordPointer,
  glFogCoordPointerEXT,
  glFogCoordPointerListIBM,
  glFogCoordd,
  glFogCoorddEXT,
  glFogCoorddv,
  glFogCoorddvEXT,
  glFogCoordf,
  glFogCoordfEXT,
  glFogCoordfv,
  glFogCoordfvEXT,
  glFogCoordhNV,
  glFogCoordhvNV,
  glFogFuncSGIS,
  glFogf,
  glFogfv,
  glFogi,
  glFogiv,
  glFogx,
  glFogxOES,
  glFogxv,
  glFogxvOES,
  glFragmentColorMaterialSGIX,
  glFragmentCoverageColorNV,
  glFragmentLightModelfSGIX,
  glFragmentLightModelfvSGIX,
  glFragmentLightModeliSGIX,
  glFragmentLightModelivSGIX,
  glFragmentLightfSGIX,
  glFragmentLightfvSGIX,
  glFragmentLightiSGIX,
  glFragmentLightivSGIX,
  glFragmentMaterialfSGIX,
  glFragmentMaterialfvSGIX
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

-- glFlushPixelDataRangeNV -----------------------------------------------------

glFlushPixelDataRangeNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type @PixelDataRangeTargetNV@.
  -> m ()
glFlushPixelDataRangeNV v1 = liftIO $ dyn4 ptr_glFlushPixelDataRangeNV v1

{-# NOINLINE ptr_glFlushPixelDataRangeNV #-}
ptr_glFlushPixelDataRangeNV :: FunPtr (GLenum -> IO ())
ptr_glFlushPixelDataRangeNV = unsafePerformIO $ getCommand "glFlushPixelDataRangeNV"

-- glFlushRasterSGIX -----------------------------------------------------------

glFlushRasterSGIX
  :: MonadIO m
  => m ()
glFlushRasterSGIX = liftIO $ dyn10 ptr_glFlushRasterSGIX

{-# NOINLINE ptr_glFlushRasterSGIX #-}
ptr_glFlushRasterSGIX :: FunPtr (IO ())
ptr_glFlushRasterSGIX = unsafePerformIO $ getCommand "glFlushRasterSGIX"

-- glFlushStaticDataIBM --------------------------------------------------------

glFlushStaticDataIBM
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> m ()
glFlushStaticDataIBM v1 = liftIO $ dyn4 ptr_glFlushStaticDataIBM v1

{-# NOINLINE ptr_glFlushStaticDataIBM #-}
ptr_glFlushStaticDataIBM :: FunPtr (GLenum -> IO ())
ptr_glFlushStaticDataIBM = unsafePerformIO $ getCommand "glFlushStaticDataIBM"

-- glFlushVertexArrayRangeAPPLE ------------------------------------------------

glFlushVertexArrayRangeAPPLE
  :: MonadIO m
  => GLsizei -- ^ @length@.
  -> Ptr a -- ^ @pointer@ pointing to @length@ elements of type @a@.
  -> m ()
glFlushVertexArrayRangeAPPLE v1 v2 = liftIO $ dyn260 ptr_glFlushVertexArrayRangeAPPLE v1 v2

{-# NOINLINE ptr_glFlushVertexArrayRangeAPPLE #-}
ptr_glFlushVertexArrayRangeAPPLE :: FunPtr (GLsizei -> Ptr a -> IO ())
ptr_glFlushVertexArrayRangeAPPLE = unsafePerformIO $ getCommand "glFlushVertexArrayRangeAPPLE"

-- glFlushVertexArrayRangeNV ---------------------------------------------------

glFlushVertexArrayRangeNV
  :: MonadIO m
  => m ()
glFlushVertexArrayRangeNV = liftIO $ dyn10 ptr_glFlushVertexArrayRangeNV

{-# NOINLINE ptr_glFlushVertexArrayRangeNV #-}
ptr_glFlushVertexArrayRangeNV :: FunPtr (IO ())
ptr_glFlushVertexArrayRangeNV = unsafePerformIO $ getCommand "glFlushVertexArrayRangeNV"

-- glFogCoordFormatNV ----------------------------------------------------------

glFogCoordFormatNV
  :: MonadIO m
  => GLenum -- ^ @type@.
  -> GLsizei -- ^ @stride@.
  -> m ()
glFogCoordFormatNV v1 v2 = liftIO $ dyn239 ptr_glFogCoordFormatNV v1 v2

{-# NOINLINE ptr_glFogCoordFormatNV #-}
ptr_glFogCoordFormatNV :: FunPtr (GLenum -> GLsizei -> IO ())
ptr_glFogCoordFormatNV = unsafePerformIO $ getCommand "glFogCoordFormatNV"

-- glFogCoordPointer -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glFogCoordPointer.xml OpenGL 2.x>.
glFogCoordPointer
  :: MonadIO m
  => GLenum -- ^ @type@ of type [FogPointerTypeEXT](Graphics-GL-Groups.html#FogPointerTypeEXT).
  -> GLsizei -- ^ @stride@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(type,stride)@ elements of type @a@.
  -> m ()
glFogCoordPointer v1 v2 v3 = liftIO $ dyn46 ptr_glFogCoordPointer v1 v2 v3

{-# NOINLINE ptr_glFogCoordPointer #-}
ptr_glFogCoordPointer :: FunPtr (GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glFogCoordPointer = unsafePerformIO $ getCommand "glFogCoordPointer"

-- glFogCoordPointerEXT --------------------------------------------------------

-- | This command is an alias for 'glFogCoordPointer'.
glFogCoordPointerEXT
  :: MonadIO m
  => GLenum -- ^ @type@ of type [FogPointerTypeEXT](Graphics-GL-Groups.html#FogPointerTypeEXT).
  -> GLsizei -- ^ @stride@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(type,stride)@ elements of type @a@.
  -> m ()
glFogCoordPointerEXT v1 v2 v3 = liftIO $ dyn46 ptr_glFogCoordPointerEXT v1 v2 v3

{-# NOINLINE ptr_glFogCoordPointerEXT #-}
ptr_glFogCoordPointerEXT :: FunPtr (GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glFogCoordPointerEXT = unsafePerformIO $ getCommand "glFogCoordPointerEXT"

-- glFogCoordPointerListIBM ----------------------------------------------------

glFogCoordPointerListIBM
  :: MonadIO m
  => GLenum -- ^ @type@ of type [FogPointerTypeIBM](Graphics-GL-Groups.html#FogPointerTypeIBM).
  -> GLint -- ^ @stride@.
  -> Ptr (Ptr a) -- ^ @pointer@ pointing to @COMPSIZE(type,stride)@ elements of type @Ptr a@.
  -> GLint -- ^ @ptrstride@.
  -> m ()
glFogCoordPointerListIBM v1 v2 v3 v4 = liftIO $ dyn280 ptr_glFogCoordPointerListIBM v1 v2 v3 v4

{-# NOINLINE ptr_glFogCoordPointerListIBM #-}
ptr_glFogCoordPointerListIBM :: FunPtr (GLenum -> GLint -> Ptr (Ptr a) -> GLint -> IO ())
ptr_glFogCoordPointerListIBM = unsafePerformIO $ getCommand "glFogCoordPointerListIBM"

-- glFogCoordd -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glFogCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glFogCoorddv'.
glFogCoordd
  :: MonadIO m
  => GLdouble -- ^ @coord@ of type @CoordD@.
  -> m ()
glFogCoordd v1 = liftIO $ dyn78 ptr_glFogCoordd v1

{-# NOINLINE ptr_glFogCoordd #-}
ptr_glFogCoordd :: FunPtr (GLdouble -> IO ())
ptr_glFogCoordd = unsafePerformIO $ getCommand "glFogCoordd"

-- glFogCoorddEXT --------------------------------------------------------------

-- | The vector equivalent of this command is 'glFogCoorddvEXT'. This command is an alias for 'glFogCoordd'.
glFogCoorddEXT
  :: MonadIO m
  => GLdouble -- ^ @coord@ of type @CoordD@.
  -> m ()
glFogCoorddEXT v1 = liftIO $ dyn78 ptr_glFogCoorddEXT v1

{-# NOINLINE ptr_glFogCoorddEXT #-}
ptr_glFogCoorddEXT :: FunPtr (GLdouble -> IO ())
ptr_glFogCoorddEXT = unsafePerformIO $ getCommand "glFogCoorddEXT"

-- glFogCoorddv ----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glFogCoord.xml OpenGL 2.x>.
glFogCoorddv
  :: MonadIO m
  => Ptr GLdouble -- ^ @coord@ pointing to @1@ element of type @CoordD@.
  -> m ()
glFogCoorddv v1 = liftIO $ dyn39 ptr_glFogCoorddv v1

{-# NOINLINE ptr_glFogCoorddv #-}
ptr_glFogCoorddv :: FunPtr (Ptr GLdouble -> IO ())
ptr_glFogCoorddv = unsafePerformIO $ getCommand "glFogCoorddv"

-- glFogCoorddvEXT -------------------------------------------------------------

-- | This command is an alias for 'glFogCoorddv'.
glFogCoorddvEXT
  :: MonadIO m
  => Ptr GLdouble -- ^ @coord@ pointing to @1@ element of type @CoordD@.
  -> m ()
glFogCoorddvEXT v1 = liftIO $ dyn39 ptr_glFogCoorddvEXT v1

{-# NOINLINE ptr_glFogCoorddvEXT #-}
ptr_glFogCoorddvEXT :: FunPtr (Ptr GLdouble -> IO ())
ptr_glFogCoorddvEXT = unsafePerformIO $ getCommand "glFogCoorddvEXT"

-- glFogCoordf -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glFogCoord.xml OpenGL 2.x>. The vector equivalent of this command is 'glFogCoordfv'.
glFogCoordf
  :: MonadIO m
  => GLfloat -- ^ @coord@ of type @CoordF@.
  -> m ()
glFogCoordf v1 = liftIO $ dyn79 ptr_glFogCoordf v1

{-# NOINLINE ptr_glFogCoordf #-}
ptr_glFogCoordf :: FunPtr (GLfloat -> IO ())
ptr_glFogCoordf = unsafePerformIO $ getCommand "glFogCoordf"

-- glFogCoordfEXT --------------------------------------------------------------

-- | The vector equivalent of this command is 'glFogCoordfvEXT'. This command is an alias for 'glFogCoordf'.
glFogCoordfEXT
  :: MonadIO m
  => GLfloat -- ^ @coord@ of type @CoordF@.
  -> m ()
glFogCoordfEXT v1 = liftIO $ dyn79 ptr_glFogCoordfEXT v1

{-# NOINLINE ptr_glFogCoordfEXT #-}
ptr_glFogCoordfEXT :: FunPtr (GLfloat -> IO ())
ptr_glFogCoordfEXT = unsafePerformIO $ getCommand "glFogCoordfEXT"

-- glFogCoordfv ----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glFogCoord.xml OpenGL 2.x>.
glFogCoordfv
  :: MonadIO m
  => Ptr GLfloat -- ^ @coord@ pointing to @1@ element of type @CoordF@.
  -> m ()
glFogCoordfv v1 = liftIO $ dyn41 ptr_glFogCoordfv v1

{-# NOINLINE ptr_glFogCoordfv #-}
ptr_glFogCoordfv :: FunPtr (Ptr GLfloat -> IO ())
ptr_glFogCoordfv = unsafePerformIO $ getCommand "glFogCoordfv"

-- glFogCoordfvEXT -------------------------------------------------------------

-- | This command is an alias for 'glFogCoordfv'.
glFogCoordfvEXT
  :: MonadIO m
  => Ptr GLfloat -- ^ @coord@ pointing to @1@ element of type @CoordF@.
  -> m ()
glFogCoordfvEXT v1 = liftIO $ dyn41 ptr_glFogCoordfvEXT v1

{-# NOINLINE ptr_glFogCoordfvEXT #-}
ptr_glFogCoordfvEXT :: FunPtr (Ptr GLfloat -> IO ())
ptr_glFogCoordfvEXT = unsafePerformIO $ getCommand "glFogCoordfvEXT"

-- glFogCoordhNV ---------------------------------------------------------------

-- | The vector equivalent of this command is 'glFogCoordhvNV'.
glFogCoordhNV
  :: MonadIO m
  => GLhalfNV -- ^ @fog@ of type @Half16NV@.
  -> m ()
glFogCoordhNV v1 = liftIO $ dyn281 ptr_glFogCoordhNV v1

{-# NOINLINE ptr_glFogCoordhNV #-}
ptr_glFogCoordhNV :: FunPtr (GLhalfNV -> IO ())
ptr_glFogCoordhNV = unsafePerformIO $ getCommand "glFogCoordhNV"

-- glFogCoordhvNV --------------------------------------------------------------

glFogCoordhvNV
  :: MonadIO m
  => Ptr GLhalfNV -- ^ @fog@ pointing to @1@ element of type @Half16NV@.
  -> m ()
glFogCoordhvNV v1 = liftIO $ dyn99 ptr_glFogCoordhvNV v1

{-# NOINLINE ptr_glFogCoordhvNV #-}
ptr_glFogCoordhvNV :: FunPtr (Ptr GLhalfNV -> IO ())
ptr_glFogCoordhvNV = unsafePerformIO $ getCommand "glFogCoordhvNV"

-- glFogFuncSGIS ---------------------------------------------------------------

glFogFuncSGIS
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLfloat -- ^ @points@ pointing to @n*2@ elements of type @GLfloat@.
  -> m ()
glFogFuncSGIS v1 v2 = liftIO $ dyn192 ptr_glFogFuncSGIS v1 v2

{-# NOINLINE ptr_glFogFuncSGIS #-}
ptr_glFogFuncSGIS :: FunPtr (GLsizei -> Ptr GLfloat -> IO ())
ptr_glFogFuncSGIS = unsafePerformIO $ getCommand "glFogFuncSGIS"

-- glFogf ----------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glFog.xml OpenGL 2.x>.
glFogf
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [FogParameter](Graphics-GL-Groups.html#FogParameter).
  -> GLfloat -- ^ @param@ of type @CheckedFloat32@.
  -> m ()
glFogf v1 v2 = liftIO $ dyn0 ptr_glFogf v1 v2

{-# NOINLINE ptr_glFogf #-}
ptr_glFogf :: FunPtr (GLenum -> GLfloat -> IO ())
ptr_glFogf = unsafePerformIO $ getCommand "glFogf"

-- glFogfv ---------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glFog.xml OpenGL 2.x>.
glFogfv
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [FogParameter](Graphics-GL-Groups.html#FogParameter).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedFloat32@.
  -> m ()
glFogfv v1 v2 = liftIO $ dyn94 ptr_glFogfv v1 v2

{-# NOINLINE ptr_glFogfv #-}
ptr_glFogfv :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glFogfv = unsafePerformIO $ getCommand "glFogfv"

-- glFogi ----------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glFog.xml OpenGL 2.x>.
glFogi
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [FogParameter](Graphics-GL-Groups.html#FogParameter).
  -> GLint -- ^ @param@ of type @CheckedInt32@.
  -> m ()
glFogi v1 v2 = liftIO $ dyn55 ptr_glFogi v1 v2

{-# NOINLINE ptr_glFogi #-}
ptr_glFogi :: FunPtr (GLenum -> GLint -> IO ())
ptr_glFogi = unsafePerformIO $ getCommand "glFogi"

-- glFogiv ---------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glFog.xml OpenGL 2.x>.
glFogiv
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [FogParameter](Graphics-GL-Groups.html#FogParameter).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedInt32@.
  -> m ()
glFogiv v1 v2 = liftIO $ dyn136 ptr_glFogiv v1 v2

{-# NOINLINE ptr_glFogiv #-}
ptr_glFogiv :: FunPtr (GLenum -> Ptr GLint -> IO ())
ptr_glFogiv = unsafePerformIO $ getCommand "glFogiv"

-- glFogx ----------------------------------------------------------------------

glFogx
  :: MonadIO m
  => GLenum -- ^ @pname@.
  -> GLfixed -- ^ @param@.
  -> m ()
glFogx v1 v2 = liftIO $ dyn1 ptr_glFogx v1 v2

{-# NOINLINE ptr_glFogx #-}
ptr_glFogx :: FunPtr (GLenum -> GLfixed -> IO ())
ptr_glFogx = unsafePerformIO $ getCommand "glFogx"

-- glFogxOES -------------------------------------------------------------------

glFogxOES
  :: MonadIO m
  => GLenum -- ^ @pname@.
  -> GLfixed -- ^ @param@.
  -> m ()
glFogxOES v1 v2 = liftIO $ dyn1 ptr_glFogxOES v1 v2

{-# NOINLINE ptr_glFogxOES #-}
ptr_glFogxOES :: FunPtr (GLenum -> GLfixed -> IO ())
ptr_glFogxOES = unsafePerformIO $ getCommand "glFogxOES"

-- glFogxv ---------------------------------------------------------------------

glFogxv
  :: MonadIO m
  => GLenum -- ^ @pname@.
  -> Ptr GLfixed -- ^ @param@ pointing to @COMPSIZE(pname)@ elements of type @GLfixed@.
  -> m ()
glFogxv v1 v2 = liftIO $ dyn95 ptr_glFogxv v1 v2

{-# NOINLINE ptr_glFogxv #-}
ptr_glFogxv :: FunPtr (GLenum -> Ptr GLfixed -> IO ())
ptr_glFogxv = unsafePerformIO $ getCommand "glFogxv"

-- glFogxvOES ------------------------------------------------------------------

glFogxvOES
  :: MonadIO m
  => GLenum -- ^ @pname@.
  -> Ptr GLfixed -- ^ @param@ pointing to @COMPSIZE(pname)@ elements of type @GLfixed@.
  -> m ()
glFogxvOES v1 v2 = liftIO $ dyn95 ptr_glFogxvOES v1 v2

{-# NOINLINE ptr_glFogxvOES #-}
ptr_glFogxvOES :: FunPtr (GLenum -> Ptr GLfixed -> IO ())
ptr_glFogxvOES = unsafePerformIO $ getCommand "glFogxvOES"

-- glFragmentColorMaterialSGIX -------------------------------------------------

glFragmentColorMaterialSGIX
  :: MonadIO m
  => GLenum -- ^ @face@ of type [MaterialFace](Graphics-GL-Groups.html#MaterialFace).
  -> GLenum -- ^ @mode@ of type [MaterialParameter](Graphics-GL-Groups.html#MaterialParameter).
  -> m ()
glFragmentColorMaterialSGIX v1 v2 = liftIO $ dyn51 ptr_glFragmentColorMaterialSGIX v1 v2

{-# NOINLINE ptr_glFragmentColorMaterialSGIX #-}
ptr_glFragmentColorMaterialSGIX :: FunPtr (GLenum -> GLenum -> IO ())
ptr_glFragmentColorMaterialSGIX = unsafePerformIO $ getCommand "glFragmentColorMaterialSGIX"

-- glFragmentCoverageColorNV ---------------------------------------------------

glFragmentCoverageColorNV
  :: MonadIO m
  => GLuint -- ^ @color@.
  -> m ()
glFragmentCoverageColorNV v1 = liftIO $ dyn2 ptr_glFragmentCoverageColorNV v1

{-# NOINLINE ptr_glFragmentCoverageColorNV #-}
ptr_glFragmentCoverageColorNV :: FunPtr (GLuint -> IO ())
ptr_glFragmentCoverageColorNV = unsafePerformIO $ getCommand "glFragmentCoverageColorNV"

-- glFragmentLightModelfSGIX ---------------------------------------------------

glFragmentLightModelfSGIX
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [FragmentLightModelParameterSGIX](Graphics-GL-Groups.html#FragmentLightModelParameterSGIX).
  -> GLfloat -- ^ @param@ of type @CheckedFloat32@.
  -> m ()
glFragmentLightModelfSGIX v1 v2 = liftIO $ dyn0 ptr_glFragmentLightModelfSGIX v1 v2

{-# NOINLINE ptr_glFragmentLightModelfSGIX #-}
ptr_glFragmentLightModelfSGIX :: FunPtr (GLenum -> GLfloat -> IO ())
ptr_glFragmentLightModelfSGIX = unsafePerformIO $ getCommand "glFragmentLightModelfSGIX"

-- glFragmentLightModelfvSGIX --------------------------------------------------

glFragmentLightModelfvSGIX
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [FragmentLightModelParameterSGIX](Graphics-GL-Groups.html#FragmentLightModelParameterSGIX).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedFloat32@.
  -> m ()
glFragmentLightModelfvSGIX v1 v2 = liftIO $ dyn94 ptr_glFragmentLightModelfvSGIX v1 v2

{-# NOINLINE ptr_glFragmentLightModelfvSGIX #-}
ptr_glFragmentLightModelfvSGIX :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glFragmentLightModelfvSGIX = unsafePerformIO $ getCommand "glFragmentLightModelfvSGIX"

-- glFragmentLightModeliSGIX ---------------------------------------------------

glFragmentLightModeliSGIX
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [FragmentLightModelParameterSGIX](Graphics-GL-Groups.html#FragmentLightModelParameterSGIX).
  -> GLint -- ^ @param@ of type @CheckedInt32@.
  -> m ()
glFragmentLightModeliSGIX v1 v2 = liftIO $ dyn55 ptr_glFragmentLightModeliSGIX v1 v2

{-# NOINLINE ptr_glFragmentLightModeliSGIX #-}
ptr_glFragmentLightModeliSGIX :: FunPtr (GLenum -> GLint -> IO ())
ptr_glFragmentLightModeliSGIX = unsafePerformIO $ getCommand "glFragmentLightModeliSGIX"

-- glFragmentLightModelivSGIX --------------------------------------------------

glFragmentLightModelivSGIX
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [FragmentLightModelParameterSGIX](Graphics-GL-Groups.html#FragmentLightModelParameterSGIX).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedInt32@.
  -> m ()
glFragmentLightModelivSGIX v1 v2 = liftIO $ dyn136 ptr_glFragmentLightModelivSGIX v1 v2

{-# NOINLINE ptr_glFragmentLightModelivSGIX #-}
ptr_glFragmentLightModelivSGIX :: FunPtr (GLenum -> Ptr GLint -> IO ())
ptr_glFragmentLightModelivSGIX = unsafePerformIO $ getCommand "glFragmentLightModelivSGIX"

-- glFragmentLightfSGIX --------------------------------------------------------

glFragmentLightfSGIX
  :: MonadIO m
  => GLenum -- ^ @light@ of type @FragmentLightNameSGIX@.
  -> GLenum -- ^ @pname@ of type @FragmentLightParameterSGIX@.
  -> GLfloat -- ^ @param@ of type @CheckedFloat32@.
  -> m ()
glFragmentLightfSGIX v1 v2 v3 = liftIO $ dyn161 ptr_glFragmentLightfSGIX v1 v2 v3

{-# NOINLINE ptr_glFragmentLightfSGIX #-}
ptr_glFragmentLightfSGIX :: FunPtr (GLenum -> GLenum -> GLfloat -> IO ())
ptr_glFragmentLightfSGIX = unsafePerformIO $ getCommand "glFragmentLightfSGIX"

-- glFragmentLightfvSGIX -------------------------------------------------------

glFragmentLightfvSGIX
  :: MonadIO m
  => GLenum -- ^ @light@ of type @FragmentLightNameSGIX@.
  -> GLenum -- ^ @pname@ of type @FragmentLightParameterSGIX@.
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedFloat32@.
  -> m ()
glFragmentLightfvSGIX v1 v2 v3 = liftIO $ dyn132 ptr_glFragmentLightfvSGIX v1 v2 v3

{-# NOINLINE ptr_glFragmentLightfvSGIX #-}
ptr_glFragmentLightfvSGIX :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glFragmentLightfvSGIX = unsafePerformIO $ getCommand "glFragmentLightfvSGIX"

-- glFragmentLightiSGIX --------------------------------------------------------

glFragmentLightiSGIX
  :: MonadIO m
  => GLenum -- ^ @light@ of type @FragmentLightNameSGIX@.
  -> GLenum -- ^ @pname@ of type @FragmentLightParameterSGIX@.
  -> GLint -- ^ @param@ of type @CheckedInt32@.
  -> m ()
glFragmentLightiSGIX v1 v2 v3 = liftIO $ dyn62 ptr_glFragmentLightiSGIX v1 v2 v3

{-# NOINLINE ptr_glFragmentLightiSGIX #-}
ptr_glFragmentLightiSGIX :: FunPtr (GLenum -> GLenum -> GLint -> IO ())
ptr_glFragmentLightiSGIX = unsafePerformIO $ getCommand "glFragmentLightiSGIX"

-- glFragmentLightivSGIX -------------------------------------------------------

glFragmentLightivSGIX
  :: MonadIO m
  => GLenum -- ^ @light@ of type @FragmentLightNameSGIX@.
  -> GLenum -- ^ @pname@ of type @FragmentLightParameterSGIX@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedInt32@.
  -> m ()
glFragmentLightivSGIX v1 v2 v3 = liftIO $ dyn133 ptr_glFragmentLightivSGIX v1 v2 v3

{-# NOINLINE ptr_glFragmentLightivSGIX #-}
ptr_glFragmentLightivSGIX :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glFragmentLightivSGIX = unsafePerformIO $ getCommand "glFragmentLightivSGIX"

-- glFragmentMaterialfSGIX -----------------------------------------------------

glFragmentMaterialfSGIX
  :: MonadIO m
  => GLenum -- ^ @face@ of type [MaterialFace](Graphics-GL-Groups.html#MaterialFace).
  -> GLenum -- ^ @pname@ of type [MaterialParameter](Graphics-GL-Groups.html#MaterialParameter).
  -> GLfloat -- ^ @param@ of type @CheckedFloat32@.
  -> m ()
glFragmentMaterialfSGIX v1 v2 v3 = liftIO $ dyn161 ptr_glFragmentMaterialfSGIX v1 v2 v3

{-# NOINLINE ptr_glFragmentMaterialfSGIX #-}
ptr_glFragmentMaterialfSGIX :: FunPtr (GLenum -> GLenum -> GLfloat -> IO ())
ptr_glFragmentMaterialfSGIX = unsafePerformIO $ getCommand "glFragmentMaterialfSGIX"

-- glFragmentMaterialfvSGIX ----------------------------------------------------

glFragmentMaterialfvSGIX
  :: MonadIO m
  => GLenum -- ^ @face@ of type [MaterialFace](Graphics-GL-Groups.html#MaterialFace).
  -> GLenum -- ^ @pname@ of type [MaterialParameter](Graphics-GL-Groups.html#MaterialParameter).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedFloat32@.
  -> m ()
glFragmentMaterialfvSGIX v1 v2 v3 = liftIO $ dyn132 ptr_glFragmentMaterialfvSGIX v1 v2 v3

{-# NOINLINE ptr_glFragmentMaterialfvSGIX #-}
ptr_glFragmentMaterialfvSGIX :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glFragmentMaterialfvSGIX = unsafePerformIO $ getCommand "glFragmentMaterialfvSGIX"

