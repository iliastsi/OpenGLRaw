--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F36
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

module Graphics.GL.Functions.F36 (
  glLabelObjectEXT,
  glLightEnviSGIX,
  glLightModelf,
  glLightModelfv,
  glLightModeli,
  glLightModeliv,
  glLightModelx,
  glLightModelxOES,
  glLightModelxv,
  glLightModelxvOES,
  glLightf,
  glLightfv,
  glLighti,
  glLightiv,
  glLightx,
  glLightxOES,
  glLightxv,
  glLightxvOES,
  glLineStipple,
  glLineWidth,
  glLineWidthx,
  glLineWidthxOES,
  glLinkProgram,
  glLinkProgramARB,
  glListBase,
  glListDrawCommandsStatesClientNV,
  glListParameterfSGIX,
  glListParameterfvSGIX,
  glListParameteriSGIX,
  glListParameterivSGIX,
  glLoadIdentity,
  glLoadIdentityDeformationMapSGIX,
  glLoadMatrixd,
  glLoadMatrixf,
  glLoadMatrixx,
  glLoadMatrixxOES,
  glLoadName,
  glLoadPaletteFromModelViewMatrixOES,
  glLoadProgramNV,
  glLoadTransposeMatrixd
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

-- glLabelObjectEXT ------------------------------------------------------------

glLabelObjectEXT
  :: MonadIO m
  => GLenum -- ^ @type@.
  -> GLuint -- ^ @object@.
  -> GLsizei -- ^ @length@.
  -> Ptr GLchar -- ^ @label@.
  -> m ()
glLabelObjectEXT v1 v2 v3 v4 = liftIO $ dyn484 ptr_glLabelObjectEXT v1 v2 v3 v4

{-# NOINLINE ptr_glLabelObjectEXT #-}
ptr_glLabelObjectEXT :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLchar -> IO ())
ptr_glLabelObjectEXT = unsafePerformIO $ getCommand "glLabelObjectEXT"

-- glLightEnviSGIX -------------------------------------------------------------

glLightEnviSGIX
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [LightEnvParameterSGIX](Graphics-GL-Groups.html#LightEnvParameterSGIX).
  -> GLint -- ^ @param@ of type @CheckedInt32@.
  -> m ()
glLightEnviSGIX v1 v2 = liftIO $ dyn55 ptr_glLightEnviSGIX v1 v2

{-# NOINLINE ptr_glLightEnviSGIX #-}
ptr_glLightEnviSGIX :: FunPtr (GLenum -> GLint -> IO ())
ptr_glLightEnviSGIX = unsafePerformIO $ getCommand "glLightEnviSGIX"

-- glLightModelf ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glLightModel.xml OpenGL 2.x>.
glLightModelf
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [LightModelParameter](Graphics-GL-Groups.html#LightModelParameter).
  -> GLfloat -- ^ @param@.
  -> m ()
glLightModelf v1 v2 = liftIO $ dyn0 ptr_glLightModelf v1 v2

{-# NOINLINE ptr_glLightModelf #-}
ptr_glLightModelf :: FunPtr (GLenum -> GLfloat -> IO ())
ptr_glLightModelf = unsafePerformIO $ getCommand "glLightModelf"

-- glLightModelfv --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glLightModel.xml OpenGL 2.x>.
glLightModelfv
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [LightModelParameter](Graphics-GL-Groups.html#LightModelParameter).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glLightModelfv v1 v2 = liftIO $ dyn94 ptr_glLightModelfv v1 v2

{-# NOINLINE ptr_glLightModelfv #-}
ptr_glLightModelfv :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glLightModelfv = unsafePerformIO $ getCommand "glLightModelfv"

-- glLightModeli ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glLightModel.xml OpenGL 2.x>.
glLightModeli
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [LightModelParameter](Graphics-GL-Groups.html#LightModelParameter).
  -> GLint -- ^ @param@.
  -> m ()
glLightModeli v1 v2 = liftIO $ dyn55 ptr_glLightModeli v1 v2

{-# NOINLINE ptr_glLightModeli #-}
ptr_glLightModeli :: FunPtr (GLenum -> GLint -> IO ())
ptr_glLightModeli = unsafePerformIO $ getCommand "glLightModeli"

-- glLightModeliv --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glLightModel.xml OpenGL 2.x>.
glLightModeliv
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [LightModelParameter](Graphics-GL-Groups.html#LightModelParameter).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glLightModeliv v1 v2 = liftIO $ dyn136 ptr_glLightModeliv v1 v2

{-# NOINLINE ptr_glLightModeliv #-}
ptr_glLightModeliv :: FunPtr (GLenum -> Ptr GLint -> IO ())
ptr_glLightModeliv = unsafePerformIO $ getCommand "glLightModeliv"

-- glLightModelx ---------------------------------------------------------------

glLightModelx
  :: MonadIO m
  => GLenum -- ^ @pname@.
  -> GLfixed -- ^ @param@.
  -> m ()
glLightModelx v1 v2 = liftIO $ dyn1 ptr_glLightModelx v1 v2

{-# NOINLINE ptr_glLightModelx #-}
ptr_glLightModelx :: FunPtr (GLenum -> GLfixed -> IO ())
ptr_glLightModelx = unsafePerformIO $ getCommand "glLightModelx"

-- glLightModelxOES ------------------------------------------------------------

glLightModelxOES
  :: MonadIO m
  => GLenum -- ^ @pname@.
  -> GLfixed -- ^ @param@.
  -> m ()
glLightModelxOES v1 v2 = liftIO $ dyn1 ptr_glLightModelxOES v1 v2

{-# NOINLINE ptr_glLightModelxOES #-}
ptr_glLightModelxOES :: FunPtr (GLenum -> GLfixed -> IO ())
ptr_glLightModelxOES = unsafePerformIO $ getCommand "glLightModelxOES"

-- glLightModelxv --------------------------------------------------------------

glLightModelxv
  :: MonadIO m
  => GLenum -- ^ @pname@.
  -> Ptr GLfixed -- ^ @param@ pointing to @COMPSIZE(pname)@ elements of type @GLfixed@.
  -> m ()
glLightModelxv v1 v2 = liftIO $ dyn95 ptr_glLightModelxv v1 v2

{-# NOINLINE ptr_glLightModelxv #-}
ptr_glLightModelxv :: FunPtr (GLenum -> Ptr GLfixed -> IO ())
ptr_glLightModelxv = unsafePerformIO $ getCommand "glLightModelxv"

-- glLightModelxvOES -----------------------------------------------------------

glLightModelxvOES
  :: MonadIO m
  => GLenum -- ^ @pname@.
  -> Ptr GLfixed -- ^ @param@ pointing to @COMPSIZE(pname)@ elements of type @GLfixed@.
  -> m ()
glLightModelxvOES v1 v2 = liftIO $ dyn95 ptr_glLightModelxvOES v1 v2

{-# NOINLINE ptr_glLightModelxvOES #-}
ptr_glLightModelxvOES :: FunPtr (GLenum -> Ptr GLfixed -> IO ())
ptr_glLightModelxvOES = unsafePerformIO $ getCommand "glLightModelxvOES"

-- glLightf --------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glLight.xml OpenGL 2.x>.
glLightf
  :: MonadIO m
  => GLenum -- ^ @light@ of type [LightName](Graphics-GL-Groups.html#LightName).
  -> GLenum -- ^ @pname@ of type [LightParameter](Graphics-GL-Groups.html#LightParameter).
  -> GLfloat -- ^ @param@ of type @CheckedFloat32@.
  -> m ()
glLightf v1 v2 v3 = liftIO $ dyn161 ptr_glLightf v1 v2 v3

{-# NOINLINE ptr_glLightf #-}
ptr_glLightf :: FunPtr (GLenum -> GLenum -> GLfloat -> IO ())
ptr_glLightf = unsafePerformIO $ getCommand "glLightf"

-- glLightfv -------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glLight.xml OpenGL 2.x>.
glLightfv
  :: MonadIO m
  => GLenum -- ^ @light@ of type [LightName](Graphics-GL-Groups.html#LightName).
  -> GLenum -- ^ @pname@ of type [LightParameter](Graphics-GL-Groups.html#LightParameter).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedFloat32@.
  -> m ()
glLightfv v1 v2 v3 = liftIO $ dyn132 ptr_glLightfv v1 v2 v3

{-# NOINLINE ptr_glLightfv #-}
ptr_glLightfv :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glLightfv = unsafePerformIO $ getCommand "glLightfv"

-- glLighti --------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glLight.xml OpenGL 2.x>.
glLighti
  :: MonadIO m
  => GLenum -- ^ @light@ of type [LightName](Graphics-GL-Groups.html#LightName).
  -> GLenum -- ^ @pname@ of type [LightParameter](Graphics-GL-Groups.html#LightParameter).
  -> GLint -- ^ @param@ of type @CheckedInt32@.
  -> m ()
glLighti v1 v2 v3 = liftIO $ dyn62 ptr_glLighti v1 v2 v3

{-# NOINLINE ptr_glLighti #-}
ptr_glLighti :: FunPtr (GLenum -> GLenum -> GLint -> IO ())
ptr_glLighti = unsafePerformIO $ getCommand "glLighti"

-- glLightiv -------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glLight.xml OpenGL 2.x>.
glLightiv
  :: MonadIO m
  => GLenum -- ^ @light@ of type [LightName](Graphics-GL-Groups.html#LightName).
  -> GLenum -- ^ @pname@ of type [LightParameter](Graphics-GL-Groups.html#LightParameter).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedInt32@.
  -> m ()
glLightiv v1 v2 v3 = liftIO $ dyn133 ptr_glLightiv v1 v2 v3

{-# NOINLINE ptr_glLightiv #-}
ptr_glLightiv :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glLightiv = unsafePerformIO $ getCommand "glLightiv"

-- glLightx --------------------------------------------------------------------

glLightx
  :: MonadIO m
  => GLenum -- ^ @light@.
  -> GLenum -- ^ @pname@.
  -> GLfixed -- ^ @param@.
  -> m ()
glLightx v1 v2 v3 = liftIO $ dyn162 ptr_glLightx v1 v2 v3

{-# NOINLINE ptr_glLightx #-}
ptr_glLightx :: FunPtr (GLenum -> GLenum -> GLfixed -> IO ())
ptr_glLightx = unsafePerformIO $ getCommand "glLightx"

-- glLightxOES -----------------------------------------------------------------

glLightxOES
  :: MonadIO m
  => GLenum -- ^ @light@.
  -> GLenum -- ^ @pname@.
  -> GLfixed -- ^ @param@.
  -> m ()
glLightxOES v1 v2 v3 = liftIO $ dyn162 ptr_glLightxOES v1 v2 v3

{-# NOINLINE ptr_glLightxOES #-}
ptr_glLightxOES :: FunPtr (GLenum -> GLenum -> GLfixed -> IO ())
ptr_glLightxOES = unsafePerformIO $ getCommand "glLightxOES"

-- glLightxv -------------------------------------------------------------------

glLightxv
  :: MonadIO m
  => GLenum -- ^ @light@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLfixed -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfixed@.
  -> m ()
glLightxv v1 v2 v3 = liftIO $ dyn163 ptr_glLightxv v1 v2 v3

{-# NOINLINE ptr_glLightxv #-}
ptr_glLightxv :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
ptr_glLightxv = unsafePerformIO $ getCommand "glLightxv"

-- glLightxvOES ----------------------------------------------------------------

glLightxvOES
  :: MonadIO m
  => GLenum -- ^ @light@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLfixed -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfixed@.
  -> m ()
glLightxvOES v1 v2 v3 = liftIO $ dyn163 ptr_glLightxvOES v1 v2 v3

{-# NOINLINE ptr_glLightxvOES #-}
ptr_glLightxvOES :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
ptr_glLightxvOES = unsafePerformIO $ getCommand "glLightxvOES"

-- glLineStipple ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glLineStipple.xml OpenGL 2.x>.
glLineStipple
  :: MonadIO m
  => GLint -- ^ @factor@ of type @CheckedInt32@.
  -> GLushort -- ^ @pattern@ of type @LineStipple@.
  -> m ()
glLineStipple v1 v2 = liftIO $ dyn485 ptr_glLineStipple v1 v2

{-# NOINLINE ptr_glLineStipple #-}
ptr_glLineStipple :: FunPtr (GLint -> GLushort -> IO ())
ptr_glLineStipple = unsafePerformIO $ getCommand "glLineStipple"

-- glLineWidth -----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glLineWidth.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glLineWidth.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glLineWidth.xhtml OpenGL 4.x>.
glLineWidth
  :: MonadIO m
  => GLfloat -- ^ @width@ of type @CheckedFloat32@.
  -> m ()
glLineWidth v1 = liftIO $ dyn79 ptr_glLineWidth v1

{-# NOINLINE ptr_glLineWidth #-}
ptr_glLineWidth :: FunPtr (GLfloat -> IO ())
ptr_glLineWidth = unsafePerformIO $ getCommand "glLineWidth"

-- glLineWidthx ----------------------------------------------------------------

glLineWidthx
  :: MonadIO m
  => GLfixed -- ^ @width@.
  -> m ()
glLineWidthx v1 = liftIO $ dyn81 ptr_glLineWidthx v1

{-# NOINLINE ptr_glLineWidthx #-}
ptr_glLineWidthx :: FunPtr (GLfixed -> IO ())
ptr_glLineWidthx = unsafePerformIO $ getCommand "glLineWidthx"

-- glLineWidthxOES -------------------------------------------------------------

glLineWidthxOES
  :: MonadIO m
  => GLfixed -- ^ @width@.
  -> m ()
glLineWidthxOES v1 = liftIO $ dyn81 ptr_glLineWidthxOES v1

{-# NOINLINE ptr_glLineWidthxOES #-}
ptr_glLineWidthxOES :: FunPtr (GLfixed -> IO ())
ptr_glLineWidthxOES = unsafePerformIO $ getCommand "glLineWidthxOES"

-- glLinkProgram ---------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glLinkProgram.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glLinkProgram.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glLinkProgram.xhtml OpenGL 4.x>.
glLinkProgram
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> m ()
glLinkProgram v1 = liftIO $ dyn2 ptr_glLinkProgram v1

{-# NOINLINE ptr_glLinkProgram #-}
ptr_glLinkProgram :: FunPtr (GLuint -> IO ())
ptr_glLinkProgram = unsafePerformIO $ getCommand "glLinkProgram"

-- glLinkProgramARB ------------------------------------------------------------

-- | This command is an alias for 'glLinkProgram'.
glLinkProgramARB
  :: MonadIO m
  => GLhandleARB -- ^ @programObj@ of type @handleARB@.
  -> m ()
glLinkProgramARB v1 = liftIO $ dyn137 ptr_glLinkProgramARB v1

{-# NOINLINE ptr_glLinkProgramARB #-}
ptr_glLinkProgramARB :: FunPtr (GLhandleARB -> IO ())
ptr_glLinkProgramARB = unsafePerformIO $ getCommand "glLinkProgramARB"

-- glListBase ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glListBase.xml OpenGL 2.x>.
glListBase
  :: MonadIO m
  => GLuint -- ^ @base@ of type @List@.
  -> m ()
glListBase v1 = liftIO $ dyn2 ptr_glListBase v1

{-# NOINLINE ptr_glListBase #-}
ptr_glListBase :: FunPtr (GLuint -> IO ())
ptr_glListBase = unsafePerformIO $ getCommand "glListBase"

-- glListDrawCommandsStatesClientNV --------------------------------------------

glListDrawCommandsStatesClientNV
  :: MonadIO m
  => GLuint -- ^ @list@.
  -> GLuint -- ^ @segment@.
  -> Ptr (Ptr a) -- ^ @indirects@.
  -> Ptr GLsizei -- ^ @sizes@.
  -> Ptr GLuint -- ^ @states@.
  -> Ptr GLuint -- ^ @fbos@.
  -> GLuint -- ^ @count@.
  -> m ()
glListDrawCommandsStatesClientNV v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn486 ptr_glListDrawCommandsStatesClientNV v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glListDrawCommandsStatesClientNV #-}
ptr_glListDrawCommandsStatesClientNV :: FunPtr (GLuint -> GLuint -> Ptr (Ptr a) -> Ptr GLsizei -> Ptr GLuint -> Ptr GLuint -> GLuint -> IO ())
ptr_glListDrawCommandsStatesClientNV = unsafePerformIO $ getCommand "glListDrawCommandsStatesClientNV"

-- glListParameterfSGIX --------------------------------------------------------

glListParameterfSGIX
  :: MonadIO m
  => GLuint -- ^ @list@ of type @List@.
  -> GLenum -- ^ @pname@ of type [ListParameterName](Graphics-GL-Groups.html#ListParameterName).
  -> GLfloat -- ^ @param@ of type @CheckedFloat32@.
  -> m ()
glListParameterfSGIX v1 v2 v3 = liftIO $ dyn487 ptr_glListParameterfSGIX v1 v2 v3

{-# NOINLINE ptr_glListParameterfSGIX #-}
ptr_glListParameterfSGIX :: FunPtr (GLuint -> GLenum -> GLfloat -> IO ())
ptr_glListParameterfSGIX = unsafePerformIO $ getCommand "glListParameterfSGIX"

-- glListParameterfvSGIX -------------------------------------------------------

glListParameterfvSGIX
  :: MonadIO m
  => GLuint -- ^ @list@ of type @List@.
  -> GLenum -- ^ @pname@ of type [ListParameterName](Graphics-GL-Groups.html#ListParameterName).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedFloat32@.
  -> m ()
glListParameterfvSGIX v1 v2 v3 = liftIO $ dyn349 ptr_glListParameterfvSGIX v1 v2 v3

{-# NOINLINE ptr_glListParameterfvSGIX #-}
ptr_glListParameterfvSGIX :: FunPtr (GLuint -> GLenum -> Ptr GLfloat -> IO ())
ptr_glListParameterfvSGIX = unsafePerformIO $ getCommand "glListParameterfvSGIX"

-- glListParameteriSGIX --------------------------------------------------------

glListParameteriSGIX
  :: MonadIO m
  => GLuint -- ^ @list@ of type @List@.
  -> GLenum -- ^ @pname@ of type [ListParameterName](Graphics-GL-Groups.html#ListParameterName).
  -> GLint -- ^ @param@ of type @CheckedInt32@.
  -> m ()
glListParameteriSGIX v1 v2 v3 = liftIO $ dyn488 ptr_glListParameteriSGIX v1 v2 v3

{-# NOINLINE ptr_glListParameteriSGIX #-}
ptr_glListParameteriSGIX :: FunPtr (GLuint -> GLenum -> GLint -> IO ())
ptr_glListParameteriSGIX = unsafePerformIO $ getCommand "glListParameteriSGIX"

-- glListParameterivSGIX -------------------------------------------------------

glListParameterivSGIX
  :: MonadIO m
  => GLuint -- ^ @list@ of type @List@.
  -> GLenum -- ^ @pname@ of type [ListParameterName](Graphics-GL-Groups.html#ListParameterName).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedInt32@.
  -> m ()
glListParameterivSGIX v1 v2 v3 = liftIO $ dyn334 ptr_glListParameterivSGIX v1 v2 v3

{-# NOINLINE ptr_glListParameterivSGIX #-}
ptr_glListParameterivSGIX :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glListParameterivSGIX = unsafePerformIO $ getCommand "glListParameterivSGIX"

-- glLoadIdentity --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glLoadIdentity.xml OpenGL 2.x>.
glLoadIdentity
  :: MonadIO m
  => m ()
glLoadIdentity = liftIO $ dyn10 ptr_glLoadIdentity

{-# NOINLINE ptr_glLoadIdentity #-}
ptr_glLoadIdentity :: FunPtr (IO ())
ptr_glLoadIdentity = unsafePerformIO $ getCommand "glLoadIdentity"

-- glLoadIdentityDeformationMapSGIX --------------------------------------------

glLoadIdentityDeformationMapSGIX
  :: MonadIO m
  => GLbitfield -- ^ @mask@ of type [FfdMaskSGIX](Graphics-GL-Groups.html#FfdMaskSGIX).
  -> m ()
glLoadIdentityDeformationMapSGIX v1 = liftIO $ dyn69 ptr_glLoadIdentityDeformationMapSGIX v1

{-# NOINLINE ptr_glLoadIdentityDeformationMapSGIX #-}
ptr_glLoadIdentityDeformationMapSGIX :: FunPtr (GLbitfield -> IO ())
ptr_glLoadIdentityDeformationMapSGIX = unsafePerformIO $ getCommand "glLoadIdentityDeformationMapSGIX"

-- glLoadMatrixd ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glLoadMatrix.xml OpenGL 2.x>.
glLoadMatrixd
  :: MonadIO m
  => Ptr GLdouble -- ^ @m@ pointing to @16@ elements of type @GLdouble@.
  -> m ()
glLoadMatrixd v1 = liftIO $ dyn39 ptr_glLoadMatrixd v1

{-# NOINLINE ptr_glLoadMatrixd #-}
ptr_glLoadMatrixd :: FunPtr (Ptr GLdouble -> IO ())
ptr_glLoadMatrixd = unsafePerformIO $ getCommand "glLoadMatrixd"

-- glLoadMatrixf ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glLoadMatrix.xml OpenGL 2.x>.
glLoadMatrixf
  :: MonadIO m
  => Ptr GLfloat -- ^ @m@ pointing to @16@ elements of type @GLfloat@.
  -> m ()
glLoadMatrixf v1 = liftIO $ dyn41 ptr_glLoadMatrixf v1

{-# NOINLINE ptr_glLoadMatrixf #-}
ptr_glLoadMatrixf :: FunPtr (Ptr GLfloat -> IO ())
ptr_glLoadMatrixf = unsafePerformIO $ getCommand "glLoadMatrixf"

-- glLoadMatrixx ---------------------------------------------------------------

glLoadMatrixx
  :: MonadIO m
  => Ptr GLfixed -- ^ @m@ pointing to @16@ elements of type @GLfixed@.
  -> m ()
glLoadMatrixx v1 = liftIO $ dyn107 ptr_glLoadMatrixx v1

{-# NOINLINE ptr_glLoadMatrixx #-}
ptr_glLoadMatrixx :: FunPtr (Ptr GLfixed -> IO ())
ptr_glLoadMatrixx = unsafePerformIO $ getCommand "glLoadMatrixx"

-- glLoadMatrixxOES ------------------------------------------------------------

glLoadMatrixxOES
  :: MonadIO m
  => Ptr GLfixed -- ^ @m@ pointing to @16@ elements of type @GLfixed@.
  -> m ()
glLoadMatrixxOES v1 = liftIO $ dyn107 ptr_glLoadMatrixxOES v1

{-# NOINLINE ptr_glLoadMatrixxOES #-}
ptr_glLoadMatrixxOES :: FunPtr (Ptr GLfixed -> IO ())
ptr_glLoadMatrixxOES = unsafePerformIO $ getCommand "glLoadMatrixxOES"

-- glLoadName ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glLoadName.xml OpenGL 2.x>.
glLoadName
  :: MonadIO m
  => GLuint -- ^ @name@ of type @SelectName@.
  -> m ()
glLoadName v1 = liftIO $ dyn2 ptr_glLoadName v1

{-# NOINLINE ptr_glLoadName #-}
ptr_glLoadName :: FunPtr (GLuint -> IO ())
ptr_glLoadName = unsafePerformIO $ getCommand "glLoadName"

-- glLoadPaletteFromModelViewMatrixOES -----------------------------------------

glLoadPaletteFromModelViewMatrixOES
  :: MonadIO m
  => m ()
glLoadPaletteFromModelViewMatrixOES = liftIO $ dyn10 ptr_glLoadPaletteFromModelViewMatrixOES

{-# NOINLINE ptr_glLoadPaletteFromModelViewMatrixOES #-}
ptr_glLoadPaletteFromModelViewMatrixOES :: FunPtr (IO ())
ptr_glLoadPaletteFromModelViewMatrixOES = unsafePerformIO $ getCommand "glLoadPaletteFromModelViewMatrixOES"

-- glLoadProgramNV -------------------------------------------------------------

glLoadProgramNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type @VertexAttribEnumNV@.
  -> GLuint -- ^ @id@.
  -> GLsizei -- ^ @len@.
  -> Ptr GLubyte -- ^ @program@ pointing to @len@ elements of type @GLubyte@.
  -> m ()
glLoadProgramNV v1 v2 v3 v4 = liftIO $ dyn489 ptr_glLoadProgramNV v1 v2 v3 v4

{-# NOINLINE ptr_glLoadProgramNV #-}
ptr_glLoadProgramNV :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLubyte -> IO ())
ptr_glLoadProgramNV = unsafePerformIO $ getCommand "glLoadProgramNV"

-- glLoadTransposeMatrixd ------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glLoadTransposeMatrix.xml OpenGL 2.x>.
glLoadTransposeMatrixd
  :: MonadIO m
  => Ptr GLdouble -- ^ @m@ pointing to @16@ elements of type @GLdouble@.
  -> m ()
glLoadTransposeMatrixd v1 = liftIO $ dyn39 ptr_glLoadTransposeMatrixd v1

{-# NOINLINE ptr_glLoadTransposeMatrixd #-}
ptr_glLoadTransposeMatrixd :: FunPtr (Ptr GLdouble -> IO ())
ptr_glLoadTransposeMatrixd = unsafePerformIO $ getCommand "glLoadTransposeMatrixd"

