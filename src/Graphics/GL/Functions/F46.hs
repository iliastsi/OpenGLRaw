--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F46
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

module Graphics.GL.Functions.F46 (
  glPathMemoryGlyphIndexArrayNV,
  glPathParameterfNV,
  glPathParameterfvNV,
  glPathParameteriNV,
  glPathParameterivNV,
  glPathStencilDepthOffsetNV,
  glPathStencilFuncNV,
  glPathStringNV,
  glPathSubCommandsNV,
  glPathSubCoordsNV,
  glPathTexGenNV,
  glPauseTransformFeedback,
  glPauseTransformFeedbackNV,
  glPixelDataRangeNV,
  glPixelMapfv,
  glPixelMapuiv,
  glPixelMapusv,
  glPixelMapx,
  glPixelStoref,
  glPixelStorei,
  glPixelStorex,
  glPixelTexGenParameterfSGIS,
  glPixelTexGenParameterfvSGIS,
  glPixelTexGenParameteriSGIS,
  glPixelTexGenParameterivSGIS,
  glPixelTexGenSGIX,
  glPixelTransferf,
  glPixelTransferi,
  glPixelTransferxOES,
  glPixelTransformParameterfEXT,
  glPixelTransformParameterfvEXT,
  glPixelTransformParameteriEXT,
  glPixelTransformParameterivEXT,
  glPixelZoom,
  glPixelZoomxOES,
  glPointAlongPathNV,
  glPointParameterf,
  glPointParameterfARB,
  glPointParameterfEXT,
  glPointParameterfSGIS
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

-- glPathMemoryGlyphIndexArrayNV -----------------------------------------------

glPathMemoryGlyphIndexArrayNV
  :: MonadIO m
  => GLuint -- ^ @firstPathName@.
  -> GLenum -- ^ @fontTarget@.
  -> GLsizeiptr -- ^ @fontSize@.
  -> Ptr a -- ^ @fontData@.
  -> GLsizei -- ^ @faceIndex@.
  -> GLuint -- ^ @firstGlyphIndex@.
  -> GLsizei -- ^ @numGlyphs@.
  -> GLuint -- ^ @pathParameterTemplate@.
  -> GLfloat -- ^ @emScale@.
  -> m GLenum
glPathMemoryGlyphIndexArrayNV v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn603 ptr_glPathMemoryGlyphIndexArrayNV v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glPathMemoryGlyphIndexArrayNV #-}
ptr_glPathMemoryGlyphIndexArrayNV :: FunPtr (GLuint -> GLenum -> GLsizeiptr -> Ptr a -> GLsizei -> GLuint -> GLsizei -> GLuint -> GLfloat -> IO GLenum)
ptr_glPathMemoryGlyphIndexArrayNV = unsafePerformIO $ getCommand "glPathMemoryGlyphIndexArrayNV"

-- glPathParameterfNV ----------------------------------------------------------

glPathParameterfNV
  :: MonadIO m
  => GLuint -- ^ @path@ of type @Path@.
  -> GLenum -- ^ @pname@ of type @PathParameter@.
  -> GLfloat -- ^ @value@.
  -> m ()
glPathParameterfNV v1 v2 v3 = liftIO $ dyn487 ptr_glPathParameterfNV v1 v2 v3

{-# NOINLINE ptr_glPathParameterfNV #-}
ptr_glPathParameterfNV :: FunPtr (GLuint -> GLenum -> GLfloat -> IO ())
ptr_glPathParameterfNV = unsafePerformIO $ getCommand "glPathParameterfNV"

-- glPathParameterfvNV ---------------------------------------------------------

glPathParameterfvNV
  :: MonadIO m
  => GLuint -- ^ @path@ of type @Path@.
  -> GLenum -- ^ @pname@ of type @PathParameter@.
  -> Ptr GLfloat -- ^ @value@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glPathParameterfvNV v1 v2 v3 = liftIO $ dyn349 ptr_glPathParameterfvNV v1 v2 v3

{-# NOINLINE ptr_glPathParameterfvNV #-}
ptr_glPathParameterfvNV :: FunPtr (GLuint -> GLenum -> Ptr GLfloat -> IO ())
ptr_glPathParameterfvNV = unsafePerformIO $ getCommand "glPathParameterfvNV"

-- glPathParameteriNV ----------------------------------------------------------

glPathParameteriNV
  :: MonadIO m
  => GLuint -- ^ @path@ of type @Path@.
  -> GLenum -- ^ @pname@ of type @PathParameter@.
  -> GLint -- ^ @value@.
  -> m ()
glPathParameteriNV v1 v2 v3 = liftIO $ dyn488 ptr_glPathParameteriNV v1 v2 v3

{-# NOINLINE ptr_glPathParameteriNV #-}
ptr_glPathParameteriNV :: FunPtr (GLuint -> GLenum -> GLint -> IO ())
ptr_glPathParameteriNV = unsafePerformIO $ getCommand "glPathParameteriNV"

-- glPathParameterivNV ---------------------------------------------------------

glPathParameterivNV
  :: MonadIO m
  => GLuint -- ^ @path@ of type @Path@.
  -> GLenum -- ^ @pname@ of type @PathParameter@.
  -> Ptr GLint -- ^ @value@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glPathParameterivNV v1 v2 v3 = liftIO $ dyn334 ptr_glPathParameterivNV v1 v2 v3

{-# NOINLINE ptr_glPathParameterivNV #-}
ptr_glPathParameterivNV :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glPathParameterivNV = unsafePerformIO $ getCommand "glPathParameterivNV"

-- glPathStencilDepthOffsetNV --------------------------------------------------

glPathStencilDepthOffsetNV
  :: MonadIO m
  => GLfloat -- ^ @factor@.
  -> GLfloat -- ^ @units@.
  -> m ()
glPathStencilDepthOffsetNV v1 v2 = liftIO $ dyn222 ptr_glPathStencilDepthOffsetNV v1 v2

{-# NOINLINE ptr_glPathStencilDepthOffsetNV #-}
ptr_glPathStencilDepthOffsetNV :: FunPtr (GLfloat -> GLfloat -> IO ())
ptr_glPathStencilDepthOffsetNV = unsafePerformIO $ getCommand "glPathStencilDepthOffsetNV"

-- glPathStencilFuncNV ---------------------------------------------------------

glPathStencilFuncNV
  :: MonadIO m
  => GLenum -- ^ @func@ of type [StencilFunction](Graphics-GL-Groups.html#StencilFunction).
  -> GLint -- ^ @ref@ of type @ClampedStencilValue@.
  -> GLuint -- ^ @mask@ of type @MaskedStencilValue@.
  -> m ()
glPathStencilFuncNV v1 v2 v3 = liftIO $ dyn604 ptr_glPathStencilFuncNV v1 v2 v3

{-# NOINLINE ptr_glPathStencilFuncNV #-}
ptr_glPathStencilFuncNV :: FunPtr (GLenum -> GLint -> GLuint -> IO ())
ptr_glPathStencilFuncNV = unsafePerformIO $ getCommand "glPathStencilFuncNV"

-- glPathStringNV --------------------------------------------------------------

glPathStringNV
  :: MonadIO m
  => GLuint -- ^ @path@ of type @Path@.
  -> GLenum -- ^ @format@ of type @PathStringFormat@.
  -> GLsizei -- ^ @length@.
  -> Ptr a -- ^ @pathString@ pointing to @length@ elements of type @a@.
  -> m ()
glPathStringNV v1 v2 v3 v4 = liftIO $ dyn605 ptr_glPathStringNV v1 v2 v3 v4

{-# NOINLINE ptr_glPathStringNV #-}
ptr_glPathStringNV :: FunPtr (GLuint -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glPathStringNV = unsafePerformIO $ getCommand "glPathStringNV"

-- glPathSubCommandsNV ---------------------------------------------------------

glPathSubCommandsNV
  :: MonadIO m
  => GLuint -- ^ @path@ of type @Path@.
  -> GLsizei -- ^ @commandStart@.
  -> GLsizei -- ^ @commandsToDelete@.
  -> GLsizei -- ^ @numCommands@.
  -> Ptr GLubyte -- ^ @commands@ pointing to @numCommands@ elements of type @PathCommand@.
  -> GLsizei -- ^ @numCoords@.
  -> GLenum -- ^ @coordType@ of type @PathCoordType@.
  -> Ptr a -- ^ @coords@ pointing to @COMPSIZE(numCoords,coordType)@ elements of type @a@.
  -> m ()
glPathSubCommandsNV v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn606 ptr_glPathSubCommandsNV v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glPathSubCommandsNV #-}
ptr_glPathSubCommandsNV :: FunPtr (GLuint -> GLsizei -> GLsizei -> GLsizei -> Ptr GLubyte -> GLsizei -> GLenum -> Ptr a -> IO ())
ptr_glPathSubCommandsNV = unsafePerformIO $ getCommand "glPathSubCommandsNV"

-- glPathSubCoordsNV -----------------------------------------------------------

glPathSubCoordsNV
  :: MonadIO m
  => GLuint -- ^ @path@ of type @Path@.
  -> GLsizei -- ^ @coordStart@.
  -> GLsizei -- ^ @numCoords@.
  -> GLenum -- ^ @coordType@ of type @PathCoordType@.
  -> Ptr a -- ^ @coords@ pointing to @COMPSIZE(numCoords,coordType)@ elements of type @a@.
  -> m ()
glPathSubCoordsNV v1 v2 v3 v4 v5 = liftIO $ dyn607 ptr_glPathSubCoordsNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glPathSubCoordsNV #-}
ptr_glPathSubCoordsNV :: FunPtr (GLuint -> GLsizei -> GLsizei -> GLenum -> Ptr a -> IO ())
ptr_glPathSubCoordsNV = unsafePerformIO $ getCommand "glPathSubCoordsNV"

-- glPathTexGenNV --------------------------------------------------------------

glPathTexGenNV
  :: MonadIO m
  => GLenum -- ^ @texCoordSet@ of type @PathColor@.
  -> GLenum -- ^ @genMode@ of type @PathGenMode@.
  -> GLint -- ^ @components@.
  -> Ptr GLfloat -- ^ @coeffs@ pointing to @COMPSIZE(genMode,components)@ elements of type @GLfloat@.
  -> m ()
glPathTexGenNV v1 v2 v3 v4 = liftIO $ dyn608 ptr_glPathTexGenNV v1 v2 v3 v4

{-# NOINLINE ptr_glPathTexGenNV #-}
ptr_glPathTexGenNV :: FunPtr (GLenum -> GLenum -> GLint -> Ptr GLfloat -> IO ())
ptr_glPathTexGenNV = unsafePerformIO $ getCommand "glPathTexGenNV"

-- glPauseTransformFeedback ----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glPauseTransformFeedback.xhtml OpenGL 4.x>.
glPauseTransformFeedback
  :: MonadIO m
  => m ()
glPauseTransformFeedback = liftIO $ dyn10 ptr_glPauseTransformFeedback

{-# NOINLINE ptr_glPauseTransformFeedback #-}
ptr_glPauseTransformFeedback :: FunPtr (IO ())
ptr_glPauseTransformFeedback = unsafePerformIO $ getCommand "glPauseTransformFeedback"

-- glPauseTransformFeedbackNV --------------------------------------------------

-- | This command is an alias for 'glPauseTransformFeedback'.
glPauseTransformFeedbackNV
  :: MonadIO m
  => m ()
glPauseTransformFeedbackNV = liftIO $ dyn10 ptr_glPauseTransformFeedbackNV

{-# NOINLINE ptr_glPauseTransformFeedbackNV #-}
ptr_glPauseTransformFeedbackNV :: FunPtr (IO ())
ptr_glPauseTransformFeedbackNV = unsafePerformIO $ getCommand "glPauseTransformFeedbackNV"

-- glPixelDataRangeNV ----------------------------------------------------------

glPixelDataRangeNV
  :: MonadIO m
  => GLenum -- ^ @target@ of type @PixelDataRangeTargetNV@.
  -> GLsizei -- ^ @length@.
  -> Ptr a -- ^ @pointer@ pointing to @length@ elements of type @a@.
  -> m ()
glPixelDataRangeNV v1 v2 v3 = liftIO $ dyn46 ptr_glPixelDataRangeNV v1 v2 v3

{-# NOINLINE ptr_glPixelDataRangeNV #-}
ptr_glPixelDataRangeNV :: FunPtr (GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glPixelDataRangeNV = unsafePerformIO $ getCommand "glPixelDataRangeNV"

-- glPixelMapfv ----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glPixelMap.xml OpenGL 2.x>.
glPixelMapfv
  :: MonadIO m
  => GLenum -- ^ @map@ of type [PixelMap](Graphics-GL-Groups.html#PixelMap).
  -> GLsizei -- ^ @mapsize@ of type @CheckedInt32@.
  -> Ptr GLfloat -- ^ @values@ pointing to @mapsize@ elements of type @GLfloat@.
  -> m ()
glPixelMapfv v1 v2 v3 = liftIO $ dyn225 ptr_glPixelMapfv v1 v2 v3

{-# NOINLINE ptr_glPixelMapfv #-}
ptr_glPixelMapfv :: FunPtr (GLenum -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glPixelMapfv = unsafePerformIO $ getCommand "glPixelMapfv"

-- glPixelMapuiv ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glPixelMap.xml OpenGL 2.x>.
glPixelMapuiv
  :: MonadIO m
  => GLenum -- ^ @map@ of type [PixelMap](Graphics-GL-Groups.html#PixelMap).
  -> GLsizei -- ^ @mapsize@ of type @CheckedInt32@.
  -> Ptr GLuint -- ^ @values@ pointing to @mapsize@ elements of type @GLuint@.
  -> m ()
glPixelMapuiv v1 v2 v3 = liftIO $ dyn197 ptr_glPixelMapuiv v1 v2 v3

{-# NOINLINE ptr_glPixelMapuiv #-}
ptr_glPixelMapuiv :: FunPtr (GLenum -> GLsizei -> Ptr GLuint -> IO ())
ptr_glPixelMapuiv = unsafePerformIO $ getCommand "glPixelMapuiv"

-- glPixelMapusv ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glPixelMap.xml OpenGL 2.x>.
glPixelMapusv
  :: MonadIO m
  => GLenum -- ^ @map@ of type [PixelMap](Graphics-GL-Groups.html#PixelMap).
  -> GLsizei -- ^ @mapsize@ of type @CheckedInt32@.
  -> Ptr GLushort -- ^ @values@ pointing to @mapsize@ elements of type @GLushort@.
  -> m ()
glPixelMapusv v1 v2 v3 = liftIO $ dyn452 ptr_glPixelMapusv v1 v2 v3

{-# NOINLINE ptr_glPixelMapusv #-}
ptr_glPixelMapusv :: FunPtr (GLenum -> GLsizei -> Ptr GLushort -> IO ())
ptr_glPixelMapusv = unsafePerformIO $ getCommand "glPixelMapusv"

-- glPixelMapx -----------------------------------------------------------------

glPixelMapx
  :: MonadIO m
  => GLenum -- ^ @map@.
  -> GLint -- ^ @size@.
  -> Ptr GLfixed -- ^ @values@ pointing to @size@ elements of type @GLfixed@.
  -> m ()
glPixelMapx v1 v2 v3 = liftIO $ dyn390 ptr_glPixelMapx v1 v2 v3

{-# NOINLINE ptr_glPixelMapx #-}
ptr_glPixelMapx :: FunPtr (GLenum -> GLint -> Ptr GLfixed -> IO ())
ptr_glPixelMapx = unsafePerformIO $ getCommand "glPixelMapx"

-- glPixelStoref ---------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glPixelStore.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glPixelStore.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glPixelStore.xhtml OpenGL 4.x>.
glPixelStoref
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [PixelStoreParameter](Graphics-GL-Groups.html#PixelStoreParameter).
  -> GLfloat -- ^ @param@ of type @CheckedFloat32@.
  -> m ()
glPixelStoref v1 v2 = liftIO $ dyn0 ptr_glPixelStoref v1 v2

{-# NOINLINE ptr_glPixelStoref #-}
ptr_glPixelStoref :: FunPtr (GLenum -> GLfloat -> IO ())
ptr_glPixelStoref = unsafePerformIO $ getCommand "glPixelStoref"

-- glPixelStorei ---------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glPixelStore.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glPixelStore.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glPixelStore.xhtml OpenGL 4.x>.
glPixelStorei
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [PixelStoreParameter](Graphics-GL-Groups.html#PixelStoreParameter).
  -> GLint -- ^ @param@ of type @CheckedInt32@.
  -> m ()
glPixelStorei v1 v2 = liftIO $ dyn55 ptr_glPixelStorei v1 v2

{-# NOINLINE ptr_glPixelStorei #-}
ptr_glPixelStorei :: FunPtr (GLenum -> GLint -> IO ())
ptr_glPixelStorei = unsafePerformIO $ getCommand "glPixelStorei"

-- glPixelStorex ---------------------------------------------------------------

glPixelStorex
  :: MonadIO m
  => GLenum -- ^ @pname@.
  -> GLfixed -- ^ @param@.
  -> m ()
glPixelStorex v1 v2 = liftIO $ dyn1 ptr_glPixelStorex v1 v2

{-# NOINLINE ptr_glPixelStorex #-}
ptr_glPixelStorex :: FunPtr (GLenum -> GLfixed -> IO ())
ptr_glPixelStorex = unsafePerformIO $ getCommand "glPixelStorex"

-- glPixelTexGenParameterfSGIS -------------------------------------------------

glPixelTexGenParameterfSGIS
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [PixelTexGenParameterNameSGIS](Graphics-GL-Groups.html#PixelTexGenParameterNameSGIS).
  -> GLfloat -- ^ @param@ of type @CheckedFloat32@.
  -> m ()
glPixelTexGenParameterfSGIS v1 v2 = liftIO $ dyn0 ptr_glPixelTexGenParameterfSGIS v1 v2

{-# NOINLINE ptr_glPixelTexGenParameterfSGIS #-}
ptr_glPixelTexGenParameterfSGIS :: FunPtr (GLenum -> GLfloat -> IO ())
ptr_glPixelTexGenParameterfSGIS = unsafePerformIO $ getCommand "glPixelTexGenParameterfSGIS"

-- glPixelTexGenParameterfvSGIS ------------------------------------------------

glPixelTexGenParameterfvSGIS
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [PixelTexGenParameterNameSGIS](Graphics-GL-Groups.html#PixelTexGenParameterNameSGIS).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedFloat32@.
  -> m ()
glPixelTexGenParameterfvSGIS v1 v2 = liftIO $ dyn94 ptr_glPixelTexGenParameterfvSGIS v1 v2

{-# NOINLINE ptr_glPixelTexGenParameterfvSGIS #-}
ptr_glPixelTexGenParameterfvSGIS :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glPixelTexGenParameterfvSGIS = unsafePerformIO $ getCommand "glPixelTexGenParameterfvSGIS"

-- glPixelTexGenParameteriSGIS -------------------------------------------------

glPixelTexGenParameteriSGIS
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [PixelTexGenParameterNameSGIS](Graphics-GL-Groups.html#PixelTexGenParameterNameSGIS).
  -> GLint -- ^ @param@ of type @CheckedInt32@.
  -> m ()
glPixelTexGenParameteriSGIS v1 v2 = liftIO $ dyn55 ptr_glPixelTexGenParameteriSGIS v1 v2

{-# NOINLINE ptr_glPixelTexGenParameteriSGIS #-}
ptr_glPixelTexGenParameteriSGIS :: FunPtr (GLenum -> GLint -> IO ())
ptr_glPixelTexGenParameteriSGIS = unsafePerformIO $ getCommand "glPixelTexGenParameteriSGIS"

-- glPixelTexGenParameterivSGIS ------------------------------------------------

glPixelTexGenParameterivSGIS
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [PixelTexGenParameterNameSGIS](Graphics-GL-Groups.html#PixelTexGenParameterNameSGIS).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedInt32@.
  -> m ()
glPixelTexGenParameterivSGIS v1 v2 = liftIO $ dyn136 ptr_glPixelTexGenParameterivSGIS v1 v2

{-# NOINLINE ptr_glPixelTexGenParameterivSGIS #-}
ptr_glPixelTexGenParameterivSGIS :: FunPtr (GLenum -> Ptr GLint -> IO ())
ptr_glPixelTexGenParameterivSGIS = unsafePerformIO $ getCommand "glPixelTexGenParameterivSGIS"

-- glPixelTexGenSGIX -----------------------------------------------------------

glPixelTexGenSGIX
  :: MonadIO m
  => GLenum -- ^ @mode@ of type @PixelTexGenModeSGIX@.
  -> m ()
glPixelTexGenSGIX v1 = liftIO $ dyn4 ptr_glPixelTexGenSGIX v1

{-# NOINLINE ptr_glPixelTexGenSGIX #-}
ptr_glPixelTexGenSGIX :: FunPtr (GLenum -> IO ())
ptr_glPixelTexGenSGIX = unsafePerformIO $ getCommand "glPixelTexGenSGIX"

-- glPixelTransferf ------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glPixelTransfer.xml OpenGL 2.x>.
glPixelTransferf
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [PixelTransferParameter](Graphics-GL-Groups.html#PixelTransferParameter).
  -> GLfloat -- ^ @param@ of type @CheckedFloat32@.
  -> m ()
glPixelTransferf v1 v2 = liftIO $ dyn0 ptr_glPixelTransferf v1 v2

{-# NOINLINE ptr_glPixelTransferf #-}
ptr_glPixelTransferf :: FunPtr (GLenum -> GLfloat -> IO ())
ptr_glPixelTransferf = unsafePerformIO $ getCommand "glPixelTransferf"

-- glPixelTransferi ------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glPixelTransfer.xml OpenGL 2.x>.
glPixelTransferi
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [PixelTransferParameter](Graphics-GL-Groups.html#PixelTransferParameter).
  -> GLint -- ^ @param@ of type @CheckedInt32@.
  -> m ()
glPixelTransferi v1 v2 = liftIO $ dyn55 ptr_glPixelTransferi v1 v2

{-# NOINLINE ptr_glPixelTransferi #-}
ptr_glPixelTransferi :: FunPtr (GLenum -> GLint -> IO ())
ptr_glPixelTransferi = unsafePerformIO $ getCommand "glPixelTransferi"

-- glPixelTransferxOES ---------------------------------------------------------

glPixelTransferxOES
  :: MonadIO m
  => GLenum -- ^ @pname@.
  -> GLfixed -- ^ @param@.
  -> m ()
glPixelTransferxOES v1 v2 = liftIO $ dyn1 ptr_glPixelTransferxOES v1 v2

{-# NOINLINE ptr_glPixelTransferxOES #-}
ptr_glPixelTransferxOES :: FunPtr (GLenum -> GLfixed -> IO ())
ptr_glPixelTransferxOES = unsafePerformIO $ getCommand "glPixelTransferxOES"

-- glPixelTransformParameterfEXT -----------------------------------------------

glPixelTransformParameterfEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type @PixelTransformTargetEXT@.
  -> GLenum -- ^ @pname@ of type @PixelTransformPNameEXT@.
  -> GLfloat -- ^ @param@.
  -> m ()
glPixelTransformParameterfEXT v1 v2 v3 = liftIO $ dyn161 ptr_glPixelTransformParameterfEXT v1 v2 v3

{-# NOINLINE ptr_glPixelTransformParameterfEXT #-}
ptr_glPixelTransformParameterfEXT :: FunPtr (GLenum -> GLenum -> GLfloat -> IO ())
ptr_glPixelTransformParameterfEXT = unsafePerformIO $ getCommand "glPixelTransformParameterfEXT"

-- glPixelTransformParameterfvEXT ----------------------------------------------

glPixelTransformParameterfvEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type @PixelTransformTargetEXT@.
  -> GLenum -- ^ @pname@ of type @PixelTransformPNameEXT@.
  -> Ptr GLfloat -- ^ @params@ pointing to @1@ element of type @GLfloat@.
  -> m ()
glPixelTransformParameterfvEXT v1 v2 v3 = liftIO $ dyn132 ptr_glPixelTransformParameterfvEXT v1 v2 v3

{-# NOINLINE ptr_glPixelTransformParameterfvEXT #-}
ptr_glPixelTransformParameterfvEXT :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glPixelTransformParameterfvEXT = unsafePerformIO $ getCommand "glPixelTransformParameterfvEXT"

-- glPixelTransformParameteriEXT -----------------------------------------------

glPixelTransformParameteriEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type @PixelTransformTargetEXT@.
  -> GLenum -- ^ @pname@ of type @PixelTransformPNameEXT@.
  -> GLint -- ^ @param@.
  -> m ()
glPixelTransformParameteriEXT v1 v2 v3 = liftIO $ dyn62 ptr_glPixelTransformParameteriEXT v1 v2 v3

{-# NOINLINE ptr_glPixelTransformParameteriEXT #-}
ptr_glPixelTransformParameteriEXT :: FunPtr (GLenum -> GLenum -> GLint -> IO ())
ptr_glPixelTransformParameteriEXT = unsafePerformIO $ getCommand "glPixelTransformParameteriEXT"

-- glPixelTransformParameterivEXT ----------------------------------------------

glPixelTransformParameterivEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type @PixelTransformTargetEXT@.
  -> GLenum -- ^ @pname@ of type @PixelTransformPNameEXT@.
  -> Ptr GLint -- ^ @params@ pointing to @1@ element of type @GLint@.
  -> m ()
glPixelTransformParameterivEXT v1 v2 v3 = liftIO $ dyn133 ptr_glPixelTransformParameterivEXT v1 v2 v3

{-# NOINLINE ptr_glPixelTransformParameterivEXT #-}
ptr_glPixelTransformParameterivEXT :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glPixelTransformParameterivEXT = unsafePerformIO $ getCommand "glPixelTransformParameterivEXT"

-- glPixelZoom -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glPixelZoom.xml OpenGL 2.x>.
glPixelZoom
  :: MonadIO m
  => GLfloat -- ^ @xfactor@.
  -> GLfloat -- ^ @yfactor@.
  -> m ()
glPixelZoom v1 v2 = liftIO $ dyn222 ptr_glPixelZoom v1 v2

{-# NOINLINE ptr_glPixelZoom #-}
ptr_glPixelZoom :: FunPtr (GLfloat -> GLfloat -> IO ())
ptr_glPixelZoom = unsafePerformIO $ getCommand "glPixelZoom"

-- glPixelZoomxOES -------------------------------------------------------------

glPixelZoomxOES
  :: MonadIO m
  => GLfixed -- ^ @xfactor@.
  -> GLfixed -- ^ @yfactor@.
  -> m ()
glPixelZoomxOES v1 v2 = liftIO $ dyn224 ptr_glPixelZoomxOES v1 v2

{-# NOINLINE ptr_glPixelZoomxOES #-}
ptr_glPixelZoomxOES :: FunPtr (GLfixed -> GLfixed -> IO ())
ptr_glPixelZoomxOES = unsafePerformIO $ getCommand "glPixelZoomxOES"

-- glPointAlongPathNV ----------------------------------------------------------

glPointAlongPathNV
  :: MonadIO m
  => GLuint -- ^ @path@ of type @Path@.
  -> GLsizei -- ^ @startSegment@.
  -> GLsizei -- ^ @numSegments@.
  -> GLfloat -- ^ @distance@.
  -> Ptr GLfloat -- ^ @x@ pointing to @1@ element of type @GLfloat@.
  -> Ptr GLfloat -- ^ @y@ pointing to @1@ element of type @GLfloat@.
  -> Ptr GLfloat -- ^ @tangentX@ pointing to @1@ element of type @GLfloat@.
  -> Ptr GLfloat -- ^ @tangentY@ pointing to @1@ element of type @GLfloat@.
  -> m GLboolean -- ^ of type [Boolean](Graphics-GL-Groups.html#Boolean).
glPointAlongPathNV v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn609 ptr_glPointAlongPathNV v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glPointAlongPathNV #-}
ptr_glPointAlongPathNV :: FunPtr (GLuint -> GLsizei -> GLsizei -> GLfloat -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> Ptr GLfloat -> IO GLboolean)
ptr_glPointAlongPathNV = unsafePerformIO $ getCommand "glPointAlongPathNV"

-- glPointParameterf -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glPointParameter.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glPointParameter.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glPointParameter.xhtml OpenGL 4.x>.
glPointParameterf
  :: MonadIO m
  => GLenum -- ^ @pname@ of type @PointParameterNameARB@.
  -> GLfloat -- ^ @param@ of type @CheckedFloat32@.
  -> m ()
glPointParameterf v1 v2 = liftIO $ dyn0 ptr_glPointParameterf v1 v2

{-# NOINLINE ptr_glPointParameterf #-}
ptr_glPointParameterf :: FunPtr (GLenum -> GLfloat -> IO ())
ptr_glPointParameterf = unsafePerformIO $ getCommand "glPointParameterf"

-- glPointParameterfARB --------------------------------------------------------

-- | This command is an alias for 'glPointParameterf'.
glPointParameterfARB
  :: MonadIO m
  => GLenum -- ^ @pname@ of type @PointParameterNameARB@.
  -> GLfloat -- ^ @param@ of type @CheckedFloat32@.
  -> m ()
glPointParameterfARB v1 v2 = liftIO $ dyn0 ptr_glPointParameterfARB v1 v2

{-# NOINLINE ptr_glPointParameterfARB #-}
ptr_glPointParameterfARB :: FunPtr (GLenum -> GLfloat -> IO ())
ptr_glPointParameterfARB = unsafePerformIO $ getCommand "glPointParameterfARB"

-- glPointParameterfEXT --------------------------------------------------------

-- | This command is an alias for 'glPointParameterf'.
glPointParameterfEXT
  :: MonadIO m
  => GLenum -- ^ @pname@ of type @PointParameterNameARB@.
  -> GLfloat -- ^ @param@ of type @CheckedFloat32@.
  -> m ()
glPointParameterfEXT v1 v2 = liftIO $ dyn0 ptr_glPointParameterfEXT v1 v2

{-# NOINLINE ptr_glPointParameterfEXT #-}
ptr_glPointParameterfEXT :: FunPtr (GLenum -> GLfloat -> IO ())
ptr_glPointParameterfEXT = unsafePerformIO $ getCommand "glPointParameterfEXT"

-- glPointParameterfSGIS -------------------------------------------------------

-- | This command is an alias for 'glPointParameterf'.
glPointParameterfSGIS
  :: MonadIO m
  => GLenum -- ^ @pname@ of type @PointParameterNameARB@.
  -> GLfloat -- ^ @param@ of type @CheckedFloat32@.
  -> m ()
glPointParameterfSGIS v1 v2 = liftIO $ dyn0 ptr_glPointParameterfSGIS v1 v2

{-# NOINLINE ptr_glPointParameterfSGIS #-}
ptr_glPointParameterfSGIS :: FunPtr (GLenum -> GLfloat -> IO ())
ptr_glPointParameterfSGIS = unsafePerformIO $ getCommand "glPointParameterfSGIS"

