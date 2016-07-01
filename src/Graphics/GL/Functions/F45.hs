--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F45
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

module Graphics.GL.Functions.F45 (
  glNormalStream3bATI,
  glNormalStream3bvATI,
  glNormalStream3dATI,
  glNormalStream3dvATI,
  glNormalStream3fATI,
  glNormalStream3fvATI,
  glNormalStream3iATI,
  glNormalStream3ivATI,
  glNormalStream3sATI,
  glNormalStream3svATI,
  glObjectLabel,
  glObjectLabelKHR,
  glObjectPtrLabel,
  glObjectPtrLabelKHR,
  glObjectPurgeableAPPLE,
  glObjectUnpurgeableAPPLE,
  glOrtho,
  glOrthof,
  glOrthofOES,
  glOrthox,
  glOrthoxOES,
  glPNTrianglesfATI,
  glPNTrianglesiATI,
  glPassTexCoordATI,
  glPassThrough,
  glPassThroughxOES,
  glPatchParameterfv,
  glPatchParameteri,
  glPatchParameteriEXT,
  glPatchParameteriOES,
  glPathColorGenNV,
  glPathCommandsNV,
  glPathCoordsNV,
  glPathCoverDepthFuncNV,
  glPathDashArrayNV,
  glPathFogGenNV,
  glPathGlyphIndexArrayNV,
  glPathGlyphIndexRangeNV,
  glPathGlyphRangeNV,
  glPathGlyphsNV
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

-- glNormalStream3bATI ---------------------------------------------------------

glNormalStream3bATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type @VertexStreamATI@.
  -> GLbyte -- ^ @nx@.
  -> GLbyte -- ^ @ny@.
  -> GLbyte -- ^ @nz@.
  -> m ()
glNormalStream3bATI v1 v2 v3 v4 = liftIO $ dyn552 ptr_glNormalStream3bATI v1 v2 v3 v4

{-# NOINLINE ptr_glNormalStream3bATI #-}
ptr_glNormalStream3bATI :: FunPtr (GLenum -> GLbyte -> GLbyte -> GLbyte -> IO ())
ptr_glNormalStream3bATI = unsafePerformIO $ getCommand "glNormalStream3bATI"

-- glNormalStream3bvATI --------------------------------------------------------

glNormalStream3bvATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type @VertexStreamATI@.
  -> Ptr GLbyte -- ^ @coords@ pointing to @3@ elements of type @GLbyte@.
  -> m ()
glNormalStream3bvATI v1 v2 = liftIO $ dyn540 ptr_glNormalStream3bvATI v1 v2

{-# NOINLINE ptr_glNormalStream3bvATI #-}
ptr_glNormalStream3bvATI :: FunPtr (GLenum -> Ptr GLbyte -> IO ())
ptr_glNormalStream3bvATI = unsafePerformIO $ getCommand "glNormalStream3bvATI"

-- glNormalStream3dATI ---------------------------------------------------------

glNormalStream3dATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type @VertexStreamATI@.
  -> GLdouble -- ^ @nx@.
  -> GLdouble -- ^ @ny@.
  -> GLdouble -- ^ @nz@.
  -> m ()
glNormalStream3dATI v1 v2 v3 v4 = liftIO $ dyn522 ptr_glNormalStream3dATI v1 v2 v3 v4

{-# NOINLINE ptr_glNormalStream3dATI #-}
ptr_glNormalStream3dATI :: FunPtr (GLenum -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glNormalStream3dATI = unsafePerformIO $ getCommand "glNormalStream3dATI"

-- glNormalStream3dvATI --------------------------------------------------------

glNormalStream3dvATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type @VertexStreamATI@.
  -> Ptr GLdouble -- ^ @coords@ pointing to @3@ elements of type @GLdouble@.
  -> m ()
glNormalStream3dvATI v1 v2 = liftIO $ dyn93 ptr_glNormalStream3dvATI v1 v2

{-# NOINLINE ptr_glNormalStream3dvATI #-}
ptr_glNormalStream3dvATI :: FunPtr (GLenum -> Ptr GLdouble -> IO ())
ptr_glNormalStream3dvATI = unsafePerformIO $ getCommand "glNormalStream3dvATI"

-- glNormalStream3fATI ---------------------------------------------------------

glNormalStream3fATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type @VertexStreamATI@.
  -> GLfloat -- ^ @nx@.
  -> GLfloat -- ^ @ny@.
  -> GLfloat -- ^ @nz@.
  -> m ()
glNormalStream3fATI v1 v2 v3 v4 = liftIO $ dyn523 ptr_glNormalStream3fATI v1 v2 v3 v4

{-# NOINLINE ptr_glNormalStream3fATI #-}
ptr_glNormalStream3fATI :: FunPtr (GLenum -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glNormalStream3fATI = unsafePerformIO $ getCommand "glNormalStream3fATI"

-- glNormalStream3fvATI --------------------------------------------------------

glNormalStream3fvATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type @VertexStreamATI@.
  -> Ptr GLfloat -- ^ @coords@ pointing to @3@ elements of type @GLfloat@.
  -> m ()
glNormalStream3fvATI v1 v2 = liftIO $ dyn94 ptr_glNormalStream3fvATI v1 v2

{-# NOINLINE ptr_glNormalStream3fvATI #-}
ptr_glNormalStream3fvATI :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glNormalStream3fvATI = unsafePerformIO $ getCommand "glNormalStream3fvATI"

-- glNormalStream3iATI ---------------------------------------------------------

glNormalStream3iATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type @VertexStreamATI@.
  -> GLint -- ^ @nx@.
  -> GLint -- ^ @ny@.
  -> GLint -- ^ @nz@.
  -> m ()
glNormalStream3iATI v1 v2 v3 v4 = liftIO $ dyn554 ptr_glNormalStream3iATI v1 v2 v3 v4

{-# NOINLINE ptr_glNormalStream3iATI #-}
ptr_glNormalStream3iATI :: FunPtr (GLenum -> GLint -> GLint -> GLint -> IO ())
ptr_glNormalStream3iATI = unsafePerformIO $ getCommand "glNormalStream3iATI"

-- glNormalStream3ivATI --------------------------------------------------------

glNormalStream3ivATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type @VertexStreamATI@.
  -> Ptr GLint -- ^ @coords@ pointing to @3@ elements of type @GLint@.
  -> m ()
glNormalStream3ivATI v1 v2 = liftIO $ dyn136 ptr_glNormalStream3ivATI v1 v2

{-# NOINLINE ptr_glNormalStream3ivATI #-}
ptr_glNormalStream3ivATI :: FunPtr (GLenum -> Ptr GLint -> IO ())
ptr_glNormalStream3ivATI = unsafePerformIO $ getCommand "glNormalStream3ivATI"

-- glNormalStream3sATI ---------------------------------------------------------

glNormalStream3sATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type @VertexStreamATI@.
  -> GLshort -- ^ @nx@.
  -> GLshort -- ^ @ny@.
  -> GLshort -- ^ @nz@.
  -> m ()
glNormalStream3sATI v1 v2 v3 v4 = liftIO $ dyn555 ptr_glNormalStream3sATI v1 v2 v3 v4

{-# NOINLINE ptr_glNormalStream3sATI #-}
ptr_glNormalStream3sATI :: FunPtr (GLenum -> GLshort -> GLshort -> GLshort -> IO ())
ptr_glNormalStream3sATI = unsafePerformIO $ getCommand "glNormalStream3sATI"

-- glNormalStream3svATI --------------------------------------------------------

glNormalStream3svATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type @VertexStreamATI@.
  -> Ptr GLshort -- ^ @coords@ pointing to @3@ elements of type @GLshort@.
  -> m ()
glNormalStream3svATI v1 v2 = liftIO $ dyn545 ptr_glNormalStream3svATI v1 v2

{-# NOINLINE ptr_glNormalStream3svATI #-}
ptr_glNormalStream3svATI :: FunPtr (GLenum -> Ptr GLshort -> IO ())
ptr_glNormalStream3svATI = unsafePerformIO $ getCommand "glNormalStream3svATI"

-- glObjectLabel ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glObjectLabel.xhtml OpenGL 4.x>.
glObjectLabel
  :: MonadIO m
  => GLenum -- ^ @identifier@.
  -> GLuint -- ^ @name@.
  -> GLsizei -- ^ @length@.
  -> Ptr GLchar -- ^ @label@ pointing to @COMPSIZE(label,length)@ elements of type @GLchar@.
  -> m ()
glObjectLabel v1 v2 v3 v4 = liftIO $ dyn484 ptr_glObjectLabel v1 v2 v3 v4

{-# NOINLINE ptr_glObjectLabel #-}
ptr_glObjectLabel :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLchar -> IO ())
ptr_glObjectLabel = unsafePerformIO $ getCommand "glObjectLabel"

-- glObjectLabelKHR ------------------------------------------------------------

-- | This command is an alias for 'glObjectLabel'.
glObjectLabelKHR
  :: MonadIO m
  => GLenum -- ^ @identifier@.
  -> GLuint -- ^ @name@.
  -> GLsizei -- ^ @length@.
  -> Ptr GLchar -- ^ @label@.
  -> m ()
glObjectLabelKHR v1 v2 v3 v4 = liftIO $ dyn484 ptr_glObjectLabelKHR v1 v2 v3 v4

{-# NOINLINE ptr_glObjectLabelKHR #-}
ptr_glObjectLabelKHR :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLchar -> IO ())
ptr_glObjectLabelKHR = unsafePerformIO $ getCommand "glObjectLabelKHR"

-- glObjectPtrLabel ------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glObjectPtrLabel.xhtml OpenGL 4.x>.
glObjectPtrLabel
  :: MonadIO m
  => Ptr a -- ^ @ptr@.
  -> GLsizei -- ^ @length@.
  -> Ptr GLchar -- ^ @label@ pointing to @COMPSIZE(label,length)@ elements of type @GLchar@.
  -> m ()
glObjectPtrLabel v1 v2 v3 = liftIO $ dyn594 ptr_glObjectPtrLabel v1 v2 v3

{-# NOINLINE ptr_glObjectPtrLabel #-}
ptr_glObjectPtrLabel :: FunPtr (Ptr a -> GLsizei -> Ptr GLchar -> IO ())
ptr_glObjectPtrLabel = unsafePerformIO $ getCommand "glObjectPtrLabel"

-- glObjectPtrLabelKHR ---------------------------------------------------------

-- | This command is an alias for 'glObjectPtrLabel'.
glObjectPtrLabelKHR
  :: MonadIO m
  => Ptr a -- ^ @ptr@.
  -> GLsizei -- ^ @length@.
  -> Ptr GLchar -- ^ @label@.
  -> m ()
glObjectPtrLabelKHR v1 v2 v3 = liftIO $ dyn594 ptr_glObjectPtrLabelKHR v1 v2 v3

{-# NOINLINE ptr_glObjectPtrLabelKHR #-}
ptr_glObjectPtrLabelKHR :: FunPtr (Ptr a -> GLsizei -> Ptr GLchar -> IO ())
ptr_glObjectPtrLabelKHR = unsafePerformIO $ getCommand "glObjectPtrLabelKHR"

-- glObjectPurgeableAPPLE ------------------------------------------------------

glObjectPurgeableAPPLE
  :: MonadIO m
  => GLenum -- ^ @objectType@.
  -> GLuint -- ^ @name@.
  -> GLenum -- ^ @option@.
  -> m GLenum
glObjectPurgeableAPPLE v1 v2 v3 = liftIO $ dyn595 ptr_glObjectPurgeableAPPLE v1 v2 v3

{-# NOINLINE ptr_glObjectPurgeableAPPLE #-}
ptr_glObjectPurgeableAPPLE :: FunPtr (GLenum -> GLuint -> GLenum -> IO GLenum)
ptr_glObjectPurgeableAPPLE = unsafePerformIO $ getCommand "glObjectPurgeableAPPLE"

-- glObjectUnpurgeableAPPLE ----------------------------------------------------

glObjectUnpurgeableAPPLE
  :: MonadIO m
  => GLenum -- ^ @objectType@.
  -> GLuint -- ^ @name@.
  -> GLenum -- ^ @option@.
  -> m GLenum
glObjectUnpurgeableAPPLE v1 v2 v3 = liftIO $ dyn595 ptr_glObjectUnpurgeableAPPLE v1 v2 v3

{-# NOINLINE ptr_glObjectUnpurgeableAPPLE #-}
ptr_glObjectUnpurgeableAPPLE :: FunPtr (GLenum -> GLuint -> GLenum -> IO GLenum)
ptr_glObjectUnpurgeableAPPLE = unsafePerformIO $ getCommand "glObjectUnpurgeableAPPLE"

-- glOrtho ---------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glOrtho.xml OpenGL 2.x>.
glOrtho
  :: MonadIO m
  => GLdouble -- ^ @left@.
  -> GLdouble -- ^ @right@.
  -> GLdouble -- ^ @bottom@.
  -> GLdouble -- ^ @top@.
  -> GLdouble -- ^ @zNear@.
  -> GLdouble -- ^ @zFar@.
  -> m ()
glOrtho v1 v2 v3 v4 v5 v6 = liftIO $ dyn295 ptr_glOrtho v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glOrtho #-}
ptr_glOrtho :: FunPtr (GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glOrtho = unsafePerformIO $ getCommand "glOrtho"

-- glOrthof --------------------------------------------------------------------

glOrthof
  :: MonadIO m
  => GLfloat -- ^ @l@.
  -> GLfloat -- ^ @r@.
  -> GLfloat -- ^ @b@.
  -> GLfloat -- ^ @t@.
  -> GLfloat -- ^ @n@.
  -> GLfloat -- ^ @f@.
  -> m ()
glOrthof v1 v2 v3 v4 v5 v6 = liftIO $ dyn96 ptr_glOrthof v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glOrthof #-}
ptr_glOrthof :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glOrthof = unsafePerformIO $ getCommand "glOrthof"

-- glOrthofOES -----------------------------------------------------------------

glOrthofOES
  :: MonadIO m
  => GLfloat -- ^ @l@.
  -> GLfloat -- ^ @r@.
  -> GLfloat -- ^ @b@.
  -> GLfloat -- ^ @t@.
  -> GLfloat -- ^ @n@.
  -> GLfloat -- ^ @f@.
  -> m ()
glOrthofOES v1 v2 v3 v4 v5 v6 = liftIO $ dyn96 ptr_glOrthofOES v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glOrthofOES #-}
ptr_glOrthofOES :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glOrthofOES = unsafePerformIO $ getCommand "glOrthofOES"

-- glOrthox --------------------------------------------------------------------

glOrthox
  :: MonadIO m
  => GLfixed -- ^ @l@.
  -> GLfixed -- ^ @r@.
  -> GLfixed -- ^ @b@.
  -> GLfixed -- ^ @t@.
  -> GLfixed -- ^ @n@.
  -> GLfixed -- ^ @f@.
  -> m ()
glOrthox v1 v2 v3 v4 v5 v6 = liftIO $ dyn296 ptr_glOrthox v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glOrthox #-}
ptr_glOrthox :: FunPtr (GLfixed -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ())
ptr_glOrthox = unsafePerformIO $ getCommand "glOrthox"

-- glOrthoxOES -----------------------------------------------------------------

glOrthoxOES
  :: MonadIO m
  => GLfixed -- ^ @l@.
  -> GLfixed -- ^ @r@.
  -> GLfixed -- ^ @b@.
  -> GLfixed -- ^ @t@.
  -> GLfixed -- ^ @n@.
  -> GLfixed -- ^ @f@.
  -> m ()
glOrthoxOES v1 v2 v3 v4 v5 v6 = liftIO $ dyn296 ptr_glOrthoxOES v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glOrthoxOES #-}
ptr_glOrthoxOES :: FunPtr (GLfixed -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> GLfixed -> IO ())
ptr_glOrthoxOES = unsafePerformIO $ getCommand "glOrthoxOES"

-- glPNTrianglesfATI -----------------------------------------------------------

glPNTrianglesfATI
  :: MonadIO m
  => GLenum -- ^ @pname@ of type @PNTrianglesPNameATI@.
  -> GLfloat -- ^ @param@.
  -> m ()
glPNTrianglesfATI v1 v2 = liftIO $ dyn0 ptr_glPNTrianglesfATI v1 v2

{-# NOINLINE ptr_glPNTrianglesfATI #-}
ptr_glPNTrianglesfATI :: FunPtr (GLenum -> GLfloat -> IO ())
ptr_glPNTrianglesfATI = unsafePerformIO $ getCommand "glPNTrianglesfATI"

-- glPNTrianglesiATI -----------------------------------------------------------

glPNTrianglesiATI
  :: MonadIO m
  => GLenum -- ^ @pname@ of type @PNTrianglesPNameATI@.
  -> GLint -- ^ @param@.
  -> m ()
glPNTrianglesiATI v1 v2 = liftIO $ dyn55 ptr_glPNTrianglesiATI v1 v2

{-# NOINLINE ptr_glPNTrianglesiATI #-}
ptr_glPNTrianglesiATI :: FunPtr (GLenum -> GLint -> IO ())
ptr_glPNTrianglesiATI = unsafePerformIO $ getCommand "glPNTrianglesiATI"

-- glPassTexCoordATI -----------------------------------------------------------

glPassTexCoordATI
  :: MonadIO m
  => GLuint -- ^ @dst@.
  -> GLuint -- ^ @coord@.
  -> GLenum -- ^ @swizzle@ of type @SwizzleOpATI@.
  -> m ()
glPassTexCoordATI v1 v2 v3 = liftIO $ dyn596 ptr_glPassTexCoordATI v1 v2 v3

{-# NOINLINE ptr_glPassTexCoordATI #-}
ptr_glPassTexCoordATI :: FunPtr (GLuint -> GLuint -> GLenum -> IO ())
ptr_glPassTexCoordATI = unsafePerformIO $ getCommand "glPassTexCoordATI"

-- glPassThrough ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glPassThrough.xml OpenGL 2.x>.
glPassThrough
  :: MonadIO m
  => GLfloat -- ^ @token@ of type @FeedbackElement@.
  -> m ()
glPassThrough v1 = liftIO $ dyn79 ptr_glPassThrough v1

{-# NOINLINE ptr_glPassThrough #-}
ptr_glPassThrough :: FunPtr (GLfloat -> IO ())
ptr_glPassThrough = unsafePerformIO $ getCommand "glPassThrough"

-- glPassThroughxOES -----------------------------------------------------------

glPassThroughxOES
  :: MonadIO m
  => GLfixed -- ^ @token@.
  -> m ()
glPassThroughxOES v1 = liftIO $ dyn81 ptr_glPassThroughxOES v1

{-# NOINLINE ptr_glPassThroughxOES #-}
ptr_glPassThroughxOES :: FunPtr (GLfixed -> IO ())
ptr_glPassThroughxOES = unsafePerformIO $ getCommand "glPassThroughxOES"

-- glPatchParameterfv ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glPatchParameter.xhtml OpenGL 4.x>.
glPatchParameterfv
  :: MonadIO m
  => GLenum -- ^ @pname@.
  -> Ptr GLfloat -- ^ @values@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glPatchParameterfv v1 v2 = liftIO $ dyn94 ptr_glPatchParameterfv v1 v2

{-# NOINLINE ptr_glPatchParameterfv #-}
ptr_glPatchParameterfv :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glPatchParameterfv = unsafePerformIO $ getCommand "glPatchParameterfv"

-- glPatchParameteri -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glPatchParameter.xhtml OpenGL 4.x>.
glPatchParameteri
  :: MonadIO m
  => GLenum -- ^ @pname@.
  -> GLint -- ^ @value@.
  -> m ()
glPatchParameteri v1 v2 = liftIO $ dyn55 ptr_glPatchParameteri v1 v2

{-# NOINLINE ptr_glPatchParameteri #-}
ptr_glPatchParameteri :: FunPtr (GLenum -> GLint -> IO ())
ptr_glPatchParameteri = unsafePerformIO $ getCommand "glPatchParameteri"

-- glPatchParameteriEXT --------------------------------------------------------

-- | This command is an alias for 'glPatchParameteri'.
glPatchParameteriEXT
  :: MonadIO m
  => GLenum -- ^ @pname@.
  -> GLint -- ^ @value@.
  -> m ()
glPatchParameteriEXT v1 v2 = liftIO $ dyn55 ptr_glPatchParameteriEXT v1 v2

{-# NOINLINE ptr_glPatchParameteriEXT #-}
ptr_glPatchParameteriEXT :: FunPtr (GLenum -> GLint -> IO ())
ptr_glPatchParameteriEXT = unsafePerformIO $ getCommand "glPatchParameteriEXT"

-- glPatchParameteriOES --------------------------------------------------------

-- | This command is an alias for 'glPatchParameteri'.
glPatchParameteriOES
  :: MonadIO m
  => GLenum -- ^ @pname@.
  -> GLint -- ^ @value@.
  -> m ()
glPatchParameteriOES v1 v2 = liftIO $ dyn55 ptr_glPatchParameteriOES v1 v2

{-# NOINLINE ptr_glPatchParameteriOES #-}
ptr_glPatchParameteriOES :: FunPtr (GLenum -> GLint -> IO ())
ptr_glPatchParameteriOES = unsafePerformIO $ getCommand "glPatchParameteriOES"

-- glPathColorGenNV ------------------------------------------------------------

glPathColorGenNV
  :: MonadIO m
  => GLenum -- ^ @color@ of type @PathColor@.
  -> GLenum -- ^ @genMode@ of type @PathGenMode@.
  -> GLenum -- ^ @colorFormat@ of type @PathColorFormat@.
  -> Ptr GLfloat -- ^ @coeffs@ pointing to @COMPSIZE(genMode,colorFormat)@ elements of type @GLfloat@.
  -> m ()
glPathColorGenNV v1 v2 v3 v4 = liftIO $ dyn320 ptr_glPathColorGenNV v1 v2 v3 v4

{-# NOINLINE ptr_glPathColorGenNV #-}
ptr_glPathColorGenNV :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glPathColorGenNV = unsafePerformIO $ getCommand "glPathColorGenNV"

-- glPathCommandsNV ------------------------------------------------------------

glPathCommandsNV
  :: MonadIO m
  => GLuint -- ^ @path@ of type @Path@.
  -> GLsizei -- ^ @numCommands@.
  -> Ptr GLubyte -- ^ @commands@ pointing to @numCommands@ elements of type @PathCommand@.
  -> GLsizei -- ^ @numCoords@.
  -> GLenum -- ^ @coordType@ of type @PathCoordType@.
  -> Ptr a -- ^ @coords@ pointing to @COMPSIZE(numCoords,coordType)@ elements of type @a@.
  -> m ()
glPathCommandsNV v1 v2 v3 v4 v5 v6 = liftIO $ dyn597 ptr_glPathCommandsNV v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glPathCommandsNV #-}
ptr_glPathCommandsNV :: FunPtr (GLuint -> GLsizei -> Ptr GLubyte -> GLsizei -> GLenum -> Ptr a -> IO ())
ptr_glPathCommandsNV = unsafePerformIO $ getCommand "glPathCommandsNV"

-- glPathCoordsNV --------------------------------------------------------------

glPathCoordsNV
  :: MonadIO m
  => GLuint -- ^ @path@ of type @Path@.
  -> GLsizei -- ^ @numCoords@.
  -> GLenum -- ^ @coordType@ of type @PathCoordType@.
  -> Ptr a -- ^ @coords@ pointing to @COMPSIZE(numCoords,coordType)@ elements of type @a@.
  -> m ()
glPathCoordsNV v1 v2 v3 v4 = liftIO $ dyn598 ptr_glPathCoordsNV v1 v2 v3 v4

{-# NOINLINE ptr_glPathCoordsNV #-}
ptr_glPathCoordsNV :: FunPtr (GLuint -> GLsizei -> GLenum -> Ptr a -> IO ())
ptr_glPathCoordsNV = unsafePerformIO $ getCommand "glPathCoordsNV"

-- glPathCoverDepthFuncNV ------------------------------------------------------

glPathCoverDepthFuncNV
  :: MonadIO m
  => GLenum -- ^ @func@ of type [DepthFunction](Graphics-GL-Groups.html#DepthFunction).
  -> m ()
glPathCoverDepthFuncNV v1 = liftIO $ dyn4 ptr_glPathCoverDepthFuncNV v1

{-# NOINLINE ptr_glPathCoverDepthFuncNV #-}
ptr_glPathCoverDepthFuncNV :: FunPtr (GLenum -> IO ())
ptr_glPathCoverDepthFuncNV = unsafePerformIO $ getCommand "glPathCoverDepthFuncNV"

-- glPathDashArrayNV -----------------------------------------------------------

glPathDashArrayNV
  :: MonadIO m
  => GLuint -- ^ @path@ of type @Path@.
  -> GLsizei -- ^ @dashCount@.
  -> Ptr GLfloat -- ^ @dashArray@ pointing to @dashCount@ elements of type @GLfloat@.
  -> m ()
glPathDashArrayNV v1 v2 v3 = liftIO $ dyn218 ptr_glPathDashArrayNV v1 v2 v3

{-# NOINLINE ptr_glPathDashArrayNV #-}
ptr_glPathDashArrayNV :: FunPtr (GLuint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glPathDashArrayNV = unsafePerformIO $ getCommand "glPathDashArrayNV"

-- glPathFogGenNV --------------------------------------------------------------

glPathFogGenNV
  :: MonadIO m
  => GLenum -- ^ @genMode@ of type @PathGenMode@.
  -> m ()
glPathFogGenNV v1 = liftIO $ dyn4 ptr_glPathFogGenNV v1

{-# NOINLINE ptr_glPathFogGenNV #-}
ptr_glPathFogGenNV :: FunPtr (GLenum -> IO ())
ptr_glPathFogGenNV = unsafePerformIO $ getCommand "glPathFogGenNV"

-- glPathGlyphIndexArrayNV -----------------------------------------------------

glPathGlyphIndexArrayNV
  :: MonadIO m
  => GLuint -- ^ @firstPathName@.
  -> GLenum -- ^ @fontTarget@.
  -> Ptr a -- ^ @fontName@.
  -> GLbitfield -- ^ @fontStyle@.
  -> GLuint -- ^ @firstGlyphIndex@.
  -> GLsizei -- ^ @numGlyphs@.
  -> GLuint -- ^ @pathParameterTemplate@.
  -> GLfloat -- ^ @emScale@.
  -> m GLenum
glPathGlyphIndexArrayNV v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn599 ptr_glPathGlyphIndexArrayNV v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glPathGlyphIndexArrayNV #-}
ptr_glPathGlyphIndexArrayNV :: FunPtr (GLuint -> GLenum -> Ptr a -> GLbitfield -> GLuint -> GLsizei -> GLuint -> GLfloat -> IO GLenum)
ptr_glPathGlyphIndexArrayNV = unsafePerformIO $ getCommand "glPathGlyphIndexArrayNV"

-- glPathGlyphIndexRangeNV -----------------------------------------------------

glPathGlyphIndexRangeNV
  :: MonadIO m
  => GLenum -- ^ @fontTarget@.
  -> Ptr a -- ^ @fontName@.
  -> GLbitfield -- ^ @fontStyle@.
  -> GLuint -- ^ @pathParameterTemplate@.
  -> GLfloat -- ^ @emScale@.
  -> Ptr GLuint -- ^ @baseAndCount@.
  -> m GLenum
glPathGlyphIndexRangeNV v1 v2 v3 v4 v5 v6 = liftIO $ dyn600 ptr_glPathGlyphIndexRangeNV v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glPathGlyphIndexRangeNV #-}
ptr_glPathGlyphIndexRangeNV :: FunPtr (GLenum -> Ptr a -> GLbitfield -> GLuint -> GLfloat -> Ptr GLuint -> IO GLenum)
ptr_glPathGlyphIndexRangeNV = unsafePerformIO $ getCommand "glPathGlyphIndexRangeNV"

-- glPathGlyphRangeNV ----------------------------------------------------------

glPathGlyphRangeNV
  :: MonadIO m
  => GLuint -- ^ @firstPathName@ of type @Path@.
  -> GLenum -- ^ @fontTarget@ of type @PathFontTarget@.
  -> Ptr a -- ^ @fontName@ pointing to @COMPSIZE(fontTarget,fontName)@ elements of type @a@.
  -> GLbitfield -- ^ @fontStyle@ of type @PathFontStyle@.
  -> GLuint -- ^ @firstGlyph@.
  -> GLsizei -- ^ @numGlyphs@.
  -> GLenum -- ^ @handleMissingGlyphs@ of type @PathHandleMissingGlyphs@.
  -> GLuint -- ^ @pathParameterTemplate@ of type @Path@.
  -> GLfloat -- ^ @emScale@.
  -> m ()
glPathGlyphRangeNV v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn601 ptr_glPathGlyphRangeNV v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glPathGlyphRangeNV #-}
ptr_glPathGlyphRangeNV :: FunPtr (GLuint -> GLenum -> Ptr a -> GLbitfield -> GLuint -> GLsizei -> GLenum -> GLuint -> GLfloat -> IO ())
ptr_glPathGlyphRangeNV = unsafePerformIO $ getCommand "glPathGlyphRangeNV"

-- glPathGlyphsNV --------------------------------------------------------------

glPathGlyphsNV
  :: MonadIO m
  => GLuint -- ^ @firstPathName@ of type @Path@.
  -> GLenum -- ^ @fontTarget@ of type @PathFontTarget@.
  -> Ptr a -- ^ @fontName@ pointing to @COMPSIZE(fontTarget,fontName)@ elements of type @a@.
  -> GLbitfield -- ^ @fontStyle@ of type @PathFontStyle@.
  -> GLsizei -- ^ @numGlyphs@.
  -> GLenum -- ^ @type@ of type @PathElementType@.
  -> Ptr b -- ^ @charcodes@ pointing to @COMPSIZE(numGlyphs,type,charcodes)@ elements of type @b@.
  -> GLenum -- ^ @handleMissingGlyphs@ of type @PathHandleMissingGlyphs@.
  -> GLuint -- ^ @pathParameterTemplate@ of type @Path@.
  -> GLfloat -- ^ @emScale@.
  -> m ()
glPathGlyphsNV v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO $ dyn602 ptr_glPathGlyphsNV v1 v2 v3 v4 v5 v6 v7 v8 v9 v10

{-# NOINLINE ptr_glPathGlyphsNV #-}
ptr_glPathGlyphsNV :: FunPtr (GLuint -> GLenum -> Ptr a -> GLbitfield -> GLsizei -> GLenum -> Ptr b -> GLenum -> GLuint -> GLfloat -> IO ())
ptr_glPathGlyphsNV = unsafePerformIO $ getCommand "glPathGlyphsNV"

