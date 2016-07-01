--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F57
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

module Graphics.GL.Functions.F57 (
  glSecondaryColor3uiEXT,
  glSecondaryColor3uiv,
  glSecondaryColor3uivEXT,
  glSecondaryColor3us,
  glSecondaryColor3usEXT,
  glSecondaryColor3usv,
  glSecondaryColor3usvEXT,
  glSecondaryColorFormatNV,
  glSecondaryColorP3ui,
  glSecondaryColorP3uiv,
  glSecondaryColorPointer,
  glSecondaryColorPointerEXT,
  glSecondaryColorPointerListIBM,
  glSelectBuffer,
  glSelectPerfMonitorCountersAMD,
  glSeparableFilter2D,
  glSeparableFilter2DEXT,
  glSetFenceAPPLE,
  glSetFenceNV,
  glSetFragmentShaderConstantATI,
  glSetInvariantEXT,
  glSetLocalConstantEXT,
  glSetMultisamplefvAMD,
  glShadeModel,
  glShaderBinary,
  glShaderOp1EXT,
  glShaderOp2EXT,
  glShaderOp3EXT,
  glShaderSource,
  glShaderSourceARB,
  glShaderStorageBlockBinding,
  glSharpenTexFuncSGIS,
  glSpriteParameterfSGIX,
  glSpriteParameterfvSGIX,
  glSpriteParameteriSGIX,
  glSpriteParameterivSGIX,
  glStartInstrumentsSGIX,
  glStartTilingQCOM,
  glStateCaptureNV,
  glStencilClearTagEXT
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

-- glSecondaryColor3uiEXT ------------------------------------------------------

-- | The vector equivalent of this command is 'glSecondaryColor3uivEXT'. This command is an alias for 'glSecondaryColor3ui'.
glSecondaryColor3uiEXT
  :: MonadIO m
  => GLuint -- ^ @red@ of type @ColorUI@.
  -> GLuint -- ^ @green@ of type @ColorUI@.
  -> GLuint -- ^ @blue@ of type @ColorUI@.
  -> m ()
glSecondaryColor3uiEXT v1 v2 v3 = liftIO $ dyn102 ptr_glSecondaryColor3uiEXT v1 v2 v3

{-# NOINLINE ptr_glSecondaryColor3uiEXT #-}
ptr_glSecondaryColor3uiEXT :: FunPtr (GLuint -> GLuint -> GLuint -> IO ())
ptr_glSecondaryColor3uiEXT = unsafePerformIO $ getCommand "glSecondaryColor3uiEXT"

-- glSecondaryColor3uiv --------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glSecondaryColor.xml OpenGL 2.x>.
glSecondaryColor3uiv
  :: MonadIO m
  => Ptr GLuint -- ^ @v@ pointing to @3@ elements of type @ColorUI@.
  -> m ()
glSecondaryColor3uiv v1 = liftIO $ dyn103 ptr_glSecondaryColor3uiv v1

{-# NOINLINE ptr_glSecondaryColor3uiv #-}
ptr_glSecondaryColor3uiv :: FunPtr (Ptr GLuint -> IO ())
ptr_glSecondaryColor3uiv = unsafePerformIO $ getCommand "glSecondaryColor3uiv"

-- glSecondaryColor3uivEXT -----------------------------------------------------

-- | This command is an alias for 'glSecondaryColor3uiv'.
glSecondaryColor3uivEXT
  :: MonadIO m
  => Ptr GLuint -- ^ @v@ pointing to @3@ elements of type @ColorUI@.
  -> m ()
glSecondaryColor3uivEXT v1 = liftIO $ dyn103 ptr_glSecondaryColor3uivEXT v1

{-# NOINLINE ptr_glSecondaryColor3uivEXT #-}
ptr_glSecondaryColor3uivEXT :: FunPtr (Ptr GLuint -> IO ())
ptr_glSecondaryColor3uivEXT = unsafePerformIO $ getCommand "glSecondaryColor3uivEXT"

-- glSecondaryColor3us ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glSecondaryColor.xml OpenGL 2.x>. The vector equivalent of this command is 'glSecondaryColor3usv'.
glSecondaryColor3us
  :: MonadIO m
  => GLushort -- ^ @red@ of type @ColorUS@.
  -> GLushort -- ^ @green@ of type @ColorUS@.
  -> GLushort -- ^ @blue@ of type @ColorUS@.
  -> m ()
glSecondaryColor3us v1 v2 v3 = liftIO $ dyn104 ptr_glSecondaryColor3us v1 v2 v3

{-# NOINLINE ptr_glSecondaryColor3us #-}
ptr_glSecondaryColor3us :: FunPtr (GLushort -> GLushort -> GLushort -> IO ())
ptr_glSecondaryColor3us = unsafePerformIO $ getCommand "glSecondaryColor3us"

-- glSecondaryColor3usEXT ------------------------------------------------------

-- | The vector equivalent of this command is 'glSecondaryColor3usvEXT'. This command is an alias for 'glSecondaryColor3us'.
glSecondaryColor3usEXT
  :: MonadIO m
  => GLushort -- ^ @red@ of type @ColorUS@.
  -> GLushort -- ^ @green@ of type @ColorUS@.
  -> GLushort -- ^ @blue@ of type @ColorUS@.
  -> m ()
glSecondaryColor3usEXT v1 v2 v3 = liftIO $ dyn104 ptr_glSecondaryColor3usEXT v1 v2 v3

{-# NOINLINE ptr_glSecondaryColor3usEXT #-}
ptr_glSecondaryColor3usEXT :: FunPtr (GLushort -> GLushort -> GLushort -> IO ())
ptr_glSecondaryColor3usEXT = unsafePerformIO $ getCommand "glSecondaryColor3usEXT"

-- glSecondaryColor3usv --------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glSecondaryColor.xml OpenGL 2.x>.
glSecondaryColor3usv
  :: MonadIO m
  => Ptr GLushort -- ^ @v@ pointing to @3@ elements of type @ColorUS@.
  -> m ()
glSecondaryColor3usv v1 = liftIO $ dyn105 ptr_glSecondaryColor3usv v1

{-# NOINLINE ptr_glSecondaryColor3usv #-}
ptr_glSecondaryColor3usv :: FunPtr (Ptr GLushort -> IO ())
ptr_glSecondaryColor3usv = unsafePerformIO $ getCommand "glSecondaryColor3usv"

-- glSecondaryColor3usvEXT -----------------------------------------------------

-- | This command is an alias for 'glSecondaryColor3usv'.
glSecondaryColor3usvEXT
  :: MonadIO m
  => Ptr GLushort -- ^ @v@ pointing to @3@ elements of type @ColorUS@.
  -> m ()
glSecondaryColor3usvEXT v1 = liftIO $ dyn105 ptr_glSecondaryColor3usvEXT v1

{-# NOINLINE ptr_glSecondaryColor3usvEXT #-}
ptr_glSecondaryColor3usvEXT :: FunPtr (Ptr GLushort -> IO ())
ptr_glSecondaryColor3usvEXT = unsafePerformIO $ getCommand "glSecondaryColor3usvEXT"

-- glSecondaryColorFormatNV ----------------------------------------------------

glSecondaryColorFormatNV
  :: MonadIO m
  => GLint -- ^ @size@.
  -> GLenum -- ^ @type@.
  -> GLsizei -- ^ @stride@.
  -> m ()
glSecondaryColorFormatNV v1 v2 v3 = liftIO $ dyn119 ptr_glSecondaryColorFormatNV v1 v2 v3

{-# NOINLINE ptr_glSecondaryColorFormatNV #-}
ptr_glSecondaryColorFormatNV :: FunPtr (GLint -> GLenum -> GLsizei -> IO ())
ptr_glSecondaryColorFormatNV = unsafePerformIO $ getCommand "glSecondaryColorFormatNV"

-- glSecondaryColorP3ui --------------------------------------------------------

glSecondaryColorP3ui
  :: MonadIO m
  => GLenum -- ^ @type@.
  -> GLuint -- ^ @color@.
  -> m ()
glSecondaryColorP3ui v1 v2 = liftIO $ dyn16 ptr_glSecondaryColorP3ui v1 v2

{-# NOINLINE ptr_glSecondaryColorP3ui #-}
ptr_glSecondaryColorP3ui :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glSecondaryColorP3ui = unsafePerformIO $ getCommand "glSecondaryColorP3ui"

-- glSecondaryColorP3uiv -------------------------------------------------------

glSecondaryColorP3uiv
  :: MonadIO m
  => GLenum -- ^ @type@.
  -> Ptr GLuint -- ^ @color@ pointing to @1@ element of type @GLuint@.
  -> m ()
glSecondaryColorP3uiv v1 v2 = liftIO $ dyn125 ptr_glSecondaryColorP3uiv v1 v2

{-# NOINLINE ptr_glSecondaryColorP3uiv #-}
ptr_glSecondaryColorP3uiv :: FunPtr (GLenum -> Ptr GLuint -> IO ())
ptr_glSecondaryColorP3uiv = unsafePerformIO $ getCommand "glSecondaryColorP3uiv"

-- glSecondaryColorPointer -----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glSecondaryColorPointer.xml OpenGL 2.x>.
glSecondaryColorPointer
  :: MonadIO m
  => GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [ColorPointerType](Graphics-GL-Groups.html#ColorPointerType).
  -> GLsizei -- ^ @stride@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(size,type,stride)@ elements of type @a@.
  -> m ()
glSecondaryColorPointer v1 v2 v3 v4 = liftIO $ dyn126 ptr_glSecondaryColorPointer v1 v2 v3 v4

{-# NOINLINE ptr_glSecondaryColorPointer #-}
ptr_glSecondaryColorPointer :: FunPtr (GLint -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glSecondaryColorPointer = unsafePerformIO $ getCommand "glSecondaryColorPointer"

-- glSecondaryColorPointerEXT --------------------------------------------------

-- | This command is an alias for 'glSecondaryColorPointer'.
glSecondaryColorPointerEXT
  :: MonadIO m
  => GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [ColorPointerType](Graphics-GL-Groups.html#ColorPointerType).
  -> GLsizei -- ^ @stride@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(size,type,stride)@ elements of type @a@.
  -> m ()
glSecondaryColorPointerEXT v1 v2 v3 v4 = liftIO $ dyn126 ptr_glSecondaryColorPointerEXT v1 v2 v3 v4

{-# NOINLINE ptr_glSecondaryColorPointerEXT #-}
ptr_glSecondaryColorPointerEXT :: FunPtr (GLint -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glSecondaryColorPointerEXT = unsafePerformIO $ getCommand "glSecondaryColorPointerEXT"

-- glSecondaryColorPointerListIBM ----------------------------------------------

glSecondaryColorPointerListIBM
  :: MonadIO m
  => GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type @SecondaryColorPointerTypeIBM@.
  -> GLint -- ^ @stride@.
  -> Ptr (Ptr a) -- ^ @pointer@ pointing to @COMPSIZE(size,type,stride)@ elements of type @Ptr a@.
  -> GLint -- ^ @ptrstride@.
  -> m ()
glSecondaryColorPointerListIBM v1 v2 v3 v4 v5 = liftIO $ dyn128 ptr_glSecondaryColorPointerListIBM v1 v2 v3 v4 v5

{-# NOINLINE ptr_glSecondaryColorPointerListIBM #-}
ptr_glSecondaryColorPointerListIBM :: FunPtr (GLint -> GLenum -> GLint -> Ptr (Ptr a) -> GLint -> IO ())
ptr_glSecondaryColorPointerListIBM = unsafePerformIO $ getCommand "glSecondaryColorPointerListIBM"

-- glSelectBuffer --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glSelectBuffer.xml OpenGL 2.x>.
glSelectBuffer
  :: MonadIO m
  => GLsizei -- ^ @size@.
  -> Ptr GLuint -- ^ @buffer@ pointing to @size@ elements of type @SelectName@.
  -> m ()
glSelectBuffer v1 v2 = liftIO $ dyn193 ptr_glSelectBuffer v1 v2

{-# NOINLINE ptr_glSelectBuffer #-}
ptr_glSelectBuffer :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glSelectBuffer = unsafePerformIO $ getCommand "glSelectBuffer"

-- glSelectPerfMonitorCountersAMD ----------------------------------------------

glSelectPerfMonitorCountersAMD
  :: MonadIO m
  => GLuint -- ^ @monitor@.
  -> GLboolean -- ^ @enable@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLuint -- ^ @group@.
  -> GLint -- ^ @numCounters@.
  -> Ptr GLuint -- ^ @counterList@ pointing to @numCounters@ elements of type @GLuint@.
  -> m ()
glSelectPerfMonitorCountersAMD v1 v2 v3 v4 v5 = liftIO $ dyn702 ptr_glSelectPerfMonitorCountersAMD v1 v2 v3 v4 v5

{-# NOINLINE ptr_glSelectPerfMonitorCountersAMD #-}
ptr_glSelectPerfMonitorCountersAMD :: FunPtr (GLuint -> GLboolean -> GLuint -> GLint -> Ptr GLuint -> IO ())
ptr_glSelectPerfMonitorCountersAMD = unsafePerformIO $ getCommand "glSelectPerfMonitorCountersAMD"

-- glSeparableFilter2D ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glSeparableFilter2D.xml OpenGL 2.x>.
glSeparableFilter2D
  :: MonadIO m
  => GLenum -- ^ @target@ of type @SeparableTarget@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @row@ pointing to @COMPSIZE(target,format,type,width)@ elements of type @a@.
  -> Ptr b -- ^ @column@ pointing to @COMPSIZE(target,format,type,height)@ elements of type @b@.
  -> m ()
glSeparableFilter2D v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn703 ptr_glSeparableFilter2D v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glSeparableFilter2D #-}
ptr_glSeparableFilter2D :: FunPtr (GLenum -> GLenum -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> Ptr b -> IO ())
ptr_glSeparableFilter2D = unsafePerformIO $ getCommand "glSeparableFilter2D"

-- glSeparableFilter2DEXT ------------------------------------------------------

-- | This command is an alias for 'glSeparableFilter2D'.
glSeparableFilter2DEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [SeparableTargetEXT](Graphics-GL-Groups.html#SeparableTargetEXT).
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @row@ pointing to @COMPSIZE(target,format,type,width)@ elements of type @a@.
  -> Ptr b -- ^ @column@ pointing to @COMPSIZE(target,format,type,height)@ elements of type @b@.
  -> m ()
glSeparableFilter2DEXT v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn703 ptr_glSeparableFilter2DEXT v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glSeparableFilter2DEXT #-}
ptr_glSeparableFilter2DEXT :: FunPtr (GLenum -> GLenum -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> Ptr b -> IO ())
ptr_glSeparableFilter2DEXT = unsafePerformIO $ getCommand "glSeparableFilter2DEXT"

-- glSetFenceAPPLE -------------------------------------------------------------

glSetFenceAPPLE
  :: MonadIO m
  => GLuint -- ^ @fence@ of type @FenceNV@.
  -> m ()
glSetFenceAPPLE v1 = liftIO $ dyn2 ptr_glSetFenceAPPLE v1

{-# NOINLINE ptr_glSetFenceAPPLE #-}
ptr_glSetFenceAPPLE :: FunPtr (GLuint -> IO ())
ptr_glSetFenceAPPLE = unsafePerformIO $ getCommand "glSetFenceAPPLE"

-- glSetFenceNV ----------------------------------------------------------------

glSetFenceNV
  :: MonadIO m
  => GLuint -- ^ @fence@ of type @FenceNV@.
  -> GLenum -- ^ @condition@ of type @FenceConditionNV@.
  -> m ()
glSetFenceNV v1 v2 = liftIO $ dyn15 ptr_glSetFenceNV v1 v2

{-# NOINLINE ptr_glSetFenceNV #-}
ptr_glSetFenceNV :: FunPtr (GLuint -> GLenum -> IO ())
ptr_glSetFenceNV = unsafePerformIO $ getCommand "glSetFenceNV"

-- glSetFragmentShaderConstantATI ----------------------------------------------

glSetFragmentShaderConstantATI
  :: MonadIO m
  => GLuint -- ^ @dst@.
  -> Ptr GLfloat -- ^ @value@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glSetFragmentShaderConstantATI v1 v2 = liftIO $ dyn377 ptr_glSetFragmentShaderConstantATI v1 v2

{-# NOINLINE ptr_glSetFragmentShaderConstantATI #-}
ptr_glSetFragmentShaderConstantATI :: FunPtr (GLuint -> Ptr GLfloat -> IO ())
ptr_glSetFragmentShaderConstantATI = unsafePerformIO $ getCommand "glSetFragmentShaderConstantATI"

-- glSetInvariantEXT -----------------------------------------------------------

glSetInvariantEXT
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @type@ of type @ScalarType@.
  -> Ptr a -- ^ @addr@ pointing to @COMPSIZE(id,type)@ elements of type @a@.
  -> m ()
glSetInvariantEXT v1 v2 v3 = liftIO $ dyn704 ptr_glSetInvariantEXT v1 v2 v3

{-# NOINLINE ptr_glSetInvariantEXT #-}
ptr_glSetInvariantEXT :: FunPtr (GLuint -> GLenum -> Ptr a -> IO ())
ptr_glSetInvariantEXT = unsafePerformIO $ getCommand "glSetInvariantEXT"

-- glSetLocalConstantEXT -------------------------------------------------------

glSetLocalConstantEXT
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @type@ of type @ScalarType@.
  -> Ptr a -- ^ @addr@ pointing to @COMPSIZE(id,type)@ elements of type @a@.
  -> m ()
glSetLocalConstantEXT v1 v2 v3 = liftIO $ dyn704 ptr_glSetLocalConstantEXT v1 v2 v3

{-# NOINLINE ptr_glSetLocalConstantEXT #-}
ptr_glSetLocalConstantEXT :: FunPtr (GLuint -> GLenum -> Ptr a -> IO ())
ptr_glSetLocalConstantEXT = unsafePerformIO $ getCommand "glSetLocalConstantEXT"

-- glSetMultisamplefvAMD -------------------------------------------------------

glSetMultisamplefvAMD
  :: MonadIO m
  => GLenum -- ^ @pname@.
  -> GLuint -- ^ @index@.
  -> Ptr GLfloat -- ^ @val@ pointing to @2@ elements of type @GLfloat@.
  -> m ()
glSetMultisamplefvAMD v1 v2 v3 = liftIO $ dyn267 ptr_glSetMultisamplefvAMD v1 v2 v3

{-# NOINLINE ptr_glSetMultisamplefvAMD #-}
ptr_glSetMultisamplefvAMD :: FunPtr (GLenum -> GLuint -> Ptr GLfloat -> IO ())
ptr_glSetMultisamplefvAMD = unsafePerformIO $ getCommand "glSetMultisamplefvAMD"

-- glShadeModel ----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glShadeModel.xml OpenGL 2.x>.
glShadeModel
  :: MonadIO m
  => GLenum -- ^ @mode@ of type [ShadingModel](Graphics-GL-Groups.html#ShadingModel).
  -> m ()
glShadeModel v1 = liftIO $ dyn4 ptr_glShadeModel v1

{-# NOINLINE ptr_glShadeModel #-}
ptr_glShadeModel :: FunPtr (GLenum -> IO ())
ptr_glShadeModel = unsafePerformIO $ getCommand "glShadeModel"

-- glShaderBinary --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glShaderBinary.xhtml OpenGL 4.x>.
glShaderBinary
  :: MonadIO m
  => GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @shaders@ pointing to @count@ elements of type @GLuint@.
  -> GLenum -- ^ @binaryformat@.
  -> Ptr a -- ^ @binary@ pointing to @length@ elements of type @a@.
  -> GLsizei -- ^ @length@.
  -> m ()
glShaderBinary v1 v2 v3 v4 v5 = liftIO $ dyn705 ptr_glShaderBinary v1 v2 v3 v4 v5

{-# NOINLINE ptr_glShaderBinary #-}
ptr_glShaderBinary :: FunPtr (GLsizei -> Ptr GLuint -> GLenum -> Ptr a -> GLsizei -> IO ())
ptr_glShaderBinary = unsafePerformIO $ getCommand "glShaderBinary"

-- glShaderOp1EXT --------------------------------------------------------------

glShaderOp1EXT
  :: MonadIO m
  => GLenum -- ^ @op@ of type @VertexShaderOpEXT@.
  -> GLuint -- ^ @res@.
  -> GLuint -- ^ @arg1@.
  -> m ()
glShaderOp1EXT v1 v2 v3 = liftIO $ dyn17 ptr_glShaderOp1EXT v1 v2 v3

{-# NOINLINE ptr_glShaderOp1EXT #-}
ptr_glShaderOp1EXT :: FunPtr (GLenum -> GLuint -> GLuint -> IO ())
ptr_glShaderOp1EXT = unsafePerformIO $ getCommand "glShaderOp1EXT"

-- glShaderOp2EXT --------------------------------------------------------------

glShaderOp2EXT
  :: MonadIO m
  => GLenum -- ^ @op@ of type @VertexShaderOpEXT@.
  -> GLuint -- ^ @res@.
  -> GLuint -- ^ @arg1@.
  -> GLuint -- ^ @arg2@.
  -> m ()
glShaderOp2EXT v1 v2 v3 v4 = liftIO $ dyn706 ptr_glShaderOp2EXT v1 v2 v3 v4

{-# NOINLINE ptr_glShaderOp2EXT #-}
ptr_glShaderOp2EXT :: FunPtr (GLenum -> GLuint -> GLuint -> GLuint -> IO ())
ptr_glShaderOp2EXT = unsafePerformIO $ getCommand "glShaderOp2EXT"

-- glShaderOp3EXT --------------------------------------------------------------

glShaderOp3EXT
  :: MonadIO m
  => GLenum -- ^ @op@ of type @VertexShaderOpEXT@.
  -> GLuint -- ^ @res@.
  -> GLuint -- ^ @arg1@.
  -> GLuint -- ^ @arg2@.
  -> GLuint -- ^ @arg3@.
  -> m ()
glShaderOp3EXT v1 v2 v3 v4 v5 = liftIO $ dyn707 ptr_glShaderOp3EXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glShaderOp3EXT #-}
ptr_glShaderOp3EXT :: FunPtr (GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> IO ())
ptr_glShaderOp3EXT = unsafePerformIO $ getCommand "glShaderOp3EXT"

-- glShaderSource --------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glShaderSource.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glShaderSource.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glShaderSource.xhtml OpenGL 4.x>.
glShaderSource
  :: MonadIO m
  => GLuint -- ^ @shader@.
  -> GLsizei -- ^ @count@.
  -> Ptr (Ptr GLchar) -- ^ @string@ pointing to @count@ elements of type @Ptr GLchar@.
  -> Ptr GLint -- ^ @length@ pointing to @count@ elements of type @GLint@.
  -> m ()
glShaderSource v1 v2 v3 v4 = liftIO $ dyn138 ptr_glShaderSource v1 v2 v3 v4

{-# NOINLINE ptr_glShaderSource #-}
ptr_glShaderSource :: FunPtr (GLuint -> GLsizei -> Ptr (Ptr GLchar) -> Ptr GLint -> IO ())
ptr_glShaderSource = unsafePerformIO $ getCommand "glShaderSource"

-- glShaderSourceARB -----------------------------------------------------------

-- | This command is an alias for 'glShaderSource'.
glShaderSourceARB
  :: MonadIO m
  => GLhandleARB -- ^ @shaderObj@ of type @handleARB@.
  -> GLsizei -- ^ @count@.
  -> Ptr (Ptr GLcharARB) -- ^ @string@ pointing to @count@ elements of type @Ptr GLcharARB@.
  -> Ptr GLint -- ^ @length@ pointing to @count@ elements of type @GLint@.
  -> m ()
glShaderSourceARB v1 v2 v3 v4 = liftIO $ dyn708 ptr_glShaderSourceARB v1 v2 v3 v4

{-# NOINLINE ptr_glShaderSourceARB #-}
ptr_glShaderSourceARB :: FunPtr (GLhandleARB -> GLsizei -> Ptr (Ptr GLcharARB) -> Ptr GLint -> IO ())
ptr_glShaderSourceARB = unsafePerformIO $ getCommand "glShaderSourceARB"

-- glShaderStorageBlockBinding -------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glShaderStorageBlockBinding.xhtml OpenGL 4.x>.
glShaderStorageBlockBinding
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLuint -- ^ @storageBlockIndex@.
  -> GLuint -- ^ @storageBlockBinding@.
  -> m ()
glShaderStorageBlockBinding v1 v2 v3 = liftIO $ dyn102 ptr_glShaderStorageBlockBinding v1 v2 v3

{-# NOINLINE ptr_glShaderStorageBlockBinding #-}
ptr_glShaderStorageBlockBinding :: FunPtr (GLuint -> GLuint -> GLuint -> IO ())
ptr_glShaderStorageBlockBinding = unsafePerformIO $ getCommand "glShaderStorageBlockBinding"

-- glSharpenTexFuncSGIS --------------------------------------------------------

glSharpenTexFuncSGIS
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLsizei -- ^ @n@.
  -> Ptr GLfloat -- ^ @points@ pointing to @n*2@ elements of type @GLfloat@.
  -> m ()
glSharpenTexFuncSGIS v1 v2 v3 = liftIO $ dyn225 ptr_glSharpenTexFuncSGIS v1 v2 v3

{-# NOINLINE ptr_glSharpenTexFuncSGIS #-}
ptr_glSharpenTexFuncSGIS :: FunPtr (GLenum -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glSharpenTexFuncSGIS = unsafePerformIO $ getCommand "glSharpenTexFuncSGIS"

-- glSpriteParameterfSGIX ------------------------------------------------------

glSpriteParameterfSGIX
  :: MonadIO m
  => GLenum -- ^ @pname@ of type @SpriteParameterNameSGIX@.
  -> GLfloat -- ^ @param@ of type @CheckedFloat32@.
  -> m ()
glSpriteParameterfSGIX v1 v2 = liftIO $ dyn0 ptr_glSpriteParameterfSGIX v1 v2

{-# NOINLINE ptr_glSpriteParameterfSGIX #-}
ptr_glSpriteParameterfSGIX :: FunPtr (GLenum -> GLfloat -> IO ())
ptr_glSpriteParameterfSGIX = unsafePerformIO $ getCommand "glSpriteParameterfSGIX"

-- glSpriteParameterfvSGIX -----------------------------------------------------

glSpriteParameterfvSGIX
  :: MonadIO m
  => GLenum -- ^ @pname@ of type @SpriteParameterNameSGIX@.
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedFloat32@.
  -> m ()
glSpriteParameterfvSGIX v1 v2 = liftIO $ dyn94 ptr_glSpriteParameterfvSGIX v1 v2

{-# NOINLINE ptr_glSpriteParameterfvSGIX #-}
ptr_glSpriteParameterfvSGIX :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glSpriteParameterfvSGIX = unsafePerformIO $ getCommand "glSpriteParameterfvSGIX"

-- glSpriteParameteriSGIX ------------------------------------------------------

glSpriteParameteriSGIX
  :: MonadIO m
  => GLenum -- ^ @pname@ of type @SpriteParameterNameSGIX@.
  -> GLint -- ^ @param@ of type @CheckedInt32@.
  -> m ()
glSpriteParameteriSGIX v1 v2 = liftIO $ dyn55 ptr_glSpriteParameteriSGIX v1 v2

{-# NOINLINE ptr_glSpriteParameteriSGIX #-}
ptr_glSpriteParameteriSGIX :: FunPtr (GLenum -> GLint -> IO ())
ptr_glSpriteParameteriSGIX = unsafePerformIO $ getCommand "glSpriteParameteriSGIX"

-- glSpriteParameterivSGIX -----------------------------------------------------

glSpriteParameterivSGIX
  :: MonadIO m
  => GLenum -- ^ @pname@ of type @SpriteParameterNameSGIX@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedInt32@.
  -> m ()
glSpriteParameterivSGIX v1 v2 = liftIO $ dyn136 ptr_glSpriteParameterivSGIX v1 v2

{-# NOINLINE ptr_glSpriteParameterivSGIX #-}
ptr_glSpriteParameterivSGIX :: FunPtr (GLenum -> Ptr GLint -> IO ())
ptr_glSpriteParameterivSGIX = unsafePerformIO $ getCommand "glSpriteParameterivSGIX"

-- glStartInstrumentsSGIX ------------------------------------------------------

glStartInstrumentsSGIX
  :: MonadIO m
  => m ()
glStartInstrumentsSGIX = liftIO $ dyn10 ptr_glStartInstrumentsSGIX

{-# NOINLINE ptr_glStartInstrumentsSGIX #-}
ptr_glStartInstrumentsSGIX :: FunPtr (IO ())
ptr_glStartInstrumentsSGIX = unsafePerformIO $ getCommand "glStartInstrumentsSGIX"

-- glStartTilingQCOM -----------------------------------------------------------

glStartTilingQCOM
  :: MonadIO m
  => GLuint -- ^ @x@.
  -> GLuint -- ^ @y@.
  -> GLuint -- ^ @width@.
  -> GLuint -- ^ @height@.
  -> GLbitfield -- ^ @preserveMask@.
  -> m ()
glStartTilingQCOM v1 v2 v3 v4 v5 = liftIO $ dyn709 ptr_glStartTilingQCOM v1 v2 v3 v4 v5

{-# NOINLINE ptr_glStartTilingQCOM #-}
ptr_glStartTilingQCOM :: FunPtr (GLuint -> GLuint -> GLuint -> GLuint -> GLbitfield -> IO ())
ptr_glStartTilingQCOM = unsafePerformIO $ getCommand "glStartTilingQCOM"

-- glStateCaptureNV ------------------------------------------------------------

glStateCaptureNV
  :: MonadIO m
  => GLuint -- ^ @state@.
  -> GLenum -- ^ @mode@.
  -> m ()
glStateCaptureNV v1 v2 = liftIO $ dyn15 ptr_glStateCaptureNV v1 v2

{-# NOINLINE ptr_glStateCaptureNV #-}
ptr_glStateCaptureNV :: FunPtr (GLuint -> GLenum -> IO ())
ptr_glStateCaptureNV = unsafePerformIO $ getCommand "glStateCaptureNV"

-- glStencilClearTagEXT --------------------------------------------------------

glStencilClearTagEXT
  :: MonadIO m
  => GLsizei -- ^ @stencilTagBits@.
  -> GLuint -- ^ @stencilClearTag@.
  -> m ()
glStencilClearTagEXT v1 v2 = liftIO $ dyn710 ptr_glStencilClearTagEXT v1 v2

{-# NOINLINE ptr_glStencilClearTagEXT #-}
ptr_glStencilClearTagEXT :: FunPtr (GLsizei -> GLuint -> IO ())
ptr_glStencilClearTagEXT = unsafePerformIO $ getCommand "glStencilClearTagEXT"

