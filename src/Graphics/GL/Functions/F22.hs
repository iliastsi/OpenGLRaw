--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F22
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

module Graphics.GL.Functions.F22 (
  glGetColorTable,
  glGetColorTableEXT,
  glGetColorTableParameterfv,
  glGetColorTableParameterfvEXT,
  glGetColorTableParameterfvSGI,
  glGetColorTableParameteriv,
  glGetColorTableParameterivEXT,
  glGetColorTableParameterivSGI,
  glGetColorTableSGI,
  glGetCombinerInputParameterfvNV,
  glGetCombinerInputParameterivNV,
  glGetCombinerOutputParameterfvNV,
  glGetCombinerOutputParameterivNV,
  glGetCombinerStageParameterfvNV,
  glGetCommandHeaderNV,
  glGetCompressedMultiTexImageEXT,
  glGetCompressedTexImage,
  glGetCompressedTexImageARB,
  glGetCompressedTextureImage,
  glGetCompressedTextureImageEXT,
  glGetCompressedTextureSubImage,
  glGetConvolutionFilter,
  glGetConvolutionFilterEXT,
  glGetConvolutionParameterfv,
  glGetConvolutionParameterfvEXT,
  glGetConvolutionParameteriv,
  glGetConvolutionParameterivEXT,
  glGetConvolutionParameterxvOES,
  glGetCoverageModulationTableNV,
  glGetDebugMessageLog,
  glGetDebugMessageLogAMD,
  glGetDebugMessageLogARB,
  glGetDebugMessageLogKHR,
  glGetDetailTexFuncSGIS,
  glGetDoubleIndexedvEXT,
  glGetDoublei_v,
  glGetDoublei_vEXT,
  glGetDoublev,
  glGetDriverControlStringQCOM,
  glGetDriverControlsQCOM
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

-- glGetColorTable -------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetColorTable.xml OpenGL 2.x>.
glGetColorTable
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ColorTableTarget@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @table@ pointing to @COMPSIZE(target,format,type)@ elements of type @a@.
  -> m ()
glGetColorTable v1 v2 v3 v4 = liftIO $ dyn317 ptr_glGetColorTable v1 v2 v3 v4

{-# NOINLINE ptr_glGetColorTable #-}
ptr_glGetColorTable :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glGetColorTable = unsafePerformIO $ getCommand "glGetColorTable"

-- glGetColorTableEXT ----------------------------------------------------------

-- | This command is an alias for 'glGetColorTable'.
glGetColorTableEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ColorTableTarget@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @data@ pointing to @COMPSIZE(target,format,type)@ elements of type @a@.
  -> m ()
glGetColorTableEXT v1 v2 v3 v4 = liftIO $ dyn317 ptr_glGetColorTableEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetColorTableEXT #-}
ptr_glGetColorTableEXT :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glGetColorTableEXT = unsafePerformIO $ getCommand "glGetColorTableEXT"

-- glGetColorTableParameterfv --------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetColorTableParameter.xml OpenGL 2.x>.
glGetColorTableParameterfv
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ColorTableTarget@.
  -> GLenum -- ^ @pname@ of type @GetColorTableParameterPName@.
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetColorTableParameterfv v1 v2 v3 = liftIO $ dyn132 ptr_glGetColorTableParameterfv v1 v2 v3

{-# NOINLINE ptr_glGetColorTableParameterfv #-}
ptr_glGetColorTableParameterfv :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetColorTableParameterfv = unsafePerformIO $ getCommand "glGetColorTableParameterfv"

-- glGetColorTableParameterfvEXT -----------------------------------------------

-- | This command is an alias for 'glGetColorTableParameterfv'.
glGetColorTableParameterfvEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ColorTableTarget@.
  -> GLenum -- ^ @pname@ of type @GetColorTableParameterPName@.
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetColorTableParameterfvEXT v1 v2 v3 = liftIO $ dyn132 ptr_glGetColorTableParameterfvEXT v1 v2 v3

{-# NOINLINE ptr_glGetColorTableParameterfvEXT #-}
ptr_glGetColorTableParameterfvEXT :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetColorTableParameterfvEXT = unsafePerformIO $ getCommand "glGetColorTableParameterfvEXT"

-- glGetColorTableParameterfvSGI -----------------------------------------------

glGetColorTableParameterfvSGI
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ColorTableTargetSGI](Graphics-GL-Groups.html#ColorTableTargetSGI).
  -> GLenum -- ^ @pname@ of type [GetColorTableParameterPNameSGI](Graphics-GL-Groups.html#GetColorTableParameterPNameSGI).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetColorTableParameterfvSGI v1 v2 v3 = liftIO $ dyn132 ptr_glGetColorTableParameterfvSGI v1 v2 v3

{-# NOINLINE ptr_glGetColorTableParameterfvSGI #-}
ptr_glGetColorTableParameterfvSGI :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetColorTableParameterfvSGI = unsafePerformIO $ getCommand "glGetColorTableParameterfvSGI"

-- glGetColorTableParameteriv --------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetColorTableParameter.xml OpenGL 2.x>.
glGetColorTableParameteriv
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ColorTableTarget@.
  -> GLenum -- ^ @pname@ of type @GetColorTableParameterPName@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetColorTableParameteriv v1 v2 v3 = liftIO $ dyn133 ptr_glGetColorTableParameteriv v1 v2 v3

{-# NOINLINE ptr_glGetColorTableParameteriv #-}
ptr_glGetColorTableParameteriv :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetColorTableParameteriv = unsafePerformIO $ getCommand "glGetColorTableParameteriv"

-- glGetColorTableParameterivEXT -----------------------------------------------

-- | This command is an alias for 'glGetColorTableParameteriv'.
glGetColorTableParameterivEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ColorTableTarget@.
  -> GLenum -- ^ @pname@ of type @GetColorTableParameterPName@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetColorTableParameterivEXT v1 v2 v3 = liftIO $ dyn133 ptr_glGetColorTableParameterivEXT v1 v2 v3

{-# NOINLINE ptr_glGetColorTableParameterivEXT #-}
ptr_glGetColorTableParameterivEXT :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetColorTableParameterivEXT = unsafePerformIO $ getCommand "glGetColorTableParameterivEXT"

-- glGetColorTableParameterivSGI -----------------------------------------------

glGetColorTableParameterivSGI
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ColorTableTargetSGI](Graphics-GL-Groups.html#ColorTableTargetSGI).
  -> GLenum -- ^ @pname@ of type [GetColorTableParameterPNameSGI](Graphics-GL-Groups.html#GetColorTableParameterPNameSGI).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetColorTableParameterivSGI v1 v2 v3 = liftIO $ dyn133 ptr_glGetColorTableParameterivSGI v1 v2 v3

{-# NOINLINE ptr_glGetColorTableParameterivSGI #-}
ptr_glGetColorTableParameterivSGI :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetColorTableParameterivSGI = unsafePerformIO $ getCommand "glGetColorTableParameterivSGI"

-- glGetColorTableSGI ----------------------------------------------------------

glGetColorTableSGI
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ColorTableTargetSGI](Graphics-GL-Groups.html#ColorTableTargetSGI).
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @table@ pointing to @COMPSIZE(target,format,type)@ elements of type @a@.
  -> m ()
glGetColorTableSGI v1 v2 v3 v4 = liftIO $ dyn317 ptr_glGetColorTableSGI v1 v2 v3 v4

{-# NOINLINE ptr_glGetColorTableSGI #-}
ptr_glGetColorTableSGI :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glGetColorTableSGI = unsafePerformIO $ getCommand "glGetColorTableSGI"

-- glGetCombinerInputParameterfvNV ---------------------------------------------

glGetCombinerInputParameterfvNV
  :: MonadIO m
  => GLenum -- ^ @stage@ of type @CombinerStageNV@.
  -> GLenum -- ^ @portion@ of type @CombinerPortionNV@.
  -> GLenum -- ^ @variable@ of type @CombinerVariableNV@.
  -> GLenum -- ^ @pname@ of type @CombinerParameterNV@.
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetCombinerInputParameterfvNV v1 v2 v3 v4 v5 = liftIO $ dyn318 ptr_glGetCombinerInputParameterfvNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetCombinerInputParameterfvNV #-}
ptr_glGetCombinerInputParameterfvNV :: FunPtr (GLenum -> GLenum -> GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetCombinerInputParameterfvNV = unsafePerformIO $ getCommand "glGetCombinerInputParameterfvNV"

-- glGetCombinerInputParameterivNV ---------------------------------------------

glGetCombinerInputParameterivNV
  :: MonadIO m
  => GLenum -- ^ @stage@ of type @CombinerStageNV@.
  -> GLenum -- ^ @portion@ of type @CombinerPortionNV@.
  -> GLenum -- ^ @variable@ of type @CombinerVariableNV@.
  -> GLenum -- ^ @pname@ of type @CombinerParameterNV@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetCombinerInputParameterivNV v1 v2 v3 v4 v5 = liftIO $ dyn319 ptr_glGetCombinerInputParameterivNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetCombinerInputParameterivNV #-}
ptr_glGetCombinerInputParameterivNV :: FunPtr (GLenum -> GLenum -> GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetCombinerInputParameterivNV = unsafePerformIO $ getCommand "glGetCombinerInputParameterivNV"

-- glGetCombinerOutputParameterfvNV --------------------------------------------

glGetCombinerOutputParameterfvNV
  :: MonadIO m
  => GLenum -- ^ @stage@ of type @CombinerStageNV@.
  -> GLenum -- ^ @portion@ of type @CombinerPortionNV@.
  -> GLenum -- ^ @pname@ of type @CombinerParameterNV@.
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetCombinerOutputParameterfvNV v1 v2 v3 v4 = liftIO $ dyn320 ptr_glGetCombinerOutputParameterfvNV v1 v2 v3 v4

{-# NOINLINE ptr_glGetCombinerOutputParameterfvNV #-}
ptr_glGetCombinerOutputParameterfvNV :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetCombinerOutputParameterfvNV = unsafePerformIO $ getCommand "glGetCombinerOutputParameterfvNV"

-- glGetCombinerOutputParameterivNV --------------------------------------------

glGetCombinerOutputParameterivNV
  :: MonadIO m
  => GLenum -- ^ @stage@ of type @CombinerStageNV@.
  -> GLenum -- ^ @portion@ of type @CombinerPortionNV@.
  -> GLenum -- ^ @pname@ of type @CombinerParameterNV@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetCombinerOutputParameterivNV v1 v2 v3 v4 = liftIO $ dyn321 ptr_glGetCombinerOutputParameterivNV v1 v2 v3 v4

{-# NOINLINE ptr_glGetCombinerOutputParameterivNV #-}
ptr_glGetCombinerOutputParameterivNV :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetCombinerOutputParameterivNV = unsafePerformIO $ getCommand "glGetCombinerOutputParameterivNV"

-- glGetCombinerStageParameterfvNV ---------------------------------------------

glGetCombinerStageParameterfvNV
  :: MonadIO m
  => GLenum -- ^ @stage@ of type @CombinerStageNV@.
  -> GLenum -- ^ @pname@ of type @CombinerParameterNV@.
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetCombinerStageParameterfvNV v1 v2 v3 = liftIO $ dyn132 ptr_glGetCombinerStageParameterfvNV v1 v2 v3

{-# NOINLINE ptr_glGetCombinerStageParameterfvNV #-}
ptr_glGetCombinerStageParameterfvNV :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetCombinerStageParameterfvNV = unsafePerformIO $ getCommand "glGetCombinerStageParameterfvNV"

-- glGetCommandHeaderNV --------------------------------------------------------

glGetCommandHeaderNV
  :: MonadIO m
  => GLenum -- ^ @tokenID@.
  -> GLuint -- ^ @size@.
  -> m GLuint
glGetCommandHeaderNV v1 v2 = liftIO $ dyn322 ptr_glGetCommandHeaderNV v1 v2

{-# NOINLINE ptr_glGetCommandHeaderNV #-}
ptr_glGetCommandHeaderNV :: FunPtr (GLenum -> GLuint -> IO GLuint)
ptr_glGetCommandHeaderNV = unsafePerformIO $ getCommand "glGetCommandHeaderNV"

-- glGetCompressedMultiTexImageEXT ---------------------------------------------

glGetCompressedMultiTexImageEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type @TextureUnit@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @lod@ of type @CheckedInt32@.
  -> Ptr a -- ^ @img@ pointing to @COMPSIZE(target,lod)@ elements of type @a@.
  -> m ()
glGetCompressedMultiTexImageEXT v1 v2 v3 v4 = liftIO $ dyn323 ptr_glGetCompressedMultiTexImageEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetCompressedMultiTexImageEXT #-}
ptr_glGetCompressedMultiTexImageEXT :: FunPtr (GLenum -> GLenum -> GLint -> Ptr a -> IO ())
ptr_glGetCompressedMultiTexImageEXT = unsafePerformIO $ getCommand "glGetCompressedMultiTexImageEXT"

-- glGetCompressedTexImage -----------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetCompressedTexImage.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGetCompressedTexImage.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGetCompressedTexImage.xhtml OpenGL 4.x>.
glGetCompressedTexImage
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> Ptr a -- ^ @img@ pointing to @COMPSIZE(target,level)@ elements of type @CompressedTextureARB@.
  -> m ()
glGetCompressedTexImage v1 v2 v3 = liftIO $ dyn324 ptr_glGetCompressedTexImage v1 v2 v3

{-# NOINLINE ptr_glGetCompressedTexImage #-}
ptr_glGetCompressedTexImage :: FunPtr (GLenum -> GLint -> Ptr a -> IO ())
ptr_glGetCompressedTexImage = unsafePerformIO $ getCommand "glGetCompressedTexImage"

-- glGetCompressedTexImageARB --------------------------------------------------

-- | This command is an alias for 'glGetCompressedTexImage'.
glGetCompressedTexImageARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> Ptr a -- ^ @img@ pointing to @COMPSIZE(target,level)@ elements of type @CompressedTextureARB@.
  -> m ()
glGetCompressedTexImageARB v1 v2 v3 = liftIO $ dyn324 ptr_glGetCompressedTexImageARB v1 v2 v3

{-# NOINLINE ptr_glGetCompressedTexImageARB #-}
ptr_glGetCompressedTexImageARB :: FunPtr (GLenum -> GLint -> Ptr a -> IO ())
ptr_glGetCompressedTexImageARB = unsafePerformIO $ getCommand "glGetCompressedTexImageARB"

-- glGetCompressedTextureImage -------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetCompressedTexImage.xhtml OpenGL 4.x>.
glGetCompressedTextureImage
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr a -- ^ @pixels@.
  -> m ()
glGetCompressedTextureImage v1 v2 v3 v4 = liftIO $ dyn325 ptr_glGetCompressedTextureImage v1 v2 v3 v4

{-# NOINLINE ptr_glGetCompressedTextureImage #-}
ptr_glGetCompressedTextureImage :: FunPtr (GLuint -> GLint -> GLsizei -> Ptr a -> IO ())
ptr_glGetCompressedTextureImage = unsafePerformIO $ getCommand "glGetCompressedTextureImage"

-- glGetCompressedTextureImageEXT ----------------------------------------------

glGetCompressedTextureImageEXT
  :: MonadIO m
  => GLuint -- ^ @texture@ of type @Texture@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @lod@ of type @CheckedInt32@.
  -> Ptr a -- ^ @img@ pointing to @COMPSIZE(target,lod)@ elements of type @a@.
  -> m ()
glGetCompressedTextureImageEXT v1 v2 v3 v4 = liftIO $ dyn326 ptr_glGetCompressedTextureImageEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetCompressedTextureImageEXT #-}
ptr_glGetCompressedTextureImageEXT :: FunPtr (GLuint -> GLenum -> GLint -> Ptr a -> IO ())
ptr_glGetCompressedTextureImageEXT = unsafePerformIO $ getCommand "glGetCompressedTextureImageEXT"

-- glGetCompressedTextureSubImage ----------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetCompressedTextureSubImage.xhtml OpenGL 4.x>.
glGetCompressedTextureSubImage
  :: MonadIO m
  => GLuint -- ^ @texture@.
  -> GLint -- ^ @level@.
  -> GLint -- ^ @xoffset@.
  -> GLint -- ^ @yoffset@.
  -> GLint -- ^ @zoffset@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr a -- ^ @pixels@.
  -> m ()
glGetCompressedTextureSubImage v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO $ dyn327 ptr_glGetCompressedTextureSubImage v1 v2 v3 v4 v5 v6 v7 v8 v9 v10

{-# NOINLINE ptr_glGetCompressedTextureSubImage #-}
ptr_glGetCompressedTextureSubImage :: FunPtr (GLuint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLsizei -> Ptr a -> IO ())
ptr_glGetCompressedTextureSubImage = unsafePerformIO $ getCommand "glGetCompressedTextureSubImage"

-- glGetConvolutionFilter ------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetConvolutionFilter.xml OpenGL 2.x>.
glGetConvolutionFilter
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ConvolutionTarget@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @image@ pointing to @COMPSIZE(target,format,type)@ elements of type @a@.
  -> m ()
glGetConvolutionFilter v1 v2 v3 v4 = liftIO $ dyn317 ptr_glGetConvolutionFilter v1 v2 v3 v4

{-# NOINLINE ptr_glGetConvolutionFilter #-}
ptr_glGetConvolutionFilter :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glGetConvolutionFilter = unsafePerformIO $ getCommand "glGetConvolutionFilter"

-- glGetConvolutionFilterEXT ---------------------------------------------------

glGetConvolutionFilterEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ConvolutionTargetEXT](Graphics-GL-Groups.html#ConvolutionTargetEXT).
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @image@ pointing to @COMPSIZE(target,format,type)@ elements of type @a@.
  -> m ()
glGetConvolutionFilterEXT v1 v2 v3 v4 = liftIO $ dyn317 ptr_glGetConvolutionFilterEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetConvolutionFilterEXT #-}
ptr_glGetConvolutionFilterEXT :: FunPtr (GLenum -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glGetConvolutionFilterEXT = unsafePerformIO $ getCommand "glGetConvolutionFilterEXT"

-- glGetConvolutionParameterfv -------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetConvolutionParameter.xml OpenGL 2.x>.
glGetConvolutionParameterfv
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ConvolutionTarget@.
  -> GLenum -- ^ @pname@ of type @GetConvolutionParameterPName@.
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetConvolutionParameterfv v1 v2 v3 = liftIO $ dyn132 ptr_glGetConvolutionParameterfv v1 v2 v3

{-# NOINLINE ptr_glGetConvolutionParameterfv #-}
ptr_glGetConvolutionParameterfv :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetConvolutionParameterfv = unsafePerformIO $ getCommand "glGetConvolutionParameterfv"

-- glGetConvolutionParameterfvEXT ----------------------------------------------

glGetConvolutionParameterfvEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ConvolutionTargetEXT](Graphics-GL-Groups.html#ConvolutionTargetEXT).
  -> GLenum -- ^ @pname@ of type [ConvolutionParameterEXT](Graphics-GL-Groups.html#ConvolutionParameterEXT).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetConvolutionParameterfvEXT v1 v2 v3 = liftIO $ dyn132 ptr_glGetConvolutionParameterfvEXT v1 v2 v3

{-# NOINLINE ptr_glGetConvolutionParameterfvEXT #-}
ptr_glGetConvolutionParameterfvEXT :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetConvolutionParameterfvEXT = unsafePerformIO $ getCommand "glGetConvolutionParameterfvEXT"

-- glGetConvolutionParameteriv -------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glGetConvolutionParameter.xml OpenGL 2.x>.
glGetConvolutionParameteriv
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ConvolutionTarget@.
  -> GLenum -- ^ @pname@ of type @GetConvolutionParameterPName@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetConvolutionParameteriv v1 v2 v3 = liftIO $ dyn133 ptr_glGetConvolutionParameteriv v1 v2 v3

{-# NOINLINE ptr_glGetConvolutionParameteriv #-}
ptr_glGetConvolutionParameteriv :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetConvolutionParameteriv = unsafePerformIO $ getCommand "glGetConvolutionParameteriv"

-- glGetConvolutionParameterivEXT ----------------------------------------------

glGetConvolutionParameterivEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ConvolutionTargetEXT](Graphics-GL-Groups.html#ConvolutionTargetEXT).
  -> GLenum -- ^ @pname@ of type [ConvolutionParameterEXT](Graphics-GL-Groups.html#ConvolutionParameterEXT).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetConvolutionParameterivEXT v1 v2 v3 = liftIO $ dyn133 ptr_glGetConvolutionParameterivEXT v1 v2 v3

{-# NOINLINE ptr_glGetConvolutionParameterivEXT #-}
ptr_glGetConvolutionParameterivEXT :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetConvolutionParameterivEXT = unsafePerformIO $ getCommand "glGetConvolutionParameterivEXT"

-- glGetConvolutionParameterxvOES ----------------------------------------------

glGetConvolutionParameterxvOES
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLfixed -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfixed@.
  -> m ()
glGetConvolutionParameterxvOES v1 v2 v3 = liftIO $ dyn163 ptr_glGetConvolutionParameterxvOES v1 v2 v3

{-# NOINLINE ptr_glGetConvolutionParameterxvOES #-}
ptr_glGetConvolutionParameterxvOES :: FunPtr (GLenum -> GLenum -> Ptr GLfixed -> IO ())
ptr_glGetConvolutionParameterxvOES = unsafePerformIO $ getCommand "glGetConvolutionParameterxvOES"

-- glGetCoverageModulationTableNV ----------------------------------------------

glGetCoverageModulationTableNV
  :: MonadIO m
  => GLsizei -- ^ @bufsize@.
  -> Ptr GLfloat -- ^ @v@.
  -> m ()
glGetCoverageModulationTableNV v1 v2 = liftIO $ dyn192 ptr_glGetCoverageModulationTableNV v1 v2

{-# NOINLINE ptr_glGetCoverageModulationTableNV #-}
ptr_glGetCoverageModulationTableNV :: FunPtr (GLsizei -> Ptr GLfloat -> IO ())
ptr_glGetCoverageModulationTableNV = unsafePerformIO $ getCommand "glGetCoverageModulationTableNV"

-- glGetDebugMessageLog --------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetDebugMessageLog.xhtml OpenGL 4.x>.
glGetDebugMessageLog
  :: MonadIO m
  => GLuint -- ^ @count@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLenum -- ^ @sources@ pointing to @count@ elements of type @GLenum@.
  -> Ptr GLenum -- ^ @types@ pointing to @count@ elements of type @GLenum@.
  -> Ptr GLuint -- ^ @ids@ pointing to @count@ elements of type @GLuint@.
  -> Ptr GLenum -- ^ @severities@ pointing to @count@ elements of type @GLenum@.
  -> Ptr GLsizei -- ^ @lengths@ pointing to @count@ elements of type @GLsizei@.
  -> Ptr GLchar -- ^ @messageLog@ pointing to @bufSize@ elements of type @GLchar@.
  -> m GLuint
glGetDebugMessageLog v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn328 ptr_glGetDebugMessageLog v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glGetDebugMessageLog #-}
ptr_glGetDebugMessageLog :: FunPtr (GLuint -> GLsizei -> Ptr GLenum -> Ptr GLenum -> Ptr GLuint -> Ptr GLenum -> Ptr GLsizei -> Ptr GLchar -> IO GLuint)
ptr_glGetDebugMessageLog = unsafePerformIO $ getCommand "glGetDebugMessageLog"

-- glGetDebugMessageLogAMD -----------------------------------------------------

glGetDebugMessageLogAMD
  :: MonadIO m
  => GLuint -- ^ @count@.
  -> GLsizei -- ^ @bufsize@.
  -> Ptr GLenum -- ^ @categories@ pointing to @count@ elements of type @GLenum@.
  -> Ptr GLuint -- ^ @severities@ pointing to @count@ elements of type @GLuint@.
  -> Ptr GLuint -- ^ @ids@ pointing to @count@ elements of type @GLuint@.
  -> Ptr GLsizei -- ^ @lengths@ pointing to @count@ elements of type @GLsizei@.
  -> Ptr GLchar -- ^ @message@ pointing to @bufsize@ elements of type @GLchar@.
  -> m GLuint
glGetDebugMessageLogAMD v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn329 ptr_glGetDebugMessageLogAMD v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glGetDebugMessageLogAMD #-}
ptr_glGetDebugMessageLogAMD :: FunPtr (GLuint -> GLsizei -> Ptr GLenum -> Ptr GLuint -> Ptr GLuint -> Ptr GLsizei -> Ptr GLchar -> IO GLuint)
ptr_glGetDebugMessageLogAMD = unsafePerformIO $ getCommand "glGetDebugMessageLogAMD"

-- glGetDebugMessageLogARB -----------------------------------------------------

-- | This command is an alias for 'glGetDebugMessageLog'.
glGetDebugMessageLogARB
  :: MonadIO m
  => GLuint -- ^ @count@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLenum -- ^ @sources@ pointing to @count@ elements of type @GLenum@.
  -> Ptr GLenum -- ^ @types@ pointing to @count@ elements of type @GLenum@.
  -> Ptr GLuint -- ^ @ids@ pointing to @count@ elements of type @GLuint@.
  -> Ptr GLenum -- ^ @severities@ pointing to @count@ elements of type @GLenum@.
  -> Ptr GLsizei -- ^ @lengths@ pointing to @count@ elements of type @GLsizei@.
  -> Ptr GLchar -- ^ @messageLog@ pointing to @bufSize@ elements of type @GLchar@.
  -> m GLuint
glGetDebugMessageLogARB v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn328 ptr_glGetDebugMessageLogARB v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glGetDebugMessageLogARB #-}
ptr_glGetDebugMessageLogARB :: FunPtr (GLuint -> GLsizei -> Ptr GLenum -> Ptr GLenum -> Ptr GLuint -> Ptr GLenum -> Ptr GLsizei -> Ptr GLchar -> IO GLuint)
ptr_glGetDebugMessageLogARB = unsafePerformIO $ getCommand "glGetDebugMessageLogARB"

-- glGetDebugMessageLogKHR -----------------------------------------------------

-- | This command is an alias for 'glGetDebugMessageLog'.
glGetDebugMessageLogKHR
  :: MonadIO m
  => GLuint -- ^ @count@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLenum -- ^ @sources@ pointing to @count@ elements of type @GLenum@.
  -> Ptr GLenum -- ^ @types@ pointing to @count@ elements of type @GLenum@.
  -> Ptr GLuint -- ^ @ids@ pointing to @count@ elements of type @GLuint@.
  -> Ptr GLenum -- ^ @severities@ pointing to @count@ elements of type @GLenum@.
  -> Ptr GLsizei -- ^ @lengths@ pointing to @count@ elements of type @GLsizei@.
  -> Ptr GLchar -- ^ @messageLog@ pointing to @bufSize@ elements of type @GLchar@.
  -> m GLuint
glGetDebugMessageLogKHR v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn328 ptr_glGetDebugMessageLogKHR v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glGetDebugMessageLogKHR #-}
ptr_glGetDebugMessageLogKHR :: FunPtr (GLuint -> GLsizei -> Ptr GLenum -> Ptr GLenum -> Ptr GLuint -> Ptr GLenum -> Ptr GLsizei -> Ptr GLchar -> IO GLuint)
ptr_glGetDebugMessageLogKHR = unsafePerformIO $ getCommand "glGetDebugMessageLogKHR"

-- glGetDetailTexFuncSGIS ------------------------------------------------------

glGetDetailTexFuncSGIS
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> Ptr GLfloat -- ^ @points@ pointing to @COMPSIZE(target)@ elements of type @GLfloat@.
  -> m ()
glGetDetailTexFuncSGIS v1 v2 = liftIO $ dyn94 ptr_glGetDetailTexFuncSGIS v1 v2

{-# NOINLINE ptr_glGetDetailTexFuncSGIS #-}
ptr_glGetDetailTexFuncSGIS :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glGetDetailTexFuncSGIS = unsafePerformIO $ getCommand "glGetDetailTexFuncSGIS"

-- glGetDoubleIndexedvEXT ------------------------------------------------------

-- | This command is an alias for 'glGetDoublei_v'.
glGetDoubleIndexedvEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type @TypeEnum@.
  -> GLuint -- ^ @index@.
  -> Ptr GLdouble -- ^ @data@ pointing to @COMPSIZE(target)@ elements of type @GLdouble@.
  -> m ()
glGetDoubleIndexedvEXT v1 v2 v3 = liftIO $ dyn330 ptr_glGetDoubleIndexedvEXT v1 v2 v3

{-# NOINLINE ptr_glGetDoubleIndexedvEXT #-}
ptr_glGetDoubleIndexedvEXT :: FunPtr (GLenum -> GLuint -> Ptr GLdouble -> IO ())
ptr_glGetDoubleIndexedvEXT = unsafePerformIO $ getCommand "glGetDoubleIndexedvEXT"

-- glGetDoublei_v --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGet.xhtml OpenGL 4.x>.
glGetDoublei_v
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLuint -- ^ @index@.
  -> Ptr GLdouble -- ^ @data@ pointing to @COMPSIZE(target)@ elements of type @GLdouble@.
  -> m ()
glGetDoublei_v v1 v2 v3 = liftIO $ dyn330 ptr_glGetDoublei_v v1 v2 v3

{-# NOINLINE ptr_glGetDoublei_v #-}
ptr_glGetDoublei_v :: FunPtr (GLenum -> GLuint -> Ptr GLdouble -> IO ())
ptr_glGetDoublei_v = unsafePerformIO $ getCommand "glGetDoublei_v"

-- glGetDoublei_vEXT -----------------------------------------------------------

-- | This command is an alias for 'glGetDoublei_v'.
glGetDoublei_vEXT
  :: MonadIO m
  => GLenum -- ^ @pname@ of type @TypeEnum@.
  -> GLuint -- ^ @index@.
  -> Ptr GLdouble -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLdouble@.
  -> m ()
glGetDoublei_vEXT v1 v2 v3 = liftIO $ dyn330 ptr_glGetDoublei_vEXT v1 v2 v3

{-# NOINLINE ptr_glGetDoublei_vEXT #-}
ptr_glGetDoublei_vEXT :: FunPtr (GLenum -> GLuint -> Ptr GLdouble -> IO ())
ptr_glGetDoublei_vEXT = unsafePerformIO $ getCommand "glGetDoublei_vEXT"

-- glGetDoublev ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glGet.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glGet.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glGet.xhtml OpenGL 4.x>.
glGetDoublev
  :: MonadIO m
  => GLenum -- ^ @pname@ of type [GetPName](Graphics-GL-Groups.html#GetPName).
  -> Ptr GLdouble -- ^ @data@ pointing to @COMPSIZE(pname)@ elements of type @GLdouble@.
  -> m ()
glGetDoublev v1 v2 = liftIO $ dyn93 ptr_glGetDoublev v1 v2

{-# NOINLINE ptr_glGetDoublev #-}
ptr_glGetDoublev :: FunPtr (GLenum -> Ptr GLdouble -> IO ())
ptr_glGetDoublev = unsafePerformIO $ getCommand "glGetDoublev"

-- glGetDriverControlStringQCOM ------------------------------------------------

glGetDriverControlStringQCOM
  :: MonadIO m
  => GLuint -- ^ @driverControl@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLsizei -- ^ @length@.
  -> Ptr GLchar -- ^ @driverControlString@ pointing to @bufSize@ elements of type @GLchar@.
  -> m ()
glGetDriverControlStringQCOM v1 v2 v3 v4 = liftIO $ dyn331 ptr_glGetDriverControlStringQCOM v1 v2 v3 v4

{-# NOINLINE ptr_glGetDriverControlStringQCOM #-}
ptr_glGetDriverControlStringQCOM :: FunPtr (GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
ptr_glGetDriverControlStringQCOM = unsafePerformIO $ getCommand "glGetDriverControlStringQCOM"

-- glGetDriverControlsQCOM -----------------------------------------------------

glGetDriverControlsQCOM
  :: MonadIO m
  => Ptr GLint -- ^ @num@.
  -> GLsizei -- ^ @size@.
  -> Ptr GLuint -- ^ @driverControls@ pointing to @size@ elements of type @GLuint@.
  -> m ()
glGetDriverControlsQCOM v1 v2 v3 = liftIO $ dyn332 ptr_glGetDriverControlsQCOM v1 v2 v3

{-# NOINLINE ptr_glGetDriverControlsQCOM #-}
ptr_glGetDriverControlsQCOM :: FunPtr (Ptr GLint -> GLsizei -> Ptr GLuint -> IO ())
ptr_glGetDriverControlsQCOM = unsafePerformIO $ getCommand "glGetDriverControlsQCOM"

