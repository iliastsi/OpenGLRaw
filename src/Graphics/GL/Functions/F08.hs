--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F08
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

module Graphics.GL.Functions.F08 (
  glColorP3uiv,
  glColorP4ui,
  glColorP4uiv,
  glColorPointer,
  glColorPointerEXT,
  glColorPointerListIBM,
  glColorPointervINTEL,
  glColorSubTable,
  glColorSubTableEXT,
  glColorTable,
  glColorTableEXT,
  glColorTableParameterfv,
  glColorTableParameterfvSGI,
  glColorTableParameteriv,
  glColorTableParameterivSGI,
  glColorTableSGI,
  glCombinerInputNV,
  glCombinerOutputNV,
  glCombinerParameterfNV,
  glCombinerParameterfvNV,
  glCombinerParameteriNV,
  glCombinerParameterivNV,
  glCombinerStageParameterfvNV,
  glCommandListSegmentsNV,
  glCompileCommandListNV,
  glCompileShader,
  glCompileShaderARB,
  glCompileShaderIncludeARB,
  glCompressedMultiTexImage1DEXT,
  glCompressedMultiTexImage2DEXT,
  glCompressedMultiTexImage3DEXT,
  glCompressedMultiTexSubImage1DEXT,
  glCompressedMultiTexSubImage2DEXT,
  glCompressedMultiTexSubImage3DEXT,
  glCompressedTexImage1D,
  glCompressedTexImage1DARB,
  glCompressedTexImage2D,
  glCompressedTexImage2DARB,
  glCompressedTexImage3D,
  glCompressedTexImage3DARB
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

-- glColorP3uiv ----------------------------------------------------------------

glColorP3uiv
  :: MonadIO m
  => GLenum -- ^ @type@.
  -> Ptr GLuint -- ^ @color@ pointing to @1@ element of type @GLuint@.
  -> m ()
glColorP3uiv v1 v2 = liftIO $ dyn125 ptr_glColorP3uiv v1 v2

{-# NOINLINE ptr_glColorP3uiv #-}
ptr_glColorP3uiv :: FunPtr (GLenum -> Ptr GLuint -> IO ())
ptr_glColorP3uiv = unsafePerformIO $ getCommand "glColorP3uiv"

-- glColorP4ui -----------------------------------------------------------------

glColorP4ui
  :: MonadIO m
  => GLenum -- ^ @type@.
  -> GLuint -- ^ @color@.
  -> m ()
glColorP4ui v1 v2 = liftIO $ dyn16 ptr_glColorP4ui v1 v2

{-# NOINLINE ptr_glColorP4ui #-}
ptr_glColorP4ui :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glColorP4ui = unsafePerformIO $ getCommand "glColorP4ui"

-- glColorP4uiv ----------------------------------------------------------------

glColorP4uiv
  :: MonadIO m
  => GLenum -- ^ @type@.
  -> Ptr GLuint -- ^ @color@ pointing to @1@ element of type @GLuint@.
  -> m ()
glColorP4uiv v1 v2 = liftIO $ dyn125 ptr_glColorP4uiv v1 v2

{-# NOINLINE ptr_glColorP4uiv #-}
ptr_glColorP4uiv :: FunPtr (GLenum -> Ptr GLuint -> IO ())
ptr_glColorP4uiv = unsafePerformIO $ getCommand "glColorP4uiv"

-- glColorPointer --------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColorPointer.xml OpenGL 2.x>.
glColorPointer
  :: MonadIO m
  => GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [ColorPointerType](Graphics-GL-Groups.html#ColorPointerType).
  -> GLsizei -- ^ @stride@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(size,type,stride)@ elements of type @a@.
  -> m ()
glColorPointer v1 v2 v3 v4 = liftIO $ dyn126 ptr_glColorPointer v1 v2 v3 v4

{-# NOINLINE ptr_glColorPointer #-}
ptr_glColorPointer :: FunPtr (GLint -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glColorPointer = unsafePerformIO $ getCommand "glColorPointer"

-- glColorPointerEXT -----------------------------------------------------------

glColorPointerEXT
  :: MonadIO m
  => GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [ColorPointerType](Graphics-GL-Groups.html#ColorPointerType).
  -> GLsizei -- ^ @stride@.
  -> GLsizei -- ^ @count@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(size,type,stride,count)@ elements of type @a@.
  -> m ()
glColorPointerEXT v1 v2 v3 v4 v5 = liftIO $ dyn127 ptr_glColorPointerEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glColorPointerEXT #-}
ptr_glColorPointerEXT :: FunPtr (GLint -> GLenum -> GLsizei -> GLsizei -> Ptr a -> IO ())
ptr_glColorPointerEXT = unsafePerformIO $ getCommand "glColorPointerEXT"

-- glColorPointerListIBM -------------------------------------------------------

glColorPointerListIBM
  :: MonadIO m
  => GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [ColorPointerType](Graphics-GL-Groups.html#ColorPointerType).
  -> GLint -- ^ @stride@.
  -> Ptr (Ptr a) -- ^ @pointer@ pointing to @COMPSIZE(size,type,stride)@ elements of type @Ptr a@.
  -> GLint -- ^ @ptrstride@.
  -> m ()
glColorPointerListIBM v1 v2 v3 v4 v5 = liftIO $ dyn128 ptr_glColorPointerListIBM v1 v2 v3 v4 v5

{-# NOINLINE ptr_glColorPointerListIBM #-}
ptr_glColorPointerListIBM :: FunPtr (GLint -> GLenum -> GLint -> Ptr (Ptr a) -> GLint -> IO ())
ptr_glColorPointerListIBM = unsafePerformIO $ getCommand "glColorPointerListIBM"

-- glColorPointervINTEL --------------------------------------------------------

glColorPointervINTEL
  :: MonadIO m
  => GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [VertexPointerType](Graphics-GL-Groups.html#VertexPointerType).
  -> Ptr (Ptr a) -- ^ @pointer@ pointing to @4@ elements of type @Ptr a@.
  -> m ()
glColorPointervINTEL v1 v2 v3 = liftIO $ dyn129 ptr_glColorPointervINTEL v1 v2 v3

{-# NOINLINE ptr_glColorPointervINTEL #-}
ptr_glColorPointervINTEL :: FunPtr (GLint -> GLenum -> Ptr (Ptr a) -> IO ())
ptr_glColorPointervINTEL = unsafePerformIO $ getCommand "glColorPointervINTEL"

-- glColorSubTable -------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColorSubTable.xml OpenGL 2.x>.
glColorSubTable
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ColorTableTarget@.
  -> GLsizei -- ^ @start@.
  -> GLsizei -- ^ @count@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @data@ pointing to @COMPSIZE(format,type,count)@ elements of type @a@.
  -> m ()
glColorSubTable v1 v2 v3 v4 v5 v6 = liftIO $ dyn130 ptr_glColorSubTable v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glColorSubTable #-}
ptr_glColorSubTable :: FunPtr (GLenum -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glColorSubTable = unsafePerformIO $ getCommand "glColorSubTable"

-- glColorSubTableEXT ----------------------------------------------------------

-- | This command is an alias for 'glColorSubTable'.
glColorSubTableEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ColorTableTarget@.
  -> GLsizei -- ^ @start@.
  -> GLsizei -- ^ @count@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @data@ pointing to @COMPSIZE(format,type,count)@ elements of type @a@.
  -> m ()
glColorSubTableEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn130 ptr_glColorSubTableEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glColorSubTableEXT #-}
ptr_glColorSubTableEXT :: FunPtr (GLenum -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glColorSubTableEXT = unsafePerformIO $ getCommand "glColorSubTableEXT"

-- glColorTable ----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColorTable.xml OpenGL 2.x>.
glColorTable
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ColorTableTarget@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @table@ pointing to @COMPSIZE(format,type,width)@ elements of type @a@.
  -> m ()
glColorTable v1 v2 v3 v4 v5 v6 = liftIO $ dyn131 ptr_glColorTable v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glColorTable #-}
ptr_glColorTable :: FunPtr (GLenum -> GLenum -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glColorTable = unsafePerformIO $ getCommand "glColorTable"

-- glColorTableEXT -------------------------------------------------------------

-- | This command is an alias for 'glColorTable'.
glColorTableEXT
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ColorTableTarget@.
  -> GLenum -- ^ @internalFormat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @table@ pointing to @COMPSIZE(format,type,width)@ elements of type @a@.
  -> m ()
glColorTableEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn131 ptr_glColorTableEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glColorTableEXT #-}
ptr_glColorTableEXT :: FunPtr (GLenum -> GLenum -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glColorTableEXT = unsafePerformIO $ getCommand "glColorTableEXT"

-- glColorTableParameterfv -----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColorTableParameter.xml OpenGL 2.x>.
glColorTableParameterfv
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ColorTableTarget@.
  -> GLenum -- ^ @pname@ of type @ColorTableParameterPName@.
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedFloat32@.
  -> m ()
glColorTableParameterfv v1 v2 v3 = liftIO $ dyn132 ptr_glColorTableParameterfv v1 v2 v3

{-# NOINLINE ptr_glColorTableParameterfv #-}
ptr_glColorTableParameterfv :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glColorTableParameterfv = unsafePerformIO $ getCommand "glColorTableParameterfv"

-- glColorTableParameterfvSGI --------------------------------------------------

-- | This command is an alias for 'glColorTableParameterfv'.
glColorTableParameterfvSGI
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ColorTableTargetSGI](Graphics-GL-Groups.html#ColorTableTargetSGI).
  -> GLenum -- ^ @pname@ of type [ColorTableParameterPNameSGI](Graphics-GL-Groups.html#ColorTableParameterPNameSGI).
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedFloat32@.
  -> m ()
glColorTableParameterfvSGI v1 v2 v3 = liftIO $ dyn132 ptr_glColorTableParameterfvSGI v1 v2 v3

{-# NOINLINE ptr_glColorTableParameterfvSGI #-}
ptr_glColorTableParameterfvSGI :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glColorTableParameterfvSGI = unsafePerformIO $ getCommand "glColorTableParameterfvSGI"

-- glColorTableParameteriv -----------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glColorTableParameter.xml OpenGL 2.x>.
glColorTableParameteriv
  :: MonadIO m
  => GLenum -- ^ @target@ of type @ColorTableTarget@.
  -> GLenum -- ^ @pname@ of type @ColorTableParameterPName@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedInt32@.
  -> m ()
glColorTableParameteriv v1 v2 v3 = liftIO $ dyn133 ptr_glColorTableParameteriv v1 v2 v3

{-# NOINLINE ptr_glColorTableParameteriv #-}
ptr_glColorTableParameteriv :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glColorTableParameteriv = unsafePerformIO $ getCommand "glColorTableParameteriv"

-- glColorTableParameterivSGI --------------------------------------------------

-- | This command is an alias for 'glColorTableParameteriv'.
glColorTableParameterivSGI
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ColorTableTargetSGI](Graphics-GL-Groups.html#ColorTableTargetSGI).
  -> GLenum -- ^ @pname@ of type [ColorTableParameterPNameSGI](Graphics-GL-Groups.html#ColorTableParameterPNameSGI).
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedInt32@.
  -> m ()
glColorTableParameterivSGI v1 v2 v3 = liftIO $ dyn133 ptr_glColorTableParameterivSGI v1 v2 v3

{-# NOINLINE ptr_glColorTableParameterivSGI #-}
ptr_glColorTableParameterivSGI :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glColorTableParameterivSGI = unsafePerformIO $ getCommand "glColorTableParameterivSGI"

-- glColorTableSGI -------------------------------------------------------------

-- | This command is an alias for 'glColorTable'.
glColorTableSGI
  :: MonadIO m
  => GLenum -- ^ @target@ of type [ColorTableTargetSGI](Graphics-GL-Groups.html#ColorTableTargetSGI).
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLenum -- ^ @type@ of type [PixelType](Graphics-GL-Groups.html#PixelType).
  -> Ptr a -- ^ @table@ pointing to @COMPSIZE(format,type,width)@ elements of type @a@.
  -> m ()
glColorTableSGI v1 v2 v3 v4 v5 v6 = liftIO $ dyn131 ptr_glColorTableSGI v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glColorTableSGI #-}
ptr_glColorTableSGI :: FunPtr (GLenum -> GLenum -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glColorTableSGI = unsafePerformIO $ getCommand "glColorTableSGI"

-- glCombinerInputNV -----------------------------------------------------------

glCombinerInputNV
  :: MonadIO m
  => GLenum -- ^ @stage@ of type @CombinerStageNV@.
  -> GLenum -- ^ @portion@ of type @CombinerPortionNV@.
  -> GLenum -- ^ @variable@ of type @CombinerVariableNV@.
  -> GLenum -- ^ @input@ of type @CombinerRegisterNV@.
  -> GLenum -- ^ @mapping@ of type @CombinerMappingNV@.
  -> GLenum -- ^ @componentUsage@ of type @CombinerComponentUsageNV@.
  -> m ()
glCombinerInputNV v1 v2 v3 v4 v5 v6 = liftIO $ dyn134 ptr_glCombinerInputNV v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glCombinerInputNV #-}
ptr_glCombinerInputNV :: FunPtr (GLenum -> GLenum -> GLenum -> GLenum -> GLenum -> GLenum -> IO ())
ptr_glCombinerInputNV = unsafePerformIO $ getCommand "glCombinerInputNV"

-- glCombinerOutputNV ----------------------------------------------------------

glCombinerOutputNV
  :: MonadIO m
  => GLenum -- ^ @stage@ of type @CombinerStageNV@.
  -> GLenum -- ^ @portion@ of type @CombinerPortionNV@.
  -> GLenum -- ^ @abOutput@ of type @CombinerRegisterNV@.
  -> GLenum -- ^ @cdOutput@ of type @CombinerRegisterNV@.
  -> GLenum -- ^ @sumOutput@ of type @CombinerRegisterNV@.
  -> GLenum -- ^ @scale@ of type @CombinerScaleNV@.
  -> GLenum -- ^ @bias@ of type @CombinerBiasNV@.
  -> GLboolean -- ^ @abDotProduct@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLboolean -- ^ @cdDotProduct@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> GLboolean -- ^ @muxSum@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glCombinerOutputNV v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO $ dyn135 ptr_glCombinerOutputNV v1 v2 v3 v4 v5 v6 v7 v8 v9 v10

{-# NOINLINE ptr_glCombinerOutputNV #-}
ptr_glCombinerOutputNV :: FunPtr (GLenum -> GLenum -> GLenum -> GLenum -> GLenum -> GLenum -> GLenum -> GLboolean -> GLboolean -> GLboolean -> IO ())
ptr_glCombinerOutputNV = unsafePerformIO $ getCommand "glCombinerOutputNV"

-- glCombinerParameterfNV ------------------------------------------------------

glCombinerParameterfNV
  :: MonadIO m
  => GLenum -- ^ @pname@ of type @CombinerParameterNV@.
  -> GLfloat -- ^ @param@.
  -> m ()
glCombinerParameterfNV v1 v2 = liftIO $ dyn0 ptr_glCombinerParameterfNV v1 v2

{-# NOINLINE ptr_glCombinerParameterfNV #-}
ptr_glCombinerParameterfNV :: FunPtr (GLenum -> GLfloat -> IO ())
ptr_glCombinerParameterfNV = unsafePerformIO $ getCommand "glCombinerParameterfNV"

-- glCombinerParameterfvNV -----------------------------------------------------

glCombinerParameterfvNV
  :: MonadIO m
  => GLenum -- ^ @pname@ of type @CombinerParameterNV@.
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedFloat32@.
  -> m ()
glCombinerParameterfvNV v1 v2 = liftIO $ dyn94 ptr_glCombinerParameterfvNV v1 v2

{-# NOINLINE ptr_glCombinerParameterfvNV #-}
ptr_glCombinerParameterfvNV :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glCombinerParameterfvNV = unsafePerformIO $ getCommand "glCombinerParameterfvNV"

-- glCombinerParameteriNV ------------------------------------------------------

glCombinerParameteriNV
  :: MonadIO m
  => GLenum -- ^ @pname@ of type @CombinerParameterNV@.
  -> GLint -- ^ @param@.
  -> m ()
glCombinerParameteriNV v1 v2 = liftIO $ dyn55 ptr_glCombinerParameteriNV v1 v2

{-# NOINLINE ptr_glCombinerParameteriNV #-}
ptr_glCombinerParameteriNV :: FunPtr (GLenum -> GLint -> IO ())
ptr_glCombinerParameteriNV = unsafePerformIO $ getCommand "glCombinerParameteriNV"

-- glCombinerParameterivNV -----------------------------------------------------

glCombinerParameterivNV
  :: MonadIO m
  => GLenum -- ^ @pname@ of type @CombinerParameterNV@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedInt32@.
  -> m ()
glCombinerParameterivNV v1 v2 = liftIO $ dyn136 ptr_glCombinerParameterivNV v1 v2

{-# NOINLINE ptr_glCombinerParameterivNV #-}
ptr_glCombinerParameterivNV :: FunPtr (GLenum -> Ptr GLint -> IO ())
ptr_glCombinerParameterivNV = unsafePerformIO $ getCommand "glCombinerParameterivNV"

-- glCombinerStageParameterfvNV ------------------------------------------------

glCombinerStageParameterfvNV
  :: MonadIO m
  => GLenum -- ^ @stage@ of type @CombinerStageNV@.
  -> GLenum -- ^ @pname@ of type @CombinerParameterNV@.
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @CheckedFloat32@.
  -> m ()
glCombinerStageParameterfvNV v1 v2 v3 = liftIO $ dyn132 ptr_glCombinerStageParameterfvNV v1 v2 v3

{-# NOINLINE ptr_glCombinerStageParameterfvNV #-}
ptr_glCombinerStageParameterfvNV :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glCombinerStageParameterfvNV = unsafePerformIO $ getCommand "glCombinerStageParameterfvNV"

-- glCommandListSegmentsNV -----------------------------------------------------

glCommandListSegmentsNV
  :: MonadIO m
  => GLuint -- ^ @list@.
  -> GLuint -- ^ @segments@.
  -> m ()
glCommandListSegmentsNV v1 v2 = liftIO $ dyn3 ptr_glCommandListSegmentsNV v1 v2

{-# NOINLINE ptr_glCommandListSegmentsNV #-}
ptr_glCommandListSegmentsNV :: FunPtr (GLuint -> GLuint -> IO ())
ptr_glCommandListSegmentsNV = unsafePerformIO $ getCommand "glCommandListSegmentsNV"

-- glCompileCommandListNV ------------------------------------------------------

glCompileCommandListNV
  :: MonadIO m
  => GLuint -- ^ @list@.
  -> m ()
glCompileCommandListNV v1 = liftIO $ dyn2 ptr_glCompileCommandListNV v1

{-# NOINLINE ptr_glCompileCommandListNV #-}
ptr_glCompileCommandListNV :: FunPtr (GLuint -> IO ())
ptr_glCompileCommandListNV = unsafePerformIO $ getCommand "glCompileCommandListNV"

-- glCompileShader -------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glCompileShader.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glCompileShader.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glCompileShader.xhtml OpenGL 4.x>.
glCompileShader
  :: MonadIO m
  => GLuint -- ^ @shader@.
  -> m ()
glCompileShader v1 = liftIO $ dyn2 ptr_glCompileShader v1

{-# NOINLINE ptr_glCompileShader #-}
ptr_glCompileShader :: FunPtr (GLuint -> IO ())
ptr_glCompileShader = unsafePerformIO $ getCommand "glCompileShader"

-- glCompileShaderARB ----------------------------------------------------------

-- | This command is an alias for 'glCompileShader'.
glCompileShaderARB
  :: MonadIO m
  => GLhandleARB -- ^ @shaderObj@ of type @handleARB@.
  -> m ()
glCompileShaderARB v1 = liftIO $ dyn137 ptr_glCompileShaderARB v1

{-# NOINLINE ptr_glCompileShaderARB #-}
ptr_glCompileShaderARB :: FunPtr (GLhandleARB -> IO ())
ptr_glCompileShaderARB = unsafePerformIO $ getCommand "glCompileShaderARB"

-- glCompileShaderIncludeARB ---------------------------------------------------

glCompileShaderIncludeARB
  :: MonadIO m
  => GLuint -- ^ @shader@.
  -> GLsizei -- ^ @count@.
  -> Ptr (Ptr GLchar) -- ^ @path@ pointing to @count@ elements of type @Ptr GLchar@.
  -> Ptr GLint -- ^ @length@ pointing to @count@ elements of type @GLint@.
  -> m ()
glCompileShaderIncludeARB v1 v2 v3 v4 = liftIO $ dyn138 ptr_glCompileShaderIncludeARB v1 v2 v3 v4

{-# NOINLINE ptr_glCompileShaderIncludeARB #-}
ptr_glCompileShaderIncludeARB :: FunPtr (GLuint -> GLsizei -> Ptr (Ptr GLchar) -> Ptr GLint -> IO ())
ptr_glCompileShaderIncludeARB = unsafePerformIO $ getCommand "glCompileShaderIncludeARB"

-- glCompressedMultiTexImage1DEXT ----------------------------------------------

glCompressedMultiTexImage1DEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type @TextureUnit@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @internalformat@ of type @TextureInternalFormat@.
  -> GLsizei -- ^ @width@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @bits@ pointing to @imageSize@ elements of type @a@.
  -> m ()
glCompressedMultiTexImage1DEXT v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn139 ptr_glCompressedMultiTexImage1DEXT v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glCompressedMultiTexImage1DEXT #-}
ptr_glCompressedMultiTexImage1DEXT :: FunPtr (GLenum -> GLenum -> GLint -> GLenum -> GLsizei -> GLint -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedMultiTexImage1DEXT = unsafePerformIO $ getCommand "glCompressedMultiTexImage1DEXT"

-- glCompressedMultiTexImage2DEXT ----------------------------------------------

glCompressedMultiTexImage2DEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type @TextureUnit@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @internalformat@ of type @TextureInternalFormat@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @bits@ pointing to @imageSize@ elements of type @a@.
  -> m ()
glCompressedMultiTexImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn140 ptr_glCompressedMultiTexImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glCompressedMultiTexImage2DEXT #-}
ptr_glCompressedMultiTexImage2DEXT :: FunPtr (GLenum -> GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedMultiTexImage2DEXT = unsafePerformIO $ getCommand "glCompressedMultiTexImage2DEXT"

-- glCompressedMultiTexImage3DEXT ----------------------------------------------

glCompressedMultiTexImage3DEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type @TextureUnit@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @internalformat@ of type @TextureInternalFormat@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @bits@ pointing to @imageSize@ elements of type @a@.
  -> m ()
glCompressedMultiTexImage3DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO $ dyn141 ptr_glCompressedMultiTexImage3DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10

{-# NOINLINE ptr_glCompressedMultiTexImage3DEXT #-}
ptr_glCompressedMultiTexImage3DEXT :: FunPtr (GLenum -> GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedMultiTexImage3DEXT = unsafePerformIO $ getCommand "glCompressedMultiTexImage3DEXT"

-- glCompressedMultiTexSubImage1DEXT -------------------------------------------

glCompressedMultiTexSubImage1DEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type @TextureUnit@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLsizei -- ^ @width@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @bits@ pointing to @imageSize@ elements of type @a@.
  -> m ()
glCompressedMultiTexSubImage1DEXT v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn142 ptr_glCompressedMultiTexSubImage1DEXT v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glCompressedMultiTexSubImage1DEXT #-}
ptr_glCompressedMultiTexSubImage1DEXT :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedMultiTexSubImage1DEXT = unsafePerformIO $ getCommand "glCompressedMultiTexSubImage1DEXT"

-- glCompressedMultiTexSubImage2DEXT -------------------------------------------

glCompressedMultiTexSubImage2DEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type @TextureUnit@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @yoffset@ of type @CheckedInt32@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @bits@ pointing to @imageSize@ elements of type @a@.
  -> m ()
glCompressedMultiTexSubImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = liftIO $ dyn143 ptr_glCompressedMultiTexSubImage2DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10

{-# NOINLINE ptr_glCompressedMultiTexSubImage2DEXT #-}
ptr_glCompressedMultiTexSubImage2DEXT :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedMultiTexSubImage2DEXT = unsafePerformIO $ getCommand "glCompressedMultiTexSubImage2DEXT"

-- glCompressedMultiTexSubImage3DEXT -------------------------------------------

glCompressedMultiTexSubImage3DEXT
  :: MonadIO m
  => GLenum -- ^ @texunit@ of type @TextureUnit@.
  -> GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLint -- ^ @xoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @yoffset@ of type @CheckedInt32@.
  -> GLint -- ^ @zoffset@ of type @CheckedInt32@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLenum -- ^ @format@ of type [PixelFormat](Graphics-GL-Groups.html#PixelFormat).
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @bits@ pointing to @imageSize@ elements of type @a@.
  -> m ()
glCompressedMultiTexSubImage3DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 = liftIO $ dyn144 ptr_glCompressedMultiTexSubImage3DEXT v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12

{-# NOINLINE ptr_glCompressedMultiTexSubImage3DEXT #-}
ptr_glCompressedMultiTexSubImage3DEXT :: FunPtr (GLenum -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedMultiTexSubImage3DEXT = unsafePerformIO $ getCommand "glCompressedMultiTexSubImage3DEXT"

-- glCompressedTexImage1D ------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glCompressedTexImage1D.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glCompressedTexImage1D.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glCompressedTexImage1D.xhtml OpenGL 4.x>.
glCompressedTexImage1D
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @data@ pointing to @imageSize@ elements of type @CompressedTextureARB@.
  -> m ()
glCompressedTexImage1D v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn145 ptr_glCompressedTexImage1D v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glCompressedTexImage1D #-}
ptr_glCompressedTexImage1D :: FunPtr (GLenum -> GLint -> GLenum -> GLsizei -> GLint -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedTexImage1D = unsafePerformIO $ getCommand "glCompressedTexImage1D"

-- glCompressedTexImage1DARB ---------------------------------------------------

-- | This command is an alias for 'glCompressedTexImage1D'.
glCompressedTexImage1DARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @data@ pointing to @imageSize@ elements of type @CompressedTextureARB@.
  -> m ()
glCompressedTexImage1DARB v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn145 ptr_glCompressedTexImage1DARB v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glCompressedTexImage1DARB #-}
ptr_glCompressedTexImage1DARB :: FunPtr (GLenum -> GLint -> GLenum -> GLsizei -> GLint -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedTexImage1DARB = unsafePerformIO $ getCommand "glCompressedTexImage1DARB"

-- glCompressedTexImage2D ------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glCompressedTexImage2D.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glCompressedTexImage2D.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glCompressedTexImage2D.xhtml OpenGL 4.x>.
glCompressedTexImage2D
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @data@ pointing to @imageSize@ elements of type @CompressedTextureARB@.
  -> m ()
glCompressedTexImage2D v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn146 ptr_glCompressedTexImage2D v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glCompressedTexImage2D #-}
ptr_glCompressedTexImage2D :: FunPtr (GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedTexImage2D = unsafePerformIO $ getCommand "glCompressedTexImage2D"

-- glCompressedTexImage2DARB ---------------------------------------------------

-- | This command is an alias for 'glCompressedTexImage2D'.
glCompressedTexImage2DARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @data@ pointing to @imageSize@ elements of type @CompressedTextureARB@.
  -> m ()
glCompressedTexImage2DARB v1 v2 v3 v4 v5 v6 v7 v8 = liftIO $ dyn146 ptr_glCompressedTexImage2DARB v1 v2 v3 v4 v5 v6 v7 v8

{-# NOINLINE ptr_glCompressedTexImage2DARB #-}
ptr_glCompressedTexImage2DARB :: FunPtr (GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedTexImage2DARB = unsafePerformIO $ getCommand "glCompressedTexImage2DARB"

-- glCompressedTexImage3D ------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glCompressedTexImage3D.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glCompressedTexImage3D.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glCompressedTexImage3D.xhtml OpenGL 4.x>.
glCompressedTexImage3D
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @data@ pointing to @imageSize@ elements of type @CompressedTextureARB@.
  -> m ()
glCompressedTexImage3D v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn147 ptr_glCompressedTexImage3D v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glCompressedTexImage3D #-}
ptr_glCompressedTexImage3D :: FunPtr (GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedTexImage3D = unsafePerformIO $ getCommand "glCompressedTexImage3D"

-- glCompressedTexImage3DARB ---------------------------------------------------

-- | This command is an alias for 'glCompressedTexImage3D'.
glCompressedTexImage3DARB
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLint -- ^ @level@ of type @CheckedInt32@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> GLsizei -- ^ @depth@.
  -> GLint -- ^ @border@ of type @CheckedInt32@.
  -> GLsizei -- ^ @imageSize@.
  -> Ptr a -- ^ @data@ pointing to @imageSize@ elements of type @CompressedTextureARB@.
  -> m ()
glCompressedTexImage3DARB v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn147 ptr_glCompressedTexImage3DARB v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glCompressedTexImage3DARB #-}
ptr_glCompressedTexImage3DARB :: FunPtr (GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr a -> IO ())
ptr_glCompressedTexImage3DARB = unsafePerformIO $ getCommand "glCompressedTexImage3DARB"

