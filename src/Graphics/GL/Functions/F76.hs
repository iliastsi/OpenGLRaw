--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F76
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

module Graphics.GL.Functions.F76 (
  glVertexAttribs4hvNV,
  glVertexAttribs4svNV,
  glVertexAttribs4ubvNV,
  glVertexBindingDivisor,
  glVertexBlendARB,
  glVertexBlendEnvfATI,
  glVertexBlendEnviATI,
  glVertexFormatNV,
  glVertexP2ui,
  glVertexP2uiv,
  glVertexP3ui,
  glVertexP3uiv,
  glVertexP4ui,
  glVertexP4uiv,
  glVertexPointer,
  glVertexPointerEXT,
  glVertexPointerListIBM,
  glVertexPointervINTEL,
  glVertexStream1dATI,
  glVertexStream1dvATI,
  glVertexStream1fATI,
  glVertexStream1fvATI,
  glVertexStream1iATI,
  glVertexStream1ivATI,
  glVertexStream1sATI,
  glVertexStream1svATI,
  glVertexStream2dATI,
  glVertexStream2dvATI,
  glVertexStream2fATI,
  glVertexStream2fvATI,
  glVertexStream2iATI,
  glVertexStream2ivATI,
  glVertexStream2sATI,
  glVertexStream2svATI,
  glVertexStream3dATI,
  glVertexStream3dvATI,
  glVertexStream3fATI,
  glVertexStream3fvATI,
  glVertexStream3iATI,
  glVertexStream3ivATI
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

-- glVertexAttribs4hvNV --------------------------------------------------------

glVertexAttribs4hvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLsizei -- ^ @n@.
  -> Ptr GLhalfNV -- ^ @v@ pointing to @n@ elements of type @Half16NV@.
  -> m ()
glVertexAttribs4hvNV v1 v2 v3 = liftIO $ dyn876 ptr_glVertexAttribs4hvNV v1 v2 v3

{-# NOINLINE ptr_glVertexAttribs4hvNV #-}
ptr_glVertexAttribs4hvNV :: FunPtr (GLuint -> GLsizei -> Ptr GLhalfNV -> IO ())
ptr_glVertexAttribs4hvNV = unsafePerformIO $ getCommand "glVertexAttribs4hvNV"

-- glVertexAttribs4svNV --------------------------------------------------------

glVertexAttribs4svNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLshort -- ^ @v@ pointing to @count*4@ elements of type @GLshort@.
  -> m ()
glVertexAttribs4svNV v1 v2 v3 = liftIO $ dyn877 ptr_glVertexAttribs4svNV v1 v2 v3

{-# NOINLINE ptr_glVertexAttribs4svNV #-}
ptr_glVertexAttribs4svNV :: FunPtr (GLuint -> GLsizei -> Ptr GLshort -> IO ())
ptr_glVertexAttribs4svNV = unsafePerformIO $ getCommand "glVertexAttribs4svNV"

-- glVertexAttribs4ubvNV -------------------------------------------------------

glVertexAttribs4ubvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLubyte -- ^ @v@ pointing to @count*4@ elements of type @ColorUB@.
  -> m ()
glVertexAttribs4ubvNV v1 v2 v3 = liftIO $ dyn878 ptr_glVertexAttribs4ubvNV v1 v2 v3

{-# NOINLINE ptr_glVertexAttribs4ubvNV #-}
ptr_glVertexAttribs4ubvNV :: FunPtr (GLuint -> GLsizei -> Ptr GLubyte -> IO ())
ptr_glVertexAttribs4ubvNV = unsafePerformIO $ getCommand "glVertexAttribs4ubvNV"

-- glVertexBindingDivisor ------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glVertexBindingDivisor.xhtml OpenGL 4.x>.
glVertexBindingDivisor
  :: MonadIO m
  => GLuint -- ^ @bindingindex@.
  -> GLuint -- ^ @divisor@.
  -> m ()
glVertexBindingDivisor v1 v2 = liftIO $ dyn3 ptr_glVertexBindingDivisor v1 v2

{-# NOINLINE ptr_glVertexBindingDivisor #-}
ptr_glVertexBindingDivisor :: FunPtr (GLuint -> GLuint -> IO ())
ptr_glVertexBindingDivisor = unsafePerformIO $ getCommand "glVertexBindingDivisor"

-- glVertexBlendARB ------------------------------------------------------------

glVertexBlendARB
  :: MonadIO m
  => GLint -- ^ @count@.
  -> m ()
glVertexBlendARB v1 = liftIO $ dyn12 ptr_glVertexBlendARB v1

{-# NOINLINE ptr_glVertexBlendARB #-}
ptr_glVertexBlendARB :: FunPtr (GLint -> IO ())
ptr_glVertexBlendARB = unsafePerformIO $ getCommand "glVertexBlendARB"

-- glVertexBlendEnvfATI --------------------------------------------------------

glVertexBlendEnvfATI
  :: MonadIO m
  => GLenum -- ^ @pname@ of type @VertexStreamATI@.
  -> GLfloat -- ^ @param@.
  -> m ()
glVertexBlendEnvfATI v1 v2 = liftIO $ dyn0 ptr_glVertexBlendEnvfATI v1 v2

{-# NOINLINE ptr_glVertexBlendEnvfATI #-}
ptr_glVertexBlendEnvfATI :: FunPtr (GLenum -> GLfloat -> IO ())
ptr_glVertexBlendEnvfATI = unsafePerformIO $ getCommand "glVertexBlendEnvfATI"

-- glVertexBlendEnviATI --------------------------------------------------------

glVertexBlendEnviATI
  :: MonadIO m
  => GLenum -- ^ @pname@ of type @VertexStreamATI@.
  -> GLint -- ^ @param@.
  -> m ()
glVertexBlendEnviATI v1 v2 = liftIO $ dyn55 ptr_glVertexBlendEnviATI v1 v2

{-# NOINLINE ptr_glVertexBlendEnviATI #-}
ptr_glVertexBlendEnviATI :: FunPtr (GLenum -> GLint -> IO ())
ptr_glVertexBlendEnviATI = unsafePerformIO $ getCommand "glVertexBlendEnviATI"

-- glVertexFormatNV ------------------------------------------------------------

glVertexFormatNV
  :: MonadIO m
  => GLint -- ^ @size@.
  -> GLenum -- ^ @type@.
  -> GLsizei -- ^ @stride@.
  -> m ()
glVertexFormatNV v1 v2 v3 = liftIO $ dyn119 ptr_glVertexFormatNV v1 v2 v3

{-# NOINLINE ptr_glVertexFormatNV #-}
ptr_glVertexFormatNV :: FunPtr (GLint -> GLenum -> GLsizei -> IO ())
ptr_glVertexFormatNV = unsafePerformIO $ getCommand "glVertexFormatNV"

-- glVertexP2ui ----------------------------------------------------------------

glVertexP2ui
  :: MonadIO m
  => GLenum -- ^ @type@.
  -> GLuint -- ^ @value@.
  -> m ()
glVertexP2ui v1 v2 = liftIO $ dyn16 ptr_glVertexP2ui v1 v2

{-# NOINLINE ptr_glVertexP2ui #-}
ptr_glVertexP2ui :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glVertexP2ui = unsafePerformIO $ getCommand "glVertexP2ui"

-- glVertexP2uiv ---------------------------------------------------------------

glVertexP2uiv
  :: MonadIO m
  => GLenum -- ^ @type@.
  -> Ptr GLuint -- ^ @value@ pointing to @1@ element of type @GLuint@.
  -> m ()
glVertexP2uiv v1 v2 = liftIO $ dyn125 ptr_glVertexP2uiv v1 v2

{-# NOINLINE ptr_glVertexP2uiv #-}
ptr_glVertexP2uiv :: FunPtr (GLenum -> Ptr GLuint -> IO ())
ptr_glVertexP2uiv = unsafePerformIO $ getCommand "glVertexP2uiv"

-- glVertexP3ui ----------------------------------------------------------------

glVertexP3ui
  :: MonadIO m
  => GLenum -- ^ @type@.
  -> GLuint -- ^ @value@.
  -> m ()
glVertexP3ui v1 v2 = liftIO $ dyn16 ptr_glVertexP3ui v1 v2

{-# NOINLINE ptr_glVertexP3ui #-}
ptr_glVertexP3ui :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glVertexP3ui = unsafePerformIO $ getCommand "glVertexP3ui"

-- glVertexP3uiv ---------------------------------------------------------------

glVertexP3uiv
  :: MonadIO m
  => GLenum -- ^ @type@.
  -> Ptr GLuint -- ^ @value@ pointing to @1@ element of type @GLuint@.
  -> m ()
glVertexP3uiv v1 v2 = liftIO $ dyn125 ptr_glVertexP3uiv v1 v2

{-# NOINLINE ptr_glVertexP3uiv #-}
ptr_glVertexP3uiv :: FunPtr (GLenum -> Ptr GLuint -> IO ())
ptr_glVertexP3uiv = unsafePerformIO $ getCommand "glVertexP3uiv"

-- glVertexP4ui ----------------------------------------------------------------

glVertexP4ui
  :: MonadIO m
  => GLenum -- ^ @type@.
  -> GLuint -- ^ @value@.
  -> m ()
glVertexP4ui v1 v2 = liftIO $ dyn16 ptr_glVertexP4ui v1 v2

{-# NOINLINE ptr_glVertexP4ui #-}
ptr_glVertexP4ui :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glVertexP4ui = unsafePerformIO $ getCommand "glVertexP4ui"

-- glVertexP4uiv ---------------------------------------------------------------

glVertexP4uiv
  :: MonadIO m
  => GLenum -- ^ @type@.
  -> Ptr GLuint -- ^ @value@ pointing to @1@ element of type @GLuint@.
  -> m ()
glVertexP4uiv v1 v2 = liftIO $ dyn125 ptr_glVertexP4uiv v1 v2

{-# NOINLINE ptr_glVertexP4uiv #-}
ptr_glVertexP4uiv :: FunPtr (GLenum -> Ptr GLuint -> IO ())
ptr_glVertexP4uiv = unsafePerformIO $ getCommand "glVertexP4uiv"

-- glVertexPointer -------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexPointer.xml OpenGL 2.x>.
glVertexPointer
  :: MonadIO m
  => GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [VertexPointerType](Graphics-GL-Groups.html#VertexPointerType).
  -> GLsizei -- ^ @stride@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(size,type,stride)@ elements of type @a@.
  -> m ()
glVertexPointer v1 v2 v3 v4 = liftIO $ dyn126 ptr_glVertexPointer v1 v2 v3 v4

{-# NOINLINE ptr_glVertexPointer #-}
ptr_glVertexPointer :: FunPtr (GLint -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glVertexPointer = unsafePerformIO $ getCommand "glVertexPointer"

-- glVertexPointerEXT ----------------------------------------------------------

glVertexPointerEXT
  :: MonadIO m
  => GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [VertexPointerType](Graphics-GL-Groups.html#VertexPointerType).
  -> GLsizei -- ^ @stride@.
  -> GLsizei -- ^ @count@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(size,type,stride,count)@ elements of type @a@.
  -> m ()
glVertexPointerEXT v1 v2 v3 v4 v5 = liftIO $ dyn127 ptr_glVertexPointerEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexPointerEXT #-}
ptr_glVertexPointerEXT :: FunPtr (GLint -> GLenum -> GLsizei -> GLsizei -> Ptr a -> IO ())
ptr_glVertexPointerEXT = unsafePerformIO $ getCommand "glVertexPointerEXT"

-- glVertexPointerListIBM ------------------------------------------------------

glVertexPointerListIBM
  :: MonadIO m
  => GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [VertexPointerType](Graphics-GL-Groups.html#VertexPointerType).
  -> GLint -- ^ @stride@.
  -> Ptr (Ptr a) -- ^ @pointer@ pointing to @COMPSIZE(size,type,stride)@ elements of type @Ptr a@.
  -> GLint -- ^ @ptrstride@.
  -> m ()
glVertexPointerListIBM v1 v2 v3 v4 v5 = liftIO $ dyn128 ptr_glVertexPointerListIBM v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexPointerListIBM #-}
ptr_glVertexPointerListIBM :: FunPtr (GLint -> GLenum -> GLint -> Ptr (Ptr a) -> GLint -> IO ())
ptr_glVertexPointerListIBM = unsafePerformIO $ getCommand "glVertexPointerListIBM"

-- glVertexPointervINTEL -------------------------------------------------------

glVertexPointervINTEL
  :: MonadIO m
  => GLint -- ^ @size@.
  -> GLenum -- ^ @type@ of type [VertexPointerType](Graphics-GL-Groups.html#VertexPointerType).
  -> Ptr (Ptr a) -- ^ @pointer@ pointing to @4@ elements of type @Ptr a@.
  -> m ()
glVertexPointervINTEL v1 v2 v3 = liftIO $ dyn129 ptr_glVertexPointervINTEL v1 v2 v3

{-# NOINLINE ptr_glVertexPointervINTEL #-}
ptr_glVertexPointervINTEL :: FunPtr (GLint -> GLenum -> Ptr (Ptr a) -> IO ())
ptr_glVertexPointervINTEL = unsafePerformIO $ getCommand "glVertexPointervINTEL"

-- glVertexStream1dATI ---------------------------------------------------------

glVertexStream1dATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type @VertexStreamATI@.
  -> GLdouble -- ^ @x@.
  -> m ()
glVertexStream1dATI v1 v2 = liftIO $ dyn541 ptr_glVertexStream1dATI v1 v2

{-# NOINLINE ptr_glVertexStream1dATI #-}
ptr_glVertexStream1dATI :: FunPtr (GLenum -> GLdouble -> IO ())
ptr_glVertexStream1dATI = unsafePerformIO $ getCommand "glVertexStream1dATI"

-- glVertexStream1dvATI --------------------------------------------------------

glVertexStream1dvATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type @VertexStreamATI@.
  -> Ptr GLdouble -- ^ @coords@ pointing to @1@ element of type @GLdouble@.
  -> m ()
glVertexStream1dvATI v1 v2 = liftIO $ dyn93 ptr_glVertexStream1dvATI v1 v2

{-# NOINLINE ptr_glVertexStream1dvATI #-}
ptr_glVertexStream1dvATI :: FunPtr (GLenum -> Ptr GLdouble -> IO ())
ptr_glVertexStream1dvATI = unsafePerformIO $ getCommand "glVertexStream1dvATI"

-- glVertexStream1fATI ---------------------------------------------------------

glVertexStream1fATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type @VertexStreamATI@.
  -> GLfloat -- ^ @x@.
  -> m ()
glVertexStream1fATI v1 v2 = liftIO $ dyn0 ptr_glVertexStream1fATI v1 v2

{-# NOINLINE ptr_glVertexStream1fATI #-}
ptr_glVertexStream1fATI :: FunPtr (GLenum -> GLfloat -> IO ())
ptr_glVertexStream1fATI = unsafePerformIO $ getCommand "glVertexStream1fATI"

-- glVertexStream1fvATI --------------------------------------------------------

glVertexStream1fvATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type @VertexStreamATI@.
  -> Ptr GLfloat -- ^ @coords@ pointing to @1@ element of type @GLfloat@.
  -> m ()
glVertexStream1fvATI v1 v2 = liftIO $ dyn94 ptr_glVertexStream1fvATI v1 v2

{-# NOINLINE ptr_glVertexStream1fvATI #-}
ptr_glVertexStream1fvATI :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glVertexStream1fvATI = unsafePerformIO $ getCommand "glVertexStream1fvATI"

-- glVertexStream1iATI ---------------------------------------------------------

glVertexStream1iATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type @VertexStreamATI@.
  -> GLint -- ^ @x@.
  -> m ()
glVertexStream1iATI v1 v2 = liftIO $ dyn55 ptr_glVertexStream1iATI v1 v2

{-# NOINLINE ptr_glVertexStream1iATI #-}
ptr_glVertexStream1iATI :: FunPtr (GLenum -> GLint -> IO ())
ptr_glVertexStream1iATI = unsafePerformIO $ getCommand "glVertexStream1iATI"

-- glVertexStream1ivATI --------------------------------------------------------

glVertexStream1ivATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type @VertexStreamATI@.
  -> Ptr GLint -- ^ @coords@ pointing to @1@ element of type @GLint@.
  -> m ()
glVertexStream1ivATI v1 v2 = liftIO $ dyn136 ptr_glVertexStream1ivATI v1 v2

{-# NOINLINE ptr_glVertexStream1ivATI #-}
ptr_glVertexStream1ivATI :: FunPtr (GLenum -> Ptr GLint -> IO ())
ptr_glVertexStream1ivATI = unsafePerformIO $ getCommand "glVertexStream1ivATI"

-- glVertexStream1sATI ---------------------------------------------------------

glVertexStream1sATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type @VertexStreamATI@.
  -> GLshort -- ^ @x@.
  -> m ()
glVertexStream1sATI v1 v2 = liftIO $ dyn544 ptr_glVertexStream1sATI v1 v2

{-# NOINLINE ptr_glVertexStream1sATI #-}
ptr_glVertexStream1sATI :: FunPtr (GLenum -> GLshort -> IO ())
ptr_glVertexStream1sATI = unsafePerformIO $ getCommand "glVertexStream1sATI"

-- glVertexStream1svATI --------------------------------------------------------

glVertexStream1svATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type @VertexStreamATI@.
  -> Ptr GLshort -- ^ @coords@ pointing to @1@ element of type @GLshort@.
  -> m ()
glVertexStream1svATI v1 v2 = liftIO $ dyn545 ptr_glVertexStream1svATI v1 v2

{-# NOINLINE ptr_glVertexStream1svATI #-}
ptr_glVertexStream1svATI :: FunPtr (GLenum -> Ptr GLshort -> IO ())
ptr_glVertexStream1svATI = unsafePerformIO $ getCommand "glVertexStream1svATI"

-- glVertexStream2dATI ---------------------------------------------------------

glVertexStream2dATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type @VertexStreamATI@.
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> m ()
glVertexStream2dATI v1 v2 v3 = liftIO $ dyn547 ptr_glVertexStream2dATI v1 v2 v3

{-# NOINLINE ptr_glVertexStream2dATI #-}
ptr_glVertexStream2dATI :: FunPtr (GLenum -> GLdouble -> GLdouble -> IO ())
ptr_glVertexStream2dATI = unsafePerformIO $ getCommand "glVertexStream2dATI"

-- glVertexStream2dvATI --------------------------------------------------------

glVertexStream2dvATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type @VertexStreamATI@.
  -> Ptr GLdouble -- ^ @coords@ pointing to @2@ elements of type @GLdouble@.
  -> m ()
glVertexStream2dvATI v1 v2 = liftIO $ dyn93 ptr_glVertexStream2dvATI v1 v2

{-# NOINLINE ptr_glVertexStream2dvATI #-}
ptr_glVertexStream2dvATI :: FunPtr (GLenum -> Ptr GLdouble -> IO ())
ptr_glVertexStream2dvATI = unsafePerformIO $ getCommand "glVertexStream2dvATI"

-- glVertexStream2fATI ---------------------------------------------------------

glVertexStream2fATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type @VertexStreamATI@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> m ()
glVertexStream2fATI v1 v2 v3 = liftIO $ dyn548 ptr_glVertexStream2fATI v1 v2 v3

{-# NOINLINE ptr_glVertexStream2fATI #-}
ptr_glVertexStream2fATI :: FunPtr (GLenum -> GLfloat -> GLfloat -> IO ())
ptr_glVertexStream2fATI = unsafePerformIO $ getCommand "glVertexStream2fATI"

-- glVertexStream2fvATI --------------------------------------------------------

glVertexStream2fvATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type @VertexStreamATI@.
  -> Ptr GLfloat -- ^ @coords@ pointing to @2@ elements of type @GLfloat@.
  -> m ()
glVertexStream2fvATI v1 v2 = liftIO $ dyn94 ptr_glVertexStream2fvATI v1 v2

{-# NOINLINE ptr_glVertexStream2fvATI #-}
ptr_glVertexStream2fvATI :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glVertexStream2fvATI = unsafePerformIO $ getCommand "glVertexStream2fvATI"

-- glVertexStream2iATI ---------------------------------------------------------

glVertexStream2iATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type @VertexStreamATI@.
  -> GLint -- ^ @x@.
  -> GLint -- ^ @y@.
  -> m ()
glVertexStream2iATI v1 v2 v3 = liftIO $ dyn264 ptr_glVertexStream2iATI v1 v2 v3

{-# NOINLINE ptr_glVertexStream2iATI #-}
ptr_glVertexStream2iATI :: FunPtr (GLenum -> GLint -> GLint -> IO ())
ptr_glVertexStream2iATI = unsafePerformIO $ getCommand "glVertexStream2iATI"

-- glVertexStream2ivATI --------------------------------------------------------

glVertexStream2ivATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type @VertexStreamATI@.
  -> Ptr GLint -- ^ @coords@ pointing to @2@ elements of type @GLint@.
  -> m ()
glVertexStream2ivATI v1 v2 = liftIO $ dyn136 ptr_glVertexStream2ivATI v1 v2

{-# NOINLINE ptr_glVertexStream2ivATI #-}
ptr_glVertexStream2ivATI :: FunPtr (GLenum -> Ptr GLint -> IO ())
ptr_glVertexStream2ivATI = unsafePerformIO $ getCommand "glVertexStream2ivATI"

-- glVertexStream2sATI ---------------------------------------------------------

glVertexStream2sATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type @VertexStreamATI@.
  -> GLshort -- ^ @x@.
  -> GLshort -- ^ @y@.
  -> m ()
glVertexStream2sATI v1 v2 v3 = liftIO $ dyn550 ptr_glVertexStream2sATI v1 v2 v3

{-# NOINLINE ptr_glVertexStream2sATI #-}
ptr_glVertexStream2sATI :: FunPtr (GLenum -> GLshort -> GLshort -> IO ())
ptr_glVertexStream2sATI = unsafePerformIO $ getCommand "glVertexStream2sATI"

-- glVertexStream2svATI --------------------------------------------------------

glVertexStream2svATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type @VertexStreamATI@.
  -> Ptr GLshort -- ^ @coords@ pointing to @2@ elements of type @GLshort@.
  -> m ()
glVertexStream2svATI v1 v2 = liftIO $ dyn545 ptr_glVertexStream2svATI v1 v2

{-# NOINLINE ptr_glVertexStream2svATI #-}
ptr_glVertexStream2svATI :: FunPtr (GLenum -> Ptr GLshort -> IO ())
ptr_glVertexStream2svATI = unsafePerformIO $ getCommand "glVertexStream2svATI"

-- glVertexStream3dATI ---------------------------------------------------------

glVertexStream3dATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type @VertexStreamATI@.
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> GLdouble -- ^ @z@.
  -> m ()
glVertexStream3dATI v1 v2 v3 v4 = liftIO $ dyn522 ptr_glVertexStream3dATI v1 v2 v3 v4

{-# NOINLINE ptr_glVertexStream3dATI #-}
ptr_glVertexStream3dATI :: FunPtr (GLenum -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glVertexStream3dATI = unsafePerformIO $ getCommand "glVertexStream3dATI"

-- glVertexStream3dvATI --------------------------------------------------------

glVertexStream3dvATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type @VertexStreamATI@.
  -> Ptr GLdouble -- ^ @coords@ pointing to @3@ elements of type @GLdouble@.
  -> m ()
glVertexStream3dvATI v1 v2 = liftIO $ dyn93 ptr_glVertexStream3dvATI v1 v2

{-# NOINLINE ptr_glVertexStream3dvATI #-}
ptr_glVertexStream3dvATI :: FunPtr (GLenum -> Ptr GLdouble -> IO ())
ptr_glVertexStream3dvATI = unsafePerformIO $ getCommand "glVertexStream3dvATI"

-- glVertexStream3fATI ---------------------------------------------------------

glVertexStream3fATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type @VertexStreamATI@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> m ()
glVertexStream3fATI v1 v2 v3 v4 = liftIO $ dyn523 ptr_glVertexStream3fATI v1 v2 v3 v4

{-# NOINLINE ptr_glVertexStream3fATI #-}
ptr_glVertexStream3fATI :: FunPtr (GLenum -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glVertexStream3fATI = unsafePerformIO $ getCommand "glVertexStream3fATI"

-- glVertexStream3fvATI --------------------------------------------------------

glVertexStream3fvATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type @VertexStreamATI@.
  -> Ptr GLfloat -- ^ @coords@ pointing to @3@ elements of type @GLfloat@.
  -> m ()
glVertexStream3fvATI v1 v2 = liftIO $ dyn94 ptr_glVertexStream3fvATI v1 v2

{-# NOINLINE ptr_glVertexStream3fvATI #-}
ptr_glVertexStream3fvATI :: FunPtr (GLenum -> Ptr GLfloat -> IO ())
ptr_glVertexStream3fvATI = unsafePerformIO $ getCommand "glVertexStream3fvATI"

-- glVertexStream3iATI ---------------------------------------------------------

glVertexStream3iATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type @VertexStreamATI@.
  -> GLint -- ^ @x@.
  -> GLint -- ^ @y@.
  -> GLint -- ^ @z@.
  -> m ()
glVertexStream3iATI v1 v2 v3 v4 = liftIO $ dyn554 ptr_glVertexStream3iATI v1 v2 v3 v4

{-# NOINLINE ptr_glVertexStream3iATI #-}
ptr_glVertexStream3iATI :: FunPtr (GLenum -> GLint -> GLint -> GLint -> IO ())
ptr_glVertexStream3iATI = unsafePerformIO $ getCommand "glVertexStream3iATI"

-- glVertexStream3ivATI --------------------------------------------------------

glVertexStream3ivATI
  :: MonadIO m
  => GLenum -- ^ @stream@ of type @VertexStreamATI@.
  -> Ptr GLint -- ^ @coords@ pointing to @3@ elements of type @GLint@.
  -> m ()
glVertexStream3ivATI v1 v2 = liftIO $ dyn136 ptr_glVertexStream3ivATI v1 v2

{-# NOINLINE ptr_glVertexStream3ivATI #-}
ptr_glVertexStream3ivATI :: FunPtr (GLenum -> Ptr GLint -> IO ())
ptr_glVertexStream3ivATI = unsafePerformIO $ getCommand "glVertexStream3ivATI"

