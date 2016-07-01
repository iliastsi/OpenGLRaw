--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F72
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

module Graphics.GL.Functions.F72 (
  glVertexAttrib4NivARB,
  glVertexAttrib4Nsv,
  glVertexAttrib4NsvARB,
  glVertexAttrib4Nub,
  glVertexAttrib4NubARB,
  glVertexAttrib4Nubv,
  glVertexAttrib4NubvARB,
  glVertexAttrib4Nuiv,
  glVertexAttrib4NuivARB,
  glVertexAttrib4Nusv,
  glVertexAttrib4NusvARB,
  glVertexAttrib4bv,
  glVertexAttrib4bvARB,
  glVertexAttrib4d,
  glVertexAttrib4dARB,
  glVertexAttrib4dNV,
  glVertexAttrib4dv,
  glVertexAttrib4dvARB,
  glVertexAttrib4dvNV,
  glVertexAttrib4f,
  glVertexAttrib4fARB,
  glVertexAttrib4fNV,
  glVertexAttrib4fv,
  glVertexAttrib4fvARB,
  glVertexAttrib4fvNV,
  glVertexAttrib4hNV,
  glVertexAttrib4hvNV,
  glVertexAttrib4iv,
  glVertexAttrib4ivARB,
  glVertexAttrib4s,
  glVertexAttrib4sARB,
  glVertexAttrib4sNV,
  glVertexAttrib4sv,
  glVertexAttrib4svARB,
  glVertexAttrib4svNV,
  glVertexAttrib4ubNV,
  glVertexAttrib4ubv,
  glVertexAttrib4ubvARB,
  glVertexAttrib4ubvNV,
  glVertexAttrib4uiv
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

-- glVertexAttrib4NivARB -------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib4Niv'.
glVertexAttrib4NivARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLint -- ^ @v@ pointing to @4@ elements of type @GLint@.
  -> m ()
glVertexAttrib4NivARB v1 v2 = liftIO $ dyn701 ptr_glVertexAttrib4NivARB v1 v2

{-# NOINLINE ptr_glVertexAttrib4NivARB #-}
ptr_glVertexAttrib4NivARB :: FunPtr (GLuint -> Ptr GLint -> IO ())
ptr_glVertexAttrib4NivARB = unsafePerformIO $ getCommand "glVertexAttrib4NivARB"

-- glVertexAttrib4Nsv ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttrib4Nsv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLshort -- ^ @v@ pointing to @4@ elements of type @GLshort@.
  -> m ()
glVertexAttrib4Nsv v1 v2 = liftIO $ dyn829 ptr_glVertexAttrib4Nsv v1 v2

{-# NOINLINE ptr_glVertexAttrib4Nsv #-}
ptr_glVertexAttrib4Nsv :: FunPtr (GLuint -> Ptr GLshort -> IO ())
ptr_glVertexAttrib4Nsv = unsafePerformIO $ getCommand "glVertexAttrib4Nsv"

-- glVertexAttrib4NsvARB -------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib4Nsv'.
glVertexAttrib4NsvARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLshort -- ^ @v@ pointing to @4@ elements of type @GLshort@.
  -> m ()
glVertexAttrib4NsvARB v1 v2 = liftIO $ dyn829 ptr_glVertexAttrib4NsvARB v1 v2

{-# NOINLINE ptr_glVertexAttrib4NsvARB #-}
ptr_glVertexAttrib4NsvARB :: FunPtr (GLuint -> Ptr GLshort -> IO ())
ptr_glVertexAttrib4NsvARB = unsafePerformIO $ getCommand "glVertexAttrib4NsvARB"

-- glVertexAttrib4Nub ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttrib4Nub
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLubyte -- ^ @x@.
  -> GLubyte -- ^ @y@.
  -> GLubyte -- ^ @z@.
  -> GLubyte -- ^ @w@.
  -> m ()
glVertexAttrib4Nub v1 v2 v3 v4 v5 = liftIO $ dyn851 ptr_glVertexAttrib4Nub v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttrib4Nub #-}
ptr_glVertexAttrib4Nub :: FunPtr (GLuint -> GLubyte -> GLubyte -> GLubyte -> GLubyte -> IO ())
ptr_glVertexAttrib4Nub = unsafePerformIO $ getCommand "glVertexAttrib4Nub"

-- glVertexAttrib4NubARB -------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib4Nub'.
glVertexAttrib4NubARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLubyte -- ^ @x@.
  -> GLubyte -- ^ @y@.
  -> GLubyte -- ^ @z@.
  -> GLubyte -- ^ @w@.
  -> m ()
glVertexAttrib4NubARB v1 v2 v3 v4 v5 = liftIO $ dyn851 ptr_glVertexAttrib4NubARB v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttrib4NubARB #-}
ptr_glVertexAttrib4NubARB :: FunPtr (GLuint -> GLubyte -> GLubyte -> GLubyte -> GLubyte -> IO ())
ptr_glVertexAttrib4NubARB = unsafePerformIO $ getCommand "glVertexAttrib4NubARB"

-- glVertexAttrib4Nubv ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttrib4Nubv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLubyte -- ^ @v@ pointing to @4@ elements of type @GLubyte@.
  -> m ()
glVertexAttrib4Nubv v1 v2 = liftIO $ dyn376 ptr_glVertexAttrib4Nubv v1 v2

{-# NOINLINE ptr_glVertexAttrib4Nubv #-}
ptr_glVertexAttrib4Nubv :: FunPtr (GLuint -> Ptr GLubyte -> IO ())
ptr_glVertexAttrib4Nubv = unsafePerformIO $ getCommand "glVertexAttrib4Nubv"

-- glVertexAttrib4NubvARB ------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib4Nubv'.
glVertexAttrib4NubvARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLubyte -- ^ @v@ pointing to @4@ elements of type @GLubyte@.
  -> m ()
glVertexAttrib4NubvARB v1 v2 = liftIO $ dyn376 ptr_glVertexAttrib4NubvARB v1 v2

{-# NOINLINE ptr_glVertexAttrib4NubvARB #-}
ptr_glVertexAttrib4NubvARB :: FunPtr (GLuint -> Ptr GLubyte -> IO ())
ptr_glVertexAttrib4NubvARB = unsafePerformIO $ getCommand "glVertexAttrib4NubvARB"

-- glVertexAttrib4Nuiv ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttrib4Nuiv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLuint -- ^ @v@ pointing to @4@ elements of type @GLuint@.
  -> m ()
glVertexAttrib4Nuiv v1 v2 = liftIO $ dyn194 ptr_glVertexAttrib4Nuiv v1 v2

{-# NOINLINE ptr_glVertexAttrib4Nuiv #-}
ptr_glVertexAttrib4Nuiv :: FunPtr (GLuint -> Ptr GLuint -> IO ())
ptr_glVertexAttrib4Nuiv = unsafePerformIO $ getCommand "glVertexAttrib4Nuiv"

-- glVertexAttrib4NuivARB ------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib4Nuiv'.
glVertexAttrib4NuivARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLuint -- ^ @v@ pointing to @4@ elements of type @GLuint@.
  -> m ()
glVertexAttrib4NuivARB v1 v2 = liftIO $ dyn194 ptr_glVertexAttrib4NuivARB v1 v2

{-# NOINLINE ptr_glVertexAttrib4NuivARB #-}
ptr_glVertexAttrib4NuivARB :: FunPtr (GLuint -> Ptr GLuint -> IO ())
ptr_glVertexAttrib4NuivARB = unsafePerformIO $ getCommand "glVertexAttrib4NuivARB"

-- glVertexAttrib4Nusv ---------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttrib4Nusv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLushort -- ^ @v@ pointing to @4@ elements of type @GLushort@.
  -> m ()
glVertexAttrib4Nusv v1 v2 = liftIO $ dyn830 ptr_glVertexAttrib4Nusv v1 v2

{-# NOINLINE ptr_glVertexAttrib4Nusv #-}
ptr_glVertexAttrib4Nusv :: FunPtr (GLuint -> Ptr GLushort -> IO ())
ptr_glVertexAttrib4Nusv = unsafePerformIO $ getCommand "glVertexAttrib4Nusv"

-- glVertexAttrib4NusvARB ------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib4Nusv'.
glVertexAttrib4NusvARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLushort -- ^ @v@ pointing to @4@ elements of type @GLushort@.
  -> m ()
glVertexAttrib4NusvARB v1 v2 = liftIO $ dyn830 ptr_glVertexAttrib4NusvARB v1 v2

{-# NOINLINE ptr_glVertexAttrib4NusvARB #-}
ptr_glVertexAttrib4NusvARB :: FunPtr (GLuint -> Ptr GLushort -> IO ())
ptr_glVertexAttrib4NusvARB = unsafePerformIO $ getCommand "glVertexAttrib4NusvARB"

-- glVertexAttrib4bv -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttrib4bv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLbyte -- ^ @v@ pointing to @4@ elements of type @GLbyte@.
  -> m ()
glVertexAttrib4bv v1 v2 = liftIO $ dyn827 ptr_glVertexAttrib4bv v1 v2

{-# NOINLINE ptr_glVertexAttrib4bv #-}
ptr_glVertexAttrib4bv :: FunPtr (GLuint -> Ptr GLbyte -> IO ())
ptr_glVertexAttrib4bv = unsafePerformIO $ getCommand "glVertexAttrib4bv"

-- glVertexAttrib4bvARB --------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib4bv'.
glVertexAttrib4bvARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLbyte -- ^ @v@ pointing to @4@ elements of type @GLbyte@.
  -> m ()
glVertexAttrib4bvARB v1 v2 = liftIO $ dyn827 ptr_glVertexAttrib4bvARB v1 v2

{-# NOINLINE ptr_glVertexAttrib4bvARB #-}
ptr_glVertexAttrib4bvARB :: FunPtr (GLuint -> Ptr GLbyte -> IO ())
ptr_glVertexAttrib4bvARB = unsafePerformIO $ getCommand "glVertexAttrib4bvARB"

-- glVertexAttrib4d ------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>. The vector equivalent of this command is 'glVertexAttrib4dv'.
glVertexAttrib4d
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> GLdouble -- ^ @z@.
  -> GLdouble -- ^ @w@.
  -> m ()
glVertexAttrib4d v1 v2 v3 v4 v5 = liftIO $ dyn852 ptr_glVertexAttrib4d v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttrib4d #-}
ptr_glVertexAttrib4d :: FunPtr (GLuint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glVertexAttrib4d = unsafePerformIO $ getCommand "glVertexAttrib4d"

-- glVertexAttrib4dARB ---------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib4dvARB'. This command is an alias for 'glVertexAttrib4d'.
glVertexAttrib4dARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> GLdouble -- ^ @z@.
  -> GLdouble -- ^ @w@.
  -> m ()
glVertexAttrib4dARB v1 v2 v3 v4 v5 = liftIO $ dyn852 ptr_glVertexAttrib4dARB v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttrib4dARB #-}
ptr_glVertexAttrib4dARB :: FunPtr (GLuint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glVertexAttrib4dARB = unsafePerformIO $ getCommand "glVertexAttrib4dARB"

-- glVertexAttrib4dNV ----------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib4dvNV'. This command is an alias for 'glVertexAttrib4d'.
glVertexAttrib4dNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLdouble -- ^ @x@.
  -> GLdouble -- ^ @y@.
  -> GLdouble -- ^ @z@.
  -> GLdouble -- ^ @w@.
  -> m ()
glVertexAttrib4dNV v1 v2 v3 v4 v5 = liftIO $ dyn852 ptr_glVertexAttrib4dNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttrib4dNV #-}
ptr_glVertexAttrib4dNV :: FunPtr (GLuint -> GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glVertexAttrib4dNV = unsafePerformIO $ getCommand "glVertexAttrib4dNV"

-- glVertexAttrib4dv -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttrib4dv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLdouble -- ^ @v@ pointing to @4@ elements of type @GLdouble@.
  -> m ()
glVertexAttrib4dv v1 v2 = liftIO $ dyn828 ptr_glVertexAttrib4dv v1 v2

{-# NOINLINE ptr_glVertexAttrib4dv #-}
ptr_glVertexAttrib4dv :: FunPtr (GLuint -> Ptr GLdouble -> IO ())
ptr_glVertexAttrib4dv = unsafePerformIO $ getCommand "glVertexAttrib4dv"

-- glVertexAttrib4dvARB --------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib4dv'.
glVertexAttrib4dvARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLdouble -- ^ @v@ pointing to @4@ elements of type @GLdouble@.
  -> m ()
glVertexAttrib4dvARB v1 v2 = liftIO $ dyn828 ptr_glVertexAttrib4dvARB v1 v2

{-# NOINLINE ptr_glVertexAttrib4dvARB #-}
ptr_glVertexAttrib4dvARB :: FunPtr (GLuint -> Ptr GLdouble -> IO ())
ptr_glVertexAttrib4dvARB = unsafePerformIO $ getCommand "glVertexAttrib4dvARB"

-- glVertexAttrib4dvNV ---------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib4dv'.
glVertexAttrib4dvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLdouble -- ^ @v@ pointing to @4@ elements of type @GLdouble@.
  -> m ()
glVertexAttrib4dvNV v1 v2 = liftIO $ dyn828 ptr_glVertexAttrib4dvNV v1 v2

{-# NOINLINE ptr_glVertexAttrib4dvNV #-}
ptr_glVertexAttrib4dvNV :: FunPtr (GLuint -> Ptr GLdouble -> IO ())
ptr_glVertexAttrib4dvNV = unsafePerformIO $ getCommand "glVertexAttrib4dvNV"

-- glVertexAttrib4f ------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>. The vector equivalent of this command is 'glVertexAttrib4fv'.
glVertexAttrib4f
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> GLfloat -- ^ @w@.
  -> m ()
glVertexAttrib4f v1 v2 v3 v4 v5 = liftIO $ dyn853 ptr_glVertexAttrib4f v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttrib4f #-}
ptr_glVertexAttrib4f :: FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glVertexAttrib4f = unsafePerformIO $ getCommand "glVertexAttrib4f"

-- glVertexAttrib4fARB ---------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib4fvARB'. This command is an alias for 'glVertexAttrib4f'.
glVertexAttrib4fARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> GLfloat -- ^ @w@.
  -> m ()
glVertexAttrib4fARB v1 v2 v3 v4 v5 = liftIO $ dyn853 ptr_glVertexAttrib4fARB v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttrib4fARB #-}
ptr_glVertexAttrib4fARB :: FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glVertexAttrib4fARB = unsafePerformIO $ getCommand "glVertexAttrib4fARB"

-- glVertexAttrib4fNV ----------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib4fvNV'. This command is an alias for 'glVertexAttrib4f'.
glVertexAttrib4fNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> GLfloat -- ^ @w@.
  -> m ()
glVertexAttrib4fNV v1 v2 v3 v4 v5 = liftIO $ dyn853 ptr_glVertexAttrib4fNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttrib4fNV #-}
ptr_glVertexAttrib4fNV :: FunPtr (GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glVertexAttrib4fNV = unsafePerformIO $ getCommand "glVertexAttrib4fNV"

-- glVertexAttrib4fv -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttrib4fv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLfloat -- ^ @v@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glVertexAttrib4fv v1 v2 = liftIO $ dyn377 ptr_glVertexAttrib4fv v1 v2

{-# NOINLINE ptr_glVertexAttrib4fv #-}
ptr_glVertexAttrib4fv :: FunPtr (GLuint -> Ptr GLfloat -> IO ())
ptr_glVertexAttrib4fv = unsafePerformIO $ getCommand "glVertexAttrib4fv"

-- glVertexAttrib4fvARB --------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib4fv'.
glVertexAttrib4fvARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLfloat -- ^ @v@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glVertexAttrib4fvARB v1 v2 = liftIO $ dyn377 ptr_glVertexAttrib4fvARB v1 v2

{-# NOINLINE ptr_glVertexAttrib4fvARB #-}
ptr_glVertexAttrib4fvARB :: FunPtr (GLuint -> Ptr GLfloat -> IO ())
ptr_glVertexAttrib4fvARB = unsafePerformIO $ getCommand "glVertexAttrib4fvARB"

-- glVertexAttrib4fvNV ---------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib4fv'.
glVertexAttrib4fvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLfloat -- ^ @v@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glVertexAttrib4fvNV v1 v2 = liftIO $ dyn377 ptr_glVertexAttrib4fvNV v1 v2

{-# NOINLINE ptr_glVertexAttrib4fvNV #-}
ptr_glVertexAttrib4fvNV :: FunPtr (GLuint -> Ptr GLfloat -> IO ())
ptr_glVertexAttrib4fvNV = unsafePerformIO $ getCommand "glVertexAttrib4fvNV"

-- glVertexAttrib4hNV ----------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib4hvNV'.
glVertexAttrib4hNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLhalfNV -- ^ @x@ of type @Half16NV@.
  -> GLhalfNV -- ^ @y@ of type @Half16NV@.
  -> GLhalfNV -- ^ @z@ of type @Half16NV@.
  -> GLhalfNV -- ^ @w@ of type @Half16NV@.
  -> m ()
glVertexAttrib4hNV v1 v2 v3 v4 v5 = liftIO $ dyn854 ptr_glVertexAttrib4hNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttrib4hNV #-}
ptr_glVertexAttrib4hNV :: FunPtr (GLuint -> GLhalfNV -> GLhalfNV -> GLhalfNV -> GLhalfNV -> IO ())
ptr_glVertexAttrib4hNV = unsafePerformIO $ getCommand "glVertexAttrib4hNV"

-- glVertexAttrib4hvNV ---------------------------------------------------------

glVertexAttrib4hvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLhalfNV -- ^ @v@ pointing to @4@ elements of type @Half16NV@.
  -> m ()
glVertexAttrib4hvNV v1 v2 = liftIO $ dyn844 ptr_glVertexAttrib4hvNV v1 v2

{-# NOINLINE ptr_glVertexAttrib4hvNV #-}
ptr_glVertexAttrib4hvNV :: FunPtr (GLuint -> Ptr GLhalfNV -> IO ())
ptr_glVertexAttrib4hvNV = unsafePerformIO $ getCommand "glVertexAttrib4hvNV"

-- glVertexAttrib4iv -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttrib4iv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLint -- ^ @v@ pointing to @4@ elements of type @GLint@.
  -> m ()
glVertexAttrib4iv v1 v2 = liftIO $ dyn701 ptr_glVertexAttrib4iv v1 v2

{-# NOINLINE ptr_glVertexAttrib4iv #-}
ptr_glVertexAttrib4iv :: FunPtr (GLuint -> Ptr GLint -> IO ())
ptr_glVertexAttrib4iv = unsafePerformIO $ getCommand "glVertexAttrib4iv"

-- glVertexAttrib4ivARB --------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib4iv'.
glVertexAttrib4ivARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLint -- ^ @v@ pointing to @4@ elements of type @GLint@.
  -> m ()
glVertexAttrib4ivARB v1 v2 = liftIO $ dyn701 ptr_glVertexAttrib4ivARB v1 v2

{-# NOINLINE ptr_glVertexAttrib4ivARB #-}
ptr_glVertexAttrib4ivARB :: FunPtr (GLuint -> Ptr GLint -> IO ())
ptr_glVertexAttrib4ivARB = unsafePerformIO $ getCommand "glVertexAttrib4ivARB"

-- glVertexAttrib4s ------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>. The vector equivalent of this command is 'glVertexAttrib4sv'.
glVertexAttrib4s
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLshort -- ^ @x@.
  -> GLshort -- ^ @y@.
  -> GLshort -- ^ @z@.
  -> GLshort -- ^ @w@.
  -> m ()
glVertexAttrib4s v1 v2 v3 v4 v5 = liftIO $ dyn855 ptr_glVertexAttrib4s v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttrib4s #-}
ptr_glVertexAttrib4s :: FunPtr (GLuint -> GLshort -> GLshort -> GLshort -> GLshort -> IO ())
ptr_glVertexAttrib4s = unsafePerformIO $ getCommand "glVertexAttrib4s"

-- glVertexAttrib4sARB ---------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib4svARB'. This command is an alias for 'glVertexAttrib4s'.
glVertexAttrib4sARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLshort -- ^ @x@.
  -> GLshort -- ^ @y@.
  -> GLshort -- ^ @z@.
  -> GLshort -- ^ @w@.
  -> m ()
glVertexAttrib4sARB v1 v2 v3 v4 v5 = liftIO $ dyn855 ptr_glVertexAttrib4sARB v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttrib4sARB #-}
ptr_glVertexAttrib4sARB :: FunPtr (GLuint -> GLshort -> GLshort -> GLshort -> GLshort -> IO ())
ptr_glVertexAttrib4sARB = unsafePerformIO $ getCommand "glVertexAttrib4sARB"

-- glVertexAttrib4sNV ----------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib4svNV'. This command is an alias for 'glVertexAttrib4s'.
glVertexAttrib4sNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLshort -- ^ @x@.
  -> GLshort -- ^ @y@.
  -> GLshort -- ^ @z@.
  -> GLshort -- ^ @w@.
  -> m ()
glVertexAttrib4sNV v1 v2 v3 v4 v5 = liftIO $ dyn855 ptr_glVertexAttrib4sNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttrib4sNV #-}
ptr_glVertexAttrib4sNV :: FunPtr (GLuint -> GLshort -> GLshort -> GLshort -> GLshort -> IO ())
ptr_glVertexAttrib4sNV = unsafePerformIO $ getCommand "glVertexAttrib4sNV"

-- glVertexAttrib4sv -----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttrib4sv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLshort -- ^ @v@ pointing to @4@ elements of type @GLshort@.
  -> m ()
glVertexAttrib4sv v1 v2 = liftIO $ dyn829 ptr_glVertexAttrib4sv v1 v2

{-# NOINLINE ptr_glVertexAttrib4sv #-}
ptr_glVertexAttrib4sv :: FunPtr (GLuint -> Ptr GLshort -> IO ())
ptr_glVertexAttrib4sv = unsafePerformIO $ getCommand "glVertexAttrib4sv"

-- glVertexAttrib4svARB --------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib4sv'.
glVertexAttrib4svARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLshort -- ^ @v@ pointing to @4@ elements of type @GLshort@.
  -> m ()
glVertexAttrib4svARB v1 v2 = liftIO $ dyn829 ptr_glVertexAttrib4svARB v1 v2

{-# NOINLINE ptr_glVertexAttrib4svARB #-}
ptr_glVertexAttrib4svARB :: FunPtr (GLuint -> Ptr GLshort -> IO ())
ptr_glVertexAttrib4svARB = unsafePerformIO $ getCommand "glVertexAttrib4svARB"

-- glVertexAttrib4svNV ---------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib4sv'.
glVertexAttrib4svNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLshort -- ^ @v@ pointing to @4@ elements of type @GLshort@.
  -> m ()
glVertexAttrib4svNV v1 v2 = liftIO $ dyn829 ptr_glVertexAttrib4svNV v1 v2

{-# NOINLINE ptr_glVertexAttrib4svNV #-}
ptr_glVertexAttrib4svNV :: FunPtr (GLuint -> Ptr GLshort -> IO ())
ptr_glVertexAttrib4svNV = unsafePerformIO $ getCommand "glVertexAttrib4svNV"

-- glVertexAttrib4ubNV ---------------------------------------------------------

-- | The vector equivalent of this command is 'glVertexAttrib4ubvNV'. This command is an alias for 'glVertexAttrib4Nub'.
glVertexAttrib4ubNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLubyte -- ^ @x@ of type @ColorUB@.
  -> GLubyte -- ^ @y@ of type @ColorUB@.
  -> GLubyte -- ^ @z@ of type @ColorUB@.
  -> GLubyte -- ^ @w@ of type @ColorUB@.
  -> m ()
glVertexAttrib4ubNV v1 v2 v3 v4 v5 = liftIO $ dyn851 ptr_glVertexAttrib4ubNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glVertexAttrib4ubNV #-}
ptr_glVertexAttrib4ubNV :: FunPtr (GLuint -> GLubyte -> GLubyte -> GLubyte -> GLubyte -> IO ())
ptr_glVertexAttrib4ubNV = unsafePerformIO $ getCommand "glVertexAttrib4ubNV"

-- glVertexAttrib4ubv ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttrib4ubv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLubyte -- ^ @v@ pointing to @4@ elements of type @GLubyte@.
  -> m ()
glVertexAttrib4ubv v1 v2 = liftIO $ dyn376 ptr_glVertexAttrib4ubv v1 v2

{-# NOINLINE ptr_glVertexAttrib4ubv #-}
ptr_glVertexAttrib4ubv :: FunPtr (GLuint -> Ptr GLubyte -> IO ())
ptr_glVertexAttrib4ubv = unsafePerformIO $ getCommand "glVertexAttrib4ubv"

-- glVertexAttrib4ubvARB -------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib4ubv'.
glVertexAttrib4ubvARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLubyte -- ^ @v@ pointing to @4@ elements of type @GLubyte@.
  -> m ()
glVertexAttrib4ubvARB v1 v2 = liftIO $ dyn376 ptr_glVertexAttrib4ubvARB v1 v2

{-# NOINLINE ptr_glVertexAttrib4ubvARB #-}
ptr_glVertexAttrib4ubvARB :: FunPtr (GLuint -> Ptr GLubyte -> IO ())
ptr_glVertexAttrib4ubvARB = unsafePerformIO $ getCommand "glVertexAttrib4ubvARB"

-- glVertexAttrib4ubvNV --------------------------------------------------------

-- | This command is an alias for 'glVertexAttrib4Nubv'.
glVertexAttrib4ubvNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLubyte -- ^ @v@ pointing to @4@ elements of type @ColorUB@.
  -> m ()
glVertexAttrib4ubvNV v1 v2 = liftIO $ dyn376 ptr_glVertexAttrib4ubvNV v1 v2

{-# NOINLINE ptr_glVertexAttrib4ubvNV #-}
ptr_glVertexAttrib4ubvNV :: FunPtr (GLuint -> Ptr GLubyte -> IO ())
ptr_glVertexAttrib4ubvNV = unsafePerformIO $ getCommand "glVertexAttrib4ubvNV"

-- glVertexAttrib4uiv ----------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glVertexAttrib.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glVertexAttrib.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glVertexAttrib.xhtml OpenGL 4.x>.
glVertexAttrib4uiv
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> Ptr GLuint -- ^ @v@ pointing to @4@ elements of type @GLuint@.
  -> m ()
glVertexAttrib4uiv v1 v2 = liftIO $ dyn194 ptr_glVertexAttrib4uiv v1 v2

{-# NOINLINE ptr_glVertexAttrib4uiv #-}
ptr_glVertexAttrib4uiv :: FunPtr (GLuint -> Ptr GLuint -> IO ())
ptr_glVertexAttrib4uiv = unsafePerformIO $ getCommand "glVertexAttrib4uiv"

