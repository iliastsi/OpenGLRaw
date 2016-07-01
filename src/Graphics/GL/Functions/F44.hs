--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F44
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

module Graphics.GL.Functions.F44 (
  glNamedProgramLocalParameterI4iEXT,
  glNamedProgramLocalParameterI4ivEXT,
  glNamedProgramLocalParameterI4uiEXT,
  glNamedProgramLocalParameterI4uivEXT,
  glNamedProgramLocalParameters4fvEXT,
  glNamedProgramLocalParametersI4ivEXT,
  glNamedProgramLocalParametersI4uivEXT,
  glNamedProgramStringEXT,
  glNamedRenderbufferStorage,
  glNamedRenderbufferStorageEXT,
  glNamedRenderbufferStorageMultisample,
  glNamedRenderbufferStorageMultisampleCoverageEXT,
  glNamedRenderbufferStorageMultisampleEXT,
  glNamedStringARB,
  glNewList,
  glNewObjectBufferATI,
  glNormal3b,
  glNormal3bv,
  glNormal3d,
  glNormal3dv,
  glNormal3f,
  glNormal3fVertex3fSUN,
  glNormal3fVertex3fvSUN,
  glNormal3fv,
  glNormal3hNV,
  glNormal3hvNV,
  glNormal3i,
  glNormal3iv,
  glNormal3s,
  glNormal3sv,
  glNormal3x,
  glNormal3xOES,
  glNormal3xvOES,
  glNormalFormatNV,
  glNormalP3ui,
  glNormalP3uiv,
  glNormalPointer,
  glNormalPointerEXT,
  glNormalPointerListIBM,
  glNormalPointervINTEL
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

-- glNamedProgramLocalParameterI4iEXT ------------------------------------------

-- | The vector equivalent of this command is 'glNamedProgramLocalParameterI4ivEXT'.
glNamedProgramLocalParameterI4iEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @target@ of type @ProgramTarget@.
  -> GLuint -- ^ @index@.
  -> GLint -- ^ @x@.
  -> GLint -- ^ @y@.
  -> GLint -- ^ @z@.
  -> GLint -- ^ @w@.
  -> m ()
glNamedProgramLocalParameterI4iEXT v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn583 ptr_glNamedProgramLocalParameterI4iEXT v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glNamedProgramLocalParameterI4iEXT #-}
ptr_glNamedProgramLocalParameterI4iEXT :: FunPtr (GLuint -> GLenum -> GLuint -> GLint -> GLint -> GLint -> GLint -> IO ())
ptr_glNamedProgramLocalParameterI4iEXT = unsafePerformIO $ getCommand "glNamedProgramLocalParameterI4iEXT"

-- glNamedProgramLocalParameterI4ivEXT -----------------------------------------

glNamedProgramLocalParameterI4ivEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @target@ of type @ProgramTarget@.
  -> GLuint -- ^ @index@.
  -> Ptr GLint -- ^ @params@ pointing to @4@ elements of type @GLint@.
  -> m ()
glNamedProgramLocalParameterI4ivEXT v1 v2 v3 v4 = liftIO $ dyn364 ptr_glNamedProgramLocalParameterI4ivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glNamedProgramLocalParameterI4ivEXT #-}
ptr_glNamedProgramLocalParameterI4ivEXT :: FunPtr (GLuint -> GLenum -> GLuint -> Ptr GLint -> IO ())
ptr_glNamedProgramLocalParameterI4ivEXT = unsafePerformIO $ getCommand "glNamedProgramLocalParameterI4ivEXT"

-- glNamedProgramLocalParameterI4uiEXT -----------------------------------------

-- | The vector equivalent of this command is 'glNamedProgramLocalParameterI4uivEXT'.
glNamedProgramLocalParameterI4uiEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @target@ of type @ProgramTarget@.
  -> GLuint -- ^ @index@.
  -> GLuint -- ^ @x@.
  -> GLuint -- ^ @y@.
  -> GLuint -- ^ @z@.
  -> GLuint -- ^ @w@.
  -> m ()
glNamedProgramLocalParameterI4uiEXT v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn584 ptr_glNamedProgramLocalParameterI4uiEXT v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glNamedProgramLocalParameterI4uiEXT #-}
ptr_glNamedProgramLocalParameterI4uiEXT :: FunPtr (GLuint -> GLenum -> GLuint -> GLuint -> GLuint -> GLuint -> GLuint -> IO ())
ptr_glNamedProgramLocalParameterI4uiEXT = unsafePerformIO $ getCommand "glNamedProgramLocalParameterI4uiEXT"

-- glNamedProgramLocalParameterI4uivEXT ----------------------------------------

glNamedProgramLocalParameterI4uivEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @target@ of type @ProgramTarget@.
  -> GLuint -- ^ @index@.
  -> Ptr GLuint -- ^ @params@ pointing to @4@ elements of type @GLuint@.
  -> m ()
glNamedProgramLocalParameterI4uivEXT v1 v2 v3 v4 = liftIO $ dyn365 ptr_glNamedProgramLocalParameterI4uivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glNamedProgramLocalParameterI4uivEXT #-}
ptr_glNamedProgramLocalParameterI4uivEXT :: FunPtr (GLuint -> GLenum -> GLuint -> Ptr GLuint -> IO ())
ptr_glNamedProgramLocalParameterI4uivEXT = unsafePerformIO $ getCommand "glNamedProgramLocalParameterI4uivEXT"

-- glNamedProgramLocalParameters4fvEXT -----------------------------------------

glNamedProgramLocalParameters4fvEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @target@ of type @ProgramTarget@.
  -> GLuint -- ^ @index@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @params@ pointing to @count*4@ elements of type @GLfloat@.
  -> m ()
glNamedProgramLocalParameters4fvEXT v1 v2 v3 v4 v5 = liftIO $ dyn585 ptr_glNamedProgramLocalParameters4fvEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glNamedProgramLocalParameters4fvEXT #-}
ptr_glNamedProgramLocalParameters4fvEXT :: FunPtr (GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glNamedProgramLocalParameters4fvEXT = unsafePerformIO $ getCommand "glNamedProgramLocalParameters4fvEXT"

-- glNamedProgramLocalParametersI4ivEXT ----------------------------------------

glNamedProgramLocalParametersI4ivEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @target@ of type @ProgramTarget@.
  -> GLuint -- ^ @index@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLint -- ^ @params@ pointing to @count*4@ elements of type @GLint@.
  -> m ()
glNamedProgramLocalParametersI4ivEXT v1 v2 v3 v4 v5 = liftIO $ dyn586 ptr_glNamedProgramLocalParametersI4ivEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glNamedProgramLocalParametersI4ivEXT #-}
ptr_glNamedProgramLocalParametersI4ivEXT :: FunPtr (GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLint -> IO ())
ptr_glNamedProgramLocalParametersI4ivEXT = unsafePerformIO $ getCommand "glNamedProgramLocalParametersI4ivEXT"

-- glNamedProgramLocalParametersI4uivEXT ---------------------------------------

glNamedProgramLocalParametersI4uivEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @target@ of type @ProgramTarget@.
  -> GLuint -- ^ @index@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLuint -- ^ @params@ pointing to @count*4@ elements of type @GLuint@.
  -> m ()
glNamedProgramLocalParametersI4uivEXT v1 v2 v3 v4 v5 = liftIO $ dyn587 ptr_glNamedProgramLocalParametersI4uivEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glNamedProgramLocalParametersI4uivEXT #-}
ptr_glNamedProgramLocalParametersI4uivEXT :: FunPtr (GLuint -> GLenum -> GLuint -> GLsizei -> Ptr GLuint -> IO ())
ptr_glNamedProgramLocalParametersI4uivEXT = unsafePerformIO $ getCommand "glNamedProgramLocalParametersI4uivEXT"

-- glNamedProgramStringEXT -----------------------------------------------------

glNamedProgramStringEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @target@ of type @ProgramTarget@.
  -> GLenum -- ^ @format@ of type @ProgramFormat@.
  -> GLsizei -- ^ @len@.
  -> Ptr a -- ^ @string@ pointing to @len@ elements of type @a@.
  -> m ()
glNamedProgramStringEXT v1 v2 v3 v4 v5 = liftIO $ dyn588 ptr_glNamedProgramStringEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glNamedProgramStringEXT #-}
ptr_glNamedProgramStringEXT :: FunPtr (GLuint -> GLenum -> GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glNamedProgramStringEXT = unsafePerformIO $ getCommand "glNamedProgramStringEXT"

-- glNamedRenderbufferStorage --------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glRenderbufferStorage.xhtml OpenGL 4.x>.
glNamedRenderbufferStorage
  :: MonadIO m
  => GLuint -- ^ @renderbuffer@.
  -> GLenum -- ^ @internalformat@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glNamedRenderbufferStorage v1 v2 v3 v4 = liftIO $ dyn589 ptr_glNamedRenderbufferStorage v1 v2 v3 v4

{-# NOINLINE ptr_glNamedRenderbufferStorage #-}
ptr_glNamedRenderbufferStorage :: FunPtr (GLuint -> GLenum -> GLsizei -> GLsizei -> IO ())
ptr_glNamedRenderbufferStorage = unsafePerformIO $ getCommand "glNamedRenderbufferStorage"

-- glNamedRenderbufferStorageEXT -----------------------------------------------

glNamedRenderbufferStorageEXT
  :: MonadIO m
  => GLuint -- ^ @renderbuffer@ of type @Renderbuffer@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glNamedRenderbufferStorageEXT v1 v2 v3 v4 = liftIO $ dyn589 ptr_glNamedRenderbufferStorageEXT v1 v2 v3 v4

{-# NOINLINE ptr_glNamedRenderbufferStorageEXT #-}
ptr_glNamedRenderbufferStorageEXT :: FunPtr (GLuint -> GLenum -> GLsizei -> GLsizei -> IO ())
ptr_glNamedRenderbufferStorageEXT = unsafePerformIO $ getCommand "glNamedRenderbufferStorageEXT"

-- glNamedRenderbufferStorageMultisample ---------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glRenderbufferStorageMultisample.xhtml OpenGL 4.x>.
glNamedRenderbufferStorageMultisample
  :: MonadIO m
  => GLuint -- ^ @renderbuffer@.
  -> GLsizei -- ^ @samples@.
  -> GLenum -- ^ @internalformat@.
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glNamedRenderbufferStorageMultisample v1 v2 v3 v4 v5 = liftIO $ dyn590 ptr_glNamedRenderbufferStorageMultisample v1 v2 v3 v4 v5

{-# NOINLINE ptr_glNamedRenderbufferStorageMultisample #-}
ptr_glNamedRenderbufferStorageMultisample :: FunPtr (GLuint -> GLsizei -> GLenum -> GLsizei -> GLsizei -> IO ())
ptr_glNamedRenderbufferStorageMultisample = unsafePerformIO $ getCommand "glNamedRenderbufferStorageMultisample"

-- glNamedRenderbufferStorageMultisampleCoverageEXT ----------------------------

glNamedRenderbufferStorageMultisampleCoverageEXT
  :: MonadIO m
  => GLuint -- ^ @renderbuffer@ of type @Renderbuffer@.
  -> GLsizei -- ^ @coverageSamples@.
  -> GLsizei -- ^ @colorSamples@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glNamedRenderbufferStorageMultisampleCoverageEXT v1 v2 v3 v4 v5 v6 = liftIO $ dyn591 ptr_glNamedRenderbufferStorageMultisampleCoverageEXT v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glNamedRenderbufferStorageMultisampleCoverageEXT #-}
ptr_glNamedRenderbufferStorageMultisampleCoverageEXT :: FunPtr (GLuint -> GLsizei -> GLsizei -> GLenum -> GLsizei -> GLsizei -> IO ())
ptr_glNamedRenderbufferStorageMultisampleCoverageEXT = unsafePerformIO $ getCommand "glNamedRenderbufferStorageMultisampleCoverageEXT"

-- glNamedRenderbufferStorageMultisampleEXT ------------------------------------

glNamedRenderbufferStorageMultisampleEXT
  :: MonadIO m
  => GLuint -- ^ @renderbuffer@ of type @Renderbuffer@.
  -> GLsizei -- ^ @samples@.
  -> GLenum -- ^ @internalformat@ of type [InternalFormat](Graphics-GL-Groups.html#InternalFormat).
  -> GLsizei -- ^ @width@.
  -> GLsizei -- ^ @height@.
  -> m ()
glNamedRenderbufferStorageMultisampleEXT v1 v2 v3 v4 v5 = liftIO $ dyn590 ptr_glNamedRenderbufferStorageMultisampleEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glNamedRenderbufferStorageMultisampleEXT #-}
ptr_glNamedRenderbufferStorageMultisampleEXT :: FunPtr (GLuint -> GLsizei -> GLenum -> GLsizei -> GLsizei -> IO ())
ptr_glNamedRenderbufferStorageMultisampleEXT = unsafePerformIO $ getCommand "glNamedRenderbufferStorageMultisampleEXT"

-- glNamedStringARB ------------------------------------------------------------

glNamedStringARB
  :: MonadIO m
  => GLenum -- ^ @type@.
  -> GLint -- ^ @namelen@.
  -> Ptr GLchar -- ^ @name@ pointing to @namelen@ elements of type @GLchar@.
  -> GLint -- ^ @stringlen@.
  -> Ptr GLchar -- ^ @string@ pointing to @stringlen@ elements of type @GLchar@.
  -> m ()
glNamedStringARB v1 v2 v3 v4 v5 = liftIO $ dyn592 ptr_glNamedStringARB v1 v2 v3 v4 v5

{-# NOINLINE ptr_glNamedStringARB #-}
ptr_glNamedStringARB :: FunPtr (GLenum -> GLint -> Ptr GLchar -> GLint -> Ptr GLchar -> IO ())
ptr_glNamedStringARB = unsafePerformIO $ getCommand "glNamedStringARB"

-- glNewList -------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glNewList.xml OpenGL 2.x>.
glNewList
  :: MonadIO m
  => GLuint -- ^ @list@ of type @List@.
  -> GLenum -- ^ @mode@ of type [ListMode](Graphics-GL-Groups.html#ListMode).
  -> m ()
glNewList v1 v2 = liftIO $ dyn15 ptr_glNewList v1 v2

{-# NOINLINE ptr_glNewList #-}
ptr_glNewList :: FunPtr (GLuint -> GLenum -> IO ())
ptr_glNewList = unsafePerformIO $ getCommand "glNewList"

-- glNewObjectBufferATI --------------------------------------------------------

glNewObjectBufferATI
  :: MonadIO m
  => GLsizei -- ^ @size@.
  -> Ptr a -- ^ @pointer@ pointing to @size@ elements of type @a@.
  -> GLenum -- ^ @usage@ of type @ArrayObjectUsageATI@.
  -> m GLuint
glNewObjectBufferATI v1 v2 v3 = liftIO $ dyn593 ptr_glNewObjectBufferATI v1 v2 v3

{-# NOINLINE ptr_glNewObjectBufferATI #-}
ptr_glNewObjectBufferATI :: FunPtr (GLsizei -> Ptr a -> GLenum -> IO GLuint)
ptr_glNewObjectBufferATI = unsafePerformIO $ getCommand "glNewObjectBufferATI"

-- glNormal3b ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glNormal.xml OpenGL 2.x>. The vector equivalent of this command is 'glNormal3bv'.
glNormal3b
  :: MonadIO m
  => GLbyte -- ^ @nx@.
  -> GLbyte -- ^ @ny@.
  -> GLbyte -- ^ @nz@.
  -> m ()
glNormal3b v1 v2 v3 = liftIO $ dyn36 ptr_glNormal3b v1 v2 v3

{-# NOINLINE ptr_glNormal3b #-}
ptr_glNormal3b :: FunPtr (GLbyte -> GLbyte -> GLbyte -> IO ())
ptr_glNormal3b = unsafePerformIO $ getCommand "glNormal3b"

-- glNormal3bv -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glNormal.xml OpenGL 2.x>.
glNormal3bv
  :: MonadIO m
  => Ptr GLbyte -- ^ @v@ pointing to @3@ elements of type @GLbyte@.
  -> m ()
glNormal3bv v1 = liftIO $ dyn37 ptr_glNormal3bv v1

{-# NOINLINE ptr_glNormal3bv #-}
ptr_glNormal3bv :: FunPtr (Ptr GLbyte -> IO ())
ptr_glNormal3bv = unsafePerformIO $ getCommand "glNormal3bv"

-- glNormal3d ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glNormal.xml OpenGL 2.x>. The vector equivalent of this command is 'glNormal3dv'.
glNormal3d
  :: MonadIO m
  => GLdouble -- ^ @nx@ of type @CoordD@.
  -> GLdouble -- ^ @ny@ of type @CoordD@.
  -> GLdouble -- ^ @nz@ of type @CoordD@.
  -> m ()
glNormal3d v1 v2 v3 = liftIO $ dyn38 ptr_glNormal3d v1 v2 v3

{-# NOINLINE ptr_glNormal3d #-}
ptr_glNormal3d :: FunPtr (GLdouble -> GLdouble -> GLdouble -> IO ())
ptr_glNormal3d = unsafePerformIO $ getCommand "glNormal3d"

-- glNormal3dv -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glNormal.xml OpenGL 2.x>.
glNormal3dv
  :: MonadIO m
  => Ptr GLdouble -- ^ @v@ pointing to @3@ elements of type @CoordD@.
  -> m ()
glNormal3dv v1 = liftIO $ dyn39 ptr_glNormal3dv v1

{-# NOINLINE ptr_glNormal3dv #-}
ptr_glNormal3dv :: FunPtr (Ptr GLdouble -> IO ())
ptr_glNormal3dv = unsafePerformIO $ getCommand "glNormal3dv"

-- glNormal3f ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glNormal.xml OpenGL 2.x>. The vector equivalent of this command is 'glNormal3fv'.
glNormal3f
  :: MonadIO m
  => GLfloat -- ^ @nx@ of type @CoordF@.
  -> GLfloat -- ^ @ny@ of type @CoordF@.
  -> GLfloat -- ^ @nz@ of type @CoordF@.
  -> m ()
glNormal3f v1 v2 v3 = liftIO $ dyn40 ptr_glNormal3f v1 v2 v3

{-# NOINLINE ptr_glNormal3f #-}
ptr_glNormal3f :: FunPtr (GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glNormal3f = unsafePerformIO $ getCommand "glNormal3f"

-- glNormal3fVertex3fSUN -------------------------------------------------------

glNormal3fVertex3fSUN
  :: MonadIO m
  => GLfloat -- ^ @nx@.
  -> GLfloat -- ^ @ny@.
  -> GLfloat -- ^ @nz@.
  -> GLfloat -- ^ @x@.
  -> GLfloat -- ^ @y@.
  -> GLfloat -- ^ @z@.
  -> m ()
glNormal3fVertex3fSUN v1 v2 v3 v4 v5 v6 = liftIO $ dyn96 ptr_glNormal3fVertex3fSUN v1 v2 v3 v4 v5 v6

{-# NOINLINE ptr_glNormal3fVertex3fSUN #-}
ptr_glNormal3fVertex3fSUN :: FunPtr (GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ())
ptr_glNormal3fVertex3fSUN = unsafePerformIO $ getCommand "glNormal3fVertex3fSUN"

-- glNormal3fVertex3fvSUN ------------------------------------------------------

glNormal3fVertex3fvSUN
  :: MonadIO m
  => Ptr GLfloat -- ^ @n@ pointing to @3@ elements of type @GLfloat@.
  -> Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @GLfloat@.
  -> m ()
glNormal3fVertex3fvSUN v1 v2 = liftIO $ dyn97 ptr_glNormal3fVertex3fvSUN v1 v2

{-# NOINLINE ptr_glNormal3fVertex3fvSUN #-}
ptr_glNormal3fVertex3fvSUN :: FunPtr (Ptr GLfloat -> Ptr GLfloat -> IO ())
ptr_glNormal3fVertex3fvSUN = unsafePerformIO $ getCommand "glNormal3fVertex3fvSUN"

-- glNormal3fv -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glNormal.xml OpenGL 2.x>.
glNormal3fv
  :: MonadIO m
  => Ptr GLfloat -- ^ @v@ pointing to @3@ elements of type @CoordF@.
  -> m ()
glNormal3fv v1 = liftIO $ dyn41 ptr_glNormal3fv v1

{-# NOINLINE ptr_glNormal3fv #-}
ptr_glNormal3fv :: FunPtr (Ptr GLfloat -> IO ())
ptr_glNormal3fv = unsafePerformIO $ getCommand "glNormal3fv"

-- glNormal3hNV ----------------------------------------------------------------

-- | The vector equivalent of this command is 'glNormal3hvNV'.
glNormal3hNV
  :: MonadIO m
  => GLhalfNV -- ^ @nx@ of type @Half16NV@.
  -> GLhalfNV -- ^ @ny@ of type @Half16NV@.
  -> GLhalfNV -- ^ @nz@ of type @Half16NV@.
  -> m ()
glNormal3hNV v1 v2 v3 = liftIO $ dyn98 ptr_glNormal3hNV v1 v2 v3

{-# NOINLINE ptr_glNormal3hNV #-}
ptr_glNormal3hNV :: FunPtr (GLhalfNV -> GLhalfNV -> GLhalfNV -> IO ())
ptr_glNormal3hNV = unsafePerformIO $ getCommand "glNormal3hNV"

-- glNormal3hvNV ---------------------------------------------------------------

glNormal3hvNV
  :: MonadIO m
  => Ptr GLhalfNV -- ^ @v@ pointing to @3@ elements of type @Half16NV@.
  -> m ()
glNormal3hvNV v1 = liftIO $ dyn99 ptr_glNormal3hvNV v1

{-# NOINLINE ptr_glNormal3hvNV #-}
ptr_glNormal3hvNV :: FunPtr (Ptr GLhalfNV -> IO ())
ptr_glNormal3hvNV = unsafePerformIO $ getCommand "glNormal3hvNV"

-- glNormal3i ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glNormal.xml OpenGL 2.x>. The vector equivalent of this command is 'glNormal3iv'.
glNormal3i
  :: MonadIO m
  => GLint -- ^ @nx@.
  -> GLint -- ^ @ny@.
  -> GLint -- ^ @nz@.
  -> m ()
glNormal3i v1 v2 v3 = liftIO $ dyn42 ptr_glNormal3i v1 v2 v3

{-# NOINLINE ptr_glNormal3i #-}
ptr_glNormal3i :: FunPtr (GLint -> GLint -> GLint -> IO ())
ptr_glNormal3i = unsafePerformIO $ getCommand "glNormal3i"

-- glNormal3iv -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glNormal.xml OpenGL 2.x>.
glNormal3iv
  :: MonadIO m
  => Ptr GLint -- ^ @v@ pointing to @3@ elements of type @GLint@.
  -> m ()
glNormal3iv v1 = liftIO $ dyn43 ptr_glNormal3iv v1

{-# NOINLINE ptr_glNormal3iv #-}
ptr_glNormal3iv :: FunPtr (Ptr GLint -> IO ())
ptr_glNormal3iv = unsafePerformIO $ getCommand "glNormal3iv"

-- glNormal3s ------------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glNormal.xml OpenGL 2.x>. The vector equivalent of this command is 'glNormal3sv'.
glNormal3s
  :: MonadIO m
  => GLshort -- ^ @nx@.
  -> GLshort -- ^ @ny@.
  -> GLshort -- ^ @nz@.
  -> m ()
glNormal3s v1 v2 v3 = liftIO $ dyn44 ptr_glNormal3s v1 v2 v3

{-# NOINLINE ptr_glNormal3s #-}
ptr_glNormal3s :: FunPtr (GLshort -> GLshort -> GLshort -> IO ())
ptr_glNormal3s = unsafePerformIO $ getCommand "glNormal3s"

-- glNormal3sv -----------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glNormal.xml OpenGL 2.x>.
glNormal3sv
  :: MonadIO m
  => Ptr GLshort -- ^ @v@ pointing to @3@ elements of type @GLshort@.
  -> m ()
glNormal3sv v1 = liftIO $ dyn45 ptr_glNormal3sv v1

{-# NOINLINE ptr_glNormal3sv #-}
ptr_glNormal3sv :: FunPtr (Ptr GLshort -> IO ())
ptr_glNormal3sv = unsafePerformIO $ getCommand "glNormal3sv"

-- glNormal3x ------------------------------------------------------------------

glNormal3x
  :: MonadIO m
  => GLfixed -- ^ @nx@.
  -> GLfixed -- ^ @ny@.
  -> GLfixed -- ^ @nz@.
  -> m ()
glNormal3x v1 v2 v3 = liftIO $ dyn106 ptr_glNormal3x v1 v2 v3

{-# NOINLINE ptr_glNormal3x #-}
ptr_glNormal3x :: FunPtr (GLfixed -> GLfixed -> GLfixed -> IO ())
ptr_glNormal3x = unsafePerformIO $ getCommand "glNormal3x"

-- glNormal3xOES ---------------------------------------------------------------

glNormal3xOES
  :: MonadIO m
  => GLfixed -- ^ @nx@.
  -> GLfixed -- ^ @ny@.
  -> GLfixed -- ^ @nz@.
  -> m ()
glNormal3xOES v1 v2 v3 = liftIO $ dyn106 ptr_glNormal3xOES v1 v2 v3

{-# NOINLINE ptr_glNormal3xOES #-}
ptr_glNormal3xOES :: FunPtr (GLfixed -> GLfixed -> GLfixed -> IO ())
ptr_glNormal3xOES = unsafePerformIO $ getCommand "glNormal3xOES"

-- glNormal3xvOES --------------------------------------------------------------

glNormal3xvOES
  :: MonadIO m
  => Ptr GLfixed -- ^ @coords@ pointing to @3@ elements of type @GLfixed@.
  -> m ()
glNormal3xvOES v1 = liftIO $ dyn107 ptr_glNormal3xvOES v1

{-# NOINLINE ptr_glNormal3xvOES #-}
ptr_glNormal3xvOES :: FunPtr (Ptr GLfixed -> IO ())
ptr_glNormal3xvOES = unsafePerformIO $ getCommand "glNormal3xvOES"

-- glNormalFormatNV ------------------------------------------------------------

glNormalFormatNV
  :: MonadIO m
  => GLenum -- ^ @type@.
  -> GLsizei -- ^ @stride@.
  -> m ()
glNormalFormatNV v1 v2 = liftIO $ dyn239 ptr_glNormalFormatNV v1 v2

{-# NOINLINE ptr_glNormalFormatNV #-}
ptr_glNormalFormatNV :: FunPtr (GLenum -> GLsizei -> IO ())
ptr_glNormalFormatNV = unsafePerformIO $ getCommand "glNormalFormatNV"

-- glNormalP3ui ----------------------------------------------------------------

glNormalP3ui
  :: MonadIO m
  => GLenum -- ^ @type@.
  -> GLuint -- ^ @coords@.
  -> m ()
glNormalP3ui v1 v2 = liftIO $ dyn16 ptr_glNormalP3ui v1 v2

{-# NOINLINE ptr_glNormalP3ui #-}
ptr_glNormalP3ui :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glNormalP3ui = unsafePerformIO $ getCommand "glNormalP3ui"

-- glNormalP3uiv ---------------------------------------------------------------

glNormalP3uiv
  :: MonadIO m
  => GLenum -- ^ @type@.
  -> Ptr GLuint -- ^ @coords@ pointing to @1@ element of type @GLuint@.
  -> m ()
glNormalP3uiv v1 v2 = liftIO $ dyn125 ptr_glNormalP3uiv v1 v2

{-# NOINLINE ptr_glNormalP3uiv #-}
ptr_glNormalP3uiv :: FunPtr (GLenum -> Ptr GLuint -> IO ())
ptr_glNormalP3uiv = unsafePerformIO $ getCommand "glNormalP3uiv"

-- glNormalPointer -------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glNormalPointer.xml OpenGL 2.x>.
glNormalPointer
  :: MonadIO m
  => GLenum -- ^ @type@ of type [NormalPointerType](Graphics-GL-Groups.html#NormalPointerType).
  -> GLsizei -- ^ @stride@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(type,stride)@ elements of type @a@.
  -> m ()
glNormalPointer v1 v2 v3 = liftIO $ dyn46 ptr_glNormalPointer v1 v2 v3

{-# NOINLINE ptr_glNormalPointer #-}
ptr_glNormalPointer :: FunPtr (GLenum -> GLsizei -> Ptr a -> IO ())
ptr_glNormalPointer = unsafePerformIO $ getCommand "glNormalPointer"

-- glNormalPointerEXT ----------------------------------------------------------

glNormalPointerEXT
  :: MonadIO m
  => GLenum -- ^ @type@ of type [NormalPointerType](Graphics-GL-Groups.html#NormalPointerType).
  -> GLsizei -- ^ @stride@.
  -> GLsizei -- ^ @count@.
  -> Ptr a -- ^ @pointer@ pointing to @COMPSIZE(type,stride,count)@ elements of type @a@.
  -> m ()
glNormalPointerEXT v1 v2 v3 v4 = liftIO $ dyn468 ptr_glNormalPointerEXT v1 v2 v3 v4

{-# NOINLINE ptr_glNormalPointerEXT #-}
ptr_glNormalPointerEXT :: FunPtr (GLenum -> GLsizei -> GLsizei -> Ptr a -> IO ())
ptr_glNormalPointerEXT = unsafePerformIO $ getCommand "glNormalPointerEXT"

-- glNormalPointerListIBM ------------------------------------------------------

glNormalPointerListIBM
  :: MonadIO m
  => GLenum -- ^ @type@ of type [NormalPointerType](Graphics-GL-Groups.html#NormalPointerType).
  -> GLint -- ^ @stride@.
  -> Ptr (Ptr a) -- ^ @pointer@ pointing to @COMPSIZE(type,stride)@ elements of type @Ptr a@.
  -> GLint -- ^ @ptrstride@.
  -> m ()
glNormalPointerListIBM v1 v2 v3 v4 = liftIO $ dyn280 ptr_glNormalPointerListIBM v1 v2 v3 v4

{-# NOINLINE ptr_glNormalPointerListIBM #-}
ptr_glNormalPointerListIBM :: FunPtr (GLenum -> GLint -> Ptr (Ptr a) -> GLint -> IO ())
ptr_glNormalPointerListIBM = unsafePerformIO $ getCommand "glNormalPointerListIBM"

-- glNormalPointervINTEL -------------------------------------------------------

glNormalPointervINTEL
  :: MonadIO m
  => GLenum -- ^ @type@ of type [NormalPointerType](Graphics-GL-Groups.html#NormalPointerType).
  -> Ptr (Ptr a) -- ^ @pointer@ pointing to @4@ elements of type @Ptr a@.
  -> m ()
glNormalPointervINTEL v1 v2 = liftIO $ dyn268 ptr_glNormalPointervINTEL v1 v2

{-# NOINLINE ptr_glNormalPointervINTEL #-}
ptr_glNormalPointervINTEL :: FunPtr (GLenum -> Ptr (Ptr a) -> IO ())
ptr_glNormalPointervINTEL = unsafePerformIO $ getCommand "glNormalPointervINTEL"

