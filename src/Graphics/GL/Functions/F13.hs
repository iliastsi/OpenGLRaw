--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F13
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

module Graphics.GL.Functions.F13 (
  glDeleteVertexArrays,
  glDeleteVertexArraysAPPLE,
  glDeleteVertexArraysOES,
  glDeleteVertexShaderEXT,
  glDepthBoundsEXT,
  glDepthBoundsdNV,
  glDepthFunc,
  glDepthMask,
  glDepthRange,
  glDepthRangeArrayfvNV,
  glDepthRangeArrayv,
  glDepthRangeIndexed,
  glDepthRangeIndexedfNV,
  glDepthRangedNV,
  glDepthRangef,
  glDepthRangefOES,
  glDepthRangex,
  glDepthRangexOES,
  glDetachObjectARB,
  glDetachShader,
  glDetailTexFuncSGIS,
  glDisable,
  glDisableClientState,
  glDisableClientStateIndexedEXT,
  glDisableClientStateiEXT,
  glDisableDriverControlQCOM,
  glDisableIndexedEXT,
  glDisableVariantClientStateEXT,
  glDisableVertexArrayAttrib,
  glDisableVertexArrayAttribEXT,
  glDisableVertexArrayEXT,
  glDisableVertexAttribAPPLE,
  glDisableVertexAttribArray,
  glDisableVertexAttribArrayARB,
  glDisablei,
  glDisableiEXT,
  glDisableiNV,
  glDisableiOES,
  glDiscardFramebufferEXT,
  glDispatchCompute
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

-- glDeleteVertexArrays --------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glDeleteVertexArrays.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glDeleteVertexArrays.xhtml OpenGL 4.x>.
glDeleteVertexArrays
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @arrays@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glDeleteVertexArrays v1 v2 = liftIO $ dyn193 ptr_glDeleteVertexArrays v1 v2

{-# NOINLINE ptr_glDeleteVertexArrays #-}
ptr_glDeleteVertexArrays :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteVertexArrays = unsafePerformIO $ getCommand "glDeleteVertexArrays"

-- glDeleteVertexArraysAPPLE ---------------------------------------------------

-- | This command is an alias for 'glDeleteVertexArrays'.
glDeleteVertexArraysAPPLE
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @arrays@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glDeleteVertexArraysAPPLE v1 v2 = liftIO $ dyn193 ptr_glDeleteVertexArraysAPPLE v1 v2

{-# NOINLINE ptr_glDeleteVertexArraysAPPLE #-}
ptr_glDeleteVertexArraysAPPLE :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteVertexArraysAPPLE = unsafePerformIO $ getCommand "glDeleteVertexArraysAPPLE"

-- glDeleteVertexArraysOES -----------------------------------------------------

-- | This command is an alias for 'glDeleteVertexArrays'.
glDeleteVertexArraysOES
  :: MonadIO m
  => GLsizei -- ^ @n@.
  -> Ptr GLuint -- ^ @arrays@ pointing to @n@ elements of type @GLuint@.
  -> m ()
glDeleteVertexArraysOES v1 v2 = liftIO $ dyn193 ptr_glDeleteVertexArraysOES v1 v2

{-# NOINLINE ptr_glDeleteVertexArraysOES #-}
ptr_glDeleteVertexArraysOES :: FunPtr (GLsizei -> Ptr GLuint -> IO ())
ptr_glDeleteVertexArraysOES = unsafePerformIO $ getCommand "glDeleteVertexArraysOES"

-- glDeleteVertexShaderEXT -----------------------------------------------------

glDeleteVertexShaderEXT
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> m ()
glDeleteVertexShaderEXT v1 = liftIO $ dyn2 ptr_glDeleteVertexShaderEXT v1

{-# NOINLINE ptr_glDeleteVertexShaderEXT #-}
ptr_glDeleteVertexShaderEXT :: FunPtr (GLuint -> IO ())
ptr_glDeleteVertexShaderEXT = unsafePerformIO $ getCommand "glDeleteVertexShaderEXT"

-- glDepthBoundsEXT ------------------------------------------------------------

glDepthBoundsEXT
  :: MonadIO m
  => GLclampd -- ^ @zmin@ of type @ClampedFloat64@.
  -> GLclampd -- ^ @zmax@ of type @ClampedFloat64@.
  -> m ()
glDepthBoundsEXT v1 v2 = liftIO $ dyn216 ptr_glDepthBoundsEXT v1 v2

{-# NOINLINE ptr_glDepthBoundsEXT #-}
ptr_glDepthBoundsEXT :: FunPtr (GLclampd -> GLclampd -> IO ())
ptr_glDepthBoundsEXT = unsafePerformIO $ getCommand "glDepthBoundsEXT"

-- glDepthBoundsdNV ------------------------------------------------------------

glDepthBoundsdNV
  :: MonadIO m
  => GLdouble -- ^ @zmin@.
  -> GLdouble -- ^ @zmax@.
  -> m ()
glDepthBoundsdNV v1 v2 = liftIO $ dyn217 ptr_glDepthBoundsdNV v1 v2

{-# NOINLINE ptr_glDepthBoundsdNV #-}
ptr_glDepthBoundsdNV :: FunPtr (GLdouble -> GLdouble -> IO ())
ptr_glDepthBoundsdNV = unsafePerformIO $ getCommand "glDepthBoundsdNV"

-- glDepthFunc -----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glDepthFunc.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glDepthFunc.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glDepthFunc.xhtml OpenGL 4.x>.
glDepthFunc
  :: MonadIO m
  => GLenum -- ^ @func@ of type [DepthFunction](Graphics-GL-Groups.html#DepthFunction).
  -> m ()
glDepthFunc v1 = liftIO $ dyn4 ptr_glDepthFunc v1

{-# NOINLINE ptr_glDepthFunc #-}
ptr_glDepthFunc :: FunPtr (GLenum -> IO ())
ptr_glDepthFunc = unsafePerformIO $ getCommand "glDepthFunc"

-- glDepthMask -----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glDepthMask.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glDepthMask.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glDepthMask.xhtml OpenGL 4.x>.
glDepthMask
  :: MonadIO m
  => GLboolean -- ^ @flag@ of type [Boolean](Graphics-GL-Groups.html#Boolean).
  -> m ()
glDepthMask v1 = liftIO $ dyn191 ptr_glDepthMask v1

{-# NOINLINE ptr_glDepthMask #-}
ptr_glDepthMask :: FunPtr (GLboolean -> IO ())
ptr_glDepthMask = unsafePerformIO $ getCommand "glDepthMask"

-- glDepthRange ----------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glDepthRange.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glDepthRange.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glDepthRange.xhtml OpenGL 4.x>.
glDepthRange
  :: MonadIO m
  => GLdouble -- ^ @near@.
  -> GLdouble -- ^ @far@.
  -> m ()
glDepthRange v1 v2 = liftIO $ dyn217 ptr_glDepthRange v1 v2

{-# NOINLINE ptr_glDepthRange #-}
ptr_glDepthRange :: FunPtr (GLdouble -> GLdouble -> IO ())
ptr_glDepthRange = unsafePerformIO $ getCommand "glDepthRange"

-- glDepthRangeArrayfvNV -------------------------------------------------------

glDepthRangeArrayfvNV
  :: MonadIO m
  => GLuint -- ^ @first@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLfloat -- ^ @v@.
  -> m ()
glDepthRangeArrayfvNV v1 v2 v3 = liftIO $ dyn218 ptr_glDepthRangeArrayfvNV v1 v2 v3

{-# NOINLINE ptr_glDepthRangeArrayfvNV #-}
ptr_glDepthRangeArrayfvNV :: FunPtr (GLuint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glDepthRangeArrayfvNV = unsafePerformIO $ getCommand "glDepthRangeArrayfvNV"

-- glDepthRangeArrayv ----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glDepthRangeArray.xhtml OpenGL 4.x>.
glDepthRangeArrayv
  :: MonadIO m
  => GLuint -- ^ @first@.
  -> GLsizei -- ^ @count@.
  -> Ptr GLdouble -- ^ @v@ pointing to @COMPSIZE(count)@ elements of type @GLdouble@.
  -> m ()
glDepthRangeArrayv v1 v2 v3 = liftIO $ dyn219 ptr_glDepthRangeArrayv v1 v2 v3

{-# NOINLINE ptr_glDepthRangeArrayv #-}
ptr_glDepthRangeArrayv :: FunPtr (GLuint -> GLsizei -> Ptr GLdouble -> IO ())
ptr_glDepthRangeArrayv = unsafePerformIO $ getCommand "glDepthRangeArrayv"

-- glDepthRangeIndexed ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glDepthRangeIndexed.xhtml OpenGL 4.x>.
glDepthRangeIndexed
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLdouble -- ^ @n@.
  -> GLdouble -- ^ @f@.
  -> m ()
glDepthRangeIndexed v1 v2 v3 = liftIO $ dyn220 ptr_glDepthRangeIndexed v1 v2 v3

{-# NOINLINE ptr_glDepthRangeIndexed #-}
ptr_glDepthRangeIndexed :: FunPtr (GLuint -> GLdouble -> GLdouble -> IO ())
ptr_glDepthRangeIndexed = unsafePerformIO $ getCommand "glDepthRangeIndexed"

-- glDepthRangeIndexedfNV ------------------------------------------------------

glDepthRangeIndexedfNV
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLfloat -- ^ @n@.
  -> GLfloat -- ^ @f@.
  -> m ()
glDepthRangeIndexedfNV v1 v2 v3 = liftIO $ dyn221 ptr_glDepthRangeIndexedfNV v1 v2 v3

{-# NOINLINE ptr_glDepthRangeIndexedfNV #-}
ptr_glDepthRangeIndexedfNV :: FunPtr (GLuint -> GLfloat -> GLfloat -> IO ())
ptr_glDepthRangeIndexedfNV = unsafePerformIO $ getCommand "glDepthRangeIndexedfNV"

-- glDepthRangedNV -------------------------------------------------------------

glDepthRangedNV
  :: MonadIO m
  => GLdouble -- ^ @zNear@.
  -> GLdouble -- ^ @zFar@.
  -> m ()
glDepthRangedNV v1 v2 = liftIO $ dyn217 ptr_glDepthRangedNV v1 v2

{-# NOINLINE ptr_glDepthRangedNV #-}
ptr_glDepthRangedNV :: FunPtr (GLdouble -> GLdouble -> IO ())
ptr_glDepthRangedNV = unsafePerformIO $ getCommand "glDepthRangedNV"

-- glDepthRangef ---------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glDepthRange.xhtml OpenGL 4.x>.
glDepthRangef
  :: MonadIO m
  => GLfloat -- ^ @n@.
  -> GLfloat -- ^ @f@.
  -> m ()
glDepthRangef v1 v2 = liftIO $ dyn222 ptr_glDepthRangef v1 v2

{-# NOINLINE ptr_glDepthRangef #-}
ptr_glDepthRangef :: FunPtr (GLfloat -> GLfloat -> IO ())
ptr_glDepthRangef = unsafePerformIO $ getCommand "glDepthRangef"

-- glDepthRangefOES ------------------------------------------------------------

-- | This command is an alias for 'glDepthRangef'.
glDepthRangefOES
  :: MonadIO m
  => GLclampf -- ^ @n@ of type @ClampedFloat32@.
  -> GLclampf -- ^ @f@ of type @ClampedFloat32@.
  -> m ()
glDepthRangefOES v1 v2 = liftIO $ dyn223 ptr_glDepthRangefOES v1 v2

{-# NOINLINE ptr_glDepthRangefOES #-}
ptr_glDepthRangefOES :: FunPtr (GLclampf -> GLclampf -> IO ())
ptr_glDepthRangefOES = unsafePerformIO $ getCommand "glDepthRangefOES"

-- glDepthRangex ---------------------------------------------------------------

glDepthRangex
  :: MonadIO m
  => GLfixed -- ^ @n@.
  -> GLfixed -- ^ @f@.
  -> m ()
glDepthRangex v1 v2 = liftIO $ dyn224 ptr_glDepthRangex v1 v2

{-# NOINLINE ptr_glDepthRangex #-}
ptr_glDepthRangex :: FunPtr (GLfixed -> GLfixed -> IO ())
ptr_glDepthRangex = unsafePerformIO $ getCommand "glDepthRangex"

-- glDepthRangexOES ------------------------------------------------------------

glDepthRangexOES
  :: MonadIO m
  => GLfixed -- ^ @n@ of type @ClampedFixed@.
  -> GLfixed -- ^ @f@ of type @ClampedFixed@.
  -> m ()
glDepthRangexOES v1 v2 = liftIO $ dyn224 ptr_glDepthRangexOES v1 v2

{-# NOINLINE ptr_glDepthRangexOES #-}
ptr_glDepthRangexOES :: FunPtr (GLfixed -> GLfixed -> IO ())
ptr_glDepthRangexOES = unsafePerformIO $ getCommand "glDepthRangexOES"

-- glDetachObjectARB -----------------------------------------------------------

-- | This command is an alias for 'glDetachShader'.
glDetachObjectARB
  :: MonadIO m
  => GLhandleARB -- ^ @containerObj@ of type @handleARB@.
  -> GLhandleARB -- ^ @attachedObj@ of type @handleARB@.
  -> m ()
glDetachObjectARB v1 v2 = liftIO $ dyn14 ptr_glDetachObjectARB v1 v2

{-# NOINLINE ptr_glDetachObjectARB #-}
ptr_glDetachObjectARB :: FunPtr (GLhandleARB -> GLhandleARB -> IO ())
ptr_glDetachObjectARB = unsafePerformIO $ getCommand "glDetachObjectARB"

-- glDetachShader --------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glDetachShader.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glDetachShader.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glDetachShader.xhtml OpenGL 4.x>.
glDetachShader
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLuint -- ^ @shader@.
  -> m ()
glDetachShader v1 v2 = liftIO $ dyn3 ptr_glDetachShader v1 v2

{-# NOINLINE ptr_glDetachShader #-}
ptr_glDetachShader :: FunPtr (GLuint -> GLuint -> IO ())
ptr_glDetachShader = unsafePerformIO $ getCommand "glDetachShader"

-- glDetailTexFuncSGIS ---------------------------------------------------------

glDetailTexFuncSGIS
  :: MonadIO m
  => GLenum -- ^ @target@ of type [TextureTarget](Graphics-GL-Groups.html#TextureTarget).
  -> GLsizei -- ^ @n@.
  -> Ptr GLfloat -- ^ @points@ pointing to @n*2@ elements of type @GLfloat@.
  -> m ()
glDetailTexFuncSGIS v1 v2 v3 = liftIO $ dyn225 ptr_glDetailTexFuncSGIS v1 v2 v3

{-# NOINLINE ptr_glDetailTexFuncSGIS #-}
ptr_glDetailTexFuncSGIS :: FunPtr (GLenum -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glDetailTexFuncSGIS = unsafePerformIO $ getCommand "glDetailTexFuncSGIS"

-- glDisable -------------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glEnable.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glEnable.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glEnable.xhtml OpenGL 4.x>.
glDisable
  :: MonadIO m
  => GLenum -- ^ @cap@ of type [EnableCap](Graphics-GL-Groups.html#EnableCap).
  -> m ()
glDisable v1 = liftIO $ dyn4 ptr_glDisable v1

{-# NOINLINE ptr_glDisable #-}
ptr_glDisable :: FunPtr (GLenum -> IO ())
ptr_glDisable = unsafePerformIO $ getCommand "glDisable"

-- glDisableClientState --------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man2/xhtml/glEnableClientState.xml OpenGL 2.x>.
glDisableClientState
  :: MonadIO m
  => GLenum -- ^ @array@ of type [EnableCap](Graphics-GL-Groups.html#EnableCap).
  -> m ()
glDisableClientState v1 = liftIO $ dyn4 ptr_glDisableClientState v1

{-# NOINLINE ptr_glDisableClientState #-}
ptr_glDisableClientState :: FunPtr (GLenum -> IO ())
ptr_glDisableClientState = unsafePerformIO $ getCommand "glDisableClientState"

-- glDisableClientStateIndexedEXT ----------------------------------------------

glDisableClientStateIndexedEXT
  :: MonadIO m
  => GLenum -- ^ @array@ of type [EnableCap](Graphics-GL-Groups.html#EnableCap).
  -> GLuint -- ^ @index@.
  -> m ()
glDisableClientStateIndexedEXT v1 v2 = liftIO $ dyn16 ptr_glDisableClientStateIndexedEXT v1 v2

{-# NOINLINE ptr_glDisableClientStateIndexedEXT #-}
ptr_glDisableClientStateIndexedEXT :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glDisableClientStateIndexedEXT = unsafePerformIO $ getCommand "glDisableClientStateIndexedEXT"

-- glDisableClientStateiEXT ----------------------------------------------------

glDisableClientStateiEXT
  :: MonadIO m
  => GLenum -- ^ @array@ of type [EnableCap](Graphics-GL-Groups.html#EnableCap).
  -> GLuint -- ^ @index@.
  -> m ()
glDisableClientStateiEXT v1 v2 = liftIO $ dyn16 ptr_glDisableClientStateiEXT v1 v2

{-# NOINLINE ptr_glDisableClientStateiEXT #-}
ptr_glDisableClientStateiEXT :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glDisableClientStateiEXT = unsafePerformIO $ getCommand "glDisableClientStateiEXT"

-- glDisableDriverControlQCOM --------------------------------------------------

glDisableDriverControlQCOM
  :: MonadIO m
  => GLuint -- ^ @driverControl@.
  -> m ()
glDisableDriverControlQCOM v1 = liftIO $ dyn2 ptr_glDisableDriverControlQCOM v1

{-# NOINLINE ptr_glDisableDriverControlQCOM #-}
ptr_glDisableDriverControlQCOM :: FunPtr (GLuint -> IO ())
ptr_glDisableDriverControlQCOM = unsafePerformIO $ getCommand "glDisableDriverControlQCOM"

-- glDisableIndexedEXT ---------------------------------------------------------

-- | This command is an alias for 'glDisablei'.
glDisableIndexedEXT
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLuint -- ^ @index@.
  -> m ()
glDisableIndexedEXT v1 v2 = liftIO $ dyn16 ptr_glDisableIndexedEXT v1 v2

{-# NOINLINE ptr_glDisableIndexedEXT #-}
ptr_glDisableIndexedEXT :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glDisableIndexedEXT = unsafePerformIO $ getCommand "glDisableIndexedEXT"

-- glDisableVariantClientStateEXT ----------------------------------------------

glDisableVariantClientStateEXT
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> m ()
glDisableVariantClientStateEXT v1 = liftIO $ dyn2 ptr_glDisableVariantClientStateEXT v1

{-# NOINLINE ptr_glDisableVariantClientStateEXT #-}
ptr_glDisableVariantClientStateEXT :: FunPtr (GLuint -> IO ())
ptr_glDisableVariantClientStateEXT = unsafePerformIO $ getCommand "glDisableVariantClientStateEXT"

-- glDisableVertexArrayAttrib --------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glEnableVertexAttribArray.xhtml OpenGL 4.x>.
glDisableVertexArrayAttrib
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @index@.
  -> m ()
glDisableVertexArrayAttrib v1 v2 = liftIO $ dyn3 ptr_glDisableVertexArrayAttrib v1 v2

{-# NOINLINE ptr_glDisableVertexArrayAttrib #-}
ptr_glDisableVertexArrayAttrib :: FunPtr (GLuint -> GLuint -> IO ())
ptr_glDisableVertexArrayAttrib = unsafePerformIO $ getCommand "glDisableVertexArrayAttrib"

-- glDisableVertexArrayAttribEXT -----------------------------------------------

glDisableVertexArrayAttribEXT
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLuint -- ^ @index@.
  -> m ()
glDisableVertexArrayAttribEXT v1 v2 = liftIO $ dyn3 ptr_glDisableVertexArrayAttribEXT v1 v2

{-# NOINLINE ptr_glDisableVertexArrayAttribEXT #-}
ptr_glDisableVertexArrayAttribEXT :: FunPtr (GLuint -> GLuint -> IO ())
ptr_glDisableVertexArrayAttribEXT = unsafePerformIO $ getCommand "glDisableVertexArrayAttribEXT"

-- glDisableVertexArrayEXT -----------------------------------------------------

glDisableVertexArrayEXT
  :: MonadIO m
  => GLuint -- ^ @vaobj@.
  -> GLenum -- ^ @array@ of type [EnableCap](Graphics-GL-Groups.html#EnableCap).
  -> m ()
glDisableVertexArrayEXT v1 v2 = liftIO $ dyn15 ptr_glDisableVertexArrayEXT v1 v2

{-# NOINLINE ptr_glDisableVertexArrayEXT #-}
ptr_glDisableVertexArrayEXT :: FunPtr (GLuint -> GLenum -> IO ())
ptr_glDisableVertexArrayEXT = unsafePerformIO $ getCommand "glDisableVertexArrayEXT"

-- glDisableVertexAttribAPPLE --------------------------------------------------

glDisableVertexAttribAPPLE
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> GLenum -- ^ @pname@.
  -> m ()
glDisableVertexAttribAPPLE v1 v2 = liftIO $ dyn15 ptr_glDisableVertexAttribAPPLE v1 v2

{-# NOINLINE ptr_glDisableVertexAttribAPPLE #-}
ptr_glDisableVertexAttribAPPLE :: FunPtr (GLuint -> GLenum -> IO ())
ptr_glDisableVertexAttribAPPLE = unsafePerformIO $ getCommand "glDisableVertexAttribAPPLE"

-- glDisableVertexAttribArray --------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man2/xhtml/glEnableVertexAttribArray.xml OpenGL 2.x> or <https://www.opengl.org/sdk/docs/man3/xhtml/glEnableVertexAttribArray.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glEnableVertexAttribArray.xhtml OpenGL 4.x>.
glDisableVertexAttribArray
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> m ()
glDisableVertexAttribArray v1 = liftIO $ dyn2 ptr_glDisableVertexAttribArray v1

{-# NOINLINE ptr_glDisableVertexAttribArray #-}
ptr_glDisableVertexAttribArray :: FunPtr (GLuint -> IO ())
ptr_glDisableVertexAttribArray = unsafePerformIO $ getCommand "glDisableVertexAttribArray"

-- glDisableVertexAttribArrayARB -----------------------------------------------

-- | This command is an alias for 'glDisableVertexAttribArray'.
glDisableVertexAttribArrayARB
  :: MonadIO m
  => GLuint -- ^ @index@.
  -> m ()
glDisableVertexAttribArrayARB v1 = liftIO $ dyn2 ptr_glDisableVertexAttribArrayARB v1

{-# NOINLINE ptr_glDisableVertexAttribArrayARB #-}
ptr_glDisableVertexAttribArrayARB :: FunPtr (GLuint -> IO ())
ptr_glDisableVertexAttribArrayARB = unsafePerformIO $ getCommand "glDisableVertexAttribArrayARB"

-- glDisablei ------------------------------------------------------------------

-- | Manual pages for <https://www.opengl.org/sdk/docs/man3/xhtml/glEnable.xml OpenGL 3.x> or <https://www.opengl.org/sdk/docs/man4/html/glEnable.xhtml OpenGL 4.x>.
glDisablei
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLuint -- ^ @index@.
  -> m ()
glDisablei v1 v2 = liftIO $ dyn16 ptr_glDisablei v1 v2

{-# NOINLINE ptr_glDisablei #-}
ptr_glDisablei :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glDisablei = unsafePerformIO $ getCommand "glDisablei"

-- glDisableiEXT ---------------------------------------------------------------

-- | This command is an alias for 'glDisablei'.
glDisableiEXT
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLuint -- ^ @index@.
  -> m ()
glDisableiEXT v1 v2 = liftIO $ dyn16 ptr_glDisableiEXT v1 v2

{-# NOINLINE ptr_glDisableiEXT #-}
ptr_glDisableiEXT :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glDisableiEXT = unsafePerformIO $ getCommand "glDisableiEXT"

-- glDisableiNV ----------------------------------------------------------------

-- | This command is an alias for 'glDisablei'.
glDisableiNV
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLuint -- ^ @index@.
  -> m ()
glDisableiNV v1 v2 = liftIO $ dyn16 ptr_glDisableiNV v1 v2

{-# NOINLINE ptr_glDisableiNV #-}
ptr_glDisableiNV :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glDisableiNV = unsafePerformIO $ getCommand "glDisableiNV"

-- glDisableiOES ---------------------------------------------------------------

-- | This command is an alias for 'glDisablei'.
glDisableiOES
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLuint -- ^ @index@.
  -> m ()
glDisableiOES v1 v2 = liftIO $ dyn16 ptr_glDisableiOES v1 v2

{-# NOINLINE ptr_glDisableiOES #-}
ptr_glDisableiOES :: FunPtr (GLenum -> GLuint -> IO ())
ptr_glDisableiOES = unsafePerformIO $ getCommand "glDisableiOES"

-- glDiscardFramebufferEXT -----------------------------------------------------

glDiscardFramebufferEXT
  :: MonadIO m
  => GLenum -- ^ @target@.
  -> GLsizei -- ^ @numAttachments@.
  -> Ptr GLenum -- ^ @attachments@ pointing to @numAttachments@ elements of type @GLenum@.
  -> m ()
glDiscardFramebufferEXT v1 v2 v3 = liftIO $ dyn226 ptr_glDiscardFramebufferEXT v1 v2 v3

{-# NOINLINE ptr_glDiscardFramebufferEXT #-}
ptr_glDiscardFramebufferEXT :: FunPtr (GLenum -> GLsizei -> Ptr GLenum -> IO ())
ptr_glDiscardFramebufferEXT = unsafePerformIO $ getCommand "glDiscardFramebufferEXT"

-- glDispatchCompute -----------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glDispatchCompute.xhtml OpenGL 4.x>.
glDispatchCompute
  :: MonadIO m
  => GLuint -- ^ @num_groups_x@.
  -> GLuint -- ^ @num_groups_y@.
  -> GLuint -- ^ @num_groups_z@.
  -> m ()
glDispatchCompute v1 v2 v3 = liftIO $ dyn102 ptr_glDispatchCompute v1 v2 v3

{-# NOINLINE ptr_glDispatchCompute #-}
ptr_glDispatchCompute :: FunPtr (GLuint -> GLuint -> GLuint -> IO ())
ptr_glDispatchCompute = unsafePerformIO $ getCommand "glDispatchCompute"

