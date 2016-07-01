--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.GL.Functions.F26
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

module Graphics.GL.Functions.F26 (
  glGetNamedProgramStringEXT,
  glGetNamedProgramivEXT,
  glGetNamedRenderbufferParameteriv,
  glGetNamedRenderbufferParameterivEXT,
  glGetNamedStringARB,
  glGetNamedStringivARB,
  glGetNextPerfQueryIdINTEL,
  glGetObjectBufferfvATI,
  glGetObjectBufferivATI,
  glGetObjectLabel,
  glGetObjectLabelEXT,
  glGetObjectLabelKHR,
  glGetObjectParameterfvARB,
  glGetObjectParameterivAPPLE,
  glGetObjectParameterivARB,
  glGetObjectPtrLabel,
  glGetObjectPtrLabelKHR,
  glGetOcclusionQueryivNV,
  glGetOcclusionQueryuivNV,
  glGetPathColorGenfvNV,
  glGetPathColorGenivNV,
  glGetPathCommandsNV,
  glGetPathCoordsNV,
  glGetPathDashArrayNV,
  glGetPathLengthNV,
  glGetPathMetricRangeNV,
  glGetPathMetricsNV,
  glGetPathParameterfvNV,
  glGetPathParameterivNV,
  glGetPathSpacingNV,
  glGetPathTexGenfvNV,
  glGetPathTexGenivNV,
  glGetPerfCounterInfoINTEL,
  glGetPerfMonitorCounterDataAMD,
  glGetPerfMonitorCounterInfoAMD,
  glGetPerfMonitorCounterStringAMD,
  glGetPerfMonitorCountersAMD,
  glGetPerfMonitorGroupStringAMD,
  glGetPerfMonitorGroupsAMD,
  glGetPerfQueryDataINTEL
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

-- glGetNamedProgramStringEXT --------------------------------------------------

glGetNamedProgramStringEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @target@ of type @ProgramTarget@.
  -> GLenum -- ^ @pname@ of type @ProgramStringProperty@.
  -> Ptr a -- ^ @string@ pointing to @COMPSIZE(program,pname)@ elements of type @a@.
  -> m ()
glGetNamedProgramStringEXT v1 v2 v3 v4 = liftIO $ dyn368 ptr_glGetNamedProgramStringEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetNamedProgramStringEXT #-}
ptr_glGetNamedProgramStringEXT :: FunPtr (GLuint -> GLenum -> GLenum -> Ptr a -> IO ())
ptr_glGetNamedProgramStringEXT = unsafePerformIO $ getCommand "glGetNamedProgramStringEXT"

-- glGetNamedProgramivEXT ------------------------------------------------------

glGetNamedProgramivEXT
  :: MonadIO m
  => GLuint -- ^ @program@.
  -> GLenum -- ^ @target@ of type @ProgramTarget@.
  -> GLenum -- ^ @pname@ of type @ProgramProperty@.
  -> Ptr GLint -- ^ @params@ pointing to @1@ element of type @GLint@.
  -> m ()
glGetNamedProgramivEXT v1 v2 v3 v4 = liftIO $ dyn363 ptr_glGetNamedProgramivEXT v1 v2 v3 v4

{-# NOINLINE ptr_glGetNamedProgramivEXT #-}
ptr_glGetNamedProgramivEXT :: FunPtr (GLuint -> GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetNamedProgramivEXT = unsafePerformIO $ getCommand "glGetNamedProgramivEXT"

-- glGetNamedRenderbufferParameteriv -------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetRenderbufferParameter.xhtml OpenGL 4.x>.
glGetNamedRenderbufferParameteriv
  :: MonadIO m
  => GLuint -- ^ @renderbuffer@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @params@.
  -> m ()
glGetNamedRenderbufferParameteriv v1 v2 v3 = liftIO $ dyn334 ptr_glGetNamedRenderbufferParameteriv v1 v2 v3

{-# NOINLINE ptr_glGetNamedRenderbufferParameteriv #-}
ptr_glGetNamedRenderbufferParameteriv :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetNamedRenderbufferParameteriv = unsafePerformIO $ getCommand "glGetNamedRenderbufferParameteriv"

-- glGetNamedRenderbufferParameterivEXT ----------------------------------------

glGetNamedRenderbufferParameterivEXT
  :: MonadIO m
  => GLuint -- ^ @renderbuffer@ of type @Renderbuffer@.
  -> GLenum -- ^ @pname@ of type @RenderbufferParameterName@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetNamedRenderbufferParameterivEXT v1 v2 v3 = liftIO $ dyn334 ptr_glGetNamedRenderbufferParameterivEXT v1 v2 v3

{-# NOINLINE ptr_glGetNamedRenderbufferParameterivEXT #-}
ptr_glGetNamedRenderbufferParameterivEXT :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetNamedRenderbufferParameterivEXT = unsafePerformIO $ getCommand "glGetNamedRenderbufferParameterivEXT"

-- glGetNamedStringARB ---------------------------------------------------------

glGetNamedStringARB
  :: MonadIO m
  => GLint -- ^ @namelen@.
  -> Ptr GLchar -- ^ @name@ pointing to @namelen@ elements of type @GLchar@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLint -- ^ @stringlen@ pointing to @1@ element of type @GLint@.
  -> Ptr GLchar -- ^ @string@ pointing to @bufSize@ elements of type @GLchar@.
  -> m ()
glGetNamedStringARB v1 v2 v3 v4 v5 = liftIO $ dyn369 ptr_glGetNamedStringARB v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetNamedStringARB #-}
ptr_glGetNamedStringARB :: FunPtr (GLint -> Ptr GLchar -> GLsizei -> Ptr GLint -> Ptr GLchar -> IO ())
ptr_glGetNamedStringARB = unsafePerformIO $ getCommand "glGetNamedStringARB"

-- glGetNamedStringivARB -------------------------------------------------------

glGetNamedStringivARB
  :: MonadIO m
  => GLint -- ^ @namelen@.
  -> Ptr GLchar -- ^ @name@ pointing to @namelen@ elements of type @GLchar@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetNamedStringivARB v1 v2 v3 v4 = liftIO $ dyn370 ptr_glGetNamedStringivARB v1 v2 v3 v4

{-# NOINLINE ptr_glGetNamedStringivARB #-}
ptr_glGetNamedStringivARB :: FunPtr (GLint -> Ptr GLchar -> GLenum -> Ptr GLint -> IO ())
ptr_glGetNamedStringivARB = unsafePerformIO $ getCommand "glGetNamedStringivARB"

-- glGetNextPerfQueryIdINTEL ---------------------------------------------------

glGetNextPerfQueryIdINTEL
  :: MonadIO m
  => GLuint -- ^ @queryId@.
  -> Ptr GLuint -- ^ @nextQueryId@.
  -> m ()
glGetNextPerfQueryIdINTEL v1 v2 = liftIO $ dyn194 ptr_glGetNextPerfQueryIdINTEL v1 v2

{-# NOINLINE ptr_glGetNextPerfQueryIdINTEL #-}
ptr_glGetNextPerfQueryIdINTEL :: FunPtr (GLuint -> Ptr GLuint -> IO ())
ptr_glGetNextPerfQueryIdINTEL = unsafePerformIO $ getCommand "glGetNextPerfQueryIdINTEL"

-- glGetObjectBufferfvATI ------------------------------------------------------

glGetObjectBufferfvATI
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLenum -- ^ @pname@ of type @ArrayObjectPNameATI@.
  -> Ptr GLfloat -- ^ @params@ pointing to @1@ element of type @GLfloat@.
  -> m ()
glGetObjectBufferfvATI v1 v2 v3 = liftIO $ dyn349 ptr_glGetObjectBufferfvATI v1 v2 v3

{-# NOINLINE ptr_glGetObjectBufferfvATI #-}
ptr_glGetObjectBufferfvATI :: FunPtr (GLuint -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetObjectBufferfvATI = unsafePerformIO $ getCommand "glGetObjectBufferfvATI"

-- glGetObjectBufferivATI ------------------------------------------------------

glGetObjectBufferivATI
  :: MonadIO m
  => GLuint -- ^ @buffer@.
  -> GLenum -- ^ @pname@ of type @ArrayObjectPNameATI@.
  -> Ptr GLint -- ^ @params@ pointing to @1@ element of type @GLint@.
  -> m ()
glGetObjectBufferivATI v1 v2 v3 = liftIO $ dyn334 ptr_glGetObjectBufferivATI v1 v2 v3

{-# NOINLINE ptr_glGetObjectBufferivATI #-}
ptr_glGetObjectBufferivATI :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetObjectBufferivATI = unsafePerformIO $ getCommand "glGetObjectBufferivATI"

-- glGetObjectLabel ------------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetObjectLabel.xhtml OpenGL 4.x>.
glGetObjectLabel
  :: MonadIO m
  => GLenum -- ^ @identifier@.
  -> GLuint -- ^ @name@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLchar -- ^ @label@ pointing to @bufSize@ elements of type @GLchar@.
  -> m ()
glGetObjectLabel v1 v2 v3 v4 v5 = liftIO $ dyn371 ptr_glGetObjectLabel v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetObjectLabel #-}
ptr_glGetObjectLabel :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
ptr_glGetObjectLabel = unsafePerformIO $ getCommand "glGetObjectLabel"

-- glGetObjectLabelEXT ---------------------------------------------------------

glGetObjectLabelEXT
  :: MonadIO m
  => GLenum -- ^ @type@.
  -> GLuint -- ^ @object@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLchar -- ^ @label@ pointing to @bufSize@ elements of type @GLchar@.
  -> m ()
glGetObjectLabelEXT v1 v2 v3 v4 v5 = liftIO $ dyn371 ptr_glGetObjectLabelEXT v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetObjectLabelEXT #-}
ptr_glGetObjectLabelEXT :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
ptr_glGetObjectLabelEXT = unsafePerformIO $ getCommand "glGetObjectLabelEXT"

-- glGetObjectLabelKHR ---------------------------------------------------------

-- | This command is an alias for 'glGetObjectLabel'.
glGetObjectLabelKHR
  :: MonadIO m
  => GLenum -- ^ @identifier@.
  -> GLuint -- ^ @name@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLsizei -- ^ @length@.
  -> Ptr GLchar -- ^ @label@ pointing to @bufSize@ elements of type @GLchar@.
  -> m ()
glGetObjectLabelKHR v1 v2 v3 v4 v5 = liftIO $ dyn371 ptr_glGetObjectLabelKHR v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetObjectLabelKHR #-}
ptr_glGetObjectLabelKHR :: FunPtr (GLenum -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
ptr_glGetObjectLabelKHR = unsafePerformIO $ getCommand "glGetObjectLabelKHR"

-- glGetObjectParameterfvARB ---------------------------------------------------

glGetObjectParameterfvARB
  :: MonadIO m
  => GLhandleARB -- ^ @obj@ of type @handleARB@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLfloat -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetObjectParameterfvARB v1 v2 v3 = liftIO $ dyn372 ptr_glGetObjectParameterfvARB v1 v2 v3

{-# NOINLINE ptr_glGetObjectParameterfvARB #-}
ptr_glGetObjectParameterfvARB :: FunPtr (GLhandleARB -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetObjectParameterfvARB = unsafePerformIO $ getCommand "glGetObjectParameterfvARB"

-- glGetObjectParameterivAPPLE -------------------------------------------------

glGetObjectParameterivAPPLE
  :: MonadIO m
  => GLenum -- ^ @objectType@.
  -> GLuint -- ^ @name@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetObjectParameterivAPPLE v1 v2 v3 v4 = liftIO $ dyn351 ptr_glGetObjectParameterivAPPLE v1 v2 v3 v4

{-# NOINLINE ptr_glGetObjectParameterivAPPLE #-}
ptr_glGetObjectParameterivAPPLE :: FunPtr (GLenum -> GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetObjectParameterivAPPLE = unsafePerformIO $ getCommand "glGetObjectParameterivAPPLE"

-- glGetObjectParameterivARB ---------------------------------------------------

glGetObjectParameterivARB
  :: MonadIO m
  => GLhandleARB -- ^ @obj@ of type @handleARB@.
  -> GLenum -- ^ @pname@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetObjectParameterivARB v1 v2 v3 = liftIO $ dyn373 ptr_glGetObjectParameterivARB v1 v2 v3

{-# NOINLINE ptr_glGetObjectParameterivARB #-}
ptr_glGetObjectParameterivARB :: FunPtr (GLhandleARB -> GLenum -> Ptr GLint -> IO ())
ptr_glGetObjectParameterivARB = unsafePerformIO $ getCommand "glGetObjectParameterivARB"

-- glGetObjectPtrLabel ---------------------------------------------------------

-- | Manual page for <https://www.opengl.org/sdk/docs/man4/html/glGetObjectPtrLabel.xhtml OpenGL 4.x>.
glGetObjectPtrLabel
  :: MonadIO m
  => Ptr a -- ^ @ptr@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLchar -- ^ @label@ pointing to @bufSize@ elements of type @GLchar@.
  -> m ()
glGetObjectPtrLabel v1 v2 v3 v4 = liftIO $ dyn374 ptr_glGetObjectPtrLabel v1 v2 v3 v4

{-# NOINLINE ptr_glGetObjectPtrLabel #-}
ptr_glGetObjectPtrLabel :: FunPtr (Ptr a -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
ptr_glGetObjectPtrLabel = unsafePerformIO $ getCommand "glGetObjectPtrLabel"

-- glGetObjectPtrLabelKHR ------------------------------------------------------

-- | This command is an alias for 'glGetObjectPtrLabel'.
glGetObjectPtrLabelKHR
  :: MonadIO m
  => Ptr a -- ^ @ptr@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLchar -- ^ @label@ pointing to @bufSize@ elements of type @GLchar@.
  -> m ()
glGetObjectPtrLabelKHR v1 v2 v3 v4 = liftIO $ dyn374 ptr_glGetObjectPtrLabelKHR v1 v2 v3 v4

{-# NOINLINE ptr_glGetObjectPtrLabelKHR #-}
ptr_glGetObjectPtrLabelKHR :: FunPtr (Ptr a -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
ptr_glGetObjectPtrLabelKHR = unsafePerformIO $ getCommand "glGetObjectPtrLabelKHR"

-- glGetOcclusionQueryivNV -----------------------------------------------------

glGetOcclusionQueryivNV
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @pname@ of type @OcclusionQueryParameterNameNV@.
  -> Ptr GLint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetOcclusionQueryivNV v1 v2 v3 = liftIO $ dyn334 ptr_glGetOcclusionQueryivNV v1 v2 v3

{-# NOINLINE ptr_glGetOcclusionQueryivNV #-}
ptr_glGetOcclusionQueryivNV :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetOcclusionQueryivNV = unsafePerformIO $ getCommand "glGetOcclusionQueryivNV"

-- glGetOcclusionQueryuivNV ----------------------------------------------------

glGetOcclusionQueryuivNV
  :: MonadIO m
  => GLuint -- ^ @id@.
  -> GLenum -- ^ @pname@ of type @OcclusionQueryParameterNameNV@.
  -> Ptr GLuint -- ^ @params@ pointing to @COMPSIZE(pname)@ elements of type @GLuint@.
  -> m ()
glGetOcclusionQueryuivNV v1 v2 v3 = liftIO $ dyn375 ptr_glGetOcclusionQueryuivNV v1 v2 v3

{-# NOINLINE ptr_glGetOcclusionQueryuivNV #-}
ptr_glGetOcclusionQueryuivNV :: FunPtr (GLuint -> GLenum -> Ptr GLuint -> IO ())
ptr_glGetOcclusionQueryuivNV = unsafePerformIO $ getCommand "glGetOcclusionQueryuivNV"

-- glGetPathColorGenfvNV -------------------------------------------------------

glGetPathColorGenfvNV
  :: MonadIO m
  => GLenum -- ^ @color@ of type @PathColor@.
  -> GLenum -- ^ @pname@ of type @PathGenMode@.
  -> Ptr GLfloat -- ^ @value@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetPathColorGenfvNV v1 v2 v3 = liftIO $ dyn132 ptr_glGetPathColorGenfvNV v1 v2 v3

{-# NOINLINE ptr_glGetPathColorGenfvNV #-}
ptr_glGetPathColorGenfvNV :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetPathColorGenfvNV = unsafePerformIO $ getCommand "glGetPathColorGenfvNV"

-- glGetPathColorGenivNV -------------------------------------------------------

glGetPathColorGenivNV
  :: MonadIO m
  => GLenum -- ^ @color@ of type @PathColor@.
  -> GLenum -- ^ @pname@ of type @PathGenMode@.
  -> Ptr GLint -- ^ @value@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetPathColorGenivNV v1 v2 v3 = liftIO $ dyn133 ptr_glGetPathColorGenivNV v1 v2 v3

{-# NOINLINE ptr_glGetPathColorGenivNV #-}
ptr_glGetPathColorGenivNV :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetPathColorGenivNV = unsafePerformIO $ getCommand "glGetPathColorGenivNV"

-- glGetPathCommandsNV ---------------------------------------------------------

glGetPathCommandsNV
  :: MonadIO m
  => GLuint -- ^ @path@ of type @Path@.
  -> Ptr GLubyte -- ^ @commands@ pointing to @COMPSIZE(path)@ elements of type @PathCommand@.
  -> m ()
glGetPathCommandsNV v1 v2 = liftIO $ dyn376 ptr_glGetPathCommandsNV v1 v2

{-# NOINLINE ptr_glGetPathCommandsNV #-}
ptr_glGetPathCommandsNV :: FunPtr (GLuint -> Ptr GLubyte -> IO ())
ptr_glGetPathCommandsNV = unsafePerformIO $ getCommand "glGetPathCommandsNV"

-- glGetPathCoordsNV -----------------------------------------------------------

glGetPathCoordsNV
  :: MonadIO m
  => GLuint -- ^ @path@ of type @Path@.
  -> Ptr GLfloat -- ^ @coords@ pointing to @COMPSIZE(path)@ elements of type @GLfloat@.
  -> m ()
glGetPathCoordsNV v1 v2 = liftIO $ dyn377 ptr_glGetPathCoordsNV v1 v2

{-# NOINLINE ptr_glGetPathCoordsNV #-}
ptr_glGetPathCoordsNV :: FunPtr (GLuint -> Ptr GLfloat -> IO ())
ptr_glGetPathCoordsNV = unsafePerformIO $ getCommand "glGetPathCoordsNV"

-- glGetPathDashArrayNV --------------------------------------------------------

glGetPathDashArrayNV
  :: MonadIO m
  => GLuint -- ^ @path@ of type @Path@.
  -> Ptr GLfloat -- ^ @dashArray@ pointing to @COMPSIZE(path)@ elements of type @GLfloat@.
  -> m ()
glGetPathDashArrayNV v1 v2 = liftIO $ dyn377 ptr_glGetPathDashArrayNV v1 v2

{-# NOINLINE ptr_glGetPathDashArrayNV #-}
ptr_glGetPathDashArrayNV :: FunPtr (GLuint -> Ptr GLfloat -> IO ())
ptr_glGetPathDashArrayNV = unsafePerformIO $ getCommand "glGetPathDashArrayNV"

-- glGetPathLengthNV -----------------------------------------------------------

glGetPathLengthNV
  :: MonadIO m
  => GLuint -- ^ @path@ of type @Path@.
  -> GLsizei -- ^ @startSegment@.
  -> GLsizei -- ^ @numSegments@.
  -> m GLfloat
glGetPathLengthNV v1 v2 v3 = liftIO $ dyn378 ptr_glGetPathLengthNV v1 v2 v3

{-# NOINLINE ptr_glGetPathLengthNV #-}
ptr_glGetPathLengthNV :: FunPtr (GLuint -> GLsizei -> GLsizei -> IO GLfloat)
ptr_glGetPathLengthNV = unsafePerformIO $ getCommand "glGetPathLengthNV"

-- glGetPathMetricRangeNV ------------------------------------------------------

glGetPathMetricRangeNV
  :: MonadIO m
  => GLbitfield -- ^ @metricQueryMask@ of type @PathMetricMask@.
  -> GLuint -- ^ @firstPathName@ of type @Path@.
  -> GLsizei -- ^ @numPaths@.
  -> GLsizei -- ^ @stride@.
  -> Ptr GLfloat -- ^ @metrics@ pointing to @COMPSIZE(metricQueryMask,numPaths,stride)@ elements of type @GLfloat@.
  -> m ()
glGetPathMetricRangeNV v1 v2 v3 v4 v5 = liftIO $ dyn379 ptr_glGetPathMetricRangeNV v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetPathMetricRangeNV #-}
ptr_glGetPathMetricRangeNV :: FunPtr (GLbitfield -> GLuint -> GLsizei -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glGetPathMetricRangeNV = unsafePerformIO $ getCommand "glGetPathMetricRangeNV"

-- glGetPathMetricsNV ----------------------------------------------------------

glGetPathMetricsNV
  :: MonadIO m
  => GLbitfield -- ^ @metricQueryMask@ of type @PathMetricMask@.
  -> GLsizei -- ^ @numPaths@.
  -> GLenum -- ^ @pathNameType@ of type @PathElementType@.
  -> Ptr a -- ^ @paths@ pointing to @COMPSIZE(numPaths,pathNameType,paths)@ elements of type @PathElement@.
  -> GLuint -- ^ @pathBase@ of type @Path@.
  -> GLsizei -- ^ @stride@.
  -> Ptr GLfloat -- ^ @metrics@ pointing to @COMPSIZE(metricQueryMask,numPaths,stride)@ elements of type @GLfloat@.
  -> m ()
glGetPathMetricsNV v1 v2 v3 v4 v5 v6 v7 = liftIO $ dyn380 ptr_glGetPathMetricsNV v1 v2 v3 v4 v5 v6 v7

{-# NOINLINE ptr_glGetPathMetricsNV #-}
ptr_glGetPathMetricsNV :: FunPtr (GLbitfield -> GLsizei -> GLenum -> Ptr a -> GLuint -> GLsizei -> Ptr GLfloat -> IO ())
ptr_glGetPathMetricsNV = unsafePerformIO $ getCommand "glGetPathMetricsNV"

-- glGetPathParameterfvNV ------------------------------------------------------

glGetPathParameterfvNV
  :: MonadIO m
  => GLuint -- ^ @path@ of type @Path@.
  -> GLenum -- ^ @pname@ of type @PathParameter@.
  -> Ptr GLfloat -- ^ @value@ pointing to @4@ elements of type @GLfloat@.
  -> m ()
glGetPathParameterfvNV v1 v2 v3 = liftIO $ dyn349 ptr_glGetPathParameterfvNV v1 v2 v3

{-# NOINLINE ptr_glGetPathParameterfvNV #-}
ptr_glGetPathParameterfvNV :: FunPtr (GLuint -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetPathParameterfvNV = unsafePerformIO $ getCommand "glGetPathParameterfvNV"

-- glGetPathParameterivNV ------------------------------------------------------

glGetPathParameterivNV
  :: MonadIO m
  => GLuint -- ^ @path@ of type @Path@.
  -> GLenum -- ^ @pname@ of type @PathParameter@.
  -> Ptr GLint -- ^ @value@ pointing to @4@ elements of type @GLint@.
  -> m ()
glGetPathParameterivNV v1 v2 v3 = liftIO $ dyn334 ptr_glGetPathParameterivNV v1 v2 v3

{-# NOINLINE ptr_glGetPathParameterivNV #-}
ptr_glGetPathParameterivNV :: FunPtr (GLuint -> GLenum -> Ptr GLint -> IO ())
ptr_glGetPathParameterivNV = unsafePerformIO $ getCommand "glGetPathParameterivNV"

-- glGetPathSpacingNV ----------------------------------------------------------

glGetPathSpacingNV
  :: MonadIO m
  => GLenum -- ^ @pathListMode@ of type @PathListMode@.
  -> GLsizei -- ^ @numPaths@.
  -> GLenum -- ^ @pathNameType@ of type @PathElementType@.
  -> Ptr a -- ^ @paths@ pointing to @COMPSIZE(numPaths,pathNameType,paths)@ elements of type @PathElement@.
  -> GLuint -- ^ @pathBase@ of type @Path@.
  -> GLfloat -- ^ @advanceScale@.
  -> GLfloat -- ^ @kerningScale@.
  -> GLenum -- ^ @transformType@ of type @PathTransformType@.
  -> Ptr GLfloat -- ^ @returnedSpacing@ pointing to @COMPSIZE(pathListMode,numPaths)@ elements of type @GLfloat@.
  -> m ()
glGetPathSpacingNV v1 v2 v3 v4 v5 v6 v7 v8 v9 = liftIO $ dyn381 ptr_glGetPathSpacingNV v1 v2 v3 v4 v5 v6 v7 v8 v9

{-# NOINLINE ptr_glGetPathSpacingNV #-}
ptr_glGetPathSpacingNV :: FunPtr (GLenum -> GLsizei -> GLenum -> Ptr a -> GLuint -> GLfloat -> GLfloat -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetPathSpacingNV = unsafePerformIO $ getCommand "glGetPathSpacingNV"

-- glGetPathTexGenfvNV ---------------------------------------------------------

glGetPathTexGenfvNV
  :: MonadIO m
  => GLenum -- ^ @texCoordSet@ of type @TextureUnit@.
  -> GLenum -- ^ @pname@ of type @PathGenMode@.
  -> Ptr GLfloat -- ^ @value@ pointing to @COMPSIZE(pname)@ elements of type @GLfloat@.
  -> m ()
glGetPathTexGenfvNV v1 v2 v3 = liftIO $ dyn132 ptr_glGetPathTexGenfvNV v1 v2 v3

{-# NOINLINE ptr_glGetPathTexGenfvNV #-}
ptr_glGetPathTexGenfvNV :: FunPtr (GLenum -> GLenum -> Ptr GLfloat -> IO ())
ptr_glGetPathTexGenfvNV = unsafePerformIO $ getCommand "glGetPathTexGenfvNV"

-- glGetPathTexGenivNV ---------------------------------------------------------

glGetPathTexGenivNV
  :: MonadIO m
  => GLenum -- ^ @texCoordSet@ of type @TextureUnit@.
  -> GLenum -- ^ @pname@ of type @PathGenMode@.
  -> Ptr GLint -- ^ @value@ pointing to @COMPSIZE(pname)@ elements of type @GLint@.
  -> m ()
glGetPathTexGenivNV v1 v2 v3 = liftIO $ dyn133 ptr_glGetPathTexGenivNV v1 v2 v3

{-# NOINLINE ptr_glGetPathTexGenivNV #-}
ptr_glGetPathTexGenivNV :: FunPtr (GLenum -> GLenum -> Ptr GLint -> IO ())
ptr_glGetPathTexGenivNV = unsafePerformIO $ getCommand "glGetPathTexGenivNV"

-- glGetPerfCounterInfoINTEL ---------------------------------------------------

glGetPerfCounterInfoINTEL
  :: MonadIO m
  => GLuint -- ^ @queryId@.
  -> GLuint -- ^ @counterId@.
  -> GLuint -- ^ @counterNameLength@.
  -> Ptr GLchar -- ^ @counterName@.
  -> GLuint -- ^ @counterDescLength@.
  -> Ptr GLchar -- ^ @counterDesc@.
  -> Ptr GLuint -- ^ @counterOffset@.
  -> Ptr GLuint -- ^ @counterDataSize@.
  -> Ptr GLuint -- ^ @counterTypeEnum@.
  -> Ptr GLuint -- ^ @counterDataTypeEnum@.
  -> Ptr GLuint64 -- ^ @rawCounterMaxValue@.
  -> m ()
glGetPerfCounterInfoINTEL v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 = liftIO $ dyn382 ptr_glGetPerfCounterInfoINTEL v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11

{-# NOINLINE ptr_glGetPerfCounterInfoINTEL #-}
ptr_glGetPerfCounterInfoINTEL :: FunPtr (GLuint -> GLuint -> GLuint -> Ptr GLchar -> GLuint -> Ptr GLchar -> Ptr GLuint -> Ptr GLuint -> Ptr GLuint -> Ptr GLuint -> Ptr GLuint64 -> IO ())
ptr_glGetPerfCounterInfoINTEL = unsafePerformIO $ getCommand "glGetPerfCounterInfoINTEL"

-- glGetPerfMonitorCounterDataAMD ----------------------------------------------

glGetPerfMonitorCounterDataAMD
  :: MonadIO m
  => GLuint -- ^ @monitor@.
  -> GLenum -- ^ @pname@.
  -> GLsizei -- ^ @dataSize@.
  -> Ptr GLuint -- ^ @data@ pointing to @dataSize@ elements of type @GLuint@.
  -> Ptr GLint -- ^ @bytesWritten@ pointing to @1@ element of type @GLint@.
  -> m ()
glGetPerfMonitorCounterDataAMD v1 v2 v3 v4 v5 = liftIO $ dyn383 ptr_glGetPerfMonitorCounterDataAMD v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetPerfMonitorCounterDataAMD #-}
ptr_glGetPerfMonitorCounterDataAMD :: FunPtr (GLuint -> GLenum -> GLsizei -> Ptr GLuint -> Ptr GLint -> IO ())
ptr_glGetPerfMonitorCounterDataAMD = unsafePerformIO $ getCommand "glGetPerfMonitorCounterDataAMD"

-- glGetPerfMonitorCounterInfoAMD ----------------------------------------------

glGetPerfMonitorCounterInfoAMD
  :: MonadIO m
  => GLuint -- ^ @group@.
  -> GLuint -- ^ @counter@.
  -> GLenum -- ^ @pname@.
  -> Ptr a -- ^ @data@ pointing to @COMPSIZE(pname)@ elements of type @a@.
  -> m ()
glGetPerfMonitorCounterInfoAMD v1 v2 v3 v4 = liftIO $ dyn384 ptr_glGetPerfMonitorCounterInfoAMD v1 v2 v3 v4

{-# NOINLINE ptr_glGetPerfMonitorCounterInfoAMD #-}
ptr_glGetPerfMonitorCounterInfoAMD :: FunPtr (GLuint -> GLuint -> GLenum -> Ptr a -> IO ())
ptr_glGetPerfMonitorCounterInfoAMD = unsafePerformIO $ getCommand "glGetPerfMonitorCounterInfoAMD"

-- glGetPerfMonitorCounterStringAMD --------------------------------------------

glGetPerfMonitorCounterStringAMD
  :: MonadIO m
  => GLuint -- ^ @group@.
  -> GLuint -- ^ @counter@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLchar -- ^ @counterString@ pointing to @bufSize@ elements of type @GLchar@.
  -> m ()
glGetPerfMonitorCounterStringAMD v1 v2 v3 v4 v5 = liftIO $ dyn305 ptr_glGetPerfMonitorCounterStringAMD v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetPerfMonitorCounterStringAMD #-}
ptr_glGetPerfMonitorCounterStringAMD :: FunPtr (GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
ptr_glGetPerfMonitorCounterStringAMD = unsafePerformIO $ getCommand "glGetPerfMonitorCounterStringAMD"

-- glGetPerfMonitorCountersAMD -------------------------------------------------

glGetPerfMonitorCountersAMD
  :: MonadIO m
  => GLuint -- ^ @group@.
  -> Ptr GLint -- ^ @numCounters@ pointing to @1@ element of type @GLint@.
  -> Ptr GLint -- ^ @maxActiveCounters@ pointing to @1@ element of type @GLint@.
  -> GLsizei -- ^ @counterSize@.
  -> Ptr GLuint -- ^ @counters@ pointing to @counterSize@ elements of type @GLuint@.
  -> m ()
glGetPerfMonitorCountersAMD v1 v2 v3 v4 v5 = liftIO $ dyn385 ptr_glGetPerfMonitorCountersAMD v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetPerfMonitorCountersAMD #-}
ptr_glGetPerfMonitorCountersAMD :: FunPtr (GLuint -> Ptr GLint -> Ptr GLint -> GLsizei -> Ptr GLuint -> IO ())
ptr_glGetPerfMonitorCountersAMD = unsafePerformIO $ getCommand "glGetPerfMonitorCountersAMD"

-- glGetPerfMonitorGroupStringAMD ----------------------------------------------

glGetPerfMonitorGroupStringAMD
  :: MonadIO m
  => GLuint -- ^ @group@.
  -> GLsizei -- ^ @bufSize@.
  -> Ptr GLsizei -- ^ @length@ pointing to @1@ element of type @GLsizei@.
  -> Ptr GLchar -- ^ @groupString@ pointing to @bufSize@ elements of type @GLchar@.
  -> m ()
glGetPerfMonitorGroupStringAMD v1 v2 v3 v4 = liftIO $ dyn331 ptr_glGetPerfMonitorGroupStringAMD v1 v2 v3 v4

{-# NOINLINE ptr_glGetPerfMonitorGroupStringAMD #-}
ptr_glGetPerfMonitorGroupStringAMD :: FunPtr (GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLchar -> IO ())
ptr_glGetPerfMonitorGroupStringAMD = unsafePerformIO $ getCommand "glGetPerfMonitorGroupStringAMD"

-- glGetPerfMonitorGroupsAMD ---------------------------------------------------

glGetPerfMonitorGroupsAMD
  :: MonadIO m
  => Ptr GLint -- ^ @numGroups@ pointing to @1@ element of type @GLint@.
  -> GLsizei -- ^ @groupsSize@.
  -> Ptr GLuint -- ^ @groups@ pointing to @groupsSize@ elements of type @GLuint@.
  -> m ()
glGetPerfMonitorGroupsAMD v1 v2 v3 = liftIO $ dyn332 ptr_glGetPerfMonitorGroupsAMD v1 v2 v3

{-# NOINLINE ptr_glGetPerfMonitorGroupsAMD #-}
ptr_glGetPerfMonitorGroupsAMD :: FunPtr (Ptr GLint -> GLsizei -> Ptr GLuint -> IO ())
ptr_glGetPerfMonitorGroupsAMD = unsafePerformIO $ getCommand "glGetPerfMonitorGroupsAMD"

-- glGetPerfQueryDataINTEL -----------------------------------------------------

glGetPerfQueryDataINTEL
  :: MonadIO m
  => GLuint -- ^ @queryHandle@.
  -> GLuint -- ^ @flags@.
  -> GLsizei -- ^ @dataSize@.
  -> Ptr a -- ^ @data@.
  -> Ptr GLuint -- ^ @bytesWritten@.
  -> m ()
glGetPerfQueryDataINTEL v1 v2 v3 v4 v5 = liftIO $ dyn386 ptr_glGetPerfQueryDataINTEL v1 v2 v3 v4 v5

{-# NOINLINE ptr_glGetPerfQueryDataINTEL #-}
ptr_glGetPerfQueryDataINTEL :: FunPtr (GLuint -> GLuint -> GLsizei -> Ptr a -> Ptr GLuint -> IO ())
ptr_glGetPerfQueryDataINTEL = unsafePerformIO $ getCommand "glGetPerfQueryDataINTEL"

