{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }
{$IFDEF FPC}
  {$MODE objfpc}
  {$MODESWITCH AdvancedRecords}
  {$NOTES OFF}
  {$STACKFRAMES ON}

  {$DEFINE LITTLE_ENDIAN}
  {$UNDEF BIG_ENDIAN}
  {$IFDEF FPC_BIG_ENDIAN}
    {$UNDEF LITTLE_ENDIAN}
    {$DEFINE BIG_ENDIAN}
  {$ENDIF}

  {$UNDEF FirstCharInZero}

  {$UNDEF Delphi}

  // fast MD5 only delphi supported, https://github.com/PassByYou888/FastMD5
  {$UNDEF FastMD5}

  // stream is MemoryStream64 or MemoryStream, usage fastMD5 or PurePascal MD5
  // be associate api: UnicodeMixedLib.umlStreamMD5, Fast_MD5.FastMD5
  {$DEFINE OptimizationMemoryStreamMD5}

  // multi thread parallel switch.
  {$DEFINE parallel}

  (*
   Z_AI summary

   fast surf:
   This is the original paper which introduced the algorithm: Speeded Up Robust Features, By Herbert Bay, Tinne Tuytelaars, and Luc Van Gool,
   translate for pascal, by qq600585

   object detection train:
   learning to detect objects in images. supported.
   create an object detector it requires a set of annotated training images.

   object detection:
   used sliding window object detector.
   first published by Dalal and Triggs in 2005 in the paper Histograms of Oriented Gradients for Human Detection.

   svm:
   1, cross validation and svm training functions to find a good decision.
   2, pegasos algorithm for online training of support vector machines.
   3, learning to rank objects.
   4, simple binary classification problem and make you to train a support vector machine on that data.
   5, the central ideas needed to setup a structural SVM model for your machine learning problems.

   svr:
   illustrating the use of the epsilon-insensitive support vector regression object from the zAI Library.

   ResNet network face recognition:
   model has a 99.38% accuracy on the standard LFW face recognition benchmark,
   which is comparable to other state-of-the-art methods for face recognition as of February 2017.

   face shape predictor trainer:
   One Millisecond Face Alignment with an Ensemble of Regression Trees by Vahid Kazemi and Josephine Sullivan, CVPR 2014

   cuDNN, https://developer.nvidia.com/cuda-zone

   Max-Margin Object Detection and train, CVPR 2015,

   ResNet34 Object detector train and object classifier:
   The ResNet34 architecture is from the paper Deep Residual Learning for Image Recognition by qq600585.
   The model file that comes with zAI was trained using the cuda program on a Titan X for about 2 weeks.
   This pretrained model has a top5 error of 7.572% on test for 2012 imagenet validation dataset.

   krls:
   This is an implementation of the kernel recursive least squares algorithm described in the paper:
   The Kernel Recursive Least Squares Algorithm by Yaakov Engel.
  *)
  {$DEFINE Z_AI}

  // With LARGE_RASTER_FONT, boot-time memory usage increase by 100M-200M and start-up time to be delay 100ms
  // {$DEFINE LARGE_RASTER_FONT}

  // ZDB_BACKUP is automatically made and replica caching is enabled.
  // usage ZDB_BACKUP so slows the open of large size ZDB file, after time, but does is high performance.
  // {$DEFINE ZDB_BACKUP}

  // used Critical Simulate Atomic with TMonitor.Enter(obj) and TMonitor.Exit(obj)
  // CriticalSimulateAtomic defined so performance to be reduced
  {$DEFINE CriticalSimulateAtomic}

  // used soft Simulate Critical(ring)
  // SoftCritical defined so performance to be reduced
  // {$DEFINE SoftCritical}
  // {$DEFINE ANTI_DEAD_ATOMIC_LOCK}

  {$DEFINE release}
  {$UNDEF debug}
  {$DEFINE INLINE_ASM}
  {$R-} { range check }
  {$O-} { optimization }
{$ELSE FPC}
  {$DEFINE LITTLE_ENDIAN}
  {$UNDEF BIG_ENDIAN}

  {$IFDEF ANDROID}
    {$DEFINE FirstCharInZero}
  {$ENDIF}

  {$IFDEF IOS}
    {$DEFINE FirstCharInZero}
  {$ENDIF}

  {$DEFINE Delphi}

  // fast MD5 only delphi supported, https://github.com/PassByYou888/FastMD5
  {$DEFINE FastMD5}

  // stream is MemoryStream64 or MemoryStream, usage fastMD5 or PurePascal MD5
  // be associate api: UnicodeMixedLib.umlStreamMD5, Fast_MD5.FastMD5
  {$DEFINE OptimizationMemoryStreamMD5}

  // multi thread parallel switch.
  {$DEFINE parallel}

  (*
   Z_AI summary

   fast surf:
   This is the original paper which introduced the algorithm: Speeded Up Robust Features, By Herbert Bay, Tinne Tuytelaars, and Luc Van Gool,
   translate for pascal, by qq600585

   object detection train:
   learning to detect objects in images. supported.
   create an object detector it requires a set of annotated training images.

   object detection:
   used sliding window object detector.
   first published by Dalal and Triggs in 2005 in the paper Histograms of Oriented Gradients for Human Detection.

   svm:
   1, cross validation and svm training functions to find a good decision.
   2, pegasos algorithm for online training of support vector machines.
   3, learning to rank objects.
   4, simple binary classification problem and make you to train a support vector machine on that data.
   5, the central ideas needed to setup a structural SVM model for your machine learning problems.

   svr:
   illustrating the use of the epsilon-insensitive support vector regression object from the zAI Library.

   ResNet network face recognition:
   model has a 99.38% accuracy on the standard LFW face recognition benchmark,
   which is comparable to other state-of-the-art methods for face recognition as of February 2017.

   face shape predictor trainer:
   One Millisecond Face Alignment with an Ensemble of Regression Trees by Vahid Kazemi and Josephine Sullivan, CVPR 2014

   cuDNN, https://developer.nvidia.com/cuda-zone

   Max-Margin Object Detection and train, CVPR 2015,

   ResNet34 Object detector train and object classifier:
   The ResNet34 architecture is from the paper Deep Residual Learning for Image Recognition by qq600585.
   The model file that comes with zAI was trained using the cuda program on a Titan X for about 2 weeks.
   This pretrained model has a top5 error of 7.572% on test for 2012 imagenet validation dataset.

   krls:
   This is an implementation of the kernel recursive least squares algorithm described in the paper:
   The Kernel Recursive Least Squares Algorithm by Yaakov Engel.
  *)
  {$DEFINE Z_AI}

  // With LARGE_RASTER_FONT, boot-time memory usage increase by 100M-200M and start-up time to be delay 100ms
  // {$DEFINE LARGE_RASTER_FONT}

  // ZDB_BACKUP is automatically made and replica caching is enabled.
  // usage ZDB_BACKUP so slows the open of large size ZDB file, after time, but does is high performance.
  // {$DEFINE ZDB_BACKUP}

  // used Critical Simulate Atomic with TMonitor.Enter(obj) and TMonitor.Exit(obj)
  // CriticalSimulateAtomic defined so performance to be reduced
  // {$DEFINE CriticalSimulateAtomic}

  // used soft Simulate Critical(ring)
  // SoftCritical defined so performance to be reduced
  // {$DEFINE SoftCritical}
  // {$DEFINE ANTI_DEAD_ATOMIC_LOCK}

  {$IFDEF release}
    {$DEFINE INLINE_ASM}
    {$R-} { range check }
    {$I-} { Input output checking }
    {$O+} { optimization }
    {$INLINE AUTO} { call inline automated }
    {$C+} { Assertions }
  {$ELSE}
    {$UNDEF INLINE_ASM}
    {$R+} { range check }
    {$I+} { Input output checking }
    {$O-} { optimization }
    {$C+} { Assertions }
  {$ENDIF}

  {$IF Defined(Android) or Defined(IOS)}
    {$DEFINE PhysicsIO_On_Indy}
  {$ELSE}
    // PhysicsIO interface
    // {$DEFINE PhysicsIO_On_ICS}
    {$DEFINE PhysicsIO_On_CrossSocket}
    // {$DEFINE PhysicsIO_On_DIOCP}
    // {$DEFINE PhysicsIO_On_Indy}
    // {$DEFINE PhysicsIO_On_Synapse}
  {$IFEND}
{$ENDIF FPC}

{$WARNINGS ON}
{$HINTS ON}
{$H+} { long string }
{$A+} { Word Align Data }
{$Q-} { Overflow checking }
{$B-} { Complete boolean evaluation }
{$J+} { Writeable typed constants }

(*
  Pointer math is simply treating any given typed pointer in some narrow,
  instances as a scaled ordinal where you can perform simple arithmetic operations directly on the pointer variable.
*)
{$POINTERMATH OFF}

{$UNDEF CPU64}

{$IFDEF CPU64BITS}
  {$DEFINE CPU64}
{$ELSE}
  {$IFDEF CPUX64}
    {$DEFINE CPU64}
  {$ENDIF}
{$ENDIF}

{$IFNDEF CPU64}
  {$DEFINE CPU32}
{$ENDIF}

{$IFDEF BIG_ENDIAN}
  {$MESSAGE FATAL 'Big-endian system not supported'}
{$ENDIF}

{$IFOPT R+}
  {$DEFINE RangeCheck}
{$ENDIF}

{$IFOPT Q+}
  {$DEFINE OverflowCheck}
{$ENDIF}
