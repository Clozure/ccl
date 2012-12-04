(defpackage "CG"
  (:use "CL" "CCL")
  (:import-from "CCL" "WITH-SPECS-AUX" "PARSE-BODY"
                "MAKE-GCABLE-RECORD" "RECORD-LENGTH")
  (:export "*BLACK-COLOR*" "*WHITE-COLOR*"
           "CGFLOAT" "WITH-RECT" "WITH-RECTS"
	   "CONTEXT-SAVE-GSTATE" "CONTEXT-RESTORE-GSTATE" "WITH-SAVED-GSTATE"
           "CONTEXT-SCALE-CTM" "CONTEXT-TRANSLATE-CTM" "CONTEXT-ROTATE-CTM"
	   "CONTEXT-CONCAT-CTM" "CONTEXT-GET-CTM"
           "CONTEXT-SET-LINE-WIDTH" "CONTEXT-SET-LINE-CAP"
	   "CONTEXT-SET-LINE-JOIN" "CONTEXT-SET-MITER-LIMIT"
	   "CONTEXT-SET-LINE-DASH" "CONTEXT-SET-FLATNESS" "CONTEXT-SET-ALPHA"
	   "CONTEXT-SET-BLEND-MODE"
	   "CONTEXT-BEGIN-PATH" "CONTEXT-MOVE-TO-POINT"
	   "CONTEXT-ADD-LINE-TO-POINT" "CONTEXT-ADD-CURVE-TO-POINT"
	   "CONTEXT-ADD-QUAD-CURVE-TO-POINT" "CONTEXT-CLOSE-PATH"
           "CONTEXT-ADD-ARC" "CONTEXT-DRAW-PATH"
	   "CONTEXT-FILL-PATH" "CONTEXT-EO-FILL-PATH" "CONTEXT-STROKE-PATH"
	   "CONTEXT-FILL-RECT" "CONTEXT-FILL-RECTS" "CONTEXT-STROKE-RECT"
	   "CONTEXT-STROKE-RECT-WITH-WIDTH" "CONTEXT-CLEAR-RECT"
	   "CONTEXT-FILL-ELLIPSE-IN-RECT" "CONTEXT-STROKE-ELLISPE-IN-RECT"
	   "CONTEXT-STROKE-LINE-SEGMENTS"
	   "CONTEXT-SET-FILL-COLOR-WITH-COLOR"
	   "CONTEXT-SET-STROKE-COLOR-WITH-COLOR"
	   "CONTEXT-SET-FILL-COLOR-SPACE" "CONTEXT-SET-STROKE-COLOR-SPACE"
	   "CONTEXT-SET-GRAY-FILL-COLOR" "CONTEXT-SET-GRAY-STROKE-COLOR"
	   "CONTEXT-SET-RGB-FILL-COLOR" "CONTEXT-SET-RGB-STROKE-COLOR"
	   "CONTEXT-SET-CMYK-FILL-COLOR" "CONTEXT-SET-CMYK-STROKE-COLOR"
	   "CONTEXT-SET-CHARACTER-SPACING" "CONTEXT-SET-TEXT-POSITION"
	   "CONTEXT-GET-TEXT-POSITION" "CONTEXT-SET-TEXT-DRAWING-MODE"
           "CONTEXT-SELECT-FONT"
	   "CONTEXT-SHOW-TEXT" "CONTEXT-SHOW-TEXT-AT-POINT"

	   "PATH-MOVE-TO-POINT" "PATH-ADD-LINE-TO-POINT"

           "COLOR-CREATE-GENERIC-RGB"
	   ))
(in-package "CG")

(defloadvar *black-color* (#_CGColorGetConstantColor #&kCGColorBlack))
(defloadvar *white-color* (#_CGColorGetConstantColor #&kCGColorWhite))

(defconstant $cgfloat-zero #+32-bit-target 0f0
                           #+64-bit-target 0d0)

(deftype cgfloat () #+32-bit-target 'single-float
                    #+64-bit-target 'double-float)

(defmacro cgfloat (n)
  `(float ,n $cgfloat-zero))

(defmacro with-rect ((rect x y width height) &body body &environment env)
  (multiple-value-bind (body decls) (parse-body body env nil)
    `(rlet ((,rect #>CGRect))
       ,@decls
       (setf (pref ,rect #>CGRect.origin.x) (cgfloat ,x)
             (pref ,rect #>CGRect.origin.y) (cgfloat ,y)
             (pref ,rect #>CGRect.size.width) (cgfloat ,width)
             (pref ,rect #>CGRect.size.height) (cgfloat ,height))
       ,@body)))

(defmacro with-rects (speclist &body body)
  (with-specs-aux 'with-rect speclist body))

;;; Graphics state
(defun context-save-gstate (context)
  (#_CGContextSaveGState context))

(defun context-restore-gstate (context)
  (#_CGContextRestoreGState context))

#|
(defmacro with-saved-gstate ((context) &body body)
  `(unwind-protect
       (progn
         (context-save-gstate ,context)
         ,@body)
     (context-restore-gstate ,context)))
|#

(defmacro with-saved-gstate ((context) &body body)
  `(progn
     (context-save-gstate ,context)
     ,@body
     (context-restore-gstate ,context)))

;;; Coordinate space transformations
(defun context-scale-ctm (context sx sy)
  (#_CGContextScaleCTM context (cgfloat sx) (cgfloat sy)))

(defun context-translate-ctm (context tx ty)
  (#_CGContextTranslateCTM context (cgfloat tx) (cgfloat ty)))

(defun context-rotate-ctm (context radians)
  (#_CGContextRotateCTM context (cgfloat radians)))

(defun context-concat-ctm (context transform)
  (#_CGContextConcatCTM context transform))

(defun context-get-ctm (context)
  (let ((ctm (make-gcable-record #>CGAffineTransform)))
    (#_CGContextGetCTM ctm context)
    ctm))

;;; Drawing attribute functions
(defun context-set-line-width (context width)
  (#_CGContextSetLineWidth context (cgfloat width)))

(defun context-set-line-cap (context cap)
  (let ((code (case cap
                (:butt #$kCGLineCapButt)
                (:square #$kCGLineCapSquare)
                (:round #$kCGLineCapRound)
                (otherwise cap))))
    (#_CGContextSetLineCap context code)))

(defun context-set-line-join (context join)
  (let ((code (ecase join
                (:miter #$kCGLineJoinMiter)
                (:round #$kCGLineJoinRound)
                (:bevel #$kCGLineJoinBevel)
                (otherwise join))))
    (#_CGContextSetLineJoin context code)))

(defun context-set-miter-limit (context limit)
  (#_CGContextSetMiterLimit context (cgfloat limit)))

(defun context-set-line-dash (context phase lengths)
  (let ((n (length lengths)))
    (%stack-block ((p (* n (record-length #>CGFloat))))
      (dotimes (i n)
        (setf (paref p (:array #>CGFloat) i) (cgfloat (elt lengths i))))
      (#_CGContextSetLineDash context (cgfloat phase) p n))))

(defun context-set-flatness (context flatness)
  (#_CGContextSetFlatness context (cgfloat flatness)))

(defun context-set-alpha (context alpha)
  (#_CGContextSetAlpha context (cgfloat alpha)))

(defparameter *blend-mode-alist*
  '((:normal . 0)
    (:multiply . 1)
    (:screen . 2)
    (:overlay . 3)
    (:darken . 4)
    (:lighten . 5)
    (:color-dodge . 6)
    (:color-burn . 7)
    (:soft-light . 8)
    (:hard-light . 9)
    (:difference . 10)
    (:exclusion . 11)
    (:hue . 12)
    (:saturation . 13)
    (:color . 14)
    (:luminosity . 15)
    (:clear . 16)
    (:copy . 17)
    (:source-in . 18)
    (:source-out . 19)
    (:source-atop . 20)
    (:destination-over . 21)
    (:destination-in . 22)
    (:destination-out . 23)
    (:destination-atop . 24)
    (:xor . 25)
    (:plus-darker . 26)
    (:plus-lighter . 27)))

(defun context-set-blend-mode (context mode)
  (let ((code (or (cdr (assoc mode *blend-mode-alist*))
                  mode)))
    (#_CGContextSetBlendMode context code)))

;;; Path construction functions
(defun context-begin-path (context)
  (#_CGContextBeginPath context))

(defun context-move-to-point (context x y)
  (#_CGContextMoveToPoint context (cgfloat x) (cgfloat y)))

(defun context-add-line-to-point (context x y)
  (#_CGContextAddLineToPoint context (cgfloat x) (cgfloat y)))

(defun context-add-curve-to-point (context cp1x cp1y cp2x cp2y x y)
  (#_CGContextAddCurveToPoint context (cgfloat cp1x) (cgfloat cp1y)
                              (cgfloat cp2x) (cgfloat cp2y)
                              (cgfloat x) (cgfloat y)))

(defun context-add-quad-curve-to-point (context cpx cpy x y)
  (#_CGContextAddQuadCurveToPoint context (cgfloat cpx) (cgfloat cpy)
                                  (cgfloat x) (cgfloat y)))

(defun context-close-path (context)
  (#_CGContextClosePath context))

;;; Path construction convenience functions
#|
CGContextAddRect
CGContextAddRects
CGContextAddLines
CGContextAddEllipseInRect
|#

(defun context-add-arc (context x y radius start-angle end-angle clockwise)
  (#_CGContextAddArc context (cgfloat x) (cgfloat y) (cgfloat radius)
                     (cgfloat start-angle) (cgfloat end-angle) clockwise))
#|
CGContextAddArcToPoint
CGContextAddPath
|#

;;; Path stroking
#|
CGContextReplacePathWithStrokedPath
|#

;;; Path information functions
#|
CGContextIsPathEmpty
CGContextGetPathCurrentPoint
CGContextGetPathBoundingBox
CGContextCopyPath
CGContextPathContainsPoint
|#

;;; Path drawing functions

(defun context-draw-path (context mode)
  (let ((code (case mode
                (:fill #$kCGPathFill)
                (:eofill #$kCGPathEOFill)
                (:stroke #$kCGPathStroke)
                (:fill-stroke #$kCGPathFillStroke)
                (:eofill-stroke #$kCGPathEOFillStroke)
                (otherwise mode))))
    (#_CGContextDrawPath context code)))

;;; Path drawing convenience functions
(defun context-fill-path (context)
  (#_CGContextFillPath context))

(defun context-eo-fill-path (context)
  (#_CGContextEOFillPath context))

(defun context-stroke-path (context)
  (#_CGContextStrokePath context))

(defun context-fill-rect (context rect)
  (#_CGContextFillRect context rect))

(defun context-fill-rects (context rects count)
  (#_CGContextFillRects context rects count))

(defun context-stroke-rect (context rect)
  (#_CGContextStrokeRect context rect))

(defun context-stroke-rect-with-width (context rect width)
  (#_CGContextStrokeRectWithWidth context rect (cgfloat width)))

(defun context-clear-rect (context rect)
  (#_CGContextClearRect context rect))

(defun context-fill-ellipse-in-rect (context rect)
  (#_CGContextFillEllipseInRect context rect))

(defun context-stroke-ellipse-in-rect (context rect)
  (#_CGContextStrokeEllipseInRect context rect))

(defun context-stroke-line-segments (context points count)
  (#_CGContextStrokeLineSegments context points count))

;;; Clipping functions
#|
CGContextClip
CGContextEOClip
CGContextClipToMask
CGContextGetClipBoundingBox
|#

;;; Clipping convenience functions
#|
CGContextClipToRect
CGContextClipToRects
|#

;;; Primitive color functions
(defun context-set-fill-color-with-color (context color)
  (#_CGContextSetFillColorWithColor context color))

(defun context-set-stroke-color-with-color (context color)
  (#_CGContextSetStrokeColorWithColor context color))

;;; Color space functions
(defun context-set-fill-color-space (context space)
  (#_CGContextSetFillColorSpace context space))

(defun context-set-stroke-color-space (context space)
  (#_CGContextSetStrokeColorSpace context space))

;;; Color functions
#|
CGContextSetFillColor
CGContextSetStrokeColor
|#

;;; Pattern functions
#|
CGContextSetFillPattern
CGContextSetStrokePattern
CGContextSetPatternPhase
|#

;;; Color convenience functions
(defun context-set-gray-fill-color (context gray &optional (alpha 1))
  (#_CGContextSetGrayFillColor context (cgfloat gray) (cgfloat alpha)))

(defun context-set-gray-stroke-color (context gray &optional (alpha 1))
  (#_CGContextSetGrayStrokeColor context (cgfloat gray) (cgfloat alpha)))

(defun context-set-rgb-fill-color (context red green blue &optional (alpha 1))
  (#_CGContextSetRGBFillColor context (cgfloat red) (cgfloat green)
                              (cgfloat blue) (cgfloat alpha)))

(defun context-set-rgb-stroke-color (context red green blue &optional (alpha 1))
  (#_CGContextSetRGBStrokeColor context (cgfloat red) (cgfloat green)
                                (cgfloat blue) (cgfloat alpha)))

(defun context-set-cmyk-fill-color (context cyan magenta yellow black &optional (alpha 1))
  (#_CGContextSetCMYKFillColor context (cgfloat cyan) (cgfloat magenta)
                               (cgfloat yellow) (cgfloat black)
                               (cgfloat alpha)))

(defun context-set-cmyk-stroke-color (context cyan magenta yellow black &optional(alpha 1))
  (#_CGContextSetCMYKStrokeColor context (cgfloat cyan) (cgfloat magenta)
                                 (cgfloat yellow) (cgfloat black)
                                 (cgfloat alpha)))

;;; Rendering intent
#|
CGContextSetRenderingIntent
|#

;;; Image functions
#|
CGContextDrawImage
CGContextDrawTiledImage
CGInterpolationQuality
CGContextSetInterpolationQuality
|#

;;; Shadow support
#|
CGContextSetShadowWithColor
CGContextSetShadow
|#

(defun context-set-shadow (context dx dy blur)
  (rlet ((offset #>CGSize :width (cgfloat dx) :height (cgfloat dy)))
    (#_CGContextSetShadow context offset (cgfloat blur))))

;;; Gradient and shading functions
#|
CGContextDrawLinearGradient
CGContextDrawRadialGradient
CGContextDrawShading
|#

;;; Text functions
(defun context-set-character-spacing (context spacing)
  (#_CGContextSetCharacterSpacing context (cgfloat spacing)))

(defun context-set-text-position (context x y)
  (#_CGContextSetTextPosition context (cgfloat x) (cgfloat y)))

(defun context-get-text-position (context)
  (let ((pt (make-gcable-record #>CGPoint)))
    (#_CGContextGetTextPosition pt context)))

#|
CGContextSetTextMatrix
CGContextGetTextMatrix
|#
(defun context-set-text-drawing-mode (context mode)
  (let ((code (case mode
                (:fill #$kCGTextFill)
                (:stroke #$kCGTextStroke)
                (:fill-stroke #$kCGTextFillStroke)
                (:invisible #$kCGTextInvisible)
                (:fill-clip #$kCGTextFillClip)
                (:stroke-clip #$kCGTextStrokeClip)
                (:fill-stroke-clip #$kCGTextFillStrokeClip)
                (:clip #$kCGTextClip)
                (otherwise mode))))
    (#_CGContextSetTextDrawingMode context code)))

#|
CGContextSetFont
CGContextSetFontSize
|#

(defun context-select-font (context font-name size encoding)
  (let ((code (case encoding
		(:macroman #$kCGEncodingMacRoman)
		(:font-specific #$kCGEncodingFontSpecific)
		(otherwise encoding))))
    (with-cstrs ((s font-name))
      (#_CGContextSelectFont context s (cgfloat size) code))))
#|
CGContextShowGlyphsAtPositions
|#

;;; Text convenience functions
(defun context-show-text (context string)
  (let ((n (string-size-in-octets string :external-format :macroman)))
    (with-encoded-cstrs :macroman ((s string))
      (#_CGContextShowText context s n))))

(defun context-show-text-at-point (context x y string)
  (let ((n (string-size-in-octets string :external-format :macroman)))
    (with-encoded-cstrs :macroman ((s string))
      (#_CGContextShowTextAtPoint context (cgfloat x) (cgfloat y) s n))))

#|
CGContextShowGlyphs
CGContextShowGlyphsAtPoint
CGContextShowGlyphsWithAdvances
|#

;;; PDF functions
#|
CGContextDrawPDFPage
|#

;;; Output page functions
#|
CGContextBeginPage
CGContextEndPage
|#

#|
CGContextFlush
CGContextSynchronize
|#

;;; Antialiasing functions
#|
CGContextSetShouldAntialias
CGContextSetAllowsAntialiasing
|#

;;; Font display functions
#|
CGContextSetShouldSmoothFonts
CGContextSetAllowsFontSmoothing
CGContextSetShouldSubpixelPositionFonts
CGContextSetAllowsFontSubpixelPositioning
CGContextSetShouldSubpixelQuantizeFonts
CGContextSetAllowsFontSubpixelQuantization
|#

;;; Transparency layer support
#|
CGContextBeginTransparencyLayer
CGContextBeginTransparencyLayerWithRect
CGContextEndTransparencyLayer
|#

;;; User space to device space transformations
#|
CGContextGetUserSpaceToDeviceSpaceTransform
CGContextConvertPointToDeviceSpace
CGContextConvertPointToUserSpace
CGContextConvertSizeToDeviceSpace
CGContextConvertSizeToUserSpace
CGContextConvertRectToDeviceSpace
CGContextConvertRectToUserSpace
|#


;;; CGPath

#|
CGPathCreateMutable
CGPathCreateCopy
CGPathCreateCopyByTransformingPath
CGPathCreateMutableCopy
CGPathCreateMutableCopyByTransformingPath
CGPathCreateWithRect
CGPathCreateWithEllipseInRect
CGPathCreateCopyByDashingPath
CGPathCreateCopyByStrokingPath
CGPathEqualToPath

|#

(defun path-move-to-point (path transform x y)
  (when (null transform) (setq transform +null-ptr+))
  (#_CGPathMoveToPoint path transform (cgfloat x) (cgfloat y)))

(defun path-add-line-to-point (path transform x y)
  (when (null transform) (setq transform +null-ptr+))
  (#_CGPathAddLineToPoint path transform (cgfloat x) (cgfloat y)))

#|
CGPathAddQuadCurveToPoint
CGPathAddCurveToPoint
CGPathCloseSubpath
CGPathAddRect
CGPathAddRects
CGPathAddLines
CGPathAddEllipseInRect
CGPathAddRelativeArc
CGPathAddArc
CGPathAddArcToPoint
CGPathAddPath
CGPathIsEmpty
CGPathIsRect
CGPathGetCurrentPoint
CGPathGetBoundingBox
CGPathGetPathBoundingBox
CGPathContainsPoint
|#

(defun color-create-generic-rgb (r g b a)
  (#_CGColorCreateGenericRGB (cgfloat r) (cgfloat g) (cgfloat b) (cgfloat a)))
