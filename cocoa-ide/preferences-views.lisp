(in-package "GUI")

(defun %checkbox-button (frame title)
  (let ((checkbox (#/initWithFrame: (#/alloc ns:ns-button) frame)))
    (#/setButtonType: checkbox #$NSSwitchButton)
    (with-cfstring (s title)
      (#/setTitle: checkbox s))
    checkbox))

(defun %small-checkbox-button (frame title)
  (let* ((checkbox (%checkbox-button frame title))
         (cell (#/cell checkbox))
         (font (#/systemFontOfSize: ns:ns-font
                                    (#/systemFontSizeForControlSize:
                                     ns:ns-font #$NSSmallControlSize))))
    (#/setControlSize: cell #$NSSmallControlSize)
    (#/setFont: cell font)
    checkbox))

(defun %push-button (frame title)
  (let* ((button (#/initWithFrame: (#/alloc ns:ns-button) frame)))
    (#/setBezelStyle: button #$NSRoundedBezelStyle)
    (with-cfstring (s title)
      (#/setTitle: button s))
    button))

(defun %small-push-button (frame title)
  (let* ((button (%push-button frame title))
         (cell (#/cell button))
         (font (#/systemFontOfSize: ns:ns-font
                                    (#/systemFontSizeForControlSize:
                                     ns:ns-font #$NSSmallControlSize))))
    (#/setControlSize: cell #$NSSmallControlSize)
    (#/setFont: cell font)
    button))

(defun %text-label (frame title)
  (let* ((label (#/initWithFrame: (#/alloc ns:ns-text-field) frame))
         (cell (#/cell label)))
    (#/setStringValue: label title)
    (#/setDrawsBackground: cell nil)
    (#/setBordered: cell nil)
    (#/setEditable: cell nil)
    (#/setSelectable: cell nil)
    label))

(defun %small-text-label (frame title)
  (let* ((label (%text-label frame title))
         (cell (#/cell label))
         (font (#/systemFontOfSize: ns:ns-font
                                    (#/systemFontSizeForControlSize:
                                     ns:ns-font #$NSSmallControlSize))))
    (#/setControlSize: cell #$NSSmallControlSize)
    (#/setFont: cell font)
    label))

(defun %small-text-field (frame)
  (let* ((field (#/initWithFrame: (#/alloc ns:ns-text-field) frame))
         (cell (#/cell field))
         (font (#/systemFontOfSize: ns:ns-font
                                    (#/systemFontSizeForControlSize:
                                     ns:ns-font #$NSSmallControlSize))))
    (#/setControlSize: cell #$NSSmallControlSize)
    (#/setFont: cell font)
    field))
    
(defun %preferences-general-view ()
  (cg:with-rects ((frame 0 0 544 106)
                  (meta-checkbox-frame 18 70 169 18)
                  (hyperspec-checkbox-frame 18 48 182 18)
                  (label-frame 27 22 33 17)
                  (text-field-frame 65 20 459 22))
    (let ((view (#/initWithFrame: (#/alloc ns:ns-view) frame))
          (meta-checkbox (%checkbox-button meta-checkbox-frame
                                           "Use option key as meta"))
          (hyperspec-checkbox (%checkbox-button hyperspec-checkbox-frame
                                           "Enable HyperSpec lookup"))
          (label (%text-label label-frame #@"URL:"))
          (text-field (#/initWithFrame: (#/alloc ns:ns-text-field)
                                        text-field-frame))
          (font (#/systemFontOfSize: ns:ns-font
                                     (#/systemFontSizeForControlSize:
                                      ns:ns-font #$NSRegularControlSize)))
          (dc (#/sharedUserDefaultsController ns:ns-user-defaults-controller)))
      (#/setFont: (#/cell meta-checkbox) font)
      (#/addSubview: view meta-checkbox)
      (#/bind:toObject:withKeyPath:options: meta-checkbox #@"value"
                                            dc
                                            #@"values.optionIsMeta"
                                            +null-ptr+)
      (#/release meta-checkbox)
      (#/setFont: (#/cell hyperspec-checkbox) font)
      (#/addSubview: view hyperspec-checkbox)
      (#/bind:toObject:withKeyPath:options: hyperspec-checkbox #@"value"
                                            dc
                                            #@"values.hyperspecLookupEnabled"
                                            +null-ptr+)
      (#/release hyperspec-checkbox)
      (#/addSubview: view label)
      (#/setFont: (#/cell label) font)
      (#/release label)
      (#/setFont: (#/cell text-field) font)
      (#/addSubview: view text-field)
      (#/bind:toObject:withKeyPath:options: text-field #@"value"
                                            dc
                                            #@"values.hyperspecURLString"
                                            +null-ptr+)
      (#/bind:toObject:withKeyPath:options: text-field #@"editable"
                                            dc
                                            #@"values.hyperspecLookupEnabled"
                                            +null-ptr+)
      (#/bind:toObject:withKeyPath:options: text-field #@"enabled"
                                            dc
                                            #@"values.hyperspecLookupEnabled"
                                            +null-ptr+)
      (#/release text-field)
      view)))

(defun %appearance-editor-box (frame)
  (let ((box (#/initWithFrame: (#/alloc ns:ns-box) frame))
        (dc (#/sharedUserDefaultsController ns:ns-user-defaults-controller)))
    (cg:with-rects ((default-font-label-frame 15 91 74 14)
                    (change-button-frame 89 83 77 28)
                    (font-name-label-frame 166 91 310 14))
      (let ((default-font-label (%small-text-label default-font-label-frame
                                                   #@"Default Font:"))
            (change-button (%small-push-button change-button-frame
                                               "Change..."))
            (font-name-label (%small-text-label font-name-label-frame
                                                #@"font name")))
        (#/addSubview: box default-font-label)
        (#/release default-font-label)
        (#/addSubview: box change-button)
        (#/release change-button)
        (let ((options (#/dictionaryWithObject:forKey:
                      ns:ns-dictionary
                      #@"FontToName"
                      #&NSValueTransformerNameBindingOption)))
          (#/bind:toObject:withKeyPath:options: font-name-label #@"value"
                                                dc
                                                #@"values.editorFont"
                                                options))
        (#/addSubview: box font-name-label)
        (#/release font-name-label)))
    (cg:with-rects ((columns-label-frame 126 61 55 14)
                    (rows-label-frame 145 36 36 14)
                    (columns-field-frame 186 59 40 19)
                    (rows-field-frame 186 34 40 19))
      (let ((columns-label (%small-text-label columns-label-frame #@"Columns:"))
            (rows-label (%small-text-label rows-label-frame #@"Rows:"))
            (columns-field (%small-text-field columns-field-frame))
            (rows-field (%small-text-field rows-field-frame)))
        (#/addSubview: box columns-label)
        (#/release columns-label)
        (#/addSubview: box rows-label)
        (#/release rows-label)
        (#/bind:toObject:withKeyPath:options: columns-field #@"value"
                                              dc
                                              #@"values.editorColumns"
                                              +null-ptr+)
        (#/addSubview: box columns-field)
        (#/release columns-field)
        (#/bind:toObject:withKeyPath:options: rows-field #@"value"
                                              dc
                                              #@"values.editorRows"
                                              +null-ptr+)
        (#/addSubview: box rows-field)
        (#/release rows-field)))
    (cg:with-rects ((background-label-frame 258 61 71 14)
                    (background-color-well-frame 334 58 28 20))
      (let ((background-label (%small-text-label background-label-frame
                                                 #@"Background:"))
            (background-color-well (#/initWithFrame:
                                    (#/alloc ns:ns-color-well)
                                    background-color-well-frame))
            (options (#/dictionaryWithObject:forKey:
                      ns:ns-dictionary
                      #@"NSKeyedUnarchiveFromData"
                      #&NSValueTransformerNameBindingOption)))
        (#/addSubview: box background-label)
        (#/release background-label)
        (#/bind:toObject:withKeyPath:options: background-color-well #@"value"
                                              dc
                                              #@"values.editorBackgroundColor"
                                              options)
        (#/addSubview: box background-color-well)
        (#/release background-color-well)))
    (cg:with-rects ((wrap-checkbox-frame 202 6 140 18))
      (let ((wrap-checkbox (%small-checkbox-button wrap-checkbox-frame
                                                   "Wrap lines to window")))
        (#/bind:toObject:withKeyPath:options: wrap-checkbox #@"value"
                                              dc
                                              #@"values.wrapLinesToWindow"
                                              +null-ptr+)
        (#/addSubview: box wrap-checkbox)
        (#/release wrap-checkbox)))
    box))

(defun %appearance-listener-box (frame)
  (let ((box (#/initWithFrame: (#/alloc ns:ns-box) frame))
        (dc (#/sharedUserDefaultsController ns:ns-user-defaults-controller)))
    (cg:with-rects ((input-font-label-frame 26 138 63 14)
                    (change-input-button-frame 89 130 77 28)
                    (input-font-name-label-frame 166 138 310 14)
                    (output-font-label-frame 15 113 74 14)
                    (change-output-button-frame 89 105 77 28)
                    (output-font-name-label-frame 166 113 310 14))
      (let ((input-font-label (%small-text-label input-font-label-frame
                                                   #@"Input Font:"))
            (change-input-button (%small-push-button change-input-button-frame
                                                     "Change..."))
            (input-font-name-label (%small-text-label
                                    input-font-name-label-frame
                                    #@"font name"))
            (output-font-label (%small-text-label output-font-label-frame
                                                 #@"Output Font:"))
            (change-output-button (%small-push-button change-output-button-frame
                                                     "Change..."))
            (output-font-name-label (%small-text-label
                                    output-font-name-label-frame
                                    #@"font name"))
            (options (#/dictionaryWithObject:forKey:
                      ns:ns-dictionary
                      #@"FontToName"
                      #&NSValueTransformerNameBindingOption)))
        (#/addSubview: box input-font-label)
        (#/release input-font-label)
        (#/addSubview: box change-input-button)
        (#/release change-input-button)
        (#/bind:toObject:withKeyPath:options: input-font-name-label #@"value"
                                              dc
                                              #@"values.listenerInputFont"
                                              options)
        (#/addSubview: box input-font-name-label)
        (#/release input-font-name-label)
        (#/addSubview: box output-font-label)
        (#/release output-font-label)
        (#/addSubview: box change-output-button)
        (#/release change-output-button)
        (#/bind:toObject:withKeyPath:options: output-font-name-label #@"value"
                                              dc
                                              #@"values.listenerOutputFont"
                                              options)
        (#/addSubview: box output-font-name-label)
        (#/release output-font-name-label)))
    (cg:with-rects ((columns-label-frame 126 70 55 14)
                    (rows-label-frame 145 45 36 14)
                    (columns-field-frame 186 68 40 19)
                    (rows-field-frame 186 43 40 19))
      (let ((columns-label (%small-text-label columns-label-frame #@"Columns:"))
            (rows-label (%small-text-label rows-label-frame #@"Rows:"))
            (columns-field (%small-text-field columns-field-frame))
            (rows-field (%small-text-field rows-field-frame)))
        (#/addSubview: box columns-label)
        (#/release columns-label)
        (#/addSubview: box rows-label)
        (#/release rows-label)
        (#/bind:toObject:withKeyPath:options: columns-field #@"value"
                                              dc
                                              #@"values.listenerColumns"
                                              +null-ptr+)
        (#/addSubview: box columns-field)
        (#/release columns-field)
        (#/bind:toObject:withKeyPath:options: rows-field #@"value"
                                              dc
                                              #@"values.listenerRows"
                                              +null-ptr+)
        (#/addSubview: box rows-field)
        (#/release rows-field)))
    (cg:with-rects ((background-label-frame 258 70 71 14)
                    (background-color-well-frame 334 67 28 20))
      (let ((background-label (%small-text-label background-label-frame
                                                 #@"Background:"))
            (background-color-well (#/initWithFrame:
                                    (#/alloc ns:ns-color-well)
                                    background-color-well-frame))
            (options (#/dictionaryWithObject:forKey:
                      ns:ns-dictionary
                      #@"NSKeyedUnarchiveFromData"
                      #&NSValueTransformerNameBindingOption)))
        (#/addSubview: box background-label)
        (#/release background-label)
        (#/bind:toObject:withKeyPath:options: background-color-well #@"value"
                                              dc
                                              #@"values.listenerBackgroundColor"
                                              options)
        (#/addSubview: box background-color-well)
        (#/release background-color-well)))
    (cg:with-rects ((ro-checkbox-frame 205 6 123 18))
      (let ((ro-checkbox (%small-checkbox-button ro-checkbox-frame
                                                   "Read-only Listener")))
        (#/bind:toObject:withKeyPath:options: ro-checkbox #@"value"
                                              dc
                                              #@"values.readOnlyListener"
                                              +null-ptr+)
        (#/addSubview: box ro-checkbox)
        (#/release ro-checkbox)))
    box))

(defun %appearance-other-box (frame)
  (let ((box (#/initWithFrame: (#/alloc ns:ns-box) frame))
        (dc (#/sharedUserDefaultsController ns:ns-user-defaults-controller)))
    (cg:with-rects ((screen-fonts-checkbox-frame 123 6 237 18))
      (let ((screen-fonts-checkbox (%small-checkbox-button
                                    screen-fonts-checkbox-frame
                                    "Use bitmap screen fonts, when available")))
        (#/bind:toObject:withKeyPath:options: screen-fonts-checkbox #@"value"
                                              dc
                                              #@"values.useScreenFonts"
                                              +null-ptr+)
        (#/addSubview: box screen-fonts-checkbox)
        (#/release screen-fonts-checkbox)))
    box))

(defun %preferences-appearance-view ()
  (cg:with-rects ((frame 0 0 507 409)
                  (editor-box-frame 7 264 493 137)
                  (listener-box-frame 7 76 493 184)
                  (other-box-frame 7 18 493 54))
    (let ((view (#/initWithFrame: (#/alloc ns:ns-view) frame))
          (editor-box (%appearance-editor-box editor-box-frame))
          (listener-box (%appearance-listener-box listener-box-frame))
          (other-box (%appearance-other-box other-box-frame)))
      (#/setTitle: editor-box #@"Editor")
      (#/addSubview: view editor-box)
      (#/release editor-box)
      
      (#/setTitle: listener-box #@"Listener")
      (#/addSubview: view listener-box)
      (#/release listener-box)

      (#/setTitle: other-box #@"Other")
      (#/addSubview: view other-box)
      (#/release other-box)
      view)))

(defun %preferences-panel ()
  (ns:with-ns-rect (r 0 0 467 312)
    (let ((panel (#/initWithContentRect:styleMask:backing:defer:
		  (#/alloc ns:ns-panel) r (logior #$NSTitledWindowMask
						  #$NSClosableWindowMask)
                  #$NSBackingStoreBuffered nil)))
      (#/setShowsToolbarButton: panel nil)
      panel)))
