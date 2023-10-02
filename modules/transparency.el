;;; transparency.el -*- lexical-binding: t; -*-

(let ((transparent? nil))
  (defun transparency (value)
    "Sets the transparency of the frame window. 0=transparent/100=opaque"
    (interactive "nTransparency Value 0 - 100 opaque:")
    (set-frame-parameter (selected-frame) 'alpha value))

  (defun toggle-transparency ()
    (interactive)
    (if transparent?
        (transparency 100)
        (transparency 70))
    (setq transparent? (not transparent?))))
