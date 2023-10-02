;;; golang.el -*- lexical-binding: t; -*-

(add-hook 'before-save-hook #'gofmt-before-save)
