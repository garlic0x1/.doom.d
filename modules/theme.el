;;; theme.el -*- lexical-binding: t; -*-

;; choose a random theme at startup and expose functions for switching

(let ((start-theme 'modus-vivendi)
      (known-themes '(doom-rouge
                      doom-palenight
                      doom-monokai-spectrum
                      doom-1337
                      doom-snazzy
                      modus-vivendi
                      deeper-blue
                      tango-dark
                      wombat
                      tsdh-dark
                      wheatgrass))
      (light-themes '(doom-flatwhite
                      doom-opera-light
                      doom-earl-grey
                      doom-gruvbox-light
                      modus-operandi
                      tsdh-light))
      (current-theme nil))

  (defun get-current-theme () (interactive) (print current-theme))

  (defun set-theme (theme)
    (load-theme theme t)
    (setq current-theme theme))

  (defun random-theme+ (themes)
    "set random theme from set"
    (set-theme (random-el (remove current-theme themes))))

  (defun random-theme ()
    "choose a random theme from all available themes"
    (interactive)
    (random-theme+ (custom-available-themes)))

  (defun random-known-theme ()
    "choose a random theme from whitelist"
    (interactive)
    (random-theme+ known-themes))

  (defun random-light-theme ()
    "choose a random light theme from whitelist"
    (interactive)
    (random-theme+ light-themes))

  (set-theme start-theme))
