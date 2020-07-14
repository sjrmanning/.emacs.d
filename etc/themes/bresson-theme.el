(require 'solarized)
(deftheme bresson "Bresson theme leveraging solarized underneath.")
(solarized-with-color-variables 'light 'bresson
  '(
    (base03      . "#002b36")
    (base02      . "#073642")
    ;; (base01      . "#586e75")
    (base01      . "#435459")
    ;; (base00      . "#657b83")
    (base00      . "#455459") ;;35
    (base0       . "#839496")
    (base1       . "#93a1a1")
    (base2       . "#ededdf")
    (base3       . "#fefef0")
    (yellow      . "#b58900")
    (orange      . "#cb4b16")
    ;; (red         . "#dc322f")
    (red         . "#ee695e")
    (magenta     . "#d33682")
    (violet      . "#6c71c4")
    (blue        . "#0048a6")
    (cyan        . "#2aa198")
    (green       . "#859900")
    (yellow-1bg  . "#f8e8c6")
    (yellow-1fg  . "#876d26")
    (yellow-2bg  . "#f1d49b")
    (yellow-2fg  . "#766634")
    (yellow-d    . "#866300")
    (yellow-l    . "#e1af4b")
    (orange-1bg  . "#fedfc5")
    (orange-1fg  . "#974727")
    (orange-2bg  . "#ffbd99")
    (orange-2fg  . "#854a33")
    (orange-d    . "#992700")
    (orange-l    . "#fb7640")
    ;; (red-1bg     . "#ffdec8") ;; diff red bg
    ;; (red-1fg     . "#a33c35") ;; diff red text
    (red-1bg     . "#fecdc0") ;; diff red bg
    (red-1fg     . "#622f2b") ;; diff red text
    ;; (red-2bg     . "#ffb9a1") ;; diff refine bg
    ;; (red-2fg     . "#8e433d") ;; diff refine fg
    (red-2bg     . "#ff8e82") ;; diff refine bg
    (red-2fg     . "#622f2b") ;; diff refine fg
    (red-d       . "#a7020a")
    (red-l       . "#ff6849")
    (magenta-1bg . "#fdded7")
    (magenta-1fg . "#9a3f6c")
    (magenta-2bg . "#fdbac6")
    (magenta-2fg . "#854568")
    (magenta-d   . "#a00559")
    (magenta-l   . "#ff699e")
    (violet-1bg  . "#ebe4e2")
    (violet-1fg  . "#4f5e99")
    (violet-2bg  . "#d1c9e3")
    (violet-2fg  . "#475a8b")
    (violet-d    . "#243e9b")
    (violet-l    . "#8d85e7")
    (blue-1bg    . "#e7e8e4")
    (blue-1fg    . "#1e6fa2")
    (blue-2bg    . "#c3d5e9")
    (blue-2fg    . "#246792")
    (blue-d      . "#0061a8")
    (blue-l      . "#74adf5")
    (cyan-1bg    . "#e4ecda")
    (cyan-1fg    . "#207e7b")
    (cyan-2bg    . "#bedfcf")
    (cyan-2fg    . "#247374")
    (cyan-d      . "#007d76")
    (cyan-l      . "#6ccec0")
    ;; (green-1bg   . "#efeac7") ;; diff green background
    ;; (green-1fg   . "#657827") ;; diff green text
    (green-1bg   . "#cbf1c5") ;; diff green background
    (green-1fg   . "#2d5430") ;; diff green text
    ;; (green-2bg   . "#dbdb9c") ;; diff highlight bg
    ;; (green-2fg   . "#5b6e35") ;; diff highlight fg
    (green-2bg   . "#89e28d") ;; diff highlight bg
    (green-2fg   . "#2d5430") ;; diff highlight fg
    (green-d     . "#5b7300")
    (green-l     . "#b3c34d"))
  'nil)
(provide-theme 'bresson)
(provide 'bresson-theme)
