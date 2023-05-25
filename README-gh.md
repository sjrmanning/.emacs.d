# George's notes and _etc_

Cute hack for getting some unused keys:


``` lisp
(dotimes (n 10)
  (global-unset-key (kbd (format "M-%d" n))))
```

[From here...](https://www.reddit.com/r/emacs/comments/3ricev/tip_for_when_you_are_running_out_of_easytopress/)
