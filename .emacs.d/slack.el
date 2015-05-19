(require 'tls)
(require 'erc)

(require 'erc-log)
(setq erc-enable-logging t)
(setq erc-log-channels-directory "~/.erc-logs")
(setq erc-save-buffer-on-part nil erc-save-queries-on-quit nil erc-log-write-after-send t erc-log-write-after-insert t)

(defun slack-me ()
  "Connect to IRC."
  (interactive)
  (erc-tls :server "rentalocalfriend.irc.slack.com" :port 6697
           :password "rentalocalfriend.HxjSjyceXk0RU2lOQb2l"
           :nick "guerrinha" :full-name "guerrinha")
  (setq erc-autojoin-channels-alist '(
                                      ("rentalocalfriend.irc.slack.com" "#support"))))
