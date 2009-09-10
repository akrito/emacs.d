;-*-emacs-lisp-*-

(require 'bbdb-wl)
(bbdb-wl-setup)

(setq
  elmo-maildir-folder-path "~/Maildir"          ;; where i store my mail

  wl-stay-folder-window nil                       ;; show the folder pane (left)
  wl-folder-window-width 25                     ;; toggle on/off with 'i'

  wl-smtp-connection-type 'starttls
  wl-smtp-posting-port 587
  wl-smtp-authenticate-type "plain"
  wl-smtp-posting-user "alex.kritikos@gmail.com"
  wl-smtp-posting-server "smtp.gmail.com"
  wl-local-domain "gmail.com"

  wl-folder-check-async t
  wl-local-domain "8bitb.us"          ;; put something here...
  wl-message-id-domain "8bitb.us"     ;; ...

  wl-from "Alex Kritikos <alex.kritikos@gmail.com>"                  ;; my From:

  ;; note: all below are dirs (Maildirs) under elmo-maildir-folder-path
  ;; the '.'-prefix is for marking them as maildirs
  ;;wl-fcc ".sent"                       ;; sent msgs go to the "sent"-folder
  ;;wl-fcc-force-as-read t               ;; mark sent messages as read
  wl-default-spec "."
  wl-default-folder ".INBOX"           ;; my main inbox
  wl-draft-folder ".Drafts"            ;; store drafts in 'postponed'
  wl-trash-folder ".Trash"             ;; put trash in 'trash'
  wl-spam-folder ".Trash"              ;; ...spam as well
  wl-queue-folder ".Queue"             ;; we don't use this

  ;; check this folder periodically, and update modeline
  wl-biff-check-folder-list '(".INBOX") ;; check every 180 seconds
                                        ;; (default: wl-biff-check-interval)
  ;; hide many fields from message buffers
  wl-message-ignored-field-list '("^.*:")
  wl-message-visible-field-list
  '("^\\(To\\|Cc\\):"

    "^Subject:"
    "^\\(From\\|Reply-To\\):"
    ;;    "^Organization:"
    ;;    "^Message-Id:"
    "^\\(Posted\\|Date\\):"
    )
  wl-message-sort-field-list
  '("^From"

    ;;    "^Organization:"
    ;;    "^X-Attribution:"
     "^Subject"
     "^Date"
     "^To")
  ;; get addresses only from these folders
  bbdb-wl-folder-regexp "^.Sent\\|^.INBOX"
  mime-button-face 'shadow)

(define-key wl-draft-mode-map (kbd "<tab>") 'bbdb-complete-name)
