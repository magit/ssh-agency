;;; ssh-agency.el --- use ssh-agent on win32 from Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2015  The Magit Project Contributors

;; Author: Noam Postavsky <npostavs@user.sourceforge.net>
;; Maintainer: Noam Postavsky <npostavs@user.sourceforge.net>

;; Package-Requires: ((emacs "24.4") (dash "2.10.0"))
;; URL: https://github.com/magit/ssh-agency
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides functions to startup ssh-agent, prompt for
;; passphrases from the Windows console, and set the needed
;; environment variables in Emacs, so that pushes and pulls from magit
;; will not require entering any passphrase.

;;; Code:

(require 'dash)

;;; Options

(defgroup ssh-agency ()
  "Using ssh-agent on `windows-nt'.")

(defcustom ssh-agency-bin-dir
  (when (eq system-type 'windows-nt)
    ;; Git for Windows keeps ssh exes in its bin/ directory.
    (-when-let* ((git-exe (executable-find "git.exe"))
                 (git-dir (directory-file-name (file-name-directory git-exe))))
      (if (equal (file-name-nondirectory git-dir) "cmd")
          (expand-file-name "bin" (file-name-directory git-dir))
        git-dir)))
  "Where to look for ssh executables."
  :group 'ssh-agency
  :type 'directory)

(defcustom ssh-agency-add-executable
  (if ssh-agency-bin-dir
      (expand-file-name "ssh-add.exe" ssh-agency-bin-dir)
    (executable-find "ssh-add"))
  "Location of ssh-add executable."
  :group 'ssh-agency
  :type '(file :must-match t))

(defcustom ssh-agency-agent-executable
  (if ssh-agency-bin-dir
      (expand-file-name "ssh-agent.exe" ssh-agency-bin-dir)
    (executable-find "ssh-agent"))
  "Location of ssh-agent execuable."
  :group 'ssh-agency
  :type '(file :must-match t))

(defcustom ssh-agency-home
  ;; Translation of the code in msysgit's /etc/profile.
  (--first (and it (file-directory-p it))
           (list (getenv "HOME")
                 (ignore-errors (concat (getenv "HOMEDRIVE") (getenv "HOMEPATH")))
                 (getenv "USERPROFILE")))
  "The directory ssh uses as `~' (aka $HOME)."
  :group 'ssh-agency
  :type 'directory)

(defcustom ssh-agency-env-file
  (expand-file-name ".ssh/agent.env" ssh-agency-home)
  "When starting a new agent, write its environment variables to this file.

This is only for the benefit of shells outside of Emacs,
ssh-agency always finds the agent without consulting this file."
  :group 'ssh-agency
  :type 'file)

(defcustom ssh-agency-keys
  (--filter (and (string-match-p "/[^.]+$" it) (ssh-agency-private-key-p it))
            (append (file-expand-wildcards (expand-file-name "~/.ssh/id*"))
                    (unless (equal (file-name-as-directory ssh-agency-home)
                                   (file-name-as-directory (expand-file-name "~")))
                      (file-expand-wildcards
                       (expand-file-name ".ssh/id*" ssh-agency-home)))))
  "A list of key files to be added to the agent.

`nil' indicates the default for `ssh-add' which is ~/.ssh/id_rsa,
~/.ssh/id_dsa, ~/.ssh/id_ecdsa, ~/.ssh/id_ed25519 and
~/.ssh/identity."
  :group 'ssh-agency
  :type '(choice (repeat (file :must-match t))
                 (const nil :tag "ssh-add's default")))

;;; Functions

(defun ssh-agency-private-key-p (keyfile)
  "Return non-nil if KEYFILE designates a private key."
  (with-temp-buffer
    (insert-file-contents-literally keyfile)
    (goto-char 1)
    (looking-at-p "\\`.*BEGIN.*PRIVATE KEY.*$")))

(defun ssh-agency-add-keys (keys)
  "Add keys to ssh-agent."
  (call-process-shell-command
   ;; Passphrase can only be entered in console, so use cmd.exe's
   ;; `start' to get one. Quoting both the executable and the first
   ;; argument breaks Windows' argument parsing, so we use the short
   ;; name for the executable instead of quoting it.
   (concat "start /WAIT \"ssh-add\" " (w32-short-file-name ssh-agency-add-executable)
           ;; When the argument is quoted `ssh-add' doesn't recognize
           ;; file abbreviations like `~', so expand first (also, it's
           ;; possible that Emacs and `ssh-add' will have different
           ;; ideas about what `~' means).
           " " (mapconcat (lambda (key) (shell-quote-argument (expand-file-name key)))
                          keys " "))))

(defun ssh-agency-start-agent ()
  "Start ssh-agent, and set corresponding environment vars.

Return the `ssh-agency-status' of the new agent, i.e. `no-keys'."
  (with-temp-buffer
    (call-process ssh-agency-agent-executable nil '(t t))
    (goto-char 1)
    (while (re-search-forward "^\\(SSH_[^=]+\\)=\\([^;]+\\)" nil t)
      (setenv (match-string 1) (match-string 2)))
    (when ssh-agency-agent-executable
      (write-file ssh-agency-env-file)))
  'no-keys)

(defun ssh-agency-find-agent ()
  "Find a running agent.

If an agent is found, set the corresponding environment vars.
Return `ssh-agency-status' of the agent."
  (let* ((status nil)
         (pid
          (--first (-let (((&alist 'comm comm 'user user) (process-attributes it)))
                     (and (string= comm "ssh-agent.exe")
                          (string= user user-login-name)))
                   (list-system-processes)))
         (sock
          (when pid
            (catch 'ssh-sock
              (dolist (sock-dir (directory-files temporary-file-directory t "\\`ssh-" t))
                (dolist (sock-file (directory-files sock-dir t "\\`agent\.[0-9]+\\'" t))
                  ;; Follow the lead of msysgit's start-ssh-agent.cmd: replace %TEMP% with "/tmp".
                  (setq sock-file (replace-regexp-in-string
                                   (regexp-quote temporary-file-directory)
                                   "/tmp/" sock-file))
                  (let ((process-environment (cons (concat "SSH_AUTH_SOCK=" sock-file)
                                                   process-environment)))
                    (when (setq status (ssh-agency-status))
                      (throw 'ssh-sock sock-file)))))))))
    (when (and pid sock)
      (setenv "SSH_AGENT_PID" (number-to-string pid))
      (setenv "SSH_AUTH_SOCK" sock))
    status))

(defun ssh-agency-status ()
  "Check the status of the ssh-agent.

Return `t' if agent has keys, `nil' if no agent found, or
`no-keys' if agent is running but has no keys."
  (pcase (call-process ssh-agency-add-executable nil nil nil "-l")
    (0 t)
    (1 'no-keys)
    (_ nil)))

;;;###autoload
(defun ssh-agency-ensure (&rest _)
  "Start ssh-agent and add keys, as needed.

Intended to be used as advice for magit functions that initiate
remote operations."
  (when (eq (or (ssh-agency-status)
                (ssh-agency-find-agent)
                (ssh-agency-start-agent))
            'no-keys)
    (ssh-agency-add-keys ssh-agency-keys)))

;;; Hooking into magit

;;;###autoload
(when (eq system-type 'windows-nt)
  (dolist (sym-fun '(magit-push magit-push-matching magit-push-tag magit-push-tags
                     magit-pull magit-pull-current
                     magit-fetch magit-fetch-current magit-fetch-all))
    (advice-add sym-fun :before #'ssh-agency-ensure)))

(provide 'ssh-agency)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; ssh-agency.el ends here
