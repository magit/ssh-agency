;;; ssh-agency.el --- manage ssh-agent from Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2015-2017  The Magit Project Contributors

;; Author: Noam Postavsky <npostavs@user.sourceforge.net>
;; Maintainer: Noam Postavsky <npostavs@user.sourceforge.net>

;; Package-Requires: ((emacs "24.4") (dash "2.10.0"))
;; URL: https://github.com/magit/ssh-agency
;; Version: 0.3

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
;;
;; It can also be useful on Unix-like platforms to delay having to
;; enter your passphrase until the first time you push to a remote.

;;; Code:

(require 'dash)

;;; Options

(defgroup ssh-agency ()
  "Managing ssh-agent from Emacs."
  :group 'comm)

(defcustom ssh-agency-bin-dir
  (when (eq system-type 'windows-nt)
    ;; Git for Windows 1.x keeps ssh exes in its bin/ directory.
    (-when-let* ((git-exe (executable-find "git.exe"))
                 (git-dir (directory-file-name (file-name-directory git-exe))))
      (if (equal (file-name-nondirectory git-dir) "cmd")
          (expand-file-name "bin" (file-name-directory git-dir))
        git-dir)))
  "Where to look for ssh executables."
  :group 'ssh-agency
  :type 'directory)

(defun ssh-agency-executable-find (exe)
  "Computes default value for `ssh-agency-EXE-executable'."
  (or (--when-let
          (ignore-errors
            ;; This will fail on Windows Git 1.x because it doesn't
            ;; handle upper case aliases.  This is good because then
            ;; we won't find and fail to use git-gui--askpass for
            ;; SSH_ASKPASS.
            (car (process-lines
                  "git" "-c" "alias.X=!x() { which \"$1\" | cygpath -mf -; }; x"
                  "X" exe)))
        ;; Note: filename *must* include ".exe" suffix (if any) or
        ;; `w32-short-file-name' returns nil.
        (or (executable-find it) it))
      (if ssh-agency-bin-dir
          (let ((bin (expand-file-name exe ssh-agency-bin-dir)))
            (and (file-executable-p bin) bin)))
      (executable-find exe)))

(defcustom ssh-agency-add-executable
  (ssh-agency-executable-find "ssh-add")
  "Location of ssh-add executable."
  :group 'ssh-agency
  :type '(file :must-match t))

(defcustom ssh-agency-agent-executable
  (ssh-agency-executable-find "ssh-agent")
  "Location of ssh-agent execuable."
  :group 'ssh-agency
  :type '(file :must-match t))

(defvar ssh-agency-gui-askpass-env nil)

(defcustom ssh-agency-gui-askpass-executable
  (when (eq system-type 'windows-nt)
    (-when-let* ((exe (ssh-agency-executable-find "git-gui--askpass"))
                 (path (ignore-errors
                         (car (process-lines
                               "git" "-c" "alias.X=!echo \"$PATH\"" "X")))))
      (setq ssh-agency-gui-askpass-env
            (list "DISPLAY=t" (concat "SSH_ASKPASS=" exe)
                  (concat "PATH=" path)))
      exe))
  "Location of SSH_ASKPASS executable.

This is only needed on `windows-nt' systems to compensate for the
lack of PTYs."
  :group 'ssh-agency
  :type '(choice (file :must-match t) (const nil)))

(defcustom ssh-agency-env-file
  (expand-file-name "~/.ssh/agent.env")
  "When starting a new agent, write its environment variables to this file.

This is only for the benefit of shells outside of Emacs,
ssh-agency always finds the agent without consulting this file."
  :group 'ssh-agency
  :type 'file)

;; This function must be defined before `ssh-agency-keys', because it
;; is used to define the default value.
(defun ssh-agency-private-key-p (keyfile)
  "Return non-nil if KEYFILE designates a private key."
  (with-temp-buffer
    (insert-file-contents-literally keyfile)
    (goto-char 1)
    (looking-at-p "\\`.*BEGIN.*PRIVATE KEY.*$")))

(defcustom ssh-agency-keys
  (--filter (and (string-match-p "/[^.]+$" it) (ssh-agency-private-key-p it))
            (file-expand-wildcards (expand-file-name "~/.ssh/id*")))
  "A list of key files to be added to the agent.

`nil' indicates the default for `ssh-add' which is ~/.ssh/id_rsa,
~/.ssh/id_dsa, ~/.ssh/id_ecdsa, ~/.ssh/id_ed25519 and
~/.ssh/identity."
  :group 'ssh-agency
  :type '(choice (repeat (file :must-match t))
                 (const nil :tag "ssh-add's default")))

;;; Functions

(defun ssh-agency-add-keys (keys)
  "Add keys to ssh-agent."
  (setq keys (mapcar #'expand-file-name keys))
  (let ((ssh-add (if (fboundp 'w32-short-file-name)
                     ;; Using short filename to avoid Emacs bug#8541.
                     (w32-short-file-name ssh-agency-add-executable)
                   ssh-agency-add-executable)))
    (cond
     (ssh-agency-gui-askpass-executable
      (let ((process-environment (append ssh-agency-gui-askpass-env
                                         process-environment)))
        (apply #'call-process ssh-add nil nil nil keys)))
     ((eq system-type 'windows-nt)
      (call-process-shell-command
       ;; Git 1.x: Passphrase can only be entered in console, so use
       ;; cmd.exe's `start' to get one.
       (concat "start \"ssh-add\" /WAIT " ssh-add " "
               (mapconcat #'shell-quote-argument keys " "))))
     (t (apply #'call-process ssh-add nil nil nil keys)))))

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
         (ssh-agent-exe (if (eq system-type 'windows-nt)
                            "ssh-agent.exe" "ssh-agent"))
         (pid
          (--first (-let (((&alist 'comm comm 'user user) (process-attributes it)))
                     (and (string= comm ssh-agent-exe)
                          (string= user user-login-name)))
                   (list-system-processes)))
         (sock
          (when pid
            (catch 'ssh-sock
              (dolist (sock-dir (directory-files temporary-file-directory t "\\`ssh-" t))
                (dolist (sock-file (directory-files sock-dir t "\\`agent\.[0-9]+\\'" t))
                  (when (eq system-type 'windows-nt)
                    ;; Follow the lead of msysgit's start-ssh-agent.cmd:
                    ;; replace %TEMP% with "/tmp".
                    (setq sock-file (replace-regexp-in-string
                                     (regexp-quote temporary-file-directory)
                                     "/tmp/" sock-file)))
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
(defun ssh-agency-ensure ()
  "Start ssh-agent and add keys, as needed.

Intended to be added to `magit-credential-hook'."
  (when (eq (or (ssh-agency-status)
                (ssh-agency-find-agent)
                (ssh-agency-start-agent))
            'no-keys)
    (ssh-agency-add-keys ssh-agency-keys)))

;;; Hooking into magit

;;;###autoload
(add-hook 'magit-credential-hook 'ssh-agency-ensure)

(provide 'ssh-agency)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; ssh-agency.el ends here
