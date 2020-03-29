;;; ssh-agency.el --- manage ssh-agent from Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2015-2017  The Magit Project Contributors

;; Author: Noam Postavsky <npostavs@user.sourceforge.net>
;; Maintainer: Noam Postavsky <npostavs@user.sourceforge.net>

;; Package-Requires: ((emacs "24.4") (dash "2.10.0"))
;; URL: https://github.com/magit/ssh-agency
;; Version: 0.4

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
(eval-when-compile (require 'cl-lib))

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
            ;; This will fail on Windows Git 1.x because it lacks
            ;; `cygpath'.  For that version we search manually anyway.
            (car (process-lines
                  "git" "-c" "alias.x=!x() { which \"$1\" | cygpath -mf -; }; x"
                  "x" exe)))
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

(defcustom ssh-agency-agent-exe-names
  (if (eq system-type 'windows-nt)
      '("ssh-agent.exe")
    ;; gnome-keyring-daemon implements ssh-agent functionality.
    '("ssh-agent" "gnome-keyring-d"))
  "List of possible agent names."
  :group 'ssh-agency
  :type '(repeat string))

(defcustom ssh-agency-askpass
  (when (eq system-type 'windows-nt)
    (-when-let* ((exe (ssh-agency-executable-find "git-gui--askpass"))
                 (path (ignore-errors
                         (car (process-lines
                               "git" "-c" "alias.x=!echo \"$PATH\"" "x")))))
      ;; Windows won't have DISPLAY, but we need to fake it for
      ;; SSH_ASKPASS to be called.
      (list "DISPLAY=t" (concat "SSH_ASKPASS=" exe)
            (concat "PATH=" path))))
  "If non-nil, let ssh-askpass query for the passphrase.
If the value is a list, it should be in the same format as
`process-environment', and specifies what environment variables
to bind while running `ssh-add' (typically specifying a value for
SSH_ASKPASS).

If nil, read the passphrase from Emacs (note, this requires using
a pty, and so will not work on `windows-nt' systems)."
  :group 'ssh-agency
  :type '(choice (const :tag "read passphrase from Emacs" nil)
                 (const :tag "read passphrase with ssh-askpass" t)
                 (repeat string)))

(defcustom ssh-agency-gui-askpass-executable nil
  "Use `ssh-agency-askpass' instead."
  :group 'ssh-agency
  :type '(choice (file :must-match t) (const nil))
  :set (lambda (sym value)
         (when (stringp value)
           (setq-default ssh-agency-askpass
                         (list (concat "SSH_ASKPASS=" value))))
         (set-default sym value)))
(make-obsolete-variable 'ssh-agency-gui-askpass-executable
                        'ssh-agency-askpass "0.4")

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

(defconst ssh-agency-socket-regexp
  ;; Try to match
  ;; - /tmp/ssh-XXXXX/agent.NNN (ssh-agent default)
  ;; - /run/user/[uid]/keyring/ssh (gnome-keyring-daemon 1)
  ;; - ~/.cache/keyring-XXXXX/ssh (gnome-keyring-daemon 2)
  ;; But don't match
  ;; - /run/user/[uid]/gnupg/S.gpg-agent.ssh
  "/\\(?:agent[.][0-9]+\\|ssh\\)\\'")

(defcustom ssh-agency-socket-locaters
  `(,@(when (executable-find "ss")
        `((ssh-agency-find-socket-from-ss
           :glob "*ssh*" :regexp ,ssh-agency-socket-regexp)))
    ,@(when     ; Windows has a netstat command, but it won't help us.
          (and (not (eq system-type 'windows-nt))
               (executable-find "netstat"))
        `((ssh-agency-find-socket-from-netstat
           :regexp ,ssh-agency-socket-regexp)))
    ;; Look in ssh-agent's default location.
    (ssh-agency-find-socket-from-glob
     ,(concat temporary-file-directory "ssh-*/agent.*")))
  "List of (FUN . ARGS) to search for ssh-agent sockets."
  :group 'ssh-agency
  :type '(alist :key-type function))

;;; Functions

(defun ssh-agency-socket-status (socket)
  "Return `ssh-agency-status' of agent corresponding to SOCKET."
  (when (eq system-type 'windows-nt)
    ;; Follow the lead of msysgit's start-ssh-agent.cmd:
    ;; replace %TEMP% with "/tmp".
    (setq socket (replace-regexp-in-string
                  (concat "\\`" (regexp-quote temporary-file-directory))
                  "/tmp/" socket)))
  (let ((process-environment (cons (concat "SSH_AUTH_SOCK=" socket)
                                   process-environment)))
    (ssh-agency-status)))

(cl-defun ssh-agency-find-socket-from-ss (&key glob regexp)
  "Use `ss' to find an ssh-agent socket matching GLOB and/or REGEXP."
  (catch 'socket
    ;; The "--no-header" flag isn't used in order to support older ss versions.
    (dolist (sock-line (cdr (with-demoted-errors "ssh-agency-find-socket: %S"
                              (apply #'process-lines
                                     "ss" "--listening" "--family=unix"
                                     (if glob (list "src" glob))))))
      (let* ((socket (nth 4 (split-string sock-line)))
             (status (and (or (null regexp) (string-match-p regexp socket))
                          (ssh-agency-socket-status socket))))
        (when status
          (throw 'socket (cons status socket)))))))

(cl-defun ssh-agency-find-socket-from-netstat (&key regexp)
  "Use `netstat' to find an ssh-agent socket REGEXP."
  (catch 'socket
    (dolist (sock-line (with-demoted-errors
                           "ssh-agency-find-socket-from-netstat: %S"
                         (process-lines "netstat" "-f" "unix")))
      (let* ((socket (car (last (split-string sock-line))))
             (status (and (or (null regexp) (string-match-p regexp socket))
                          (ssh-agency-socket-status socket))))
        (when status
          (throw 'socket (cons status socket)))))))

(cl-defun ssh-agency-find-socket-from-glob (glob)
  "Find an ssh-agent socket matching GLOB."
  (catch 'socket
    (dolist (socket (file-expand-wildcards glob t))
      (let ((status (ssh-agency-socket-status socket)))
        (when status
          (throw 'socket (cons status socket)))))))

(defun ssh-agency-askpass-filter (proc string)
  (condition-case ()
      (with-current-buffer (process-buffer proc)
        (goto-char (point-max))
        (insert string)                 ; Record all output.
        (when (string-match-p "^.*: *\\'" string)
          (let ((pwd (read-passwd string)))
            (send-string proc pwd)
            (clear-string pwd)
            (send-string proc "\n"))))
    (quit (kill-process proc)
          (process-put proc :user-quit t))))

(defun ssh-agency-add-keys (keys)
  "Add keys to ssh-agent."
  (setq keys (mapcar #'expand-file-name keys))
  (let ((ssh-add (if (fboundp 'w32-short-file-name)
                     ;; Using short filename to avoid Emacs bug#8541.
                     (w32-short-file-name ssh-agency-add-executable)
                   ssh-agency-add-executable)))
    (with-temp-buffer     ; Catch any process output in a temp buffer.
      (let ((exit-status
             (cond
              (ssh-agency-askpass
               (let ((process-environment (append (when (listp ssh-agency-askpass)
                                                    ssh-agency-askpass)
                                                  process-environment)))
                 (apply #'call-process ssh-add nil '(t t) nil keys)))
              ((eq system-type 'windows-nt)
               (call-process-shell-command
                ;; Git 1.x: Passphrase can only be entered in console, so use
                ;; cmd.exe's `start' to get one.
                (concat "start \"ssh-add\" /WAIT " ssh-add " "
                        (mapconcat #'shell-quote-argument keys " "))
                ;; We probably can't get any output in this case, but
                ;; it doesn't hurt to try.
                nil '(t t)))
              (t (let* ((process-connection-type t) ; pty needed for filter.
                        (proc (apply #'start-process "ssh-add" (current-buffer)
                                     ssh-add keys)))
                   (setf (process-filter proc) #'ssh-agency-askpass-filter)
                   (while (eq (process-status proc) 'run)
                     (accept-process-output proc))
                   (if (process-get proc :user-quit)
                       0 ;; Quitting is not an error.
                     (process-exit-status proc)))))))
        (unless (eq 0 exit-status)
          (lwarn '(ssh-agency ssh-add) :error
                 "`%s' failed with status %d: %s"
                 ssh-add exit-status (buffer-string)))))))

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
  (let ((pid
          (--first (-let (((&alist 'comm comm 'user user) (process-attributes it)))
                     (and (member comm ssh-agency-agent-exe-names)
                          (string= user user-login-name)))
                   (list-system-processes))))
    (when pid
      (cl-loop for (fun . args) in ssh-agency-socket-locaters
               for (status . socket) = (apply fun args)
               when socket return
               (progn
                 (setenv "SSH_AGENT_PID" (number-to-string pid))
                 (setenv "SSH_AUTH_SOCK" socket)
                 status)))))

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
