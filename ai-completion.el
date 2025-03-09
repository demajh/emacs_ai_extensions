;;; ai-completion.el --- Minimal AI code completion in Emacs using OpenAI API -*- lexical-binding: t; -*-

(require 'request-deferred)
(require 'json)
(require 'subr-x) ;; for 'string-trim' if needed

(defgroup ai-completion nil
  "AI code completion using OpenAI."
  :group 'tools)

(defcustom ai-completion-openai-api-key ""
  "OpenAI API key for AI code completion."
  :type 'string
  :group 'ai-completion)

(defcustom ai-completion-model "gpt-4o"
  "OpenAI model for AI code completion (e.g. gpt-3.5-turbo, gpt-4)."
  :type 'string
  :group 'ai-completion)

(defcustom ai-completion-max-tokens 1000
  "Maximum tokens to request in the AI completion."
  :type 'integer
  :group 'ai-completion)

(defcustom ai-completion-temperature 0.2
  "Sampling temperature for AI completions. 0.0 is the most deterministic."
  :type 'float
  :group 'ai-completion)

(defun ai-completion--get-context ()
  "Return a portion of the current buffer around point as context.
You can tweak how many characters to include."
  (let* ((context-window 1000) ;; number of chars before & after point
	 (start (max (point-min) (- (point) context-window)))
	 (end   (min (point-max) (+ (point) context-window))))
    (buffer-substring-no-properties start end)))

(defun ai-completion--call-openai (prompt callback)
  "Call the OpenAI Chat Completion API with PROMPT and run CALLBACK on the text response."
  (let ((url "https://api.openai.com/v1/chat/completions")
	(data (json-encode
	       `(("model" . ,ai-completion-model)
		 ("messages" . [
				;; System role can provide high-level instructions
				(("role" . "system")
				 ("content" . "You are a helpful coding assistant. Please continue or complete the provided code context.  ONLY RETURN CODE IN YOUR RESPONSE, NEVER INCLUDE ANY OTHER TEXT!!!"))
				;; User's 'prompt' or code context
				(("role" . "user")
				 ("content" . ,prompt))
				])
		 ("max_tokens" . ,ai-completion-max-tokens)
		 ("temperature" . ,ai-completion-temperature))))
	(headers `(("Content-Type" . "application/json")
		   ("Authorization" . ,(concat "Bearer " ai-completion-openai-api-key)))))
    (request-deferred
     url
     :type "POST"
     :data data
     :headers headers
     :parser 'json-read
     :error (cl-function
	     (lambda (&rest args &key error-thrown &allow-other-keys)
	       (message "ai-completion error: %S" error-thrown)))
     :success (cl-function
	       (lambda (&key data &allow-other-keys)
		 (let* ((choices (assoc-default 'choices data))
			(first-choice (and choices (aref choices 0)))
			(message (assoc-default 'message first-choice))
			(content (assoc-default 'content message)))
		   (when callback
		     (funcall callback content))))))))

(defun ai-completion-insert ()
  "Fetch an AI completion and insert it at point."
  (interactive)
  (let* ((context (ai-completion--get-context)))
    ;; Async request to OpenAI, then insert response on success
    (ai-completion--call-openai
     context
     (lambda (response)
       (when response
	 (save-excursion
	   (insert (concat "\n" response))))))))

;;;###autoload
(define-minor-mode ai-completion-mode
  "Minor mode for simple AI code completion using OpenAI."
  :lighter " AI-Complete"
  :keymap (let ((map (make-sparse-keymap)))
	    ;; For example: C-c C-a to trigger completion
	    (define-key map (kbd "C-c C-a") 'ai-completion-insert)
	    map)
  (if ai-completion-mode
      (message "ai-completion-mode enabled")
    (message "ai-completion-mode disabled")))

(provide 'ai-completion)
;;; ai-completion.el ends here
