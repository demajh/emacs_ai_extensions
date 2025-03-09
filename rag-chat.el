;;; rag-chat.el --- Retrieval-Augmented Chat for a codebase, using local Chroma DB  -*- lexical-binding: t; -*-
;;
;; Usage:
;;   1. Place this file somewhere in your Emacs load-path, then:
;;        (require 'rag-chat)
;;   2. Set the following variables as needed:
;;        (setq rag-chat--index-script "/path/to/index_codebase.py")
;;        (setq rag-chat--query-script "/path/to/query_codebase.py")
;;        (setq rag-chat--openai-api-key "sk-XXXX")
;;   3. M-x rag-chat-mode  -> opens the *RAG Chat* buffer with instructions.
;;   4. M-x rag-chat-index-codebase -> prompts for project root & optional subdir to index.
;;   5. M-x rag-chat-ask-question -> queries the local DB, calls OpenAI, and displays the answer.

(require 'json)
(require 'request-deferred)
(require 'subr-x) ;; For string-trim

(defgroup rag-chat nil
  "Retrieval-Augmented Chat in Emacs for local codebases."
  :group 'tools)

(defcustom rag-chat--openai-api-key ""
  "Your OpenAI API key for chat completions."
  :type 'string
  :group 'rag-chat)

(defcustom rag-chat--openai-model "gpt-4o"
  "Which OpenAI Chat Completion model to use (e.g. gpt-3.5-turbo, gpt-4)."
  :type 'string
  :group 'rag-chat)

(defcustom rag-chat--index-script "/path/to/index_codebase.py"
  "Path to the Python script that indexes the codebase (writes embeddings to <project_root>/chroma_db)."
  :type 'string
  :group 'rag-chat)

(defcustom rag-chat--query-script "/path/to/query_codebase.py"
  "Path to the Python script that queries the local vector DB (<project_root>/chroma_db)."
  :type 'string
  :group 'rag-chat)

(defcustom rag-chat--num-results 3
  "Number of relevant chunks to retrieve from the codebase for each query."
  :type 'integer
  :group 'rag-chat)

(defvar rag-chat--buffer-name "*RAG Chat*"
  "Name of the buffer where RAG chat Q&A is displayed.")



;;; --- Indexing the Codebase ---

(defun rag-chat-index-codebase (&optional project-root subdir)
  "Index the code in PROJECT-ROOT or optional SUBDIR using index_codebase.py.
The embeddings are stored in <project_root>/chroma_db."
  (interactive
   (list (read-directory-name "Project root: " default-directory)
         (read-string "Subdir to index (optional): ")))
  (let* ((bufname "*RAG Index Output*")
         (script rag-chat--index-script)
         (project-arg (expand-file-name project-root))
         (subdir-arg subdir))
    (with-current-buffer (get-buffer-create bufname)
      (erase-buffer)
      (insert (format "Indexing project root: %s\n" project-arg))
      (when (and subdir-arg (not (string-empty-p subdir-arg)))
        (insert (format "Indexing subdir: %s\n" subdir-arg)))
      (insert (format "Using script: %s\n\n" script))
      ;; Start an async process to run index_codebase.py
      (if (and subdir-arg (not (string-empty-p subdir-arg)))
          (start-process "rag-index-process" bufname
                         "python3" script project-arg subdir-arg)
        (start-process "rag-index-process" bufname
                       "python3" script project-arg)))
    (pop-to-buffer bufname)))



;;; --- Querying the Codebase & Building Chat Prompts ---

(defun rag-chat--query-codebase (project-root question callback)
  "Run query_codebase.py' for PROJECT-ROOT and QUESTION. Pass JSON results to CALLBACK."
  (let* ((script rag-chat--query-script)
         (project-arg (expand-file-name project-root))
         ;; Build a shell command like: python3 /path/to/query_codebase.py <root> "<question>"
         (cmd (mapconcat #'shell-quote-argument
                         (list "python3" script project-arg question)
                         " "))
         ;; Run it (synchronously) and capture stdout as a string
         (raw-output (shell-command-to-string cmd)))
    (condition-case err
        (let ((json-data (json-read-from-string raw-output)))
          (funcall callback json-data))
      (error
       (message "RAG Chat Query Error: %S" err)
       nil))))

(defun rag-chat--query-codebase-async (project-root question callback)
  "Asynchronously run query_codebase.py' and parse JSON for PROJECT-ROOT and QUESTION.
When the process exits, call CALLBACK with parsed JSON (or nil if error)."
  (let* ((script rag-chat--query-script)
         (project-arg (expand-file-name project-root))
         (cmd-args (list "python3" script project-arg question))
         (buf (generate-new-buffer "*RAG Query Output*")))
    (make-process
     :name "rag-query-process"
     :buffer buf
     :command cmd-args
     :sentinel
     (lambda (proc event)
       (when (eq (process-status proc) 'exit)
         (let ((exit-code (process-exit-status proc)))
           (if (not (zerop exit-code))
               (message "RAG Chat Query Error: process exited with code %d" exit-code)
             ;; Parse JSON from buffer
             (with-current-buffer buf
               (goto-char (point-min))
               (condition-case err
                   (let ((json-data (json-read)))
                     (funcall callback json-data))
                 (error
                  (message "RAG Chat Query JSON parse error: %S" err))))))
         (kill-buffer buf))))))

(defun rag-chat--openai-chat (messages callback)
  "Send MESSAGES (list of role/content objects) to the OpenAI Chat endpoint.
Call CALLBACK with the final answer string."
  (request-deferred
   "https://api.openai.com/v1/chat/completions"
   :type "POST"
   :headers `(("Content-Type" . "application/json")
              ("Authorization" . ,(concat "Bearer " rag-chat--openai-api-key)))
   :data (json-encode
          `(("model" . ,rag-chat--openai-model)
            ("messages" . ,messages)
            ("max_tokens" . 10000)
            ("temperature" . 0.2)))
   :parser 'json-read
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (let* ((choices (assoc-default 'choices data))
                      (first-choice (and choices (aref choices 0)))
                      (msg (assoc-default 'message first-choice))
                      (content (assoc-default 'content msg)))
                 (funcall callback content))))
   :error (cl-function
           (lambda (&rest args &key error-thrown &allow-other-keys)
             (message "OpenAI Chat Error: %S" error-thrown)))))



;;; --- High-Level Q&A Function ---


(defun rag-chat-ask-question (&optional project-root question)
  "Prompt for PROJECT-ROOT (directory) and QUESTION (string).
1. Query codebase for relevant snippets (top `rag-chat--num-results`).
2. Send them + QUESTION to OpenAI Chat Completion API.
3. Display answer in `rag-chat--buffer-name` buffer, then pop that buffer in a new window."
  (interactive
   (list (read-directory-name "Project root: " default-directory)
         (read-string "Question: ")))
  (unless (and question (not (string-empty-p question)))
    (user-error "No question provided."))
  (let* ((root (expand-file-name project-root))
         (q question))
    ;; 1) Query the codebase asynchronously or synchronously (this snippet uses sync).
    (rag-chat--query-codebase
     root q
     (lambda (retrieval-result)
       ;; 2) Parse the 'documents' array from the JSON (which has string keys).
       (let* ((docs (cdr (assoc "documents" retrieval-result))) 
              ;; docs is typically a vector-of-vectors: e.g. [[ "..." "...", ... ]]
              (docs-list (append docs nil)) ;; convert top-level vector -> list
              (docs-lol (mapcar (lambda (subv) (append subv nil)) docs-list))
              (snippets (apply #'append docs-lol)) ;; flatten sub-vectors into one list of strings

              ;; 3) Build the textual prompt
              (system-message
               "You are a coding assistant. Use the following code snippets if relevant to answer the user's question.")
              (user-message
               (concat
                "Relevant Code Snippets:\n"
                (mapconcat #'identity snippets "\n\n---\n\n")
                "\n\nUser Question:\n"
                q))

              ;; 4) Construct the Chat messages as a list of alists
              ;;    Each element is an alist, e.g. (("role" . "system") ("content" . "..."))
              (messages
               (list
                (list (cons "role" "system")
                      (cons "content" system-message))
                (list (cons "role" "user")
                      (cons "content" user-message)))))

         ;; 5) Send the messages to OpenAI
         (rag-chat--openai-chat
          messages
          (lambda (answer)
            ;; 6) Insert Q&A into *RAG Chat* buffer, pop it in a new window
            (with-current-buffer (get-buffer-create rag-chat--buffer-name)
              (goto-char (point-max))
              (insert (format "Q: %s\nA: %s\n\n" q answer)))
            (pop-to-buffer (get-buffer-create rag-chat--buffer-name)
                           '((display-buffer-pop-up-window)
                             (inhibit-same-window . t))))))))))


;;; --- A Simple RAG Chat Mode ---

(defun rag-chat-mode ()
  "Major mode for retrieval-augmented chat with a local codebase.
Use:
  M-x rag-chat-index-codebase -> to index <project_root>/chroma_db
  M-x rag-chat-ask-question   -> to query codebase + LLM for an answer.

All Q&A is displayed in the *RAG Chat* buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create rag-chat--buffer-name))
  (rag-chat-mode-init))

(defun rag-chat-mode-init ()
  (kill-all-local-variables)
  (use-local-map (make-sparse-keymap))
  (setq major-mode 'rag-chat-mode
        mode-name "RAG-Chat")
  (read-only-mode -1)
  (local-set-key (kbd "q") #'quit-window)
  (insert "Welcome to RAG Chat Mode.\n\n")
  (insert "Commands:\n")
  (insert "  M-x rag-chat-index-codebase -> Index project code into <root>/chroma_db\n")
  (insert "  M-x rag-chat-ask-question -> Query the codebase with an OpenAI-augmented Q&A.\n\n"))

(provide 'rag-chat)
;;; rag-chat.el ends here

