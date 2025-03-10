# Emacs AI Extensions

This repository contains two Emacs extensions that leverage OpenAI’s API to enhance your coding workflow:

1. **AI Completion Extension** – Provides AI code completion within Emacs.
2. **RAG-Based QA Extension** – Implements a retrieval-augmented generation (RAG) workflow to answer code-related questions using local embeddings and OpenAI’s Chat API.

Both extensions are designed to work locally with minimal setup. Follow the instructions below to install and run them.

---

## Directory Structure

``` .
├── ai-completion.el # Emacs Lisp code for AI code completion.
├── rag-chat.el # Emacs Lisp code for RAG-based Q&A.
├── index_codebase.py # Python script to index your codebase into a local Chroma DB.
├── query_codebase.py # Python script to query the local Chroma DB.
├── requirements.txt # (Optional) List of required Python packages.
└── README.md # This file.
```

---

## Prerequisites

### For Emacs Extensions

- **Emacs 26 or newer** is recommended.
- Add the following to your Emacs configuration (`init.el` or `.emacs`) to enable MELPA (for installing dependencies like `request-deferred`):

```
elisp (require 'package) (setq package-archives '(("melpa" . "https://melpa.org/packages/") ("gnu" . "https://elpa.gnu.org/packages/"))) (package-initialize)
;; Refresh package contents if necessary: (unless package-archive-contents (package-refresh-contents))
```

- Ensure that `ai-completion.el` and `rag-chat.el` are placed in a directory on your Emacs load-path (e.g. `~/.emacs.d/lisp/`).

### For Python Scripts (Indexing & Querying)

- **Python 3** installed.
- Install required Python packages. You can use the provided `requirements.txt` or run:

```
bash pip install chromadb openai tiktoken pathspec
```

- Set your OpenAI API key as an environment variable:

```
bash export OPENAI_API_KEY="sk-XXXXXXXXXXXXXXXXXXXX"
```

Alternatively, you can set it inside Emacs (see below).

---

## Installation

### Emacs Setup

1. **Place the Files:**
- Copy `ai-completion.el` and `rag-chat.el` to a directory (e.g. `~/.emacs.d/lisp/`).

2. **Configure Your Emacs Init File:** Add the following lines to your `init.el` or `.emacs`:

```
elisp
;; Add the directory containing the extensions to your load-path (add-to-list 'load-path "~/.emacs.d/lisp/")
;; Load the AI Completion extension (if you plan to use it) (require 'ai-completion)
;; Load the RAG-based QA extension (require 'rag-chat)
;; Set your OpenAI API key and export it so Python scripts can read it (setq rag-chat--openai-api-key "sk-XXXXXXXXXXXXXXXXXXXX") (setenv "OPENAI_API_KEY" rag-chat--openai-api-key)
;; Configure paths to your Python scripts (setq rag-chat--index-script "/absolute/path/to/index_codebase.py") (setq rag-chat--query-script "/absolute/path/to/query_codebase.py")
```

3. **Restart Emacs** or evaluate the new configuration.

---

## Usage

### AI Completion Extension

- **Activate the Minor Mode:** Open any code buffer and run:

```
elisp

M-x ai-completion-mode
```

You should see “AI-Complete” in your mode line.

- **Trigger Completion:** Press `C-c C-a` to send the surrounding code to OpenAI and insert the completion at point.

### RAG-Based QA Extension

1. **Start RAG Chat Mode:**

```
elisp

M-x rag-chat-mode
```

This opens the *RAG Chat* buffer with usage instructions.

2. **Index Your Codebase:**

```
elisp

M-x rag-chat-index-codebase
```

You will be prompted for:

- **Project root:** The top directory of your project (where a folder `chroma_db` will be created).

- **Subdir to index (optional):** If you want to index only a subdirectory. The Python script will run asynchronously. Check the *RAG Index Output* buffer for progress.

3. **Ask a Question:**

```
elisp

M-x rag-chat-ask-question
```

You will be prompted for:

- **Project root:** Enter the same directory used during indexing.

- **Question:** For example, “How do I call get_secrets()?” The extension will query the local Chroma DB, send a combined prompt to OpenAI, and display the final Q&A in the *RAG Chat* buffer in a new window.

4. **Closing the *RAG Chat* Buffer:** While in the *RAG Chat* buffer, press `q` (bound to `quit-window`), or use `C-x k` to kill the buffer.

---

## Python Scripts Overview

### index_codebase.py

- **Purpose:** Recursively scans all files in a given directory, chunks each file into manageable pieces (approx. 1000 tokens by word count), obtains embeddings using OpenAI’s text-embedding-ada-002, and stores them in a local Chroma database located at `<project_root>/chroma_db`.

- **Key Functions:**

- `chunk_text(text, max_chunk_size)`: Splits file content into chunks.

- `embed_text(text)`: Gets vector embeddings from OpenAI.

- `index_directory(code_dir, db_dir)`: Processes each file and stores the embeddings.

-**Usage Example:**

```
bash

python3 index_codebase.py /path/to/project [subdir]
```

### query_codebase.py

- **Purpose:** Given a project root and a query, it loads the Chroma DB from `<project_root>/chroma_db` and retrieves the top relevant code chunks.

- **Key Functions:**

- `retrieve_relevant_chunks(db_dir, query, top_k)`: Queries the database for matching chunks. - **Usage Example:**

```
bash

python3 query_codebase.py /path/to/project "How do I call get_secrets()?"
```

---

## Troubleshooting

- **Ensure Consistency:** Use the same project root when indexing and querying so that the `<project_root>/chroma_db` folder is correctly referenced.

- **Environment Variables:** Make sure the `OPENAI_API_KEY` is set in your shell environment or via Emacs’s `setenv`.

- **Dependencies:** Verify all Python packages (`chromadb`, `openai`, `tiktoken`, `pathspec`) are installed.

---

## License

This repository is provided under the GNU General Public License v3.0 License. See the LICENSE file for details.

---

## Contributing

Feel free to submit issues or pull requests. Contributions that improve the code, documentation, or add new features (such as better multi-turn support) are welcome.

---

Happy coding and enjoy your enhanced Emacs AI experience!

