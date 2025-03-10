#!/usr/bin/env python3
import os
import sys
import fnmatch
import openai
import chromadb
from chromadb.config import Settings

openai.api_key = os.getenv("OPENAI_API_KEY")  # or set your key here

# ------------------------------------------------------------------------------
# 1) PATHSPEC-BASED IGNORE HANDLING
# ------------------------------------------------------------------------------
try:
    import pathspec
except ImportError:
    print("Please install pathspec (pip install pathspec) for .gitignore semantics.")
    sys.exit(1)

def load_ignore_spec(project_root):
    
    """
    Load patterns from a .noindex (or .gitignore-like) file in `project_root`.
    Return a PathSpec object that can be used to match file paths.
    If no .noindex file is found, return an empty spec that doesn't ignore anything.

    We parse lines in 'gitwildmatch' style to replicate .gitignore semantics:
      - Directory globs (e.g. node_modules/)
      - Wildcards (e.g. *.log)
      - Negation (e.g. !main.py)
      - etc.
    """
    
    noindex_file = os.path.join(project_root, ".noindex")
    if not os.path.isfile(noindex_file):
        # Return an empty spec if there's no .noindex
        return pathspec.PathSpec.from_lines("gitwildmatch", [])

    # Read lines
    with open(noindex_file, "r", encoding="utf-8") as f:
        lines = f.readlines()

    # Build a PathSpec
    spec = pathspec.PathSpec.from_lines("gitwildmatch", lines)
    return spec

def path_is_ignored(file_path, spec, project_root):
    
    """
    Return True if file_path should be ignored, according to the PathSpec `spec`.
    We match files relative to `project_root` because .gitignore patterns are relative.
    """
    
    rel_path = os.path.relpath(file_path, project_root)
    # pathspec can check if a relative path is ignored
    return spec.match_file(rel_path)

# ------------------------------------------------------------------------------
# 2) CORE INDEXING CODE
# ------------------------------------------------------------------------------

def chunk_text(text, max_chunk_size=1000):
    
    """
    Break text into ~max_chunk_size tokens (approx. by word count).
    Adjust or replace with a real token counter if needed.
    """
    
    lines = text.split("\n")
    chunks = []
    current_chunk = []
    current_size = 0

    for line in lines:
        line_len = len(line.split())
        if current_size + line_len > max_chunk_size:
            chunks.append("\n".join(current_chunk))
            current_chunk = [line]
            current_size = line_len
        else:
            current_chunk.append(line)
            current_size += line_len
    if current_chunk:
        chunks.append("\n".join(current_chunk))
    return chunks

def embed_text(text):
    
    """
    Use OpenAI embeddings endpoint (text-embedding-ada-002).
    """
    
    response = openai.Embedding.create(
        input=[text],
        model="text-embedding-ada-002"
    )
    embedding = response["data"][0]["embedding"]
    return embedding

def index_directory(code_dir, db_dir, project_root, ignore_spec):
    
    """
    Recursively index all files under `code_dir`, chunk & embed them, and store
    in a local Chroma DB at `db_dir`. If a file or directory is ignored by
    `ignore_spec` (gitwildmatch semantics), skip it.
    """
    
    # Initialize Chroma client
    client = chromadb.Client(Settings(
        chroma_db_impl="duckdb+parquet",
        persist_directory=db_dir  # store local DB in <project_root>/chroma_db
    ))
    collection = client.get_or_create_collection(name="my_codebase")

    for root, dirs, files in os.walk(code_dir):
        # Remove ignored directories from traversal in-place
        # (otherwise, we'd visit them anyway).
        dirs_to_keep = []
        for d in dirs:
            dir_path = os.path.join(root, d)
            if path_is_ignored(dir_path, ignore_spec, project_root):
                # We'll skip descending into this directory
                print(f"Skipping directory: {dir_path}")
            else:
                dirs_to_keep.append(d)
        dirs[:] = dirs_to_keep

        # Also skip if this path is the DB directory itself
        if os.path.abspath(db_dir).startswith(os.path.abspath(root)):
            # If we find ourselves in the same or a subdir of db_dir, skip
            continue

        for f in files:
            file_path = os.path.join(root, f)

            # Check if file is ignored
            if path_is_ignored(file_path, ignore_spec, project_root):
                # e.g., .gitignore has a pattern matching it
                # or the .noindex file includes it
                # or a subdir pattern that also applies
                print(f"Skipping file: {file_path}")
                continue

            # Attempt to read the file
            try:
                with open(file_path, "r", encoding="utf-8") as fp:
                    content = fp.read()
            except Exception as e:
                print(f"Error reading {file_path}: {e}")
                continue

            # Chunk the file content
            chunks = chunk_text(content)
            for i, chunk in enumerate(chunks):
                try:
                    emb = embed_text(chunk)
                except Exception as e_emb:
                    print(f"Embedding error for {file_path}, chunk {i}: {e_emb}")
                    continue

                doc_id = f"{file_path}-chunk{i}"
                metadata = {"path": file_path, "chunk_index": i}
                collection.add(
                    documents=[chunk],
                    embeddings=[emb],
                    metadatas=[metadata],
                    ids=[doc_id]
                )

def main():
    
    """
    Usage:
        python index_codebase.py <project_root> [<code_dir>]

    1. <project_root> is the absolute or relative path to your project's top directory.
       We'll store the DB in: <project_root>/chroma_db

    2. <code_dir> is optional. If you omit it, we'll index the entire project root.
       If provided, we only index the subdirectory of the project root.

    3. We'll load patterns from <project_root>/.noindex (if present),
       using 'gitwildmatch' semantics via PathSpec. This allows advanced
       .gitignore-like rules to skip indexing certain files/dirs.
    """
    
    if len(sys.argv) < 2:
        print("Usage: python index_codebase.py <project_root> [<code_dir>]")
        sys.exit(1)

    project_root = os.path.abspath(sys.argv[1])
    code_dir = project_root
    if len(sys.argv) > 2:
        code_dir = os.path.join(project_root, sys.argv[2])

    db_dir = os.path.join(project_root, "chroma_db")

    # Load advanced ignore spec from .noindex
    ignore_spec = load_ignore_spec(project_root)

    print(f"Project root: {project_root}")
    print(f"Indexing code in: {code_dir}")
    print(f"DB location: {db_dir}")
    print(f"Ignore spec loaded from: {os.path.join(project_root, '.noindex') if os.path.exists(os.path.join(project_root, '.noindex')) else '(none)'}")

    index_directory(code_dir, db_dir, project_root, ignore_spec)
    print("Indexing complete.")

if __name__ == "__main__":
    main()
