#!/usr/bin/env python3
import os
import sys
import json
import chromadb
from chromadb.config import Settings

import openai
from chromadb.utils import embedding_functions

openai.api_key = os.getenv("OPENAI_API_KEY")

def retrieve_relevant_chunks(db_dir, query, top_k=3):
    
    """
    Query local Chroma DB (stored at db_dir) for the top_k relevant code chunks.
    """

    openai_ef = embedding_functions.OpenAIEmbeddingFunction(
        api_key=openai.api_key,
        model_name="text-embedding-ada-002"
    )
    client = chromadb.Client(
        Settings(
            chroma_db_impl="duckdb+parquet",
            persist_directory=db_dir
        )
    )
    collection = client.get_or_create_collection(
        name="my_codebase",
        embedding_function=openai_ef
    )
    # Then you can do:
    results = collection.query(query_texts=[query], n_results=top_k)
    
#    client = chromadb.Client(Settings(
#        chroma_db_impl="duckdb+parquet",
#        persist_directory=db_dir
#    ))
#    collection = client.get_collection("my_codebase")
#    results = collection.query(query_texts=[query], n_results=top_k)

    return results

def main():
    
    """
    Usage:
      python query_codebase.py <project_root> <question>
    Example:
      python query_codebase.py /path/to/myproject "How do I call get_secrets()?"
    """
    
    if len(sys.argv) < 3:
#        print("Usage: python query_codebase.py <project_root> <question>")
        sys.exit(1)

    project_root = os.path.abspath(sys.argv[1])
    question = " ".join(sys.argv[2:])

    db_dir = os.path.join(project_root, "chroma_db")
    # Retrieve top 3 relevant chunks from the local DB
    results = retrieve_relevant_chunks(db_dir, question, top_k=3)
    print(json.dumps(results, indent=2))

if __name__ == "__main__":
    main()
