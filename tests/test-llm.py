#!/usr/bin/env python3
import os
import requests
import json
import time
from statistics import mean, stdev

# API tokens from environment variables
LITELLM_API_TOKEN = os.getenv('LITELLM_API_TOKEN')
CIRRUS_KEY = os.getenv('CIRRUS_KEY')
OPENROUTER_API_TOKEN = os.getenv('OPENROUTER_API_TOKEN')

# Check for required environment variables
if not LITELLM_API_TOKEN:
    print("Warning: LITELLM_API_TOKEN environment variable not set")
if not CIRRUS_KEY:
    print("Warning: CIRRUS_KEY environment variable not set")
if not OPENROUTER_API_TOKEN:
    print("Warning: OPENROUTER_API_TOKEN environment variable not set")

# API endpoints
LITELLM_BASE = "https://llm.nrp-nautilus.io"
CIRRUS_BASE = "https://llm.cirrus.carlboettiger.info/v1"
OPENROUTER_BASE = "https://openrouter.ai/api/v1"

def get_models(base_url, token):
    """Get available models from an API endpoint"""
    try:
        response = requests.get(
            f"{base_url}/models",
            headers={"Authorization": f"Bearer {token}"}
        )
        response.raise_for_status()
        return response.json()
    except Exception as e:
        print(f"Error getting models from {base_url}: {e}")
        return None

def get_test_messages():
    """Return a list of different test messages to avoid KV cache hits"""
    return [
        "What is the capital of France?",
        "Explain quantum computing in simple terms.",
        "Write a haiku about programming.",
        "What are the benefits of renewable energy?",
        "How does photosynthesis work?"
    ]

def test_completion(base_url, token, model, message_index=0):
    """Test a single completion request with varied messages"""
    messages = get_test_messages()
    message = messages[message_index % len(messages)]
    
    try:
        start_time = time.time()
        response = requests.post(
            f"{base_url}/chat/completions",
            headers={
                "Content-Type": "application/json",
                "Authorization": f"Bearer {token}"
            },
            json={
                "model": model,
                "messages": [{"role": "user", "content": message}]
            }
        )
        end_time = time.time()
        response.raise_for_status()
        return {
            "success": True,
            "response_time": end_time - start_time,
            "response": response.json(),
            "message": message
        }
    except Exception as e:
        return {
            "success": False,
            "error": str(e),
            "response_time": None,
            "message": message
        }

def benchmark_model(base_url, token, model, num_calls=5):
    """Benchmark a model with multiple calls using different messages"""
    print(f"\nBenchmarking {model}...")
    results = []
    
    for i in range(num_calls):
        print(f"  Call {i+1}/{num_calls}...", end=" ")
        result = test_completion(base_url, token, model, message_index=i)
        results.append(result)
        
        if result["success"]:
            print(f"✓ {result['response_time']:.2f}s")
        else:
            print(f"✗ {result['error']}")
    
    # Calculate statistics
    successful_times = [r["response_time"] for r in results if r["success"]]
    if successful_times:
        avg_time = mean(successful_times)
        std_time = stdev(successful_times) if len(successful_times) > 1 else 0
        success_rate = len(successful_times) / len(results) * 100
        
        print(f"  Results: {len(successful_times)}/{num_calls} successful ({success_rate:.1f}%)")
        print(f"  Average time: {avg_time:.2f}s ± {std_time:.2f}s")
    else:
        print("  All calls failed")
    
    return results

def main():
    print("=== LLM API Testing and Benchmarking ===\n")
    
    # Test configurations
    configs = [
        {
            "name": "LiteLLM (Nautilus)",
            "base_url": LITELLM_BASE,
            "token": LITELLM_API_TOKEN,
            "models": ["gemma3", "llama3"]  # Known models from original script
        },
        {
            "name": "Cirrus",
            "base_url": CIRRUS_BASE,
            "token": CIRRUS_KEY,
            "models": ["cirrus"]  # Known model from original script
        },
        {
            "name": "OpenRouter",
            "base_url": OPENROUTER_BASE,
            "token": OPENROUTER_API_TOKEN,
            "models": ["openai/gpt-oss-20b:free"]  # Known model from original script
        }
    ]
    
    all_results = {}
    
    for config in configs:
        # Skip if token is not available
        if not config["token"]:
            print(f"--- {config['name']} ---")
            print(f"Skipping {config['name']} - API token not set\n")
            continue
            
        print(f"--- {config['name']} ---")
        
        # Get available models
        print("Getting available models...")
        models_response = get_models(config["base_url"], config["token"])
        
        if models_response:
            if isinstance(models_response, dict) and "data" in models_response:
                available_models = [model["id"] for model in models_response["data"]]
                print(f"Available models ({len(available_models)}): {available_models}")
                
                # For LiteLLM, test ALL available models
                if config["name"] == "LiteLLM (Nautilus)":
                    models_to_test = available_models
                else:
                    # For other services, use intersection of known models and available models
                    models_to_test = [m for m in config["models"] if m in available_models] or config["models"]
            else:
                available_models = config["models"]  # Fallback to known models
                models_to_test = config["models"]
                print(f"Available models: {available_models}")
        else:
            models_to_test = config["models"]
            print(f"Using fallback models: {models_to_test}")
        
        # Benchmark each model
        config_results = {}
        for model in models_to_test:
            results = benchmark_model(config["base_url"], config["token"], model)
            config_results[model] = results
        
        all_results[config["name"]] = config_results
        print()
    
    # Summary
    print("=== Summary ===")
    for service, models in all_results.items():
        print(f"\n{service}:")
        for model, results in models.items():
            successful_times = [r["response_time"] for r in results if r["success"]]
            if successful_times:
                avg_time = mean(successful_times)
                success_rate = len(successful_times) / len(results) * 100
                print(f"  {model}: {avg_time:.2f}s avg, {success_rate:.1f}% success")
            else:
                print(f"  {model}: All calls failed")

if __name__ == "__main__":
    main()
