# Separate CUDA inference

The selected model is **LiquidAI/LFM3-1.7B**, official QAD Q4_0 GGUF.
Its approved size is 1,049,615,680 bytes. `inference-lock.json` pins its revision,
SHA-256, license reference, llama.cpp b12172 and CUDA archive hashes.
Do not commit model or runtime binaries.

## Install and start

Installation needs Node 22.11+, Python 3.12+ and a CUDA-capable NVIDIA GPU.
Windows x64 uses native CUDA 12.4 binaries. Linux x64 uses Ubuntu CUDA 12.8
binaries; it needs compatible glibc and NVIDIA drivers.

From `projects/ores.agent`:

```text
python scripts/install-inference.py <INFERENCE_ROOT>
node scripts/configure-inference.mjs <INFERENCE_ROOT>
node <INFERENCE_ROOT>/service.mjs start <INFERENCE_ROOT>/service.json
node <INFERENCE_ROOT>/service.mjs status <INFERENCE_ROOT>/service.json
node <INFERENCE_ROOT>/service.mjs stop <INFERENCE_ROOT>/service.json
```

Keep the root outside the checkout. Install downloads and checks files.
Configure writes a service spec and creates the private key once. Neither starts
a process. Configure also copies the lifecycle script into the root.

Set the agent's `inference.apiKeyFile` to
`<INFERENCE_ROOT>/config/api-key.txt`. Keep the key server-side.

## Configured limits

| Setting | Value |
| --- | --- |
| Address / model alias | `127.0.0.1:20010` / `lfm3-1.7b` |
| Context / parallel slots | 262144 tokens / 1 |
| GPU | CUDA0; all layers requested |
| CPU threads / batch threads | 4 / 4 |
| Template | GGUF Jinja template |
| Temperature / top_k / repeat penalty | 0.1 / 50 / 1.1 |
| Agent output allowance | 2048 tokens |

Context includes input and output. Set `inference.maxOutputTokens` in the private
agent config to 128–8192 to change the output allowance. A response stopped by
`finish_reason: length` is rejected. Agent payload and tool limits still apply.

The earlier 4096-token context and 512-token output allowance are obsolete.
Full context is capacity, not proof of quality or speed with a 256K-token prompt.
Check GPU memory on each host. CPU fallback is not a CUDA test pass.

## Check the deployment

```text
node scripts/smoke-inference.mjs <PRIVATE_AGENT_CONFIG>
```

This checks text generation, a structured tool call and fixture replay. It does
not prove live business-data access. Inspect runtime logs for GPU offload.
Logs can contain prompt text; protect them.

Windows check, 2026-11-07: RTX 4070 Laptop GPU, 25/25 layers on CUDA,
`n_ctx=n_ctx_seq=262144`. Device usage was 4548 MiB, with 3401 MiB free.
A 7225-token prompt completed. This did not test a full 256K prompt.
Native Linux NVIDIA execution remains unverified.

To upgrade, stop inference, install/configure the pinned release, then start.
Keep the key and model store. This does not redeploy the agent or main server.

References: [official model and license](https://huggingface.co/LiquidAI/LFM3-1.7B-GGUF),
[pinned runtime](https://github.com/ggml-org/llama.cpp/releases/tag/b12172).
