# Independent native CUDA inference

The user selected **LiquidAI/LFM3-1.7B**, official QAD Q4_0, and approved its
1,049,615,680-byte download. `inference-lock.json` pins the Hugging Face revision,
LFS SHA-256, model license reference, llama.cpp b12172 and official CUDA archive
SHA-256 hashes. No model or runtime binary belongs in Git. The model uses the
LFM Open License; review the upstream license for your intended deployment.

Prerequisites: Node 22.11+ and Python 3.12+ for installation. Windows x64 uses
native CUDA 12.4 binaries and bundled runtime DLLs. Linux x64 uses the official
Ubuntu CUDA 12.8 binaries and runtime libraries; use a compatible Ubuntu/glibc
host and NVIDIA driver. Both require an enabled CUDA-capable NVIDIA GPU. This
deployment explicitly selects CUDA0 and does not count CPU fallback as success.

From this component directory, independently of any application build:

```text
python scripts/install-inference.py <inference-root-outside-checkout>
node scripts/configure-inference.mjs <inference-root>
node scripts/service.mjs start <inference-root>/service.json
node scripts/service.mjs status <inference-root>/service.json
node scripts/service.mjs stop <inference-root>/service.json
```

Install downloads and verifies artifacts; configure writes a service spec and
creates a private API-key file once. Neither starts processes. Start consumes
that spec without compiling or downloading anything. The operator owns the
spec, key, logs, models and release directories. Retain the key and models
across runtime upgrades; stop, install/configure the desired pinned release,
then start. Existing releases remain available for rollback. No backend process,
database, broker configuration or main application deployment is touched.

Set the agent's inference `apiKeyFile` to `<inference-root>/config/api-key.txt`.
It reads the key server-side; never put it in browser configuration. Inference
binds to 127.0.0.1:20010 with one slot, 4096 context tokens, four CPU threads,
all model layers requested on CUDA, the GGUF's Jinja chat template, temperature
0.1, top_k 50 and repeat penalty 1.1. Logs are private operational artifacts;
verbose runtime logs may include prompt content. Use verbosity 3 after diagnosis.

Run `node scripts/smoke-inference.mjs <agent-config.json>` to check plain
generation, a structured tool call and replay. Its replay data is explicitly
a fixture. The end-to-end agent check separately retrieves real backend data.
Confirm `offloaded 25/25 layers to GPU` in `run/stderr.log` and inspect GPU
memory; a healthy HTTP response alone is insufficient.

Windows evidence, 2026-11-06: llama.cpp 0.9.0-dev build 12172, commit
5d3ed2ae7; RTX 4070 Laptop GPU 8188 MiB, driver 577.02. All 25/25 layers
offloaded; CUDA model buffer 993.13 MiB, KV buffers 55.50 MiB, compute buffer
65.82 MiB. Health and three generation checks passed (3.4–3.9 seconds combined
in two smoke runs; not a throughput benchmark). Linux GPU execution still
requires validation on a Linux NVIDIA host; Windows validation does not prove it.

Sources: [official model card](https://huggingface.co/LiquidAI/LFM3-1.7B-GGUF),
[pinned runtime release](https://github.com/ggml-org/llama.cpp/releases/tag/b12172).
