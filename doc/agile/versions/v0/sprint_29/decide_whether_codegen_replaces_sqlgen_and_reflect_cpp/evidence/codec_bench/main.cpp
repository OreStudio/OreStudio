#include "codecs.hpp"
#include <algorithm>
#include <atomic>
#include <chrono>
#include <cstdio>
#include <cstdlib>
#include <functional>
#include <new>
#include <stdexcept>

namespace {

std::atomic<long> allocations{0};

}

void* operator new(std::size_t n) {
    allocations.fetch_add(1, std::memory_order_relaxed);
    if (void* p = std::malloc(n ? n : 1))
        return p;
    throw std::bad_alloc();
}
void operator delete(void* p) noexcept { std::free(p); }
void operator delete(void* p, std::size_t) noexcept { std::free(p); }

namespace {

using bench::response;

/*
 * A list response as the service sends it. The text carries a quote, a
 * backslash, a control character and non-ASCII letters, so the identity check
 * covers escaping as well as layout.
 */
response make_response(int n) {
    response r;
    r.result.code = "ok";
    r.result.message = "Listed \"categories\" \\ fine\t— ünïcode";
    r.result.fields.push_back({"limit", "clamped", "Limit clamped to 100."});
    r.total = 12345;
    const auto at = std::chrono::system_clock::time_point(std::chrono::seconds(1793000000));
    for (int i = 0; i < n; ++i) {
        ores::dq::domain::change_reason_category c;
        c.version = i % 7 + 1;
        c.code = "CATEGORY_" + std::to_string(i);
        c.description = "Change reason category number " + std::to_string(i) + ", for the benchmark.";
        c.modified_by = "admin@acme.example";
        c.performed_by = "ores.dq.service";
        c.change_reason_code = "system.new";
        c.change_commentary = "Seeded by the codec benchmark.";
        c.recorded_at = at + std::chrono::milliseconds(i * 1013);
        r.categories.push_back(std::move(c));
    }
    return r;
}

void check(bool ok, const char* what) {
    std::printf("%-58s %s\n", what, ok ? "yes" : "NO");
    if (!ok)
        std::exit(1);
}

void prove_identity(const response& r, const char* label) {
    namespace g = bench::generated;
    namespace f = bench::reflected;
    const auto json = f::to_json(r);
    const auto pack = f::to_msgpack(r);
    std::printf("== %s: JSON %zu bytes, MessagePack %zu bytes\n", label, json.size(), pack.size());
    check(g::to_json(r) == json, "generated JSON is byte-identical to reflect-cpp");
    check(g::to_msgpack(r) == pack, "generated MessagePack is byte-identical to reflect-cpp");
    check(f::to_msgpack(g::from_json(json)) == pack, "generated JSON decode matches reflect-cpp");
    check(f::to_msgpack(g::from_msgpack(pack)) == pack,
          "generated MessagePack decode matches reflect-cpp");
}

struct sample {
    double ns;
    double allocs;
};

sample measure(int iterations, const std::function<void()>& f) {
    std::vector<double> runs;
    long allocs = 0;
    for (int run = 0; run < 7; ++run) {
        const auto a = allocations.load();
        const auto t = std::chrono::steady_clock::now();
        for (int i = 0; i < iterations; ++i)
            f();
        runs.push_back(
            std::chrono::duration<double, std::nano>(std::chrono::steady_clock::now() - t).count() /
            iterations);
        allocs = allocations.load() - a;
    }
    std::ranges::sort(runs);
    return {runs[runs.size() / 2], static_cast<double>(allocs) / iterations};
}

void compare(const response& r, int iterations, const char* label) {
    namespace g = bench::generated;
    namespace f = bench::reflected;
    const auto json = f::to_json(r);
    const auto pack = f::to_msgpack(r);
    struct op {
        const char* name;
        std::function<void()> reflected, generated;
    };
    const op ops[] = {
        {"JSON encode", [&] { (void)f::to_json(r); }, [&] { (void)g::to_json(r); }},
        {"JSON decode", [&] { (void)f::from_json(json); }, [&] { (void)g::from_json(json); }},
        {"MessagePack encode", [&] { (void)f::to_msgpack(r); }, [&] { (void)g::to_msgpack(r); }},
        {"MessagePack decode", [&] { (void)f::from_msgpack(pack); },
         [&] { (void)g::from_msgpack(pack); }},
    };
    std::printf("== %s, median of 7 runs of %d\n", label, iterations);
    std::printf("%-20s %14s %14s %8s %12s %12s\n", "operation", "reflect-cpp ns", "generated ns",
                "speedup", "rfl allocs", "gen allocs");
    for (const auto& o : ops) {
        const auto a = measure(iterations, o.reflected);
        const auto b = measure(iterations, o.generated);
        std::printf("%-20s %14.0f %14.0f %7.2fx %12.1f %12.1f\n", o.name, a.ns, b.ns, a.ns / b.ns,
                    a.allocs, b.allocs);
    }
}

}

int main() {
    const auto one = make_response(1);
    const auto hundred = make_response(100);
    prove_identity(one, "list of 1");
    prove_identity(hundred, "list of 100");
    compare(one, 20000, "list of 1");
    compare(hundred, 500, "list of 100");
}
