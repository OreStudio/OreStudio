#include "clients.hpp"
#include "ores.platform/environment/environment.hpp"
#include <algorithm>
#include <atomic>
#include <chrono>
#include <cstdio>
#include <cstdlib>
#include <functional>
#include <new>

namespace {

std::atomic<long> allocations{0};

}

void* operator new(std::size_t n) {
    allocations.fetch_add(1, std::memory_order_relaxed);
    if (void* p = std::malloc(n ? n : 1))
        return p;
    throw std::bad_alloc();
}
void operator delete(void* p) noexcept {
    std::free(p);
}
void operator delete(void* p, std::size_t) noexcept {
    std::free(p);
}

namespace {

using bench::row;
constexpr int rows_per_batch = 1000;
constexpr int runs = 5;

std::vector<row> make_rows(int run) {
    std::vector<row> r;
    r.reserve(rows_per_batch);
    for (int i = 0; i < rows_per_batch; ++i) {
        const auto code = "CRC_" + std::to_string(run) + "_" + std::to_string(i);
        r.push_back({code,
                     "00000000-0000-0000-0000-000000000001",
                     1,
                     "Benchmark change reason category " + code,
                     "bench",
                     "bench",
                     "system.new",
                     "Created by the repository benchmark.",
                     {}});
    }
    return r;
}

struct sample {
    double ms;
    long allocs;
};

sample measure(const std::function<void()>& f) {
    const auto a = allocations.load();
    const auto t = std::chrono::steady_clock::now();
    f();
    return {std::chrono::duration<double, std::milli>(std::chrono::steady_clock::now() - t).count(),
            allocations.load() - a};
}

void report(const char* client, const char* workload, std::vector<sample> s) {
    std::ranges::sort(s, {}, &sample::ms);
    std::printf("%-12s %-28s median %8.2f ms  min %8.2f  max %8.2f  allocations %ld\n",
                client,
                workload,
                s[s.size() / 2].ms,
                s.front().ms,
                s.back().ms,
                s[s.size() / 2].allocs);
}

void run(const char* name, std::unique_ptr<bench::client> c) {
    std::vector<sample> writes, reads, points;
    for (int run = 0; run < runs; ++run) {
        const auto rows = make_rows(run);
        c->reset();
        writes.push_back(measure([&] { c->write_batch(rows); }));
        reads.push_back(measure([&] {
            if (c->read_latest().size() != static_cast<std::size_t>(rows_per_batch))
                throw std::runtime_error("read_latest returned the wrong count");
        }));
        points.push_back(measure([&] {
            for (const auto& v : rows)
                if (c->read_latest(v.code).size() != 1)
                    throw std::runtime_error("point read missed " + v.code);
        }));
    }
    report(name, "write 1000 rows", writes);
    report(name, "read all latest", reads);
    report(name, "1000 point reads", points);
}

}

int main() {
    using ores::platform::environment::environment;
    const bench::credentials c{environment::get_value_or_throw("BENCH_DB_HOST"),
                               environment::get_value_or_throw("BENCH_DB_NAME"),
                               environment::get_value_or_throw("BENCH_DB_USER"),
                               environment::get_value_or_throw("BENCH_DB_PASSWORD")};
    run("sqlgen", bench::make_sqlgen_client(c));
    run("pqxx", bench::make_pqxx_client(c));
    run("pqxx+copy", bench::make_pqxx_copy_client(c));
    run("libpq", bench::make_libpq_client(c));
}
