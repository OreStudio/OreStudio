#include "alloc_counter.hpp"
#include "ores.ore.core/domain/domain.hpp"
#include <chrono>
#include <cstdio>
#include <fstream>
#include <sstream>
#include <string>

namespace {
std::string slurp(const char* path) {
    std::ifstream f(path, std::ios::binary);
    std::ostringstream ss;
    ss << f.rdbuf();
    return ss.str();
}
long rss_kb() {
    std::ifstream f("/proc/self/status");
    std::string line;
    while (std::getline(f, line))
        if (line.rfind("VmHWM:", 0) == 0) return std::stol(line.substr(6));
    return 0;
}
}

int main(int, char** argv) {
    const std::string content = slurp(argv[1]);
    std::printf("file       : %s\n", argv[1]);
    std::printf("size       : %zu bytes\n", content.size());
    { ores::ore::domain::portfolio p; ores::ore::domain::load_data(content, p); }

    double best = 1e18;
    unsigned long long a = 0, b = 0;
    for (int i = 0; i < 5; ++i) {
        ores::ore::domain::portfolio p;
        bench::scope s;
        const auto t0 = std::chrono::steady_clock::now();
        ores::ore::domain::load_data(content, p);
        const double ms = std::chrono::duration<double, std::milli>(std::chrono::steady_clock::now() - t0).count();
        a = s.allocs(); b = s.bytes();
        if (ms < best) best = ms;
    }
    std::printf("xsdcpp load: %.2f ms  (allocs %llu, bytes %llu)\n", best, a, b);
    std::printf("peak rss   : %ld kB\n", rss_kb());
    return 0;
}
