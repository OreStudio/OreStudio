// Reads every XML document under a directory with the scanner and reports
// what it rejected, so the subset the scanner accepts is a measured claim
// rather than a stated one.
//
// Usage: corpus <directory> [more directories...]

#include "scanner.hpp"

#include <algorithm>
#include <chrono>
#include <cstdio>
#include <filesystem>
#include <fstream>
#include <map>
#include <string>
#include <vector>

namespace {

using clock_type = std::chrono::steady_clock;

auto slurp(const std::filesystem::path& path) -> std::string {
    std::ifstream file(path, std::ios::binary);
    return std::string(std::istreambuf_iterator<char>(file), std::istreambuf_iterator<char>());
}

struct tally {
    std::size_t documents = 0;
    std::size_t bytes = 0;
    std::size_t events = 0;
    std::size_t failed = 0;
    std::map<std::string, std::size_t> roots;
    std::vector<std::string> failures;
};

auto scan(const std::string& document, tally& totals) -> void {
    ore::xml::reader input(document);
    std::size_t events = 0;
    bool first = true;
    for (;;) {
        const auto ev = input.next();
        if (ev.kind == ore::xml::event_type::end_of_document)
            break;
        if (ev.kind == ore::xml::event_type::error) {
            totals.failed += 1;
            if (totals.failures.size() < 40)
                totals.failures.push_back(std::string(input.error()) + " at line " +
                                          std::to_string(input.line()) + ", column " +
                                          std::to_string(input.column()));
            return;
        }
        if (first && ev.kind == ore::xml::event_type::begin_element) {
            totals.roots[std::string(ev.name)] += 1;
            first = false;
        }
        ++events;
    }
    totals.events += events;
}

}

int main(int argc, char** argv) {
    if (argc < 2) {
        std::printf("usage: corpus <directory> [more directories...]\n");
        return 1;
    }

    tally totals;
    std::vector<std::filesystem::path> failures;
    const auto from = clock_type::now();

    for (int i = 1; i < argc; ++i) {
        const std::filesystem::path root(argv[i]);
        for (const auto& entry : std::filesystem::recursive_directory_iterator(root)) {
            if (!entry.is_regular_file())
                continue;
            const auto extension = entry.path().extension();
            if (extension != ".xml" && extension != ".XML")
                continue;
            const auto document = slurp(entry.path());
            if (document.empty())
                continue;

            tally one;
            scan(document, one);
            totals.documents += 1;
            totals.bytes += document.size();
            totals.events += one.events;
            totals.roots = [&] {
                auto merged = totals.roots;
                for (const auto& [name, count] : one.roots)
                    merged[name] += count;
                return merged;
            }();
            if (one.failed != 0) {
                totals.failed += 1;
                if (failures.size() < 200)
                    failures.push_back(entry.path());
                if (totals.failures.size() < 40)
                    for (const auto& message : one.failures)
                        totals.failures.push_back(entry.path().string() + ": " + message);
            }
        }
    }

    const auto seconds =
        std::chrono::duration<double>(clock_type::now() - from).count();

    std::printf("documents  : %zu\n", totals.documents);
    std::printf("bytes      : %zu (%.1f MB)\n",
                totals.bytes,
                static_cast<double>(totals.bytes) / (1024.0 * 1024.0));
    std::printf("events     : %zu\n", totals.events);
    std::printf("rejected   : %zu\n", totals.failed);
    std::printf("throughput : %.0f MB/s and %zu files/s\n",
                static_cast<double>(totals.bytes) / seconds / (1024.0 * 1024.0),
                static_cast<std::size_t>(static_cast<double>(totals.documents) / seconds));
    std::printf("root elements: %zu\n", totals.roots.size());
    for (const auto& [name, count] : totals.roots)
        std::printf("  %-40s %zu\n", name.c_str(), count);
    if (!totals.failures.empty()) {
        std::printf("first rejections:\n");
        for (const auto& message : totals.failures)
            std::printf("  %s\n", message.c_str());
    }
    return totals.failed == 0 ? 0 : 2;
}
