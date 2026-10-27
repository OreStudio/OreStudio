/*
 * The per-field conversions both codecs share, timed alone, to show how much
 * of each codec's time they account for.
 */
#include "ores.platform/time/datetime.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <algorithm>
#include <chrono>
#include <cstdio>
#include <string>
#include <vector>

namespace {

template <typename F>
double median_ns(int iterations, F&& f) {
    std::vector<double> runs;
    for (int run = 0; run < 7; ++run) {
        const auto t = std::chrono::steady_clock::now();
        for (int i = 0; i < iterations; ++i)
            f();
        runs.push_back(
            std::chrono::duration<double, std::nano>(std::chrono::steady_clock::now() - t).count() /
            iterations);
    }
    std::ranges::sort(runs);
    return runs[runs.size() / 2];
}

}

int main() {
    using ores::platform::time::datetime;
    using ores::utility::uuid::tenant_id;
    const auto at = std::chrono::system_clock::time_point(std::chrono::seconds(1793000000));
    const auto at_text = datetime::to_iso8601_utc(at);
    const auto tid = tenant_id::system();
    const auto tid_text = tid.to_string();
    volatile std::size_t sink = 0;
    std::printf("datetime::to_iso8601_utc     %8.0f ns\n",
                median_ns(100000, [&] { sink = sink + datetime::to_iso8601_utc(at).size(); }));
    std::printf("datetime::from_iso8601_utc   %8.0f ns\n", median_ns(100000, [&] {
                    sink = sink + datetime::from_iso8601_utc(at_text).time_since_epoch().count();
                }));
    std::printf("tenant_id::to_string         %8.0f ns\n",
                median_ns(100000, [&] { sink = sink + tid.to_string().size(); }));
    std::printf("tenant_id::from_string       %8.0f ns\n", median_ns(100000, [&] {
                    sink = sink + tenant_id::from_string(tid_text).has_value();
                }));
}
