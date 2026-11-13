/* -*- mode: c++; tab-width: 4; c-indent-level: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.marketdata.service/app/staleness_policy.hpp"
#include <atomic>
#include <charconv>
#include <cstdlib>
#include <string_view>

namespace ores::marketdata::service::app {

namespace {

constexpr std::int64_t default_threshold_seconds = 3600;

struct threshold_state final {
    std::atomic<std::int64_t> seconds{default_threshold_seconds};
    std::atomic<bool> configured{false};
    std::atomic<bool> read{false};
};

threshold_state& state() {
    static threshold_state instance;
    return instance;
}

void read_environment_once() {
    auto& s = state();
    if (s.read.exchange(true))
        return;
    const auto* raw = std::getenv("ORES_MARKETDATA_STALE_SECONDS");
    if (raw == nullptr)
        return;
    std::int64_t value = 0;
    const std::string_view text{raw};
    const auto parsed = std::from_chars(text.data(), text.data() + text.size(), value);
    if (parsed.ec != std::errc{} || value <= 0)
        return;
    s.seconds.store(value);
    s.configured.store(true);
}

}

std::int64_t staleness_policy::threshold_seconds() {
    read_environment_once();
    return state().seconds.load();
}

bool staleness_policy::is_configured() {
    read_environment_once();
    return state().configured.load();
}

void staleness_policy::configure(std::int64_t seconds) {
    auto& s = state();
    s.read.store(true);
    s.seconds.store(seconds > 0 ? seconds : default_threshold_seconds);
    s.configured.store(true);
}

std::int64_t staleness_policy::age_seconds(std::chrono::system_clock::time_point as_of,
                                           std::chrono::system_clock::time_point now) {
    if (as_of == std::chrono::system_clock::time_point{})
        return 0;
    const auto age = std::chrono::duration_cast<std::chrono::seconds>(now - as_of).count();
    return age < 0 ? 0 : age;
}

bool staleness_policy::is_stale(std::chrono::system_clock::time_point as_of,
                                std::chrono::system_clock::time_point now) {
    if (as_of == std::chrono::system_clock::time_point{})
        return true;
    return age_seconds(as_of, now) > threshold_seconds();
}

}
