/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.telemetry/domain/trace_id.hpp"
#include "ores.telemetry/generators/trace_id_generator.hpp"

#include <catch2/catch_test_macros.hpp>
#include <set>
#include <thread>
#include <vector>
#include "ores.telemetry/log/make_logger.hpp"

namespace {

const std::string test_suite("ores.telemetry.tests");
const std::string tags("[trace_id]");

}

using namespace ores::telemetry::domain;
using namespace ores::telemetry::generators;
using namespace ores::telemetry::log;

TEST_CASE("default_trace_id_is_invalid", tags) {
    auto lg(make_logger(test_suite));
    trace_id id;
    REQUIRE_FALSE(id.is_valid());
    BOOST_LOG_SEV(lg, debug) << "Default trace_id: " << id.to_hex();
}

TEST_CASE("generated_trace_id_is_valid", tags) {
    auto lg(make_logger(test_suite));
    trace_id_generator gen;
    const auto id = gen();

    REQUIRE(id.is_valid());
    BOOST_LOG_SEV(lg, debug) << "Generated trace_id: " << id.to_hex();
}

TEST_CASE("trace_id_to_hex_produces_32_chars", tags) {
    trace_id_generator gen;
    const auto id = gen();
    const auto hex = id.to_hex();

    REQUIRE(hex.size() == 32);
}

TEST_CASE("trace_id_from_hex_roundtrips", tags) {
    trace_id_generator gen;
    const auto original = gen();
    const auto hex = original.to_hex();
    const auto parsed = trace_id::from_hex(hex);

    REQUIRE(original == parsed);
}

TEST_CASE("trace_id_from_hex_with_invalid_input_returns_invalid", tags) {
    // Too short
    auto id1 = trace_id::from_hex("abc");
    REQUIRE_FALSE(id1.is_valid());

    // Too long
    auto id2 = trace_id::from_hex("00000000000000000000000000000000extra");
    REQUIRE_FALSE(id2.is_valid());

    // Invalid hex chars
    auto id3 = trace_id::from_hex("0000000000000000000000000000gggg");
    REQUIRE_FALSE(id3.is_valid());
}

TEST_CASE("generated_trace_ids_are_unique", tags) {
    trace_id_generator gen;
    std::set<std::string> ids;

    for (int i = 0; i < 1000; ++i) {
        const auto id = gen();
        const auto hex = id.to_hex();
        REQUIRE(ids.find(hex) == ids.end());
        ids.insert(hex);
    }
}

TEST_CASE("trace_id_generator_uses_machine_id", tags) {
    auto lg(make_logger(test_suite));
    trace_id_generator gen(0x1234);

    REQUIRE(gen.machine_id() == 0x1234);

    const auto id = gen();
    // Machine ID is in bytes 6-7
    const auto machine_high = static_cast<std::uint8_t>(id.bytes[6]);
    const auto machine_low = static_cast<std::uint8_t>(id.bytes[7]);
    const auto machine_id = (static_cast<std::uint16_t>(machine_high) << 8) |
                            static_cast<std::uint16_t>(machine_low);

    REQUIRE(machine_id == 0x1234);
    BOOST_LOG_SEV(lg, debug) << "Trace ID with machine 0x1234: " << id.to_hex();
}

TEST_CASE("trace_ids_are_time_sortable", tags) {
    trace_id_generator gen;

    const auto id1 = gen();
    std::this_thread::sleep_for(std::chrono::milliseconds(2));
    const auto id2 = gen();

    // The defaulted operator<=> allows for direct comparison.
    REQUIRE(id1 <= id2);
}

TEST_CASE("trace_id_generator_is_thread_safe", tags) {
    trace_id_generator gen;
    std::vector<std::thread> threads;
    std::mutex mutex;
    std::set<std::string> all_ids;
    constexpr int ids_per_thread = 100;
    constexpr int num_threads = 4;

    for (int t = 0; t < num_threads; ++t) {
        threads.emplace_back([&gen, &mutex, &all_ids]() {
            std::vector<std::string> local_ids;
            for (int i = 0; i < ids_per_thread; ++i) {
                const auto id = gen();
                local_ids.push_back(id.to_hex());
            }

            std::lock_guard<std::mutex> lock(mutex);
            for (const auto& id : local_ids) {
                all_ids.insert(id);
            }
        });
    }

    for (auto& t : threads) {
        t.join();
    }

    // All IDs should be unique
    REQUIRE(all_ids.size() == num_threads * ids_per_thread);
}
