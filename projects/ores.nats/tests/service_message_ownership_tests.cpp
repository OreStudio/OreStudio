/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
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
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/subscription.hpp"
#include "ores.testing/nats_options_helper.hpp"
#include <atomic>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <condition_variable>
#include <cstddef>
#include <fstream>
#include <mutex>
#include <random>
#include <string>
#include <vector>

// Every message a subscription delivers must be freed once its handler
// returns. A core subscription that kept them grew ores.marketdata.service by
// about 290 bytes per tick, until the machine ran out of memory.
//
// The subject is derived from this run, so no stream of the running fleet is
// touched and two runs never share one.
namespace {

using namespace std::chrono_literals;

constexpr std::size_t payload_size = 16 * 1024;
constexpr std::size_t warm_up_messages = 2'000;
constexpr std::size_t measured_messages = 20'000;
constexpr std::size_t batch_size = 500;

// Kept messages would hold about 312 MiB; freed ones leave the heap where it was.
constexpr long long allowed_growth_kib = 64 * 1024;

// The client compresses a payload when that is worth it, and a repeated byte
// compresses to almost nothing, so a kept message would cost only its header.
// Random bytes go out at full size.
std::vector<std::byte> incompressible_payload() {
    std::mt19937 gen(42);
    std::uniform_int_distribution<int> byte(0, 255);
    std::vector<std::byte> payload(payload_size);
    for (auto& b : payload)
        b = static_cast<std::byte>(byte(gen));
    return payload;
}

std::string unique_suffix() {
    static std::atomic<unsigned long long> counter{0};
    const auto now = std::chrono::steady_clock::now().time_since_epoch().count();
    return std::to_string(static_cast<unsigned long long>(now)) + "_" +
           std::to_string(counter.fetch_add(1));
}

long long anonymous_rss_kib() {
    std::ifstream status("/proc/self/status");
    std::string key;
    while (status >> key) {
        if (key == "RssAnon:") {
            long long kib = 0;
            status >> kib;
            return kib;
        }
        status.ignore(4096, '\n');
    }
    return -1;
}

struct delivery_counter {
    std::mutex mu;
    std::condition_variable cv;
    std::size_t received = 0;

    void count() {
        std::lock_guard lock(mu);
        ++received;
        cv.notify_all();
    }

    bool await(std::size_t target, std::chrono::milliseconds timeout) {
        std::unique_lock lock(mu);
        return cv.wait_for(lock, timeout, [&] { return received >= target; });
    }
};

void publish_and_drain(ores::nats::service::client& nats,
                       const std::string& subject,
                       delivery_counter& counter,
                       std::size_t& published,
                       std::size_t messages) {
    const auto payload = incompressible_payload();
    // Publishing in batches and waiting for each to arrive keeps the subscription
    // under its pending limit, so no message is dropped as a slow consumer.
    for (std::size_t sent = 0; sent < messages; sent += batch_size) {
        for (std::size_t i = 0; i < batch_size; ++i)
            nats.publish(subject, payload);
        published += batch_size;
        REQUIRE(counter.await(published, 10s));
    }
}

}

TEST_CASE("a core subscription frees every message it delivers", "[nats][subscription]") {
    ores::nats::service::client nats(ores::testing::make_nats_options());
    nats.connect();

    const std::string subject = "test.subscription.ownership." + unique_suffix();
    delivery_counter counter;
    auto sub = nats.subscribe(subject, [&counter](ores::nats::message) { counter.count(); });

    // A fresh subscription may not have reached the server yet, so publish until
    // one message arrives before counting anything.
    const std::vector<std::byte> probe{std::byte{1}};
    std::size_t published = 0;
    const auto deadline = std::chrono::steady_clock::now() + 10s;
    while (!counter.await(1, 100ms)) {
        REQUIRE(std::chrono::steady_clock::now() < deadline);
        nats.publish(subject, probe);
    }
    {
        std::lock_guard lock(counter.mu);
        published = counter.received;
    }

    publish_and_drain(nats, subject, counter, published, warm_up_messages);
    const auto before = anonymous_rss_kib();
    REQUIRE(before > 0);

    publish_and_drain(nats, subject, counter, published, measured_messages);
    const auto growth = anonymous_rss_kib() - before;

    INFO("anonymous RSS grew by " << growth << " KiB over " << measured_messages << " messages of "
                                  << payload_size << " bytes");
    CHECK(growth < allowed_growth_kib);
}
