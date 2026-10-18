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
#include "ores.nats/config/nats_options.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/jetstream_admin.hpp"
#include "ores.nats/service/subscription.hpp"
#include "ores.testing/nats_options_helper.hpp"
#include <atomic>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <cstdint>
#include <nats/nats.h>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <thread>

// What a service shutdown does to its durable consumer, exercised directly
// with the client rather than by stopping a service. The production path is
// ensure_durable_consumer through the js_queue_subscribe helpers, so a service
// restart is only a drain followed by a new subscription with the same durable
// name. Under interest retention, the commands published between the two are
// kept only while that consumer exists, which is the property proved here.
//
// Every name is derived from this run, so a stream of the running fleet is
// never touched and two runs never share a consumer.
namespace {

using ores::nats::service::consumer_status;
using ores::nats::service::jetstream_admin;
using namespace std::chrono_literals;

/**
 * @brief A suffix no other run of this test can use.
 */
std::string unique_suffix() {
    static std::atomic<unsigned long long> counter{0};
    const auto now = std::chrono::steady_clock::now().time_since_epoch().count();
    return std::to_string(static_cast<unsigned long long>(now)) + "_" +
           std::to_string(counter.fetch_add(1));
}

/**
 * @brief Poll the server's consumer record until @p done, or the deadline.
 *
 * @return the last record read, or nothing when the consumer was removed while
 * waiting.
 */
template <typename Predicate>
std::optional<consumer_status> await_state(jetstream_admin admin,
                                           const std::string& stream,
                                           const std::string& consumer,
                                           Predicate done,
                                           std::chrono::milliseconds timeout) {
    const auto deadline = std::chrono::steady_clock::now() + timeout;
    std::optional<consumer_status> state;
    for (;;) {
        state = admin.consumer_state(stream, consumer);
        if (state && done(*state))
            return state;
        if (!state || std::chrono::steady_clock::now() >= deadline)
            return state;
        std::this_thread::sleep_for(25ms);
    }
}

/**
 * @brief Delete the test's stream when the case leaves, however it leaves.
 */
struct stream_cleanup {
    jetstream_admin admin;
    std::string stream;

    ~stream_cleanup() {
        try {
            admin.delete_stream(stream);
        } catch (...) {
            // A stream left behind is harmless; a throw from a destructor is not.
        }
    }
};

void require_ok(natsStatus status, std::string_view what) {
    if (status != NATS_OK)
        throw std::runtime_error(std::string(what) + ": " + natsStatus_GetText(status));
}

/**
 * @brief A raw cnats JetStream context.
 *
 * Only the negative case needs one: it has to call @c js_Subscribe, the call
 * the production helper replaced.
 */
struct raw_jetstream {
    natsConnection* conn = nullptr;
    jsCtx* js = nullptr;

    explicit raw_jetstream(const ores::nats::config::nats_options& options) {
        natsOptions* opts = nullptr;
        require_ok(natsOptions_Create(&opts), "natsOptions_Create");
        natsOptions_SetURL(opts, options.url.c_str());
        if (!options.tls_ca_cert.empty()) {
            natsOptions_SetSecure(opts, true);
            const natsStatus ca =
                natsOptions_LoadCATrustedCertificates(opts, options.tls_ca_cert.c_str());
            if (ca != NATS_OK) {
                natsOptions_Destroy(opts);
                require_ok(ca, "load the CA certificate");
            }
            const natsStatus chain = natsOptions_LoadCertificatesChain(
                opts, options.tls_client_cert.c_str(), options.tls_client_key.c_str());
            if (chain != NATS_OK) {
                natsOptions_Destroy(opts);
                require_ok(chain, "load the client certificate");
            }
        }
        const natsStatus connected = natsConnection_Connect(&conn, opts);
        natsOptions_Destroy(opts);
        require_ok(connected, "connect");
        require_ok(natsConnection_JetStream(&js, conn, nullptr), "natsConnection_JetStream");
    }

    ~raw_jetstream() {
        if (js != nullptr)
            jsCtx_Destroy(js);
        if (conn != nullptr) {
            natsConnection_Close(conn);
            natsConnection_Destroy(conn);
        }
    }
};

void discard_message(natsConnection*, natsSubscription*, natsMsg* msg, void*) {
    natsMsg_Destroy(msg);
}

}

TEST_CASE("a_durable_consumer_survives_the_drain_that_stops_a_service",
          "[nats][jetstream][integration]") {
    const auto suffix = unique_suffix();
    const std::string subject = "test.durable." + suffix;
    const std::string durable = "test_durable_" + suffix;
    const std::string queue = "ores.test.durable." + suffix;

    ores::nats::service::client nats(ores::testing::make_nats_options());
    nats.connect();
    const std::string stream = nats.make_stream_name("test_durable_" + suffix);
    auto admin = nats.make_admin();
    const stream_cleanup cleanup{admin, stream};
    admin.ensure_stream(stream,
                        nats.covering_subjects({subject}),
                        1,
                        ores::nats::service::stream_retention::interest);

    std::atomic<int> delivered{0};
    auto handler = [&delivered](ores::nats::message msg) {
        ores::nats::ack_guard ack(msg);
        ack.accept();
        delivered.fetch_add(1);
    };

    std::int64_t created_ns = 0;
    {
        auto bound = nats.js_queue_subscribe(subject, durable, queue, handler, stream, 10, 30s);
        const auto created =
            await_state(admin, stream, durable, [](const consumer_status&) { return true; }, 5s);
        REQUIRE(created.has_value());
        created_ns = created->created_ns;
        CHECK(created_ns > 0);
    }

    // Leaving the scope drained the subscription, which is what a service
    // shutdown does. The server no longer reports the consumer as bound, so the
    // drain has been processed and what remains is the record it kept.
    const auto drained = await_state(
        admin, stream, durable, [](const consumer_status& s) { return !s.push_bound; }, 5s);
    if (!drained)
        FAIL("the consumer did not survive the drain: the server removed the "
             "consumer, which is the defect this test pins");
    CHECK_FALSE(drained->push_bound);
    CHECK(drained->created_ns == created_ns);

    // The command is published with no subscriber, which is the down window.
    // Interest retention keeps it because the consumer still filters the
    // subject, and its position says the work is outstanding.
    const std::string command = "command published while the service was down";
    nats.js_publish(subject, ores::nats::as_bytes(command));
    const auto pending = await_state(
        admin, stream, durable, [](const consumer_status& s) { return s.num_pending == 1; }, 5s);
    if (!pending)
        FAIL("the command published while the service was down was not retained "
             "as outstanding work on the consumer");
    CHECK(pending->num_pending == 1);
    CHECK(pending->num_ack_pending == 0);
    CHECK(pending->ack_floor_stream == 0);
    CHECK(pending->num_redelivered == 0);

    {
        auto bound = nats.js_queue_subscribe(subject, durable, queue, handler, stream, 10, 30s);
        const auto served = await_state(
            admin,
            stream,
            durable,
            [](const consumer_status& s) {
                return s.num_pending == 0 && s.num_ack_pending == 0 && s.ack_floor_stream == 1;
            },
            10s);
        if (!served)
            FAIL("the command published during the down window was not delivered "
                 "and acknowledged after the subscription came back");
        CHECK(served->num_pending == 0);
        CHECK(served->num_ack_pending == 0);
        CHECK(served->ack_floor_stream == 1);
        CHECK(served->delivered == 1);
        CHECK(served->num_redelivered == 0);
        // The creation time is the identity of the consumer. An unchanged one
        // means the second subscription attached to the stored position rather
        // than making a new consumer.
        CHECK(served->created_ns == created_ns);
        CHECK(delivered.load() == 1);
    }
}

TEST_CASE("a_consumer_created_by_js_subscribe_is_removed_when_its_subscription_drains",
          "[nats][jetstream][integration]") {
    const auto suffix = unique_suffix();
    const std::string subject = "test.implicit." + suffix;
    const std::string durable = "test_implicit_" + suffix;
    const std::string queue = "ores.test.implicit." + suffix;

    const auto options = ores::testing::make_nats_options();
    ores::nats::service::client nats(options);
    nats.connect();
    const std::string stream = nats.make_stream_name("test_implicit_" + suffix);
    auto admin = nats.make_admin();
    const stream_cleanup cleanup{admin, stream};
    admin.ensure_stream(stream,
                        nats.covering_subjects({subject}),
                        1,
                        ores::nats::service::stream_retention::interest);

    raw_jetstream raw(options);
    const std::string full_subject = nats.make_subject(subject);

    // Exactly the call the production helper used before the fix: the consumer
    // is named in the subscription options, but not created, so the server ties
    // it to this subscription.
    jsSubOptions sub_options;
    jsSubOptions_Init(&sub_options);
    sub_options.Config.Durable = durable.c_str();
    sub_options.Config.AckPolicy = js_AckExplicit;
    sub_options.Config.MaxDeliver = 10;
    sub_options.Queue = queue.c_str();

    natsSubscription* sub = nullptr;
    const natsStatus subscribed = js_Subscribe(&sub,
                                               raw.js,
                                               full_subject.c_str(),
                                               discard_message,
                                               nullptr,
                                               nullptr,
                                               &sub_options,
                                               nullptr);
    require_ok(subscribed, "js_Subscribe with an implicit consumer");

    const auto created =
        await_state(admin, stream, durable, [](const consumer_status&) { return true; }, 5s);
    REQUIRE(created.has_value());

    natsSubscription_Drain(sub);
    natsSubscription_WaitForDrainCompletion(sub, 5000);
    natsSubscription_Destroy(sub);

    // The same drain that leaves the production consumer in place removes this
    // one. If it did not, the main case would pass against the old behaviour
    // and would prove nothing.
    const auto removed =
        await_state(admin, stream, durable, [](const consumer_status&) { return false; }, 5s);
    INFO("an implicitly created consumer must disappear on drain");
    CHECK_FALSE(removed.has_value());
}

TEST_CASE("a_stored_consumer_takes_the_delivery_bound_the_code_asks_for",
          "[nats][jetstream][integration]") {
    const auto suffix = unique_suffix();
    const std::string subject = "test.migrate." + suffix;
    const std::string durable = "test_migrate_" + suffix;
    const std::string queue = "ores.test.migrate." + suffix;

    ores::nats::service::client nats(ores::testing::make_nats_options());
    nats.connect();
    const std::string stream = nats.make_stream_name("test_migrate_" + suffix);
    auto admin = nats.make_admin();
    const stream_cleanup cleanup{admin, stream};
    admin.ensure_stream(stream,
                        nats.covering_subjects({subject}),
                        1,
                        ores::nats::service::stream_retention::interest);

    std::atomic<int> delivered{0};
    auto handler = [&delivered](ores::nats::message msg) {
        ores::nats::ack_guard ack(msg);
        ack.accept();
        delivered.fetch_add(1);
    };

    // The first start writes its numbers onto the consumer.
    {
        auto bound = nats.js_queue_subscribe(subject, durable, queue, handler, stream, 10, 30s);
        const auto created =
            await_state(admin, stream, durable, [](const consumer_status&) { return true; }, 5s);
        REQUIRE(created.has_value());
        CHECK(created->max_deliver == 10);
        CHECK(created->ack_wait_ns == 30'000'000'000LL);

        // One command is served, so the position to preserve is a real one.
        nats.js_publish(subject, ores::nats::as_bytes("serve before the change"));
        const auto served = await_state(
            admin,
            stream,
            durable,
            [](const consumer_status& s) { return s.ack_floor_stream == 1; },
            10s);
        REQUIRE(served.has_value());
        CHECK(served->max_deliver == 10);
        CHECK(served->created_ns == created->created_ns);
    }

    // A later start asks for different numbers. The consumer is updated in
    // place, not replaced: the creation time and the acknowledged position are
    // the same consumer's. This is the restart path a live broker takes when a
    // service's bound changes.
    {
        auto bound = nats.js_queue_subscribe(subject, durable, queue, handler, stream, 3, 5s);
        const auto migrated = await_state(
            admin,
            stream,
            durable,
            [](const consumer_status& s) {
                return s.max_deliver == 3 && s.ack_wait_ns == 5'000'000'000LL;
            },
            5s);
        if (!migrated)
            FAIL("the stored consumer did not take the delivery bound and ack wait the "
                 "subscription asked for");
        CHECK(migrated->max_deliver == 3);
        CHECK(migrated->ack_wait_ns == 5'000'000'000LL);
        CHECK(migrated->ack_floor_stream == 1);
        CHECK(migrated->num_pending == 0);
        CHECK(migrated->created_ns > 0);
        CHECK(delivered.load() == 1);
        // The position was kept, not replayed.
        CHECK(migrated->delivered >= 1);
    }
}
