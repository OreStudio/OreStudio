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
#include "ores.mq/pgmq/client.hpp"

#include <string>
#include <thread>
#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.testing/database_helper.hpp"

namespace {

const std::string test_suite("ores.mq.pgmq.tests");
const std::string tags("[pgmq][client]");

/**
 * @brief Returns a unique queue name for the current thread.
 *
 * Using thread ID avoids collisions when tests run in parallel.
 */
std::string unique_queue(const std::string& prefix) {
    return prefix + "_" +
        std::to_string(std::hash<std::thread::id>{}(std::this_thread::get_id()));
}

/**
 * @brief Simple aggregate payload used by typed send/read tests.
 *
 * rfl::json can reflect any aggregate struct; no macro annotation needed.
 */
struct test_payload {
    std::string task;
    int priority{0};
};

}

using namespace ores::logging;
using namespace ores::mq::pgmq;
using ores::testing::database_helper;

// ---------------------------------------------------------------------------
// Queue management
// ---------------------------------------------------------------------------

TEST_CASE("pgmq_queue_lifecycle", tags) {
    auto lg(make_logger(test_suite));
    database_helper h;
    const std::string q = unique_queue("lifecycle");
    client c;

    // Create queue – must appear in list_queues
    c.create(h.context(), q);
    {
        auto queues = c.list_queues(h.context());
        bool found = false;
        for (const auto& qi : queues) {
            if (qi.queue_name == q) { found = true; break; }
        }
        REQUIRE(found);
    }

    // Drop queue
    REQUIRE(c.drop(h.context(), q));

    // Second drop – queue no longer exists, returns false
    REQUIRE_FALSE(c.drop(h.context(), q));
}

TEST_CASE("pgmq_create_unlogged", tags) {
    auto lg(make_logger(test_suite));
    database_helper h;
    const std::string q = unique_queue("unlogged");
    client c;

    c.create_unlogged(h.context(), q);
    {
        auto queues = c.list_queues(h.context());
        bool found = false;
        for (const auto& qi : queues) {
            if (qi.queue_name == q) {
                found = true;
                REQUIRE(qi.is_unlogged);
                break;
            }
        }
        REQUIRE(found);
    }

    c.drop(h.context(), q);
}

// ---------------------------------------------------------------------------
// Send / read
// ---------------------------------------------------------------------------

TEST_CASE("pgmq_send_and_read_typed", tags) {
    auto lg(make_logger(test_suite));
    database_helper h;
    const std::string q = unique_queue("send_read");
    client c;
    c.create(h.context(), q);

    // Send a typed message
    test_payload p{.task = "process_task", .priority = 3};
    const auto msg_id = c.send(h.context(), q, p);
    REQUIRE(msg_id > 0);

    // Read back with 30-second visibility timeout
    auto msgs = c.read<test_payload>(h.context(), q, std::chrono::seconds(30));
    REQUIRE(msgs.size() == 1);
    REQUIRE(msgs[0].msg_id == msg_id);
    REQUIRE(msgs[0].read_ct == 1);
    REQUIRE(msgs[0].body.task == "process_task");
    REQUIRE(msgs[0].body.priority == 3);

    c.erase(h.context(), q, msg_id);
    c.drop(h.context(), q);
}

TEST_CASE("pgmq_send_and_read_raw_string", tags) {
    auto lg(make_logger(test_suite));
    database_helper h;
    const std::string q = unique_queue("send_raw");
    client c;
    c.create(h.context(), q);

    const std::string raw = R"({"key":"value","num":42})";
    const auto msg_id = c.send<std::string>(h.context(), q, raw);
    REQUIRE(msg_id > 0);

    // Read back as raw string – body is the JSONB text as-is
    auto msgs = c.read<std::string>(h.context(), q, std::chrono::seconds(30));
    REQUIRE(msgs.size() == 1);
    REQUIRE(msgs[0].msg_id == msg_id);
    REQUIRE_FALSE(msgs[0].body.empty());

    c.erase(h.context(), q, msg_id);
    c.drop(h.context(), q);
}

TEST_CASE("pgmq_send_batch", tags) {
    auto lg(make_logger(test_suite));
    database_helper h;
    const std::string q = unique_queue("send_batch");
    client c;
    c.create(h.context(), q);

    std::vector<test_payload> payloads = {
        {.task = "task_a", .priority = 1},
        {.task = "task_b", .priority = 2},
        {.task = "task_c", .priority = 3},
    };
    auto ids = c.send_batch(h.context(), q, payloads);
    REQUIRE(ids.size() == 3);
    for (const auto id : ids) REQUIRE(id > 0);

    // All three messages are readable
    auto msgs = c.read<test_payload>(h.context(), q, std::chrono::seconds(30), 10);
    REQUIRE(msgs.size() == 3);

    c.drop(h.context(), q);
}

// ---------------------------------------------------------------------------
// Pop
// ---------------------------------------------------------------------------

TEST_CASE("pgmq_pop", tags) {
    auto lg(make_logger(test_suite));
    database_helper h;
    const std::string q = unique_queue("pop");
    client c;
    c.create(h.context(), q);

    // Pop from empty queue returns nullopt
    REQUIRE_FALSE(c.pop<test_payload>(h.context(), q).has_value());

    // Send a message then pop it
    const auto msg_id = c.send(h.context(), q, test_payload{.task = "pop_me", .priority = 0});
    REQUIRE(msg_id > 0);

    auto msg = c.pop<test_payload>(h.context(), q);
    REQUIRE(msg.has_value());
    REQUIRE(msg->msg_id == msg_id);
    REQUIRE(msg->body.task == "pop_me");

    // Pop again – queue is empty
    REQUIRE_FALSE(c.pop<test_payload>(h.context(), q).has_value());

    c.drop(h.context(), q);
}

// ---------------------------------------------------------------------------
// Erase
// ---------------------------------------------------------------------------

TEST_CASE("pgmq_erase_single", tags) {
    auto lg(make_logger(test_suite));
    database_helper h;
    const std::string q = unique_queue("erase_single");
    client c;
    c.create(h.context(), q);

    const auto msg_id = c.send(h.context(), q, test_payload{.task = "erase_me", .priority = 0});

    REQUIRE(c.erase(h.context(), q, msg_id));

    // Erasing the same message again returns false
    REQUIRE_FALSE(c.erase(h.context(), q, msg_id));

    c.drop(h.context(), q);
}

TEST_CASE("pgmq_erase_batch", tags) {
    auto lg(make_logger(test_suite));
    database_helper h;
    const std::string q = unique_queue("erase_batch");
    client c;
    c.create(h.context(), q);

    std::vector<test_payload> payloads = {
        {.task = "a", .priority = 0},
        {.task = "b", .priority = 0},
        {.task = "c", .priority = 0}
    };
    const auto ids = c.send_batch(h.context(), q, payloads);
    REQUIRE(ids.size() == 3);

    auto erased = c.erase(h.context(), q, ids);
    REQUIRE(erased.size() == 3);

    c.drop(h.context(), q);
}

// ---------------------------------------------------------------------------
// Archive
// ---------------------------------------------------------------------------

TEST_CASE("pgmq_archive_single", tags) {
    auto lg(make_logger(test_suite));
    database_helper h;
    const std::string q = unique_queue("archive_single");
    client c;
    c.create(h.context(), q);

    const auto msg_id = c.send(h.context(), q, test_payload{.task = "archive_me", .priority = 0});

    REQUIRE(c.archive(h.context(), q, msg_id));

    // Archiving a message removes it from the main queue; archiving again fails
    REQUIRE_FALSE(c.archive(h.context(), q, msg_id));

    c.drop(h.context(), q);
}

TEST_CASE("pgmq_archive_batch", tags) {
    auto lg(make_logger(test_suite));
    database_helper h;
    const std::string q = unique_queue("archive_batch");
    client c;
    c.create(h.context(), q);

    std::vector<test_payload> payloads = {
        {.task = "x", .priority = 0},
        {.task = "y", .priority = 0}
    };
    const auto ids = c.send_batch(h.context(), q, payloads);
    REQUIRE(ids.size() == 2);

    auto archived = c.archive(h.context(), q, ids);
    REQUIRE(archived.size() == 2);

    c.drop(h.context(), q);
}

// ---------------------------------------------------------------------------
// Purge
// ---------------------------------------------------------------------------

TEST_CASE("pgmq_purge", tags) {
    auto lg(make_logger(test_suite));
    database_helper h;
    const std::string q = unique_queue("purge");
    client c;
    c.create(h.context(), q);

    std::vector<test_payload> payloads = {
        {.task = "p1", .priority = 0},
        {.task = "p2", .priority = 0},
        {.task = "p3", .priority = 0}
    };
    c.send_batch(h.context(), q, payloads);

    const auto n = c.purge(h.context(), q);
    REQUIRE(n == 3);

    // After purge the queue is empty
    auto msgs = c.read<test_payload>(h.context(), q, std::chrono::seconds(5));
    REQUIRE(msgs.empty());

    c.drop(h.context(), q);
}

// ---------------------------------------------------------------------------
// Visibility timeout
// ---------------------------------------------------------------------------

TEST_CASE("pgmq_set_visibility_timeout", tags) {
    auto lg(make_logger(test_suite));
    database_helper h;
    const std::string q = unique_queue("set_vt");
    client c;
    c.create(h.context(), q);

    const auto msg_id = c.send(h.context(), q, test_payload{.task = "vt_test", .priority = 0});

    // Read to lock message (vt = 60s)
    auto msgs = c.read<test_payload>(h.context(), q, std::chrono::seconds(60));
    REQUIRE(msgs.size() == 1);

    // Immediately shorten visibility to 0 so it becomes re-readable
    auto updated = c.set_visibility_timeout<test_payload>(
        h.context(), q, msg_id, std::chrono::seconds(0));
    REQUIRE(updated.has_value());
    REQUIRE(updated->msg_id == msg_id);

    c.erase(h.context(), q, msg_id);
    c.drop(h.context(), q);
}

// ---------------------------------------------------------------------------
// Metrics
// ---------------------------------------------------------------------------

TEST_CASE("pgmq_metrics", tags) {
    auto lg(make_logger(test_suite));
    database_helper h;
    const std::string q = unique_queue("metrics");
    client c;
    c.create(h.context(), q);

    // Send some messages
    c.send(h.context(), q, test_payload{.task = "m1", .priority = 0});
    c.send(h.context(), q, test_payload{.task = "m2", .priority = 0});

    auto m = c.metrics(h.context(), q);
    REQUIRE(m.queue_name == q);
    REQUIRE(m.queue_length >= 2);
    REQUIRE(m.total_messages >= 2);

    c.drop(h.context(), q);
}

TEST_CASE("pgmq_metrics_all", tags) {
    auto lg(make_logger(test_suite));
    database_helper h;
    const std::string q = unique_queue("metrics_all");
    client c;
    c.create(h.context(), q);
    c.send(h.context(), q, test_payload{.task = "x", .priority = 0});

    auto all = c.metrics_all(h.context());
    REQUIRE_FALSE(all.empty());

    // Our queue must appear in the result
    bool found = false;
    for (const auto& m : all) {
        if (m.queue_name == q) { found = true; break; }
    }
    REQUIRE(found);

    c.drop(h.context(), q);
}

// ---------------------------------------------------------------------------
// NOTIFY / LISTEN integration
// ---------------------------------------------------------------------------

TEST_CASE("pgmq_enable_disable_notify", tags) {
    auto lg(make_logger(test_suite));
    database_helper h;
    const std::string q = unique_queue("notify");
    client c;
    c.create(h.context(), q);

    // Should not throw
    c.enable_notify(h.context(), q);
    c.disable_notify(h.context(), q);

    c.drop(h.context(), q);
}

// ---------------------------------------------------------------------------
// Read-with-poll (short poll, empty queue → empty result)
// ---------------------------------------------------------------------------

TEST_CASE("pgmq_read_with_poll_empty_queue", tags) {
    auto lg(make_logger(test_suite));
    database_helper h;
    const std::string q = unique_queue("poll_empty");
    client c;
    c.create(h.context(), q);

    // Poll for up to 1 second on an empty queue; should return empty
    auto msgs = c.read_with_poll<test_payload>(
        h.context(), q,
        std::chrono::seconds(30), 1,
        std::chrono::seconds(1),
        std::chrono::milliseconds(100));
    REQUIRE(msgs.empty());

    c.drop(h.context(), q);
}
