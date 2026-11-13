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
#include "ores.iam.core/service/request_retry.hpp"
#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_string.hpp>
#include <chrono>
#include <functional>
#include <stdexcept>
#include <string>
#include <vector>

namespace {

using ores::iam::service::call_with_retry;
using ores::iam::service::classify_request_failure;
using ores::iam::service::request_failure_class;
using ores::iam::service::request_retry_policy;
using ores::iam::service::request_with_retry;
using ores::iam::service::retry_request_options;
using ores::nats::service::nats_connect_error;
using ores::nats::service::nats_error_kind;

/// The smallest request the helper needs: a response type and a subject.
struct test_request {
    using response_type = int;
    static constexpr std::string_view nats_subject = "test.v1.ops.answer";
};

/// A client that answers from a script, one entry per attempt.
struct scripted_client {
    std::vector<std::function<int()>> script;
    std::size_t calls = 0;

    template <typename Request>
    typename Request::response_type request(const Request&, std::chrono::milliseconds) {
        const auto index = calls++;
        if (index >= script.size())
            throw std::runtime_error("the script ran out");
        return script[index]();
    }
};

/// Records each wait rather than spending it, so a test runs in no time.
struct wait_recorder {
    std::vector<std::chrono::milliseconds> waits;

    request_retry_policy policy() {
        request_retry_policy out;
        out.wait = [this](std::chrono::milliseconds delay) {
            waits.push_back(delay);
        };
        return out;
    }
};

retry_request_options
options_after(std::chrono::milliseconds budget, request_retry_policy policy, std::string what) {
    return retry_request_options{.deadline = std::chrono::steady_clock::now() + budget,
                                 .attempt_timeout = std::chrono::milliseconds{1000},
                                 .what = std::move(what),
                                 .policy = std::move(policy)};
}

}

TEST_CASE("A timeout and a missing responder are transient", "[request_retry]") {
    CHECK(classify_request_failure(nats_connect_error(nats_error_kind::request_timeout, "late")) ==
          request_failure_class::transient);
    CHECK(classify_request_failure(
              nats_connect_error(nats_error_kind::services_unavailable, "nobody")) ==
          request_failure_class::transient);
}

TEST_CASE("A refusal and an unnamed fault are not retried", "[request_retry]") {
    CHECK(classify_request_failure(ores::iam::service::internal_request_client::service_error(
              "forbidden", "test.v1.ops.answer")) == request_failure_class::refused);
    CHECK(classify_request_failure(nats_connect_error(nats_error_kind::tls_error, "tls")) ==
          request_failure_class::permanent);
    CHECK(classify_request_failure(std::runtime_error("the reply did not parse")) ==
          request_failure_class::permanent);
}

TEST_CASE("A request that misses its deadline once and answers is retried", "[request_retry]") {
    scripted_client client;
    client.script.push_back(
        []() -> int { throw nats_connect_error(nats_error_kind::request_timeout, "late"); });
    client.script.push_back([] { return 7; });

    wait_recorder waits;
    const auto answer =
        request_with_retry(client,
                           test_request{},
                           options_after(std::chrono::seconds{30}, waits.policy(), "a surface"));

    CHECK(answer == 7);
    CHECK(client.calls == 2);
    CHECK(waits.waits.size() == 1);
    CHECK(waits.waits.front() >= std::chrono::milliseconds{250});
    CHECK(waits.waits.front() <= std::chrono::milliseconds{500});
}

TEST_CASE("A refusal is thrown at once, with the server's reason", "[request_retry]") {
    scripted_client client;
    client.script.push_back([]() -> int {
        throw ores::iam::service::internal_request_client::service_error("forbidden", "test");
    });

    wait_recorder waits;
    std::string said;
    try {
        static_cast<void>(request_with_retry(
            client,
            test_request{},
            options_after(std::chrono::seconds{30}, waits.policy(), "a surface")));
        FAIL("the refusal was not thrown");
    } catch (const std::runtime_error& e) {
        said = e.what();
    }

    CHECK(client.calls == 1);
    CHECK(waits.waits.empty());
    CHECK_THAT(said, Catch::Matchers::ContainsSubstring("a surface failed after 1 attempt"));
    CHECK_THAT(said, Catch::Matchers::ContainsSubstring("forbidden"));
}

TEST_CASE("The attempts stop at the policy's limit", "[request_retry]") {
    scripted_client client;
    for (int i = 0; i < 5; ++i)
        client.script.push_back(
            []() -> int { throw nats_connect_error(nats_error_kind::request_timeout, "late"); });

    wait_recorder waits;
    auto policy = waits.policy();
    policy.max_attempts = 3;

    std::string said;
    try {
        static_cast<void>(request_with_retry(
            client, test_request{}, options_after(std::chrono::seconds{30}, policy, "a surface")));
        FAIL("the request was not abandoned");
    } catch (const std::runtime_error& e) {
        said = e.what();
    }

    CHECK(client.calls == 3);
    CHECK(waits.waits.size() == 2);
    // The wait grows, and the second is never shorter than the first.
    CHECK(waits.waits[1] >= waits.waits[0]);
    CHECK_THAT(said, Catch::Matchers::ContainsSubstring("failed after 3 attempts"));
}

TEST_CASE("A request is not retried past the caller's deadline", "[request_retry]") {
    scripted_client client;
    client.script.push_back(
        []() -> int { throw nats_connect_error(nats_error_kind::request_timeout, "late"); });
    client.script.push_back([] { return 7; });

    wait_recorder waits;
    auto options = options_after(std::chrono::milliseconds{0}, waits.policy(), "a surface");

    CHECK_THROWS_AS(request_with_retry(client, test_request{}, options), std::runtime_error);
    CHECK(client.calls == 1);
    CHECK(waits.waits.empty());
}

TEST_CASE("A repeat takes the result an earlier attempt already produced", "[request_retry]") {
    wait_recorder waits;
    int publishes = 0;
    int attempts = 0;
    bool published = false;

    // The first attempt publishes and loses the reply. The second reads what
    // the first published, so the work happens once and the answer is taken.
    const auto answer = call_with_retry(
        [&]() -> int {
            ++attempts;
            if (published)
                return 7;
            ++publishes;
            published = true;
            throw nats_connect_error(nats_error_kind::request_timeout, "reply lost");
        },
        {.deadline = std::chrono::steady_clock::now() + std::chrono::seconds{30},
         .what = "a surface publish",
         .policy = waits.policy()});

    CHECK(answer == 7);
    CHECK(attempts == 2);
    CHECK(publishes == 1);
    CHECK(waits.waits.size() == 1);
}
