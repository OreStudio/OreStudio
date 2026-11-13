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
#include <optional>
#include <stdexcept>
#include <string>
#include <vector>

namespace {

using ores::iam::service::call_with_retry;
using ores::iam::service::classify_request_failure;
using ores::iam::service::do_unless_already_done;
using ores::iam::service::request_failure_class;
using ores::iam::service::request_retry_policy;
using ores::iam::service::request_with_retry;
using ores::iam::service::retry_attempt;
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
    std::vector<std::chrono::milliseconds> timeouts;
    std::size_t calls = 0;

    template <typename Request>
    typename Request::response_type request(const Request&, std::chrono::milliseconds timeout) {
        timeouts.push_back(timeout);
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

TEST_CASE("a_timeout_and_a_missing_responder_are_transient", "[request_retry]") {
    CHECK(classify_request_failure(nats_connect_error(nats_error_kind::request_timeout, "late")) ==
          request_failure_class::transient);
    CHECK(classify_request_failure(
              nats_connect_error(nats_error_kind::services_unavailable, "nobody")) ==
          request_failure_class::transient);
}

TEST_CASE("a_refusal_and_an_unnamed_fault_are_not_retried", "[request_retry]") {
    CHECK(classify_request_failure(ores::iam::service::internal_request_client::service_error(
              "forbidden", "test.v1.ops.answer")) == request_failure_class::refused);
    CHECK(classify_request_failure(nats_connect_error(nats_error_kind::tls_error, "tls")) ==
          request_failure_class::permanent);
    CHECK(classify_request_failure(std::runtime_error("the reply did not parse")) ==
          request_failure_class::permanent);
}

TEST_CASE("a_request_that_misses_its_deadline_once_and_answers_is_retried", "[request_retry]") {
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

TEST_CASE("a_refusal_is_thrown_at_once_with_the_servers_reason", "[request_retry]") {
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

TEST_CASE("the_attempts_stop_at_the_policys_limit", "[request_retry]") {
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
    // The report keeps the reason it gave up on.
    CHECK_THAT(said, Catch::Matchers::ContainsSubstring("late"));
}

TEST_CASE("a_steps_failure_names_the_party_the_item_and_the_attempts", "[request_retry]") {
    scripted_client client;
    client.script.push_back([]() -> int {
        throw nats_connect_error(nats_error_kind::request_timeout,
                                 "NATS request timed out: no reply from "
                                 "'marketdata.v1.ops.calibrate_fx_vol_surface' within 120000 ms");
    });

    wait_recorder waits;
    auto policy = waits.policy();
    policy.max_attempts = 1;

    std::string said;
    try {
        static_cast<void>(request_with_retry(
            client,
            test_request{},
            options_after(std::chrono::seconds{30}, policy, "ACME: EUR/USD surface calibration")));
        FAIL("the request was not abandoned");
    } catch (const std::runtime_error& e) {
        said = e.what();
    }

    // A person reading the run knows which party, which item, how many
    // attempts, and what the transport said.
    CHECK_THAT(said, Catch::Matchers::ContainsSubstring("ACME: EUR/USD surface calibration"));
    CHECK_THAT(said, Catch::Matchers::ContainsSubstring("failed after 1 attempt"));
    CHECK_THAT(said,
               Catch::Matchers::ContainsSubstring("marketdata.v1.ops.calibrate_fx_vol_surface"));
}

TEST_CASE("a_request_is_not_retried_past_the_callers_deadline", "[request_retry]") {
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

TEST_CASE("an_attempt_waits_no_longer_than_the_budget_left", "[request_retry]") {
    scripted_client client;
    client.script.push_back([] { return 7; });

    wait_recorder waits;
    auto options = options_after(std::chrono::milliseconds{40}, waits.policy(), "a try");
    // The request names a patience far longer than the step has left, so the
    // attempt is cut to the budget or it would outlive the step.
    options.attempt_timeout = std::chrono::seconds{120};

    CHECK(request_with_retry(client, test_request{}, options) == 7);
    REQUIRE(client.timeouts.size() == 1);
    CHECK(client.timeouts.front() <= std::chrono::milliseconds{40});
    CHECK(client.timeouts.front() >= std::chrono::milliseconds{1});
}

TEST_CASE("a_repeat_takes_the_result_an_earlier_attempt_already_produced", "[request_retry]") {
    wait_recorder waits;
    int publishes = 0;
    int attempts = 0;
    bool published = false;

    // The first attempt publishes and loses the reply. The second reads what
    // the first published, so the work happens once and the answer is taken.
    const auto answer = call_with_retry(
        [&](const retry_attempt&) -> int {
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

TEST_CASE("an_attempt_knows_which_time_round_it_is_and_what_is_left", "[request_retry]") {
    wait_recorder waits;
    std::vector<int> numbers;
    std::vector<std::chrono::milliseconds> budgets;

    const auto answer = call_with_retry(
        [&](const retry_attempt& attempt) -> int {
            numbers.push_back(attempt.number);
            budgets.push_back(attempt.budget_left);
            if (attempt.number == 1)
                throw nats_connect_error(nats_error_kind::request_timeout, "late");
            return 7;
        },
        {.deadline = std::chrono::steady_clock::now() + std::chrono::seconds{10},
         .what = "a surface",
         .policy = waits.policy()});

    CHECK(answer == 7);
    CHECK(numbers == std::vector<int>{1, 2});
    REQUIRE(budgets.size() == 2);
    // The budget is the caller's, counted down: both attempts see time left,
    // and neither sees more than the caller stated.
    CHECK(budgets.front() > std::chrono::milliseconds{9000});
    CHECK(budgets.front() <= std::chrono::seconds{10});
    CHECK(budgets.back() > std::chrono::milliseconds{0});
}

TEST_CASE("the_first_attempt_does_not_read_back_and_a_repeat_does", "[request_retry]") {
    wait_recorder waits;
    int reads = 0;
    int works = 0;

    const auto answer = call_with_retry(
        [&](const retry_attempt& attempt) -> int {
            return do_unless_already_done(
                attempt,
                [&]() -> std::optional<int> {
                    ++reads;
                    return std::nullopt;
                },
                [&]() -> int {
                    ++works;
                    if (attempt.number == 1)
                        throw nats_connect_error(nats_error_kind::request_timeout, "late");
                    return 7;
                });
        },
        {.deadline = std::chrono::steady_clock::now() + std::chrono::seconds{30},
         .what = "a curve publish",
         .policy = waits.policy()});

    CHECK(answer == 7);
    // The first attempt did not read: no earlier attempt of this chain had run.
    CHECK(reads == 1);
    CHECK(works == 2);
}

TEST_CASE("a_repeat_whose_read_back_answers_does_not_do_the_work_again", "[request_retry]") {
    wait_recorder waits;
    int works = 0;

    const auto answer = call_with_retry(
        [&](const retry_attempt& attempt) -> int {
            return do_unless_already_done(
                attempt,
                []() -> std::optional<int> { return 7; },
                [&]() -> int {
                    ++works;
                    throw nats_connect_error(nats_error_kind::request_timeout, "late");
                });
        },
        {.deadline = std::chrono::steady_clock::now() + std::chrono::seconds{30},
         .what = "a curve publish",
         .policy = waits.policy()});

    CHECK(answer == 7);
    CHECK(works == 1);
    CHECK(waits.waits.size() == 1);
}

TEST_CASE("an_attempt_is_reported_to_the_runs_own_record", "[request_retry]") {
    scripted_client client;
    client.script.push_back(
        []() -> int { throw nats_connect_error(nats_error_kind::request_timeout, "late"); });
    client.script.push_back([] { return 7; });

    wait_recorder waits;
    std::vector<std::string> notes;
    auto options = options_after(std::chrono::seconds{30}, waits.policy(), "ACME: EUR/USD surface");
    options.note = [&notes](const std::string& line) {
        notes.push_back(line);
    };

    CHECK(request_with_retry(client, test_request{}, options) == 7);
    REQUIRE(notes.size() == 1);
    CHECK_THAT(notes.front(), Catch::Matchers::ContainsSubstring("ACME: EUR/USD surface"));
    CHECK_THAT(notes.front(), Catch::Matchers::ContainsSubstring("attempt 1 failed"));
    CHECK_THAT(notes.front(), Catch::Matchers::ContainsSubstring("late"));
}
