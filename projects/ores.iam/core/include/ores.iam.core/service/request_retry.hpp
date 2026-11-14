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
#ifndef ORES_IAM_SERVICE_REQUEST_RETRY_HPP
#define ORES_IAM_SERVICE_REQUEST_RETRY_HPP

#include "ores.iam.core/service/internal_request_client.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/nats_connect_error.hpp"
#include "ores.nats/service/timeouts.hpp"
#include <algorithm>
#include <chrono>
#include <cstdint>
#include <functional>
#include <random>
#include <stdexcept>
#include <string>
#include <thread>
#include <type_traits>
#include <utility>

namespace ores::iam::service {

/**
 * @brief Whether a failed request is worth asking again.
 *
 * The three answers are the three things a caller can do about a failure: try
 * again, stop and report the server's reason, or stop and report the fault.
 */
enum class request_failure_class {
    /**
     * The transport could not deliver the request or bring the reply back in
     * time, and nothing says the server refused it. A second attempt may pass.
     */
    transient,
    /**
     * The server answered and refused: an expired token, a permission, a bad
     * request. Asking again with the same request changes nothing.
     */
    refused,
    /**
     * Anything else: a reply that would not parse, a fault inside the service,
     * an unrecognised transport status. A second attempt repeats the fault.
     */
    permanent
};

/**
 * @brief Reads a failed request's exception as one of the three answers.
 *
 * A timeout and a missing responder are transient. A refusal from the service
 * is refused. Everything else is permanent, because nothing in the failure
 * says it would pass.
 */
inline request_failure_class classify_request_failure(const std::exception& failure) {
    if (dynamic_cast<const internal_request_client::service_error*>(&failure))
        return request_failure_class::refused;

    if (const auto* transport =
            dynamic_cast<const ores::nats::service::nats_connect_error*>(&failure)) {
        switch (transport->kind()) {
            case ores::nats::service::nats_error_kind::request_timeout:
            case ores::nats::service::nats_error_kind::services_unavailable:
                return request_failure_class::transient;
            default:
                return request_failure_class::permanent;
        }
    }
    return request_failure_class::permanent;
}

/**
 * @brief How many attempts a request gets, how long it waits between them, and
 * how it waits.
 *
 * The wait is a member so a test can replace it. Production waits on the
 * thread, which is what a synchronous step handler has.
 */
struct request_retry_policy {
    /// The most attempts one request may take, the first one included.
    int max_attempts = 3;
    /// The wait before the second attempt. It doubles after each attempt.
    std::chrono::milliseconds first_delay{500};
    /// The longest wait between two attempts.
    std::chrono::milliseconds max_delay{4000};
    /// How the wait is spent. A test replaces this with a recorder.
    std::function<void(std::chrono::milliseconds)> wait = [](std::chrono::milliseconds delay) {
        std::this_thread::sleep_for(delay);
    };
};

/**
 * @brief One try at the work: which time round it is, and what is left of the
 * caller's budget to spend on it.
 *
 * The operation reads this rather than assuming it may take its own time. A
 * callable that would wait, or that asks a service and waits for its reply,
 * sizes that wait from @c budget_left, so the chain of attempts cannot outlive
 * the deadline the caller stated.
 */
struct retry_attempt {
    /// The first attempt is 1.
    int number = 1;
    /// What is left of the caller's budget when this attempt starts.
    std::chrono::milliseconds budget_left{0};
};

/**
 * @brief What one retried request is asked of, what it is, and how long it may
 * take.
 */
struct retry_request_options {
    /// The instant past which the caller gives up. The caller states it once,
    /// from the step's own budget, so a retry cannot outlive the step.
    std::chrono::steady_clock::time_point deadline;
    /// How long one attempt waits for its reply.
    std::chrono::milliseconds attempt_timeout{ores::nats::service::default_request_timeout};
    /// The work the request does, in a person's words, for the log line and
    /// for the error a person reads.
    std::string what;
    /// How many attempts, and how they wait.
    request_retry_policy policy{};
    /// Where an attempt is reported, so a person watching the run sees it.
    /// Empty when the caller has nowhere to record it.
    std::function<void(const std::string&)> note{};
};

/**
 * @brief What one retried call is for, and how long it may take.
 */
struct retry_call_options {
    /// The instant past which the caller gives up.
    std::chrono::steady_clock::time_point deadline;
    /// The work, in a person's words, for the log line and the error.
    std::string what;
    /// How many attempts, and how they wait.
    request_retry_policy policy{};
    /// Where an attempt is reported, so a person watching the run sees it.
    /// Empty when the caller has nowhere to record it.
    std::function<void(const std::string&)> note{};
};

/**
 * @brief Runs @p operation, and runs it again while a transient failure may pass.
 *
 * A transient failure is retried with a backoff that grows and is jittered, so
 * two steps that failed together do not return together. A refusal and a
 * permanent fault are thrown at once, with the server's own reason. The
 * attempts stop at @c policy.max_attempts and at @c deadline, whichever comes
 * first.
 *
 * The deadline bounds the whole chain, not only the gaps between attempts: the
 * wait before an attempt is cut to what is left, and the operation is told that
 * remainder as @c retry_attempt::budget_left, so an attempt that waits for a
 * reply waits no longer than the deadline allows. The first attempt is made
 * whatever the deadline says, because the caller asked for that work.
 *
 * The operation takes a @c retry_attempt rather than nothing so that it can
 * first read whether an earlier attempt already did the work, and report the
 * progress it makes between attempts. A repeat that would write a second row,
 * raise a second request or publish a second set is the caller's to prevent,
 * and that is where it is prevented.
 *
 * @throws std::runtime_error when the operation is not answered and will not be
 *         tried again, naming @c what, the attempts it took and the reason.
 */
template <typename Operation>
auto call_with_retry(Operation&& operation, const retry_call_options& options)
    -> std::invoke_result_t<Operation, const retry_attempt&> {
    static auto lg = ores::logging::make_logger("ores.iam.service.request_retry");

    auto delay = options.policy.first_delay;
    for (int attempt = 1;; ++attempt) {
        const auto budget_left = std::chrono::duration_cast<std::chrono::milliseconds>(
            options.deadline - std::chrono::steady_clock::now());
        try {
            return operation(retry_attempt{.number = attempt, .budget_left = budget_left});
        } catch (const std::exception& failure) {
            const auto kind = classify_request_failure(failure);
            const auto remaining = options.deadline - std::chrono::steady_clock::now();
            const bool last_attempt = attempt >= options.policy.max_attempts;
            if (kind != request_failure_class::transient || last_attempt ||
                remaining <= std::chrono::steady_clock::duration::zero()) {
                throw std::runtime_error(options.what + " failed after " + std::to_string(attempt) +
                                         (attempt == 1 ? " attempt: " : " attempts: ") +
                                         failure.what());
            }

            // Jitter keeps two steps that failed together from returning
            // together. The wait never runs past the caller's deadline.
            static thread_local std::mt19937 generator(std::random_device{}());
            const auto jittered =
                std::chrono::milliseconds(std::uniform_int_distribution<std::int64_t>(
                    delay.count() / 2, delay.count())(generator));
            const auto wait_for = std::min(
                std::chrono::duration_cast<std::chrono::milliseconds>(remaining), jittered);
            const auto line = options.what + ": attempt " + std::to_string(attempt) + " failed (" +
                              failure.what() + "); asking again in " +
                              std::to_string(wait_for.count()) + " ms";
            BOOST_LOG_SEV(lg, ores::logging::warn) << line;
            // The step's own record carries the attempt, so the person
            // watching the run reads it without a service log.
            if (options.note)
                options.note(line);
            options.policy.wait(wait_for);
            delay = std::min(delay * 2, options.policy.max_delay);
        }
    }
}

/**
 * @brief Does the work, unless an earlier attempt already did it.
 *
 * A request that writes and then loses its reply has written all the same, so
 * a repeat reads what is there before it writes again: @p read_back answers the
 * result the earlier attempt produced, or nothing when it produced none.
 *
 * The first attempt does not read back. Nothing an earlier attempt did is there
 * to find, and a record an earlier run left is not this run's work: taking it
 * would report work the step did not do.
 *
 * @param read_back Answers what an earlier attempt of this chain left, or
 *        nothing. Called only on a repeat.
 * @param work Does the work and answers its result.
 */
template <typename ReadBack, typename Work>
auto do_unless_already_done(const retry_attempt& attempt,
                            ReadBack&& read_back,
                            Work&& work) -> std::invoke_result_t<Work> {
    if (attempt.number > 1) {
        if (auto found = read_back())
            return std::move(*found);
    }
    return work();
}

/**
 * @brief Issues @p req through @p client, and asks again while a transient
 * failure may pass.
 *
 * The request sent on each attempt is the same request, so the caller must
 * state one whose repeat changes nothing: the same identity, the same as-of.
 * A request whose repeat would write something must go through
 * call_with_retry() with a read of what an earlier attempt did.
 */
template <typename Client, typename Request>
typename Request::response_type
request_with_retry(Client& client, const Request& req, const retry_request_options& options) {
    return call_with_retry(
        [&client, &req, &options](const retry_attempt& attempt) {
            // One attempt waits no longer than the step's budget still allows,
            // so the chain of attempts cannot outlive the deadline.
            const auto patience = std::max(attempt.budget_left, std::chrono::milliseconds{1});
            return client.request(req, std::min(options.attempt_timeout, patience));
        },
        {.deadline = options.deadline,
         .what = options.what,
         .policy = options.policy,
         .note = options.note});
}

}

#endif
