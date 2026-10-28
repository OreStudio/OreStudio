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
#include "ores.compute.wrapper/app/claim_loop.hpp"
#include "ores.compute.api/messaging/claim_pacing.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include <exception>
#include <random>
#include <stdexcept>
#include <utility>

namespace ores::compute::wrapper::app {

using namespace ores::logging;

namespace {

auto& lg() {
    static auto instance = make_logger("ores.compute.wrapper.app.claim_loop");
    return instance;
}

/// A random number from 0 up to but not including 1, for the jitter.
double unit_random() {
    static thread_local std::mt19937_64 engine{std::random_device{}()};
    return std::uniform_real_distribution<double>(0.0, 1.0)(engine);
}

} // namespace

claim_loop::claim_loop(ores::nats::service::client& nats,
                       std::string host_id,
                       std::string platform,
                       std::chrono::milliseconds poll_interval,
                       handler on_job)
    : nats_(nats)
    , host_id_(std::move(host_id))
    , platform_(std::move(platform))
    , poll_interval_(poll_interval)
    , on_job_(std::move(on_job)) {}

claim_loop::~claim_loop() {
    stop();
}

void claim_loop::start() {
    thread_ = std::thread([this] { run(); });
}

void claim_loop::stop() {
    stopping_ = true;
    {
        std::lock_guard<std::mutex> lock(mutex_);
        rung_ = true;
    }
    wake_.notify_all();
    if (thread_.joinable())
        thread_.join();
}

void claim_loop::ring() {
    {
        std::lock_guard<std::mutex> lock(mutex_);
        rung_ = true;
    }
    wake_.notify_all();
}

void claim_loop::wait_for(std::chrono::milliseconds delay) {
    std::unique_lock<std::mutex> lock(mutex_);
    wake_.wait_for(lock, delay, [this] { return rung_ || stopping_.load(); });
    rung_ = false;
}

void claim_loop::run() {
    using ores::compute::messaging::claim_work_request;
    using ores::compute::messaging::claim_work_response;
    namespace pacing = ores::compute::messaging;
    const auto& codec = ores::nats::default_wire_codec();
    int failures = 0;

    BOOST_LOG_SEV(lg(), info) << "Claim loop started for platform " << platform_
                              << ", polling every " << poll_interval_.count() << " ms";
    while (!stopping_) {
        try {
            const claim_work_request request{.host_id = host_id_, .platform = platform_};
            const auto reply =
                nats_.request_sync(claim_work_request::nats_subject, codec.encode(request));
            const auto response = codec.decode<claim_work_response>(reply.data);
            if (!response) {
                throw std::runtime_error("The claim reply could not be read: " +
                                         std::string(response.error().what()));
            }
            if (!response->success) {
                throw std::runtime_error("The claim was refused: " + response->message);
            }
            failures = 0;
            if (response->has_work) {
                ores::compute::messaging::work_assignment_event job{
                    .tenant_id = response->tenant_id,
                    .result_id = response->result_id,
                    .workunit_id = response->workunit_id,
                    .app_version_id = response->app_version_id,
                    .package_uri = response->package_uri,
                    .package_sha256 = response->package_sha256,
                    .input_uri = response->input_uri,
                    .config_uri = response->config_uri,
                    .output_uri = response->output_uri,
                    .results_uri = response->results_uri,
                    .logs_uri = response->logs_uri,
                    .storage_token = response->storage_token};
                BOOST_LOG_SEV(lg(), info) << "Claimed result " << job.result_id << " (attempt "
                                          << response->attempt << ")";
                try {
                    on_job_(job);
                } catch (const std::exception& e) {
                    // The handler reports its own outcome. A failure that escapes it is not a
                    // failed claim, so it does not slow the next ask.
                    BOOST_LOG_SEV(lg(), error)
                        << "The job " << job.result_id << " ended with an error: " << e.what();
                }
                // The node is free again and asks at once, so a notice that came during the job
                // is already answered. Clear it, or the next empty ask would return early.
                {
                    std::lock_guard<std::mutex> lock(mutex_);
                    rung_ = false;
                }
                continue;
            }
            wait_for(pacing::poll_delay(poll_interval_, unit_random()));
        } catch (const std::exception& e) {
            ++failures;
            const auto delay = pacing::error_delay(failures, unit_random());
            BOOST_LOG_SEV(lg(), warn) << "Claim failed (" << failures << " in a row): " << e.what()
                                      << "; trying again in " << delay.count() << " ms";
            wait_for(delay);
        }
    }
    BOOST_LOG_SEV(lg(), info) << "Claim loop stopped.";
}

} // namespace ores::compute::wrapper::app
