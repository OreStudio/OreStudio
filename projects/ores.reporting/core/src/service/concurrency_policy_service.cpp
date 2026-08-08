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
#include "ores.reporting.core/service/concurrency_policy_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::reporting::service {

using namespace ores::logging;

concurrency_policy_service::concurrency_policy_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::concurrency_policy>
concurrency_policy_service::list_policies(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all concurrency policies";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t concurrency_policy_service::count_policies() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total concurrency policies count";
    return repo_.get_total_policy_count(ctx_);
}


std::optional<domain::concurrency_policy>
concurrency_policy_service::get_policy_at_version(const std::string& code, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting concurrency policy at version. " << "code: " << code
                               << " version: " << version;
    return repo_.read_at_version(ctx_, code, version);
}

std::optional<domain::concurrency_policy>
concurrency_policy_service::get_policy(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting concurrency policy. " << "code: " << code;
    auto results = repo_.read_latest(ctx_, code);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void concurrency_policy_service::save_policy(const domain::concurrency_policy& v) {
    if (v.code.empty())
        throw std::invalid_argument("Concurrency Policy code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving concurrency policy. " << "code: " << v.code;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved concurrency policy. " << "code: " << v.code;
}

void concurrency_policy_service::save_policies(
    const std::vector<domain::concurrency_policy>& policies) {
    for (const auto& e : policies) {
        if (e.code.empty())
            throw std::invalid_argument("Concurrency Policy code cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << policies.size() << " concurrency policies";
    auto ts = policies;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void concurrency_policy_service::delete_policy(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing concurrency policy. " << "code: " << code;
    repo_.remove(ctx_, code);
    BOOST_LOG_SEV(lg(), info) << "Removed concurrency policy. " << "code: " << code;
}

void concurrency_policy_service::delete_policies(const std::vector<std::string>& codes) {
    repo_.remove(ctx_, codes);
}

std::vector<domain::concurrency_policy>
concurrency_policy_service::get_policy_history(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for concurrency policy. " << "code: " << code;
    return repo_.read_all(ctx_, code);
}

}
