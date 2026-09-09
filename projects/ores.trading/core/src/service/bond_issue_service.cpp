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
#include "ores.trading.core/service/bond_issue_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::trading::service {

using namespace ores::logging;

bond_issue_service::bond_issue_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::bond_issue> bond_issue_service::list_issues(std::uint32_t offset,
                                                                std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all bond issues";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t bond_issue_service::count_issues() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total bond issues count";
    return repo_.get_total_issue_count(ctx_);
}


std::optional<domain::bond_issue>
bond_issue_service::get_issue_at_version(const std::string& issue_id, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting bond issue at version. " << "issue_id: " << issue_id
                               << " version: " << version;
    return repo_.read_at_version(ctx_, issue_id, version);
}

std::optional<domain::bond_issue> bond_issue_service::get_issue(const std::string& issue_id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting bond issue. " << "issue_id: " << issue_id;
    auto results = repo_.read_latest(ctx_, issue_id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void bond_issue_service::save_issue(const domain::bond_issue& v) {
    if (v.issue_id.is_nil())
        throw std::invalid_argument("Bond Issue issue_id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving bond issue. " << "issue_id: " << v.issue_id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved bond issue. " << "issue_id: " << v.issue_id;
}

void bond_issue_service::save_issues(const std::vector<domain::bond_issue>& issues) {
    for (const auto& e : issues) {
        if (e.issue_id.is_nil())
            throw std::invalid_argument("Bond Issue issue_id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << issues.size() << " bond issues";
    auto ts = issues;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void bond_issue_service::delete_issue(const std::string& issue_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing bond issue. " << "issue_id: " << issue_id;
    repo_.remove(ctx_, issue_id);
    BOOST_LOG_SEV(lg(), info) << "Removed bond issue. " << "issue_id: " << issue_id;
}

void bond_issue_service::delete_issues(const std::vector<std::string>& issue_ids) {
    repo_.remove(ctx_, issue_ids);
}

std::vector<domain::bond_issue> bond_issue_service::get_issue_history(const std::string& issue_id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for bond issue. " << "issue_id: " << issue_id;
    return repo_.read_all(ctx_, issue_id);
}

}
