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
#include "ores.trading.core/service/bond_issue_call_date_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::trading::service {

using namespace ores::logging;

bond_issue_call_date_service::bond_issue_call_date_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::bond_issue_call_date>
bond_issue_call_date_service::list_call_dates(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all bond issue call dates";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t bond_issue_call_date_service::count_call_dates() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total bond issue call dates count";
    return repo_.get_total_call_date_count(ctx_);
}


std::optional<domain::bond_issue_call_date> bond_issue_call_date_service::get_call_date_at_version(
    const std::string& issue_id, const std::string& sequence_number, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting bond issue call date at version. "
                               << "issue_id: " << issue_id
                               << " sequence_number: " << sequence_number
                               << " version: " << version;
    return repo_.read_at_version(ctx_, issue_id, sequence_number, version);
}

std::optional<domain::bond_issue_call_date>
bond_issue_call_date_service::get_call_date(const std::string& issue_id,
                                            const std::string& sequence_number) {
    BOOST_LOG_SEV(lg(), debug) << "Getting bond issue call date. " << "issue_id: " << issue_id
                               << " sequence_number: " << sequence_number;
    auto results = repo_.read_latest(ctx_, issue_id, sequence_number);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void bond_issue_call_date_service::save_call_date(const domain::bond_issue_call_date& v) {
    if (v.issue_id.is_nil())
        throw std::invalid_argument("Bond Issue Call Date issue_id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving bond issue call date. " << "issue_id: " << v.issue_id
                               << " sequence_number: " << v.sequence_number;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved bond issue call date. " << "issue_id: " << v.issue_id
                              << " sequence_number: " << v.sequence_number;
}

void bond_issue_call_date_service::save_call_dates(
    const std::vector<domain::bond_issue_call_date>& call_dates) {
    for (const auto& e : call_dates) {
        if (e.issue_id.is_nil())
            throw std::invalid_argument("Bond Issue Call Date issue_id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << call_dates.size() << " bond issue call dates";
    auto ts = call_dates;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void bond_issue_call_date_service::delete_call_date(const std::string& issue_id,
                                                    const std::string& sequence_number) {
    BOOST_LOG_SEV(lg(), debug) << "Removing bond issue call date. " << "issue_id: " << issue_id
                               << " sequence_number: " << sequence_number;
    repo_.remove(ctx_, issue_id, sequence_number);
    BOOST_LOG_SEV(lg(), info) << "Removed bond issue call date. " << "issue_id: " << issue_id
                              << " sequence_number: " << sequence_number;
}

void bond_issue_call_date_service::delete_call_dates(
    const std::vector<std::string>& issue_ids, const std::vector<std::string>& sequence_numbers) {
    repo_.remove(ctx_, issue_ids, sequence_numbers);
}

std::vector<domain::bond_issue_call_date>
bond_issue_call_date_service::get_call_date_history(const std::string& issue_id,
                                                    const std::string& sequence_number) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for bond issue call date. "
                               << "issue_id: " << issue_id
                               << " sequence_number: " << sequence_number;
    return repo_.read_all(ctx_, issue_id, sequence_number);
}

}
