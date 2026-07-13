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
#include "ores.refdata.core/service/ledger_feed_type_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::refdata::service {

using namespace ores::logging;

ledger_feed_type_service::ledger_feed_type_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::ledger_feed_type> ledger_feed_type_service::list_types(std::uint32_t offset,
                                                                           std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all ledger feed types";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t ledger_feed_type_service::count_types() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total ledger feed types count";
    return repo_.get_total_type_count(ctx_);
}

std::optional<domain::ledger_feed_type>
ledger_feed_type_service::get_type_at_version(const std::string& code, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting ledger feed type at version: " << code
                               << " version: " << version;
    return repo_.read_at_version(ctx_, code, version);
}

std::optional<domain::ledger_feed_type>
ledger_feed_type_service::get_type(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting ledger feed type: " << code;
    auto results = repo_.read_latest(ctx_, code);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void ledger_feed_type_service::save_type(const domain::ledger_feed_type& v) {
    if (v.code.empty())
        throw std::invalid_argument("Ledger Feed Type code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving ledger feed type: " << v.code;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved ledger feed type: " << v.code;
}

void ledger_feed_type_service::save_types(const std::vector<domain::ledger_feed_type>& types) {
    for (const auto& e : types)
        if (e.code.empty())
            throw std::invalid_argument("Ledger Feed Type code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving " << types.size() << " ledger feed types";
    auto ts = types;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void ledger_feed_type_service::delete_type(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing ledger feed type: " << code;
    repo_.remove(ctx_, code);
    BOOST_LOG_SEV(lg(), info) << "Removed ledger feed type: " << code;
}

void ledger_feed_type_service::delete_types(const std::vector<std::string>& codes) {
    repo_.remove(ctx_, codes);
}

std::vector<domain::ledger_feed_type>
ledger_feed_type_service::get_type_history(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for ledger feed type: " << code;
    return repo_.read_all(ctx_, code);
}

}
