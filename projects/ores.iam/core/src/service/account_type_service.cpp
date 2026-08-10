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
#include "ores.iam.core/service/account_type_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::iam::service {

using namespace ores::logging;

account_type_service::account_type_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::account_type> account_type_service::list_types(std::uint32_t offset,
                                                                   std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all account types";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t account_type_service::count_types() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total account types count";
    return repo_.get_total_type_count(ctx_);
}


std::optional<domain::account_type>
account_type_service::get_type_at_version(const std::string& type, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting account type at version. " << "type: " << type
                               << " version: " << version;
    return repo_.read_at_version(ctx_, type, version);
}

std::optional<domain::account_type> account_type_service::find_type(const std::string& type) {
    BOOST_LOG_SEV(lg(), debug) << "Finding account type. " << "type: " << type;
    auto results = repo_.read_latest(ctx_, type);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void account_type_service::save_type(const domain::account_type& v) {
    if (v.type.empty())
        throw std::invalid_argument("Account Type type cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving account type. " << "type: " << v.type;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved account type. " << "type: " << v.type;
}

void account_type_service::save_types(const std::vector<domain::account_type>& types) {
    for (const auto& e : types) {
        if (e.type.empty())
            throw std::invalid_argument("Account Type type cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << types.size() << " account types";
    auto ts = types;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void account_type_service::delete_type(const std::string& type) {
    BOOST_LOG_SEV(lg(), debug) << "Removing account type. " << "type: " << type;
    repo_.remove(ctx_, type);
    BOOST_LOG_SEV(lg(), info) << "Removed account type. " << "type: " << type;
}

void account_type_service::delete_types(const std::vector<std::string>& types) {
    repo_.remove(ctx_, types);
}

std::vector<domain::account_type> account_type_service::get_type_history(const std::string& type) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for account type. " << "type: " << type;
    return repo_.read_all(ctx_, type);
}

}
