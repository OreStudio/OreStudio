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
#include "ores.refdata.core/service/diary_entry_type_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::refdata::service {

using namespace ores::logging;

diary_entry_type_service::diary_entry_type_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::diary_entry_type>
diary_entry_type_service::list_entry_types(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all diary entry types";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t diary_entry_type_service::count_entry_types() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total diary entry types count";
    return repo_.get_total_entry_type_count(ctx_);
}


std::optional<domain::diary_entry_type>
diary_entry_type_service::get_entry_type_at_version(const std::string& code,
                                                    std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting diary entry type at version. " << "code: " << code
                               << " version: " << version;
    return repo_.read_at_version(ctx_, code, version);
}

std::optional<domain::diary_entry_type>
diary_entry_type_service::get_entry_type(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting diary entry type. " << "code: " << code;
    auto results = repo_.read_latest(ctx_, code);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void diary_entry_type_service::save_entry_type(const domain::diary_entry_type& v) {
    if (v.code.empty())
        throw std::invalid_argument("Diary Entry Type code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving diary entry type. " << "code: " << v.code;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved diary entry type. " << "code: " << v.code;
}

void diary_entry_type_service::save_entry_types(
    const std::vector<domain::diary_entry_type>& entry_types) {
    for (const auto& e : entry_types) {
        if (e.code.empty())
            throw std::invalid_argument("Diary Entry Type code cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << entry_types.size() << " diary entry types";
    auto ts = entry_types;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void diary_entry_type_service::delete_entry_type(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing diary entry type. " << "code: " << code;
    repo_.remove(ctx_, code);
    BOOST_LOG_SEV(lg(), info) << "Removed diary entry type. " << "code: " << code;
}

void diary_entry_type_service::delete_entry_types(const std::vector<std::string>& codes) {
    repo_.remove(ctx_, codes);
}

std::vector<domain::diary_entry_type>
diary_entry_type_service::get_entry_type_history(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for diary entry type. " << "code: " << code;
    return repo_.read_all(ctx_, code);
}

}
