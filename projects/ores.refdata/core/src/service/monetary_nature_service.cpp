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
#include "ores.refdata.core/service/monetary_nature_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::refdata::service {

using namespace ores::logging;

monetary_nature_service::monetary_nature_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::monetary_nature> monetary_nature_service::list_types(std::uint32_t offset,
                                                                         std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all monetary natures";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t monetary_nature_service::count_types() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total monetary natures count";
    return repo_.get_total_type_count(ctx_);
}

std::optional<domain::monetary_nature>
monetary_nature_service::get_type_at_version(const std::string& code, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting monetary nature at version: " << code
                               << " version: " << version;
    return repo_.read_at_version(ctx_, code, version);
}

std::optional<domain::monetary_nature> monetary_nature_service::get_type(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting monetary nature: " << code;
    auto results = repo_.read_latest(ctx_, code);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void monetary_nature_service::save_type(const domain::monetary_nature& v) {
    if (v.code.empty())
        throw std::invalid_argument("Monetary Nature code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving monetary nature: " << v.code;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved monetary nature: " << v.code;
}

void monetary_nature_service::save_types(const std::vector<domain::monetary_nature>& types) {
    for (const auto& e : types)
        if (e.code.empty())
            throw std::invalid_argument("Monetary Nature code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving " << types.size() << " monetary natures";
    auto ts = types;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void monetary_nature_service::delete_type(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing monetary nature: " << code;
    repo_.remove(ctx_, code);
    BOOST_LOG_SEV(lg(), info) << "Removed monetary nature: " << code;
}

void monetary_nature_service::delete_types(const std::vector<std::string>& codes) {
    repo_.remove(ctx_, codes);
}

std::vector<domain::monetary_nature>
monetary_nature_service::get_type_history(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for monetary nature: " << code;
    return repo_.read_all(ctx_, code);
}

}
