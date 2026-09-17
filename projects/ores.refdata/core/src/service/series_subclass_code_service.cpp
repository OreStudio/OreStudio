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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_service.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.refdata.core/service/series_subclass_code_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::refdata::service {

using namespace ores::logging;

series_subclass_code_service::series_subclass_code_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::series_subclass_code>
series_subclass_code_service::list_series_subclasses(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all series subclass codes";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t series_subclass_code_service::count_series_subclasses() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total series subclass codes count";
    return repo_.get_total_series_subclass_count(ctx_);
}


std::optional<domain::series_subclass_code>
series_subclass_code_service::get_series_subclass_at_version(const std::string& code,
                                                             std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting series subclass code at version. " << "code: " << code
                               << " version: " << version;
    return repo_.read_at_version(ctx_, code, version);
}

std::optional<domain::series_subclass_code>
series_subclass_code_service::get_series_subclass(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting series subclass code. " << "code: " << code;
    auto results = repo_.read_latest(ctx_, code);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void series_subclass_code_service::save_series_subclass(const domain::series_subclass_code& v) {
    if (v.code.empty())
        throw std::invalid_argument("Series Subclass Code code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving series subclass code. " << "code: " << v.code;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved series subclass code. " << "code: " << v.code;
}

void series_subclass_code_service::save_series_subclasses(
    const std::vector<domain::series_subclass_code>& series_subclasses) {
    for (const auto& e : series_subclasses) {
        if (e.code.empty())
            throw std::invalid_argument("Series Subclass Code code cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << series_subclasses.size() << " series subclass codes";
    auto ts = series_subclasses;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void series_subclass_code_service::delete_series_subclass(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing series subclass code. " << "code: " << code;
    repo_.remove(ctx_, code);
    BOOST_LOG_SEV(lg(), info) << "Removed series subclass code. " << "code: " << code;
}

void series_subclass_code_service::delete_series_subclasses(const std::vector<std::string>& codes) {
    repo_.remove(ctx_, codes);
}

std::vector<domain::series_subclass_code>
series_subclass_code_service::get_series_subclass_history(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for series subclass code. " << "code: " << code;
    return repo_.read_all(ctx_, code);
}

}
