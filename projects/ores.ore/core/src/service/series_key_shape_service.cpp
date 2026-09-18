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
#include "ores.ore.core/service/series_key_shape_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::ore::service {

using namespace ores::logging;

series_key_shape_service::series_key_shape_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::series_key_shape> series_key_shape_service::list_shapes(std::uint32_t offset,
                                                                            std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all series key shapes";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t series_key_shape_service::count_shapes() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total series key shapes count";
    return repo_.get_total_shape_count(ctx_);
}


std::optional<domain::series_key_shape>
series_key_shape_service::get_shape_at_version(const std::string& series_type,
                                               std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting series key shape at version. "
                               << "series_type: " << series_type << " version: " << version;
    return repo_.read_at_version(ctx_, series_type, version);
}

std::optional<domain::series_key_shape>
series_key_shape_service::get_shape(const std::string& series_type) {
    BOOST_LOG_SEV(lg(), debug) << "Getting series key shape. " << "series_type: " << series_type;
    auto results = repo_.read_latest(ctx_, series_type);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void series_key_shape_service::save_shape(const domain::series_key_shape& v) {
    if (v.series_type.empty())
        throw std::invalid_argument("Series Key Shape series_type cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving series key shape. " << "series_type: " << v.series_type;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved series key shape. " << "series_type: " << v.series_type;
}

void series_key_shape_service::save_shapes(const std::vector<domain::series_key_shape>& shapes) {
    for (const auto& e : shapes) {
        if (e.series_type.empty())
            throw std::invalid_argument("Series Key Shape series_type cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << shapes.size() << " series key shapes";
    auto ts = shapes;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void series_key_shape_service::delete_shape(const std::string& series_type) {
    BOOST_LOG_SEV(lg(), debug) << "Removing series key shape. " << "series_type: " << series_type;
    repo_.remove(ctx_, series_type);
    BOOST_LOG_SEV(lg(), info) << "Removed series key shape. " << "series_type: " << series_type;
}

void series_key_shape_service::delete_shapes(const std::vector<std::string>& series_types) {
    repo_.remove(ctx_, series_types);
}

std::vector<domain::series_key_shape>
series_key_shape_service::get_shape_history(const std::string& series_type) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for series key shape. "
                               << "series_type: " << series_type;
    return repo_.read_all(ctx_, series_type);
}

}
