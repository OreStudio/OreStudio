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
#include "ores.refdata.core/service/asset_class_code_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::refdata::service {

using namespace ores::logging;

asset_class_code_service::asset_class_code_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::asset_class_code>
asset_class_code_service::list_asset_classes(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all asset class codes";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t asset_class_code_service::count_asset_classes() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total asset class codes count";
    return repo_.get_total_asset_class_count(ctx_);
}

std::optional<domain::asset_class_code>
asset_class_code_service::get_asset_class_at_version(const std::string& code,
                                                     std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting asset class code at version: " << code
                               << " version: " << version;
    return repo_.read_at_version(ctx_, code, version);
}

std::optional<domain::asset_class_code>
asset_class_code_service::get_asset_class(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting asset class code: " << code;
    auto results = repo_.read_latest(ctx_, code);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void asset_class_code_service::save_asset_class(const domain::asset_class_code& v) {
    if (v.code.empty())
        throw std::invalid_argument("Asset Class Code code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving asset class code: " << v.code;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved asset class code: " << v.code;
}

void asset_class_code_service::save_asset_classes(
    const std::vector<domain::asset_class_code>& asset_classes) {
    for (const auto& e : asset_classes)
        if (e.code.empty())
            throw std::invalid_argument("Asset Class Code code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving " << asset_classes.size() << " asset class codes";
    auto ts = asset_classes;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void asset_class_code_service::delete_asset_class(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing asset class code: " << code;
    repo_.remove(ctx_, code);
    BOOST_LOG_SEV(lg(), info) << "Removed asset class code: " << code;
}

void asset_class_code_service::delete_asset_classes(const std::vector<std::string>& codes) {
    repo_.remove(ctx_, codes);
}

std::vector<domain::asset_class_code>
asset_class_code_service::get_asset_class_history(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for asset class code: " << code;
    return repo_.read_all(ctx_, code);
}

}
