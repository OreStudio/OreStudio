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
#include "ores.refdata.core/service/tenor_anchor_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::refdata::service {

using namespace ores::logging;

tenor_anchor_service::tenor_anchor_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::tenor_anchor> tenor_anchor_service::list_anchors(std::uint32_t offset,
                                                                     std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all tenor anchors";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t tenor_anchor_service::count_anchors() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total tenor anchors count";
    return repo_.get_total_anchor_count(ctx_);
}

std::optional<domain::tenor_anchor>
tenor_anchor_service::get_anchor_at_version(const std::string& code, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting tenor anchor at version: " << code
                               << " version: " << version;
    return repo_.read_at_version(ctx_, code, version);
}

std::optional<domain::tenor_anchor> tenor_anchor_service::get_anchor(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting tenor anchor: " << code;
    auto results = repo_.read_latest(ctx_, code);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void tenor_anchor_service::save_anchor(const domain::tenor_anchor& v) {
    if (v.code.empty())
        throw std::invalid_argument("Tenor Anchor code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving tenor anchor: " << v.code;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved tenor anchor: " << v.code;
}

void tenor_anchor_service::save_anchors(const std::vector<domain::tenor_anchor>& anchors) {
    for (const auto& e : anchors)
        if (e.code.empty())
            throw std::invalid_argument("Tenor Anchor code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving " << anchors.size() << " tenor anchors";
    auto ts = anchors;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void tenor_anchor_service::delete_anchor(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing tenor anchor: " << code;
    repo_.remove(ctx_, code);
    BOOST_LOG_SEV(lg(), info) << "Removed tenor anchor: " << code;
}

void tenor_anchor_service::delete_anchors(const std::vector<std::string>& codes) {
    repo_.remove(ctx_, codes);
}

std::vector<domain::tenor_anchor>
tenor_anchor_service::get_anchor_history(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for tenor anchor: " << code;
    return repo_.read_all(ctx_, code);
}

}
