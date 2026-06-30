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
#include "ores.marketdata.core/service/feed_binding_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::marketdata::service {

using namespace ores::logging;

feed_binding_service::feed_binding_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::feed_binding> feed_binding_service::list_feed_bindings(std::uint32_t offset,
                                                                           std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all feed bindings";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t feed_binding_service::count_feed_bindings() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total feed bindings count";
    return repo_.get_total_feed_binding_count(ctx_);
}

std::optional<domain::feed_binding> feed_binding_service::get_feed_binding(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting feed binding: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void feed_binding_service::save_feed_binding(const domain::feed_binding& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Feed Binding id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving feed binding: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved feed binding: " << v.id;
}

void feed_binding_service::save_feed_bindings(
    const std::vector<domain::feed_binding>& feed_bindings) {
    for (const auto& e : feed_bindings)
        if (e.id.is_nil())
            throw std::invalid_argument("Feed Binding id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving " << feed_bindings.size() << " feed bindings";
    auto ts = feed_bindings;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void feed_binding_service::delete_feed_binding(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing feed binding: " << id;
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed feed binding: " << id;
}

void feed_binding_service::delete_feed_bindings(const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::feed_binding>
feed_binding_service::get_feed_binding_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for feed binding: " << id;
    return repo_.read_all(ctx_, id);
}

}
