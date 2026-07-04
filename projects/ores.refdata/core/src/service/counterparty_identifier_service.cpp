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
#include "ores.refdata.core/service/counterparty_identifier_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::refdata::service {

using namespace ores::logging;

counterparty_identifier_service::counterparty_identifier_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::counterparty_identifier>
counterparty_identifier_service::list_counterparty_identifiers(std::uint32_t offset,
                                                               std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all counterparty identifiers";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t counterparty_identifier_service::count_counterparty_identifiers() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total counterparty identifiers count";
    return repo_.get_total_counterparty_identifier_count(ctx_);
}

std::optional<domain::counterparty_identifier>
counterparty_identifier_service::get_counterparty_identifier(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting counterparty identifier: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void counterparty_identifier_service::save_counterparty_identifier(
    const domain::counterparty_identifier& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Counterparty Identifier id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving counterparty identifier: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved counterparty identifier: " << v.id;
}

void counterparty_identifier_service::save_counterparty_identifiers(
    const std::vector<domain::counterparty_identifier>& counterparty_identifiers) {
    for (const auto& e : counterparty_identifiers)
        if (e.id.is_nil())
            throw std::invalid_argument("Counterparty Identifier id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving " << counterparty_identifiers.size()
                               << " counterparty identifiers";
    auto ts = counterparty_identifiers;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void counterparty_identifier_service::delete_counterparty_identifier(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing counterparty identifier: " << id;
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed counterparty identifier: " << id;
}

void counterparty_identifier_service::delete_counterparty_identifiers(
    const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::counterparty_identifier>
counterparty_identifier_service::get_counterparty_identifier_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for counterparty identifier: " << id;
    return repo_.read_all(ctx_, id);
}

}
