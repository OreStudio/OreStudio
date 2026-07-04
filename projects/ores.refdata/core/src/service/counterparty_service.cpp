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
#include "ores.refdata.core/service/counterparty_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::refdata::service {

using namespace ores::logging;

counterparty_service::counterparty_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::counterparty> counterparty_service::list_counterparties(std::uint32_t offset,
                                                                            std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all counterparties";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t counterparty_service::count_counterparties() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total counterparties count";
    return repo_.get_total_counterparty_count(ctx_);
}


std::optional<domain::counterparty> counterparty_service::get_counterparty(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting counterparty: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void counterparty_service::save_counterparty(const domain::counterparty& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Counterparty id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving counterparty: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved counterparty: " << v.id;
}

void counterparty_service::save_counterparties(
    const std::vector<domain::counterparty>& counterparties) {
    for (const auto& e : counterparties)
        if (e.id.is_nil())
            throw std::invalid_argument("Counterparty id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving " << counterparties.size() << " counterparties";
    auto ts = counterparties;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void counterparty_service::delete_counterparty(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing counterparty: " << id;
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed counterparty: " << id;
}

void counterparty_service::delete_counterparties(const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::counterparty>
counterparty_service::get_counterparty_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for counterparty: " << id;
    return repo_.read_all(ctx_, id);
}

std::vector<ores::utility::domain::hierarchy_node>
counterparty_service::get_hierarchy(const boost::uuids::uuid& root_id, bool from_root) {
    BOOST_LOG_SEV(lg(), debug) << "Getting hierarchy for counterparty root: " << root_id;
    auto rows = repo_.get_hierarchy(ctx_, root_id, from_root);
    return ores::utility::domain::build_tree(rows);
}

}
