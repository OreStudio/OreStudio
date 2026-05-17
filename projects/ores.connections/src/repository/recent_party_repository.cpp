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
#include "ores.connections/repository/recent_party_repository.hpp"

#include <algorithm>
#include <chrono>
#include <format>
#include <stdexcept>
#include <boost/uuid/uuid_io.hpp>
#include <sqlgen/read.hpp>
#include <sqlgen/insert.hpp>
#include "ores.connections/repository/recent_party_entity.hpp"
#include "ores.connections/repository/recent_party_mapper.hpp"

namespace ores::connections::repository {

using namespace sqlgen;

recent_party_repository::recent_party_repository(sqlite_context& ctx)
    : ctx_(ctx) {}

void recent_party_repository::record(
    const boost::uuids::uuid& party_id, const std::string& party_name) {
    auto conn_result = ctx_.connect();
    if (!conn_result)
        throw std::runtime_error("Failed to connect to database");

    auto& conn = *conn_result;

    const auto now = std::chrono::floor<std::chrono::seconds>(
        std::chrono::system_clock::now());
    const std::string ts = std::format("{:%Y-%m-%d %H:%M:%S}", now);

    recent_party_entity e;
    e.party_id = boost::uuids::to_string(party_id);
    e.party_name = party_name;
    e.last_selected_at = ts;

    conn->begin_transaction();
    auto ins = sqlgen::insert_or_replace(conn, e);
    if (!ins) {
        conn->rollback();
        throw std::runtime_error(std::format("Failed to record recent party: {}",
            ins.error().what()));
    }

    // Prune entries beyond max_recents, keeping the most recent
    conn->execute(std::format(
        "DELETE FROM recent_parties WHERE party_id NOT IN "
        "(SELECT party_id FROM recent_parties "
        " ORDER BY last_selected_at DESC LIMIT {})",
        max_recents));

    conn->commit();
}

std::vector<domain::recent_party> recent_party_repository::read_recent() {
    auto conn_result = ctx_.connect();
    if (!conn_result)
        throw std::runtime_error("Failed to connect to database");

    auto& conn = *conn_result;
    auto query = sqlgen::read<std::vector<recent_party_entity>>;
    auto result = query(conn);
    if (!result)
        throw std::runtime_error(std::format("Failed to read recent parties: {}",
            result.error().what()));

    auto parties = recent_party_mapper::to_domain(*result);
    std::sort(parties.begin(), parties.end(),
        [](const domain::recent_party& a, const domain::recent_party& b) {
            return a.last_selected_at > b.last_selected_at;
        });
    return parties;
}

}
