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
#include "ores.dq/repository/fsm_transition_repository.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.dq/repository/fsm_transition_entity.hpp"
#include "ores.dq/repository/fsm_transition_mapper.hpp"

namespace ores::dq::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string fsm_transition_repository::sql() {
    return generate_create_table_sql<fsm_transition_entity>(lg());
}

void fsm_transition_repository::write(context ctx, const domain::fsm_transition& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing FSM transition: " << v.name;
    execute_write_query(ctx, fsm_transition_mapper::map(v),
        lg(), "Writing FSM transition to database.");
}

void fsm_transition_repository::write(
    context ctx, const std::vector<domain::fsm_transition>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing FSM transitions. Count: " << v.size();
    execute_write_query(ctx, fsm_transition_mapper::map(v),
        lg(), "Writing FSM transitions to database.");
}

std::vector<domain::fsm_transition>
fsm_transition_repository::read_latest(context ctx) {
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<fsm_transition_entity>> |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
        order_by("name"_c);

    return execute_read_query<fsm_transition_entity, domain::fsm_transition>(
        ctx, query,
        [](const auto& entities) { return fsm_transition_mapper::map(entities); },
        lg(), "Reading latest FSM transitions.");
}

std::vector<domain::fsm_transition>
fsm_transition_repository::read_latest_by_machine(
    context ctx, const boost::uuids::uuid& machine_id) {
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto mid = boost::uuids::to_string(machine_id);
    const auto query = sqlgen::read<std::vector<fsm_transition_entity>> |
        where("tenant_id"_c == tid && "machine_id"_c == mid &&
              "valid_to"_c == max.value()) |
        order_by("name"_c);

    return execute_read_query<fsm_transition_entity, domain::fsm_transition>(
        ctx, query,
        [](const auto& entities) { return fsm_transition_mapper::map(entities); },
        lg(), "Reading latest FSM transitions by machine.");
}

std::optional<domain::fsm_transition>
fsm_transition_repository::find_by_id(
    context ctx, const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Finding FSM transition by id.";
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto id_str = boost::uuids::to_string(id);
    const auto query = sqlgen::read<std::vector<fsm_transition_entity>> |
        where("tenant_id"_c == tid && "id"_c == id_str &&
              "valid_to"_c == max.value());

    auto results = execute_read_query<fsm_transition_entity, domain::fsm_transition>(
        ctx, query,
        [](const auto& entities) { return fsm_transition_mapper::map(entities); },
        lg(), "Finding FSM transition by id.");
    if (results.empty()) return std::nullopt;
    return results.front();
}

void fsm_transition_repository::remove(context ctx, const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing FSM transition.";
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto sid = boost::uuids::to_string(id);
    const auto query = sqlgen::delete_from<fsm_transition_entity> |
        where("tenant_id"_c == tid && "id"_c == sid &&
              "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing FSM transition from database.");
}

}
