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
#include "ores.refdata.core/repository/counterparty_contact_information_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.refdata.api/domain/counterparty_contact_information_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/counterparty_contact_information_entity.hpp"
#include "ores.refdata.core/repository/counterparty_contact_information_mapper.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::refdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string counterparty_contact_information_repository::sql() {
    return generate_create_table_sql<counterparty_contact_information_entity>(lg());
}

void counterparty_contact_information_repository::write(
    context ctx, const domain::counterparty_contact_information& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing counterparty contact information: " << v.id;
    execute_write_query(ctx,
                        counterparty_contact_information_mapper::map(v),
                        lg(),
                        "Writing counterparty contact information to database.");
}

void counterparty_contact_information_repository::write(
    context ctx, const std::vector<domain::counterparty_contact_information>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing counterparty contact informations. Count: " << v.size();
    execute_write_query(ctx,
                        counterparty_contact_information_mapper::map(v),
                        lg(),
                        "Writing counterparty contact informations to database.");
}

std::vector<domain::counterparty_contact_information>
counterparty_contact_information_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<counterparty_contact_information_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c);

    return execute_read_query<counterparty_contact_information_entity,
                              domain::counterparty_contact_information>(
        ctx,
        query,
        [](const auto& entities) { return counterparty_contact_information_mapper::map(entities); },
        lg(),
        "Reading latest counterparty contact informations");
}

std::vector<domain::counterparty_contact_information>
counterparty_contact_information_repository::read_latest(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest counterparty contact information. id: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<counterparty_contact_information_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    return execute_read_query<counterparty_contact_information_entity,
                              domain::counterparty_contact_information>(
        ctx,
        query,
        [](const auto& entities) { return counterparty_contact_information_mapper::map(entities); },
        lg(),
        "Reading latest counterparty contact information by id.");
}

std::vector<domain::counterparty_contact_information>
counterparty_contact_information_repository::read_all(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all counterparty contact information versions. id: "
                               << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<counterparty_contact_information_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id) | order_by("version"_c.desc());

    return execute_read_query<counterparty_contact_information_entity,
                              domain::counterparty_contact_information>(
        ctx,
        query,
        [](const auto& entities) { return counterparty_contact_information_mapper::map(entities); },
        lg(),
        "Reading all counterparty contact information versions by id.");
}

void counterparty_contact_information_repository::remove(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing counterparty contact information: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<counterparty_contact_information_entity> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    execute_delete_query(
        ctx, query, lg(), "Removing counterparty contact information from database.");
}

std::vector<domain::counterparty_contact_information>
counterparty_contact_information_repository::read_latest(context ctx,
                                                         std::uint32_t offset,
                                                         std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest counterparty contact informations with offset: "
                               << offset << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<counterparty_contact_information_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<counterparty_contact_information_entity,
                              domain::counterparty_contact_information>(
        ctx,
        query,
        [](const auto& entities) { return counterparty_contact_information_mapper::map(entities); },
        lg(),
        "Reading latest counterparty contact informations with pagination.");
}

std::uint32_t
counterparty_contact_information_repository::get_total_counterparty_contact_information_count(
    context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active counterparty contact information count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::select_from<counterparty_contact_information_entity>(
                           sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active counterparty contact information count: " << count;
    return count;
}

void counterparty_contact_information_repository::remove(context ctx,
                                                         const std::vector<std::string>& ids) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<counterparty_contact_information_entity> |
                       where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing counterparty contact informations.");
}


}
