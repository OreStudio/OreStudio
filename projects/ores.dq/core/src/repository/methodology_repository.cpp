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
 * Template: cpp_domain_type_repository.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.dq.core/repository/methodology_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/stated_order.hpp"
#include "ores.dq.api/domain/methodology_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq.core/repository/methodology_entity.hpp"
#include "ores.dq.core/repository/methodology_mapper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <algorithm>
#include <initializer_list>
#include <sqlgen/postgres.hpp>
#include <stdexcept>
#include <string_view>

namespace ores::dq::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string methodology_repository::sql() {
    return generate_create_table_sql<methodology_entity>(lg());
}

bool methodology_repository::is_sortable(std::string_view field) {
    const std::initializer_list<std::string_view> sortable = {};
    return std::ranges::find(sortable, field) != sortable.end();
}

namespace {

/*
 * The order a page is read in. An empty field is the default order, which
 * the stated direction reverses; any other field must be sortable, because
 * the service refuses the rest before it reaches the store.
 */
sqlgen::dynamic::OrderBy list_order(const ores::utility::domain::order& order,
                                    std::initializer_list<std::string> default_columns,
                                    bool default_descending) {
    if (order.field.empty())
        return make_order(default_columns, default_descending != order.descending, {"id"});
    if (!methodology_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of methodologies cannot be ordered by " + order.field +
                                    ".");
    return make_order({order.field}, order.descending, {"id"});
}

}

ores::utility::domain::precondition
methodology_repository::replace_claim(context ctx, const domain::methodology& v) {
    const auto current = read_latest(ctx, boost::uuids::to_string(v.id));
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::methodology methodology_repository::apply_claim(
    context ctx, const domain::methodology& v, const ores::utility::domain::precondition& claim) {
    using ores::utility::domain::precondition_kind;
    auto t = v;
    switch (claim.kind) {
        case precondition_kind::must_not_exist:
            // Zero states that no current row exists, which is the one meaning the
            // store gives a zero version.
            t.version = 0;
            break;
        case precondition_kind::must_match_version:
            t.version = claim.version ? static_cast<int>(*claim.version) : 0;
            break;
        case precondition_kind::any: {
            // A caller that claims nothing still has to say what it replaces, so
            // the row is read and its version stated. A row that moved on between
            // this read and the write is a conflict the trigger raises, never a
            // silent overwrite.
            const auto current = read_latest(ctx, boost::uuids::to_string(v.id));
            t.version = current.empty() ? 0 : current.front().version;
            break;
        }
    }
    return t;
}

void methodology_repository::write(context ctx, const domain::methodology& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void methodology_repository::write(context ctx, const std::vector<domain::methodology>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void methodology_repository::write(context ctx,
                                   const domain::methodology& v,
                                   const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing methodology. " << "id: " << v.id;
    const auto t = apply_claim(ctx, v, claim);
    execute_write_query(ctx, methodology_mapper::map(t), lg(), "Writing methodology to database.");
}

void methodology_repository::write(context ctx,
                                   const std::vector<domain::methodology>& v,
                                   const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing methodologies. Count: " << v.size();
    std::vector<domain::methodology> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    execute_write_query(
        ctx, methodology_mapper::map(batch), lg(), "Writing methodologies to database.");
}

std::vector<domain::methodology> methodology_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<methodology_entity>> |
                       where("valid_to"_c == max.value()) | order_by("id"_c);

    return execute_read_query<methodology_entity, domain::methodology>(
        ctx,
        query,
        [](const auto& entities) { return methodology_mapper::map(entities); },
        lg(),
        "Reading latest methodologies");
}

std::vector<domain::methodology> methodology_repository::read_latest(context ctx,
                                                                     const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest methodology. " << "id: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<methodology_entity>> |
                       where("id"_c == id && "valid_to"_c == max.value());

    return execute_read_query<methodology_entity, domain::methodology>(
        ctx,
        query,
        [](const auto& entities) { return methodology_mapper::map(entities); },
        lg(),
        "Reading latest methodology by id.");
}

std::vector<domain::methodology>
methodology_repository::read_latest_by_name(context ctx, const std::string& name) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest methodology by name: " << name;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<methodology_entity>> |
                       where("name"_c == name && "valid_to"_c == max.value());

    return execute_read_query<methodology_entity, domain::methodology>(
        ctx,
        query,
        [](const auto& entities) { return methodology_mapper::map(entities); },
        lg(),
        "Reading latest methodology by name.");
}

std::vector<domain::methodology> methodology_repository::read_any_by_name(context ctx,
                                                                          const std::string& name) {
    BOOST_LOG_SEV(lg(), debug) << "Reading any methodology by name: " << name;
    const auto query = sqlgen::read<std::vector<methodology_entity>> | where("name"_c == name) |
                       order_by("valid_from"_c.desc()) | sqlgen::limit(1);

    return execute_read_query<methodology_entity, domain::methodology>(
        ctx,
        query,
        [](const auto& entities) { return methodology_mapper::map(entities); },
        lg(),
        "Reading any methodology by name.");
}


std::vector<domain::methodology> methodology_repository::read_all(context ctx,
                                                                  const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all methodology versions. " << "id: " << id;
    const auto query = sqlgen::read<std::vector<methodology_entity>> | where("id"_c == id) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<methodology_entity, domain::methodology>(
        ctx,
        query,
        [](const auto& entities) { return methodology_mapper::map(entities); },
        lg(),
        "Reading all methodology versions by id.");
}

std::optional<domain::methodology>
methodology_repository::read_at_version(context ctx, const std::string& id, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading methodology at version. " << "id: " << id
                               << " version: " << version;
    const auto query = sqlgen::read<std::vector<methodology_entity>> |
                       where("id"_c == id && "version"_c == version) | sqlgen::limit(1);

    const auto entities = execute_read_query<methodology_entity, domain::methodology>(
        ctx,
        query,
        [](const auto& entities) { return methodology_mapper::map(entities); },
        lg(),
        "Reading methodology at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

methodology_repository::remove_status methodology_repository::remove(
    context ctx, const std::string& id, std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing methodology. " << "id: " << id;
    const auto current = read_latest(ctx, id);
    if (current.empty())
        return remove_status::missing;
    // The protocol states the version as a uint32 and the row carries it as an
    // int, so the comparison states the conversion rather than relying on one.
    if (version && static_cast<std::uint32_t>(current.front().version) != *version)
        return remove_status::conflicting;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    // The row is named by its version as well as by its key, so the removal
    // cannot close a row that replaced the one the caller read between the
    // read above and this statement.
    const auto expected = version ? static_cast<int>(*version) : current.front().version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<methodology_entity> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value() &&
                             "version"_c == expected);

    execute_delete_query(ctx, query, lg(), "Removing methodology from database.");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(ctx, id).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void methodology_repository::remove(context ctx, const std::string& id) {
    static_cast<void>(remove(ctx, id, std::nullopt));
}

std::vector<domain::methodology>
methodology_repository::read_latest(context ctx,
                                    std::uint32_t offset,
                                    std::uint32_t limit,
                                    const ores::utility::domain::order& order) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest methodologies with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<methodology_entity>> |
                       where("valid_to"_c == max.value()) | sqlgen::offset(offset) |
                       sqlgen::limit(limit);

    return execute_ordered_read_query<methodology_entity, domain::methodology>(
        ctx,
        query,
        list_order(order, {"id"}, false),
        [](const auto& entities) { return methodology_mapper::map(entities); },
        lg(),
        "Reading latest methodologies with pagination.");
}

std::uint32_t methodology_repository::get_total_methodology_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active methodology count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto query = sqlgen::select_from<methodology_entity>(sqlgen::count().as<"count">()) |
                       where("valid_to"_c == max.value()) | sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active methodology count: " << count;
    return count;
}

std::vector<domain::methodology>
methodology_repository::read_latest(context ctx, const std::vector<std::string>& ids) {
    if (ids.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<methodology_entity>> |
                       where("id"_c.in(ids) && "valid_to"_c == max.value());
    auto result = execute_read_query<methodology_entity, domain::methodology>(
        ctx,
        query,
        [](const auto& entities) { return methodology_mapper::map(entities); },
        lg(),
        "Reading latest methodologies by ids.");
    return result;
}

void methodology_repository::remove(context ctx, const std::vector<std::string>& ids) {
    // A batch of nothing addresses no row, so there is nothing to delete. The
    // query builder renders an empty key list as an empty IN (), which the
    // server refuses as a syntax error; the read overloads answer the empty
    // case the same way. The compound branch above is left alone: it loops, so
    // it already removes nothing, and its length check still refuses an
    // asymmetric pair.
    if (ids.empty())
        return;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<methodology_entity> |
                       where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing methodologies.");
}


}
