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
#include "ores.dq.core/repository/subject_area_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/stated_order.hpp"
#include "ores.dq.api/domain/subject_area_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq.core/repository/subject_area_entity.hpp"
#include "ores.dq.core/repository/subject_area_mapper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <algorithm>
#include <initializer_list>
#include <set>
#include <sqlgen/postgres.hpp>
#include <stdexcept>
#include <string_view>
#include <tuple>

namespace ores::dq::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string subject_area_repository::sql() {
    return generate_create_table_sql<subject_area_entity>(lg());
}

bool subject_area_repository::is_sortable(std::string_view field) {
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
        return make_order(
            default_columns, default_descending != order.descending, {"name", "domain_name"});
    if (!subject_area_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of subject areas cannot be ordered by " + order.field +
                                    ".");
    return make_order({order.field}, order.descending, {"name", "domain_name"});
}

}

ores::utility::domain::precondition
subject_area_repository::replace_claim(context ctx, const domain::subject_area& v) {
    const auto current = read_latest(ctx, v.name, v.domain_name);
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::subject_area subject_area_repository::apply_claim(
    context ctx, const domain::subject_area& v, const ores::utility::domain::precondition& claim) {
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
            const auto current = read_latest(ctx, v.name, v.domain_name);
            t.version = current.empty() ? 0 : current.front().version;
            break;
        }
    }
    return t;
}

void subject_area_repository::write(context ctx, const domain::subject_area& v) {
    write(ctx, v, replace_claim(ctx, v));
}

void subject_area_repository::write(context ctx, const std::vector<domain::subject_area>& v) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(replace_claim(ctx, item));
    write(ctx, v, claims);
}

void subject_area_repository::write(context ctx,
                                    const domain::subject_area& v,
                                    const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing subject area. " << "name: " << v.name
                               << " domain_name: " << v.domain_name;
    const auto t = apply_claim(ctx, v, claim);
    execute_write_query(
        ctx, subject_area_mapper::map(t), lg(), "Writing subject area to database.");
}

void subject_area_repository::write(
    context ctx,
    const std::vector<domain::subject_area>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing subject areas. Count: " << v.size();
    std::vector<domain::subject_area> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(apply_claim(ctx, v[i], claims[i]));
    execute_write_query(
        ctx, subject_area_mapper::map(batch), lg(), "Writing subject areas to database.");
}

std::vector<domain::subject_area> subject_area_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<subject_area_entity>> |
                       where("valid_to"_c == max.value()) | order_by("name"_c, "domain_name"_c);

    return execute_read_query<subject_area_entity, domain::subject_area>(
        ctx,
        query,
        [](const auto& entities) { return subject_area_mapper::map(entities); },
        lg(),
        "Reading latest subject areas");
}

std::vector<domain::subject_area> subject_area_repository::read_latest(
    context ctx, const std::string& name, const std::string& domain_name) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest subject area. " << "name: " << name
                               << " domain_name: " << domain_name;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query =
        sqlgen::read<std::vector<subject_area_entity>> |
        where("name"_c == name && "domain_name"_c == domain_name && "valid_to"_c == max.value());

    return execute_read_query<subject_area_entity, domain::subject_area>(
        ctx,
        query,
        [](const auto& entities) { return subject_area_mapper::map(entities); },
        lg(),
        "Reading latest subject area by name.");
}


std::vector<domain::subject_area> subject_area_repository::read_all(
    context ctx, const std::string& name, const std::string& domain_name) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all subject area versions. " << "name: " << name
                               << " domain_name: " << domain_name;
    const auto query = sqlgen::read<std::vector<subject_area_entity>> |
                       where("name"_c == name && "domain_name"_c == domain_name) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<subject_area_entity, domain::subject_area>(
        ctx,
        query,
        [](const auto& entities) { return subject_area_mapper::map(entities); },
        lg(),
        "Reading all subject area versions by name.");
}

std::optional<domain::subject_area> subject_area_repository::read_at_version(
    context ctx, const std::string& name, const std::string& domain_name, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading subject area at version. " << "name: " << name
                               << " domain_name: " << domain_name << " version: " << version;
    const auto query =
        sqlgen::read<std::vector<subject_area_entity>> |
        where("name"_c == name && "domain_name"_c == domain_name && "version"_c == version) |
        sqlgen::limit(1);

    const auto entities = execute_read_query<subject_area_entity, domain::subject_area>(
        ctx,
        query,
        [](const auto& entities) { return subject_area_mapper::map(entities); },
        lg(),
        "Reading subject area at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

subject_area_repository::remove_status
subject_area_repository::remove(context ctx,
                                const std::string& name,
                                const std::string& domain_name,
                                std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing subject area. " << "name: " << name
                               << " domain_name: " << domain_name;
    const auto current = read_latest(ctx, name, domain_name);
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
    const auto query =
        sqlgen::delete_from<subject_area_entity> |
        where("tenant_id"_c == tid && "name"_c == name && "domain_name"_c == domain_name &&
              "valid_to"_c == max.value() && "version"_c == expected);

    execute_delete_query(ctx, query, lg(), "Removing subject area from database.");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(ctx, name, domain_name).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void subject_area_repository::remove(context ctx,
                                     const std::string& name,
                                     const std::string& domain_name) {
    static_cast<void>(remove(ctx, name, domain_name, std::nullopt));
}

std::vector<domain::subject_area>
subject_area_repository::read_latest(context ctx,
                                     std::uint32_t offset,
                                     std::uint32_t limit,
                                     const ores::utility::domain::order& order) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest subject areas with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<subject_area_entity>> |
                       where("valid_to"_c == max.value()) | sqlgen::offset(offset) |
                       sqlgen::limit(limit);

    return execute_ordered_read_query<subject_area_entity, domain::subject_area>(
        ctx,
        query,
        list_order(order, {"name", "domain_name"}, false),
        std::nullopt,
        [](const auto& entities) { return subject_area_mapper::map(entities); },
        lg(),
        "Reading latest subject areas with pagination.");
}

std::uint32_t subject_area_repository::get_total_area_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active subject area count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    const auto query =
        sqlgen::read<std::vector<subject_area_entity>> | where("valid_to"_c == max.value());

    return execute_count_query<subject_area_entity>(
        ctx, query, std::nullopt, lg(), "Counting subject areas");
}

std::vector<domain::subject_area>
subject_area_repository::read_latest(context ctx,
                                     const std::vector<std::string>& names,
                                     const std::vector<std::string>& domain_names) {
    if (names.empty() || domain_names.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<subject_area_entity>> |
                       where("name"_c.in(names) && "domain_name"_c.in(domain_names) &&
                             "valid_to"_c == max.value());
    auto result = execute_read_query<subject_area_entity, domain::subject_area>(
        ctx,
        query,
        [](const auto& entities) { return subject_area_mapper::map(entities); },
        lg(),
        "Reading latest subject areas by ids.");
    // Compound key: the query above is a per-column .in() cross-product
    // over-fetch (sqlgen has no tuple/composite IN), so filter down to the
    // exact requested key-tuples here.
    if (domain_names.size() != names.size())
        throw std::invalid_argument(
            "subject_area_repository::read_latest: key column vectors must be the same length");
    std::set<std::tuple<std::string, std::string>> requested;
    for (std::size_t i = 0; i < names.size(); ++i)
        requested.emplace(names[i], domain_names[i]);
    std::vector<domain::subject_area> filtered;
    filtered.reserve(result.size());
    for (auto& item : result) {
        if (requested.contains(std::make_tuple(item.name, item.domain_name)))
            filtered.push_back(std::move(item));
    }
    return filtered;
}

void subject_area_repository::remove(context ctx,
                                     const std::vector<std::string>& names,
                                     const std::vector<std::string>& domain_names) {
    // Compound key: a per-column .in() DELETE would be a cross-product
    // over-delete (rows outside the requested tuples), and a DELETE can't
    // be filtered after the fact like a read -- remove one tuple at a time.
    if (domain_names.size() != names.size())
        throw std::invalid_argument(
            "subject_area_repository::remove: key column vectors must be the same length");
    for (std::size_t i = 0; i < names.size(); ++i)
        remove(ctx, names[i], domain_names[i]);
}


}
