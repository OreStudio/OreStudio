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
#include "ores.trading.core/repository/structure_identifier_repository.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.trading.api/domain/structure_identifier.hpp"
#include "ores.trading.api/domain/structure_identifier_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.api/messaging/structure_identifier_protocol.hpp"
#include "ores.trading.core/repository/structure_identifier_entity.hpp"
#include "ores.trading.core/repository/structure_identifier_mapper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <initializer_list>
#include <iterator>
#include <map>
#include <optional>
#include <set>
#include <stdexcept>
#include <string>
#include <string_view>
#include <tuple>
#include <utility>
#include <vector>

namespace ores::trading::repository {

namespace query_builder = ores::database::query_builder;
using namespace ores::database::query_builder::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string structure_identifier_repository::sql() {
    return query_builder::insert_statement_sql<structure_identifier_entity>(lg());
}

bool structure_identifier_repository::is_sortable(std::string_view field) {
    const std::initializer_list<std::string_view> sortable = {};
    return std::ranges::find(sortable, field) != sortable.end();
}

namespace {

/*
 * The order a page is read in. An empty field is the default order, which
 * the stated direction reverses; any other field must be sortable, because
 * the service refuses the rest before it reaches the store.
 */
query_builder::order
structure_identifier_list_order(const ores::utility::domain::order& order,
                                std::initializer_list<std::string> default_columns,
                                bool default_descending) {
    if (order.field.empty())
        return query_builder::make_order(
            default_columns, default_descending != order.descending, {"structure_id", "id_type"});
    if (!structure_identifier_repository::is_sortable(order.field))
        throw std::invalid_argument("A list of structure identifiers cannot be ordered by " +
                                    order.field + ".");
    return query_builder::make_order({order.field}, order.descending, {"structure_id", "id_type"});
}

/*
 * The conditions the filter record sets. Every member is optional, and the
 * members a request sets must all hold.
 */
std::optional<query_builder::condition> structure_identifier_filter_condition(
    const std::optional<messaging::structure_identifiers_filter>& filter) {
    if (!filter)
        return std::nullopt;
    std::vector<query_builder::condition> r;
    if (filter->structure_id)
        r.push_back(query_builder::equals("structure_id",
                                          query_builder::filter_value(*filter->structure_id)));
    if (filter->structure_id_one_of) {
        std::vector<query_builder::value> values;
        for (const auto& v : *filter->structure_id_one_of)
            values.push_back(query_builder::filter_value(v));
        r.push_back(query_builder::one_of("structure_id", std::move(values)));
    }
    return query_builder::all_of(std::move(r));
}

}

namespace {

/**
 * @brief Whether a claim is checked against the row its key names now.
 */
bool structure_identifier_claim_reads_current(const ores::utility::domain::precondition& claim) {
    using ores::utility::domain::precondition_kind;
    // The store checks a stated version itself. A claim of nothing still has
    // to state the version it replaces, so only that claim needs the row.
    return claim.kind == precondition_kind::any;
}

using structure_identifier_rows_by_key =
    std::map<std::tuple<std::string, std::string>, std::vector<domain::structure_identifier>>;

/**
 * @brief The current rows the objects name, by the text form of their key.
 *
 * One batch read for the set, so a batch checks its claims against one read
 * rather than one read per row.
 */
structure_identifier_rows_by_key
structure_identifier_current_rows(structure_identifier_repository& repo,
                                  const ores::database::context& ctx,
                                  const std::vector<domain::structure_identifier>& of) {
    std::vector<std::string> structure_ids;
    structure_ids.reserve(of.size());
    std::vector<std::string> id_types;
    id_types.reserve(of.size());
    for (const auto& row : of) {
        structure_ids.push_back(boost::uuids::to_string(row.structure_id));
        id_types.push_back(row.id_type);
    }
    structure_identifier_rows_by_key current;
    for (auto& item : repo.read_latest(ctx, structure_ids, id_types))
        current[std::make_tuple(boost::uuids::to_string(item.structure_id), item.id_type)]
            .push_back(std::move(item));
    return current;
}

/**
 * @brief The current rows that the key of an object names, or none.
 */
const std::vector<domain::structure_identifier>&
structure_identifier_rows_of(const structure_identifier_rows_by_key& current,
                             const domain::structure_identifier& item) {
    static const std::vector<domain::structure_identifier> none;
    const auto found =
        current.find(std::make_tuple(boost::uuids::to_string(item.structure_id), item.id_type));
    return found == current.end() ? none : found->second;
}

/**
 * @brief The claim a replace makes: the version the row carries now, or
 * that no row exists yet.
 */
ores::utility::domain::precondition
structure_identifier_replace_claim(const std::vector<domain::structure_identifier>& current) {
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

/**
 * @brief The object with the claim's version stamped onto it.
 *
 * The caller passes the row the key names now whenever the claim reads it. A
 * claim the store cannot check is refused here rather than ignored.
 */
domain::structure_identifier
structure_identifier_apply_claim(const domain::structure_identifier& v,
                                 const ores::utility::domain::precondition& claim,
                                 const std::vector<domain::structure_identifier>& current) {
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
        case precondition_kind::any:
            // A caller that claims nothing still has to say what it replaces, so
            // the row's current version is stated. A row that moved on between
            // the read and the write is a conflict the trigger raises, never a
            // silent overwrite.
            t.version = current.empty() ? 0 : current.front().version;
            break;
    }
    return t;
}

}

void structure_identifier_repository::write(const context& ctx,
                                            const domain::structure_identifier& v) {
    write(ctx,
          v,
          structure_identifier_replace_claim(
              read_latest(ctx, boost::uuids::to_string(v.structure_id), v.id_type)));
}

void structure_identifier_repository::write(const context& ctx,
                                            const std::vector<domain::structure_identifier>& v) {
    const auto current = structure_identifier_current_rows(*this, ctx, v);
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(v.size());
    for (const auto& item : v)
        claims.push_back(
            structure_identifier_replace_claim(structure_identifier_rows_of(current, item)));
    write(ctx, v, claims);
}

void structure_identifier_repository::write(const context& ctx,
                                            const domain::structure_identifier& v,
                                            const ores::utility::domain::precondition& claim) {
    ORES_LOG_SEV(lg(), debug) << "Writing structure identifier. "
                              << "structure_id: " << v.structure_id << " id_type: " << v.id_type;
    const auto t = structure_identifier_apply_claim(
        v,
        claim,
        structure_identifier_claim_reads_current(claim) ?
            read_latest(ctx, boost::uuids::to_string(v.structure_id), v.id_type) :
            std::vector<domain::structure_identifier>{});
    query_builder::execute_write_query(ctx,
                                       structure_identifier_mapper::map(t),
                                       lg(),
                                       "Writing structure identifier to database.");
}

void structure_identifier_repository::write(
    const context& ctx,
    const std::vector<domain::structure_identifier>& v,
    const std::vector<ores::utility::domain::precondition>& claims) {
    ORES_LOG_SEV(lg(), debug) << "Writing structure identifiers. Count: " << v.size();
    if (claims.size() != v.size())
        throw std::invalid_argument(
            "structure_identifier_repository::write: one claim is needed for each row");
    std::vector<domain::structure_identifier> to_read;
    for (std::size_t i = 0; i < v.size(); ++i) {
        if (structure_identifier_claim_reads_current(claims[i]))
            to_read.push_back(v[i]);
    }
    const auto current = structure_identifier_current_rows(*this, ctx, to_read);
    std::vector<domain::structure_identifier> batch;
    batch.reserve(v.size());
    for (std::size_t i = 0; i < v.size(); ++i)
        batch.push_back(structure_identifier_apply_claim(
            v[i], claims[i], structure_identifier_rows_of(current, v[i])));
    query_builder::execute_write_query(ctx,
                                       structure_identifier_mapper::map(batch),
                                       lg(),
                                       "Writing structure identifiers to database.");
}

std::vector<domain::structure_identifier>
structure_identifier_repository::read_latest(const context& ctx) {
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<structure_identifier_entity>> |
                       query_builder::where("tenant_id"_c == tid && "valid_to"_c == max) |
                       query_builder::order_by("structure_id"_c, "id_type"_c);

    return query_builder::execute_read_query<structure_identifier_entity,
                                             domain::structure_identifier>(
        ctx,
        query,
        [](const auto& entities) { return structure_identifier_mapper::map(entities); },
        lg(),
        "Reading latest structure identifiers");
}

std::vector<domain::structure_identifier> structure_identifier_repository::read_latest(
    const context& ctx, const std::string& structure_id, const std::string& id_type) {
    ORES_LOG_SEV(lg(), debug) << "Reading latest structure identifier. "
                              << "structure_id: " << structure_id << " id_type: " << id_type;
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<structure_identifier_entity>> |
        query_builder::where("tenant_id"_c == tid && "structure_id"_c == structure_id &&
                             "id_type"_c == id_type && "valid_to"_c == max);

    return query_builder::execute_read_query<structure_identifier_entity,
                                             domain::structure_identifier>(
        ctx,
        query,
        [](const auto& entities) { return structure_identifier_mapper::map(entities); },
        lg(),
        "Reading latest structure identifier by structure_id.");
}


std::vector<domain::structure_identifier> structure_identifier_repository::read_all(
    const context& ctx, const std::string& structure_id, const std::string& id_type) {
    ORES_LOG_SEV(lg(), debug) << "Reading all structure identifier versions. "
                              << "structure_id: " << structure_id << " id_type: " << id_type;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<structure_identifier_entity>> |
        query_builder::where("tenant_id"_c == tid && "structure_id"_c == structure_id &&
                             "id_type"_c == id_type) |
        query_builder::order_by("version"_c.desc(), "valid_from"_c.desc());

    return query_builder::execute_read_query<structure_identifier_entity,
                                             domain::structure_identifier>(
        ctx,
        query,
        [](const auto& entities) { return structure_identifier_mapper::map(entities); },
        lg(),
        "Reading all structure identifier versions by structure_id.");
}

std::optional<domain::structure_identifier>
structure_identifier_repository::read_at_version(const context& ctx,
                                                 const std::string& structure_id,
                                                 const std::string& id_type,
                                                 std::uint32_t version) {
    ORES_LOG_SEV(lg(), debug) << "Reading structure identifier at version. "
                              << "structure_id: " << structure_id << " id_type: " << id_type
                              << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<structure_identifier_entity>> |
        query_builder::where("tenant_id"_c == tid && "structure_id"_c == structure_id &&
                             "id_type"_c == id_type && "version"_c == version) |
        query_builder::limit(1);

    const auto entities = query_builder::execute_read_query<structure_identifier_entity,
                                                            domain::structure_identifier>(
        ctx,
        query,
        [](const auto& entities) { return structure_identifier_mapper::map(entities); },
        lg(),
        "Reading structure identifier at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

std::vector<domain::structure_identifier>
structure_identifier_repository::read_latest_by_structure_id(
    const context& ctx,
    const std::string& structure_id,
    std::uint32_t offset,
    std::uint32_t limit,
    const ores::utility::domain::order& order,
    const std::optional<messaging::structure_identifiers_filter>& filter) {
    ORES_LOG_SEV(lg(), debug) << "Reading latest structure identifiers. structure_id: "
                              << structure_id << " offset: " << offset << " limit: " << limit;
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<structure_identifier_entity>> |
        query_builder::where("tenant_id"_c == tid && "structure_id"_c == structure_id &&
                             "valid_to"_c == max) |
        query_builder::offset(offset) | query_builder::limit(limit);

    return query_builder::execute_ordered_read_query<structure_identifier_entity,
                                                     domain::structure_identifier>(
        ctx,
        query,
        structure_identifier_list_order(order, {"structure_id", "id_type"}, false),
        structure_identifier_filter_condition(filter),
        [](const auto& entities) { return structure_identifier_mapper::map(entities); },
        lg(),
        "Reading latest structure identifiers by structure_id.");
}

std::uint32_t structure_identifier_repository::get_total_identifier_count_by_structure_id(
    const context& ctx,
    const std::string& structure_id,
    const std::optional<messaging::structure_identifiers_filter>& filter) {
    ORES_LOG_SEV(lg(), debug)
        << "Retrieving total active structure identifiers count. structure_id: " << structure_id;
    static constexpr auto max = db_timestamp::max();

    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<structure_identifier_entity>> |
        query_builder::where("tenant_id"_c == tid && "structure_id"_c == structure_id &&
                             "valid_to"_c == max);

    return query_builder::execute_count_query<structure_identifier_entity>(
        ctx,
        query,
        structure_identifier_filter_condition(filter),
        lg(),
        "Counting structure identifiers by structure_id");
}


structure_identifier_repository::remove_status
structure_identifier_repository::remove(const context& ctx,
                                        const std::string& structure_id,
                                        const std::string& id_type,
                                        std::optional<std::uint32_t> version) {
    ORES_LOG_SEV(lg(), debug) << "Removing structure identifier. "
                              << "structure_id: " << structure_id << " id_type: " << id_type;
    const auto current = read_latest(ctx, structure_id, id_type);
    if (current.empty())
        return remove_status::missing;
    // The protocol states the version as a uint32 and the row carries it as an
    // int, so the comparison states the conversion rather than relying on one.
    if (version && static_cast<std::uint32_t>(current.front().version) != *version)
        return remove_status::conflicting;
    static constexpr auto max = db_timestamp::max();
    // The row is named by its version as well as by its key, so the removal
    // cannot close a row that replaced the one the caller read between the
    // read above and this statement.
    const auto expected = version ? static_cast<int>(*version) : current.front().version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::delete_from<structure_identifier_entity> |
        query_builder::where("tenant_id"_c == tid && "structure_id"_c == structure_id &&
                             "id_type"_c == id_type && "valid_to"_c == max &&
                             "version"_c == expected);

    query_builder::execute_delete_query(
        ctx, query, lg(), "Removing structure identifier from database.");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(ctx, structure_id, id_type).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void structure_identifier_repository::remove(const context& ctx,
                                             const std::string& structure_id,
                                             const std::string& id_type) {
    static_cast<void>(remove(ctx, structure_id, id_type, std::nullopt));
}

std::vector<domain::structure_identifier> structure_identifier_repository::read_latest(
    const context& ctx,
    std::uint32_t offset,
    std::uint32_t limit,
    const ores::utility::domain::order& order,
    const std::optional<messaging::structure_identifiers_filter>& filter,
    const std::optional<std::string>& as_of) {
    ORES_LOG_SEV(lg(), debug) << "Reading latest structure identifiers with offset: " << offset
                              << " and limit: " << limit;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<structure_identifier_entity>> |
                       query_builder::where("tenant_id"_c == tid) | query_builder::offset(offset) |
                       query_builder::limit(limit);

    return query_builder::execute_ordered_read_query<structure_identifier_entity,
                                                     domain::structure_identifier>(
        ctx,
        query,
        structure_identifier_list_order(order, {"structure_id", "id_type"}, false),
        query_builder::narrowed(query_builder::valid_at(as_of),
                                structure_identifier_filter_condition(filter)),
        [](const auto& entities) { return structure_identifier_mapper::map(entities); },
        lg(),
        "Reading latest structure identifiers with pagination.");
}

std::uint32_t structure_identifier_repository::get_total_identifier_count(
    const context& ctx,
    const std::optional<messaging::structure_identifiers_filter>& filter,
    const std::optional<std::string>& as_of) {
    ORES_LOG_SEV(lg(), debug) << "Retrieving total active structure identifier count";

    const auto tid = ctx.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<structure_identifier_entity>> |
                       query_builder::where("tenant_id"_c == tid);

    return query_builder::execute_count_query<structure_identifier_entity>(
        ctx,
        query,
        query_builder::narrowed(query_builder::valid_at(as_of),
                                structure_identifier_filter_condition(filter)),
        lg(),
        "Counting structure identifiers");
}

std::vector<domain::structure_identifier>
structure_identifier_repository::read_latest(const context& ctx,
                                             const std::vector<std::string>& structure_ids,
                                             const std::vector<std::string>& id_types) {
    if (structure_ids.empty() || id_types.empty())
        return {};
    if (id_types.size() != structure_ids.size())
        throw std::invalid_argument("structure_identifier_repository::read_latest: key column "
                                    "vectors must be the same length");
    // Every key is a bind and the store's bind count is finite, so a larger
    // key set is read one chunk at a time.
    constexpr std::size_t chunk_size = 1000;
    if (structure_ids.size() > chunk_size) {
        std::vector<domain::structure_identifier> all;
        for (std::size_t begin = 0; begin < structure_ids.size(); begin += chunk_size) {
            const auto end = std::min(begin + chunk_size, structure_ids.size());
            auto part = read_latest(
                ctx,
                std::vector<std::string>(structure_ids.begin() + begin,
                                         structure_ids.begin() + end),
                std::vector<std::string>(id_types.begin() + begin, id_types.begin() + end));
            all.insert(all.end(),
                       std::make_move_iterator(part.begin()),
                       std::make_move_iterator(part.end()));
        }
        return all;
    }
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<structure_identifier_entity>> |
        query_builder::where("tenant_id"_c == tid && "structure_id"_c.in(structure_ids) &&
                             "id_type"_c.in(id_types) && "valid_to"_c == max);
    auto result = query_builder::execute_read_query<structure_identifier_entity,
                                                    domain::structure_identifier>(
        ctx,
        query,
        [](const auto& entities) { return structure_identifier_mapper::map(entities); },
        lg(),
        "Reading latest structure identifiers by ids.");
    // Compound key: the query above is a per-column .in() cross-product
    // over-fetch (the query builder has no tuple IN), so filter down to the
    // exact requested key-tuples here.
    std::set<std::tuple<std::string, std::string>> requested;
    for (std::size_t i = 0; i < structure_ids.size(); ++i)
        requested.emplace(structure_ids[i], id_types[i]);
    std::vector<domain::structure_identifier> filtered;
    filtered.reserve(result.size());
    for (auto& item : result) {
        if (requested.contains(
                std::make_tuple(boost::uuids::to_string(item.structure_id), item.id_type)))
            filtered.push_back(std::move(item));
    }
    return filtered;
}

void structure_identifier_repository::remove(const context& ctx,
                                             const std::vector<std::string>& structure_ids,
                                             const std::vector<std::string>& id_types) {
    // Compound key: a per-column .in() DELETE would be a cross-product
    // over-delete (rows outside the requested tuples), and a DELETE can't
    // be filtered after the fact like a read -- remove one tuple at a time.
    if (id_types.size() != structure_ids.size())
        throw std::invalid_argument(
            "structure_identifier_repository::remove: key column vectors must be the same length");
    for (std::size_t i = 0; i < structure_ids.size(); ++i)
        remove(ctx, structure_ids[i], id_types[i]);
}


}
