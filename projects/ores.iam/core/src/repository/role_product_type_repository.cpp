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
#include "ores.iam.core/repository/role_product_type_repository.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.iam.api/domain/role_product_type.hpp"
#include "ores.iam.api/domain/role_product_type_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam.core/repository/role_product_type_entity.hpp"
#include "ores.iam.core/repository/role_product_type_mapper.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

namespace ores::iam::repository {

namespace query_builder = ores::database::query_builder;
using namespace ores::database::query_builder::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string role_product_type_repository::sql() {
    return query_builder::insert_statement_sql<role_product_type_entity>(lg());
}

role_product_type_repository::role_product_type_repository(context ctx)
    : ctx_(std::move(ctx)) {}

ores::utility::domain::precondition
role_product_type_repository::replace_claim(const domain::role_product_type& v) {
    const auto current = read_latest(v.role_id, v.product_type_code);
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::role_product_type
role_product_type_repository::apply_claim(const domain::role_product_type& v,
                                          const ores::utility::domain::precondition& claim) {
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
            const auto current = read_latest(v.role_id, v.product_type_code);
            t.version = current.empty() ? 0 : current.front().version;
            break;
        }
    }
    return t;
}

void role_product_type_repository::write(const domain::role_product_type& role_product_type) {
    write(role_product_type, replace_claim(role_product_type));
}

void role_product_type_repository::write(
    const std::vector<domain::role_product_type>& role_product_types) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(role_product_types.size());
    for (const auto& item : role_product_types)
        claims.push_back(replace_claim(item));
    write(role_product_types, claims);
}

void role_product_type_repository::write(const domain::role_product_type& role_product_type,
                                         const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing permitted product type to database: "
                               << role_product_type.role_id << "/"
                               << role_product_type.product_type_code;
    const auto t = apply_claim(role_product_type, claim);
    query_builder::execute_write_query(
        ctx_, role_product_type_mapper::map(t), lg(), "writing permitted product type to database");
}

void role_product_type_repository::write(
    const std::vector<domain::role_product_type>& role_product_types,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing permitted product types to database. Count: "
                               << role_product_types.size();
    std::vector<domain::role_product_type> batch;
    batch.reserve(role_product_types.size());
    for (std::size_t i = 0; i < role_product_types.size(); ++i)
        batch.push_back(apply_claim(role_product_types[i], claims[i]));
    query_builder::execute_write_query(ctx_,
                                       role_product_type_mapper::map(batch),
                                       lg(),
                                       "writing permitted product types to database");
}

std::vector<domain::role_product_type> role_product_type_repository::read_latest() {
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<role_product_type_entity>> |
                       query_builder::where("tenant_id"_c == tid && "valid_to"_c == max) |
                       query_builder::order_by("role_id"_c, "product_type_code"_c);

    return query_builder::execute_read_query<role_product_type_entity, domain::role_product_type>(
        ctx_,
        query,
        [](const auto& entities) { return role_product_type_mapper::map(entities); },
        lg(),
        "Reading latest permitted product types");
}

std::vector<domain::role_product_type>
role_product_type_repository::read_latest(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest permitted product types with offset: " << offset
                               << " and limit: " << limit;
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<role_product_type_entity>> |
                       query_builder::where("tenant_id"_c == tid && "valid_to"_c == max) |
                       query_builder::order_by("role_id"_c, "product_type_code"_c) |
                       query_builder::offset(offset) | query_builder::limit(limit);

    return query_builder::execute_read_query<role_product_type_entity, domain::role_product_type>(
        ctx_,
        query,
        [](const auto& entities) { return role_product_type_mapper::map(entities); },
        lg(),
        "Reading latest permitted product types (paginated).");
}

std::vector<domain::role_product_type>
role_product_type_repository::read_latest(const boost::uuids::uuid& role_id,
                                          const std::string& product_type_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest permitted product type. " << role_id << "/"
                               << product_type_code;

    static constexpr auto max = db_timestamp::max();
    const auto role_id_str = boost::uuids::to_string(role_id);
    const auto product_type_code_str = product_type_code;
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<role_product_type_entity>> |
        query_builder::where("tenant_id"_c == tid && "role_id"_c == role_id_str &&
                             "product_type_code"_c == product_type_code && "valid_to"_c == max);

    return query_builder::execute_read_query<role_product_type_entity, domain::role_product_type>(
        ctx_,
        query,
        [](const auto& entities) { return role_product_type_mapper::map(entities); },
        lg(),
        "Reading latest permitted product type by key.");
}

std::uint32_t role_product_type_repository::get_total_role_product_type_count() {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active permitted product types count";
    static constexpr auto max = db_timestamp::max();

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<role_product_type_entity>> |
                       query_builder::where("tenant_id"_c == tid && "valid_to"_c == max);

    const auto count = query_builder::execute_count_query<role_product_type_entity>(
        ctx_, query, std::nullopt, lg(), "Counting permitted product types");
    BOOST_LOG_SEV(lg(), debug) << "Total active permitted product types count: " << count;
    return count;
}

std::vector<domain::role_product_type>
role_product_type_repository::read_latest_by_role(const boost::uuids::uuid& role_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest permitted product types. Role: " << role_id;

    static constexpr auto max = db_timestamp::max();
    const auto role_id_str = boost::uuids::to_string(role_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<role_product_type_entity>> |
                       query_builder::where("tenant_id"_c == tid && "role_id"_c == role_id_str &&
                                            "valid_to"_c == max) |
                       query_builder::order_by("product_type_code"_c);

    auto rows =
        query_builder::execute_read_query<role_product_type_entity, domain::role_product_type>(
            ctx_,
            query,
            [](const auto& entities) { return role_product_type_mapper::map(entities); },
            lg(),
            "Reading latest permitted product types by role.");

    return rows;
}

std::vector<domain::role_product_type>
role_product_type_repository::read_latest_by_product_type(const std::string& product_type_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest permitted product types. Product Type: "
                               << product_type_code;

    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<role_product_type_entity>> |
        query_builder::where("tenant_id"_c == tid && "product_type_code"_c == product_type_code &&
                             "valid_to"_c == max) |
        query_builder::order_by("role_id"_c);

    auto rows =
        query_builder::execute_read_query<role_product_type_entity, domain::role_product_type>(
            ctx_,
            query,
            [](const auto& entities) { return role_product_type_mapper::map(entities); },
            lg(),
            "Reading latest permitted product types by product_type.");

    return rows;
}

std::vector<domain::role_product_type> role_product_type_repository::read_latest_by_role(
    const boost::uuids::uuid& role_id, std::uint32_t offset, std::uint32_t limit) {
    const auto role_id_str = boost::uuids::to_string(role_id);
    BOOST_LOG_SEV(lg(), debug) << "Reading latest permitted product types. Role: " << role_id
                               << " offset: " << offset << " limit: " << limit;

    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<role_product_type_entity>> |
                       query_builder::where("tenant_id"_c == tid && "role_id"_c == role_id_str &&
                                            "valid_to"_c == max) |
                       query_builder::order_by("product_type_code"_c) |
                       query_builder::offset(offset) | query_builder::limit(limit);

    auto rows =
        query_builder::execute_read_query<role_product_type_entity, domain::role_product_type>(
            ctx_,
            query,
            [](const auto& entities) { return role_product_type_mapper::map(entities); },
            lg(),
            "Reading latest permitted product types by role (paginated).");

    return rows;
}

std::uint32_t role_product_type_repository::get_total_role_product_type_count_by_role(
    const boost::uuids::uuid& role_id) {
    const auto role_id_str = boost::uuids::to_string(role_id);
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active permitted product types count. Role: "
                               << role_id;
    static constexpr auto max = db_timestamp::max();

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<role_product_type_entity>> |
                       query_builder::where("tenant_id"_c == tid && "role_id"_c == role_id_str &&
                                            "valid_to"_c == max);

    const auto count = query_builder::execute_count_query<role_product_type_entity>(
        ctx_, query, std::nullopt, lg(), "Counting permitted product types");
    BOOST_LOG_SEV(lg(), debug) << "Total active permitted product types count by role: " << count;
    return count;
}

std::vector<domain::role_product_type> role_product_type_repository::read_latest_by_product_type(
    const std::string& product_type_code, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest permitted product types. Product Type: "
                               << product_type_code << " offset: " << offset << " limit: " << limit;

    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<role_product_type_entity>> |
        query_builder::where("tenant_id"_c == tid && "product_type_code"_c == product_type_code &&
                             "valid_to"_c == max) |
        query_builder::order_by("role_id"_c) | query_builder::offset(offset) |
        query_builder::limit(limit);

    auto rows =
        query_builder::execute_read_query<role_product_type_entity, domain::role_product_type>(
            ctx_,
            query,
            [](const auto& entities) { return role_product_type_mapper::map(entities); },
            lg(),
            "Reading latest permitted product types by product_type (paginated).");

    return rows;
}

std::uint32_t role_product_type_repository::get_total_role_product_type_count_by_product_type(
    const std::string& product_type_code) {
    BOOST_LOG_SEV(lg(), debug)
        << "Retrieving total active permitted product types count. Product Type: "
        << product_type_code;
    static constexpr auto max = db_timestamp::max();

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<role_product_type_entity>> |
        query_builder::where("tenant_id"_c == tid && "product_type_code"_c == product_type_code &&
                             "valid_to"_c == max);

    const auto count = query_builder::execute_count_query<role_product_type_entity>(
        ctx_, query, std::nullopt, lg(), "Counting permitted product types");
    BOOST_LOG_SEV(lg(), debug) << "Total active permitted product types count by product_type: "
                               << count;
    return count;
}

std::vector<role_product_type_history_row>
role_product_type_repository::read_all_by_role(const boost::uuids::uuid& role_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading every version of the permitted product types. Role: "
                               << role_id;

    const auto role_id_str = boost::uuids::to_string(role_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<role_product_type_entity>> |
                       query_builder::where("tenant_id"_c == tid && "role_id"_c == role_id_str) |
                       query_builder::order_by("product_type_code"_c, "valid_from"_c.desc());

    return query_builder::execute_read_query<role_product_type_entity,
                                             role_product_type_history_row>(
        ctx_,
        query,
        [](const auto& entities) {
            std::vector<role_product_type_history_row> rows;
            rows.reserve(entities.size());
            for (const auto& entity : entities) {
                role_product_type_history_row row;
                row.link = role_product_type_mapper::map(entity);
                row.modified_by = row.link.modified_by;
                row.recorded_at = row.link.recorded_at;
                // A version ends when a later version replaces it, which is not
                // a closing, or when the link is closed. The rows of one link run
                // newest first, so the row before this one, when it is the same
                // link, is the next newer version, and it replaced this one only
                // if it carries the next version number. A link opened again
                // after a closing starts its numbering over.
                const bool replaced =
                    !rows.empty() &&
                    rows.back().link.product_type_code == row.link.product_type_code &&
                    rows.back().link.version == row.link.version + 1;
                if (!replaced) {
                    row.closed_at = validity_end(entity.valid_to);
                    if (row.closed_at)
                        row.closed_by = entity.closed_by.value_or("");
                }
                rows.push_back(std::move(row));
            }
            return rows;
        },
        lg(),
        "Reading every version of the permitted product types by role.");
}

void role_product_type_repository::remove(const boost::uuids::uuid& role_id,
                                          const std::string& product_type_code) {
    static_cast<void>(remove(role_id, product_type_code, std::nullopt));
}

role_product_type_repository::remove_status
role_product_type_repository::remove(const boost::uuids::uuid& role_id,
                                     const std::string& product_type_code,
                                     std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing permitted product type from database: " << role_id
                               << "/" << product_type_code;

    const auto current = read_latest(role_id, product_type_code);
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
    const auto role_id_str = boost::uuids::to_string(role_id);
    const auto product_type_code_str = product_type_code;
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::delete_from<role_product_type_entity> |
                       query_builder::where("tenant_id"_c == tid && "role_id"_c == role_id_str &&
                                            "product_type_code"_c == product_type_code &&
                                            "valid_to"_c == max && "version"_c == expected);

    query_builder::execute_delete_query(
        ctx_, query, lg(), "removing permitted product type from database");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(role_id, product_type_code).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void role_product_type_repository::remove(const std::vector<boost::uuids::uuid>& role_ids,
                                          const std::vector<std::string>& product_type_codes) {
    // A junction's key is the pair of columns, and a per-column .in() DELETE
    // would be a cross-product over-delete (rows outside the requested pairs),
    // so each pair is removed on its own.
    if (role_ids.size() != product_type_codes.size())
        throw std::invalid_argument(
            "role_product_type_repository::remove: key column vectors must be the same length");
    for (std::size_t i = 0; i < role_ids.size(); ++i)
        static_cast<void>(remove(role_ids[i], product_type_codes[i], std::nullopt));
}

void role_product_type_repository::remove_by_role(const boost::uuids::uuid& role_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing all permitted product types from database: " << role_id;

    const auto role_id_str = boost::uuids::to_string(role_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::delete_from<role_product_type_entity> |
                       query_builder::where("tenant_id"_c == tid && "role_id"_c == role_id_str);

    query_builder::execute_delete_query(
        ctx_, query, lg(), "removing all permitted product types from database");
}


}
