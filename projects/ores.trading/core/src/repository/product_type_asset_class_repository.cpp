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
#include "ores.trading.core/repository/product_type_asset_class_repository.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.trading.api/domain/product_type_asset_class.hpp"
#include "ores.trading.api/domain/product_type_asset_class_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.core/repository/product_type_asset_class_entity.hpp"
#include "ores.trading.core/repository/product_type_asset_class_mapper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

namespace ores::trading::repository {

namespace query_builder = ores::database::query_builder;
using namespace ores::database::query_builder::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string product_type_asset_class_repository::sql() {
    return query_builder::insert_statement_sql<product_type_asset_class_entity>(lg());
}

product_type_asset_class_repository::product_type_asset_class_repository(context ctx)
    : ctx_(std::move(ctx)) {}

ores::utility::domain::precondition
product_type_asset_class_repository::replace_claim(const domain::product_type_asset_class& v) {
    const auto current = read_latest(v.product_type_code, v.asset_class_code);
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::product_type_asset_class
product_type_asset_class_repository::apply_claim(const domain::product_type_asset_class& v,
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
            const auto current = read_latest(v.product_type_code, v.asset_class_code);
            t.version = current.empty() ? 0 : current.front().version;
            break;
        }
    }
    return t;
}

void product_type_asset_class_repository::write(
    const domain::product_type_asset_class& product_type_asset_class) {
    write(product_type_asset_class, replace_claim(product_type_asset_class));
}

void product_type_asset_class_repository::write(
    const std::vector<domain::product_type_asset_class>& product_type_asset_classes) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(product_type_asset_classes.size());
    for (const auto& item : product_type_asset_classes)
        claims.push_back(replace_claim(item));
    write(product_type_asset_classes, claims);
}

void product_type_asset_class_repository::write(
    const domain::product_type_asset_class& product_type_asset_class,
    const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing product type market to database: "
                               << product_type_asset_class.product_type_code << "/"
                               << product_type_asset_class.asset_class_code;
    const auto t = apply_claim(product_type_asset_class, claim);
    query_builder::execute_write_query(ctx_,
                                       product_type_asset_class_mapper::map(t),
                                       lg(),
                                       "writing product type market to database");
}

void product_type_asset_class_repository::write(
    const std::vector<domain::product_type_asset_class>& product_type_asset_classes,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing product type markets to database. Count: "
                               << product_type_asset_classes.size();
    std::vector<domain::product_type_asset_class> batch;
    batch.reserve(product_type_asset_classes.size());
    for (std::size_t i = 0; i < product_type_asset_classes.size(); ++i)
        batch.push_back(apply_claim(product_type_asset_classes[i], claims[i]));
    query_builder::execute_write_query(ctx_,
                                       product_type_asset_class_mapper::map(batch),
                                       lg(),
                                       "writing product type markets to database");
}

std::vector<domain::product_type_asset_class> product_type_asset_class_repository::read_latest() {
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<product_type_asset_class_entity>> |
                       query_builder::where("tenant_id"_c == tid && "valid_to"_c == max) |
                       query_builder::order_by("product_type_code"_c, "asset_class_code"_c);

    return query_builder::execute_read_query<product_type_asset_class_entity,
                                             domain::product_type_asset_class>(
        ctx_,
        query,
        [](const auto& entities) { return product_type_asset_class_mapper::map(entities); },
        lg(),
        "Reading latest product type markets");
}

std::vector<domain::product_type_asset_class>
product_type_asset_class_repository::read_latest(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest product type markets with offset: " << offset
                               << " and limit: " << limit;
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<product_type_asset_class_entity>> |
                       query_builder::where("tenant_id"_c == tid && "valid_to"_c == max) |
                       query_builder::order_by("product_type_code"_c, "asset_class_code"_c) |
                       query_builder::offset(offset) | query_builder::limit(limit);

    return query_builder::execute_read_query<product_type_asset_class_entity,
                                             domain::product_type_asset_class>(
        ctx_,
        query,
        [](const auto& entities) { return product_type_asset_class_mapper::map(entities); },
        lg(),
        "Reading latest product type markets (paginated).");
}

std::vector<domain::product_type_asset_class>
product_type_asset_class_repository::read_latest(const std::string& product_type_code,
                                                 const std::string& asset_class_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest product type market. " << product_type_code << "/"
                               << asset_class_code;

    static constexpr auto max = db_timestamp::max();
    const auto product_type_code_str = product_type_code;
    const auto asset_class_code_str = asset_class_code;
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<product_type_asset_class_entity>> |
        query_builder::where("tenant_id"_c == tid && "product_type_code"_c == product_type_code &&
                             "asset_class_code"_c == asset_class_code && "valid_to"_c == max);

    return query_builder::execute_read_query<product_type_asset_class_entity,
                                             domain::product_type_asset_class>(
        ctx_,
        query,
        [](const auto& entities) { return product_type_asset_class_mapper::map(entities); },
        lg(),
        "Reading latest product type market by key.");
}

std::uint32_t product_type_asset_class_repository::get_total_product_type_asset_class_count() {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active product type markets count";
    static constexpr auto max = db_timestamp::max();

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<product_type_asset_class_entity>> |
                       query_builder::where("tenant_id"_c == tid && "valid_to"_c == max);

    const auto count = query_builder::execute_count_query<product_type_asset_class_entity>(
        ctx_, query, std::nullopt, lg(), "Counting product type markets");
    BOOST_LOG_SEV(lg(), debug) << "Total active product type markets count: " << count;
    return count;
}

std::vector<domain::product_type_asset_class>
product_type_asset_class_repository::read_latest_by_product_type(
    const std::string& product_type_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest product type markets. Product Type: "
                               << product_type_code;

    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<product_type_asset_class_entity>> |
        query_builder::where("tenant_id"_c == tid && "product_type_code"_c == product_type_code &&
                             "valid_to"_c == max) |
        query_builder::order_by("asset_class_code"_c);

    auto rows = query_builder::execute_read_query<product_type_asset_class_entity,
                                                  domain::product_type_asset_class>(
        ctx_,
        query,
        [](const auto& entities) { return product_type_asset_class_mapper::map(entities); },
        lg(),
        "Reading latest product type markets by product_type.");

    return rows;
}

std::vector<domain::product_type_asset_class>
product_type_asset_class_repository::read_latest_by_asset_class(
    const std::string& asset_class_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest product type markets. Market: "
                               << asset_class_code;

    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<product_type_asset_class_entity>> |
        query_builder::where("tenant_id"_c == tid && "asset_class_code"_c == asset_class_code &&
                             "valid_to"_c == max) |
        query_builder::order_by("product_type_code"_c);

    auto rows = query_builder::execute_read_query<product_type_asset_class_entity,
                                                  domain::product_type_asset_class>(
        ctx_,
        query,
        [](const auto& entities) { return product_type_asset_class_mapper::map(entities); },
        lg(),
        "Reading latest product type markets by asset_class.");

    return rows;
}

std::vector<domain::product_type_asset_class>
product_type_asset_class_repository::read_latest_by_product_type(
    const std::string& product_type_code, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest product type markets. Product Type: "
                               << product_type_code << " offset: " << offset << " limit: " << limit;

    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<product_type_asset_class_entity>> |
        query_builder::where("tenant_id"_c == tid && "product_type_code"_c == product_type_code &&
                             "valid_to"_c == max) |
        query_builder::order_by("asset_class_code"_c) | query_builder::offset(offset) |
        query_builder::limit(limit);

    auto rows = query_builder::execute_read_query<product_type_asset_class_entity,
                                                  domain::product_type_asset_class>(
        ctx_,
        query,
        [](const auto& entities) { return product_type_asset_class_mapper::map(entities); },
        lg(),
        "Reading latest product type markets by product_type (paginated).");

    return rows;
}

std::uint32_t
product_type_asset_class_repository::get_total_product_type_asset_class_count_by_product_type(
    const std::string& product_type_code) {
    BOOST_LOG_SEV(lg(), debug)
        << "Retrieving total active product type markets count. Product Type: "
        << product_type_code;
    static constexpr auto max = db_timestamp::max();

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<product_type_asset_class_entity>> |
        query_builder::where("tenant_id"_c == tid && "product_type_code"_c == product_type_code &&
                             "valid_to"_c == max);

    const auto count = query_builder::execute_count_query<product_type_asset_class_entity>(
        ctx_, query, std::nullopt, lg(), "Counting product type markets");
    BOOST_LOG_SEV(lg(), debug) << "Total active product type markets count by product_type: "
                               << count;
    return count;
}

std::uint32_t
product_type_asset_class_repository::get_total_product_type_asset_class_count_by_asset_class(
    const std::string& asset_class_code) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active product type markets count. Market: "
                               << asset_class_code;
    static constexpr auto max = db_timestamp::max();

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<product_type_asset_class_entity>> |
        query_builder::where("tenant_id"_c == tid && "asset_class_code"_c == asset_class_code &&
                             "valid_to"_c == max);

    const auto count = query_builder::execute_count_query<product_type_asset_class_entity>(
        ctx_, query, std::nullopt, lg(), "Counting product type markets");
    BOOST_LOG_SEV(lg(), debug) << "Total active product type markets count by asset_class: "
                               << count;
    return count;
}

std::vector<product_type_asset_class_history_row>
product_type_asset_class_repository::read_all_by_product_type(
    const std::string& product_type_code) {
    BOOST_LOG_SEV(lg(), debug)
        << "Reading every version of the product type markets. Product Type: " << product_type_code;

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<product_type_asset_class_entity>> |
        query_builder::where("tenant_id"_c == tid && "product_type_code"_c == product_type_code) |
        query_builder::order_by("asset_class_code"_c, "valid_from"_c.desc());

    return query_builder::execute_read_query<product_type_asset_class_entity,
                                             product_type_asset_class_history_row>(
        ctx_,
        query,
        [](const auto& entities) {
            std::vector<product_type_asset_class_history_row> rows;
            rows.reserve(entities.size());
            for (const auto& entity : entities) {
                product_type_asset_class_history_row row;
                row.link = product_type_asset_class_mapper::map(entity);
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
                    rows.back().link.asset_class_code == row.link.asset_class_code &&
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
        "Reading every version of the product type markets by product_type.");
}

void product_type_asset_class_repository::remove(const std::string& product_type_code,
                                                 const std::string& asset_class_code) {
    static_cast<void>(remove(product_type_code, asset_class_code, std::nullopt));
}

product_type_asset_class_repository::remove_status
product_type_asset_class_repository::remove(const std::string& product_type_code,
                                            const std::string& asset_class_code,
                                            std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing product type market from database: "
                               << product_type_code << "/" << asset_class_code;

    const auto current = read_latest(product_type_code, asset_class_code);
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
    const auto product_type_code_str = product_type_code;
    const auto asset_class_code_str = asset_class_code;
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::delete_from<product_type_asset_class_entity> |
        query_builder::where("tenant_id"_c == tid && "product_type_code"_c == product_type_code &&
                             "asset_class_code"_c == asset_class_code && "valid_to"_c == max &&
                             "version"_c == expected);

    query_builder::execute_delete_query(
        ctx_, query, lg(), "removing product type market from database");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(product_type_code, asset_class_code).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void product_type_asset_class_repository::remove(
    const std::vector<std::string>& product_type_codes,
    const std::vector<std::string>& asset_class_codes) {
    // A junction's key is the pair of columns, and a per-column .in() DELETE
    // would be a cross-product over-delete (rows outside the requested pairs),
    // so each pair is removed on its own.
    if (product_type_codes.size() != asset_class_codes.size())
        throw std::invalid_argument("product_type_asset_class_repository::remove: key column "
                                    "vectors must be the same length");
    for (std::size_t i = 0; i < product_type_codes.size(); ++i)
        static_cast<void>(remove(product_type_codes[i], asset_class_codes[i], std::nullopt));
}

void product_type_asset_class_repository::remove_by_product_type(
    const std::string& product_type_code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing all product type markets from database: "
                               << product_type_code;

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::delete_from<product_type_asset_class_entity> |
        query_builder::where("tenant_id"_c == tid && "product_type_code"_c == product_type_code);

    query_builder::execute_delete_query(
        ctx_, query, lg(), "removing all product type markets from database");
}


}
