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
#include "ores.refdata.core/repository/book_product_type_repository.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.refdata.api/domain/book_product_type.hpp"
#include "ores.refdata.api/domain/book_product_type_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/book_product_type_entity.hpp"
#include "ores.refdata.core/repository/book_product_type_mapper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <map>
#include <optional>
#include <set>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

namespace ores::refdata::repository {

namespace query_builder = ores::database::query_builder;
using namespace ores::database::query_builder::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string book_product_type_repository::sql() {
    return query_builder::insert_statement_sql<book_product_type_entity>(lg());
}

book_product_type_repository::book_product_type_repository(const context& ctx)
    : ctx_(ctx) {}

namespace {

/**
 * @brief The key of a link in the text form the store compares it in.
 */
std::pair<std::string, std::string> book_product_type_pair_of(const domain::book_product_type& v) {
    return {boost::uuids::to_string(v.book_id), v.product_type_code};
}

/**
 * @brief The claim a replace makes, from the rows the key names now: the
 * version the row carries, or that no row exists yet.
 */
ores::utility::domain::precondition
book_product_type_replace_claim_of(const std::vector<domain::book_product_type>& current) {
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

/**
 * @brief The object with the claim's version stamped onto it.
 *
 * The caller passes the rows the key names now whenever the claim is any.
 */
domain::book_product_type
book_product_type_apply_claim(const domain::book_product_type& v,
                              const ores::utility::domain::precondition& claim,
                              const std::vector<domain::book_product_type>& current) {
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

/**
 * @brief The current rows the key of an object names, or none.
 */
const std::vector<domain::book_product_type>&
book_product_type_rows_of(const book_product_type_repository::rows_by_pair& current,
                          const domain::book_product_type& v) {
    static const std::vector<domain::book_product_type> none;
    const auto found = current.find(book_product_type_pair_of(v));
    return found == current.end() ? none : found->second;
}

}

book_product_type_repository::rows_by_pair
book_product_type_repository::read_current(const std::vector<domain::book_product_type>& of) {
    rows_by_pair current;
    static constexpr auto max = db_timestamp::max();
    // Every key is a bind and the store's bind count is finite, so a larger
    // set is read one part at a time.
    constexpr std::size_t part_size = 1000;
    for (std::size_t begin = 0; begin < of.size(); begin += part_size) {
        const auto end = std::min(begin + part_size, of.size());
        std::set<std::pair<std::string, std::string>> requested;
        std::set<std::string> left_set;
        std::set<std::string> right_set;
        for (std::size_t i = begin; i < end; ++i) {
            auto pair = book_product_type_pair_of(of[i]);
            left_set.insert(pair.first);
            right_set.insert(pair.second);
            requested.insert(std::move(pair));
        }
        const std::vector<std::string> lefts(left_set.begin(), left_set.end());
        const std::vector<std::string> rights(right_set.begin(), right_set.end());
        const auto tid = ctx_.tenant_id().to_string();
        const auto query =
            query_builder::read<std::vector<book_product_type_entity>> |
            query_builder::where("tenant_id"_c == tid && "book_id"_c.in(lefts) &&
                                 "product_type_code"_c.in(rights) && "valid_to"_c == max);
        auto rows =
            query_builder::execute_read_query<book_product_type_entity, domain::book_product_type>(
                ctx_,
                query,
                [](const auto& entities) { return book_product_type_mapper::map(entities); },
                lg(),
                "Reading current allowed product types by key.");
        // The query matches every left with every right, so a row outside the
        // requested pairs is dropped here.
        for (auto& row : rows) {
            auto pair = book_product_type_pair_of(row);
            if (requested.contains(pair))
                current[std::move(pair)].push_back(std::move(row));
        }
    }
    return current;
}

ores::utility::domain::precondition
book_product_type_repository::replace_claim(const domain::book_product_type& v) {
    return book_product_type_replace_claim_of(read_latest(v.book_id, v.product_type_code));
}

void book_product_type_repository::write(const domain::book_product_type& book_product_type) {
    write(book_product_type, replace_claim(book_product_type));
}

void book_product_type_repository::write(
    const std::vector<domain::book_product_type>& book_product_types) {
    // One read for the set, so a batch states its claims from one read rather
    // than one read per row.
    const auto current = read_current(book_product_types);
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(book_product_types.size());
    for (const auto& item : book_product_types)
        claims.push_back(
            book_product_type_replace_claim_of(book_product_type_rows_of(current, item)));
    write(book_product_types, claims);
}

void book_product_type_repository::write(const domain::book_product_type& book_product_type,
                                         const ores::utility::domain::precondition& claim) {
    ORES_LOG_SEV(lg(), debug) << "Writing allowed product type to database: "
                              << book_product_type.book_id << "/"
                              << book_product_type.product_type_code;
    const auto current =
        claim.kind == ores::utility::domain::precondition_kind::any ?
            read_latest(book_product_type.book_id, book_product_type.product_type_code) :
            std::vector<domain::book_product_type>{};
    const auto t = book_product_type_apply_claim(book_product_type, claim, current);
    query_builder::execute_write_query(
        ctx_, book_product_type_mapper::map(t), lg(), "writing allowed product type to database");
}

void book_product_type_repository::write(
    const std::vector<domain::book_product_type>& book_product_types,
    const std::vector<ores::utility::domain::precondition>& claims) {
    ORES_LOG_SEV(lg(), debug) << "Writing allowed product types to database. Count: "
                              << book_product_types.size();
    if (claims.size() != book_product_types.size())
        throw std::invalid_argument(
            "book_product_type_repository::write: one claim is needed for each row");
    std::vector<domain::book_product_type> to_read;
    for (std::size_t i = 0; i < book_product_types.size(); ++i) {
        if (claims[i].kind == ores::utility::domain::precondition_kind::any)
            to_read.push_back(book_product_types[i]);
    }
    const auto current = read_current(to_read);
    std::vector<domain::book_product_type> batch;
    batch.reserve(book_product_types.size());
    for (std::size_t i = 0; i < book_product_types.size(); ++i)
        batch.push_back(book_product_type_apply_claim(
            book_product_types[i],
            claims[i],
            book_product_type_rows_of(current, book_product_types[i])));
    query_builder::execute_write_query(ctx_,
                                       book_product_type_mapper::map(batch),
                                       lg(),
                                       "writing allowed product types to database");
}

std::vector<domain::book_product_type> book_product_type_repository::read_latest() {
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<book_product_type_entity>> |
                       query_builder::where("tenant_id"_c == tid && "valid_to"_c == max) |
                       query_builder::order_by("book_id"_c, "product_type_code"_c);

    return query_builder::execute_read_query<book_product_type_entity, domain::book_product_type>(
        ctx_,
        query,
        [](const auto& entities) { return book_product_type_mapper::map(entities); },
        lg(),
        "Reading latest allowed product types");
}

std::vector<domain::book_product_type>
book_product_type_repository::read_latest(std::uint32_t offset, std::uint32_t limit) {
    ORES_LOG_SEV(lg(), debug) << "Reading latest allowed product types with offset: " << offset
                              << " and limit: " << limit;
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<book_product_type_entity>> |
                       query_builder::where("tenant_id"_c == tid && "valid_to"_c == max) |
                       query_builder::order_by("book_id"_c, "product_type_code"_c) |
                       query_builder::offset(offset) | query_builder::limit(limit);

    return query_builder::execute_read_query<book_product_type_entity, domain::book_product_type>(
        ctx_,
        query,
        [](const auto& entities) { return book_product_type_mapper::map(entities); },
        lg(),
        "Reading latest allowed product types (paginated).");
}

std::vector<domain::book_product_type>
book_product_type_repository::read_latest(const boost::uuids::uuid& book_id,
                                          const std::string& product_type_code) {
    ORES_LOG_SEV(lg(), debug) << "Reading latest allowed product type. " << book_id << "/"
                              << product_type_code;

    static constexpr auto max = db_timestamp::max();
    const auto book_id_str = boost::uuids::to_string(book_id);
    const auto product_type_code_str = product_type_code;
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<book_product_type_entity>> |
        query_builder::where("tenant_id"_c == tid && "book_id"_c == book_id_str &&
                             "product_type_code"_c == product_type_code && "valid_to"_c == max);

    return query_builder::execute_read_query<book_product_type_entity, domain::book_product_type>(
        ctx_,
        query,
        [](const auto& entities) { return book_product_type_mapper::map(entities); },
        lg(),
        "Reading latest allowed product type by key.");
}

std::uint32_t book_product_type_repository::get_total_book_product_type_count() {
    ORES_LOG_SEV(lg(), debug) << "Retrieving total active allowed product types count";
    static constexpr auto max = db_timestamp::max();

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<book_product_type_entity>> |
                       query_builder::where("tenant_id"_c == tid && "valid_to"_c == max);

    const auto count = query_builder::execute_count_query<book_product_type_entity>(
        ctx_, query, std::nullopt, lg(), "Counting allowed product types");
    ORES_LOG_SEV(lg(), debug) << "Total active allowed product types count: " << count;
    return count;
}

std::vector<domain::book_product_type>
book_product_type_repository::read_latest_by_book(const boost::uuids::uuid& book_id) {
    ORES_LOG_SEV(lg(), debug) << "Reading latest allowed product types. Book: " << book_id;

    static constexpr auto max = db_timestamp::max();
    const auto book_id_str = boost::uuids::to_string(book_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<book_product_type_entity>> |
                       query_builder::where("tenant_id"_c == tid && "book_id"_c == book_id_str &&
                                            "valid_to"_c == max) |
                       query_builder::order_by("product_type_code"_c);

    auto rows =
        query_builder::execute_read_query<book_product_type_entity, domain::book_product_type>(
            ctx_,
            query,
            [](const auto& entities) { return book_product_type_mapper::map(entities); },
            lg(),
            "Reading latest allowed product types by book.");

    return rows;
}

std::vector<domain::book_product_type>
book_product_type_repository::read_latest_by_product_type(const std::string& product_type_code) {
    ORES_LOG_SEV(lg(), debug) << "Reading latest allowed product types. Product Type: "
                              << product_type_code;

    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<book_product_type_entity>> |
        query_builder::where("tenant_id"_c == tid && "product_type_code"_c == product_type_code &&
                             "valid_to"_c == max) |
        query_builder::order_by("book_id"_c);

    auto rows =
        query_builder::execute_read_query<book_product_type_entity, domain::book_product_type>(
            ctx_,
            query,
            [](const auto& entities) { return book_product_type_mapper::map(entities); },
            lg(),
            "Reading latest allowed product types by product_type.");

    return rows;
}

std::vector<domain::book_product_type> book_product_type_repository::read_latest_by_book(
    const boost::uuids::uuid& book_id, std::uint32_t offset, std::uint32_t limit) {
    const auto book_id_str = boost::uuids::to_string(book_id);
    ORES_LOG_SEV(lg(), debug) << "Reading latest allowed product types. Book: " << book_id
                              << " offset: " << offset << " limit: " << limit;

    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<book_product_type_entity>> |
                       query_builder::where("tenant_id"_c == tid && "book_id"_c == book_id_str &&
                                            "valid_to"_c == max) |
                       query_builder::order_by("product_type_code"_c) |
                       query_builder::offset(offset) | query_builder::limit(limit);

    auto rows =
        query_builder::execute_read_query<book_product_type_entity, domain::book_product_type>(
            ctx_,
            query,
            [](const auto& entities) { return book_product_type_mapper::map(entities); },
            lg(),
            "Reading latest allowed product types by book (paginated).");

    return rows;
}

std::uint32_t book_product_type_repository::get_total_book_product_type_count_by_book(
    const boost::uuids::uuid& book_id) {
    const auto book_id_str = boost::uuids::to_string(book_id);
    ORES_LOG_SEV(lg(), debug) << "Retrieving total active allowed product types count. Book: "
                              << book_id;
    static constexpr auto max = db_timestamp::max();

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<book_product_type_entity>> |
                       query_builder::where("tenant_id"_c == tid && "book_id"_c == book_id_str &&
                                            "valid_to"_c == max);

    const auto count = query_builder::execute_count_query<book_product_type_entity>(
        ctx_, query, std::nullopt, lg(), "Counting allowed product types");
    ORES_LOG_SEV(lg(), debug) << "Total active allowed product types count by book: " << count;
    return count;
}

std::vector<domain::book_product_type> book_product_type_repository::read_latest_by_product_type(
    const std::string& product_type_code, std::uint32_t offset, std::uint32_t limit) {
    ORES_LOG_SEV(lg(), debug) << "Reading latest allowed product types. Product Type: "
                              << product_type_code << " offset: " << offset << " limit: " << limit;

    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<book_product_type_entity>> |
        query_builder::where("tenant_id"_c == tid && "product_type_code"_c == product_type_code &&
                             "valid_to"_c == max) |
        query_builder::order_by("book_id"_c) | query_builder::offset(offset) |
        query_builder::limit(limit);

    auto rows =
        query_builder::execute_read_query<book_product_type_entity, domain::book_product_type>(
            ctx_,
            query,
            [](const auto& entities) { return book_product_type_mapper::map(entities); },
            lg(),
            "Reading latest allowed product types by product_type (paginated).");

    return rows;
}

std::uint32_t book_product_type_repository::get_total_book_product_type_count_by_product_type(
    const std::string& product_type_code) {
    ORES_LOG_SEV(lg(), debug)
        << "Retrieving total active allowed product types count. Product Type: "
        << product_type_code;
    static constexpr auto max = db_timestamp::max();

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<book_product_type_entity>> |
        query_builder::where("tenant_id"_c == tid && "product_type_code"_c == product_type_code &&
                             "valid_to"_c == max);

    const auto count = query_builder::execute_count_query<book_product_type_entity>(
        ctx_, query, std::nullopt, lg(), "Counting allowed product types");
    ORES_LOG_SEV(lg(), debug) << "Total active allowed product types count by product_type: "
                              << count;
    return count;
}

std::vector<book_product_type_history_row>
book_product_type_repository::read_all_by_book(const boost::uuids::uuid& book_id) {
    ORES_LOG_SEV(lg(), debug) << "Reading every version of the allowed product types. Book: "
                              << book_id;

    const auto book_id_str = boost::uuids::to_string(book_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<book_product_type_entity>> |
                       query_builder::where("tenant_id"_c == tid && "book_id"_c == book_id_str) |
                       query_builder::order_by("product_type_code"_c, "valid_from"_c.desc());

    return query_builder::execute_read_query<book_product_type_entity,
                                             book_product_type_history_row>(
        ctx_,
        query,
        [](const auto& entities) {
            std::vector<book_product_type_history_row> rows;
            rows.reserve(entities.size());
            for (const auto& entity : entities) {
                book_product_type_history_row row;
                row.link = book_product_type_mapper::map(entity);
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
        "Reading every version of the allowed product types by book.");
}

void book_product_type_repository::remove(const boost::uuids::uuid& book_id,
                                          const std::string& product_type_code) {
    static_cast<void>(remove(book_id, product_type_code, std::nullopt));
}

book_product_type_repository::remove_status
book_product_type_repository::remove(const boost::uuids::uuid& book_id,
                                     const std::string& product_type_code,
                                     std::optional<std::uint32_t> version) {
    ORES_LOG_SEV(lg(), debug) << "Removing allowed product type from database: " << book_id << "/"
                              << product_type_code;

    const auto current = read_latest(book_id, product_type_code);
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
    const auto book_id_str = boost::uuids::to_string(book_id);
    const auto product_type_code_str = product_type_code;
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::delete_from<book_product_type_entity> |
                       query_builder::where("tenant_id"_c == tid && "book_id"_c == book_id_str &&
                                            "product_type_code"_c == product_type_code &&
                                            "valid_to"_c == max && "version"_c == expected);

    query_builder::execute_delete_query(
        ctx_, query, lg(), "removing allowed product type from database");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(book_id, product_type_code).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void book_product_type_repository::remove(const std::vector<boost::uuids::uuid>& book_ids,
                                          const std::vector<std::string>& product_type_codes) {
    // A junction's key is the pair of columns, and a per-column .in() DELETE
    // would be a cross-product over-delete (rows outside the requested pairs),
    // so each pair is removed on its own.
    if (book_ids.size() != product_type_codes.size())
        throw std::invalid_argument(
            "book_product_type_repository::remove: key column vectors must be the same length");
    for (std::size_t i = 0; i < book_ids.size(); ++i)
        static_cast<void>(remove(book_ids[i], product_type_codes[i], std::nullopt));
}

void book_product_type_repository::remove_by_book(const boost::uuids::uuid& book_id) {
    ORES_LOG_SEV(lg(), debug) << "Removing all allowed product types from database: " << book_id;

    const auto book_id_str = boost::uuids::to_string(book_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::delete_from<book_product_type_entity> |
                       query_builder::where("tenant_id"_c == tid && "book_id"_c == book_id_str);

    query_builder::execute_delete_query(
        ctx_, query, lg(), "removing all allowed product types from database");
}


}
