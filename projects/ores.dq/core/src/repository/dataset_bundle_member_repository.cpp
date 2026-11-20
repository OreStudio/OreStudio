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
#include "ores.dq.core/repository/dataset_bundle_member_repository.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.dq.api/domain/dataset_bundle_member.hpp"
#include "ores.dq.api/domain/dataset_bundle_member_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq.core/repository/dataset_bundle_member_entity.hpp"
#include "ores.dq.core/repository/dataset_bundle_member_mapper.hpp"
#include "ores.utility/domain/protocol.hpp"
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

namespace ores::dq::repository {

namespace query_builder = ores::database::query_builder;
using namespace ores::database::query_builder::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string dataset_bundle_member_repository::sql() {
    return query_builder::insert_statement_sql<dataset_bundle_member_entity>(lg());
}

dataset_bundle_member_repository::dataset_bundle_member_repository(const context& ctx)
    : ctx_(ctx) {}

namespace {

/**
 * @brief The key of a link in the text form the store compares it in.
 */
std::pair<std::string, std::string>
dataset_bundle_member_pair_of(const domain::dataset_bundle_member& v) {
    return {v.bundle_code, v.dataset_code};
}

/**
 * @brief The claim a replace makes, from the rows the key names now: the
 * version the row carries, or that no row exists yet.
 */
ores::utility::domain::precondition
dataset_bundle_member_replace_claim_of(const std::vector<domain::dataset_bundle_member>& current) {
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
domain::dataset_bundle_member
dataset_bundle_member_apply_claim(const domain::dataset_bundle_member& v,
                                  const ores::utility::domain::precondition& claim,
                                  const std::vector<domain::dataset_bundle_member>& current) {
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
const std::vector<domain::dataset_bundle_member>&
dataset_bundle_member_rows_of(const dataset_bundle_member_repository::rows_by_pair& current,
                              const domain::dataset_bundle_member& v) {
    static const std::vector<domain::dataset_bundle_member> none;
    const auto found = current.find(dataset_bundle_member_pair_of(v));
    return found == current.end() ? none : found->second;
}

}

dataset_bundle_member_repository::rows_by_pair dataset_bundle_member_repository::read_current(
    const std::vector<domain::dataset_bundle_member>& of) {
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
            auto pair = dataset_bundle_member_pair_of(of[i]);
            left_set.insert(pair.first);
            right_set.insert(pair.second);
            requested.insert(std::move(pair));
        }
        const std::vector<std::string> lefts(left_set.begin(), left_set.end());
        const std::vector<std::string> rights(right_set.begin(), right_set.end());
        const auto tid = ctx_.tenant_id().to_string();
        const auto query = query_builder::read<std::vector<dataset_bundle_member_entity>> |
                           query_builder::where("tenant_id"_c == tid && "bundle_code"_c.in(lefts) &&
                                                "dataset_code"_c.in(rights) && "valid_to"_c == max);
        auto rows = query_builder::execute_read_query<dataset_bundle_member_entity,
                                                      domain::dataset_bundle_member>(
            ctx_,
            query,
            [](const auto& entities) { return dataset_bundle_member_mapper::map(entities); },
            lg(),
            "Reading current dataset bundle members by key.");
        // The query matches every left with every right, so a row outside the
        // requested pairs is dropped here.
        for (auto& row : rows) {
            auto pair = dataset_bundle_member_pair_of(row);
            if (requested.contains(pair))
                current[std::move(pair)].push_back(std::move(row));
        }
    }
    return current;
}

ores::utility::domain::precondition
dataset_bundle_member_repository::replace_claim(const domain::dataset_bundle_member& v) {
    return dataset_bundle_member_replace_claim_of(read_latest(v.bundle_code, v.dataset_code));
}

void dataset_bundle_member_repository::write(const domain::dataset_bundle_member& member) {
    write(member, replace_claim(member));
}

void dataset_bundle_member_repository::write(
    const std::vector<domain::dataset_bundle_member>& members) {
    // One read for the set, so a batch states its claims from one read rather
    // than one read per row.
    const auto current = read_current(members);
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(members.size());
    for (const auto& item : members)
        claims.push_back(
            dataset_bundle_member_replace_claim_of(dataset_bundle_member_rows_of(current, item)));
    write(members, claims);
}

void dataset_bundle_member_repository::write(const domain::dataset_bundle_member& member,
                                             const ores::utility::domain::precondition& claim) {
    ORES_LOG_SEV(lg(), debug) << "Writing dataset bundle member to database: " << member.bundle_code
                              << "/" << member.dataset_code;
    const auto current = claim.kind == ores::utility::domain::precondition_kind::any ?
                             read_latest(member.bundle_code, member.dataset_code) :
                             std::vector<domain::dataset_bundle_member>{};
    const auto t = dataset_bundle_member_apply_claim(member, claim, current);
    query_builder::execute_write_query(ctx_,
                                       dataset_bundle_member_mapper::map(t),
                                       lg(),
                                       "writing dataset bundle member to database");
}

void dataset_bundle_member_repository::write(
    const std::vector<domain::dataset_bundle_member>& members,
    const std::vector<ores::utility::domain::precondition>& claims) {
    ORES_LOG_SEV(lg(), debug) << "Writing dataset bundle members to database. Count: "
                              << members.size();
    if (claims.size() != members.size())
        throw std::invalid_argument(
            "dataset_bundle_member_repository::write: one claim is needed for each row");
    std::vector<domain::dataset_bundle_member> to_read;
    for (std::size_t i = 0; i < members.size(); ++i) {
        if (claims[i].kind == ores::utility::domain::precondition_kind::any)
            to_read.push_back(members[i]);
    }
    const auto current = read_current(to_read);
    std::vector<domain::dataset_bundle_member> batch;
    batch.reserve(members.size());
    for (std::size_t i = 0; i < members.size(); ++i)
        batch.push_back(dataset_bundle_member_apply_claim(
            members[i], claims[i], dataset_bundle_member_rows_of(current, members[i])));
    query_builder::execute_write_query(ctx_,
                                       dataset_bundle_member_mapper::map(batch),
                                       lg(),
                                       "writing dataset bundle members to database");
}

std::vector<domain::dataset_bundle_member> dataset_bundle_member_repository::read_latest() {
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<dataset_bundle_member_entity>> |
                       query_builder::where("tenant_id"_c == tid && "valid_to"_c == max) |
                       query_builder::order_by("bundle_code"_c, "display_order"_c);

    return query_builder::execute_read_query<dataset_bundle_member_entity,
                                             domain::dataset_bundle_member>(
        ctx_,
        query,
        [](const auto& entities) { return dataset_bundle_member_mapper::map(entities); },
        lg(),
        "Reading latest dataset bundle members");
}

std::vector<domain::dataset_bundle_member>
dataset_bundle_member_repository::read_latest(std::uint32_t offset, std::uint32_t limit) {
    ORES_LOG_SEV(lg(), debug) << "Reading latest dataset bundle members with offset: " << offset
                              << " and limit: " << limit;
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<dataset_bundle_member_entity>> |
                       query_builder::where("tenant_id"_c == tid && "valid_to"_c == max) |
                       query_builder::order_by("bundle_code"_c, "display_order"_c) |
                       query_builder::offset(offset) | query_builder::limit(limit);

    return query_builder::execute_read_query<dataset_bundle_member_entity,
                                             domain::dataset_bundle_member>(
        ctx_,
        query,
        [](const auto& entities) { return dataset_bundle_member_mapper::map(entities); },
        lg(),
        "Reading latest dataset bundle members (paginated).");
}

std::vector<domain::dataset_bundle_member>
dataset_bundle_member_repository::read_latest(const std::string& bundle_code,
                                              const std::string& dataset_code) {
    ORES_LOG_SEV(lg(), debug) << "Reading latest dataset bundle member. " << bundle_code << "/"
                              << dataset_code;

    static constexpr auto max = db_timestamp::max();
    const auto bundle_code_str = bundle_code;
    const auto dataset_code_str = dataset_code;
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<dataset_bundle_member_entity>> |
        query_builder::where("tenant_id"_c == tid && "bundle_code"_c == bundle_code &&
                             "dataset_code"_c == dataset_code && "valid_to"_c == max);

    return query_builder::execute_read_query<dataset_bundle_member_entity,
                                             domain::dataset_bundle_member>(
        ctx_,
        query,
        [](const auto& entities) { return dataset_bundle_member_mapper::map(entities); },
        lg(),
        "Reading latest dataset bundle member by key.");
}

std::uint32_t dataset_bundle_member_repository::get_total_member_count() {
    ORES_LOG_SEV(lg(), debug) << "Retrieving total active dataset bundle members count";
    static constexpr auto max = db_timestamp::max();

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<dataset_bundle_member_entity>> |
                       query_builder::where("tenant_id"_c == tid && "valid_to"_c == max);

    const auto count = query_builder::execute_count_query<dataset_bundle_member_entity>(
        ctx_, query, std::nullopt, lg(), "Counting dataset bundle members");
    ORES_LOG_SEV(lg(), debug) << "Total active dataset bundle members count: " << count;
    return count;
}

std::vector<domain::dataset_bundle_member>
dataset_bundle_member_repository::read_latest_by_bundle(const std::string& bundle_code) {
    ORES_LOG_SEV(lg(), debug) << "Reading latest dataset bundle members. Bundle: " << bundle_code;

    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<dataset_bundle_member_entity>> |
                       query_builder::where("tenant_id"_c == tid &&
                                            "bundle_code"_c == bundle_code && "valid_to"_c == max) |
                       query_builder::order_by("display_order"_c);

    auto rows = query_builder::execute_read_query<dataset_bundle_member_entity,
                                                  domain::dataset_bundle_member>(
        ctx_,
        query,
        [](const auto& entities) { return dataset_bundle_member_mapper::map(entities); },
        lg(),
        "Reading latest dataset bundle members by bundle.");

    return rows;
}

std::vector<domain::dataset_bundle_member>
dataset_bundle_member_repository::read_latest_by_dataset(const std::string& dataset_code) {
    ORES_LOG_SEV(lg(), debug) << "Reading latest dataset bundle members. Dataset: " << dataset_code;

    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<dataset_bundle_member_entity>> |
        query_builder::where("tenant_id"_c == tid && "dataset_code"_c == dataset_code &&
                             "valid_to"_c == max) |
        query_builder::order_by("bundle_code"_c);

    auto rows = query_builder::execute_read_query<dataset_bundle_member_entity,
                                                  domain::dataset_bundle_member>(
        ctx_,
        query,
        [](const auto& entities) { return dataset_bundle_member_mapper::map(entities); },
        lg(),
        "Reading latest dataset bundle members by dataset.");

    return rows;
}

std::vector<domain::dataset_bundle_member> dataset_bundle_member_repository::read_latest_by_bundle(
    const std::string& bundle_code, std::uint32_t offset, std::uint32_t limit) {
    ORES_LOG_SEV(lg(), debug) << "Reading latest dataset bundle members. Bundle: " << bundle_code
                              << " offset: " << offset << " limit: " << limit;

    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<dataset_bundle_member_entity>> |
                       query_builder::where("tenant_id"_c == tid &&
                                            "bundle_code"_c == bundle_code && "valid_to"_c == max) |
                       query_builder::order_by("display_order"_c) | query_builder::offset(offset) |
                       query_builder::limit(limit);

    auto rows = query_builder::execute_read_query<dataset_bundle_member_entity,
                                                  domain::dataset_bundle_member>(
        ctx_,
        query,
        [](const auto& entities) { return dataset_bundle_member_mapper::map(entities); },
        lg(),
        "Reading latest dataset bundle members by bundle (paginated).");

    return rows;
}

std::uint32_t
dataset_bundle_member_repository::get_total_member_count_by_bundle(const std::string& bundle_code) {
    ORES_LOG_SEV(lg(), debug) << "Retrieving total active dataset bundle members count. Bundle: "
                              << bundle_code;
    static constexpr auto max = db_timestamp::max();

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<dataset_bundle_member_entity>> |
                       query_builder::where("tenant_id"_c == tid &&
                                            "bundle_code"_c == bundle_code && "valid_to"_c == max);

    const auto count = query_builder::execute_count_query<dataset_bundle_member_entity>(
        ctx_, query, std::nullopt, lg(), "Counting dataset bundle members");
    ORES_LOG_SEV(lg(), debug) << "Total active dataset bundle members count by bundle: " << count;
    return count;
}

std::uint32_t dataset_bundle_member_repository::get_total_member_count_by_dataset(
    const std::string& dataset_code) {
    ORES_LOG_SEV(lg(), debug) << "Retrieving total active dataset bundle members count. Dataset: "
                              << dataset_code;
    static constexpr auto max = db_timestamp::max();

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<dataset_bundle_member_entity>> |
        query_builder::where("tenant_id"_c == tid && "dataset_code"_c == dataset_code &&
                             "valid_to"_c == max);

    const auto count = query_builder::execute_count_query<dataset_bundle_member_entity>(
        ctx_, query, std::nullopt, lg(), "Counting dataset bundle members");
    ORES_LOG_SEV(lg(), debug) << "Total active dataset bundle members count by dataset: " << count;
    return count;
}

std::vector<dataset_bundle_member_history_row>
dataset_bundle_member_repository::read_all_by_bundle(const std::string& bundle_code) {
    ORES_LOG_SEV(lg(), debug) << "Reading every version of the dataset bundle members. Bundle: "
                              << bundle_code;

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<dataset_bundle_member_entity>> |
        query_builder::where("tenant_id"_c == tid && "bundle_code"_c == bundle_code) |
        query_builder::order_by("dataset_code"_c, "valid_from"_c.desc());

    return query_builder::execute_read_query<dataset_bundle_member_entity,
                                             dataset_bundle_member_history_row>(
        ctx_,
        query,
        [](const auto& entities) {
            std::vector<dataset_bundle_member_history_row> rows;
            rows.reserve(entities.size());
            for (const auto& entity : entities) {
                dataset_bundle_member_history_row row;
                row.link = dataset_bundle_member_mapper::map(entity);
                row.modified_by = row.link.modified_by;
                row.recorded_at = row.link.recorded_at;
                // A version ends when a later version replaces it, which is not
                // a closing, or when the link is closed. The rows of one link run
                // newest first, so the row before this one, when it is the same
                // link, is the next newer version, and it replaced this one only
                // if it carries the next version number. A link opened again
                // after a closing starts its numbering over.
                const bool replaced = !rows.empty() &&
                                      rows.back().link.dataset_code == row.link.dataset_code &&
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
        "Reading every version of the dataset bundle members by bundle.");
}

void dataset_bundle_member_repository::remove(const std::string& bundle_code,
                                              const std::string& dataset_code) {
    static_cast<void>(remove(bundle_code, dataset_code, std::nullopt));
}

dataset_bundle_member_repository::remove_status
dataset_bundle_member_repository::remove(const std::string& bundle_code,
                                         const std::string& dataset_code,
                                         std::optional<std::uint32_t> version) {
    ORES_LOG_SEV(lg(), debug) << "Removing dataset bundle member from database: " << bundle_code
                              << "/" << dataset_code;

    const auto current = read_latest(bundle_code, dataset_code);
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
    const auto bundle_code_str = bundle_code;
    const auto dataset_code_str = dataset_code;
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::delete_from<dataset_bundle_member_entity> |
        query_builder::where("tenant_id"_c == tid && "bundle_code"_c == bundle_code &&
                             "dataset_code"_c == dataset_code && "valid_to"_c == max &&
                             "version"_c == expected);

    query_builder::execute_delete_query(
        ctx_, query, lg(), "removing dataset bundle member from database");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(bundle_code, dataset_code).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void dataset_bundle_member_repository::remove(const std::vector<std::string>& bundle_codes,
                                              const std::vector<std::string>& dataset_codes) {
    // A junction's key is the pair of columns, and a per-column .in() DELETE
    // would be a cross-product over-delete (rows outside the requested pairs),
    // so each pair is removed on its own.
    if (bundle_codes.size() != dataset_codes.size())
        throw std::invalid_argument(
            "dataset_bundle_member_repository::remove: key column vectors must be the same length");
    for (std::size_t i = 0; i < bundle_codes.size(); ++i)
        static_cast<void>(remove(bundle_codes[i], dataset_codes[i], std::nullopt));
}

void dataset_bundle_member_repository::remove_by_bundle(const std::string& bundle_code) {
    ORES_LOG_SEV(lg(), debug) << "Removing all dataset bundle members from database: "
                              << bundle_code;

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::delete_from<dataset_bundle_member_entity> |
                       query_builder::where("tenant_id"_c == tid && "bundle_code"_c == bundle_code);

    query_builder::execute_delete_query(
        ctx_, query, lg(), "removing all dataset bundle members from database");
}


}
