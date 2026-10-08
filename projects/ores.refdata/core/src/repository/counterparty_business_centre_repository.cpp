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
#include "ores.refdata.core/repository/counterparty_business_centre_repository.hpp"
#include "ores.database/domain/tenant_aware_pool.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.refdata.api/domain/counterparty_business_centre.hpp"
#include "ores.refdata.api/domain/counterparty_business_centre_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/counterparty_business_centre_entity.hpp"
#include "ores.refdata.core/repository/counterparty_business_centre_mapper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <sqlgen/aggregations.hpp>
#include <sqlgen/delete_from.hpp>
#include <sqlgen/limit.hpp>
#include <sqlgen/literals.hpp>
#include <sqlgen/offset.hpp>
#include <sqlgen/order_by.hpp>
#include <sqlgen/read.hpp>
#include <sqlgen/select_from.hpp>
#include <sqlgen/to.hpp>
#include <sqlgen/where.hpp>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

namespace ores::refdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string counterparty_business_centre_repository::sql() {
    return generate_create_table_sql<counterparty_business_centre_entity>(lg());
}

counterparty_business_centre_repository::counterparty_business_centre_repository(context ctx)
    : ctx_(std::move(ctx)) {}

ores::utility::domain::precondition counterparty_business_centre_repository::replace_claim(
    const domain::counterparty_business_centre& v) {
    const auto current = read_latest(v.counterparty_id, v.business_centre_code);
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::counterparty_business_centre counterparty_business_centre_repository::apply_claim(
    const domain::counterparty_business_centre& v,
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
            const auto current = read_latest(v.counterparty_id, v.business_centre_code);
            t.version = current.empty() ? 0 : current.front().version;
            break;
        }
    }
    return t;
}

void counterparty_business_centre_repository::write(
    const domain::counterparty_business_centre& counterparty_business_centre) {
    write(counterparty_business_centre, replace_claim(counterparty_business_centre));
}

void counterparty_business_centre_repository::write(
    const std::vector<domain::counterparty_business_centre>& counterparty_business_centres) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(counterparty_business_centres.size());
    for (const auto& item : counterparty_business_centres)
        claims.push_back(replace_claim(item));
    write(counterparty_business_centres, claims);
}

void counterparty_business_centre_repository::write(
    const domain::counterparty_business_centre& counterparty_business_centre,
    const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing counterparty business centre to database: "
                               << counterparty_business_centre.counterparty_id << "/"
                               << counterparty_business_centre.business_centre_code;
    const auto t = apply_claim(counterparty_business_centre, claim);
    execute_write_query(ctx_,
                        counterparty_business_centre_mapper::map(t),
                        lg(),
                        "writing counterparty business centre to database");
}

void counterparty_business_centre_repository::write(
    const std::vector<domain::counterparty_business_centre>& counterparty_business_centres,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing counterparty business centres to database. Count: "
                               << counterparty_business_centres.size();
    std::vector<domain::counterparty_business_centre> batch;
    batch.reserve(counterparty_business_centres.size());
    for (std::size_t i = 0; i < counterparty_business_centres.size(); ++i)
        batch.push_back(apply_claim(counterparty_business_centres[i], claims[i]));
    execute_write_query(ctx_,
                        counterparty_business_centre_mapper::map(batch),
                        lg(),
                        "writing counterparty business centres to database");
}

std::vector<domain::counterparty_business_centre>
counterparty_business_centre_repository::read_latest() {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<counterparty_business_centre_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("counterparty_id"_c, "business_centre_code"_c);

    return execute_read_query<counterparty_business_centre_entity,
                              domain::counterparty_business_centre>(
        ctx_,
        query,
        [](const auto& entities) { return counterparty_business_centre_mapper::map(entities); },
        lg(),
        "Reading latest counterparty business centres");
}

std::vector<domain::counterparty_business_centre>
counterparty_business_centre_repository::read_latest(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest counterparty business centres with offset: "
                               << offset << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<counterparty_business_centre_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("counterparty_id"_c, "business_centre_code"_c) |
                       sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<counterparty_business_centre_entity,
                              domain::counterparty_business_centre>(
        ctx_,
        query,
        [](const auto& entities) { return counterparty_business_centre_mapper::map(entities); },
        lg(),
        "Reading latest counterparty business centres (paginated).");
}

std::vector<domain::counterparty_business_centre>
counterparty_business_centre_repository::read_latest(const boost::uuids::uuid& counterparty_id,
                                                     const std::string& business_centre_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest counterparty business centre. " << counterparty_id
                               << "/" << business_centre_code;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto counterparty_id_str = boost::uuids::to_string(counterparty_id);
    const auto business_centre_code_str = business_centre_code;
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<counterparty_business_centre_entity>> |
        where("tenant_id"_c == tid && "counterparty_id"_c == counterparty_id_str &&
              "business_centre_code"_c == business_centre_code && "valid_to"_c == max.value());

    return execute_read_query<counterparty_business_centre_entity,
                              domain::counterparty_business_centre>(
        ctx_,
        query,
        [](const auto& entities) { return counterparty_business_centre_mapper::map(entities); },
        lg(),
        "Reading latest counterparty business centre by key.");
}

std::uint32_t
counterparty_business_centre_repository::get_total_counterparty_business_centre_count() {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active counterparty business centres count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<counterparty_business_centre_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) | sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active counterparty business centres count: " << count;
    return count;
}

std::vector<domain::counterparty_business_centre>
counterparty_business_centre_repository::read_latest_by_counterparty(
    const boost::uuids::uuid& counterparty_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest counterparty business centres. Counterparty: "
                               << counterparty_id;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto counterparty_id_str = boost::uuids::to_string(counterparty_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<counterparty_business_centre_entity>> |
                       where("tenant_id"_c == tid && "counterparty_id"_c == counterparty_id_str &&
                             "valid_to"_c == max.value()) |
                       order_by("business_centre_code"_c);

    auto rows = execute_read_query<counterparty_business_centre_entity,
                                   domain::counterparty_business_centre>(
        ctx_,
        query,
        [](const auto& entities) { return counterparty_business_centre_mapper::map(entities); },
        lg(),
        "Reading latest counterparty business centres by counterparty.");

    return rows;
}

std::vector<domain::counterparty_business_centre>
counterparty_business_centre_repository::read_latest_by_business_centre(
    const std::string& business_centre_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest counterparty business centres. Business Centre: "
                               << business_centre_code;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<counterparty_business_centre_entity>> |
        where("tenant_id"_c == tid && "business_centre_code"_c == business_centre_code &&
              "valid_to"_c == max.value()) |
        order_by("counterparty_id"_c);

    auto rows = execute_read_query<counterparty_business_centre_entity,
                                   domain::counterparty_business_centre>(
        ctx_,
        query,
        [](const auto& entities) { return counterparty_business_centre_mapper::map(entities); },
        lg(),
        "Reading latest counterparty business centres by business_centre.");

    return rows;
}

std::vector<domain::counterparty_business_centre>
counterparty_business_centre_repository::read_latest_by_counterparty(
    const boost::uuids::uuid& counterparty_id, std::uint32_t offset, std::uint32_t limit) {
    const auto counterparty_id_str = boost::uuids::to_string(counterparty_id);
    BOOST_LOG_SEV(lg(), debug) << "Reading latest counterparty business centres. Counterparty: "
                               << counterparty_id << " offset: " << offset << " limit: " << limit;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<counterparty_business_centre_entity>> |
                       where("tenant_id"_c == tid && "counterparty_id"_c == counterparty_id_str &&
                             "valid_to"_c == max.value()) |
                       order_by("business_centre_code"_c) | sqlgen::offset(offset) |
                       sqlgen::limit(limit);

    auto rows = execute_read_query<counterparty_business_centre_entity,
                                   domain::counterparty_business_centre>(
        ctx_,
        query,
        [](const auto& entities) { return counterparty_business_centre_mapper::map(entities); },
        lg(),
        "Reading latest counterparty business centres by counterparty (paginated).");

    return rows;
}

std::uint32_t counterparty_business_centre_repository::
    get_total_counterparty_business_centre_count_by_counterparty(
        const boost::uuids::uuid& counterparty_id) {
    const auto counterparty_id_str = boost::uuids::to_string(counterparty_id);
    BOOST_LOG_SEV(lg(), debug)
        << "Retrieving total active counterparty business centres count. Counterparty: "
        << counterparty_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<counterparty_business_centre_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "counterparty_id"_c == counterparty_id_str &&
              "valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug)
        << "Total active counterparty business centres count by counterparty: " << count;
    return count;
}

std::uint32_t counterparty_business_centre_repository::
    get_total_counterparty_business_centre_count_by_business_centre(
        const std::string& business_centre_code) {
    BOOST_LOG_SEV(lg(), debug)
        << "Retrieving total active counterparty business centres count. Business Centre: "
        << business_centre_code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<counterparty_business_centre_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "business_centre_code"_c == business_centre_code &&
              "valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug)
        << "Total active counterparty business centres count by business_centre: " << count;
    return count;
}

void counterparty_business_centre_repository::remove(const boost::uuids::uuid& counterparty_id,
                                                     const std::string& business_centre_code) {
    static_cast<void>(remove(counterparty_id, business_centre_code, std::nullopt));
}

counterparty_business_centre_repository::remove_status
counterparty_business_centre_repository::remove(const boost::uuids::uuid& counterparty_id,
                                                const std::string& business_centre_code,
                                                std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing counterparty business centre from database: "
                               << counterparty_id << "/" << business_centre_code;

    const auto current = read_latest(counterparty_id, business_centre_code);
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
    const auto counterparty_id_str = boost::uuids::to_string(counterparty_id);
    const auto business_centre_code_str = business_centre_code;
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::delete_from<counterparty_business_centre_entity> |
                       where("tenant_id"_c == tid && "counterparty_id"_c == counterparty_id_str &&
                             "business_centre_code"_c == business_centre_code &&
                             "valid_to"_c == max.value() && "version"_c == expected);

    execute_delete_query(ctx_, query, lg(), "removing counterparty business centre from database");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(counterparty_id, business_centre_code).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void counterparty_business_centre_repository::remove(
    const std::vector<boost::uuids::uuid>& counterparty_ids,
    const std::vector<std::string>& business_centre_codes) {
    // A junction's key is the pair of columns, and a per-column .in() DELETE
    // would be a cross-product over-delete (rows outside the requested pairs),
    // so each pair is removed on its own.
    if (counterparty_ids.size() != business_centre_codes.size())
        throw std::invalid_argument("counterparty_business_centre_repository::remove: key column "
                                    "vectors must be the same length");
    for (std::size_t i = 0; i < counterparty_ids.size(); ++i)
        static_cast<void>(remove(counterparty_ids[i], business_centre_codes[i], std::nullopt));
}

void counterparty_business_centre_repository::remove_by_counterparty(
    const boost::uuids::uuid& counterparty_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing all counterparty business centres from database: "
                               << counterparty_id;

    const auto counterparty_id_str = boost::uuids::to_string(counterparty_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::delete_from<counterparty_business_centre_entity> |
                       where("tenant_id"_c == tid && "counterparty_id"_c == counterparty_id_str);

    execute_delete_query(
        ctx_, query, lg(), "removing all counterparty business centres from database");
}


}
