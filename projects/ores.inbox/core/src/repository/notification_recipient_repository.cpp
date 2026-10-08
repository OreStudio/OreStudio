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
#include "ores.inbox.core/repository/notification_recipient_repository.hpp"
#include "ores.database/domain/tenant_aware_pool.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.inbox.api/domain/notification_recipient.hpp"
#include "ores.inbox.api/domain/notification_recipient_json_io.hpp" // IWYU pragma: keep.
#include "ores.inbox.core/repository/notification_recipient_entity.hpp"
#include "ores.inbox.core/repository/notification_recipient_mapper.hpp"
#include "ores.logging/boost_severity.hpp"
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

namespace ores::inbox::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string notification_recipient_repository::sql() {
    return generate_create_table_sql<notification_recipient_entity>(lg());
}

notification_recipient_repository::notification_recipient_repository(context ctx)
    : ctx_(std::move(ctx)) {}

ores::utility::domain::precondition
notification_recipient_repository::replace_claim(const domain::notification_recipient& v) {
    const auto current = read_latest(v.notification_id, v.account_id);
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::notification_recipient
notification_recipient_repository::apply_claim(const domain::notification_recipient& v,
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
            const auto current = read_latest(v.notification_id, v.account_id);
            t.version = current.empty() ? 0 : current.front().version;
            break;
        }
    }
    return t;
}

void notification_recipient_repository::write(const domain::notification_recipient& recipient) {
    write(recipient, replace_claim(recipient));
}

void notification_recipient_repository::write(
    const std::vector<domain::notification_recipient>& recipients) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(recipients.size());
    for (const auto& item : recipients)
        claims.push_back(replace_claim(item));
    write(recipients, claims);
}

void notification_recipient_repository::write(const domain::notification_recipient& recipient,
                                              const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing notification recipient to database: "
                               << recipient.notification_id << "/" << recipient.account_id;
    const auto t = apply_claim(recipient, claim);
    execute_write_query(ctx_,
                        notification_recipient_mapper::map(t),
                        lg(),
                        "writing notification recipient to database");
}

void notification_recipient_repository::write(
    const std::vector<domain::notification_recipient>& recipients,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing notification recipients to database. Count: "
                               << recipients.size();
    std::vector<domain::notification_recipient> batch;
    batch.reserve(recipients.size());
    for (std::size_t i = 0; i < recipients.size(); ++i)
        batch.push_back(apply_claim(recipients[i], claims[i]));
    execute_write_query(ctx_,
                        notification_recipient_mapper::map(batch),
                        lg(),
                        "writing notification recipients to database");
}

std::vector<domain::notification_recipient> notification_recipient_repository::read_latest() {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<notification_recipient_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("notification_id"_c, "notification_id"_c);

    return execute_read_query<notification_recipient_entity, domain::notification_recipient>(
        ctx_,
        query,
        [](const auto& entities) { return notification_recipient_mapper::map(entities); },
        lg(),
        "Reading latest notification recipients");
}

std::vector<domain::notification_recipient>
notification_recipient_repository::read_latest(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest notification recipients with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<notification_recipient_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("notification_id"_c, "notification_id"_c) | sqlgen::offset(offset) |
                       sqlgen::limit(limit);

    return execute_read_query<notification_recipient_entity, domain::notification_recipient>(
        ctx_,
        query,
        [](const auto& entities) { return notification_recipient_mapper::map(entities); },
        lg(),
        "Reading latest notification recipients (paginated).");
}

std::vector<domain::notification_recipient>
notification_recipient_repository::read_latest(const boost::uuids::uuid& notification_id,
                                               const boost::uuids::uuid& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest notification recipient. " << notification_id
                               << "/" << account_id;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto notification_id_str = boost::uuids::to_string(notification_id);
    const auto account_id_str = boost::uuids::to_string(account_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<notification_recipient_entity>> |
                       where("tenant_id"_c == tid && "notification_id"_c == notification_id_str &&
                             "account_id"_c == account_id_str && "valid_to"_c == max.value());

    return execute_read_query<notification_recipient_entity, domain::notification_recipient>(
        ctx_,
        query,
        [](const auto& entities) { return notification_recipient_mapper::map(entities); },
        lg(),
        "Reading latest notification recipient by key.");
}

std::uint32_t notification_recipient_repository::get_total_recipient_count() {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active notification recipients count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<notification_recipient_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) | sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active notification recipients count: " << count;
    return count;
}

std::vector<domain::notification_recipient>
notification_recipient_repository::read_latest_by_notification(
    const boost::uuids::uuid& notification_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest notification recipients. Notification: "
                               << notification_id;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto notification_id_str = boost::uuids::to_string(notification_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<notification_recipient_entity>> |
                       where("tenant_id"_c == tid && "notification_id"_c == notification_id_str &&
                             "valid_to"_c == max.value()) |
                       order_by("notification_id"_c);

    auto rows = execute_read_query<notification_recipient_entity, domain::notification_recipient>(
        ctx_,
        query,
        [](const auto& entities) { return notification_recipient_mapper::map(entities); },
        lg(),
        "Reading latest notification recipients by notification.");

    return rows;
}

std::vector<domain::notification_recipient>
notification_recipient_repository::read_latest_by_account(const boost::uuids::uuid& account_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest notification recipients. Account: " << account_id;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto account_id_str = boost::uuids::to_string(account_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<notification_recipient_entity>> |
                       where("tenant_id"_c == tid && "account_id"_c == account_id_str &&
                             "valid_to"_c == max.value()) |
                       order_by("notification_id"_c);

    auto rows = execute_read_query<notification_recipient_entity, domain::notification_recipient>(
        ctx_,
        query,
        [](const auto& entities) { return notification_recipient_mapper::map(entities); },
        lg(),
        "Reading latest notification recipients by account.");

    return rows;
}

std::uint32_t notification_recipient_repository::get_total_recipient_count_by_notification(
    const boost::uuids::uuid& notification_id) {
    const auto notification_id_str = boost::uuids::to_string(notification_id);
    BOOST_LOG_SEV(lg(), debug)
        << "Retrieving total active notification recipients count. Notification: "
        << notification_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<notification_recipient_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "notification_id"_c == notification_id_str &&
              "valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active notification recipients count by notification: "
                               << count;
    return count;
}

std::vector<domain::notification_recipient>
notification_recipient_repository::read_latest_by_account(const boost::uuids::uuid& account_id,
                                                          std::uint32_t offset,
                                                          std::uint32_t limit) {
    const auto account_id_str = boost::uuids::to_string(account_id);
    BOOST_LOG_SEV(lg(), debug) << "Reading latest notification recipients. Account: " << account_id
                               << " offset: " << offset << " limit: " << limit;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<notification_recipient_entity>> |
                       where("tenant_id"_c == tid && "account_id"_c == account_id_str &&
                             "valid_to"_c == max.value()) |
                       order_by("notification_id"_c) | sqlgen::offset(offset) |
                       sqlgen::limit(limit);

    auto rows = execute_read_query<notification_recipient_entity, domain::notification_recipient>(
        ctx_,
        query,
        [](const auto& entities) { return notification_recipient_mapper::map(entities); },
        lg(),
        "Reading latest notification recipients by account (paginated).");

    return rows;
}

std::uint32_t notification_recipient_repository::get_total_recipient_count_by_account(
    const boost::uuids::uuid& account_id) {
    const auto account_id_str = boost::uuids::to_string(account_id);
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active notification recipients count. Account: "
                               << account_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<notification_recipient_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "account_id"_c == account_id_str &&
              "valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active notification recipients count by account: "
                               << count;
    return count;
}

void notification_recipient_repository::remove(const boost::uuids::uuid& notification_id,
                                               const boost::uuids::uuid& account_id) {
    static_cast<void>(remove(notification_id, account_id, std::nullopt));
}

notification_recipient_repository::remove_status
notification_recipient_repository::remove(const boost::uuids::uuid& notification_id,
                                          const boost::uuids::uuid& account_id,
                                          std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing notification recipient from database: "
                               << notification_id << "/" << account_id;

    const auto current = read_latest(notification_id, account_id);
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
    const auto notification_id_str = boost::uuids::to_string(notification_id);
    const auto account_id_str = boost::uuids::to_string(account_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::delete_from<notification_recipient_entity> |
                       where("tenant_id"_c == tid && "notification_id"_c == notification_id_str &&
                             "account_id"_c == account_id_str && "valid_to"_c == max.value() &&
                             "version"_c == expected);

    execute_delete_query(ctx_, query, lg(), "removing notification recipient from database");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(notification_id, account_id).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void notification_recipient_repository::remove(
    const std::vector<boost::uuids::uuid>& notification_ids,
    const std::vector<boost::uuids::uuid>& account_ids) {
    // A junction's key is the pair of columns, and a per-column .in() DELETE
    // would be a cross-product over-delete (rows outside the requested pairs),
    // so each pair is removed on its own.
    if (notification_ids.size() != account_ids.size())
        throw std::invalid_argument("notification_recipient_repository::remove: key column vectors "
                                    "must be the same length");
    for (std::size_t i = 0; i < notification_ids.size(); ++i)
        static_cast<void>(remove(notification_ids[i], account_ids[i], std::nullopt));
}

void notification_recipient_repository::remove_by_notification(
    const boost::uuids::uuid& notification_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing all notification recipients from database: "
                               << notification_id;

    const auto notification_id_str = boost::uuids::to_string(notification_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::delete_from<notification_recipient_entity> |
                       where("tenant_id"_c == tid && "notification_id"_c == notification_id_str);

    execute_delete_query(ctx_, query, lg(), "removing all notification recipients from database");
}


}
