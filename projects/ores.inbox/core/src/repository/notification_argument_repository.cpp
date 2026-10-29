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
#include "ores.inbox.core/repository/notification_argument_repository.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.inbox.api/domain/notification_argument.hpp"
#include "ores.inbox.api/domain/notification_argument_json_io.hpp" // IWYU pragma: keep.
#include "ores.inbox.core/repository/notification_argument_entity.hpp"
#include "ores.inbox.core/repository/notification_argument_mapper.hpp"
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

namespace ores::inbox::repository {

namespace query_builder = ores::database::query_builder;
using namespace ores::database::query_builder::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string notification_argument_repository::sql() {
    return query_builder::insert_statement_sql<notification_argument_entity>(lg());
}

notification_argument_repository::notification_argument_repository(context ctx)
    : ctx_(std::move(ctx)) {}

ores::utility::domain::precondition
notification_argument_repository::replace_claim(const domain::notification_argument& v) {
    const auto current = read_latest(v.notification_id, v.name);
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::notification_argument
notification_argument_repository::apply_claim(const domain::notification_argument& v,
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
            const auto current = read_latest(v.notification_id, v.name);
            t.version = current.empty() ? 0 : current.front().version;
            break;
        }
    }
    return t;
}

void notification_argument_repository::write(const domain::notification_argument& argument) {
    write(argument, replace_claim(argument));
}

void notification_argument_repository::write(
    const std::vector<domain::notification_argument>& arguments) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(arguments.size());
    for (const auto& item : arguments)
        claims.push_back(replace_claim(item));
    write(arguments, claims);
}

void notification_argument_repository::write(const domain::notification_argument& argument,
                                             const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing notification argument to database: "
                               << argument.notification_id << "/" << argument.name;
    const auto t = apply_claim(argument, claim);
    query_builder::execute_write_query(ctx_,
                                       notification_argument_mapper::map(t),
                                       lg(),
                                       "writing notification argument to database");
}

void notification_argument_repository::write(
    const std::vector<domain::notification_argument>& arguments,
    const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing notification arguments to database. Count: "
                               << arguments.size();
    std::vector<domain::notification_argument> batch;
    batch.reserve(arguments.size());
    for (std::size_t i = 0; i < arguments.size(); ++i)
        batch.push_back(apply_claim(arguments[i], claims[i]));
    query_builder::execute_write_query(ctx_,
                                       notification_argument_mapper::map(batch),
                                       lg(),
                                       "writing notification arguments to database");
}

std::vector<domain::notification_argument> notification_argument_repository::read_latest() {
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<notification_argument_entity>> |
                       query_builder::where("tenant_id"_c == tid && "valid_to"_c == max) |
                       query_builder::order_by("notification_id"_c, "name"_c);

    return query_builder::execute_read_query<notification_argument_entity,
                                             domain::notification_argument>(
        ctx_,
        query,
        [](const auto& entities) { return notification_argument_mapper::map(entities); },
        lg(),
        "Reading latest notification arguments");
}

std::vector<domain::notification_argument>
notification_argument_repository::read_latest(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest notification arguments with offset: " << offset
                               << " and limit: " << limit;
    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<notification_argument_entity>> |
                       query_builder::where("tenant_id"_c == tid && "valid_to"_c == max) |
                       query_builder::order_by("notification_id"_c, "name"_c) |
                       query_builder::offset(offset) | query_builder::limit(limit);

    return query_builder::execute_read_query<notification_argument_entity,
                                             domain::notification_argument>(
        ctx_,
        query,
        [](const auto& entities) { return notification_argument_mapper::map(entities); },
        lg(),
        "Reading latest notification arguments (paginated).");
}

std::vector<domain::notification_argument>
notification_argument_repository::read_latest(const boost::uuids::uuid& notification_id,
                                              const std::string& name) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest notification argument. " << notification_id << "/"
                               << name;

    static constexpr auto max = db_timestamp::max();
    const auto notification_id_str = boost::uuids::to_string(notification_id);
    const auto name_str = name;
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<notification_argument_entity>> |
        query_builder::where("tenant_id"_c == tid && "notification_id"_c == notification_id_str &&
                             "name"_c == name && "valid_to"_c == max);

    return query_builder::execute_read_query<notification_argument_entity,
                                             domain::notification_argument>(
        ctx_,
        query,
        [](const auto& entities) { return notification_argument_mapper::map(entities); },
        lg(),
        "Reading latest notification argument by key.");
}

std::uint32_t notification_argument_repository::get_total_argument_count() {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active notification arguments count";
    static constexpr auto max = db_timestamp::max();

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = query_builder::read<std::vector<notification_argument_entity>> |
                       query_builder::where("tenant_id"_c == tid && "valid_to"_c == max);

    const auto count = query_builder::execute_count_query<notification_argument_entity>(
        ctx_, query, std::nullopt, lg(), "Counting notification arguments");
    BOOST_LOG_SEV(lg(), debug) << "Total active notification arguments count: " << count;
    return count;
}

std::vector<domain::notification_argument>
notification_argument_repository::read_latest_by_notification(
    const boost::uuids::uuid& notification_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest notification arguments. Notification: "
                               << notification_id;

    static constexpr auto max = db_timestamp::max();
    const auto notification_id_str = boost::uuids::to_string(notification_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<notification_argument_entity>> |
        query_builder::where("tenant_id"_c == tid && "notification_id"_c == notification_id_str &&
                             "valid_to"_c == max) |
        query_builder::order_by("name"_c);

    auto rows = query_builder::execute_read_query<notification_argument_entity,
                                                  domain::notification_argument>(
        ctx_,
        query,
        [](const auto& entities) { return notification_argument_mapper::map(entities); },
        lg(),
        "Reading latest notification arguments by notification.");

    return rows;
}

std::vector<domain::notification_argument>
notification_argument_repository::read_latest_by_name(const std::string& name) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest notification arguments. Name: " << name;

    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<notification_argument_entity>> |
        query_builder::where("tenant_id"_c == tid && "name"_c == name && "valid_to"_c == max) |
        query_builder::order_by("notification_id"_c);

    auto rows = query_builder::execute_read_query<notification_argument_entity,
                                                  domain::notification_argument>(
        ctx_,
        query,
        [](const auto& entities) { return notification_argument_mapper::map(entities); },
        lg(),
        "Reading latest notification arguments by name.");

    return rows;
}

std::vector<domain::notification_argument>
notification_argument_repository::read_latest_by_notification(
    const boost::uuids::uuid& notification_id, std::uint32_t offset, std::uint32_t limit) {
    const auto notification_id_str = boost::uuids::to_string(notification_id);
    BOOST_LOG_SEV(lg(), debug) << "Reading latest notification arguments. Notification: "
                               << notification_id << " offset: " << offset << " limit: " << limit;

    static constexpr auto max = db_timestamp::max();
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<notification_argument_entity>> |
        query_builder::where("tenant_id"_c == tid && "notification_id"_c == notification_id_str &&
                             "valid_to"_c == max) |
        query_builder::order_by("name"_c) | query_builder::offset(offset) |
        query_builder::limit(limit);

    auto rows = query_builder::execute_read_query<notification_argument_entity,
                                                  domain::notification_argument>(
        ctx_,
        query,
        [](const auto& entities) { return notification_argument_mapper::map(entities); },
        lg(),
        "Reading latest notification arguments by notification (paginated).");

    return rows;
}

std::uint32_t notification_argument_repository::get_total_argument_count_by_notification(
    const boost::uuids::uuid& notification_id) {
    const auto notification_id_str = boost::uuids::to_string(notification_id);
    BOOST_LOG_SEV(lg(), debug)
        << "Retrieving total active notification arguments count. Notification: "
        << notification_id;
    static constexpr auto max = db_timestamp::max();

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<notification_argument_entity>> |
        query_builder::where("tenant_id"_c == tid && "notification_id"_c == notification_id_str &&
                             "valid_to"_c == max);

    const auto count = query_builder::execute_count_query<notification_argument_entity>(
        ctx_, query, std::nullopt, lg(), "Counting notification arguments");
    BOOST_LOG_SEV(lg(), debug) << "Total active notification arguments count by notification: "
                               << count;
    return count;
}

std::uint32_t
notification_argument_repository::get_total_argument_count_by_name(const std::string& name) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active notification arguments count. Name: "
                               << name;
    static constexpr auto max = db_timestamp::max();

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<notification_argument_entity>> |
        query_builder::where("tenant_id"_c == tid && "name"_c == name && "valid_to"_c == max);

    const auto count = query_builder::execute_count_query<notification_argument_entity>(
        ctx_, query, std::nullopt, lg(), "Counting notification arguments");
    BOOST_LOG_SEV(lg(), debug) << "Total active notification arguments count by name: " << count;
    return count;
}

std::vector<notification_argument_history_row>
notification_argument_repository::read_all_by_notification(
    const boost::uuids::uuid& notification_id) {
    BOOST_LOG_SEV(lg(), debug)
        << "Reading every version of the notification arguments. Notification: " << notification_id;

    const auto notification_id_str = boost::uuids::to_string(notification_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::read<std::vector<notification_argument_entity>> |
        query_builder::where("tenant_id"_c == tid && "notification_id"_c == notification_id_str) |
        query_builder::order_by("name"_c, "valid_from"_c.desc());

    return query_builder::execute_read_query<notification_argument_entity,
                                             notification_argument_history_row>(
        ctx_,
        query,
        [](const auto& entities) {
            std::vector<notification_argument_history_row> rows;
            rows.reserve(entities.size());
            for (const auto& entity : entities) {
                notification_argument_history_row row;
                row.link = notification_argument_mapper::map(entity);
                row.modified_by = row.link.modified_by;
                row.recorded_at = row.link.recorded_at;
                // A version ends when a later version replaces it, which is not
                // a closing, or when the link is closed. The rows of one link run
                // newest first, so the row before this one, when it is the same
                // link, is the next newer version, and it replaced this one only
                // if it carries the next version number. A link opened again
                // after a closing starts its numbering over.
                const bool replaced = !rows.empty() && rows.back().link.name == row.link.name &&
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
        "Reading every version of the notification arguments by notification.");
}

void notification_argument_repository::remove(const boost::uuids::uuid& notification_id,
                                              const std::string& name) {
    static_cast<void>(remove(notification_id, name, std::nullopt));
}

notification_argument_repository::remove_status
notification_argument_repository::remove(const boost::uuids::uuid& notification_id,
                                         const std::string& name,
                                         std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing notification argument from database: "
                               << notification_id << "/" << name;

    const auto current = read_latest(notification_id, name);
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
    const auto notification_id_str = boost::uuids::to_string(notification_id);
    const auto name_str = name;
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::delete_from<notification_argument_entity> |
        query_builder::where("tenant_id"_c == tid && "notification_id"_c == notification_id_str &&
                             "name"_c == name && "valid_to"_c == max && "version"_c == expected);

    query_builder::execute_delete_query(
        ctx_, query, lg(), "removing notification argument from database");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(notification_id, name).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void notification_argument_repository::remove(
    const std::vector<boost::uuids::uuid>& notification_ids,
    const std::vector<std::string>& names) {
    // A junction's key is the pair of columns, and a per-column .in() DELETE
    // would be a cross-product over-delete (rows outside the requested pairs),
    // so each pair is removed on its own.
    if (notification_ids.size() != names.size())
        throw std::invalid_argument(
            "notification_argument_repository::remove: key column vectors must be the same length");
    for (std::size_t i = 0; i < notification_ids.size(); ++i)
        static_cast<void>(remove(notification_ids[i], names[i], std::nullopt));
}

void notification_argument_repository::remove_by_notification(
    const boost::uuids::uuid& notification_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing all notification arguments from database: "
                               << notification_id;

    const auto notification_id_str = boost::uuids::to_string(notification_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        query_builder::delete_from<notification_argument_entity> |
        query_builder::where("tenant_id"_c == tid && "notification_id"_c == notification_id_str);

    query_builder::execute_delete_query(
        ctx_, query, lg(), "removing all notification arguments from database");
}


}
