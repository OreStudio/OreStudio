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
#include "ores.mq/repository/message_repository.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"

namespace ores::mq::repository {

using namespace ores::logging;
using namespace ores::database::repository;

namespace {

[[nodiscard]] auto& lg() {
    static auto instance = make_logger("ores.mq.repository.message_repository");
    return instance;
}

/**
 * @brief Parses a PostgreSQL timestamptz string into a system_clock::time_point.
 */
std::chrono::system_clock::time_point parse_pg_timestamp(const std::string& s) {
    if (s.empty()) return {};

    std::string base = s;
    const auto dot = base.find('.');
    if (dot != std::string::npos) {
        base = base.substr(0, dot);
    } else {
        const auto tz = base.find_first_of("+-", 11);
        if (tz != std::string::npos) {
            base = base.substr(0, tz);
        }
    }

    try {
        return ores::platform::time::datetime::parse_time_point_utc(base);
    } catch (const std::exception&) {
        return {};
    }
}

/**
 * @brief Converts a string to mq_message_status.
 */
domain::mq_message_status parse_status(const std::string& s) {
    if (s == "processing") return domain::mq_message_status::processing;
    if (s == "done")       return domain::mq_message_status::done;
    if (s == "failed")     return domain::mq_message_status::failed;
    return domain::mq_message_status::pending;
}

/**
 * @brief Parses a row from a message read query into an mq_message.
 *
 * Expected column order (from ores_mq_messages_read_fn):
 *   id(0), queue_id(1), tenant_id(2), party_id(3),
 *   message_type(4), payload_type(5), payload(6),
 *   status(7), visible_after(8), created_at(9), read_count(10),
 *   error_message(11)
 */
domain::mq_message parse_message_row(
    const std::vector<std::optional<std::string>>& row) {

    domain::mq_message msg;
    if (row.size() < 10) return msg;

    if (row[0]) {
        try { msg.id = std::stoll(*row[0]); } catch (...) {}
    }

    if (row[1]) {
        try {
            msg.queue_id = boost::lexical_cast<boost::uuids::uuid>(*row[1]);
        } catch (...) {}
    }

    if (row[2]) {
        try {
            msg.tenant_id = boost::lexical_cast<boost::uuids::uuid>(*row[2]);
        } catch (...) {}
    }

    if (row[3]) {
        try {
            msg.party_id = boost::lexical_cast<boost::uuids::uuid>(*row[3]);
        } catch (...) {}
    }

    msg.message_type  = row[4].value_or("");
    msg.payload_type  = row[5].value_or("json");
    msg.payload       = row[6];
    msg.status        = parse_status(row[7].value_or("pending"));
    msg.visible_after = parse_pg_timestamp(row[8].value_or(""));
    msg.created_at    = parse_pg_timestamp(row[9].value_or(""));

    if (row.size() > 10 && row[10]) {
        try { msg.read_count = std::stoi(*row[10]); } catch (...) {}
    }

    if (row.size() > 11 && row[11])
        msg.error_message = *row[11];

    return msg;
}

} // anonymous namespace

// ---------------------------------------------------------------------------
// message_repository
// ---------------------------------------------------------------------------

std::int64_t message_repository::send(context ctx,
    const boost::uuids::uuid& queue_id,
    const std::string& message_type,
    const std::string& payload_json,
    int delay_seconds) {

    BOOST_LOG_SEV(lg(), debug) << "Sending message to queue: " << queue_id;

    const std::string sql =
        "SELECT ores_mq_messages_send_fn("
        "$1::uuid, $2, $3::jsonb, $4::integer)::bigint";

    auto rows = execute_parameterized_string_query(ctx, sql,
        {boost::uuids::to_string(queue_id),
         message_type,
         payload_json,
         std::to_string(delay_seconds)},
        lg(), "Sending message to queue");

    if (rows.empty()) return 0;
    try { return std::stoll(rows.front()); } catch (...) { return 0; }
}

std::vector<domain::mq_message> message_repository::read(context ctx,
    const boost::uuids::uuid& queue_id, int batch, int vt_seconds) {

    BOOST_LOG_SEV(lg(), debug) << "Reading messages from queue: " << queue_id
                               << " batch=" << batch
                               << " vt=" << vt_seconds << "s";

    const std::string sql =
        "SELECT id::text, queue_id::text, tenant_id::text, party_id::text, "
        "message_type, payload_type, payload::text, "
        "status, visible_after::text, created_at::text, "
        "read_count::text, error_message "
        "FROM ores_mq_messages_read_fn($1::uuid, $2::integer, $3::integer)";

    auto rows = execute_parameterized_multi_column_query(ctx, sql,
        {boost::uuids::to_string(queue_id),
         std::to_string(batch),
         std::to_string(vt_seconds)},
        lg(), "Reading messages from queue");

    std::vector<domain::mq_message> result;
    result.reserve(rows.size());
    for (const auto& row : rows)
        result.push_back(parse_message_row(row));
    return result;
}

void message_repository::ack(context ctx,
    const std::vector<std::int64_t>& message_ids) {

    if (message_ids.empty()) return;

    BOOST_LOG_SEV(lg(), debug) << "Acknowledging " << message_ids.size()
                               << " message(s).";

    // Build bigint array literal: {1,2,3}
    std::string array_lit = "{";
    for (std::size_t i = 0; i < message_ids.size(); ++i) {
        if (i > 0) array_lit += ",";
        array_lit += std::to_string(message_ids[i]);
    }
    array_lit += "}";

    const std::string sql =
        "SELECT ores_mq_messages_ack_fn($1::bigint[])";

    execute_parameterized_command(std::move(ctx), sql, {array_lit},
        lg(), "Acknowledging messages");
}

void message_repository::nack(context ctx, std::int64_t message_id,
    const std::string& error) {

    BOOST_LOG_SEV(lg(), debug) << "Nacking message: " << message_id;

    const std::string sql =
        "SELECT ores_mq_messages_nack_fn($1::bigint, $2)";

    execute_parameterized_command(std::move(ctx), sql,
        {std::to_string(message_id), error},
        lg(), "Nacking message");
}

std::int64_t message_repository::purge(context ctx,
    const boost::uuids::uuid& queue_id) {

    BOOST_LOG_SEV(lg(), info) << "Purging queue: " << queue_id;

    const std::string sql =
        "SELECT ores_mq_messages_purge_fn($1::uuid)::bigint";

    auto rows = execute_parameterized_string_query(ctx, sql,
        {boost::uuids::to_string(queue_id)},
        lg(), "Purging queue messages");

    if (rows.empty()) return 0;
    try { return std::stoll(rows.front()); } catch (...) { return 0; }
}

}
