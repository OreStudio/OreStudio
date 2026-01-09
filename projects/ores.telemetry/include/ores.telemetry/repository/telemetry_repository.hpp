/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_TELEMETRY_REPOSITORY_TELEMETRY_REPOSITORY_HPP
#define ORES_TELEMETRY_REPOSITORY_TELEMETRY_REPOSITORY_HPP

#include <string>
#include <vector>
#include <optional>
#include <boost/uuid/uuid.hpp>
#include <sqlgen/postgres.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.telemetry/domain/telemetry_log_entry.hpp"
#include "ores.telemetry/domain/telemetry_query.hpp"
#include "ores.telemetry/domain/telemetry_stats.hpp"
#include "ores.telemetry/domain/telemetry_batch.hpp"

namespace ores::telemetry::repository {

/**
 * @brief Repository for telemetry log persistence and querying.
 *
 * Handles CRUD operations for telemetry log records stored in a TimescaleDB
 * hypertable. Supports time-range queries and statistics aggregation.
 */
class telemetry_repository {
private:
    inline static std::string_view logger_name =
        "ores.telemetry.repository.telemetry_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;
    explicit telemetry_repository(context ctx);

    /**
     * @brief Returns the SQL created by sqlgen to construct the table.
     */
    std::string sql();

    /**
     * @brief Creates a single log entry.
     */
    void create(const domain::telemetry_log_entry& entry);

    /**
     * @brief Creates multiple log entries in a batch.
     *
     * More efficient than individual inserts for bulk operations.
     *
     * @return Number of entries successfully inserted.
     */
    std::size_t create_batch(const domain::telemetry_batch& batch);

    /**
     * @brief Queries log entries with filters.
     */
    std::vector<domain::telemetry_log_entry> query(
        const domain::telemetry_query& q);

    /**
     * @brief Counts log entries matching a query.
     *
     * Useful for pagination.
     */
    std::uint64_t count(const domain::telemetry_query& q);

    /**
     * @brief Reads logs for a specific session.
     *
     * @param session_id The session UUID
     * @param limit Maximum number of logs to return
     * @return Logs ordered by timestamp descending (newest first)
     */
    std::vector<domain::telemetry_log_entry> read_by_session(
        const boost::uuids::uuid& session_id,
        std::uint32_t limit = 1000);

    /**
     * @brief Reads logs for a specific account.
     *
     * @param account_id The account UUID
     * @param start Start of time range
     * @param end End of time range
     * @param limit Maximum number of logs to return
     */
    std::vector<domain::telemetry_log_entry> read_by_account(
        const boost::uuids::uuid& account_id,
        const std::chrono::system_clock::time_point& start,
        const std::chrono::system_clock::time_point& end,
        std::uint32_t limit = 1000);

    /**
     * @brief Reads hourly statistics.
     */
    std::vector<domain::telemetry_stats> read_hourly_stats(
        const domain::telemetry_stats_query& q);

    /**
     * @brief Reads daily statistics.
     */
    std::vector<domain::telemetry_stats> read_daily_stats(
        const domain::telemetry_stats_query& q);

    /**
     * @brief Gets a summary of telemetry activity.
     *
     * @param hours Number of hours to include (default: 24)
     */
    domain::telemetry_summary get_summary(std::uint32_t hours = 24);

    /**
     * @brief Counts error logs in the last N hours for a source.
     *
     * Useful for monitoring and alerting.
     */
    std::uint64_t count_errors(const std::string& source_name,
        std::uint32_t hours = 1);

    /**
     * @brief Deletes logs older than the specified retention period.
     *
     * Note: With TimescaleDB, this is typically handled by retention
     * policies. This method is for manual cleanup or non-TimescaleDB
     * deployments.
     *
     * @param older_than Delete logs older than this timestamp
     * @return Number of logs deleted
     */
    std::uint64_t delete_old_logs(
        const std::chrono::system_clock::time_point& older_than);

private:
    context ctx_;
};

}

#endif
