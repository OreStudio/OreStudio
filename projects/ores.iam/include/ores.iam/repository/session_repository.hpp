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
#ifndef ORES_IAM_REPOSITORY_SESSION_REPOSITORY_HPP
#define ORES_IAM_REPOSITORY_SESSION_REPOSITORY_HPP

#include <string>
#include <vector>
#include <optional>
#include <boost/uuid/uuid.hpp>
#include <sqlgen/postgres.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.comms/service/session_data.hpp"
#include "ores.iam/domain/session.hpp"

namespace ores::iam::repository {

/**
 * @brief Repository for session persistence and querying.
 *
 * Handles CRUD operations for session records stored in a TimescaleDB
 * hypertable. Supports time-range queries and statistics aggregation.
 */
class session_repository {
private:
    inline static std::string_view logger_name =
        "ores.iam.repository.session_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;
    explicit session_repository(context ctx);

    /**
     * @brief Returns the SQL created by sqlgen to construct the table.
     */
    std::string sql();

    /**
     * @brief Creates a new session record.
     */
    void create(const domain::session& session);

    /**
     * @brief Updates an existing session record.
     *
     * Typically used to set end_time and final byte counts.
     */
    void update(const domain::session& session);

    /**
     * @brief Updates byte counts for an active session.
     *
     * @param session_id The session UUID
     * @param start_time The session start time (required for hypertable lookup)
     * @param bytes_sent New total bytes sent
     * @param bytes_received New total bytes received
     */
    void update_bytes(const boost::uuids::uuid& session_id,
        const std::chrono::system_clock::time_point& start_time,
        std::uint64_t bytes_sent, std::uint64_t bytes_received);

    /**
     * @brief Ends a session by setting the end_time.
     *
     * @param session_id The session UUID
     * @param start_time The session start time (required for hypertable lookup)
     * @param end_time The session end time
     * @param bytes_sent Final bytes sent count
     * @param bytes_received Final bytes received count
     */
    void end_session(const boost::uuids::uuid& session_id,
        const std::chrono::system_clock::time_point& start_time,
        const std::chrono::system_clock::time_point& end_time,
        std::uint64_t bytes_sent, std::uint64_t bytes_received);

    /**
     * @brief Reads a session by ID.
     */
    std::optional<domain::session> read(const boost::uuids::uuid& session_id);

    /**
     * @brief Reads all sessions for an account.
     *
     * @param account_id The account UUID
     * @param limit Maximum number of sessions to return (0 = no limit)
     * @param offset Number of sessions to skip
     * @return Sessions ordered by start_time descending (newest first)
     */
    std::vector<domain::session> read_by_account(
        const boost::uuids::uuid& account_id,
        std::uint32_t limit = 0, std::uint32_t offset = 0);

    /**
     * @brief Reads active (non-ended) sessions for an account.
     */
    std::vector<domain::session> read_active_by_account(
        const boost::uuids::uuid& account_id);

    /**
     * @brief Counts active sessions for an account.
     */
    std::uint32_t count_active_by_account(const boost::uuids::uuid& account_id);

    /**
     * @brief Counts all sessions (active and inactive) for an account.
     *
     * Used for pagination when listing session history.
     */
    std::uint32_t count_by_account(const boost::uuids::uuid& account_id);

    /**
     * @brief Reads sessions within a time range.
     *
     * @param start Start of time range
     * @param end End of time range
     * @param limit Maximum number of sessions to return
     */
    std::vector<domain::session> read_by_time_range(
        const std::chrono::system_clock::time_point& start,
        const std::chrono::system_clock::time_point& end,
        std::uint32_t limit = 1000);

    /**
     * @brief Reads all active sessions across all accounts.
     */
    std::vector<domain::session> read_all_active();

    /**
     * @brief Counts all active sessions.
     */
    std::uint32_t count_all_active();

    /**
     * @brief Reads daily session statistics for an account.
     *
     * Uses the pre-computed continuous aggregate for fast queries.
     *
     * @param account_id The account UUID
     * @param start Start of time range
     * @param end End of time range
     */
    std::vector<domain::session_statistics> read_daily_statistics(
        const boost::uuids::uuid& account_id,
        const std::chrono::system_clock::time_point& start,
        const std::chrono::system_clock::time_point& end);

    /**
     * @brief Reads aggregate daily statistics across all accounts.
     */
    std::vector<domain::session_statistics> read_aggregate_daily_statistics(
        const std::chrono::system_clock::time_point& start,
        const std::chrono::system_clock::time_point& end);

    /**
     * @brief Inserts time-series samples for a session in a single batch.
     *
     * Called at logout to persist all in-memory samples accumulated during
     * the session. Wrapped in a single transaction for efficiency.
     *
     * @param session_id The session UUID
     * @param tenant_id The tenant UUID for RLS isolation
     * @param samples Collection of samples recorded at heartbeat frequency
     */
    void insert_samples(const boost::uuids::uuid& session_id,
        const boost::uuids::uuid& tenant_id,
        const std::vector<comms::service::session_sample>& samples);

    /**
     * @brief Reads all time-series samples for a session, ordered by time.
     *
     * @param session_id The session UUID
     * @return Samples ordered by sample_time ascending
     */
    std::vector<comms::service::session_sample>
    read_samples(const boost::uuids::uuid& session_id);

    /**
     * @brief Removes all sessions for an account.
     *
     * Used when deleting an account.
     */
    void remove_by_account(const boost::uuids::uuid& account_id);

private:
    context ctx_;
};

}

#endif
