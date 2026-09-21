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
 * Template: cpp_domain_type_repository.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_IAM_CORE_REPOSITORY_SESSION_REPOSITORY_HPP
#define ORES_IAM_CORE_REPOSITORY_SESSION_REPOSITORY_HPP

#include "ores.database/domain/context.hpp"
#include "ores.iam.api/domain/session.hpp"
#include "ores.iam.core/export.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <sqlgen/postgres.hpp>
#include <string>
#include <vector>

namespace ores::iam::repository {

/**
 * @brief Reads and writes sessions to data storage.
 */
class ORES_IAM_CORE_EXPORT session_repository {
private:
    inline static std::string_view logger_name = "ores.iam.repository.session_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Returns the SQL created by sqlgen to construct the table.
     */
    std::string sql();

    /**
     * @brief Writes sessions to database.
     */
    /**@{*/
    void write(context ctx, const domain::session& v);
    void write(context ctx, const std::vector<domain::session>& v);
    /**@}*/

    /**
     * @brief Reads latest sessions, possibly filtered by primary key.
     */
    /**@{*/
    std::vector<domain::session> read_latest(context ctx);
    std::vector<domain::session>
    read_latest(context ctx, const std::string& id, const std::string& start_time);
    std::vector<domain::session> read_latest(context ctx,
                                             const std::vector<std::string>& ids,
                                             const std::vector<std::string>& start_times);
    /**@}*/

    /**
     * @brief Reads the session rows for the given primary key.
     *
     * A current-state table holds one row per key, so this is the single
     * current row, not a version history.
     */
    std::vector<domain::session>
    read_all(context ctx, const std::string& id, const std::string& start_time);


    /**
     * @brief Reads latest sessions with pagination support.
     * @param ctx Repository context with database connection
     * @param offset Number of records to skip
     * @param limit Maximum number of records to return
     */
    std::vector<domain::session>
    read_latest(context ctx, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active sessions.
     * @param ctx Repository context with database connection
     * @return Total number of active sessions
     */
    std::uint32_t get_total_session_count(context ctx);

    /**
     * @brief Deletes a session permanently.
     *
     * A current-state table has no history, so the row is removed, not
     * soft-closed.
     */
    void remove(context ctx, const std::string& id, const std::string& start_time);

    /**
     * @brief What a removal did, so a caller reports a conflict as an outcome
     * rather than catching an exception.
     *
     * @c missing means there was no current row to remove, and @c unsupported
     * means the store cannot answer the version at all -- a current-state
     * table has no version column, so a versioned removal has no meaning
     * there.
     */
    enum class remove_status { removed, conflicting, missing, unsupported };

    /**
     * @brief Removes a session, refusing a row that moved on.
     *
     * A stated version is the version the caller read. The removal is refused
     * with @c conflicting when the current row carries another, so a caller
     * that decided on stale state cannot remove a change it never saw. A null
     * version removes whatever is current, which is what a caller that stated
     * no version asked for.
     */
    remove_status remove(context ctx,
                         const std::string& id,
                         const std::string& start_time,
                         std::optional<std::uint32_t> version);

    /**
     * @brief Deletes sessions permanently.
     */
    void remove(context ctx,
                const std::vector<std::string>& ids,
                const std::vector<std::string>& start_times);

    std::optional<domain::session> read(context ctx, const boost::uuids::uuid& session_id);

    void update_bytes(context ctx,
                      const boost::uuids::uuid& session_id,
                      const std::chrono::system_clock::time_point& start_time,
                      std::uint64_t bytes_sent,
                      std::uint64_t bytes_received);

    void end_session(context ctx,
                     const boost::uuids::uuid& session_id,
                     const std::chrono::system_clock::time_point& start_time,
                     const std::chrono::system_clock::time_point& end_time,
                     std::uint64_t bytes_sent,
                     std::uint64_t bytes_received);

    std::vector<domain::session> read_by_account(context ctx,
                                                 const boost::uuids::uuid& account_id,
                                                 std::uint32_t limit = 0,
                                                 std::uint32_t offset = 0);

    std::vector<domain::session> read_active_by_account(context ctx,
                                                        const boost::uuids::uuid& account_id);

    std::uint32_t count_by_account(context ctx, const boost::uuids::uuid& account_id);

    std::vector<domain::session> read_all_active(context ctx);
};

}

#endif
