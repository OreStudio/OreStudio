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
 * Template: cpp_service.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_IAM_CORE_SERVICE_SESSION_SERVICE_HPP
#define ORES_IAM_CORE_SERVICE_SESSION_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.iam.api/domain/session.hpp"
#include "ores.iam.core/export.hpp"
#include "ores.iam.core/repository/session_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::iam::service {

/**
 * @brief Service for managing sessions.
 *
 * Provides a higher-level interface for session operations,
 * wrapping the underlying repository.
 */
class ORES_IAM_CORE_EXPORT session_service {
private:
    inline static std::string_view logger_name = "ores.iam.service.session_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a session_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit session_service(context ctx);

    /**
     * @brief Lists sessions with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of sessions for the requested page.
     */
    std::vector<domain::session> list_sessions(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active sessions.
     *
     * @return Total number of active sessions.
     */
    std::uint32_t count_sessions();


    /**
     * @brief Retrieves a single session by its primary key.
     *
     * @return The session if found, std::nullopt otherwise.
     */
    std::optional<domain::session> get_session(const std::string& id,
                                               const std::string& start_time);

    /**
     * @brief Saves a session (creates or updates).
     *
     * @param session The session to save.
     * @throws std::exception on failure.
     */
    void save_session(const domain::session& session);

    /**
     * @brief Saves a batch of sessions.
     *
     * @param sessions The sessions to save.
     * @throws std::exception on failure.
     */
    void save_sessions(const std::vector<domain::session>& sessions);

    /**
     * @brief Deletes a session by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_session(const std::string& id, const std::string& start_time);

    /**
     * @brief Deletes sessions by their primary keys.
     */
    void delete_sessions(const std::vector<std::string>& ids,
                         const std::vector<std::string>& start_times);


private:
    context ctx_;
    repository::session_repository repo_;
};

}

#endif
