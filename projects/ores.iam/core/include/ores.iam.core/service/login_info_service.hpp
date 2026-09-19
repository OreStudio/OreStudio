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
#ifndef ORES_IAM_CORE_SERVICE_LOGIN_INFO_SERVICE_HPP
#define ORES_IAM_CORE_SERVICE_LOGIN_INFO_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.iam.api/domain/login_info.hpp"
#include "ores.iam.core/export.hpp"
#include "ores.iam.core/repository/login_info_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::iam::service {

/**
 * @brief Service for managing login info.
 *
 * Provides a higher-level interface for login info operations,
 * wrapping the underlying repository.
 */
class ORES_IAM_CORE_EXPORT login_info_service {
private:
    inline static std::string_view logger_name = "ores.iam.service.login_info_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a login_info_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit login_info_service(context ctx);

    /**
     * @brief Lists login info with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of login info for the requested page.
     */
    std::vector<domain::login_info> list_login_info(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active login info.
     *
     * @return Total number of active login info.
     */
    std::uint32_t count_login_info();


    /**
     * @brief Retrieves a single login info by its primary key.
     *
     * @return The login info if found, std::nullopt otherwise.
     */
    std::optional<domain::login_info> get_login_info(const std::string& account_id);

    /**
     * @brief Saves a login info (creates or updates).
     *
     * @param login_info The login info to save.
     * @throws std::exception on failure.
     */
    void save_login_info(const domain::login_info& login_info);

    /**
     * @brief Saves a batch of login info.
     *
     * @param login_info The login info to save.
     * @throws std::exception on failure.
     */
    void save_login_info(const std::vector<domain::login_info>& login_info);

    /**
     * @brief Deletes a login info by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_login_info(const std::string& account_id);

    /**
     * @brief Deletes login info by their primary keys.
     */
    void delete_login_info(const std::vector<std::string>& account_ids);


private:
    context ctx_;
    repository::login_info_repository repo_;
};

}

#endif
