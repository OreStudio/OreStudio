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
#ifndef ORES_DQ_CORE_SERVICE_LEI_BIC_SERVICE_HPP
#define ORES_DQ_CORE_SERVICE_LEI_BIC_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.dq.api/domain/lei_bic.hpp"
#include "ores.dq.api/messaging/lei_bic_protocol.hpp"
#include "ores.dq.core/export.hpp"
#include "ores.dq.core/repository/lei_bic_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::dq::service {

/**
 * @brief Service for managing LEI BIC.
 *
 * Provides a higher-level interface for LEI BIC operations,
 * wrapping the underlying repository.
 */
class ORES_DQ_CORE_EXPORT lei_bic_service {
private:
    inline static std::string_view logger_name = "ores.dq.service.lei_bic_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a lei_bic_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit lei_bic_service(context ctx);

    /**
     * @brief The protocol operations, one method per subject.
     *
     * A method takes the canonical request and answers its response, so the
     * handler that serves the subject decodes, calls and replies without
     * deciding anything. The result a caller reads -- missing, conflicting,
     * denied -- is filled here, where the storage call that decided it is
     * made, rather than being inferred from an exception.
     */
    /**@{*/
    messaging::list_lei_bic_response list_lei_bic(const messaging::list_lei_bic_request& request);
    messaging::get_lei_bic_response get_lei_bic(const messaging::get_lei_bic_request& request);
    messaging::get_many_lei_bic_response
    get_many_lei_bic(const messaging::get_many_lei_bic_request& request);
    /**@}*/

    /**
     * @brief Lists LEI BIC with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of LEI BIC for the requested page.
     */
    std::vector<domain::lei_bic> list_lei_bic(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active LEI BIC.
     *
     * @return Total number of active LEI BIC.
     */
    std::uint32_t count_lei_bic();


    /**
     * @brief Retrieves a single LEI BIC by its primary key.
     *
     * @return The LEI BIC if found, std::nullopt otherwise.
     */
    std::optional<domain::lei_bic> get_lei_bic(const std::string& lei);

    /**
     * @brief Retrieves a batch of LEI BIC by primary key.
     */
    std::vector<domain::lei_bic> get_lei_bic(const std::vector<std::string>& leis);

    /**
     * @brief Saves a LEI BIC (creates or updates).
     *
     * @param lei_bic The LEI BIC to save.
     * @throws std::exception on failure.
     */
    void save_lei_bic(const domain::lei_bic& lei_bic);

    /**
     * @brief Saves a batch of LEI BIC.
     *
     * @param lei_bic The LEI BIC to save.
     * @throws std::exception on failure.
     */
    void save_lei_bic(const std::vector<domain::lei_bic>& lei_bic);

    /**
     * @brief Deletes a LEI BIC by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_lei_bic(const std::string& lei);

    /**
     * @brief Deletes LEI BIC by their primary keys.
     */
    void delete_lei_bic(const std::vector<std::string>& leis);


private:
    context ctx_;
    repository::lei_bic_repository repo_;
};

}

#endif
