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
#ifndef ORES_DQ_CORE_SERVICE_NETTING_SET_SERVICE_HPP
#define ORES_DQ_CORE_SERVICE_NETTING_SET_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.dq.api/domain/netting_set.hpp"
#include "ores.dq.api/messaging/netting_set_protocol.hpp"
#include "ores.dq.core/export.hpp"
#include "ores.dq.core/repository/netting_set_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::dq::service {

/**
 * @brief Service for managing netting sets.
 *
 * Provides a higher-level interface for netting set operations,
 * wrapping the underlying repository.
 */
class ORES_DQ_CORE_EXPORT netting_set_service {
private:
    inline static std::string_view logger_name = "ores.dq.service.netting_set_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a netting_set_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit netting_set_service(context ctx);

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
    messaging::list_netting_sets_response
    list_netting_sets(const messaging::list_netting_sets_request& request);
    messaging::get_netting_set_response
    get_netting_set(const messaging::get_netting_set_request& request);
    messaging::get_many_netting_sets_response
    get_many_netting_sets(const messaging::get_many_netting_sets_request& request);
    /**@}*/

    /**
     * @brief Lists netting sets with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of netting sets for the requested page.
     */
    std::vector<domain::netting_set> list_netting_sets(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active netting sets.
     *
     * @return Total number of active netting sets.
     */
    std::uint32_t count_netting_sets();


    /**
     * @brief Retrieves a single netting set by its primary key.
     *
     * @return The netting set if found, std::nullopt otherwise.
     */
    std::optional<domain::netting_set> get_netting_set(const std::string& code);

    /**
     * @brief Retrieves a batch of netting sets by primary key.
     */
    std::vector<domain::netting_set> get_netting_sets(const std::vector<std::string>& codes);

    /**
     * @brief Saves a netting set (creates or updates).
     *
     * @param netting_set The netting set to save.
     * @throws std::exception on failure.
     */
    void save_netting_set(const domain::netting_set& netting_set);

    /**
     * @brief Saves a batch of netting sets.
     *
     * @param netting_sets The netting sets to save.
     * @throws std::exception on failure.
     */
    void save_netting_sets(const std::vector<domain::netting_set>& netting_sets);

    /**
     * @brief Deletes a netting set by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_netting_set(const std::string& code);

    /**
     * @brief Deletes netting sets by their primary keys.
     */
    void delete_netting_sets(const std::vector<std::string>& codes);


private:
    context ctx_;
    repository::netting_set_repository repo_;
};

}

#endif
