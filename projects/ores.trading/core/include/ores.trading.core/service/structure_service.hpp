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
#ifndef ORES_TRADING_CORE_SERVICE_STRUCTURE_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_STRUCTURE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/structure.hpp"
#include "ores.trading.api/messaging/structure_protocol.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/structure_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing structures.
 *
 * Provides a higher-level interface for structure operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT structure_service {
private:
    inline static std::string_view logger_name = "ores.trading.service.structure_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a structure_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit structure_service(context ctx);

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
    messaging::list_structures_response
    list_structures(const messaging::list_structures_request& request);
    messaging::get_structure_response
    get_structure(const messaging::get_structure_request& request);
    messaging::get_many_structures_response
    get_many_structures(const messaging::get_many_structures_request& request);
    /**@}*/

    /**
     * @brief Lists structures with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of structures for the requested page.
     */
    std::vector<domain::structure> list_structures(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active structures.
     *
     * @return Total number of active structures.
     */
    std::uint32_t count_structures();


    /**
     * @brief Retrieves a single structure by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The structure if found, std::nullopt otherwise.
     */
    std::optional<domain::structure> get_structure(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of structures by primary key.
     */
    std::vector<domain::structure> get_structures(const std::vector<std::string>& ids);

    /**
     * @brief Saves a structure (creates or updates).
     *
     * @param structure The structure to save.
     * @throws std::exception on failure.
     */
    void save_structure(const domain::structure& structure);

    /**
     * @brief Saves a batch of structures.
     *
     * @param structures The structures to save.
     * @throws std::exception on failure.
     */
    void save_structures(const std::vector<domain::structure>& structures);

    /**
     * @brief Deletes a structure by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_structure(const boost::uuids::uuid& id);

    /**
     * @brief Deletes structures by their primary keys.
     */
    void delete_structures(const std::vector<std::string>& ids);


private:
    context ctx_;
    repository::structure_repository repo_;
};

}

#endif
