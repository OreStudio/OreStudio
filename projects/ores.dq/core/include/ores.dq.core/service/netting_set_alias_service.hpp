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
#ifndef ORES_DQ_CORE_SERVICE_NETTING_SET_ALIAS_SERVICE_HPP
#define ORES_DQ_CORE_SERVICE_NETTING_SET_ALIAS_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.dq.api/domain/netting_set_alias.hpp"
#include "ores.dq.api/messaging/netting_set_alias_protocol.hpp"
#include "ores.dq.core/export.hpp"
#include "ores.dq.core/repository/netting_set_alias_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::dq::service {

/**
 * @brief Service for managing netting set aliases.
 *
 * Provides a higher-level interface for netting set alias operations,
 * wrapping the underlying repository.
 */
class ORES_DQ_CORE_EXPORT netting_set_alias_service {
private:
    inline static std::string_view logger_name = "ores.dq.service.netting_set_alias_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a netting_set_alias_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit netting_set_alias_service(context ctx);

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
    messaging::list_netting_set_aliases_response
    list_netting_set_aliases(const messaging::list_netting_set_aliases_request& request);
    messaging::get_netting_set_alias_response
    get_netting_set_alias(const messaging::get_netting_set_alias_request& request);
    messaging::get_many_netting_set_aliases_response
    get_many_netting_set_aliases(const messaging::get_many_netting_set_aliases_request& request);
    /**@}*/

    /**
     * @brief Lists netting set aliases with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of netting set aliases for the requested page.
     */
    std::vector<domain::netting_set_alias> list_netting_set_aliases(std::uint32_t offset,
                                                                    std::uint32_t limit);

    /**
     * @brief Gets the total count of active netting set aliases.
     *
     * @return Total number of active netting set aliases.
     */
    std::uint32_t count_netting_set_aliases();


    /**
     * @brief Retrieves a single netting set alias by its primary key.
     *
     * @return The netting set alias if found, std::nullopt otherwise.
     */
    std::optional<domain::netting_set_alias> get_netting_set_alias(const std::string& id_value);

    /**
     * @brief Retrieves a batch of netting set aliases by primary key.
     */
    std::vector<domain::netting_set_alias>
    get_netting_set_aliases(const std::vector<std::string>& id_values);

    /**
     * @brief Saves a netting set alias (creates or updates).
     *
     * @param netting_set_alias The netting set alias to save.
     * @throws std::exception on failure.
     */
    void save_netting_set_alias(const domain::netting_set_alias& netting_set_alias);

    /**
     * @brief Saves a batch of netting set aliases.
     *
     * @param netting_set_aliases The netting set aliases to save.
     * @throws std::exception on failure.
     */
    void
    save_netting_set_aliases(const std::vector<domain::netting_set_alias>& netting_set_aliases);

    /**
     * @brief Deletes a netting set alias by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_netting_set_alias(const std::string& id_value);

    /**
     * @brief Deletes netting set aliases by their primary keys.
     */
    void delete_netting_set_aliases(const std::vector<std::string>& id_values);


private:
    context ctx_;
    repository::netting_set_alias_repository repo_;
};

}

#endif
