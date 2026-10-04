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
#ifndef ORES_DQ_CORE_SERVICE_COUNTERPARTY_ALIAS_SERVICE_HPP
#define ORES_DQ_CORE_SERVICE_COUNTERPARTY_ALIAS_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.dq.api/domain/counterparty_alias.hpp"
#include "ores.dq.api/messaging/counterparty_alias_protocol.hpp"
#include "ores.dq.core/export.hpp"
#include "ores.dq.core/repository/counterparty_alias_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::dq::service {

/**
 * @brief Service for managing counterparty aliases.
 *
 * Provides a higher-level interface for counterparty alias operations,
 * wrapping the underlying repository.
 */
class ORES_DQ_CORE_EXPORT counterparty_alias_service {
private:
    inline static std::string_view logger_name = "ores.dq.service.counterparty_alias_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a counterparty_alias_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit counterparty_alias_service(context ctx);

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
    messaging::list_counterparty_aliases_response
    list_counterparty_aliases(const messaging::list_counterparty_aliases_request& request);
    messaging::get_counterparty_alias_response
    get_counterparty_alias(const messaging::get_counterparty_alias_request& request);
    messaging::get_many_counterparty_aliases_response
    get_many_counterparty_aliases(const messaging::get_many_counterparty_aliases_request& request);
    /**@}*/

    /**
     * @brief Lists counterparty aliases with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of counterparty aliases for the requested page.
     */
    std::vector<domain::counterparty_alias> list_counterparty_aliases(std::uint32_t offset,
                                                                      std::uint32_t limit);

    /**
     * @brief Gets the total count of active counterparty aliases.
     *
     * @return Total number of active counterparty aliases.
     */
    std::uint32_t count_counterparty_aliases();


    /**
     * @brief Retrieves a single counterparty alias by its primary key.
     *
     * @return The counterparty alias if found, std::nullopt otherwise.
     */
    std::optional<domain::counterparty_alias> get_counterparty_alias(const std::string& id_value);

    /**
     * @brief Retrieves a batch of counterparty aliases by primary key.
     */
    std::vector<domain::counterparty_alias>
    get_counterparty_aliases(const std::vector<std::string>& id_values);

    /**
     * @brief Saves a counterparty alias (creates or updates).
     *
     * @param counterparty_alias The counterparty alias to save.
     * @throws std::exception on failure.
     */
    void save_counterparty_alias(const domain::counterparty_alias& counterparty_alias);

    /**
     * @brief Saves a batch of counterparty aliases.
     *
     * @param counterparty_aliases The counterparty aliases to save.
     * @throws std::exception on failure.
     */
    void
    save_counterparty_aliases(const std::vector<domain::counterparty_alias>& counterparty_aliases);

    /**
     * @brief Deletes a counterparty alias by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_counterparty_alias(const std::string& id_value);

    /**
     * @brief Deletes counterparty aliases by their primary keys.
     */
    void delete_counterparty_aliases(const std::vector<std::string>& id_values);


private:
    context ctx_;
    repository::counterparty_alias_repository repo_;
};

}

#endif
