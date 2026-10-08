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
#ifndef ORES_DQ_CORE_SERVICE_RISK_REPORT_CONFIG_SERVICE_HPP
#define ORES_DQ_CORE_SERVICE_RISK_REPORT_CONFIG_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.dq.api/domain/risk_report_config.hpp"
#include "ores.dq.api/messaging/risk_report_config_protocol.hpp"
#include "ores.dq.core/export.hpp"
#include "ores.dq.core/repository/risk_report_config_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::dq::service {

/**
 * @brief Service for managing risk report configs.
 *
 * Provides a higher-level interface for risk report config operations,
 * wrapping the underlying repository.
 */
class ORES_DQ_CORE_EXPORT risk_report_config_service {
private:
    inline static std::string_view logger_name = "ores.dq.service.risk_report_config_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a risk_report_config_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit risk_report_config_service(context ctx);

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
    messaging::list_risk_report_configs_response
    list_risk_report_configs(const messaging::list_risk_report_configs_request& request);
    messaging::get_risk_report_config_response
    get_risk_report_config(const messaging::get_risk_report_config_request& request);
    messaging::get_many_risk_report_configs_response
    get_many_risk_report_configs(const messaging::get_many_risk_report_configs_request& request);
    /**@}*/

    /**
     * @brief Lists risk report configs with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of risk report configs for the requested page.
     */
    std::vector<domain::risk_report_config> list_configs(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active risk report configs.
     *
     * @return Total number of active risk report configs.
     */
    std::uint32_t count_configs();


    /**
     * @brief Retrieves a single risk report config by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The risk report config if found, std::nullopt otherwise.
     */
    std::optional<domain::risk_report_config> get_config(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of risk report configs by primary key.
     */
    std::vector<domain::risk_report_config> get_configs(const std::vector<std::string>& ids);

    /**
     * @brief Saves a risk report config (creates or updates).
     *
     * @param config The risk report config to save.
     * @throws std::exception on failure.
     */
    void save_config(const domain::risk_report_config& config);

    /**
     * @brief Saves a batch of risk report configs.
     *
     * @param configs The risk report configs to save.
     * @throws std::exception on failure.
     */
    void save_configs(const std::vector<domain::risk_report_config>& configs);

    /**
     * @brief Deletes a risk report config by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_config(const boost::uuids::uuid& id);

    /**
     * @brief Deletes risk report configs by their primary keys.
     */
    void delete_configs(const std::vector<std::string>& ids);


private:
    context ctx_;
    repository::risk_report_config_repository repo_;
};

}

#endif
