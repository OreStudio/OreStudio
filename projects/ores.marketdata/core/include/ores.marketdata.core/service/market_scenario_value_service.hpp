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
#ifndef ORES_MARKETDATA_CORE_SERVICE_MARKET_SCENARIO_VALUE_SERVICE_HPP
#define ORES_MARKETDATA_CORE_SERVICE_MARKET_SCENARIO_VALUE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/market_scenario_value.hpp"
#include "ores.marketdata.api/messaging/market_scenario_value_protocol.hpp"
#include "ores.marketdata.core/export.hpp"
#include "ores.marketdata.core/repository/market_scenario_value_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::marketdata::service {

/**
 * @brief Service for managing market scenario values.
 *
 * Provides a higher-level interface for market scenario value operations,
 * wrapping the underlying repository.
 */
class ORES_MARKETDATA_CORE_EXPORT market_scenario_value_service {
private:
    inline static std::string_view logger_name =
        "ores.marketdata.service.market_scenario_value_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a market_scenario_value_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit market_scenario_value_service(context ctx);

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
    ::ores::marketdata::messaging::list_market_scenario_values_response list_market_scenario_values(
        const ::ores::marketdata::messaging::list_market_scenario_values_request& request);
    ::ores::marketdata::messaging::get_market_scenario_value_response get_market_scenario_value(
        const ::ores::marketdata::messaging::get_market_scenario_value_request& request);
    ::ores::marketdata::messaging::get_many_market_scenario_values_response
    get_many_market_scenario_values(
        const ::ores::marketdata::messaging::get_many_market_scenario_values_request& request);
    ::ores::marketdata::messaging::put_market_scenario_value_response put_market_scenario_value(
        const ::ores::marketdata::messaging::put_market_scenario_value_request& request);
    ::ores::marketdata::messaging::put_many_market_scenario_values_response
    put_many_market_scenario_values(
        const ::ores::marketdata::messaging::put_many_market_scenario_values_request& request);
    ::ores::marketdata::messaging::delete_market_scenario_value_response
    delete_market_scenario_value(
        const ::ores::marketdata::messaging::delete_market_scenario_value_request& request);
    ::ores::marketdata::messaging::delete_many_market_scenario_values_response
    delete_many_market_scenario_values(
        const ::ores::marketdata::messaging::delete_many_market_scenario_values_request& request);
    /**@}*/

    /**
     * @brief Lists market scenario values with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of market scenario values for the requested page.
     */
    std::vector<domain::market_scenario_value> list_values(std::uint32_t offset,
                                                           std::uint32_t limit);

    /**
     * @brief Gets the total count of active market scenario values.
     *
     * @return Total number of active market scenario values.
     */
    std::uint32_t count_values();


    /**
     * @brief Retrieves a single market scenario value by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The market scenario value if found, std::nullopt otherwise.
     */
    std::optional<domain::market_scenario_value> get_value(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of market scenario values by primary key.
     */
    std::vector<domain::market_scenario_value> get_values(const std::vector<std::string>& ids);

    /**
     * @brief Saves a market scenario value (creates or updates).
     *
     * @param value The market scenario value to save.
     * @throws std::exception on failure.
     */
    void save_value(const domain::market_scenario_value& value);

    /**
     * @brief Saves a batch of market scenario values.
     *
     * @param values The market scenario values to save.
     * @throws std::exception on failure.
     */
    void save_values(const std::vector<domain::market_scenario_value>& values);

    /**
     * @brief Deletes a market scenario value by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_value(const boost::uuids::uuid& id);

    /**
     * @brief Deletes market scenario values by their primary keys.
     */
    void delete_values(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a market scenario value.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::market_scenario_value> get_value_history(const std::string& id);

private:
    context ctx_;
    repository::market_scenario_value_repository repo_;

    /**
     * @brief Checks one change against the row it names, and stamps it.
     *
     * A single write and a batch state the same claim, so the check, the
     * server-derived provenance and the version the store must match are one
     * decision made in one place. A batch that made the decision per element
     * would eventually make it differently from the single write.
     *
     * @param change The change as the caller stated it.
     * @param intent The reason and commentary the caller gave.
     * @param out The stamped domain object, written only when the result is ok.
     * @return ok, or why the change was refused.
     */
    ores::utility::domain::result
    prepare_change(const ::ores::marketdata::messaging::market_scenario_value_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::market_scenario_value& out);
};

}

#endif
