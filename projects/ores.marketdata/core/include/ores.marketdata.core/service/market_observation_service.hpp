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
#ifndef ORES_MARKETDATA_CORE_SERVICE_MARKET_OBSERVATION_SERVICE_HPP
#define ORES_MARKETDATA_CORE_SERVICE_MARKET_OBSERVATION_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/market_observation.hpp"
#include "ores.marketdata.api/messaging/market_observation_protocol.hpp"
#include "ores.marketdata.core/export.hpp"
#include "ores.marketdata.core/repository/market_observation_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::marketdata::service {

/**
 * @brief Service for managing market observations.
 *
 * Provides a higher-level interface for market observation operations,
 * wrapping the underlying repository.
 */
class ORES_MARKETDATA_CORE_EXPORT market_observation_service {
private:
    inline static std::string_view logger_name =
        "ores.marketdata.service.market_observation_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a market_observation_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit market_observation_service(context ctx);

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
    messaging::list_market_observations_response
    list_market_observations(const messaging::list_market_observations_request& request);
    messaging::get_market_observation_response
    get_market_observation(const messaging::get_market_observation_request& request);
    messaging::get_many_market_observations_response
    get_many_market_observations(const messaging::get_many_market_observations_request& request);
    messaging::put_market_observation_response
    put_market_observation(const messaging::put_market_observation_request& request);
    messaging::put_many_market_observations_response
    put_many_market_observations(const messaging::put_many_market_observations_request& request);
    messaging::delete_market_observation_response
    delete_market_observation(const messaging::delete_market_observation_request& request);
    messaging::delete_many_market_observations_response delete_many_market_observations(
        const messaging::delete_many_market_observations_request& request);
    messaging::list_by_series_id_market_observations_response list_by_series_id_market_observations(
        const messaging::list_by_series_id_market_observations_request& request);
    /**@}*/

    /**
     * @brief Lists market observations with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of market observations for the requested page.
     */
    std::vector<domain::market_observation> list_market_observations(std::uint32_t offset,
                                                                     std::uint32_t limit);

    /**
     * @brief Gets the total count of active market observations.
     *
     * @return Total number of active market observations.
     */
    std::uint32_t count_market_observations();


    /**
     * @brief Lists market observations filtered by series_id, with pagination.
     *
     * @param series_id The series_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching market observations for the requested page.
     */
    std::vector<domain::market_observation> list_market_observations_by_series_id(
        const std::string& series_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active market observations filtered by series_id.
     *
     * @param series_id The series_id to filter by.
     * @return Total number of matching market observations.
     */
    std::uint32_t count_market_observations_by_series_id(const std::string& series_id);


    /**
     * @brief Retrieves a single market observation by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The market observation if found, std::nullopt otherwise.
     */
    std::optional<domain::market_observation> get_market_observation(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of market observations by primary key.
     */
    std::vector<domain::market_observation>
    get_market_observations(const std::vector<std::string>& ids);

    /**
     * @brief Saves a market observation (creates or updates).
     *
     * @param market_observation The market observation to save.
     * @throws std::exception on failure.
     */
    void save_market_observation(const domain::market_observation& market_observation);

    /**
     * @brief Saves a batch of market observations.
     *
     * @param market_observations The market observations to save.
     * @throws std::exception on failure.
     */
    void
    save_market_observations(const std::vector<domain::market_observation>& market_observations);

    /**
     * @brief Deletes a market observation by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_market_observation(const boost::uuids::uuid& id);

    /**
     * @brief Deletes market observations by their primary keys.
     */
    void delete_market_observations(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a market observation.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::market_observation> get_market_observation_history(const std::string& id);

private:
    context ctx_;
    repository::market_observation_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::market_observation_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::market_observation& out);
};

}

#endif
