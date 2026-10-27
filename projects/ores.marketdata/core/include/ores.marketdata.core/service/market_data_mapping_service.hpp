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
#ifndef ORES_MARKETDATA_CORE_SERVICE_MARKET_DATA_MAPPING_SERVICE_HPP
#define ORES_MARKETDATA_CORE_SERVICE_MARKET_DATA_MAPPING_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/market_data_mapping.hpp"
#include "ores.marketdata.api/messaging/market_data_mapping_protocol.hpp"
#include "ores.marketdata.core/export.hpp"
#include "ores.marketdata.core/repository/market_data_mapping_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::marketdata::service {

/**
 * @brief Service for managing market data mappings.
 *
 * Provides a higher-level interface for market data mapping operations,
 * wrapping the underlying repository.
 */
class ORES_MARKETDATA_CORE_EXPORT market_data_mapping_service {
private:
    inline static std::string_view logger_name =
        "ores.marketdata.service.market_data_mapping_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a market_data_mapping_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit market_data_mapping_service(context ctx);

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
    ::ores::marketdata::messaging::list_market_data_mappings_response list_market_data_mappings(
        const ::ores::marketdata::messaging::list_market_data_mappings_request& request);
    ::ores::marketdata::messaging::get_market_data_mapping_response get_market_data_mapping(
        const ::ores::marketdata::messaging::get_market_data_mapping_request& request);
    ::ores::marketdata::messaging::get_many_market_data_mappings_response
    get_many_market_data_mappings(
        const ::ores::marketdata::messaging::get_many_market_data_mappings_request& request);
    ::ores::marketdata::messaging::put_market_data_mapping_response put_market_data_mapping(
        const ::ores::marketdata::messaging::put_market_data_mapping_request& request);
    ::ores::marketdata::messaging::put_many_market_data_mappings_response
    put_many_market_data_mappings(
        const ::ores::marketdata::messaging::put_many_market_data_mappings_request& request);
    ::ores::marketdata::messaging::delete_market_data_mapping_response delete_market_data_mapping(
        const ::ores::marketdata::messaging::delete_market_data_mapping_request& request);
    ::ores::marketdata::messaging::delete_many_market_data_mappings_response
    delete_many_market_data_mappings(
        const ::ores::marketdata::messaging::delete_many_market_data_mappings_request& request);
    ::ores::marketdata::messaging::list_market_data_mapping_versions_response
    list_market_data_mapping_versions(
        const ::ores::marketdata::messaging::list_market_data_mapping_versions_request& request);
    ::ores::marketdata::messaging::get_market_data_mapping_version_response
    get_market_data_mapping_version(
        const ::ores::marketdata::messaging::get_market_data_mapping_version_request& request);
    /**@}*/

    /**
     * @brief Lists market data mappings with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of market data mappings for the requested page.
     */
    std::vector<domain::market_data_mapping> list_mappings(std::uint32_t offset,
                                                           std::uint32_t limit);

    /**
     * @brief Gets the total count of active market data mappings.
     *
     * @return Total number of active market data mappings.
     */
    std::uint32_t count_mappings();


    /**
     * @brief Retrieves a single market data mapping as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The market data mapping at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::market_data_mapping> get_mapping_at_version(const boost::uuids::uuid& id,
                                                                      std::uint32_t version);

    /**
     * @brief Retrieves a single market data mapping by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The market data mapping if found, std::nullopt otherwise.
     */
    std::optional<domain::market_data_mapping> get_mapping(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a single market data mapping by the key the model
     * declares -- the human-readable key a caller holds.
     *
     * This is the counterpart of the uuid overload above: the two keys an
     * entity holds are different keys, and a call site has to say which one it
     * means.
     *
     * @return The market data mapping if found, std::nullopt otherwise.
     */
    std::optional<domain::market_data_mapping>
    get_mapping_by_oresmd_uri(const std::string& oresmd_uri);

    /**
     * @brief Retrieves a batch of market data mappings by primary key.
     */
    std::vector<domain::market_data_mapping> get_mappings(const std::vector<std::string>& ids);

    /**
     * @brief Saves a market data mapping (creates or updates).
     *
     * @param mapping The market data mapping to save.
     * @throws std::exception on failure.
     */
    void save_mapping(const domain::market_data_mapping& mapping);

    /**
     * @brief Saves a batch of market data mappings.
     *
     * @param mappings The market data mappings to save.
     * @throws std::exception on failure.
     */
    void save_mappings(const std::vector<domain::market_data_mapping>& mappings);

    /**
     * @brief Deletes a market data mapping by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_mapping(const boost::uuids::uuid& id);

    /**
     * @brief Deletes market data mappings by their primary keys.
     */
    void delete_mappings(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a market data mapping.
     *
     * Addressed by the key the model declares, which is the one a caller
     * holds; the storage key is resolved from it here, the same step every
     * other read makes.
     */
    std::vector<domain::market_data_mapping> get_mapping_history(const std::string& key);

private:
    context ctx_;
    repository::market_data_mapping_repository repo_;

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
    prepare_change(const ::ores::marketdata::messaging::market_data_mapping_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::market_data_mapping& out);
};

}

#endif
