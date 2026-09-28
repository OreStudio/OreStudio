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
#ifndef ORES_TRADING_CORE_SERVICE_CAP_FLOOR_INSTRUMENT_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_CAP_FLOOR_INSTRUMENT_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/cap_floor_instrument.hpp"
#include "ores.trading.api/messaging/cap_floor_instrument_protocol.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/cap_floor_instrument_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing cap/floor instruments.
 *
 * Provides a higher-level interface for cap/floor instrument operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT cap_floor_instrument_service {
private:
    inline static std::string_view logger_name =
        "ores.trading.service.cap_floor_instrument_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a cap_floor_instrument_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit cap_floor_instrument_service(context ctx);

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
    messaging::list_cap_floor_instruments_response
    list_cap_floor_instruments(const messaging::list_cap_floor_instruments_request& request);
    messaging::get_cap_floor_instrument_response
    get_cap_floor_instrument(const messaging::get_cap_floor_instrument_request& request);
    messaging::get_many_cap_floor_instruments_response get_many_cap_floor_instruments(
        const messaging::get_many_cap_floor_instruments_request& request);
    messaging::put_cap_floor_instrument_response
    put_cap_floor_instrument(const messaging::put_cap_floor_instrument_request& request);
    messaging::put_many_cap_floor_instruments_response put_many_cap_floor_instruments(
        const messaging::put_many_cap_floor_instruments_request& request);
    messaging::delete_cap_floor_instrument_response
    delete_cap_floor_instrument(const messaging::delete_cap_floor_instrument_request& request);
    messaging::delete_many_cap_floor_instruments_response delete_many_cap_floor_instruments(
        const messaging::delete_many_cap_floor_instruments_request& request);
    messaging::list_cap_floor_instrument_versions_response list_cap_floor_instrument_versions(
        const messaging::list_cap_floor_instrument_versions_request& request);
    messaging::get_cap_floor_instrument_version_response get_cap_floor_instrument_version(
        const messaging::get_cap_floor_instrument_version_request& request);
    /**@}*/

    /**
     * @brief Lists cap/floor instruments with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of cap/floor instruments for the requested page.
     */
    std::vector<domain::cap_floor_instrument> list_cap_floor_instruments(std::uint32_t offset,
                                                                         std::uint32_t limit);

    /**
     * @brief Gets the total count of active cap/floor instruments.
     *
     * @return Total number of active cap/floor instruments.
     */
    std::uint32_t count_cap_floor_instruments();


    /**
     * @brief Retrieves a single cap/floor instrument as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The cap/floor instrument at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::cap_floor_instrument>
    get_cap_floor_instrument_at_version(const boost::uuids::uuid& trade_id, std::uint32_t version);

    /**
     * @brief Retrieves a single cap/floor instrument by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The cap/floor instrument if found, std::nullopt otherwise.
     */
    std::optional<domain::cap_floor_instrument>
    get_cap_floor_instrument(const boost::uuids::uuid& trade_id);

    /**
     * @brief Retrieves a batch of cap/floor instruments by primary key.
     */
    std::vector<domain::cap_floor_instrument>
    get_cap_floor_instruments(const std::vector<std::string>& trade_ids);

    /**
     * @brief Saves a cap/floor instrument (creates or updates).
     *
     * @param cap_floor_instrument The cap/floor instrument to save.
     * @throws std::exception on failure.
     */
    void save_cap_floor_instrument(const domain::cap_floor_instrument& cap_floor_instrument);

    /**
     * @brief Saves a batch of cap/floor instruments.
     *
     * @param cap_floor_instruments The cap/floor instruments to save.
     * @throws std::exception on failure.
     */
    void save_cap_floor_instruments(
        const std::vector<domain::cap_floor_instrument>& cap_floor_instruments);

    /**
     * @brief Deletes a cap/floor instrument by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_cap_floor_instrument(const boost::uuids::uuid& trade_id);

    /**
     * @brief Deletes cap/floor instruments by their primary keys.
     */
    void delete_cap_floor_instruments(const std::vector<std::string>& trade_ids);

    /**
     * @brief Retrieves all historical versions of a cap/floor instrument.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::cap_floor_instrument>
    get_cap_floor_instrument_history(const std::string& trade_id);

private:
    context ctx_;
    repository::cap_floor_instrument_repository repo_;

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
    prepare_change(const messaging::cap_floor_instrument_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::cap_floor_instrument& out);
};

}

#endif
