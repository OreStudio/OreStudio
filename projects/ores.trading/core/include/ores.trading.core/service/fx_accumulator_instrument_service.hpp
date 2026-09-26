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
#ifndef ORES_TRADING_CORE_SERVICE_FX_ACCUMULATOR_INSTRUMENT_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_FX_ACCUMULATOR_INSTRUMENT_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/fx_accumulator_instrument.hpp"
#include "ores.trading.api/messaging/fx_accumulator_instrument_protocol.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/fx_accumulator_instrument_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing FX accumulator instruments.
 *
 * Provides a higher-level interface for FX accumulator instrument operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT fx_accumulator_instrument_service {
private:
    inline static std::string_view logger_name =
        "ores.trading.service.fx_accumulator_instrument_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a fx_accumulator_instrument_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit fx_accumulator_instrument_service(context ctx);

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
    messaging::list_fx_accumulator_instruments_response list_fx_accumulator_instruments(
        const messaging::list_fx_accumulator_instruments_request& request);
    messaging::get_fx_accumulator_instrument_response
    get_fx_accumulator_instrument(const messaging::get_fx_accumulator_instrument_request& request);
    messaging::get_many_fx_accumulator_instruments_response get_many_fx_accumulator_instruments(
        const messaging::get_many_fx_accumulator_instruments_request& request);
    messaging::put_fx_accumulator_instrument_response
    put_fx_accumulator_instrument(const messaging::put_fx_accumulator_instrument_request& request);
    messaging::put_many_fx_accumulator_instruments_response put_many_fx_accumulator_instruments(
        const messaging::put_many_fx_accumulator_instruments_request& request);
    messaging::delete_fx_accumulator_instrument_response delete_fx_accumulator_instrument(
        const messaging::delete_fx_accumulator_instrument_request& request);
    messaging::delete_many_fx_accumulator_instruments_response
    delete_many_fx_accumulator_instruments(
        const messaging::delete_many_fx_accumulator_instruments_request& request);
    messaging::list_fx_accumulator_instrument_versions_response
    list_fx_accumulator_instrument_versions(
        const messaging::list_fx_accumulator_instrument_versions_request& request);
    messaging::get_fx_accumulator_instrument_version_response get_fx_accumulator_instrument_version(
        const messaging::get_fx_accumulator_instrument_version_request& request);
    /**@}*/

    /**
     * @brief Lists FX accumulator instruments with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of FX accumulator instruments for the requested page.
     */
    std::vector<domain::fx_accumulator_instrument>
    list_fx_accumulator_instruments(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active FX accumulator instruments.
     *
     * @return Total number of active FX accumulator instruments.
     */
    std::uint32_t count_fx_accumulator_instruments();


    /**
     * @brief Retrieves a single FX accumulator instrument as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The FX accumulator instrument at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::fx_accumulator_instrument>
    get_fx_accumulator_instrument_at_version(const boost::uuids::uuid& instrument_id,
                                             std::uint32_t version);

    /**
     * @brief Retrieves a single FX accumulator instrument by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The FX accumulator instrument if found, std::nullopt otherwise.
     */
    std::optional<domain::fx_accumulator_instrument>
    get_fx_accumulator_instrument(const boost::uuids::uuid& instrument_id);

    /**
     * @brief Retrieves a batch of FX accumulator instruments by primary key.
     */
    std::vector<domain::fx_accumulator_instrument>
    get_fx_accumulator_instruments(const std::vector<std::string>& instrument_ids);

    /**
     * @brief Saves a FX accumulator instrument (creates or updates).
     *
     * @param fx_accumulator_instrument The FX accumulator instrument to save.
     * @throws std::exception on failure.
     */
    void save_fx_accumulator_instrument(
        const domain::fx_accumulator_instrument& fx_accumulator_instrument);

    /**
     * @brief Saves a batch of FX accumulator instruments.
     *
     * @param fx_accumulator_instruments The FX accumulator instruments to save.
     * @throws std::exception on failure.
     */
    void save_fx_accumulator_instruments(
        const std::vector<domain::fx_accumulator_instrument>& fx_accumulator_instruments);

    /**
     * @brief Deletes a FX accumulator instrument by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_fx_accumulator_instrument(const boost::uuids::uuid& instrument_id);

    /**
     * @brief Deletes FX accumulator instruments by their primary keys.
     */
    void delete_fx_accumulator_instruments(const std::vector<std::string>& instrument_ids);

    /**
     * @brief Retrieves all historical versions of a FX accumulator instrument.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::fx_accumulator_instrument>
    get_fx_accumulator_instrument_history(const std::string& instrument_id);

private:
    context ctx_;
    repository::fx_accumulator_instrument_repository repo_;

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
    prepare_change(const messaging::fx_accumulator_instrument_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::fx_accumulator_instrument& out);
};

}

#endif
