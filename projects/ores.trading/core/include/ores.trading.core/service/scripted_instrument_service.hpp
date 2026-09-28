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
#ifndef ORES_TRADING_CORE_SERVICE_SCRIPTED_INSTRUMENT_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_SCRIPTED_INSTRUMENT_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/scripted_instrument.hpp"
#include "ores.trading.api/messaging/scripted_instrument_protocol.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/scripted_instrument_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing scripted instruments.
 *
 * Provides a higher-level interface for scripted instrument operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT scripted_instrument_service {
private:
    inline static std::string_view logger_name = "ores.trading.service.scripted_instrument_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a scripted_instrument_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit scripted_instrument_service(context ctx);

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
    messaging::list_scripted_instruments_response
    list_scripted_instruments(const messaging::list_scripted_instruments_request& request);
    messaging::get_scripted_instrument_response
    get_scripted_instrument(const messaging::get_scripted_instrument_request& request);
    messaging::get_many_scripted_instruments_response
    get_many_scripted_instruments(const messaging::get_many_scripted_instruments_request& request);
    messaging::put_scripted_instrument_response
    put_scripted_instrument(const messaging::put_scripted_instrument_request& request);
    messaging::put_many_scripted_instruments_response
    put_many_scripted_instruments(const messaging::put_many_scripted_instruments_request& request);
    messaging::delete_scripted_instrument_response
    delete_scripted_instrument(const messaging::delete_scripted_instrument_request& request);
    messaging::delete_many_scripted_instruments_response delete_many_scripted_instruments(
        const messaging::delete_many_scripted_instruments_request& request);
    messaging::list_scripted_instrument_versions_response list_scripted_instrument_versions(
        const messaging::list_scripted_instrument_versions_request& request);
    messaging::get_scripted_instrument_version_response get_scripted_instrument_version(
        const messaging::get_scripted_instrument_version_request& request);
    /**@}*/

    /**
     * @brief Lists scripted instruments with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of scripted instruments for the requested page.
     */
    std::vector<domain::scripted_instrument> list_scripted_instruments(std::uint32_t offset,
                                                                       std::uint32_t limit);

    /**
     * @brief Gets the total count of active scripted instruments.
     *
     * @return Total number of active scripted instruments.
     */
    std::uint32_t count_scripted_instruments();


    /**
     * @brief Retrieves a single scripted instrument as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The scripted instrument at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::scripted_instrument>
    get_scripted_instrument_at_version(const boost::uuids::uuid& instrument_id,
                                       std::uint32_t version);

    /**
     * @brief Retrieves a single scripted instrument by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The scripted instrument if found, std::nullopt otherwise.
     */
    std::optional<domain::scripted_instrument>
    get_scripted_instrument(const boost::uuids::uuid& instrument_id);

    /**
     * @brief Retrieves a batch of scripted instruments by primary key.
     */
    std::vector<domain::scripted_instrument>
    get_scripted_instruments(const std::vector<std::string>& instrument_ids);

    /**
     * @brief Saves a scripted instrument (creates or updates).
     *
     * @param scripted_instrument The scripted instrument to save.
     * @throws std::exception on failure.
     */
    void save_scripted_instrument(const domain::scripted_instrument& scripted_instrument);

    /**
     * @brief Saves a batch of scripted instruments.
     *
     * @param scripted_instruments The scripted instruments to save.
     * @throws std::exception on failure.
     */
    void
    save_scripted_instruments(const std::vector<domain::scripted_instrument>& scripted_instruments);

    /**
     * @brief Deletes a scripted instrument by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_scripted_instrument(const boost::uuids::uuid& instrument_id);

    /**
     * @brief Deletes scripted instruments by their primary keys.
     */
    void delete_scripted_instruments(const std::vector<std::string>& instrument_ids);

    /**
     * @brief Retrieves all historical versions of a scripted instrument.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::scripted_instrument>
    get_scripted_instrument_history(const std::string& instrument_id);

private:
    context ctx_;
    repository::scripted_instrument_repository repo_;

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
    prepare_change(const messaging::scripted_instrument_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::scripted_instrument& out);
};

}

#endif
