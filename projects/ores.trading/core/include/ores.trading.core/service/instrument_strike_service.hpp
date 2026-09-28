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
#ifndef ORES_TRADING_CORE_SERVICE_INSTRUMENT_STRIKE_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_INSTRUMENT_STRIKE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/instrument_strike.hpp"
#include "ores.trading.api/messaging/instrument_strike_protocol.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/instrument_strike_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing instrument strikes.
 *
 * Provides a higher-level interface for instrument strike operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT instrument_strike_service {
private:
    inline static std::string_view logger_name = "ores.trading.service.instrument_strike_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a instrument_strike_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit instrument_strike_service(context ctx);

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
    messaging::list_instrument_strikes_response
    list_instrument_strikes(const messaging::list_instrument_strikes_request& request);
    messaging::get_instrument_strike_response
    get_instrument_strike(const messaging::get_instrument_strike_request& request);
    messaging::get_many_instrument_strikes_response
    get_many_instrument_strikes(const messaging::get_many_instrument_strikes_request& request);
    messaging::put_instrument_strike_response
    put_instrument_strike(const messaging::put_instrument_strike_request& request);
    messaging::put_many_instrument_strikes_response
    put_many_instrument_strikes(const messaging::put_many_instrument_strikes_request& request);
    messaging::delete_instrument_strike_response
    delete_instrument_strike(const messaging::delete_instrument_strike_request& request);
    messaging::delete_many_instrument_strikes_response delete_many_instrument_strikes(
        const messaging::delete_many_instrument_strikes_request& request);
    messaging::list_instrument_strike_versions_response list_instrument_strike_versions(
        const messaging::list_instrument_strike_versions_request& request);
    messaging::get_instrument_strike_version_response
    get_instrument_strike_version(const messaging::get_instrument_strike_version_request& request);
    /**@}*/

    /**
     * @brief Lists instrument strikes with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of instrument strikes for the requested page.
     */
    std::vector<domain::instrument_strike> list_instrument_strikes(std::uint32_t offset,
                                                                   std::uint32_t limit);

    /**
     * @brief Gets the total count of active instrument strikes.
     *
     * @return Total number of active instrument strikes.
     */
    std::uint32_t count_instrument_strikes();


    /**
     * @brief Retrieves a single instrument strike as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The instrument strike at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::instrument_strike>
    get_instrument_strike_at_version(const boost::uuids::uuid& trade_id, std::uint32_t version);

    /**
     * @brief Retrieves a single instrument strike by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The instrument strike if found, std::nullopt otherwise.
     */
    std::optional<domain::instrument_strike>
    get_instrument_strike(const boost::uuids::uuid& trade_id);

    /**
     * @brief Retrieves a batch of instrument strikes by primary key.
     */
    std::vector<domain::instrument_strike>
    get_instrument_strikes(const std::vector<std::string>& trade_ids);

    /**
     * @brief Saves a instrument strike (creates or updates).
     *
     * @param instrument_strike The instrument strike to save.
     * @throws std::exception on failure.
     */
    void save_instrument_strike(const domain::instrument_strike& instrument_strike);

    /**
     * @brief Saves a batch of instrument strikes.
     *
     * @param instrument_strikes The instrument strikes to save.
     * @throws std::exception on failure.
     */
    void save_instrument_strikes(const std::vector<domain::instrument_strike>& instrument_strikes);

    /**
     * @brief Deletes a instrument strike by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_instrument_strike(const boost::uuids::uuid& trade_id);

    /**
     * @brief Deletes instrument strikes by their primary keys.
     */
    void delete_instrument_strikes(const std::vector<std::string>& trade_ids);

    /**
     * @brief Retrieves all historical versions of a instrument strike.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::instrument_strike>
    get_instrument_strike_history(const std::string& trade_id);

private:
    context ctx_;
    repository::instrument_strike_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::instrument_strike_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::instrument_strike& out);
};

}

#endif
