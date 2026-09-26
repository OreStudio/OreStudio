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
#ifndef ORES_TRADING_CORE_SERVICE_BALANCE_GUARANTEED_SWAP_INSTRUMENT_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_BALANCE_GUARANTEED_SWAP_INSTRUMENT_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/balance_guaranteed_swap_instrument.hpp"
#include "ores.trading.api/messaging/balance_guaranteed_swap_instrument_protocol.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/balance_guaranteed_swap_instrument_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing balance guaranteed swap instruments.
 *
 * Provides a higher-level interface for balance guaranteed swap instrument operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT balance_guaranteed_swap_instrument_service {
private:
    inline static std::string_view logger_name =
        "ores.trading.service.balance_guaranteed_swap_instrument_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a balance_guaranteed_swap_instrument_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit balance_guaranteed_swap_instrument_service(context ctx);

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
    messaging::list_balance_guaranteed_swap_instruments_response
    list_balance_guaranteed_swap_instruments(
        const messaging::list_balance_guaranteed_swap_instruments_request& request);
    messaging::get_balance_guaranteed_swap_instrument_response
    get_balance_guaranteed_swap_instrument(
        const messaging::get_balance_guaranteed_swap_instrument_request& request);
    messaging::get_many_balance_guaranteed_swap_instruments_response
    get_many_balance_guaranteed_swap_instruments(
        const messaging::get_many_balance_guaranteed_swap_instruments_request& request);
    messaging::put_balance_guaranteed_swap_instrument_response
    put_balance_guaranteed_swap_instrument(
        const messaging::put_balance_guaranteed_swap_instrument_request& request);
    messaging::put_many_balance_guaranteed_swap_instruments_response
    put_many_balance_guaranteed_swap_instruments(
        const messaging::put_many_balance_guaranteed_swap_instruments_request& request);
    messaging::delete_balance_guaranteed_swap_instrument_response
    delete_balance_guaranteed_swap_instrument(
        const messaging::delete_balance_guaranteed_swap_instrument_request& request);
    messaging::delete_many_balance_guaranteed_swap_instruments_response
    delete_many_balance_guaranteed_swap_instruments(
        const messaging::delete_many_balance_guaranteed_swap_instruments_request& request);
    messaging::list_balance_guaranteed_swap_instrument_versions_response
    list_balance_guaranteed_swap_instrument_versions(
        const messaging::list_balance_guaranteed_swap_instrument_versions_request& request);
    messaging::get_balance_guaranteed_swap_instrument_version_response
    get_balance_guaranteed_swap_instrument_version(
        const messaging::get_balance_guaranteed_swap_instrument_version_request& request);
    /**@}*/

    /**
     * @brief Lists balance guaranteed swap instruments with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of balance guaranteed swap instruments for the requested page.
     */
    std::vector<domain::balance_guaranteed_swap_instrument>
    list_balance_guaranteed_swap_instruments(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active balance guaranteed swap instruments.
     *
     * @return Total number of active balance guaranteed swap instruments.
     */
    std::uint32_t count_balance_guaranteed_swap_instruments();


    /**
     * @brief Retrieves a single balance guaranteed swap instrument as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The balance guaranteed swap instrument at that version if found, std::nullopt
     * otherwise.
     */
    std::optional<domain::balance_guaranteed_swap_instrument>
    get_balance_guaranteed_swap_instrument_at_version(const boost::uuids::uuid& instrument_id,
                                                      std::uint32_t version);

    /**
     * @brief Retrieves a single balance guaranteed swap instrument by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The balance guaranteed swap instrument if found, std::nullopt otherwise.
     */
    std::optional<domain::balance_guaranteed_swap_instrument>
    get_balance_guaranteed_swap_instrument(const boost::uuids::uuid& instrument_id);

    /**
     * @brief Retrieves a batch of balance guaranteed swap instruments by primary key.
     */
    std::vector<domain::balance_guaranteed_swap_instrument>
    get_balance_guaranteed_swap_instruments(const std::vector<std::string>& instrument_ids);

    /**
     * @brief Saves a balance guaranteed swap instrument (creates or updates).
     *
     * @param balance_guaranteed_swap_instrument The balance guaranteed swap instrument to save.
     * @throws std::exception on failure.
     */
    void save_balance_guaranteed_swap_instrument(
        const domain::balance_guaranteed_swap_instrument& balance_guaranteed_swap_instrument);

    /**
     * @brief Saves a batch of balance guaranteed swap instruments.
     *
     * @param balance_guaranteed_swap_instruments The balance guaranteed swap instruments to save.
     * @throws std::exception on failure.
     */
    void save_balance_guaranteed_swap_instruments(
        const std::vector<domain::balance_guaranteed_swap_instrument>&
            balance_guaranteed_swap_instruments);

    /**
     * @brief Deletes a balance guaranteed swap instrument by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_balance_guaranteed_swap_instrument(const boost::uuids::uuid& instrument_id);

    /**
     * @brief Deletes balance guaranteed swap instruments by their primary keys.
     */
    void delete_balance_guaranteed_swap_instruments(const std::vector<std::string>& instrument_ids);

    /**
     * @brief Retrieves all historical versions of a balance guaranteed swap instrument.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::balance_guaranteed_swap_instrument>
    get_balance_guaranteed_swap_instrument_history(const std::string& instrument_id);

private:
    context ctx_;
    repository::balance_guaranteed_swap_instrument_repository repo_;

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
    prepare_change(const messaging::balance_guaranteed_swap_instrument_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::balance_guaranteed_swap_instrument& out);
};

}

#endif
