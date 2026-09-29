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
#ifndef ORES_TRADING_CORE_SERVICE_EQUITY_BARRIER_OPTION_INSTRUMENT_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_EQUITY_BARRIER_OPTION_INSTRUMENT_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/equity_barrier_option_instrument.hpp"
#include "ores.trading.api/messaging/equity_barrier_option_instrument_protocol.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/equity_barrier_option_instrument_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing Equity Barrier Option instruments.
 *
 * Provides a higher-level interface for Equity Barrier Option instrument operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT equity_barrier_option_instrument_service {
private:
    inline static std::string_view logger_name =
        "ores.trading.service.equity_barrier_option_instrument_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a equity_barrier_option_instrument_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit equity_barrier_option_instrument_service(context ctx);

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
    messaging::list_equity_barrier_option_instruments_response
    list_equity_barrier_option_instruments(
        const messaging::list_equity_barrier_option_instruments_request& request);
    messaging::get_equity_barrier_option_instrument_response get_equity_barrier_option_instrument(
        const messaging::get_equity_barrier_option_instrument_request& request);
    messaging::get_many_equity_barrier_option_instruments_response
    get_many_equity_barrier_option_instruments(
        const messaging::get_many_equity_barrier_option_instruments_request& request);
    messaging::put_equity_barrier_option_instrument_response put_equity_barrier_option_instrument(
        const messaging::put_equity_barrier_option_instrument_request& request);
    messaging::put_many_equity_barrier_option_instruments_response
    put_many_equity_barrier_option_instruments(
        const messaging::put_many_equity_barrier_option_instruments_request& request);
    messaging::delete_equity_barrier_option_instrument_response
    delete_equity_barrier_option_instrument(
        const messaging::delete_equity_barrier_option_instrument_request& request);
    messaging::delete_many_equity_barrier_option_instruments_response
    delete_many_equity_barrier_option_instruments(
        const messaging::delete_many_equity_barrier_option_instruments_request& request);
    messaging::list_equity_barrier_option_instrument_versions_response
    list_equity_barrier_option_instrument_versions(
        const messaging::list_equity_barrier_option_instrument_versions_request& request);
    messaging::get_equity_barrier_option_instrument_version_response
    get_equity_barrier_option_instrument_version(
        const messaging::get_equity_barrier_option_instrument_version_request& request);
    /**@}*/

    /**
     * @brief Lists Equity Barrier Option instruments with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of Equity Barrier Option instruments for the requested page.
     */
    std::vector<domain::equity_barrier_option_instrument>
    list_equity_barrier_option_instruments(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active Equity Barrier Option instruments.
     *
     * @return Total number of active Equity Barrier Option instruments.
     */
    std::uint32_t count_equity_barrier_option_instruments();


    /**
     * @brief Retrieves a single Equity Barrier Option instrument as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The Equity Barrier Option instrument at that version if found, std::nullopt
     * otherwise.
     */
    std::optional<domain::equity_barrier_option_instrument>
    get_equity_barrier_option_instrument_at_version(const boost::uuids::uuid& trade_id,
                                                    std::uint32_t version);

    /**
     * @brief Retrieves a single Equity Barrier Option instrument by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The Equity Barrier Option instrument if found, std::nullopt otherwise.
     */
    std::optional<domain::equity_barrier_option_instrument>
    get_equity_barrier_option_instrument(const boost::uuids::uuid& trade_id);

    /**
     * @brief Retrieves a batch of Equity Barrier Option instruments by primary key.
     */
    std::vector<domain::equity_barrier_option_instrument>
    get_equity_barrier_option_instruments(const std::vector<std::string>& trade_ids);

    /**
     * @brief Saves a Equity Barrier Option instrument (creates or updates).
     *
     * @param equity_barrier_option_instrument The Equity Barrier Option instrument to save.
     * @throws std::exception on failure.
     */
    void save_equity_barrier_option_instrument(
        const domain::equity_barrier_option_instrument& equity_barrier_option_instrument);

    /**
     * @brief Saves a batch of Equity Barrier Option instruments.
     *
     * @param equity_barrier_option_instruments The Equity Barrier Option instruments to save.
     * @throws std::exception on failure.
     */
    void save_equity_barrier_option_instruments(
        const std::vector<domain::equity_barrier_option_instrument>&
            equity_barrier_option_instruments);

    /**
     * @brief Deletes a Equity Barrier Option instrument by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_equity_barrier_option_instrument(const boost::uuids::uuid& trade_id);

    /**
     * @brief Deletes Equity Barrier Option instruments by their primary keys.
     */
    void delete_equity_barrier_option_instruments(const std::vector<std::string>& trade_ids);

    /**
     * @brief Retrieves all historical versions of a Equity Barrier Option instrument.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::equity_barrier_option_instrument>
    get_equity_barrier_option_instrument_history(const std::string& trade_id);

private:
    context ctx_;
    repository::equity_barrier_option_instrument_repository repo_;

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
    prepare_change(const messaging::equity_barrier_option_instrument_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::equity_barrier_option_instrument& out);
};

}

#endif
