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
#ifndef ORES_TRADING_CORE_SERVICE_TRADE_ENVELOPE_PORTFOLIO_ID_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_TRADE_ENVELOPE_PORTFOLIO_ID_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/trade_envelope_portfolio_id.hpp"
#include "ores.trading.api/messaging/trade_envelope_portfolio_id_protocol.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/trade_envelope_portfolio_id_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing trade envelope portfolio identifiers.
 *
 * Provides a higher-level interface for trade envelope portfolio identifier operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT trade_envelope_portfolio_id_service {
private:
    inline static std::string_view logger_name =
        "ores.trading.service.trade_envelope_portfolio_id_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a trade_envelope_portfolio_id_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit trade_envelope_portfolio_id_service(context ctx);

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
    messaging::list_trade_envelope_portfolio_ids_response list_trade_envelope_portfolio_ids(
        const messaging::list_trade_envelope_portfolio_ids_request& request);
    messaging::get_trade_envelope_portfolio_id_response get_trade_envelope_portfolio_id(
        const messaging::get_trade_envelope_portfolio_id_request& request);
    messaging::get_many_trade_envelope_portfolio_ids_response get_many_trade_envelope_portfolio_ids(
        const messaging::get_many_trade_envelope_portfolio_ids_request& request);
    messaging::put_trade_envelope_portfolio_id_response put_trade_envelope_portfolio_id(
        const messaging::put_trade_envelope_portfolio_id_request& request);
    messaging::put_many_trade_envelope_portfolio_ids_response put_many_trade_envelope_portfolio_ids(
        const messaging::put_many_trade_envelope_portfolio_ids_request& request);
    messaging::delete_trade_envelope_portfolio_id_response delete_trade_envelope_portfolio_id(
        const messaging::delete_trade_envelope_portfolio_id_request& request);
    messaging::delete_many_trade_envelope_portfolio_ids_response
    delete_many_trade_envelope_portfolio_ids(
        const messaging::delete_many_trade_envelope_portfolio_ids_request& request);
    messaging::list_trade_envelope_portfolio_id_versions_response
    list_trade_envelope_portfolio_id_versions(
        const messaging::list_trade_envelope_portfolio_id_versions_request& request);
    messaging::get_trade_envelope_portfolio_id_version_response
    get_trade_envelope_portfolio_id_version(
        const messaging::get_trade_envelope_portfolio_id_version_request& request);
    /**@}*/

    /**
     * @brief Lists trade envelope portfolio identifiers with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of trade envelope portfolio identifiers for the requested page.
     */
    std::vector<domain::trade_envelope_portfolio_id>
    list_trade_envelope_portfolio_ids(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active trade envelope portfolio identifiers.
     *
     * @return Total number of active trade envelope portfolio identifiers.
     */
    std::uint32_t count_trade_envelope_portfolio_ids();


    /**
     * @brief Retrieves a single trade envelope portfolio identifier as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The trade envelope portfolio identifier at that version if found, std::nullopt
     * otherwise.
     */
    std::optional<domain::trade_envelope_portfolio_id> get_trade_envelope_portfolio_id_at_version(
        const std::string& trade_id, const std::string& sequence_number, std::uint32_t version);

    /**
     * @brief Retrieves a single trade envelope portfolio identifier by its primary key.
     *
     * @return The trade envelope portfolio identifier if found, std::nullopt otherwise.
     */
    std::optional<domain::trade_envelope_portfolio_id>
    get_trade_envelope_portfolio_id(const std::string& trade_id,
                                    const std::string& sequence_number);

    /**
     * @brief Retrieves a batch of trade envelope portfolio identifiers by primary key.
     */
    std::vector<domain::trade_envelope_portfolio_id>
    get_trade_envelope_portfolio_ids(const std::vector<std::string>& trade_ids,
                                     const std::vector<std::string>& sequence_numbers);

    /**
     * @brief Saves a trade envelope portfolio identifier (creates or updates).
     *
     * @param trade_envelope_portfolio_id The trade envelope portfolio identifier to save.
     * @throws std::exception on failure.
     */
    void save_trade_envelope_portfolio_id(
        const domain::trade_envelope_portfolio_id& trade_envelope_portfolio_id);

    /**
     * @brief Saves a batch of trade envelope portfolio identifiers.
     *
     * @param trade_envelope_portfolio_ids The trade envelope portfolio identifiers to save.
     * @throws std::exception on failure.
     */
    void save_trade_envelope_portfolio_ids(
        const std::vector<domain::trade_envelope_portfolio_id>& trade_envelope_portfolio_ids);

    /**
     * @brief Deletes a trade envelope portfolio identifier by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_trade_envelope_portfolio_id(const std::string& trade_id,
                                            const std::string& sequence_number);

    /**
     * @brief Deletes trade envelope portfolio identifiers by their primary keys.
     */
    void delete_trade_envelope_portfolio_ids(const std::vector<std::string>& trade_ids,
                                             const std::vector<std::string>& sequence_numbers);

    /**
     * @brief Retrieves all historical versions of a trade envelope portfolio identifier.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::trade_envelope_portfolio_id>
    get_trade_envelope_portfolio_id_history(const std::string& trade_id,
                                            const std::string& sequence_number);

private:
    context ctx_;
    repository::trade_envelope_portfolio_id_repository repo_;

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
    prepare_change(const messaging::trade_envelope_portfolio_id_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::trade_envelope_portfolio_id& out);
};

}

#endif
