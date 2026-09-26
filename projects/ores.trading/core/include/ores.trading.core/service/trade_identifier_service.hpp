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
#ifndef ORES_TRADING_CORE_SERVICE_TRADE_IDENTIFIER_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_TRADE_IDENTIFIER_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/trade_identifier.hpp"
#include "ores.trading.api/messaging/trade_identifier_protocol.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/trade_identifier_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing trade identifiers.
 *
 * Provides a higher-level interface for trade identifier operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT trade_identifier_service {
private:
    inline static std::string_view logger_name = "ores.trading.service.trade_identifier_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a trade_identifier_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit trade_identifier_service(context ctx);

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
    messaging::list_trade_identifiers_response
    list_trade_identifiers(const messaging::list_trade_identifiers_request& request);
    messaging::get_trade_identifier_response
    get_trade_identifier(const messaging::get_trade_identifier_request& request);
    messaging::get_many_trade_identifiers_response
    get_many_trade_identifiers(const messaging::get_many_trade_identifiers_request& request);
    messaging::put_trade_identifier_response
    put_trade_identifier(const messaging::put_trade_identifier_request& request);
    messaging::put_many_trade_identifiers_response
    put_many_trade_identifiers(const messaging::put_many_trade_identifiers_request& request);
    messaging::delete_trade_identifier_response
    delete_trade_identifier(const messaging::delete_trade_identifier_request& request);
    messaging::delete_many_trade_identifiers_response
    delete_many_trade_identifiers(const messaging::delete_many_trade_identifiers_request& request);
    messaging::list_trade_identifier_versions_response list_trade_identifier_versions(
        const messaging::list_trade_identifier_versions_request& request);
    messaging::get_trade_identifier_version_response
    get_trade_identifier_version(const messaging::get_trade_identifier_version_request& request);
    /**@}*/

    /**
     * @brief Lists trade identifiers with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of trade identifiers for the requested page.
     */
    std::vector<domain::trade_identifier> list_identifiers(std::uint32_t offset,
                                                           std::uint32_t limit);

    /**
     * @brief Gets the total count of active trade identifiers.
     *
     * @return Total number of active trade identifiers.
     */
    std::uint32_t count_identifiers();


    /**
     * @brief Retrieves a single trade identifier as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The trade identifier at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::trade_identifier> get_identifier_at_version(const boost::uuids::uuid& id,
                                                                      std::uint32_t version);

    /**
     * @brief Retrieves a single trade identifier by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The trade identifier if found, std::nullopt otherwise.
     */
    std::optional<domain::trade_identifier> get_identifier(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of trade identifiers by primary key.
     */
    std::vector<domain::trade_identifier> get_identifiers(const std::vector<std::string>& ids);

    /**
     * @brief Saves a trade identifier (creates or updates).
     *
     * @param identifier The trade identifier to save.
     * @throws std::exception on failure.
     */
    void save_identifier(const domain::trade_identifier& identifier);

    /**
     * @brief Saves a batch of trade identifiers.
     *
     * @param identifiers The trade identifiers to save.
     * @throws std::exception on failure.
     */
    void save_identifiers(const std::vector<domain::trade_identifier>& identifiers);

    /**
     * @brief Deletes a trade identifier by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_identifier(const boost::uuids::uuid& id);

    /**
     * @brief Deletes trade identifiers by their primary keys.
     */
    void delete_identifiers(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a trade identifier.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::trade_identifier> get_identifier_history(const std::string& id);

private:
    context ctx_;
    repository::trade_identifier_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::trade_identifier_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::trade_identifier& out);
};

}

#endif
