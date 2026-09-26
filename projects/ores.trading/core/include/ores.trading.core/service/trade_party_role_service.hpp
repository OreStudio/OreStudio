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
#ifndef ORES_TRADING_CORE_SERVICE_TRADE_PARTY_ROLE_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_TRADE_PARTY_ROLE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/trade_party_role.hpp"
#include "ores.trading.api/messaging/trade_party_role_protocol.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/trade_party_role_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing trade party roles.
 *
 * Provides a higher-level interface for trade party role operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT trade_party_role_service {
private:
    inline static std::string_view logger_name = "ores.trading.service.trade_party_role_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a trade_party_role_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit trade_party_role_service(context ctx);

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
    messaging::list_trade_party_roles_response
    list_trade_party_roles(const messaging::list_trade_party_roles_request& request);
    messaging::get_trade_party_role_response
    get_trade_party_role(const messaging::get_trade_party_role_request& request);
    messaging::get_many_trade_party_roles_response
    get_many_trade_party_roles(const messaging::get_many_trade_party_roles_request& request);
    messaging::put_trade_party_role_response
    put_trade_party_role(const messaging::put_trade_party_role_request& request);
    messaging::put_many_trade_party_roles_response
    put_many_trade_party_roles(const messaging::put_many_trade_party_roles_request& request);
    messaging::delete_trade_party_role_response
    delete_trade_party_role(const messaging::delete_trade_party_role_request& request);
    messaging::delete_many_trade_party_roles_response
    delete_many_trade_party_roles(const messaging::delete_many_trade_party_roles_request& request);
    messaging::list_trade_party_role_versions_response list_trade_party_role_versions(
        const messaging::list_trade_party_role_versions_request& request);
    messaging::get_trade_party_role_version_response
    get_trade_party_role_version(const messaging::get_trade_party_role_version_request& request);
    /**@}*/

    /**
     * @brief Lists trade party roles with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of trade party roles for the requested page.
     */
    std::vector<domain::trade_party_role> list_roles(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active trade party roles.
     *
     * @return Total number of active trade party roles.
     */
    std::uint32_t count_roles();


    /**
     * @brief Retrieves a single trade party role as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The trade party role at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::trade_party_role> get_role_at_version(const boost::uuids::uuid& id,
                                                                std::uint32_t version);

    /**
     * @brief Retrieves a single trade party role by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The trade party role if found, std::nullopt otherwise.
     */
    std::optional<domain::trade_party_role> get_role(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of trade party roles by primary key.
     */
    std::vector<domain::trade_party_role> get_roles(const std::vector<std::string>& ids);

    /**
     * @brief Saves a trade party role (creates or updates).
     *
     * @param role The trade party role to save.
     * @throws std::exception on failure.
     */
    void save_role(const domain::trade_party_role& role);

    /**
     * @brief Saves a batch of trade party roles.
     *
     * @param roles The trade party roles to save.
     * @throws std::exception on failure.
     */
    void save_roles(const std::vector<domain::trade_party_role>& roles);

    /**
     * @brief Deletes a trade party role by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_role(const boost::uuids::uuid& id);

    /**
     * @brief Deletes trade party roles by their primary keys.
     */
    void delete_roles(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a trade party role.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::trade_party_role> get_role_history(const std::string& id);

private:
    context ctx_;
    repository::trade_party_role_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::trade_party_role_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::trade_party_role& out);
};

}

#endif
