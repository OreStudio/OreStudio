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
#ifndef ORES_TRADING_CORE_SERVICE_BOND_OPTION_SERVICE_HPP
#define ORES_TRADING_CORE_SERVICE_BOND_OPTION_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/bond_option.hpp"
#include "ores.trading.api/messaging/bond_option_protocol.hpp"
#include "ores.trading.core/export.hpp"
#include "ores.trading.core/repository/bond_option_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::service {

/**
 * @brief Service for managing bond options.
 *
 * Provides a higher-level interface for bond option operations,
 * wrapping the underlying repository.
 */
class ORES_TRADING_CORE_EXPORT bond_option_service {
private:
    inline static std::string_view logger_name = "ores.trading.service.bond_option_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a bond_option_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit bond_option_service(context ctx);

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
    messaging::list_bond_options_response
    list_bond_options(const messaging::list_bond_options_request& request);
    messaging::get_bond_option_response
    get_bond_option(const messaging::get_bond_option_request& request);
    messaging::get_many_bond_options_response
    get_many_bond_options(const messaging::get_many_bond_options_request& request);
    messaging::put_bond_option_response
    put_bond_option(const messaging::put_bond_option_request& request);
    messaging::put_many_bond_options_response
    put_many_bond_options(const messaging::put_many_bond_options_request& request);
    messaging::delete_bond_option_response
    delete_bond_option(const messaging::delete_bond_option_request& request);
    messaging::delete_many_bond_options_response
    delete_many_bond_options(const messaging::delete_many_bond_options_request& request);
    messaging::list_bond_option_versions_response
    list_bond_option_versions(const messaging::list_bond_option_versions_request& request);
    messaging::get_bond_option_version_response
    get_bond_option_version(const messaging::get_bond_option_version_request& request);
    /**@}*/

    /**
     * @brief Lists bond options with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of bond options for the requested page.
     */
    std::vector<domain::bond_option> list_options(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active bond options.
     *
     * @return Total number of active bond options.
     */
    std::uint32_t count_options();


    /**
     * @brief Retrieves a single bond option as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The bond option at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::bond_option> get_option_at_version(const boost::uuids::uuid& trade_id,
                                                             std::uint32_t version);

    /**
     * @brief Retrieves a single bond option by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The bond option if found, std::nullopt otherwise.
     */
    std::optional<domain::bond_option> get_option(const boost::uuids::uuid& trade_id);

    /**
     * @brief Retrieves a batch of bond options by primary key.
     */
    std::vector<domain::bond_option> get_options(const std::vector<std::string>& trade_ids);

    /**
     * @brief Saves a bond option (creates or updates).
     *
     * @param option The bond option to save.
     * @throws std::exception on failure.
     */
    void save_option(const domain::bond_option& option);

    /**
     * @brief Saves a batch of bond options.
     *
     * @param options The bond options to save.
     * @throws std::exception on failure.
     */
    void save_options(const std::vector<domain::bond_option>& options);

    /**
     * @brief Deletes a bond option by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_option(const boost::uuids::uuid& trade_id);

    /**
     * @brief Deletes bond options by their primary keys.
     */
    void delete_options(const std::vector<std::string>& trade_ids);

    /**
     * @brief Retrieves all historical versions of a bond option.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::bond_option> get_option_history(const std::string& trade_id);

private:
    context ctx_;
    repository::bond_option_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::bond_option_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::bond_option& out);
};

}

#endif
